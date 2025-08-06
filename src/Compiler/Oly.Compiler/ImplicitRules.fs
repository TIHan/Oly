module internal Oly.Compiler.Internal.ImplicitRules

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.WellKnownExpressions

let private isIntrinsicBitwiseForEnum (func: IFunctionSymbol) =
    if func.TypeParameters.IsEmpty then
        match func.WellKnownFunction with
        | WellKnownFunction.BitwiseOr
        | WellKnownFunction.BitwiseAnd
        | WellKnownFunction.BitwiseExclusiveOr
        | WellKnownFunction.BitwiseNot -> true
        | _ -> false
    else
        false

let private isIntrinsicForEnum (func: IFunctionSymbol) =
    if func.TypeParameters.IsEmpty then
        match func.WellKnownFunction with
        | WellKnownFunction.Equal
        | WellKnownFunction.NotEqual -> true
        | _ -> isIntrinsicBitwiseForEnum func
    else
        false

let private hasAllEnumTypes (tys: TypeSymbol imarray) =
    tys |> ImArray.forall (fun x -> x.IsEnum)

let private isFirstExpressionAnEnumType (exprs: E imarray) =
    if exprs.IsEmpty then
        false
    else
        exprs[0].Type.IsEnum

let private alterParameterAndReturnTypesForEnumOperations (func: IFunctionSymbol) (ty: TypeSymbol) =
    match func with
    | :? FunctionGroupSymbol ->
        OlyAssert.Fail("Unexpected function group.")
    | _ ->
        OlyAssert.True(func.TryWellKnownFunction.IsSome)
        OlyAssert.True(func.TypeParameters.IsEmpty)
        OlyAssert.True(func.TypeArguments.IsEmpty)
        OlyAssert.True(isIntrinsicForEnum func)

        let id = newId()

        let newPars =
            func.Parameters
            |> ImArray.map (fun par ->
                createLocalParameterValue(par.Attributes, par.Name, ty, par.IsMutable)
            )

        let returnTy =
            match func.TryWellKnownFunction with
            | ValueSome(WellKnownFunction.Equal)
            | ValueSome(WellKnownFunction.NotEqual) ->
                TypeSymbol.Bool
            | _ ->
                ty

        let funcTy =
            match stripTypeEquations func.Type with
            | TypeSymbol.Function(_, _, kind) ->
                TypeSymbol.CreateFunction(
                    ImArray.init func.Parameters.Length (fun _ -> ty),
                    returnTy,
                    kind
                )
            | _ ->
                OlyAssert.Fail("Unsupported type.")

        { new IFunctionSymbol with

                member _.Enclosing = func.Enclosing

                member _.Name = func.Name

                member _.Type = funcTy

                member _.Id = id

                member _.TypeParameters = func.TypeParameters

                member _.TypeArguments = func.TypeArguments

                member _.IsFunction = func.IsFunction

                member _.IsFunctionGroup = func.IsFunctionGroup

                member _.IsField = func.IsField

                member _.MemberFlags = func.MemberFlags

                member _.FunctionFlags = func.FunctionFlags

                member _.FunctionOverrides = func.FunctionOverrides

                member _.IsProperty = func.IsProperty

                member _.IsPattern = false

                member this.Formal = func.Formal

                member _.Parameters = newPars

                member _.ReturnType = returnTy

                member _.Attributes = func.Attributes

                member _.ValueFlags = func.ValueFlags

                member _.IsThis = func.IsThis

                member _.IsBase = func.IsBase

                member _.Semantic = func.Semantic

                member _.WellKnownFunction = func.WellKnownFunction

                member _.AssociatedFormalPattern = func.AssociatedFormalPattern

            }

let private tryMorphSimpleImplicit (expectedTy: TypeSymbol) (expr: E) =
    if expectedTy.IsUnit_t && expr.Type.IsRealUnit then
        Ignore expr
    elif expectedTy.IsRealUnit && expr.Type.IsUnit_t then
        E.Sequential(BoundSyntaxInfo.Generated(expr.Syntax.Tree),
            expr,
            E.Unit(BoundSyntaxInfo.Generated(expr.Syntax.Tree)),
            NormalSequential
        )
    else
        expr

let private tryMorphPartialCall (expr: E) =
    match expr with
    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, flags)
        when flags.HasFlag(CallFlags.Partial) &&
                receiverExprOpt.IsSome && 
                argExprs.IsEmpty && 
                value.IsInstance && 
                value.IsFunction &&
                not value.IsFunctionGroup ->

        let isVirtualCall = flags.HasFlag(CallFlags.Virtual)

        let func = value.AsFunction

        // partial application / partial call
        // example: Add(a.FunctionCall)
        let pars =
            func.LogicalParameters 
            |> ROMem.mapAsImArray (fun p ->
                createLocalParameterValue(ImArray.empty, p.Name, p.Type, false)
            )
        let argExprs =
            pars
            |> ImArray.map (fun x -> E.CreateValue(expr.Syntax.Tree, x))
        E.CreateLambda(BoundSyntaxInfo.Generated(expr.Syntax.Tree),
            LambdaFlags.None,
            ImArray.empty,
            pars,
            LazyExpression.CreateNonLazy(None, 
                fun _ ->
                    E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, if isVirtualCall then CallFlags.Virtual else CallFlags.None)
            )
        )
    | _ ->
        expr

let private tryMorphArgumentImplicit hasStrictInference (expectedTy: TypeSymbol) (expr: E) =
    let expr = tryMorphSimpleImplicit expectedTy expr
    let exprTy = expr.Type
    if expectedTy.IsFunctionNotPtr && exprTy.IsFunctionNotPtr then
        match expectedTy.TryFunction, exprTy.TryFunction with
        | ValueSome(_, expectedReturnTy), ValueSome(_, returnTy) when not expectedReturnTy.IsNativeFunctionPtr_t ->
            if (expectedReturnTy.IsRealUnit || (expectedReturnTy.IsTypeVariableZeroArity && not hasStrictInference)) && returnTy.IsUnit_t then
                match expr with
                | E.Lambda(syntaxInfo, lambdaFlags, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr, _, _, _) ->
                    E.CreateLambda(syntaxInfo, lambdaFlags, lambdaTyPars, lambdaPars,
                        LazyExpression.CreateNonLazy(
                            lazyLambdaBodyExpr.TrySyntax,
                            fun _ ->
                                E.Sequential(BoundSyntaxInfo.Generated(expr.Syntax.Tree),
                                    lazyLambdaBodyExpr.Expression,
                                    E.Unit(BoundSyntaxInfo.Generated(expr.Syntax.Tree)),
                                    BoundSequentialSemantic.NormalSequential
                                )
                        )
                    )
                | _ ->
                    // Create a lambda wrapper
                    createLocalDeclarationExpression
                        expr
                        (fun syntaxInfo local -> 
                            let pars =
                                local.Type.FunctionArgumentTypes
                                |> ImArray.map (fun parTy ->
                                    createLocalParameterValue(ImArray.empty, System.String.Empty, parTy, false)
                                )
                            let argExprs =
                                pars
                                |> ImArray.map (fun x -> E.CreateValue(expr.Syntax.Tree, x))
                            E.CreateLambda(BoundSyntaxInfo.Generated(expr.Syntax.Tree),
                                LambdaFlags.None,
                                ImArray.empty,
                                pars,
                                LazyExpression.CreateNonLazy(None, 
                                    fun _ ->
                                        E.Sequential(BoundSyntaxInfo.Generated(expr.Syntax.Tree),
                                            E.Call(syntaxInfo, None, ImArray.empty, argExprs, local, CallFlags.None),
                                            E.Unit(BoundSyntaxInfo.Generated(expr.Syntax.Tree)),
                                            BoundSequentialSemantic.NormalSequential
                                        )
                                )
                            )
                        )
                    |> fst
            else
                expr
        | _ ->
            expr
    else
        expr

let private tryImplicitArguments hasStrictInference (parTys: TypeSymbol imarray) (argExprs: E imarray) =
    if (not parTys.IsEmpty) && parTys.Length = argExprs.Length then

        // We lazily create a new argument expression array when it is needed.
        let mutable newArgExprs = Unchecked.defaultof<E imarrayb>

        let setNewArgExpr i newArgExpr =
            if newArgExprs = Unchecked.defaultof<_> then
                newArgExprs <- ImArray.builderWithSize argExprs.Length
                newArgExprs.AddRange(argExprs)

            newArgExprs[i] <- newArgExpr  

        for i = 0 to argExprs.Length - 1 do
            let argExpr = argExprs[i]
            let parTy = parTys[i]
            if parTy.IsScopedFunction then                          
                match argExpr with
                | E.Lambda(syntaxInfo, lambdaFlags, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr, _, _, _) when not(lambdaFlags.HasFlag(LambdaFlags.Scoped)) ->
                    let newArgExpr = E.CreateLambda(syntaxInfo, lambdaFlags ||| LambdaFlags.Scoped, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr)
                    setNewArgExpr i newArgExpr                           
                | _ ->
                    ()
            else
                let newArgExpr = tryMorphArgumentImplicit hasStrictInference parTy argExpr
                if newArgExpr <> argExpr then
                    setNewArgExpr i newArgExpr

        if newArgExprs = Unchecked.defaultof<_> then
            ValueNone
        else
            newArgExprs.MoveToImmutable()
            |> ValueSome
    else
        ValueNone

let ImplicitPassingArgumentsForOverloading (funcs: IFunctionSymbol imarray) (argTys: TypeSymbol imarray) =
    // Implicits for Enums
    if hasAllEnumTypes argTys then
        let implicitFuncs =
            funcs
            |> ImArray.filter (fun func -> 
                if argTys.IsEmpty then false
                else isIntrinsicForEnum func
            )
        if not implicitFuncs.IsEmpty then
            let argTys =
                argTys
                |> ImArray.map (fun x ->
                    match x.TryEntity with
                    | ValueSome(ent) when ent.IsEnum ->
                        ent.UnderlyingTypeOfEnumOrNewtype
                    | _ ->
                        x
                )
            implicitFuncs, argTys
        else
            funcs, argTys
    else
        funcs, argTys

let ImplicitArgumentsForFunctionType (funcTy: TypeSymbol) (argExprs: E imarray) =
    OlyAssert.True(funcTy.IsAnyFunction)
    match tryImplicitArguments false funcTy.FunctionArgumentTypes argExprs with
    | ValueSome(newArgExprs) -> newArgExprs
    | _ -> argExprs

let ImplicitArgumentsForFunction (benv: BoundEnvironment) (func: IFunctionSymbol) (argExprs: E imarray) =
    // Implicits for Enums
    if isIntrinsicForEnum func && isFirstExpressionAnEnumType argExprs then
        let argTys = func.Parameters |> ImArray.map (fun x -> x.Type)
        if (not argTys.IsEmpty) && argTys.Length = argExprs.Length then

#if DEBUG || CHECKED
            match func with
            | :? FunctionGroupSymbol ->
                OlyAssert.Fail("Unexpected function group.")
            | _ ->
                OlyAssert.True(func.TryWellKnownFunction.IsSome)
                OlyAssert.True(func.TypeParameters.IsEmpty)
                OlyAssert.True(func.TypeArguments.IsEmpty)
                OlyAssert.True(func.IsStatic)
                OlyAssert.False(func.IsConstructor)
                OlyAssert.True(isIntrinsicForEnum func)
#endif

            let alteredFunc =
                // TODO: An altered func could be altered again. We should prevent this with a flag on the function?
                alterParameterAndReturnTypesForEnumOperations func argExprs[0].Type

            Some(alteredFunc), argExprs
        else
            None, argExprs
    else
        if func.IsFunctionGroup then
            None, argExprs
        else
            match tryImplicitArguments func.HasStrictInference func.LogicalType.FunctionArgumentTypes argExprs with
            | ValueSome(newArgExprs) -> None, newArgExprs
            | _ -> None, argExprs

let ImplicitCallExpression (_benv: BoundEnvironment) (expr: E) =
    match expr with
    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, flags) 
            when not value.IsFunctionGroup && 
                 value.Type.IsAnyFunction && 
                 value.LogicalType.FunctionParameterCount = argExprs.Length ->
        match tryImplicitArguments value.HasStrictInference value.LogicalType.FunctionArgumentTypes argExprs with
        | ValueSome(newArgExprs) ->
            E.Call(syntaxInfo, receiverExprOpt, witnessArgs, newArgExprs, value, flags)
        | _ ->
            expr
    | _ ->
        expr

let ImplicitReturn (expectedTyOpt: TypeSymbol option) (expr: E) =
    let expr = tryMorphPartialCall expr
    match expectedTyOpt with
    | Some(expectedTy) ->
        tryMorphSimpleImplicit expectedTy expr
    | _ ->
        expr