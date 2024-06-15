module internal rec Oly.Compiler.Internal.ImplicitRules

open Oly.Core
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
                    | ValueSome(ent) when ent.TryEnumUnderlyingType.IsSome ->
                        ent.TryEnumUnderlyingType.Value
                    | _ ->
                        x
                )
            implicitFuncs, argTys
        else
            funcs, argTys
    else
        funcs, argTys

let private tryImplicitArguments (parTys: TypeSymbol imarray) (argExprs: E imarray) =
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
                let newArgExpr = tryMorphSimpleImplicit parTy argExpr
                if newArgExpr <> argExpr then
                    setNewArgExpr i newArgExpr

        if newArgExprs = Unchecked.defaultof<_> then
            ValueNone
        else
            newArgExprs.MoveToImmutable()
            |> ValueSome
    else
        ValueNone

let ImplicitArgumentsForFunctionType (funcTy: TypeSymbol) (argExprs: E imarray) =
    OlyAssert.True(funcTy.IsFunction_t)
    match tryImplicitArguments funcTy.FunctionArgumentTypes argExprs with
    | ValueSome(newArgExprs) -> newArgExprs
    | _ -> argExprs

let ImplicitArgumentsForFunction (benv: BoundEnvironment) (func: IFunctionSymbol) (argExprs: E imarray) =
    // Implicits for Enums
    if isIntrinsicForEnum func && isFirstExpressionAnEnumType argExprs then
        let argTys = func.Parameters |> ImArray.map (fun x -> x.Type)
        if (not argTys.IsEmpty) && argTys.Length = argExprs.Length then

            let principalTy = argExprs[0].Type
            let func =
                alterParameterAndReturnTypesForEnumOperations
                    func
                    principalTy

            func, argExprs
        else
            func, argExprs
    else
        if func.IsFunctionGroup then
            func, argExprs
        else
            match tryImplicitArguments func.LogicalType.FunctionArgumentTypes argExprs with
            | ValueSome(newArgExprs) -> func, newArgExprs
            | _ -> func, argExprs

let ImplicitCallExpression (_benv: BoundEnvironment) (expr: E) =
    match expr with
    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, flags) 
            when not value.IsFunctionGroup && 
                 value.Type.IsFunction_t && 
                 value.LogicalType.FunctionParameterCount = argExprs.Length ->
        match tryImplicitArguments value.LogicalType.FunctionArgumentTypes argExprs with
        | ValueSome(newArgExprs) ->
            E.Call(syntaxInfo, receiverExprOpt, witnessArgs, newArgExprs, value, flags)
        | _ ->
            expr
    | _ ->
        expr

let ImplicitReturn (expectedTyOpt: TypeSymbol option) (expr: E) =
    match expectedTyOpt with
    | Some(expectedTy) ->
        tryMorphSimpleImplicit expectedTy expr
    | _ ->
        expr