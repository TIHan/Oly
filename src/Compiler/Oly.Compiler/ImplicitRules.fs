module internal rec Oly.Compiler.Internal.ImplicitRules

open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.BoundTreePatterns

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

let private hasAllExpressionEnumTypes (exprs: E imarray) =
    exprs |> ImArray.forall (fun x -> x.Type.IsEnum)

let private alterReturnType (func: IFunctionSymbol) (returnTy: TypeSymbol) =
    match func with
    | :? FunctionGroupSymbol ->
        OlyAssert.Fail("Unexpected function group.")
    | _ ->
        OlyAssert.True(func.TryWellKnownFunction.IsSome)
        OlyAssert.True(func.TypeParameters.IsEmpty)
        OlyAssert.True(func.TypeArguments.IsEmpty)

        let id = newId()

        let funcTy =
            match stripTypeEquations func.Type with
            | TypeSymbol.Function(inputTy, _, kind) ->
                TypeSymbol.Function(inputTy, returnTy, kind)
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
    
                member _.Formal = func.Formal
    
                member _.Parameters = func.Parameters
    
                member _.ReturnType = returnTy
    
                member _.Attributes = func.Attributes
    
                member _.ValueFlags = func.ValueFlags

                member _.IsThis = func.IsThis
                        
                member _.IsBase = func.IsBase

                member _.Semantic = func.Semantic

                member _.WellKnownFunction = func.WellKnownFunction

                member _.AssociatedFormalPattern = func.AssociatedFormalPattern

            }

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
                    | ValueSome(ent) when ent.RuntimeType.IsSome ->
                        ent.RuntimeType.Value
                    | _ ->
                        x
                )
            implicitFuncs, argTys
        else
            funcs, argTys
    else
        funcs, argTys

let ImplicitPassingArguments (benv: BoundEnvironment) (func: IFunctionSymbol) (argExprs: E imarray) =
    // Implicits for Enums
    if isIntrinsicForEnum func && hasAllExpressionEnumTypes argExprs then
        let argTys = func.Parameters |> ImArray.map (fun x -> x.Type)
        if (not argTys.IsEmpty) && argTys.Length = argExprs.Length then
            let func =
                if isIntrinsicBitwiseForEnum func then
                    // Alter return type.
                    alterReturnType func argExprs[0].Type
                else
                    func

            let argExprs =
                (argExprs, argTys)
                ||> ImArray.map2 (fun argExpr argTy ->
                    // REVIEW: Should we alter the parameter types, like the return type, of the function instead of doing an implicit cast?
                    Oly.Compiler.Internal.WellKnownExpressions.ImplicitCast benv argExpr argTy
                )

            func, argExprs
        else
            func, argExprs
    else
        if func.IsFunctionGroup then
            func, argExprs
        else
            let pars = func.LogicalParameters
            if (not pars.IsEmpty) && pars.Length = argExprs.Length then

                // We lazily create a new argument expression array when it is needed.
                let mutable newArgExprs = Unchecked.defaultof<E imarrayb>

                for i = 0 to argExprs.Length - 1 do
                    let argExpr = argExprs[i]
                    let par = pars[i]
                    if par.Type.IsScopedFunction then                          
                        match argExpr with
                        | E.Lambda(syntaxInfo, lambdaFlags, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr, _, _, _) when not(lambdaFlags.HasFlag(LambdaFlags.Scoped)) ->
                            let newArgExpr = E.CreateLambda(syntaxInfo, lambdaFlags ||| LambdaFlags.Scoped, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr)

                            if newArgExprs = Unchecked.defaultof<_> then
                                newArgExprs <- ImArray.builderWithSize argExprs.Length
                                newArgExprs.AddRange(argExprs)

                            newArgExprs[i] <- newArgExpr                              
                        | _ ->
                            ()

                if newArgExprs = Unchecked.defaultof<_> then
                    func, argExprs
                else
                    func, newArgExprs.MoveToImmutable()
            else
                func, argExprs