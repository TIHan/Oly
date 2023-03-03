[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Checking

open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.WellKnownExpressions
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.FunctionOverloading
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.PrettyPrint

let checkSyntaxBindingDeclaration (cenv: cenv) (valueExplicitness: ValueExplicitness) (syntaxBindingDecl: OlySyntaxBindingDeclaration) =
    if not valueExplicitness.IsExplicitLet && not syntaxBindingDecl.IsExplicitNew && not syntaxBindingDecl.IsExplicitGet && not syntaxBindingDecl.IsExplicitSet then
        if not syntaxBindingDecl.HasReturnTypeAnnotation && not cenv.syntaxTree.HasErrors then
            if syntaxBindingDecl.IsExplicitFunction then
                cenv.diagnostics.Error(sprintf "The function declaration '%s' must have an explicit return type annotation." syntaxBindingDecl.Identifier.ValueText, 5, syntaxBindingDecl.Identifier)
            else
                cenv.diagnostics.Error(sprintf "The member declaration '%s' must have an explicit type annotation." syntaxBindingDecl.Identifier.ValueText, 5, syntaxBindingDecl.Identifier)

let checkSyntaxDeclarationBinding (cenv: cenv) (enclosing: EnclosingSymbol) memberFlags (valueExplicitness: ValueExplicitness) (syntaxBinding: OlySyntaxBinding) =        
    (* BEGIN CHECK DEFAULT MODIFIER *)
    if valueExplicitness.IsExplicitDefault then
        if not enclosing.IsInterface && not enclosing.IsClass then
            cenv.diagnostics.Error("A non-abstract by default member does not need to have a 'default' modifier.", 10, syntaxBinding.Declaration.Identifier)

    if not valueExplicitness.IsExplicitDefault && (memberFlags &&& MemberFlags.Abstract = MemberFlags.Abstract) then
        cenv.diagnostics.Error("An abstract member with an implementation must have a 'default' modifier.", 10, syntaxBinding.Declaration.Identifier)
    (* END CHECK DEFAULT MODIFIER *)

    match enclosing with
    | EnclosingSymbol.Entity(ent) ->
        if ent.IsShape then
            cenv.diagnostics.Error("Shapes cannot have members with implementations.", 10, syntaxBinding.Declaration.Identifier)
        elif ent.IsImported then
            cenv.diagnostics.Error("Imported types cannot have members with implementations.", 10, syntaxBinding.Declaration.Identifier)
    | _ ->
        ()

let checkTypeParameterCount (cenv: cenv) syntaxNode expectedTyParCount tyParCount =
    if expectedTyParCount <> tyParCount then
        cenv.diagnostics.Error(sprintf "Expected '%i' type argument(s) but got '%i'." expectedTyParCount tyParCount, 0, syntaxNode)

let checkBindingSignature (cenv: cenv) attrs (enclosing: EnclosingSymbol) (bindingInfo: BindingInfoSymbol) memberFlags (valueExplicitness: ValueExplicitness) (syntaxBindingDecl: OlySyntaxBindingDeclaration) =
    let mutable hasErrors = false

    let mustHaveImpl =
        if bindingInfo.Value.IsFunction then
            match enclosing with
            | EnclosingSymbol.Entity(ent) ->
                not ent.IsShape && 
                not (memberFlags &&& MemberFlags.Abstract = MemberFlags.Abstract) && 
                not (attributesContainImport attrs) && 
                not (attributesContainIntrinsic attrs)
            | _ ->
                true
        else
            false

    if bindingInfo.Value.IsFunction then
        if mustHaveImpl then
            cenv.diagnostics.Error(sprintf "The function '%s' must have an implementation." bindingInfo.Value.Name, 10, syntaxBindingDecl.Identifier)
            hasErrors <- true

    elif bindingInfo.Value.IsField then
        if valueExplicitness.IsExplicitDefault then
            cenv.diagnostics.Error("Fields cannot be marked with 'default'.", 10, syntaxBindingDecl.Identifier)
            hasErrors <- true

        if not bindingInfo.Value.IsInstance then
            match enclosing with
            | EnclosingSymbol.Entity(ent) when ent.IsShape ->
                cenv.diagnostics.Error("Shapes cannot have static fields.", 10, syntaxBindingDecl.Identifier)
                hasErrors <- true
            | _ ->
                ()

    if valueExplicitness.IsExplicitOverrides then
        cenv.diagnostics.Error("'overrides' cannot be used in a context where there is no implementation. Remove 'overrides'.", 10, syntaxBindingDecl.Identifier)
        hasErrors <- true

    if hasErrors then
        let ty = bindingInfo.Type
        let tyParOpt = match ty.TryTypeParameter with ValueSome tyPar -> Some tyPar | _ -> None
        // We do unification with the binding's type and an error type as to prevent a cascade of errors.
        match ty.TryFunction with
        | ValueSome(argTys, returnTy) ->
            argTys
            |> ImArray.iter (fun x ->
                UnifyTypes TypeVariableRigidity.Flexible x (TypeSymbol.Error(tyParOpt))
                |> ignore
            )
            UnifyTypes TypeVariableRigidity.Flexible returnTy (TypeSymbol.Error(tyParOpt))
            |> ignore
        | _ ->
            UnifyTypes TypeVariableRigidity.Flexible ty (TypeSymbol.Error(tyParOpt))
            |> ignore

    not hasErrors

let checkEnumForInvalidFieldOrFunction (cenv: cenv) syntaxNode (binding: BindingInfoSymbol) =
    if binding.Value.Enclosing.IsEnum then
        cenv.diagnostics.Error("Value declaration not valid on an 'enum' type.", 10, syntaxNode)

let private checkUsageTypeExport cenv env syntaxNode (name: string) (ty: TypeSymbol) =
    // Do not strip type equations here.
    match ty with
    | TypeSymbol.Entity(ent) ->
        if not ent.IsExported && not ent.IsImported then
            cenv.diagnostics.Error($"'{name}' cannot be exported as its usage of type '{ent.Name}' is not imported or exported.", 10, syntaxNode)
        ent.TypeArguments
        |> ImArray.iter (checkUsageTypeExport cenv env syntaxNode name)
    | _ ->
        ty.TypeArguments
        |> ImArray.iter (checkUsageTypeExport cenv env syntaxNode name)

let private checkTypeParameterExport cenv env syntaxNode (name: string) (tyPar: TypeParameterSymbol) =
    tyPar.Constraints
    |> ImArray.iter (fun x ->
        match x.TryGetSubtypeOf() with
        | ValueSome constrTy ->
            checkUsageTypeExport cenv env syntaxNode name constrTy
        | _ ->
            ()
    )

let checkEntityExport cenv env syntaxNode (ent: IEntitySymbol) =
    if ent.IsExported && ent.IsImported then
        cenv.diagnostics.Error($"'{ent.Name}' cannot be imported and exported at the same time.", 10, syntaxNode)
    else
        if ent.IsExported then
            if not ent.Enclosing.IsNamespace && not ent.Enclosing.IsExported then
                cenv.diagnostics.Error($"'{ent.Name}' cannot be exported as its enclosing is not exported.", 10, syntaxNode)

            ent.TypeParameters
            |> ImArray.iter (fun tyPar ->
                checkTypeParameterExport cenv env syntaxNode ent.Name tyPar
            )

            ent.Implements 
            |> ImArray.iter (fun x -> 
                if not x.IsExported && not x.IsImported then
                    cenv.diagnostics.Error($"'{ent.Name}' cannot be exported as its implementation for '{x.Name}' is not imported or exported.", 10, syntaxNode)
                x.TypeArguments
                |> ImArray.iter (fun tyArg ->
                    checkUsageTypeExport cenv env syntaxNode ent.Name tyArg
                )
            )

            ent.Extends
            |> ImArray.iter (fun x -> 
                if not x.IsExported && not x.IsImported then
                    cenv.diagnostics.Error($"'{ent.Name}' cannot be exported as its inheritance of '{x.Name}' is not imported or exported.", 10, syntaxNode)
                x.TypeArguments
                |> ImArray.iter (fun tyArg ->
                    checkUsageTypeExport cenv env syntaxNode ent.Name tyArg
                )
            )

let checkValueExport cenv env syntaxNode (value: IValueSymbol) =
    if value.IsImported && value.IsExported then
        cenv.diagnostics.Error($"'{value.Name}' cannot be imported and exported at the same time.", 10, syntaxNode)
    else
        if value.IsExported then
            if not value.Enclosing.IsExported then
                cenv.diagnostics.Error($"'{value.Name}' cannot be exported as its enclosing is not exported.", 10, syntaxNode)

            value.TypeParameters
            |> ImArray.iter (fun tyPar ->
                checkTypeParameterExport cenv env syntaxNode value.Name tyPar
            )

            checkUsageTypeExport cenv env syntaxNode value.Name value.Type

        if value.IsImported then
            if value.IsInstance && not value.Enclosing.IsImported then
                cenv.diagnostics.Error($"'{value.Name}' cannot be imported as its enclosing is not imported.", 10, syntaxNode)

let private autoDereferenceExpression expr =
    match expr with
    | BoundExpression.Call(value=value) ->
        if value.IsAddressOf then
            expr
        else
            AutoDereferenceIfPossible expr
    | BoundExpression.Value _ ->
        AutoDereferenceIfPossible expr
    | _ ->
        expr

let private filterByRefReturnTypes (argExprs: BoundExpression imarray) (funcs: IFunctionSymbol imarray) =
    if funcs.Length <= 1 then funcs
    else

    funcs
    |> ImArray.filter (fun x ->
        if x.IsAddressOf then
            let argExpr = argExprs[0]
            match argExpr with
            | BoundExpression.Value(value=value) when value.IsLocal ->
                if value.IsMutable && x.ReturnType.IsReadWriteByRef then
                    true
                elif not value.IsMutable && x.ReturnType.IsReadOnlyByRef then
                    true
                else
                    false
            | BoundExpression.GetField(receiver=receiverExpr;field=field) ->
                let isReadOnly =
                    if not field.IsMutable then
                        true
                    else
                        if field.Enclosing.IsAnyStruct then
                            receiverExpr.Type.IsReadOnlyByRef
                        else
                            false
                if isReadOnly && x.ReturnType.IsReadOnlyByRef then
                    true
                elif not isReadOnly && x.ReturnType.IsReadWriteByRef then
                    true
                else
                    false
            | FromAddress(expr) ->
                areTypesEqual expr.Type.Formal x.ReturnType.Formal
            | BoundExpression.Call(value=value) ->
                match value.TryWellKnownFunction with
                | ValueSome(WellKnownFunction.GetArrayElement) ->
                    let par = (value :?> IFunctionSymbol).Parameters[0]
                    if par.Type.IsReadOnly && x.ReturnType.IsReadOnlyByRef then
                        true
                    elif not par.Type.IsReadOnly && x.ReturnType.IsReadWriteByRef then
                        true
                    else
                        false
                | _ ->
                    false
            | _ ->
                false
        else
            false
    )

let private tryOverloadResolution
        (env: BinderEnvironment) 
        (expectedReturnTyOpt: TypeSymbol option) 
        (resArgs: ResolutionArguments)
        (isArgForAddrOf: bool)
        (funcs: IFunctionSymbol imarray) =

    if funcs.IsEmpty then None
    elif funcs.Length = 1 then funcs |> Some
    else

    match expectedReturnTyOpt with
    | Some expectedTy when expectedTy.IsError_t -> None
    | _ ->

    let filteredFuncs =
        let funcs =
            if isArgForAddrOf then
                funcs
                |> ImArray.filter (fun x -> x.ReturnType.IsByRef_t)
            else
                funcs

        funcs
        |> filterFunctionsForOverloadingPart2 resArgs expectedReturnTyOpt

    if filteredFuncs.IsEmpty then
        None
    else
        filteredFuncs |> Some

let private tryOverloadedCallExpression 
        (cenv: cenv) 
        (env: BinderEnvironment) 
        (expectedTyOpt: TypeSymbol option) 
        (syntaxInfo: BoundSyntaxInfo) 
        (syntaxNameOpt: OlySyntaxName option)
        (receiverExprOpt: BoundExpression option)
        (argExprs: BoundExpression imarray)
        (isArgForAddrOf: bool)
        (funcs: IFunctionSymbol imarray) =

    let resArgs = 
        argExprs
        |> ImArray.map (fun x ->
            match x with
            | BoundExpression.Lambda(body=body) ->
                // We must evaluate the lambda at this point.
                if not body.HasExpression then
                    body.Run()
            | _ ->
                ()
            x.Type
        )
        |> ResolutionArguments.ByType
    match tryOverloadResolution env expectedTyOpt resArgs isArgForAddrOf funcs with
    | None -> None
    | Some funcs ->
        let funcs = filterByRefReturnTypes argExprs funcs
        if funcs.IsEmpty then
            None
        else
            let func = FunctionGroupSymbol.CreateIfPossible(funcs)
            bindValueAsCallExpressionWithOptionalSyntaxName cenv env syntaxInfo receiverExprOpt argExprs (func, syntaxNameOpt)
            |> Some

let private createPartialCallExpression (cenv: cenv) (env: BinderEnvironment) syntaxNode syntaxNameOpt (tyArgs: _ imarray) (func: IFunctionSymbol) =
    let freshFunc = freshenValue env.benv (func.Substitute(tyArgs)) :?> IFunctionSymbol
    
    let lambdaPars =
        freshFunc.LogicalParameters
        |> ROMem.toImArray
    
    let argExprs =
        lambdaPars
        |> ImArray.map (fun x -> BoundExpression.CreateValue(cenv.syntaxTree, x))
    
    let callExpr =
        BoundExpression.Call(
            BoundSyntaxInfo.User(syntaxNode, env.benv),
            None,
            (CacheValueWithArg.FromValue ImArray.empty),
            argExprs,
            syntaxNameOpt,
            freshFunc,
            func.IsVirtual && not func.IsFinal
        )
    
    let lambdaExpr =
        BoundExpression.CreateLambda(
            cenv.syntaxTree,
            LambdaFlags.None,
            ImArray.empty,
            lambdaPars,
            (LazyExpression(None, fun _ -> callExpr))
        )
    
    checkImmediateExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv)) env.isReturnable lambdaExpr
    
    lambdaExpr

let private createPartialCallExpressionWithSyntaxTypeArguments (cenv: cenv) (env: BinderEnvironment) syntaxNode syntaxNameOpt (syntaxTyArgsRoot, syntaxTyArgs) (func: IFunctionSymbol) =
    let tyArgs = bindTypeArguments cenv env 0 func.TypeParametersOrConstructorEnclosingTypeParameters (syntaxTyArgsRoot, syntaxTyArgs)
    createPartialCallExpression cenv env syntaxNode syntaxNameOpt tyArgs func

let private tryOverloadPartialCallExpression
        (cenv: cenv) 
        (env: BinderEnvironment) 
        (expectedTyOpt: TypeSymbol option) 
        (syntaxInfo: BoundSyntaxInfo) 
        (syntaxNameOpt: OlySyntaxName option)
        (funcs: IFunctionSymbol imarray) =

    let resArgs =
        match expectedTyOpt with
        | Some(expectedTy) when expectedTy.IsFunction_t ->
            ResolutionArguments.ByFunctionType(expectedTy)
        | _ ->
            ResolutionArguments.Any

    match tryOverloadResolution env None resArgs false funcs with
    | None -> None
    | Some funcs ->
        if funcs.IsEmpty then
            None
        else
            let func = FunctionGroupSymbol.CreateIfPossible(funcs)
            match syntaxNameOpt with
            | Some(OlySyntaxName.Generic(_, syntaxTyArgs)) ->
                createPartialCallExpressionWithSyntaxTypeArguments cenv env syntaxInfo.Syntax syntaxNameOpt (syntaxTyArgs, syntaxTyArgs.Values) func
                |> Some
            | _ -> 
                createPartialCallExpression cenv env syntaxInfo.Syntax syntaxNameOpt ImArray.empty func
                |> Some

let private checkCalleeExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (expr: BoundExpression) =
    match expr with
    | BoundExpression.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, syntaxNameOpt, value, isVirtualCall) ->

        let isAddrOf = value.IsAddressOf

        let argExprs =
            if value.IsFunctionGroup then
                if isAddrOf then 
                    let argExpr = argExprs[0]
                    let argExpr = 
                        checkCallExpression cenv env expectedTyOpt true argExpr
                        |> checkExpressionTypes cenv env None
                    ImArray.createOne argExpr
                else
                    argExprs
                    |> ImArray.map (fun argExpr ->
                        checkExpression cenv env None argExpr
                    )
            else
                let argTys = value.Type.FunctionArgumentTypes
                if argTys.Length = argExprs.Length then              
                    (argExprs, argTys)
                    ||> ImArray.mapi2 (fun i argExpr parTy ->
                        match argExpr with
                        | BoundExpression.Call(value=funcGroup) when funcGroup.IsFunctionGroup ->
                            let expectedTy =
                                if isAddrOf then
                                    match value.Type.TryFunction with
                                    | ValueSome(_, outputTy) ->
                                        if outputTy.IsReadOnlyByRef then
                                            TypeSymbol.CreateByRef(parTy, ByRefKind.Read)
                                        else
                                            TypeSymbol.CreateByRef(parTy, ByRefKind.ReadWrite)
                                    | _ ->
                                        parTy
                                else
                                    parTy
                            let newArgExpr = checkExpression cenv env (Some expectedTy) argExpr
                            if newArgExpr = argExpr then
                                argExpr
                            else
                                newArgExpr

                        | BoundExpression.Lambda(syntaxInfo, lambdaFlags, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr, lazyTy, freeLocals, freeTyVars) ->
                            let parsOpt =
                                if value.IsFunction then
                                    (value :?> IFunctionSymbol).Parameters
                                    |> ValueSome
                                else
                                    ValueNone
                            match parsOpt with
                            | ValueSome pars when not(lambdaFlags.HasFlag(LambdaFlags.Inline)) ->
                                if attributesContainInline pars[i].Attributes then
                                    BoundExpression.Lambda(syntaxInfo, lambdaFlags ||| LambdaFlags.Inline, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr, lazyTy, freeLocals, freeTyVars)
                                else
                                    argExpr
                            | _ ->
                                argExpr

                        | _ ->
                            argExpr
                    )
                else
                    // Not enough arguments, but we check for this elsewhere.
                    // REVIEW: Maybe we should actually check it here...
                    argExprs

        if isAddrOf then
            let argExpr = argExprs[0]
            match argExpr with
            | AutoDereferenced _ -> ()
            | BoundExpression.Value _ -> ()
            | BoundExpression.GetField _ -> ()
            | BoundExpression.Call(value=value) ->
                match value.TryWellKnownFunction with
                | ValueSome(WellKnownFunction.GetArrayElement) -> ()
                | _ ->
                    let argExprTy = argExpr.Type
                    if argExprTy.IsByRef_t || argExprTy.IsError_t then ()
                    else
                        cenv.diagnostics.Error("Invalid address of.", 10, syntaxInfo.Syntax)
            | _ ->
                let argExprTy = argExpr.Type
                if argExprTy.IsByRef_t || argExprTy.IsError_t then ()
                else
                    cenv.diagnostics.Error("Invalid address of.", 10, syntaxInfo.Syntax)

        BoundExpression.Call(
            syntaxInfo,
            receiverExprOpt,
            witnessArgs,
            argExprs,
            syntaxNameOpt,
            value,
            isVirtualCall
        )
    | _ ->
        expr

let private checkCallerExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (isArgForAddrOf: bool) (expr: BoundExpression) =
    match expr with
    | BoundExpression.Value(syntaxInfo, value) when value.IsFunction ->
        let syntaxNameOpt =
            match syntaxInfo.Syntax with
            | :? OlySyntaxName as syntaxName -> Some syntaxName
            | _ -> None

        match expectedTyOpt with
        | Some _ ->
            match value with
            | :? FunctionGroupSymbol as funcGroup ->
                match tryOverloadPartialCallExpression cenv env expectedTyOpt syntaxInfo syntaxNameOpt funcGroup.Functions with
                | Some expr -> expr
                | _ -> expr
            | :? IFunctionSymbol as func ->
                match tryOverloadPartialCallExpression cenv env expectedTyOpt syntaxInfo syntaxNameOpt (ImArray.createOne func) with
                | Some expr -> expr
                | _ -> expr
            | _ ->
                expr
        | _ ->
            match value with
            | :? IFunctionSymbol as func when not func.IsFunctionGroup ->
                match tryOverloadPartialCallExpression cenv env None syntaxInfo syntaxNameOpt (ImArray.createOne func) with
                | Some expr -> expr
                | _ -> expr
            | _ ->
                expr

    | BoundExpression.Witness(bodyExpr, witnessTy, exprTy) ->
        let newBodyExpr = checkExpression cenv env None bodyExpr
        if newBodyExpr = bodyExpr then
            expr
        else
            BoundExpression.Witness(newBodyExpr, witnessTy, exprTy)

    | BoundExpression.Call(syntaxInfo, receiverExprOpt, _, argExprs, syntaxNameOpt, value, _) ->
        match value with
        | :? FunctionGroupSymbol as funcGroup ->
            if funcGroup.IsAddressOf then
                match expectedTyOpt with
                | Some expectedTy when not expectedTy.IsSolved ->
                    match argExprs[0] with
                    | AutoDereferenced(argExpr) ->
                        UnifyTypes Flexible expectedTy argExpr.Type
                        |> ignore
                    | _ ->
                        ()
                | _ ->
                    ()
                    
            match tryOverloadedCallExpression cenv env expectedTyOpt syntaxInfo syntaxNameOpt receiverExprOpt argExprs isArgForAddrOf funcGroup.Functions with
            | Some expr -> expr
            | _ -> expr
        | _ ->
            expr
    | _ ->
        expr

let private lateCheckCalleeExpression cenv env expr =
    match expr with
    | LoadFunctionPtr(syntaxInfo, _, funcLoadFunctionPtr, argExpr) ->
        match argExpr with
        | LambdaWrappedFunctionCall(syntaxInfo, func) ->
            match func.Type.TryFunction with
            | ValueSome(inputTy, outputTy) ->
                // TODO: This is weird, all because this is to satisfy __oly_load_function_ptr type arguments.
                //       Perhaps we should just change __oly_load_function_ptr to simply have 1 type argument be the return type.
                let inputTyWithoutInstance =
                    if func.IsInstance then
                        inputTy.RemoveAt(0)
                    else
                        inputTy
                let expectedFuncTy = TypeSymbol.Function(inputTyWithoutInstance, outputTy)
                checkTypes
                    (SolverEnvironment.Create(cenv.diagnostics, env.benv)) 
                    syntaxInfo.Syntax 
                    expectedFuncTy
                    funcLoadFunctionPtr.Parameters[0].Type

                let ilCallConv =
                    if func.IsBlittable then
                        Oly.Metadata.OlyILCallingConvention.Blittable
                    else
                        Oly.Metadata.OlyILCallingConvention.Default

                let expectedReturnTy = TypeSymbol.NativeFunctionPtr(ilCallConv, inputTy, outputTy)
                checkTypes
                    (SolverEnvironment.Create(cenv.diagnostics, env.benv)) 
                    syntaxInfo.Syntax 
                    expectedReturnTy 
                    funcLoadFunctionPtr.ReturnType

                if func.AllTypeParameterCount > 0 then
                    cenv.diagnostics.Error("Getting the address of a function requires the function not be generic or enclosed by a generic type.", 10, syntaxInfo.Syntax)
            | _ ->
                cenv.diagnostics.Error("Invalid use of 'LoadFunctionPtr'.", 10, syntaxInfo.Syntax)
        | _ ->
            cenv.diagnostics.Error("Invalid use of 'LoadFunctionPtr'.", 10, syntaxInfo.Syntax) 
    | _ ->
        ()

    // TODO: Ideally we should not do these checks based on syntax after its bound.
    //       We need to have access to the original ResolutionInfo at the time this was bound.
    //       The best way to do that is to store the ResolutionInfo *optionally* on the Call expression itself.
    match expr with
    | E.Call(syntaxInfo=syntaxInfo;value=value) when value.IsParameterLessFunction || value.RequiresExplicitTypeArguments ->
        match syntaxInfo.Syntax with
        | :? OlySyntaxExpression as syntaxExpr ->
            match syntaxExpr with
            | OlySyntaxExpression.Call(OlySyntaxExpression.Name(syntaxName), _) ->
                if value.IsParameterLessFunction then
                    cenv.diagnostics.Error($"'{value.Name}' is parameter-less which requires not to be explicit with '()'.", 10, syntaxInfo.Syntax)

                if value.RequiresExplicitTypeArguments then
                    let resTyArity = typeResolutionArityOfName syntaxName
                    if resTyArity.IsAny_t then
                        cenv.diagnostics.Error($"'{value.Name}' requires explicit type arguments.", 10, syntaxInfo.Syntax)
            | _ ->
                ()
        | _ ->
            ()
    | _ ->
        ()

    autoDereferenceExpression expr

let private checkCallExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (isArgForAddrOf: bool) (expr: BoundExpression) =
    checkCalleeExpression cenv env None expr
    |> checkCallerExpression cenv env expectedTyOpt isArgForAddrOf
    |> checkCalleeExpression cenv env expectedTyOpt
    |> lateCheckCalleeExpression cenv env

let private checkExpressionTypes (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) expr =
    let recheckExpectedTy =
        match expectedTyOpt with
        // While a type variable is technically "solved", we do not want to do this check early
        // because the return type could be a byref.
        | Some expectedTy when expectedTy.IsSolved && not expectedTy.IsTypeVariable && not env.isReturnable ->
            checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv)) expectedTy expr
            false
        | _ ->
            true

    match expr with
    | AutoDereferenced bodyExpr ->
        checkImmediateExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv)) env.isReturnable bodyExpr
    | _ ->
        checkImmediateExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv)) env.isReturnable expr

    let expr = autoDereferenceExpression expr

    if recheckExpectedTy then
        match expectedTyOpt with
        | Some expectedTy ->
            checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv)) expectedTy expr
        | _ ->
            ()

    autoDereferenceExpression expr

let checkExpression (cenv: cenv) (env: BinderEnvironment) expectedTyOpt (expr: BoundExpression) =
    checkCallExpression cenv env expectedTyOpt false expr
    |> checkExpressionTypes cenv env expectedTyOpt
