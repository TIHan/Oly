[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Checking

open Oly.Core
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

let checkBindingSignature (cenv: cenv) attrs (enclosing: EnclosingSymbol) (bindingInfo: BindingInfoSymbol) memberFlags (valueExplicitness: ValueExplicitness) mustHaveImpl (syntaxBindingDecl: OlySyntaxBindingDeclaration) =
    let mutable hasErrors = false

    let mustHaveImpl =
        if mustHaveImpl then
            let mustHaveImpl = 
                (not (memberFlags &&& MemberFlags.Abstract = MemberFlags.Abstract)) ||
                (memberFlags &&& MemberFlags.Sealed = MemberFlags.Sealed)

            if mustHaveImpl && bindingInfo.Value.IsFunction then
                match enclosing with
                | EnclosingSymbol.Entity(ent) ->
                    (not ent.IsShape && (ent.IsInterface || (not ent.IsAbstract || ent.IsSealed))) &&
                    not (attributesContainImport attrs) && 
                    not (attributesContainIntrinsic attrs)
                | _ ->
                    true
            else
                false
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

    if valueExplicitness.IsExplicitOverrides && mustHaveImpl then
        cenv.diagnostics.Error("'overrides' cannot be used in a context where there is no implementation. Remove 'overrides'.", 10, syntaxBindingDecl.Identifier)
        hasErrors <- true

    if hasErrors then
        let ty = bindingInfo.Type
        let tyParOpt = match ty.TryTypeParameter with ValueSome tyPar -> Some tyPar | _ -> None
        // We do unification with the binding's type and an error type as to prevent a cascade of errors.
        match ty.TryGetFunctionWithParameters() with
        | ValueSome(argTys, returnTy) ->
            argTys
            |> ImArray.iter (fun x ->
                UnifyTypes TypeVariableRigidity.Flexible x (TypeSymbol.Error(tyParOpt, None))
                |> ignore
            )
            UnifyTypes TypeVariableRigidity.Flexible returnTy (TypeSymbol.Error(tyParOpt, None))
            |> ignore
        | _ ->
            UnifyTypes TypeVariableRigidity.Flexible ty (TypeSymbol.Error(tyParOpt, None))
            |> ignore

    not hasErrors

let checkEnumForInvalidFieldOrFunction (cenv: cenv) syntaxNode (binding: BindingInfoSymbol) =
    if binding.Value.Enclosing.IsEnum && binding.Value.IsInstance then
        cenv.diagnostics.Error("Instance member not valid on an 'enum' type.", 10, syntaxNode)

let private checkUsageTypeExport cenv syntaxNode (name: string) (ty: TypeSymbol) =
    // Do not strip type equations here.
    match ty with
    | TypeSymbol.Entity(ent) ->
        if not ent.IsExported && not ent.IsImported then
            cenv.diagnostics.Error($"'{name}' cannot be exported as its usage of type '{ent.Name}' is not imported or exported.", 10, syntaxNode)
        ent.TypeArguments
        |> ImArray.iter (checkUsageTypeExport cenv syntaxNode name)
    | _ ->
        ty.TypeArguments
        |> ImArray.iter (checkUsageTypeExport cenv syntaxNode name)

let private checkTypeParameterExport cenv syntaxNode (name: string) (tyPar: TypeParameterSymbol) =
    tyPar.Constraints
    |> ImArray.iter (fun x ->
        match x.TryGetSubtypeOf() with
        | ValueSome constrTy ->
            checkUsageTypeExport cenv syntaxNode name constrTy
        | _ ->
            ()
    )

let checkEntityExport cenv env syntaxNode (ent: EntitySymbol) =
    if ent.IsExported && ent.IsImported then
        cenv.diagnostics.Error($"'{ent.Name}' cannot be imported and exported at the same time.", 10, syntaxNode)
    else
        if ent.IsExported then
            if not ent.Enclosing.IsNamespace && not ent.Enclosing.IsExported then
                cenv.diagnostics.Error($"'{ent.Name}' cannot be exported as its enclosing is not exported.", 10, syntaxNode)

            ent.TypeParameters
            |> ImArray.iter (fun tyPar ->
                checkTypeParameterExport cenv syntaxNode ent.Name tyPar
            )

            ent.Implements 
            |> ImArray.iter (fun x -> 
                if not x.IsExported && not x.IsImported then
                    cenv.diagnostics.Error($"'{ent.Name}' cannot be exported as its implementation for '{x.Name}' is not imported or exported.", 10, syntaxNode)
                x.TypeArguments
                |> ImArray.iter (fun tyArg ->
                    checkUsageTypeExport cenv syntaxNode ent.Name tyArg
                )
            )

            ent.Extends
            |> ImArray.iter (fun x -> 
                if not x.IsExported && not x.IsImported && not x.IsBuiltIn then
                    cenv.diagnostics.Error($"'{ent.Name}' cannot be exported as its inheritance of '{x.Name}' is not imported or exported.", 10, syntaxNode)
                x.TypeArguments
                |> ImArray.iter (fun tyArg ->
                    checkUsageTypeExport cenv syntaxNode ent.Name tyArg
                )
            )

let checkValueExport cenv syntaxNode (value: IValueSymbol) =
    if value.IsImported && value.IsExported then
        cenv.diagnostics.Error($"'{value.Name}' cannot be imported and exported at the same time.", 10, syntaxNode)
    else
        if value.IsExported then
            if not value.Enclosing.IsExported then
                cenv.diagnostics.Error($"'{value.Name}' cannot be exported as its enclosing is not exported.", 10, syntaxNode)

            value.TypeParameters
            |> ImArray.iter (fun tyPar ->
                checkTypeParameterExport cenv syntaxNode value.Name tyPar
            )

            checkUsageTypeExport cenv syntaxNode value.Name value.Type

        if value.IsImported then
            if value.IsInstance && not value.Enclosing.IsImported then
                cenv.diagnostics.Error($"'{value.Name}' cannot be imported as its enclosing is not imported.", 10, syntaxNode)

let private autoDereferenceExpression expr =
    match expr with
    | E.Call(value=value) ->
        if value.IsAddressOf then
            expr
        else
            AutoDereferenceIfPossible expr
    | E.Value _ ->
        AutoDereferenceIfPossible expr
    | _ ->
        expr

let private filterByRefReturnTypes (argExprs: E imarray) (funcs: IFunctionSymbol imarray) =
    if funcs.Length <= 1 then funcs
    else

    funcs
    |> ImArray.filter (fun x ->
        if x.IsAddressOf then
            let argExpr = argExprs[0]
            match argExpr with
            | E.Value(value=value) when value.IsLocal ->
                if value.IsMutable && x.ReturnType.IsReadWriteByRef then
                    true
                elif not value.IsMutable && x.ReturnType.IsReadOnlyByRef then
                    true
                else
                    false
            | E.GetField(receiver=receiverExpr;field=field) ->
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
            | E.Call(value=value) ->
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
        skipEager
        (expectedTyOpt: TypeSymbol option) 
        (syntaxInfo: BoundSyntaxInfo) 
        (receiverExprOpt: E option)
        (argExprs: E imarray)
        (isArgForAddrOf: bool)
        (funcs: IFunctionSymbol imarray)
        (flags: CallFlags) =

    let resArgs = 
        argExprs
        |> ImArray.map (fun x -> x.Type)
        |> ResolutionArguments.ByType
    match tryOverloadResolution expectedTyOpt resArgs isArgForAddrOf funcs with
    | None -> None
    | Some funcs ->
        let checkLambdaArguments() =
            let solverEnv = SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)
            argExprs
            |> ImArray.iter (fun x ->
                x.ForEachReturningTargetExpression(fun x ->
                    match x with
                    | E.Lambda _ ->
                        // We must evaluate the lambda at this point.
                        checkImmediateExpression solverEnv false x
                    | _ ->
                        ()
                )
            )
        if funcs.Length = 1 then
            let expr = bindValueAsCallExpressionWithOptionalSyntaxName cenv env syntaxInfo receiverExprOpt (ValueSome argExprs) (funcs[0], syntaxInfo.TrySyntaxName)
            let expr =
                // partial call / partial overloaded call
                if flags.HasFlag(CallFlags.Partial) then
                    match expr with
                    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, flags) ->
                        E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, flags ||| CallFlags.Partial)
                    | expr ->
                        expr
                else
                    expr
            checkLambdaArguments()
            Some expr
        else
            
            let funcs2 = filterFunctionsForOverloadingPart3 skipEager resArgs expectedTyOpt funcs
            let funcs = if funcs2.IsEmpty then funcs else funcs2

            let funcs = filterByRefReturnTypes argExprs funcs
            if funcs.IsEmpty then
                checkLambdaArguments()
                None
            else       
                let func = FunctionGroupSymbol.CreateIfPossible(funcs)
                let expr = bindValueAsCallExpressionWithOptionalSyntaxName cenv env syntaxInfo receiverExprOpt (ValueSome argExprs) (func, syntaxInfo.TrySyntaxName)
                checkLambdaArguments()
                Some expr

let private createPartialCallExpression (cenv: cenv) (env: BinderEnvironment) syntaxNode syntaxNameOpt (tyArgs: _ imarray) (func: IFunctionSymbol) =
    let freshFunc = freshenValue env.benv (func.Substitute(tyArgs)) :?> IFunctionSymbol
    
    let lambdaPars =
        freshFunc.LogicalParameters
        |> ROMem.toImArray
    
    let argExprs =
        lambdaPars
        |> ImArray.map (fun x -> E.CreateValue(cenv.syntaxTree, x))

    let syntaxInfo = BoundSyntaxInfo.User(syntaxNode, env.benv, syntaxNameOpt, None)
    
    let callExpr =
        E.Call(
            syntaxInfo,
            None,
            ImArray.empty,
            argExprs,
            freshFunc,
            if func.IsVirtual && not func.IsFinal then CallFlags.Virtual else CallFlags.None
        )
    
    let lambdaExpr =
        E.CreateLambda(
            syntaxInfo,
            LambdaFlags.None,
            ImArray.empty,
            lambdaPars,
            (LazyExpression.CreateNonLazy(None, fun _ -> callExpr))
        )
    
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

    match tryOverloadResolution None resArgs false funcs with
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

let private checkCalleeExpression (cenv: cenv) (env: BinderEnvironment) (expr: E) =
    match expr with
    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, isVirtualCall) ->

        let isAddrOf = value.IsAddressOf

        let argExprs =
            if value.IsFunctionGroup then
                if isAddrOf then
                    checkFunctionGroupCalleeArgumentExpressionForAddressOf cenv env argExprs[0]
                    |> ImArray.createOne
                else
                    argExprs
                    |> ImArray.map (checkFunctionGroupCalleeArgumentExpression cenv env)
            else
                checkCalleeArgumentExpressions cenv env value argExprs

        if isAddrOf then
            let argExpr = argExprs[0]
            match argExpr with
            | AutoDereferenced _ -> ()
            | E.Value _ -> ()
            | E.GetField(field=field) ->
                if field.IsInstance && field.Enclosing.IsNewtype then
                    cenv.diagnostics.Error("Newtypes do not allow getting the address of its field.", 10, syntaxInfo.Syntax)
            | E.Call(value=value) ->
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

        E.Call(
            syntaxInfo,
            receiverExprOpt,
            witnessArgs,
            argExprs,
            value,
            isVirtualCall
        )
    | _ ->
        expr

let private checkCallerCallExpression (cenv: cenv) (env: BinderEnvironment) skipEager (expectedTyOpt: TypeSymbol option) isArgForAddrOf expr =
    match expr with
    | E.Call(syntaxInfo, receiverExprOpt, _, argExprs, value, flags) ->

        let checkLambdaArguments() =
            let solverEnv = SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)
            argExprs
            |> ImArray.iter (fun x ->           
                x.ForEachReturningTargetExpression(fun x ->
                    match x with
                    | E.Lambda _ ->
                        // We must evaluate the lambda at this point.
                        checkImmediateExpression solverEnv false x
                    | _ ->
                        ()
                )
            )

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
                   
            match tryOverloadedCallExpression cenv env skipEager expectedTyOpt syntaxInfo receiverExprOpt argExprs isArgForAddrOf funcGroup.Functions flags with
            | Some expr -> expr
            | _ -> expr
        | _ ->
            checkLambdaArguments()
            expr
    | _ ->
        OlyAssert.Fail("Expected 'Call' expression.")

let private checkCallerExpression (cenv: cenv) (env: BinderEnvironment) skipEager (expectedTyOpt: TypeSymbol option) (isArgForAddrOf: bool) (expr: E) =
    match expr with
    | E.Value(syntaxInfo, value) when value.IsFunction ->
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

    | E.Witness(syntaxInfo, benv, castFunc, bodyExpr, witnessTy, exprTy) ->
        let newBodyExpr = checkExpression cenv env None bodyExpr
        if newBodyExpr = bodyExpr then
            expr
        else
            E.Witness(syntaxInfo, benv, castFunc, newBodyExpr, witnessTy, exprTy)

    | E.Call _ ->
        checkCallerCallExpression cenv env skipEager expectedTyOpt isArgForAddrOf expr

    | AutoDereferenced(exprAsAddr) ->
        // We do this to make sure we actually check the call 'FromAddress'.
        checkCallerExpression cenv env skipEager None false exprAsAddr
        |> autoDereferenceExpression

    | _ ->
        expr

let private lateCheckCalleeExpression cenv env expr =
    match expr with
    | LoadFunctionPtr(syntaxInfo, funcLoadFunctionPtr, _) ->
        match expr with
        | LoadFunctionPtrOfLambdaWrappedFunctionCall(_, _, innerSyntaxInfo, func) ->
            match func.Type.TryGetFunctionWithParameters() with
            | ValueSome(argTys, returnTy) ->
                // TODO: This is weird, all because this is to satisfy __oly_load_function_ptr type arguments.
                //       Perhaps we should just change __oly_load_function_ptr to simply have 1 type argument be the return type.
                let argTysWithoutInstance =
                    if func.IsInstance then
                        argTys.RemoveAt(0)
                    else
                        argTys
                let expectedFuncTy = TypeSymbol.CreateFunction(argTysWithoutInstance, returnTy, FunctionKind.Normal)
                checkTypes
                    (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) 
                    innerSyntaxInfo.Syntax 
                    expectedFuncTy
                    funcLoadFunctionPtr.Parameters[0].Type

                let ilCallConv =
                    if func.IsBlittable then
                        Oly.Metadata.OlyILCallingConvention.Blittable
                    else
                        Oly.Metadata.OlyILCallingConvention.Default

                let expectedReturnTy = TypeSymbol.CreateFunctionPtr(ilCallConv, argTys, returnTy)
                checkTypes
                    (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) 
                    innerSyntaxInfo.Syntax 
                    expectedReturnTy 
                    funcLoadFunctionPtr.ReturnType

                if func.AllTypeParameterCount > 0 then
                    cenv.diagnostics.Error("Getting the address of a function requires the function not be generic or enclosed by a generic type.", 10, innerSyntaxInfo.Syntax)
            | _ ->
                cenv.diagnostics.Error("Invalid use of 'LoadFunctionPtr'.", 10, innerSyntaxInfo.Syntax)
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

    | E.GetProperty(syntaxInfo=syntaxInfo;prop=prop) ->
        if prop.Getter.IsNone || not (canAccessValue env.benv.ac prop.Getter.Value) then
            cenv.diagnostics.Error($"Unable to get property value as '{prop.Name}' does not have a getter.", 10, syntaxInfo.SyntaxNameOrDefault)

    | E.SetProperty(syntaxInfo=syntaxInfo;prop=prop) ->
        if prop.Setter.IsNone || not (canAccessValue env.benv.ac prop.Setter.Value) then
            cenv.diagnostics.Error($"Unable to set property value as '{prop.Name}' does not have a setter.", 10, syntaxInfo.SyntaxNameOrDefault)

    | _ ->
        ()

    checkReceiverOfExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expr
    autoDereferenceExpression expr

let private checkCallReturnExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) expr =

    let expr =
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
                |> ImArray.map (fun x -> E.CreateValue(cenv.syntaxTree, x))
            E.CreateLambda(BoundSyntaxInfo.Generated(cenv.syntaxTree),
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

    let expr = autoDereferenceExpression expr
    let recheckExpectedTy =
        match expectedTyOpt with
        | Some expectedTy when expectedTy.IsSolved ->
            let exprTy = expr.Type
            if exprTy.IsSolved then
                checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expectedTy expr
            else
                match expr with
                | AutoDereferenced expr when expectedTy.IsByRef_t ->
                    checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expectedTy expr
                | _ ->
                    checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expectedTy expr
            false
        | _ ->
            true

    match expr with
    | AutoDereferenced bodyExpr ->
        match bodyExpr with
        | E.Call _ ->
            checkConstraintsFromCallExpression cenv.diagnostics true cenv.pass bodyExpr
        | _ ->
            ()
    | _ ->
        match expr with
        | E.Call _ ->
            checkConstraintsFromCallExpression cenv.diagnostics true cenv.pass expr 
        | _ ->
            ()

    let expr = autoDereferenceExpression expr
    if recheckExpectedTy then
        match expectedTyOpt with
        | Some expectedTy ->
            checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expectedTy expr
        | _ ->
            ()

    expr

let private checkCallExpression (cenv: cenv) (env: BinderEnvironment) (skipEager: bool) (expectedTyOpt: TypeSymbol option) (isArgForAddrOf: bool) (expr: E) =
    checkCallerExpression cenv env true None isArgForAddrOf expr
    |> checkCalleeExpression cenv env
    |> checkCallerExpression cenv env skipEager expectedTyOpt isArgForAddrOf
    |> checkCalleeExpression cenv env
    |> lateCheckCalleeExpression cenv env
    |> checkCallReturnExpression cenv env expectedTyOpt

let private checkCalleeArgumentExpression cenv env (caller: IValueSymbol) index parTy argExpr =
    match argExpr with
    | E.Call(value=funcGroup) when funcGroup.IsFunctionGroup ->
        let isAddrOf = caller.IsAddressOf
        let expectedTy =
            if isAddrOf then
                match caller.Type.TryFunction with
                | ValueSome(_, outputTy) ->
                    if outputTy.IsReadOnlyByRef then
                        TypeSymbol.CreateByRef(parTy, ByRefKind.Read)
                    else
                        TypeSymbol.CreateByRef(parTy, ByRefKind.ReadWrite)
                | _ ->
                    parTy
            else
                parTy
        let newArgExpr = checkCallExpression cenv env false (Some expectedTy) isAddrOf argExpr
        if newArgExpr = argExpr then
            argExpr
        else
            newArgExpr

    | E.Lambda(syntaxInfo, lambdaFlags, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr, lazyTy, freeLocals, freeTyVars) ->
        let parsOpt =
            if caller.IsFunction then
                (caller :?> IFunctionSymbol).LogicalParameters
                |> ValueSome
            else
                ValueNone

        let argExpr =
            match parsOpt with
            | ValueSome pars when not(lambdaFlags.HasFlag(LambdaFlags.Inline)) ->
                let lambdaInlineFlagsOpt =
                    pars[index].Attributes
                    |> ImArray.tryPick (function
                        | AttributeSymbol.Inline(inlineArg) ->
                            inlineArg.ToLambdaFlags() |> Some
                        | _ ->
                            None
                    )
                match lambdaInlineFlagsOpt with
                | Some lambdaInlineFlags ->
                    E.Lambda(syntaxInfo, lambdaFlags ||| lambdaInlineFlags, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr, lazyTy, freeLocals, freeTyVars)
                | _ ->
                    argExpr
            | _ ->
                argExpr

        checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) parTy argExpr

        let argExpr =
            match parTy.TryFunction, argExpr.Type.TryFunction with
            | ValueSome(_, outputTy), ValueSome(_, argTy) when caller.TryWellKnownFunction.IsNone ->
                if outputTy.IsRealUnit && argTy.IsUnit_t && not argTy.IsRealUnit then
                    match argExpr with
                    | E.Lambda(syntaxInfo, lambdaFlags, lambdaTyPars, lambdaPars, lazyLambdaBodyExpr, _, _, _) ->
                        E.CreateLambda(syntaxInfo, lambdaFlags, lambdaTyPars, lambdaPars,
                            LazyExpression.CreateNonLazy(
                                lazyLambdaBodyExpr.TrySyntax,
                                fun _ ->
                                    E.Sequential(BoundSyntaxInfo.Generated(cenv.syntaxTree),
                                        lazyLambdaBodyExpr.Expression,
                                        E.Unit(BoundSyntaxInfo.Generated(cenv.syntaxTree)),
                                        BoundSequentialSemantic.NormalSequential
                                    )
                            )
                        )
                    | _ ->
                        OlyAssert.Fail("Expected lambda expression.")
                else
                    argExpr
            | _ ->
                argExpr

        argExpr

    | _ ->
        argExpr

let private checkCalleeArgumentExpressions cenv env (caller: IValueSymbol) (argExprs: E imarray) =
    let argTys = caller.LogicalType.FunctionArgumentTypes
    if argTys.Length = argExprs.Length then              
        (argTys, argExprs)
        ||> ImArray.mapi2 (fun i argTy argExpr ->
            argExpr.RewriteReturningTargetExpression(fun x ->
                checkCalleeArgumentExpression cenv env caller i argTy x
            )
        )
    else
        // Not enough arguments, but we check for this elsewhere.
        // REVIEW: Maybe we should actually check it here...
        argExprs

let private checkFunctionGroupCalleeArgumentExpression cenv env argExpr =
    checkCallExpression cenv env false None false argExpr

let private checkFunctionGroupCalleeArgumentExpressionForAddressOf cenv env argExpr =
    checkCallExpression cenv env false None true argExpr

let checkExpression (cenv: cenv) (env: BinderEnvironment) expectedTyOpt (expr: E) =
    // If the expression is used as an argument, then we will skip eager inference in function overloads.
    checkCallExpression cenv env env.isPassedAsArgument expectedTyOpt false expr
    |> checkVirtualUsage cenv env