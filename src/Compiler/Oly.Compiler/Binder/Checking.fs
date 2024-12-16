[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Checking

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
open Oly.Compiler.Internal.SymbolQuery
open Oly.Compiler.Internal.SymbolQuery.Extensions
open Oly.Compiler.Internal

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

let checkUsageTypeExport cenv syntaxNode (name: string) (ty: TypeSymbol) =
    // Do not strip type equations here.
    match ty with
    | TypeSymbol.Entity(ent) ->
        if not ent.IsExported && not ent.IsImported then
            cenv.diagnostics.Error($"'{name}' cannot be exported as its usage of type '{ent.Name}' s neither imported or exported.", 10, syntaxNode)
        ent.TypeArguments
        |> ImArray.iter (checkUsageTypeExport cenv syntaxNode name)
    | _ ->
        ty.TypeArguments
        |> ImArray.iter (checkUsageTypeExport cenv syntaxNode name)

let checkTypeParameterExport cenv syntaxNode (name: string) (tyPar: TypeParameterSymbol) =
    tyPar.Constraints
    |> ImArray.iter (fun x ->
        match x.TryGetAnySubtypeOf() with
        | ValueSome constrTy ->
            checkUsageTypeExport cenv syntaxNode name constrTy
        | _ ->
            ()
    )

let checkEntityExport cenv syntaxNode (ent: EntitySymbol) =
    if ent.IsExported && ent.IsImported then
        cenv.diagnostics.Error($"'{ent.Name}' cannot be imported and exported at the same time.", 10, syntaxNode)
    else
        if ent.IsExported then
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
            value.TypeParameters
            |> ImArray.iter (fun tyPar ->
                checkTypeParameterExport cenv syntaxNode value.Name tyPar
            )

            checkUsageTypeExport cenv syntaxNode value.Name value.LogicalType

        //if value.IsImported then
        //    if value.IsInstance && not value.Enclosing.IsImported then
        //        cenv.diagnostics.Error($"'{value.Name}' cannot be imported as its enclosing is not imported.", 10, syntaxNode)

let autoDereferenceExpression expr =
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

let filterByRefReturnTypes (argExprs: E imarray) (funcs: IFunctionSymbol imarray) =
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

[<RequireQualifiedAccess>]
type TypeChecking =
    | Enabled
    | Disabled

let tryOverloadResolution
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

let tryOverloadedCallExpression 
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
            Some expr
        else
            
            let funcs2 = filterFunctionsForOverloadingPart3 skipEager resArgs expectedTyOpt funcs
            let funcs = if funcs2.IsEmpty then funcs else funcs2

            let funcs = filterByRefReturnTypes argExprs funcs
            if funcs.IsEmpty then
                None
            else       
                let func = FunctionGroupSymbol.CreateIfPossible(funcs)
                let expr = bindValueAsCallExpressionWithOptionalSyntaxName cenv env syntaxInfo receiverExprOpt (ValueSome argExprs) (func, syntaxInfo.TrySyntaxName)
                Some expr

let createPartialCallExpression (cenv: cenv) (env: BinderEnvironment) syntaxNode syntaxNameOpt (tyArgs: _ imarray) (func: IFunctionSymbol) =
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

let createPartialCallExpressionWithSyntaxTypeArguments (cenv: cenv) (env: BinderEnvironment) syntaxNode syntaxNameOpt (syntaxTyArgsRoot, syntaxTyArgs) (func: IFunctionSymbol) =
    let tyArgs = bindTypeArguments cenv env func.HasStrictInference 0 func.TypeParametersOrConstructorEnclosingTypeParameters (syntaxTyArgsRoot, syntaxTyArgs)
    createPartialCallExpression cenv env syntaxNode syntaxNameOpt tyArgs func

/// TODO: There is duplication when it comes to handling overloading for non-partial and partial calls. We should figure out a way to combine them.
let tryOverloadPartialCallExpression
        (cenv: cenv) 
        (env: BinderEnvironment) 
        (expectedTyOpt: TypeSymbol option) 
        (syntaxInfo: BoundSyntaxInfo) 
        (syntaxNameOpt: OlySyntaxName option)
        (funcs: IFunctionSymbol imarray) =

    let resArgs =
        match expectedTyOpt with
        | Some(expectedTy) when expectedTy.IsAnyFunction ->
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

let checkCalleeExpression (cenv: cenv) (env: BinderEnvironment) (tyChecking: TypeChecking) (expr: E) =
    match expr with
    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, isVirtualCall) ->

        let isAddrOf = value.IsAddressOf

        let argExprs =
            if value.IsFunctionGroup then
                if isAddrOf then
                    checkFunctionGroupCalleeArgumentExpressionForAddressOf cenv env tyChecking argExprs[0]
                    |> ImArray.createOne
                else
                    argExprs
                    |> ImArray.map (checkFunctionGroupCalleeArgumentExpression cenv env tyChecking)
            else
                checkCalleeArgumentExpressions cenv env tyChecking value argExprs

        if isAddrOf then
            let argExpr = argExprs[0]
            match argExpr with
            | AutoDereferenced _ -> ()
            | E.Value _ -> ()
            | E.GetField _ -> ()
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

let checkCallerCallExpression (cenv: cenv) (env: BinderEnvironment) skipEager (expectedTyOpt: TypeSymbol option) isArgForAddrOf expr =
    match expr with
    | E.Call(syntaxInfo, receiverExprOpt, _, argExprs, value, flags) ->

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
            expr
    | _ ->
        OlyAssert.Fail("Expected 'Call' expression.")

let checkCallerExpression (cenv: cenv) (env: BinderEnvironment) (tyChecking: TypeChecking) skipEager (expectedTyOpt: TypeSymbol option) (isArgForAddrOf: bool) (expr: E) =
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
        let newBodyExpr = checkExpressionAux cenv env tyChecking None bodyExpr
        if newBodyExpr = bodyExpr then
            expr
        else
            E.Witness(syntaxInfo, benv, castFunc, newBodyExpr, witnessTy, exprTy)

    | E.Call _ ->
        checkCallerCallExpression cenv env skipEager expectedTyOpt isArgForAddrOf expr

    | AutoDereferenced(exprAsAddr) ->
        // We do this to make sure we actually check the call 'FromAddress'.
        checkCallerExpression cenv env tyChecking skipEager None false exprAsAddr
        |> autoDereferenceExpression

    | _ ->
        expr

let lateCheckCalleeExpression cenv env expr =
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
                    let resTyArity = typeResolutionArityOfName syntaxName.LastGenericNameIfPossible
                    if resTyArity.IsAny_t then
                        cenv.diagnostics.Error($"'{value.Name}' requires explicit type arguments.", 10, syntaxInfo.Syntax)
            | _ ->
                ()
        | _ ->
            ()

    | E.GetProperty(syntaxInfo=syntaxInfo;prop=prop) ->
        if prop.Getter.IsNone || not (prop.Getter.Value.IsAccessible(env.benv.ac)) then
            cenv.diagnostics.Error($"Unable to get property value as '{prop.Name}' does not have a getter.", 10, syntaxInfo.SyntaxNameOrDefault)

    | E.SetProperty(syntaxInfo=syntaxInfo;prop=prop) ->
        if prop.Setter.IsNone || not (prop.Setter.Value.IsAccessible(env.benv.ac)) then
            cenv.diagnostics.Error($"Unable to set property value as '{prop.Name}' does not have a setter.", 10, syntaxInfo.SyntaxNameOrDefault)

    | _ ->
        ()

    checkReceiverOfExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expr

    autoDereferenceExpression expr

let checkCallReturnExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) expr =
    let expr = autoDereferenceExpression expr
    let expr = ImplicitRules.ImplicitReturn expectedTyOpt expr
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

let checkCallExpression (cenv: cenv) (env: BinderEnvironment) (tyChecking: TypeChecking) (skipEager: bool) (expectedTyOpt: TypeSymbol option) (isArgForAddrOf: bool) (expr: E) =
    checkCallerExpression cenv env tyChecking true None isArgForAddrOf expr
    |> checkCalleeExpression cenv env tyChecking
    |> checkEarlyArgumentsOfCallExpression cenv env
    |> checkCallerExpression cenv env tyChecking skipEager expectedTyOpt isArgForAddrOf
    |> checkCalleeExpression cenv env tyChecking
    |> ImplicitRules.ImplicitCallExpression env.benv
    |> checkArgumentsOfCallLikeExpression cenv env tyChecking
    |> lateCheckCalleeExpression cenv env
    |> checkCallReturnExpression cenv env expectedTyOpt

let checkCalleeArgumentExpression cenv env (tyChecking: TypeChecking) (caller: IValueSymbol) (parAttrs: AttributeSymbol imarray) parTy argExpr =
    match argExpr with
    | E.Call(value=funcGroup) when funcGroup.IsFunctionGroup ->
        let isAddrOf = caller.IsAddressOf
        let expectedTy =
            if isAddrOf then
                match caller.Type.TryFunction with
                | ValueSome(_, outputTy) ->
                    if outputTy.IsReadOnlyByRef then
                        TypeSymbol.CreateByRef(parTy, ByRefKind.ReadOnly)
                    else
                        TypeSymbol.CreateByRef(parTy, ByRefKind.ReadWrite)
                | _ ->
                    parTy
            else
                parTy
        let newArgExpr = checkCallExpression cenv env tyChecking false (Some expectedTy) isAddrOf argExpr
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
                    parAttrs
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

        argExpr

    | _ ->
        argExpr

let checkCalleeArgumentExpressions cenv env (tyChecking: TypeChecking) (caller: IValueSymbol) (argExprs: E imarray) =
    let argTys = caller.LogicalType.FunctionArgumentTypes
    if argTys.Length = argExprs.Length then       
        let env = env.SetReturnable(false).SetPassedAsArgument(false)
        (argTys, argExprs)
        ||> ImArray.mapi2 (fun i argTy argExpr ->
            let parAttrs =
                match caller.TryGetFunctionLogicalParameterAttributesByIndex(i) with
                | ValueSome(attrs) -> attrs
                | _ -> ImArray.empty
            argExpr.RewriteReturningTargetExpression(fun x ->
                checkCalleeArgumentExpression cenv env tyChecking caller parAttrs argTy x
            )
        )
    else
        // Not enough arguments, but we check for this elsewhere.
        // REVIEW: Maybe we should actually check it here...
        argExprs

let checkFunctionGroupCalleeArgumentExpression cenv env (tyChecking: TypeChecking) argExpr =
    checkCallExpression cenv env tyChecking false None false argExpr

let checkFunctionGroupCalleeArgumentExpressionForAddressOf cenv env (tyChecking: TypeChecking) argExpr =
    checkCallExpression cenv env tyChecking false None true argExpr

let checkArgumentExpression cenv env (tyChecking: TypeChecking) expectedTyOpt (argExpr: E) =
    argExpr.RewriteReturningTargetExpression(
        fun argExpr ->
            match argExpr with
            | E.Literal _
            | E.Lambda _ ->
                checkExpressionAux cenv env tyChecking expectedTyOpt argExpr
            | _ ->
                checkExpressionTypeIfPossible cenv env tyChecking expectedTyOpt argExpr
                argExpr
    )

let checkExpressionTypeIfPossible cenv env (tyChecking: TypeChecking) (expectedTyOpt: TypeSymbol option) expr =
    match expectedTyOpt with
    | Some expectedTy ->
        match tyChecking with
        | TypeChecking.Enabled ->
            checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expectedTy expr
        | TypeChecking.Disabled ->
            checkExpressionType (SolverEnvironment.CreateNoTypeErrors(cenv.diagnostics, env.benv, cenv.pass)) expectedTy expr
    | _ ->
        ()

let checkEarlyArgumentsOfCallExpression cenv (env: BinderEnvironment) expr =
    match expr with
    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, callFlags) ->
        let argTys = value.LogicalType.FunctionArgumentTypes

        let newArgExprs =
            let env = env.SetReturnable(false).SetPassedAsArgument(false)
            argExprs
            |> ImArray.mapi (fun i argExpr ->
                let expectedArgTy =
                    if i < argTys.Length then
                        argTys[i]
                    else
                        TypeSymbolError

                let parAttrs =
                    match value.TryGetFunctionLogicalParameterAttributesByIndex(i) with
                    | ValueSome(attrs) -> attrs
                    | _ -> ImArray.empty

                argExpr.RewriteReturningTargetExpression(fun x ->
                    checkCalleeArgumentExpression cenv env TypeChecking.Disabled value parAttrs expectedArgTy x
                    |> checkArgumentExpression cenv env TypeChecking.Disabled (Some expectedArgTy)
                )
            )
        
        E.Call(syntaxInfo, receiverExprOpt, witnessArgs, newArgExprs, value, callFlags)
    | _ ->
        expr

let checkArgumentsOfCallLikeExpression cenv (env: BinderEnvironment) (tyChecking: TypeChecking) expr =
    match expr with
    | E.NewArray(syntaxExpr, benv, argExprs, exprTy) when not argExprs.IsEmpty ->
        let expectedArgTyOpt = Some exprTy.FirstTypeArgument
        let newArgExprs =         
            let env = env.SetReturnable(false).SetPassedAsArgument(false)
            argExprs
            |> ImArray.map (fun argExpr ->
                checkArgumentExpression cenv env TypeChecking.Enabled expectedArgTyOpt argExpr
            )
        E.NewArray(syntaxExpr, benv, newArgExprs, exprTy)

    | E.NewTuple(syntaxInfo, argExprs, exprTy) ->
        OlyAssert.True(argExprs.Length > 1)
        let newArgExprs =       
            let env = env.SetReturnable(false).SetPassedAsArgument(false)
            argExprs
            |> ImArray.mapi (fun i argExpr ->
                let expectedArgTy =
                    if i < exprTy.TypeArguments.Length then
                        exprTy.TypeArguments[i]
                    else
                        TypeSymbolError
                checkArgumentExpression cenv env tyChecking (Some expectedArgTy) argExpr
            )
        E.NewTuple(syntaxInfo, newArgExprs, exprTy)

    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, callFlags) ->
        let argTys = value.LogicalType.FunctionArgumentTypes

        if not value.IsFunctionGroup && not(callFlags.HasFlag(CallFlags.Partial)) && argTys.Length <> argExprs.Length && not value.LogicalType.IsError_t then
            cenv.diagnostics.Error(sprintf "Expected %i argument(s) but only given %i." argTys.Length argExprs.Length, 0, syntaxInfo.Syntax)

        let tyChecking =
            if value.IsFunctionGroup && env.isPassedAsArgument then
                TypeChecking.Disabled
            else
                tyChecking

        let newArgExprs =
            let env = env.SetReturnable(false).SetPassedAsArgument(false)
            argExprs
            |> ImArray.mapi (fun i argExpr ->
                let expectedArgTy =
                    if i < argTys.Length then
                        argTys[i]
                    else
                        TypeSymbolError

                let parAttrs =
                    match value.TryGetFunctionLogicalParameterAttributesByIndex(i) with
                    | ValueSome(attrs) -> attrs
                    | _ -> ImArray.empty

                argExpr.RewriteReturningTargetExpression(fun x ->
                    let result =
                        checkCalleeArgumentExpression cenv env tyChecking value parAttrs expectedArgTy x
                        |> checkArgumentExpression cenv env tyChecking (Some expectedArgTy)
                    result
                )
            )

        match tyChecking with
        | TypeChecking.Enabled ->
            if value.Enclosing.IsAbstract && value.IsConstructor && not value.IsBase then
                cenv.diagnostics.Error(sprintf "The constructor call is not allowed as the enclosing type '%s' is abstract." (printEnclosing env.benv value.Enclosing), 10, syntaxInfo.Syntax)

            if not env.isReturnable && value.IsInstanceConstructor && value.IsBase then
                cenv.diagnostics.Error("The base constructor call is only allowed as the last expression of a branch.", 10, syntaxInfo.Syntax)
        | _ ->
            ()
        
        E.Call(syntaxInfo, receiverExprOpt, witnessArgs, newArgExprs, value, callFlags)

    // REVIEW: This isn't particularly great, but it is the current way we handle indirect calls from property getters.
    | E.Let(syntaxInfo, bindingInfo, ((E.GetProperty _) as rhsExpr), bodyExpr) 
            when 
                bindingInfo.Value.IsSingleUse && 
                bindingInfo.Value.IsGenerated ->
        let newBodyExpr = checkExpressionAux cenv env tyChecking None bodyExpr

        if newBodyExpr = bodyExpr then
            expr
        else
            E.Let(syntaxInfo, bindingInfo, rhsExpr, newBodyExpr)

    | _ ->
        expr

let checkExpressionAux (cenv: cenv) (env: BinderEnvironment) (tyChecking: TypeChecking) expectedTyOpt (expr: E) =
    match expr with
    | E.Literal(syntaxInfo, BoundLiteral.NumberInference(lazyLiteral, _)) when env.isReturnable || not env.isPassedAsArgument ->
        checkExpressionTypeIfPossible cenv env tyChecking expectedTyOpt expr

        let canEval =
            match tyChecking with
            | TypeChecking.Enabled -> true
            | TypeChecking.Disabled -> false

        if canEval then
            match tryEvaluateLazyLiteral cenv.diagnostics lazyLiteral with
            | ValueSome(literal) ->
                E.Literal(syntaxInfo, stripLiteral literal)
            | _ ->
                expr
        else
            expr

    | E.Literal _ when env.isReturnable || not env.isPassedAsArgument ->
        checkExpressionTypeIfPossible cenv env tyChecking expectedTyOpt expr
        expr

    | E.Lambda(body=lazyBodyExpr) when env.isReturnable || not env.isPassedAsArgument ->
        checkExpressionTypeIfPossible cenv env tyChecking expectedTyOpt expr
        if not lazyBodyExpr.HasExpression then
            lazyBodyExpr.Run()
        expr

    | E.NewArray _
    | E.NewTuple _ ->
        checkExpressionTypeIfPossible cenv env tyChecking expectedTyOpt expr
        checkArgumentsOfCallLikeExpression cenv env tyChecking expr

    | _ ->
        // If the expression is used as an argument, then we will skip eager inference in function overloads.
        // REVIEW: The name 'checkCallExpression' isn't quite accurate because it can affect non-call expressions.
        checkCallExpression cenv env tyChecking env.isPassedAsArgument expectedTyOpt false expr
        |> checkVirtualUsage cenv env

let checkExpression (cenv: cenv) (env: BinderEnvironment) expectedTyOpt (expr: E) =
    checkExpressionAux cenv env TypeChecking.Enabled expectedTyOpt expr