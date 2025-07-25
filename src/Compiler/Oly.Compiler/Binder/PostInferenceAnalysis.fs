﻿[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.Binder.PostInferenceAnalysis

open System

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker

[<Flags>]
type ScopeLimits =
    | None                    = 0b0000
    | ByRef                   = 0b0001
    | StackReferring          = 0b0010
    | StackReferringByRef     = 0b0011

[<Struct;NoEquality;NoComparison>]
type ScopeValues =
    {
        Value: int32
        ValueLambda: int32
        Limits: ScopeLimits
    }

[<Struct;NoEquality;NoComparison>]
type ScopeResult =
    {
        ScopeValue: int32
        ScopeLimits: ScopeLimits
    }

type acenv = 
    { 
        cenv: cenv 
        scopes: System.Collections.Generic.Dictionary<int64, ScopeValues>
        checkedTypeParameters: System.Collections.Generic.HashSet<int64> 
    }

type Limits =
    | None                      = 0x000000
    | UnmanagedAllocationOnly   = 0x0000001

type aenv = 
    { 
        envRoot: BinderEnvironment 
        scope: int 
        scopeLambda: int
        isLastExprOfScope: bool 
        isReturnable: bool
        benv: BoundEnvironment 
        isMemberSig: bool
        memberFlags: MemberFlags
        limits: Limits
        currentNonLocalFunctionOpt: IFunctionSymbol option
        currentFunctionOpt: IFunctionSymbol option
    }

    member this.IsInUnmanagedAllocationOnlyContext =
        this.limits.HasFlag(Limits.UnmanagedAllocationOnly)

let notLastExprOfScope aenv = 
    if aenv.isLastExprOfScope then
        { aenv with isLastExprOfScope = false }
    else
        aenv

let notReturnable aenv = 
    if aenv.isReturnable then
        { aenv with isReturnable = false }
    else
        aenv

let reportUnmanagedAllocationOnly acenv (syntaxNode: OlySyntaxNode) =
    acenv.cenv.diagnostics.Error("Managed allocations are not allowed.", 10, syntaxNode.BestSyntaxForReporting)

let reportUnmanagedAllocationOnlyBoxing acenv (syntaxNode: OlySyntaxNode) =
    acenv.cenv.diagnostics.Error("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.", 10, syntaxNode.BestSyntaxForReporting)

let reportScopedTypeBoxing acenv benv (ty: TypeSymbol) (syntaxNode: OlySyntaxNode) =
    acenv.cenv.diagnostics.Error($"'{printType benv ty}' is scoped and cannot be boxed.", 10, syntaxNode.BestSyntaxForReporting)

let reportAddressOfValueOutOfScope acenv (syntaxNode: OlySyntaxNode) (value: IValueSymbol) =
    acenv.cenv.diagnostics.Error($"Cannot take the address of '{value.Name}' as it might escape its scope at this point.", 10, syntaxNode)

let reportExpressionOutOfScope acenv (syntaxNode: OlySyntaxNode) =
    acenv.cenv.diagnostics.Error($"Expression is scoped and might escape its scope at this point.", 10, syntaxNode)

let reportAddressValueCannotBeCaptured acenv (syntaxNode: OlySyntaxNode) (value: IValueSymbol) =
    OlyAssert.True(value.Type.IsScoped)
    acenv.cenv.diagnostics.Error($"'{value.Name}' is an address and cannot be captured.", 10, syntaxNode)

let reportRestrictedTypeParameter acenv (syntaxNode: OlySyntaxNode) (tyPar: TypeParameterSymbol) =
    acenv.cenv.diagnostics.Error($"Type parameter '{tyPar.Name}' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:", 10, syntaxNode)

[<Flags>]
type TypeAnalysisFlags =
    | None                        = 0b0000000
    | PermitByRef                 = 0b0000001
    | RestrictTypeParameterUse    = 0b0000010

let canPermitByRef (flags: TypeAnalysisFlags) =
    flags.HasFlag(TypeAnalysisFlags.PermitByRef)

let hasRestrictedTypeParameterUse (flags: TypeAnalysisFlags) =
    flags.HasFlag(TypeAnalysisFlags.RestrictTypeParameterUse)

let analyzeTypeParameterUse (acenv: acenv) (aenv: aenv) (flags: TypeAnalysisFlags) (syntaxNode: OlySyntaxNode) (tyPar: TypeParameterSymbol) =
    if hasRestrictedTypeParameterUse flags then
        match aenv.currentNonLocalFunctionOpt with
        | Some(func) when 
                func.IsExported || 
                func.Enclosing.IsExported ->
            match tyPar.Kind with
            | TypeParameterKind.Type when func.Enclosing.IsExported ->
                reportRestrictedTypeParameter acenv syntaxNode tyPar
            | TypeParameterKind.Function _ when func.IsExported ->
                reportRestrictedTypeParameter acenv syntaxNode tyPar
            | _ ->
                ()
        | _ ->
            ()

let rec analyzeTypeAux (acenv: acenv) (aenv: aenv) (flags: TypeAnalysisFlags) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    let benv = aenv.envRoot.benv
    let diagnostics = acenv.cenv.diagnostics

    let partiallyStrippedTy = stripTypeEquationsExceptAlias ty

    match partiallyStrippedTy with
    | TypeSymbol.Entity(ent) when ent.IsAlias ->
        analyzeTypeEntityAccessibility acenv aenv syntaxNode ent
    | _ ->
        ()

    match stripTypeEquations partiallyStrippedTy with
    | TypeSymbol.Entity(ent) ->
        analyzeTypeEntity acenv aenv syntaxNode ent

    | TypeSymbol.InferenceVariable(Some tyPar, _)
    | TypeSymbol.HigherInferenceVariable(Some tyPar, _, _, _) ->
        diagnostics.Error(sprintf "Type parameter '%s' was unable to be inferred." (printType benv ty), 5, syntaxNode)
        // We solve the inference variable with an error as to prevent a cascade of the same errors.
        // We only care about it at the earliest possible point.
        UnifyTypes TypeVariableRigidity.Flexible ty (TypeSymbol.Error(Some tyPar, None))
        |> ignore

    | TypeSymbol.EagerInferenceVariable _
    | TypeSymbol.InferenceVariable(_, _)
    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        diagnostics.Error("Unable to infer type at this location.", 5, syntaxNode)
        // We solve the inference variable with an error as to prevent a cascade of the same errors.
        // We only care about it at the earliest possible point.
        UnifyTypes TypeVariableRigidity.Flexible ty TypeSymbolError
        |> ignore

    | TypeSymbol.NativeFunctionPtr(_, inputTy, returnTy)
    | TypeSymbol.Function(inputTy, returnTy, _) ->
        match inputTy with
        | TypeSymbol.Tuple(tyArgs, _) ->
            tyArgs
            |> ImArray.iter (analyzeTypePermitByRef acenv aenv syntaxNode)
        | _ ->
            analyzeTypePermitByRef acenv aenv syntaxNode inputTy
        analyzeType acenv aenv syntaxNode returnTy

    | TypeSymbol.ForAll(tyPars, innerTy) ->
        tyPars 
        |> ImArray.iter (fun tyPar -> analyzeType acenv aenv syntaxNode tyPar.AsType)
        analyzeTypeAux acenv aenv flags syntaxNode innerTy

    | TypeSymbol.Tuple(tyArgs, _) ->
        analyzeTypeTuple acenv aenv syntaxNode tyArgs

    | TypeSymbol.Variable(tyPar) ->
        analyzeTypeParameterUse acenv aenv flags syntaxNode tyPar

    | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
        analyzeTypeParameterUse acenv aenv flags syntaxNode tyPar
        tyArgs
        |> ImArray.iter (fun tyArg ->
            analyzeType acenv aenv syntaxNode tyArg
        )

    | TypeSymbol.NativePtr(elementTy) ->
        if not elementTy.IsVoid_t then
            analyzeType acenv aenv syntaxNode elementTy

    | TypeSymbol.Void ->
        diagnostics.Error($"'{printType benv ty}' can only be used as a type argument of a native pointer.", 10, syntaxNode)

    | TypeSymbol.Error(_, Some msg) ->
        diagnostics.Error(msg, 10, syntaxNode)
    
    | strippedTy ->
        match strippedTy with
        | TypeSymbol.ByRef _ when not(canPermitByRef flags) ->
            match ty with
            | TypeSymbol.InferenceVariable(Some tyPar, _)
            | TypeSymbol.HigherInferenceVariable(Some tyPar, _, _, _) when tyPar.Constraints |> ImArray.exists (function ConstraintSymbol.Scoped -> true | _ -> false) ->
                ()
            | _ ->
                diagnostics.Error($"'{printType benv ty}' not permitted in this context.", 10, syntaxNode)
        | _ ->
            ()
        ty.TypeArguments |> ImArray.iter (analyzeType acenv aenv syntaxNode)

and analyzeType (acenv: acenv) (aenv: aenv) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    analyzeTypeAux acenv aenv TypeAnalysisFlags.None syntaxNode ty

and analyzeTypePermitByRef (acenv: acenv) (aenv: aenv) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    analyzeTypeAux acenv aenv TypeAnalysisFlags.PermitByRef syntaxNode ty

and analyzeTypeRestrictTypeParameterUse (acenv: acenv) (aenv: aenv) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    analyzeTypeAux acenv aenv TypeAnalysisFlags.RestrictTypeParameterUse syntaxNode ty

and analyzeTypePermitByRefAndRestrictTypeParameterUse (acenv: acenv) (aenv: aenv) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    analyzeTypeAux acenv aenv (TypeAnalysisFlags.PermitByRef ||| TypeAnalysisFlags.RestrictTypeParameterUse) syntaxNode ty

and analyzeTypeParameterDefinition acenv aenv (syntaxConstrClause: OlySyntaxConstraintClause) (tyPar: TypeParameterSymbol) (constrs: ConstraintSymbol imarray) =
    if not constrs.IsEmpty && acenv.checkedTypeParameters.Add(tyPar.Id) then
        match syntaxConstrClause with
        | OlySyntaxConstraintClause.ConstraintClause(_, _, _, syntaxConstrsList) ->
            let syntaxConstrs = syntaxConstrsList.ChildrenOfType
            if syntaxConstrs.Length = constrs.Length then 
                (syntaxConstrs, constrs)
                ||> ImArray.iter2 (fun syntaxConstr constr ->
                    match constr with
                    | ConstraintSymbol.SubtypeOf(lazyTy) ->
                        analyzeType acenv aenv syntaxConstr lazyTy.Value
                    | ConstraintSymbol.ConstantType(lazyTy) ->
                        analyzeType acenv aenv syntaxConstr lazyTy.Value
                    | ConstraintSymbol.TraitType(lazyTy) ->
                        analyzeType acenv aenv syntaxConstr lazyTy.Value
                    | _ ->
                        ()
                )
        | _ ->
            ()

and analyzeTypeForParameter acenv aenv (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) when tyPar.IsVariadic ->
        acenv.cenv.diagnostics.Error($"Invalid use of variadic type parameter.", 10, syntaxNode)
    | _ ->
        analyzeTypePermitByRef acenv aenv syntaxNode ty

and analyzeTypeArgumentsWithSyntax acenv aenv syntaxNode (syntaxTyArgs: OlySyntaxType imarray) (tyArgs: TypeSymbol imarray) =
    if syntaxTyArgs.Length <> tyArgs.Length then
        tyArgs |> ImArray.iter (analyzeType acenv aenv syntaxNode)
    else
        (syntaxTyArgs, tyArgs)
        ||> ImArray.iter2 (fun syntaxTy tyArg ->
            analyzeType acenv aenv syntaxTy tyArg
        )

and analyzeTypeArgumentsWithSyntaxTuple acenv aenv syntaxNode (syntaxTupleElements: OlySyntaxTupleElement imarray) (tyArgs: TypeSymbol imarray) =
    if syntaxTupleElements.Length <> tyArgs.Length then
         tyArgs |> ImArray.iter (analyzeType acenv aenv syntaxNode)
    else
        (syntaxTupleElements, tyArgs)
        ||> ImArray.iter2 (fun syntaxTupleElement tyArg ->
            match syntaxTupleElement with
            | OlySyntaxTupleElement.Type(syntaxTy)
            | OlySyntaxTupleElement.IdentifierWithTypeAnnotation(_, _, syntaxTy) ->
                analyzeType acenv aenv syntaxTy tyArg
            | _ ->
                analyzeType acenv aenv syntaxTupleElement tyArg
        )

and analyzeTypeEntityAccessibility acenv aenv syntaxNode (ent: EntitySymbol) =
    let isAccessible =
        if aenv.isMemberSig then
            let flags = ent.Flags &&& EntityFlags.AccessorMask
            let memberFlags = aenv.memberFlags &&& MemberFlags.AccessorMask

            match flags, memberFlags with
            | EntityFlags.Private, MemberFlags.Internal
            | EntityFlags.Private, MemberFlags.Protected
            | EntityFlags.Private, MemberFlags.Public -> 
                match aenv.benv.ac.Entity with
                | Some ent1 -> 
                    if ent1.IsPrivate then
                        true
                    else
                        areEntitiesEqual ent1 ent
                | _ -> 
                    false

            | EntityFlags.Internal, MemberFlags.Protected
            | EntityFlags.Internal, MemberFlags.Public ->
                match aenv.benv.ac.Entity with
                | Some ent1 -> 
                    if ent1.IsInternal || ent1.IsPrivate then
                        true
                    else
                        areEntitiesEqual ent1 ent
                | _ ->
                    false

            | _, _ ->
                true
        else
            true
        
    if not isAccessible then
        acenv.cenv.diagnostics.Error($"'{printEntity aenv.benv ent}' is less accessible than the member its used in.", 10, syntaxNode)

and analyzeTypeEntity acenv aenv (syntaxNode: OlySyntaxNode) (ent: EntitySymbol) =   
    analyzeTypeEntityAccessibility acenv aenv syntaxNode ent

    let cont() =
        if not ent.IsFormal then
            if not ent.IsExported && not ent.IsImported then
                ent.TypeArguments |> ImArray.iter (analyzeTypeRestrictTypeParameterUse acenv aenv syntaxNode)
            else
                ent.TypeArguments |> ImArray.iter (analyzeType acenv aenv syntaxNode)

    let rec check (syntaxName: OlySyntaxName) =
        match syntaxName with
        | OlySyntaxName.Qualified(_, _, syntaxName) ->
            check syntaxName
        | OlySyntaxName.Generic(_, syntaxTyArgs) ->
            match syntaxTyArgs with
            | OlySyntaxTypeArguments.TypeArguments(_, syntaxTyArgList, _) ->
                analyzeTypeArgumentsWithSyntax acenv aenv syntaxNode syntaxTyArgList.ChildrenOfType ent.TypeArguments
            | _ ->
                cont()
        | _ ->
            cont()

    match syntaxNode with
    | :? OlySyntaxType as syntaxTy ->
        match syntaxTy with
        | OlySyntaxType.Name(syntaxName) ->
            check syntaxName
        | _ ->
            cont()
    | _ ->
        cont()

and analyzeTypeTuple acenv aenv (syntaxNode: OlySyntaxNode) (tyArgs: TypeSymbol imarray) =
    let cont() =
         tyArgs |> ImArray.iter (analyzeType acenv aenv syntaxNode)

    match syntaxNode with
    | :? OlySyntaxType as syntaxTy ->
        match syntaxTy with
        | OlySyntaxType.Tuple(_, syntaxTupleElementList, _) ->
            analyzeTypeArgumentsWithSyntaxTuple acenv aenv syntaxNode syntaxTupleElementList.ChildrenOfType tyArgs
        | _ ->
            cont()
    | _ ->
        cont()

and checkWitness acenv aenv (syntaxNode: OlySyntaxNode) (witness: WitnessSymbol) =
    match witness with
    | WitnessSymbol.TypeExtension(tyExt, specificAbstractFuncOpt) ->
        checkEntity acenv aenv syntaxNode tyExt
        specificAbstractFuncOpt
        |> Option.iter (fun x -> checkValue acenv aenv syntaxNode x)

    // TODO: Do we need to check these?
    | WitnessSymbol.TypeParameter _
    | WitnessSymbol.Type _ ->
        ()

and checkWitnessSolution acenv aenv (syntaxNode: OlySyntaxNode) (witness: WitnessSolution) =
    OlyAssert.True(witness.HasSolution)
    checkEntity acenv aenv syntaxNode witness.Entity
    match witness.Solution with
    | Some witness -> checkWitness acenv aenv syntaxNode witness
    | _ -> ()

and checkEntity acenv aenv syntaxNode (ent: EntitySymbol) =
    ent.TypeArguments
    |> ImArray.iter (analyzeType acenv aenv syntaxNode)

and checkEnclosing acenv aenv syntaxNode (enclosing: EnclosingSymbol) =
    match enclosing with
    | EnclosingSymbol.Entity(ent) ->
        checkEntity acenv aenv syntaxNode ent
    | EnclosingSymbol.Witness(concreteTy, abstractEnt) ->
        analyzeType acenv aenv syntaxNode concreteTy
        checkEntity acenv aenv syntaxNode abstractEnt
    | EnclosingSymbol.Local
    | EnclosingSymbol.RootNamespace ->
        ()

and checkPattern acenv aenv syntaxNode (pat: IPatternSymbol) =
    checkEnclosing acenv aenv syntaxNode pat.Enclosing
    checkValue acenv aenv syntaxNode pat.PatternFunction
    pat.PatternGuardFunction |> Option.iter (fun func -> checkValue acenv aenv syntaxNode func)

and checkValue acenv aenv syntaxNode (value: IValueSymbol) =
    if value.IsBase then
        if not value.IsFunction then
            acenv.cenv.diagnostics.Error($"Improper use of 'base'.", 0, syntaxNode)
    elif value.IsPattern then
        checkPattern acenv aenv syntaxNode (value :?> IPatternSymbol)
    else
        checkEnclosing acenv aenv syntaxNode value.Enclosing

        let isVanilla = value.IsVanilla

        // TODO: We need to use the syntaxNode to get access to the type arguments if they exists, parameters, and return type.
        if value.IsFunction then
            let func = value.AsFunction
            match func.TryWellKnownFunction with
            | ValueSome(WellKnownFunction.LoadNullPtr)
            | ValueSome(WellKnownFunction.UnsafeCast) ->

                match func.TryWellKnownFunction with
                | ValueSome(WellKnownFunction.LoadNullPtr) ->
                    func.TypeArguments
                    |> ImArray.iter (fun tyArg ->
                        if not tyArg.IsVoid_t then
                            analyzeType acenv aenv syntaxNode tyArg
                    )

                | ValueSome(WellKnownFunction.UnsafeCast) ->
                    func.TypeArguments
                    |> ImArray.iter (fun tyArg ->
                        if not tyArg.IsVoid_t then
                            analyzeTypePermitByRef acenv aenv syntaxNode tyArg
                    )

                | _ ->
                    ()

                match stripTypeEquations func.ReturnType with
                | TypeSymbol.NativePtr(elementTy) when elementTy.IsVoid_t -> ()
                | _ ->
                    analyzeTypeForParameter acenv aenv syntaxNode func.ReturnType
            | _ ->
                let tyPars = func.TypeParameters
                func.TypeArguments
                |> ImArray.iteri (fun i tyArg -> 
                    let isScoped =
                        if i < tyPars.Length then
                            tyPars[i].Constraints
                            |> ImArray.exists (function ConstraintSymbol.Scoped -> true | _ -> false)
                        else
                            false
                    if isScoped then
                        if isVanilla then
                            analyzeTypePermitByRefAndRestrictTypeParameterUse acenv aenv syntaxNode tyArg
                        else
                            analyzeTypePermitByRef acenv aenv syntaxNode tyArg
                    else
                        if isVanilla then
                            analyzeTypeRestrictTypeParameterUse acenv aenv syntaxNode tyArg
                        else
                            analyzeType acenv aenv syntaxNode tyArg
                )
                func.Parameters
                |> ImArray.iter (fun par ->
                    analyzeTypeForParameter acenv aenv syntaxNode par.Type
                )
                analyzeTypeForParameter acenv aenv syntaxNode func.ReturnType
                match func with
                | :? FunctionGroupSymbol as funcGroup ->
                    acenv.cenv.diagnostics.Error(sprintf "'%s' has ambiguous functions." funcGroup.Name, 0, syntaxNode)
                | _ ->
                    ()
        else
            value.TypeArguments
            |> ImArray.iter (fun tyArg -> 
                if isVanilla then
                    analyzeTypePermitByRefAndRestrictTypeParameterUse acenv aenv syntaxNode tyArg
                else
                    analyzeTypePermitByRef acenv aenv syntaxNode tyArg
            )
            analyzeTypePermitByRef acenv aenv syntaxNode value.Type            

let handleLambda acenv aenv (lambdaFlags: LambdaFlags) (pars: ILocalParameterSymbol imarray) =
    let aenv = 
        if lambdaFlags.HasFlag(LambdaFlags.Scoped) then
            aenv
        else
            { aenv with scopeLambda = aenv.scopeLambda + 1 }

    pars
    |> ImArray.iter (fun par ->
        acenv.scopes[par.Id] <- { Value = aenv.scope; ValueLambda = aenv.scopeLambda; Limits = ScopeLimits.None }
    )

    aenv

let rec analyzeBindingInfo acenv (aenv: aenv) (syntaxNode: OlySyntaxNode) (rhsExprOpt: E voption) (value: IValueSymbol) =
    let scopeResult =
        match rhsExprOpt with
        | ValueSome(E.Lambda(flags=lambdaFlags;pars=pars;body=lazyBodyExpr)) when value.IsFunction ->
            let aenv = handleLambda acenv aenv lambdaFlags pars

            OlyAssert.True(lazyBodyExpr.HasExpression)

            // Context Analysis: UnmanagedAllocationOnly
            if aenv.IsInUnmanagedAllocationOnlyContext then
                if value.IsLocal then
                    if not value.IsStaticLocalFunction || not value.IsUnmanagedAllocationOnly then
                        reportUnmanagedAllocationOnly acenv syntaxNode

            let limits =
                if value.IsUnmanagedAllocationOnly then
                    aenv.limits ||| Limits.UnmanagedAllocationOnly
                else
                    aenv.limits

            if value.IsLocal then
                analyzeExpression acenv { aenv with scope = aenv.scope + 1; isLastExprOfScope = true; isReturnable = true; limits = limits; currentFunctionOpt = Some value.AsFunction } lazyBodyExpr.Expression |> ignore
            else
                analyzeExpression acenv { aenv with scope = aenv.scope + 1; isLastExprOfScope = true; isReturnable = true; limits = limits; currentNonLocalFunctionOpt = Some value.AsFunction; currentFunctionOpt = Some value.AsFunction } lazyBodyExpr.Expression |> ignore
            { ScopeValue = aenv.scope; ScopeLimits = ScopeLimits.None }

        | ValueSome(rhsExpr) ->
            if value.IsLocal then
                analyzeExpressionWithType acenv { aenv with scope = aenv.scope + 1; isReturnable = false; isLastExprOfScope = true } rhsExpr value.Type
            else
                analyzeExpression acenv { aenv with scope = aenv.scope + 1; isReturnable = false; isLastExprOfScope = true } rhsExpr

        | _ ->
            { ScopeValue = aenv.scope; ScopeLimits = ScopeLimits.None }

    let aenv = 
        if value.Enclosing.IsLocalEnclosing then
            aenv
        else
            { aenv with isMemberSig = true }

    if value.IsLocal && not(value.IsFunction) && not value.IsGenerated then
        acenv.scopes[value.Id] <- { Value = aenv.scope; ValueLambda = aenv.scopeLambda; Limits = scopeResult.ScopeLimits }

    let checkValueTy () =
        match value.LogicalType.TryGetFunctionWithParameters() with
        | ValueSome(argTys, returnTy) ->
            argTys
            |> ImArray.iter (analyzeTypeForParameter acenv aenv syntaxNode)
            analyzeTypeForParameter acenv aenv syntaxNode returnTy
        | _ ->
            analyzeTypeForParameter acenv aenv syntaxNode value.Type

    match value with
    | :? IFunctionSymbol as func ->

        func.Attributes |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)

        match syntaxNode with
        | :? OlySyntaxExpression as syntaxExpr ->
            match syntaxExpr with
            | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, _, _, _, syntaxBinding) ->
                match syntaxBinding with
                | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _)
                | OlySyntaxBinding.Signature(syntaxBindingDecl) ->
                    match syntaxBindingDecl with
                    | OlySyntaxBindingDeclaration.Function(_, _, syntaxPars, syntaxReturnTyAnnot, syntaxConstrClauseList) ->

                        // Check type parameters
                        // --------------------------------------------------------
                        let syntaxConstrClauses = syntaxConstrClauseList.ChildrenOfType
            
                        forEachConstraintBySyntaxConstraintClause 
                            syntaxConstrClauses 
                            func.TypeParameters 
                            (fun syntaxConstrClause tyPar constrs ->                          
                                analyzeTypeParameterDefinition acenv aenv syntaxConstrClause tyPar constrs
                            )
                        // --------------------------------------------------------

                        let syntaxReturnTy =
                            match syntaxReturnTyAnnot with
                            | OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy) -> syntaxTy :> OlySyntaxNode
                            | _ -> syntaxNode
                        match syntaxPars with
                        | OlySyntaxParameters.Parameters(_, syntaxParList, _) ->
                            (syntaxParList.ChildrenOfType.AsMemory(), func.LogicalParameters)
                            ||> ROMem.tryIter2 (fun syntaxPar par ->
                                par.Attributes |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
                                match syntaxPar with
                                | OlySyntaxParameter.Pattern(_, _, _, _, syntaxTy)
                                | OlySyntaxParameter.Type(_, syntaxTy) ->
                                    analyzeTypeForParameter acenv aenv syntaxTy par.Type
                                | _ ->
                                    analyzeTypeForParameter acenv aenv syntaxPar par.Type
                            )
                            analyzeTypeForParameter acenv aenv syntaxReturnTy func.ReturnType
                        | OlySyntaxParameters.Empty _ ->
                            analyzeTypeForParameter acenv aenv syntaxReturnTy func.ReturnType
                        | _ ->
                            checkValueTy()
                    | _ ->
                        checkValueTy()
                | _ ->
                    checkValueTy()
            | _ ->
                checkValueTy()
        | _ ->
            checkValueTy()

    | :? IFieldSymbol as field ->

        let defaultCheck() =
            field.Attributes |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
            checkValueTy()

        match syntaxNode with
        | :? OlySyntaxExpression as syntaxExpr ->
            match syntaxExpr with
            | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, _, _, _, syntaxBinding) ->
                match syntaxBinding with             
                | OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Value(_, OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy)))
                | OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Value(_, OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy)), _, _) ->
                    let syntaxAttrs = syntaxAttrs.Values
                    for i = 0 to field.Attributes.Length - 1 do
                        let syntaxNode =
                            if i < syntaxAttrs.Length then
                                syntaxAttrs[i] :> OlySyntaxNode
                            else
                                syntaxBinding :> OlySyntaxNode
                        analyzeAttribute acenv aenv syntaxNode field.Attributes[i]
                    analyzeType acenv aenv syntaxTy field.Type
                | _ ->
                    defaultCheck()
            | _ ->
                defaultCheck()
        | _ ->
            defaultCheck()

    | :? IPropertySymbol as prop ->

        let defaultCheck() =
            prop.Attributes |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
            checkValueTy()

        match syntaxNode with
        | :? OlySyntaxExpression as syntaxExpr ->
            match syntaxExpr with
            | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, _, _, _, syntaxBinding) ->
                match syntaxBinding with             
                | OlySyntaxBinding.Property(OlySyntaxBindingDeclaration.Value(_, OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy)), _)
                | OlySyntaxBinding.PropertyWithDefault(OlySyntaxBindingDeclaration.Value(_, OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy)), _, _, _) ->
                    let syntaxAttrs = syntaxAttrs.Values
                    for i = 0 to prop.Attributes.Length - 1 do
                        let syntaxNode =
                            if i < syntaxAttrs.Length then
                                syntaxAttrs[i] :> OlySyntaxNode
                            else
                                syntaxBinding :> OlySyntaxNode
                        analyzeAttribute acenv aenv syntaxNode prop.Attributes[i]
                    analyzeTypePermitByRef acenv aenv syntaxTy prop.Type
                | _ ->
                    defaultCheck()
            | _ ->
                defaultCheck()
        | _ ->
            defaultCheck()

    | :? IPatternSymbol as pat ->
        pat.Attributes |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
        checkValueTy()

    | _ ->
        checkValueTy()

and analyzeBinding acenv aenv syntaxNode (binding: BoundBinding) =
    analyzeBindingInfo acenv aenv syntaxNode binding.TryExpression binding.Info.Value

and analyzePattern acenv aenv (pat: BoundCasePattern) =
    let syntaxNode = pat.Syntax

    match pat with
    | BoundCasePattern.Function(_,  pat, witnessArgs, pats) ->
        checkValue acenv aenv syntaxNode pat
        witnessArgs
        |> ImArray.iter (checkWitnessSolution acenv aenv syntaxNode)
        pats
        |> ImArray.iter (analyzePattern acenv aenv)

    | BoundCasePattern.Tuple(_, pats) ->
        pats
        |> ImArray.iter (analyzePattern acenv aenv)

    | BoundCasePattern.Local(_, value) ->
        checkValue acenv aenv syntaxNode value

    | BoundCasePattern.FieldConstant(_, field) ->
        checkValue acenv aenv syntaxNode field

    | BoundCasePattern.Literal(_, literal) ->
        analyzeLiteral acenv aenv syntaxNode literal

    | BoundCasePattern.Discard _ ->
        ()

and analyzeMatchPattern acenv aenv (matchPat: BoundMatchPattern) =
    match matchPat with
    | BoundMatchPattern.Cases(_, pats) ->
        pats |> ImArray.iter (analyzePattern acenv aenv)
    | BoundMatchPattern.Or(_, lhsMatchPat, rhsMatchPat) ->
        analyzeMatchPattern acenv aenv lhsMatchPat
        analyzeMatchPattern acenv aenv rhsMatchPat

and analyzeLiteral acenv aenv (syntaxNode: OlySyntaxNode) (literal: BoundLiteral) =
    let diagnostics = acenv.cenv.diagnostics
    let benv = aenv.envRoot.benv

    match literal with
    | BoundLiteral.NumberInference(lazyLiteral, ty) ->
        // TODO: Consider we must always have the 'lazyLiteral' evaluated at this point.
        //       Maybe use 'OlyAssert.True(lazyLiteral.IsValueCreated)'?
        tryEvaluateLazyLiteral diagnostics lazyLiteral
        |> ignore
        analyzeType acenv aenv syntaxNode ty

    | BoundLiteral.NullInference(ty) ->
        if not ty.IsNullable && ty.IsSolved then
            diagnostics.Error($"'null' is not allowed for '{printType benv ty}'.", 10, syntaxNode)
    | BoundLiteral.DefaultInference(ty, isUnchecked) ->
        if not ty.IsAnyStruct && not ty.IsNullable && ty.IsSolved && not isUnchecked then
            diagnostics.Error($"'default' is not allowed for '{printType benv ty}' as it could be null.", 10, syntaxNode)

    | _ ->
        // Context Analysis: UnmanagedAllocationOnly
        if aenv.IsInUnmanagedAllocationOnlyContext then
            match literal with
            | BoundLiteral.ConstantEnum(_, enumTy) when not (enumTy.IsUnmanaged(PostInferenceAnalysis)) ->
                reportUnmanagedAllocationOnly acenv syntaxNode
            | BoundLiteral.Constant(cns) when not (cns.Type.IsUnmanaged(PostInferenceAnalysis)) ->
                reportUnmanagedAllocationOnly acenv syntaxNode
            | _ ->
                ()
    analyzeType acenv aenv syntaxNode literal.Type

and analyzeConstant acenv aenv (syntaxNode: OlySyntaxNode) (constant: ConstantSymbol) =
    match constant with
    | ConstantSymbol.Array(elementTy, elements) ->
        analyzeType acenv aenv syntaxNode elementTy
        elements |> ImArray.iter (analyzeConstant acenv aenv syntaxNode)

    | ConstantSymbol.External(func) ->
        checkValue acenv aenv syntaxNode func

    | ConstantSymbol.TypeVariable(tyPar) ->
        analyzeType acenv aenv syntaxNode tyPar.AsType

    | _ ->
        ()

and analyzeAttribute acenv aenv (syntaxNode: OlySyntaxNode) (attr: AttributeSymbol) =
    // No limits when analyzing attributes.
    let aenv = if aenv.limits = Limits.None then aenv else { aenv with limits = Limits.None }

    match attr with
    | AttributeSymbol.Constructor(ctor, args, namedArgs, _) ->
        checkValue acenv  aenv syntaxNode ctor
        args |> ImArray.iter (analyzeConstant acenv aenv syntaxNode)
        namedArgs
        |> ImArray.iter (function
            | AttributeNamedArgumentSymbol.Field(field, constant) ->
                checkValue acenv aenv syntaxNode field
                analyzeConstant acenv aenv syntaxNode constant
            | AttributeNamedArgumentSymbol.Property(prop, constant) ->
                checkValue acenv aenv syntaxNode prop
                analyzeConstant acenv aenv syntaxNode constant
        )

    | _ ->
        ()

and analyzeAddressOf acenv aenv scopeValue scopeLimits expr =
    let checkScope (syntaxInfo: BoundSyntaxInfo) (value: IValueSymbol) =
        match acenv.scopes.TryGetValue value.Id with
        | true, scope ->
            if aenv.isLastExprOfScope && scope.Value = aenv.scope then
                reportAddressOfValueOutOfScope acenv syntaxInfo.SyntaxNameOrDefault value
        | _ ->
            ()

    let createScopeResult isThisOnly (value: IValueSymbol) =
        let (newScopeValue, newScopeLimits) =
            match acenv.scopes.TryGetValue value.Id with
            | true, scope -> (scope.Value, scope.Limits)
            | _ -> (0, ScopeLimits.None)
        if value.IsParameter then
            if isThisOnly then
                // This prevents taking the address of 'this' and returning it.
                { ScopeValue = newScopeValue + 1; ScopeLimits = newScopeLimits ||| ScopeLimits.StackReferringByRef }
            else
                { ScopeValue = newScopeValue; ScopeLimits = newScopeLimits }
        else
            let newScopeLimits2 =
                if newScopeLimits.HasFlag(ScopeLimits.ByRef) then
                    newScopeLimits
                else
                    newScopeLimits ||| ScopeLimits.StackReferringByRef
            { ScopeValue = newScopeValue; ScopeLimits = newScopeLimits2 }

    let createScopeResultForReceiver (value: IValueSymbol) =
        let (newScopeValue, newScopeLimits) =
            match acenv.scopes.TryGetValue value.Id with
            | true, scope -> (scope.Value, scope.Limits)
            | _ -> (0, ScopeLimits.None)
        let newScopeLimits2 =
            if newScopeLimits.HasFlag(ScopeLimits.StackReferring) |> not then
                ScopeLimits.ByRef
            else
                newScopeLimits ||| ScopeLimits.ByRef
        { ScopeValue = newScopeValue; ScopeLimits = newScopeLimits2 }

    // Context Analysis: byref/byref-like
    match expr with
    | AddressOf(AutoDereferenced(expr2)) -> 
        match expr2 with
        | E.Value(value=value) ->
            createScopeResult value.IsThis value
        | E.GetField(receiver=AddressOf(E.Value(value=value))) ->
            createScopeResult false value
        | _ ->
            analyzeExpression acenv (aenv |> notLastExprOfScope) expr2

    | AddressOf(E.Value(value=value)) ->
        createScopeResult value.IsThis value
    | AddressOf(E.GetField(receiver=AddressOf(E.Value(value=value)))) ->
        createScopeResult false value

    | AddressOf(E.GetField(receiver=E.Value(syntaxInfo, value))) when value.Type.IsScoped ->
        if value.Type.IsByRef_t then
            createScopeResultForReceiver value
        else
            // TODO: What is this doing here again?
            checkScope syntaxInfo value
            { ScopeValue = scopeValue; ScopeLimits = ScopeLimits.None }

    | _ ->
        { ScopeValue = scopeValue; ScopeLimits = scopeLimits }

and analyzeExpressionWithType acenv (aenv: aenv) (expr: E) (expectedTy: TypeSymbol) =
    analyzeExpressionWithTypeAux acenv aenv expr false expectedTy
    analyzeExpression acenv aenv expr

and analyzeReceiverExpressionWithType acenv (aenv: aenv) (expr: E) (expectedTy: TypeSymbol) =
    analyzeExpressionWithTypeAux acenv aenv expr true expectedTy
    analyzeExpression acenv aenv expr

and analyzeExpressionWithTypeAux acenv (aenv: aenv) (expr: E) (isReceiver: bool) (expectedTy: TypeSymbol) =
    let exprTy = expr.Type
    let willBox = 
        if exprTy.IsScoped then
            not isReceiver && expectedTy.IsAnyNonStruct && not expectedTy.IsScoped
        else
            (exprTy.IsAnyStruct || exprTy.IsAnyTypeVariableWithoutStructOrUnmanagedOrNotStructConstraint) && expectedTy.IsAnyNonStruct

    // Context Analysis: UnmanagedAllocationOnly
    if willBox && aenv.IsInUnmanagedAllocationOnlyContext then
        reportUnmanagedAllocationOnlyBoxing acenv expr.Syntax.BestSyntaxForReporting

    if willBox && exprTy.IsScoped then
        reportScopedTypeBoxing acenv aenv.benv exprTy expr.Syntax.BestSyntaxForReporting

and analyzeExpression acenv aenv (expr: E) : ScopeResult =
    if aenv.isReturnable then
        match aenv.currentFunctionOpt with
        | Some func ->
            match expr with
            // Skip these expressions as we will check their bodies.
            | E.Try _
            | E.IfElse _
            | E.Match _
            | E.While _ 
            | E.Sequential _ 
            | E.Let _ ->
                ()
            | _ ->
                analyzeExpressionWithTypeAux acenv aenv expr (* isReceiver *) false func.ReturnType
            analyzeExpressionAux acenv aenv expr
        | _ ->
            analyzeExpressionAux acenv aenv expr
    else
        analyzeExpressionAux acenv aenv expr

#if DEBUG || CHECKED
and analyzeExpressionAux acenv aenv (expr: E) : ScopeResult =
    StackGuard.Do(fun () ->
        analyzeExpressionAuxAux acenv aenv expr
    )
and analyzeExpressionAuxAux acenv aenv (expr: E) : ScopeResult =
#else
and analyzeExpressionAux acenv aenv (expr: E) : ScopeResult =
#endif
    acenv.cenv.ct.ThrowIfCancellationRequested()

    let syntaxNode = expr.Syntax
    match expr with
    | E.Value(syntaxInfo, value) ->
        if value.Type.IsScoped then
            match acenv.scopes.TryGetValue value.Id with
            | true, scope ->
                if aenv.isLastExprOfScope && scope.Value = aenv.scope then
                    reportAddressOfValueOutOfScope acenv syntaxInfo.SyntaxNameOrDefault value

                if scope.ValueLambda < aenv.scopeLambda then
                    reportAddressValueCannotBeCaptured acenv syntaxInfo.SyntaxNameOrDefault value

            | _ ->
                ()

            checkValue acenv aenv syntaxNode value

            { ScopeValue = 0; ScopeLimits = ScopeLimits.None }
        else
            match syntaxInfo.TrySyntaxName with
            | Some(syntaxName) ->
                checkValue acenv aenv syntaxName value
            | _ ->
                checkValue acenv aenv syntaxNode value

            { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.IfElse(_, conditionExpr, trueTargetExpr, falseTargetExpr, exprTy) ->
        analyzeTypePermitByRef acenv aenv syntaxNode exprTy

        let aenvConditionExpr = { aenv with scope = aenv.scope + 1; isReturnable = false; isLastExprOfScope = true }
        analyzeExpression acenv aenvConditionExpr conditionExpr |> ignore

        analyzeExpression acenv aenv trueTargetExpr |> ignore
        analyzeExpression acenv aenv falseTargetExpr |> ignore
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.Match(_, _, matchExprs, matchClauses, exprTy) ->
        analyzeTypePermitByRef acenv aenv syntaxNode exprTy

        let aenvMatchExpr = { aenv with scope = aenv.scope + 1; isReturnable = false; isLastExprOfScope = true }
        matchExprs
        |> ImArray.iter (analyzeExpression acenv aenvMatchExpr >> ignore)

        matchClauses
        |> ImArray.iter (function
            | BoundMatchClause.MatchClause(_, matchPat, conditionExprOpt, targetExpr) ->
                analyzeMatchPattern acenv aenv matchPat

                match conditionExprOpt with
                | Some(conditionExpr) ->
                    let aenvConditionExpr = { aenv with scope = aenv.scope + 1; isReturnable = false; isLastExprOfScope = true }
                    analyzeExpression acenv aenvConditionExpr conditionExpr |> ignore
                | _ ->
                    ()

                analyzeExpression acenv aenv targetExpr |> ignore
        )
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.While(_, conditionExpr, bodyExpr) ->
        let aenvConditionExpr = { aenv with scope = aenv.scope + 1; isReturnable = false; isLastExprOfScope = true }
        analyzeExpression acenv aenvConditionExpr conditionExpr |> ignore
        analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) bodyExpr |> ignore
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.Try(_, bodyExpr, catchCases, finallyBodyExprOpt) ->
        analyzeExpression acenv aenv bodyExpr |> ignore

        catchCases
        |> ImArray.iter (function
            | BoundCatchCase.CatchCase(_, value, catchBodyExpr) ->
                // TODO: 'syntaxNode' is not the accurate place for this.
                analyzeType acenv aenv syntaxNode value.Type
                analyzeExpression acenv aenv catchBodyExpr |> ignore
        )

        finallyBodyExprOpt
        |> Option.iter (fun finallyBodyExpr ->
            analyzeExpression acenv (notReturnable aenv) finallyBodyExpr |> ignore
        )
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.Witness(_, benv, _, bodyExpr, witnessArgOptRef, exprTy) ->
        OlyAssert.True(witnessArgOptRef.contents.IsNone)

        let bodyTy = bodyExpr.Type   
        if subsumesTypeWith Generalizable exprTy bodyTy then
            checkSubsumesType (SolverEnvironment.Create(acenv.cenv.diagnostics, benv, PostInferenceAnalysis)) bodyExpr.Syntax exprTy bodyTy
        else
            match tryFindTypeHasTypeExtensionImplementedType benv exprTy bodyTy with
            | ValueSome entSet when entSet.Count > 0 ->
                let ents = entSet.Values |> ImArray.ofSeq
                if ents.Length = 1 then
                    let ent = ents[0].SubstituteExtension(bodyTy.TypeArguments)
                    analyzeTypeEntity acenv aenv expr.Syntax ent
                    witnessArgOptRef.contents <- Some(ent.AsType)
                else
                    acenv.cenv.diagnostics.Error($"Ambiguous extensions. Unable to upcast type '{printType benv bodyTy}' to '{printType benv exprTy}.", 10, expr.Syntax)
            | _ ->
                acenv.cenv.diagnostics.Error($"Unable to upcast type '{printType benv bodyTy}' to '{printType benv exprTy}.", 10, expr.Syntax)

        analyzeExpression acenv aenv bodyExpr |> ignore
        analyzeType acenv aenv syntaxNode exprTy
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.ErrorWithNamespace _
    | E.ErrorWithType _ ->
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.NewTuple(_, argExprs, exprTy) ->
        // Context Analysis: UnmanagedAllocationOnly
        if aenv.IsInUnmanagedAllocationOnlyContext then
            reportUnmanagedAllocationOnly acenv syntaxNode

        let itemTys =
            match exprTy.TryGetTupleItemTypes() with
            | ValueSome(itemTys) -> itemTys
            | _ -> ImArray.empty         

        argExprs 
        |> ImArray.iteri (fun i argExpr -> 
            let itemTy =
                if i < itemTys.Length then
                    itemTys[i]
                else
                    TypeSymbol.Error(None, None)
            analyzeExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) argExpr itemTy |> ignore
        )

        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.NewArray(_, _, argExprs, exprTy) ->
        // Context Analysis: UnmanagedAllocationOnly
        if aenv.IsInUnmanagedAllocationOnlyContext then
            reportUnmanagedAllocationOnly acenv syntaxNode

        let elementTy =
            match exprTy.TryGetArrayElementType() with
            | ValueSome(elementTy) -> elementTy
            | _ -> TypeSymbol.Error(None, None)

        argExprs
        |> ImArray.iter (fun argExpr -> 
            analyzeExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) argExpr elementTy |> ignore
        )
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.Typed(body=bodyExpr;ty=exprTy) -> 
        analyzeExpressionWithType acenv aenv bodyExpr exprTy

    | E.Call(syntaxInfo, receiverArgExprOpt, witnessArgs, logicalArgExprs, value, _) ->
        // Note: Constraints can retry solving in this analysis.
        //       However, overloads of a call cannot retry solving, except in constraints.
        // REVIEW: In the future, consider allowing overloads of a call to retry solving.
        //         What are the pitfalls? It would make analysis more than just analysis, but doesn't retrying to solve constraints also mean that?

        // Re-check constraints
        checkConstraintsFromCallExpression acenv.cenv.diagnostics false PostInferenceAnalysis expr

        if not value.IsFunctionGroup then
            witnessArgs
            |> ImArray.iter (fun x ->
                checkWitnessSolution acenv aenv syntaxNode x
            )

            // Context Analysis: UnmanagedAllocationOnly
            if aenv.IsInUnmanagedAllocationOnlyContext && not value.IsUnmanagedAllocationOnly then
                reportUnmanagedAllocationOnly acenv syntaxInfo.Syntax

            // Scope lambda call
            if value.Type.IsScoped then
                match acenv.scopes.TryGetValue(value.Formal.Id) with
                | true, scope ->
                    if scope.ValueLambda < aenv.scopeLambda then
                        acenv.cenv.diagnostics.Error("Value cannot be captured.", 10, syntaxInfo.SyntaxNameOrDefault)
                | _ ->
                    ()
                
        let argCount =
            match receiverArgExprOpt with
            | Some _ -> logicalArgExprs.Length + 1
            | _ -> logicalArgExprs.Length

        let isValidCall = not(value.IsFunctionGroup || (value.Type.FunctionParameterCount <> argCount))

        let scopeResult =
            if isValidCall then
                if witnessArgs.IsEmpty |> not then
                    match value.Formal.Type.TryGetFunctionWithParameters() with
                    | ValueSome(parTys, returnTy) ->               
                        let rec check (ty: TypeSymbol) =
                            let tyArgs = ty.TypeArguments
                            let tyPars = ty.TypeParameters
                            if tyArgs.Length = tyPars.Length then
                                (tyArgs, tyPars)
                                ||> ImArray.iter2 (fun tyArg tyPar ->
                                    let exists =
                                        tyPar.Constraints
                                        |> ImArray.exists (fun x ->
                                            match x with
                                            | ConstraintSymbol.TraitType(constrTy) ->
                                                let constrTy = constrTy.Value
                                                witnessArgs
                                                |> ImArray.exists (fun x ->
                                                    match x.Solution with
                                                    | Some(WitnessSymbol.TypeExtension(tyExt, _)) ->
                                                        subsumesType constrTy tyExt.AsType
                                                    | _ ->
                                                        false
                                                )
                                            | _ ->
                                                false
                                        )
                                    check tyArg
                                    if exists then
                                        acenv.cenv.diagnostics.Error("Witnesses are escaping the scope. (TODO: better error message)", 10, syntaxInfo.Syntax.BestSyntaxForReporting)
                                )

                        parTys
                        |> ImArray.iter check

                        check returnTy
                    | _ ->
                        ()
                match value.Type.TryGetFunctionWithParameters() with
                | ValueSome(parTys, _) ->
                    let mutable scopeValue = 0
                    let mutable scopeLimits = ScopeLimits.None
                    match receiverArgExprOpt with
                    | Some receiverArgExpr ->
                        // Ignore receiver scope result
                        analyzeReceiverExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) receiverArgExpr parTys[0] |> ignore
                        for i = 1 to parTys.Length - 1 do
                            let result = analyzeExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) logicalArgExprs[i - 1] parTys[i]
                            scopeLimits <- scopeLimits ||| result.ScopeLimits
                            scopeValue <- max scopeValue result.ScopeValue
                    | _ ->
                        for i = 0 to parTys.Length - 1 do
                            let result = analyzeExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) logicalArgExprs[i] parTys[i]
                            scopeLimits <- scopeLimits ||| result.ScopeLimits
                            scopeValue <- max scopeValue result.ScopeValue
                    { ScopeValue = scopeValue; ScopeLimits = scopeLimits }
                | _ -> 
                    { ScopeValue = 0; ScopeLimits = ScopeLimits.None }
            else
                logicalArgExprs 
                |> ImArray.iter (analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) >> ignore)

                receiverArgExprOpt
                |> Option.iter (analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) >> ignore)
                { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

        match syntaxInfo.TrySyntaxName with
        | Some(syntaxName) ->
            checkValue acenv aenv syntaxName value
        | _ ->
            checkValue acenv aenv syntaxNode value

        let scopeResult2 = analyzeAddressOf acenv aenv scopeResult.ScopeValue scopeResult.ScopeLimits expr

        match value.Type.TryFunction with
        | ValueSome(_, returnTy) ->
            if aenv.isLastExprOfScope && returnTy.IsByRef_t && scopeResult2.ScopeLimits.HasFlag(ScopeLimits.StackReferringByRef) then
                if scopeResult2.ScopeValue >= aenv.scope then
                    match expr with
                    | AddressOf(AutoDereferenced(expr2)) ->
                        match expr2 with
                        | E.Value(syntaxInfo, value)
                        | E.GetField(receiver=AddressOf(E.Value(syntaxInfo, value))) ->
                            reportAddressOfValueOutOfScope acenv syntaxInfo.SyntaxNameOrDefault value
                        | _ ->
                            reportExpressionOutOfScope acenv syntaxNode
                    | AddressOf(E.Value(syntaxInfo, value)) 
                    | AddressOf(E.GetField(receiver=AddressOf(E.Value(syntaxInfo, value))))
                    | AddressOf(E.GetField(receiver=E.Value(syntaxInfo, value))) ->
                        reportAddressOfValueOutOfScope acenv syntaxInfo.SyntaxNameOrDefault value
                    | _ ->
                        reportExpressionOutOfScope acenv syntaxNode

            { ScopeValue = scopeResult2.ScopeValue; ScopeLimits = scopeResult2.ScopeLimits }
        | _ ->
            { ScopeValue = scopeResult2.ScopeValue; ScopeLimits = scopeResult2.ScopeLimits }

    | E.None _ ->
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.GetField(receiver=receiver) ->
        analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) receiver |> ignore
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.SetField(_, receiver, field, rhs, _) ->
        analyzeExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) rhs field.Type |> ignore
        analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) receiver |> ignore
        checkValue acenv aenv syntaxNode field
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.GetProperty(receiverOpt=receiverOpt;prop=prop) ->
        receiverOpt
        |> Option.iter (analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) >> ignore)
        checkValue acenv aenv syntaxNode prop
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.SetProperty(receiverOpt=receiverOpt;prop=prop;rhs=rhs) ->
        analyzeExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) rhs prop.Type |> ignore
        receiverOpt
        |> Option.iter (analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) >> ignore)
        checkValue acenv aenv syntaxNode prop
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.SetValue(value=value;rhs=rhs) ->
        analyzeExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) rhs value.Type |> ignore
        checkValue acenv aenv syntaxNode value
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.SetContentsOfAddress(_, lhsExpr, rhsExpr) ->
        analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) lhsExpr |> ignore
        analyzeExpressionWithType acenv (notReturnable aenv |> notLastExprOfScope) rhsExpr (stripByRef lhsExpr.Type) |> ignore
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.Lambda(_, lambdaFlags, _, pars, lazyBodyExpr, lazyTy, _, _) ->
        let aenv = handleLambda acenv aenv lambdaFlags pars

        OlyAssert.True(lazyBodyExpr.HasExpression)
        OlyAssert.True(lazyTy.Type.IsAnyFunction)

        // Context Analysis: UnmanagedAllocationOnly
        if aenv.IsInUnmanagedAllocationOnlyContext then
            reportUnmanagedAllocationOnly acenv syntaxNode

        match lazyTy.Type.TryFunction with
        | ValueSome(_, outputTy) ->
            let syntaxPars =
                match syntaxNode with
                | :? OlySyntaxExpression as syntaxExpr ->
                    match syntaxExpr with
                    | OlySyntaxExpression.Lambda(_, syntaxPars, _, _) ->
                        match syntaxPars with
                        | OlySyntaxParameters.Parameters(_, syntaxParList, _) ->
                            syntaxParList.ChildrenOfType
                        | _ ->
                            ImArray.empty
                    | _ ->
                        ImArray.empty
                | _ ->
                    ImArray.empty

            let syntaxNode =
                // Putting an error against the entire Lambda may not be helpful, so use the body if it has a better one.
                let possibleSyntaxNode = lazyBodyExpr.Expression.Syntax
                if possibleSyntaxNode.IsDummy then
                    syntaxNode
                else
                    possibleSyntaxNode

            if syntaxPars.IsEmpty || (syntaxPars.Length <> pars.Length) then
                pars
                |> ImArray.iter (fun par ->
                    analyzeTypeForParameter acenv aenv syntaxNode par.Type
                )
            else
                (syntaxPars, pars)
                ||> ImArray.iter2 (fun syntaxPar par ->
                    analyzeTypeForParameter acenv aenv syntaxPar par.Type
                )
            analyzeTypeForParameter acenv aenv syntaxNode outputTy
        | _ ->
            ()

        analyzeExpression acenv { aenv with scope = aenv.scope + 1; isReturnable = true; isLastExprOfScope = true } lazyBodyExpr.Expression |> ignore
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.MemberDefinition(_, binding) ->
        let aenv = (notLastExprOfScope aenv)
        let aenv = { aenv with memberFlags = binding.Info.Value.MemberFlags }
        Assert.ThrowIf(binding.Info.Value.IsLocal)
        analyzeBinding acenv aenv expr.Syntax binding
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.Sequential(_, e1, e2, _) ->
        analyzeExpression acenv (notReturnable aenv |> notLastExprOfScope) e1 |> ignore
        analyzeExpression acenv aenv e2

    | E.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
        analyzeBindingInfo acenv aenv syntaxInfo.Syntax (ValueSome rhsExpr) bindingInfo.Value
        analyzeExpression acenv aenv bodyExpr

    | E.Literal(_, literal) ->
        analyzeLiteral acenv aenv syntaxNode literal
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.EntityDefinition(syntaxInfo=syntaxInfo;body=bodyExpr;ent=ent) ->
        let aenv = (notReturnable aenv)

        // Context Analysis: UnmanagedAllocationOnly
        if aenv.IsInUnmanagedAllocationOnlyContext && ent.IsClass then
            reportUnmanagedAllocationOnly acenv syntaxInfo.Syntax

        let aenv = 
            match syntaxInfo.TryEnvironment with
            | Some benv ->
                { aenv with benv = { benv with ac = { benv.ac with Entity = Some ent } } }
            | _ ->
                { aenv with benv = { aenv.benv with ac = { aenv.benv.ac with Entity = Some ent } } }
        ent.Attributes
        |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
        analyzeExpression acenv aenv bodyExpr |> ignore
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }

    | E.Unit _
    | E.Error _ ->
        { ScopeValue = 0; ScopeLimits = ScopeLimits.None }
    
let analyzeRoot acenv aenv (root: BoundRoot) =
    match root with
    | BoundRoot.Namespace(body=bodyExpr)
    | BoundRoot.Global(body=bodyExpr) ->
        analyzeExpression acenv aenv bodyExpr |> ignore

let analyzeBoundTree (cenv: cenv) (env: BinderEnvironment) (tree: BoundTree) =
    let acenv = { cenv = cenv; scopes = System.Collections.Generic.Dictionary(); checkedTypeParameters = System.Collections.Generic.HashSet() }
    let aenv = 
        { 
            envRoot = env 
            scope = 0
            scopeLambda = 0
            isLastExprOfScope = false 
            benv = env.benv 
            isMemberSig = false 
            memberFlags = MemberFlags.None
            limits = Limits.None
            currentNonLocalFunctionOpt = None
            currentFunctionOpt = None
            isReturnable = false
        }
    analyzeRoot acenv aenv tree.Root


