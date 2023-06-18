[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.Binder.PostInferenceAnalysis

open System.Threading
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

type acenv = { cenv: cenv; scopes: System.Collections.Generic.Dictionary<int64, int>; checkedTypeParameters: System.Collections.Generic.HashSet<int64> }
type aenv = { envRoot: BinderEnvironment; scope: int; isReturnableAddress: bool; freeLocals: ReadOnlyFreeLocals; benv: BoundEnvironment; isMemberSig: bool; memberFlags: MemberFlags }

let notReturnableAddress aenv = 
    if aenv.isReturnableAddress then
        { aenv with isReturnableAddress = false }
    else
        aenv

let returnableAddress aenv =
    if aenv.isReturnableAddress then
        aenv
    else
        { aenv with isReturnableAddress = true }

let rec analyzeTypeAux (acenv: acenv) (aenv: aenv) (permitByRef: bool) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
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

    | TypeSymbol.InferenceVariable(_, _)
    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        diagnostics.Error("Unable to infer type at this location.", 5, syntaxNode)
        // We solve the inference variable with an error as to prevent a cascade of the same errors.
        // We only care about it at the earliest possible point.
        UnifyTypes TypeVariableRigidity.Flexible ty TypeSymbolError
        |> ignore

    | TypeSymbol.NativeFunctionPtr(_, inputTy, returnTy)
    | TypeSymbol.Function(inputTy, returnTy) ->
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
        analyzeType acenv aenv syntaxNode innerTy

    | TypeSymbol.Tuple(tyArgs, _) ->
        analyzeTypeTuple acenv aenv syntaxNode tyArgs

    | TypeSymbol.Variable(_) ->
        ()

    | TypeSymbol.HigherVariable(_, tyArgs) ->
        analyzeTypeArguments acenv aenv syntaxNode tyArgs

    | TypeSymbol.NativePtr(elementTy) ->
        if not elementTy.IsVoid_t then
            analyzeType acenv aenv syntaxNode elementTy

    | TypeSymbol.Void ->
        diagnostics.Error($"'{printType benv ty}' can only be used as a type argument of a native pointer.", 10, syntaxNode)

    | TypeSymbol.Error(_, Some msg) ->
        diagnostics.Error(msg, 10, syntaxNode)
    
    | strippedTy ->
        match strippedTy with
        | TypeSymbol.ByRef _ when not permitByRef ->
            match ty with
            | TypeSymbol.InferenceVariable(Some tyPar, _)
            | TypeSymbol.HigherInferenceVariable(Some tyPar, _, _, _) when tyPar.Constraints |> ImArray.exists (function ConstraintSymbol.Scoped -> true | _ -> false) ->
                ()
            | _ ->
                diagnostics.Error($"'{printType benv ty}' not permitted in this context.", 10, syntaxNode)
        | _ ->
            ()
        analyzeTypeArguments acenv aenv syntaxNode ty.TypeArguments

and analyzeType (acenv: acenv) (aenv: aenv) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    analyzeTypeAux acenv aenv false syntaxNode ty

and analyzeTypePermitByRef (acenv: acenv) (aenv: aenv) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    analyzeTypeAux acenv aenv true syntaxNode ty

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

and analyzeTypeArguments acenv aenv syntaxNode (tyArgs: TypeSymbol imarray) =
    tyArgs
    |> ImArray.iter (analyzeType acenv aenv syntaxNode)

and analyzeTypeArgumentsWithSyntax acenv aenv syntaxNode (syntaxTyArgs: OlySyntaxType imarray) (tyArgs: TypeSymbol imarray) =
    if syntaxTyArgs.Length <> tyArgs.Length then
        analyzeTypeArguments acenv aenv syntaxNode tyArgs
    else
        (syntaxTyArgs, tyArgs)
        ||> ImArray.iter2 (fun syntaxTy tyArg ->
            analyzeType acenv aenv syntaxTy tyArg
        )

and analyzeTypeArgumentsWithSyntaxTuple acenv aenv syntaxNode (syntaxTupleElements: OlySyntaxTupleElement imarray) (tyArgs: TypeSymbol imarray) =
    if syntaxTupleElements.Length <> tyArgs.Length then
        analyzeTypeArguments acenv aenv syntaxNode tyArgs
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

    match acenv.scopes.TryGetValue(ent.Formal.Id) with
    | true, scope ->
        () // TODO: We will need scopes at some point right?
    | _ ->
        ()

    let cont() =
        analyzeTypeArguments acenv aenv syntaxNode ent.TypeArguments

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
        analyzeTypeArguments acenv aenv syntaxNode tyArgs

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
  //  OlyAssert.True(witness.HasSolution)
    checkEntity acenv aenv syntaxNode witness.Entity
    match witness.Solution with
    | Some witness -> checkWitness acenv aenv syntaxNode witness
    | _ -> ()

and checkEntity acenv aenv syntaxNode (ent: EntitySymbol) =
    match acenv.scopes.TryGetValue ent.Id with
    | true, scope ->
        ()
    | _ ->
        ()
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
    if value.IsPattern then
        checkPattern acenv aenv syntaxNode (value :?> IPatternSymbol)
    else
        checkEnclosing acenv aenv syntaxNode value.Enclosing

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
                func.TypeArguments
                |> ImArray.iter (fun tyArg -> 
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
                analyzeType acenv aenv syntaxNode tyArg
            )
            analyzeType acenv aenv syntaxNode value.Type

let rec getAddressReturningScope acenv aenv (expr: BoundExpression) =
    expr.GetReturningTargetExpressions()
    |> ImArray.map (fun x ->
        match x with
        | AddressOf(expr) ->
            getAddressReturningScope acenv aenv expr
        | BoundExpression.Value(value=value) ->
            match acenv.scopes.TryGetValue value.Id with
            | true, scope -> scope
            | _ -> -1
        | _ ->
            -1
    )
    |> ImArray.min

let rec analyzeBindingInfo acenv aenv (syntaxNode: OlySyntaxNode) (rhsExprOpt: BoundExpression voption) (value: IValueSymbol) =
    match rhsExprOpt with
    | ValueSome(E.Lambda(body=lazyBodyExpr)) when value.IsFunction ->
        OlyAssert.True(lazyBodyExpr.HasExpression)
        analyzeExpression acenv { aenv with scope = aenv.scope + 1; isReturnableAddress = false } lazyBodyExpr.Expression
    | ValueSome(rhsExpr) ->
        analyzeExpression acenv { aenv with scope = aenv.scope + 1; isReturnableAddress = true } rhsExpr
    | _ ->
        ()

    let aenv = 
        if value.Enclosing.IsLocalEnclosing then
            aenv
        else
            { aenv with isMemberSig = true }

    if value.IsLocal && not(value.IsFunction) then
        let scope =
            if value.Type.IsByRef_t then
                match rhsExprOpt with
                | ValueSome rhsExpr ->
                    getAddressReturningScope acenv aenv rhsExpr
                | _ ->
                    -1
            else
                aenv.scope
        if scope <> -1 then
            acenv.scopes[value.Id] <- scope

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
                                | OlySyntaxParameter.IdentifierWithTypeAnnotation(_, _, _, _, syntaxTy)
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
    | BoundLiteral.NumberInference(lazyLiteral, _) -> 
        tryEvaluateLazyLiteral diagnostics lazyLiteral
        |> ignore
    | BoundLiteral.NullInference(ty) ->
        if not ty.IsNullable && ty.IsSolved then
            diagnostics.Error($"'null' is not allowed for '{printType benv ty}'.", 10, syntaxNode)
    | BoundLiteral.DefaultInference(ty, isUnchecked) ->
        if not ty.IsAnyStruct && not ty.IsNullable && ty.IsSolved && not isUnchecked then
            diagnostics.Error($"'default' is not allowed for '{printType benv ty}' as it could be null.", 10, syntaxNode)
    | _ -> ()
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

and analyzeExpression acenv aenv (expr: BoundExpression) =
    acenv.cenv.ct.ThrowIfCancellationRequested()

    let diagnostics = acenv.cenv.diagnostics

    let syntaxNode = expr.Syntax
    match expr with
    | BoundExpression.Value(syntaxInfo, value) 
            when value.IsLocal && not(value.IsFunction) && value.Type.IsByRef_t ->
        match acenv.scopes.TryGetValue value.Id with
        | true, scope ->
            if aenv.isReturnableAddress && scope = aenv.scope then
                diagnostics.Error($"Cannot take the address of '{value.Name}' as it might escape its scope at this point.", 10, syntaxInfo.SyntaxNameOrDefault)
        | _ ->
            ()

    | BoundExpression.IfElse(_, conditionExpr, trueTargetExpr, falseTargetExpr, exprTy) ->
        analyzeTypePermitByRef acenv aenv syntaxNode exprTy
        analyzeExpression acenv (notReturnableAddress aenv) conditionExpr
        analyzeExpression acenv aenv trueTargetExpr
        analyzeExpression acenv aenv falseTargetExpr

    | BoundExpression.Match(_, _, matchExprs, matchClauses, exprTy) ->
        analyzeTypePermitByRef acenv aenv syntaxNode exprTy
        matchExprs
        |> ImArray.iter (analyzeExpression acenv (notReturnableAddress aenv))
        matchClauses
        |> ImArray.iter (function
            | BoundMatchClause.MatchClause(_, matchPat, conditionExprOpt, targetExpr) ->
                analyzeMatchPattern acenv aenv matchPat
                conditionExprOpt |> Option.iter (analyzeExpression acenv (notReturnableAddress aenv))
                analyzeExpression acenv (notReturnableAddress aenv) targetExpr
        )

    | BoundExpression.While(_, conditionExpr, bodyExpr) ->
        analyzeExpression acenv (notReturnableAddress aenv) conditionExpr
        analyzeExpression acenv (notReturnableAddress aenv) bodyExpr

    | BoundExpression.Try(_, bodyExpr, catchCases, finallyBodyExprOpt) ->
        analyzeExpression acenv aenv bodyExpr

        catchCases
        |> ImArray.iter (function
            | BoundCatchCase.CatchCase(value, catchBodyExpr) ->
                // TODO: 'syntaxNode' is not the accurate place for this.
                analyzeType acenv aenv syntaxNode value.Type
                analyzeExpression acenv aenv catchBodyExpr
        )

        finallyBodyExprOpt
        |> Option.iter (fun finallyBodyExpr ->
            analyzeExpression acenv (notReturnableAddress aenv) finallyBodyExpr
        )

    | BoundExpression.Witness(_, benv, _, bodyExpr, witnessArgOptRef, exprTy) ->
        OlyAssert.True(witnessArgOptRef.contents.IsNone)

        let bodyTy = bodyExpr.Type   
        if subsumesTypeWith Generalizable exprTy bodyTy then
            checkSubsumesType (SolverEnvironment.Create(acenv.cenv.diagnostics, benv)) bodyExpr.Syntax exprTy bodyTy
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

        analyzeExpression acenv aenv bodyExpr
        analyzeType acenv aenv syntaxNode exprTy

    | BoundExpression.ErrorWithNamespace _
    | BoundExpression.ErrorWithType _ -> ()

    | BoundExpression.NewTuple(_, items, _) ->
        items |> ImArray.iter (fun item -> analyzeExpression acenv (notReturnableAddress aenv) item)

    | BoundExpression.NewArray(_, _, elements, _) ->
        elements |> ImArray.iter (fun element -> analyzeExpression acenv (notReturnableAddress aenv) element)

    | BoundExpression.Typed(body=bodyExpr) -> 
        analyzeExpression acenv aenv bodyExpr

    | BoundExpression.Call(syntaxInfo, receiverOpt, witnessArgs, args, value, _) ->
        let aenv =
            if aenv.isReturnableAddress then
                match value.Type.TryFunction with
                | ValueSome(_, outputTy) when outputTy.IsByRef_t ->
                    aenv
                | _ ->
                    notReturnableAddress aenv
            else
                aenv

        // Re-check constraints
        checkConstraintsFromCallExpression acenv.cenv.diagnostics false expr

        if not value.IsFunctionGroup then
            witnessArgs
            |> ImArray.iter (fun x ->
                checkWitnessSolution acenv aenv syntaxNode x
            )

        args |> ImArray.iter (fun arg -> analyzeExpression acenv aenv arg)
        receiverOpt
        |> Option.iter (fun receiver -> analyzeExpression acenv aenv receiver)

        match syntaxInfo.TrySyntaxName with
        | Some(syntaxName) ->
            checkValue acenv aenv syntaxName value
        | _ ->
            checkValue acenv aenv syntaxNode value

        match expr with
        | AddressOf(AutoDereferenced(expr)) -> 
            analyzeExpression acenv aenv expr
        | AddressOf(BoundExpression.Value(syntaxInfo, value)) ->
            match acenv.scopes.TryGetValue value.Id with
            | true, scope ->
                if aenv.isReturnableAddress && scope = aenv.scope then
                    diagnostics.Error($"Cannot take the address of '{value.Name}' as it might escape its scope at this point.", 10, syntaxInfo.SyntaxNameOrDefault)
            | _ ->
                ()
            match aenv.freeLocals.TryGetValue value.Id with
            | true, (syntaxNameOpt, _) when value.Type.IsByRef_t ->
                let syntaxNode =
                    match syntaxNameOpt with
                    | Some syntaxName -> syntaxName :> OlySyntaxNode
                    | _ -> expr.SyntaxNameOrDefault
                diagnostics.Error($"Cannot take the address of '{value.Name}' as it is captured.", 10, syntaxNode)
            | _ ->
                ()
        | _ ->
            ()

    | BoundExpression.None _ -> ()

    | BoundExpression.GetField(receiver=receiver) ->
        analyzeExpression acenv aenv receiver

    | BoundExpression.SetField(_, receiver, field, rhs) ->
        analyzeExpression acenv (notReturnableAddress aenv) rhs
        analyzeExpression acenv (notReturnableAddress aenv) receiver
        checkValue acenv aenv syntaxNode field

    | BoundExpression.GetProperty(receiverOpt=receiverOpt) ->
        receiverOpt
        |> Option.iter (analyzeExpression acenv (notReturnableAddress aenv))

    | BoundExpression.SetProperty(receiverOpt=receiverOpt;prop=prop;rhs=rhs) ->
        analyzeExpression acenv (notReturnableAddress aenv) rhs
        receiverOpt
        |> Option.iter (analyzeExpression acenv (notReturnableAddress aenv))
        checkValue acenv aenv syntaxNode prop

    | BoundExpression.SetValue(value=value;rhs=rhs) ->
        analyzeExpression acenv (notReturnableAddress aenv) rhs
        checkValue acenv aenv syntaxNode value

    | BoundExpression.SetContentsOfAddress(_, lhsExpr, rhsExpr) ->
        analyzeExpression acenv (notReturnableAddress aenv) lhsExpr
        analyzeExpression acenv (notReturnableAddress aenv) rhsExpr

    | BoundExpression.Lambda(_, _, _, pars, lazyBodyExpr, lazyTy, _, _) ->
        OlyAssert.True(lazyBodyExpr.HasExpression)
        OlyAssert.True(lazyTy.Type.IsFunction_t)

        match lazyTy.Type.TryFunction with
        | ValueSome(_, outputTy) ->
            pars
            |> ImArray.iter (fun par ->
                analyzeTypeForParameter acenv aenv syntaxNode par.Type
            )
            analyzeTypeForParameter acenv aenv syntaxNode outputTy
        | _ ->
            ()

        let freeLocals = expr.GetFreeLocals()
        freeLocals
        |> Seq.iter (fun pair ->
            match pair.Value with
            | (syntaxNameOpt, x) ->
                if x.Type.IsByRef_t then
                    let syntaxNode =
                        match syntaxNameOpt with
                        | Some syntaxName -> syntaxName :> OlySyntaxNode
                        | _ -> expr.Syntax
                    diagnostics.Error($"'{x.Name}' is an address and cannot be captured.", 10, syntaxNode)
        )

        analyzeExpression acenv { aenv with scope = aenv.scope + 1; isReturnableAddress = false; freeLocals = freeLocals } lazyBodyExpr.Expression

    | BoundExpression.MemberDefinition(_, binding) ->
        let aenv = (notReturnableAddress aenv)
        let aenv = { aenv with memberFlags = binding.Info.Value.MemberFlags }
        Assert.ThrowIf(binding.Info.Value.IsLocal)
        analyzeBinding acenv aenv expr.Syntax binding

    | BoundExpression.Sequential(_, e1, e2, _) ->
        analyzeExpression acenv (notReturnableAddress aenv) e1
        analyzeExpression acenv aenv e2

    | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
        analyzeBindingInfo acenv aenv syntaxInfo.Syntax (ValueSome rhsExpr) bindingInfo.Value
        analyzeExpression acenv aenv bodyExpr

    | BoundExpression.Value(_, value) ->
        checkValue acenv aenv syntaxNode value

    | BoundExpression.Literal(_, literal) ->
        analyzeLiteral acenv aenv syntaxNode literal

    | BoundExpression.EntityDefinition(syntaxInfo=syntaxInfo;body=bodyExpr;ent=ent) ->
        let aenv = 
            match syntaxInfo.TryEnvironment with
            | Some benv ->
                { aenv with benv = { benv with ac = { benv.ac with Entity = Some ent } } }
            | _ ->
                { aenv with benv = { aenv.benv with ac = { aenv.benv.ac with Entity = Some ent } } }
        ent.Attributes
        |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
        analyzeExpression acenv aenv bodyExpr

    | BoundExpression.Unit _ -> ()

    | BoundExpression.Error _ -> ()
    
let analyzeRoot acenv aenv (root: BoundRoot) =
    match root with
    | BoundRoot.Namespace(body=bodyExpr)
    | BoundRoot.Global(body=bodyExpr) ->
        analyzeExpression acenv aenv bodyExpr

let analyzeBoundTree (cenv: cenv) (env: BinderEnvironment) (tree: BoundTree) =
    let acenv = { cenv = cenv; scopes = System.Collections.Generic.Dictionary(); checkedTypeParameters = System.Collections.Generic.HashSet() }
    let aenv = { envRoot = env; scope = 0; isReturnableAddress = false; freeLocals = ReadOnlyFreeLocals(System.Collections.Generic.Dictionary()); benv = env.benv; isMemberSig = false; memberFlags = MemberFlags.None }
    analyzeRoot acenv aenv tree.Root


