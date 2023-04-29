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
open Oly.Compiler.Internal.Checker

type acenv = { cenv: cenv; scopes: System.Collections.Generic.Dictionary<int64, int>; checkedTypeParameters: System.Collections.Generic.HashSet<int64> }
type aenv = { envRoot: BinderEnvironment; scope: int; isReturnableAddress: bool; freeLocals: ReadOnlyFreeLocals }

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

let rec analyzeType (acenv: acenv) (aenv: aenv) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    let benv = aenv.envRoot.benv
    let diagnostics = acenv.cenv.diagnostics

    match stripTypeEquations ty with
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
        analyzeType acenv aenv syntaxNode inputTy
        analyzeType acenv aenv syntaxNode returnTy

    | TypeSymbol.ForAll(tyPars, innerTy) ->
        tyPars 
        |> ImArray.iter (fun tyPar -> analyzeType acenv aenv syntaxNode tyPar.AsType)
        analyzeType acenv aenv syntaxNode innerTy

    | TypeSymbol.Tuple(tyArgs, _) ->
        analyzeTypeTuple acenv aenv syntaxNode tyArgs

    | TypeSymbol.Variable(tyPar) ->
        analyzeTypeParameter acenv aenv syntaxNode tyPar

    | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
        analyzeTypeParameter acenv aenv syntaxNode tyPar
        analyzeTypeArguments acenv aenv syntaxNode tyArgs

    | TypeSymbol.NativePtr(elementTy) ->
        if not elementTy.IsVoid_t then
            analyzeType acenv aenv syntaxNode elementTy

    | TypeSymbol.Void ->
        diagnostics.Error($"'{printType benv ty}' can only be used as a type argument of a native pointer.", 10, syntaxNode)

    | TypeSymbol.Error(_, Some msg) ->
        diagnostics.Error(msg, 10, syntaxNode)
    
    | _ ->
        analyzeTypeArguments acenv aenv syntaxNode ty.TypeArguments

and analyzeTypeParameter acenv aenv (syntaxNode: OlySyntaxNode) (tyPar: TypeParameterSymbol) =
    if not tyPar.Constraints.IsEmpty && acenv.checkedTypeParameters.Add(tyPar.Id) then
        tyPar.Constraints
        |> ImArray.iter (function
            | ConstraintSymbol.SubtypeOf(lazyTy) ->
                analyzeType acenv aenv syntaxNode lazyTy.Value
            | ConstraintSymbol.ConstantType(lazyTy) ->
                analyzeType acenv aenv syntaxNode lazyTy.Value
            | _ ->
                ()
        )

and analyzeTypeForParameter acenv aenv (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) when tyPar.IsVariadic ->
        acenv.cenv.diagnostics.Error($"Invalid use of variadic type parameter.", 10, syntaxNode)
    | _ ->
        analyzeType acenv aenv syntaxNode ty

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

and analyzeTypeEntity acenv aenv (syntaxNode: OlySyntaxNode) (ent: IEntitySymbol) =
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

and checkEntity acenv aenv syntaxNode (ent: IEntitySymbol) =
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

        let cont() =
            value.TypeArguments
            |> ImArray.iter (fun tyArg -> 
                analyzeType acenv aenv syntaxNode tyArg
            )
            analyzeType acenv aenv syntaxNode value.Type
            match value with
            | :? FunctionGroupSymbol as funcGroup ->
                acenv.cenv.diagnostics.Error(sprintf "'%s' has ambiguous functions." funcGroup.Name, 0, syntaxNode)
            | _ ->
                ()

        // TODO: We need to use the syntaxNode to get access to the type arguments if they exists, parameters, and return type.
        if value.IsFunction then
            let func = value.AsFunction
            match func.TryWellKnownFunction with
            | ValueSome(WellKnownFunction.LoadNullPtr) ->
                let tyArg = func.TypeArguments[0]
                if not tyArg.IsVoid_t then
                    analyzeType acenv aenv syntaxNode tyArg
                match stripTypeEquations func.ReturnType with
                | TypeSymbol.NativePtr(elementTy) when elementTy.IsVoid_t -> ()
                | _ ->
                    analyzeType acenv aenv syntaxNode func.ReturnType
            | _ ->
                cont()
        else
            cont()

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
    | ValueSome rhsExpr ->
        analyzeExpression acenv { aenv with scope = aenv.scope + 1; isReturnableAddress = true } rhsExpr
    | _ ->
        ()

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
        match value.LogicalType.TryFunction with
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
        | :? OlySyntaxBinding as syntaxBinding ->
            match syntaxBinding with
            | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _)
            | OlySyntaxBinding.Signature(syntaxBindingDecl) ->
                match syntaxBindingDecl with
                | OlySyntaxBindingDeclaration.Function(_, _, syntaxPars, syntaxReturnTyAnnot, _) ->
                    let syntaxReturnTy =
                        match syntaxReturnTyAnnot with
                        | OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy) -> syntaxTy :> OlySyntaxNode
                        | _ -> syntaxNode
                    match syntaxPars with
                    | OlySyntaxParameters.Parameters(_, syntaxParList, _) ->
                        (syntaxParList.ChildrenOfType.AsMemory(), func.LogicalParameters)
                        ||> ROMem.iter2 (fun syntaxPar par ->
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

    | :? IFieldSymbol as field ->
        field.Attributes |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
        checkValueTy()

    | :? IPropertySymbol as prop ->
        prop.Attributes |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
        checkValueTy()

    | :? IPatternSymbol as pat ->
        pat.Attributes |> ImArray.iter (analyzeAttribute acenv aenv syntaxNode)
        checkValueTy()

    | _ ->
        checkValueTy()

and analyzeBinding acenv aenv (binding: BoundBinding) =
    analyzeBindingInfo acenv aenv binding.SyntaxInfo.Syntax binding.TryExpression binding.Info.Value

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
        analyzeType acenv aenv syntaxNode exprTy
        analyzeExpression acenv (notReturnableAddress aenv) conditionExpr
        analyzeExpression acenv aenv trueTargetExpr
        analyzeExpression acenv aenv falseTargetExpr

    | BoundExpression.Match(_, _, matchExprs, matchClauses, exprTy) ->
        analyzeType acenv aenv syntaxNode exprTy
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

    | BoundExpression.Witness(expr, witnessArg, ty) ->
        analyzeExpression acenv aenv expr
        analyzeType acenv aenv syntaxNode witnessArg
        analyzeType acenv aenv syntaxNode ty

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

    | BoundExpression.Lambda(_, _, _, _, lazyBodyExpr, lazyTy, _, _) ->
        OlyAssert.True(lazyBodyExpr.HasExpression)

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

        let isReturnableAddress = lazyTy.Type.IsByRef_t
        analyzeExpression acenv { aenv with scope = aenv.scope + 1; isReturnableAddress = isReturnableAddress; freeLocals = freeLocals } lazyBodyExpr.Expression

    | BoundExpression.MemberDefinition(_, binding) ->
        Assert.ThrowIf(binding.Info.Value.IsLocal)
        analyzeBinding acenv (notReturnableAddress aenv) binding

    | BoundExpression.Sequential(_, e1, e2) ->
        analyzeExpression acenv (notReturnableAddress aenv) e1
        analyzeExpression acenv aenv e2

    | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
        analyzeBindingInfo acenv aenv syntaxInfo.Syntax (ValueSome rhsExpr) bindingInfo.Value
        analyzeExpression acenv aenv bodyExpr

    | BoundExpression.Value(_, value) ->
        checkValue acenv aenv syntaxNode value

    | BoundExpression.Literal(_, literal) ->
        analyzeLiteral acenv aenv syntaxNode literal

    | BoundExpression.EntityDefinition(body=bodyExpr;ent=ent) ->
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
    let aenv = { envRoot = env; scope = 0; isReturnableAddress = false; freeLocals = ReadOnlyFreeLocals(System.Collections.Generic.Dictionary()) }
    analyzeRoot acenv aenv tree.Root


