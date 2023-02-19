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

type cenv = { scopes: System.Collections.Generic.Dictionary<int64, int>; diagnostics: OlyDiagnosticLogger; ct: CancellationToken }
type env = { scope: int; benv: BoundEnvironment; isReturnableAddress: bool; freeLocals: ReadOnlyFreeLocals }

let notReturnableAddress env = 
    if env.isReturnableAddress then
        { env with isReturnableAddress = false }
    else
        env

let returnableAddress env =
    if env.isReturnableAddress then
        env
    else
        { env with isReturnableAddress = true }

let rec analyzeType cenv env (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    let benv = env.benv

    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) ->
        analyzeTypeEntity cenv env syntaxNode ent

    | TypeSymbol.InferenceVariable(Some tyPar, _)
    | TypeSymbol.HigherInferenceVariable(Some tyPar, _, _, _) ->
        cenv.diagnostics.Error(sprintf "Type parameter '%s' was unable to be inferred." (printType benv ty), 5, syntaxNode)
        // We solve the inference variable with an error as to prevent a cascade of the same errors.
        // We only care about it at the earliest possible point.
        UnifyTypes TypeVariableRigidity.Flexible ty (TypeSymbol.Error(Some tyPar))
        |> ignore

    | TypeSymbol.InferenceVariable(_, _)
    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        cenv.diagnostics.Error("Unable to infer type at this location.", 5, syntaxNode)
        // We solve the inference variable with an error as to prevent a cascade of the same errors.
        // We only care about it at the earliest possible point.
        UnifyTypes TypeVariableRigidity.Flexible ty (TypeSymbol.Error(None))
        |> ignore

    | TypeSymbol.NativeFunctionPtr(_, argTys, returnTy)
    | TypeSymbol.Function(argTys, returnTy) ->
        argTys
        |> ImArray.iter (analyzeType cenv env syntaxNode)
        analyzeType cenv env syntaxNode returnTy

    | TypeSymbol.ForAll(tyPars, innerTy) ->
        tyPars 
        |> ImArray.iter (fun tyPar -> analyzeType cenv env syntaxNode tyPar.AsType)
        analyzeType cenv env syntaxNode innerTy

    | TypeSymbol.Tuple(tyArgs, _) ->
        analyzeTypeTuple cenv env syntaxNode tyArgs

    | TypeSymbol.Variable(_) ->
        ()

    | TypeSymbol.HigherVariable(_, tyArgs) ->
        analyzeTypeArguments cenv env syntaxNode ty.TypeArguments

    | TypeSymbol.NativePtr(elementTy) ->
        if not elementTy.IsVoid_t then
            analyzeType cenv env syntaxNode elementTy

    | TypeSymbol.Void ->
        cenv.diagnostics.Error($"'{printType env.benv ty}' can only be used as a type argument of a native pointer.", 10, syntaxNode)
    
    | _ ->
        analyzeTypeArguments cenv env syntaxNode ty.TypeArguments

and analyzeTypeForParameter cenv env (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) when tyPar.IsVariadic ->
        cenv.diagnostics.Error($"Invalid use of variadic type parameter.", 10, syntaxNode)
    | _ ->
        analyzeType cenv env syntaxNode ty

and analyzeTypeArguments cenv env syntaxNode (tyArgs: TypeSymbol imarray) =
    tyArgs
    |> ImArray.iter (analyzeType cenv env syntaxNode)

and analyzeTypeArgumentsWithSyntax cenv env syntaxNode (syntaxTyArgs: OlySyntaxType imarray) (tyArgs: TypeSymbol imarray) =
    if syntaxTyArgs.Length <> tyArgs.Length then
        analyzeTypeArguments cenv env syntaxNode tyArgs
    else
        (syntaxTyArgs, tyArgs)
        ||> ImArray.iter2 (fun syntaxTy tyArg ->
            analyzeType cenv env syntaxTy tyArg
        )

and analyzeTypeArgumentsWithSyntaxTuple cenv env syntaxNode (syntaxTupleElements: OlySyntaxTupleElement imarray) (tyArgs: TypeSymbol imarray) =
    if syntaxTupleElements.Length <> tyArgs.Length then
        analyzeTypeArguments cenv env syntaxNode tyArgs
    else
        (syntaxTupleElements, tyArgs)
        ||> ImArray.iter2 (fun syntaxTupleElement tyArg ->
            match syntaxTupleElement with
            | OlySyntaxTupleElement.Type(syntaxTy)
            | OlySyntaxTupleElement.IdentifierWithTypeAnnotation(_, _, syntaxTy) ->
                analyzeType cenv env syntaxTy tyArg
            | _ ->
                analyzeType cenv env syntaxTupleElement tyArg
        )

and analyzeTypeEntity cenv env (syntaxNode: OlySyntaxNode) (ent: IEntitySymbol) =
    match cenv.scopes.TryGetValue(ent.Formal.Id) with
    | true, scope ->
        () // TODO: We will need scopes at some point right?
    | _ ->
        ()

    let cont() =
        analyzeTypeArguments cenv env syntaxNode ent.TypeArguments

    let rec check (syntaxName: OlySyntaxName) =
        match syntaxName with
        | OlySyntaxName.Qualified(_, _, syntaxName) ->
            check syntaxName
        | OlySyntaxName.Generic(_, syntaxTyArgs) ->
            match syntaxTyArgs with
            | OlySyntaxTypeArguments.TypeArguments(_, syntaxTyArgList, _) ->
                analyzeTypeArgumentsWithSyntax cenv env syntaxNode syntaxTyArgList.ChildrenOfType ent.TypeArguments
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

and analyzeTypeTuple cenv env (syntaxNode: OlySyntaxNode) (tyArgs: TypeSymbol imarray) =
    let cont() =
        analyzeTypeArguments cenv env syntaxNode tyArgs

    match syntaxNode with
    | :? OlySyntaxType as syntaxTy ->
        match syntaxTy with
        | OlySyntaxType.Tuple(_, syntaxTupleElementList, _) ->
            analyzeTypeArgumentsWithSyntaxTuple cenv env syntaxNode syntaxTupleElementList.ChildrenOfType tyArgs
        | _ ->
            cont()
    | _ ->
        cont()

and checkWitness cenv env (syntaxNode: OlySyntaxNode) (witness: WitnessSymbol) =
    match witness with
    | WitnessSymbol.TypeExtension(tyExt, specificAbstractFuncOpt) ->
        checkEntity cenv env syntaxNode tyExt
        specificAbstractFuncOpt
        |> Option.iter (fun x -> checkValue cenv env syntaxNode x)

    // TODO: Do we need to check these?
    | WitnessSymbol.TypeParameter _
    | WitnessSymbol.Type _ ->
        ()

and checkWitnessSolution cenv env (syntaxNode: OlySyntaxNode) (witness: WitnessSolution) =
    checkEntity cenv env syntaxNode witness.Entity
    match witness.Solution with
    | Some witness -> checkWitness cenv env syntaxNode witness
    | _ -> ()

and checkEntity cenv env syntaxNode (ent: IEntitySymbol) =
    match cenv.scopes.TryGetValue ent.Id with
    | true, scope ->
        ()
    | _ ->
        ()
    ent.TypeArguments
    |> ImArray.iter (analyzeType cenv env syntaxNode)

and checkEnclosing cenv env syntaxNode (enclosing: EnclosingSymbol) =
    match enclosing with
    | EnclosingSymbol.Entity(ent) ->
        checkEntity cenv env syntaxNode ent
    | EnclosingSymbol.Witness(concreteTy, abstractEnt) ->
        analyzeType cenv env syntaxNode concreteTy
        checkEntity cenv env syntaxNode abstractEnt
    | EnclosingSymbol.Local
    | EnclosingSymbol.RootNamespace ->
        ()

and checkPattern cenv env syntaxNode (pat: IPatternSymbol) =
    checkEnclosing cenv env syntaxNode pat.Enclosing
    checkValue cenv env syntaxNode pat.PatternFunction
    pat.PatternGuardFunction |> Option.iter (fun func -> checkValue cenv env syntaxNode func)

and checkValue cenv env syntaxNode (value: IValueSymbol) =
    if value.IsPattern then
        checkPattern cenv env syntaxNode (value :?> IPatternSymbol)
    else
        checkEnclosing cenv env syntaxNode value.Enclosing

        let cont() =
            value.TypeArguments
            |> ImArray.iter (fun tyArg -> analyzeType cenv env syntaxNode tyArg)
            analyzeType cenv env syntaxNode value.Type
            match value with
            | :? FunctionGroupSymbol as funcGroup ->
                cenv.diagnostics.Error(sprintf "'%s' has ambiguous functions." funcGroup.Name, 0, syntaxNode)
            | _ ->
                ()

        // TODO: We need to use the syntaxNode to get access to the type arguments if they exists, parameters, and return type.
        if value.IsFunction then
            let func = value.AsFunction
            match func.TryWellKnownFunction with
            | ValueSome(WellKnownFunction.LoadNullPtr) ->
                let tyArg = func.TypeArguments[0]
                if not tyArg.IsVoid_t then
                    analyzeType cenv env syntaxNode tyArg
                match stripTypeEquations func.ReturnType with
                | TypeSymbol.NativePtr(elementTy) when elementTy.IsVoid_t -> ()
                | _ ->
                    analyzeType cenv env syntaxNode func.ReturnType
            | _ ->
                cont()
        else
            cont()

let rec getAddressReturningScope cenv env (expr: BoundExpression) =
    expr.GetReturningTargetExpressions()
    |> ImArray.map (fun x ->
        match x with
        | AddressOf(expr) ->
            getAddressReturningScope cenv env expr
        | BoundExpression.Value(value=value) ->
            match cenv.scopes.TryGetValue value.Id with
            | true, scope -> scope
            | _ -> -1
        | _ ->
            -1
    )
    |> ImArray.min

let rec analyzeBindingInfo cenv env (syntaxNode: OlySyntaxNode) (rhsExprOpt: BoundExpression voption) (value: IValueSymbol) =
    match rhsExprOpt with
    | ValueSome rhsExpr ->
        analyzeExpression cenv { env with scope = env.scope + 1; isReturnableAddress = true } rhsExpr
    | _ ->
        ()

    if value.IsLocal && not(value.IsFunction) then
        let scope =
            if value.Type.IsByRef_t then
                match rhsExprOpt with
                | ValueSome rhsExpr ->
                    getAddressReturningScope cenv env rhsExpr
                | _ ->
                    -1
            else
                env.scope
        if scope <> -1 then
            cenv.scopes[value.Id] <- scope

    let checkValueTy () =
        match value.LogicalType.TryFunction with
        | ValueSome(argTys, returnTy) ->
            argTys
            |> ImArray.iter (analyzeTypeForParameter cenv env syntaxNode)
            analyzeTypeForParameter cenv env syntaxNode returnTy
        | _ ->
            analyzeTypeForParameter cenv env syntaxNode value.Type

    match value with
    | :? IFunctionSymbol as func ->
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
                            match syntaxPar with
                            | OlySyntaxParameter.IdentifierWithTypeAnnotation(_, _, _, _, syntaxTy)
                            | OlySyntaxParameter.Type(_, syntaxTy) ->
                                analyzeTypeForParameter cenv env syntaxTy par.Type
                            | _ ->
                                analyzeTypeForParameter cenv env syntaxPar par.Type
                        )
                        analyzeTypeForParameter cenv env syntaxReturnTy func.ReturnType
                    | OlySyntaxParameters.Empty _ ->
                        analyzeTypeForParameter cenv env syntaxReturnTy func.ReturnType
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

and analyzeBinding cenv env (binding: BoundBinding) =
    analyzeBindingInfo cenv env binding.SyntaxInfo.Syntax binding.TryExpression binding.Info.Value

and analyzePattern cenv env (pat: BoundCasePattern) =
    match pat with
    | BoundCasePattern.Function(syntaxPat, _, pat, witnessArgs, pats) ->
        checkValue cenv env syntaxPat pat
        witnessArgs
        |> ImArray.iter (checkWitnessSolution cenv env syntaxPat)
        pats
        |> ImArray.iter (analyzePattern cenv env)

    | BoundCasePattern.Tuple(_, pats) ->
        pats
        |> ImArray.iter (analyzePattern cenv env)

    | BoundCasePattern.Local(syntaxPat, _, value) ->
        checkValue cenv env syntaxPat value

    | BoundCasePattern.FieldConstant(syntaxPat, _, field) ->
        checkValue cenv env syntaxPat field

    | BoundCasePattern.Literal(syntaxPat, _, literal) ->
        analyzeLiteral cenv env syntaxPat literal

    | BoundCasePattern.Discard _ ->
        ()

and analyzeMatchPattern cenv env (matchPat: BoundMatchPattern) =
    match matchPat with
    | BoundMatchPattern.Cases(_, pats) ->
        pats |> ImArray.iter (analyzePattern cenv env)
    | BoundMatchPattern.Or(_, lhsMatchPat, rhsMatchPat) ->
        analyzeMatchPattern cenv env lhsMatchPat
        analyzeMatchPattern cenv env rhsMatchPat

and analyzeLiteral cenv env (syntaxNode: OlySyntaxNode) (literal: BoundLiteral) =
    match literal with
    | BoundLiteral.NumberInference(lazyLiteral, _) -> 
        tryEvaluateLazyLiteral cenv.diagnostics lazyLiteral
        |> ignore
    | BoundLiteral.NullInference(ty) ->
        if not ty.IsNullable && ty.IsSolved then
            cenv.diagnostics.Error($"'null' is not allowed for '{printType env.benv ty}'.", 10, syntaxNode)
    | BoundLiteral.DefaultInference(ty, isUnchecked) ->
        if not ty.IsAnyStruct && not ty.IsNullable && ty.IsSolved && not isUnchecked then
            cenv.diagnostics.Error($"'default' is not allowed for '{printType env.benv ty}' as it could be null.", 10, syntaxNode)
    | _ -> ()
    analyzeType cenv env syntaxNode literal.Type

and analyzeExpression cenv env (expr: BoundExpression) =
    cenv.ct.ThrowIfCancellationRequested()

    let syntaxNode = expr.Syntax
    match expr with
    | AddressOf(AutoDereferenced(expr)) -> 
        analyzeExpression cenv env expr
    | AddressOf(BoundExpression.Value(syntaxInfo, value)) ->
        match cenv.scopes.TryGetValue value.Id with
        | true, scope ->
            if env.isReturnableAddress && scope = env.scope then
                cenv.diagnostics.Error($"Cannot take the address of '{value.Name}' as it might escape its scope at this point.", 10, syntaxInfo.Syntax)
        | _ ->
            ()
        match env.freeLocals.TryGetValue value.Id with
        | true, (syntaxNameOpt, _) when value.Type.IsByRef_t ->
            let syntaxNode =
                match syntaxNameOpt with
                | Some syntaxName -> syntaxName :> OlySyntaxNode
                | _ -> expr.Syntax
            cenv.diagnostics.Error($"Cannot take the address of '{value.Name}' as it is captured.", 10, syntaxNode)
        | _ ->
            ()
    | BoundExpression.Value(syntaxInfo, value) 
            when value.IsLocal && not(value.IsFunction) && value.Type.IsByRef_t ->
        match cenv.scopes.TryGetValue value.Id with
        | true, scope ->
            if env.isReturnableAddress && scope = env.scope then
                cenv.diagnostics.Error($"Cannot take the address of '{value.Name}' as it might escape its scope at this point.", 10, syntaxInfo.Syntax)
        | _ ->
            ()

    | BoundExpression.IfElse(_, conditionExpr, trueTargetExpr, falseTargetExpr, exprTy) ->
        analyzeType cenv env syntaxNode exprTy
        analyzeExpression cenv (notReturnableAddress env) conditionExpr
        analyzeExpression cenv env trueTargetExpr
        analyzeExpression cenv env falseTargetExpr

    | BoundExpression.Match(_, _, matchExprs, matchClauses, exprTy) ->
        analyzeType cenv env syntaxNode exprTy
        matchExprs
        |> ImArray.iter (analyzeExpression cenv (notReturnableAddress env))
        matchClauses
        |> ImArray.iter (function
            | BoundMatchClause.MatchClause(_, matchPat, conditionExprOpt, targetExpr) ->
                analyzeMatchPattern cenv env matchPat
                conditionExprOpt |> Option.iter (analyzeExpression cenv (notReturnableAddress env))
                analyzeExpression cenv (notReturnableAddress env) targetExpr
        )

    | BoundExpression.While(_, conditionExpr, bodyExpr) ->
        analyzeExpression cenv (notReturnableAddress env) conditionExpr
        analyzeExpression cenv (notReturnableAddress env) bodyExpr

    | BoundExpression.Try(_, bodyExpr, catchCases, finallyBodyExprOpt) ->
        analyzeExpression cenv env bodyExpr

        catchCases
        |> ImArray.iter (function
            | BoundCatchCase.CatchCase(value, catchBodyExpr) ->
                // TODO: 'syntaxNode' is not the accurate place for this.
                analyzeType cenv env syntaxNode value.Type
                analyzeExpression cenv env catchBodyExpr
        )

        finallyBodyExprOpt
        |> Option.iter (fun finallyBodyExpr ->
            analyzeExpression cenv (notReturnableAddress env) finallyBodyExpr
        )

    | BoundExpression.Witness(expr, witnessArg, ty) ->
        analyzeExpression cenv env expr
        analyzeType cenv env syntaxNode witnessArg
        analyzeType cenv env syntaxNode ty

    | BoundExpression.ErrorWithNamespace _
    | BoundExpression.ErrorWithType _ -> ()

    | BoundExpression.NewTuple(_, items, _) ->
        items |> ImArray.iter (fun item -> analyzeExpression cenv (notReturnableAddress env) item)

    | BoundExpression.NewArray(_, _, elements, _) ->
        elements |> ImArray.iter (fun element -> analyzeExpression cenv (notReturnableAddress env) element)

    | BoundExpression.Typed(body=bodyExpr) -> 
        analyzeExpression cenv env bodyExpr

    | BoundExpression.Call(_, receiverOpt, witnessArgs, args, syntaxNameOpt, value, _) ->
        if not value.IsFunctionGroup then
            Assert.ThrowIfNot(witnessArgs.HasValue)

        let env =
            if env.isReturnableAddress then
                match value.Type.TryFunction with
                | ValueSome(_, outputTy) when outputTy.IsByRef_t ->
                    env
                | _ ->
                    notReturnableAddress env
            else
                env

        witnessArgs.GetValue(None, CancellationToken.None)
        |> ImArray.iter (fun x ->
            checkWitnessSolution cenv env syntaxNode x
        )
        args |> ImArray.iter (fun arg -> analyzeExpression cenv env arg)
        receiverOpt
        |> Option.iter (fun receiver -> analyzeExpression cenv env receiver)

        match syntaxNameOpt with
        | Some syntaxName ->
            checkValue cenv env syntaxName value
        | _ ->
            checkValue cenv env syntaxNode value

    | BoundExpression.None _ -> ()

    | BoundExpression.GetField(receiver=receiver) ->
        analyzeExpression cenv env receiver

    | BoundExpression.SetField(_, receiver, _, field, rhs) ->
        analyzeExpression cenv (notReturnableAddress env) rhs
        analyzeExpression cenv (notReturnableAddress env) receiver
        checkValue cenv env syntaxNode field

    | BoundExpression.GetProperty(receiverOpt=receiverOpt) ->
        receiverOpt
        |> Option.iter (analyzeExpression cenv (notReturnableAddress env))

    | BoundExpression.SetProperty(receiverOpt=receiverOpt;prop=prop;rhs=rhs) ->
        analyzeExpression cenv (notReturnableAddress env) rhs
        receiverOpt
        |> Option.iter (analyzeExpression cenv (notReturnableAddress env))
        checkValue cenv env syntaxNode prop

    | BoundExpression.SetValue(value=value;rhs=rhs) ->
        analyzeExpression cenv (notReturnableAddress env) rhs
        checkValue cenv env syntaxNode value

    | BoundExpression.SetContentsOfAddress(_, lhsExpr, rhsExpr) ->
        analyzeExpression cenv (notReturnableAddress env) lhsExpr
        analyzeExpression cenv (notReturnableAddress env) rhsExpr

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
                    cenv.diagnostics.Error($"'{x.Name}' is an address and cannot be captured.", 10, syntaxNode)
        )

        let isReturnableAddress = lazyTy.Type.IsByRef_t
        analyzeExpression cenv { env with scope = env.scope + 1; isReturnableAddress = isReturnableAddress; freeLocals = freeLocals } lazyBodyExpr.Expression

    | BoundExpression.MemberDefinition(_, binding) ->
        Assert.ThrowIf(binding.Info.Value.IsLocal)
        analyzeBinding cenv (notReturnableAddress env) binding

    | BoundExpression.Sequential(_, e1, e2) ->
        analyzeExpression cenv (notReturnableAddress env) e1
        analyzeExpression cenv env e2

    | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
        analyzeBindingInfo cenv env syntaxInfo.Syntax (ValueSome rhsExpr) bindingInfo.Value
        analyzeExpression cenv env bodyExpr

    | BoundExpression.Value(_, value) ->
        checkValue cenv env syntaxNode value

    | BoundExpression.Literal(_, literal) ->
        analyzeLiteral cenv env syntaxNode literal

    | BoundExpression.EntityDefinition(body=bodyExpr) ->
        analyzeExpression cenv env bodyExpr

    | BoundExpression.Unit _ -> ()

    | BoundExpression.Error _ -> ()
    
let analyzeRoot cenv env (root: BoundRoot) =
    match root with
    | BoundRoot.Namespace(body=bodyExpr)
    | BoundRoot.Global(body=bodyExpr) ->
        analyzeExpression cenv env bodyExpr

let analyzeBoundTree diagLogger (tree: BoundTree) ct =
    let cenv = { scopes = System.Collections.Generic.Dictionary(); diagnostics = diagLogger; ct = ct }
    let env = { scope = 0; benv = tree.RootEnvironment; isReturnableAddress = false; freeLocals = ReadOnlyFreeLocals(System.Collections.Generic.Dictionary()) }
    analyzeRoot cenv env tree.Root


