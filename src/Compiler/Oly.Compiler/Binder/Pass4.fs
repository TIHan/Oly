[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Pass4

open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.WellKnownExpressions

let isInLocalLambda env = env.isInLocalLambda

let setIsInLocalLambda (env: BinderEnvironment) =
    if env.isInLocalLambda then 
        env
    else 
        { env with isInLocalLambda = true }

let unsetIsInLocalLambda (env: BinderEnvironment) =
    if env.isInLocalLambda then
        { env with isInLocalLambda = false }
    else
        env

let bindNameAsCasePattern (cenv: cenv) (env: BinderEnvironment) (solverEnv: SolverEnvironment) isActive syntaxPattern isFirstPatternSet clauseLocals patternLocals ty (syntaxPatArgs: OlySyntaxPattern imarray) (syntaxName: OlySyntaxName) =
    let expectedPatArgTys =
        ImArray.init syntaxPatArgs.Length (fun _ -> mkInferenceVariableType None)
    
    let expectedTy =
        if expectedPatArgTys.IsEmpty then
            TypeSymbol.Unit
        elif expectedPatArgTys.Length = 1 then
            expectedPatArgTys[0]
        else
            TypeSymbol.Tuple(expectedPatArgTys, ImArray.empty)
    
    let resTyArity = typeResolutionArityOfName syntaxName
    let resInfo = ResolutionInfo.Create(ImArray.createOne ty, resTyArity, ResolutionContext.PatternOnly)
    let resItem = bindNameAsItem cenv env None None resInfo syntaxName
    
    bindPatternByResolutionItem
        cenv
        env
        solverEnv
        expectedTy
        isActive
        isFirstPatternSet
        clauseLocals
        patternLocals
        syntaxPattern
        ty
        (syntaxName, syntaxPatArgs, expectedPatArgTys)
        resItem  

let private bindPattern (cenv: cenv) (env: BinderEnvironment) (solverEnv: SolverEnvironment) isFirstPatternSet (clauseLocals: Dictionary<string, OlySyntaxToken * ILocalSymbol>) (patternLocals: HashSet<string>) (matchTy: TypeSymbol) (syntaxPattern: OlySyntaxPattern) =
    let syntaxInfo = BoundSyntaxInfo.User(syntaxPattern, env.benv)
    match syntaxPattern with
    | OlySyntaxPattern.Discard _ ->
        env, BoundCasePattern.Discard(syntaxInfo)
    
    | OlySyntaxPattern.Literal(syntaxLiteral) ->
        let literal = bindLiteral cenv env (Some matchTy) syntaxLiteral
        env, BoundCasePattern.Literal(syntaxInfo, literal)

    | OlySyntaxPattern.Name(syntaxName) ->
        let syntaxIdentOpt =
            match syntaxName with
            | OlySyntaxName.Identifier(syntaxIdent) ->
                if env.UnqualifiedPatternExists(syntaxIdent.ValueText) then
                    // A pattern exists with this identifier so do not create a local.
                    None
                else
                    // Create a new local.
                    Some(syntaxIdent)
            | _ ->
                // Do not create a local.
                None

        match syntaxIdentOpt with
        | Some(syntaxIdent) ->
            let env, local =
                if not (patternLocals.Add syntaxIdent.ValueText) then
                    cenv.diagnostics.Error($"'%s{syntaxIdent.ValueText}' has already been declared in the pattern set.", 10, syntaxIdent)
                    env, invalidLocal()
                else
                    match clauseLocals.TryGetValue syntaxIdent.ValueText with
                    | true, (_, local) -> 
                        checkTypes solverEnv syntaxIdent local.Type matchTy
                        env, local
                    | _ ->
                        if isFirstPatternSet then
                            let local = createLocalValue syntaxIdent.ValueText matchTy
                            clauseLocals.[syntaxIdent.ValueText] <- (syntaxIdent, local)
                            env.SetUnqualifiedValue(local), local
                        else
                            cenv.diagnostics.Error($"'%s{syntaxIdent.ValueText}' has not been declared in the first pattern set.", 10, syntaxIdent)
                            env, invalidLocal()
            env, BoundCasePattern.Local(syntaxInfo, local)
        | _ ->
            bindNameAsCasePattern cenv env solverEnv false syntaxInfo isFirstPatternSet clauseLocals patternLocals matchTy ImArray.empty syntaxName 

    | OlySyntaxPattern.Function(syntaxName, _, syntaxPatList, _) ->
        bindNameAsCasePattern cenv env solverEnv true syntaxInfo isFirstPatternSet clauseLocals patternLocals matchTy syntaxPatList.ChildrenOfType syntaxName 

    | OlySyntaxPattern.Parenthesis(_, syntaxPatList, _) ->
        // Unit
        if syntaxPatList.ChildrenOfType.IsEmpty then
            checkTypes solverEnv syntaxPattern TypeSymbol.Unit matchTy
            env, BoundCasePattern.Discard(syntaxInfo)

        // Pattern in parenthesis
        elif syntaxPatList.ChildrenOfType.Length = 1 then
            let syntaxPattern = syntaxPatList.ChildrenOfType.[0]
            bindPattern cenv env solverEnv isFirstPatternSet clauseLocals patternLocals matchTy syntaxPattern

        // Tuple
        else
            let (elementTys, syntaxPatterns) =
                let results =
                    syntaxPatList.ChildrenOfType
                    |> ImArray.map (fun syntaxPattern ->
                        let elementTy = mkInferenceVariableType None
                        (elementTy, syntaxPattern)
                    )
                results
                |> Array.ofSeq
                |> Array.unzip

            let elementTys = elementTys |> ImArray.ofSeq
            let syntaxPatterns = syntaxPatterns |> ImArray.ofSeq
            let tupleTy = TypeSymbol.CreateTuple(elementTys)
            checkTypes solverEnv syntaxPattern tupleTy matchTy

            let env, casePats =
                let env, results =
                    (env, elementTys, syntaxPatterns)
                    |||> ImArray.foldMap2 (fun env elementTy syntaxPattern ->
                        bindPattern cenv env solverEnv isFirstPatternSet clauseLocals patternLocals elementTy syntaxPattern
                    )
                env,
                results

            env, BoundCasePattern.Tuple(syntaxInfo, casePats)

    | OlySyntaxPattern.Error _ ->
        checkTypes solverEnv syntaxPattern TypeSymbolError matchTy
        env, BoundCasePattern.Discard(syntaxInfo)

    | _ ->
        OlyAssert.Fail("New pattern matching syntax not handled.")

let private bindPatternByResolutionItem 
        (cenv: cenv) 
        (env: BinderEnvironment) 
        (solverEnv: SolverEnvironment) 
        (expectedTy: TypeSymbol)
        isActive
        isFirstPatternSet 
        (clauseLocals: Dictionary<string, OlySyntaxToken * ILocalSymbol>) 
        (patternLocals: HashSet<string>) 
        (syntaxInfo: BoundSyntaxInfo)
        (matchTy: TypeSymbol)
        (syntaxName, syntaxPatArgs, expectedPatArgTys: _ imarray)
        resItem =
    match resItem with
    | ResolutionItem.Error _ ->         
        env, BoundCasePattern.Local(syntaxInfo, invalidLocal())
    | ResolutionItem.Pattern(_, pat, witnessArgs) ->
        let func = pat.PatternFunction
        let returnTys = 
            let returnTy = func.ReturnType
            match stripTypeEquations returnTy with
            | TypeSymbol.Tuple(argTys, _) -> argTys
            | TypeSymbol.Unit -> ImArray.empty
            | _ -> ImArray.createOne returnTy
        if expectedPatArgTys.Length <> returnTys.Length then
            cenv.diagnostics.Error($"Pattern '{func.Name}' expected '{returnTys.Length}' argument(s), but given '{expectedPatArgTys.Length}'.", 10, syntaxInfo.Syntax)
            env, BoundCasePattern.Function(syntaxInfo, pat, ImArray.empty, ImArray.empty)
        else

            (syntaxPatArgs, expectedPatArgTys, returnTys)
            |||> ImArray.iter3 (fun syntaxPatArg expectedPatArgTy returnTy ->
                checkTypes solverEnv syntaxPatArg expectedPatArgTy returnTy
            )

            let env, casePatArgs =
                (env, syntaxPatArgs, expectedPatArgTys)
                |||> ImArray.foldMap2 (fun env syntaxPatArg patArgTy ->
                    bindPattern cenv env solverEnv isFirstPatternSet clauseLocals patternLocals patArgTy syntaxPatArg
                )

            env, BoundCasePattern.Function(syntaxInfo, pat, witnessArgs, casePatArgs)
    | ResolutionItem.Expression(E.Value(value=(:? IFunctionSymbol as func))) ->
        // We do not show the error for a function group because it means we already have ambiguous functions.
        if not func.IsFunctionGroup then
            cenv.diagnostics.Error($"'{func.Name}' is not a pattern.", 10, syntaxName)

        // TODO: We should return BoundCasePattern.Function, but we do not have a pattern at this point.
        env, BoundCasePattern.Local(syntaxInfo, invalidLocal())
    | ResolutionItem.Expression(E.Call(syntaxInfoFromCall, _, _, _, value, _) as callExpr) ->
        let syntaxInfo =
            BoundSyntaxInfo.User(syntaxInfo.Syntax, env.benv, syntaxInfoFromCall.TrySyntaxName, syntaxInfoFromCall.TryType)
        // Pattern overloading specific
        let callExpr =
            match value with
            | :? FunctionGroupSymbol as funcGroup ->
                let argExprs =
                    E.CreateValue(syntaxInfo, createLocalGeneratedValue "tmp" matchTy)
                    |> ImArray.createOne
                let funcs =
                    funcGroup.Functions
                    |> ImArray.filter (fun x ->
                        match stripTypeEquations x.ReturnType with
                        | TypeSymbol.Tuple(tyArgs, _) ->
                            tyArgs.Length = syntaxPatArgs.Length
                        | TypeSymbol.Unit ->
                            syntaxPatArgs.IsEmpty
                        | _ ->
                            syntaxPatArgs.Length = 1
                    )
                let value =
                    if funcs.IsEmpty then
                        funcGroup :> IValueSymbol
                    elif funcs.Length = 1 then
                        funcs[0]
                    else
                        FunctionGroupSymbol(funcGroup.Name, funcs, (funcGroup :> IFunctionSymbol).Parameters.Length, funcGroup.IsPatternFunction)
                let callExpr = 
                    bindValueAsCallExpressionWithOptionalSyntaxName
                        cenv
                        env
                        syntaxInfo
                        None
                        (ValueSome argExprs)
                        (value, syntaxInfo.TrySyntaxName)
                callExpr
            | _ ->
                callExpr

        match checkExpression cenv env (Some expectedTy) callExpr with
        | BoundExpression.Call(witnessArgs=witnessArgs;value=value) when value.IsFunction ->
            let func = value :?> IFunctionSymbol
            if func.IsPatternFunction then
                OlyAssert.True(func.AssociatedFormalPattern.IsSome)

                if isActive then
                    match value with
                    | :? IFunctionSymbol as func ->
                        if func.ReturnType.IsUnit_t then
                            cenv.diagnostics.Error($"'{func.Name}' returns '()' which requires not to be explicit with '()'.", 10, syntaxInfo.Syntax)
                    | _ ->
                        ()

                let pat2 = actualPattern func.Enclosing func.AllTypeArguments func.AssociatedFormalPattern.Value
                bindPatternByResolutionItem 
                    cenv 
                    env 
                    solverEnv
                    expectedTy
                    isActive
                    isFirstPatternSet
                    clauseLocals
                    patternLocals
                    syntaxInfo
                    matchTy
                    (syntaxName, syntaxPatArgs, expectedPatArgTys)
                    (ResolutionItem.Pattern(syntaxInfo.Syntax, pat2, witnessArgs))
            elif func.IsFunctionGroup then
                // Ambiguous overloads, reported in PostInferenceAnalysis.
                let pat = PatternSymbol(func.Enclosing, ImArray.empty, func.Name, func)
                env, BoundCasePattern.Function(syntaxInfo, pat, witnessArgs, ImArray.empty)
            else
                cenv.diagnostics.Error($"Invalid pattern.", 10, syntaxName)
                env, BoundCasePattern.Local(syntaxInfo, invalidLocal()) 
        | _ ->
            cenv.diagnostics.Error($"Invalid pattern.", 10, syntaxName)
            env, BoundCasePattern.Local(syntaxInfo, invalidLocal())
            
    | ResolutionItem.Expression(E.Value(value=value)) when value.IsFieldConstant ->
        let field = value :?> IFieldSymbol
        checkTypes (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxInfo.Syntax matchTy field.Type
        env, BoundCasePattern.FieldConstant(syntaxInfo, field)

    | _ ->
        cenv.diagnostics.Error($"Invalid pattern.", 10, syntaxName)
        env, BoundCasePattern.Local(syntaxInfo, invalidLocal()) 

// TODO: We should not be using EntitySymbolBuilder.
//       The reason is because it is mutable and we do not want to do mutable things in this pass.
//       Instead, we should have a EntitySymbolInfo which contains the syntax associated for read-only purposes.
//       It's ok that we do not have this right now as AFAIK there is no mutability going on.

/// Pass 4 - Bind entity definition implementation.
let bindTypeDeclarationPass4 (cenv: cenv) (env: BinderEnvironment) syntaxToCapture (entities: EntitySymbolBuilder imarray) (syntaxIdent: OlySyntaxToken) syntaxTyPars syntaxConstrClauseList (syntaxTyDeclBody: OlySyntaxTypeDeclarationBody) =
    if isInLocalLambda env then
        cenv.diagnostics.Error("Type declarations are not allowed in local lambda expressions due to possible inference variables escaping.", 10, syntaxIdent)
        env, BoundExpression.None(BoundSyntaxInfo.User(syntaxToCapture, env.benv))
    else

    let entBuilder = entities.[cenv.entityDefIndex]
    let ent = entBuilder.Entity
    cenv.entityDefIndex <- 0

    OlyAssert.True(ent.IsFormal)

    let bindingInfos =
        let syntaxMemberDecls = syntaxTyDeclBody.GetMemberDeclarations()
        (syntaxMemberDecls, entBuilder.Bindings)
        ||> ImArray.map2 (fun (_, syntaxBindingDecl) (binding, _) -> KeyValuePair(syntaxBindingDecl, binding))
        |> ImmutableDictionary.CreateRange

    let boundExpr = bindTypeDeclarationBodyPass4 cenv env entBuilder entBuilder.NestedEntityBuilders bindingInfos false syntaxTyDeclBody

    // Interfaces
    if ent.IsInterface then
        checkInterfaceDefinition (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxIdent ent

        // ent.Inherits and ent.Implements need to have a specific order so we can get the correct symbols for the associated syntax.
        // At the moment they do because the builder adds them in order.
        env, BoundExpression.CreateEntityDefinition(BoundSyntaxInfo.User(syntaxToCapture, env.benv), boundExpr, ent)
    else
        if not ent.IsAlias && not ent.IsShape then
            ent.AllLogicalInheritsAndImplements
            |> filterTypesAsAbstract
            |> filterMostSpecificTypes
            |> ImArray.iter (fun super ->
                checkImplementation (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxIdent ent.AsType super
            )

        env, BoundExpression.CreateEntityDefinition(BoundSyntaxInfo.User(syntaxToCapture, env.benv), boundExpr, ent)

/// Pass 4 - Bind all implementations.
let bindTypeDeclarationBodyPass4 (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) (entities: EntitySymbolBuilder imarray) bindingInfos isRoot syntaxEntDefBody =
    let env = env.UnsetResolutionMustSolveTypes()

    let ent = entBuilder.Entity

    let env = 
        unsetSkipCheckTypeConstructor env
        |> unsetIsInLocalLambda

    let env = env.SetAccessorContext(ent)
    let env = env.SetEnclosing(EnclosingSymbol.Entity(ent))
    let env = openContentsOfEntityAndOverride env OpenContent.All ent
    let env = env.SetEnclosingTypeArguments(ent.Id, env.GetEnclosingTypeParametersAsTypes())
    let env = env.SetEnclosingTypeParameters(ent.TypeParameters)

    let env =
        // Add intrinsic keyword identifiers to scope
        // If the entity can declare a constructor, it most likely can have a super.
        if not ent.Extends.IsEmpty && ent.CanDeclareConstructor then
            // TODO: Implement
            env
        else
            env       

    match syntaxEntDefBody with
    | OlySyntaxTypeDeclarationBody.None _ ->
        BoundExpression.None(BoundSyntaxInfo.User(syntaxEntDefBody, env.benv))

    | OlySyntaxTypeDeclarationBody.Body(syntaxCastList, syntaxInherits, syntaxImplements, syntaxExpr) ->
        let _, boundExpr = bindTopLevelExpressionPass4 cenv env entities bindingInfos syntaxExpr
        boundExpr

    | _ ->
        raise(InternalCompilerException())

/// Pass 4 - Bind all implementations.
let bindTopLevelExpressionPass4 (cenv: cenv) (env: BinderEnvironment) (entities: EntitySymbolBuilder imarray) (bindingInfos: ImmutableDictionary<OlySyntaxBindingDeclaration, BindingInfoSymbol>) (syntaxExpr: OlySyntaxExpression) =
    cenv.ct.ThrowIfCancellationRequested()

    match syntaxExpr with
    | OlySyntaxExpression.OpenDeclaration _
    | OlySyntaxExpression.OpenStaticDeclaration _
    | OlySyntaxExpression.OpenExtensionDeclaration _ ->
        env, BoundExpression.None(BoundSyntaxInfo.User(syntaxExpr, env.benv))

    | OlySyntaxExpression.ValueDeclaration(_, _, _, _, syntaxValueDeclPostmodifierList, syntaxBinding) ->
        match bindingInfos.TryGetValue(syntaxBinding.Declaration) with
        | true, bindingInfo ->
            // TODO: We could probably make this more efficient...
            let isExplicitMutable =
                if bindingInfo.Value.IsProperty then
                    false
                else
                    syntaxValueDeclPostmodifierList.ChildrenOfType
                    |> ImArray.exists (function OlySyntaxValueDeclarationPostmodifier.Mutable _ -> true | _ -> false)

            let expr =
                bindTopLevelBinding cenv env syntaxExpr isExplicitMutable bindingInfo syntaxBinding

            env, expr
        | _ ->
            // This is for error recovery purposes.
            // REVIEW: We do not need to diagnostic error, because
            // we are *pretty* sure an error has been raised before this point.
            env, BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))

    | OlySyntaxExpression.TypeDeclaration(_, _, _, syntaxTyDefName, syntaxTyPars, syntaxConstrClauseList, _, syntaxTyDefBody) ->
        let prevEntityDefIndex = cenv.entityDefIndex
        let result = bindTypeDeclarationPass4 cenv env syntaxExpr entities syntaxTyDefName.Identifier syntaxTyPars syntaxConstrClauseList syntaxTyDefBody
        cenv.entityDefIndex <- prevEntityDefIndex + 1
        result

    | OlySyntaxExpression.Sequential(syntaxExpr1, syntaxExpr2) ->
        let env1, boundExpr1 = bindTopLevelExpressionPass4 cenv env entities bindingInfos syntaxExpr1
        let env2, boundExpr2 = bindTopLevelExpressionPass4 cenv env1 entities bindingInfos syntaxExpr2
        env2, BoundExpression.Sequential(BoundSyntaxInfo.User(syntaxExpr, env.benv), boundExpr1, boundExpr2, NormalSequential)

    | _ ->
        env, BoundExpression.None(BoundSyntaxInfo.Generated(cenv.syntaxTree))

let private bindTopLevelPropertyBinding cenv env syntaxParentNode syntaxNode bindingInfo (syntaxPropBindings: OlySyntaxPropertyBinding imarray) (syntaxRhsExprOpt: OlySyntaxExpression option) =
    let exprs =
        match bindingInfo with
        | BindingProperty(getterAndSetterBindings=bindings) ->
            (syntaxPropBindings, bindings)
            ||> ImArray.map2 (fun syntaxPropBinding bindingInfo ->
                match syntaxPropBinding with
                | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, syntaxBinding) ->
                    bindTopLevelBinding cenv env syntaxParentNode false bindingInfo syntaxBinding
                | _ ->
                    raise(InternalCompilerUnreachedException())
            )
        | _ ->
            raise(InternalCompilerException())

    let expr = 
        if bindingInfo.Value.IsAutoProperty then
            match syntaxRhsExprOpt with
            | Some(syntaxRhsExpr) ->
                bindLocalExpression cenv env (Some bindingInfo.Type) syntaxRhsExpr syntaxRhsExpr
                |> snd
            | _ ->
                E.None(BoundSyntaxInfo.Generated(cenv.syntaxTree))
        else
            BoundExpression.CreateSequential(cenv.syntaxTree, exprs)

    match expr with
    | E.None _ ->
        BoundExpression.MemberDefinition(
            BoundSyntaxInfo.User(syntaxParentNode, env.benv),
            BoundBinding.Signature(BoundSyntaxInfo.User(syntaxNode, env.benv), bindingInfo)
        )
    | _ ->
        BoundExpression.MemberDefinition(
            BoundSyntaxInfo.User(syntaxParentNode, env.benv),
            BoundBinding.Implementation(BoundSyntaxInfo.User(syntaxNode, env.benv), bindingInfo, expr)
        )

let private bindTopLevelBinding (cenv: cenv) (env: BinderEnvironment) syntaxNode isExplicitMutable (bindingInfo: BindingInfoSymbol) (syntaxBinding: OlySyntaxBinding) =
    match syntaxBinding with
    | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, syntaxRhs) ->

        // TODO: This needs cleanup. If we create a 'base' value, that means we absolutely have access to 'this'. It's a bug otherwise.
        //       So, re-write to guarantee that when 'base' is in scope, we have access to 'this'.

        let env1 =
            match currentEnclosing env with
            | EnclosingSymbol.Entity(ent) when not ent.Extends.IsEmpty && ent.IsClass && bindingInfo.Value.IsFunction ->
                let baseTy = ent.Extends.[0]
                match env.TryFindConcreteEntityByType(baseTy) with
                | ValueSome baseEnt ->
                    if bindingInfo.Value.IsInstance then
                        if bindingInfo.Value.IsConstructor then
                            let baseCtors = createBaseInstanceConstructors "base" baseEnt
                            if baseCtors.IsEmpty then
                                env
                            else
                                let funcGroup = FunctionGroupSymbol("base", baseCtors, baseCtors[0].Parameters.Length, false)
                                env.SetUnqualifiedValue(funcGroup)
                        else
                            let mightBeReadOnly = not isExplicitMutable
                            let baseValue = createBaseValue "base" false mightBeReadOnly baseEnt
                            let env = env.SetUnqualifiedValue(baseValue)
                            env
                    else
                        env
                | _ ->
                    env
            | _ ->
                env

        let env2, implicitThisOpt =
            match currentEnclosing env with
            | EnclosingSymbol.Entity(ent) when ent.IsModule ->
                env1, None
            | EnclosingSymbol.Entity(ent) ->
                if bindingInfo.Value.IsInstance && bindingInfo.Value.IsFunction then
                    let mightBeReadOnly = not isExplicitMutable && (bindingInfo.Value.AsFunction.Semantic <> FunctionSemantic.SetterFunction)
                    let thisPar = createThisValue "this" bindingInfo.Value.IsConstructor mightBeReadOnly (ent.ToInstantiation())
                    env1.SetUnqualifiedValue(thisPar), Some thisPar
                else
                    env1, None
            | _ ->
                env1, None

        let env3 =
            (env2, bindingInfo.Value.TypeParameters)
            ||> ImArray.fold scopeInTypeParameter

        let rhsExpr = bindMemberValueRightSideExpression cenv { env3 with implicitThisOpt = implicitThisOpt } syntaxBindingDecl bindingInfo syntaxRhs
        let bindingInfo, rhsExpr = checkMemberBindingDeclaration (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxBinding bindingInfo rhsExpr
        let binding =
            BoundBinding.Implementation(
                BoundSyntaxInfo.User(syntaxBinding, env.benv),
                bindingInfo,
                rhsExpr
            )
        BoundExpression.MemberDefinition(BoundSyntaxInfo.User(syntaxNode, env.benv), binding)

    | OlySyntaxBinding.Signature(_) ->
        BoundExpression.MemberDefinition(
            BoundSyntaxInfo.User(syntaxNode, env.benv),
            BoundBinding.Signature(BoundSyntaxInfo.User(syntaxBinding, env.benv), bindingInfo))

    | OlySyntaxBinding.Property(_, syntaxPropBindingList) ->
        OlyAssert.False(isExplicitMutable)
        bindTopLevelPropertyBinding cenv env syntaxNode syntaxBinding bindingInfo syntaxPropBindingList.ChildrenOfType None

    | OlySyntaxBinding.PropertyWithDefault(_, syntaxPropBindingList, _, syntaxRhsExpr) ->
        OlyAssert.False(isExplicitMutable)
        bindTopLevelPropertyBinding cenv env syntaxNode syntaxBinding bindingInfo syntaxPropBindingList.ChildrenOfType (Some syntaxRhsExpr)

    | OlySyntaxBinding.PatternWithGuard(_, syntaxImplicitGuardBinding) ->
        let guardFuncOpt =
            match bindingInfo with
            | BindingPattern(pat, _) -> pat.PatternGuardFunction |> Option.map (fun x -> x :?> FunctionSymbol)
            | _ -> None

        match bindingInfo, guardFuncOpt, syntaxImplicitGuardBinding with
        | BindingPattern(_, func), Some guardFunc, OlySyntaxGuardBinding.Implementation(syntaxWhenToken, _, _, syntaxCondExpr, _, _, syntaxRhsExpr) ->
            let bindingGuardInfo = BindingFunction(guardFunc)

            let env1 =
                (env, guardFunc.TypeParameters)
                ||> ImArray.fold scopeInTypeParameter
            let condExpr = bindMemberValueRightSideExpression cenv env1 syntaxWhenToken bindingGuardInfo syntaxCondExpr
            let bindingGuardInfo = BindingFunction(guardFunc)
            let bindingGuardInfo, condExpr = checkMemberBindingDeclaration (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxBinding bindingGuardInfo condExpr
            let bindingGuard =
                BoundBinding.Implementation(
                    BoundSyntaxInfo.User(syntaxImplicitGuardBinding, env.benv),
                    bindingGuardInfo,
                    condExpr
                )

            let env1 =
                (env, func.TypeParameters)
                ||> ImArray.fold scopeInTypeParameter
            let rhsExpr = bindMemberValueRightSideExpression cenv env1 syntaxWhenToken bindingInfo syntaxRhsExpr
            let bindingInfo = BindingFunction(func)
            let bindingInfo, rhsExpr = checkMemberBindingDeclaration (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxBinding bindingInfo rhsExpr
            let binding =
                BoundBinding.Implementation(
                    BoundSyntaxInfo.User(syntaxBinding, env.benv),
                    bindingInfo,
                    rhsExpr
                )

            E.Sequential(
                BoundSyntaxInfo.User(syntaxBinding, env.benv),
                BoundExpression.MemberDefinition(BoundSyntaxInfo.User(syntaxNode, env.benv), bindingGuard),
                BoundExpression.MemberDefinition(BoundSyntaxInfo.User(syntaxNode, env.benv), binding),
                NormalSequential
            )
        | _ ->
            E.Error(BoundSyntaxInfo.User(syntaxBinding, env.benv))

    | _ ->
        raise(InternalCompilerUnreachedException())

let bindArguments (cenv: cenv) (env: BinderEnvironment) (syntaxArgs: OlySyntaxExpression imarray) =
    let env = env.SetPassedAsArgument(true)
    syntaxArgs
    |> ImArray.map (fun x ->
        let _, expr = bindLocalExpression cenv env None x x
        expr
    )

let bindItemAsExpression (cenv: cenv) (env: BinderEnvironment) (nameRes: ResolutionItem) =
    match nameRes with
    | ResolutionItem.Invalid(syntax)
    | ResolutionItem.Pattern(syntax, _, _) -> 
        cenv.diagnostics.Error("This is not a valid expression.", 10, syntax)
        BoundExpression.Error(BoundSyntaxInfo.User(syntax, env.benv))
    | ResolutionItem.Namespace(syntax, namespaceEnt) ->
        if not cenv.syntaxTree.HasErrors then
            cenv.diagnostics.Error("A namespace cannot be used as a result of an expression.", 10, syntax)
        BoundExpression.ErrorWithNamespace(syntax, env.benv, namespaceEnt)
    | ResolutionItem.Error(syntax) ->
        BoundExpression.Error(BoundSyntaxInfo.User(syntax, env.benv))

    | ResolutionItem.Property(syntaxToCapture, syntaxNameOpt, receiverInfoOpt, prop) ->
        bindPropertyAsGetPropertyExpression cenv env syntaxToCapture receiverInfoOpt syntaxNameOpt prop

    | ResolutionItem.Type(syntaxName, ty) ->
        let constTyParOpt =
            match ty.TryTypeParameter with
            | ValueSome tyPar ->
                let exists =
                    tyPar.Constraints
                    |> ImArray.exists (function
                        | ConstraintSymbol.ConstantType _ -> true
                        | _ -> false
                    )
                if exists then
                    ValueSome tyPar
                else
                    ValueNone
            | _ ->
                ValueNone

        match constTyParOpt with
        | ValueSome constTyPar ->
            BoundExpression.Literal(
                BoundSyntaxInfo.User(syntaxName, env.benv),
                BoundLiteral.Constant(ConstantSymbol.TypeVariable(constTyPar))
            )
        | _ ->
            cenv.diagnostics.Error(sprintf "The type '%s' cannot be used as a result of an expression." (printType env.benv ty), 10, syntaxName)
            BoundExpression.ErrorWithType(syntaxName, env.benv, ty)

    | ResolutionItem.Expression(expr) -> expr

    | ResolutionItem.MemberCall(syntaxToCapture, receiverInfoOpt, syntaxCallBodyExpr, syntaxArgs, syntaxMemberExprOpt) ->
        match syntaxMemberExprOpt with
        | Some syntaxMemberExpr ->
            let expr = bindCallExpression cenv env syntaxToCapture receiverInfoOpt syntaxCallBodyExpr syntaxArgs

            // REVIEW: This is a little weird. We do not have to provide an expected type as we actually won't check against it.
            //         However, having an expected type enables the use of overload resolution's final phase which is useful.
            //         We could write this differently and just pass a flag of some kind instead of doing this hack.
            let useOverloadingFinalPhase = (Some(mkInferenceVariableType None))
            let expr = checkExpression cenv env useOverloadingFinalPhase expr
            bindMemberExpressionAsItem cenv env syntaxToCapture (expr |> Choice1Of2) syntaxMemberExpr
            |> bindItemAsExpression cenv env
        | _ ->
            bindCallExpression cenv env syntaxToCapture receiverInfoOpt syntaxCallBodyExpr syntaxArgs

    | ResolutionItem.MemberIndexerCall(syntaxToCapture, syntaxReceiver, syntaxBrackets, syntaxMemberExprOpt, (* TODO: get rid of this parameter? *) _expectedTyOpt) ->
        match syntaxMemberExprOpt with
        | Some syntaxMemberExpr ->
            let _, expr = bindIndexer cenv env syntaxToCapture syntaxReceiver syntaxBrackets None

            // REVIEW: This is a little weird. We do not have to provide an expected type as we actually won't check against it.
            //         However, having an expected type enables the use of overload resolution's final phase which is useful.
            //         We could write this differently and just pass a flag of some kind instead of doing this hack.
            let useOverloadingFinalPhase = (Some(mkInferenceVariableType None))
            let expr = checkExpression cenv env useOverloadingFinalPhase expr
            bindMemberExpressionAsItem cenv env syntaxToCapture (expr |> Choice1Of2) syntaxMemberExpr
            |> bindItemAsExpression cenv env
        | _ ->
            bindIndexer cenv env syntaxToCapture syntaxReceiver syntaxBrackets None
            |> snd

    | ResolutionItem.Parenthesis(syntaxToCapture, syntaxParen, syntaxMemberExprOpt) ->
        let expr = bindParenthesisExpression cenv env None syntaxToCapture syntaxParen |> snd
        match syntaxMemberExprOpt with
        | Some syntaxMemberExpr ->
            bindMemberExpressionAsItem cenv env syntaxToCapture (expr |> Choice1Of2) syntaxMemberExpr
            |> bindItemAsExpression cenv env
        | _ ->
            expr

let bindParenthesisExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) syntaxNode (syntaxExprList: OlySyntaxSeparatorList<OlySyntaxExpression>) =
    // Unit
    if syntaxExprList.ChildrenOfType.IsEmpty then
        match expectedTyOpt with
        | Some(expectedTy) when not expectedTy.IsRealUnit ->
            checkTypes (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxNode expectedTy TypeSymbol.Unit
        | _ ->
            ()
        match expectedTyOpt with
        | Some(expectedTy) when expectedTy.IsRealUnit ->
            env, BoundExpression.Unit(BoundSyntaxInfo.User(syntaxNode, env.benv))
        | _ ->
            env, BoundExpression.None(BoundSyntaxInfo.User(syntaxNode, env.benv))

    // Body
    elif syntaxExprList.ChildrenOfType.Length = 1 then
        let syntaxBodyExpr = syntaxExprList.ChildrenOfType.[0]
        bindLocalExpression cenv env expectedTyOpt syntaxNode syntaxBodyExpr

    // Tuple
    else
        let argExprs =
            syntaxExprList.ChildrenOfType
            |> ImArray.map (fun syntaxExpr ->
                let _, item = bindLocalExpression cenv (env.SetReturnable(false)) None syntaxExpr syntaxExpr
                item
            )

        let tupleTy = 
            match expectedTyOpt with
            | Some expectedTy when expectedTy.IsAnyTuple && expectedTy.TypeArguments.Length = argExprs.Length ->
                expectedTy
            | _ ->
                TypeSymbol.CreateTuple(ImArray.init argExprs.Length (fun _ -> mkInferenceVariableType None))

        (tupleTy.TypeArguments, argExprs)
        ||> ImArray.iter2 (checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)))

        let newTupleExpr = BoundExpression.NewTuple(BoundSyntaxInfo.User(syntaxNode, env.benv), argExprs, tupleTy)

        env, checkExpression cenv env expectedTyOpt newTupleExpr

let bindCallExpression (cenv: cenv) (env: BinderEnvironment) syntaxToCapture (receiverInfoOpt: ReceiverInfo option) (syntaxCallBodyExpr: OlySyntaxExpression) (syntaxArgs: OlySyntaxExpression imarray) : BoundExpression =
    let argExprs = bindArguments cenv (env.SetReturnable(false)) syntaxArgs

    match syntaxCallBodyExpr with
    | OlySyntaxExpression.Name(syntaxName) ->
        let resTyArity = typeResolutionArityOfName syntaxName
        let resInfo = ResolutionInfo.Create(ValueSome argExprs, resTyArity, ResolutionContext.ValueOnly)
        let expr =
            bindNameAsItem cenv env (Some syntaxToCapture) receiverInfoOpt resInfo syntaxName
            |> bindItemAsExpression cenv env

        match expr with
        | E.GetProperty(syntaxInfo, _, _, _) ->
            let expr = checkExpression cenv env None expr
            let bridge = createLocalBridgeValue expr.Type
            let callExpr = E.Call(syntaxInfo, None, ImArray.empty, argExprs, bridge, CallFlags.None)
            BoundExpression.Let(
                syntaxInfo,
                BindingLocal(bridge),
                expr,
                callExpr
            )
        | _ ->
            expr

    | OlySyntaxExpression.Call(syntaxCallBodyExpr2, syntaxArgs2) ->
        let syntaxArgs2 = getSyntaxArgumentsAsSyntaxExpressions cenv syntaxArgs2
        let expr = 
            bindCallExpression cenv env syntaxCallBodyExpr receiverInfoOpt syntaxCallBodyExpr2 syntaxArgs2
            |> checkExpression cenv env None

        let bridge = createLocalBridgeValue expr.Type
        match syntaxCallBodyExpr2.TryName with
        | Some syntaxName ->
            let bodyExpr =
                let syntaxInfo = 
                    BoundSyntaxInfo.User(
                        syntaxToCapture, 
                        env.benv, 
                        Some syntaxName,
                        receiverInfoOpt
                        |> Option.bind (fun x ->
                            match x.item with
                            | ReceiverItem.Type(ty) -> Some ty
                            | _ -> None
                        )
                    )
                bindValueAsCallExpression cenv env syntaxInfo None (ValueSome argExprs) ImArray.empty bridge
                |> fst
                |> checkExpression cenv env None

            let syntaxInfo = BoundSyntaxInfo.User(syntaxToCapture, env.benv)
            BoundExpression.Let(
                syntaxInfo,
                BindingLocal(bridge),
                expr,
                bodyExpr
            )
        | _ ->
            cenv.diagnostics.Error("Invalid expression for function call.", 10, syntaxCallBodyExpr)
            BoundExpression.Error(BoundSyntaxInfo.User(syntaxCallBodyExpr2, env.benv))
    | _ ->
        cenv.diagnostics.Error("Invalid expression for function call.", 10, syntaxCallBodyExpr)
        BoundExpression.Error(BoundSyntaxInfo.Generated(cenv.syntaxTree))

let bindSequentialExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) syntaxToCapture syntaxExpr1 syntaxExpr2 =
    let (env1: BinderEnvironment), expr1 = bindLocalExpression cenv (env.SetReturnable(false)) None syntaxExpr1 syntaxExpr1

    let expr1 =
        match expr1 with
        | BoundExpression.Call(value=value) when value.IsFunctionGroup ->
            checkExpression cenv env (Some TypeSymbol.Unit) expr1
        | _ ->
            expr1

    let env2, expr2 =
        bindLocalExpression cenv (env1.SetReturnable(env.isReturnable)) expectedTyOpt syntaxExpr2 syntaxExpr2

    let boundExpression = BoundExpression.Sequential(BoundSyntaxInfo.User(syntaxToCapture, env.benv), expr1, expr2, NormalSequential)
    env2, boundExpression

let bindFunctionRightSideExpression (cenv: cenv) (env: BinderEnvironment) (envOfBinding: BinderEnvironment) (syntaxRhs: OlySyntaxExpression) (pars: ILocalParameterSymbol imarray) (func: FunctionSymbol) : BoundExpression =
    let envRhs = envOfBinding
    let expectedRhsTyOpt =
        if not func.IsConstructor then
            Some func.ReturnType
        else
            None

    let envBody = envRhs.SetReturnable(true)
    let _, rhsBodyExpr = bindLocalExpression cenv envBody expectedRhsTyOpt syntaxRhs syntaxRhs

    let rhsBodyExpr =
        if func.IsLocal then
            checkExpression cenv env expectedRhsTyOpt rhsBodyExpr
        else
            rhsBodyExpr

    let rhsExpr = 
        BoundExpression.CreateLambda(
            cenv.syntaxTree,
            LambdaFlags.None,
            func.TypeParameters, 
            pars, 
            LazyExpression.CreateNonLazy(None, fun _ -> rhsBodyExpr)
        )
    let solverEnv = SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)
    checkLocalLambdaKind solverEnv rhsExpr pars func.IsStaticLocalFunction
    rhsExpr

let bindValueRightSideExpression (cenv: cenv) (env: BinderEnvironment) (expectedTy: TypeSymbol) (envOfBinding: BinderEnvironment) (syntaxRhs: OlySyntaxExpression) : BoundExpression =
    let _, rhsExpr = bindLocalExpression cenv (envOfBinding.SetReturnable(false)) (Some expectedTy) syntaxRhs syntaxRhs
    match rhsExpr with
    | E.Lambda _ ->
        checkImmediateExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) false rhsExpr
        checkExpression cenv env (Some expectedTy) rhsExpr
    | _ ->
        rhsExpr

let bindMemberValueRightSideExpression (cenv: cenv) (env: BinderEnvironment) (syntaxNode: OlySyntaxNode) (binding: BindingInfoSymbol) (syntaxRhs: OlySyntaxExpression) : BoundExpression =
    let envOfBinding, pars =
        // TODO: Do we need to do anything else here? Handle other bindings?
        match binding with
        | BindingFunction(func)
        | BindingPattern(_, func) ->
            let pars =
                func.Parameters
                |> ImArray.mapi (fun i par ->
                    if i = 0 then
                        match env.implicitThisOpt with
                        | Some(receiver) -> receiver
                        | _ -> par
                    else
                        par
                )

            let envOfBinding =
                (env, pars)
                ||> ImArray.fold (fun env pv ->
                    env.SetUnqualifiedValue(pv)
                )
            if func.IsConstructor then
                envOfBinding, pars
            else
                envOfBinding.SetEnclosingTypeParameters(envOfBinding.EnclosingTypeParameters.AddRange(func.TypeParameters)), pars
        | BindingField(field=field) ->
            env, ImArray.empty
        | BindingProperty(_, prop) ->
            if not prop.IsAutoProperty then
                match syntaxRhs with
                | OlySyntaxExpression.Error _ -> ()
                | _ ->
                    cenv.diagnostics.Error("Expression is not expected.", 10, syntaxNode)
            env, ImArray.empty

    let envOfBinding =
        if binding.Value.IsConstructor then 
            match currentEnclosing env with
            | EnclosingSymbol.Entity(ent) when not ent.IsInterface && not ent.IsModule ->
                envOfBinding.SetIsInInstanceConstructorType(ent)
            | _ ->
                envOfBinding.UnsetIsInInstanceConstructorType()
        elif (binding.Value.IsFunction && not binding.Value.IsLocal) || binding.Value.IsStaticLocalFunction then
            envOfBinding.UnsetIsInInstanceConstructorType()
        else
            envOfBinding

    match binding with
    | BindingFunction(func=func)
    | BindingPattern(_, func) ->
        bindFunctionRightSideExpression cenv env envOfBinding syntaxRhs pars func
    | _ ->
        bindValueRightSideExpression cenv env binding.Value.Type envOfBinding syntaxRhs

let bindLetValueRightSideExpression (cenv: cenv) (env: BinderEnvironment) (binding: LocalBindingInfoSymbol) (syntaxBindingDecl: OlySyntaxBindingDeclaration) (syntaxRhs: OlySyntaxExpression) : BoundExpression =
    let envOfBinding =
        match binding with
        | BindingLocalFunction(func=func) ->
            let pars = func.Parameters
            let envOfBinding =
                (env, pars)
                ||> ImArray.fold (fun env pv ->
                    env.SetUnqualifiedValue(pv)
                )
            envOfBinding.SetEnclosingTypeParameters(envOfBinding.EnclosingTypeParameters.AddRange(func.TypeParameters))
        | BindingLocal _ ->
            env

    match binding with
    | BindingLocalFunction(func=func) ->
        bindFunctionRightSideExpression cenv env envOfBinding syntaxRhs func.Parameters func
    | _ ->
        bindValueRightSideExpression cenv env binding.Value.Type envOfBinding syntaxRhs

let bindLambdaExpression (cenv: cenv) (env: BinderEnvironment) syntaxToCapture syntaxLambdaKind syntaxPars syntaxBodyExpr =

    let isStatic =
        match syntaxLambdaKind with
        | OlySyntaxLambdaKind.None _ -> false
        | OlySyntaxLambdaKind.Static _ -> true
        | _ ->
            raise(InternalCompilerException())

    let bind (pars: ImmutableArray<ILocalParameterSymbol>) isStatic syntaxBodyExpr =
        let argTys = pars |> ImArray.map (fun x -> x.Type)

        let bodyExpr =
            LazyExpression(Some syntaxBodyExpr, fun _ -> 
                let env1 =
                    (env, pars)
                    ||> Seq.fold (fun env pv ->
                        env.SetUnqualifiedValue(pv)
                    )
                let _, bodyExpr = bindLocalExpression cenv (env1.SetReturnable(false)) None syntaxBodyExpr syntaxBodyExpr
                let solverEnv = SolverEnvironment.Create(cenv.diagnostics, env1.benv, cenv.pass)
                let ty = TypeSymbol.CreateFunction(ImmutableArray.Empty, argTys, mkInferenceVariableType(None), FunctionKind.Normal)
                checkLambdaExpression solverEnv pars bodyExpr ty

                checkLocalLambdaKind solverEnv bodyExpr pars isStatic

                bodyExpr
            )

        pars, bodyExpr
   
    let pars, bodyExpr =
        let _, pars = bindParameters cenv env false syntaxPars
        bind pars isStatic syntaxBodyExpr
    
    let lambdaFlags =
        if isStatic then
            LambdaFlags.Static
        else
            LambdaFlags.None

    env, BoundExpression.CreateLambda(syntaxToCapture, env.benv, lambdaFlags, ImArray.empty, pars, bodyExpr)

let private bindMatchPattern (cenv: cenv) (env: BinderEnvironment) solverEnv isFirstPatternSet (clauseLocals: Dictionary<_, _>) (matchTys: TypeSymbol imarray) (syntaxMatchPattern: OlySyntaxMatchPattern) =
    match syntaxMatchPattern with
    | OlySyntaxMatchPattern.Patterns(syntaxPatternList) ->
        let patternLocals = HashSet()
        let env, casePats =
            (env, syntaxPatternList.ChildrenOfType)
            ||> ImArray.foldMapi (fun env i syntaxPattern ->
                let exprTy =
                    if i >= matchTys.Length then
                        cenv.diagnostics.Error("Excess pattern.", 10, syntaxPattern)
                        TypeSymbolError
                    else
                        matchTys.[i]
                bindPattern cenv env solverEnv isFirstPatternSet clauseLocals patternLocals exprTy syntaxPattern
            )

        if casePats.Length < matchTys.Length then
            cenv.diagnostics.Error("Not enough patterns specified for the match.", 10, syntaxMatchPattern)
        elif not isFirstPatternSet && clauseLocals.Count <> patternLocals.Count then
            clauseLocals
            |> Seq.iter (fun pair ->
                if not (patternLocals.Contains(pair.Key)) then
                    let syntaxIdent = pair.Value |> fst
                    cenv.diagnostics.Error($"'{syntaxIdent.ValueText}' is not part of all unioned pattern sets.", 10, syntaxIdent)
            )

        env, BoundMatchPattern.Cases(syntaxMatchPattern, casePats)

    | OlySyntaxMatchPattern.Or(syntaxLhsMatchPattern, _, syntaxRhsMatchPattern) ->
        let env, lhsMatchPattern = bindMatchPattern cenv env solverEnv isFirstPatternSet clauseLocals matchTys syntaxLhsMatchPattern
        let env, rhsMatchPattern = bindMatchPattern cenv env solverEnv false clauseLocals matchTys syntaxRhsMatchPattern
        env, BoundMatchPattern.Or(syntaxMatchPattern, lhsMatchPattern, rhsMatchPattern)

    | OlySyntaxMatchPattern.Discard(syntaxToken) ->
        let patterns = ImArray.init matchTys.Length (fun _ -> BoundCasePattern.Discard(BoundSyntaxInfo.User(syntaxToken, env.benv)))
        env, BoundMatchPattern.Cases(syntaxMatchPattern, patterns)

    | OlySyntaxMatchPattern.Error _ ->
        env, BoundMatchPattern.Cases(syntaxMatchPattern, ImArray.empty)

    | _ ->
        failwith "New pattern matching syntax not handled."

let private bindMatchGuard (cenv: cenv) (env: BinderEnvironment) (syntaxMatchGuard: OlySyntaxMatchGuard) =
    match syntaxMatchGuard with
    | OlySyntaxMatchGuard.MatchGuard(_, _, syntaxConditionExpr, _) ->
        bindLocalExpression cenv env (Some TypeSymbol.Bool) syntaxConditionExpr syntaxConditionExpr |> snd
        |> Some
    | OlySyntaxMatchGuard.None ->
        None
    | _ ->
        failwith "New pattern matching syntax not handled."

let private bindMatchClause (cenv: cenv) (env: BinderEnvironment) solverEnv expectedTargetTy (exprTys: TypeSymbol imarray) (syntaxMatchClause: OlySyntaxMatchClause) =
    match syntaxMatchClause with
    | OlySyntaxMatchClause.MatchClause(_, syntaxMatchPattern, syntaxMatchGuard, _, syntaxTargetExpr) ->
        let clauseLocals = Dictionary()
        let env, matchPattern = bindMatchPattern cenv env solverEnv true clauseLocals exprTys syntaxMatchPattern
        let conditionExprOpt = bindMatchGuard cenv (env.SetReturnable(false)) syntaxMatchGuard
        let targetExpr = bindLocalExpression cenv env (Some expectedTargetTy) syntaxTargetExpr syntaxTargetExpr |> snd
        checkSubsumesType solverEnv syntaxTargetExpr expectedTargetTy targetExpr.Type
        BoundMatchClause.MatchClause(syntaxMatchClause, matchPattern, conditionExprOpt, targetExpr)
    | _ ->
        failwith "New pattern matching syntax not handled."

let private isCasePatternExhaustive (catchCase: BoundCasePattern) =
    match catchCase with
    | BoundCasePattern.Discard _
    | BoundCasePattern.Local _ -> true
    | BoundCasePattern.Function(_, pat, _, casePatArgs) -> 
        if pat.PatternGuardFunction.IsNone then
            casePatArgs
            |> ImArray.forall isCasePatternExhaustive
        else
            false
    | _ -> 
        false

let private checkExhaustiveness (cenv: cenv) (_env: BinderEnvironment) syntaxNode matchClauses =
    let rows =
        let rec loop (matchPattern: BoundMatchPattern) =
            match matchPattern with
            | BoundMatchPattern.Cases(syntaxMatchPattern, patterns) ->
                let patterns =
                    patterns
                    |> Seq.collect (fun x -> x.ExhaustivenessColumns)
                    |> ImArray.ofSeq
                ImArray.createOne (ImArray.createOne(syntaxMatchPattern, patterns))
            | BoundMatchPattern.Or(_, lhs, rhs) ->
                ImArray.append (loop lhs) (loop rhs)

        matchClauses
        |> ImArray.map (fun matchClause ->
            match matchClause with
            | BoundMatchClause.MatchClause(_, matchPattern, guardExprOpt, _) -> 
                let disjunctionRows =
                    loop matchPattern
                    |> Seq.concat
                    |> ImArray.ofSeq
                (disjunctionRows, guardExprOpt.IsSome)
        )

    let columns =
        let columnCount =
            if rows.IsEmpty then
                0
            else
                rows
                |> Seq.map (fun (x, _) -> 
                    if x.IsEmpty then 0
                    else
                        x
                        |> Seq.map (fun (_, x) -> 
                            x
                            |> Seq.sumBy (fun x -> x.ExhaustivenessColumnCount)
                        )
                        |> Seq.min
                )
                |> Seq.min

        seq {
            for columnIndex = 0 to columnCount - 1 do
                seq {
                    for rowIndex = 0 to rows.Length - 1 do
                        let disjunctionRows, _ = rows.[rowIndex]
                        for disjunctionRowIndex = 0 to disjunctionRows.Length - 1 do
                            let _, disjunctionRow = disjunctionRows.[disjunctionRowIndex]
                            (rowIndex, disjunctionRow.[columnIndex])
                }
                |> ImArray.ofSeq
        }
        |> ImArray.ofSeq

    let mutable hasError = false
    let mutable isMatchExhaustive = true
    for columnIndex = 0 to columns.Length - 1 do
        let columnRows = columns.[columnIndex]

        let mutable wasPreviousExhaustive = false
        let mutable isExhaustive = true
        let mutable isLastRowExhaustive = true
        for columnRowIndex = 0 to columnRows.Length - 1 do
            let isLastRow = columnRowIndex = (columnRows.Length - 1)
            let rowIndex, casePat = columnRows.[columnRowIndex]

            if isCasePatternExhaustive casePat then
                let disjunctionRows, hasGuard = rows.[rowIndex]

                if not hasGuard then                    
                    let areAllDiscards =
                        disjunctionRows
                        |> ImArray.exists (fun (_, disjunctionRow) ->
                            disjunctionRow
                            |> ImArray.forall isCasePatternExhaustive
                        )

                    if isExhaustive && not areAllDiscards then
                        isExhaustive <- false
                        if isLastRow then
                            isLastRowExhaustive <- false
                else
                    isExhaustive <- false
                    if isLastRow then
                        isLastRowExhaustive <- false
            else
                isExhaustive <- false
                if isLastRow then
                    isLastRowExhaustive <- false
            
            if wasPreviousExhaustive then
                let disjunctionRows, _ = rows.[rowIndex]
                disjunctionRows
                |> ImArray.iter (fun (syntaxMatchPattern, _) ->
                    cenv.diagnostics.Error("Specified pattern will never be matched.", 10, syntaxMatchPattern)
                    hasError <- true
                )
            wasPreviousExhaustive <- isExhaustive

        if not isLastRowExhaustive then
            isMatchExhaustive <- false

    if not isMatchExhaustive && not hasError then
        // TODO: More descriptive error message.
        cenv.diagnostics.Error("Match is not exhaustive.", 10, syntaxNode)

let private bindMatchExpression (cenv: cenv) (env: BinderEnvironment) solverEnv expectedTargetTyOpt (syntaxNode: OlySyntaxExpression) (syntaxMatchToken: OlySyntaxToken) (matchExprs: BoundExpression imarray) (syntaxMatchClauses: OlySyntaxMatchClause imarray) =
    let matchExprTys =
        matchExprs
        |> ImArray.map (fun x -> x.Type)

    let expectedTargetTy =
        match expectedTargetTyOpt with
        | Some expectedTargetTy -> expectedTargetTy
        | _ -> mkInferenceVariableType None

    let matchClauses =
        syntaxMatchClauses
        |> ImArray.map (fun syntaxMatchClause ->
            bindMatchClause cenv env solverEnv expectedTargetTy matchExprTys syntaxMatchClause
        )

    checkExhaustiveness cenv env syntaxMatchToken matchClauses   

    env, BoundExpression.Match(syntaxNode, env.benv, matchExprs, matchClauses, expectedTargetTy)

/// Returns a SetField expression or multiple SetField expressions in a Sequential.
let private bindConstructType (cenv: cenv) (env: BinderEnvironment) syntaxNode (syntaxConstructTy: OlySyntaxConstructType) =
    match syntaxConstructTy with
    | OlySyntaxConstructType.Anonymous(_, syntaxFieldPatList, _) ->
        match env.isInInstanceConstructorType with
        | Some(ty) ->
            if not env.isReturnable then
                cenv.diagnostics.Error("Constructing the type in a constructor is only allowed as the last expression of a branch.", 10, syntaxNode)

            let fields = ty.GetInstanceFields()

            let syntaxFields = 
                syntaxFieldPatList.ChildrenOfType
        
            let thisExpr = 
                match env.implicitThisOpt with
                | Some(receiver) -> BoundExpression.CreateValue(cenv.syntaxTree, receiver)
                | _ ->
                    // TODO: We need a test for this error.
                    cenv.diagnostics.Error("Expected a 'this' value.", 10, syntaxNode)
                    E.Error(BoundSyntaxInfo.Generated(cenv.syntaxTree))

            let currentFieldSet = HashSet()

            let setFieldExprs =
                syntaxFields
                |> Seq.map (fun syntaxFieldPat ->
                    match syntaxFieldPat with
                    | OlySyntaxFieldPattern.FieldPattern(syntaxName, _, syntaxExpr) ->
                        let fieldName = syntaxName.LastIdentifier.ValueText
                        let tryFindField() =
                            fields 
                            |> ImArray.tryFind (fun x -> 
                                if x.Name = fieldName then
                                    true
                                else
                                    match x.AssociatedFormalPropertyId with
                                    | Some(formalPropId) ->
                                        // TODO/REVIEW: Extrinsic could pick up properties from extensions, do we want that?
                                        ty.FindProperties(env.benv, QueryMemberFlags.Instance, QueryProperty.IntrinsicAndExtrinsic, fieldName)
                                        |> Seq.tryExactlyOne
                                        |> Option.filter (fun x -> x.Formal.Id = formalPropId)
                                        |> Option.isSome
                                    | _ ->
                                        false
                            )
                        match tryFindField() with
                        | Some(field) ->
                            if not(currentFieldSet.Add(fieldName)) then
                                cenv.diagnostics.Error($"Field '{fieldName}' has already been assigned.", 10, syntaxConstructTy)

                            let expectedTyOpt = Some field.Type
                            let _, boundExpr = bindLocalExpression cenv (env.SetReturnable(false)) expectedTyOpt syntaxExpr syntaxExpr

                            let syntaxInfo =
                                BoundSyntaxInfo.User(syntaxFieldPat, env.benv, Some syntaxName, None)

                            BoundExpression.SetField(syntaxInfo, thisExpr, field, boundExpr)
                        | _ ->
                            cenv.diagnostics.Error($"Field '{fieldName}' does not exist on type '{printType env.benv ty}'.", 10, syntaxConstructTy)
                            invalidExpression syntaxFieldPat env.benv

                    | OlySyntaxFieldPattern.Error _ ->
                        invalidExpression syntaxFieldPat env.benv

                    | _ ->
                        raise(InternalCompilerException())
                )
                |> List.ofSeq

            if setFieldExprs.IsEmpty then
                let noneExpr = BoundExpression.None(BoundSyntaxInfo.Generated(cenv.syntaxTree))
                BoundExpression.CreateSequential(noneExpr, noneExpr, ConstructorInitSequential)
            elif setFieldExprs.Length = 1 then
                let noneExpr = BoundExpression.None(BoundSyntaxInfo.Generated(cenv.syntaxTree))
                BoundExpression.CreateSequential(noneExpr, setFieldExprs[0], ConstructorInitSequential)
            else
                BoundExpression.CreateSequential(cenv.syntaxTree, setFieldExprs, ConstructorInitSequential)
        | _ ->
            cenv.diagnostics.Error("Construction of a type not allowed in this context.", 10, syntaxNode)
            invalidExpression syntaxNode env.benv

    | _ ->
        cenv.diagnostics.Error("Creating named records not implemented.", 10, syntaxNode)
        BoundExpression.Error(BoundSyntaxInfo.Generated(cenv.syntaxTree))

let private bindElseIfOrElseExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (syntaxToCapture: OlySyntaxNode) syntaxTargetExpr conditionExpr syntaxElseIfOrElseExpr =
    match syntaxElseIfOrElseExpr with
    | OlySyntaxElseIfOrElseExpression.ElseIf(_, _, _, syntaxWhenConditionExpr, _, syntaxWhenTargetExpr, syntaxNextExpr) ->
        let whenConditionExpr = bindLocalExpression cenv (env.SetReturnable(false)) (Some TypeSymbol.Bool) syntaxWhenConditionExpr syntaxWhenConditionExpr |> snd

        let expectedTy =
            match expectedTyOpt with
            | None -> mkInferenceVariableType None
            | Some(expectedTy) -> expectedTy
        let trueTargetExpr = bindLocalExpression cenv env (Some expectedTy) syntaxTargetExpr syntaxTargetExpr |> snd
        let falseTargetExpr = bindElseIfOrElseExpression cenv env (Some expectedTy) syntaxElseIfOrElseExpr syntaxWhenTargetExpr whenConditionExpr syntaxNextExpr |> snd
        env, BoundExpression.IfElse(BoundSyntaxInfo.User(syntaxToCapture, env.benv), conditionExpr, trueTargetExpr, falseTargetExpr, expectedTy)

    | OlySyntaxElseIfOrElseExpression.Else(_, syntaxElseTargetExpr) ->
        let expectedTy =
            match expectedTyOpt with
            | None -> mkInferenceVariableType None
            | Some(expectedTy) -> expectedTy
        let trueTargetExpr = bindLocalExpression cenv env (Some expectedTy) syntaxTargetExpr syntaxTargetExpr |> snd
        let falseTargetExpr = bindLocalExpression cenv env (Some expectedTy) syntaxElseTargetExpr syntaxElseTargetExpr |> snd
        env, BoundExpression.IfElse(BoundSyntaxInfo.User(syntaxToCapture, env.benv), conditionExpr, trueTargetExpr, falseTargetExpr, expectedTy)

    | OlySyntaxElseIfOrElseExpression.None _ ->
        let targetExpr = bindLocalExpression cenv env (Some TypeSymbol.Unit) syntaxTargetExpr syntaxTargetExpr |> snd
        env, BoundExpression.IfElse(BoundSyntaxInfo.User(syntaxToCapture, env.benv), conditionExpr, targetExpr,
            BoundExpression.None(BoundSyntaxInfo.Generated(cenv.syntaxTree)),
            TypeSymbol.Unit
        )

    | _ ->
        failwith "Invalid expression."

let private bindCatchOrFinallyExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (catchCasesBuilder: BoundCatchCase imarrayb) syntaxCatchOrFinallyExpr =
    match syntaxCatchOrFinallyExpr with
    | OlySyntaxCatchOrFinallyExpression.None _ ->
        catchCasesBuilder.ToImmutable(), None

    | OlySyntaxCatchOrFinallyExpression.Finally(_, syntaxFinallyBodyExpr) ->
        let _, finallyBodyExpr = bindLocalExpression cenv (env.SetReturnable(false)) (Some TypeSymbol.Unit) syntaxFinallyBodyExpr syntaxFinallyBodyExpr
        catchCasesBuilder.ToImmutable(), Some finallyBodyExpr

    | OlySyntaxCatchOrFinallyExpression.Catch(_, _, syntaxPar, _, _, syntaxCatchBodyExpr, syntaxNextCatchOrFinallyExpr) ->
        let _, par = bindParameter cenv env None false syntaxPar
        let envForCatchCase = env.SetUnqualifiedValue(par)
        let _, catchBodyExpr = bindLocalExpression cenv envForCatchCase expectedTyOpt syntaxCatchBodyExpr syntaxCatchBodyExpr
        let catchCase = 
            BoundCatchCase.CatchCase(
                BoundSyntaxInfo.User(syntaxCatchOrFinallyExpr, env.benv), 
                par, 
                catchBodyExpr
            )
        catchCasesBuilder.Add(catchCase)
        bindCatchOrFinallyExpression cenv env expectedTyOpt catchCasesBuilder syntaxNextCatchOrFinallyExpr

    | _ ->
        raise(InternalCompilerUnreachedException())

let private resolveLetBindFunction (cenv: cenv) (env: BinderEnvironment) syntaxToCapture syntaxNode (funcTy: TypeSymbol) =
    let resInfo = ResolutionInfo.Create(ValueNone, ResolutionTypeArity.Any, ResolutionContext.ValueOnly)
    let resInfo = { resInfo with resArgs = ResolutionArguments.ByFunctionType(funcTy) }

    let ident = "let!"

    let value =
        match bindIdentifierAsFormalItem cenv env syntaxNode None resInfo ident with
        | ResolutionFormalItem.Value(_, value) when value.IsFunction ->
            let resItem = resolveFormalValue cenv env syntaxToCapture syntaxNode None resInfo (cenv.syntaxTree.DummyNode, ImArray.empty) value
            match resItem with
            | ResolutionItem.Expression(BoundExpression.Value(value=value)) -> value
            | _ ->
                cenv.diagnostics.Error("Invalid use of 'let!'.", 10, syntaxNode)
                invalidValue None
        | ResolutionFormalItem.Error ->
            invalidValue None
        | _ ->
            cenv.diagnostics.Error("Invalid use of 'let!'.", 10, syntaxNode)
            invalidValue None
            
    env, value

let private bindThrowExpression cenv (env: BinderEnvironment) syntaxNode syntaxArgExpr : _ * _ =
    let argExprs = 
        bindLocalExpression cenv (env.SetReturnable(false)) None syntaxArgExpr syntaxArgExpr |> snd
        |> ImArray.createOne

    let resInfo = ResolutionInfo.Create(ValueSome argExprs, ResolutionTypeArity.Any, ResolutionContext.ValueOnly)
    let expr =
        match bindIdentifierAsFormalItem cenv env syntaxNode None resInfo "throw" with
        | ResolutionFormalItem.Value(_, value) ->
            let syntaxInfo = BoundSyntaxInfo.User(syntaxNode, env.benv, None, None)
            bindValueAsCallExpression cenv env syntaxInfo None (ValueSome argExprs) ImArray.empty value
            |> fst
        | ResolutionFormalItem.Error ->
            invalidExpression syntaxNode env.benv
        | _ ->
            cenv.diagnostics.Error("Invalid use of 'throw'.", 10, syntaxNode)
            invalidExpression syntaxNode env.benv
       
    env, expr

let private bindIndexer cenv (env: BinderEnvironment) syntaxToCapture syntaxBodyExpr (syntaxBrackets: OlySyntaxBrackets<OlySyntaxSeparatorList<OlySyntaxExpression>>) (syntaxRhsExprOpt: OlySyntaxExpression option): _ * _ =
    let syntaxIndexArgExprs = syntaxBrackets.Element.ChildrenOfType

    let rank = syntaxIndexArgExprs.Length
    let ident =
        // TODO: Handle more ranks..
        match rank with
        | 2 -> "[,]"
        | _ -> "[]"

    let syntaxArgExprs = syntaxIndexArgExprs |> ImArray.prependOne syntaxBodyExpr
    let syntaxArgExprs =
        match syntaxRhsExprOpt with
        | Some syntaxRhsExpr ->
            syntaxArgExprs.Add(syntaxRhsExpr)
        | _ ->
            syntaxArgExprs
    let argExprs = 
        syntaxArgExprs
        |> ImArray.mapi (fun i x ->
            let expr = bindLocalExpression cenv (env.SetReturnable(false)) None x x |> snd
            if i = 0 then
                expr
                |> AddressOfReceiverIfPossible expr.Type
            else
                expr
        )

    let resInfo = ResolutionInfo.Create(ValueSome argExprs, ResolutionTypeArity.Any, ResolutionContext.ValueOnly)
    let expr =
        match bindIdentifierAsFormalItem cenv env syntaxToCapture None resInfo ident with
        | ResolutionFormalItem.Value(_, value) ->
            let syntaxInfo = BoundSyntaxInfo.User(syntaxToCapture, env.benv, None, None)
            bindValueAsCallExpression cenv env syntaxInfo None (ValueSome argExprs) ImArray.empty value
            |> fst
        | ResolutionFormalItem.Error ->
            invalidExpression syntaxToCapture env.benv
        | _ ->
            cenv.diagnostics.Error("Expected '[]' to be an operator.", 10, syntaxToCapture)
            invalidExpression syntaxToCapture env.benv
       
    env, expr

let private bindNewArrayExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (syntaxToCapture: OlySyntaxExpression) (isMutable: bool) (syntaxElements: OlySyntaxExpression imarray) =
    let elements =
        syntaxElements
        |> ImArray.map (fun syntaxElement ->
            let _, item = bindLocalExpression cenv (env.SetReturnable(false)) None syntaxElement syntaxElement
            item
        )
    let elementTy =
        elements
        |> ImArray.tryPick (fun x ->
            let ty = x.Type
            match stripTypeEquations ty with
            | TypeSymbol.InferenceVariable _
            | TypeSymbol.HigherInferenceVariable _ 
            | TypeSymbol.EagerInferenceVariable _ -> None
            | _ -> Some ty
        )
        |> Option.defaultWith(fun () ->
            mkInferenceVariableType None
        )

    match expectedTyOpt with
    | Some(expectedTy) when expectedTy.IsAnyArray ->
        checkTypes (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxToCapture expectedTy.FirstTypeArgument elementTy
    | _ ->
        ()

    elements
    |> ImArray.iter (fun element ->
        checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) elementTy element
    )

    let arrayTy =
        if isMutable then
            TypeSymbol.CreateMutableArray(elementTy)
        else
            TypeSymbol.CreateArray(elementTy)

    env, BoundExpression.NewArray(syntaxToCapture, env.benv, elements, arrayTy)

/// Binds a OlySyntaxName to a BoundExpression. However, the resulting BoundExpression is not checked.
let private bindNameAsExpressionWithoutChecking (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) syntaxToCapture syntaxName =
    let resInfo = ResolutionInfo.Create(ValueNone, ResolutionTypeArity.Any, ResolutionContext.ValueOnly)
    let resInfo =
        // If the expected type is a function, our resInfo should include the arg types.
        match expectedTyOpt with
        | Some(ty) ->
            if ty.IsAnyFunction then
                { resInfo with resArgs = ResolutionArguments.ByFunctionType(ty) }
            else
                resInfo
        | _ ->
            resInfo
    bindNameAsItem cenv env (Some syntaxToCapture) None resInfo syntaxName
    |> bindItemAsExpression cenv env
    |> AutoDereferenceIfPossible

let private bindSetExpression (cenv: cenv) (env: BinderEnvironment) syntaxToCapture syntaxLhs syntaxRhs =
    let lhs = 
        match syntaxLhs with
        | OlySyntaxExpression.Name(syntaxName) ->
            bindNameAsExpressionWithoutChecking cenv (env.SetReturnable(false)) None syntaxLhs syntaxName
        | _ ->
            bindLocalExpression cenv (env.SetReturnable(false)) None syntaxLhs syntaxLhs
            |> snd

    let _, rhs = bindLocalExpression cenv (env.SetReturnable(false)) (Some lhs.Type) syntaxRhs syntaxRhs

    let expr =
        let rec setExpr lhs =
            match lhs with
            | BoundExpression.Value(syntaxInfo, value) when not value.IsFunction ->
                // REVIEW: We need force the lambda body expression evaluation.
                //         Could we make this better?
                match rhs with
                | BoundExpression.Lambda(body=lazyBodyExpr) when not lazyBodyExpr.HasExpression ->
                    OlyAssert.Fail("Lambda expression needs evaluation.")
                | _ -> ()

                let syntaxInfo = syntaxInfo.ReplaceIfPossible(syntaxToCapture)
                BoundExpression.SetValue(syntaxInfo, value, rhs)

            | BoundExpression.GetField(syntaxInfo, receiver, field) ->
                let receiverExpr =
                    if field.Enclosing.IsType then
                        AddressOfReceiverIfPossible field.Enclosing.AsType receiver
                    else
                        receiver

                let syntaxInfo = syntaxInfo.ReplaceIfPossible(syntaxToCapture)
                BoundExpression.SetField(syntaxInfo, receiverExpr, field, rhs)

            | BoundExpression.GetProperty(syntaxInfo, receiverOpt, prop, isVirtual) ->
                if prop.Setter.IsNone then
                    cenv.diagnostics.Error($"Property '{prop.Name}' cannot be set.", 10, syntaxInfo.SyntaxNameOrDefault)
                let receiverExprOpt =
                    if prop.Enclosing.IsType then
                        receiverOpt |> Option.map (AddressOfReceiverIfPossible prop.Enclosing.AsType)
                    else
                        receiverOpt

                let syntaxInfo = syntaxInfo.ReplaceIfPossible(syntaxToCapture)
                BoundExpression.SetProperty(syntaxInfo, receiverExprOpt, prop, rhs, isVirtual)

            // Undo the automatic dereference when we are trying to set the value of the by-ref.
            | AutoDereferenced(undoDerefLhsExpr) ->
                if undoDerefLhsExpr.Type.IsByRef_t then
                    BoundExpression.SetContentsOfAddress(BoundSyntaxInfo.User(syntaxToCapture, env.benv), undoDerefLhsExpr, rhs)
                else
                    cenv.diagnostics.Error("Left-hand expression is not a valid mutation.", 10, syntaxLhs)
                    invalidExpression syntaxToCapture env.benv // TODO: We should pass some kind of symbol here, or something.
            | _ ->
                cenv.diagnostics.Error("Left-hand expression is not a valid mutation.", 10, syntaxLhs)
                invalidExpression syntaxToCapture env.benv // TODO: We should pass some kind of symbol here, or something.

        setExpr lhs
    expr

let private bindSetIndexerExpression (cenv: cenv) (env: BinderEnvironment) syntaxToCapture (expectedTyOpt: TypeSymbol option) syntaxBodyExpr syntaxIndexArgExprs syntaxRhs =
    let _, expr = bindIndexer cenv env syntaxToCapture syntaxBodyExpr syntaxIndexArgExprs (Some syntaxRhs)
    expr

let private bindLocalValueDeclaration 
        (cenv: cenv) 
        (env: BinderEnvironment) 
        (expectedTyOpt: TypeSymbol option) 
        (syntaxToCapture: OlySyntaxExpression) 
        (syntaxAttrs, syntaxValueDeclPremodifierList: OlySyntaxList<_>, syntaxValueDeclKind, syntaxValueDeclPostmodifierList: OlySyntaxList<_>, syntaxBinding, syntaxBindingDeclExpr: OlySyntaxExpression)
        (syntaxBodyExprOpt: OlySyntaxExpression option) =

    let memberFlags, valueExplicitness = bindValueModifiersAndKindAsMemberFlags cenv env false syntaxValueDeclPremodifierList.ChildrenOfType syntaxValueDeclKind syntaxValueDeclPostmodifierList.ChildrenOfType
    let isExplicitStatic = valueExplicitness.IsExplicitStatic
    let isExplicitLetBind =
        match syntaxValueDeclKind with
        | OlySyntaxValueDeclarationKind.LetBind _ -> true
        | _ -> false

    let attrs = bindAttributes cenv env true syntaxAttrs

    match syntaxBinding with
    | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, syntaxRhs) ->
        match tryFindIntrinsicAttribute syntaxAttrs attrs with
        | ValueSome(syntaxAttr, _) ->
            cenv.diagnostics.Error("Local values with 'intrinsic' attributes are not allowed.", 10, syntaxAttr)
        | _ ->
            ()

        if isExplicitLetBind then
            let bindFuncTy =
                let tyPar1 = TypeParameterSymbol("t", 0, 1, TypeParameterKind.Function 0, ref ImArray.empty)
                let tyPar2 = TypeParameterSymbol("a", 1, 0, TypeParameterKind.Function 1, ref ImArray.empty)
                let tyPar3 = TypeParameterSymbol("b", 2, 0, TypeParameterKind.Function 2, ref ImArray.empty)

                let returnTy =
                    TypeSymbol.HigherVariable(tyPar1, ImArray.createOne (tyPar3.AsType))

                let par1Ty =
                    TypeSymbol.HigherVariable(tyPar1, ImArray.createOne (tyPar2.AsType))

                let par2Ty =
                    TypeSymbol.CreateFunction(
                        ImArray.empty,
                        ImArray.createOne tyPar2.AsType,
                        returnTy,
                        FunctionKind.Normal
                    )

                TypeSymbol.CreateFunction(ImArray.empty, ImArray.createTwo par1Ty par2Ty, returnTy, FunctionKind.Normal)

            let freshBindFuncTy = freshenType env.benv ImArray.empty ImArray.empty bindFuncTy
            let _, bindValue = resolveLetBindFunction cenv env syntaxToCapture syntaxBindingDecl.Identifier freshBindFuncTy

            let _, arg1 =
                bindLocalExpression cenv env None syntaxRhs syntaxRhs

            let arg2 =
                let par1 = createLocalParameterValue(ImArray.empty, syntaxBindingDecl.Identifier.ValueText, mkInferenceVariableType None, false)
                match syntaxBodyExprOpt with
                | Some syntaxBodyExpr ->
                    BoundExpression.CreateLambda(
                        BoundSyntaxInfo.User(syntaxBodyExpr, env.benv),
                        LambdaFlags.None,
                        ImArray.empty,
                        ImArray.createOne par1,
                        LazyExpression(Some syntaxBodyExpr, fun _ -> 
                            let _, bodyExpr = bindLocalExpression cenv (env.SetUnqualifiedValue(par1)) expectedTyOpt syntaxBodyExpr syntaxBodyExpr
                            bodyExpr
                        )
                    )
                | _ ->
                    BoundExpression.CreateLambda(
                        BoundSyntaxInfo.Generated(cenv.syntaxTree),
                        LambdaFlags.None,
                        ImArray.empty,
                        ImArray.createOne par1,
                        LazyExpression(None, fun _ -> 
                            BoundExpression.None(BoundSyntaxInfo.Generated(cenv.syntaxTree))
                        )
                    )

            let callExpr, _ =
                let syntaxInfo = BoundSyntaxInfo.User(syntaxToCapture, env.benv, None, None)
                bindValueAsCallExpression
                    cenv
                    env
                    syntaxInfo
                    None
                    (ValueSome(ImArray.createTwo arg1 arg2))
                    ImArray.empty
                    bindValue

            let callExpr =
                checkExpression cenv env None callExpr

            env, callExpr
        else

        let bindingInfo = bindLetBindingDeclaration cenv env (syntaxAttrs, attrs) false memberFlags valueExplicitness syntaxBindingDecl

        if isExplicitStatic && not bindingInfo.IsFunction then
            cenv.diagnostics.Error("The 'static' modifier can only be used on local functions.", 10, syntaxBindingDecl.Identifier)

        let envWithValue =
            if bindingInfo.Value.IsFunction then
                env.SetUnqualifiedValue(bindingInfo.Value)
            else
                env

        let envForRhsExpr = 
            let env = envWithValue.SetReturnable(false)
            if bindingInfo.Value.IsLocal && bindingInfo.Value.IsFunction then
                setIsInLocalLambda env
            else
                env
        let rhsExpr = bindLetValueRightSideExpression cenv envForRhsExpr bindingInfo syntaxBindingDecl syntaxRhs
        let bindingInfo, rhsExpr = checkLetBindingDeclarationAndAutoGeneralize (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxBinding bindingInfo rhsExpr
        recordValueDeclaration cenv bindingInfo.Value syntaxBindingDecl.Identifier

        let bodyExpr = 
            match syntaxBodyExprOpt with
            | Some syntaxBodyExpr ->
                let syntaxToCaptureForBodyExpr =
                    match syntaxBodyExpr with
                    | OlySyntaxExpression.None _ ->
                        syntaxBindingDeclExpr
                    | _ ->
                        syntaxBodyExpr
                bindLocalExpression cenv (env.SetUnqualifiedValue(bindingInfo.Value)) expectedTyOpt syntaxToCaptureForBodyExpr syntaxBodyExpr
                |> snd
            | _ ->
                BoundExpression.None(BoundSyntaxInfo.Generated(cenv.syntaxTree))

        env, BoundExpression.Let(BoundSyntaxInfo.User(syntaxToCapture, env.benv), bindingInfo, rhsExpr, bodyExpr)

    | _ ->
        cenv.diagnostics.Error("Member declarations are not allowed in a local context.", 10, syntaxBinding.Declaration.Identifier)
        env, BoundExpression.Error(BoundSyntaxInfo.User(syntaxToCapture, env.benv))

let private bindLetPatternBinding (cenv: cenv) (env: BinderEnvironment) expectedTyOpt (syntaxToCapture: OlySyntaxExpression) (syntaxLetPatternBinding: OlySyntaxLetPatternBinding) (syntaxBodyExprOpt: OlySyntaxExpression option) =
    match syntaxLetPatternBinding with
    | OlySyntaxLetPatternBinding.Binding(syntaxLetToken, syntaxPat, _, syntaxRhsExpr) ->
        let matchTy = mkInferenceVariableType None
        let envOfBinding, pat = bindPattern cenv env (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) true (Dictionary()) (HashSet()) matchTy syntaxPat
        let rhsExpr = bindValueRightSideExpression cenv env matchTy (env.SetReturnable(false)) syntaxRhsExpr

        let _, bodyExpr =
            match syntaxBodyExprOpt with
            | Some(syntaxBodyExpr) ->
                bindLocalExpression cenv envOfBinding expectedTyOpt syntaxBodyExpr syntaxBodyExpr
            | _ ->
                envOfBinding, BoundExpression.None(BoundSyntaxInfo.Generated(cenv.syntaxTree))

        let matchPat = BoundMatchPattern.Cases(syntaxPat, ImArray.createOne pat)
        let matchClause = BoundMatchClause.MatchClause(syntaxPat, matchPat, None, bodyExpr)
        let matchClauses = ImArray.createOne matchClause

        checkExhaustiveness cenv env syntaxLetToken matchClauses

        env, BoundExpression.Match(syntaxToCapture, env.benv, ImArray.createOne rhsExpr, matchClauses, bodyExpr.Type)
    | _ ->
        OlyAssert.Fail("Unmatched syntax expression.")

#if DEBUG || CHECKED
let private bindLocalExpressionAux (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (syntaxToCapture: OlySyntaxExpression) (syntaxExpr: OlySyntaxExpression) =
    StackGuard.Do(fun () ->
        bindLocalExpressionAuxAux cenv env expectedTyOpt syntaxToCapture syntaxExpr
    )
let private bindLocalExpressionAuxAux (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (syntaxToCapture: OlySyntaxExpression) (syntaxExpr: OlySyntaxExpression) =
#else
let private bindLocalExpressionAux (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (syntaxToCapture: OlySyntaxExpression) (syntaxExpr: OlySyntaxExpression) =
#endif
    cenv.ct.ThrowIfCancellationRequested()

    let env = env.SetEnclosing(EnclosingSymbol.Local)

    match syntaxExpr with
    | OlySyntaxExpression.Mutate(syntaxLhs, _, syntaxRhs) ->
        match syntaxLhs with
        | OlySyntaxExpression.Indexer(syntaxBodyExpr, syntaxIndexArgExprs) ->
            env, bindSetIndexerExpression cenv env syntaxToCapture expectedTyOpt syntaxBodyExpr syntaxIndexArgExprs syntaxRhs |> checkExpression cenv env expectedTyOpt
        | _ ->
            env, bindSetExpression cenv env syntaxToCapture syntaxLhs syntaxRhs |> checkExpression cenv env expectedTyOpt

    | OlySyntaxExpression.Typed(syntaxBody, _, syntaxTy) ->
        let ty = bindType cenv env None ResolutionTypeArityZero syntaxTy
        let env1, expr = bindLocalExpression cenv (env.SetReturnable(false)) (Some ty) syntaxBody syntaxBody
        let expr = BoundExpression.Typed(BoundSyntaxInfo.User(syntaxExpr, env.benv), expr, ty)
        env1, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.Lambda(syntaxLambdaKind, syntaxPars, _, syntaxBodyExpr) ->
        let env = setIsInLocalLambda env
        let env1, expr = bindLambdaExpression cenv env syntaxToCapture syntaxLambdaKind syntaxPars syntaxBodyExpr
        env1, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.UpdateRecord(syntaxBody, _, syntaxConstructTy) ->
        match syntaxConstructTy with
        | OlySyntaxConstructType.Anonymous _ when env.isReturnable && env.isInInstanceConstructorType.IsSome ->
            // We call the Aux version as to prevent the constructor field assignment checks from happening at this point.
            // REVIEW: Is there a better way to handle this without having to call the Aux version?
            let env1, bodyExpr = bindLocalExpressionAux cenv env (* do not set returnable *) None syntaxBody syntaxBody
            match bodyExpr with
            | BoundExpression.Call(value=value) when value.IsBase && value.IsFunction ->
                env,
                E.Sequential(
                    BoundSyntaxInfo.User(syntaxExpr, env.benv),
                    bodyExpr, // IMPORTANT: Call base constructor first!
                    bindConstructType cenv env1 syntaxExpr syntaxConstructTy,
                    ConstructorInitSequential
                )
            | _ ->
                cenv.diagnostics.Error("This kind of record updates are not implemented.", 10, syntaxToCapture)
                env, BoundExpression.Error(BoundSyntaxInfo.Generated(cenv.syntaxTree))
        | _ ->
            let _env1, _bodyExpr = bindLocalExpression cenv (env.SetReturnable(false)) None syntaxBody syntaxBody
            cenv.diagnostics.Error("Records not implemented (yet).", 10, syntaxToCapture)
            env, BoundExpression.Error(BoundSyntaxInfo.Generated(cenv.syntaxTree))

    | OlySyntaxExpression.CreateRecord(syntaxConstructTy) ->
        let implicitBaseCtorCallExprOpt =
            match syntaxConstructTy, env.isInInstanceConstructorType with
            | OlySyntaxConstructType.Anonymous _, Some(ty) ->
                if not ty.Inherits.IsEmpty && not ty.IsAnyStruct then
                    let implicitParameterlessBaseInstanceCtorOpt =
                        if env.implicitThisOpt.IsNone then
                            None
                        else
                            match env.benv.TryGetUnqualifiedFunction("base", 0) with
                            | Some func ->
                                let funcs =
                                    match func with
                                    | :? FunctionGroupSymbol as funcGroup ->
                                        funcGroup.Functions
                                    | _ ->
                                        ImArray.createOne func
                                funcs
                                |> ImArray.tryFind (fun x -> 
                                    x.IsInstanceConstructor && x.LogicalParameterCount = 0
                                )
                            | _ ->
                                None
                    match implicitParameterlessBaseInstanceCtorOpt with
                    | None ->
                        // This is a special case. 
                        // The Oly runtime does not require that we need to call the base object constructor.
                        if ty.Inherits[0].IsBaseObject_t |> not then
                            cenv.diagnostics.Error($"Cannot implicitly call parameterless base constructor for type '{printType env.benv ty}' as it does not exist.", 10, syntaxConstructTy)
                        None
                    | Some(baseCtor) ->
                        let thisValue = env.implicitThisOpt.Value
                        BoundExpression.Call(
                            BoundSyntaxInfo.Generated(cenv.syntaxTree), 
                            Some(BoundExpression.Value(BoundSyntaxInfo.Generated(cenv.syntaxTree), thisValue)),
                            ImArray.empty,
                            ImArray.empty,
                            baseCtor,
                            CallFlags.None
                        )
                        |> Some
                else
                    None

            | _ ->
                None

        match implicitBaseCtorCallExprOpt with
        | Some(baseCtorCallExpr) ->
            env, BoundExpression.CreateSequential(baseCtorCallExpr, bindConstructType cenv env syntaxToCapture syntaxConstructTy)
        | _ ->
            env, bindConstructType cenv env syntaxToCapture syntaxConstructTy

    | OlySyntaxExpression.OpenDeclaration _
    | OlySyntaxExpression.OpenStaticDeclaration _
    | OlySyntaxExpression.OpenExtensionDeclaration _ ->
        cenv.diagnostics.Error("Open declarations are not allowed in a local context.", 10, syntaxExpr)
        env, BoundExpression.None(BoundSyntaxInfo.User(syntaxExpr, env.benv))

    | OlySyntaxExpression.Sequential(
            OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, syntaxValueDeclPremodifierList, syntaxValueDeclKind, syntaxValueDeclPostmodifierList, syntaxBinding) as syntaxBindingDeclExpr,
            syntaxBodyExpr
      ) ->
        bindLocalValueDeclaration cenv env expectedTyOpt syntaxToCapture (syntaxAttrs, syntaxValueDeclPremodifierList, syntaxValueDeclKind, syntaxValueDeclPostmodifierList, syntaxBinding, syntaxBindingDeclExpr)
            (Some syntaxBodyExpr)

    | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, syntaxValueDeclPremodifierList, syntaxValueDeclKind, syntaxValueDeclPostmodifierList, syntaxBinding) as syntaxBindingDeclExpr ->
        bindLocalValueDeclaration cenv env expectedTyOpt syntaxToCapture (syntaxAttrs, syntaxValueDeclPremodifierList, syntaxValueDeclKind, syntaxValueDeclPostmodifierList, syntaxBinding, syntaxBindingDeclExpr)
            None

    | OlySyntaxExpression.Sequential(
            OlySyntaxExpression.LetPatternDeclaration(syntaxLetPatternBinding),
            syntaxBodyExpr
      ) ->
        bindLetPatternBinding cenv env expectedTyOpt syntaxToCapture syntaxLetPatternBinding (Some syntaxBodyExpr)

    | OlySyntaxExpression.LetPatternDeclaration(syntaxLetPatternBinding) ->
        bindLetPatternBinding cenv env expectedTyOpt syntaxExpr syntaxLetPatternBinding None

    | OlySyntaxExpression.Sequential(leftSyntax, rightSyntax) ->
        bindSequentialExpression cenv env expectedTyOpt syntaxToCapture leftSyntax rightSyntax

    | OlySyntaxExpression.Parenthesis(_, syntaxExprList, _) ->
        bindParenthesisExpression cenv env expectedTyOpt syntaxToCapture syntaxExprList

    | OlySyntaxExpression.Name syntaxName ->
        let expr = bindNameAsExpressionWithoutChecking cenv env expectedTyOpt syntaxExpr syntaxName
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.Literal syntaxLiteral ->
        let expr = BoundExpression.Literal(BoundSyntaxInfo.User(syntaxLiteral, env.benv), bindLiteral cenv env expectedTyOpt syntaxLiteral)
        env, expr

    | OlySyntaxExpression.MemberAccess(syntaxReceiver, _, syntaxMemberExpr) ->
        let expr =
            bindMemberAccessExpressionAsItem cenv (env.SetReturnable(false)) syntaxExpr None syntaxReceiver syntaxMemberExpr
            |> bindItemAsExpression cenv env
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.Call(syntaxAppBodyExpr, syntaxArgs) ->
        let syntaxArgs = getSyntaxArgumentsAsSyntaxExpressions cenv syntaxArgs
        let expr = bindCallExpression cenv env syntaxToCapture None syntaxAppBodyExpr syntaxArgs
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.InfixCall(syntaxLeft, syntaxName, syntaxRight) ->
        let _, left = bindLocalExpression cenv (env.SetReturnable(false)) None syntaxLeft syntaxLeft
        let _, right = bindLocalExpression cenv (env.SetReturnable(false)) None syntaxRight syntaxRight
        let argExprs = ImmutableArray.CreateRange[left;right]
        let resInfo = ResolutionInfo.Create(ValueSome argExprs, ResolutionTypeArity.Any, ResolutionContext.ValueOnly)
        let expr =
            bindNameAsItem cenv env (Some syntaxToCapture) None resInfo syntaxName
            |> bindItemAsExpression cenv env
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.PrefixCall(syntaxName, syntaxArg) ->
        let _, argExpr = bindLocalExpression cenv (env.SetReturnable(false)) None syntaxArg syntaxArg
        let argExprs = ImmutableArray.CreateRange[argExpr]
        let resInfo = ResolutionInfo.Create(ValueSome argExprs, ResolutionTypeArity.Any, ResolutionContext.ValueOnly)
        let expr =
            bindNameAsItem cenv env (Some syntaxToCapture) None resInfo syntaxName
            |> bindItemAsExpression cenv env
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.Throw(_, syntaxArgExpr) ->
        let env, expr = bindThrowExpression cenv env syntaxToCapture syntaxArgExpr
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.Indexer(syntaxLhsExpr, syntaxBrackets) ->
        let env, expr = bindIndexer cenv env syntaxExpr syntaxLhsExpr syntaxBrackets None
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.Array(_, syntaxElements, _) ->
        let env, expr = bindNewArrayExpression cenv env expectedTyOpt syntaxToCapture false syntaxElements.ChildrenOfType
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.MutableArray(_, _, syntaxElements, _) ->
        let env, expr = bindNewArrayExpression cenv env expectedTyOpt syntaxToCapture true syntaxElements.ChildrenOfType
        env, checkExpression cenv env expectedTyOpt expr

    | OlySyntaxExpression.If(_, _, syntaxConditionExpr, _, syntaxTargetExpr, syntaxElseIfOrElseExpr) ->
        let conditionExpr = bindLocalExpression cenv (env.SetReturnable(false)) (Some TypeSymbol.Bool) syntaxConditionExpr syntaxConditionExpr |> snd
        bindElseIfOrElseExpression cenv env expectedTyOpt syntaxToCapture syntaxTargetExpr conditionExpr syntaxElseIfOrElseExpr

    | OlySyntaxExpression.While(_, _, syntaxConditionExpr, _, syntaxBodyExpr) ->
        let conditionExpr = bindLocalExpression cenv (env.SetReturnable(false)) (Some TypeSymbol.Bool) syntaxConditionExpr syntaxConditionExpr |> snd
        let bodyExpr = bindLocalExpression cenv (env.SetReturnable(false)) (Some TypeSymbol.Unit) syntaxBodyExpr syntaxBodyExpr |> snd
        env, E.While(BoundSyntaxInfo.User(syntaxToCapture, env.benv), conditionExpr, bodyExpr)

    | OlySyntaxExpression.Match(syntaxMatchToken, _, syntaxExprList, _, syntaxMatchCaseList) ->
        let exprs =
            syntaxExprList.ChildrenOfType
            |> ImArray.map (fun syntaxExpr -> bindLocalExpression cenv (env.SetReturnable(false)) None syntaxExpr syntaxExpr |> snd)
        bindMatchExpression cenv env (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expectedTyOpt syntaxToCapture syntaxMatchToken exprs syntaxMatchCaseList.ChildrenOfType

    | OlySyntaxExpression.Try(_, syntaxBodyExpr, syntaxCatchOrFinallyExpr) ->
        let bodyExpr = bindLocalExpression cenv env expectedTyOpt syntaxBodyExpr syntaxBodyExpr |> snd
        let catchCases, finallyBodyExprOpt = bindCatchOrFinallyExpression cenv env expectedTyOpt (ImArray.builder()) syntaxCatchOrFinallyExpr
        env, E.Try(BoundSyntaxInfo.User(syntaxToCapture, env.benv), bodyExpr, catchCases, finallyBodyExprOpt)

    | OlySyntaxExpression.TypeDeclaration(syntaxAttrs, syntaxAccessor, syntaxTyDefKind, syntaxTyDefName, syntaxTyPars, syntaxConstrClauseList, _, syntaxTyDefBody) ->
        let syntaxIdent = syntaxTyDefName.Identifier
        let innerEnv = setSkipCheckTypeConstructor env
        let innerEnv, entities, entBuilder = bindTypeDeclarationPass0 { cenv with pass = Pass0; entityDefIndex = 0 } innerEnv syntaxAttrs syntaxAccessor syntaxTyDefKind syntaxIdent syntaxTyPars syntaxTyDefBody ImArray.empty
        let innerEnv = scopeInEntity innerEnv entBuilder.Entity
        let innerEnv = bindTypeDeclarationPass1 { cenv with pass = Pass1; entityDefIndex = 0 } innerEnv entities syntaxIdent syntaxTyPars syntaxConstrClauseList.ChildrenOfType syntaxTyDefBody
        bindTypeDeclarationPass2 { cenv with pass = Pass2; entityDefIndex = 0 } innerEnv entities syntaxIdent syntaxTyPars syntaxTyDefBody
        let innerEnv = scopeInInstanceConstructors innerEnv entBuilder.Entity |> unsetSkipCheckTypeConstructor
        let innerEnv = bindTypeDeclarationPass3 { cenv with pass = Pass3; entityDefIndex = 0 } innerEnv entities syntaxAttrs syntaxIdent syntaxConstrClauseList.ChildrenOfType syntaxTyDefBody
        let innerEnv, expr = bindTypeDeclarationPass4 { cenv with pass = Pass4; entityDefIndex = 0 } innerEnv syntaxToCapture entities syntaxIdent syntaxTyPars syntaxConstrClauseList syntaxTyDefBody

        // REVIEW: This *could* be expensive if the locally declared type is complicated enough.
        //         We do this for better error reporting, otherwise, simply clearing all locals before
        //         binding the type declaration would also work, but the error message would be a generic
        //         "identifier not found".
        checkStaticContextForFreeLocals (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expr ImArray.empty

        let enclosingTyParTys = innerEnv.GetEnclosingTypeParametersAsTypes()
        // ---------
        let env = env.SetEnclosingTypeArguments(entBuilder.Entity.Id, enclosingTyParTys)
        let env = scopeInInstanceConstructors env entBuilder.Entity
        scopeInEntity env entBuilder.Entity, expr

    | OlySyntaxExpression.None _ ->
        env, checkExpression cenv env expectedTyOpt (BoundExpression.None(BoundSyntaxInfo.User(syntaxToCapture, env.benv)))

    | OlySyntaxExpression.Error _ ->
        env, invalidExpression syntaxToCapture env.benv

    | _ ->
        raise(InternalCompilerUnreachedException())

let bindLocalExpression (cenv: cenv) (env: BinderEnvironment) (expectedTyOpt: TypeSymbol option) (syntaxToCapture: OlySyntaxExpression) (syntaxExpr: OlySyntaxExpression) =
    let env, expr = bindLocalExpressionAux cenv env expectedTyOpt syntaxToCapture syntaxExpr

    let expr =
        // Specific checks for expressions on return.
        if env.isReturnable && not env.isInLocalLambda then
            // Check constructor to ensure fields are set.
            match env.isInInstanceConstructorType with
            | Some(enclosingTy) ->
                OlyAssert.True(env.implicitThisOpt.IsSome)
                let thisValue = env.implicitThisOpt.Value
                checkConstructorImplementation (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) thisValue enclosingTy expr
            | _ ->
                expr
        else
            expr

    if env.isInInstanceConstructorType.IsNone || (env.isInInstanceConstructorType.IsSome && not env.isReturnable) then
        match expr with
        | E.Lambda _ ->
            if not env.isPassedAsArgument then
                checkImmediateExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) env.isReturnable expr
                match expectedTyOpt with
                // TODO: Do we really need to check for 'isReturnable' here? It was put here to prevent duplicate error messages...
                | Some(expectedTy) when not env.isReturnable ->
                    checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) expectedTy expr
                | _ ->
                    ()
        | _ ->
            checkImmediateExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) env.isReturnable expr

    let expr =
        match expr with
        | Cast(castFunc, argExpr) ->
            let syntaxInfo = BoundSyntaxInfo.User(syntaxToCapture, env.benv)
            BoundExpression.Witness(syntaxInfo, env.benv, castFunc, argExpr, ref None, expr.Type)
        | _ ->
            Oly.Compiler.Internal.ImplicitRules.ImplicitReturn
                expectedTyOpt
                expr

    env, expr

let bindAnonymousShapeType (cenv: cenv) (env: BinderEnvironment) (tyPars: TypeParameterSymbol imarray) (syntaxExprList: OlySyntaxSeparatorList<OlySyntaxExpression>) =
    let env =
        if tyPars.IsEmpty then env
        else
            (env, tyPars)
            ||> ImArray.fold (fun env tyPar ->
                scopeInTypeParameter env tyPar
            )
    let entBuilder = EntitySymbolBuilder.CreateAnonymousShape(currentEnclosing env, cenv.asm)
    let env = env.SetEnclosing(entBuilder.Entity.AsEnclosing).SetEnclosingTypeParameters(tyPars)

    entBuilder.SetTypeParameters(Pass0, tyPars)

    let syntaxExprs = syntaxExprList.ChildrenOfType

    let env, entities =
        let cenv = { cenv with pass = Pass0 }
        ((env, ImArray.empty), syntaxExprs)
        ||> ImArray.fold (fun (env, entities) syntaxExpr ->
            bindTopLevelExpressionPass0 cenv env entities syntaxExpr
        )
    
    let env =
        let cenv = { cenv with pass = Pass1 }
        (env, syntaxExprs)
        ||> ImArray.fold (fun env syntaxExpr ->
            let env, _ = bindTopLevelExpressionPass1 cenv env false entBuilder entities syntaxExpr
            env
        )

    let bindingInfos, env =
        let cenv = { cenv with pass = Pass2 }
        ((ImArray.empty, env), syntaxExprs)
        ||> ImArray.fold (fun (bindingInfos, env) syntaxExpr ->
            let bindingInfos, env = bindTopLevelExpressionPass2 cenv env ImArray.empty entities bindingInfos syntaxExpr
            bindingInfos, env
        )

    addBindingDeclarationsToEntityPass2 { cenv with pass = Pass2 } env bindingInfos entBuilder

    entBuilder.Entity.AsType