[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Binder

open System
open System.Threading
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.CompilerImports
open Oly.Compiler.Internal.Binder.Attributes
open Oly.Compiler.Internal.Binder.EarlyAttributes

let importReferences (importer: Importer) (env: BinderEnvironment) (ct: CancellationToken) callback =
    let mutable env = env

    let diagnostics = ResizeArray()
    importer.ForEachEntity(diagnostics, ct, (fun ent ->
        ct.ThrowIfCancellationRequested()
        match ent.Enclosing with
        | EnclosingSymbol.RootNamespace ->
            env <- callback env ent
        | _ ->
            if ent.IsNamespace then
                 env <- callback env ent
    ), fun ty ent ->
        env <- env.SetIntrinsicType(ty, ent)
    )

    env, diagnostics.ToImmutableArray()

let computePrologEnvironment (imports: CompilerImports) (diagnostics: OlyDiagnosticLogger) env (declTable: BoundDeclarationTable) openContent ct =
    let env, importDiags = 
        importReferences imports.Importer env ct
            (fun env ent ->
                let env =
                    if ent.IsNamespace then
                        env.AddNamespace(ent)
                    else
                        env
                match ent.Enclosing with
                | EnclosingSymbol.RootNamespace ->
                    if ent.IsPrivate && not (declTable.EntityDeclarations.ContainsKey(ent.Formal)) then
                        env
                    else
                        let env = scopeInEntity env ent
                        if ent.IsAutoOpenable then
                            openContentsOfEntity declTable env openContent ent
                        else
                            env
                | _ ->
                    env
            )

    importDiags
    |> ImArray.iter diagnostics.AddDiagnostic

    env

let bindNamespaceOrModuleDefinitionPass0 (cenv: cenv) (env: BinderEnvironment) syntaxNode (entBuilder: EntitySymbolBuilder) syntaxTyDefBody =
    if not entBuilder.Entity.IsNamespaceOrModule then failwith "Expected namespace or module."

    let nestedEntBuilders =
        if entBuilder.Entity.IsNamespace then
            entBuilder.NestedEntityBuilders
        else
            ImArray.empty

    let env1, nestedEntBuilders = Pass0.bindTypeDeclarationBody cenv env syntaxNode entBuilder nestedEntBuilders syntaxTyDefBody
    entBuilder.SetEntities(cenv.pass, nestedEntBuilders)
    env1

let bindNamespaceOrModuleDefinitionPass1 (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) syntaxTyPars syntaxConstrClauses syntaxTyDefBody =
    if not entBuilder.Entity.IsNamespaceOrModule then failwith "Expected namespace or module."

    let nestedEntBuilders = entBuilder.NestedEntityBuilders
    Pass1.bindTypeDeclarationBody cenv env cenv.syntaxTree.DummyNode true entBuilder nestedEntBuilders syntaxTyPars syntaxConstrClauses syntaxTyDefBody

let bindNamespaceOrModuleDefinitionPass2 (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) syntaxTyPars syntaxTyDefBody =
    let nestedEntBuilders = entBuilder.NestedEntityBuilders
    Pass2.bindTypeDeclarationBody cenv env nestedEntBuilders entBuilder syntaxTyPars syntaxTyDefBody

let bindNamespaceOrModuleDefinitionPass3 (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) syntaxTyDefBody =
    let nestedEntBuilders = entBuilder.NestedEntityBuilders
    Pass3.bindTypeDeclarationBody cenv env nestedEntBuilders entBuilder true syntaxTyDefBody

let bindNamespaceOrModuleDefinitionPass4 (cenv: cenv) (env: BinderEnvironment) syntaxToCapture (entBuilder: EntitySymbolBuilder) (syntaxTyDeclBody: OlySyntaxTypeDeclarationBody) =
    let nestedEntBuilders = entBuilder.NestedEntityBuilders
    let bindingInfos =
        let bindingInfosBuilder = ImmutableDictionary.CreateBuilder()
        (syntaxTyDeclBody, entBuilder.Bindings)
        |> Pass3.ForEachBinding (
            fun _syntaxAttrs syntaxBinding (binding, _) ->
                bindingInfosBuilder.Add(syntaxBinding, binding)
        )
        bindingInfosBuilder.ToImmutable()
    let expr = Pass4.bindTypeDeclarationBody cenv env entBuilder nestedEntBuilders bindingInfos true syntaxTyDeclBody
    if entBuilder.Entity.IsNamespace then
        env, BoundRoot.Namespace(syntaxToCapture, env.benv, entBuilder.Entity, expr)
    else
        let exprWithDef =
            BoundExpression.CreateEntityDefinition(
                BoundSyntaxInfo.User(syntaxToCapture, env.benv),
                expr,
                entBuilder.Entity
            )
        env, BoundRoot.Global(syntaxToCapture, env.benv, exprWithDef)

let bindRootPrePass (cenv: cenv) (env: BinderEnvironment) (syntaxRoot: OlySyntaxCompilationUnit) =
    match syntaxRoot with
    | OlySyntaxCompilationUnit.Namespace(_, _, syntaxTyDefBody, _) ->
        match syntaxTyDefBody with
        | OlySyntaxTypeDeclarationBody.Body(_, _, _, syntaxExpr) ->
            bindTopLevelExpressionPrePass cenv env true syntaxExpr |> fst
        | _ ->
            env

    | OlySyntaxCompilationUnit.AnonymousModule(syntaxTyDefBody, _) ->
        match syntaxTyDefBody with
        | OlySyntaxTypeDeclarationBody.Body(_, _, _, syntaxExpr) ->
            bindTopLevelExpressionPrePass cenv env true syntaxExpr |> fst
        | _ ->
            env

    | OlySyntaxCompilationUnit.Module(_, _, _, _, _, syntaxTyDefBody, _) ->
        match syntaxTyDefBody with
        | OlySyntaxTypeDeclarationBody.Body(_, _, _, syntaxExpr) ->
            bindTopLevelExpressionPrePass cenv env true syntaxExpr |> fst
        | _ ->
            env

    | _ ->
        raise(InternalCompilerException())

let bindRootPass0 (cenv: cenv) (nmsEnv: NamespaceEnvironment) (env: BinderEnvironment) (syntaxRoot: OlySyntaxCompilationUnit) =
    match syntaxRoot with
    | OlySyntaxCompilationUnit.Namespace(_, syntaxName, syntaxTyDefBody, _) ->
        let nmsBuilder = nmsEnv.GetOrCreate(syntaxName.EnclosingPath @ [syntaxName.NameText] |> ImArray.ofSeq)
        let entBuilder = nmsBuilder.EntityBuilder

        let env1 = { env with benv = { env.benv with senv = { env.benv.senv with enclosing = EnclosingSymbol.Entity entBuilder.Entity } } }
        bindNamespaceOrModuleDefinitionPass0 cenv env1 syntaxName entBuilder syntaxTyDefBody, entBuilder       

    | OlySyntaxCompilationUnit.AnonymousModule(syntaxTyDefBody, _) ->
        if cenv.syntaxTree.ParsingOptions.AnonymousModuleDefinitionAllowed |> not then
            cenv.diagnostics.Error("Anonymous module definitions are not available in this context.", 10, cenv.syntaxTree.DummyNode)

        let anonModuleBuilder = EntitySymbolBuilder.CreateModule(Some cenv.asm, EnclosingSymbol.RootNamespace, EntityFlags.Private ||| EntityFlags.AutoOpen, AnonymousEntityName, String.Empty)
        let env1 = { env with benv = { env.benv with senv = { env.benv.senv with enclosing = EnclosingSymbol.Entity anonModuleBuilder.Entity } } }

        recordEntityDeclaration cenv anonModuleBuilder.Entity syntaxRoot
        bindNamespaceOrModuleDefinitionPass0 cenv env1 cenv.syntaxTree.DummyNode anonModuleBuilder syntaxTyDefBody, anonModuleBuilder

    | OlySyntaxCompilationUnit.Module(syntaxAttrs, _, _, syntaxName, _, syntaxTyDefBody, _) ->
        // We only early bind built-in attributes (import, export, intrinsic) in pass(0).
        let attrs = bindEarlyAttributes cenv env syntaxAttrs

        let flags = Pass0.processAttributesForEntityFlags EntityFlags.None attrs

        let entBuilder =
            match syntaxName.EnclosingPath with
            | [] -> EntitySymbolBuilder.CreateModule(Some cenv.asm, EnclosingSymbol.RootNamespace, flags, syntaxName.NameText, syntaxRoot.GetLeadingCommentText())
            | path ->
                let nmsBuilder = nmsEnv.GetOrCreate(path |> ImArray.ofSeq)
                let entBuilder = EntitySymbolBuilder.CreateModule(Some cenv.asm, EnclosingSymbol.Entity(nmsBuilder.Entity), flags, syntaxName.NameText, syntaxRoot.GetLeadingCommentText())
                nmsBuilder.AddEntity(entBuilder.Entity, entBuilder.Entity.LogicalTypeParameterCount)
                entBuilder

        // BEGIN - Bind type parameters
        let syntaxTyPars = syntaxName.GetAllTypeArguments()
        let _, tyPars = bindTypeParameters cenv env false syntaxTyPars

        if OlySyntaxFacts.IsOperator entBuilder.Entity.Name && tyPars.Length <> 1 then
            cenv.diagnostics.Error("Postfix type operators must only have a single type parameter.", 10, syntaxName.LastIdentifier)
        entBuilder.SetTypeParameters(cenv.pass, tyPars)
        // END - Bind type parameters

        let env1 = env.SetEnclosing(EnclosingSymbol.Entity entBuilder.Entity)
        recordEntityDeclaration cenv entBuilder.Entity syntaxName.LastIdentifier
        bindNamespaceOrModuleDefinitionPass0 cenv env1 syntaxName entBuilder syntaxTyDefBody, entBuilder

    | _ ->
        raise(InternalCompilerException())

let bindRootPass1 (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) (syntaxRoot: OlySyntaxCompilationUnit) =
    let env = env.SetEnclosing(entBuilder.Entity.AsEnclosing).SetAccessorContext(entBuilder.Entity)
    match syntaxRoot with
    | OlySyntaxCompilationUnit.Namespace(_, _, syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass1 cenv env entBuilder ImArray.empty ImArray.empty syntaxTyDefBody

    | OlySyntaxCompilationUnit.AnonymousModule(syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass1 cenv env entBuilder ImArray.empty ImArray.empty syntaxTyDefBody

    | OlySyntaxCompilationUnit.Module(_, _, _, syntaxName, syntaxConstrClauseList, syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass1 cenv env entBuilder (syntaxName.GetAllTypeArguments()) syntaxConstrClauseList.ChildrenOfType syntaxTyDefBody

    | _ ->
        raise(InternalCompilerException())

let bindRootPass2 (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) (syntaxRoot: OlySyntaxCompilationUnit) =
    match syntaxRoot with
    | OlySyntaxCompilationUnit.Namespace(_, _, syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass2 cenv env entBuilder ImArray.empty syntaxTyDefBody

    | OlySyntaxCompilationUnit.AnonymousModule(syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass2 cenv env entBuilder ImArray.empty syntaxTyDefBody

    | OlySyntaxCompilationUnit.Module(_, _, _, syntaxName, _, syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass2 cenv env entBuilder (syntaxName.GetAllTypeArguments()) syntaxTyDefBody

    | _ ->
        raise(InternalCompilerException())

let bindRootPass3 (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) (syntaxRoot: OlySyntaxCompilationUnit) =
    let env = env.SetEnclosing(entBuilder.Entity.AsEnclosing).SetAccessorContext(entBuilder.Entity)

    match syntaxRoot with
    | OlySyntaxCompilationUnit.Namespace(_, _, syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass3 cenv env entBuilder syntaxTyDefBody

    | OlySyntaxCompilationUnit.AnonymousModule(syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass3 cenv env entBuilder syntaxTyDefBody

    | OlySyntaxCompilationUnit.Module(syntaxAttrs, _, _, _, syntaxConstrClauseList, syntaxTyDefBody, _) ->
        checkConstraintClauses (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxConstrClauseList.ChildrenOfType entBuilder.Entity.TypeParameters

        let attrs = bindAttributes cenv env syntaxAttrs
        entBuilder.SetAttributes(cenv.pass, attrs)

        bindNamespaceOrModuleDefinitionPass3 cenv env entBuilder syntaxTyDefBody

    | _ ->
        raise(InternalCompilerException())

let bindRootPass4 (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) (syntaxRoot: OlySyntaxCompilationUnit) =
    match syntaxRoot with
    | OlySyntaxCompilationUnit.Namespace(_, _, syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass4 cenv env syntaxRoot entBuilder syntaxTyDefBody

    | OlySyntaxCompilationUnit.AnonymousModule(syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass4 cenv env syntaxRoot entBuilder syntaxTyDefBody

    | OlySyntaxCompilationUnit.Module(_, _, _, _, _, syntaxTyDefBody, _) ->
        bindNamespaceOrModuleDefinitionPass4 cenv env syntaxRoot entBuilder syntaxTyDefBody

    | _ ->
        raise(InternalCompilerException())

let bindSyntaxTreePrePass cenv env (syntaxTree: OlySyntaxTree) =
    let syntaxRoot = syntaxTree.GetRoot(cenv.ct) :?> OlySyntaxCompilationUnit
    bindRootPrePass cenv env syntaxRoot

let bindSyntaxTreePass0 cenv nmsEnv env (syntaxTree: OlySyntaxTree) =
    let syntaxRoot = syntaxTree.GetRoot(cenv.ct) :?> OlySyntaxCompilationUnit
    bindRootPass0 cenv nmsEnv env syntaxRoot

let bindSyntaxTreePass1 cenv env (entBuilder: EntitySymbolBuilder) (syntaxTree: OlySyntaxTree) =
    let syntaxRoot = syntaxTree.GetRoot(cenv.ct) :?> OlySyntaxCompilationUnit
    bindRootPass1 cenv env entBuilder syntaxRoot

let bindSyntaxTreePass2 cenv env (entBuilder: EntitySymbolBuilder) (syntaxTree: OlySyntaxTree) =
    let syntaxRoot = syntaxTree.GetRoot(cenv.ct) :?> OlySyntaxCompilationUnit
    bindRootPass2 cenv env entBuilder syntaxRoot

let bindSyntaxTreePass3 cenv env (entBuilder: EntitySymbolBuilder) (syntaxTree: OlySyntaxTree) =
    let syntaxRoot = syntaxTree.GetRoot(cenv.ct) :?> OlySyntaxCompilationUnit
    bindRootPass3 cenv env entBuilder syntaxRoot

let bindSyntaxTreePass4 cenv env (entBuilder: EntitySymbolBuilder) (syntaxTree: OlySyntaxTree) =
    let syntaxRoot = syntaxTree.GetRoot(cenv.ct) :?> OlySyntaxCompilationUnit
    let _, boundRoot = bindRootPass4 cenv env entBuilder syntaxRoot
    BoundTree(cenv.asm, cenv.declTable.contents, syntaxTree, boundRoot, entBuilder.Entity, cenv.diagnostics.GetDiagnostics())

[<NoEquality;NoComparison>]
type PassState =
    {
        asm: AssemblySymbol
        declTable: BoundDeclarationTable
        diags: OlyDiagnostic imarray
        env: BinderEnvironment
        entBuilder: EntitySymbolBuilder
        syntaxTree: OlySyntaxTree
    }

[<Sealed>]
type BinderPass4(state: PassState) =

    let compute ct =
        let diagLogger = OlyDiagnosticLogger.Create()

        let asm = state.asm

        let cenv =
            {
                bindAnonymousShapeTypeHole = Pass2.bindAnonymousShapeType
                declTable = ref state.declTable
                asm = asm
                syntaxTree = state.syntaxTree
                diagnostics = diagLogger
                pass = Pass4
                ct = ct
                entryPoint = None
                entityDefIndex = 0
                memberDefIndex = 0
            }

        let env = state.env     

        let boundTree = bindSyntaxTreePass4 cenv env state.entBuilder state.syntaxTree

        if diagLogger.HasAnyErrors then
            // Suppress errors from post-inference analysis if we already have errors.
            PostInferenceAnalysis.analyzeBoundTree cenv env boundTree
            boundTree
        else
            PostInferenceAnalysis.analyzeBoundTree cenv env boundTree
            boundTree.AppendDiagnostics(diagLogger.GetDiagnostics())

    let cachedValue = CacheValue(compute)

    member _.PartialDeclarationTable = state.declTable

    member _.Entity = state.entBuilder.Entity
    
    member _.Bind(ct) =
        cachedValue.GetValue(ct)

    member _.SyntaxTree = state.syntaxTree

[<Sealed>]
type BinderPass3(state: PassState) =

    let compute ct =
        let declTable = state.declTable
        let diagLogger = OlyDiagnosticLogger.Create()

        let cenv =
            {
                bindAnonymousShapeTypeHole = Pass2.bindAnonymousShapeType
                declTable = ref declTable
                asm = state.asm
                syntaxTree = state.syntaxTree
                diagnostics = diagLogger
                pass = Pass3
                ct = ct
                entryPoint = None
                entityDefIndex = 0
                memberDefIndex = 0
            }

        // TODO: We need a test that covers this.
        let autoOpenedRoot =
            state.env.benv.partialAutoOpenedRootEnts
            |> ImArray.ofSeq

        let env =
            if autoOpenedRoot.IsEmpty then
                state.env
            else
                (state.env, autoOpenedRoot)
                ||> ImArray.fold (fun env ent ->
                    openContentsOfEntity state.declTable env OpenContent.Values ent
                )      

        let env1 = bindSyntaxTreePass3 cenv env state.entBuilder state.syntaxTree
        let diags = state.diags.AddRange(diagLogger.GetDiagnostics())
        BinderPass4(
            { state with
                env = env1
                declTable = cenv.declTable.contents
                diags = diags
            }
        ), diags
    
    member _.Bind(ct) = 
        compute ct

[<Sealed>]
type BinderPass2(state: PassState) =

    let compute ct =
        let diagLogger = OlyDiagnosticLogger.Create()

        let cenv =
            {
                bindAnonymousShapeTypeHole = Pass2.bindAnonymousShapeType
                declTable = ref state.declTable
                asm = state.asm
                syntaxTree = state.syntaxTree
                diagnostics = diagLogger
                pass = Pass2
                ct = ct
                entryPoint = None
                entityDefIndex = 0
                memberDefIndex = 0
            }

        bindSyntaxTreePass2 cenv state.env state.entBuilder state.syntaxTree
        BinderPass3(
            { state with
                env = unsetSkipCheckTypeConstructor state.env
                declTable = cenv.declTable.contents
                diags = state.diags.AddRange(diagLogger.GetDiagnostics())
            }
        )
    
    member _.Bind(ct) =
        compute ct

[<Sealed>]
type BinderPass1(state: PassState) =

    let compute imports ct =
        let diagLogger = OlyDiagnosticLogger.Create()

        let env2 = 
            computePrologEnvironment
                imports
                diagLogger
                state.env
                state.declTable
                OpenContent.Entities
                ct

        let cenv =
            {
                bindAnonymousShapeTypeHole = Pass2.bindAnonymousShapeType
                declTable = ref state.declTable
                asm = state.asm
                syntaxTree = state.syntaxTree
                diagnostics = diagLogger
                pass = Pass1
                ct = ct
                entryPoint = None
                entityDefIndex = 0
                memberDefIndex = 0
            }

        let env3 = bindSyntaxTreePass1 { cenv with ct = ct } env2 state.entBuilder state.syntaxTree
        BinderPass2(
            { state with 
                declTable = cenv.declTable.contents
                env = env3
                diags = state.diags.AddRange(diagLogger.GetDiagnostics())
            }
        )

    member _.Entity = state.entBuilder.Entity
    
    member _.Bind(imports, ct) = 
        compute imports ct

[<Sealed>]
type BinderPass0(asm: AssemblySymbol, syntaxTree: OlySyntaxTree, env: BinderEnvironment, declTable: BoundDeclarationTable, diags: OlyDiagnostic imarray) =

    let compute ct =
        let nmsEnv = NamespaceEnvironment.Create()
        let diagLogger = OlyDiagnosticLogger.Create()
        let cenv =
            {
                bindAnonymousShapeTypeHole = Pass2.bindAnonymousShapeType
                declTable = ref declTable
                asm = asm
                syntaxTree = syntaxTree
                diagnostics = diagLogger
                pass = Pass0
                ct = ct
                entryPoint = None
                entityDefIndex = 0
                memberDefIndex = 0
            }
        let env, entBuilder = bindSyntaxTreePass0 cenv nmsEnv env syntaxTree
        BinderPass1(
            {
                asm = asm
                env = env
                diags = diags.AddRange(diagLogger.GetDiagnostics())
                declTable = cenv.declTable.contents
                entBuilder = entBuilder
                syntaxTree = syntaxTree
            }
        )

    member this.Bind(ct) =
        compute ct

[<Sealed>]
type BinderPrePass(asm: AssemblySymbol, syntaxTree: OlySyntaxTree, prePassEnv: CacheValue<BinderEnvironment * BoundDeclarationTable * OlyDiagnostic imarray>) =

    let compute ct =
        let env, declTable, diags = prePassEnv.GetValue(ct)
        BinderPass0(asm, syntaxTree, env, declTable, diags)

    member _.PrePassEnvironment = prePassEnv

    member this.Bind(ct) =
        compute ct

//*************************************************************************************************

let createInitialBoundEnvironment asmIdent =
    let funcs =
        seq {
            WellKnownFunctions.addFunc
            WellKnownFunctions.subtractFunc
            WellKnownFunctions.multiplyFunc
            WellKnownFunctions.divideFunc
            WellKnownFunctions.remainderFunc
            WellKnownFunctions.andFunc
            WellKnownFunctions.orFunc
            WellKnownFunctions.notFunc
            WellKnownFunctions.negateFunc
            WellKnownFunctions.equalFunc
            WellKnownFunctions.notEqualFunc
            WellKnownFunctions.greaterThanFunc
            WellKnownFunctions.greaterThanOrEqualFunc
            WellKnownFunctions.lessThanFunc
            WellKnownFunctions.lessThanOrEqualFunc
            WellKnownFunctions.importAttrFunc
            WellKnownFunctions.LoadFunctionPtr
        }

    let mk (ty: TypeSymbol) =
        System.Collections.Generic.KeyValuePair(ty.Name, ImArray.createOne ty)

    let tys =
        let arityGroup0 =
            seq {
                mk TypeSymbol.Int8
                mk TypeSymbol.UInt8
                mk TypeSymbol.Int16
                mk TypeSymbol.UInt16
                mk TypeSymbol.Int32
                mk TypeSymbol.UInt32
                mk TypeSymbol.Int64
                mk TypeSymbol.UInt64
                mk TypeSymbol.Float64
                mk TypeSymbol.Float32
                mk TypeSymbol.Char16
                mk TypeSymbol.Bool
                mk TypeSymbol.Utf16
                mk TypeSymbol.BaseObject
            }
            |> NameMap.ofSeq

        let arityGroup1 =
            seq {
                mk Types.ByRef
                mk Types.InRef
                mk Types.OutRef
                mk Types.Tuple
            }
            |> NameMap.ofSeq

        IntMap.empty.SetItem(0, arityGroup0).SetItem(1, arityGroup1)

    let senv = 
        {
            entitiesByIntrinsicTypes = TypeSymbolGeneralizedMap.Create()
            intrinsicTypesByAliasTypes = TypeSymbolGeneralizedMap.Create()
            aliasTypesByIntrinsicTypes = TypeSymbolGeneralizedMap.Create()

            namespaces = MultiNameMap.emptyWithComparer

            unqualifiedTypes = tys

            unqualifiedSymbols =
                funcs
                |> Seq.map (fun func -> System.Collections.Generic.KeyValuePair(func.Name, UnqualifiedSymbol.Function(func)))
                |> NameMap.ofSeq

            unqualifiedPatterns = NameMap.Empty

            parameters = ImArray.empty

            typeParameters = ImArray.empty
            enclosing = EnclosingSymbol.Local
            enclosingTyInst = IdMap.Empty

            typeExtensionsWithImplements = TypeSymbolGeneralizedMap.Create()
            typeExtensionMembers = TypeSymbolGeneralizedMap.Create()
        }

    {
        senv = senv
        openedEnts = ImmutableHashSet.Empty
        partialAutoOpenedRootEnts = ImmutableHashSet.Empty
        openDecls = ImArray.empty
        ac = { Entity = None; AssemblyIdentity = asmIdent }
        implicitExtendsForStruct = None
        implicitExtendsForEnum = None
    }

let CreateDefaultBinderEnvironment asmIdent =
    {
        benv = createInitialBoundEnvironment asmIdent
        isIntrinsic = false
        isInInstanceConstructorType = None
        isInEntityDefinitionTypeParameters = false
        isInFunctionDefinitionTypeParameters = false
        isInConstraint = false
        isInOpenDeclaration = false
        isInLocalLambda = false
        isInExport = false
        resolutionMustSolveTypes = false
        skipCheckTypeConstructor = false
        skipTypeExtensionBinding = false
        contextTypeOrTypeConstructor = None
        implicitThisOpt = None
        isReturnable = false
        isExecutable = false
        isPassedAsArgument = false
    }

let bindSyntaxTree asm env (syntaxTree: OlySyntaxTree) =
    let env = setSkipCheckTypeConstructor env
    let prePassEnv =
        CacheValue(fun ct ->
            let declTable = BoundDeclarationTable()
            let diagLogger = OlyDiagnosticLogger.Create()
            let cenv =
                {
                    bindAnonymousShapeTypeHole = Pass2.bindAnonymousShapeType
                    declTable = ref declTable
                    asm = asm
                    syntaxTree = syntaxTree
                    diagnostics = diagLogger
                    pass = Pass0
                    ct = ct
                    entryPoint = None
                    entityDefIndex = 0
                    memberDefIndex = 0
                }
            let env = bindSyntaxTreePrePass cenv env syntaxTree
            env, cenv.declTable.contents, diagLogger.GetDiagnostics()
        )
    BinderPrePass(asm, syntaxTree, prePassEnv)

let bindSyntaxTreeFast asm (syntaxTree: OlySyntaxTree) prePassEnv =
    BinderPrePass(asm, syntaxTree, prePassEnv)