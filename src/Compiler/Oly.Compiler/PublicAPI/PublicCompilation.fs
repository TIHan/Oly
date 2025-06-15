[<AutoOpen>]
module rec Oly.Compiler.Compilation

open System
open System.Collections.Immutable
open System.Collections.Generic
open System.Threading
open System.Runtime.CompilerServices

open Oly.Core
open Oly.Metadata
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax

open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.ILGen
open Oly.Compiler.Internal.Lowering
open Oly.Compiler.Internal.CompilerImports
open Oly.Compiler.Internal.SymbolEnvironments
open System.Diagnostics.Tracing

[<EventSource(Name = "OlyCompilation")>]
type private OlyCompilationEventSource() =
    inherit EventSource()

    static member val Log = new OlyCompilationEventSource()

    [<Event(1)>]
    member this.BeginPass3(message: string) =
        this.WriteEvent(1, message)

    [<Event(2)>]
    member this.EndPass3(message: string) =
        this.WriteEvent(2, message)

type OlyCompilationOptions =
    {
        Debuggable: bool
        Executable: bool
        Parallel: bool
        ImplicitExtendsForStruct: string option
        ImplicitExtendsForEnum: string option
    }

    static member Default =
        {
            Debuggable = false
            Executable = false
            Parallel = true
            ImplicitExtendsForStruct = None
            ImplicitExtendsForEnum = None
        }

type private CompilationSignature = (BinderPass4 * OlyDiagnostic imarray) imarray

type private CompilationUnitImplementation = BinderPass4 * OlyDiagnostic imarray

[<NoComparison;NoEquality>]
type private CompilationUnitImplementationState =
    {
        boundModel: OlyBoundModel
        lazyImplPass: CacheValue<CompilationUnitImplementation>
        syntaxDiagnostics: CacheValue<OlyDiagnostic imarray>
        semanticDiagnostics: CacheValue<OlyDiagnostic imarray>
    }

    static member Create(asm, tryGetLocation, lazyImplPass: CacheValue<CompilationUnitImplementation>, syntaxTree: OlySyntaxTree) =

        let getPartialDeclTable =
            fun ct ->
                let binder, _ = lazyImplPass.GetValue(ct)
                binder.PartialDeclarationTable

        let boundTree =
            CacheValue(fun ct -> 
                let binder, diags = lazyImplPass.GetValue(ct)
                let s = System.Diagnostics.Stopwatch.StartNew()
                let boundTree = binder.Bind(ct)
                s.Stop()
                OlyTrace.Log("[Compilation] Implementation Pass: " + syntaxTree.Path.ToString() + $" - {s.Elapsed.TotalMilliseconds} ms")
                boundTree.PrependDiagnostics(diags)
            )

        let getBoundTree =
            fun ct -> boundTree.GetValue(ct)

        let boundModel =
            OlyBoundModel(asm, syntaxTree, tryGetLocation, getPartialDeclTable, getBoundTree)

        let lazySyntaxDiagnostics =
            CacheValue(fun ct -> syntaxTree.GetDiagnostics(ct))

        let lazySemanticDiagnostics =
            CacheValue(fun ct -> boundModel.GetDiagnostics(ct))

        {
            boundModel = boundModel
            lazyImplPass = lazyImplPass
            syntaxDiagnostics = lazySyntaxDiagnostics
            semanticDiagnostics = lazySemanticDiagnostics
        }

[<NoComparison;NoEquality>]
type private CompilationUnitState =
    {
        syntaxTree: OlySyntaxTree
        lazyInitialState: CacheValue<InitialState>
        initialPass: CacheValue<BinderPrePass>
        implState: CompilationUnitImplementationState
    }

[<Sealed>]
type internal CompilationUnit private (unitState: CompilationUnitState) =

    member _.LazyInitialPass = unitState.initialPass

    member _.GetSyntaxDiagnostics(ct) =
        unitState.implState.syntaxDiagnostics.GetValue(ct)

    member _.GetSemanticDiagnostics(ct) =
        unitState.implState.semanticDiagnostics.GetValue(ct)

    member _.BoundModel: OlyBoundModel = unitState.implState.boundModel

    member this.GetBoundTree(ct) =
        this.BoundModel.GetBoundTree(ct)

    member this.Update(asm, compRef: OlyCompilation ref, syntaxTree: OlySyntaxTree) =
        let implPass =
            CacheValue(fun ct ->
                let compSigPass = compRef.contents.LazySignaturePhase.GetValue(ct)
                let sigPasses = compSigPass
                // TODO: Instead of doing 'ImArray.find', this should have simply been a dictionary lookup.
                let pass4, diags = sigPasses |> ImArray.find (fun (x, _) -> OlyPath.Equals(x.SyntaxTree.Path, syntaxTree.Path))
                pass4, diags
            )

        let tryGetLocation = OlyCompilation.tryGetLocation compRef

        let implState = CompilationUnitImplementationState.Create(asm, tryGetLocation, implPass, syntaxTree)

        let unitState =
            let areOpenDeclsSame =
                if unitState.initialPass.HasValue then
                    OlySyntaxTree.AreOpenDeclarationsEqual(unitState.syntaxTree, syntaxTree, CancellationToken.None)
                else
                    false

            let initial = unitState.lazyInitialState

            // This effectively will cause the compilation unit to be re-evaluated entirely,
            // except for opened entities who are external dependencies.
            // It's important that this is correct in order to maintain an immutable
            // compilation model. Sharing entities between different versions of the same
            // compilation model need to be handled with care.
            let initialPass = 
                if areOpenDeclsSame then
                    let oldInitialPass = unitState.initialPass.GetValue(CancellationToken.None)
                    let prePassEnv = oldInitialPass.PrePassEnvironment
                    CacheValue(fun ct ->
                        ct.ThrowIfCancellationRequested()
                        bindSyntaxTreeFast asm syntaxTree prePassEnv
                    )
                else
                    CacheValue(fun ct ->
                        let initial = initial.GetValue(ct)
                        bindSyntaxTree asm initial.env syntaxTree
                    )
            { unitState with
                initialPass = initialPass
                syntaxTree = syntaxTree
                implState = implState
            }
        CompilationUnit(unitState)

    static member internal Create(asm, initial: CacheValue<InitialState>, compRef: OlyCompilation ref, tryGetLocation, syntaxTree: OlySyntaxTree) =
        let initialPass = 
            CacheValue(fun ct ->
                let initial = initial.GetValue(ct)
                bindSyntaxTree asm initial.env syntaxTree
            )

        let implPass =
            CacheValue(fun ct ->
                let compSigPass = compRef.contents.LazySignaturePhase.GetValue(ct)
                let pass4, diags = compSigPass |> ImArray.find (fun (x, _) -> OlyPath.Equals(x.SyntaxTree.Path, syntaxTree.Path))
                pass4, diags
            )

        let implState = CompilationUnitImplementationState.Create(asm, tryGetLocation, implPass, syntaxTree)

        let unitState =
            {
                syntaxTree = syntaxTree
                lazyInitialState = initial
                initialPass = initialPass
                implState = implState
            }
        CompilationUnit(unitState)

[<NoComparison;NoEquality>]
type internal CompilationState =
    {
        options: OlyCompilationOptions
        lazyInitialState: CacheValue<InitialState>
        lazySig: CacheValue<CompilationSignature>
        assembly: AssemblySymbol
        references: OlyCompilationReference imarray
        cunits: ImmutableDictionary<OlyPath, CompilationUnit>
        version: uint64
    }

    member this.RefreshSignature() =
        let mutable newState = this
        newState <-
            { this with
                lazySig = 
                    CacheValue(fun ct -> 
                        let s = System.Diagnostics.Stopwatch.StartNew()
                        let result = CompilationPhases.signature newState ct
                        s.Stop()
                        OlyTrace.Log($"[Compilation] Signature Pass: {newState.assembly.Name} {newState.version} - {s.Elapsed.TotalMilliseconds}ms")
                        result
                    )
            }
        newState

[<NoEquality;NoComparison;RequireQualifiedAccess>] 
type OlyCompilationReference =
    private 
    | CompilationReference of refId: OlyPath * CacheValue<OlyCompilation>
    | AssemblyReference of refId: OlyPath * version: uint64 * Lazy<Result<OlyILAssembly, OlyDiagnostic>>

    member this.IsCompilation =
        match this with
        | CompilationReference _ -> true
        | _ -> false
        
    member this.TryGetCompilation(ct) =
        match this with
        | CompilationReference(_, compf) -> compf.GetValue(ct) |> Some
        | _ -> None

    member this.GetILAssembly(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        match this with
        | CompilationReference(_, compilation) -> 
            compilation.GetValue(ct).GetILAssembly(ct)
        | AssemblyReference(_, _, ilAsmLazy) -> 
            match ilAsmLazy.Value with
            | Result.Ok(ilAsm) -> Result.Ok(ilAsm)
            | Result.Error(diag) -> Result.Error(ImArray.createOne diag)

    member this.GetVersion(ct) =
        match this with
        | CompilationReference(_, compf) -> compf.GetValue(ct).Version
        | AssemblyReference(_, version, _) -> version

    member this.Path =
        match this with
        | CompilationReference(refId=refId)
        | AssemblyReference(refId=refId) -> refId

    static member Create(referenceId, version, ilAsm: OlyILAssembly) = AssemblyReference(referenceId, version, Lazy<_>.CreateFromValue(Result.Ok ilAsm))
    static member Create(referenceId, comp: OlyCompilation) = CompilationReference(referenceId, CacheValue.FromValue(comp))
    static member Create(referenceId, version, ilAsmLazy: Lazy<_>) = AssemblyReference(referenceId, version, ilAsmLazy)
    static member Create(referenceId, compf: CacheValue<OlyCompilation>) = CompilationReference(referenceId, compf)

type private BoundEnv = Oly.Compiler.Internal.Binder.Environment.BinderEnvironment

[<NoEquality;NoComparison>]
type internal InitialState =
    {
        sharedImportCache: SharedImportCache
        importDiags: OlyDiagnostic imarray
        env: BoundEnv
    }

let private importCompilations (ilAsmIdent: OlyILAssemblyIdentity) (importer: Importer) (comps: OlyCompilation seq) ct =
    for comp in comps do
        let refBinders = comp.LazySignaturePhase.GetValue(ct)
        refBinders
        |> ImArray.iter (fun (x: BinderPass4, _) ->
            importer.ImportAndRetargetEntity(ilAsmIdent, x.Entity)
        )

let private createInitialState (options: OlyCompilationOptions) (ilAsmIdent: OlyILAssemblyIdentity) (compRefs: OlyCompilationReference imarray, ct) =
    let sharedImportCache = SharedImportCache.Create()
    let imports = CompilerImports(ilAsmIdent, sharedImportCache)
    let importer = imports.Importer
    let importDiags = ImArray.builder()

    let comps = ResizeArray()
    for reference in compRefs do
        match reference with
        | OlyCompilationReference.AssemblyReference(_, _, ilAsmLazy) ->
            match ilAsmLazy.Value with
            | Result.Ok ilAsm ->
                importer.ImportAssembly(ilAsm.ToReadOnly())
            | Result.Error diag ->
                importDiags.Add(diag)
        | OlyCompilationReference.CompilationReference(_, getComp) ->
            let comp = getComp.GetValue(ct)          
            let isCyclic =
                comp.GetTransitiveReferenceCompilations(ct)
                |> ImArray.exists (fun x -> x.AssemblyIdentity = ilAsmIdent)

            if isCyclic then
                // TODO: Report error if cyclic...
                ()
            else
                comps.Add(comp)

    importCompilations ilAsmIdent importer comps ct

    let importDiagnostics = OlyDiagnosticLogger.Create()
    let env = 
        computePrologEnvironment
            imports
            importDiagnostics
            { CreateDefaultBinderEnvironment ilAsmIdent with isExecutable = options.Executable }
            (BoundDeclarationTable())
            OpenContent.All
            ct

    let importDiags = importDiags.ToImmutable()

    let implicitExtendsForStructOpt =
        options.ImplicitExtendsForStruct
        |> Option.bind (fun x ->
            match imports.Importer.TryGetEntity(x) with
            | true, ent -> Some ent.AsType
            | _ -> None
        )

    let implicitExtendsForEnumOpt =
        options.ImplicitExtendsForEnum
        |> Option.bind (fun x ->
            match imports.Importer.TryGetEntity(x) with
            | true, ent -> Some ent.AsType
            | _ -> None
        )

    let env =
        { env with 
            benv = 
                { env.benv with 
                    implicitExtendsForStruct = implicitExtendsForStructOpt
                    implicitExtendsForEnum = implicitExtendsForEnumOpt 
                }
        }

    {
        sharedImportCache = sharedImportCache
        importDiags = importDiags.AddRange(importDiagnostics.GetDiagnostics())
        env = env
    }

let private setup isApp ilAsmIdent references =
    let initial = 
        CacheValue(fun ct ->
            createInitialState isApp ilAsmIdent (references, ct)
        )
    initial

[<RequireQualifiedAccess>]
module private CompilationPhases =

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let checkDuplications (state: CompilationState) (binders4: (BinderPass4 * OlyDiagnostic imarray) imarray) =
        let map f = 
            if state.options.Parallel then
                ImArray.Parallel.map f
            else
                ImArray.map f

        let checkDuplicate (b: BinderPass4) (ent: EntitySymbol) =
            match b.PartialDeclarationTable.EntityDeclarations.TryGetValue ent with
            | true, srcLoc ->
                OlyDiagnostic.CreateSyntacticError($"'{ent.Name}' already exists across compilation units.", 10, srcLoc)
                |> Some
            | _ ->
                None

        // This checks for ambiguity of types with the same signature declared across multiple compilation units.
        // TODO: This is quadratic, but not super bad since we are able to look at a dictionary to determine if a similar entity exists in
        //       each compilation unit. The quadratic'ness is caused by iterating over each compilation unit for every compilation unit.
        //       This is mitigated by doing each check potentially in parallel.
        //       We should find another way to do this without being quadratic.
        binders4
        |> map (fun (b1, diags) ->
            let newDiags = ImArray.builder()
            binders4
            |> ImArray.iter (fun (b2, _) ->
                if obj.ReferenceEquals(b1, b2) |> not then
                    if b1.Entity.IsNamespace then
                        b1.Entity.Entities
                        |> ImArray.iter (fun ent ->
                            match checkDuplicate b2 ent with
                            | Some(diag) -> newDiags.Add(diag)
                            | _ -> ()
                        )
                    else
                        match checkDuplicate b2 b1.Entity with
                        | Some(diag) -> newDiags.Add(diag)
                        | _ -> ()
            )
            (b1, diags.AddRange(newDiags))
        )

    let signature (state: CompilationState) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let map f = 
            if state.options.Parallel then
                ImArray.Parallel.map f
            else
                ImArray.map f

        let imports = CompilerImports(state.assembly.Identity, SharedImportCache.Create())
        let importer = imports.Importer

        let s = System.Diagnostics.Stopwatch.StartNew()

        let binders1 =
            let passes =
                seq {
                    for pair in state.cunits do
                        pair.Value.LazyInitialPass.GetValue(ct).Bind(ct)
                }
                |> ImArray.ofSeq
            OlyTrace.Log($"[Compilation] PrePass: {state.assembly.Name} - {s.Elapsed.TotalMilliseconds}ms")
            s.Restart()

            let result =
                passes
                |> map (fun pass ->
                    pass.Bind(ct)
                )
            OlyTrace.Log($"[Compilation] Pass0: {state.assembly.Name} - {s.Elapsed.TotalMilliseconds}ms")
            s.Restart()
            result

        binders1
        |> ImArray.iter (fun x ->
            importer.ImportEntity(x.Entity)
        )

        let binders2 =
            binders1
            |> map (fun x ->
                x.Bind(imports, ct)
            )

        OlyTrace.Log($"[Compilation] Pass1: {state.assembly.Name} - {s.Elapsed.TotalMilliseconds}ms")
        s.Restart()

        let binders3 =
            binders2
            |> map (fun x -> x.Bind(ct))

        OlyTrace.Log($"[Compilation] Pass2: {state.assembly.Name} - {s.Elapsed.TotalMilliseconds}ms")
        s.Restart()

        OlyCompilationEventSource.Log.BeginPass3(state.assembly.Name)
        let binders4 =
            binders3
            |> map (fun x -> x.Bind(ct))
        OlyCompilationEventSource.Log.EndPass3(state.assembly.Name)

        OlyTrace.Log($"[Compilation] Pass3: {state.assembly.Name} - {s.Elapsed.TotalMilliseconds}ms")
        s.Stop()

        checkDuplications state binders4

    let implementation (state: CompilationState) (binders4: (BinderPass4 * OlyDiagnostic imarray) imarray) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let map f = 
            if state.options.Parallel then
                ImArray.Parallel.map f
            else
                ImArray.map f

        binders4
        |> map (fun (x, diags) ->
            let boundTree = x.Bind(ct)
            boundTree, diags.AddRange(boundTree.Diagnostics)
        )

    let lowering (state: CompilationState) (boundTrees: (BoundTree * OlyDiagnostic imarray) imarray) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        // TODO: We should create this earlier, such as in the initial state.
        let g = 
            {
                ImplicitExtendsForStruct = state.lazyInitialState.GetValue(ct).env.benv.implicitExtendsForStruct
                BaseObjectConstructor =
                    // REVIEW: This is very fast considering there will not be a lot of functions on the object type.
                    //         But, there could be in the future.
                    match state.lazyInitialState.GetValue(ct).env.TryFindConcreteEntityByType(TypeSymbol.BaseObject) with
                    | ValueSome x ->
                        x.Functions
                        |> ImArray.filter (fun x -> x.IsInstanceConstructor && x.LogicalParameterCount = 0)
                        |> ImArray.tryExactlyOne
                    | _ -> 
                        None
            }

        let map f = 
            if state.options.Parallel then
                ImArray.Parallel.map f
            else
                ImArray.map f

        let outputTree (tree: BoundTree) =
            System.IO.File.WriteAllText(OlyPath.ChangeExtension(tree.SyntaxTree.Path, ".txt").ToString(), Oly.Compiler.Internal.Dump.dumpTree tree)
            tree

        // Lowering is REQUIRED before codegen, 
        //     otherwise, there will be expressions that the codegen will not understand.
        // The order of lowering matters.
        let loweredBoundTrees =               
            boundTrees
            |> map (fun (boundTree, diags) -> 
                ct.ThrowIfCancellationRequested()
                let loweredBoundTree =
                    boundTree
                    |> PatternMatchCompilation.Lower ct
                   // |> outputTree
                    |> CommonLowering.Lower ct
                    //|> outputTree
                    |> Optimizer.Lower ct { LocalValueElimination = not state.options.Debuggable; BranchElimination = true }
                  //  |> outputTree
                    |> RefCellLowering.Lower
                   //|> outputTree
                    |> LambdaLifting.Lower g
                   // |> outputTree
                loweredBoundTree, diags
            )

        loweredBoundTrees

    let generateAssembly (state: CompilationState) (loweredBoundTrees: (BoundTree * OlyDiagnostic imarray) imarray) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let ilGen = OlyILAssemblyGenerator(state.assembly, state.options.Debuggable)
        loweredBoundTrees
        |> ImArray.iter (fun (x, _) ->
            try
                ilGen.Generate(x)
            with
            | ex ->
                let msg = $"Internal Error (ILGen) in '{x.SyntaxTree.Path}':\n{ex.Message}"
                raise(Exception(msg, ex))
        )

        ilGen.ILAssembly

[<Sealed>] 
type OlyCompilation private (state: CompilationState) =

    let state = { state with version = state.version + 1UL }

    static let checkHasErrors (diags: OlyDiagnostic imarray) =
        diags
        |> ImArray.exists (fun x -> x.IsError)

    static member internal tryGetLocation (compRef: OlyCompilation ref) (identity: OlyILAssemblyIdentity, s: ISymbol, ct) =
        if compRef.contents.AssemblyIdentity = identity then
            compRef.contents.State.cunits.Values
            |> Seq.tryPick (fun cunit ->
                cunit.BoundModel.TryFindDefinition(s, ct)
            )
        else
            compRef.contents.GetReferenceCompilations(ct)
            |> ImArray.tryFind (fun (c: OlyCompilation) -> c.AssemblyIdentity = identity)
            |> Option.bind (fun c -> 
                c.State.cunits.Values
                |> Seq.tryPick (fun cunit ->
                    cunit.BoundModel.TryFindDefinition(s, ct)
                )
            )

    member internal this.LazySignaturePhase: CacheValue<CompilationSignature> = state.lazySig

    member internal this.State: CompilationState = state

    member this.GetReferenceCompilations(ct: CancellationToken) : OlyCompilation imarray =
        ct.ThrowIfCancellationRequested()
        state.references
        |> ImArray.choose (function
            | OlyCompilationReference.CompilationReference(_, compf) -> 
                ct.ThrowIfCancellationRequested()
                compf.GetValue(ct) |> Some
            | _ -> 
                ct.ThrowIfCancellationRequested()
                None
        )

    member this.GetTransitiveReferenceCompilations(ct: CancellationToken): OlyCompilation imarray =
        ct.ThrowIfCancellationRequested()
        let comps = ImArray.builder()
        let set = HashSet<OlyILAssemblyIdentity>()

        let rec processTransitive (comp: OlyCompilation) =           
            comp.GetReferenceCompilations(ct)
            |> ImArray.iter (fun (comp: OlyCompilation) ->
                ct.ThrowIfCancellationRequested()
                if set.Add(comp.AssemblyIdentity) then
                    comps.Add(comp)
                    processTransitive comp
            )

        processTransitive this
        comps.ToImmutable()

    member this.RemoveSyntaxTree(path: OlyPath) =
#if DEBUG || CHECKED
        OlyTrace.Log($"Refresh - Removing Syntax Tree - {this.AssemblyName} {this.Version} - {path.ToString()}")
#endif
        let cunits = state.cunits.Remove(path)
        this.InitialSetSyntaxTreeBatch(cunits.Values |> Seq.map (fun x -> x.BoundModel.SyntaxTree) |> ImArray.ofSeq)

    /// Adds a syntax tree to the compilation.
    /// Will overwrite existing syntax trees.
    member this.SetSyntaxTree(syntaxTree: OlySyntaxTree) =
        let mutable alreadyHasSyntaxTree = false

        let syntaxTrees =
            this.SyntaxTrees
            |> ImArray.map (fun (x: OlySyntaxTree) ->
                if OlyPath.Equals(x.Path, syntaxTree.Path) then
                    alreadyHasSyntaxTree <- true
                    syntaxTree
                else
                    x
            )

        let syntaxTrees =
            if alreadyHasSyntaxTree then
                syntaxTrees
            else
                syntaxTrees.Add(syntaxTree)

        if alreadyHasSyntaxTree then
            let compRef = ref Unchecked.defaultof<_>
                  
            let cunits =
                let asm = state.assembly
                    
                // REVIEW: Would it be worth doing this in parallel?
                state.cunits
                |> Seq.map (fun pair ->
                    if OlyPath.Equals(pair.Key, syntaxTree.Path) then
                        KeyValuePair(pair.Key, pair.Value.Update(asm, compRef, syntaxTree))
                    else
                        KeyValuePair(pair.Key, pair.Value.Update(asm, compRef, pair.Value.BoundModel.SyntaxTree))
                )
                |> ImmutableDictionary.CreateRange

#if DEBUG || CHECKED
            OlyTrace.Log($"Refresh - Setting Same Syntax Tree - {this.AssemblyName} {this.Version} - {syntaxTree.Path.ToString()}")
#endif
            let state =
                { state with
                    cunits = cunits
                }.RefreshSignature()

            compRef.contents <- OlyCompilation state
            compRef.contents
        else
#if DEBUG || CHECKED
            OlyTrace.Log($"Refresh - Setting Syntax Tree - {this.AssemblyName} {this.Version} - {syntaxTree.Path.ToString()}")
#endif
            this.InitialSetSyntaxTreeBatch(syntaxTrees)

    /// Adds a syntax tree to the compilation.
    /// Will overwrite existing syntax trees.
    member internal this.InitialSetSyntaxTreeBatch(syntaxTrees: OlySyntaxTree imarray) =
        let compRef = ref Unchecked.defaultof<_>

        let tryGetLocation = OlyCompilation.tryGetLocation compRef
              
        let cunits =
            let lazyInitialState = state.lazyInitialState
            let asm = state.assembly
                
            (ImmutableDictionary.Empty, syntaxTrees)
            ||> ImArray.fold (fun cunits syntaxTree ->
                let cunit = CompilationUnit.Create(asm, lazyInitialState, compRef, tryGetLocation, syntaxTree)
                cunits.Add(syntaxTree.Path, cunit)
            )

        let state =
            { state with
                cunits = cunits
            }.RefreshSignature()

        compRef.contents <- OlyCompilation state
        compRef.contents

    member this.Update(?references: OlyCompilationReference seq, ?options: OlyCompilationOptions) =
        if references.IsNone && options.IsNone then
            this
        else
            let updatingReferences = references.IsSome
            let references = defaultArg (references |> Option.map ImArray.ofSeq) state.references
            let options = defaultArg options state.options

            let lazyInitialState = 
                // If we are not updating references and the Executable option has not changed,
                // then we do not need to re-compute the initial state.
                if not updatingReferences && (options.Executable = state.options.Executable) then
                    state.lazyInitialState
                else
                    // This will effectively rebuild everything. 
                    // Initial state must be re-computed which is expensive, but it is lazy (will not happen immediately here).
                    setup state.options this.AssemblyIdentity references
                
#if DEBUG || CHECKED
            OlyTrace.Log($"Refresh - Updating - {this.AssemblyName} {this.Version}")
#endif
            let state =
                { state with
                    options = options
                    lazyInitialState = lazyInitialState
                    references = references
                }.RefreshSignature()

            let c = OlyCompilation state
            c.InitialSetSyntaxTreeBatch(this.SyntaxTrees)

    member this.SyntaxTrees =
        state.cunits.Values
        |> Seq.map (fun x -> x.BoundModel.SyntaxTree)
        |> ImArray.ofSeq

    member _.GetSyntaxTree(path): OlySyntaxTree = state.cunits.[path].BoundModel.SyntaxTree

    member _.GetBoundModel(path): OlyBoundModel = state.cunits.[path].BoundModel

    member private _.GetSyntaxDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        state.cunits.Values
        |> Seq.map (fun x -> x.GetSyntaxDiagnostics(ct))
        |> Seq.concat
        |> ImArray.ofSeq

    member private _.GetSemanticDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        state.cunits.Values
        |> Seq.map (fun x -> x.GetSemanticDiagnostics(ct))
        |> Seq.concat
        |> ImArray.ofSeq

    member private _.GetImportDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        state.lazyInitialState.GetValue(ct).importDiags

    member this.GetDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        this.Bind(ct) |> ignore

        let diags =
            this.GetImportDiagnostics(ct).AddRange(
                this.GetSyntaxDiagnostics(ct).AddRange(
                    this.GetSemanticDiagnostics(ct)))

        let refDiags =
            this.GetTransitiveReferenceCompilations(ct)
            |> ImArray.collect (fun x -> 
                x.GetImportDiagnostics(ct).AddRange(
                    x.GetSyntaxDiagnostics(ct).AddRange(
                        x.GetSemanticDiagnostics(ct)))
            )

        ImArray.append diags refDiags
        |> ImArray.sortBy (fun x ->
            match x.SyntaxTree with
            | Some syntaxTree -> syntaxTree.Path.ToString()
            | _ -> String.Empty
        )

    member private this.Bind(ct) =
        ct.ThrowIfCancellationRequested()
        let binders4 = state.lazySig.GetValue(ct)
        CompilationPhases.implementation state binders4 ct
        
    member this.Version = state.version

    member this.GetILAssembly(ct) : Result<OlyILAssembly, OlyDiagnostic imarray> = 
        let diags = this.GetDiagnostics(ct)
        if checkHasErrors diags then
            Result.Error(diags)
        else
            let boundTrees = this.Bind(ct)
            let s = System.Diagnostics.Stopwatch.StartNew()

            let loweredBoundTrees = CompilationPhases.lowering state boundTrees ct
            OlyTrace.Log($"[Compilation] Lowering Pass - {state.assembly.Name} {state.version} - {s.Elapsed.TotalMilliseconds}ms")
            s.Restart()

            let ilAsm = CompilationPhases.generateAssembly state loweredBoundTrees ct
            OlyTrace.Log($"[Compilation] Assembly Generation Pass - {state.assembly.Name} {state.version} - {s.Elapsed.TotalMilliseconds}ms")

            Result.Ok(ilAsm)  

    member _.AssemblyName = state.assembly.Name
    member _.AssemblyIdentity = state.assembly.Identity

    member _.References = state.references
    member _.Options = state.options

    static member Create(assemblyName: string, syntaxTrees: OlySyntaxTree seq, ?references: OlyCompilationReference seq, ?options: OlyCompilationOptions) =
        let references = defaultArg (references |> Option.map ImArray.ofSeq) ImArray.empty
        let options = defaultArg options OlyCompilationOptions.Default

        let asm = AssemblySymbol.IL(OlyILAssemblyIdentity(assemblyName, "")) // TODO: Not deterministic, but we will fix it later.

        let lazyInitialState = setup options asm.Identity references
                
        let state =
            {
                options = options
                lazyInitialState = lazyInitialState
                lazySig = CacheValue.FromValue(ImArray.empty)
                assembly = asm
                references = references
                cunits = ImmutableDictionary.Empty
                version = 0UL
            }

        let syntaxTrees = syntaxTrees |> ImArray.ofSeq
#if DEBUG || CHECKED
        OlyTrace.Log($"Creating - {assemblyName} - Syntax Tree Count: {syntaxTrees.Length}")
#endif
        let c = OlyCompilation state
        c.InitialSetSyntaxTreeBatch(syntaxTrees)

