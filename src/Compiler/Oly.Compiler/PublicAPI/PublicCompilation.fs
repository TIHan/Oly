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

type private CompilationSignaturePass = (BinderPass4 * OlyDiagnostic imarray) imarray

type private ImplementationPass = BinderPass4 * OlyDiagnostic imarray

[<NoComparison;NoEquality>]
type private CompilationUnitImplementationState =
    {
        boundModel: OlyBoundModel
        lazyImplPass: CacheValue<ImplementationPass>
        syntaxDiagnostics: CacheValue<OlyDiagnostic imarray>
        semanticDiagnostics: CacheValue<OlyDiagnostic imarray>
    }

    static member Create(asm, tryGetLocation, lazyImplPass: CacheValue<ImplementationPass>, syntaxTree: OlySyntaxTree) =

        let getPartialDeclTable =
            fun ct ->
                let binder, _ = lazyImplPass.GetValue(ct)
                binder.PartialDeclarationTable

        let boundTree =
            CacheValue(fun ct -> 
                let binder, diags = lazyImplPass.GetValue(ct)
                let boundTree = binder.Bind(ct)
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
        initialPass: CacheValue<BinderPass0>
        implState: CompilationUnitImplementationState
        extraDiags: OlyDiagnostic imarray
    }

[<Sealed>]
type internal CompilationUnit private (unitState: CompilationUnitState) =

    member _.LazyInitialPass = unitState.initialPass

    member _.GetSyntaxDiagnostics(ct) =
        unitState.implState.syntaxDiagnostics.GetValue(ct)

    member _.GetSemanticDiagnostics(ct) =
        unitState.extraDiags.AddRange(unitState.implState.semanticDiagnostics.GetValue(ct))

    member _.BoundModel: OlyBoundModel = unitState.implState.boundModel

    member this.GetBoundTree(ct) =
        this.BoundModel.GetBoundTree(ct)

    member this.SetExtraDiagnostics(extraDiags) =
        let newUnitState = { unitState with extraDiags = extraDiags }
        CompilationUnit(newUnitState)

    member this.GetExtraDiagnostics() =
        unitState.extraDiags

    member this.Update(asm, compRef: OlyCompilation ref, tryGetLocation, syntaxTree: OlySyntaxTree) =
        let implPass =
            CacheValue(fun ct ->
                let compSigPass = compRef.contents.LazySignaturePhase.GetValue(ct)
                let sigPasses = compSigPass
                let pass4, diags = sigPasses |> ImArray.find (fun (x, _) -> OlyPath.Equals(x.SyntaxTree.Path, syntaxTree.Path))
                pass4, diags
            )

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
                        bindSyntaxTreeFast asm prePassEnv syntaxTree
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
                extraDiags = ImArray.empty
            }
        CompilationUnit(unitState)

[<NoComparison;NoEquality>]
type internal CompilationState =
    {
        options: OlyCompilationOptions
        lazyInitialState: CacheValue<InitialState>
        assembly: AssemblySymbol
        references: OlyCompilationReference imarray
        cunits: ImmutableDictionary<OlyPath, CompilationUnit>
        version: uint64
    }

[<NoEquality;NoComparison;RequireQualifiedAccess>] 
type OlyCompilationReference =
    private 
    | CompilationReference of refId: OlyPath * (unit -> OlyCompilation)
    | AssemblyReference of refId: OlyPath * version: uint64 * Lazy<Result<OlyILAssembly, OlyDiagnostic>>

    member this.IsCompilation =
        match this with
        | CompilationReference _ -> true
        | _ -> false    
        
    member this.TryGetCompilation() =
        match this with
        | CompilationReference(_, compf) -> compf() |> Some
        | _ -> None

    member this.GetILAssembly(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        match this with
        | CompilationReference(_, compilation) -> 
            compilation().GetILAssembly(ct)
        | AssemblyReference(_, _, ilAsmLazy) -> 
            match ilAsmLazy.Value with
            | Result.Ok(ilAsm) -> Result.Ok(ilAsm)
            | Result.Error(diag) -> Result.Error(ImArray.createOne diag)

    member this.Version =
        match this with
        | CompilationReference(_, compf) -> compf().Version
        | AssemblyReference(_, version, _) -> version

    member this.Path =
        match this with
        | CompilationReference(refId=refId)
        | AssemblyReference(refId=refId) -> refId

    static member Create(referenceId, version, ilAsm: OlyILAssembly) = AssemblyReference(referenceId, version, Lazy<_>.CreateFromValue(Result.Ok ilAsm))
    static member Create(referenceId, comp: OlyCompilation) = CompilationReference(referenceId, (fun () -> comp))
    static member Create(referenceId, version, ilAsmLazy: Lazy<_>) = AssemblyReference(referenceId, version, ilAsmLazy)
    static member Create(referenceId, compf: unit -> OlyCompilation) = CompilationReference(referenceId, compf)

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
        let refBinders = CompilationPhases.signature comp.State ct
        refBinders
        |> ImArray.iter (fun (x: BinderPass4, _) ->
            importer.ImportAndRetargetEntity(ilAsmIdent, x.Entity)
        )

let private createInitialState (options: OlyCompilationOptions) (ilAsmIdent: OlyILAssemblyIdentity) (compRefs: OlyCompilationReference imarray, ct) =
    let sharedImportCache = SharedImportCache.Create()
    let imports = CompilerImports(sharedImportCache)
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
            let comp = getComp()          
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

        let imports = CompilerImports(SharedImportCache.Create())
        let importer = imports.Importer

        let binders1 =
            let passes =
                seq {
                    for pair in state.cunits do
                        pair.Value.LazyInitialPass
                }
                |> ImArray.ofSeq

            passes
            |> map (fun pass ->
                pass.GetValue(ct).Bind(ct)
            )

        binders1
        |> ImArray.iter (fun x ->
            importer.ImportEntity(x.Entity)
        )

        let binders2 =
            binders1
            |> map (fun x ->
                x.Bind(imports, ct)
            )

        let binders3 =
            binders2
            |> map (fun x -> x.Bind(ct))

        let binders4 =
            binders3
            |> map (fun x -> x.Bind(ct))

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
                   // |> outputTree
                    |> LambdaLifting.Lower g
                    //|> outputTree
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

    static let checkHasErrors (diags: OlyDiagnostic imarray) =
        diags
        |> ImArray.exists (fun x -> x.IsError)

    let lazySigPhase =
        CacheValue(fun ct -> CompilationPhases.signature state ct)

    static member private tryGetLocation (compRef: OlyCompilation ref) (identity: OlyILAssemblyIdentity, s: ISymbol, ct) =
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

    member internal this.LazySignaturePhase: CacheValue<CompilationSignaturePass> = lazySigPhase

    member internal this.State: CompilationState = state

    member this.GetReferenceCompilations(ct: CancellationToken) : OlyCompilation imarray =
        ct.ThrowIfCancellationRequested()
        state.references
        |> ImArray.choose (function
            | OlyCompilationReference.CompilationReference(_, compf) -> 
                ct.ThrowIfCancellationRequested()
                compf() |> Some
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
        let cunits = state.cunits.Remove(path)
        this.InitialSetSyntaxTreeBatch(cunits.Values |> Seq.map (fun x -> x.BoundModel.SyntaxTree) |> ImArray.ofSeq)

    member this.SetExtraDiagnostics(path: OlyPath, extraDiags) =
        let cunit = state.cunits[path]
        let newCUnit = cunit.SetExtraDiagnostics(extraDiags)
        { state with
            cunits = state.cunits.SetItem(path, newCUnit)
        }
        |> OlyCompilation

    member this.GetExtraDiagnostics(path: OlyPath) =
        let cunit = state.cunits[path]
        cunit.GetExtraDiagnostics()

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

            let tryGetLocation = OlyCompilation.tryGetLocation compRef
                  
            let cunits =
                let asm = state.assembly
                    
                state.cunits
                |> Seq.map (fun pair ->
                    if OlyPath.Equals(pair.Key, syntaxTree.Path) then
                        KeyValuePair(pair.Key, pair.Value.Update(asm, compRef, tryGetLocation, syntaxTree))
                    else
                        KeyValuePair(pair.Key, pair.Value.Update(asm, compRef, tryGetLocation, pair.Value.BoundModel.SyntaxTree))
                )
                |> ImmutableDictionary.CreateRange

            let state =
                { state with
                    version = state.version + 1UL
                    cunits = cunits
                }

            compRef.contents <- OlyCompilation state
            compRef.contents
        else
            this.InitialSetSyntaxTreeBatch(syntaxTrees)

    /// Adds a syntax tree to the compilation.
    /// Will overwrite existing syntax trees.
    member internal this.InitialSetSyntaxTreeBatch(syntaxTrees: OlySyntaxTree imarray) =
        let compRef = ref Unchecked.defaultof<_>

        let prevCUnits = state.cunits

        let tryGetLocation = OlyCompilation.tryGetLocation compRef
              
        let cunits =
            let lazyInitialState = state.lazyInitialState
            let asm = state.assembly
                
            (ImmutableDictionary.Empty, syntaxTrees)
            ||> ImArray.fold (fun cunits syntaxTree ->
                let cunit = CompilationUnit.Create(asm, lazyInitialState, compRef, tryGetLocation, syntaxTree)
                let cunit =
                    match prevCUnits.TryGetValue(syntaxTree.Path) with
                    | true, prevCUnit ->
                        cunit.SetExtraDiagnostics(prevCUnit.GetExtraDiagnostics())
                    | _ ->
                        cunit
                cunits.Add(syntaxTree.Path, cunit)
            )

        let state =
            { state with
                version = state.version + 1UL
                cunits = cunits
            }

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
                
            let state =
                { state with
                    options = options
                    lazyInitialState = lazyInitialState
                    references = references
                }

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

    member private this.Bind(ct) =
        ct.ThrowIfCancellationRequested()
        let binders4 = lazySigPhase.GetValue(ct)
        CompilationPhases.implementation state binders4 ct
        
    member this.Version = state.version

    member this.GetILAssembly(ct) : Result<OlyILAssembly, OlyDiagnostic imarray> = 
        let diags = this.GetDiagnostics(ct)
        if checkHasErrors diags then
            Result.Error(diags)
        else
            let boundTrees = this.Bind(ct)
            let loweredBoundTrees = CompilationPhases.lowering state boundTrees ct
            CompilationPhases.generateAssembly state loweredBoundTrees ct
            |> Result.Ok      

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
                assembly = asm
                references = references
                cunits = ImmutableDictionary.Empty
                version = 0UL
            }

        let c = OlyCompilation state
        c.InitialSetSyntaxTreeBatch(syntaxTrees |> ImArray.ofSeq)

