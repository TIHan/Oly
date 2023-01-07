[<AutoOpen>]
module rec Oly.Compiler.Compilation

open System
open System.Collections.Immutable
open System.Collections.Generic
open System.Threading

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

type OlyCompilationOptions =
    {
        Debuggable: bool
        Executable: bool
        Parallel: bool
    }

    static member Default =
        {
            Debuggable = false
            Executable = false
            Parallel = true
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
                let boundTree, diags2 = binder.Bind(ct)

                let diags =
                    diags.AddRange(diags2)
                    |> ImArray.ofSeq

                boundTree, diags
            )

        let getResult =
            fun ct ->
                let boundTree, diags = boundTree.GetValue(ct)
                { BoundTree = boundTree; SemanticDiagnostics = diags }

        let boundModel =
            OlyBoundModel(asm, syntaxTree, tryGetLocation, getPartialDeclTable, getResult)

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
                let pass4, diags = sigPasses |> ImArray.find (fun (x, _) -> x.SyntaxTree.Path = syntaxTree.Path)
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
                let pass4, diags = compSigPass |> ImArray.find (fun (x, _) -> x.SyntaxTree.Path = syntaxTree.Path)
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
            match compilation().GetILAssembly(ct) with
            | Result.Ok(ilAsm) -> ilAsm
            | Result.Error(ex) -> raise ex
        | AssemblyReference(_, _, ilAsmLazy) -> 
            match ilAsmLazy.Value with
            | Result.Ok(ilAsm) -> ilAsm
            | Result.Error(diag) -> failwithf "%s" diag.Message

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
        autoOpens: IEntitySymbol imarray
        env: BoundEnv
    }

let private createInitialState isApp (ilAsmIdent: OlyILAssemblyIdentity) (compRefs: OlyCompilationReference imarray, ct) =
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
                importer.ImportAssembly(ilAsm)
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

    for comp in comps do
        let refBinders = CompilationPhases.signature comp.State ct
        refBinders
        |> ImArray.iter (fun (x: BinderPass4, _) ->
            importer.ImportAndRetargetEntity(ilAsmIdent, x.Entity)
        )

    let importDiagnostics = OlyDiagnosticLogger.Create()
    let env, autoOpens1 = 
        computePrologEnvironment
            imports
            importDiagnostics
            { DefaultBinderEnvironment with isApp = isApp }
            (BoundDeclarationTable())
            OpenContent.All
            ct

    let importDiags = importDiags.ToImmutable()

    {
        sharedImportCache = sharedImportCache
        importDiags = importDiags.AddRange(importDiagnostics.GetDiagnostics())
        autoOpens = autoOpens1
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

    let signature (state: CompilationState) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let map f = 
            if state.options.Parallel then
                ImArray.Parallel.map f
            else
                ImArray.map f

        let initialState = state.lazyInitialState.GetValue(ct)
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
                x.Bind(imports, initialState.autoOpens, ct)
            )

        let binders3 =
            binders2
            |> map (fun x -> x.Bind(ct))

        let binders4 =
            binders3
            |> map (fun x -> x.Bind(ct))

        binders4

    let implementation (state: CompilationState) (binders4: (BinderPass4 * OlyDiagnostic imarray) imarray) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let map f = 
            if state.options.Parallel then
                ImArray.Parallel.map f
            else
                ImArray.map f

        binders4
        |> map (fun (x, diags) ->
            let boundTree, diags2 = x.Bind(ct)
            boundTree, diags.AddRange(diags2)
        )

    let lowering (state: CompilationState) (boundTrees: (BoundTree * OlyDiagnostic imarray) imarray) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let map f = 
            if state.options.Parallel then
                ImArray.Parallel.map f
            else
                ImArray.map f

        let outputTree (tree: BoundTree) =
            System.IO.File.WriteAllText("output.txt", Oly.Compiler.Internal.Dump.dumpTree tree)
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
                 //   |> outputTree
                    |> CommonLowering.Lower ct
                    |> Optimizer.Lower ct { LocalValueElimination = not state.options.Debuggable; BranchElimination = true }
                  //  |> outputTree
                    |> RefCellLowering.Lower
                   // |> outputTree
                    |> LambdaLifting.Lower
                 //   |> outputTree
                loweredBoundTree, diags
            )

        loweredBoundTrees

    let generateAssembly (state: CompilationState) (loweredBoundTrees: (BoundTree * OlyDiagnostic imarray) imarray) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let ilGen = OlyILAssemblyGenerator(state.assembly, state.options.Debuggable)
        loweredBoundTrees
        |> ImArray.iter (fun (x, _) ->
            ilGen.Generate(x)
        )

        ilGen.ILAssembly

[<Sealed>] 
type OlyCompilation private (state: CompilationState) =

    static let checkDiags (diags: OlyDiagnostic imarray) =
        diags
        |> ImArray.iter (fun x -> if x.IsError then failwithf "%A" x)

    let lazySigPhase =
        CacheValue(fun ct -> CompilationPhases.signature state ct)

    static let tryGetLocation (compRef: OlyCompilation ref) (identity: OlyILAssemblyIdentity, s: ISymbol, ct) =
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
                if x.Path = syntaxTree.Path then
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

            let tryGetLocation = tryGetLocation compRef
                  
            let cunits =
                let asm = state.assembly
                    
                state.cunits
                |> Seq.map (fun pair ->
                    if pair.Key = syntaxTree.Path then
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

        let tryGetLocation = tryGetLocation compRef
              
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
        let references = defaultArg (references |> Option.map ImArray.ofSeq) state.references
        let options = defaultArg options state.options

        let lazyInitialState = setup state.options.Executable this.AssemblyIdentity references
                
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

    member _.GetSyntaxDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        state.cunits.Values
        |> Seq.map (fun x -> x.GetSyntaxDiagnostics(ct))
        |> Seq.concat
        |> ImArray.ofSeq

    member _.GetSemanticDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        state.cunits.Values
        |> Seq.map (fun x -> x.GetSemanticDiagnostics(ct))
        |> Seq.concat
        |> ImArray.ofSeq

    member _.GetImportDiagnostics(ct: CancellationToken) =
        state.lazyInitialState.GetValue(ct).importDiags

    member this.GetDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        this.GetImportDiagnostics(ct).AddRange(
            this.GetSyntaxDiagnostics(ct).AddRange(
                this.GetSemanticDiagnostics(ct)))
        
    member this.Version = state.version

    member this.GetILAssembly(ct) : Result<OlyILAssembly, Exception> = 
       // try
            checkDiags (this.GetImportDiagnostics(ct))
            let binders4 = lazySigPhase.GetValue(ct)
            let boundTrees = CompilationPhases.implementation state binders4 ct
            boundTrees
            |> ImArray.iter (fun (_, diags) -> checkDiags diags)
            let loweredBoundTrees = CompilationPhases.lowering state boundTrees ct
            CompilationPhases.generateAssembly state loweredBoundTrees ct
            |> Result.Ok
        //with
        //| ex ->
        //    Result.Error(ex)        

    member _.AssemblyName = state.assembly.Name
    member _.AssemblyIdentity = state.assembly.Identity

    member _.References = state.references
    member _.Options = state.options

    static member Create(assemblyName: string, syntaxTrees: OlySyntaxTree seq, ?references: OlyCompilationReference seq, ?options: OlyCompilationOptions) =
        let references = defaultArg (references |> Option.map ImArray.ofSeq) ImArray.empty
        let options = defaultArg options OlyCompilationOptions.Default

        let asm = AssemblySymbol.IL(OlyILAssemblyIdentity(assemblyName, Guid.NewGuid().ToString())) // TODO: Not deterministic, but we will fix it later.

        let lazyInitialState = setup options.Executable asm.Identity references
                
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

