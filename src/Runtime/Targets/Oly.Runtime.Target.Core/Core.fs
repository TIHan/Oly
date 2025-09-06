namespace Oly.Targets.Core

open System

open Oly.Core
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace

module OlyTarget =

    let private handleException (project: OlyProject) (ex: Exception) ct =
        match ex with
        | :? Oly.Runtime.CodeGen.OlyGenericRecursionLimitReached as ex ->
            let solution = project.Solution
            let docs = solution.GetDocuments(ex.textRange.Path)
            if docs.IsEmpty then
                Some(OlyDiagnostic.CreateError(ex.message))
            else
                let doc = docs[0]
                let syntaxNode =
                    doc.SyntaxTree.TryFindNode(
                        OlyTextRange(
                            OlyTextPosition(
                                ex.textRange.StartLine, 
                                ex.textRange.StartColumn
                            ), 
                            OlyTextPosition(
                                ex.textRange.EndLine, 
                                ex.textRange.EndColumn
                            )
                        ),
                        ct
                    )
                let syntaxNode = syntaxNode |> Option.defaultValue doc.SyntaxTree.DummyNode
                Some(OlyDiagnostic.CreateError(ex.message, OlyDiagnostic.CodePrefixOLY, 9999, syntaxNode))
        | _ ->
            None

    let CheckedEmit (hasEntryPoint: bool, project: OlyProject, runtime: OlyRuntime<_, _, _>, ct) =
        try
            if hasEntryPoint then
                runtime.EmitEntryPoint()
                None
            else
                runtime.EmitAheadOfTime()
                None
        with
        | :? AggregateException as ex when ex.InnerExceptions.Count >= 1 ->
            let rec innerMost (ex: Exception) =
                match ex with
                | :? AggregateException as ex when ex.InnerExceptions.Count >= 1 ->
                    innerMost ex.InnerExceptions[0]
                | _ ->
                    ex
            let innerEx =  innerMost ex
            let res = handleException project innerEx ct
            if res.IsSome then
                res
            else
                reraise()
        | ex ->
            let res = handleException project ex ct
            if res.IsSome then
                res
            else
                reraise()


[<AbstractClass>]
type OlyTargetOutputOnly<'Emitter, 'Type, 'Function, 'Field when 'Emitter :> IOlyRuntimeEmitter<'Type, 'Function, 'Field>>(platformName) =
    inherit OlyBuild(platformName)

    abstract CreateEmitter : OlyTargetInfo -> 'Emitter

    abstract EmitOutput : OlyProject * binDirectory: OlyPath * 'Emitter * isDebuggable: bool -> unit

    override this.BuildProjectAsync(proj, ct: System.Threading.CancellationToken) = backgroundTask { 
        let diags = proj.GetDiagnostics(ct)
        if diags |> ImArray.exists (fun x -> x.IsError) then
            return Error(diags)
        else

        let analyzerTask =
            // This is to make the happy path of successful compilations
            // faster as we can run the analyzers in the background
            // while we are compiling to IL.
            // TODO: Add a way to cancel this sooner.
            System.Threading.Tasks.Task.Factory.StartNew(fun () ->
                let diags = proj.GetAnalyzerDiagnostics(ct)
                if diags |> ImArray.exists (fun x -> x.IsError) then
                    Error(diags)
                else
                    Ok()
            )

        let comp = proj.Compilation
        let asm = comp.GetILAssembly(ct)
        match asm with
        | Error diags -> return Error(diags)
        | Ok asm ->

        let emitter = this.CreateEmitter(proj.TargetInfo)
        let runtime = OlyRuntime(emitter)

        let refDiags = ImArray.builder()
        comp.References
        |> ImArray.iter (fun x ->
            match x.GetILAssembly(ct) with
            | Ok x -> x.ToReadOnly() |> runtime.ImportAssembly
            | Error diags -> refDiags.AddRange(diags |> ImArray.filter (fun x -> x.IsError))
        )

        if refDiags.Count > 0 then
            return Error(refDiags.ToImmutable())
        else

        match! analyzerTask with
        | Error diags -> return Error(diags)
        | _ ->

        runtime.ImportAssembly(asm.ToReadOnly())

        runtime.InitializeEmitter()

        match OlyTarget.CheckedEmit(asm.EntryPoint.IsSome, proj, runtime, ct) with
        | Some(diag) -> return Error(ImArray.createOne diag)
        | _ ->

        let binDir = this.GetProjectBinDirectory(proj.TargetInfo, proj.Path)
        this.EmitOutput(proj, binDir, emitter, asm.IsDebuggable)
        return Ok(OlyProgram(proj.Path, fun _ -> failwith "Cannot run this program."))
        }

    override this.CanImportReference(path: OlyPath): bool = false

    override this.ImportReferenceAsync(_, _, path: OlyPath, ct: System.Threading.CancellationToken) =
        backgroundTask {
            try
                return raise (System.NotSupportedException($"{path}"))
            with
            | ex ->
                return Error(ex.Message)
        }

    override _.ResolveReferencesAsync(_, _, _, _, ct) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            return OlyReferenceResolutionInfo(ImArray.empty, ImArray.empty, ImArray.empty)
        }

    override this.OnPropertyValidation (targetInfo: OlyTargetInfo, name: string, value: bool): Result<unit,string> = 
        match name with
        | _ -> Error($"'{name}' is listed as a valid property for target platform")

