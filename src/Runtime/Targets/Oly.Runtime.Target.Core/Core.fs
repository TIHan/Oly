namespace Oly.Targets.Core

open Oly.Core
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Compiler.Workspace

[<AbstractClass>]
type OlyTargetOutputOnly<'Emitter, 'Type, 'Function, 'Field when 'Emitter :> IOlyRuntimeEmitter<'Type, 'Function, 'Field>>(platformName) =
    inherit OlyBuild(platformName)

    abstract CreateEmitter : OlyTargetInfo -> 'Emitter

    abstract EmitOutput : OlyProject * binDirectory: OlyPath * 'Emitter * isDebuggable: bool -> unit

    override this.OnBeforeReferencesImportedAsync(_, _, _) = 
        backgroundTask {
            return ()
        }
        
    override this.OnAfterReferencesImported() = ()

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

        if asm.EntryPoint.IsSome then
            runtime.EmitEntryPoint()
        else
            runtime.EmitAheadOfTime()

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

