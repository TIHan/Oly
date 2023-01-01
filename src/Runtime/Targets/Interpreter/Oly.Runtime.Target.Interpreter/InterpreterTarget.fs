namespace Oly.Runtime.Target.Interpreter

open System
open System.IO
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Runtime.Interpreter

[<Sealed>]
type InterpreterTarget() =
    inherit OlyBuild("i")

    let relativeOutputDir = String.Empty

    override this.OnBeforeReferencesImported() = ()
        
    override this.OnAfterReferencesImported() = ()

    override this.BuildProjectAsync(proj, ct: System.Threading.CancellationToken) = backgroundTask { 
        let comp = proj.Compilation
        let asm = comp.GetILAssembly(ct)
        match asm with
        | Error msg -> return Error msg.Message
        | Ok asm ->

        let emitter = InterpreterRuntimeEmitter()
        let runtime = OlyRuntime(emitter)

        comp.References
        |> ImArray.iter (fun x -> x.GetILAssembly(ct).ToReadOnly() |> runtime.ImportAssembly)
        runtime.ImportAssembly(asm.ToReadOnly())

        if asm.EntryPoint.IsSome then
            runtime.EmitEntryPoint()
        else
            runtime.EmitAheadOfTime()

        return Ok ("")
        }

    override this.CanImportReference(path: OlyPath): bool = false

    override this.ImportReferenceAsync(_, path: OlyPath, ct: System.Threading.CancellationToken) =
        backgroundTask {
            try
                return raise (System.NotSupportedException($"{path}"))
            with
            | ex ->
                return Error(ex.Message)
        }

    override this.IsValidTargetName targetInfo = targetInfo.Name = "default"

    override _.ResolveReferencesAsync(_, _, _, _, ct) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            return OlyReferenceResolutionInfo(ImArray.empty, ImArray.empty)
        }

