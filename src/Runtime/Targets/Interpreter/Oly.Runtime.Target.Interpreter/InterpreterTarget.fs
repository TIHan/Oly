﻿namespace Oly.Runtime.Target.Interpreter

open System
open System.IO
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Runtime.Interpreter

[<Sealed>]
type InterpreterTarget() =
    inherit OlyBuild("interpreter")

    let mutable emitterOpt: InterpreterRuntimeEmitter option = None

    member this.Run(args) =
        match emitterOpt with
        | Some emitter -> 
            emitterOpt <- None
            emitter.Run(args)
        | _ ->
            failwith "No successful build found"

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

        let comp = proj.Compilation
        let asm = comp.GetILAssembly(ct)
        match asm with
        | Error diags -> return Error(diags)
        | Ok asm ->

        let emitter = InterpreterRuntimeEmitter(Console.Out)
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

        runtime.ImportAssembly(asm.ToReadOnly())

        runtime.InitializeEmitter()

        if asm.EntryPoint.IsSome then
            runtime.EmitEntryPoint()
        else
            runtime.EmitAheadOfTime()

        return Ok(OlyProgram(proj.Path, fun () -> emitter.Run(ImArray.empty)))
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

    override this.IsValidTargetName targetInfo = targetInfo.Name = "default"

    override _.ResolveReferencesAsync(_, _, _, _, ct) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            return OlyReferenceResolutionInfo(ImArray.empty, ImArray.empty, ImArray.empty)
        }

