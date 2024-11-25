namespace Oly.Runtime.Target.Spirv

open System
open System.IO
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Runtime.Target.Core
open Oly.Runtime.Target.Spirv.Emitter
open Spirv
open Spirv.SpirvModule

[<Sealed>]
type SpirvTarget() =
    inherit OlyTargetOutputOnly<SpirvEmitter, SpirvType, SpirvFunction, SpirvField>("spirv")

    override this.CreateEmitter(targetInfo) = 
        let splits = targetInfo.Name.Split(',')
        if splits.Length = 2 && targetInfo.IsExecutable then
            let executionModel =
                match splits[0] with
                | "vertex" -> ExecutionModel.Vertex
                | "fragment" -> ExecutionModel.Fragment
                | "compute" -> ExecutionModel.GLCompute
                | _ -> raise(InvalidOperationException())

            match splits[1] with
            | "1.0" -> SpirvEmitter(1u, 0u, executionModel)
            | "1.1" -> SpirvEmitter(1u, 1u, executionModel)
            | "1.2" -> SpirvEmitter(1u, 2u, executionModel)
            | "1.3" -> SpirvEmitter(1u, 3u, executionModel)
            | "1.4" -> SpirvEmitter(1u, 4u, executionModel)
            | "1.5" -> SpirvEmitter(1u, 5u, executionModel)
            | "1.6" -> SpirvEmitter(1u, 6u, executionModel)
            | _ -> raise(InvalidOperationException(""))
        else
            raise(InvalidOperationException())

    override this.EmitOutput(project, _binDirectory, emitter, isDebuggable) = 
        let output = emitter.EmitOutput(isDebuggable)
        let outputFilePath = OlyPath.ChangeExtension(project.Path, ".spv")
        use fs = File.Open(outputFilePath.ToString(), FileMode.Create)
        SpirvModule.Serialize(fs, output.Module)

    override this.IsValidTargetName targetInfo = 
        // Target support
        let splits = targetInfo.Name.Split(',')
        if splits.Length = 2 && targetInfo.IsExecutable then
            match splits[0] with
            | "vertex"
            | "fragment"
            | "compute" ->
                match splits[1] with
                | "1.0"
                | "1.1"
                | "1.2"
                | "1.3" 
                | "1.4" 
                | "1.5"
                | "1.6" -> true
                | _ -> false
            | _ -> 
                false
        elif splits.Length = 1 && not targetInfo.IsExecutable then
            match splits[0] with
            | "lib" -> true
            | _ -> false
        else
            false
