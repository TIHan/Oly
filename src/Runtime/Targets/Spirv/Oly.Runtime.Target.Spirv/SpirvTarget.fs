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

    override this.CreateEmitter() = SpirvEmitter()

    override this.EmitOutput(project, _binDirectory, emitter, isDebuggable) = 
        let output = emitter.EmitOutput(isDebuggable)
        let outputFilePath = OlyPath.ChangeExtension(project.Path, ".spv")
        use fs = File.Open(outputFilePath.ToString(), FileMode.Create)
        SpirvModule.Serialize(fs, output.Module)

    override this.IsValidTargetName targetInfo = 
        // Target support
        let splits = targetInfo.Name.Split(',')
        if splits.Length = 2 && targetInfo.IsExecutable then
            match splits[0], splits[1] with
            | "1.3", "vertex" -> true
            | _ -> false
        elif splits.Length = 1 && not targetInfo.IsExecutable then
            match splits[0] with
            | "lib" -> true
            | _ -> false
        else
            false
