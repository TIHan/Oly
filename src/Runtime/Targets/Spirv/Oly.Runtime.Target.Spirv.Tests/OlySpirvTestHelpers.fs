module Oly.Runtime.Target.Spirv.OlySpirvTestHelpers

open System
open System.Numerics
open System.Diagnostics
open WorkspaceUtilities
open Xunit
open Spirv.TestHelpers
open Oly.Compiler.Workspace
open Oly.Core

let private build isDebug src =
    buildWith (SpirvTarget()) isDebug src

let private runAux input (program: OlyProgram) =
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())
    compute(sm, input)

[<DebuggerHidden>]
let run<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> (input: 'T array) (expectedOutput: 'T array) (src: string) =
    let tyName = getNumericsDataKind<'T>

    let src = $"""
#target "spirv: compute, 1.3"

Buffer: mutable {tyName}[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

Test(input: {tyName}): {tyName} =
    {src}

main(): () =
    let index = GlobalInvocationId.X
    Buffer[index] <- Test(Buffer[index])
"""
    let output = build true src |> runAux input
    Assert.Equal<'T>(expectedOutput, output)
    let output = build false src |> runAux input
    Assert.Equal<'T>(expectedOutput, output)