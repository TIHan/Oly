module Oly.Targets.Spirv.OlySpirvTestHelpers

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
    let fs = System.IO.File.OpenRead(program.Path.ChangeExtension(".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())
    compute(sm, input)

[<DebuggerHidden>]
let run<'T, 'TResult when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T) and 'T : unmanaged and 'TResult : struct and 'TResult :> ValueType and 'TResult : (new : unit-> 'TResult)> (input: 'T array) (expectedOutput: 'TResult array) (src: string) =
    let tyName = getNumericsDataKind<'T>
    let returnTyName = getNumericsDataKind<'TResult>

    let src = $"""
#target "spirv: compute, 1.3"

InputBuffer: mutable {tyName}[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

OutputBuffer: mutable {returnTyName}[]
    #[storage_buffer, descriptor_set(1), binding(0)]
    get

Test(input: {tyName}): {returnTyName} =
    {src}

main(): () =
    let index = GlobalInvocationId.X
    OutputBuffer[index] <- Test(InputBuffer[index])
"""
    let output = build true src |> runAux input
    Assert.Equal<'TResult>(expectedOutput, output)
    let output = build false src |> runAux input
    Assert.Equal<'TResult>(expectedOutput, output)