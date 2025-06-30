module Oly.Runtime.Target.Spirv.OlySpirvTestHelpers

open System
open System.Numerics
open System.Diagnostics
open System.Text.Json
open WorkspaceUtilities
open Oly.Compiler.Workspace
open Oly.Runtime.Target.DotNet
open TestPlatform
open Xunit
open Oly.Core

let private target = DotNetTarget()
let private cachedBuild isDebug src =
    cachedBuildWith target isDebug src

let private serializeInput dataKind input =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    let data = JsonSerializer.Serialize<_>(input, options)

    $"""{{ "DataKind": "{dataKind}", "Data": """ + data + " }"

let private deserializeOutput<'T> (output: string) =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    JsonSerializer.Deserialize<'T array>(output, options)

let private runAux<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> dataKind (input: 'T array) (program: OlyProgram) : 'T array =
    let input = serializeInput dataKind input

    let fs = System.IO.File.OpenRead(program.Path.ToString())
    use ms = new System.IO.MemoryStream()
    fs.CopyTo(ms)
    ms.Position <- 0
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())

    let output = runAndReturnOutput(ms, [|input|])
    deserializeOutput output

[<DebuggerHidden>]
let run<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> (input: 'T array) (expectedOutput: 'T array) (src: string) =
    let tyName =
        match typeof<'T> with
        | x when x = typeof<float32> -> "float32"
        | x when x = typeof<Vector2> -> "vec2"
        | x when x = typeof<Vector3> -> "vec3"
        | x when x = typeof<Vector4> -> "vec4"
        | x when x = typeof<int32> -> "int32"
        | x ->
            invalidOp $"Type '{x.FullName}' not supported or implemented."

    let dataKind = getNumericsDataKind<'T>

    let src = $"""
#target "dotnet: net8"

module OlyNumericsTest

open System
open System.Numerics
open System.Text.Json

#[export]
class InputDataKind =

    DataKind: string get, set = ""

#[export]
class InputData<T> =

    Data: mutable T[] get, set = mutable []

SerializeOutput<T>(output: mutable T[]): string =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    JsonSerializer.Serialize<mutable T[]>(output, options)

DeserializeInputKind(input: string): string =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    JsonSerializer.Deserialize<InputDataKind>(input, options).DataKind

DeserializeInput<T>(input: string): mutable T[] =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    JsonSerializer.Deserialize<InputData<T>>(input, options).Data

Test(input: {tyName}): {tyName} =
    {src}

Run<T>(input: string, f: T -> T): () =
    let xs = DeserializeInput<T>(input)

    let mutable i = 0
    while (i < xs.Length)
        xs[i] <- f(xs[i])
        i <- i + 1

    print(SerializeOutput(xs))

main(args: string[]): () =
    if (args.Length != 1)
        fail("Invalid test input")

    Run<{tyName}>(args[0], Test)  
"""
    let output = cachedBuild true src |> runAux dataKind input
    Assert.Equal<'T>(expectedOutput, output)
    let output = cachedBuild false src |> runAux dataKind input
    Assert.Equal<'T>(expectedOutput, output)