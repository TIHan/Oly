module Oly.Targets.Spirv.OlySpirvTestHelpers

open System
open System.Numerics
open System.Diagnostics
open System.Text.Json
open WorkspaceUtilities
open Oly.Compiler.Workspace
open Oly.Targets.DotNet
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

    $"""{{ "InputDataKind": "{dataKind}", "InputData": """ + data + " }"

let private deserializeOutput<'T> (output: string) =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    JsonSerializer.Deserialize<'T array>(output, options)

let private runAux<'T, 'TResult when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T) and 'TResult : unmanaged and 'TResult : struct and 'TResult :> ValueType and 'TResult : (new : unit-> 'TResult)> dataKind (input: 'T array) (program: OlyProgram) : 'TResult array =
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
let run<'T, 'TResult when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T) and 'TResult : unmanaged and 'TResult : struct and 'TResult :> ValueType and 'TResult : (new : unit-> 'TResult)> (input: 'T array) (expectedOutput: 'TResult array) (src: string) =
    let tyName =
        match typeof<'T> with
        | x when x = typeof<float32> -> "float32"
        | x when x = typeof<Vector2> -> "vec2"
        | x when x = typeof<Vector3> -> "vec3"
        | x when x = typeof<Vector4> -> "vec4"
        | x when x = typeof<int32> -> "int32"
        | x ->
            invalidOp $"Type '{x.FullName}' not supported or implemented."
    let returnTyName =
        match typeof<'TResult> with
        | x when x = typeof<float32> -> "float32"
        | x when x = typeof<Vector2> -> "vec2"
        | x when x = typeof<Vector3> -> "vec3"
        | x when x = typeof<Vector4> -> "vec4"
        | x when x = typeof<int32> -> "int32"
        | x ->
            invalidOp $"Type '{x.FullName}' not supported or implemented."

    let inputDataKind = getNumericsDataKind<'T>

    let src = $"""
#target "dotnet: net10.0"

module OlyNumericsTest

open System
open System.Numerics
open System.Text.Json

#[export]
class InputDataKind =

    InputDataKind: string get, set = ""

#[export]
class InputData<T> =

    InputData: mutable T[] get, set = mutable []

SerializeOutput<T>(output: mutable T[]): string =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    JsonSerializer.Serialize<mutable T[]>(output, options)

DeserializeInputKind(input: string): string =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    JsonSerializer.Deserialize<InputDataKind>(input, options).InputDataKind

DeserializeInput<T>(input: string): mutable T[] =
    let options = JsonSerializerOptions()
    options.IncludeFields <- true
    JsonSerializer.Deserialize<InputData<T>>(input, options).InputData

Test(input: {tyName}): {returnTyName} =
    {src}

Run<T, TResult>(input: string, f: T -> TResult): () =
    let xs = DeserializeInput<T>(input)
    let output = zeroArray<TResult>(xs.Length)

    let mutable i = 0
    while (i < xs.Length)
        output[i] <- f(xs[i])
        i <- i + 1

    print(SerializeOutput(output))

main(args: string[]): () =
    if (args.Length != 1)
        fail("Invalid test input")

    Run<{tyName}, {returnTyName}>(args[0], Test)  
"""
    let output = cachedBuild true src |> runAux inputDataKind input
    Assert.Equal<'TResult>(expectedOutput, output)
    let output = cachedBuild false src |> runAux inputDataKind input
    Assert.Equal<'TResult>(expectedOutput, output)