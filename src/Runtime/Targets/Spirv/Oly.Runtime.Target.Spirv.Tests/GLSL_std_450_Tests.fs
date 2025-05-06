module Oly.Runtime.Target.Spirv.GLSL_std_450_Tests

open System
open System.Numerics
open System.Diagnostics
open WorkspaceUtilities
open Xunit
open Spirv.TestHelpers
open Oly.Compiler.Workspace
open Oly.Core

let build isDebug src =
    buildWith (SpirvTarget()) isDebug src

let runAux input (program: OlyProgram) =
    program.Run()
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())
    compute(sm, input)

[<DebuggerHidden>]
let run<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> (input: 'T array) (expectedOutput: 'T array) (src: string) =
    let tyName =
        match typeof<'T> with
        | x when x = typeof<float32> -> "float"
        | x when x = typeof<Vector2> -> "vec2"
        | x when x = typeof<Vector3> -> "vec3"
        | x when x = typeof<Vector4> -> "vec4"
        | x when x = typeof<int32> -> "int"
        | x ->
            invalidOp $"Type '{x.FullName}' not supported or implemented."

    let src = $"""
#target "spirv: compute, 1.3"

Buffer: mutable {tyName}[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

Test(input: {tyName}): {tyName} =
    {src}

main(): () =
    let index = GlobalInvocationId.x
    Buffer[index] <- Test(Buffer[index])
"""
    let output = build true src |> runAux input
    Assert.Equal<'T>(expectedOutput, output)
    let output = build false src |> runAux input
    Assert.Equal<'T>(expectedOutput, output)

[<Fact>]
let ``round - float``() =
    "round(input)"
    |> run [|0.1f|] [|0.0f|]

    "round(input)"
    |> run [|0.9f|] [|1.0f|]

    "round(input)"
    |> run [|0.5f|] [|0.0f|]

    "round(input)"
    |> run [|1.5f|] [|2.0f|]

    "round(input)"
    |> run [|3.5f|] [|4.0f|]

    "round(input)"
    |> run [|4.5f|] [|4.0f|]

    "round(input)"
    |> run [|2.6f|] [|3.0f|]

[<Fact>]
let ``round - vec2``() =
    "round(input)"
    |> run [|Vector2(0.1f)|] [|Vector2(0.0f)|]

    "round(input)"
    |> run [|Vector2(0.1f, 0.9f)|] [|Vector2(0.0f, 1.0f)|]

[<Fact>]
let ``round - vec3``() =
    "let result = round(vec3(input.y, input.z, input.w))\n    vec4(0, result.x, result.y, result.z)"
    |> run [|Vector4(0.1f)|] [|Vector4(0.0f)|]

    "let result = round(vec3(input.y, input.z, input.w))\n    vec4(0, result.x, result.y, result.z)"
    |> run [|Vector4(0.1f, 0.1f, 0.1f, 0.9f)|] [|Vector4(0.0f, 0.0f, 0.0f, 1.0f)|]

[<Fact>]
let ``round - vec4``() =
    "round(input)"
    |> run [|Vector4(0.1f)|] [|Vector4(0.0f)|]

    "round(input)"
    |> run [|Vector4(0.1f, 0.1f, 0.1f, 0.9f)|] [|Vector4(0.0f, 0.0f, 0.0f, 1.0f)|]

[<Fact>]
let ``roundEven - float``() =
    "roundEven(input)"
    |> run [|0.1f|] [|0.0f|]

    "roundEven(input)"
    |> run [|0.9f|] [|1.0f|]

    "roundEven(input)"
    |> run [|0.5f|] [|0.0f|]

    "roundEven(input)"
    |> run [|1.5f|] [|2.0f|]

    "roundEven(input)"
    |> run [|3.5f|] [|4.0f|]

    "roundEven(input)"
    |> run [|4.5f|] [|4.0f|]

    "roundEven(input)"
    |> run [|2.5f|] [|2.0f|]

[<Fact>]
let ``roundEven - vec2``() =
    "roundEven(input)"
    |> run [|Vector2(0.1f)|] [|Vector2(0.0f)|]

    "roundEven(input)"
    |> run [|Vector2(0.1f, 0.9f)|] [|Vector2(0.0f, 1.0f)|]

[<Fact>]
let ``roundEven - vec3``() =
    "let result = roundEven(vec3(input.y, input.z, input.w))\n    vec4(0, result.x, result.y, result.z)"
    |> run [|Vector4(0.1f)|] [|Vector4(0.0f)|]

    "let result = roundEven(vec3(input.y, input.z, input.w))\n    vec4(0, result.x, result.y, result.z)"
    |> run [|Vector4(0.1f, 0.1f, 0.1f, 0.9f)|] [|Vector4(0.0f, 0.0f, 0.0f, 1.0f)|]

[<Fact>]
let ``roundEven - vec4``() =
    "roundEven(input)"
    |> run [|Vector4(0.1f)|] [|Vector4(0.0f)|]

    "roundEven(input)"
    |> run [|Vector4(0.1f, 0.1f, 0.1f, 0.9f)|] [|Vector4(0.0f, 0.0f, 0.0f, 1.0f)|]

[<Fact>]
let ``floor - float``() =
    "floor(input)"
    |> run [|0.1f|] [|0.0f|]

