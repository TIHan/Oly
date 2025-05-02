module Oly.Runtime.Target.Spirv.GLSL_std_450_Tests

open System
open System.Numerics
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
let ``Round``() =
    "round(input)"
    |> run [|0.1f|] [|0.0f|]

[<Fact>]
let ``Round 2``() =
    "round(input)"
    |> run [|0.9f|] [|1.0f|]

[<Fact>]
let ``Round 3``() =
    "round(input)"
    |> run [|0.5f|] [|0.0f|]

[<Fact>]
let ``Round 4``() =
    "round(input)"
    |> run [|1.5f|] [|2.0f|]

[<Fact>]
let ``Round 5``() =
    "round(input)"
    |> run [|3.5f|] [|4.0f|]

[<Fact>]
let ``Round 6``() =
    "round(input)"
    |> run [|4.5f|] [|4.0f|]

[<Fact>]
let ``Round 7``() =
    "round(input)"
    |> run [|Vector2(0.1f)|] [|Vector2(0.0f)|]

    "round(input)"
    |> run [|Vector2(0.1f, 0.9f)|] [|Vector2(0.0f, 1.0f)|]

[<Fact>]
let ``Round 8``() =
    "let result = round(vec3(input.y, input.z, input.w))\n    vec4(0, result.x, result.y, result.z)"
    |> run [|Vector4(0.1f)|] [|Vector4(0.0f)|]

    "let result = round(vec3(input.y, input.z, input.w))\n    vec4(0, result.x, result.y, result.z)"
    |> run [|Vector4(0.1f, 0.1f, 0.1f, 0.9f)|] [|Vector4(0.0f, 0.0f, 0.0f, 1.0f)|]

[<Fact>]
let ``Round 9``() =
    "round(input)"
    |> run [|Vector4(0.1f)|] [|Vector4(0.0f)|]

    "round(input)"
    |> run [|Vector4(0.1f, 0.1f, 0.1f, 0.9f)|] [|Vector4(0.0f, 0.0f, 0.0f, 1.0f)|]

[<Fact>]
let ``RoundEven``() =
    "roundEven(input)"
    |> run [|0.1f|] [|0.0f|]

[<Fact>]
let ``RoundEven 2``() =
    "roundEven(input)"
    |> run [|0.9f|] [|1.0f|]

[<Fact>]
let ``RoundEven 3``() =
    "roundEven(input)"
    |> run [|0.5f|] [|0.0f|]

[<Fact>]
let ``RoundEven 4``() =
    "roundEven(input)"
    |> run [|1.5f|] [|2.0f|]

[<Fact>]
let ``RoundEven 5``() =
    "roundEven(input)"
    |> run [|3.5f|] [|4.0f|]

[<Fact>]
let ``RoundEven 6``() =
    "roundEven(input)"
    |> run [|4.5f|] [|4.0f|]

