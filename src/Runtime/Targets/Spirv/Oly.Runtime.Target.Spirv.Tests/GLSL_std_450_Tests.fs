module Oly.Runtime.Target.Spirv.GLSL_std_450_Tests

open Xunit
open System.Numerics
open Oly.Runtime.Target.Spirv.OlySpirvTestHelpers

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

