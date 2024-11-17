module Spirv.Tests

open Xunit
open Spirv.TestHelpers

[<Fact>]
let ``Run a simple shader``() =
    let vertexCode = @"
#version 450

layout(location = 0) in vec2 Position;
layout(location = 1) in vec4 Color;

layout(location = 0) out vec4 fsin_Color;

void main()
{
    gl_Position = vec4(Position, 0, 1);
    fsin_Color = Color;
}"

    let fragmentCode = @"
#version 450

layout(location = 0) in vec4 fsin_Color;
layout(location = 0) out vec4 fsout_Color;

void main()
{
    fsout_Color = fsin_Color;
}"


    let vertex = glsl_to_vertex vertexCode
    let fragment = glsl_to_fragment fragmentCode
    draw_quad(vertex, fragment)