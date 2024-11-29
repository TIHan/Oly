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

[<Fact>]
let ``Run a simple shader that produces a phi instruction``() =
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
    vec4 color = fsin_Color;
    if (color.x == 1)
    {
        color = vec4(0.5);
        color.x = 2;
    }
    else
    {
        color = vec4(0.6);
    }
    fsout_Color = color;
}"

    let vertex = glsl_to_vertex vertexCode
    let fragment = glsl_to_fragment fragmentCode
    draw_quad(vertex, fragment)

[<Fact>]
let ``Run a simple compute shader``() =
    let computeCode = @"
#version 450

void main()
{
}"

    let output = compute_floats(glsl_to_compute computeCode, [|0f|])
    Assert.Equal(0f, output[0])

[<Fact>]
let ``Run a simple compute shader with output``() =
    let computeCode = @"
#version 450

layout(set = 0, binding = 0) buffer Buffer
{
    float data[];
};

void main()
{
    uvec3 abc = gl_GlobalInvocationID;
    uint index = abc.x;
    data[index] = 123;
}"

    let output = compute_floats(glsl_to_compute computeCode, [|0f;0f;0f;0f|])
    Assert.Equal(123f, output[0])
    Assert.Equal(123f, output[1])
    Assert.Equal(123f, output[2])
    Assert.Equal(123f, output[3])
