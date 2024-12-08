module Spirv.Tests

open Xunit
open Spirv.TestHelpers

[<Fact>]
let ``Run a simple shader``() =
    let vertexCode = @"
#version 450

layout(location = 0) in vec2 Position;
layout(location = 1) in vec2 TexCoords;
layout(location = 2) in vec4 Color;

layout(location = 0) out vec2 fsin_TexCoords;
layout(location = 1) out vec4 fsin_Color;

void main()
{
    gl_Position = vec4(Position, 0, 1);
    fsin_TexCoords = TexCoords;
    fsin_Color = Color;
}"

    let fragmentCode = @"
#version 450

layout(location = 0) in vec2 fsin_TexCoords;
layout(location = 1) in vec4 fsin_Color;
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
layout(location = 1) in vec2 TexCoords;
layout(location = 2) in vec4 Color;

layout(location = 0) out vec2 fsin_TexCoords;
layout(location = 1) out vec4 fsin_Color;

void main()
{
    gl_Position = vec4(Position, 0, 1);
    fsin_Color = Color;
}"

    let fragmentCode = @"
#version 450

layout(location = 0) in vec2 fsin_TexCoords;
layout(location = 1) in vec4 fsin_Color;
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

    let output = compute(glsl_to_compute computeCode, [|0f|])
    Assert.Equal(0f, output[0])

[<Fact>]
let ``Run a simple compute shader with output``() =
    let computeCode = @"
#version 450

layout(set = 0, binding = 0) buffer Buffer
{
    float data[];
};

float GetValue(float x)
{
    return x;
}

void main()
{
    uvec3 abc = gl_GlobalInvocationID;
    uint index = abc.x;
    data[index] = GetValue(123);
}"

    let output = compute(glsl_to_compute computeCode, [|0f;0f;0f;0f|])
    Assert.Equal(123f, output[0])
    Assert.Equal(123f, output[1])
    Assert.Equal(123f, output[2])
    Assert.Equal(123f, output[3])

[<Fact>]
let ``Run fractal shader``() =
    let fragmentCode = @"
#version 450

layout(location = 0) out vec4 outColor;

layout(location = 0) in vec2 fragTexCoord;

// --------------------------------------------------

// https://creativecommons.org/licenses/by-nc-sa/3.0/deed.en
// Author: bradjamesgrant
// https://www.shadertoy.com/view/tsXBzS
// No modifications have been made.

vec3 palette(float d){
	return mix(vec3(0.2,0.7,0.9),vec3(1.,0.,1.),d);
}

vec2 rotate(vec2 p,float a){
	float c = cos(a);
    float s = sin(a);
    return p*mat2(c,s,-s,c);
}

float map(vec3 p){
    for( int i = 0; i<8; ++i){
        float t = 1*0.2;
        p.xz =rotate(p.xz,t);
        p.xy =rotate(p.xy,t*1.89);
        p.xz = abs(p.xz);
        p.xz-=.5;
	}
	return dot(sign(p),p)/5.;
}

vec4 rm (vec3 ro, vec3 rd){
    float t = 0.;
    vec3 col = vec3(0.);
    float d;
    for(float i =0.; i<64.; i++){
		vec3 p = ro + rd*t;
        d = map(p)*.5;
        if(d<0.02){
            break;
        }
        if(d>100.){
        	break;
        }
        //col+=vec3(0.6,0.8,0.8)/(400.*(d));
        col+=palette(length(p)*.1)/(400.*(d));
        t+=d;
    }
    return vec4(col,1./(d*100.));
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = (fragCoord-(vec2(256, 256)/2.))/256;
	vec3 ro = vec3(0.,0.,-50.);
    ro.xz = rotate(ro.xz,1);
    vec3 cf = normalize(-ro);
    vec3 cs = normalize(cross(cf,vec3(0.,1.,0.)));
    vec3 cu = normalize(cross(cf,cs));
    
    vec3 uuv = ro+cf*3. + uv.x*cs + uv.y*cu;
    
    vec3 rd = normalize(uuv-ro);
    
    vec4 col = rm(ro,rd);
    
    
    fragColor = col;
}

// --------------------------------------------------

void main() 
{
    vec4 fragColor = vec4(0);
    vec2 fragCoord = gl_FragCoord.xy;
    mainImage(fragColor, fragCoord);
    outColor = fragColor;
}"

    let vertexCode = @"
#version 450

layout(location = 0) in vec2 Position;
layout(location = 1) in vec2 TexCoords;
layout(location = 2) in vec4 Color;

layout(location = 0) out vec2 outTexCoords;

void main()
{
    gl_Position = vec4(Position, 0, 1);
    outTexCoords = TexCoords;
}"

    let vertex = glsl_to_vertex vertexCode
    let fragment = glsl_to_fragment fragmentCode
    draw_quad(vertex, fragment)