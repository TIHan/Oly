#version 450

layout(location = 0) in vec4 fsin_Color;
layout(location = 1) in vec2 fsin_texCoord;
layout(location = 2) in vec3 fsin_Diffuse;

layout(location = 0) out vec4 fsout_Color;

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;

void main()
{
    vec4 color = texture(sampler2D(Texture, Sampler), fsin_texCoord);
    color.xyz = (vec3(0.2, 0.2, 0.2) + fsin_Diffuse) * color.xyz;
    fsout_Color = color;
}