#version 450

layout(location = 0) in vec4 fsin_Color;
layout(location = 1) in vec2 fsin_texCoord;
layout(location = 0) out vec4 fsout_Color;

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;

void main()
{
    fsout_Color = texture(sampler2D(Texture, Sampler), fsin_texCoord);
}