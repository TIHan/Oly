#version 450

layout(location = 0) in vec2 fsin_TexCoord;
layout(location = 1) in vec3 fsin_Normal;
layout(location = 2) in vec3 fsin_Position;
layout(location = 3) in mat4 fsin_ViewProjectionInverse;
layout(location = 7) in mat4 fsin_PreviousViewProjectionInverse;
layout(location = 11) in mat4 fsin_Projection;

layout(location = 0) out vec4 fsout_Color;

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;
layout(set = 2, binding = 2) uniform texture2D DepthTexture;

vec2 ConvertToScreenPosition(vec4 position)
{
    vec4 clipSpacePos = position;//projectionMatrix * viewMatrix * vec4(worldSpacePos, 1.0);
    vec3 ndcSpacePos = clipSpacePos.xyz / clipSpacePos.w;
    return vec2(ndcSpacePos.x, -ndcSpacePos.y) * 0.5 + 0.5 * vec2(1280, 720);
}

void main()
{   
    vec4 value = texture(sampler2D(DepthTexture, Sampler), fsin_TexCoord);
    float zOverW = value.x / value.w;
 
    vec2 currentPos = ConvertToScreenPosition(fsin_ViewProjectionInverse[3]);
    vec2 previousPos = ConvertToScreenPosition(fsin_PreviousViewProjectionInverse[3]);

    vec2 velocity = (currentPos - previousPos) / 2 * 0.1 * zOverW;
  
    vec4 color = texture(sampler2D(Texture, Sampler), fsin_TexCoord);

    vec2 texCoord = fsin_TexCoord;
    texCoord += velocity;

    int numSamples = 4;
    for(int i = 1; i < numSamples; ++i, texCoord += velocity) 
    {   
        vec4 currentColor = texture(sampler2D(Texture, Sampler), texCoord);  
        color += currentColor; 
    } 
  
    fsout_Color = color / numSamples; 
}