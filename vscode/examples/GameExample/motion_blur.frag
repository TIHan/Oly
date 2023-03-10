#version 450

layout(location = 0) in vec2 fsin_TexCoord;
layout(location = 1) in vec3 fsin_Normal;
layout(location = 2) in vec3 fsin_Position;
layout(location = 3) in mat4 fsin_ViewProjectionInverse;
layout(location = 7) in mat4 fsin_PreviousViewProjectionInverse;
layout(location = 11) in mat4 fsin_Projection;
layout(location = 15) in vec4 in_ViewPort;

layout(location = 0) out vec4 fsout_Color;

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;
layout(set = 2, binding = 2) uniform texture2D DepthTexture;

vec2 ConvertToScreenPosition(vec4 position)
{
    float x = in_ViewPort.x;
    float y = in_ViewPort.y;
    float width = in_ViewPort.z;
    float height = in_ViewPort.w;
    vec4 clipSpacePos = position;//projectionMatrix * viewMatrix * vec4(worldSpacePos, 1.0);
    vec3 ndc_position = clipSpacePos.xyz / clipSpacePos.w;

    float xw = (ndc_position.x + 1) * (width / 2) + x;
    float yw = (ndc_position.y + 1) * (height / 2 ) + y;

    return vec2(xw, yw);
}

void main()
{   
    vec4 value = texture(sampler2D(DepthTexture, Sampler), fsin_TexCoord);
    float depth = value.x / value.w;
 
    vec2 currentPos = ConvertToScreenPosition(fsin_ViewProjectionInverse[3]);
    vec2 previousPos = ConvertToScreenPosition(fsin_PreviousViewProjectionInverse[3]);

    vec2 velocity = (currentPos - previousPos) * 0.000015 * abs(1 - depth);
  
    vec4 color = texture(sampler2D(Texture, Sampler), fsin_TexCoord);

    vec2 texCoord = fsin_TexCoord;
    texCoord += velocity;

    int numSamples = 20;
    for(int i = 1; i < numSamples; ++i, texCoord += velocity) 
    {   
        texCoord.x = clamp(texCoord.x, 0, 1);
        texCoord.y = clamp(texCoord.y, -1, 0);
        vec4 currentColor = texture(sampler2D(Texture, Sampler), texCoord);  
        color += currentColor; 
    } 
  
    fsout_Color = color / numSamples; 
}