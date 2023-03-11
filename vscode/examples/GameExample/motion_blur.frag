#version 450

layout(location = 0) in vec4 in_ViewPort;
layout(location = 1) in vec4 in_Position_world;
layout(location = 2) in vec2 in_TexCoord;
layout(location = 3) in vec3 in_Normal;
layout(location = 4) in mat4 in_ViewProjectionInverse;
layout(location = 8) in mat4 in_PreviousViewProjection;

layout(location = 0) out vec4 fsout_Color;

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;
layout(set = 2, binding = 2) uniform texture2D DepthTexture;

// x and y will be range -1 to 1.
vec4 CalculatePixelPosition(vec2 texCoord, float depth)
{
    float x = in_ViewPort.x;
    float y = in_ViewPort.y;
    float width = in_ViewPort.z;
    float height = in_ViewPort.w;

    float xw = texCoord.x * (width / 2) + x;
    float yw = (texCoord.y + 1) * (height / 2 ) + y;

    float px = xw - (width / 4);
    px = px / (width / 4);

    float py = yw - (height / 4);
    py = py / (height / 4);

    return vec4(px, py, depth, 1);
}

void main()
{   
    float intensity = 10;

    vec4 value = texture(sampler2D(DepthTexture, Sampler), in_TexCoord);
    float depth = value.x / value.w;

    vec4 pixelPosition = CalculatePixelPosition(in_TexCoord, depth);

    vec4 worldPosition = in_ViewProjectionInverse * pixelPosition;
    worldPosition /= worldPosition.w;

    vec4 previousPosition = in_PreviousViewProjection * worldPosition;
    previousPosition /= previousPosition.w;

    vec2 velocity = ((pixelPosition - previousPosition) / 2).xy * intensity;
  
    vec4 color = texture(sampler2D(Texture, Sampler), in_TexCoord);

    vec2 texCoord = in_TexCoord;
    texCoord += velocity;

    int numSamples = 2;
    for(int i = 1; i < numSamples; ++i, texCoord += velocity) 
    {   
        texCoord.x = clamp(texCoord.x, 0, 1);
        texCoord.y = clamp(texCoord.y, -1, 0);
        vec4 currentColor = texture(sampler2D(Texture, Sampler), texCoord);  
        color += currentColor;
    } 
  
    color = color / numSamples;

    fsout_Color = color;
}