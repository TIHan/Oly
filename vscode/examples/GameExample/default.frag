#version 450

layout(location = 0) in vec4 fsin_Color;
layout(location = 1) in vec2 fsin_TexCoord;
layout(location = 2) in vec3 fsin_Normal;
layout(location = 3) in vec3 fsin_Position;
layout(location = 4) in vec3 fsin_LightPosition;

layout(location = 0) out vec4 fsout_Color;

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;

void main()
{
    float maxIntensity = 1;
    float radius = 5;

    // ambient
    float ambientStrength = 0.25;
    vec3 ambient = ambientStrength * fsin_Color.xyz;

    // diffuse
    vec3 dirMag = fsin_LightPosition - fsin_Position.xyz;
    vec3 norm = normalize(fsin_Normal);
    vec3 lightDir = normalize(dirMag);
    float diff = max(dot(norm, lightDir), 0.0);

    float intensity = min(maxIntensity, radius / length(dirMag));

    vec3 diffuse = diff * intensity * fsin_Color.xyz;

    vec4 color = texture(sampler2D(Texture, Sampler), fsin_TexCoord);
    color.xyz = (ambient + diffuse) * color.xyz;
    fsout_Color = color;
}