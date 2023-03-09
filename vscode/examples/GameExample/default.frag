#version 450

layout(location = 0) in vec4 in_Color;
layout(location = 1) in vec2 in_TexCoord;
layout(location = 2) in vec3 in_Normal;
layout(location = 3) in vec3 in_VertexPosition_world;
layout(location = 4) in vec3 in_LightPosition_world;
layout(location = 5) in vec3 in_ViewPosition_world;

layout(location = 0) out vec4 out_Color;

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;

vec3 Ambient(float ambientStrength, vec3 color)
{
    return ambientStrength * color;
}

vec3 Specular(vec3 lightDir, vec3 viewPosition_world, vec3 vertexPosition_world, vec3 normal, float specularStrength, vec3 color)
{
    vec3 viewDir = normalize(viewPosition_world - vertexPosition_world);
    vec3 reflectDir = reflect(-lightDir, normal);

    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
    return specularStrength * spec * color;  
}

vec3 DiffuseSpecular(
        vec3 lightPosition_world, 
        vec3 viewPosition_world, 
        vec3 vertexPosition_world, 
        vec3 normal, 
        float radius, 
        float maxIntensity, 
        float specularStrength, 
        vec3 color)
{
    vec3 lightVec = lightPosition_world - vertexPosition_world;
    vec3 lightDir = normalize(lightVec);

    float diffuse = clamp(dot(normal, lightDir), 0, 1);
    float intensity = min(maxIntensity, radius / length(lightVec));

    vec3 specular = Specular(lightDir, viewPosition_world, vertexPosition_world, normal, specularStrength, color);

    return (diffuse * intensity * color) + specular;
}

void main()
{
    // Settings
    float ambientStrength = 0.25;
    float specularStrength = 0.5;
    float maxIntensity = 1;
    float radius = 5;

    vec3 ambient = Ambient(ambientStrength, in_Color.xyz);
    vec3 diffuseSpecular = DiffuseSpecular(in_LightPosition_world, in_ViewPosition_world, in_VertexPosition_world, in_Normal, radius, maxIntensity, specularStrength, in_Color.xyz);

    vec4 color = texture(sampler2D(Texture, Sampler), in_TexCoord);
    color.xyz = (ambient + diffuseSpecular) * color.xyz;
    out_Color = color;
}