#version 450

layout(set = 0, binding = 0) uniform _Global
{
    mat4 View;
    mat4 Projection;
    mat4 NormalMatrix;
    mat4 PreviousView;
    mat4 PreviousViewProjection;
    mat4 InverseViewProjection;
    vec4 ViewPort;
    float DeltaTime;
};

vec3 GetGlobalViewPosition()
{
    return View[3].xyz;
}

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;

layout(location = 0) in vec4 in_Color;
layout(location = 1) in vec2 in_TexCoord;
layout(location = 2) in vec3 in_Normal;
layout(location = 3) in vec3 in_Position_world;
layout(location = 4) in vec3 in_LightPosition_world;

layout(location = 0) out vec4 out_Color;

vec3 Ambient(
    /* property */ float ambientIntensity, 
    vec3 color)
{
    return ambientIntensity * color;
}

vec3 Diffuse(vec3 lightDir, vec3 normal, vec3 color)
{
    return max(dot(normal, lightDir), 0.0) * color;
}

vec3 Specular(
    /* property */ float specularIntensity,
    vec3 lightDir, 
    vec3 viewDir, 
    vec3 normal,
    vec3 color)
{
    vec3 reflectDir = reflect(-lightDir, normal);
    float specularAmount = pow(max(dot(viewDir, reflectDir), 0.0), 32);
    return specularIntensity * specularAmount * color;  
}

vec3 Phong(
    /* property */ float ambientIntensity,
    /* property */ float specularIntensity,
    /* property */ float intensity, 
    vec3 lightDir,
    vec3 viewDir,
    vec3 normal, 
    vec3 color)
{
    vec3 ambientColor = Ambient(ambientIntensity, color);
    vec3 diffuseColor = Diffuse(lightDir, normal, color);
    vec3 specularColor = Specular(specularIntensity, lightDir, viewDir, normal, color);

    return ambientColor + (intensity * (diffuseColor + specularColor));
}

float PointLightIntensity(
    /* property */ float intensity,
    /* property */ float radius,
    vec3 lightVec)
{
    /* internal property */ float radiusFadeScale = 0.95;

    float radiusFade  = radius * radiusFadeScale;
    float lightLength = abs(length(lightVec));

    // This makes the light not extend past the radius.
    if (lightLength > radius)
    {
        return 0;
    }

    // When the light is nearing the edge of the radius,
    //     start to smoothly fade it.
    if (lightLength > radiusFade)
    {
        float n = abs(1 - smoothstep(radiusFade, radius, lightLength));
        return intensity * n;
    }

    return intensity;
}

void main()
{
    /* internal property */ float ambientIntensity = 0.25;
    /* internal property */ float specularIntensity = 0.5;
    /* internal property */ float intensity = 1;
    /* internal property */ float radius = 5;

    vec3 lightVec = in_LightPosition_world - in_Position_world;
    vec3 lightDir = normalize(lightVec);
    vec3 viewDir  = normalize(GetGlobalViewPosition() - in_Position_world);

    intensity = PointLightIntensity(intensity, radius, lightVec);
    vec3  phongColor = Phong(ambientIntensity, specularIntensity, intensity, lightDir, viewDir, in_Normal, in_Color.xyz);

    vec4 color = texture(sampler2D(Texture, Sampler), in_TexCoord);
    color.xyz = phongColor * color.xyz;
    out_Color = color;
}