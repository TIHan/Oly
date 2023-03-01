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
    //     // ambient
    // float ambientStrength = 0.1;
    // vec3 ambient = ambientStrength * lightColor;
  	
    // // diffuse 
    // vec3 norm = normalize(Normal);
    // vec3 lightDir = normalize(lightPos - FragPos);
    // float diff = max(dot(norm, lightDir), 0.0);
    // vec3 diffuse = diff * lightColor;
            
    // vec3 result = (ambient + diffuse) * objectColor;
    // FragColor = vec4(result, 1.0);

    // ambient
    float ambientStrength = 0.1;
    vec3 ambient = ambientStrength * fsin_Color.xyz;

    // diffuse
    vec3 norm = normalize(fsin_Normal);
    vec3 lightDir = normalize(fsin_LightPosition - fsin_Position.xyz);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 diffuse = diff * fsin_Color.xyz;

    vec4 color = texture(sampler2D(Texture, Sampler), fsin_TexCoord);
    color.xyz = (ambient + diffuse) * color.xyz;
    fsout_Color = color;
}