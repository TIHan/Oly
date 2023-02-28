#version 450

struct Instance
{
  mat4 Transform;
  vec3 Scale;
  float Padding;
};

layout(location = 0) in vec3 Position;
layout(location = 1) in vec2 TexCoord;
layout(location = 2) in vec3 Normal;

layout(location = 0) out vec4 fsin_Color;
layout(location = 1) out vec2 fsin_texCoord;
layout(location = 2) out vec3 fsin_Diffuse;

layout(set = 0, binding = 0) uniform _ModelViewProjection
{
    mat4 ModelViewProjection;
    float DeltaTime;
};

layout(set = 1, binding = 0) readonly buffer _InstanceData
{
    Instance[] Instances;
};

void main()
{
    mat4 Transform = Instances[gl_InstanceIndex].Transform;
    vec3 scale = Instances[gl_InstanceIndex].Scale;

    vec3 position = Position * scale;

    vec4 color = vec4(1, 1, 1, 1);

    gl_Position = ModelViewProjection * Transform * vec4(position, 1);
    fsin_Color = color;
    fsin_texCoord = TexCoord * scale.xy;

    vec3 norm = normalize(vec3((Transform * vec4(Normal * scale, 1)).xyz));
    vec3 lightPos = normalize(vec3(0, 500, 200));
    vec3 lightDir = normalize(lightPos - position);
    float diff = max(dot(norm, lightDir), 0.0);
    fsin_Diffuse = diff * color.xyz;
}