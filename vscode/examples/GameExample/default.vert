#version 450

struct Instance
{
  mat4 Transform;
};

layout(location = 0) in vec3 Position;
layout(location = 1) in vec2 TexCoord;
layout(location = 2) in vec3 Normal;

layout(location = 0) out vec4 fsin_Color;
layout(location = 1) out vec2 fsin_TexCoord;
layout(location = 2) out vec3 fsin_Normal;
layout(location = 3) out vec4 fsin_Position;
layout(location = 4) out vec3 fsin_LightPosition;

layout(set = 0, binding = 0) uniform _Global
{
    mat4 Model;
    mat4 View;
    mat4 Projection;
    mat4 NormalMatrix;
    float DeltaTime;
};

layout(set = 1, binding = 0) readonly buffer _InstanceData
{
    Instance[] Instances;
};

float GetScalingFactor(mat4 m)
{
    return sqrt(m[0][0] * m[0][0] + m[0][1] * m[0][1] + m[0][2] * m[0][2]);
}

void main()
{
    mat4 transform = Instances[gl_InstanceIndex].Transform;

    vec4 ambientColor = vec4(1, 1, 1, 1);
    mat4 model = Model * transform;

    vec4 position = model * vec4(Position, 1);
    vec3 normal = mat3(transpose(inverse(model))) * Normal;

    fsin_Color = ambientColor;
    fsin_TexCoord = TexCoord * GetScalingFactor(model);
    fsin_Normal = normal;
    fsin_Position = position;
    fsin_LightPosition = vec3(0, 0, 0);

    gl_Position = Projection * View * position;
}