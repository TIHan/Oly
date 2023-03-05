#version 450

struct Instance
{
  mat4 Transform;
};

layout(location = 0) in vec3 Position;
layout(location = 1) in vec2 TexCoord;
layout(location = 2) in vec3 Normal;

layout(location = 0) out vec2 fsin_TexCoord;
layout(location = 1) out vec3 fsin_Normal;
layout(location = 2) out vec4 fsin_Position;

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

void main()
{
    mat4 transform = Instances[gl_InstanceIndex].Transform;

    mat4 model = Model * transform;

    vec4 position = model * vec4(Position, 1);
    vec3 normal = mat3(transpose(inverse(model))) * Normal;

    fsin_TexCoord = TexCoord;
    fsin_Normal = normal;
    fsin_Position = position;

    gl_Position = Projection * View * position;
}