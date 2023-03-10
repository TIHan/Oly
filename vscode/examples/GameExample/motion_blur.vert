#version 450

struct Instance
{
  mat4 Transform;
};

layout(location = 0) in vec3 in_Position_local;
layout(location = 1) in vec2 TexCoord;
layout(location = 2) in vec3 Normal;

layout(location = 0) out vec2 out_TexCoord;

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

layout(set = 1, binding = 0) readonly buffer _InstanceData
{
    Instance[] Instances;
};

void main()
{
    mat4 transform = Instances[gl_InstanceIndex].Transform;
    mat4 model = transform;

    vec4 position_world = model * vec4(in_Position_local, 1);

    out_TexCoord = vec2(TexCoord.x, -TexCoord.y);

    gl_Position = position_world;
}