#version 450

struct Instance
{
  mat4 Transform;
};

layout(location = 0) in vec3 in_Position_local;
layout(location = 1) in vec2 TexCoord;
layout(location = 2) in vec3 Normal;

layout(location = 0) out vec4 out_ViewPort;
layout(location = 1) out vec4 out_Position_world;
layout(location = 2) out vec2 out_TexCoord;
layout(location = 3) out vec3 out_Normal;
layout(location = 4) out mat4 out_ViewProjectionInverse;
layout(location = 8) out mat4 out_PreviousViewProjection;

layout(set = 0, binding = 0) uniform _Global
{
    mat4 View;
    mat4 Projection;
    mat4 NormalMatrix;
    mat4 PreviousView;
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
    vec3 normal = mat3(transpose(inverse(model))) * Normal;

    out_TexCoord = vec2(TexCoord.x, -TexCoord.y);
    out_Normal = normal;
    out_Position_world = position_world;
    out_ViewProjectionInverse = inverse(Projection * View);
    out_PreviousViewProjection = Projection * PreviousView;
    out_ViewPort = ViewPort;

    gl_Position = position_world;
}