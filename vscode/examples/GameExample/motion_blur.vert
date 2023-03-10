#version 450

struct Instance
{
  mat4 Transform;
};

layout(location = 0) in vec3 in_Position_local;
layout(location = 1) in vec2 TexCoord;
layout(location = 2) in vec3 Normal;

layout(location = 0) out vec2 fsin_TexCoord;
layout(location = 1) out vec3 fsin_Normal;
layout(location = 2) out vec4 fsin_Position;
layout(location = 3) out mat4 fsin_ViewProjectionInverse;
layout(location = 7) out mat4 fsin_PreviousViewProjectionInverse;
layout(location = 11) out mat4 fsin_Projection;
layout(location = 15) out vec4 fsin_ViewPort;

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

    vec4 position = model * vec4(in_Position_local, 1);
    vec3 normal = mat3(transpose(inverse(model))) * Normal;

    fsin_TexCoord = vec2(TexCoord.x, -TexCoord.y);
    fsin_Normal = normal;
    fsin_Position = position;
    fsin_ViewProjectionInverse = inverse(Projection * View * model);
    fsin_PreviousViewProjectionInverse = inverse(Projection * PreviousView * model);
    fsin_Projection = Projection;

    fsin_ViewPort = ViewPort;

    gl_Position = position;
}