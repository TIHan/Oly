#version 450

struct Instance
{
  mat4 Model;
};

layout(location = 0) in vec3 in_Position_local;
layout(location = 1) in vec2 in_TexCoord;
layout(location = 2) in vec3 in_Normal;

layout(location = 0) out vec4 out_Color;
layout(location = 1) out vec2 out_TexCoord;
layout(location = 2) out vec3 out_Normal;
layout(location = 3) out vec4 out_Position_world;

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

layout(set = 4, binding = 0) readonly buffer _Colors
{
    vec4[] Colors;
};
layout(set = 4, binding = 1) uniform _ColorsCount
{
    int ColorsCount;
};

float GetScalingFactor(mat4 m)
{
    return sqrt(m[0][0] * m[0][0] + m[0][1] * m[0][1] + m[0][2] * m[0][2]);
}

vec3 extractScale(mat4 matrix) {
  vec3 scale;
  scale.x = length(matrix[0].xyz);
  scale.y = length(matrix[1].xyz);
  scale.z = length(matrix[2].xyz);
  return scale;
}

vec4 ConvertLocalToWorldSpace(vec3 v_local, mat4 model)
{
    return model * vec4(v_local, 1);
}

vec4 ConvertWorldToViewSpace(vec4 v_world, mat4 view)
{
    return view * v_world;
}

vec4 ConvertViewToClipSpace(vec4 v_view, mat4 projection)
{
    return projection * v_view;
}

void main()
{
    mat4 model = Instances[gl_InstanceIndex].Model;

    vec4 position_world = ConvertLocalToWorldSpace(in_Position_local, model);
    vec3 normal = normalize(mat3(transpose(inverse(model))) * in_Normal);
    vec3 scale = extractScale(model);

    out_Color = Colors[gl_InstanceIndex];
    
    if (in_Normal.z == 0 && in_Normal.y < 0)
    {
        out_TexCoord = in_TexCoord * vec2(scale.x, -scale.z);
    }
    else if (in_Normal.z == 0 && in_Normal.y > 0)
    {
        out_TexCoord = in_TexCoord * vec2(scale.x, -scale.z);
    }
    else if (in_Normal.z == 0 && in_Normal.x > 0)
    {
        out_TexCoord = in_TexCoord * vec2(scale.y, -scale.z);
    }
    else if (in_Normal.z == 0 && in_Normal.x < 0)
    {
        out_TexCoord = in_TexCoord * vec2(scale.y, -scale.z);
    }
    else
    {
        out_TexCoord = in_TexCoord * vec2(scale.x, -scale.y);
    }
    out_Normal = normal;
    out_Position_world = position_world;

    gl_Position = ConvertViewToClipSpace(ConvertWorldToViewSpace(position_world, View), Projection);
}