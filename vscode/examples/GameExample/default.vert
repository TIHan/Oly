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
    mat4 PreviousView;
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

vec3 extractScale(mat4 matrix) {
  vec3 scale;
  scale.x = length(matrix[0].xyz);
  scale.y = length(matrix[1].xyz);
  scale.z = length(matrix[2].xyz);
  return scale;
}

void main()
{
    mat4 transform = Instances[gl_InstanceIndex].Transform;

    vec4 ambientColor = vec4(1, 1, 1, 1);
    mat4 model = Model * transform;

    vec4 position = model * vec4(Position, 1);
    vec3 normal = mat3(transpose(inverse(model))) * Normal;

    vec3 scale = extractScale(model);

    fsin_Color = ambientColor;
    // TODO: This isn't 100% correct. What is the proper way to adjust this?
    if (Normal.z == 0 && Normal.y < 0)
    {
        fsin_TexCoord = TexCoord * vec2(scale.x, -scale.z);
    }
    else if (Normal.z == 0 && Normal.y > 0)
    {
        fsin_TexCoord = TexCoord * vec2(-scale.x, scale.z);
    }
    else if (Normal.z == 0 && Normal.x > 0)
    {
        fsin_TexCoord = TexCoord * vec2(-scale.z, scale.y);
    }
    else if (Normal.z == 0 && Normal.x < 0)
    {
        fsin_TexCoord = TexCoord * vec2(scale.z, -scale.y);
    }
    else
    {
        fsin_TexCoord = TexCoord * vec2(scale.x, -scale.y);
    }
    fsin_Normal = normal;
    fsin_Position = position;
    fsin_LightPosition = vec3(0, 0, 0);

    gl_Position = Projection * View * position;
}