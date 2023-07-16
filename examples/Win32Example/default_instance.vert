#version 450

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec2 fragTexCoord;

layout(set = 0, binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
} ubo;

layout(set = 0, binding = 2) readonly buffer _InstanceData
{
    mat4[] Instances;
};

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

vec3 extractScale(mat4 matrix) {
  vec3 scale;
  scale.x = length(matrix[0].xyz);
  scale.y = length(matrix[1].xyz);
  scale.z = length(matrix[2].xyz);
  return scale;
}

vec2 CalculateTexCoord(vec2 texCoord, vec3 normal, vec3 scale)
{
    if (normal.z == 0 && normal.y < 0)
    {
        return texCoord * vec2(scale.x, scale.z);
    }

    if (normal.z == 0 && normal.y > 0)
    {
        return texCoord * vec2(scale.x, scale.z);
    }

    if (normal.z == 0 && normal.x > 0)
    {
        return texCoord * vec2(scale.y, scale.z);
    }

    if (normal.z == 0 && normal.x < 0)
    {
        return texCoord * vec2(scale.y, scale.z);
    }

    return texCoord * vec2(scale.x, scale.y);
}

void main() {
    mat4 model = Instances[gl_InstanceIndex];
    vec3 scale = extractScale(model);

    vec4 position_world = ConvertLocalToWorldSpace(inPosition, model);

    gl_Position = ConvertViewToClipSpace(ConvertWorldToViewSpace(position_world, ubo.view), ubo.proj);
    fragTexCoord = CalculateTexCoord(inTexCoord, inNormal, scale);
}