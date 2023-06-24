#version 450

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec2 inTexCoord;

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

void main() {
    gl_Position = ubo.proj * ubo.view * Instances[gl_InstanceIndex] * vec4(inPosition, 1.0);
    fragTexCoord = inTexCoord;
}