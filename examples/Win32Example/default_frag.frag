#version 450

layout(location = 0) out vec4 outColor;

layout(location = 0) in vec2 fragTexCoord;

layout(binding = 1) uniform sampler2D texSampler;

void main() {
    vec4 color = texture(texSampler, fragTexCoord);
    outColor = color;
}