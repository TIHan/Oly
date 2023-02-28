#version 450

layout(location = 0) in vec3 Position;
//layout(location = 1) in vec3 Normal;
layout(location = 1) in vec2 TexCoord;

layout(location = 0) out vec4 fsin_Color;
layout(location = 1) out vec2 fsin_texCoord;

layout(set = 0, binding = 0) uniform _ModelViewProjection
{
    mat4 ModelViewProjection;
    float DeltaTime;
};

layout(set = 1, binding = 0) readonly buffer _Transforms
{
    mat4[] Transforms;
};

void main()
{
    mat4 Transform = Transforms[gl_InstanceIndex];
    gl_Position = ModelViewProjection * Transform * vec4(Position, 1);
    fsin_Color = vec4(Position.x, Position.y, 1, 1);
    fsin_texCoord = TexCoord;
}