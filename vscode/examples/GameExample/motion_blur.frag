/*

// Shader Code That Uses the Velocity Vector at the Current Pixel to Sample the Color Buffer Multiple Times to Achieve the Motion Blur Effect
// Get the initial color at this pixel.    
float4 color = tex2D(sceneSampler, texCoord)
texCoord += velocity
for(int i = 1; i < g_numSamples; ++i, texCoord += velocity) {   
    // Sample the color buffer along the velocity vector.    
    float4 currentColor = tex2D(sceneSampler, texCoord);   
    // Add the current color to our color sum.   
    color += currentColor; } 
// Average all of the samples to get the final blur color.    
float4 finalColor = color / numSamples; 
*/

#version 450

layout(location = 0) in vec2 fsin_TexCoord;
layout(location = 1) in vec3 fsin_Normal;
layout(location = 2) in vec3 fsin_Position;
layout(location = 3) in mat4 fsin_ViewProjectionInverse;
layout(location = 7) in mat4 fsin_PreviousViewProjectionInverse;

layout(location = 0) out vec4 fsout_Color;

layout(set = 2, binding = 0) uniform texture2D Texture;
layout(set = 2, binding = 1) uniform sampler Sampler;

void main()
{
    // Shader Code That Extracts the Per-Pixel World-Space Positions of the Objects That Were Rendered to the Depth Buffer
    // ----
    // Get the depth buffer value at this pixel.    
    float zOverW = texture(sampler2D(Texture, Sampler), fsin_TexCoord).z;

    // H is the viewport position at this pixel in the range -1 to 1.   
    vec4 H = vec4(fsin_TexCoord.x * 2 - 1, (1 - fsin_TexCoord.y) * 2 - 1, zOverW, 1); 

     // Transform by the view-projection inverse.    
    vec4 D = H * fsin_ViewProjectionInverse;

     // Divide by w to get the world position.    
    vec4 worldPos = D / D.w; 

    // Shader Code That Computes the Per-Pixel Velocity Vectors That Determine the Direction to Blur the Image
    // ----
    // Current viewport position    
    vec4 currentPos = H; 

    // Use the world position, and transform by the previous view-projection matrix.    
    vec4 previousPos = worldPos * fsin_PreviousViewProjectionInverse; 

    // Convert to nonhomogeneous points [-1,1] by dividing by w. 
    previousPos /= previousPos.w; 

    // Use this frame's position and last frame's to compute the pixel velocity.    
    vec2 velocity = ((currentPos - previousPos) / 2).xy; 

//     // Shader Code That Uses the Velocity Vector at the Current Pixel to Sample the Color Buffer Multiple Times to Achieve the Motion Blur Effect
//     // ----
//     // Get the initial color at this pixel.    
// float4 color = tex2D(sceneSampler, texCoord)
// texCoord += velocity
// for(int i = 1; i < g_numSamples; ++i, texCoord += velocity) {   
//     // Sample the color buffer along the velocity vector.    
//     float4 currentColor = tex2D(sceneSampler, texCoord);   
//     // Add the current color to our color sum.   
//     color += currentColor; } 
// // Average all of the samples to get the final blur color.    
// float4 finalColor = color / numSamples; 

    fsout_Color = texture(sampler2D(Texture, Sampler), fsin_TexCoord);
}