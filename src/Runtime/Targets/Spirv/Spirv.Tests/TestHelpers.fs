module Spirv.TestHelpers

open System
open System.IO
open System.Text
open System.Numerics
open Xunit
open Spirv
open Veldrid
open Veldrid.StartupUtilities
open Veldrid.SPIRV

[<Struct>]
type VertexPositionColor(position: Vector2, color: RgbaFloat) =
    member _.Position = position
    member _.Color = color
    static member SizeInBytes = 24

/// This tests serialization and deserialization.
let glsl_to_spirv_vertex (code: string) =
    let options = GlslCompileOptions()
    let result = SPIRV.SpirvCompilation.CompileGlslToSpirv(code, "vertex.vert", ShaderStages.Vertex, options)
    let origSpirvBytes = new MemoryStream(result.SpirvBytes)
    let spirvBytes = new MemoryStream()
    SpirvModule.Serialize(spirvBytes, SpirvModule.Deserialize(origSpirvBytes))
    let newResult = spirvBytes.ToArray()
    Assert.Equal<byte>(result.SpirvBytes, newResult)
    spirvBytes.Dispose()
    origSpirvBytes.Dispose()
    newResult

/// This tests serialization and deserialization.
let glsl_to_spirv_fragment (code: string) =
    let options = GlslCompileOptions()
    let result = SPIRV.SpirvCompilation.CompileGlslToSpirv(code, "fragment.frag", ShaderStages.Fragment, options)
    let origSpirvBytes = new MemoryStream(result.SpirvBytes)
    let spirvBytes = new MemoryStream()
    SpirvModule.Serialize(spirvBytes, SpirvModule.Deserialize(origSpirvBytes))
    let newResult = spirvBytes.ToArray()
    Assert.Equal<byte>(result.SpirvBytes, newResult)
    spirvBytes.Dispose()
    origSpirvBytes.Dispose()
    newResult

let draw_quad (vertexCode: byte array, fragmentCode: byte array) =

    let mutable windowCI = 
        new WindowCreateInfo
            (
                X = 100,
                Y = 100,
                WindowWidth = 960,
                WindowHeight = 540,
                WindowTitle = "Veldrid Tutorial"
            )
    let window = VeldridStartup.CreateWindow(&windowCI);
    let options = 
        new GraphicsDeviceOptions
            (
                PreferStandardClipSpaceYDirection = true,
                PreferDepthRangeZeroToOne = true
            )
    let graphicsDevice = VeldridStartup.CreateGraphicsDevice(window, options)
    let factory = graphicsDevice.ResourceFactory

    // ----

    let quadVertices =
        [|
            VertexPositionColor(Vector2(-0.75f, 0.75f), RgbaFloat.Red)
            VertexPositionColor(Vector2(0.75f, 0.75f), RgbaFloat.Green)
            VertexPositionColor(Vector2(-0.75f, -0.75f), RgbaFloat.Blue)
            VertexPositionColor(Vector2(0.75f, -0.75f), RgbaFloat.Yellow)
        |]

    let quadIndices = [| 0us; 1us; 2us; 3us |]

    let vertexBuffer = factory.CreateBuffer(BufferDescription(4u * uint32(VertexPositionColor.SizeInBytes), BufferUsage.VertexBuffer))
    let indexBuffer = factory.CreateBuffer(BufferDescription(4u * uint32(sizeof<uint16>), BufferUsage.IndexBuffer))

    graphicsDevice.UpdateBuffer(vertexBuffer, 0u, quadVertices)
    graphicsDevice.UpdateBuffer(indexBuffer, 0u, quadIndices)

    let vertexLayout = VertexLayoutDescription(
        VertexElementDescription("Position", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float2),
        VertexElementDescription("Color", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float4))

    let vertexShaderDesc = ShaderDescription(
        ShaderStages.Vertex,
        vertexCode,
        "main")
    let fragmentShaderDesc = ShaderDescription(
        ShaderStages.Fragment,
        fragmentCode,
        "main")

    let shaders = factory.CreateFromSpirv(vertexShaderDesc, fragmentShaderDesc)

    let mutable pipelineDescription = GraphicsPipelineDescription()
    pipelineDescription.BlendState <- BlendStateDescription.SingleOverrideBlend;

    pipelineDescription.DepthStencilState <- DepthStencilStateDescription(
        depthTestEnabled = true,
        depthWriteEnabled = true,
        comparisonKind = ComparisonKind.LessEqual)

    pipelineDescription.RasterizerState <- RasterizerStateDescription(
        cullMode = FaceCullMode.Back,
        fillMode = PolygonFillMode.Solid,
        frontFace = FrontFace.Clockwise,
        depthClipEnabled = true,
        scissorTestEnabled = false);

    pipelineDescription.PrimitiveTopology <- PrimitiveTopology.TriangleStrip;
    pipelineDescription.ResourceLayouts <- System.Array.Empty<ResourceLayout>();

    pipelineDescription.ShaderSet <- ShaderSetDescription(
        vertexLayouts = [| vertexLayout |],
        shaders = shaders)

    pipelineDescription.Outputs <- graphicsDevice.SwapchainFramebuffer.OutputDescription;
    let pipeline = factory.CreateGraphicsPipeline(pipelineDescription)

    // ----

    let commandList = factory.CreateCommandList()

    commandList.Begin()
    commandList.SetFramebuffer(graphicsDevice.SwapchainFramebuffer)
    commandList.ClearColorTarget(0u, RgbaFloat.Black)

    commandList.SetVertexBuffer(0u, vertexBuffer);
    commandList.SetIndexBuffer(indexBuffer, IndexFormat.UInt16);
    commandList.SetPipeline(pipeline);
    commandList.DrawIndexed(
        indexCount = 4u,
        instanceCount = 1u,
        indexStart = 0u,
        vertexOffset = 0,
        instanceStart = 0u)

    commandList.End()

    graphicsDevice.SubmitCommands(commandList)
    graphicsDevice.SwapBuffers()
    
    System.Threading.Thread.Sleep(1000)

    // ----

    pipeline.Dispose()
    shaders
    |> Array.iter (fun shader -> 
        shader.Dispose()
    )
    commandList.Dispose()
    vertexBuffer.Dispose()
    indexBuffer.Dispose()
    graphicsDevice.Dispose()

