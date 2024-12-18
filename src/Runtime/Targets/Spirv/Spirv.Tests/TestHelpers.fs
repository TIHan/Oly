﻿module Spirv.TestHelpers

#nowarn "9"

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
type VertexPositionColor(position: Vector2, texCoords: Vector2, color: RgbaFloat) =
    member _.Position = position
    member _.TextureCoordinates = texCoords
    member _.Color = color
    static member SizeInBytes = 32

/// This tests serialization and deserialization.
let glsl_to_vertex (code: string) =
    let options = GlslCompileOptions()
    options.Debug <- true
    let result = SPIRV.SpirvCompilation.CompileGlslToSpirv(code, "vertex.vert", ShaderStages.Vertex, options)
    use origSpirvBytes = new MemoryStream(result.SpirvBytes)
    use spirvBytes = new MemoryStream()
    let spvModule = SpirvModule.Deserialize(origSpirvBytes)
    SpirvModule.Serialize(spirvBytes, spvModule)
    let newResult = spirvBytes.ToArray()
    Assert.Equal<byte>(result.SpirvBytes, newResult)
    spvModule

/// This tests serialization and deserialization.
let glsl_to_fragment (code: string) =
    let options = GlslCompileOptions()
    options.Debug <- true
    let result = SPIRV.SpirvCompilation.CompileGlslToSpirv(code, "fragment.frag", ShaderStages.Fragment, options)
    use origSpirvBytes = new MemoryStream(result.SpirvBytes)
    use spirvBytes = new MemoryStream()
    let spvModule = SpirvModule.Deserialize(origSpirvBytes)
    SpirvModule.Serialize(spirvBytes, spvModule)
    let newResult = spirvBytes.ToArray()
    Assert.Equal<byte>(result.SpirvBytes, newResult)
    spvModule

/// This tests serialization and deserialization.
let glsl_to_compute (code: string) =
    let options = GlslCompileOptions()
    options.Debug <- true
    let result = SPIRV.SpirvCompilation.CompileGlslToSpirv(code, "compute.comp", ShaderStages.Compute, options)
    use origSpirvBytes = new MemoryStream(result.SpirvBytes)
    use spirvBytes = new MemoryStream()
    let spvModule = SpirvModule.Deserialize(origSpirvBytes)
    SpirvModule.Serialize(spirvBytes, spvModule)
    let newResult = spirvBytes.ToArray()
    Assert.Equal<byte>(result.SpirvBytes, newResult)
    spvModule

let draw_quad (spvVertex: SpirvModule, spvFragment: SpirvModule) =

    use spvVertexBytes = new MemoryStream()
    use spvFragmentBytes = new MemoryStream()

    SpirvModule.Serialize(spvVertexBytes, spvVertex)
    SpirvModule.Serialize(spvFragmentBytes, spvFragment)

    let mutable windowCI = 
        new WindowCreateInfo
            (
                X = 0,
                Y = 0,
                WindowWidth = 256,
                WindowHeight = 256,
                WindowTitle = "SpirV Test"
            )
    let window = VeldridStartup.CreateWindow(&windowCI);
    let options = 
        new GraphicsDeviceOptions
            (
                PreferStandardClipSpaceYDirection = true,
                PreferDepthRangeZeroToOne = true,
                Debug = true
            )
    let graphicsDevice = VeldridStartup.CreateVulkanGraphicsDevice(options, window)
    let factory = graphicsDevice.ResourceFactory

    // ----

    let quadVertices =
        [|
            VertexPositionColor(Vector2(-1.f, 1.f), Vector2(0.f, 1.f), RgbaFloat.Red)
            VertexPositionColor(Vector2(1.f, 1.f), Vector2(1.f, 1.f), RgbaFloat.Green)
            VertexPositionColor(Vector2(-1f, -1f), Vector2(0.f, 0.f), RgbaFloat.Blue)
            VertexPositionColor(Vector2(1f, -1f), Vector2(1.f, 0.f), RgbaFloat.Yellow)
        |]

    let quadIndices = [| 0us; 1us; 2us; 3us |]

    let vertexBuffer = factory.CreateBuffer(BufferDescription(4u * uint32(VertexPositionColor.SizeInBytes), BufferUsage.VertexBuffer))
    let indexBuffer = factory.CreateBuffer(BufferDescription(4u * uint32(sizeof<uint16>), BufferUsage.IndexBuffer))

    graphicsDevice.UpdateBuffer(vertexBuffer, 0u, quadVertices)
    graphicsDevice.UpdateBuffer(indexBuffer, 0u, quadIndices)

    let vertexLayout = VertexLayoutDescription(
        VertexElementDescription("Position", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float2),
        VertexElementDescription("TexCoords", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float2),
        VertexElementDescription("Color", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float4))

    let vertexShaderDesc = ShaderDescription(
        ShaderStages.Vertex,
        spvVertexBytes.ToArray(),
        "main")
    let fragmentShaderDesc = ShaderDescription(
        ShaderStages.Fragment,
        spvFragmentBytes.ToArray(),
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
    graphicsDevice.WaitForIdle()
    commandList.Dispose()

    System.Threading.Thread.Sleep(1000)

    // ----
    window.Close()
    pipeline.Dispose()
    shaders
    |> Array.iter (fun shader -> 
        shader.Dispose()
    )
    commandList.Dispose()
    vertexBuffer.Dispose()
    indexBuffer.Dispose()
    graphicsDevice.Dispose()

let compute<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> (spvCompute: SpirvModule, input: 'T array) =

    let spvComputeBytes = new FileStream("compute.spv", FileMode.Create)
    SpirvModule.Serialize(spvComputeBytes, spvCompute)
    spvComputeBytes.Dispose()

    let shaderPath = spvComputeBytes.Name

    let dataKind =
        let ty = typeof<'T>
        if ty = typeof<float32> then
            "float32"
        elif ty = typeof<int32> then
            "int32"
        else
            failwith $"Invalid data kind: {ty.FullName}"
    let data = Json.JsonSerializer.Serialize<_>(input)

    use p = 
        let gpuTest = """C:\work\Evergreen\src\managed\Engine\bin\dotnet\gpu_test.olyx\gpu_test.dll"""
        let inputJson = $"""{{ \"ShaderKind\": \"compute\", \"DataKind\": \"{dataKind}\", \"Data\": """ + data + " }"
        new Oly.Core.ExternalProcess(
            "dotnet",
            $"""{gpuTest} "" "{inputJson}" "{shaderPath}" """
        )

    let output = p.RunAsync(Threading.CancellationToken.None).Result
    if String.IsNullOrWhiteSpace(output.Errors) then
        Json.JsonSerializer.Deserialize<'T[]>(output.Output)
    else
        failwith output.Errors

    //let mutable windowCI = 
    //    new WindowCreateInfo
    //        (
    //            X = 0,
    //            Y = 0,
    //            WindowWidth = 256,
    //            WindowHeight = 256,
    //            WindowTitle = "SpirV Test"
    //        )
    //windowCI.WindowInitialState <- WindowState.Hidden
    //let window = VeldridStartup.CreateWindow(&windowCI);
    //let options = 
    //    new GraphicsDeviceOptions
    //        (
    //            PreferStandardClipSpaceYDirection = true,
    //            PreferDepthRangeZeroToOne = true,
    //            Debug = true
    //        )
    //let graphicsDevice = VeldridStartup.CreateVulkanGraphicsDevice(options, window)
    //let factory = graphicsDevice.ResourceFactory

    //// ----

    //let buffer = factory.CreateBuffer(BufferDescription(uint32(sizeof<'T> * input.Length), BufferUsage.StructuredBufferReadWrite, uint32(sizeof<'T>), true))
    //graphicsDevice.UpdateBuffer(buffer, 0u, input)

    //let computeShaderDesc = ShaderDescription(
    //    ShaderStages.Compute,
    //    spvComputeBytes.ToArray(),
    //    "main")

    //let shader = factory.CreateFromSpirv(computeShaderDesc)

    //let resDesc = ResourceLayoutDescription(ResourceLayoutElementDescription("buffer", ResourceKind.StructuredBufferReadWrite, ShaderStages.Compute))
    //let layout = factory.CreateResourceLayout(resDesc)

    //let mutable pipelineDescription = ComputePipelineDescription(shader, [|layout|], 1u, 1u, 1u)
    //let pipeline = factory.CreateComputePipeline(pipelineDescription)

    //// ----

    //let resSetDesc = ResourceSetDescription(layout, buffer)
    //let resSet = factory.CreateResourceSet(resSetDesc)

    //let readBuffer = factory.CreateBuffer(BufferDescription(uint32(sizeof<'T> * input.Length), BufferUsage.Staging, 0u, false))
    //graphicsDevice.UpdateBuffer(buffer, 0u, Array.zeroCreate<'T> input.Length)

    //let commandList = factory.CreateCommandList()

    //commandList.Begin()

    //commandList.SetPipeline(pipeline);
    //commandList.SetComputeResourceSet(0u, resSet)
    //commandList.Dispatch(uint32(input.Length), 1u, 1u)

    //commandList.CopyBuffer(buffer, 0u, readBuffer, 0u, uint32(sizeof<'T> * input.Length))

    //commandList.End()

    //graphicsDevice.SubmitCommands(commandList)
    //graphicsDevice.WaitForIdle()
    //commandList.Dispose()

    //let output = Array.zeroCreate input.Length
    //let mapped = graphicsDevice.Map(readBuffer, MapMode.Read)
    //let mappedSpan = Span<'T>(mapped.Data |> NativeInterop.NativePtr.ofNativeInt<byte> |> NativeInterop.NativePtr.toVoidPtr, int32(mapped.SizeInBytes) / sizeof<'T>)
    //mappedSpan.CopyTo(Span(output))
    //graphicsDevice.Unmap(readBuffer)

    //// ----
    //window.Close()
    //pipeline.Dispose()
    //resSet.Dispose()
    //shader.Dispose()
    //commandList.Dispose()
    //buffer.Dispose()
    //graphicsDevice.Dispose()

    //output
