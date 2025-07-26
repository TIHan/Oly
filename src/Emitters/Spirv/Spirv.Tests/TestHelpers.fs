module Spirv.TestHelpers

#nowarn "9"

open System
open System.IO
open System.Text
open System.Numerics
open System.Drawing
open Spirv

let lockGpuTestServiceObj = obj()
type GPU =
    [<DefaultValue>]
    static val mutable private gpuTestService: Oly.Core.ExternalProcess option

    static member GetTestService(): Oly.Core.ExternalProcess =
        match GPU.gpuTestService with
        | Some gpuTestService -> gpuTestService
        | _ ->
            lock lockGpuTestServiceObj (fun () ->
                match GPU.gpuTestService with
                | Some gpuTestService -> gpuTestService
                | _ ->
                    let gpuTest = Environment.GetEnvironmentVariable("EVERGREEN_GPU_TEST_PATH", EnvironmentVariableTarget.User)
                    if isNull gpuTest then
                        failwith "User environment variable 'EVERGREEN_GPU_TEST_PATH' not set."
                    if File.Exists(gpuTest) |> not then
                        failwith "Unable to find gpu_test program."
                    let result =
                        new Oly.Core.ExternalProcess(
                            "dotnet",
                            gpuTest
                        )
                    result.Start()
                    GPU.gpuTestService <- Some result
                    result
            )

[<Struct;NoComparison>]
type TestData =
    {
        Value: float32
    }

[<Struct;NoComparison>]
type TestData2 =
    {
        Value: TestData
    }

[<Struct;NoComparison>]
type TestData3 =
    {
        Value: TestData2
    }

let compute<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> (spvCompute: SpirvModule, input: 'T array) =

    let spvFilePath = Guid.NewGuid().ToString() + ".spv"
    let spvComputeBytes = new FileStream(spvFilePath, FileMode.Create)
    SpirvModule.Serialize(spvComputeBytes, spvCompute)
    spvComputeBytes.Dispose()

    let shaderPath = spvComputeBytes.Name

    let dataKind =
        let ty = typeof<'T>
        if ty = typeof<float32> then
            "float"
        elif ty = typeof<int32> then
            "int"
        elif ty = typeof<Vector2> then
            "vec2"
        elif ty = typeof<Vector3> then
            "vec3"
        elif ty = typeof<Vector4> then
            "vec4"
        elif ty = typeof<TestData> then
            "TestData"
        elif ty = typeof<TestData2> then
            "TestData2"
        elif ty = typeof<TestData3> then
            "TestData3"
        else
            failwith $"Invalid data kind: {ty.FullName}"
    let options = Json.JsonSerializerOptions()
    options.IncludeFields <- true
    let data = Json.JsonSerializer.Serialize<_>(input, options)

    let inputJson = $"""{{ "ShaderKind": "compute", "DataKind": "{dataKind}", "Data": """ + data + " }"

    let p = GPU.GetTestService()

    let output = p.SendLine($"shader;{inputJson};{shaderPath}", Unchecked.defaultof<_>)
    if String.IsNullOrWhiteSpace(output.Errors) then
        Json.JsonSerializer.Deserialize<'T[]>(output.Output, options)
    else
        failwith output.Errors

let private shaderAux (kind: string) (spv: SpirvModule) : Bitmap =

    let spvFilePath = Guid.NewGuid().ToString() + ".spv"
    let spvBytes = new FileStream(spvFilePath, FileMode.Create)
    SpirvModule.Serialize(spvBytes, spv)
    spvBytes.Dispose()

    let shaderPath = spvBytes.Name

    let inputJson = $"""{{ "ShaderKind": "{kind}", "DataKind": "int", "Data": """ + "[123]" + " }"

    let p = GPU.GetTestService()

    let output = p.SendLine($"shader;{inputJson};{shaderPath}", Unchecked.defaultof<_>)
    if String.IsNullOrWhiteSpace(output.Errors) then
        let imageData = Json.JsonSerializer.Deserialize<int32[]>(output.Output)
        let width = 256 // hard-coded in gpu_test.olyx
        let height = 256 // hard-coded in gpu_test.olyx
        let bitmap = new Bitmap(width, height)

        let mutable i = 0
        let mutable y = 0
        while (y < height) do
            let mutable x = 0
            while (x < width) do
                // bgra -> rgba
                bitmap.SetPixel(x, y, 
                    Color.FromArgb(
                        imageData[i + 3], 
                        imageData[i + 2], 
                        imageData[i + 1], 
                        imageData[i]
                    )
                )
                i <- i + 4
                x <- x + 1
            y <- y + 1

        bitmap
    else
        failwith output.Errors

let vertex spv =
    shaderAux "vertex" spv

let fragment spv =
    shaderAux "fragment" spv