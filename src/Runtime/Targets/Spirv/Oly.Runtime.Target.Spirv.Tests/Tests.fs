[<Xunit.Collection("Sequential")>]
module Oly.Runtime.Target.Spirv.Tests

open System
open Utilities
open TestUtilities
open Xunit
open Spirv.TestHelpers
open Oly.Compiler.Syntax
open Oly.Compiler.Text
open Oly.Runtime.Target.Spirv
open Oly.Compiler.Workspace
open Oly.Core

let OlyShaderPrelude isDebug (src: string) =
    let preludeDirName = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    let preludeDir = OlyPath.CreateAbsolute(preludeDirName)
    let preludeDir =
        if preludeDir.IsDirectory then
            preludeDir
        else
            OlyPath.Create(preludeDirName + "/")

    let workspace = OlyWorkspace.Create([SpirvTarget()])

    let documentPath = OlyPath.Combine(preludeDir, "test.olyx")
    let srcText = OlySourceText.Create(src)

    let projConfigPath = OlyPath.Combine(preludeDir, "test.json")
    use projConfigMs = new IO.MemoryStream(Text.Encoding.Default.GetBytes("""{ "configurations": [{ "name": "Debug", "defines": ["DEBUG"], "debuggable": true }, { "name": "Release", "defines": ["RELEASE"], "debuggable": false }] }"""))

    let configPath = OlyPath.Create("state.json")
    use configMs = 
        if isDebug then
            new IO.MemoryStream(Text.Encoding.Default.GetBytes("""{ "activeConfiguration": "Debug" }"""))
        else
            new IO.MemoryStream(Text.Encoding.Default.GetBytes("""{ "activeConfiguration": "Release" }"""))

    let textManager = OlySourceTextManager.Empty.Set(documentPath, srcText, 1)
    let resourceSnapshot = OlyWorkspaceResourceSnapshot.Create(configPath).WithTextEditors(textManager)

    let resourceSnapshot = resourceSnapshot.SetResourceAsCopy(projConfigPath, projConfigMs)
    let resourceSnapshot = resourceSnapshot.SetResourceAsCopy(configPath, configMs)

    // Set up prelude
    let preludePath = OlyPath.Combine(preludeDir, "spirv_prelude.olyx")
    let resourceSnapshot = resourceSnapshot.SetResourceAsCopy(preludePath)

    let _doc = workspace.UpdateDocumentAsync(resourceSnapshot, documentPath, srcText, Threading.CancellationToken.None).Result[0]

    match workspace.BuildProjectAsync(resourceSnapshot, documentPath, Threading.CancellationToken.None).Result with
    | Error(diags) ->
        let resourceSnapshot = resourceSnapshot.RemoveResource(documentPath)
        workspace.RemoveProject(resourceSnapshot, documentPath, Threading.CancellationToken.None)
        raise(Exception(OlyDiagnostic.PrepareForOutput(diags, Threading.CancellationToken.None)))
    | Ok(program) ->
        let resourceSnapshot = resourceSnapshot.RemoveResource(documentPath)
        workspace.RemoveProject(resourceSnapshot, documentPath, Threading.CancellationToken.None)
        program

let shouldRunFragment (program: OlyProgram) =
    program.Run()
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())
    let defaultVertexCode = @"
#version 450

layout(location = 0) in vec2 Position;
layout(location = 1) in vec4 Color;

layout(location = 0) out vec4 fsin_Color;

void main()
{
    gl_Position = vec4(Position, 0, 1);
    fsin_Color = Color;
}"

    draw_quad(glsl_to_vertex(defaultVertexCode), sm)

let shouldRunVertex (program: OlyProgram) =
    program.Run()
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())

    let defaultFragmentCode = @"
#version 450

layout(location = 0) in vec4 fsin_Color;
layout(location = 0) out vec4 fsout_Color;

void main()
{
    fsout_Color = fsin_Color;
}"

    draw_quad(sm, glsl_to_fragment(defaultFragmentCode))

let shouldRunCompute input (program: OlyProgram) =
    program.Run()
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())

    compute(sm, input)

let OlyVertex (src: string) =
    let src = $"""
#target "spirv: vertex, 1.0"

{src}
"""
    OlyShaderPrelude true src |> shouldRunVertex
    OlyShaderPrelude false src |> shouldRunVertex

let OlyFragment (src: string) =
    let src = $"""
#target "spirv: fragment, 1.0"

{src}
"""
    OlyShaderPrelude true src |> shouldRunFragment
    OlyShaderPrelude false src |> shouldRunFragment

let OlyCompute<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> (input: 'T array) (expectedOutput: 'T array) (src: string) =
    let src = $"""
#target "spirv: compute, 1.3"

{src}
"""
    let output = OlyShaderPrelude true src |> shouldRunCompute input
    Assert.Equal<'T>(expectedOutput, output)
    let output = OlyShaderPrelude false src |> shouldRunCompute input
    Assert.Equal<'T>(expectedOutput, output)

[<Fact>]
let ``Blank vertex shader`` () =
//#version 450

//layout(location = 0) out vec4 fsin_Color;

//void main()
//{
//}
    let src =
        """
outColor: vec4
    #[location(0)]
    set

main(): () =
    ()
        """
    OlyVertex src

[<Fact>]
let ``Blank vertex shader but has output`` () =
//#version 450

//layout(location = 0) out vec4 fsin_Color;

//void main()
//{
//    gl_Position = vec4(1);
//}
    let src =
        """
outColor: vec4
    #[location(0)]
    set

main(): () =
    BuiltIn.Position <- vec4(1)
        """
    OlyVertex src

[<Fact>]
let ``Basic vertex shader`` () =
//#version 450

//layout(location = 0) in vec2 Position;
//layout(location = 1) in vec4 Color;

//layout(location = 0) out vec4 fsin_Color;

//void main()
//{
//    gl_Position = vec4(Position, 0, 1);
//    fsin_Color = Color;
//}
    let src =
        """
position: vec2
    #[location(0)]
    get

color: vec4
    #[location(1)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    BuiltIn.Position <- vec4(position, 0, 1)
    outColor <- color
        """
    OlyVertex src

[<Fact>]
let ``Blank fragment shader`` () =
//#version 450

//void main()
//{
//}
    let src =
        """
main(): () =
    ()
        """
    OlyFragment src

[<Fact>]
let ``Basic fragment shader`` () =
//#version 450

//layout(location = 0) in vec4 fsin_Color;
//layout(location = 0) out vec4 fsout_Color;

//void main()
//{
//    fsout_Color = fsin_Color;
//}
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    outColor <- color
        """
    OlyFragment src

[<Fact>]
let ``Should create a new value and use it`` () =
    let src =
        """
position: vec2
    #[location(0)]
    get

color: vec4
    #[location(1)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let position = position
    BuiltIn.Position <- vec4(position, 0, 1)
    outColor <- color
        """
    OlyVertex src

[<Fact>]
let ``Should mutate a new value and use it`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    outColor <- color
        """
    OlyFragment src // Should show white

[<Fact>]
let ``Should use if/else`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    if (color.X == 1)
        color <- vec4(0.5)
    outColor <- color
        """
    OlyFragment src // should show grey

[<Fact>]
let ``Should use if/else 2`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    if (color.X == 1)
        color <- vec4(0.5)
    else
        color <- vec4(0)
    outColor <- color
        """
    OlyFragment src // should show grey

[<Fact>]
let ``Should use if/else 3`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    if (color.X == 1.1)
        color <- vec4(0.5)
    else
        color <- vec4(0)
    outColor <- color
        """
    OlyFragment src // should show black

[<Fact>]
let ``Should use if/else 4`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.X == 1)
            vec4(0.5)
        else
            color
    outColor <- color
        """
    OlyFragment src // should show grey

[<Fact>]
let ``Should use if/else 5`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.X == 1)
            if (color.X == 1)
                if (color.X == 1)
                    color <- vec4(1)
                    vec4(0.5)
                else
                    vec4(0)
            else
                vec4(1)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment src // should show grey

[<Fact>]
let ``Should use if/else 6`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.X == 1.1)
            vec4(0.5)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment src // should show black

[<Fact>]
let ``Should use if/else 7`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.X == 1)
            color <- vec4(0)
            vec4(0.5)
        else
            color <- vec4(0)
            vec4(0)
    outColor <- color
        """
    OlyFragment src // should show grey

[<Fact>]
let ``Should use if/else 8`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.X == 1)
            if (color.X == 1)
                color <- vec4(1)
            vec4(0.5)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment src // should show grey

[<Fact>]
let ``Should use if/else 9`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(0)
    color <-
        if (color.X == 1)
            if (color.X == 1)
                color <- vec4(1)
            vec4(0.5)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment src // should show black

[<Fact>]
let ``Should use if/else 10`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.X == 1)
            if (color.X == 1)
                color <- vec4(1)
            if (color.X == 1)
                vec4(1)
            else
                vec4(0)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment src // should show white

[<Fact>]
let ``Should use if/else 11`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.X == 1)
            if (color.X == 1)
                color <- vec4(1)
            vec4(color.X)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment src // should show white

[<Fact>]
let ``Should use if/else 12`` () =
    let src =
        """
color: vec4
    #[location(0)]
    get

outColor: vec4
    #[location(0)]
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.X == 1)
            if (color.X == 1)
                color <- vec4(0.5)
            vec4(color.X)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment src // should show grey

[<Fact>]
let ``Blank compute shader`` () =
    let src =
        """
main(): () =
    ()
        """
    OlyCompute [|0f|] [|0f|] src

[<Fact>]
let ``Basic compute shader`` () =
//#version 450

//layout(set = 0, binding = 0) buffer Buffer
//{
//    float data[];
//};

//void main()
//{
//    uint index = gl_GlobalInvocationID.x;
//    data[index] = 123;
//}
    let src =
        """
buffer: mutable float32[]
    #[uniform]
    #[descriptor_set(0)]
    #[binding(0)]
    get

main(): () =
    let index = BuiltIn.GlobalInvocationId.X
    buffer[int32(index)] <- 123
        """
    OlyCompute [|0f;0f;0f;0f|] [|123f;123f;123f;123f|] src

[<Fact>]
let ``Basic compute shader 2`` () =
//#version 450

//layout(set = 0, binding = 0) buffer Buffer
//{
//    float data[];
//};

//void main()
//{
//    uvec3 abc = gl_GlobalInvocationID;
//    uint index = abc.x;
//    data[index] = 123;
//}
    let src =
        """
buffer: mutable float32[]
    #[uniform]
    #[descriptor_set(0)]
    #[binding(0)]
    get

main(): () =
    let abc = BuiltIn.GlobalInvocationId
    let index = abc.X
    buffer[int32(index)] <- 123
        """
    OlyCompute [|0f;0f;0f;0f|] [|123f;123f;123f;123f|] src

[<Fact>]
let ``Basic compute shader 3`` () =
    let src =
        """
buffer: mutable float32[]
    #[uniform]
    #[descriptor_set(0)]
    #[binding(0)]
    get

GetValue(x: float32): float32 =
    x

main(): () =
    buffer[1] <- GetValue(123)
        """
    OlyCompute [|0f;0f;0f;0f|] [|0f;123f;0f;0f|] src

[<Fact>]
let ``Basic compute shader 4`` () =
    let src =
        """
buffer: mutable int32[]
    #[uniform]
    #[descriptor_set(0)]
    #[binding(0)]
    get

main(): () =
    buffer[1] <- 123
        """
    OlyCompute [|0;0;0;0|] [|0;123;0;0|] src

[<Struct;NoComparison>]
type TestData =
    {
        Value: float32
    }

[<Fact>]
let ``Basic compute shader 5`` () =
    let src =
        """
struct TestData =
    public field mutable Value: float32 = 0

buffer: mutable TestData[]
    #[storage_buffer]
    #[descriptor_set(0)]
    #[binding(0)]
    get

main(): () =
    let mutable tdata = TestData()
    tdata.Value <- 123
    buffer[1] <- tdata
        """
    src
    |> OlyCompute 
        [|{ Value = 0f };{ Value =   0f };{ Value = 0f }|] 
        [|{ Value = 0f };{ Value = 123f };{ Value = 0f }|]

[<Struct;NoComparison>]
type TestData2 =
    {
        Value: TestData
    }

[<Fact>]
let ``Basic compute shader 6`` () =
    let src =
        """
struct TestData =
    public field mutable Value: float32 = 0

struct TestData2 =
    public field mutable Value: TestData = TestData()

buffer: mutable TestData2[]
    #[storage_buffer]
    #[descriptor_set(0)]
    #[binding(0)]
    get

main(): () =
    let mutable tdata2 = TestData2()
    tdata2.Value.Value <- 123
    buffer[1] <- tdata2
        """
    src
    |> OlyCompute 
        [|{ Value = { Value = 0f } };{ Value = { Value =   0f } };{ Value = { Value = 0f } }|] 
        [|{ Value = { Value = 0f } };{ Value = { Value = 123f } };{ Value = { Value = 0f } }|]