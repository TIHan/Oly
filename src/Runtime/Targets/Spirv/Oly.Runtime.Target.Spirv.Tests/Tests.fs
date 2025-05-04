module Oly.Runtime.Target.Spirv.Tests

open System
open System.Drawing
open System.Numerics
open WorkspaceUtilities
open Xunit
open Spirv.TestHelpers
open Oly.Compiler.Workspace
open Oly.Core

let build isDebug src =
    buildWith (SpirvTarget()) isDebug src

let shouldRunFragment (program: OlyProgram) =
    program.Run()
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())
    fragment(sm)

let shouldRunVertex (program: OlyProgram) =
    program.Run()
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())
    vertex(sm)

//    let defaultFragmentCode = @"
//#version 450

//layout(location = 0) in vec2 fsin_TexCoords;
//layout(location = 1) in vec4 fsin_Color;
//layout(location = 0) out vec4 fsout_Color;

//void main()
//{
//    fsout_Color = fsin_Color;
//}"

//    draw_quad(sm, glsl_to_fragment(defaultFragmentCode))

let shouldRunCompute input (program: OlyProgram) =
    program.Run()
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())
    compute(sm, input)

let OlyVertex_1_0 (x, y, expectedColor: Color) (src: string) =
    let src = $"""
#target "spirv: vertex, 1.0"

{src}
"""
    let output = build true src |> shouldRunVertex
    Assert.Equal<Color>(expectedColor, output.GetPixel(x, y))
    let output = build false src |> shouldRunVertex
    Assert.Equal<Color>(expectedColor, output.GetPixel(x, y))

let OlyFragment_1_0 (x, y, expectedColor: Color) (src: string) =
    let src = $"""
#target "spirv: fragment, 1.0"

{src}
"""
    let output = build true src |> shouldRunFragment
    Assert.Equal<Color>(expectedColor, output.GetPixel(x, y))
    let output = build false src |> shouldRunFragment
    Assert.Equal<Color>(expectedColor, output.GetPixel(x, y))

let OlyCompute_1_0<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> (input: 'T array) (expectedOutput: 'T array) (src: string) =
    let src = $"""
#target "spirv: compute, 1.0"

{src}
"""
    let output = build true src |> shouldRunCompute input
    Assert.Equal<'T>(expectedOutput, output)
    let output = build false src |> shouldRunCompute input
    Assert.Equal<'T>(expectedOutput, output)

let OlyCompute_1_3<'T when 'T : unmanaged and 'T : struct and 'T :> ValueType and 'T : (new : unit-> 'T)> (input: 'T array) (expectedOutput: 'T array) (src: string) =
    let src = $"""
#target "spirv: compute, 1.3"

{src}
"""
    let output = build true src |> shouldRunCompute input
    Assert.Equal<'T>(expectedOutput, output)
    let output = build false src |> shouldRunCompute input
    Assert.Equal<'T>(expectedOutput, output)

[<Fact>]
let ``Blank vertex shader`` () =
//#version 450

//layout(location = 0) out vec2 fsin_TexCoords;
//layout(location = 1) out vec4 fsin_Color;

//void main()
//{
//}
    let src =
        """
position: vec2
    #[location(0)]
    get

texCoords: vec2
    #[location(1)]
    get

color: vec4
    #[location(2)]
    get

outTexCoords: vec2
    #[location(0)]
    set

outColor: vec4
    #[location(1)]
    set

main(): () =
    ()
        """
    OlyVertex_1_0 (64, 64, Color.FromArgb(255, 0, 0, 0)) src

[<Fact>]
let ``Blank vertex shader but has output`` () =
//#version 450

//layout(location = 0) out vec2 fsin_TexCoords;
//layout(location = 1) out vec4 fsin_Color;

//void main()
//{
//    gl_Position = vec4(1);
//}
    let src =
        """
position: vec2
    #[location(0)]
    get

texCoords: vec2
    #[location(1)]
    get

color: vec4
    #[location(2)]
    get

outTexCoords: vec2
    #[location(0)]
    set

outColor: vec4
    #[location(1)]
    set

main(): () =
    Position <- vec4(1)
        """
    OlyVertex_1_0 (64, 64, Color.FromArgb(255, 0, 0, 0)) src

[<Fact>]
let ``Basic vertex shader`` () =
//#version 450

//layout(location = 0) in vec2 Position;
//layout(location = 1) in vec2 TexCoords;
//layout(location = 2) in vec4 Color;

//layout(location = 0) out vec2 fsin_TexCoords;
//layout(location = 1) out vec4 fsin_Color;

//void main()
//{
//    gl_Position = vec4(Position, 0, 1);
//    fsin_TexCoords = TexCoords;
//    fsin_Color = Color;
//}
    let src =
        """
position: vec2
    #[location(0)]
    get

texCoords: vec2
    #[location(1)]
    get

color: vec4
    #[location(2)]
    get

outTexCoords: vec2
    #[location(0)]
    set

outColor: vec4
    #[location(1)]
    set

main(): () =
    Position <- vec4(position, 0, 1)
    outTexCoords <- texCoords
    outColor <- color
        """
    OlyVertex_1_0 (64, 64, Color.FromArgb(255, 0, 64, 191)) src

[<Fact>]
let ``Blank fragment shader`` () =
//#version 450

//void main()
//{
//}
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    ()
        """
    OlyFragment_1_0 (64, 64, Color.FromArgb(255, 0, 0, 0)) src

[<Fact>]
let ``Basic fragment shader`` () =
//#version 450

//layout(location = 0) in vec2 fsin_TexCoords;
//layout(location = 1) in vec4 fsin_Color;
//layout(location = 0) out vec4 fsout_Color;

//void main()
//{
//    fsout_Color = fsin_Color;
//}
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    outColor <- color
        """
    OlyFragment_1_0 (64, 64, Color.FromArgb(255, 0, 64, 191)) src

[<Fact>]
let ``Should create a new value and use it`` () =
    let src =
        """
position: vec2
    #[location(0)]
    get

texCoords: vec2
    #[location(1)]
    get

color: vec4
    #[location(2)]
    get

outTexCoords: vec2
    #[location(0)]
    set

outColor: vec4
    #[location(1)]
    set

main(): () =
    let position = position
    Position <- vec4(position, 0, 1)
    outTexCoords <- texCoords
    outColor <- color
        """
    OlyVertex_1_0 (64, 64, Color.FromArgb(255, 0, 64, 191)) src

[<Fact>]
let ``Should mutate a new value and use it`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(255, 255, 255, 255)) src // Should show white

[<Fact>]
let ``Should use if/else`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    if (color.x == 1)
        color <- vec4(0.5)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(127, 127, 127, 127)) src // should show grey

[<Fact>]
let ``Should use if/else 2`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    if (color.x == 1)
        color <- vec4(0.5)
    else
        color <- vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(127, 127, 127, 127)) src // should show grey

[<Fact>]
let ``Should use if/else 3`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    if (color.x == 1.1)
        color <- vec4(0.5)
    else
        color <- vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(0, 0, 0, 0)) src // should show black

[<Fact>]
let ``Should use if/else 4`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.x == 1)
            vec4(0.5)
        else
            color
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(127, 127, 127, 127)) src // should show grey

[<Fact>]
let ``Should use if/else 5`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.x == 1)
            if (color.x == 1)
                if (color.x == 1)
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
    OlyFragment_1_0 (0, 0, Color.FromArgb(127, 127, 127, 127)) src // should show grey

[<Fact>]
let ``Should use if/else 6`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.x == 1.1)
            vec4(0.5)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(0, 0, 0, 0)) src // should show black

[<Fact>]
let ``Should use if/else 7`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.x == 1)
            color <- vec4(0)
            vec4(0.5)
        else
            color <- vec4(0)
            vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(127, 127, 127, 127)) src // should show grey

[<Fact>]
let ``Should use if/else 8`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.x == 1)
            if (color.x == 1)
                color <- vec4(1)
            vec4(0.5)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(127, 127, 127, 127)) src // should show grey

[<Fact>]
let ``Should use if/else 9`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(0)
    color <-
        if (color.x == 1)
            if (color.x == 1)
                color <- vec4(1)
            vec4(0.5)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(0, 0, 0, 0)) src // should show black

[<Fact>]
let ``Should use if/else 10`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.x == 1)
            if (color.x == 1)
                color <- vec4(1)
            if (color.x == 1)
                vec4(1)
            else
                vec4(0)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(255, 255, 255, 255)) src // should show white

[<Fact>]
let ``Should use if/else 11`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.x == 1)
            if (color.x == 1)
                color <- vec4(1)
            vec4(color.x)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(255, 255, 255, 255)) src // should show white

[<Fact>]
let ``Should use if/else 12`` () =
    let src =
        """
texCoords: vec2
    #[location(0)]
    get

color: vec4    
    #[location(1)] 
    get

outColor: vec4 
    #[location(0)] 
    set

main(): () =
    let mutable color = color
    color <- vec4(1)
    color <-
        if (color.x == 1)
            if (color.x == 1)
                color <- vec4(0.5)
            vec4(color.x)
        else
            vec4(0)
    outColor <- color
        """
    OlyFragment_1_0 (0, 0, Color.FromArgb(127, 127, 127, 127)) src // should show grey

[<Fact>]
let ``Blank compute shader`` () =
    let src =
        """
main(): () =
    ()
        """
    OlyCompute_1_0 [|0f|] [|0f|] src

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
buffer: mutable float[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    let index = GlobalInvocationId.x
    buffer[index] <- 123
        """
    OlyCompute_1_3 [|0f;0f;0f;0f|] [|123f;123f;123f;123f|] src

[<Fact>]
let ``Basic compute shader 2 - verify use of local creation`` () =
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
buffer: mutable float[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    let abc = GlobalInvocationId
    let index = abc.x
    buffer[index] <- 123
        """
    OlyCompute_1_3 [|0f;0f;0f;0f|] [|123f;123f;123f;123f|] src

[<Fact>]
let ``Basic compute shader 3 - verify use of function GetValue`` () =
    let src =
        """
buffer: mutable float[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

GetValue(x: float): float =
    x

main(): () =
    buffer[1] <- GetValue(123)
        """
    OlyCompute_1_3 [|0f;0f;0f;0f|] [|0f;123f;0f;0f|] src

[<Fact>]
let ``Basic compute shader 4 - verify use of int32`` () =
    let src =
        """
buffer: mutable int[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    buffer[1] <- 123
        """
    OlyCompute_1_3 [|0;0;0;0|] [|0;123;0;0|] src

[<Fact>]
let ``Basic compute shader 5 - verify struct`` () =
    let src =
        """
struct TestData =
    public field mutable Value: float = 0

buffer: mutable TestData[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    let mutable v = vec2(0)
    v.x <- 123
    v.y <- 456
    let mutable tdata = TestData()
    tdata.Value <- v.x
    buffer[1] <- tdata
        """
    src
    |> OlyCompute_1_3
        [|{ TestData.Value = 0f };{ Value =   456f };{ Value = 0f }|] 
        [|{ Value = 0f };{ Value = 123f };{ Value = 0f }|]

[<Fact>]
let ``Basic compute shader 6 - verify nested struct`` () =
    let src =
        """
struct TestData =
    public field mutable Value: float = 0

struct TestData2 =
    public field mutable Value: TestData = TestData()

buffer: mutable TestData2[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    let mutable tdata2 = TestData2()
    tdata2.Value.Value <- 123
    buffer[1] <- tdata2
        """
    src
    |> OlyCompute_1_3
        [|{ TestData2.Value = { TestData.Value = 0f } };{ Value = { Value =   0f } };{ Value = { Value = 0f } }|] 
        [|{ Value = { Value = 0f } };{ Value = { Value = 123f } };{ Value = { Value = 0f } }|]

[<Fact>]
let ``Basic compute shader 7 - verify doubly nested structs`` () =
    let src =
        """
struct TestData =
    public field mutable Value: float = 0

struct TestData2 =
    public field mutable Value: TestData = TestData()

struct TestData3 =
    public field mutable Value: TestData2 = TestData2()

buffer: mutable TestData3[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    let mutable tdata3 = TestData3()
    tdata3.Value.Value.Value <- 123
    buffer[1] <- tdata3
        """
    src
    |> OlyCompute_1_3 
        [|{ Value = { Value = { TestData.Value = 0f } } };{ Value = { Value = { Value =   0f } } };{ Value = { Value = { Value = 0f } } }|] 
        [|{ Value = { Value = { Value = 0f } } };{ Value = { Value = { Value = 123f } } };{ Value = { Value = { Value = 0f } } }|]

[<Fact>]
let ``Basic compute shader 8 - verify vec2`` () =
    let src =
        """
buffer: mutable vec2[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    let mutable v = vec2(0)
    v.x <- 123
    v.y <- 456
    buffer[0] <- v 
        """
    src
    |> OlyCompute_1_3
        [|Vector2(5.0f, 5.0f)|] 
        [|Vector2(123.0f, 456.0f)|]

[<Fact>]
let ``Basic compute shader 9 - verify vec3`` () =
    let src =
        """
struct new_vec4 =
    public field mutable xyz: vec3 = vec3(0)
    public field mutable w: float = 0

buffer: mutable new_vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    let mutable v = new_vec4()
    v.xyz <- vec3(123, 456, 789)
    v.w <- 999
    buffer[0] <- v
    buffer[1] <- v
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f);Vector4(0.0f)|] 
        [|Vector4(123.0f, 456.0f, 789.0f, 999.0f);Vector4(123.0f, 456.0f, 789.0f, 999.0f)|]

[<Fact>]
let ``Basic compute shader 9 - verify vec3 - 2`` () =
    let src =
        """
struct new_vec4 =
    public field mutable w: float = 0
    public field mutable xyz: vec3 = vec3(0)

buffer: mutable new_vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    let mutable v = new_vec4()
    v.xyz <- vec3(123, 456, 789)
    v.w <- 999
    buffer[0] <- v
    buffer[1] <- v
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f);Vector4(0.0f)|] 
        [|Vector4(999.0f, 123.0f, 456.0f, 789.0f);Vector4(999.0f, 123.0f, 456.0f, 789.0f)|]

[<Fact>]
let ``Basic compute shader 10 - verify vec4`` () =
    let src =
        """
buffer: mutable vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    buffer[0] <- vec4(123, 456, 789, 999)
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f)|] 
        [|Vector4(123.0f, 456.0f, 789.0f, 999.0f)|]

[<Fact>]
let ``Basic compute shader 11 - verify struct ctor`` () =
    let src =
        """
buffer: mutable vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

struct Doot =
    public field mutable X: float = 999

main(): () =
    let mutable doot = Doot()
    buffer[0] <- vec4(123, 456, 789, doot.X)
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f)|] 
        [|Vector4(123.0f, 456.0f, 789.0f, 999.0f)|]

[<Fact>]
let ``Basic compute shader 12 - verify struct instance method`` () =
    let src =
        """
buffer: mutable vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

struct Doot =
    public field mutable X: float = 0

    mutable SetX(x: float): () =
        this.X <- x

main(): () =
    let mutable doot = Doot()
    doot.SetX(999)
    buffer[0] <- vec4(123, 456, 789, doot.X)
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f)|] 
        [|Vector4(123.0f, 456.0f, 789.0f, 999.0f)|]

[<Fact>]
let ``Basic compute shader 12 - verify struct instance method - 2``() =
    let src =
        """
buffer: mutable vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

struct Doot =
    public field mutable X: float = 0

    mutable M(): () =
        this.X <- 999

main(): () =
    let mutable doot = Doot()
    doot.M()
    buffer[0] <- vec4(123, 456, 789, doot.X)
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f)|] 
        [|Vector4(123.0f, 456.0f, 789.0f, 999.0f)|]

[<Fact>]
let ``Basic compute shader 12 - verify struct instance method - 3``() =
    let src =
        """
buffer: mutable vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

struct Doot =
    public field mutable X: float = 0

    mutable M(): () =
        this.X <- 999

    #[inline(never)]
    mutable M2(): () =
        this.M()

main(): () =
    let mutable doot = Doot()
    doot.M2()
    buffer[0] <- vec4(123, 456, 789, doot.X)
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f)|] 
        [|Vector4(123.0f, 456.0f, 789.0f, 999.0f)|]

[<Fact>]
let ``Basic compute shader 12 - verify struct instance method - 4``() =
    let src =
        """
buffer: mutable vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

struct NestedDoot =
    public field mutable X: float = 0

    mutable M(): () =
        this.X <- 999

struct Doot =
    public field mutable Nested: NestedDoot = NestedDoot()

main(): () =
    let mutable doot = Doot()
    doot.Nested.M()
    buffer[0] <- vec4(123, 456, 789, doot.Nested.X)
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f)|] 
        [|Vector4(123.0f, 456.0f, 789.0f, 999.0f)|]

[<Fact>]
let ``Basic compute shader 13 - verify modifying argument does not impact the original local that was passed``() =
    let src =
        """
buffer: mutable vec4[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

test(mutable x: float): () = x <- 555

main(): () =
    let mutable y: float = 999
    test(y)
    buffer[0] <- vec4(123, 456, 789, y)
        """
    src
    |> OlyCompute_1_3
        [|Vector4(0.0f)|] 
        [|Vector4(123.0f, 456.0f, 789.0f, 999.0f)|]