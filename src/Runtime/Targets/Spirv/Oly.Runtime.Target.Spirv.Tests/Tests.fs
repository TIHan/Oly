module Oly.Runtime.Target.Spirv.Tests

open System
open Utilities
open TestUtilities
open Xunit
open Spirv.TestHelpers
open Oly.Runtime.Target.Spirv

let PreludeSrc =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[import("spirv", "__oly_spirv_", "vec2")]
struct vec2 =

    public field mutable X: float32
    public field mutable Y: float32

#[import("spirv", "__oly_spirv_", "vec3")]
struct vec3 =

    public field mutable X: float32
    public field mutable Y: float32
    public field mutable Z: float32

#[import("spirv", "__oly_spirv_", "vec4")]
struct vec4 =

    public field mutable X: float32
    public field mutable Y: float32
    public field mutable Z: float32
    public field mutable W: float32

    new(float32)

    new(v: vec2, z: float32, w: float32)
    """

let OlyVertex (src: string) =
    $"""
{PreludeSrc}

{src}
    """
    |> Oly

[<Fact>]
let ``Blank vertex shader`` () =
//#version 450

//void main()
//{
//}
    let src =
        """
main(): () =
    ()
        """
    OlyVertex src
    |> withCompile
    |> shouldRunWithExpectedOutput String.Empty

[<Fact>]
let ``Blank vertex shader but has output`` () =
//#version 450

//void main()
//{
//    gl_Position = vec4(1);
//}
    let src =
        """
main(): vec4 =
    vec4(1)
        """
    OlyVertex src
    |> withCompile
    |> shouldRunWithExpectedOutput String.Empty

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
main(position: vec2, color: vec4): (position: vec4, color: vec4) =
    (vec4(position, 0, 1), color)
        """
    OlyVertex src
    |> withCompile
    |> shouldRunWithExpectedOutput String.Empty

