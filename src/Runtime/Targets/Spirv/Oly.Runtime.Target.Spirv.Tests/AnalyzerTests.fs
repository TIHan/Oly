module Oly.Runtime.Target.Spirv.AnalyzerTests

open Xunit
open WorkspaceUtilities

let buildHasErrors expected src =
    buildHasErrorsWith (SpirvTarget()) expected src

[<Fact>]
let ``Should error with incorrect use of runtime array``() =
    """
#target "spirv: vertex, 1.0"

buffer: float[]
    #[uniform, descriptor_set(0), binding(0)]
    get

main(): () =
    let x = buffer
    """
    |> buildHasErrors
        [
            ("SPIR-V: Invalid use of runtime array.",
                """
    let x = buffer
            ^^^^^^
"""
            )
        ]

[<Fact>]
let ``Should error with incorrect use of GlobalInvocationId``() =
    """
#target "spirv: vertex, 1.0"

main(): () =
    let x = GlobalInvocationId.x
    """
    |> buildHasErrors
        [
            ("SPIR-V: Only available in execution model(s) 'compute'.",
                """
    let x = GlobalInvocationId.x
            ^^^^^^^^^^^^^^^^^^
"""
            )
        ]

[<Fact>]
let ``Should error with incorrect use of Position``() =
    """
#target "spirv: compute, 1.0"

main(): () =
    Position <- vec4(0)
    """
    |> buildHasErrors
        [
            ("SPIR-V: Only available in execution model(s) 'vertex'.",
                """
    Position <- vec4(0)
    ^^^^^^^^
"""
            )
        ]

[<Fact>]
let ``Should error with struct only supported in 1_3``() =
    """
#target "spirv: compute, 1.2"

struct TestData =
    public field mutable Value: float = 0

struct TestData2 =
    public field mutable Value: TestData = TestData()

main(): () =
    ()
    """
    |> buildHasErrors
        [
            ("SPIR-V: Nested structs are only available in version 1.3 or greater.",
                """
    public field mutable Value: TestData = TestData()
                         ^^^^^
"""
            )
        ]

[<Fact>]
let ``Should error with empty struct``() =
    """
#target "spirv: compute, 1.0"

struct EmptyStruct

main(): () =
    ()
    """
    |> buildHasErrors
        [
            ("SPIR-V: 'EmptyStruct' must declare at least one or more fields.",
                """
struct EmptyStruct
       ^^^^^^^^^^^
"""
            )
        ]

[<Fact>]
let ``Should error with a mutable array as a uniform``() =
    """
#target "spirv: compute, 1.0"

buffer: mutable vec2[]
    #[uniform, descriptor_set(0), binding(0)]
    get

main(): () =
    ()
    """
    |> buildHasErrors
        [
            ("SPIR-V: Uniforms cannot be a mutable array type.",
                """
buffer: mutable vec2[]
^^^^^^
"""
            )
        ]

[<Fact>]
let ``Should error with storage buffer under version 1_3``() =
    """
#target "spirv: compute, 1.2"

buffer: mutable vec2[]
    #[storage_buffer, descriptor_set(0), binding(0)]
    get

main(): () =
    ()
    """
    |> buildHasErrors
        [
            ("SPIR-V: Storage buffers are only available in version 1.3 or greater.",
                """
buffer: mutable vec2[]
^^^^^^
"""
            )
        ]

[<Fact>]
let ``Should error when not 16-byte aligned`` () =
    """
#target "spirv: compute, 1.1"

buffer: vec3[]
    #[uniform, descriptor_set(0), binding(0)]
    get

main(): () =
    ()
    """
    |> buildHasErrors
        [
            ("SPIR-V: 'vec3' must be 16-byte aligned.",
                """
buffer: vec3[]
^^^^^^
"""
            )
        ]

[<Fact>]
let ``Should error when not 16-byte aligned - 2`` () =
    """
#target "spirv: compute, 1.1"

struct S =
    public field mutable Value: vec3 = vec3(0)

main(): () =
    ()
    """
    |> buildHasErrors
        [
            ("SPIR-V: 'S' must be 16-byte aligned.",
                """
struct S =
       ^
"""
            )
        ]