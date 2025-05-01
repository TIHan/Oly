module Oly.Runtime.Target.Spirv.AnalyzerTests

open Xunit
open WorkspaceUtilities

let buildHasErrors expected src =
    buildHasErrorsWith (SpirvTarget()) expected src

[<Fact>]
let ``Should error with incorrect use of runtime array``() =
    """
#target "spirv: vertex, 1.0"

buffer: mutable float[]
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
