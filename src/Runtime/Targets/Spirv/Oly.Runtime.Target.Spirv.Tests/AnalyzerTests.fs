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
