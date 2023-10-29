module Conformance.ContextualAnalysis.UnmanagedAllocationOnlyTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Simple unmanaged(allocation_only) should compile``() =
    """
#[unmanaged(allocation_only)]
main(): () =
    ()
    """
    |> Oly
    |> shouldCompile

[<Fact>]
let ``Simple unmanaged(allocation_only) should error``() =
    """
class C

#[unmanaged(allocation_only)]
main(): () =
    let _ = C()
    ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let _ = C()
            ^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
box(o: obj): () = ()

#[unmanaged(allocation_only)]
main(): () =
    box(1234)
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    box(1234)
        ^^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
box(o: obj): () = ()

#[unmanaged(allocation_only)]
main(): () =
    let x: obj = 1234
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let x: obj = 1234
                 ^^^^
"""
            )
        ]


[<Fact>]
let ``Simple unmanaged(allocation_only) should error as the local type definition is for managed allocation``() =
    """
#[intrinsic("int32")]
alias int32

#[unmanaged(allocation_only)]
main(): () =
    class C =
        field X: int32 = 0
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    class C =
          ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as a lambda may be a managed allocation``() =
    """
#[unmanaged(allocation_only)]
main(): () =
    let f = () -> ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let f = () -> ()
        ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as a lambda may be a managed allocation 2``() =
    """
#[unmanaged(allocation_only)]
main(): () =
    let mutable f = () -> ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let mutable f = () -> ()
                    ^^^^^^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as a lambda may be a managed allocation 3``() =
    """
#[unmanaged(allocation_only)]
main(): () =
    let f() = ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let f() = ()
        ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should compile as we have a static local function``() =
    """
#[unmanaged(allocation_only)]
main(): () =
    #[unmanaged(allocation_only)]
    static let f() = ()
    """
    |> Oly
    |> shouldCompile

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as we are partially applying a static local function``() =
    """
#[unmanaged(allocation_only)]
main(): () =
    #[unmanaged(allocation_only)]
    static let f() = ()
    let _ = f
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let _ = f
            ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as the type parameters must be constrained to 'unmanaged'``() =
    """
#[unmanaged(allocation_only)]
M<T>(): () =
    ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
M<T>(): () =
  ^
"""
            )
        ]


[<Fact>]
let ``Simple unmanaged(allocation_only) should error as the type parameters must be constrained to 'unmanaged' 2``() =
    """
module MD<T> =
    #[unmanaged(allocation_only)]
    M<U>(): () where U: unmanaged =
        ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
module MD<T> =
          ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error a string literal may be considered an allocation``() =
    """
#[unmanaged(allocation_only)]
M(): () =
    let _ = "a string"
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let _ = "a string"
            ^^^^^^^^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as tuples are not allowed``() =
    // TODO: Eventually, we will make tuples structs so this test will become a positive instead of a negative.
    """
#[intrinsic("int32")]
alias int32

#[unmanaged(allocation_only)]
main(): () =
    let _ = (1, 2)
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let _ = (1, 2)
            ^^^^^^
"""
            )
        ]


[<Fact>]
let ``Simple unmanaged(allocation_only) should error as arrays are not allowed``() =
    """
#[intrinsic("int32")]
alias int32

#[unmanaged(allocation_only)]
main(): () =
    let _ = [1;2;3]
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let _ = [1;2;3]
            ^^^^^^^
"""
            )
        ]
