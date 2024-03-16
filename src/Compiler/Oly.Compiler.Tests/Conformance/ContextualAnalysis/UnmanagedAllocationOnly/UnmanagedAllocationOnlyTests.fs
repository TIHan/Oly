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
let ``Simple unmanaged(allocation_only) should compile with setting contents of an address``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[unmanaged(allocation_only)]
M(x: byref<int32>): () =
    x <- 123
    """
    |> Oly
    |> shouldCompile

[<Fact>]
let ``Simple unmanaged(allocation_only) should compile with the constraint not struct``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
box(o: obj): () = ()

#[unmanaged(allocation_only)]
M<T>(x: T): () where T: not struct =
    box(x)
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
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
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
main(): () =
    let x: obj = 1234
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    let x: obj = 1234
                 ^^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
main(): () =
    let x = 1234: obj
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    let x = 1234: obj
            ^^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 4``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
main(): () =
    let x: obj[] = [1234]
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let x: obj[] = [1234]
                   ^^^^^^
"""
            )
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    let x: obj[] = [1234]
                    ^^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 5``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
main(): () =
    let x: (int32, obj) = (1234, 678)
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Managed allocations are not allowed.",
                """
    let x: (int32, obj) = (1234, 678)
                          ^^^^^^^^^^^
"""
            )
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    let x: (int32, obj) = (1234, 678)
                                 ^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 6``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

struct S =
    public field mutable X: obj

    #[unmanaged(allocation_only)]
    new() = { X = null }

#[unmanaged(allocation_only)]
main(): () =
    let mutable s = S()
    s.X <- 123
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    s.X <- 123
           ^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 7``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

struct S =
    X: obj get, set

    #[unmanaged(allocation_only)]
    new() = { X = null }

#[unmanaged(allocation_only)]
main(): () =
    let mutable s = S()
    s.X <- 123
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    s.X <- 123
           ^^^
"""
            )
        ]
        
[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 8``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
main(): () =
    let mutable x: obj = null
    x <- 123
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    x <- 123
         ^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 9``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[unmanaged(allocation_only)]
M(x: byref<obj>): () =
    x <- 123
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    x <- 123
         ^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 10``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
box(o: obj): () = ()

#[unmanaged(allocation_only)]
M<T>(x: T): () =
    box(x)
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    box(x)
        ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 11``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
box(o: obj): () = ()

#[unmanaged(allocation_only)]
M<T>(x: T): () where T: struct =
    box(x)
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    box(x)
        ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 12``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
box(o: obj): () = ()

#[unmanaged(allocation_only)]
M<T>(x: T): () where T: unmanaged =
    box(x)
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    box(x)
        ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 13``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
M(): obj = 1234
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
M(): obj = 1234
           ^^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 14``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
M(x: int32): obj = x
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
M(x: int32): obj = x
                   ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 15``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
M(x: int32): obj =
    try
        x
    finally
        let _ = x
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
        x
        ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 16``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
M(x: int32): obj =
    let y = x
    y
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    y
    ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 17``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
M(x: int32): obj =
    ()
    x
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
    x
    ^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 18``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
M(x: int32): obj =
    if (true)
        x
    else
        1234
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
        x
        ^
"""
            )
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
        1234
        ^^^^
"""
            )
        ]

[<Fact>]
let ``Simple unmanaged(allocation_only) should error as it will cause boxing 19``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

#[unmanaged(allocation_only)]
M(x: int32): obj =
    match (x)
    | 1 =>
        567
    | _ =>
        x
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
        567
        ^^^
"""
            )
            ("Expression can potentially box and cause a managed allocation. Managed allocations are not allowed.",
                """
        x
        ^
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
