module Conformance.ContextualAnalysis.ByRefTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``ByRef should fail as it goes out of scope``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

main(): () =
    let y =
        let x = 1
        &x
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'x' as it might escape its scope at this point.",
                """
        &x
         ^
"""
            )
        ]

[<Fact>]
let ``ByRef should fail as it goes out of scope 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

main(): () =
    let _ =
        let x = 1
        &x
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'x' as it might escape its scope at this point.",
                """
        &x
         ^
"""
            )
        ]

[<Fact>]
let ``ByRef should fail as it cannot be captured inside a non-scoped lambda``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

M(f: () -> int32): int32 = f()

M2(x: byref<int32>): int32 =
    M(() -> x)

main(): () = ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("'x' is an address and cannot be captured.",
                """
    M(() -> x)
            ^
"""
            )
        ]

[<Fact>]
let ``ByRef should pass as it can be captured inside a scoped lambda``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

M(f: scoped () -> int32): int32 = f()

M2(x: byref<int32>): int32 =
    M(() -> x)

main(): () = ()
    """
    |> Oly
    |> shouldCompile
