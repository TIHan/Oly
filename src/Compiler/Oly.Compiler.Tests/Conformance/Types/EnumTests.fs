module Conformance.Types.EnumTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Enum with correct signature``() =
    let src =
        """
enum ABC =
    | ~^~A
    | B
    | C
        """
    src |> hasSymbolSignatureTextByCursor "constant A: ABC = 0"

[<Fact>]
let ``Enum declaration that is a uint8``() =
    let src =
        """
enum ABC = 
    inherits __oly_uint8
    | ~^~A
    | B
    | C
        """
    src |> hasSymbolSignatureTextByCursor "constant A: ABC = 0"

[<Fact>]
let ``Enum declaration should error because is not an integer``() =
    let src =
        """
enum ABC = 
    inherits __oly_object
    | A
    | B
    | C
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'ABC' can only extend integers.",
                """
    inherits __oly_object
    ^^^^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Enum use should work for intrinsic operators``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bitwise_or")]
(|)(int32, int32): int32

enum ABC = 
    | A
    | B
    | C

main(): () =
    let _result = ABC.A | ABC.B
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``Enum use should not work for intrinsic operators with the underlying type``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bitwise_or")]
(|)(int32, int32): int32

enum ABC = 
    | A
    | B
    | C

main(): () =
    let _result = ABC.A | 2
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'ABC' but is 'int32'.",
                """
    let _result = ABC.A | 2
                          ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Enum use should not work for intrinsic operators with the underlying type 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bitwise_or")]
(|)(int32, int32): int32

enum ABC = 
    | A
    | B
    | C

main(): () =
    let _result = 2 | ABC.A
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'int32' but is 'ABC'.",
                """
    let _result = 2 | ABC.A
                      ^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Enum use should not work for intrinsic operators with the underlying type 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bitwise_or")]
(|)(int32, int32): int32

enum ABC = 
    | A
    | B
    | C

main(): () =
    let _result : int32 = ABC.C | ABC.A
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'int32' but is 'ABC'.",
                """
    let _result : int32 = ABC.C | ABC.A
                          ^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Enum use should not work for intrinsic operators with other enum types of the same the underlying type``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bitwise_or")]
(|)(int32, int32): int32

enum ABC = 
    | A
    | B
    | C

enum DEF =
    | D
    | E
    | F

main(): () =
    let _result = ABC.C | DEF.D
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'ABC' but is 'DEF'.",
                """
    let _result = ABC.C | DEF.D
                          ^^^^^
"""
            )
        ]
    |> ignore