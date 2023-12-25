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