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