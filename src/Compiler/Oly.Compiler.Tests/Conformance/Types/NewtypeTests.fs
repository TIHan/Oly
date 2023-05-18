module Conformance.Types.NewtypeTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Newtype should pass``() =
    let src =
        """
newtype A =
    field Value: __oly_int32
        """
    Oly src
    |> shouldCompile
    |> ignore