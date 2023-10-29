module Conformance.Types.NamespaceTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Namespace with nothing in it should pass``() =
    let src =
        """
namespace Test
        """
    Oly src
    |> withCompile
    |> ignore

