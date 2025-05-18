module Conformance.Types.ArrayTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Fixed array type should compile``() =
    let src =
        """
#[intrinsic("float32")]
alias float32

Test(x: float32[3]): () = ()
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``Fixed array type should compile 2``() =
    let src =
        """
#[intrinsic("float32")]
alias float32

Test(x: float32[4][4]): () = ()
        """
    Oly src
    |> shouldCompile