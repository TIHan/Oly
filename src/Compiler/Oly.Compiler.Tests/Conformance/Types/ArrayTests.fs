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
let ``Fixed mutable array type should compile``() =
    let src =
        """
#[intrinsic("float32")]
alias float32

Test(x: mutable float32[3]): () = ()
        """
    Oly src
    |> shouldCompile