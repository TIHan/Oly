module Conformance.Expressions.TryCatchFinallyTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Simple throw should compile``() =
    let src =
        """
#[intrinsic("throw")]
(throw)<TResult>(__oly_utf16): TResult

test(): __oly_int32 =
    throw "a message for throw"
        """
    Oly src
    |> shouldCompile
    |> ignore