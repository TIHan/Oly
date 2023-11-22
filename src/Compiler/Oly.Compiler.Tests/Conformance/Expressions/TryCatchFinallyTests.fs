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
    |> withCompile
    |> ignore

[<Fact>]
let ``Able to use try/catch``() =
    """
main(): () =
    try
        let _ = 1
    catch (ex: __oly_object) =>
        ()
    """
    |> Oly
    |> shouldCompile

[<Fact>]
let ``Able to use try/finally``() =
    """
main(): () =
    try
        let _ = 1
    finally
        ()
    """
    |> Oly
    |> shouldCompile

[<Fact>]
let ``Able to use try/catch/finally``() =
    """
main(): () =
    try
        let _ = 1
    catch (ex: __oly_object) =>
        ()
    finally
        ()
    """
    |> Oly
    |> shouldCompile

[<Fact>]
let ``Get symbol for catch type``() =
    """
main(): () =
    try
        let _ = 1
    catch (~^~ex: __oly_object) =>
        ()
    """
    |> hasSymbolSignatureTextByCursor "ex: __oly_object"