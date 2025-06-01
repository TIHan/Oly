module DotNet.Conformance.ArrayTests

open Xunit
open TestUtilities
open Utilities

[<Fact>]
let ``Should throw ArgumentOutOfRangeException for fixed array``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("get_element")]
(`[]`)<T, N>(inref<T[N]>, index: int32): T where N: constant int32

main(): () =
    let xs: int32[3] = [4;5;6]
    try
        print(xs[5])
    catch (ex: ArgumentOutOfRangeException) =>
        print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore
