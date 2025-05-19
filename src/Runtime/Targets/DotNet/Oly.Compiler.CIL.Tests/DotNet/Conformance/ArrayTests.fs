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

[<Fact>]
let ``Should throw ArgumentOutOfRangeException for fixed array with more than one column``() =
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
(`[,]`)<T, Row, Column>(inref<T[Row,Column]>, rowIndex: int32, columnIndex: int32): T where Row: constant int32 where Column: constant int32

main(): () =
    let xs: int32[3,2] = 
        [
            4;5;6
            7;8;9
        ]
    try
        print(xs[3,0])
    catch (ex: ArgumentOutOfRangeException) =>
        print("passed")

    try
        print(xs[0,2])
    catch (ex: ArgumentOutOfRangeException) =>
        print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passedpassed"
    |> ignore

