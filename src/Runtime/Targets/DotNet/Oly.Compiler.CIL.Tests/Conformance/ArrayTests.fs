module Conformance.ArrayTests

open Xunit
open TestUtilities
open Utilities

[<Fact>]
let ``Able to get element of a fixed array``() =
    let src =
        """
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
    print(xs[0])
    print(xs[1])
    print(xs[2])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Able to get element of a fixed array with more than one column``() =
    let src =
        """
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
    print(xs[0,0])
    print(xs[1,0])
    print(xs[2,0])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

