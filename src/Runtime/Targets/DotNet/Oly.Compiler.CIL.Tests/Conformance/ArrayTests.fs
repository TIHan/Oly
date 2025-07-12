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
let ``Able to get and set element of a mutable fixed array``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("get_element")]
(`[]`)<T, N>(byref<mutable T[N]>, index: int32): T where N: constant int32

#[intrinsic("set_element")]
(`[]`)<T, N>(byref<mutable T[N]>, index: int32, T): () where N: constant int32

main(): () =
    let mutable xs: mutable int32[3] = mutable [4;5;6]
    print(xs[0])
    print(xs[1])
    print(xs[2])
    xs[1] <- 9
    print(xs[1])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4569"
    |> ignore

[<Fact>]
let ``Able to get and set element of a mutable fixed array and test copy-by-value``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("get_element")]
(`[]`)<T, N>(byref<mutable T[N]>, index: int32): T where N: constant int32

#[intrinsic("set_element")]
(`[]`)<T, N>(byref<mutable T[N]>, index: int32, T): () where N: constant int32

main(): () =
    let mutable xs: mutable int32[3] = mutable [4;5;6]
    let mutable xs2 = xs
    xs2[1] <- 9
    print(xs[1])
    print(xs2[1])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "59"
    |> ignore

[<Fact>]
let ``Able to get element address of a mutable fixed array``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("get_element")]
(`[]`)<T, N>(byref<mutable T[N]>, index: int32): T where N: constant int32

main(): () =
    let mutable xs: mutable int32[3] = mutable [4;5;6]
    print(xs[0])
    print(xs[1])
    print(xs[2])
    let addr = &xs[1]
    print(xs[1])
    addr <- 9
    print(xs[1])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "45659"
    |> ignore

