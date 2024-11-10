module DotNet.Conformance.IsOperatorTests

open Xunit
open TestUtilities
open Utilities

let PreludeSrc = Conformance.IsOperatorTests.PreludeSrc

[<Fact>]
let ``Unit``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print(() is type(ValueTuple))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2) is type(ValueTuple<int32, int32>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple3``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2, 3) is type(ValueTuple<int32, int32, int32>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple4``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2, 3, 4) is type(ValueTuple<int32, int32, int32, int32>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple5``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2, 3, 4, 5) is type(ValueTuple<int32, int32, int32, int32, int32>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple6``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2, 3, 4, 5, 6) is type(ValueTuple<int32, int32, int32, int32, int32, int32>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple7``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2, 3, 4, 5, 6, 7) is type(ValueTuple<int32, int32, int32, int32, int32, int32, int32>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple8``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2, 3, 4, 5, 6, 7, 8: uint32) is type(ValueTuple<int32, int32, int32, int32, int32, int32, int32, ValueTuple<uint32>>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple9``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2, 3, 4, 5, 6, 7, 8, 9: uint32) is type(ValueTuple<int32, int32, int32, int32, int32, int32, int32, ValueTuple<int32, uint32>>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Tuple18``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print((1, 2, 3, 4, 5, 6, 7, 8, 9: uint32, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19) is type(ValueTuple<int32, int32, int32, int32, int32, int32, int32, ValueTuple<int32, uint32, int32, int32, int32, int32, int32, ValueTuple<int32, int32, int32, int32>>>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"