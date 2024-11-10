module DotNet.Conformance.IsSubtypeOfTests

open Xunit
open TestUtilities
open Utilities

let PreludeSrc = """
#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("uint32")]
alias uint32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(object): ()

module DotNet =

    #[import("intrinsic-CLR", "", "is")]
    IsSubtypeOf<require T>(object): bool
"""

[<Fact>]
let ``Unit``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print(DotNet.IsSubtypeOf<ValueTuple>(()))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32>>((1, 2)))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32, int32>>((1, 2, 3)))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32, int32, int32>>((1, 2, 3, 4)))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32, int32, int32, int32>>((1, 2, 3, 4, 5)))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32, int32, int32, int32, int32>>((1, 2, 3, 4, 5, 6)))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32, int32, int32, int32, int32, int32>>((1, 2, 3, 4, 5, 6, 7)))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32, int32, int32, int32, int32, int32, ValueTuple<uint32>>>((1, 2, 3, 4, 5, 6, 7, 8: uint32)))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32, int32, int32, int32, int32, int32, ValueTuple<int32, uint32>>>((1, 2, 3, 4, 5, 6, 7, 8, 9: uint32)))
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
    print(DotNet.IsSubtypeOf<ValueTuple<int32, int32, int32, int32, int32, int32, int32, ValueTuple<int32, uint32, int32, int32, int32, int32, int32, ValueTuple<int32, int32, int32, int32>>>>((1, 2, 3, 4, 5, 6, 7, 8, 9: uint32, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Immutable array is actually a mutable array``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print(DotNet.IsSubtypeOf<mutable int32[]>([1]))
    print(DotNet.IsSubtypeOf<Array>([1]))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrue"

[<Fact>]
let ``Mutable array is actually an immutable array``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    print(DotNet.IsSubtypeOf<int32[]>(mutable [1]))
    print(DotNet.IsSubtypeOf<Array>(mutable [1]))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrue"

[<Fact>]
let ``Underlying type of a newtype is the same``() =
    let src =
        $"""
open System

{PreludeSrc}

newtype NewInt32 =
    field value: int32

main(): () =
    print(DotNet.IsSubtypeOf<int32>(NewInt32(1)))
    print(DotNet.IsSubtypeOf<NewInt32>(1))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrue"

[<Fact>]
let ``Enum is a System_Enum``() =
    let src =
        $"""
open System

{PreludeSrc}

enum E =
    | A

main(): () =
    print(DotNet.IsSubtypeOf<E>(E.A))
    print(DotNet.IsSubtypeOf<Enum>(E.A))
    print(DotNet.IsSubtypeOf<ValueType>(E.A))
    print(DotNet.IsSubtypeOf<Object>(E.A))
    print(DotNet.IsSubtypeOf<int32>(E.A))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrueTrueTrueFalse"

[<Fact>]
let ``Enum is a System_Enum 2``() =
    let src =
        $"""
open System

{PreludeSrc}

enum E =
    inherits uint32
    | A

main(): () =
    print(DotNet.IsSubtypeOf<E>(E.A))
    print(DotNet.IsSubtypeOf<Enum>(E.A))
    print(DotNet.IsSubtypeOf<ValueType>(E.A))
    print(DotNet.IsSubtypeOf<Object>(E.A))
    print(DotNet.IsSubtypeOf<int32>(E.A))
    print(DotNet.IsSubtypeOf<uint32>(E.A))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrueTrueTrueFalseFalse"

[<Fact>]
let ``Struct is a System_ValueType``() =
    let src =
        $"""
open System

{PreludeSrc}

struct S

main(): () =
    print(DotNet.IsSubtypeOf<ValueType>(S()))
    print(DotNet.IsSubtypeOf<Object>(S()))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrue"

[<Fact>]
let ``Action1``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    let f = (x: int32) -> ()
    print(DotNet.IsSubtypeOf<Action<int32>>(f))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Action2``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    let f = (x: int32, y: uint32) -> ()
    print(DotNet.IsSubtypeOf<Action<int32, uint32>>(f))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Func1``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    let f = () -> 1
    print(DotNet.IsSubtypeOf<Func<int32>>(f))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``Func2``() =
    let src =
        $"""
open System

{PreludeSrc}

main(): () =
    let f = (x: uint32) -> 1
    print(DotNet.IsSubtypeOf<Func<uint32, int32>>(f))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"