module Conformance.IsOperatorTests

open Xunit
open TestUtilities
open Utilities

let PreludeSrc =
    """
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

#[intrinsic("is")]
(is)<T>(object): bool
    """

[<Fact>]
let ``Unit``() =
    let src =
        $"""
{PreludeSrc}

M(x: ()): () =
    print(x is type(()))
    print(x is type(object))

main(): () =
    let x = ()
    print(x is type(()))
    print(x is type(object))
    print(() is type(()))
    print(() is type(object))
    M(())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrueTrueTrueTrueTrue"

[<Fact>]
let ``Tuple``() =
    let src =
        $"""
{PreludeSrc}

M(x: (int32, int32)): () =
    print(x is type((int32, int32)))
    print(x is type(object))

main(): () =
    let x = (1, 5)
    print(x is type((int32, int32)))
    print(x is type(object))
    print((5, 8) is type((int32, int32)))
    print(() is type(object))
    M((2, 9))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrueTrueTrueTrueTrue"

[<Fact>]
let ``Array``() =
    let src =
        $"""
{PreludeSrc}

M(x: int32[]): () =
    print(x is type(int32[]))
    print(x is type(object))

main(): () =
    let x = [1]
    print(x is type(int32[]))
    print(x is type(object))
    print(([2]) is type(int32[]))
    print(([3]) is type(object))
    M([4])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrueTrueTrueTrueTrue"

[<Fact>]
let ``Array 2``() =
    let src =
        $"""
{PreludeSrc}

M(x: int32[]): () =
    print(x is type(int32[]))
    print(x is type(object))

main(): () =
    let x = [1]
    print(x is type(int32[]))
    print(x is type(object))
    print([2] is type(int32[]))
    print([3] is type(object))
    M([4])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrueTrueTrueTrueTrue"

[<Fact>]
let ``Mutable Array``() =
    let src =
        $"""
{PreludeSrc}

M(x: mutable int32[]): () =
    print(x is type(mutable int32[]))
    print(x is type(object))

main(): () =
    let x = mutable [1]
    print(x is type(mutable int32[]))
    print(x is type(object))
    print((mutable [2]) is type(mutable int32[]))
    print((mutable [3]) is type(object))
    M(mutable [4])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrueTrueTrueTrueTrue"

[<Fact>]
let ``Mutable Array 2``() =
    let src =
        $"""
{PreludeSrc}

M(x: mutable int32[]): () =
    print(x is type(mutable int32[]))
    print(x is type(object))

main(): () =
    let x = mutable [1]
    print(x is type(mutable int32[]))
    print(x is type(object))
    print(mutable [2] is type(mutable int32[]))
    print(mutable [3] is type(object))
    M(mutable [4])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "TrueTrueTrueTrueTrueTrue"

[<Fact>]
let ``Array is not mutable array``() =
    let src =
        $"""
{PreludeSrc}

M(x: int32[]): () =
    print(x is type(mutable int32[]))

main(): () =
    let x = [1]
    print(x is type(mutable int32[]))
    print(([2]) is type(mutable int32[]))
    M([4])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "FalseFalseFalse"

[<Fact>]
let ``Mutable array is not array``() =
    let src =
        $"""
{PreludeSrc}

M(x: mutable int32[]): () =
    print(x is type(int32[]))

main(): () =
    let x = mutable [1]
    print(x is type(int32[]))
    print((mutable [2]) is type(int32[]))
    M(mutable [4])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "FalseFalseFalse"

