module PatternMatchStressTests

open Xunit
open TestUtilities
open Utilities

[<Fact>]
let ``Pattern matching - stress test 001``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(value: int32): () =
    match (value)
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | _ => print("failed")

main(): () =
    test(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Pattern matching - stress test 002``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(value: int32): () =
    match (value)
    | 1 => print("1")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("_")
    | _ => print("failed")

main(): () =
    test(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Pattern matching - stress test 003``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(value: int32): () =
    match (value)
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1
    | 1 => print("1")
    | _ => print("failed")

main(): () =
    test(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Pattern matching - stress test 004``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(value: int32): () =
    match (value)
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")
    | 1 => print("1")

    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | 2 => print("2")
    | _ => print("failed")

main(): () =
    test(1)
    test(2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "12"
    |> ignore

[<Fact>]
let ``Pattern matching - stress test 008``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(value1: int32, value2: int32): () =
    match (value1, value2)
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | _ => print("failed")

main(): () =
    test(1, 2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "12"
    |> ignore

[<Fact>]
let ``Pattern matching - stress test 009``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(value1: int32, value2: int32): () =
    match (value1, value2)
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")
    | 1, 2 => print("12")

    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | 1, 3 => print("13")
    | _ => print("failed")

main(): () =
    test(1, 2)
    test(1, 3)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1213"
    |> ignore

[<Fact>]
let ``Pattern matching - stress test 0010``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(value1: int32, value2: int32): () =
    match (value1, value2)
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2
    | 1, 2 => print("12")

    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3
    | 1, 3 => print("13")
    | _ => print("failed")

main(): () =
    test(1, 2)
    test(1, 3)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1213"
    |> ignore

[<Fact>]
let ``Pattern matching - stress test 011``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(value: int32): () =
    match (value)
    | 1 => print("12")
    | 2 => print("12")
    | 3 => print("12")
    | 4 => print("12")
    | 5 => print("12")
    | 6 => print("12")
    | 7 => print("12")
    | 8 => print("12")
    | 9 => print("12")
    | 10 => print("12")
    | 11 => print("12")
    | 12 => print("12")
    | 13 => print("12")
    | 14 => print("12")
    | 15 => print("12")
    | 16 => print("12")
    | 17 => print("12")
    | 18 => print("12")
    | 19 => print("12")
    | 20 => print("12")
    | 21 => print("12")
    | 22 => print("12")
    | 23 => print("12")
    | 24 => print("12")
    | 25 => print("12")
    | 26 => print("12")
    | 27 => print("12")
    | 28 => print("12")
    | 29 => print("12")
    | 30 => print("12")
    | 31 => print("12")
    | 32 => print("12")
    | 33 => print("12")
    | 34 => print("12")
    | 35 => print("12")
    | 36 => print("12")
    | 37 => print("12")
    | 38 => print("12")
    | 39 => print("12")
    | 40 => print("12")
    | 41 => print("12")
    | 42 => print("12")
    | 43 => print("12")
    | 44 => print("12")
    | 45 => print("12")
    | 46 => print("12")
    | 47 => print("12")
    | 48 => print("12")
    | 49 => print("12")
    | 50 => print("12")
    | 51 => print("12")
    | 52 => print("12")
    | 53 => print("12")
    | 54 => print("12")
    | 55 => print("12")
    | 56 => print("12")
    | 57 => print("12")
    | 58 => print("12")
    | 59 => print("12")
    | 60 => print("12")
    | 61 => print("12")
    | 62 => print("12")
    | 63 => print("12")
    | 64 => print("12")
    | 65 => print("12")
    | 66 => print("12")
    | 67 => print("12")
    | 68 => print("12")
    | 69 => print("12")
    | 70 => print("12")
    | 71 => print("12")
    | 72 => print("12")
    | 73 => print("12")
    | 74 => print("12")
    | 75 => print("12")
    | 76 => print("12")
    | 77 => print("12")
    | 78 => print("12")
    | 79 => print("12")
    | 80 => print("12")
    | 81 => print("12")
    | 82 => print("12")
    | 83 => print("12")
    | 84 => print("12")
    | 85 => print("12")
    | 86 => print("12")
    | 87 => print("12")
    | 88 => print("12")
    | 89 => print("89")
    | 90 => print("90")
    | _ => print("failed")

main(): () =
    test(1)
    test(89)
    test(90)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "128990"
    |> ignore