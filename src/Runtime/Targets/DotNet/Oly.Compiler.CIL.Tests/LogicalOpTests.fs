module LogicalOpTests

open Xunit
open TestUtilities
open Utilities

[<Fact>]
let ``Test 'greater than (int32)' intrinsic condition``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 4)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than (int32)' intrinsic condition 2``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 5)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'greater than (int32)' intrinsic condition 3``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 6)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (int32)' intrinsic condition``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 4)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (int32)' intrinsic condition 2``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 5)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (int32)' intrinsic condition 3``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 6)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'greater than (float32)' intrinsic condition``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 4.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than (float32)' intrinsic condition 2``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 5.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'greater than (float32)' intrinsic condition 3``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 6.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (float32)' intrinsic condition``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 4.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (float32)' intrinsic condition 2``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 5.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (float32)' intrinsic condition 3``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 6.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'greater than (uint32)' intrinsic condition``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(4294967295u, 0u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than (uint32)' intrinsic condition 2``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(4294967295u, 4294967295u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'greater than (uint32)' intrinsic condition 3``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_greater_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(0u, 4294967295u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (uint32)' intrinsic condition``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(4294967295u, 0u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (uint32)' intrinsic condition 2``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(4294967295u, 4294967295u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'greater than or equal (uint32)' intrinsic condition 3``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_greater_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(0u, 4294967295u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than (int32)' intrinsic condition``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 4)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than (int32)' intrinsic condition 2``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 5)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than (int32)' intrinsic condition 3``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 6)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (int32)' intrinsic condition``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 4)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (int32)' intrinsic condition 2``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 5)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (int32)' intrinsic condition 3``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5, 6)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'less than (float32)' intrinsic condition``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 4.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than (float32)' intrinsic condition 2``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 5.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than (float32)' intrinsic condition 3``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 6.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (float32)' intrinsic condition``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 4.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (float32)' intrinsic condition 2``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 5.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (float32)' intrinsic condition 3``() =
    let src =
        """
test(x: float32, y: float32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(5.0f, 6.0f)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'less than (uint32)' intrinsic condition``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(4294967295u, 0u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than (uint32)' intrinsic condition 2``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(4294967295u, 4294967295u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than (uint32)' intrinsic condition 3``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_less_than(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(0u, 4294967295u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (uint32)' intrinsic condition``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(4294967295u, 0u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (uint32)' intrinsic condition 2``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(4294967295u, 4294967295u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'less than or equal (uint32)' intrinsic condition 3``() =
    let src =
        """
test(x: uint32, y: uint32) : () =
    if (__oly_less_than_or_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(0u, 4294967295u)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'equal (int32)' intrinsic condition``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(3, 3)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'equal (int32)' intrinsic condition 2``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(3, 2)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'equal (int32)' intrinsic condition 3``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(2, 3)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'not equal (int32)' intrinsic condition``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_not_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(3, 3)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'not equal (int32)' intrinsic condition 2``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_not_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(3, 2)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'not equal (int32)' intrinsic condition 3``() =
    let src =
        """
test(x: int32, y: int32) : () =
    if (__oly_not_equal(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(2, 3)
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'bitwise exclusive or (int32)' ``() =
    let src =
        """
#[intrinsic("bitwise_exclusive_or")]
(^)(int32, int32): int32

test(x: int32, y: int32) : int32 =
    x ^ y

main() : () =
    print(test(1, 4))
        """
    OlySharp src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore