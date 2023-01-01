module SamplePrograms

open Xunit
open TestUtilities
open Utilities

let coreLib =
    """
// Interfaces

interface Add<T1, T2, T3> =

    static abstract (+)(T1, T2) : T3

interface Subtract<T1, T2, T3> =

    static abstract (-)(T1, T2) : T3

interface Multiply<T1, T2, T3> =

    static abstract (*)(T1, T2) : T3

interface Divide<T1, T2, T3> =

    static abstract (/)(T1, T2) : T3

interface Remainder<T1, T2, T3> =

    static abstract (%)(T1, T2) : T3

interface Equal<T1, T2, T3> =

    static abstract (==)(T1, T2) : T3

interface NotEqual<T1, T2, T3> =

    static abstract (!=)(T1, T2) : T3

interface GreaterThan<T1, T2, T3> =

    static abstract (>)(T1, T2) : T3

interface GreaterThanOrEqual<T1, T2, T3> =

    static abstract (>=)(T1, T2) : T3

interface LessThan<T1, T2, T3> =

    static abstract (<)(T1, T2) : T3

interface LessThanOrEqual<T1, T2, T3> =

    static abstract (<=)(T1, T2) : T3

// Operators

#[intrinsic("add")]
(+)(int32, int32) : int32
#[intrinsic("add")]
(+)(float32, float32) : float32
(+)<T>(x: T, y: T) : T where T : Add<T, T, T> = T.(+)(x, y)

#[intrinsic("subtract")]
(-)(int32, int32) : int32
(-)<T>(x: T, y: T) : T where T : Subtract<T, T, T> = T.(-)(x, y)

#[intrinsic("multiply")]
(*)(int32, int32) : int32
#[intrinsic("multiply")]
(*)(float32, float32) : float32
(*)<T>(x: T, y: T) : T where T : Multiply<T, T, T> = T.(*)(x, y)

#[intrinsic("divide")]
(/)(int32, int32) : int32
(/)<T>(x: T, y: T) : T where T : Divide<T, T, T> = T.(/)(x, y)

#[intrinsic("remainder")]
(%)(int32, int32) : int32
(%)<T>(x: T, y: T) : T where T : Remainder<T, T, T> = T.(%)(x, y)

#[intrinsic("equal")]
(==)(int32, int32) : bool
(==)<T>(x: T, y: T) : bool where T : Equal<T, T, bool> = T.(==)(x, y)

#[intrinsic("not_equal")]
(!=)(int32, int32) : bool
(!=)<T>(x: T, y: T) : bool where T : NotEqual<T, T, bool> = T.(!=)(x, y)

#[intrinsic("greater_than")]
(>)(int32, int32) : bool
(>)<T>(x: T, y: T) : bool where T : GreaterThan<T, T, bool> = T.(>)(x, y)

#[intrinsic("greater_than_or_equal")]
(>=)(int32, int32) : bool
(>=)<T>(x: T, y: T) : bool where T : GreaterThanOrEqual<T, T, bool> = T.(>=)(x, y)

#[intrinsic("less_than")]
(<)(int32, int32) : bool
(<)<T>(x: T, y: T) : bool where T : LessThan<T, T, bool> = T.(<)(x, y)

#[intrinsic("less_than_or_equal")]
(<=)(int32, int32) : bool
(<=)<T>(x: T, y: T) : bool where T : LessThanOrEqual<T, T, bool> = T.(<=)(x, y)

#[intrinsic("and")]
(&&)(bool, bool) : bool

#[intrinsic("or")]
(||)(bool, bool) : bool

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>
"""

let run expectedOutput src =
    OlySharp (coreLib + src)
    |> shouldCompile
    |> shouldRunWithExpectedOutput expectedOutput
    |> ignore

[<Fact>]
let ``Simple 'and' operation``() =
    let src =
        """
main() : () =
    let result =
        __oly_and(print(5)
                  true,
                  print(10)
                  true)
        """
    run "510" src

[<Fact>]
let ``Simple 'and' operation 2``() =
    let src =
        """
main() : () =
    let result =
        __oly_and(print(5)
                  false,
                  print(10)
                  true)
        """
    run "5" src

[<Fact>]
let ``Simple 'and' operation 3``() =
    let src =
        """
main() : () =
    let result =
        (print(5)
         true) && (print(10)
                   true)
        """
    run "510" src

[<Fact>]
let ``Simple 'and' operation 4``() =
    let src =
        """
main() : () =
    let result =
        (print(5)
         false) && (print(10)
                    true)
        """
    run "5" src

[<Fact>]
let ``Simple 'and' operation 5``() =
    let src =
        """
test(x: bool, y: bool) : bool = x && y

main() : () =
    let result =
        test(print(5)
             false,
             print(10)
             true)
        """
    run "510" src

[<Fact>]
let ``Simple 'or' operation``() =
    let src =
        """
main() : () =
    let result =
        __oly_or(print(5)
                 true,
                 print(10)
                 true)
        """
    run "5" src

[<Fact>]
let ``Simple 'or' operation 2``() =
    let src =
        """
main() : () =
    let result =
        __oly_or(print(5)
                 false,
                 print(10)
                 true)
        """
    run "510" src

[<Fact>]
let ``Simple 'or' operation 3``() =
    let src =
        """
main() : () =
    let result =
        (print(5)
         true) || (print(10)
                   true)
        """
    run "5" src

[<Fact>]
let ``Simple 'or' operation 4``() =
    let src =
        """
main() : () =
    let result =
        (print(5)
         false) || (print(10)
                    true)
        """
    run "510" src

[<Fact>]
let ``Simple 'or' operation 5``() =
    let src =
        """
test(x: bool, y: bool) : bool = x || y

main() : () =
    let result =
        test(print(5)
             true,
             print(10)
             true)
        """
    run "510" src

[<Fact>]
let ``Fizz buzz``() =
    let src =
        """
loop(i: int32) : () =
    if (i <= 100)
        if (i % 3 == 0 && i % 5 == 0)
            print("FizzBuzz")
        else if (i % 3 == 0)
            print("Fizz")
        else if (i % 5 == 0)
            print("Buzz")
        else
            print(i)
        loop(i + 1)
 
main() : () =
    loop(1)
        """

    let expectedOutput =
        """12Fizz4BuzzFizz78FizzBuzz11Fizz1314FizzBuzz1617Fizz19BuzzFizz2223FizzBuzz26Fizz2829FizzBuzz3132Fizz34BuzzFizz3738FizzBuzz41Fizz4344FizzBuzz4647Fizz49BuzzFizz5253FizzBuzz56Fizz5859FizzBuzz6162Fizz64BuzzFizz6768FizzBuzz71Fizz7374FizzBuzz7677Fizz79BuzzFizz8283FizzBuzz86Fizz8889FizzBuzz9192Fizz94BuzzFizz9798FizzBuzz"""

    run expectedOutput src

[<Fact>]
let ``Fizz buzz with some random assignments byref``() =
    let src =
        """
#[inline]
loop(i: int32) : () =
    let mutable y = i
    if (i <= 100)
        if (i % 3 == 0 && i % 5 == 0)
            print("FizzBuzz")
        else if (i % 3 == 0)
            print("Fizz")
        else if (i % 5 == 0)
            print("Buzz")
        else
            print(i)
        let z = &y
        z <- 50
        loop(i + 1)
 
main() : () =
    loop(1)
        """

    let expectedOutput =
        """12Fizz4BuzzFizz78FizzBuzz11Fizz1314FizzBuzz1617Fizz19BuzzFizz2223FizzBuzz26Fizz2829FizzBuzz3132Fizz34BuzzFizz3738FizzBuzz41Fizz4344FizzBuzz4647Fizz49BuzzFizz5253FizzBuzz56Fizz5859FizzBuzz6162Fizz64BuzzFizz6768FizzBuzz71Fizz7374FizzBuzz7677Fizz79BuzzFizz8283FizzBuzz86Fizz8889FizzBuzz9192Fizz94BuzzFizz9798FizzBuzz"""

    run expectedOutput src