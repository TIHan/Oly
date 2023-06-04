module Conformance.Expressions.ExpressionTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Shift right definition``() =
    let src =
        """
#[intrinsic("bitwise_shift_right")]
(>>)(__oly_int32, __oly_int32): __oly_int32
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should error as return value is not supposed to be unit``() =
    let src =
        """
M(): __oly_utf16 =
    let x = 1
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '__oly_utf16' but is '()'.",
                """
    let x = 1
    ^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should error as return value is not supposed to be unit 2``() =
    let src =
        """
M(): __oly_utf16 =
    if (true)
        let x = 1
    else
        "test"
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '__oly_utf16' but is '()'.",
                """
        let x = 1
        ^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should error as return value is not supposed to be unit 3``() =
    let src =
        """
M(): __oly_utf16 =
    if (true)
        "test"
    else
        let x = 1
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '__oly_utf16' but is '()'.",
                """
        let x = 1
        ^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Typed expression has right symbol``() =
    let src =
        """
test() : () =
    let ~^~f(x) = x : __oly_float32
        """
    src |> hasSymbolSignatureTextByCursor "f(x: __oly_float32): __oly_float32"

[<Fact>]
let ``Typed expression has right symbol 2``() =
    let src =
        """
test() : () =
    let f(~^~x) = x : __oly_float32
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_float32"

[<Fact>]
let ``Typed expression has right symbol 3``() =
    let src =
        """
test() : () =
    let f(x) = ~^~x : __oly_float32
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_float32"

[<Fact>]
let ``Typed expression has right symbol 4``() =
    let src =
        """
test() : () =
    let f(x) = x : ~^~__oly_float32
        """
    src |> hasSymbolSignatureTextByCursor "__oly_float32"

[<Fact>]
let ``Inner expression has the right symbol``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("base_object")]
alias object

#[import("CLR", "System", "Console")]
class Console =
   
    static Write(value: utf16) : ()
    static Write(value: int32) : ()

struct TestData =
    field str: utf16

    new(str: utf16) = { str = str }

    TestFunc() : utf16 =
        this.str

main() : () =
    let td = TestData("Hello World from Test data!")
    Console.Write(~^~td.TestFunc())
        """
    src |> hasSymbolSignatureTextByCursor "td: TestData"

[<Fact>]
let ``Should error for missing right-hand side expression``() =
    let src =
        """
(<)(x: __oly_int32, y: __oly_int32) : __oly_bool = true

main() : () =
    let mutable x = 1
    x <
    ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected 'right-side argument expression' after '<'."
        "Expected type '()' but is '__oly_bool'."
    ]
    |> ignore

[<Fact>]
let ``Should error for sequential expression1``() =
    let src =
        """
(<)(x: __oly_int32, y: __oly_int32) : __oly_bool = true

main() : () =
    let mutable x = 1
    x < 2
    ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '()' but is '__oly_bool'."
    ]
    |> ignore

[<Fact>]
let ``Should error for sequential expression1 2``() =
    let src =
        """
(<)(x: __oly_int32, y: __oly_int32) : __oly_bool = true

main() : () =
    let mutable x = 1
    x < 2
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '()' but is '__oly_bool'."
    ]
    |> ignore

[<Fact>]
let ``Cannot mutate value``() =
    let src =
        """
main() : () =
    let x = 1
    x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'x' is not mutable."
    ]
    |> ignore

[<Fact>]
let ``Can mutate value``() =
    let src =
        """
main() : () =
    let mutable x = 1
    x <- 5
        """
    Oly src
    |> withNoDiagnostics
    |> ignore

[<Fact>]
let ``Expected correct symbol on nested structs``() =
    let src =
        """
struct Test =

    public mutable field orange: __oly_int32
    public mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { orange = x; apple = y }

struct Test2 =
    public mutable field x: Test
    new(x: Test) = { x = x }

test(mutable t: Test2) : () =
    t.~^~x.orange <- 100

main() : () =
    let mutable t = Test2(Test(7, 9))
    test(t)
        """
    src |> hasSymbolSignatureTextByCursor "mutable field x: Test"

[<Fact>]
let ``Expected correct symbol on nested structs 2``() =
    let src =
        """
struct Test =

    public mutable field orange: __oly_int32
    public mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { orange = x; apple = y }

struct Test2 =
    public mutable field x: Test
    new(x: Test) = { x = x }

test(mutable t: Test2) : () =
    t.x.~^~orange <- 100

main() : () =
    let mutable t = Test2(Test(7, 9))
    test(t)
        """
    src |> hasSymbolSignatureTextByCursor "mutable field orange: __oly_int32"

[<Fact>]
let ``Expected correct symbol on nested structs 3``() =
    let src =
        """
struct Test =

    public mutable field orange: __oly_int32
    public mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { orange = x; apple = y }

struct Test2 =
    public mutable field x: Test
    new(x: Test) = { x = x }

test(mutable t: Test2) : () =
    ~^~t.x.orange <- 100

main() : () =
    let mutable t = Test2(Test(7, 9))
    test(t)
        """
    src |> hasSymbolSignatureTextByCursor "t: Test2"

[<Fact>]
let ``Expected correct symbol on nested structs 4``() =
    let src =
        """
struct Test =

    public mutable field orange: __oly_int32
    public mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { orange = x; apple = y }

struct Test2 =
    public mutable field x: Test
    new(x: Test) = { x = x }

struct Test3 =
    public mutable field y: Test2
    new(y: Test2) = { y = y }

test(mutable t: Test3) : () =
    t.~^~y.x.orange <- 100

main() : () =
    let mutable t = Test3(Test2(Test(7, 9)))
    test(t)
        """
    src |> hasSymbolSignatureTextByCursor "mutable field y: Test2"

[<Fact>]
let ``Expected correct symbol on nested structs 5``() =
    let src =
        """
struct Test =

    public mutable field orange: __oly_int32
    public mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { orange = x; apple = y }

struct Test2 =
    public mutable field x: Test
    new(x: Test) = { x = x }

struct Test3 =
    public mutable field y: Test2
    new(y: Test2) = { y = y }

test(mutable t: Test3) : () =
    t.y.~^~x.orange <- 100

main() : () =
    let mutable t = Test3(Test2(Test(7, 9)))
    test(t)
        """
    src |> hasSymbolSignatureTextByCursor "mutable field x: Test"

[<Fact>]
let ``Expected correct symbol on nested structs 6``() =
    let src =
        """
struct Test =

    public mutable field orange: __oly_int32
    public mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { orange = x; apple = y }

struct Test2 =
    public mutable field x: Test
    new(x: Test) = { x = x }

struct Test3 =
    public mutable field y: Test2
    new(y: Test2) = { y = y }

test(mutable t: Test3) : () =
    t.y.x.~^~orange <- 100

main() : () =
    let mutable t = Test3(Test2(Test(7, 9)))
    test(t)
        """
    src |> hasSymbolSignatureTextByCursor "mutable field orange: __oly_int32"

[<Fact>]
let ``Expected correct symbol on nested structs 7``() =
    let src =
        """
struct Test =

    public mutable field orange: __oly_int32
    public mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { orange = x; apple = y }

struct Test2 =
    public mutable field x: Test
    new(x: Test) = { x = x }

struct Test3 =
    public mutable field y: Test2
    new(y: Test2) = { y = y }

test(mutable t: Test3) : () =
    ~^~t.y.x.orange <- 100

main() : () =
    let mutable t = Test3(Test2(Test(7, 9)))
    test(t)
        """
    src |> hasSymbolSignatureTextByCursor "t: Test3"

[<Fact>]
let ``Expected correct symbol on constructor construct``() =
    let src =
        """
struct Test =

    mutable field orange: __oly_int32
    mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { ~^~orange = x; apple = y }
        """
    src |> hasSymbolSignatureTextByCursor "mutable field orange: __oly_int32"

[<Fact>]
let ``Expected correct symbol on constructor construct 2``() =
    let src =
        """
struct Test =

    mutable field orange: __oly_int32
    mutable field apple: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { orange = ~^~x; apple = y }
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``Cannot mutate field value on type``() =
    let src =
        """
class Test =
    public field x: __oly_int32
    new(x: __oly_int32) = { x = x }

main() : () =
    let t = Test(1)
    t.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'x' is not mutable."
    ]
    |> ignore

[<Fact>]
let ``Cannot mutate field value on struct``() =
    let src =
        """
struct Test =
    public field x: __oly_int32
    new(x: __oly_int32) = { x = x }

main() : () =
    let t = Test(1)
    t.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'t' is not mutable."
    ]
    |> ignore

[<Fact>]
let ``Cannot mutate field value on struct 2``() =
    let src =
        """
struct Test =
    public mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

main() : () =
    let t = Test(1)
    t.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'t' is not mutable."
    ]
    |> ignore

[<Fact>]
let ``Cannot mutate field value on struct 3``() =
    let src =
        """
struct Test =
    public field x: __oly_int32
    new(x: __oly_int32) = { x = x }

test(t: __oly_read_by_ref<Test>): () =
    t.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'t' is not mutable."
    ]

[<Fact>]
let ``Cannot mutate field value on struct 4``() =
    let src =
        """
struct Test =
    public field x: __oly_int32
    new(x: __oly_int32) = { x = x }

test(t: __oly_read_write_by_ref<Test>): () =
    t.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'x' is not mutable."
    ]

[<Fact>]
let ``Cannot mutate field value on struct 5``() =
    let src =
        """
struct Test =
    public mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

test(t: __oly_read_by_ref<Test>): () =
    t.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'t' is not mutable."
    ]

[<Fact>]
let ``Cannot mutate field value on struct 6``() =
    let src =
        """
struct Test =
    public mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

test(mutable t: __oly_read_by_ref<Test>): () =
    t.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'t' is not mutable."
    ]

[<Fact>]
let ``Cannot mutate field value on nested struct``() =
    let src =
        """
class Test2 =
    public field test: Test
    new(test: Test) = { test = test }

struct Test =
    public mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

main() : () =
    let t = Test2(Test(1))
    t.test.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'test' is not mutable."
    ]
    |> ignore

[<Fact>]
let ``Cannot mutate field value on nested struct 2``() =
    let src =
        """
class Test2 =
    public field test: Test
    new(test: Test) = { test = test }

struct Test =
    public mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

main() : () =
    let mutable t = Test2(Test(1))
    t.test.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'test' is not mutable."
    ]
    |> ignore

[<Fact>]
let ``Cannot mutate field value on nested struct 3``() =
    let src =
        """
class Test2 =
    public mutable field test: Test
    new(test: Test) = { test = test }

struct Test =
    public field x: __oly_int32
    new(x: __oly_int32) = { x = x }

main() : () =
    let t = Test2(Test(1))
    t.test.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'x' is not mutable."
    ]
    |> ignore

[<Fact>]
let ``Cannot mutate field value on nested struct with chained call``() =
    let src =
        """
class Test2 =
    public mutable field test: Test
    new(test: Test) = { test = test }

struct Test =
    public field x: __oly_int32
    new(x: __oly_int32) = { x = x }

test() : Test2 =
    Test2(Test(1))

main() : () =
    test().test.x <- 5
        """
    Oly src
    |> withErrorDiagnostics [
        "'x' is not mutable."
    ]
    |> ignore

[<Fact>]
let ``Can mutate field value on nested struct with chained call``() =
    let src =
        """
class Test2 =
    public mutable field test: Test
    new(test: Test) = { test = test }

struct Test =
    public mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

test() : Test2 =
    Test2(Test(1))

main() : () =
    test().test.x <- 5
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Struct cycle should not compile``() =
    let src =
        """
struct Test =
    field x: Test = default
        """
    Oly src
    |> withErrorDiagnostics [
        "'Test' has fields that cause a cycle."
    ]
    |> ignore

[<Fact>]
let ``Struct cycle should not compile 2``() =
    let src =
        """
struct Test2 =
    field y: Test = default

struct Test =
    field x: Test2 = default
        """
    Oly src
    |> withErrorDiagnostics [
        "'Test' has fields that cause a cycle."
        "'Test2' has fields that cause a cycle."
    ]
    |> ignore

[<Fact>]
let ``Struct cycle should not compile 3``() =
    let src =
        """
struct Test3 =
    field z: Test = default

struct Test2 =
    field y: Test3 = default

struct Test =
    field x: Test2 = default
        """
    Oly src
    |> withErrorDiagnostics [
        "'Test' has fields that cause a cycle."
        "'Test3' has fields that cause a cycle."
        "'Test2' has fields that cause a cycle."
    ]
    |> ignore

[<Fact>]
let ``Struct cycle should not compile 4``() =
    let src =
        """
struct Test3 =
    field z: Test = default

struct Test2 =
    field y: Test = default

struct Test =
    field x: Test2 = default
        """
    Oly src
    |> withErrorDiagnostics [
        "'Test' has fields that cause a cycle."
        "'Test' has fields that cause a cycle."
        "'Test2' has fields that cause a cycle."
    ]
    |> ignore

[<Fact>]
let ``Generic struct cycle should not compile``() =
    let src =
        """
struct C<T> =
    field y: T

    new(y: T) = { y = y }

struct B =
    field x: C<B>

    new(x: C<B>) = { x = x }
        """
    Oly src
    |> withErrorDiagnostics [
        "'B' has fields that cause a cycle."
        "'C<B>' is causing a cycle on a struct."
    ]
    |> ignore

[<Fact>]
let ``Second-order generic struct cycle should not compile``() =
    let src =
        """
struct A<T<_>> =
    field y: T<__oly_int32>

    new(y: T<__oly_int32>) = { y = y }

struct B<T> =
    field x: A<B>

    new(x: A<B>) = { x = x }
        """
    Oly src
    |> withErrorDiagnostics [
        "'B<T>' has fields that cause a cycle."
        "'A<B>' is causing a cycle on a struct."
    ]
    |> ignore

[<Fact>]
let ``Second-order generic``() =
    let src =
        """
class A<T<_>> =
    field y: T<A<T>>
    new(y: T<A<T>>) = { y = y }

class B<T> =
    field x: T
    new(x: T) = { x = x }

test(z: B<A<B>>) : () =
    let x = A<B>(z)
    let y = A<_>(z)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Second-order generic inference``() =
    let src =
        """
class A<T<_>> =
    field y: T<A<T>>
    new(y: T<A<T>>) = { y = y }

class B<T> =
    field x: T
    new(x: T) = { x = x }

test() : () =
    let test2(~^~z) =
        let y = A<_>(z)
        """
    src |> hasSymbolSignatureTextByCursor "z: T<A<T>>"

[<Fact>]
let ``Second-order generic inference 2``() =
    let src =
        """
class ~^~A<T<_>> =
    field y: T<A<T>>
    new(y: T<A<T>>) = { y = y }

class B<T> =
    field x: T
    new(x: T) = { x = x }

test() : () =
    let test2(z) =
        let y = A<_>(z)
        """
    src |> hasSymbolSignatureTextByCursor "A<T<_>>"

[<Fact>]
let ``Second-order generic inference 3``() =
    let src =
        """
class A<T<_>> =
    field y: T<A<T>>
    new(y: T<A<T>>) = { y = y }

class B<T> =
    field x: T
    new(x: T) = { x = x }

test() : () =
    let ~^~test2(z) =
        let y = A<_>(z)
        """
    src |> hasSymbolSignatureTextByCursor "test2<T<_>>(z: T<A<T>>): ()"

[<Fact>]
let ``Second-order generic struct should fail``() =
    let src =
        """
struct A<T<_>> =
    field y: T<A<T>>
    new(y: T<A<T>>) = { y = y }

struct B<T> =
    field x: T
    new(x: T) = { x = x }

test(x: B<A<B>>) : () = ()
        """
    Oly src
    |> withErrorDiagnostics [
        "'A<B>' is causing a cycle on a struct."
        "'B<A<B>>' is causing a cycle on a struct."
    ]
    |> ignore

[<Fact>]
let ``Second-order generic struct should fail 2``() =
    let src =
        """
struct A<T<_>> =
    field y: T<A<T>>
    new(y: T<A<T>>) = { y = y }

struct B<T> =
    field x: T
    new(x: T) = { x = x }

test() : () =
    let test2(z) =
        let y = A<B>(z)
        """
    Oly src
    |> withErrorDiagnostics [
        "'B<A<B>>' is causing a cycle on a struct."
    ]
    |> ignore

[<Fact>]
let ``Top-level function should error due to missing explicit return type``() =
    let src =
        """
id<T>(x: T) = x
        """
    Oly src
    |> withErrorDiagnostics [
        "The function declaration 'id' must have an explicit return type annotation."
    ]
    |> ignore

[<Fact>]
let ``Top-level function should error due to missing explicit parameter type annotation``() =
    let src =
        """
id<T>(x) : T = x
        """
    Oly src
    |> withErrorDiagnostics [
        "Parameter must have an explicit type annotation as it is part of a top-level function."
    ]
    |> ignore

[<Fact>]
let ``Closure test 1``() =
    let src =
        """
main() : () =
    let x = 123
    let f() = x
    let y = f()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Closure test 2``() =
    let src =
        """
class Test =

    public field x: __oly_int32

    new(x: __oly_int32) = { x = x }

main() : () =
    let t = Test(456)
    let f() = t.x
    let y = f()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Simple curried lambda expression should not compile``() =
    let src =
        """
main() : () =
    let a = (x: __oly_int32) -> (y: __oly_int32) -> x
    let result = a(456)(123.0f)
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '__oly_int32' but is '__oly_float32'."
    ]
    |> ignore

[<Fact>]
let ``Simple partial application should have the correct symbol``() =
    let src =
        """
id(x: __oly_int32) : __oly_int32 = x

main() : () =
    let a = ~^~id
        """
    src |> hasSymbolSignatureTextByCursor "static id(x: __oly_int32): __oly_int32"

[<Fact>]
let ``Nested generic function fully typed with constraint compiles``() =
    let src =
        """
interface Add<T> =

    static abstract add(x: T, y: T) : T

main() : () =
    let test<T>(x: T) : T where T : Add<T> = T.add(x, x)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Curried lambda call has correct signature``() =
    let src =
        """
main() : () =
    let a = (x: __oly_int32) -> (y: __oly_int32) -> x
    let x = ~^~a(456)(123)
        """
    src |> hasSymbolSignatureTextByCursor "a(x: __oly_int32): __oly_int32 -> __oly_int32"

[<Fact>]
let ``A type annotation in paranthesis gives the correct signature``() =
    let src =
        """
test() : (~^~__oly_int32) = 1
        """
    src |> hasSymbolSignatureTextByCursor "__oly_int32"

[<Fact>]
let ``Static local lambda expression should not capture``() =
    let src =
        """
test() : () =
    let x = 1
    let f = static () -> x
        """
    Oly src
    |> withErrorDiagnostics [
        "The free local value 'x' cannot be used in the context of a static local function."
    ]
    |> ignore

[<Fact>]
let ``Static local lambda expression should not capture 2``() =
    let src =
        """
id(x: __oly_int32) : __oly_int32 = x

test() : () =
    let g = id
    let f = static () -> g
        """
    Oly src
    |> withErrorDiagnostics [
        "The free local value 'g' cannot be used in the context of a static local function."
    ]
    |> ignore

[<Fact>]
let ``Static local lambda expression should not capture 3``() =
    let src =
        """
test(z: __oly_int32) : () =
    let f = static () -> z
        """
    Oly src
    |> withErrorDiagnostics [
        "The free local value 'z' cannot be used in the context of a static local function."
    ]
    |> ignore

[<Fact>]
let ``Static local function should not capture``() =
    let src =
        """
test() : () =
    let x = 1
    static let f() = x
        """
    Oly src
    |> withErrorDiagnostics [
        "The free local value 'x' cannot be used in the context of a static local function."
    ]
    |> ignore

[<Fact>]
let ``Static local function should not capture 2``() =
    let src =
        """
test() : () =
    static let f() =
        let x = 1
        static () -> x
        """
    Oly src
    |> withErrorDiagnostics [
        "The free local value 'x' cannot be used in the context of a static local function."
    ]
    |> ignore

[<Fact>]
let ``Static local function should capture another static local function``() =
    let src =
        """
test() : () =
    static let f() = ()
    static let g() = f()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Static local lambda should capture another static local function``() =
    let src =
        """
test() : () =
    static let f() = ()
    let g = static () -> f()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Static local lambda should capture another static local lambda``() =
    let src =
        """
test() : () =
    let f = static () -> ()
    let g = static () -> f()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Static let value not allowed``() =
    let src =
        """
test() : () =
    static let x = 1
        """
    Oly src
    |> withErrorDiagnostics [
        "The 'static' modifier can only be used on local functions."
    ]
    |> ignore

[<Fact>]
let ``Static let value not allowed 2``() =
    let src =
        """
static let x = 1
        """
    Oly src
    |> withErrorDiagnostics [
        "Module members are always implicitly static. Remove 'static'."
        "Modules can never have let-bound members (yet)."
        "Short-hand property getters not implemented (yet)."
    ]
    |> ignore

[<Fact>]
let ``Static let value not allowed 3``() =
    let src =
        """
test() : () =
    static let x = () -> 1
        """
    Oly src
    |> withErrorDiagnostics [
        "The 'static' modifier can only be used on local functions."
    ]
    |> ignore

[<Fact>]
let ``Static local function should compile``() =
    let src =
        """
test() : () =
    static let f() = () -> ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``First class generic value should fail because inference kicks in for mutable values``() =
    let src =
        """
id<T>(x: T) : () = ()
id2<T>(x: T) : () = ()

main() : () =
    let mutable f = id<_>
    f("test")
    f <- id2<_>
    f(5)
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '__oly_utf16' but is '__oly_int32'."
    ]
    |> ignore

[<Fact>]
let ``First class generic value should fail because inference kicks in for mutable values` 2``() =
    let src =
        """
id<T>(x: T) : () = ()
id2<T>(x: T) : () = ()

main() : () =
    let mutable f = (x) -> id<_>(x)
    f("test")
    f <- id2<_>
    f(5)
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '__oly_utf16' but is '__oly_int32'."
    ]
    |> ignore

[<Fact>]
let ``Cannot have an implementation for a value that is platform imported``() =
    let src =
        """
#[import("test", "test", "test")]
class Test =

    test() : () = ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Imported types cannot have members with implementations."
    ]
    |> ignore

[<Fact>]
let ``Should error on struct return instance function``() =
    let src =
        """
struct Test =

    mutable field x: __oly_int32
    field y: __oly_int32
    new(x: __oly_int32, y: __oly_int32) = { x = x; y = y }

    test() : __oly_int32 = this
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '__oly_int32' but is 'Test'."
    ]
    |> ignore

[<Fact>]
let ``Should error when trying to set the receiver``() =
    let src =
        """
struct Test =

    mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

    test() : () =
        this <- Test(1)
        """
    Oly src
    |> withErrorDiagnostics [
        "Cannot set contents of a read-only address."
    ]
    |> ignore

[<Fact>]
let ``Should error when trying to set the receiver 2 - NOW PASSES``() =
    let src =
        """
struct Test =

    mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

    mutable test() : () =
        this <- Test(1)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should error on out-of-scope address-of``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

test(): () =
    let x =
        let mutable y = 1
        &y
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'y' as it might escape its scope at this point.",
                """
        &y
         ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should error on out-of-scope address-of 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =
    public mutable field X: int32 = 1

test(): () =
    let x =
        let mutable y = Test()
        &y.X
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'y' as it might escape its scope at this point.",
                """
        &y.X
         ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should error on out-of-scope address-of 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

test(): () =
    let x =
        let mutable y = 1
        let z = &y
        &z
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'z' as it might escape its scope at this point.",
                """
        &z
         ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should error on out-of-scope address-of 4``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =
    public mutable field X: int32 = 1

test(): () =
    let x =
        let mutable y = Test()
        let z = &y
        &z.X
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'z' as it might escape its scope at this point.",
                """
        &z.X
         ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should error on trying to re-assign byref``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

test(x: byref<int32>): () =
    &x <- 1
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'byref<int32>' but is 'int32'.",
                """
    &x <- 1
          ^
"""
            )
            ("Left-hand expression is not a valid mutation.",
            """
    &x <- 1
    ^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should error on trying to re-assign byref 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

test(x: byref<int32>): () =
    (&x) <- 1
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'byref<int32>' but is 'int32'.",
                """
    (&x) <- 1
            ^
"""
            )
            ("Left-hand expression is not a valid mutation.",
            """
    (&x) <- 1
    ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Half-complete identifier should have the right signature``() =
    let src =
        """
class Test =

    static test() : () = ()

main() : () =
    ~^~Test.
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "Test"

[<Fact>]
let ``Half-complete identifier should have the right signature 2``() =
    let src =
        """
class Test =

    static test() : () = ()

main() : () =
    ~^~Test.te
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "Test"

[<Fact>]
let ``Half-complete identifier should have the right signature 3``() =
    let src =
        """
module Test =

    test() : () = ()

main() : () =
    ~^~Test
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "Test"

[<Fact>]
let ``Half-complete identifier should have the right signature 4``() =
    let src =
        """
class Test =

    new() = { }

    static test() : () = ()

main() : () =
    ~^~Test
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "new(): Test"

[<Fact>]
let ``Qualified interface call should have right signature``() =
    let src =
        """
interface Test =

    static abstract test() : ()

f<T>() : () where T : Test = ~^~T.test()
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "T"

[<Fact>]
let ``Qualified interface call should have right signature 2``() =
    let src =
        """
interface Test =

    static abstract (+)() : ()

f<T>() : () where T : Test = ~^~T.(+)()
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "T"

[<Fact>]
let ``Should be a function group symbol``() =
    let src =
        """
class Test =

    static test() : () = ()
    static test(x: __oly_int32) : () = ()

main() : () =
    Test.~^~test
        """
    let symbol = getSymbolByCursorIgnoreDiagnostics src
    match symbol with
    | :? OlyFunctionGroupSymbol as symbol ->
        Assert.Equal("test", symbol.Name)
        Assert.Equal("test", symbol.SignatureText)
        Assert.Equal(2, symbol.Functions.Length)
    | _ ->
        failwith "Expected a function group symbol."

[<Fact>]
let ``Should be a function group symbol 2``() =
    let src =
        """
class Test =

    static test() : () = ()
    static test(x: __oly_int32) : () = ()
    static test(y: __oly_float32) : () = ()

main() : () =
    let f(x) =
        Test.~^~test(x)
        """
    let symbol = getSymbolByCursorIgnoreDiagnostics src
    match symbol with
    | :? OlyFunctionGroupSymbol as symbol ->
        Assert.Equal("test", symbol.Name)
        Assert.Equal("test", symbol.SignatureText)
        Assert.Equal(2, symbol.Functions.Length)
    | _ ->
        failwith "Expected a function group symbol."

[<Fact>]
let ``Should be a function group symbol 3``() =
    let src =
        """
class Test =

    static test() : () = ()
    static test(x: __oly_int32) : () = ()
    static test(y: __oly_float32) : () = ()

main() : () =
    let f(x) =
        ~^~Test.test(x)
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "Test"

[<Fact>]
let ``Should error on function group for ambiguity overloads``() =
    let src =
        """
class Test =

    static test() : () = ()
    static test(x: __oly_int32) : () = ()
    static test(y: __oly_float32) : () = ()

main() : () =
    let f(x) =
        Test.test(x)
        """
    Oly src
    |> withErrorDiagnostics [
        "'test' has ambiguous functions."
    ]
    |> ignore

[<Fact>]
let ``Can return byref from struct field``() =
    let src =
        """
#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    mutable field x: __oly_int32
    new(x: __oly_int32) = { x = x }

    test() : inref<__oly_int32> =
        &this.x
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Automatic deref from function call``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    mutable field x: int32
    new(x: int32) = { x = x }

    test() : inref<__oly_int32> =
        &this.x

derefTest() : () =
    let x = Test(1)
    let w: int32 = x.test() 
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Automatic deref from function call should fail as it is expected to be dereferenced``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    mutable field x: int32
    new(x: int32) = { x = x }

    test() : inref<__oly_int32> =
        &this.x

derefTest() : () =
    let x = Test(1)
    let w: inref<__oly_int32> = x.test() 
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type 'inref<int32>' but is 'int32'."
    ]
    |> ignore

[<Fact>]
let ``Automatic deref from function call should work with including a call to __oly_address_of``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>


struct Test =

    mutable field x: int32
    new(x: int32) = { x = x }

    test() : inref<__oly_int32> =
        &this.x

derefTest() : () =
    let x = Test(1)
    let w: inref<int32> = &x.test() 
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Basic extension``() =
    let src =
        """
open extension Int32Extension

extension Int32Extension = 
    inherits __oly_int32

    test() : () = ()

main() : () =
    let x = 1
    x.test()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Basic alias``() =
    let src =
        """
alias int32 = __oly_int32
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should error on function that takes a physical unit type``() =
    let src =
        """
f(x: ()) : __oly_int32 = 123

main() : () =
    let x = f() // <- this should fail
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected 1 argument(s) but only given 0."
    ]
    |> ignore

[<Fact>]
let ``Should error on function that takes a physical unit type 2``() =
    let src =
        """
test(x: ()) : __oly_int32 = 1234

f() : (()) -> __oly_int32 =
    test

main() : () =
    let g = f()
    let x = g() // <- this should fail
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected 1 argument(s) but only given 0."
    ]
    |> ignore

[<Fact>]
let ``Should error on function that takes a physical unit type 3``() =
    let src =
        """
test(mutable x: ()) : __oly_int32 = 
    x <- ()
    1234

f() : ((()) -> __oly_int32) =
    test

main() : () =
    let x = f()()
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected 1 argument(s) but only given 0."
    ]
    |> ignore

[<Fact>]
let ``Closure should compile``() =
    let src =
        """
test(mutable x: __oly_int32) : () =
    let a = () -> x
    x <- 765
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Simple lambda expression should compile``() =
    let src =
        """
main() : () =
    let a = (x) -> 1
    let result = a(456.0f)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Simple lambda expression should compile 2``() =
    let src =
        """
main() : () =
    let a = x -> y -> 1
    let result = a(456.0f)
    let z : __oly_int32 -> _ = result
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Simple lambda expression should compile 3``() =
    let src =
        """
main() : () =
    let mutable a = x -> y -> 1
    let result = a(456.0f)
    let z : __oly_int32 -> _ = result
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function should compile``() =
    let src =
        """
id<T>(x: T) : T = x

main() : __oly_int32 =
    let a = id<_>
    a(456)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function should compile 2``() =
    let src =
        """
id<T>(x: T) : T = x

test<T>(x: T, f: T -> T) : T = f(x)

main() : __oly_int32 =
    let a = id<_>
    test<_>(456, a)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function should have correct symbol``() =
    let src =
        """
id<T>(x: T) : T = x

main() : __oly_int32 =
    let ~^~a = id<_>
    a(456)
        """
    src |> hasSymbolSignatureTextByCursor "a<T>(x: T): T"

[<Fact>]
let ``Simple partial application of generic id function should have correct symbol 2``() =
    let src =
        """
id<T>(x: T) : T = x

test<T>(x: T, f: T -> T) : T = f(x)

main() : __oly_int32 =
    let ~^~a = id<_>
    test<_>(456, a)
        """
    src |> hasSymbolSignatureTextByCursor "a<T>(x: T): T"

[<Fact>]
let ``Simple partial application of generic id function should have correct symbol 3``() =
    let src =
        """
id<T>(x: T) : T = x

test<T>(x: T, f: T -> T) : T = f(x)

main() : __oly_int32 =
    let a = id<_>
    test<_>(456, ~^~a)
        """
    src |> hasSymbolSignatureTextByCursor "a<T>(x: T): T"

[<Fact>]
let ``Simple partial application of generic id function should have correct symbol 4``() =
    let src =
        """
id<T>(x: T) : T = x

test<T>(x: T, f: T -> T) : T = f(x)

main() : __oly_int32 =
    let a = id<_>
    let r : __oly_float64 = test<_>(456.0, a)
    test<_>(456, ~^~a)
        """
    src |> hasSymbolSignatureTextByCursor "a<T>(x: T): T"

[<Fact>]
let ``Should error with right diagnostics``() =
    let src =
        """
test(y)
        """
    Oly src
    |> withErrorDiagnostics [
        "Type 'y' does not exist in the current scope."
        "The function 'test' must have an implementation."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 2``() =
    let src =
        """
field test
        """
    Oly src
    |> withErrorDiagnostics [
        "The member declaration 'test' must have an explicit type annotation."
        "The field 'test' must be given a default value."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 3``() =
    let src =
        """
field test : __oly_int32
        """
    Oly src
    |> withErrorDiagnostics [
        "The field 'test' must be given a default value."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 4 - NOW PASSES``() =
    let src =
        """
field test : __oly_int32 = 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 5``() =
    let src =
        """
field test : __oly_int32 =
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected 'expression' after '='."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 6``() =
    let src =
        """
test(y: __oly_int32) : = 1
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected 'type' after ':'."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 7``() =
    let src =
        """
test(y: __oly_int32) : () = 
    if (true)
        1
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '()' but is '__oly_int32'."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 7|5``() =
    let src =
        """
test(y: __oly_int32) : () = 
    if (true)
        1
    ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '()' but is '__oly_int32'."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 8``() =
    let src =
        """
test(y: __oly_int32) : () = 
    if (true)
        1
    else
        2
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '()' but is '__oly_int32'."
        "Expected type '()' but is '__oly_int32'."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 9``() =
    let src =
        """
test(y: __oly_int32) : = 
    if (true)
        1
    else
        2.0
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected 'type' after ':'."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 10``() =
    let src =
        """
test(y: __oly_int32) : __oly_int32 = 
    if (true)
        1
    else
        2.0
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '__oly_int32' but is '__oly_float64'."
    ]
    |> ignore

[<Fact>]
let ``Should error with right diagnostics 11``() =
    let src =
        """
test() : () = 
    let f = (x: __oly_int32) -> if (true) 1 else 2.0
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '__oly_int32' but is '__oly_float64'."
    ]
    |> ignore

[<Fact>]
let ``Should compile with certain constraint order``() =
    let src =
        """
test<T>() : () where T : TestTrait<T>, { x: int32 get } = ()
    
interface TestTrait<T> where T : { x: int32 get }

#[intrinsic("int32")]
alias int32
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should compile with certain constraint order 2``() =
    let src =
        """
test<T>() : () where T : { x: int32 get }, TestTrait<T> = ()
    
interface TestTrait<T> where T : { x: int32 get }

#[intrinsic("int32")]
alias int32
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should compile with certain constraint order 3``() =
    let src =
        """
open extension TestExtension
    
interface TestTrait<T> where T : { x: int32 get }
    
test<T>() : () where T : TestTrait<T>, { x: int32 get } = ()
    
#[intrinsic("int32")]
alias int32
    
class Test =
    x: int32 get = 0
    
extension TestExtension =
    inherits Test
    implements TestTrait<Test>
    
main() : () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should compile with certain constraint order 4``() =
    let src =
        """
open extension TestExtension
    
interface TestTrait<T> where T : { x: int32 get }
    
test<T>() : () where T : { x: int32 get }, TestTrait<T> = ()
    
#[intrinsic("int32")]
alias int32
    
class Test =
    x: int32 get = 0
    
extension TestExtension =
    inherits Test
    implements TestTrait<Test>
    
main() : () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should not error on a field with default value assignment``() =
    let src =
        """
class TestData =

    field x: __oly_int32 = 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should error on partial identifier and not crash``() =
    let src =
        """
test(x: __oly_int32.): () = ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected 'an identifier' after '.'."
    ]
    |> ignore

[<Fact>]
let ``Nested type should work in a function signature``() =
    let src =
        """
class Test =

    class NestedTest

test(x: Test.NestedTest): () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested type should work in a function signature 2``() =
    let src =
        """
class Test =

    class NestedTest =

        class NestedTest2

test(x: Test.NestedTest.NestedTest2): () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested type should work in a function signature 3``() =
    let src =
        """
class Test =

    class NestedTest<T>

test(x: Test.NestedTest<__oly_int32>): () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested type should work in a function signature 4``() =
    let src =
        """
class Test<T> =

    class NestedTest<U>

test(x: Test<__oly_int32>.NestedTest<__oly_float32>): () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Parameterized module call should have correct signature``() =
    let src =
        """
module Best<T> =

    test(x: T): () = ()

main(): () =
    Best<__oly_int32>.test(5)
    Best<~^~__oly_float32>.test(1.4f)
    ()
        """
    src
    |> hasSymbolSignatureTextByCursor "__oly_float32"

[<Fact>]
let ``Nested module use should have right signature at the top-most parent``() =
    let src =
        """
module Test1 =

    printTest1(): () = ()

    module Test2 =

        printTest2(): () = ()

main(): () =
    Test1.printTest1()
    ~^~Test1.Test2.printTest2()
        """
    src
    |> hasSymbolSignatureTextByCursor "Test1"

[<Fact>]
let ``Nested type use should have right signature - should not be a constructor``() =
    let src =
        """
class Test1 =

    static printTest1(): () = ()

    class Test2 =

        new() = {}

        static printTest2(): () = ()

main(): () =
    Test1.printTest1()
    Test1.~^~Test2.printTest2()
        """
    src
    |> hasSymbolSignatureTextByCursor "Test2"

[<Fact>]
let ``Nested type use should have right signature - should not be a constructor 2``() =
    let src =
        """
class Test1 =

    class Test2 =

        new() = {}

        printTest2(): () = ()

main(): () =
    Test1.~^~Test2.printTest2()
        """
    src
    |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "Test2"

[<Fact>]
let ``Nested type use should have right signature - should be a constructor``() =
    let src =
        """
class Test1 =

    class Test2 =

        new() = {}

main(): () =
    let x = ~^~Test1.Test2()
        """
    src
    |> hasSymbolSignatureTextByCursor "Test1"

[<Fact>]
let ``Nested types with type arguments should not crash``() =
    let src =
        """
#[import("CLR", "System", "Console")]
class Console =

    static WriteLine(value: __oly_object): ()

class Test1<T> =

    class Test2<U, V> =

        new() = {}

        printTest2(t: T, u: U, v: V): () = 
            Console.WriteLine(t)
            Console.WriteLine(u)
            Console.WriteLine(v)

main(): () =
    let f = Test1<__oly_int32>.Test2<_, _>
    let x = f()
    x.printTest2()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Member 'printTest2' does not exist on type 'Test2<?U, ?V>'."
        ]
    |> ignore

[<Fact>]
let ``Nested types with type arguments should not crash 2``() =
    let src =
        """
#[import("CLR", "System", "Console")]
class Console =

    static WriteLine(value: __oly_object) : ()

class Test1<T> =

    class Test2<U, V> =

        new() = {}

        printTest2(t: T, u: U, v: V) : () = 
            Console.WriteLine(t)
            Console.WriteLine(u)
            Console.WriteLine(v)

main(): () =
    Test1<__oly_int32>.printTest1()
    let f = Test1.Test2
    let x = f()
    x.printTest2()
    ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Member 'printTest1' does not exist on type 'Test1<__oly_int32>'."
            "Identifier 'Test1' not found in scope."
        ]
    |> ignore

[<Fact>]
let ``Nested types with type arguments should not crash 3``() =
    let src =
        """
#[import("CLR", "System", "Console")]
class Console =

    static WriteLine(value: __oly_object) : ()

class Test1<T> =

    class Test2<U, V> =

        new() = {}

        printTest2(t: T, u: U, v: V) : () = 
            Console.WriteLine(t)
            Console.WriteLine(u)
            Console.WriteLine(v)

        class Test3<Z> =

            new() = {}

            print(t: T, u: U, v: V, z: Z) : () =
                Console.WriteLine(t)
                Console.WriteLine(u)
                Console.WriteLine(v)
                Console.WriteLine(z)

main(): () =
    let x = Test1<__oly_int32>.Test2<__oly_float32, __oly_utf16>.()
    x.printTest2(1, 2.3f, "Hello World!")
    ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Expected 'an identifier' after '.'."
        ]
    |> ignore

[<Fact>]
let ``Nested types with type arguments should not crash 4``() =
    let src =
        """
#[import("CLR", "System", "Console")]
class Console =

    static WriteLine(value: __oly_object) : ()

class Test1<T> =

    class Test2<U, V> =

        new() = {}

        printTest2(t: T, u: U, v: V) : () = 
            Console.WriteLine(t)
            Console.WriteLine(u)
            Console.WriteLine(v)

        class Test3<Z> =

            new() = {}

            print(t: T, u: U, v: V, z: Z) : () =
                Console.WriteLine(t)
                Console.WriteLine(u)
                Console.WriteLine(v)
                Console.WriteLine(z)

main(): () =
    let x = Test1<__oly_int32>.Test2<__oly_float32, __oly_utf16>.Test3<_>
    x.printTest2(1, 2.3f, "Hello World!")
    ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            // It's not in scope because `x` becomes a real function and can only be called like a real function.
            "Identifier 'x' not found in scope."
        ]
    |> ignore

[<Fact>]
let ``Nested types with type arguments should not crash 4 with mutable``() =
    let src =
        """
#[import("CLR", "System", "Console")]
class Console =

    static WriteLine(value: __oly_object) : ()

class Test1<T> =

    class Test2<U, V> =

        new() = {}

        printTest2(t: T, u: U, v: V) : () = 
            Console.WriteLine(t)
            Console.WriteLine(u)
            Console.WriteLine(v)

        class Test3<Z> =

            new() = {}

            print(t: T, u: U, v: V, z: Z) : () =
                Console.WriteLine(t)
                Console.WriteLine(u)
                Console.WriteLine(v)
                Console.WriteLine(z)

main(): () =
    let mutable x = Test1<__oly_int32>.Test2<__oly_float32, __oly_utf16>.Test3<_>
    x.printTest2(1, 2.3f, "Hello World!")
    ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Member 'printTest2' does not exist on type '() -> Test3<?Z>'."
        ]
    |> ignore

[<Fact>]
let ``Nested types with type arguments should compile``() =
    let src =
        """
#[import("CLR", "System", "Console")]
class Console =

    static WriteLine(value: __oly_object) : ()

class Test1<T> =

    class Test2<U, V> =

        new() = {}

        printTest2(t: T, u: U, v: V) : () = 
            Console.WriteLine(t)
            Console.WriteLine(u)
            Console.WriteLine(v)

main(): () =
    let x = Test1<__oly_int32>.Test2<__oly_float64, __oly_utf16>()
    x.printTest2(1, 2.3, "Hello World!")
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested types with type arguments should compile with right signature``() =
    let src =
        """
#[import("CLR", "System", "Console")]
class Console =

    static WriteLine(value: __oly_object) : ()

class Test1<T> =

    class Test2<U, V> =

        new() = {}

        printTest2(t: T, u: U, v: V) : () = 
            Console.WriteLine(t)
            Console.WriteLine(u)
            Console.WriteLine(v)

main(): () =
    let x = Test1<__oly_int32>.Test2<~^~__oly_float64, __oly_utf16>()
    x.printTest2(1, 2.3, "Hello World!")
        """
    src
    |> hasSymbolSignatureTextByCursor "__oly_float64"

[<Fact>]
let ``Should infer correctly but still error``() =
    let src =
        """
open extension Int32Extension

interface TraitTest

class Test<T> where T: TraitTest =

    new() = { }

extension Int32Extension =
    inherits __oly_int32
    implements TraitTest

main(): () =
    let x: Test<__oly_int32> = Test<_>()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation '__oly_int32' is missing the constraint 'TraitTest'.", """
    let x: Test<__oly_int32> = Test<_>()
                ^^^^^^^^^^^
"""
            )
            ("Type instantiation '__oly_int32' is missing the constraint 'TraitTest'.", """
    let x: Test<__oly_int32> = Test<_>()
                                    ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should infer correctly but still error 2``() =
    let src =
        """
open extension Int32Extension

interface TraitTest

class Test<T> where T: TraitTest =

    new() = { }

extension Int32Extension =
    inherits __oly_int32
    implements TraitTest

main(): () =
    let x: Test<__oly_int32> = Test()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation '__oly_int32' is missing the constraint 'TraitTest'.", """
    let x: Test<__oly_int32> = Test()
                ^^^^^^^^^^^
"""
            )
            ("Type instantiation '__oly_int32' is missing the constraint 'TraitTest'.", """
    let x: Test<__oly_int32> = Test()
                               ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should infer correctly but still error 3``() =
    let src =
        """
open extension Int32Extension

interface TraitTest

class Test<T> where T: TraitTest =

    new() = { }

extension Int32Extension =
    inherits __oly_int32
    implements TraitTest

main(): () =
    let mutable x: Test<_> = Test()

    x <- Test<__oly_int32>()

        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation '__oly_int32' is missing the constraint 'TraitTest'.", """
    x <- Test<__oly_int32>()
              ^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should infer correctly``() =
    let src =
        """
class Test =

    new() = { }

class Test<T> =

    new() = { }

main(): () =
    let x: Test = Test()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should infer correctly 2``() =
    let src =
        """
class Test =

    new() = { }

class Test<T> =

    new() = { }

main(): () =
    let x = Test()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should infer correctly 3``() =
    let src =
        """
class Test =

    new() = { }

class Test<T> =

    new() = { }

test(t: Test<__oly_int32>): () = ()

main(): () =
    test(Test())
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should infer correctly reversed``() =
    let src =
        """
class Test<T> =

    new() = { }

class Test =

    new() = { }

main(): () =
    let x: Test = Test()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``File module should have correct symbols under it``() =
    let src =
        """
module Test

class TT =
    static test(x: __oly_int32): () =
        ()

main(): () =
    ~^~TT.test(1)
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "TT"

[<Fact>]
let ``File module should have correct symbols under it 2``() =
    let src =
        """
// a comment

module Test

class TT =
    static test(x: __oly_int32): () =
        ()

main(): () =
    ~^~TT.test(1)
        """
    src |> hasSymbolSignatureTextByCursorIgnoreDiagnostics "TT"

[<Fact>]
let ``Second order generics test 1``() =
    let src =
        """
test<T<_>>(): () = ()

class Test<G<_>> =

    static m(): () =
        test<G>(): ()
        """

    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class has a generic field``() =
    let src =
        """
class Test<T> =

    field x: T

    new(x: T) = { x = x }
        """

    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class has a generic field with default``() =
    let src =
        """
class Test<T> where T: struct =

    field x: T = default
        """

    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Default should have an inference error``() =
    let src =
        """
test(): () =
    let x = default
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Unable to infer type at this location.", "
    let x = default
            ^^^^^^^
")
        ]
    |> ignore

[<Fact>]
let ``Type can have a mutable field for the shape``() =
    let src =
        """
struct TestStruct =
    x: __oly_int32 get, set
    new(x: __oly_int32) = { x = x }

test<T>(t: T): __oly_int32 where T: { x: __oly_int32 get } = t.x

main(): () =
    let ts = TestStruct(123)
    let result = test<_>(ts)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Type must have a mutable field for the shape``() =
    let src =
        """
struct TestStruct =
    x: __oly_int32 get
    new(x: __oly_int32) = { x = x }

test<T>(t: T): __oly_int32 where T: { x: __oly_int32 get, set } = t.x

main(): () =
    let ts = TestStruct(123)
    let result = test<_>(ts)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Shape member 'set_x(__oly_int32): ()' does not exist on 'TestStruct'.", "
    let result = test<_>(ts)
                      ^
")
        ]
    |> ignore

[<Fact>]
let ``Struct type that is instantiated as immutable cannot call a mutable function on it``() =
    let src =
        """
struct TestStruct =
    mutable field x: __oly_int32

    mutable Change(): () =
        this.x <- 100

    new(x: __oly_int32) = { x = x }

main(): () =
    let ts = TestStruct(123)
    ts.Change()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'ts' is not mutable.", "
    ts.Change()
    ^^
"           )
            ("Function call 'Change' is not read-only and cannot be called on an immutable struct instance.", "
    ts.Change()
       ^^^^^^
"           )
        ]
    |> ignore

[<Fact>]
let ``Simple array``() =
    let src =
        """
main(): () =
    let x: (__oly_int32[], __oly_int32[]) = ([1], [1])
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance``() =
    let src =
        """
open extension Int32TestExtension

#[intrinsic("int32")]
alias int32

interface ITest =
 
    default test(): int32 = 567

extension Int32TestExtension =
    inherits int32
    implements ITest

getResult(x: ITest): int32 =
    x.test()

#[intrinsic("cast")]
cast<T>(__oly_object): T

main(): () =
    let result = getResult(cast<ITest>(123))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 2``() =
    let src =
        """
open extension Int32TestExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

interface ITest =
 
    default test(): int32 = 567

extension Int32TestExtension =
    inherits int32
    implements ITest

getResult(x: ITest): int32 =
    x.test()

getResult2(x: byref<int32>): int32 =
    getResult(cast<ITest>(x))

#[intrinsic("cast")]
cast<T>(__oly_object): T

main(): () =
    let result = getResult(cast<ITest>(123))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Extended type should have access to the extension functions of an implemented interface``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

interface ITest =
 
    test(): int32

#[open]
extension Int32TestExtension =
    inherits int32
    implements ITest

    test(): int32 = 1234

main(): () =
    let result = 1
    let result = result.test()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Auto generalized partially applied function should have correct signature``() =
    """
interface IExample

test<T>(x: T): () where T: IExample = ()

main(): () =
    let ~^~f = test<_>
    """
    |> hasSymbolSignatureTextByCursor "f<T>(x: T): () where T: IExample"
    |> ignore

[<Fact>]
let ``Should have correct signature when referencing a field in module that is in a namespace``() =
    """
namespace TestNamespace

#[intrinsic("utf16")]
alias string

module TestModule =

    field TestField: string = "test"

    main(): () =
        let result = ~^~TestField
    """
    |> hasSymbolSignatureTextByCursor "static field TestField: string"
    |> ignore

[<Fact>]
let ``Should have correct signature when referencing a field in module that is in a namespace 2``() =
    """
namespace TestNamespace

#[intrinsic("utf16")]
alias string

struct TestStruct =
    public field TestStructField: string = "test"

module TestModule =

    field TestModuleField: TestStruct = TestStruct()

    main(): () =
        let result = ~^~TestModuleField.TestStructField
    """
    |> hasSymbolSignatureTextByCursor "static field TestModuleField: TestStruct"
    |> ignore

[<Fact>]
let ``Witness pass for a type argument on a type should error``() =
    let src =
        """
open extension Int32Extension

class Test<T> where T: Add<T, T, T> =

    new() = { }
    add(x: T, y: T) : T = T.add(x, y)

interface Add<T1, T2, T3> =

    static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

#[intrinsic("int32")]
alias int32

extension Int32Extension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32): int32 = __oly_add(x, y)

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = Test<int32>()
    let r = x.add(1, 2)
    print(r)
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation 'int32' is missing the constraint 'Add<int32, int32, int32>'."
            "Type instantiation 'int32' is missing the constraint 'Add<int32, int32, int32>'."
        ]
    |> ignore

[<Fact>]
let ``Number inference negation overload``() =
    let src =
        """
#[intrinsic("int8")]
alias int8

#[intrinsic("int32")]
alias int32

#[intrinsic("negate")]
(-)(int8): int8

#[intrinsic("negate")]
(-)(int32): int32

main(): () =
    let result: int8 = -1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Number inference negation overload 2``() =
    let src =
        """
#[intrinsic("int8")]
alias int8

#[intrinsic("int32")]
alias int32

#[intrinsic("negate")]
(-)(int8): int8

#[intrinsic("negate")]
(-)(int32): int32

#[intrinsic("add")]
(+)(int8, int8): int8

#[intrinsic("add")]
(+)(int32, int32): int32

main(): () =
    let result: int8 = 1 + 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Number inference negation overload 3``() =
    let src =
        """
#[intrinsic("int8")]
alias int8

#[intrinsic("int32")]
alias int32

#[intrinsic("negate")]
(-)(int8): int8

#[intrinsic("negate")]
(-)(int32): int32

#[intrinsic("add")]
(+)(int8, int8): int8

#[intrinsic("add")]
(+)(int32, int32): int32

main(): () =
    let result: int8 = 1 + -1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Negation in parenthesis``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("negate")]
(-)(int32): int32

#[intrinsic("add")]
(+)(int32, int32): int32

main(): () =
    let result = 1 + (-1)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Back quote identifier``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("negate")]
(-)(int32): int32

#[intrinsic("add")]
(+)(int32, int32): int32

main(): () =
    let `result` = 1 + (-1)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should get symbol on open declaration``() =
    let src =
        """
open static ~^~Test
module Test
        """
    src
    |> hasSymbolSignatureTextByCursor "Test"

[<Fact>]
let ``Open static declaration should error when not using wild cards for all type arguments``() =
    let src =
        """
open static A<_, __oly_int32, _>

class A<T1, T2, T3> =

    static M(): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Open declarations using one or more wild cards, '_', requires using wild cards for all type arguments.",
                """
open static A<_, __oly_int32, _>
             ^^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Open static declaration should error when an inner type is not solved``() =
    let src =
        """
open static A<__oly_int32, __oly_int32, B<_>>

class B<T>

class A<T1, T2, T3> =

    static M(): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Inferring types are not allowed in this context, be explicit.",
                """
open static A<__oly_int32, __oly_int32, B<_>>
                                          ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Array get length should compile``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("get_length")]
getLength<T>(T[]): int32

main(): () =
    let result = getLength<_>([1])
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Array and mutable array get length should compile``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("get_length")]
getLength<T>(T[]): int32

#[intrinsic("get_length")]
getLength<T>(mutable T[]): int32

main(): () =
    let result = getLength<_>([1])
    let result = getLength<_>(mutable [1])
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Index getter call``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("get_element")]
(`[]`)<T>(T[], int32): T

test(xs: int32[]): () =
    let x = xs[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Index getter call 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("get_element")]
(`[]`)<T>(T[], int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], int32): T

test(xs: int32[]): () =
    let x = xs[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Index getter call 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("get_element")]
(`[]`)<T>(T[], int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], int32): T

shape DotNetIndexGetter<T> =

    get_Item(int32): T

(`[]`)<T1, T2>(x: T1, index: int32): T2 where T1: DotNetIndexGetter<T2> = x.get_Item(index)

test(xs: int32[]): () =
    let x = xs[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Index getter call 4``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("get_element")]
(`[]`)<T>(T[], int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], int32): T

#[intrinsic("equal")]
(==)(int32, int32): bool

shape DotNetIndexGetter<T> =

    get_Item(int32): T

(`[]`)<T1, T2>(x: T1, index: int32): T2 where T1: DotNetIndexGetter<T2> = x.get_Item(index)

test(xs: int32[]): () =
    if (xs[0] == 1)
        ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Index setter call``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], int32, T): ()

test(xs: mutable int32[]): () =
    xs[0] <- 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Invalid case definitions``() =
    let src =
        """
class Test =
    | A = 1
    | B
    | C
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Only 'enum' types can define cases."
        ]
    |> ignore

[<Fact>]
let ``Enum types require cases``() =
    let src =
        """
enum Test
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Enum declaration must specify one or more cases."
        ]
    |> ignore

[<Fact>]
let ``Enum types require cases 2``() =
    let src =
        """
enum Test =

    test(): () = ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Enum declaration must specify one or more cases."
            "Instance member not valid on an 'enum' type."
        ]
    |> ignore

[<Fact>]
let ``Should error but not crash``() =
    let src =
        """
namespace Test

sealed

sealed interface Test1
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Expected 'type declaration name' after 'type declaration kind'."
            "Expected 'interface' after 'sealed'."
        ]
    |> ignore

[<Fact>]
let ``Should error but not crash 2``() =
    let src =
        """
namespace Test

seale

sealed interface Test1
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Values cannot be declared on a namespace."
            "The member declaration 'seale' must have an explicit type annotation."
        ]
    |> ignore

[<Fact>]
let ``let! example should compile``() =
    let src =
        """
module Test

#[null]
class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

(let!)<T<_>, A, B>(a: T<A>, f: A -> T<B>): T<B> where T<_>: null = default

(return)<T<_>, A>(a: A): T<A> where T<_>: null = default

test1(x: Wrapper<__oly_int32>): Wrapper<__oly_float32> =
    let! result: __oly_float32 = x
    return 1.0f
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``let! example should compile 2``() =
    let src =
        """
module Test

class Wrapper<T> =

    public field Value: T

    new(value: T) = { Value = value }

(let!)<A, B>(a: Wrapper<A>, f: A -> Wrapper<B>): Wrapper<B> =
    f(a.Value)

(return)<A>(a: A): Wrapper<A> =
    Wrapper(a)

test1(x: Wrapper<__oly_int32>): Wrapper<__oly_int32> =
    let! result = x
    return result
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``let! example should compile 3``() =
    let src =
        """
module Test

class Wrapper<T> =

    public field Value: T

    new(value: T) = { Value = value }

(let!)<A, B>(a: Wrapper<A>, f: A -> Wrapper<B>): Wrapper<B> =
    f(a.Value)

(return)<A>(a: A): Wrapper<A> =
    Wrapper(a)

test1(x: Wrapper<__oly_int32>): Wrapper<__oly_int32> =
    let! result = x
    let! result2 = x
    return result
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Indexer operator example with struct``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Indexer operator example with struct 2``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Indexer operator example with struct 3``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

struct Test =

    mutable field X: __oly_int32 = 1

    mutable get_Item(index: __oly_int32): byref<__oly_int32> = &this.X

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)
(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)

main(): () =
    let mutable s = Test()
    (s[0]) <- 5
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Indexer operator example with struct should error``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Shape member 'get_Item(TKey): TValue' does not exist on 'byref<Test<__oly_int32>>'."
        ]
    |> ignore

[<Fact>]
let ``Indexer operator example with struct should error 2``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Expected type 'byref<Test<__oly_int32>>' but is 'inref<Test<__oly_int32>>'."
        ]
    |> ignore

[<Fact>]
let ``Indexer operator example with struct should error 3``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

struct Test<T> where T: struct =

    mutable get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Shape member 'get_Item(TKey): TValue' does not exist on 'Test<__oly_int32>'."
        ]
    |> ignore

[<Fact>]
let ``Indexer operator example with struct should error 4``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x: __oly_int32 = s[0]
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Shape member 'get_Item(TKey): TValue' does not exist on 'byref<Test<__oly_int32>>'."
        ]
    |> ignore

[<Fact>]
let ``Indexer operator example with class``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

class Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Indexer operator example with class 2``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

class Test<T> where T: struct =

    mutable field X: T = default

    get_Item(index: __oly_int32): byref<T> = &this.X

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let ~^~x = &s[0]
        """
    src
    |> hasSymbolSignatureTextByCursor "x: byref<__oly_int32>"

[<Fact>]
let ``Indexer operator example with class 3``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

class Test<T> where T: struct =

    mutable field X: T = default

    get_Item(index: __oly_int32): byref<T> = &this.X

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let ~^~x = s[0]
        """
    src
    |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``Indexer operator example with class should error``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

class Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Expected type 'byref<?T>' but is 'Test<__oly_int32>'."
        ]
    |> ignore

[<Fact>]
let ``Get symbol of type in cast``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =

    B(): ()

    default A(): () =
        this.B()

struct Test =
    implements IA
    
    public mutable field X: __oly_int32 = 3

    mutable B(): () =
        this.X <- 5

main(): () =
    let x = Test()
    (x: ~^~IA).A()
    print(x.X)
        """
    src |> hasSymbolSignatureTextByCursor "IA"

[<Fact>]
let ``Get symbol of value in cast``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =

    B(): ()

    default A(): () =
        this.B()

struct Test =
    implements IA
    
    public mutable field X: __oly_int32 = 3

    mutable B(): () =
        this.X <- 5

main(): () =
    let x = Test()
    (~^~x: IA).A()
    print(x.X)
        """
    src |> hasSymbolSignatureTextByCursor "x: Test"

[<Fact>]
let ``Get symbol of function from cast``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =

    B(): ()

    default A(): () =
        this.B()

struct Test =
    implements IA
    
    public mutable field X: __oly_int32 = 3

    mutable B(): () =
        this.X <- 5

main(): () =
    let x = Test()
    (x: IA).~^~A()
    print(x.X)
        """
    src |> hasSymbolSignatureTextByCursor "A(): ()"

[<Fact>]
let ``Get type parameter symbol of nested type``() =
    let src =
        """
class A<T, U> =

    class B<~^~V> 
        """
    src |> hasSymbolSignatureTextByCursor "V"

[<Fact>]
let ``Should error on premodifier``() =
    let src =
        """
class Test =

    default abstract M(): () = ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Invalid use of 'abstract' premodifier."
        ]
    |> ignore

[<Fact>]
let ``Should error on premodifier 2``() =
    let src =
        """
class Test =

    abstract abstract default M(): () = ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Invalid use of 'abstract' premodifier."
        ]
    |> ignore

[<Fact>]
let ``Should error on premodifier 3``() =
    let src =
        """
class Test =

    abstract overrides M(): () = ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "'abstract' and 'overrides' premodifiers cannot be used together."
            "'abstract' and 'overrides' premodifiers cannot be used together."
            "The function 'M(): ()' cannot find a function to override."
        ]
    |> ignore

[<Fact>]
let ``Get hash directive symbol signature``() =
    let src = "#load ~^~\"test.oly\""
    src |> hasSymbolSignatureTextByCursor "#load \"test.oly\""

[<Fact>]
let ``Get hash directive symbol signature 2``() =
    let src =
        """
#load ~^~"test.oly"
        """
    src |> hasSymbolSignatureTextByCursor "#load \"test.oly\""

[<Fact>]
let ``Get hash directive symbol signature 3``() =
    let src =
        """
#load ~^~"test.oly"

main(): () = ()
        """
    src |> hasSymbolSignatureTextByCursor "#load \"test.oly\""

[<Fact>]
let ``Should not cause an infinite loop in the compiler``() =
    let src =
        """
module HelloWorld

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class Test =

    PrintHelloWorld(): () = print("Hello World!")

    get x: int32
    new() = { x = 5 }  

class Test2 =

    get x: Test = Test()

main(): () =    
    let t2 = Test2()
    let test = x ->
1
    let t = t2.x
    t.PrintHelloWorld()
        """
    Oly src
    |> hasErrorDiagnostics

[<Fact>]
let ``Export example 1``() =
    let src =
        """
namespace Test

#[export]
class GenericType<Z> where Z: struct =

    Test<U>(u: U): Z = default

module Test =
    main(): () =
      let x = GenericType<__oly_int32>()
      ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should error when using open declaration in wrong place``() =
    let src =
        """
class Test

open static Test
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Top-level open declarations can only be declared in modules or namespaces."
        ]
    |> ignore

[<Fact>]
let ``Should error when using open declaration in wrong place 2``() =
    let src =
        """
class Test =
    open static Test
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Top-level open declarations can only be declared in modules or namespaces."
        ]
    |> ignore

[<Fact>]
let ``Tuple with named elements should compile``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

test(): (x1: int32, x2: int32) = (1, 2)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Tuple with named elements should compile 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

test(): (int32, x2: int32) = (1, 2)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Tuple with named elements should compile 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

test(): (int32: int32, int32) = (1, 2)
        """
    Oly src
    |> shouldCompile
    |> ignore
   
[<Fact>]
let ``Tuple with named elements should have correct signature``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

~^~test(): (x1: int32, x2: int32) = (1, 2)
        """
    src |> hasSymbolSignatureTextByCursor "static test(): (x1: int32, x2: int32)"

[<Fact>]
let ``Tuple with named elements should have correct signature 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

~^~test(): (int32, x2: int32) = (1, 2)
        """
    src |> hasSymbolSignatureTextByCursor "static test(): (int32, x2: int32)"

[<Fact>]
let ``Tuple with named elements should have correct signature 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

~^~test(): (x1: int32, int32) = (1, 2)
        """
    src |> hasSymbolSignatureTextByCursor "static test(): (x1: int32, int32)"

[<Fact>]
let ``Second-order generic constraint type parameter enclosing instantiation``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

interface IMemory<T> =

    static abstract Allocate(): IMemory<T>

struct Memory<T> =
    implements IMemory<T>

    static overrides Allocate(): IMemory<T> =
        Memory<T>()

struct Chunk<TMemory<_>> where TMemory<_>: IMemory =

    field X: int32
    new() =
        let m = TMemory<int32>.Allocate()
        {
            X = 1
        }
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex test should compile``() =
    let src =
        """
module Oly.Entities

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("bool")]
alias bool

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T
#[intrinsic("get_element")]
(`[,]`)<T>(T[,], index1: int32, index2: int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()
#[intrinsic("get_element")]
(`[,]`)<T>(mutable T[,], index1: int32, index2: int32): T
#[intrinsic("set_element")]
(`[,]`)<T>(mutable T[,], index1: int32, index2: int32, T): ()

module Array =

    #[intrinsic("new_array")]
    ZeroCreate<T>(size: int32): mutable T[]

interface IMemory<T> where T: struct =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

    static abstract Allocate(size: int32): IMemory<T>

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    private field Buffer: mutable T[]

    private new(buffer: mutable T[]) =
        {
            Buffer = buffer
        }

    get_Item(index: int32): T = this.Buffer[index]
    set_Item(index: int32, item: T): () = this.Buffer[index] <- item

    static overrides Allocate(size: int32): IMemory<T> = 
        DefaultMemory(Array.ZeroCreate<T>(size))

interface IComponent

private struct Chunk1<TMemory<_>, TComponent1> 
    where TMemory<_>: IMemory;
    where TComponent1: IComponent, struct 
    =

    public field Lookup: TMemory<int32>
    public field Data1: TMemory<TComponent1>

    new(lookup: TMemory<int32>, data1: TMemory<TComponent1>) =
        {
           Lookup = lookup
           Data1 = data1 
        }

private struct Chunk2<TMemory<_>, TComponent1, TComponent2> 
    where TMemory<_>: IMemory;
    where TComponent1: IComponent, struct;
    where TComponent2: IComponent, struct
    =

    public field Lookup: TMemory<int32>
    public field Data1: TMemory<TComponent1>
    public field Data2: TMemory<TComponent2>

    new(lookup: TMemory<int32>, data1: TMemory<TComponent1>, data2: TMemory<TComponent2>) =
        {
           Lookup = lookup
           Data1 = data1
           Data2 = data2
        }

private struct IndexQueue<TMemory<_>> where TMemory<_>: IMemory =

    public mutable field Indices: TMemory<int32>
    public mutable field Count: int32

    new(indices: TMemory<int32>, count: int32) =
        {
            Indices = indices
            Count = count
        }

    mutable Enqueue(index: int32): () =
        ()

    mutable TryDequeue(index: byref<int32>): bool =
        true

        
test(): () =
    let m = DefaultMemory<int32>(mutable [])
    let x = IndexQueue<DefaultMemory>(m, 0)
    ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex test should compile 2``() =
    let src =
        """
module Oly.Entities

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("bool")]
alias bool

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T
#[intrinsic("get_element")]
(`[,]`)<T>(T[,], index1: int32, index2: int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()
#[intrinsic("get_element")]
(`[,]`)<T>(mutable T[,], index1: int32, index2: int32): T
#[intrinsic("set_element")]
(`[,]`)<T>(mutable T[,], index1: int32, index2: int32, T): ()

#[intrinsic("get_length")]
getLength<T>(mutable T[]): int32

#[intrinsic("print")]
print(__oly_object): ()

module Array =

    #[intrinsic("new_array")]
    ZeroCreate<T>(size: int32): mutable T[]

interface IMemory<T> where T: struct =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

    Length: int32 get

interface IMemoryAllocator<TMemory<_>> where TMemory<_>: IMemory =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    private field Buffer: mutable T[]

    private new(buffer: mutable T[]) =
        {
            Buffer = buffer
        }

    get_Item(index: int32): T = this.Buffer[index]
    set_Item(index: int32, item: T): () = this.Buffer[index] <- item

    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

interface IComponent

internal struct IndexQueue<TMemory<_>, TMemoryAllocator> 
    where TMemory<_>: IMemory; 
    where TMemoryAllocator: IMemoryAllocator<TMemory> 
    =

    public mutable field Indices: TMemory<int32>
    public mutable field Count: int32

    new(indices: TMemory<int32>, count: int32) =
        {
            Indices = indices
            Count = count
        }

    new() =
        {
            Indices = TMemoryAllocator.Allocate(8)
            Count = 0
        }

    mutable Enqueue(index: int32): () =
        ()

    mutable TryDequeue(index: byref<int32>): bool =
        true

    GetFirst(): int32 = this.Indices.get_Item(0)

        
internal test(): IndexQueue<DefaultMemory, DefaultMemoryAllocator> =
    let x = IndexQueue<DefaultMemory, DefaultMemoryAllocator>()
    print(x.GetFirst())
    x
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex test should fail``() =
    let src =
        """
module Oly.Entities

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("bool")]
alias bool

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T
#[intrinsic("get_element")]
(`[,]`)<T>(T[,], index1: int32, index2: int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()
#[intrinsic("get_element")]
(`[,]`)<T>(mutable T[,], index1: int32, index2: int32): T
#[intrinsic("set_element")]
(`[,]`)<T>(mutable T[,], index1: int32, index2: int32, T): ()

#[intrinsic("get_length")]
getLength<T>(mutable T[]): int32

#[intrinsic("print")]
print(__oly_object): ()

module Array =

    #[intrinsic("new_array")]
    ZeroCreate<T>(size: int32): mutable T[]

interface IMemory<T> where T: struct =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

    Length: int32 get

interface IMemoryAllocator<TMemory<_>> where TMemory<_>: IMemory =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    private field Buffer: mutable T[]

    private new(buffer: mutable T[]) =
        {
            Buffer = buffer
        }

    get_Item(index: int32): T = this.Buffer[index]
    set_Item(index: int32, item: T): () = this.Buffer[index] <- item

    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator

interface IComponent

private struct IndexQueue<TMemory<_>, TMemoryAllocator> 
    where TMemory<_>: IMemory; 
    where TMemoryAllocator: IMemoryAllocator<TMemory> 
    =

    public mutable field Indices: TMemory<int32>
    public mutable field Count: int32

    new() =
        {
            Indices = TMemoryAllocator.Allocate(8)
            Count = 0
        }
        
test(): IndexQueue<DefaultMemory, DefaultMemoryAllocator> =
    IndexQueue<DefaultMemory, DefaultMemoryAllocator>()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'DefaultMemoryAllocator' is missing the constraint 'IMemoryAllocator<DefaultMemory>'.",
                """
test(): IndexQueue<DefaultMemory, DefaultMemoryAllocator> =
                                  ^^^^^^^^^^^^^^^^^^^^^^
"""
            )
            ("Type instantiation 'DefaultMemoryAllocator' is missing the constraint 'IMemoryAllocator<DefaultMemory>'.",
                """
    IndexQueue<DefaultMemory, DefaultMemoryAllocator>()
                              ^^^^^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    mutable field Buffer: mutable int32[] = unchecked default

    mutable A(): () =
        let z: byref<_> = &this.Buffer
        z[0] <- 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass 2``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)
(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)

interface IMemory<T> where T: struct =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

struct Test<TMemory<_>> where TMemory<_>: IMemory =

    mutable field Buffer: TMemory<int32>

    new(buffer: TMemory<int32>) =
        {
            Buffer = buffer
        }

    mutable A(): () =
        let z = &this.Buffer
        z[0] <- 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass 3``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)

interface IMemory<T> where T: struct =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

struct Test<TMemory<_>> where TMemory<_>: IMemory =

    mutable field Buffer: TMemory<int32>

    new(buffer: TMemory<int32>) =
        {
            Buffer = buffer
        }

    mutable A(): () =
        this.Buffer[0] <- 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass 4``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)

interface IMemory<T> where T: struct =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

struct Test<TMemory<_>> where TMemory<_>: IMemory =

    mutable field Buffer: TMemory<int32>

    new(buffer: TMemory<int32>) =
        {
            Buffer = buffer
        }

    mutable A(): () =
        this.Buffer[0] <- 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass 5``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)

interface IMemory<T> where T: struct =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

struct Test<TMemory<_>> where TMemory<_>: IMemory =

    mutable field Buffer: TMemory<int32>

    new(buffer: TMemory<int32>) =
        {
            Buffer = buffer
        }

    mutable A(): () =
        this.Buffer[0] <- 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass 6``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): byref<T>

test(xs: mutable int32[]): () =
    let z: int32 = xs[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass 7``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

test(xs: mutable int32[]): () =
    let z: byref<int32> = &xs[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass 8``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

test(xs: mutable int32[]): () =
    let z: int32 = xs[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref of field indexer should pass 9``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

test(xs: mutable int32[]): int32 = xs[0]
test(xs: mutable int32[]): byref<int32> = &xs[0]

test2(xs: mutable int32[]): () =
    // The non-byref return takes precedence.
    let z = test(xs)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref indexer should pass``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): byref<TValue> where T: { mutable get_Item(TKey): byref<TValue> } = &x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): inref<TValue> where T: { get_Item(TKey): inref<TValue> } = &x.get_Item(key)

interface ITest<T> =

    get_Item(index: int32): T
    get_Item(index: int32): byref<T>

struct TestStruct =
    implements ITest<int32>

    mutable field Value: int32 = 0

    get_Item(index: int32): int32 =
        index

    mutable get_Item(index: int32): byref<int32> =
        &this.Value

    get_Item(index: int32): inref<int32> =
        &this.Value

test(): () =
    let mutable t = TestStruct()
    let x: int32 = t[0]
    let y: byref<int32> = &t[0]
    let z: inref<int32> = &t[0]
    let w = &t[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Byref indexer should have right symbol``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): byref<TValue> where T: { mutable get_Item(TKey): byref<TValue> } = &x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): inref<TValue> where T: { get_Item(TKey): inref<TValue> } = &x.get_Item(key)

interface ITest<T> =

    get_Item(index: int32): T
    get_Item(index: int32): byref<T>

struct TestStruct =
    implements ITest<int32>

    mutable field Value: int32 = 0

    get_Item(index: int32): int32 =
        index

    mutable get_Item(index: int32): byref<int32> =
        &this.Value

    get_Item(index: int32): inref<int32> =
        &this.Value

test(): () =
    let mutable t = TestStruct()
    let w = ~^~&t[0]
        """
    src |> hasSymbolSignatureTextByCursor "(&)<T>(T): byref<T>"

[<Fact>]
let ``Byref indexer should have right symbol 2``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): byref<TValue> where T: { mutable get_Item(TKey): byref<TValue> } = &x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): inref<TValue> where T: { get_Item(TKey): inref<TValue> } = &x.get_Item(key)

interface ITest<T> =

    get_Item(index: int32): T
    get_Item(index: int32): byref<T>

struct TestStruct =
    implements ITest<int32>

    mutable field Value: int32 = 0

    get_Item(index: int32): int32 =
        index

    mutable get_Item(index: int32): byref<int32> =
        &this.Value

    get_Item(index: int32): inref<int32> =
        &this.Value

test(): () =
    let t = TestStruct()
    let w = ~^~&t[0]
        """
    src |> hasSymbolSignatureTextByCursor "(&)<T>(T): inref<T>"

[<Fact>]
let ``Using a wild card in a type parameter definition should fail``() =
    let src =
        """
interface ITest<_>
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Invalid use of wild card in type parameter definition.",
                """
interface ITest<_>
                ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Indexer set call should fail``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)

interface IMemory<T> where T: struct =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

struct Test<TMemory<_>> where TMemory<_>: IMemory =

    public mutable field Buffer: TMemory<int32>

    new(buffer: TMemory<int32>) =
        {
            Buffer = buffer
        }

    mutable A(): () =
        this.Buffer[0] <- (1.0: __oly_float32)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'TMemory<int32>' is missing the constraint '{ set_Item(int32, __oly_float32): () }'.",
                """
        this.Buffer[0] <- (1.0: __oly_float32)
                   ^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Assignment with a generic should fail``() =
    let src =
        """
test<T>(x: T): () =
    let y: __oly_int32 = x

        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '__oly_int32' but is 'T'.",
                """
    let y: __oly_int32 = x
                         ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Property has the right signature``() =
    let src =
        """
class Test =

    ~^~Length: __oly_int32 
        get() = 0
        """
    src |> hasSymbolSignatureTextByCursor "Length: __oly_int32 get"

[<Fact>]
let ``Module with nested struct should pass``() =
    let src =
        """
namespace Oly

module Entities =

    struct EntityId =

        field Index: __oly_int32
        field Version: __oly_uint32

        new(index: __oly_int32, version: __oly_uint32) =
            {
                Index = index
                Version = version
            }

        static None: EntityId get = EntityId(0, 0)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Module with nested struct should pass 2``() =
    let src =
        """
namespace Oly

module Entities =

    struct EntityId =

        field Index: __oly_int32
        field Version: __oly_uint32

        new(index: __oly_int32, version: __oly_uint32) =
            {
                Index = index
                Version = version
            }

        static None: EntityId get = let x: EntityId = EntityId(0, 0)
                                    x
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Lambda return type should fail``() =
    let src =
        """
test(x: __oly_int32 -> ()): () =
    x(0)

test2(): () =
    test(x -> 1)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '()' but is '__oly_int32'.",
                """
    test(x -> 1)
              ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Binary literal should get correct signature``() =
    let src =
        """
test(): () =
    let ~^~x = 0b01
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``Binary literal should get correct signature 2``() =
    let src =
        """
test(): () =
    let ~^~x = 0b00000001000000000000000000000
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``Hex literal should get correct signature``() =
    let src =
        """
test(): () =
    let x = ~^~0x4889f8
        """
    let symbol = getSymbolByCursor src
    Assert.Equal(4753912, symbol.AsConstant.Value.AsInt32)

[<Fact>]
let ``Hex literal should get correct signature 2``() =
    let src =
        """
test(): () =
    let ~^~x = 0x01
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``Hex literal should get correct signature 3``() =
    let src =
        """
test(): () =
    let ~^~x = 0xc3
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``ByRef of a field should have correct signature``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    mutable field X: __oly_int32 = 0

    get_Item(): inref<__oly_int32> =
        let ~^~w = &this.X
        &w
        """
    src |> hasSymbolSignatureTextByCursor "w: inref<__oly_int32>"

[<Fact>]
let ``ByRef of a field should have correct signature 2``() =
    let src =
        """
module TestModule

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    mutable field X: int32 = 0

    mutable get_Item(): byref<int32> =
        let ~^~w = &this.X
        &w
        """
    src |> hasSymbolSignatureTextByCursor "w: byref<int32>"

[<Fact>]
let ``ByRef of a field should have correct signature 3``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    field X: __oly_int32 = 0

    mutable get_Item(): inref<__oly_int32> =
        let ~^~w = &this.X
        &w
        """
    src |> hasSymbolSignatureTextByCursor "w: inref<__oly_int32>"

[<Fact>]
let ``ByRef of a field should have correct signature 4``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    field X: __oly_int32 = 0

    get_Item(): inref<__oly_int32> =
        &this.X

test(): () =
    let t = Test()
    let ~^~x = t.get_Item()
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``ByRef of a field should have correct signature 5``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    public field X: __oly_int32 = 0

test(): () =
    let mutable t = Test()
    let ~^~x = &t.X
        """
    src |> hasSymbolSignatureTextByCursor "x: inref<__oly_int32>"

[<Fact>]
let ``ByRef of a field should have correct signature 6``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    public mutable field X: __oly_int32 = 0

test(): () =
    let mutable t = Test()
    let ~^~x = &t.X
        """
    src |> hasSymbolSignatureTextByCursor "x: byref<__oly_int32>"

[<Fact>]
let ``ByRef of a field should have correct signature 7``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test =

    public mutable field X: __oly_int32 = 0

test(): () =
    let t = Test()
    let ~^~x = &t.X
        """
    src |> hasSymbolSignatureTextByCursor "x: inref<__oly_int32>"

[<Fact>]
let ``ByRef of a field should have correct signature 8``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

class Test =

    public field X: __oly_int32 = 0

test(): () =
    let t = Test()
    let ~^~x = &t.X
        """
    src |> hasSymbolSignatureTextByCursor "x: inref<__oly_int32>"

[<Fact>]
let ``ByRef of a field should have correct signature 9``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref_read_write")]
alias byref<T>

test(): () =
    let t = Test()
    let ~^~x = &t.X

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

class Test =

    public mutable field X: __oly_int32 = 0
        """
    src |> hasSymbolSignatureTextByCursor "x: byref<__oly_int32>"

[<Fact>]
let ``Incomplete branch should error``() =
    let src =
        """
test(): __oly_bool =
    if(true)
        false
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '()' but is '__oly_bool'.",
                """
        false
        ^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Incomplete branch should error 2``() =
    let src =
        """
test(): __oly_bool =
    if(true)
        false
    else
        ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '__oly_bool' but is '()'.",
                """
        ()
        ^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Incomplete branch should error 3``() =
    let src =
        """
module TestModule<T> =

    class TestClass<U> =

        test(): __oly_bool =
            if(true)
                false
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '()' but is '__oly_bool'.",
                """
                false
                ^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Complete branch should pass``() =
    let src =
        """
test(): () =
    if(true)
        if(true)
            ()
        else if(true)
            ()
        else
            ()

    ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complete branch should pass 2``() =
    let src =
        """
test(): () =
    if(true)
        ()
    else if(true)
        ()
    else
        ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Incorrect static overrides signature should error``() =
    let src =
        """
class Test =

    static X: __oly_int64 overrides get = 1
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("The property 'static X: __oly_int64 get' cannot find a 'get' to override.",
                """
    static X: __oly_int64 overrides get = 1
           ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Incorrect static overrides signature should error 2``() =
    let src =
        """
interface ITest =

    static X: __oly_int32 abstract get

class Test =
    implements ITest

    static X: __oly_int64 overrides get = 1
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("The property 'static X: __oly_int64 get' cannot find a 'get' to override.",
                """
    static X: __oly_int64 overrides get = 1
           ^
"""
            )
            ("The function 'static get_X(): __oly_int32' is not implemented for 'ITest' on 'Test'.",
                """
class Test =
      ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Overloaded "+" should compile``() =
    let src =
        """
#[intrinsic("uint8")]
alias uint8

#[intrinsic("int8")]
alias int8

#[intrinsic("uint16")]
alias uint16

#[intrinsic("int16")]
alias int16

#[intrinsic("uint32")]
alias uint32

#[intrinsic("int32")]
alias int32

#[intrinsic("uint64")]
alias uint64

#[intrinsic("int64")]
alias int64

#[intrinsic("float32")]
alias float32

#[intrinsic("float64")]
alias float64

#[intrinsic("add")]
(+)(uint8, uint8): uint8
#[intrinsic("add")]
(+)(int8, int8): int8
#[intrinsic("add")]
(+)(uint16, uint16): uint16
#[intrinsic("add")]
(+)(int16, int16): int16
#[intrinsic("add")]
(+)(uint32, uint32): uint32
#[intrinsic("add")]
(+)(int32, int32): int32
#[intrinsic("add")]
(+)(uint64, uint64): uint64
#[intrinsic("add")]
(+)(int64, int64): int64
#[intrinsic("add")]
(+)(float32, float32): float32
#[intrinsic("add")]
(+)(float64, float64): float64

main(): () =
    let x = 1 + 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Overloaded "-" should compile``() =
    let src =
        """
#[intrinsic("uint8")]
alias uint8

#[intrinsic("int8")]
alias int8

#[intrinsic("uint16")]
alias uint16

#[intrinsic("int16")]
alias int16

#[intrinsic("uint32")]
alias uint32

#[intrinsic("int32")]
alias int32

#[intrinsic("uint64")]
alias uint64

#[intrinsic("int64")]
alias int64

#[intrinsic("float32")]
alias float32

#[intrinsic("float64")]
alias float64

#[intrinsic("negate")]
(-)(int8): int8
#[intrinsic("negate")]
(-)(int16): int16
#[intrinsic("negate")]
(-)(int32): int32
#[intrinsic("negate")]
(-)(int64): int64
#[intrinsic("negate")]
(-)(float32): float32
#[intrinsic("negate")]
(-)(float64): float64

(-)<T1, T2>(x: T1): T2 where T1: { static op_UnaryNegation(T1): T2 } = T1.op_UnaryNegation(x)

class Test =

    public mutable field X: float32 = 5

test(x1: float32, x2: float32, x3: float32): () = ()
test(x1: int32, x2: int32, x3: int32): () = ()

main(): () =
    let t = Test()
    test(0, 0, -t.X)

        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Overloaded "&" should compile``() =
    let src =
        """
#[intrinsic("int8")]
alias int8

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

test(x: byref<int32>): () = ()
test(x: byref<int8>): () = ()

main(): () =
    let mutable x = 1
    test(&x)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Regression - should get right symbol signature when inheriting``() =
    let src =
        """
class Test

class Test2 =
    //inherits Test // intentionally commented out to showcase that not inheriting finds the symbol fine.
                    // See the test right below this one to test the real scenario.

    M(): () =
        let x = 1
        let y = ~^~x
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``Regression - should get right symbol signature when inheriting 2``() =
    let src =
        """
abstract class Test

class Test2 =
    inherits Test

    M(): () =
        let x = 1
        let y = ~^~x
        """
    src |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``Regression - should get error due to inheriting itself``() =
    let src =
        """
class Test

abstract class Test2 =
    inherits Test2
        """
    Oly src
    |> withErrorDiagnostics
        [
            "'Test2' is recursively extending itself."
        ]
    |> ignore

[<Fact>]
let ``Regression - should get error due to inheriting itself 2``() =
    let src =
        """
class Test

abstract class Test2 =
    inherits Test3

abstract class Test3 =
    inherits Test2
        """
    Oly src
    |> withErrorDiagnostics
        [
            "'Test3' is recursively extending itself."
        ]
    |> ignore

[<Fact>]
let ``Regression - Test2 module should not auto-open``() =
    let src =
        """
module Test

module Test2 =
    
    F(): () = ()

G(): () =
    F()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Identifier 'F' not found in scope.",
                """
    F()
    ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should not get error for two static local functions with the same name``() =
    let src =
        """
main(): () =
    let loop() = ()
    let loop() = ()
    loop()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Local generic type example in a local almbda should fail``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

test<A, B>(a: A, b: B): () =
    let test2<Z>() =
        struct Test<T> =

            mutable field a: A = default
            mutable field b: B = unchecked default
        ()
    test2<A>()

main(): () =
    let x = test<__oly_int32, __oly_utf16>(123, "test")
    print(x)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type declarations are not allowed in local lambda expressions due to possible inference variables escaping.",
                """
        struct Test<T> =
               ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Generic type constructor should fail when not instantiating it``() =
    let src =
        """
test<T<_>>(x: T): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type argument count do not match the type parameter count.",
                """
test<T<_>>(x: T): () = ()
              ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Generic type constructor should fail when instantiating it to call a static function``() =
    let src =
        """
interface ITest<T> =

    static abstract M(): ()

test<T<_>>(): () where T<_>: ITest =
    T<_>.M()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type parameter '?T' was unable to be inferred.",
                """
    T<_>.M()
    ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should error about expected return type``() =
    let src =
        """
struct TStruct =
    public mutable field X: __oly_int32 = 0

Test(): TStruct =
    let mutable s = TStruct()
    s.X <- 123
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'TStruct' but is '()'.",
                """
    s.X <- 123
    ^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Parameter-less function should pass``() =
    let src =
        """
module TestModule

Null<T>: T where T: not struct = unchecked default: T

main(): () =
    let x: __oly_utf16 = Null
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Parameter-less function should pass 2``() =
    let src =
        """
module TestModule

class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

None<T>: Option<T> where T: not struct = unchecked default: Option<T>

main(): () =
    let x: Option<__oly_utf16> = None
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Parameter-less function should pass 3``() =
    let src =
        """
module TestModule

class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

pattern None<T>(value: Option<T>): () =
    ()

None<T>: Option<T> where T: not struct = unchecked default: Option<T>

main(): () =
    let x: Option<__oly_utf16> = None
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Parameter-less function should pass 4``() =
    let src =
        """
module TestModule

class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

module Test2Module =
    pattern None<T>(value: Option<T>): () =
        ()

    None<T>: Option<T> where T: not struct = unchecked default: Option<T>

main(): () =
    let x: Option<__oly_utf16> = Test2Module.None
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Parameter-less pattern function should pass``() =
    let src =
        """
module TestModule

class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

module Test2Module =
    pattern None<T>(value: Option<T>): () =
        ()

    None<T>: Option<T> where T: not struct = unchecked default: Option<T>

test(x: Option<__oly_utf16>): () =
    match (x)
    | Test2Module.None => ()
    | _ => ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Parameter-less pattern function should pass 2``() =
    let src =
        """
module TestModule

class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

pattern None<T>(value: Option<T>): () =
    ()

None<T>: Option<T> where T: not struct = unchecked default: Option<T>

test(x: Option<__oly_utf16>): () =
    match (x)
    | ~^~None => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "pattern None<T>(value: Option<T>): ()"

[<Fact>]
let ``Pattern function should pass if it was in a parameterized module``() =
    let src =
        """
module TestModule

class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

#[open]
module OptionPatterns<T> =

    pattern Some(value: Option<T>): T =
        value.Value

test(x: Option<__oly_utf16>): () =
    match (x)
    | Some(_) => ()
    | _ => ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Parameter-less function should error``() =
    let src =
        """
module TestModule

Null<T>: T where T: not struct = unchecked default: T

main(): () =
    let x: __oly_utf16 = Null()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'Null' is parameter-less which requires not to be explicit with '()'.",
            """
    let x: __oly_utf16 = Null()
                         ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Parameter-less function should error 2``() =
    let src =
        """
module TestModule

module Test2Module =
    Null<T>: T where T: not struct = unchecked default: T

main(): () =
    let x: __oly_utf16 = Test2Module.Null()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'Null' is parameter-less which requires not to be explicit with '()'.",
            """
    let x: __oly_utf16 = Test2Module.Null()
                         ^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Parameter-less pattern function should error``() =
    let src =
        """
module TestModule

class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

module Test2Module =
    pattern None<T>(value: Option<T>): () =
        ()

    None<T>: Option<T> where T: not struct = unchecked default: Option<T>

test(x: Option<__oly_utf16>): () =
    match (x)
    | Test2Module.None() => ()
    | _ => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'None' returns '()' which requires not to be explicit with '()'.",
            """
    | Test2Module.None() => ()
      ^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore
    |> ignore

[<Fact>]
let ``Function requires explicit type arguments should pass``() =
    let src =
        """
module TestModule

Null<require T>(): T where T: not struct = unchecked default: T

main(): () =
    let x = Null<__oly_utf16>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Function requires explicit type arguments should fail``() =
    let src =
        """
module TestModule

Null<require T>(): T where T: not struct = unchecked default: T

main(): () =
    let x: __oly_utf16 = Null()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'Null' requires explicit type arguments.",
            """
    let x: __oly_utf16 = Null()
                         ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Regression - should not crash compiler and should error``() =
    let src =
        """
test(): __oly_bool = true

work(): () = ()

main(): () =
    if(test() == true)s
        work()
        work()
        """
    Oly src
    |> hasErrorDiagnostics
    |> ignore

[<Fact>]
let ``Regression - should not crash compiler and should error 2``() =
    let src =
        """
open static Test1
open static Test2, // Adding the comma here would cause the compiler to crash

class Marker

module Test1 =

    work1(): () = ()

module Test2 =

    work2(): () = ()
        """
    Oly src
    |> hasErrorDiagnostics
    |> ignore

[<Fact>]
let ``Regression - Should error when trying to get the address-of``() =
    let src =
        """
#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

class A

a(value1: A, value2: A): () = ()

b(value: byref<A>): () =
    a(value, &value)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'A' but is 'byref<A>'.",
            """
    a(value, &value)
             ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Regression - extension of array should pass``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("get_length")]
getLength<T>(mutable T[]): int32

#[open]
extension ArrayExtensions<T> =
    inherits mutable T[]

    Length: int32 
        #[inline]
        get() = getLength(this)

main(): () =
    let arr = mutable [1]
    let result = arr.Length
    ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Cannot use void like this``() =
    let src =
        """
#[intrinsic("void")]
alias void

f(x: void): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(x: void): () = ()
     ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot use void like this 2``() =
    let src =
        """
#[intrinsic("void")]
alias void

f(): void = unchecked default
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): void = unchecked default
            ^^^^^^^^^^^^^^^^^
"""
            )
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): void = unchecked default
     ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot use void like this 3``() =
    let src =
        """
#[intrinsic("void")]
alias void

f(): (void, void) = unchecked default
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): (void, void) = unchecked default
                    ^^^^^^^^^^^^^^^^^
"""
            )
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): (void, void) = unchecked default
                    ^^^^^^^^^^^^^^^^^
"""
            )
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): (void, void) = unchecked default
      ^^^^
"""
            )
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): (void, void) = unchecked default
            ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot use void like this 4``() =
    let src =
        """
#[intrinsic("void")]
alias void

class Test<T>

f(): Test<void> = unchecked default
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): Test<void> = unchecked default
                  ^^^^^^^^^^^^^^^^^
"""
            )
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): Test<void> = unchecked default
          ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot use void like this 5``() =
    let src =
        """
#[intrinsic("void")]
alias void

module Module =

    class Test<T>

f(): Module.Test<void> = unchecked default
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): Module.Test<void> = unchecked default
                         ^^^^^^^^^^^^^^^^^
"""
            )
            ("'void' can only be used as a type argument of a native pointer.",
            """
f(): Module.Test<void> = unchecked default
                 ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot use void like this 6``() =
    let src =
        """
#[intrinsic("void")]
alias void<T>
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Invalid intrinsic for this construct.",
            """
#[intrinsic("void")]
  ^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot declare load_null_ptr like this``() =
    let src =
        """
#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("load_null_ptr")]
nullptr(): __oly_int32*
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Invalid intrinsic for this construct.",
            """
#[intrinsic("load_null_ptr")]
  ^^^^^^^^^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Marking a function 'pure' should work as reflected in the symbol info``() =
    let src =
        """
#[pure]
~^~test(): () = ()
        """
    let symbol = (getSymbolByCursor src).AsValue
    Assert.True(symbol.IsPure)

// ************ VARIADIC TESTS ***************

[<Fact>]
let ``Simple variadic generic type should have correct signature``() =
    let src =
        """
class Test<T...>

main(): () =
    let x = Test<__oly_int32>()
    let y = Test<~^~__oly_int32, __oly_float32>()
        """
    src |> hasSymbolSignatureTextByCursor "__oly_int32"

[<Fact>]
let ``Simple variadic generic type should have correct signature 2``() =
    let src =
        """
class Test<T...>

main(): () =
    let x = Test<__oly_int32>()
    let y = Test<__oly_int32, ~^~__oly_float32>()
        """
    src |> hasSymbolSignatureTextByCursor "__oly_float32"

[<Fact>]
let ``Simple load_function_ptr``() =
    let src =
        """
test(x: __oly_int32, y: __oly_int32): () = ()

main(): () =
    let ~^~f = __oly_load_function_ptr(test)
        """
    src |> hasSymbolSignatureTextByCursor "f: static (__oly_int32, __oly_int32) -> ()"

[<Fact>]
let ``Simple load_function_ptr 2``() =
    let src =
        """
test(x: __oly_int32): () = ()

main(): () =
    let ~^~f = __oly_load_function_ptr(test)
        """
    src |> hasSymbolSignatureTextByCursor "f: static __oly_int32 -> ()"

[<Fact>]
let ``Simple load_function_ptr 3``() =
    let src =
        """
test(): () = ()

main(): () =
    let ~^~f = __oly_load_function_ptr(test)
        """
    src |> hasSymbolSignatureTextByCursor "f: static () -> ()"

[<Fact>]
let ``Simple blittable load_function_ptr``() =
    let src =
        """
#[blittable]
test(x: __oly_int32, y: __oly_int32): () = ()

main(): () =
    let ~^~f = __oly_load_function_ptr(test)
        """
    src |> hasSymbolSignatureTextByCursor "f: static blittable (__oly_int32, __oly_int32) -> ()"

[<Fact>]
let ``Property returning function ptr should pass``() =
    let src =
        """
module TestModule =

    X: static __oly_int32 -> () get = unchecked default

main(): () =
    TestModule.X(123)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Property returning function ptr should pass 2``() =
    let src =
        """
class TestClass =

    X: static __oly_int32 -> () get = unchecked default

main(): () =
    let t = TestClass()
    t.X(123)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Property returning function ptr should pass 3``() =
    let src =
        """
module TestModule =

    X: static __oly_int32 -> (static __oly_int32 -> ()) get = unchecked default

main(): () =
    TestModule.X(123)(345)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Property returning function ptr should pass 4``() =
    let src =
        """
module TestModule =

    X: static __oly_int32 -> static __oly_int32 -> () get = unchecked default

main(): () =
    TestModule.X(123)(345)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Property returning function ptr should pass 5``() =
    let src =
        """
module TestModule =

    X: static __oly_int32 -> __oly_int32 -> () get = unchecked default

main(): () =
    TestModule.X(123)(345)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Property returning function ptr should pass 6``() =
    let src =
        """
class TestClass =

    X: static __oly_int32 -> static __oly_int32 -> () get = unchecked default

main(): () =
    let t = TestClass()
    t.X(123)(345)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Property returning function ptr should fail``() =
    let src =
        """
module TestModule =

    X: static __oly_int32 -> ()
        set(value) = ()

main(): () =
    TestModule.X(123)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Property 'X' cannot be used for an indirect call because it does not have a 'get' function.",
            """
    TestModule.X(123)
               ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Property returning function ptr should fail 2``() =
    let src =
        """
class TestClass =

    X: static __oly_int32 -> ()
        set(value) = ()

main(): () =
    let t = TestClass()
    t.X(123)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Property 'X' cannot be used for an indirect call because it does not have a 'get' function.",
            """
    t.X(123)
      ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Field function ptr should pass``() =
    let src =
        """
module TestModule =

    public field X: static __oly_int32 -> () = unchecked default

main(): () =
    TestModule.X(123)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Field function ptr should pass 2``() =
    let src =
        """
class TestClass =

    public field X: static __oly_int32 -> () = unchecked default

main(): () =
    let t = TestClass()
    t.X(123)
        """
    Oly src
    |> shouldCompile
    |> ignore

// *************(*****************************************

[<Fact>]
let ``Character literal should error``() =
    let src =
        """
main(): () =
    let x = 'xx'
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Invalid character literal.",
            """
    let x = 'xx'
            ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Extension with an generic alias/phantom type should not work``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

alias AliasObject<T1, T2, T3> = int32

#[open]
extension AdditionExtends<T1, T2, T3> =
    inherits AliasObject<T1, T2, T3>
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'AliasObject<T1, T2, T3>' is an alias and cannot be used with a type extension.",
            """
    inherits AliasObject<T1, T2, T3>
             ^^^^^^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should get right signature for enclosing type of enum``() =
    let src =
        """
enum TestEnum =
    | A

main(): () =
    let a = ~^~TestEnum.A
        """
    src |> hasSymbolSignatureTextByCursor "TestEnum"

[<Fact>]
let ``Should get right signature for alias member call``() =
    let src =
        """
class CoolClass =

    static Test(): () = ()

alias CoolClassAlias = CoolClass

main(): () =
    ~^~CoolClassAlias.Test()
        """
    src |> hasSymbolSignatureTextByCursor "CoolClassAlias"

[<Fact>]
let ``Should get right signature for alias member property getter``() =
    let src =
        """
class CoolClass =

    static Test: __oly_int32 get() = 1

alias CoolClassAlias = CoolClass

main(): () =
    let result = ~^~CoolClassAlias.Test
        """
    src |> hasSymbolSignatureTextByCursor "CoolClassAlias"

[<Fact>]
let ``Should get right signature for alias member property setter``() =
    let src =
        """
class CoolClass =

    public static field Test: __oly_int32 get, set = 0

alias CoolClassAlias = CoolClass

main(): () =
    ~^~CoolClassAlias.Test <- 5
        """
    src |> hasSymbolSignatureTextByCursor "CoolClassAlias"

[<Fact>]
let ``Should get right signature for alias member field``() =
    let src =
        """
class CoolClass =

    public static field Test: __oly_int32 = 1

alias CoolClassAlias = CoolClass

main(): () =
    let result = ~^~CoolClassAlias.Test
        """
    src |> hasSymbolSignatureTextByCursor "CoolClassAlias"

[<Fact>]
let ``Should get right signature for alias member field 2``() =
    let src =
        """
class CoolClass =

    public static mutable field Test: __oly_int32 = 1

alias CoolClassAlias = CoolClass

main(): () =
    ~^~CoolClassAlias.Test <- 5
        """
    src |> hasSymbolSignatureTextByCursor "CoolClassAlias"

[<Fact>]
let ``Should get right signature for alias member pattern``() =
    let src =
        """
class CoolClass =

    pattern Test(x: __oly_int32): __oly_int32 = x

alias CoolClassAlias = CoolClass

main(): () =
    match (1)
    | ~^~CoolClassAlias.Test(x) => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "CoolClassAlias"

[<Fact>]
let ``Should get right signature for type extension member``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[open]
extension Int32Extensions =
    inherits int32

    static Test(): () = ()

main(): () =
    ~^~int32.Test()
        """
    src |> hasSymbolSignatureTextByCursor "int32"

[<Fact>]
let ``Should get right signature for type extension member 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[open]
extension Int32Extensions =
    inherits int32

    Test(): () = ()

main(): () =
    let x = 1: int32
    ~^~x.Test()
        """
    src |> hasSymbolSignatureTextByCursor "x: int32"

[<Fact>]
let ``This should error with constraints and inference``() =
    let src =
        """
interface ITest

class Test

test<T>(x: T): () where T: ITest = ()

main(): () =
    let x = unchecked default
    test(x)
    let y: Test = x
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'Test' is missing the constraint 'ITest'.",
                """
    test(x)
    ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``This should error with constraints and inference 2``() =
    let src =
        """
interface ITest

class Test

test<T>(x: T): () where T: ITest = ()

main(): () =
    let x: Test = unchecked default
    test(x)
    let y: Test = x
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'Test' is missing the constraint 'ITest'.",
                """
    test(x)
    ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``This should error with constraints and inference 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

interface ITest

test<T>(x: T): () where T: ITest = ()

main(): () =
    let f(x) = 
        test(x)
        x: int32
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'int32' is missing the constraint 'ITest'.",
                """
        test(x)
        ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Alias to a namespace should not crash``() =
    let src =
        """
namespace TestNamespace

alias AliasTest = TestNamespace
        """
    Oly src
    |> hasErrorDiagnostics

[<Fact>]
let ``Lambda expression should infer its parameters correctly``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

struct TestStruct =

    static op_Equality(x: TestStruct, y: TestStruct): bool = true

struct TestStruct2 =
    
    public field S: TestStruct = default

Find<T>(arr: T[], predicate: T -> bool): T = unchecked default

main(): () =
    let s = TestStruct()
    let xs: TestStruct2[] = []
    let result = Find(xs, x -> if (x.S == s) true else false)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Lambda expression should infer its parameters correctly 2``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

struct TestStruct =

    static op_Equality(x: TestStruct, y: TestStruct): bool = true

struct TestStruct2 =
    
    public field S: TestStruct = default

Find<T>(arr: T[], predicate: T -> bool): T = unchecked default

main(): () =
    let s = TestStruct()
    let xs: TestStruct2[] = []
    let result = 
        Find(xs, 
                 let s = s
                 x -> if (x.S == s) true else false
        )
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Lambda expression should infer its parameters correctly with overloads``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

struct TestStruct =

    static op_Equality(x: TestStruct, y: TestStruct): bool = true

struct TestStruct2 =
    
    public field S: TestStruct = default

Find<T>(arr: T[], predicate: T -> bool): T = unchecked default
Find<T>(arr: mutable T[], predicate: T -> bool): T = unchecked default

main(): () =
    let s = TestStruct()
    let xs: TestStruct2[] = []
    let result = Find(xs, x -> if (x.S == s) true else false)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested class with constructor is chosen correctly with potential ambiguous scopes with auto-open``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

class C1

#[open]
class C2 =
    field C: C1

    new() = { C = C1(123) }

    class C1 =

        new(x: int32) = { }
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested class with constructor is chosen correctly with potential ambiguous scopes with auto-open 2``() =
    let src =
        """
namespace N

#[intrinsic("int32")]
alias int32

class C1

#[open]
class C2 =
    field C: C1

    new() = { C = C1(123) }

    class C1 =

        new(x: int32) = { }
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested class with constructor is chosen correctly with potential ambiguous scopes with auto-open 3``() =
    let src =
        """
namespace N

open N

#[intrinsic("int32")]
alias int32

class C1

#[open]
class C2 =
    field C: C1

    new() = { C = C1(123) }

    class C1 =

        new(x: int32) = { }
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested class with constructor is chosen correctly with potential ambiguous scopes with auto-open 4``() =
    let src =
        """
module M

open static M

#[intrinsic("int32")]
alias int32

class C1

#[open]
class C2 =
    field C: C1

    new() = { C = C1(123) }

    class C1 =

        new(x: int32) = { }
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Nested class with constructor is chosen correctly with potential ambiguous scopes with auto-open 5``() =
    let src =
        """
namespace N.N2

open N.N2

#[intrinsic("int32")]
alias int32

class C1

#[open]
class C2 =
    field C: C1

    new() = { C = C1(123) }

    class C1 =

        new(x: int32) = { }
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Trying to access nested class should error since it is private``() =
    let src =
        """
class C1 =
    private class PC1

main(): () =
    let x = C1.PC1()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Member 'PC1' does not exist on type 'C1'.",
            """
    let x = C1.PC1()
               ^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Public signature contains private type should error``() =
    let src =
        """
class C1 =
    private class PC1

    static ShouldError(x: PC1): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'PC1' is less accessible than the member its used in.",
            """
    static ShouldError(x: PC1): () = ()
                          ^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Public signature contains private type should error 2``() =
    let src =
        """
class C1 =
    private class PC1

    static ShouldError: PC1 get = unchecked default
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'PC1' is less accessible than the member its used in.",
            """
    static ShouldError: PC1 get = unchecked default
                        ^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Public signature contains private type should error 3``() =
    let src =
        """
class C1 =
    private interface PI1

    static ShouldError<T>(): () where T: PI1 = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'PI1' is less accessible than the member its used in.",
            """
    static ShouldError<T>(): () where T: PI1 = ()
                                         ^^^
"""
            )
        ]
    |> ignore


[<Fact>]
let ``Private signature contains private type should NOT error``() =
    let src =
        """
class C1 =
    private class PC1

    private static ShouldNotError(x: PC1): () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Private signature contains private type should NOT error 2``() =
    let src =
        """
class C1 =
    private class PC1

    private static ShouldNotError: PC1 get = unchecked default
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Private signature contains private type should NOT error 3``() =
    let src =
        """
class C1 =
    private class PC1

    private class PC2 =

        public mutable field ShouldNotError: PC1 = unchecked default
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Internal signature contains internal type should NOT error``() =
    let src =
        """
class C1 =
    internal class PC1

    internal class PC2 =

        public mutable field ShouldNotError: PC1 = unchecked default
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Private signature contains internal type should NOT error``() =
    let src =
        """
class C1 =
    internal class PC1

    private class PC2 =

        public mutable field ShouldNotError: PC1 = unchecked default
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Internal signature contains private type should error``() =
    let src =
        """
class C1 =
    private class PC1

    internal class PC2 =

        public mutable field ShouldNotError: PC1 = unchecked default
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'PC1' is less accessible than the member its used in.",
            """
        public mutable field ShouldNotError: PC1 = unchecked default
                                             ^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Trying to access nested class should NOT error since it is internal``() =
    let src =
        """
class C1 =
    internal class PC1

main(): () =
    let x = C1.PC1()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Trying to declared nested class as protected should error``() =
    let src =
        """
class C1 =
    protected class PC1
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Types cannot be declared as 'protected'.",
            """
    protected class PC1
    ^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Trying to declared nested class as private should NOT error in namespace``() =
    let src =
        """
namespace Test

private class C1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Internal function should allowed to be accessed``() =
    let src =
        """
namespace Test

#[open]
internal module Helpers =

    internal fixed(): () = ()

    internal M(): () =
        fixed()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Subtyping should work when returning in a function``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

interface ITest

interface ITest<T> =
    inherits ITest

test1(): ITest<int32> = unchecked default

test2(): ITest =
    test1()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Subtyping should work when returning in a function 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

interface ITest

interface ITest2

interface ITest<T> =
    inherits ITest, ITest2

test1(): ITest<int32> = unchecked default

test2(): ITest2 =
    test1()
        """
    Oly src
    |> shouldCompile
    |> ignore


[<Fact>]
let ``Subtyping should error when returning in a function``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

interface ITest

interface ITest2

interface ITest<T> =
    inherits ITest

test1(): ITest<int32> = unchecked default

test2(): ITest2 =
    test1()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'ITest2' but is 'ITest<int32>'.",
            """
    test1()
    ^^^^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Trying to call constructor in a constructor with the explicit type name should work``() =
    let src =
        """
#[intrinsic("utf16")]
alias string

#[intrinsic("print")]
print(__oly_object): ()

class C =

    public field value1: string
    public field value2: string

    new(value1: string) =
        C(value1, "passed")

    new(value1: string, value2: string) =
        {
            value1 = value1
            value2 = value2
        }     
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Struct should not cycle``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

test<T>(): () where T: unmanaged = ()

struct Test<T> =
    X: T get = unchecked default

main(): () =
    test<Test<Test<int32>>>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Array expression without semi-colons``() =
    let src =
        """
main(): () =
    let xs = 
        [
            1
            2
        ]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Array expression without semi-colons 2``() =
    let src =
        """
main(): () =
    let xs = 
        [
            [1; 2]
            [3; 4]
        ]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Array expression without semi-colons 3``() =
    let src =
        """
main(): () =
    let xs = 
        [
            (
                let xs = [1; 2]
                xs
            )
            [3; 4]
        ]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Array expression without semi-colons 4``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("add")]
(+)(int32, int32): int32

main(): () =
    let xs = 
        [
            1 + 2
            3
        ]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Array expression without semi-colons should fail``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("add")]
(+)(int32, int32): int32

main(): () =
    let xs = 
        [
            let x = 1
            3
        ]
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Declaration not valid in this context.",
            """
            let x = 1
            ^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Array expression without semi-colons should fail 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("add")]
(+)(int32, int32): int32

main(): () =
    let xs = 
        [
            class C1
            3
        ]
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Declaration not valid in this context.",
            """
            class C1
            ^^^^^^^^
"""
            )
        ]
    |> ignore
