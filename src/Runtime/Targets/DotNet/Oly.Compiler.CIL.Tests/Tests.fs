﻿module Tests

open Xunit
open TestUtilities
open Utilities

[<Fact>]
let ``Blank``() =
    let src =
        """
main() : () = ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Hello World``() =
    let src =
        """
main() : () =
    print("Hello World")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "Hello World"
    |> ignore

[<Fact>]
let ``Hello World from instance function``() =
    let src =
        """
class Test =
    test() : () =
        print("Hello World")
    field x: int32
    new(x: int32) = this { x = x }

main() : () =
    let x = Test(123)
    x.test()
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "Hello World"
    |> ignore

[<Fact>]
let ``Hello World twice``() =
    let src =
        """
main() : () =
    print("Hello World")
    print("Hello World")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "Hello WorldHello World"
    |> ignore

[<Fact>]
let ``Print 'Hello World' if the branch is true``() =
    let src =
        """
main() : () =
    if (true)
        print("Hello World")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "Hello World"
    |> ignore

[<Fact>]
let ``Print 'HelloWorld' if the branch is true``() =
    let src =
        """
main() : () =
    if (true)
        print("Hello")
    print("World")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "HelloWorld"
    |> ignore

[<Fact>]
let ``Do not print 'Hello World' if the branch is false``() =
    let src =
        """
main() : () =
    if (false)
        print("Hello World")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Do not print 'HelloWorld' if the branch is false, but print 'World'``() =
    let src =
        """
main() : () =
    if (false)
        print("Hello")
    print("World")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "World"
    |> ignore

[<Fact>]
let ``Print 'Hello' if the branch is true``() =
    let src =
        """
main() : () =
    if (true)
        print("Hello")
    else
        print("World")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "Hello"
    |> ignore

[<Fact>]
let ``Print 'World' if the branch is false``() =
    let src =
        """
main() : () =
    if (false)
        print("Hello")
    else
        print("World")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "World"
    |> ignore

[<Fact>]
let ``Print 'HelloEarth' if the branch is true``() =
    let src =
        """
main() : () =
    if (true)
        print("Hello")
    else
        print("World")
    print("Earth")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "HelloEarth"
    |> ignore

[<Fact>]
let ``Print 'WorldEarth' if the branch is false``() =
    let src =
        """
main() : () =
    if (false)
        print("Hello")
    else
        print("World")
    print("Earth")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "WorldEarth"
    |> ignore

[<Fact>]
let ``Print 'WorldEarth' if the branch is false - 2``() =
    let src =
        """
main() : () =
    if (false)
        print("Hello")
    else
        if (true)
            print("World")
    print("Earth")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "WorldEarth"
    |> ignore

[<Fact>]
let ``Print 'HelloEarth'``() =
    let src =
        """
main() : () =
    if (true)
        print("Hello")
    else if (true)
        print("World")
    else if (true)
        print("Mars")
    else if (true) print("Jupiter")
    print("Earth")
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "HelloEarth"
    |> ignore

[<Fact>]
let ``Simple While loop``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let mutable x = 1
    while (x < 5)
        print(x)
        x <- x + 1
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1234"
    |> ignore

[<Fact>]
let ``Simple functions and private ones``() =
    let src =
        """
module Test =

    private test_private(): () =
        print("test")
        print("_private")

    test(): () = Test.test_private()

main() : () =
    Test.test()
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "test_private"
    |> ignore

[<Fact>]
let ``Basic use of IAdd interface should give the expected output``() =
    let src =
        """
open extension Int32Extensions

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IAdd<T1, T2, T3> =

    static abstract Add(x: T1, y: T2) : T3

extension Int32Extensions =
    inherits int32
    implements IAdd<int32, int32, int32>

    static overrides Add(x: int32, y: int32) : int32 =
        __oly_add(x, y)

(+)<T>(x: T, y: T) : T where T : trait IAdd<T, T, T> = T.Add(x, y)

main () : () =
    let x = 1
    print(x + x)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Basic use of Add trait should give the expected output``() =
    let src =
        """
open extension Int32AddExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

extension Int32AddExtension =
    inherits int32
    implements Add<int32, int32, int32>

    static overrides add(x: int32, y: int32) : int32 =
      __oly_add(x, y)

(+)<T>(x: T, y: T) : T where T : trait Add<T, T, T> = T.add(x, y)

main () : () =
   let x = 1
   print(x + x)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Basic use of inherited Add trait should give the expected output``() =
    let src =
        """
open extension Int32AddExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32) : int32 =
      __oly_add(x, y)

(+)<T>(x: T, y: T) : T where T : trait Add<T> = T.add(x, y)

main () : () =
   let x = 1
   print(x + x)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Example 1``() =
    let src =
        """
open extension AddInt32Extension

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

extension AddInt32Extension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32) : int32 =
        __oly_add(x, y)

(+)<T1, T2, T3>(x: T1, y: T2) : T3 where T1 : Add<T1, T2, T3> = T1.add(x, y)

class Hoot<T> =
    public field value: T

    new(value: T) = this { value = value }

extension HootAddExtension<T> where T: Add<T> =
    inherits Hoot<T>
    implements Add<Hoot<T>>

    static overrides add(x: Hoot<T>, y: Hoot<T>): Hoot<T> =
        let v1 = x.value
        let v2 = y.value
        let result = v1 + v2
        Hoot<_>(result)

f<T>(x: T, y: T): T where T: Add<T, T, T> = x + y

main () : () =
   let x = 4
   print(x)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"
    |> ignore

[<Fact>]
let ``Example 2``() =
    let src =
        """
open extension Int32AddExtension
open extension Float64AddExtension
open extension MaybeMonadExtension<_>

#[intrinsic("int32")]
alias int32

#[intrinsic("float64")]
alias float64

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface Add<T> =

   static abstract add(x: T, y: T): T

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32): int32 =
        __oly_add(x, y)

extension Float64AddExtension =
    inherits float64
    implements Add<float64>

    static overrides add(x: float64, y: float64) : float64 =
        __oly_add(x, y)

(+)<T>(x: T, y: T): T where T: Add<T> = T.add(x, y)

interface Functor<F<_>> =

   static abstract map<A, B>(fa: F<A>, f: A -> B): F<B>

interface Monad<M<_>> =

   static abstract Bind<A, B>(ma: M<A>, f: A -> M<B>): M<B>

   static abstract Return<A>(a: A): M<A>

class Maybe<T> =
    public field value: T

    new(value: T) = this { value = value }

extension MaybeMonadExtension<T> =
    inherits Maybe<T>
    implements Monad<Maybe>

    static overrides Bind<A, B>(ma: Maybe<A>, f: A -> Maybe<B>) : Maybe<B> =
        let res = ma.value
        f(res)

    static overrides Return<A>(a: A) : Maybe<A> =
        Maybe<_>(a)

(>>=)<Toot<_>, A, B>(ma: Toot<A>, f: A -> Toot<B>): Toot<B> where Toot: trait Monad<Toot>  =
   Toot<object>.Bind<_, _>(ma, f)

transform (x: int32) : Maybe<float64> = Maybe<_>(228888.45)

example() : () =
   let m = Maybe<_>(1)
   let res: Maybe<float64> = m >>= transform
   print(res.value)

main() : () =
   example()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "228888.45"
    |> ignore

[<Fact>]
let ``Example 3``() =
    let src =
        """
open extension MaybeMonadExtension<_>

#[intrinsic("int32")]
alias int32

#[intrinsic("float64")]
alias float64

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface Monad<M<_>, M2<_>> =

    static abstract bind<A, B>(ma: M<A>, f: A -> M2<B>): M2<B>

class Maybe<T> =
    public field value: T

    new(value: T) = this { value = value }

class Maybe2<T> =
    public field value: T

    new(value: T) = this { value = value }

extension MaybeMonadExtension<T> =
    inherits Maybe<T>
    implements Monad<Maybe, Maybe2>

    static overrides bind<A, B>(ma: Maybe<A>, f: A -> Maybe2<B>): Maybe2<B> =
        let res = ma.value
        f(res)

(>>=)<M<_>, M2<_>, A, B>(ma: M<A>, f: A -> M2<B>): M2<B> where M: trait Monad<M, M2> =
   M.bind<_, _>(ma, f)

transform (x: int32): Maybe2<float64> = Maybe2<_>(123.45)

example(): () =
   let m = Maybe<_>(1)
   let res: Maybe2<float64> = m >>= transform
   print(res.value)

main() : () =
   example()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123.45"
    |> ignore

[<Fact>]
let ``Resolve type inference on the + operator``() =
    let src =
        """
open extension Int32AddExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

(+)<T1, T2, T3>(x: T1, y: T2) : T3 where T1 : trait Add<T1, T2, T3> = T1.add(x, y)

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32) : int32 =
      __oly_add(x, y)

main () : () =
   let f(x) = 1 + x
   let x = 4
   print(f(x))
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Resolve type inference on the + operator - 2``() =
    let src =
        """
open extension Int32AddExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2): T3

interface Add<T> =
   inherits Add<T, T, T>

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait Add<T1, T2, T3> = T1.add(x, y)

f<A, B>(x: A): B where A: trait Add<A, int32, B> = x + 1

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32): int32 =
      __oly_add(x, y)

main () : () =
   let x = 4
   print(f<_, int32>(x))
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Automatic generalization - simple``() =
    let src =
        """
main () : () =
    let f(x) = x
    print(f(12))
        """

    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "12"
    |> ignore

[<Fact>]
let ``Interface static abstract default implementation``() =
    let src =
        """
interface TestInterface =
 
    static abstract default test() : int32 = 567

class Test =
    implements TestInterface

getResult<T>() : int32 where T : TestInterface =
    T.test()

main() : () =
    print(getResult<Test>())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "567"
    |> ignore

[<Fact>]
let ``Interface static abstract default implementation 2``() =
    let src =
        """
interface TestInterface =
 
    static abstract default test() : int32 = 567

class Test =
    implements TestInterface

    static overrides test() : int32 = 123

getResult<T>() : int32 where T : TestInterface =
    T.test()

main() : () =
    print(getResult<Test>())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Interface static abstract implementation``() =
    let src =
        """
interface TestInterface =
 
    static abstract test() : int32

class Test =
    implements TestInterface

    static overrides test() : int32 = 123

getResult<T>() : int32 where T : TestInterface =
    T.test()

main() : () =
    print(getResult<Test>())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Parameterized module should work``() =
    let src =
        """
module Best<T> =

    test(x: T): () = print(x)

main(): () =
    Best<int32>.test(5)
    Best<float32>.test(1.4f)
    ()
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "51.4"
    |> ignore

[<Fact>]
let ``Trait default implementation``() =
    let src =
        """
open extension Int32Extension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface TestTrait =
 
    static abstract default test() : int32 = 567

extension Int32Extension =
    inherits int32
    implements TestTrait

getResult<T>() : int32 where T : trait TestTrait =
    T.test()

main() : () =
    print(getResult<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "567"
    |> ignore

[<Fact>]
let ``Trait default implementation 2``() =
    let src =
        """
open extension Int32Extension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface TestTrait =
 
    static abstract default test() : int32 = 567

extension Int32Extension =
    inherits int32
    implements TestTrait

    static overrides test() : int32 = 123

getResult<T>() : int32 where T : trait TestTrait =
    T.test()

main() : () =
    print(getResult<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Trait default implementation 3``() =
    let src =
        """
open extension Int32Extension<int32>

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface TestTrait<T> =
 
    static abstract default test(x: T) : T = x

extension Int32Extension<T> =
    inherits int32
    implements TestTrait<T>

getResult<T>() : int32 where T: trait TestTrait<int32> =
    T.test(878)

main() : () =
    print(getResult<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "878"
    |> ignore

[<Fact>]
let ``Interface static type parameter default instance implementation``() =
    let src =
        """
open extension Int32Extension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface TestTrait =
 
    default test(): int32 = 567

extension Int32Extension =
    inherits int32
    implements TestTrait

getResult<T>(x: T): int32 where T: trait TestTrait =
    x.test()

main() : () =
    print(getResult<_>(123))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "567"
    |> ignore

[<Fact>]
let ``Trait instance implementation``() =
    let src =
        """
open extension Int32Extension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface TestTrait =
 
    test(): int32

extension Int32Extension =
    inherits int32
    implements TestTrait

    test(): int32 = 987

getResult<T>(x: T): int32 where T: trait TestTrait =
    x.test()

main(): () =
    print(getResult<_>(123))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "987"
    |> ignore

[<Fact>]
let ``Trait instance override default implementation``() =
    let src =
        """
open extension Int32Extension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface TestTrait =
 
    default test(): int32 = 567

extension Int32Extension =
    inherits int32
    implements TestTrait

    test(): int32 = 987

getResult<T>(x: T): int32 where T: trait TestTrait =
    x.test()

main() : () =
    print(getResult<_>(123))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "987"
    |> ignore

[<Fact>]
let ``Interface implementation``() =
    let src =
        """
interface ITest =
 
    test(): int32

class Test =
    implements ITest

    test() : int32 = 123

    new() = this { }

getResult(t: ITest) : int32 =
    t.test()

main() : () =
    let r = Test()
    print(getResult(r))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Interface implementation 2``() =
    let src =
        """
interface ITest =

    get_x(): int32
 
    test(): int32

class Test =
    implements ITest

    field x: int32

    get_x(): int32 = this.x
    test() : int32 = 123

    new(x: int32) = this { x = x }

getResult(t: ITest) : int32 =
    t.test()

main() : () =
    let r = Test(1)
    print(getResult(r))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Generic interface method implementation``() =
    let src =
        """
interface ITest =
 
    test<T>() : int32

class Test =
    implements ITest

    field x: int32

    test<T>() : int32 = 123

    new(x: int32) = this { x = x }

getResult(t: ITest) : int32 =
    t.test<float64>()

main() : () =
    let r = Test(1)
    print(getResult(r))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Generic interface with generic method overloaded implementation``() =
    let src =
        """
interface ITest<T1, T2> =
 
    test<T>(x: T1) : int32
    test<T>(x: T2) : int32

class Test =
    implements ITest<int16, float64>

    field x: int32

    test<T>(x: int16) : int32 = 123
    test<T>(x: float64) : int32 = 456

    new(x: int32) = this { x = x }

getResult(t: ITest<int16, float64>) : int32 =
    t.test<float64>(123.0)

main() : () =
    let r = Test(1)
    print(getResult(r))
    
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Interface default implementation``() =
    let src =
        """
interface ITest =
 
    default test() : int32 = 894

class Test =
    implements ITest

    field x: int32

    new(x: int32) = this { x = x }

getResult(t: ITest) : int32 =
    t.test()

main() : () =
    let r = Test(1)
    print(getResult(r))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "894"
    |> ignore

[<Fact>]
let ``Interface default implementation 2``() =
    let src =
        """
interface ITest =
 
    default test() : int32 = 894

class Test =
    implements ITest

    field x: int32

    test() : int32 = 123

    new(x: int32) = this { x = x }

getResult(t: ITest) : int32 =
    t.test()

main() : () =
    let r = Test(1)
    print(getResult(r))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Hello World with generic erasure``() =
    let src =
        """
class Test<T> =

    field x: int32

    new(x: int32) = this { x = x }

    M<U>() : utf16 = "Hello World"

main() : () =
    let x = Test<float64>(1)
    print(x.M<float32>())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "Hello World"
    |> ignore

[<Fact>]
let ``Generic type calling static function``() =
    let src =
        """
class Test<T> =

    static test(x: T) : T = x

main() : () =
    let x = Test<int32>.test(123)
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Generic type example``() =
    let src =
        """
class Test<T> =

    public field x: T

    new(x: T) = this { x = x }

main() : () =
    let x = Test<int32>(123)
    print(x.x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Generic type example 2``() =
    let src =
        """
class Test<T> =

    public field x: T

    new(x: T) = this { x = x }

main() : () =
    let x = Test<int32>(123).x
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Local generic function inside generic function``() =
    let src =
        """
main() : () =
    let f<T>(x: T) =
        let g<U>(y: T, z: U) = z
        g(x, x)
    print(f(789))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "789"
    |> ignore

[<Fact>]
let ``Instance function with receiver``() =
    let src =
        """
class Test =
    test(y: int32) : () =
        print(this.x)
        print(y)
    field x: int32
    new(x: int32) = this { x = x }

main() : () =
    let x = Test(123)
    x.test(456)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123456"
    |> ignore

[<Fact>]
let ``Mutate value``() =
    let src =
        """
main() : () =
    let mutable x = 1
    print(x)
    x <- 5
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "15"
    |> ignore

[<Fact>]
let ``Mutate field``() =
    let src =
        """
class Test =
    public field mutable x: int32
    new(x: int32) = this { x = x }

main() : () =
    let test = Test(1)
    test.x <- 50
    print(test.x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "50"
    |> ignore

[<Fact>]
let ``Mutate field 2``() =
    let src =
        """
class Test =
    public field mutable x: int32
    new(x: int32) = this { x = x }

main() : () =
    let test = Test(1)
    test.x <- 
        test.x <- 30
        test.x
    print(test.x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "30"
    |> ignore

[<Fact>]
let ``Mutate field 3``() =
    let src =
        """
class Test =
    public field mutable x: int32
    new(x: int32) = this { x = x }

class Test2 =
    public field y: Test
    new(y: Test) = this { y = y }

main() : () =
    let test = Test(1)
    let test2 = Test2(test)
    test2.y.x <- 45
    print(test.x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "45"
    |> ignore

[<Fact>]
let ``Mutate field value on nested struct``() =
    let src =
        """
class Test2 =
    public field mutable test: Test
    new(test: Test) = this { test = test }

struct Test =
    public field mutable x: int32
    new(x: int32) = this { x = x }

main() : () =
    let t = Test2(Test(1))
    print(t.test.x)
    t.test.x <- 5
    print(t.test.x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "15"
    |> ignore

[<Fact>]
let ``Mutate field value on nested struct on type with chained call``() =
    let src =
        """
class Test2 =
    public field mutable test: Test
    new(test: Test) = this { test = test }

struct Test =
    public field mutable x: int32
    new(x: int32) = this { x = x }

test(x: Test2) : Test2 = 
    print(x.test.x)
    x

main() : () =
    let t = Test2(Test(1))
    test(t).test.x <- 3
    print(t.test.x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "13"
    |> ignore

[<Fact>]
let ``Mutate field value on nested struct on struct with chained call``() =
    let src =
        """
struct Test2 =
    public field mutable test: Test
    new(test: Test) = this { test = test }

struct Test =
    public field mutable x: int32
    new(x: int32) = this { x = x }

test(x: Test2) : Test2 = 
    print(x.test.x)
    x

main() : () =
    let t = Test2(Test(1))
    test(t).test.x <- 3
    print(t.test.x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "11"
    |> ignore

[<Fact>]
let ``Mutate field value on nested struct on struct with chained call 2``() =
    let src =
        """
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct Test2 =
    public field mutable test: Test
    new(test: Test) = this { test = test }

struct Test =
    public field mutable x: int32
    new(x: int32) = this { x = x }

main() : () =
    let mutable t = Test2(Test(1))
    let result = &t.test.x
    result <- 3
    print(t.test.x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Nested class instantiation``() =
    let src =
        """
class Test =
    public field x: int32

    class Test2 =
        public field y: float64
        new(y: float64) = this { y = y }
    new(x: int32) = this { x = x }

test() : () =
    let x = Test.Test2(2.0)
    print(x.y)

main() : () =
    test()
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Able to use an instance member with the same name implemented via an extension``() =
    let src =
        """
open extension TestAddInt32Extension

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

class Test =

    public field x: int32
    new(x: int32) = this { x = x }

    add(x: int32, y: int32) : int32 = y

interface Add<T> =

   add(x: T, y: T) : T

extension TestAddInt32Extension =
    inherits Test
    implements Add<int32>

    add(x: int32, y: int32) : int32 = x

test<T>(x: T) : () where T : trait Add<int32> =
    let x = x.add(1, 2)
    print(x)

main() : () =
    let x = Test(789)
    test<_>(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Able to use simple struct``() =
    let src =
        """
struct Test =

    public field x: int32
    public field y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

main() : () =
    let t = Test(7, 9)
    print(t.x)
    print(t.y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "79"
    |> ignore

[<Fact>]
let ``Able to use simple struct 2``() =
    let src =
        """
struct Test =

    public field mutable x: int32
    public field mutable y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

main() : () =
    let mutable t = Test(7, 9)
    t.x <- 100
    print(t.x)
    print(t.y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "1009"
    |> ignore

[<Fact>]
let ``Able to use simple struct 3``() =
    let src =
        """
struct Test =

    public field mutable x: int32
    public field mutable y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

test(mutable t: Test) : () =
    print(t.x)
    t.x <- 100
    print(t.x)

main() : () =
    let t = Test(7, 9)
    test(t)
    print(t.x)
    print(t.y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "710079"
    |> ignore

[<Fact>]
let ``Able to use simple struct 4``() =
    let src =
        """
struct Test =

    public field mutable x: int32
    public field mutable y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

test(mutable t: Test) : () =
    print(t.x)
    t.x <- 100
    print(t.x)

main() : () =
    let mutable t = Test(7, 9)
    test(t)
    print(t.x)
    print(t.y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "710079"
    |> ignore

[<Fact>]
let ``Able to use simple struct 5``() =
    let src =
        """
struct Test =

    public field mutable x: int32
    public field mutable y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

struct Test2 =
    public field mutable x: Test
    new(x: Test) = this { x = x }

test(mutable t: Test2) : () =
    print(t.x.x)
    t.x.x <- 100
    print(t.x.x)

main() : () =
    let mutable t = Test2(Test(7, 9))
    test(t)
    print(t.x.x)
    print(t.x.y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "710079"
    |> ignore

[<Fact>]
let ``Able to use simple struct 6``() =
    let src =
        """
struct Test =

    public field mutable x: int32
    public field y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

    test() : int32 = this.x

main() : () =
    let t = Test(7, 9)
    print(t.test())
    print(t.y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "79"
    |> ignore

[<Fact>]
let ``Able to use simple struct 7``() =
    let src =
        """
struct Test =

    public field x: int32
    public field y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

    test() : int32 = this.x

main() : () =
    let t = Test(7, 9)
    print(t.test())
    print(t.y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "79"
    |> ignore

[<Fact>]
let ``Able to use simple struct 8``() =
    let src =
        """
struct Test =

    public field mutable x: int32
    public field mutable y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

struct Test2 =
    public field mutable x: Test
    new(x: Test) = this { x = x }

test(mutable t: Test2) : () =
    t.x.x <- 100
    print(t.x.x)

main() : () =
    let mutable t = Test2(Test(7, 9))
    test(t)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "100"
    |> ignore


[<Fact>]
let ``Able to use simple mutable type``() =
    let src =
        """
class Test =

    public field mutable x: int32
    public field mutable y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

main() : () =
    let t = Test(7, 9)
    t.x <- 100
    print(t.x)
    print(t.y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "1009"
    |> ignore

[<Fact>]
let ``Able to use simple shape``() =
    let src =
        """
class Test =

    x: int32 get
    y: int32 get
    new(x: int32, y: int32) = this { x = x; y = y }

test<T>(x: T): int32 where T: { x: int32 get; y: int32 get } =
    x.y

main(): () =
    let t = Test(7, 9)
    let result = test<_>(t)
    print(result)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Able to use simple shape 2``() =
    let src =
        """
struct Test =

    x: int32 get
    y: int32 get
    new(x: int32, y: int32) = this { x = x; y = y }

test<T>(x: T): int32 where T: { x: int32 get; y: int32 get } =
    x.y

main(): () =
    let t = Test(7, 9)
    let result = test<_>(t)
    print(result)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Able to use simple shape that captures type parameter``() =
    let src =
        """
struct Test =

    x: int32 get
    y: int32 get
    new(x: int32, y: int32) = this { x = x; y = y }

test<T, U>(x: T): U where T: { x: int32 get; y: U get } =
    x.y

main(): () =
    let t = Test(7, 9)
    let result = test<_, _>(t)
    print(result)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple partial application of id function``() =
    let src =
        """
id(x: int32) : int32 = x

main() : () =
    let a = id
    print(a(456))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Simple partial application of id function 2``() =
    let src =
        """
id(x: int32) : int32 = x

test<T>(x: T, f: T -> T) : T = f(x)

main() : () =
    let a = id
    print(test<_>(159, a))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "159"
    |> ignore

[<Fact>]
let ``Simple partial application of id function 3``() =
    let src =
        """
id(x: int32): int32 = x

test<T>(x: T, f: T -> T): T = f(x)

main(): () =
    print(test<_>(999, id))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "999"
    |> ignore

[<Fact>]
let ``Simple partial application of id function 4``() =
    let src =
        """
id(x: int32, y: int32) : int32 = y

main() : () =
    let a = id
    print(a(456, 123))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple lambda expression``() =
    let src =
        """
main() : () =
    let a = (x: int32) -> x
    print(a(456))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Simple lambda expression 2``() =
    let src =
        """
main() : () =
    let a = (x) -> x
    print(a(456))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Simple lambda expression 3``() =
    let src =
        """
main() : () =
    let a = (x) -> x
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore


[<Fact>]
let ``Simple lambda expression 4``() =
    let src =
        """
test(f: int32 -> int32) : int32 = f(123)
main() : () =
    let result = test((x) -> x)
    print(result)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple lambda expression 5``() =
    let src =
        """
main() : () =
    let a: int32 -> int32 = (x) -> x
    print(a(456))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Simple lambda expression 6``() =
    let src =
        """
main() : () =
    let a = (x) -> x : int32
    print(a(456))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Simple lambda expression 7``() =
    let src =
        """
main() : () =
    let a = (x) -> 1
    print(a(456))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Simple lambda expression 8``() =
    let src =
        """
main() : () =
    let a = (x) -> 1
    print(a(456))
    print(a(5.0f))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "11"
    |> ignore

[<Fact>]
let ``Simple lambda expression 9``() =
    let src =
        """
main() : () =
    let y = 123
    let a = (x) -> y
    print(a(456))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple lambda expression 10``() =
    let src =
        """
main() : () =
    let mutable x = 1
    let a = () -> x
    print(a())
    x <- 55
    print(a())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "155"
    |> ignore

[<Fact>]
let ``Simple lambda expression 11``() =
    let src =
        """
class Test =
    public field x: int32
    new(x: int32) = this { x = x }

test(f: Test -> ()) : () =
    let t = Test(567)
    f(t)

main() : () =
    test((t) -> print(t.x))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "567"
    |> ignore

[<Fact>]
let ``Simple lambda expression 12``() =
    let src =
        """
id(x: int32) : () = print(x)
id2(x: int32) : () = print("hello")

main() : () =
    let mutable f = id
    f(5)
    f <- id2
    f(5)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "5hello"
    |> ignore

[<Fact>]
let ``Simple lambda expression 13``() =
    let src =
        """
id<T>(x: T) : () = print("earth")
id2<T>(x: T) : () = print("hello")

main() : () =
    let mutable f = id<_>
    f(5)
    f <- id2<_>
    f(5)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "earthhello"
    |> ignore

[<Fact>]
let ``Simple lambda expression 14``() =
    let src =
        """
class Test =

    public field x: int32

    new(x: int32) = this { x = x }

main() : () =
    let t = Test(256)
    let a = () -> t.x
    print(a())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "256"
    |> ignore

[<Fact>]
let ``Simple lambda expression 15``() =
    let src =
        """
class Test =

    public field x: int32

    new(x: int32) = this { x = x }

main() : () =
    let mutable t = Test(256)
    let a = () -> t.x
    print(a())
    t <- Test(128)
    print(a())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "256128"
    |> ignore

[<Fact>]
let ``Simple lambda expression 16``() =
    let src =
        """
test(mutable x: int32) : () =
    let a = () -> x
    print(a())
    x <- 765
    print(a())

main() : () =
    test(256)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "256765"
    |> ignore

[<Fact>]
let ``Simple lambda expression 17``() =
    let src =
        """
class Test =

    public field x: int32

    new(x: int32) = this { x = x }

test(t: Test) : () =
    let a = () -> t.x
    print(a())

main() : () =
    let t = Test(256)
    test(t)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "256"
    |> ignore

[<Fact>]
let ``Simple lambda expression 18``() =
    let src =
        """
class Test =

    public field x: int32

    new(x: int32) = this { x = x }

test(mutable t: Test) : () =
    let a = () -> t.x
    print(a())
    t <- Test(999)
    print(a())

main() : () =
    let t = Test(256)
    test(t)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "256999"
    |> ignore

[<Fact>]
let ``Simple lambda expression 19``() =
    let src =
        """
id(x: int32) : () = print(x)

main() : () =
    let f = id
    let g = (x) -> f(x)
    g(42)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "42"
    |> ignore

[<Fact>]
let ``Simple lambda expression 20``() =
    let src =
        """
id(x: int32) : () = print(x)
id2(x: int32) : () = print("hello")

main() : () =
    let mutable f = id
    let g = (x) -> f(x)
    g(22)
    f <- id2
    f(7)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "22hello"
    |> ignore

[<Fact>]
let ``Simple lambda expression 21``() =
    let src =
        """
test() : () =
    static let f() = static () -> () -> print(123)
    let g = static () -> f()
    g()()()

main() : () =
    test()
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple lambda expression 22``() =
    let src =
        """
class Test =

    public field x: int32

    new(x: int32) = this { x = x }

main() : () =
    let mutable x = 5
    let a = (y: int32, z: int32) -> x
    print(a(1, 2))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple lambda expression 23``() =
    let src =
        """
class Test =

    field x: int32

    new(x: int32) = this { x = x }

    P: int32
        get() =
            let mutable x = 5
            let a = (y: int32, z: int32) -> x
            a(1, 2)

main() : () =
    let t = Test(123)
    print(t.P)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple curried lambda expression``() =
    let src =
        """
main() : () =
    let a = (x: int32) -> (x: int32) -> x
    print(a(456)(123))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple curried lambda expression 2``() =
    let src =
        """
main() : () =
    let a = (x: int32) -> (x: int32) -> x
    let r = a(456)
    print(r(123))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple curried lambda expression 3``() =
    let src =
        """
main() : () =
    let a = (x: int32) -> (x: int32) -> x
    let b = a
    print(b(456)(123))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple curried lambda expression 4``() =
    let src =
        """
main() : () =
    let a = (x: int32) -> (y: int32) -> x
    print(a(456)(123))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Simple capturing lambda expression``() =
    let src =
        """
main() : () =
    let x = 232
    let a = () -> x
    print(a())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "232"
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function``() =
    let src =
        """
id<T>(x: T) : T = x

main() : () =
    let a = id<_>
    print(a(456))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function 1-5``() =
    let src =
        """
id<T>(x: T) : T = x

main() : () =
    let a = id<int32>
    print(a(898))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "898"
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function 2``() =
    let src =
        """
id<T>(x: T) : T = x

main() : () =
    let a = id<int32>
    let b = id<float64>
    print(a(898))
    print(b(34.3))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "89834.3"
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function 3``() =
    let src =
        """
id<T>(x: T) : T = x

test<T>(x: T, f: T -> T) : T = f(x)

main() : () =
    let a = id<_>
    print(test<_>(456, a))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function 4``() =
    let src =
        """
id<T>(x: T) : T = x

test<T>(x: T, f: T -> T) : T = f(x)

main() : () =
    print(test<_>(456, id<_>))
    print(test<_>(89.5, id<_>))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "45689.5"
    |> ignore

[<Fact>]
let ``Simple partial application of generic id function 5``() =
    let src =
        """
id<T>(x: T) : T = x

test<T>(x: T, f: T -> T) : T = f(x)

main() : () =
    let a = id<_>
    let x = test<_>(456, a)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Simple partial application of a function that takes a tuple``() =
    let src =
        """
class Test =

    field x: int32

    new(x: int32) = this { x = x }

test(x: (int32, int32)) : int32 = 123

main() : () =
    let mutable a = test
    print(a((1, 2)))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple partial application of a function that takes a tuple 2``() =
    let src =
        """
class Test =

    field x: int32

    new(x: int32) = this { x = x }

test(x: (int32, int32), y: (int32, int32)) : int32 = 123

main() : () =
    let mutable a = test
    print(a((1, 2), (3, 4)))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Test precedence``() =
    let src =
        """
open extension Int32AddExtension
open extension Int32MultiplyExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface Add<T> =

    static abstract add(x: T, y: T) : T

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32) : int32 =
        __oly_add(x, y)

interface Multiply<T> =

    static abstract multiply(x: T, y: T) : T

extension Int32MultiplyExtension =
    inherits int32
    implements Multiply<int32>

    static overrides multiply(x: int32, y: int32) : int32 =
        __oly_multiply(x, y)

(+)<T>(x: T, y: T) : T where T : trait Add<T> =
    T.add(x, y)

(*)<T>(x: T, y: T) : T where T : trait Multiply<T> =
    T.multiply(x, y)

main() : () =
    let x = 2 + 2 * 3
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "8"
    |> ignore

[<Fact>]
let ``Test precedence 2``() =
    let src =
        """
open extension Int32AddExtension
open extension Int32SubtractExtension
open extension Int32MultiplyExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface Add<T> =

    static abstract add(x: T, y: T) : T

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32) : int32 =
        __oly_add(x, y)

interface Subtract<T> =

    static abstract subtract(x: T, y: T) : T

extension Int32SubtractExtension =
    inherits int32
    implements Subtract<int32>

    static overrides subtract(x: int32, y: int32) : int32 =
        __oly_subtract(x, y)

interface Multiply<T> =

    static abstract multiply(x: T, y: T) : T

extension Int32MultiplyExtension =
    inherits int32
    implements Multiply<int32>

    static overrides multiply(x: int32, y: int32) : int32 =
        __oly_multiply(x, y)

(+)<T>(x: T, y: T) : T where T : trait Add<T> =
    T.add(x, y)

(-)<T>(x: T, y: T) : T where T : trait Subtract<T> =
    T.subtract(x, y)

(*)<T>(x: T, y: T) : T where T : trait Multiply<T> =
    T.multiply(x, y)

main() : () =
    let x = 2 + 2 * 3 - 1
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Test precedence 3``() =
    let src =
        """
open extension Int32AddExtension
open extension Int32SubtractExtension
open extension Int32MultiplyExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface Add<T> =

    static abstract add(x: T, y: T) : T

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32) : int32 =
        __oly_add(x, y)

interface Subtract<T> =

    static abstract subtract(x: T, y: T) : T

extension Int32SubtractExtension =
    inherits int32
    implements Subtract<int32>

    static overrides subtract(x: int32, y: int32) : int32 =
        __oly_subtract(x, y)

interface Multiply<T> =

    static abstract multiply(x: T, y: T) : T

extension Int32MultiplyExtension =
    inherits int32
    implements Multiply<int32>

    static overrides multiply(x: int32, y: int32) : int32 =
        __oly_multiply(x, y)

(+)<T>(x: T, y: T) : T where T : trait Add<T> =
    T.add(x, y)

(-)<T>(x: T, y: T) : T where T : trait Subtract<T> =
    T.subtract(x, y)

(*)<T>(x: T, y: T) : T where T : trait Multiply<T> =
    T.multiply(x, y)

main() : () =
    let x = 2 + 2 * 3 - 1 * 5
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Test 'and' intrinsic condition``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_and(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(true, true)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'and' intrinsic condition 2``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_and(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(true, false)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'and' intrinsic condition 3``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_and(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(false, true)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'and' intrinsic condition 4``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_and(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(false, false)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'and' intrinsic condition 5``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_and(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(true, if (__oly_and(false, true)) true else false)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'and' intrinsic condition 6``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_and(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(true, if (__oly_and(true, true)) true else false)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'or' intrinsic condition``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_or(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(true, true)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'or' intrinsic condition 2``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_or(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(true, false)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'or' intrinsic condition 3``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_or(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(false, true)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'or' intrinsic condition 4``() =
    let src =
        """
test(x: bool, y: bool) : () =
    if (__oly_or(x, y))
        print("True")
    else
        print("False")

main() : () =
    test(false, false)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'not' intrinsic condition``() =
    let src =
        """
test(x: bool) : () =
    if (__oly_not(x))
        print("True")
    else
        print("False")

main() : () =
    test(true)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Test 'not' intrinsic condition 2``() =
    let src =
        """
test(x: bool) : () =
    if (__oly_not(x))
        print("True")
    else
        print("False")

main() : () =
    test(false)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'not' intrinsic condition 3``() =
    let src =
        """
test(x: bool) : () =
    let r = __oly_not(x)
    if (r)
        print("True")
    else
        print("False")

main() : () =
    test(false)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Test 'not' intrinsic condition 4``() =
    let src =
        """
test(x: bool) : () =
    let r = __oly_not(x)
    if (r)
        print("True")
    else
        print("False")

main() : () =
    test(true)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "False"
    |> ignore

[<Fact>]
let ``Simple expression based control flow is correct``() =
    let src =
        """
main() : () =
    let x =
        if (true)
            1
        else
            2
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Simple expression based control flow is correct 2``() =
    let src =
        """
main() : () =
    let x =
        if (false)
            1
        else
            2
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Simple expression based control flow is correct 3``() =
    let src =
        """
main() : () =
    let x =
        if (true)
            print(10)
            1
        else
            print(15)
            2
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "101"
    |> ignore

[<Fact>]
let ``Simple expression based control flow is correct 4``() =
    let src =
        """
main() : () =
    let x =
        if (false)
            print(10)
            1
        else
            print(15)
            2
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "152"
    |> ignore

[<Fact>]
let ``Closure test 1``() =
    let src =
        """
main() : () =
    let x = 123
    let f() = x
    print(f())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Closure test 2``() =
    let src =
        """
class Test =

    public field x: int32

    new(x: int32) = this { x = x }

main() : () =
    let t = Test(456)
    let f() = t
    print(f().x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Closure test 3``() =
    let src =
        """
class Test =

    public field x: int32

    new(x: int32) = this { x = x }

main() : () =
    let t = Test(789)
    let f() = t.x
    print(f())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "789"
    |> ignore

[<Fact>]
let ``Simple unit type test``() =
    let src =
        """
test(x: ()) : () = ()
main() : () =
    test(())
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Simple unit type test 2``() =
    let src =
        """
test(x: (), y: int32) : () = print(y)
main() : () =
    test((), 5)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple unit type test 3``() =
    let src =
        """
test(x: (), y: int32, z: (), w: int32) : () = 
    print(y)
    print(w)
main() : () =
    test((), 5, (), 10)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "510"
    |> ignore

[<Fact>]
let ``Simple unit type test 4``() =
    let src =
        """
main() : () =
    let x =
        print(123)
        ()

    let y = x
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple unit type test 5``() =
    let src =
        """
test() : int32 = 1234

f() : () -> int32 =
    test

main() : () =
    let x = f()()
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "1234"
    |> ignore

[<Fact>]
let ``Simple unit type test 6``() =
    let src =
        """
test(x: ()) : int32 = 1234

f() : ((()) -> int32) =
    test

main() : () =
    let x = f()(())
    print(x)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "1234"
    |> ignore

[<Fact>]
let ``Simple unit type test 7``() =
    let src =
        """
main() : () =
    let x = ()
    print(45)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "45"
    |> ignore

[<Fact>]
let ``Simple unit type test 8``() =
    let src =
        """
test(x: ()) : int32 = 234

main() : () =
    let x = ()
    print(test(x))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "234"
    |> ignore

[<Fact>]
let ``Simple unit type test 9``() =
    let src =
        """   
test(x: bool): int32 =
    if(x)
        ()
    else
        ()
    123

main(): () =
    print(test(true))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple unit type test 10``() =
    let src =
        """   
test(x: bool): int32 =
    ()
    123

main(): () =
    print(test(true))
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple unit type test 11``() =
    let src =
        """
test(): (()) =
    ()

main(): () =
    test()
    print(123)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple tuple type test``() =
    let src =
        """
main() : () =
    let x = (1, 2.0f)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Able to use any with a struct type - this requires boxing``() =
    let src =
        """
main() : () =
    let a = 
        (x: object) ->
            print(x)
    a(2)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Able to use any with a struct type - this requires boxing 2``() =
    let src =
        """
main() : () =
    let a = 
        static (x: object) ->
            print(x)
    a(2)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore


[<Fact>]
let ``Lambda expression can infer constraints - (not implemented)``() =
    let src =
        """
open extension Int32AddExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface Add<T> =

    static abstract add(x: T, y: T) : T

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32) : int32 =
        __oly_add(x, y)

test<T>(x: T) : T where T : trait Add<T> =
    T.add(x, x)

main() : () =
    let a = (x: int32) -> test<_>(x) // let a = (x) -> test<_>(x) // <-- implementation
    print(a(2))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"
    |> ignore

[<Fact>]
let ``Local function can infer constraints - (not implemented)``() =
    let src =
        """
open extension Int32AddExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T> =

    static abstract add(x: T, y: T): T

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32): int32 =
        __oly_add(x, y)

test<T>(x: T): T where T: trait Add<T> =
    T.add(x, x)

main(): () =
    let a(x: int32) = test<_>(x) // let a(x) = test<_>(x) // <-- implementation
    print(a(2))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"
    |> ignore

[<Fact>]
let ``Local function can infer constraints 2 - (not implemented)``() =
    let src =
        """
open extension Int32AddExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

    static abstract add(x: T1, y: T2) : T3

test<T1, T2, T3>(x: T1, y: T2) : T3 where T1 : trait Add<T1, T2, T3> =
    T1.add(x, y)

extension Int32AddExtension =
    inherits int32
    implements Add<int32, int32, int32>

    static overrides add(x: int32, y: int32) : int32 =
        __oly_add(x, y)

main() : () =
    let a(x, y) = test<int32, int32, _>(x, y) // let a(x, y) = test<_, _, _>(x, y) // <-- implementation

    let z = a(1, 2)

    print(z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Custom intrinsic int32``() =
    let src =
        """
open extension CustomInt32Extension

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias CustomInt32

extension CustomInt32Extension =
    inherits CustomInt32

    test(): CustomInt32 =
        print("hello")
        this

main(): () =
    let x = 123
    print(x.test())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello123"
    |> ignore

[<Fact>]
let ``Basic extension``() =
    let src =
        """
open extension Int32Extension

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

extension Int32Extension =
    inherits __oly_int32

    test(x: __oly_int32) : __oly_int32 = x

main() : () =
    let x = 1
    print(x.test(5))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Interface with implemented functions``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    get_x(): int32
    set_x(value: int32): ()

class Test =
    implements ITest

    new(x: int32) = this { x = x }

    field mutable x: int32
    get_x(): int32 = this.x
    set_x(value: int32): () = this.x <- value

test(t: ITest) : () = t.set_x(789)

main() : () =
    let t = Test(123)
    test(t)
    print(t.get_x())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "789"
    |> ignore

[<Fact>]
let ``Interface with implemented functions and struct impl``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    get_x(): int32
    set_x(value: int32): ()

class Test =
    implements ITest

    new(x: int32) = this { x = x }

    field mutable x: int32
    get_x(): int32 = this.x
    set_x(value: int32): () = this.x <- value

castTest(t: ITest) : ITest = t

test(t: ITest) : () = t.set_x(789)

main() : () =
    let t = Test(123)
    test(t)
    print(t.get_x())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "789"
    |> ignore

[<Fact>]
let ``Interface with implemented functions and type impl and passing the interface as byref``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    get_x(): int32
    set_x(value: int32): ()

class Test =
    implements ITest

    new(x: int32) = this { x = x }

    field mutable x: int32
    get_x(): int32 = this.x
    set_x(value: int32): () = this.x <- value

castTest(t: ITest) : ITest = t

test(t: byref<ITest>) : () = t.set_x(789)

main() : () =
    let mutable tOrig = Test(123)
    let mutable t = castTest(tOrig)
    test(&t)
    print(t.get_x())
    print(tOrig.get_x())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "789789"
    |> ignore

[<Fact>]
let ``Interface with implemented functions and struct impl and passing the interface as byref``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    get_x(): int32
    set_x(value: int32): ()

struct Test =
    implements ITest

    new(x: int32) = this { x = x }

    field mutable x: int32
    get_x(): int32 = this.x
    mutable set_x(value: int32): () = this.x <- value

castTest(t: ITest) : ITest = t

test(t: byref<ITest>) : () = t.set_x(789)

main() : () =
    let mutable tOrig = Test(123)
    let mutable t = castTest(tOrig)
    test(&t)
    print(t.get_x())
    print(tOrig.get_x())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "789123"
    |> ignore

[<Fact>]
let ``Interface with a field mutable and struct impl and passing the interface as byref 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    get_x(): int32
    set_x(value: int32): ()

interface ITest2 =

    get_t(): ITest
    set_t(value: ITest): ()

struct Test =
    implements ITest

    new(x: int32) = this { x = x }

    public field mutable x: int32
    get_x(): int32 = this.x
    mutable set_x(value: int32): () = this.x <- value

struct Test2 =
    implements ITest2
    
    new(t: ITest) = this { t = t }

    public field mutable t: ITest
    get_t(): ITest = this.t
    mutable set_t(value: ITest): () = this.t <- value

test(t2: byref<ITest2>) : () = t2.get_t().set_x(789)

castTest2(t2: ITest2) : ITest2 = t2

main() : () =
    let mutable tOrig = Test(123)
    let mutable t2Orig = Test2(tOrig)
    let mutable t2 = castTest2(t2Orig)
    test(&t2)
    print(t2.get_t().get_x())
    print(t2Orig.get_t().get_x())
    print(tOrig.x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "789789123"
    |> ignore

[<Fact>]
let ``Local function capturing type var to build a closure works``() =
    let src =
        """
main() : () =
    print(Invoke<int32>(1))

Invoke<T>(x: T) : T =
    let call() = x
    call()
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Should run successfully with an attempted ambiguous trait impl``() =
    let src =
        """
open extension Int32AddExtension
open extension Int32AddFloat64Extension

#[intrinsic("int32")]
alias int32

#[intrinsic("float64")]
alias float64

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

    static abstract add(x: T1, y: T2) : T3

extension Int32AddExtension =
    inherits int32
    implements Add<int32, int32, int32>

    static overrides add(x: int32, y: int32) : int32 =
        __oly_add(x, y)

extension Int32AddFloat64Extension =
    inherits int32
    implements Add<int32, float64, float64>

    static overrides add(x: int32, y: float64) : float64 =
        y

test<T1, T2, T3>(x: T1, y: T2) : T3 where T1 : trait Add<T1, T2, T3> =
    T1.add(x, y)

main() : () =
    print(test<_, _, int32>(1, 2))
    print(test<_, _, float64>(1, 2.0))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "32"
    |> ignore

[<Fact>]
let ``Should run successfully with an attempted ambiguous trait impl 2``() =
    let src =
        """
open extension Int32AddExtension
open extension Int32AddFloat64Extension
open extension Float64AddInt32Extension
open extension Float64AddInt32Extension2

#[intrinsic("int32")]
alias int32

#[intrinsic("float64")]
alias float64

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface Add<T1, T2, T3> =

    static abstract add(x: T1, y: T2): T3

extension Int32AddExtension =
    inherits int32
    implements Add<int32, int32, int32>

    static overrides add(x: int32, y: int32): int32 =
        __oly_add(x, y)

extension Int32AddFloat64Extension =
    inherits int32
    implements Add<int32, float64, int32>

    static overrides add(x: int32, y: float64): int32 =
        x

extension Float64AddInt32Extension =
    inherits float64
    implements Add<int32, int32, int32>

    static overrides add(x: int32, y: int32): int32 =
        __oly_add(y, y)

extension Float64AddInt32Extension2 =
    inherits float64
    implements Add<int32, float64, int32>

    static overrides add(x: int32, y: float64): int32 =
        676

test<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait Add<T1, T2, T1>; where T2: trait Add<T1, T2, T3> =
    T2.add(T1.add(x, y), y)

main(): () =
    print(test<_, _, int32>(1, 2))
    print(test<_, _, int32>(1, 2.0))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5676"
    |> ignore

[<Fact>]
let ``Should run successfully with a open module``() =
    let src =
        """
open static TestModule

#[intrinsic("int32")]
alias int32

module TestModule =

    class Console =

        #[intrinsic("print")]
        static Write(int32) : ()

main() : () =
    Console.Write(5)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Should run successfully with a open module 2``() =
    let src =
        """
open static TestModule
open extension Int32Extension

test<T>() : () where T : trait Test = ()

#[intrinsic("int32")]
alias int32

module TestModule =

    interface Test

main() : () =
    test<int32>()

extension Int32Extension =
    inherits int32
    implements Test
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Should run successfully with a open module 3``() =
    let src =
        """
open static TestModule

test<T>() : () where T : { x: int32 get } = ()

#[intrinsic("int32")]
alias int32

module TestModule =

    class Test =
        x: int32 get = 0

main() : () =
    test<Test>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Should run successfully with a open module 4``() =
    let src =
        """
open static TestModule
open extension TestExtension

interface TestTrait<T> where T : trait { x: int32 get }

test<T>() : () where T : trait { x: int32 get }, trait TestTrait<T> = ()

#[intrinsic("int32")]
alias int32

module TestModule =

    class Test =
        x: int32 get = 0

extension TestExtension =
    inherits Test
    implements TestTrait<Test>

main() : () =
    test<Test>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Should run successfully with an explicit extension``() =
    let src =
        """
open extension Int32Extension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

    static abstract (+)(x: T1, y: T2) : T3

(+)<T>(x: T, y: T) : T where T : trait Add<T, T, T> =
    T.(+)(x, y)

extension Int32Extension =
    inherits int32
    implements Add<int32, int32, int32>

    static overrides (+)(x: int32, y: int32) : int32 =
        __oly_add(x, y)

main() : () =
    print(1 + 3)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"
    |> ignore

[<Fact>]
let ``Nested module calls should work``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("print")]
print(__oly_object): ()

module Test1 =

    printTest1(): () = print("Test1")

    module Test2 =

        printTest2(): () = print("Test2")

main(): () =
    Test1.printTest1()
    Test1.Test2.printTest2()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test1Test2"
    |> ignore

[<Fact>]
let ``Nested type calls with ctor value should work``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("print")]
print(__oly_object): ()

class Test1 =

    static printTest1() : () = print("Test1")

    class Test2 =

        new() = this { }

        printTest2() : () = print("Test2")

main(): () =
    Test1.printTest1()
    let f = Test1.Test2
    let x = f()
    x.printTest2()
    ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test1Test2"
    |> ignore

[<Fact>]
let ``Nested type calls should work``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

class Test1<T> =

    class Test2<U, V> =

        new() = this { }

        printTest2(t: T, u: U, v: V) : () = 
            print(t)
            print(u)
            print(v)

main(): () =
    let x = Test1<int32>.Test2<float32, utf16>()
    x.printTest2(1, 2.3f, "Hello World!")
    ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "12.3Hello World!"
    |> ignore

[<Fact>]
let ``Nested type calls should work 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("float32")]
alias float32

#[intrinsic("bool")]
alias bool

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

class Test1<T> =

    class Test2<U, V> =

        new() = this { }

        class Test3<Z> =

            new() = this { }

            print(t: T, u: U, v: V, z: Z) : () =
                print(t)
                print(u)
                print(v)
                print(z)

main(): () =
    let x = Test1<int32>.Test2<float32, utf16>.Test3<bool>()
    x.print(1, 2.3f, "Hello World!", true)
    ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "12.3Hello World!True"
    |> ignore

[<Fact>]
let ``Nested type calls should work 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(object): ()

class Test1<T> =

    class Test2<U, V> =

        new() = this { }

        class Test3<Z> =

            new() = this { }

            static print(t: T, u: U, v: V, z: Z) : () =
                print(t)
                print(u)
                print(v)
                print(z)

main(): () =
    Test1<int32>.Test2<float32, utf16>.Test3<bool>.print(1,  4.3f, "yeet", true)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "14.3yeetTrue"
    |> ignore

[<Fact>]
let ``Nested type calls should work 4``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(object): ()

class Test1<T> =

    class Test2<U, V> =

        new() = this { }

        class Test3<Z> =

            new() = this { }

            static print(t: T, u: U, v: V, z: Z) : () =
                print(t)
                print(u)
                print(v)
                print(z)

test<T<_>>(x: T<bool>): () = print("test")

main(): () =
    let t = Test1<int32>.Test2<float32, utf16>.Test3<bool>()
    test<Test1<int32>.Test2<float32, utf16>.Test3>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Nested type calls should work 5``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(object): ()

interface ITest =

    M(): ()

class Test1<T> =

    class Test2<U, V> =

        new() = this { }

        class Test3<Z> =
            implements ITest

            new() = this { }

            M(): () = print("M")

            static print(t: T, u: U, v: V, z: Z) : () =
                print(t)
                print(u)
                print(v)
                print(z)

test<T<_>>(x: T<bool>): () where T: ITest = 
    (x : ITest).M()
    print("test")

main(): () =
    let t = Test1<int32>.Test2<float32, utf16>.Test3<bool>()
    test<Test1<int32>.Test2<float32, utf16>.Test3>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Mtest"
    |> ignore

[<Fact>]
let ``Nested type calls should work 6``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(object): ()

interface ITest =

    M(): ()

class Test1<T> =

    class Test2<U, V> =

        new() = this { }

        class Test3<Z> =
            implements ITest

            new() = this { }

            M(): () = print("M")

            static print(t: T, u: U, v: V, z: Z) : () =
                print(t)
                print(u)
                print(v)
                print(z)

test<T<_>>(x: T<bool>): () where T: ITest = 
    x.M()
    print("test")

main(): () =
    let t = Test1<int32>.Test2<float32, utf16>.Test3<bool>()
    test<Test1<int32>.Test2<float32, utf16>.Test3>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Mtest"
    |> ignore

[<Fact>]
let ``Nested type calls should work 7``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(object): ()

class Test1<T> =

    class Test2<U, V> =

        new() = this { }

        class Test3<Z> =

            new() = this { }

            static print(t: T, u: U, v: V, z: Z) : () =
                print(t)
                print(u)
                print(v)
                print(z)

test<T<_>>(x: T<bool>): () = print("test")

main(): () =
    let t = Test1<int32>.Test2<float32, utf16>.Test3<bool>()
    test(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Nested type calls should work 8``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias utf16

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

class Test1<T> =

    class Test2<U, V> =

        new() = this { }

        class Test3<Z> =

            new() = this { }

            static print(t: T, u: U, v: V, z: Z) : () =
                print(t)
                print(u)
                print(v)
                print(z)

class A<T<_>>

test<T<_>>(x: T<A<T>>): () = print("test")

main(): () =
    let t = Test1<int32>.Test2<float32, utf16>.Test3<A<Test1<int32>.Test2<float32, utf16>.Test3>>()
    test(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``A basic function marked with the inline attribute will be inlined by the runtime``() =
    let src =
        """
#[inline]
add(x: __oly_int32, y: __oly_int32): __oly_int32 = __oly_add(x, y)

main(): () =
    let result = add(1, 2)
    print(result)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``A basic function marked with the inline attribute will be inlined by the runtime 2``() =
    let src =
        """
#[inline]
add(x: __oly_int32, y: __oly_int32): __oly_int32 = 
    print(y)
    __oly_add(x, y)

main(): () =
    let result = add(1, 2)
    print(result)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "23"
    |> ignore

[<Fact>]
let ``byref set should work``() =
    let src =
        """
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

main(): () =
    let mutable y = 1
    print(y)

    let yref = &y
    yref <- 3
    print(y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "13"
    |> ignore

[<Fact>]
let ``byref set should work 2``() =
    let src =
        """
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

test(x: byref<int32>): () = x <- 5

main(): () =
    let mutable y = 1
    print(y)

    test(&y)
    print(y)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "15"
    |> ignore

[<Fact>]
let ``Mutable value with a function gets captured correctly``() =
    let src =
        """
Σ(min: int32, max: int32, f: int32 -> (), x: int32): () =
    let loop(min) =
        if(__oly_less_than_or_equal(min, max))
            f(min)
            loop(__oly_add(min, 1))
        else
            min
    let min = loop(min)

main(): () =
    let mutable a = (x) -> x
    let λ = 1

    Σ(λ, 5, x -> print(a(x)), 5)
        """
    OlySharp src
    |> withCompile
    |> shouldRunWithExpectedOutput "12345"
    |> ignore

[<Fact>]
let ``Witness pass subsumption``() =
    let src =
        """
open extension Int32AddExtension

interface Add<T1, T2, T3> =

    static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

#[intrinsic("int32")]
alias int32

(+)<T>(x: T, y: T): T where T: trait Add<T, T, T> = T.add(x, y)

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides add(x: int32, y: int32): int32 = __oly_add(x, y)

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    print(1 + 1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Witness pass subsumption 2``() =
    let src =
        """
open extension Int32AddExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

    static abstract test(): ()

test<T>(): () where T: trait Add<T, T, T> = 
    T.test()
    print("witness")

interface Add<T> =
    inherits Add<T, T, T>

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides test(): () = ()

main(): () =
    test<int32>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "witness"
    |> ignore

[<Fact>]
let ``Witness pass subsumption 3``() =
    let src =
        """
open extension Int32AddExtension
open static TestModule

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module TestModule =

    interface Add<T1, T2, T3> =

        static abstract test(): ()

test2<T>(): () where T: trait Add<T, T, T> = 
    T.test()
    print("witness")

test<T>(): () where T: trait Add<T, T, T> = 
    T.test()
    print("witness")

interface Add<T> =
    inherits Add<T, T, T>

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides test(): () = ()

main(): () =
    test<int32>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "witness"
    |> ignore

[<Fact>]
let ``Byref should work with shape``() =
    let src =
        """
#[intrinsic("by_ref")]
alias (&)<T>

#[intrinsic("address_of")]
(&)<T>(T): T&

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct TestStruct =

    x: int32 get, set

    new(x: int32) = this { x = x }

test<T>(x: T&): int32 where T: { x: int32 get, set } = 
    x.x <- 500
    x.x

main(): () =
    let mutable testStruct = TestStruct(123)
    print(testStruct.x)
    let result = test<_>(&testStruct)
    print(testStruct.x)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123500500"
    |> ignore

[<Fact>]
let ``Shape constraint should run and succeed from inherited instance function``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

abstract class Test1 =

    new() = this { }

    test(): () = print("Test1")

class Test =
    inherits Test1

    field x: int32
    field y: int32
    new(x: int32, y: int32) = this { x = x; y = y }

shape TestShape =

    test(): ()

test<T>(x: T) : int32 where T: TestShape =
    x.test()
    88

main() : () =
    let t = Test(7, 9)
    let result = test<_>(t)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test188"
    |> ignore

[<Fact>]
let ``Generic shape constraint should run and succeed from inherited instance function``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

abstract class Test1 =

    new() = this { }

    test(): () = print("Test1")

class Test =
    inherits Test1

    x: float32 get
    y: int32 get
    new(x: float32, y: int32) = this { x = x; y = y }

test<T>(x: T) : int32 where T: { x: float32 get; y: int32 get; test(): () } =
    x.test()
    x.y

main() : () =
    let t = Test(7.0f, 9)
    let result = test<_>(t)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test19"
    |> ignore

[<Fact>]
let ``Generic shape constraint should run and succeed from inherited instance function 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

abstract class Test1 =

    new() = this { }

    test(): () = print("Test1")

class Test<T1, T2, T3, T4, T5> =
    inherits Test1

    x: T3 get
    y: T1 get
    new(x: T3, y: T1) = this { x = x; y = y }

test<T>(x: T) : int32 where T: { x: float32 get; y: int32 get; test(): () } =
    x.test()
    x.y

main() : () =
    let t = Test<int32, __oly_object, float32, __oly_object, __oly_object>(7.0f, 9)
    let result = test<_>(t)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test19"
    |> ignore

[<Fact>]
let ``Complex interface``() =
    let src =
        """
open extension Int32DootExtension
open extension TestBestTrait
open extension Int32BestTrait

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface BestTrait =

  Best<T>(x: T): T

interface IDoot =
  
  Doot<T>(x: T): T where T: trait BestTrait

extension Int32DootExtension =
    inherits int32
    implements IDoot

    Doot<T>(x: T): T where T: trait BestTrait =
      print("Int32DootExtension-Doot")
      x.Best<T>(x)

test<T>(x: T): () where T: trait IDoot, trait BestTrait = 
  let res = x.Doot<T>(x)
  ()

class Test =
  implements IDoot

  new() = this { }

  Doot<T>(x: T): T where T: trait BestTrait = 
    print("Test-Doot")
    x.Best<T>(x)

extension TestBestTrait =
    inherits Test
    implements BestTrait

    Best<T>(x: T): T = x

extension Int32BestTrait =
    inherits int32
    implements BestTrait

    Best<T>(x: T): T = x

main(): () =
  let tt = Test()
  test<_>(tt)
  test<_>(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test-DootInt32DootExtension-Doot"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance``() =
    let src =
        """
open extension Int32TestExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

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
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "567"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 2``() =
    let src =
        """
open extension Int32TestExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =
 
    default test(): int32 = 567

extension Int32TestExtension =
    inherits int32
    implements ITest

    test(): int32 = 123

getResult(x: ITest): int32 =
    x.test()

#[intrinsic("cast")]
cast<T>(__oly_object): T

main(): () =
    let result = getResult(cast<ITest>(567))
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 3``() =
    let src =
        """
open extension Int32TestExtension

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test(): int32

extension Int32TestExtension =
    inherits int32
    implements ITest

    test(): int32 = 123

getResult(x: ITest): int32 =
    x.test()

getResult2<T>(x: T): int32 where T: trait ITest =
    getResult(x)

main(): () =
    let result = getResult2<_>(567)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 4``() =
    let src =
        """
open extension TestExtension<_>

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test(): int32

class Test<T> =

    new() = this { }

extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    test(): int32 = 123

getResult(x: ITest): int32 =
    x.test()

getResult2<T<_>>(x: T<int32>): int32 where T: trait ITest =
    getResult(x)

main(): () =
    let result = getResult2<_>(Test<int32>())
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 5``() =
    let src =
        """
open extension TestExtension<_>

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test(): int32

class Test<T> =

    new() = this { }

extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    test(): int32 = 123

getResult2<T<_>>(x: T<int32>): int32 where T: trait ITest =
    (x : ITest).test()

main(): () =
    let result = getResult2<_>(Test<int32>())
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 6``() =
    let src =
        """
open extension TestExtension<_>

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test(): int32

class Test<T> =

    new() = this { }

extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    test(): int32 = 123

getResult2<T<_>>(x: T<int32>): int32 where T: trait ITest =
    x.test()

main(): () =
    let result = getResult2<_>(Test<int32>())
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 7``() =
    let src =
        """
open extension TestExtension<_>

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test<T>(): int32

class Test<T> =

    new() = this { }

extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    test<U>(): int32 = 123

getResult2<T<_>>(x: T<int32>): int32 where T: trait ITest =
    x.test<T<object>>()

main(): () =
    let result = getResult2<_>(Test<int32>())
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 8``() =
    let src =
        """
open extension TestExtension<_>

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test<T>(): int32 where T: trait ITest

class Test<T> =

    new() = this { }

extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    test<U>(): int32 where U: trait ITest = 123

getResult2<T<_>>(x: T<int32>): int32 where T: trait ITest =
    x.test<T<object>>()

main(): () =
    let result = getResult2<_>(Test<int32>())
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 9``() =
    let src =
        """
open extension TestExtension<_>
open extension TestExtension2<_>

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test<T>(): int32 where T: trait ITest

interface ITest2 =

   test2<T>(): int32 where T: trait ITest2

class Test<T> =

    new() = this { }

extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    test<U>(): int32 where U: trait ITest = 123

extension TestExtension2<T> =
    inherits Test<T>
    implements ITest2

    test2<U>(): int32 where U: trait ITest2 = 456

getResult<T<_>>(x: T<int32>): int32 where T: trait ITest, trait ITest2 =
    x.test<T<object>>()

getResult2<T<_>>(x: T<int32>): int32 where T: trait ITest, trait ITest2 =
    x.test2<T<object>>()

printCombined<T<_>>(x: T<int32>): () where T: trait ITest, trait ITest2 =
    print(x.test<T<object>>())
    print(x.test2<T<object>>())

main(): () =
    let result = getResult<_>(Test<int32>())
    print(result)
    let result = getResult2<_>(Test<int32>())
    print(result)
    printCombined(Test<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123456123456"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 10``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test<T>(): int32 where T: trait ITest

interface ITest2 =

   test2<T>(): int32 where T: trait ITest2

class Test<T> =

    new() = this { }

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    test<U>(): int32 where U: trait ITest = 123

#[open]
extension TestExtension2<T> =
    inherits Test<T>
    implements ITest2

    test2<U>(): int32 where U: trait ITest2 = 456

getResult<T<_>>(x: T<int32>): int32 where T: trait ITest, trait ITest2 =
    x.test<Test<int32>>()

getResult2<T<_>>(x: T<int32>): int32 where T: trait ITest, trait ITest2 =
    x.test2<Test<int32>>()

printCombined<T<_>>(x: T<int32>): () where T: trait ITest, trait ITest2 =
    print(x.test<Test<int32>>())
    print(x.test2<Test<int32>>())

main(): () =
    let result = getResult<_>(Test<int32>())
    print(result)
    let result = getResult2<_>(Test<int32>())
    print(result)
    printCombined(Test<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123456123456"
    |> ignore

[<Fact>]
let ``Pass an extended type as an instance 11``() =
    let src =
        """
open extension TestExtension<_>
open extension TestExtension2<_>

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test<T>(): int32 where T: trait ITest

interface ITest2 =

   test2<T>(): int32 where T: trait ITest2

class Test<T> =

    new() = this { }

extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    test<U>(): int32 where U: trait ITest = 123

extension TestExtension2<T> =
    inherits Test<T>
    implements ITest2

    test2<U>(): int32 where U: trait ITest2 = 456

getResult<T<_>>(x: T<int32>): int32 where T: trait ITest, trait ITest2 =
    x.test<T<object>>()

getResult2<T<_>>(x: T<int32>): int32 where T: trait ITest, trait ITest2 =
    x.test2<T<object>>()

printCombined<T<_>>(x: T<int32>): () where T: trait ITest, trait ITest2 =
    print(x.test<T<object>>())
    print(x.test2<T<object>>())

main(): () =
    let result = getResult<_>(Test<int32>())
    print(result)
    let result = getResult2<_>(Test<int32>())
    print(result)
    printCombined(Test<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123456123456"
    |> ignore

[<Fact>]
let ``Pass an implemented interface type as an instance``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(object): ()

interface ITest =
 
    test(): int32

class Test =
    implements ITest

    new() = this { }

    test(): int32 = 123

getResult(x: ITest): int32 =
    x.test()

getResult2<T>(x: T): int32 where T: ITest =
    getResult(x)

main(): () =
    let result = getResult2<_>(Test())
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Auto generalized partially applied function should run``() =
    let src = 
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IExample

class Example =
    implements IExample

    new() = this { }

test<T>(x: T): () where T: IExample = print("success")

main(): () =
    let example = Example()
    let f = test<_>
    f(example)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "success"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 1``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32): int32 =
    let result =
        match (x)
        | 1 => 5
        | 4 => 9
        | _ => 3

    result    
    
main(): () =
    let result = test(1, 3)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 2``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32): int32 =
    let result =
        match (x)
        | 1 => 5
        | 4 => 9
        | _ => 3

    result    
    
main(): () =
    let result = test(4, 3)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 3``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32): int32 =
    let result =
        match (x)
        | 1 => 5
        | 4 => 9
        | _ => 3

    result    
    
main(): () =
    let result = test(5, 3)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 4``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32): int32 =
    let result =
        match (x, y)
        | 1, 10 => 5
        | 4, 25 => 9
        | _, _ => 3

    result    
    
main(): () =
    let result = test(1, 10)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 5``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32): int32 =
    let result =
        match (x, y)
        | 1, 10 => 5
        | 4, 25 => 9
        | _, _ => 3

    result    
    
main(): () =
    let result = test(4, 25)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 6``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32): int32 =
    let result =
        match (x, y)
        | 1, 10 => 5
        | 4, 25 => 9
        | _, _ => 3

    result    
    
main(): () =
    let result = test(100, 100)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 7``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32, z: int32): int32 =
    let result =
        match (x, y, z)
        | 1, 10, 14 => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(1, 10, 14)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 8``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32, z: int32): int32 =
    let result =
        match (x, y, z)
        | 1, 10, 14 => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(4, 25, 63)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 9``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32, z: int32): int32 =
    let result =
        match (x, y, z)
        | 1, 10, 14 => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(1, 1, 1)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 10``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 1, 10, 14 when (w == 4) => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(1, 10, 14, 4)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 11``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 1, 10, 14 when (w == 4) => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(1, 10, 14, 5)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 12``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 1, 10, 14 => 5
        | 4, 25, 63 when (w == 5) => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(1, 10, 14, 4)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 13``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 1, 10, 14 => 5
        | 4, 25, 63 when (w == 5) => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(4, 25, 63, 2)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore
    
[<Fact>]
let ``Simple pattern match should give expected output 14``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 1, 10, 14 => 5
        | 4, 25, 63 when (w == 5) => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(4, 25, 63, 5)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 15``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32): int32 =
    let result =
        match (x, y, z)
        | 4, 25, 63
        | 1, 10, 14 => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(4, 25, 63)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 16``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32): int32 =
    let result =
        match (x, y, z)
        | 4, 25, 63
        | 1, 10, 14 => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(1, 10, 14)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 17``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32): int32 =
    let result =
        match (x, y, z)
        | 4, 25, 63
        | 1, 10, 14 => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(2, 10, 14)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 18``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 4, 25, 63
        | 1, 10, 14 when (w == 99) => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(4, 25, 63, 99)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 19``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 4, 25, 63
        | 1, 10, 14 when (w == 99) => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(4, 25, 63, 98)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 20``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 4, 25, 63
        | 1, 10, 14 when (w == 99) => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(1, 10, 14, 99)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 21``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    let result =
        match (x, y, z)
        | 4, 25, 63
        | 1, 10, 14 when (w == 99) => 5
        | 4, 25, 63 => 9
        | _, _, _ => 3

    result    
    
main(): () =
    let result = test(1, 10, 14, 98)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 22``() =
    let src = 
        """
#[intrinsic("utf16")]
alias string

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(str1: string, str2: string): string =
    let result =
        match (str1, str2)
        | "hello", "world" => "hello world"
        | _, _ => "no hello world"

    result    
    
main(): () =
    let result = test("hello", "world")
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello world"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 23``() =
    let src = 
        """
#[intrinsic("utf16")]
alias string

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

test(str1: string, str2: string): string =
    let result =
        match (str1, str2)
        | "hello", "world" => "hello world"
        | _, _ => "no hello world"

    result    
    
main(): () =
    let result = test("hello", "world0")
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "no hello world"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 24``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | _, 678, _
    | 1, 10, 14 when (w == 99) => 5
    | 4, 25, 63 => 9
    | _, _, _ => 3   
    
main(): () =
    let result = test(5, 678, 1, 99)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 24_5``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | _, 678, _
    | _, _, _ when (w == 99) => 5
    | _, _, _ => 3   
    
main(): () =
    let result = test(5, 678, 1, 99)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore


[<Fact>]
let ``Simple pattern match should give expected output 25``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | _, 678, _
    | 1, 10, 14 when (w == 99) => 5
    | 4, 25, 63 => 9
    | _, _, _ => 3   
    
main(): () =
    let result = test(5, 677, 1, 99)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 26``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | _, 678, _
    | 1, _, 14 when (w == 99) => 5
    | 4, 25, 63 => 9
    | _, _, _ => 3   
    
main(): () =
    let result = test(1, 55, 14, 99)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 27``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | _, 678, _
    | 1, _, 14 when (w == 99) => 5
    | 4, 25, 63 => 9
    | _, _, _ => 3   
    
main(): () =
    let result = test(1, 55, 14, 98)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 28``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | _, 678, _
    | 1, _, 14 when (w == 99) => 5
    | 4, 25, 63 => 9
    | _, _, _ => 3   
    
main(): () =
    let result = test(4, 25, 63, 99)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 29``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | _, 678, _
    | 1, _, 14 when (w == 99) => 5
    | 4, 25, 63 when (w == 100) => 9
    | _, _, _ => 3   
    
main(): () =
    let result = test(4, 25, 63, 100)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 30``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | 4, 678, _
    | 4, _, 14 when (w == 99) => 5
    | 4, 25, 63 when (w == 100) => 9
    | _, _, _ => 3   
    
main(): () =
    let result = test(4, 25, 63, 100)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 31``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): int32 =
    match (x, y, z)
    | 1, m, 14 when (w == 99) => m
    | 4, 25, m => m
    | _, _, _ => 3   
    
main(): () =
    let result = test(1, 55, 14, 99)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "55"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 32``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32): int32 =
    match (x, y, z)
    | m, b, _
    | _, m, b when (m == 100) => b
    | _, _, _ => 3   
    
main(): () =
    let result = test(1, 100, 55)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "55"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 33``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 = x

test(x: int32): () =
    match(x)
    | GetValue(123) => print("it is 123 !!")
    | _ => ()

main(): () =
    test(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "it is 123 !!"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 34``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 = x

test(x: int32, y: int32): () =
    match(x, y)
    | GetValue(123), GetValue(456) => print("passed")
    | _ => ()

main(): () =
    test(123, 456)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 35``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 =
    print("GetValue")
    x

test(x: int32, y: int32): () =
    match(x, y)
    | GetValue(123), GetValue(456) => print("passed")
    | _ => ()

main(): () =
    test(123, 456)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "GetValueGetValuepassed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 36``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 =
    print("GetValue")
    x

test(x: int32, y: int32): () =
    match(x, y)
    | GetValue(123), GetValue(456) => print("passed")
    | _ => ()

main(): () =
    test(999, 456)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "GetValue"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 37``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 =
    print("GetValue")
    x

test(x: int32, y: int32): () =
    match(x, y)
    | 999, _ => print("passed")
    | GetValue(123), GetValue(456) => print("failed")
    | _ => ()

main(): () =
    test(999, 456)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 38``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 =
    print("GetValue")
    x

test(x: int32, y: int32): () =
    match(x, y)
    | 999, _ => print("failed")
    | GetValue(123), GetValue(456) => print("failed")
    | 888, 456 => print("passed")
    | _ => ()

main(): () =
    test(888, 456)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "GetValuepassed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 39``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 =
    print("GetValue")
    x

test(x: int32, y: int32): () =
    match(x, y)
    | 999, 555 => print("failed")
    | GetValue(123), GetValue(456) => print("failed")
    | 888, 456 => print("failed")
    | _ => ()

main(): () =
    test(888, 555)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "GetValue"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 40``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 =
    print("GetValue")
    x

test(x: int32, y: int32): () =
    match(x, y)
    | 999, 555 => print("failed")
    | GetValue(123), GetValue(456) => print("failed")
    | 888, 456 => print("failed")
    | _ => ()

main(): () =
    test(123, 555)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "GetValueGetValue"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 41``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern GetValue(x: int32): int32 =
    print("GetValue")
    x

test(x: int32, y: int32): () =
    match(x, y)
    | GetValue(123), GetValue(GetValue(456)) => print("passed")
    | _ => ()

main(): () =
    test(123, 456)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "GetValueGetValueGetValuepassed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 42``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(x: int32, y: int32): () =
    match((x, y))
    | (z, w) =>
        print(z)
        print(w)

main(): () =
    test(123, 555)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123555"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 43``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32): () =
    match((x, y))
    | (z, w) when (w == 555) =>
        print(z)
        print(w)
    | _ => 
        ()

main(): () =
    test(123, 555)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123555"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 44``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32): () =
    match((x, y))
    | (z, w) when (w == 55) =>
        print(z)
        print(w)
    | (_, w) when (w == 555) =>
        print("_")
        print(w)
    | _ => 
        ()

main(): () =
    test(123, 555)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "_555"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 45``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

test(x: int32, y: int32, z: int32, w: int32): () =
    match((x, y), z, (w, x))
    | (a, b), c, (d, e) =>
        print(a)
        print(b)
        print(c)
        print(d)
        print(e)

main(): () =
    test(1, 2, 3, 4)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "12341"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 46``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("and")]
(&&)(bool, bool): bool

test(x: int32, y: int32, z: int32, w: int32): () =
    match((x, y), z, (w, x))
    | (a, b), c, (d, e) when (e == 10) =>
        print(a)
        print(b)
        print(c)
        print(d)
        print(e)
    | (a, b), c, (d, e) when (a == 1 && e == 1) =>
        print("passed")
    | _ => 
        ()

main(): () =
    test(1, 2, 3, 4)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 47``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("and")]
(&&)(bool, bool): bool

test(x: int32, y: int32, z: int32, w: int32): () =
    match((x, y), z, (w, x))
    | (1, b), c, (d, e) when (e == 10) =>
        print("failed")
    | (a, 2), 3, (d, 1) when (d == 4 && a == 1) =>
        print("passed")
    | _ => 
        ()

main(): () =
    test(1, 2, 3, 4)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 48``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("and")]
(&&)(bool, bool): bool

#[intrinsic("add")]
(+)(int32, int32): int32

pattern AddOne(x: int32): int32 =
    print("AddOne")
    x + 1

test(x: int32, y: int32, z: int32, w: int32): () =
    match((x, y), z, (w, x))
    | (a, b), c, (d, 2) =>
        print("failed")
    | (a, 2), 3, (d, AddOne(2)) when (d == 4 && a == 1) =>
        print("passed")
    | _ => 
        ()

main(): () =
    test(1, 2, 3, 4)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "AddOnepassed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 49``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("and")]
(&&)(bool, bool): bool

#[intrinsic("add")]
(+)(int32, int32): int32

pattern AddOne(x: int32): int32 =
    print("AddOne")
    x + 1

test(x: int32, y: int32, z: int32, w: int32): () =
    match((x, y), z, (w, x))
    | (a, b), c, (d, 2) =>
        print("failed")
    | (a, 2), 3, (d, AddOne(AddOne(3))) when (d == 4 && a == 1) =>
        print("passed")
    | _ => 
        ()

main(): () =
    test(1, 2, 3, 4)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "AddOneAddOnepassed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 50``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

pattern Tuple(x: int32): (int32, int32) =
    (x, x)

test(x: int32): () =
    match(x)
    | Tuple(x, y) =>
        print(x)
        print(y)

main(): () =
    test(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "11"
    |> ignore


[<Fact>]
let ``Simple pattern match should give expected output 51``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

pattern Tuple(x: int32): (int32, int32) =
    (x, x)

test(x: int32): () =
    match(x)
    | Tuple(x, Tuple(y, z)) =>
        print(x)
        print(y)
        print(z)

main(): () =
    test(2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "222"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 52``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern Tuple(x: int32): (int32, int32) =
    (x, x)

test(x: int32): () =
    match(x)
    | Tuple(_, Tuple(_, z)) when (z == 3) =>
        print("failed")
    | Tuple(2, w) when (w == 2) =>
        print("passed")
    | _ => 
        ()

main(): () =
    test(2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 53``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern Tuple(x: int32): (int32, int32) =
    (x, x)

test(x: int32): () =
    match(x)
    | Tuple(_, Tuple(_, z)) when (z == 3) =>
        print("failed")
    | Tuple(2, w) when (w == 2) =>
        match(w)
        | Tuple(_, Tuple(_, z)) when (z == 3) =>
            print("failed")
        | Tuple(2, w) when (w == 2) =>
            print("passed")
        | _ => 
            ()
    | _ => 
        ()

main(): () =
    test(2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match should give expected output 54``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

#[open]
enum ABC =
    | A
    | B
    | C

test2(): int32 = 998

test(abc: ABC): () =
    let value =
        match (abc)
        | A
        | B =>
            let x = test2()
            x
        | C =>
            123
        | _ =>
            997

    print(value)

main(): () =
    test(C)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<int32>): () =
    match (option)
    | Some(value) => print(value)
    | _ => print("--None--")

main(): () =
    test(Some(456))
    test(None())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456--None--"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 2``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<int32>): () =
    match ((option, option))
    | (_, Some(value)) => print(value)
    | _ => print("--None--")

main(): () =
    test(Some(456))
    test(None())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456--None--"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 3``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<int32>): () =
    match ((option, option), option)
    | (_, Some(value)), Some(value2) when (value2 == 456) => print(value)
    | _, _ => print("--None--")

main(): () =
    test(Some(456))
    test(None())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456--None--"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 4``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<int32>): () =
    match ((option, option), option)
    | (_, Some(value)), Some(value2) when (value2 == 452) => print(value)
    | _, _ => print("--None--")

main(): () =
    test(Some(456))
    test(None())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "--None----None--"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 5``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<int32>): () =
    match ((option, option), option)
    | (_, Some(value)), Some(value2) when (value2 == 452) => print(value)
    | _, Some(456) => print("passed")
    | _, _ => print("--None--")

main(): () =
    test(Some(456))
    test(None())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed--None--"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 6``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<int32>): () =
    match ((option, option), option)
    | (Some(value3), Some(value)), Some(value2) when (value2 == 452) => print(value)
    | _, Some(456) => print("passed")
    | _, _ => print("--None--")

main(): () =
    test(Some(456))
    test(None())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed--None--"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 7``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<int32>): () =
    match ((option, option), option)
    | (Some(_), Some(value)), Some(value2) when (value2 == 452) => print(value)
    | _, Some(456) => print("passed")
    | _, _ => print("--None--")

main(): () =
    test(Some(456))
    test(None())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed--None--"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 8``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<Option<int32>>): () =
    match (option)
    | Some(Some(123)) => print("failed")
    | Some(Some(456)) => print("passed")
    | _ => print("failed")

main(): () =
    test(Some(Some(456)))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 9``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<Option<int32>>): () =
    match (option)
    | Some(Some(123)) => print("failed")
    | Some(Some(456)) => print("failed")
    | _ => print("passed")

main(): () =
    test(Some(null: Option<_>))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 10``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

pattern Some<T>(value: Option<T>): (T, T) when SomeGuardTuple(value !== null) =>
    (value.Value, value.Value)

test(option: Option<Option<int32>>): () =
    match (option)
    | Some(Some(123)) => print("passed")
    | _ => print("failed")

main(): () =
    test(Some(Some(123)))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 11``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

pattern Some<T>(value: Option<T>): (T, T) when SomeGuardTuple(value !== null) =>
    (value.Value, value.Value)

test(option: Option<Option<int32>>): () =
    match (option)
    | Some(Some(123, 124)) => print("failed1")
    | Some(Some(123, 123)) => print("passed")
    | _ => print("failed2")

main(): () =
    test(Some(Some(123)))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 12``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

pattern Some2<T>(value: Option<T>): (T, T) when (value !== null) =>
    (value.Value, value.Value)

test(option: Option<Option<int32>>): () =
    match (option)
    | Some(Some2(123, 124)) => print("failed1")
    | Some(Some2(123, 123)) => print("passed")
    | _ => print("failed2")

main(): () =
    test(Some(Some(123)))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 13``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

pattern Some2<T>(value: Option<T>): (T, T) when (value !== null) =>
    (value.Value, value.Value)

test(option: Option<Option<Option<int32>>>): () =
    match (option)
    | Some(Some2(Some(123), Some(124))) => print("failed1")
    | Some(Some2(Some(123), Some(123))) => print("passed")
    | _ => print("failed2")

main(): () =
    test(Some(Some(Some(123))))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 14``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<Option<(Option<(int32, int32)>, Option<(int32, int32)>)>>): () =
    match (option)
    | Some(Some(Some(123, 123), Some(123, 124))) => print("failed1")
    | Some(Some(Some(123, 123), Some(123, 123))) => print("passed")
    | _ => print("failed2")

main(): () =
    let x = Some((123, 123))
    test(Some(Some((x, x))))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 15``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<Option<(Option<(int32, int32)>, Option<(int32, int32)>)>>): () =
    match (option)
    | Some(Some(Some(123, 123), Some(123, 124)))
    | Some(Some(Some(123, 123), Some(123, 123))) => print("passed")
    | _ => print("failed2")

main(): () =
    let x = Some((123, 123))
    test(Some(Some((x, x))))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 16``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>: Option<T> =
    null

pattern None<T>(value: Option<T>): () when (value === null) => ()

pattern Some<T>(value: Option<T>): T when (value !== null) => value.Value

pattern Some<T>(value: Option<T>): (T, T) when SomeGuardTuple(value !== null) =>
    (value.Value, value.Value)

test(option: Option<Option<Option<int32>>>): () =
    match (option)
    | Some(Some(Some(123), None))
    | Some(Some(Some(123), Some(124))) => print("failed")
    | _ => print("passed")

main(): () =
    test(Some(Some(Some(123))))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 16 but with a passing byref``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>: Option<T> =
    null

pattern None<T>(value: Option<T>): () when (value === null) => ()

pattern Some<T>(value: Option<T>): T when (value !== null) => value.Value

pattern Some<T>(value: Option<T>): (T, T) when SomeGuardTuple(value !== null) =>
    (value.Value, value.Value)

test(valueref: byref<int32>, option: Option<Option<Option<int32>>>): () =
    match (option)
    | Some(Some(Some(123), None))
    | Some(Some(Some(123), Some(124))) => 
        valueref <- 2323
        print("failed")
    | _ => 
        valueref <- 987
        print("passed")

main(): () =
    let mutable valueref = 678
    test(&valueref, Some(Some(Some(123))))
    print(valueref)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed987"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 17``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>: Option<T> =
    null

pattern None<T>(value: Option<T>): () when (value === null) => ()

pattern Some(value: Option<int32>): int32 when (value !== null) => value.Value
pattern Some(value: Option<float32>): float32 when (value !== null) => value.Value

pattern Some<T>(value: Option<T>): (T, T) when (value !== null) =>
    (value.Value, value.Value)

test(option: Option<float32>): () =
    match (option)
    | Some(_) => print("passed")
    | _ => print("failed2")

main(): () =
    test(Some(123.0f))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output 18``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>: Option<T> =
    null

pattern None<T>(value: Option<T>): () when (value === null) => ()

pattern Some<T>(value: Option<T>): T when (value !== null) => value.Value

pattern Some<T>(value: Option<T>): (T, T) when SomeGuardTuple(value !== null) =>
    (value.Value, value.Value)

test(option: Option<Option<Option<int32>>>): () =
    match (option)
    | Some(Some(Some(689), _))
    | Some(Some(Some(123), Some(124))) => print("failed")
    | _ => print("passed")

main(): () =
    test(Some(Some(Some(123))))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output with newtype``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
newtype Option<T> =
    public field Value: T

Some<T>(value: T): Option<T> =
    Option(value)

None<T>: Option<T> =
    null

pattern None<T>(value: Option<T>): () when (value === null) => ()

pattern Some<T>(value: Option<T>): T when (value !== null) => value.Value

pattern Some<T>(value: Option<T>): (T, T) when SomeGuardTuple(value !== null) =>
    (value.Value, value.Value)

test(option: Option<Option<Option<__oly_utf16>>>): () =
    match (option)
    | Some(Some(Some("hello"), None))
    | Some(Some(Some("hello"), Some(_))) => print("passed")
    | _ => print("failed")

main(): () =
    test(Some(Some(Some("hello"))))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Simple pattern match with implicit guard should give expected output wrapped in lambda``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("equal")]
(==)(int32, int32): bool

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

pattern Some<T>(value: Option<T>): T when (value !== null) =>
    value.Value

test(option: Option<int32>): () =
    let f() =
        match ((option, option), option)
        | (Some(_), Some(value)), Some(value2) when (value2 == 452) => print(value)
        | _, Some(456) => print("passed")
        | _, _ => print("--None--")
    f()

main(): () =
    test(Some(456))
    test(None())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed--None--"
    |> ignore

[<Fact>]
let ``Complicated lambda with generic should work``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

#[intrinsic("get_length")]
private getLength<T>(T[]): int32

#[open]
extension ArrayExtensions<T> =
    inherits T[]

    Length: int32
        #[inline]
        get() = getLength(this)

#[inline(always)]
ForEach<T>(xs: T[], #[inline(always)] f: T -> ()): () =
    let mutable i = 0
    while (i < xs.Length)
        f(xs[i])
        i <- i + 1

class C1 =
    Do(x: int32, y: int32, f: (int32, int32) -> ()): () =
        f(x, y)

    Run<T>(xs: T): () where T: int32[] =
        this.Do(1, 2, (a, b) -> ForEach(xs, x -> print(x)))

main(): () =
    let c = C1()
    c.Run([123]: int32[])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Complicated lambda with generic should work 2 - uses let bindings``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

#[intrinsic("get_length")]
private getLength<T>(T[]): int32

#[open]
extension ArrayExtensions<T> =
    inherits T[]

    Length: int32
        #[inline]
        get() = getLength(this)

#[inline(always)]
ForEach<T>(xs: T[], #[inline(always)] f: T -> ()): () =
    let mutable i = 0
    while (i < xs.Length)
        f(xs[i])
        i <- i + 1

class C1 =
    Do(x: int32, y: int32, f: (int32, int32) -> ()): () =
        f(x, y)

    Run<T>(xs: T): () where T: int32[] =
        let f = 
            (a: int32, b: int32) -> 
                let g = (x: int32) -> print(x)
                ForEach(xs, g)
        this.Do(1, 2, f)

main(): () =
    let c = C1()
    c.Run([123]: int32[])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

pattern addOne(x: int32): int32 = __oly_add(x, 1)
test(x: int32): () =
    match (x)
    | addOne(y) => print(y)

main(): () =
    test(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 2``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

pattern addOne(x: int32): int32 = __oly_add(x, 1)
test(x1: int32, x2: int32): () =
    match (x1, x2)
    | addOne(y), 5 => print(y)
    | _ => print(x1)

main(): () =
    test(1, 5)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 3``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

pattern addOne(x: int32): int32 = 
    print("side effect")
    __oly_add(x, 1)

test(x1: int32, x2: int32): () =
    match (x1, x2)
    | addOne(y), 5 => print(y)
    | _ => print(x1)

main(): () =
    test(99, 10)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "side effect99"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 4``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

pattern addOne(x: int32): int32 = 
    print("side effect")
    __oly_add(x, 1)

test(x1: int32, x2: int32): () =
    match (x1, x2)
    | 5, addOne(y) => print(y)
    | _ => print(x1)

main(): () =
    test(99, 10)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "99"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 5``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("side effect")
    __oly_add(x, 1)

test(x1: int32, x2: int32): () =
    match (x1, x2)
    | 5, addOne(y) => print(y)
    | 8, addOne(y) => print(y)
    | 98, addOne(y) when (y == 50) => print(y)
    | _ => print(x1)

main(): () =
    test(99, 10)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "99"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 6``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("side effect")
    __oly_add(x, 1)

test(x1: int32, x2: int32): () =
    match (x1, x2)
    | 5, addOne(y) => print(y)
    | 8, addOne(y) => print(y)
    | 99, addOne(y) when (y == 50) => print(y)
    | _ => print(x1)

main(): () =
    test(99, 10)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "side effect99"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 7``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("(side effect)")
    __oly_add(x, 1)

test(x1: int32, x2: int32): () =
    match (x1, x2)
    | 5, addOne(y) => print(y)
    | 8, addOne(y) => print(y)
    | 99, addOne(y) when (y == 49) => print(y)
    | 99, addOne(y) when (y == 50) => print(y)
    | _ => print(x1)

main(): () =
    test(99, 49)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "(side effect)(side effect)50"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 8``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("(side effect)")
    __oly_add(x, 1)

test(x1: int32, x2: int32): () =
    match (x1, x2)
    | 99, addOne(y)
    | 99, addOne(y) when (y == 50) => print(y)
    | _ => print(x1)

main(): () =
    test(99, 49)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "(side effect)50"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 9``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("(side effect)")
    __oly_add(x, 1)

test(x1: int32, x2: int32, x3: int32): () =
    match (x1, x2, x3)
    | 5, addOne(y), 2 => print(y)
    | 8, addOne(y), 3 => print(y)
    | 99, addOne(y), 4 when (y == 49) => print(y)
    | 99, addOne(y), 5 when (y == 50) => print(y)
    | _ => print(x1)

main(): () =
    test(99, 49, 5)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "(side effect)(side effect)50"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 10``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("(side effect)")
    __oly_add(x, 1)

test(x1: int32, x2: int32, x3: int32): () =
    match (x1, x2, x3)
    | 5, addOne(y), 2 => print(y)
    | 8, addOne(y), _ => print(y)
    | 99, addOne(y), 4 when (y == 49) => print(y)
    | 99, addOne(y), 5 when (y == 50) => print(y)
    | _ => print(x1)

main(): () =
    test(99, 49, 5)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "(side effect)(side effect)50"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 11``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("(side effect)")
    __oly_add(x, 1)

test<T>(x1: int32, x2: int32, x3: int32, z: T): () =
    match (x1, x2, x3)
    | 5, addOne(y), 2 => print(y)
    | 8, addOne(y), _ => print(y)
    | 99, addOne(y), 4 when (y == 49) => print(y)
    | 99, addOne(y), 5 when (y == 50) => print(y)
    | _ => print(z)

main(): () =
    test(99, 49, 8, 3.3)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "(side effect)(side effect)3.3"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 12``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("(side effect)")
    __oly_add(x, 1)

test<T>(x1: int32, x2: int32, x3: int32, mutable z: T): () where T: struct =
    match (x1, x2, x3)
    | 5, addOne(y), 2 => print(y)
    | 8, addOne(y), _ => print(y)
    | 99, addOne(y), 4 when (y == 49) => print(y)
    | 99, addOne(y), 5 when (y == 50) => print(y)
    | _ =>
        let f() =
            z <- default
        f()
        print(z)

    print("hello")
    print(z)

main(): () =
    test(99, 49, 8, 3.3)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "(side effect)(side effect)0hello0"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 13``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern addOne(x: int32): int32 = 
    print("(side effect)")
    __oly_add(x, 1)

test<T>(x1: int32, x2: int32, x3: int32, mutable z: T): () where T: struct =
    match (x1, x2, x3)
    | 5, addOne(y), 2 => print(y)
    | 8, addOne(y), _ => print(y)
    | 99, addOne(y), 4 when (y == 49) => print(y)
    | 99, addOne(y), 5 when (y == 50) => print(y)
    | _ =>
        z <- default
        print(z)

    print("hello")
    print(z)

main(): () =
    test(99, 49, 8, 3.3)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "(side effect)(side effect)0hello0"
    |> ignore

[<Fact>]
let ``Active pattern match should give expected output 14``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

pattern addOne(x: int32): int32 = 
    print("(side effect)")
    __oly_add(x, 1)

test<T>(x1: int32, x2: int32, x3: int32, z: byref<T>): () where T: struct =
    match (x1, x2, x3)
    | 5, addOne(y), 2 => print(y)
    | 8, addOne(y), _ => print(y)
    | 99, addOne(y), 4 when (y == 49) => print(y)
    | 99, addOne(y), 5 when (y == 50) => print(y)
    | _ =>
        z <- default
        print(z)

    print("hello")
    print(z)

main(): () =
    let mutable x = 3.3
    test(99, 49, 8, &x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "(side effect)(side effect)0hello0"
    |> ignore

[<Fact>]
let ``Simple lambda test via let-binding``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

test(f: int32 -> int32): int32 =
    let g = f
    g(123)
    
main(): () =
    let result = test(x -> x)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple class inheritance``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

abstract class A =

    field x: int32

    new() = this { x = 1 }

    abstract default test(): int32 = 1

class B =
    inherits A

    new(x: int32) = 
        this { }

    overrides test(): int32 = 4
    
main(): () =
    let b = B(1)
    let result = b.test()
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"
    |> ignore

[<Fact>]
let ``Simple class inheritance 2``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

abstract class A =

    field x: int32

    new() = this { x = 1 }

    abstract default test(): int32 = 1

class B =
    inherits A

    new(x: int32) = 
        this { }
    
main(): () =
    let b = B(1)
    let result = b.test()
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Simple class inheritance 3``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

abstract class A =

    new() = this { }

    abstract default test(): int32 = 1

abstract class B =
    inherits A

    new() = 
        this { }

class C =
    inherits B

    new() = 
        this { }

    test2(): int32 = base.test()
    overrides test(): int32 = 2

main(): () =
    let c = C()
    let result = c.test2()
    let result2 = c.test()
    print(result)
    print(result2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "12"
    |> ignore

[<Fact>]
let ``Simple class inheritance 4``() =
    let src = 
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

abstract class A =

    new() = this { }

    abstract default test(): int32 = 1

abstract class B =
    inherits A

    new() = 
        this { }

    overrides test(): int32 = 5

class C =
    inherits B

    new() = 
        this { }

    overrides test(): int32 = 2

main(): () =
    let c = C()
    let result = c.test()
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Interface with a property should pass``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    x: int32 get

class Test =
    implements ITest

    x: int32 get
    new() = this { x = 5 }

test(t: ITest): int32 = t.x

main(): () =
    let t = Test()
    let result = test(t)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Interface with a property should pass 2``() =
    let src =
        """
open extension TestExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    x: int32 get

class Test =

    x: int32 abstract default get

    new() = this { x = 5 }

extension TestExtension =
    inherits Test
    implements ITest

test(t: ITest): int32 = t.x

#[intrinsic("cast")]
cast<T>(__oly_object): T

main(): () =
    let t = Test()
    let result = test(cast<ITest>(t))
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Interface with a property should pass 3``() =
    let src =
        """
open extension TestExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    x: int32 get

interface ITest2 =

    x: int32 get

class Test =

    implements ITest2

    x: int32 get
    new() = this { x = 5 }

extension TestExtension =
    inherits ITest2
    implements ITest

test(t: ITest): int32 = t.x

#[intrinsic("cast")]
cast<T>(__oly_object): T

main(): () =
    let t = Test(): ITest2
    let result = test(cast<ITest>(t))
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Interface with a function should pass``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    x(): int32

abstract class Test1 =
    abstract default x(): int32 = 123

    new() = this { }

class Test =
    inherits Test1
    implements ITest

    new() = this { }

test(t: ITest): int32 = t.x()

main(): () =
    let t = Test()
    let result = test(t)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Interface with a function should pass 2``() =
    let src =
        """
open extension TestExtension

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    x(): int32

class Test =

    abstract default x(): int32 = 123
    new() = this { }

extension TestExtension =
    inherits Test
    implements ITest

test(t: ITest): int32 = t.x()

#[intrinsic("cast")]
cast<T>(__oly_object): T

main(): () =
    let t = Test()
    let result = test(cast<ITest>(t))
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Test null``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = unchecked default: __oly_object
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Implicit default constructor on class``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class Test

main(): () =
    let x = Test()
    print("Test")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test"
    |> ignore

[<Fact>]
let ``Implicit default constructor on class 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class Test =

    public field X: __oly_int32 = 123

main(): () =
    let x = Test()
    print(x.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Implicit default constructor on struct``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

struct Test

main(): () =
    let x = Test()
    print("Test")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test"
    |> ignore

[<Fact>]
let ``Implicit default constructor on struct 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field X: __oly_int32 = 123

main(): () =
    let x = Test()
    print(x.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Interface inherits more than one interface``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA
interface IB
interface IC =
    inherits IA, IB

class A =
    implements IC

test(x: IA): () = print("IA")
test2(x: IB): () = print("IB")

main(): () =
    let a = A()
    test(a)
    test2(a)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "IAIB"
    |> ignore

[<Fact>]
let ``Basic enum``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

enum Test =
    | A
    | B
    | C

main(): () =
    let x = Test.A
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "A"
    |> ignore

[<Fact>]
let ``Basic enum 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

enum Test =
    | A
    | B
    | C

main(): () =
    print(Test.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "A"
    |> ignore

[<Fact>]
let ``let! example``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

class Wrapper<T> =

    public field Value: T

    new(value: T) = this { Value = value }

(let!)<A, B>(a: Wrapper<A>, f: A -> Wrapper<B>): Wrapper<B> =
    f(a.Value)

(return)<A>(a: A): Wrapper<A> =
    Wrapper(a)

test1(x: Wrapper<int32>): Wrapper<int32> =
    let! result = x
    return result + 1

main(): () =
    let x = Wrapper(567)
    let z = test1(x)
    print(z.Value)
    ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "568"
    |> ignore

[<Fact>]
let ``let! example 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

class Wrapper<T> =

    public field Value: T

    new(value: T) = this { Value = value }

(let!)<A, B>(a: Wrapper<A>, f: A -> Wrapper<B>): Wrapper<B> =
    f(a.Value)

(return)<A>(a: A): Wrapper<A> =
    Wrapper(a)

test1(x: Wrapper<int32>): Wrapper<int32> =
    let! result = x
    let! result2 = x
    return result + result2

main(): () =
    let x = Wrapper(567)
    let z = test1(x)
    print(z.Value)
    ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1134"
    |> ignore

[<Fact>]
let ``Local generic type example``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

test<A, B>(a: A, b: B): B where A: struct; where B: struct =
    struct Test =

        public field mutable a: A = default
        public field mutable b: B = default

    let mutable s: Test = default
    s.a <- a
    s.b <- b
    s.b

main(): () =
    let x = test<__oly_int32, __oly_float32>(123, 456)
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Local generic type example 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

test<A, B>(a: A, b: B): B where A: struct =
    struct Test =

        public field mutable a: A = default
        public field mutable b: B = unchecked default

    let mutable s: Test = default
    s.a <- a
    s.b <- b
    s.b

main(): () =
    let x = test<__oly_int32, __oly_utf16>(123, "test")
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Local generic type example 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

test<A, B>(a: A, b: B): B where A: struct =
    struct Test<T> =

        public field mutable a: A = default
        public field mutable b: B = unchecked default

    let mutable s: Test<__oly_int32> = default
    s.a <- a
    s.b <- b
    s.b

main(): () =
    let x = test<__oly_int32, __oly_utf16>(123, "test")
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Local generic function has a static local generic function``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

test<A, B>(a: A, b: B): A where A: struct =
    static let test2<T>(): A = default
    test2<__oly_int32>()

main(): () =
    let x = test<__oly_int32, __oly_utf16>(123, "test")
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Indexer operator example with struct``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x = s[0]
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Indexer operator example with struct 2``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

struct Test2 =

    public field mutable s: Test<__oly_int32> = default

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test2()
    let x = s.s[0]
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Struct and interface, mutable behaviour testing``() =
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
    
    public field mutable X: __oly_int32 = 3

    mutable B(): () =
        this.X <- 5

main(): () =
    let x = Test()
    let z = x: IA
    z.A()
    print(x.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Struct and interface, mutable behaviour testing 2``() =
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
    
    public field mutable X: __oly_int32 = 3

    mutable B(): () =
        this.X <- 5

main(): () =
    let mutable x = Test()
    let z = x: IA
    z.A()
    print(x.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Struct and interface, mutable behaviour testing 3``() =
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
    
    public field mutable X: __oly_int32 = 3

    mutable B(): () =
        this.X <- 5

main(): () =
    let x = Test()
    (x: IA).A()
    print(x.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Basic explicit property with getter``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class A =

   X: __oly_int32
      get() = 3

main(): () =
    let a = A()
    print(a.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"
    |> ignore

[<Fact>]
let ``Property with default value``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class Test =

    y: int32 get = 256

main(): () =
    let t = Test()
    print(t.y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "256"
    |> ignore

[<Fact>]
let ``Static auto property on class``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class Test =

    public field Value: int32
    new(value: int32) = this { Value = value }

    static Default: Test get = Test(123)

main(): () =
    let t = Test.Default
    print(t.Value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Static auto property on class 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class Test =

    static Default: int32 get = 123

main(): () =
    print(Test.Default)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Static auto property on Module``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test =

    Default: int32 get = 123

main(): () =
    print(Test.Default)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Static field and property calls complex``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test2 =

    public field X: int32 =
        print("X")
        Test.Y

module Test =

    public field Y: int32 =
        print("Y")
        789

    public field Default: int32 
        get =
            print("_DEFAULT_")
            Test2.X

main(): () =
    print(Test2.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "XY_DEFAULT_789"
    |> ignore

[<Fact>]
let ``Static field and property calls complex 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test2 =

    public field X: int32 =
        print("_X_")
        let y = Test.Y
        print(y)
        print("_MID_")
        y

module Test =

    public field Y: int32 =
        print("_Y_")
        789

    public field Default: int32 
        get =
            print("_DEFAULT_")
            Test2.X

main(): () =
    print(Test2.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "_X__Y__DEFAULT_789_MID_789"
    |> ignore

[<Fact>]
let ``Static field and property calls complex 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test2 =

    public field X: int32 =
        print("_X_")
        let y = Test.Y
        print("_MID_")
        y

module Test =

    public field Y: int32 =
        print("_Y_")
        789

    public field Default: int32 
        get =
            print("_DEFAULT_")
            Test2.X

main(): () =
    print(Test2.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "_X__Y__DEFAULT__MID_789"
    |> ignore

[<Fact>]
let ``Static field and property calls complex 4``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test2 =

    public field X: int32 =
        print("_X_")
        let y = Test.Y
        print("_MID_")
        y

module Test =

    public field Y: int32 =
        print("_Y_")
        789

    public field Default: int32
        get =
            print("_DEFAULT_")
            let w = Test2.X
            print(w)
            print("_AAA_")
            print(w)
            w

main(): () =
    print(Test2.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "_X__Y__DEFAULT_0_AAA_0_MID_789"
    |> ignore

[<Fact>]
let ``Static field and static constructor initialization behavior``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test =

    public field A: int32 =
        print("abc")
        123

main(): () =
    print(Test.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "abc123"
    |> ignore

[<Fact>]
let ``Static field and static constructor initialization behavior 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test =

    field A: int32 =
        print("abc")
        123

test<T>(): () = print("test")

main(): () =
    test<Test>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Static field and static constructor initialization behavior 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test =

    field B: int32 =
        print("def")
        456

    public field A: int32 =
        print("abc")
        123

main(): () =
    print(Test.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "defabc123"
    |> ignore

[<Fact>]
let ``Static field and static constructor initialization behavior 4``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test =

    public field A: int32 =
        print("abc")
        123

    public field B: int32 =
        print("def")
        456

main(): () =
    print(Test.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "abcdef123"
    |> ignore

[<Fact>]
let ``Static field and static constructor initialization behavior 5``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test =

    public field A: int32 =
        print("abc")
        123

    field B: int32 =
        print("def")
        456

main(): () =
    print("before_")
    print(Test.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "before_abcdef123"
    |> ignore

[<Fact>]
let ``Static field and static constructor initialization behavior 6``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test =

    field A: int32 =
        print("abc")
        123

    Get(): int32 = 
        45

main(): () =
    print("before_")
    print(Test.Get())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "before_45"
    |> ignore

[<Fact>]
let ``Static field and static constructor initialization behavior 7``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Test =

    field A: int32 =
        print("abc")
        123

    #[inline]
    Get(): int32 = 45

main(): () =
    print("before_")
    print(Test.Get())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "before_45"
    |> ignore

[<Fact>]
let ``Get tuple element``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("get_tuple_element")]
GetTupleElement<N, T...>(__oly_tuple<T...>): T...[N] where N: constant int32

main(): () =
    let x = (5, 8.0f)
    let value: int32 = GetTupleElement<0, _>(x)
    print(value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Get tuple element 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("get_tuple_element")]
GetTupleElement<N, T...>(__oly_tuple<T...>): T...[N] where N: constant int32

main(): () =
    let x = (5, 8.0f)
    let value: float32 = GetTupleElement<1, _>(x)
    print(value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "8"
    |> ignore

[<Fact>]
let ``Get array element``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

// Immutable array
#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

// Mutable array
#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

// Mutable array
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

main(): () =
    let xs = [1;2;3;4]
    print(xs[0])
    print(xs[3])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "14"
    |> ignore

[<Fact>]
let ``New array``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module Array =

    #[intrinsic("new_array")]
    ZeroCreate<T>(size: int32): mutable T[]

    #[intrinsic("get_length")]
    GetLength<T>(mutable T[]): int32

// Immutable array
#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

// Mutable array
#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

// Mutable array
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

main(): () =
    let xs = Array.ZeroCreate<int32>(5)
    print(Array.GetLength(xs))
    print(xs[0])
    print(xs[3])

    xs[0] <- 8
    xs[3] <- 9

    print(xs[0])
    print(xs[3])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "50089"
    |> ignore

[<Fact>]
let ``Complex test``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
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

module Array =

    #[intrinsic("new_array")]
    ZeroCreate<T>(size: int32): mutable T[]

interface IMemory<T> =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

    Length: int32 get

interface IMemoryAllocator<TMemory<_>> where TMemory<_>: IMemory =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

struct DefaultMemory<T> =
    implements IMemory<T>

    private field Buffer: mutable T[]

    new(buffer: mutable T[]) =
        this {
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

private struct IndexQueue<TMemory<_>, TMemoryAllocator> 
    where TMemory<_>: IMemory; 
    where TMemoryAllocator: IMemoryAllocator<TMemory> 
    =

    public field mutable Indices: TMemory<int32>
    public field mutable Count: int32

    new(indices: TMemory<int32>, count: int32) =
        this {
            Indices = indices
            Count = count
        }

    new() =
        this {
            Indices = TMemoryAllocator.Allocate(8)
            Count = 0
        }

    mutable Enqueue(index: int32): () =
        ()

    mutable TryDequeue(index: byref<int32>): bool =
        true

    GetFirst(): int32 = this.Indices.get_Item(0)

        
test(): () =
    let x = IndexQueue<DefaultMemory, DefaultMemoryAllocator>()
    print(x.GetFirst())
        
main(): () =
    test()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Complex test 2``() =
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

#[intrinsic("native_int")]
alias nint

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("bool")]
alias bool

#[intrinsic("utf16")]
alias string

#[intrinsic("char16")]
alias char

#[intrinsic("base_object")]
alias object

#[intrinsic("native_ptr")]
alias (*)<T>

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

#[intrinsic("subtract")]
(-)(uint8, uint8): uint8
#[intrinsic("subtract")]
(-)(int8, int8): int8
#[intrinsic("subtract")]
(-)(uint16, uint16): uint16
#[intrinsic("subtract")]
(-)(int16, int16): int16
#[intrinsic("subtract")]
(-)(uint32, uint32): uint32
#[intrinsic("subtract")]
(-)(int32, int32): int32
#[intrinsic("subtract")]
(-)(uint64, uint64): uint64
#[intrinsic("subtract")]
(-)(int64, int64): int64
#[intrinsic("subtract")]
(-)(float32, float32): float32
#[intrinsic("subtract")]
(-)(float64, float64): float64

#[intrinsic("multiply")]
(*)(uint8, uint8): uint8
#[intrinsic("multiply")]
(*)(int8, int8): int8
#[intrinsic("multiply")]
(*)(uint16, uint16): uint16
#[intrinsic("multiply")]
(*)(int16, int16): int16
#[intrinsic("multiply")]
(*)(uint32, uint32): uint32
#[intrinsic("multiply")]
(*)(int32, int32): int32
#[intrinsic("multiply")]
(*)(uint64, uint64): uint64
#[intrinsic("multiply")]
(*)(int64, int64): int64
#[intrinsic("multiply")]
(*)(float32, float32): float32
#[intrinsic("multiply")]
(*)(float64, float64): float64

#[intrinsic("divide")]
(/)(uint8, uint8): uint8
#[intrinsic("divide")]
(/)(int8, int8): int8
#[intrinsic("divide")]
(/)(uint16, uint16): uint16
#[intrinsic("divide")]
(/)(int16, int16): int16
#[intrinsic("divide")]
(/)(uint32, uint32): uint32
#[intrinsic("divide")]
(/)(int32, int32): int32
#[intrinsic("divide")]
(/)(uint64, uint64): uint64
#[intrinsic("divide")]
(/)(int64, int64): int64
#[intrinsic("divide")]
(/)(float32, float32): float32
#[intrinsic("divide")]
(/)(float64, float64): float64

#[intrinsic("remainder")]
(%)(uint8, uint8): uint8
#[intrinsic("remainder")]
(%)(int8, int8): int8
#[intrinsic("remainder")]
(%)(uint16, uint16): uint16
#[intrinsic("remainder")]
(%)(int16, int16): int16
#[intrinsic("remainder")]
(%)(uint32, uint32): uint32
#[intrinsic("remainder")]
(%)(int32, int32): int32
#[intrinsic("remainder")]
(%)(uint64, uint64): uint64
#[intrinsic("remainder")]
(%)(int64, int64): int64
#[intrinsic("remainder")]
(%)(float32, float32): float32
#[intrinsic("remainder")]
(%)(float64, float64): float64

#[intrinsic("less_than")]
(<)(uint8, uint8): bool
#[intrinsic("less_than")]
(<)(int8, int8): bool
#[intrinsic("less_than")]
(<)(uint16, uint16): bool
#[intrinsic("less_than")]
(<)(int16, int16): bool
#[intrinsic("less_than")]
(<)(uint32, uint32): bool
#[intrinsic("less_than")]
(<)(int32, int32): bool
#[intrinsic("less_than")]
(<)(uint64, uint64): bool
#[intrinsic("less_than")]
(<)(int64, int64): bool
#[intrinsic("less_than")]
(<)(float32, float32): bool
#[intrinsic("less_than")]
(<)(float64, float64): bool

#[intrinsic("less_than_or_equal")]
(<=)(uint8, uint8): bool
#[intrinsic("less_than_or_equal")]
(<=)(int8, int8): bool
#[intrinsic("less_than_or_equal")]
(<=)(uint16, uint16): bool
#[intrinsic("less_than_or_equal")]
(<=)(int16, int16): bool
#[intrinsic("less_than_or_equal")]
(<=)(uint32, uint32): bool
#[intrinsic("less_than_or_equal")]
(<=)(int32, int32): bool
#[intrinsic("less_than_or_equal")]
(<=)(uint64, uint64): bool
#[intrinsic("less_than_or_equal")]
(<=)(int64, int64): bool
#[intrinsic("less_than_or_equal")]
(<=)(float32, float32): bool
#[intrinsic("less_than_or_equal")]
(<=)(float64, float64): bool

#[intrinsic("greater_than")]
(>)(uint8, uint8): bool
#[intrinsic("greater_than")]
(>)(int8, int8): bool
#[intrinsic("greater_than")]
(>)(uint16, uint16): bool
#[intrinsic("greater_than")]
(>)(int16, int16): bool
#[intrinsic("greater_than")]
(>)(uint32, uint32): bool
#[intrinsic("greater_than")]
(>)(int32, int32): bool
#[intrinsic("greater_than")]
(>)(uint64, uint64): bool
#[intrinsic("greater_than")]
(>)(int64, int64): bool
#[intrinsic("greater_than")]
(>)(float32, float32): bool
#[intrinsic("greater_than")]
(>)(float64, float64): bool

#[intrinsic("greater_than_or_equal")]
(>=)(uint8, uint8): bool
#[intrinsic("greater_than_or_equal")]
(>=)(int8, int8): bool
#[intrinsic("greater_than_or_equal")]
(>=)(uint16, uint16): bool
#[intrinsic("greater_than_or_equal")]
(>=)(int16, int16): bool
#[intrinsic("greater_than_or_equal")]
(>=)(uint32, uint32): bool
#[intrinsic("greater_than_or_equal")]
(>=)(int32, int32): bool
#[intrinsic("greater_than_or_equal")]
(>=)(uint64, uint64): bool
#[intrinsic("greater_than_or_equal")]
(>=)(int64, int64): bool
#[intrinsic("greater_than_or_equal")]
(>=)(float32, float32): bool
#[intrinsic("greater_than_or_equal")]
(>=)(float64, float64): bool

#[intrinsic("equal")]
(==)(uint8, uint8): bool
#[intrinsic("equal")]
(==)(int8, int8): bool
#[intrinsic("equal")]
(==)(uint16, uint16): bool
#[intrinsic("equal")]
(==)(int16, int16): bool
#[intrinsic("equal")]
(==)(uint32, uint32): bool
#[intrinsic("equal")]
(==)(int32, int32): bool
#[intrinsic("equal")]
(==)(uint64, uint64): bool
#[intrinsic("equal")]
(==)(int64, int64): bool
#[intrinsic("equal")]
(==)(float32, float32): bool
#[intrinsic("equal")]
(==)(float64, float64): bool
#[intrinsic("equal")]
(==)(bool, bool): bool

#[intrinsic("not_equal")]
(!=)(uint8, uint8): bool
#[intrinsic("not_equal")]
(!=)(int8, int8): bool
#[intrinsic("not_equal")]
(!=)(uint16, uint16): bool
#[intrinsic("not_equal")]
(!=)(int16, int16): bool
#[intrinsic("not_equal")]
(!=)(uint32, uint32): bool
#[intrinsic("not_equal")]
(!=)(int32, int32): bool
#[intrinsic("not_equal")]
(!=)(uint64, uint64): bool
#[intrinsic("not_equal")]
(!=)(int64, int64): bool
#[intrinsic("not_equal")]
(!=)(float32, float32): bool
#[intrinsic("not_equal")]
(!=)(float64, float64): bool
#[intrinsic("not_equal")]
(!=)(bool, bool): bool

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

#[intrinsic("or")]
(||)(bool, bool): bool

#[intrinsic("and")]
(&&)(bool, bool): bool

#[intrinsic("not")]
(!)(bool): bool

#[intrinsic("print")]
print(object): ()

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

#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)
(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Addition(T1, T2): T3 } = T1.op_Addition(x, y)
(-)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Subtraction(T1, T2): T3 } = T1.op_Subtraction(x, y)
(*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)
(/)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Division(T1, T2): T3 } = T1.op_Division(x, y)
(%)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Remainder(T1, T2): T3 } = T1.op_Remainder(x, y)
(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)
(!=)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Inequality(T1, T2): T3 } = T1.op_Inequality(x, y)
(-)<T1, T2>(x: T1): T2 where T1: { static op_UnaryNegation(T1): T2 } = T1.op_UnaryNegation(x)

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

#[intrinsic("cast")]
unsafeCast<T>(object): T

#[intrinsic("bitwise_and")]
(&)(int32, int32): int32

#[intrinsic("bitwise_or")]
(|)(int32, int32): int32

#[intrinsic("bitwise_not")]
(~)(int32): int32

#[intrinsic("bitwise_shift_left")]
(<<)(int32, int32): int32

#[intrinsic("get_length")]
getLength<T>(T[]): int32

#[intrinsic("get_length")]
getLength<T>(mutable T[]): int32

module Array =

    #[intrinsic("new_array")]
    ZeroCreate<T>(size: int32): mutable T[]

interface IMemory<T> =

    get_Item(index: int32): T
    set_Item(index: int32, item: T): ()

    Length: int32 get

interface IMemoryAllocator<TMemory<_>> where TMemory<_>: IMemory =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

struct DefaultMemory<T> =
    implements IMemory<T>

    private field Buffer: mutable T[]

    new(buffer: mutable T[]) =
        this {
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

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

interface IComponent

struct IndexQueue<TMemory<_>, TMemoryAllocator> 
    where TMemory<_>: IMemory; 
    where TMemoryAllocator: IMemoryAllocator<TMemory>
    =  

        public field mutable Indices: TMemory<int32>
        public field mutable Count: int32
        public field mutable Offset: int32

        new() =
            this {
                Indices = TMemoryAllocator.Allocate(8)
                Count = 0
                Offset = 0
            }

        mutable Enqueue(index: int32): () =
            if (this.Count >= this.Indices.Length)
                let prevIndices = this.Indices
                this.Indices <- TMemoryAllocator.Allocate(this.Indices.Length * 2)
                // TODO: Copy

            let i =
                if (this.Offset + this.Count >= this.Indices.Length)
                    if (this.Offset > this.Count)
                        this.Offset - this.Count
                    else
                        this.Count - this.Offset
                else
                    this.Offset + this.Count

            this.Indices[i] <- index
            this.Count <- this.Count + 1

        mutable TryDequeue(index: byref<int32>): bool =
            if (this.Count == 0)
                false
            else
                index <- this.Indices[this.Offset]
                this.Count <- this.Count - 1
                this.Offset <- this.Offset + 1
                if (this.Offset >= this.Indices.Length)
                    this.Offset <- 0
                true

test(): () =
    let mutable x = IndexQueue<DefaultMemory, DefaultMemoryAllocator>()

    x.Enqueue(5)

    let mutable result = 0
    if (x.TryDequeue(&result))
        print(result)
        
main(): () =
    test()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Complex ByRef``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: __oly_int32): T

struct Test<T> where T: struct =
    
    public field mutable Data1: mutable T[] = mutable [default;default]

    mutable A(f: T -> ()): () =
        let data1 = this.Data1
        let z = &data1[0]
        let w = &data1[0]
        f(w)

main(): () =
    let mutable t = Test<__oly_int32>()
    t.A(x -> print(x))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Complex ByRef 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: __oly_int32): T

struct Test<T> where T: struct =
    
    public field mutable Data1: mutable T[] = mutable [default;default]

    mutable A(f: byref<T> -> ()): () =
        f(&this.Data1[0])

main(): () =
    let mutable t = Test<__oly_int32>()
    t.A(x -> print(x))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Call function indirectly from field``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class Test =

    public field F: int32 -> ()

    new(f: int32 -> ()) =
        this {
            F = f
        }

main(): () =
    let t = Test(i -> print(i))
    t.F(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Call function indirectly from field 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field F: int32 -> ()

    new(f: int32 -> ()) =
        this {
            F = f
        }

main(): () =
    let t = Test(i -> print(i))
    t.F(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Custom inlined for-loop API``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("less_than")]
(<)(int32, int32): __oly_bool

#[inline]
forLoop(length: int32, #[inline] f: int32 -> ()): () =
    #[inline]
    let loop(i) =
        if (i < length)
            f(i)
            loop(i + 1)
    loop(0)

main(): () =
    forLoop(5, i -> print(i))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "01234"
    |> ignore

[<Fact>]
let ``Component Bit Mask test 1``() =
    let src =
        """
#[intrinsic("uint64")]
alias uint64

#[intrinsic("bool")]
alias bool

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

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

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)

#[inline]
(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)

#[intrinsic("divide")]
(/)(int32, int32): int32

#[intrinsic("remainder")]
(%)(int32, int32): int32

#[intrinsic("bitwise_shift_left")]
(<<)(uint64, int32): uint64

#[intrinsic("bitwise_not")]
(~)(uint64): uint64

#[intrinsic("bitwise_and")]
(&)(uint64, uint64): uint64

#[intrinsic("bitwise_or")]
(|)(uint64, uint64): uint64

#[intrinsic("not_equal")]
(!=)(uint64, uint64): bool

struct ComponentBitMask =
    field mutable Page0: uint64 = 0
    field mutable Page1: uint64 = 0
    field mutable Page2: uint64 = 0
    field mutable Page3: uint64 = 0
    field mutable Page4: uint64 = 0
    field mutable Page5: uint64 = 0
    field mutable Page6: uint64 = 0
    field mutable Page7: uint64 = 0

    #[inline]
    mutable get_Item(index: int32): byref<uint64> =
        match (index)
        | 0 => &this.Page0
        | 1 => &this.Page1
        | 2 => &this.Page2
        | 3 => &this.Page3
        | 4 => &this.Page4
        | 5 => &this.Page5
        | 6 => &this.Page6
        | _ => &this.Page7

    #[inline]
    get_Item(index: int32): uint64 =
        match (index)
        | 0 => this.Page0
        | 1 => this.Page1
        | 2 => this.Page2
        | 3 => this.Page3
        | 4 => this.Page4
        | 5 => this.Page5
        | 6 => this.Page6
        | _ => this.Page7

    #[inline]
    mutable Set(index: int32, value: bool): () =
        let fieldIndex = index / 64
        let bitIndex = index % 64
        let item = &this.get_Item(fieldIndex)
        if (value)
            item <- item | (1u64 << bitIndex)
        else
            item <- item & ~(1u64 << bitIndex)

    #[inline]
    IsSet(index: int32): bool =
        let fieldIndex = index / 64
        let bitIndex = index % 64
        (this.get_Item(fieldIndex) & (1u64 << bitIndex)) != 0

main(): () =
    let mutable bits = ComponentBitMask()
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "FalseTrueFalseFalseTrue"
    |> ignore

[<Fact>]
let ``Inline test with a byref field``() =
    let src =
        """
#[intrinsic("uint64")]
alias uint64

#[intrinsic("bool")]
alias bool

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("bitwise_and")]
(&)(uint64, uint64): uint64

struct TestStruct =
    field mutable A: uint64 = 0

    #[inline]
    mutable Set(index: int32, value: bool): () =
        let item = &this.A
        if (value)
            item <- item & 2
        else
            item <- item & 2

main(): () =
    let mutable bits = TestStruct()
    bits.Set(0, true)
    bits.Set(25, true)
    print("test")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Inline lambda 1``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
test(#[inline] f: int32 -> ()): () =
    f(345)

main(): () =
    test(x -> print(x))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "345"
    |> ignore

[<Fact>]
let ``Static field converts to a constant``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

field X: int32 = 123

main(): () =
    print(X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Setting this because it is a byref``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct =
    public field mutable A: int32

    new(a: int32) = this { A = a }

    mutable SetA(): () =
        this <- TestStruct(5)  

main(): () =
    let mutable s = TestStruct(0)
    s.SetA()
    print(s.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Using byref from a function``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct =
    public field mutable A: int32 = 0

    mutable GetByRefOfA(): byref<int32> = &this.A

    mutable SetA(): () =
        this.GetByRefOfA() <- 1     

main(): () =
    let mutable s = TestStruct()
    s.SetA()
    print(s.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Using byref from a function 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct =
    public field mutable A: int32 = 0

    mutable GetByRefOfA(): byref<int32> = &this.A

    mutable SetA(): () =
        this.GetByRefOfA() <- 1    
        
struct TestStruct2 =
    public field mutable S: TestStruct = default

    mutable SetA(): () =
        this.S.SetA()

main(): () =
    let mutable s = TestStruct2()
    s.SetA()
    print(s.S.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Using byref from a function 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct =
    public field mutable A: int32 = 0

    mutable SetA(): () =
        this.A <- 5    
        
struct TestStruct2 =
    public field mutable S: TestStruct = default

    mutable SetA(): () =
        this.S.SetA()

main(): () =
    let mutable s = TestStruct2()
    s.SetA()
    print(s.S.A)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
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

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): byref<TValue> where T: { get_Item(TKey): byref<TValue> } = &x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): inref<TValue> where T: { get_Item(TKey): inref<TValue> } = &x.get_Item(key)

interface ITest<T> =

    get_Item(index: int32): T
    get_Item(index: int32): byref<T>

struct TestStruct =
    implements ITest<int32>

    field mutable Value: int32 = 123

    get_Item(index: int32): int32 =
        print("normal")
        this.Value

    mutable get_Item(index: int32): byref<int32> =
        print("byref")
        &this.Value

    get_Item(index: int32): inref<int32> =
        print("inref")
        &this.Value

test(x: inref<int32>): () = ()

main(): () =
    let mutable t = TestStruct()
    let x: int32 = t[0]
    let y: byref<int32> = &t[0]
    let z: inref<int32> = &t[0]
    let w = &t[0]

    test(&t[0])

    print(x)
    print(y)
    print(z)
    print(w)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "normalbyrefbyrefbyrefbyref123123123123"
    |> ignore

[<Fact>]
let ``Byref indexer should pass 2``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): byref<TValue> where T: { get_Item(TKey): byref<TValue> } = &x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): inref<TValue> where T: { get_Item(TKey): inref<TValue> } = &x.get_Item(key)

interface ITest<T> =

    get_Item(index: int32): T
    get_Item(index: int32): byref<T>

struct TestStruct =
    implements ITest<int32>

    field mutable Value: int32 = 123

    get_Item(index: int32): int32 =
        print("normal")
        this.Value

    mutable get_Item(index: int32): byref<int32> =
        print("byref")
        &this.Value

    get_Item(index: int32): inref<int32> =
        print("inref")
        &this.Value

main(): () =
    let mutable t = TestStruct()
    let x: int32 = t[0]
    let y: byref<int32> = &t[0]
    let z: inref<int32> = &t[0]
    let w = &t[0]

    print(x)
    print(y)
    print(z)
    print(w)

    let t = TestStruct()
    let x: int32 = t[0]
    let z: inref<int32> = &t[0]
    let w = &t[0]

    print(x)
    print(z)
    print(w)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "normalbyrefbyrefbyref123123123123normalinrefinref123123123"
    |> ignore

[<Fact>]
let ``Byref indexer should pass 3``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: T, key: TKey): byref<TValue> where T: { get_Item(TKey): byref<TValue> } = &x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: T, key: TKey): inref<TValue> where T: { get_Item(TKey): inref<TValue> } = &x.get_Item(key)

interface ITest<T> =

    get_Item(index: int32): T
    get_Item(index: int32): byref<T>
    get_Item(index: int32): inref<T>

class TestClass =
    implements ITest<int32>

    field mutable Value: int32 = 123

    get_Item(index: int32): int32 =
        print("normal")
        this.Value

    get_Item(index: int32): byref<int32> =
        print("byref")
        &this.Value

    get_Item(index: int32): inref<int32> =
        print("inref")
        &this.Value

main(): () =
    let t = TestClass()
    let x: int32 = t[0]
    let y: byref<int32> = &t[0]
    let z: inref<int32> = &t[0]

    print(x)
    print(y)
    print(z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "normalbyrefinref123123123"
    |> ignore

[<Fact>]
let ``Byref indexer should pass 4``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

(`[]`)<T, TKey, TValue>(x: T, key: TKey): byref<TValue> where T: { get_Item(TKey): byref<TValue> } = &x.get_Item(key)

interface ITest<T> =

    get_Item(index: int32): T
    get_Item(index: int32): byref<T>
    get_Item(index: int32): inref<T>

class TestClass =
    implements ITest<int32>

    field mutable Value: int32 = 123

    get_Item(index: int32): int32 =
        print("normal")
        this.Value

    get_Item(index: int32): byref<int32> =
        print("byref")
        &this.Value

    get_Item(index: int32): inref<int32> =
        print("inref")
        &this.Value

main(): () =
    let t = TestClass()
    let x: int32 = t[0]
    let y: byref<int32> = &t[0]

    print(x)
    print(y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "byrefbyref123123"
    |> ignore

[<Fact>]
let ``Indexer operator that returns a byref should pass on set``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

    mutable get_Item(index: __oly_int32): byref<__oly_int32> = &this.X

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = x.get_Item(key)
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)

main(): () =
    let mutable s = Test()
    print(s.X)
    (s[0]) <- 5
    print(s.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "15"
    |> ignore

[<Fact>]
let ``Indexer operator that returns a byref should pass on set 2``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

    mutable get_Item(index: __oly_int32): byref<__oly_int32> = &this.X

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = x.get_Item(key)
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)

#[open]
extension TestSetItemExtension =
    inherits Test

    mutable set_Item(index: __oly_int32, value: __oly_int32): () =
        (this[index]) <- value

main(): () =
    let mutable s = Test()
    print(s.X)
    s[0] <- 5
    print(s.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "15"
    |> ignore

[<Fact>]
let ``Indexer operator that returns a byref should pass on set 3``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

    mutable get_Item(index: __oly_int32): byref<__oly_int32> = &this.X

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = x.get_Item(key)
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = x.get_Item(key)
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)

#[open]
extension TestSetItemExtension =
    inherits Test

    mutable set_Item(index: __oly_int32, value: __oly_int32): () =
        (this[index]) <- value

main(): () =
    let mutable s = Test()
    print(s.X)
    s[0] <- 5
    print(s.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "15"
    |> ignore

[<Fact>]
let ``Use of field as byref should succeed``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

#[inline(never)]
test(x: byref<__oly_int32>): () = x <- 99

main(): () =
    let mutable s = Test()
    let x = &s.X
    let y = &x
    print(x)
    test(&x)
    print(y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "199"
    |> ignore

[<Fact>]
let ``Use of field as byref should succeed 2``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

#[inline(never)]
test(x: byref<__oly_int32>): () = x <- 99

main(): () =
    let mutable s = Test()
    let x = &s.X
    let y = &x
    print(x)
    test(&y)
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "199"
    |> ignore

[<Fact>]
let ``Use of field as byref should succeed 3``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

#[inline(never)]
test(x: byref<__oly_int32>): () = x <- 99

main(): () =
    let mutable s = Test()
    print(s.X)
    test(&s.X)
    print(s.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "199"
    |> ignore

[<Fact>]
let ``Use of field as byref should succeed 4``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

#[inline(never)]
test(x: byref<__oly_int32>): () = x <- 99

main(): () =
    let mutable s = Test()
    let x1r = &s.X
    let x1 = s.X
    print(x1)
    test(&s.X)
    let x2 = x1r
    print(x2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "199"
    |> ignore

[<Fact>]
let ``Use of field as byref should succeed 5``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

#[inline(never)]
test(x: byref<__oly_int32>): () = x <- 99

main(): () =
    let mutable s = Test()
    let x1 = s.X
    let x1r = &s.X
    print(x1)
    let zz = s.X
    test(&s.X)
    let x2 = x1r
    print(zz)
    print(x2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1199"
    |> ignore

[<Fact>]
let ``Use of field as byref should succeed 6``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 1

#[inline(never)]
test(x: byref<__oly_int32>): () = x <- 99

main(): () =
    let mutable s = Test()
    let x1 = s.X
    let x1r = &s.X
    print(x1)
    let zz = s.X
    test(&x1r)
    let x2 = x1r
    print(zz)
    print(x2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1199"
    |> ignore

[<Fact>]
let ``Use of field as byref should succeed 7``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

struct Test =

    public field mutable X: __oly_int32 = 123

#[inline]
test(x: byref<Test>): byref<__oly_int32> =
    &x.X

main(): () =
    let mutable s = Test()
    let y = &test(&s)
    print(y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Simple variadic generic type``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class Test<T...>

main(): () =
    let x = Test<__oly_int32>()
    let y = Test<__oly_int32, __oly_float32>()
    print("test")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Simple variadic generic type 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("get_tuple_element")]
GetTupleElement<N, T...>(__oly_tuple<T...>): T...[N] where N: constant int32

main(): () =
    print(GetTupleElement<1, _>(("hello", 5)))
    print(GetTupleElement<0, _>(("world", 6)))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5world"
    |> ignore

[<Fact>]
let ``Simple constant constraint should run``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

test<N>(): () where N: constant int32 =
    print("it ran")

main(): () =
    test<1>()
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "it ran"
    |> ignore

[<Fact>]
let ``Simple constant constraint should run 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

test<N>(): () where N: constant int32 =
    print(N)

main(): () =
    test<123>()
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Can we break SSA?``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    if (__oly_equal(x, 5))
        y <- 10
    else
        y <- 3
    print(y)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "10"
    |> ignore

[<Fact>]
let ``Can we break SSA? 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        if (__oly_equal(x, 5))
            y <- 10
            44
        else
            y <- 3
            55
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1044"
    |> ignore

[<Fact>]
let ``Can we break SSA? 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        if (__oly_equal(x, 5))
            y <- 10
        else
            y <- 3
        55
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1055"
    |> ignore

[<Fact>]
let ``Can we break SSA? 4``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        let w =
            if (__oly_equal(x, 5))
                y <- 10
                44
            else
                y <- 3
                55
        w
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1044"
    |> ignore

[<Fact>]
let ``Can we break SSA? 5``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        let w =
            if (y <- 15
                __oly_equal(x, 5))
                y <- 10
                44
            else
                y <- 3
                55
        w
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1044"
    |> ignore

[<Fact>]
let ``Can we break SSA? 6``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        let w =
            if (let q = if (__oly_equal(x, 5)) 22 else 33
                print(q)
                __oly_equal(x, 5))
                y <- 10
                44
            else
                y <- 3
                55
        w
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "221044"
    |> ignore

[<Fact>]
let ``Can we break SSA? 7``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test2(x: int32, y: int32): int32 =
    if (__oly_equal(y, 5))
        x
    else
        __oly_add(x, 1) 

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        test2(y, 
            if (__oly_equal(y, 5))
                y <- 77
                x
            else
                y <- 88
                x
        )
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "775"
    |> ignore

[<Fact>]
let ``Can we break SSA? 8``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        if (__oly_equal(y, 5))
            y <- 22
            x
        else
            x
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "225"
    |> ignore

[<Fact>]
let ``Can we break SSA? 9``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let result = __oly_equal(y, 5)
    let z =
        if (result)
            y <- 22
            x
        else
            x
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "225"
    |> ignore

[<Fact>]
let ``Can we break SSA? 10``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        let result = __oly_equal(y, 5)
        if (result)
            y <- 22
            x
        else
            x
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "225"
    |> ignore

[<Fact>]
let ``Can we break SSA? 11``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        let result = __oly_equal(y, 5)
        if (result)
            y <- 22
            if (result)
                y <- 33
                x
            else
                x
        else
            x
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "335"
    |> ignore

[<Fact>]
let ``Can we break SSA? 12``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(mutable x: int32): () =
    let mutable f = () -> ()
    if (__oly_equal(x, 5))
        f <- (() -> x <- 123)
    else
        f <- (() -> x <- 456)
    f()
    print(x)

main(): () =
    test(5)
    test(2)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123456"
    |> ignore

[<Fact>]
let ``Can we break SSA? 13``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("float32")]
alias float32

#[intrinsic("greater_than")]
(>)(float32, float32): bool

#[intrinsic("less_than")]
(<)(float32, float32): bool

#[intrinsic("add")]
(+)(float32, float32): float32

#[intrinsic("add")]
(-)(float32, float32): float32

#[inline(never)]
Clamp(value: float32, min: float32, max: float32): float32 =
    if (value < min)
        min
    else if (value > max)
        max
    else
        value

#[inline(never)]
ClampAngle(mutable lfAngle: float32, lfMin: float32, lfMax: float32): float32 =
    if(lfAngle < -360) lfAngle <- lfAngle + 360
    if(lfAngle > 360) lfAngle <- lfAngle - 360
    Clamp(lfAngle, lfMin, lfMax)

main(): () =
    let lfAngle = 4.5f32
    print(ClampAngle(lfAngle, -80, 80))
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4.5"
    |> ignore

[<Fact>]
let ``Can we break SSA? 14``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test2(x: int32, y: int32): int32 =
    if (__oly_equal(y, 5))
        x
    else
        __oly_add(x, 1) 

#[inline(never)]
id(x: int32): int32 = x

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        test2(y, 
            let w = id(y)
            if (__oly_equal(w, 5))
                y <- 77
                x
            else
                y <- 88
                x
        )
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "775"
    |> ignore

[<Fact>]
let ``Can we break SSA? 15``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test2(x: int32, y: int32): int32 =
    if (__oly_equal(y, 5))
        x
    else
        __oly_add(x, 1) 

#[inline(never)]
id(x: int32): int32 = x

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    let z =
        test2(y, 
            let w = id(y)
            if (__oly_equal(w, 5))
                y <- 77
            else
                y <- 88
            x
        )
    print(y)
    print(z)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "775"
    |> ignore

[<Fact>]
let ``Can we break SSA? 16``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("get_length")]
getLength<T>(T[]): int32

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("and")]
(&&)(bool, bool): bool

#[open]
extension ArrayExtensions<T> =
    inherits T[]

    Length: int32 
        #[inline]
        get() = getLength(this)

#[inline(never)]
test(o1: int32[], o2: int32[]): bool =
    if (o1.Length == o2.Length)
        let mutable result = true
        let mutable i = 0
        while (i < o1.Length && result)
            let ds1 = o1[i]
            let ds2 = o2[i]
            result <- ds1 == ds2 
            i <- i + 1
        result
    else
        false

main(): () =
    print(test([1],[1]))
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"
    |> ignore

[<Fact>]
let ``Can we break SSA? 17``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("add")]
(+)(int32, int32): int32

#[inline(never)]
id(o: int32): int32 = o

#[inline(never)]
test(o: int32): int32 =
    let result =
        let mutable z = o
        let mutable i = 0
        while (i < 5)
            z <- z + 1
            i <- i + 1
        z

    id(result) + 2

main(): () =
    print(test(0))
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Can we break SSA? 18``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("uint64")]
alias uint64

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(uint64, uint64): bool

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("not_equal")]
(!=)(uint64, uint64): bool

#[intrinsic("bitwise_shift_left")]
(<<)(uint64, int32): uint64

#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("multiply")]
(*)(int32, int32): int32

#[intrinsic("bitwise_and")]
(&)(uint64, uint64): uint64

#[intrinsic("less_than")]
(<)(int32, int32): bool

struct BitSet512 =
    field mutable value0: uint64
    field mutable value1: uint64 // 128
    field mutable value2: uint64
    field mutable value3: uint64 // 256
    field mutable value4: uint64
    field mutable value5: uint64
    field mutable value6: uint64
    field mutable value7: uint64 // 512
    new(value: uint64) =
        this {
            value0 = value
            value1 = 0
            value2 = 0
            value3 = 0
            value4 = 0
            value5 = 0
            value6 = 0
            value7 = 0
        }

    new(value0: uint64, value1: uint64, value2: uint64, value3: uint64, value4: uint64, value5: uint64, value6: uint64, value7: uint64) =
        this {
            value0 = value0
            value1 = value1
            value2 = value2
            value3 = value3
            value4 = value4
            value5 = value5
            value6 = value6
            value7 = value7
        }

    ToIndex(): int32 =
        let valueIndex = this.ToValueIndex()
        let value = this.GetValue(valueIndex)
        let mutable result = -1
        let mutable i = 0
        while (i < 64)
            if (((1: uint64) << i) == value)
                result <- valueIndex * 8 + i
                i <- 64
            i <- i + 1
        result

    ToValueIndex(): int32 = 
        if (this.value0 & 0xFFFFFFFFFFFFFFFF != 0)
            0
        else if (this.value1 & 0xFFFFFFFFFFFFFFFF != 0)
            1
        else if (this.value2 & 0xFFFFFFFFFFFFFFFF != 0)
            2
        else if (this.value3 & 0xFFFFFFFFFFFFFFFF != 0)
            3
        else if (this.value4 & 0xFFFFFFFFFFFFFFFF != 0)
            4
        else if (this.value5 & 0xFFFFFFFFFFFFFFFF != 0)
            5
        else if (this.value6 & 0xFFFFFFFFFFFFFFFF != 0)
            6
        else if (this.value7 & 0xFFFFFFFFFFFFFFFF != 0)
            7
        else
            0

    GetValue(valueIndex: int32): uint64 =
        match (valueIndex)
        | 0 => this.value0
        | 1 => this.value1
        | 2 => this.value2
        | 3 => this.value3
        | 4 => this.value4
        | 5 => this.value5
        | 6 => this.value6
        | 7 => this.value7
        | _ => 999

main(): () =
    let b = BitSet512(10)
    print(b.ToIndex())
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "-1"
    |> ignore

[<Fact>]
let ``Can we break SSA? 19``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[inline(never)]
test(x: int32): () =
    let mutable y = x
    if (__oly_equal(x, 5))
        y <- 10
        print("hello")
    else
        y <- 3
        print("world")
    print(y)

main(): () =
    test(5)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello10"
    |> ignore

[<Fact>]
let ``Extended type should have access to the extension functions of an implemented interface``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

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
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1234"
    |> ignore

[<Fact>]
let ``Basic newtype``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

newtype NewtypeTest =
    public field Value: int32

main(): () =
    let t = NewtypeTest(123)
    print(t.Value)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Basic newtype 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("utf16")]
alias string

newtype NewtypeTest =
    public field Value: string

main(): () =
    let t = NewtypeTest("hello")
    print(t.Value)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"
    |> ignore

[<Fact>]
let ``Basic newtype 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

newtype NewtypeTest =
    field value: int32

    GetValue(): int32 = this.value

main(): () =
    let t = NewtypeTest(123)
    print(t.GetValue())
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Basic newtype 4``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

newtype NewtypeTest =
    field value: int32

    GetValue(): int32 = this.value

#[inline(never)]
getValue(t: NewtypeTest): int32 = t.GetValue()

main(): () =
    let t = NewtypeTest(123)
    print(getValue(t))
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Basic newtype 5``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("utf16")]
alias string

newtype NewtypeTest =
    field value: string

    GetValue(): string = this.value

#[inline(never)]
getValue(t: NewtypeTest): string = t.GetValue()

main(): () =
    let t = NewtypeTest("hello")
    print(getValue(t))
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"
    |> ignore

[<Fact>]
let ``Basic newtype 6``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("utf16")]
alias string

newtype NewtypeTest =
    field value: string

    #[inline]
    GetValue(): string = this.value

main(): () =
    let t = NewtypeTest("hello")
    print(t.GetValue())
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"
    |> ignore

[<Fact>]
let ``Extended members should be picked up by witnesses if not part of concrete type``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class A =

    M(): () = print("M")

#[open]
extension AExtensions =
    inherits A

    M(): () = print("extM")

    M2(): () = print("extM2")

printM<T>(x: T): () where T: { M(): (); M2(): () } =
    x.M()
    x.M2()

main(): () =
    let a = A()
    a.M()
    a.M2()
    printM(a)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "MextM2MextM2"
    |> ignore

[<Fact>]
let ``Extended members should be picked up by witnesses if not part of concrete type 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class A =

    M(): () = print("M")

#[open]
extension AExtensions =
    inherits A

    M(): () = print("extM")

    M2(): () = print("extM2")

printM<T>(x: T): () where T: { M(): () }, { M2(): () } =
    x.M()
    x.M2()

main(): () =
    let a = A()
    a.M()
    a.M2()
    printM(a)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "MextM2MextM2"
    |> ignore

[<Fact>]
let ``Extended members should be picked up by witnesses if not part of concrete type 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class A<T> =

    M(): () = print("M")

#[open]
extension AExtensions<T> =
    inherits A<T>

    M(): () = print("extM")

    M2(): () = print("extM2")

printM<T>(x: T): () where T: { M(): (); M2(): () } =
    x.M()
    x.M2()

main(): () =
    let a = A<__oly_object>()
    a.M()
    a.M2()
    printM(a)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "MextM2MextM2"
    |> ignore

[<Fact>]
let ``Extended members should be picked up by witnesses if not part of concrete type 4``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class A<T> =

    M<U>(): U = unchecked default

#[open]
extension AExtensions<T> =
    inherits A<T>

    M<B>(): B = unchecked default

    M2<B>(): B = unchecked default

printM<T>(x: T): () where T: { M<C>(): C; M2<D>(): D } =
    let a1 = x.M<__oly_int32>()
    let a2 = x.M2<__oly_int32>()
    print(a1)
    print(a2)

main(): () =
    let a = A<__oly_object>()
    printM(a)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "00"
    |> ignore

[<Fact>]
let ``Extended members should be picked up by witnesses if not part of concrete type 5``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class A<T> =

    M<U>(): U = unchecked default

#[open]
extension AExtensions<T> =
    inherits A<T>

    M<B>(): B = unchecked default

    M2<B>(): B = unchecked default

    Test(): A<T> = this

printM<T>(x: T): () where T: { M<C>(): C; M2<D>(): D; Test(): T } =
    let a1 = x.M<__oly_int32>()
    let a2 = x.M2<__oly_int32>()
    print(a1)
    print(a2)

main(): () =
    let a = A<__oly_object>()
    printM(a)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "00"
    |> ignore

[<Fact>]
let ``Regression - closure should capture correctly``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

test(mutable x: __oly_int32): ()  =
    let f() =
        let g() =
            x <- 123
        g()
    f()
    print(x)

main(): () =
    test(55)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Regression - slow option type pattern matching``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

pattern Some<T>(option: Option<T>): T =
    option.Value

main(): () =
    match (Option(123))
    | Some(value) =>
        print(value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Regression - slow option type pattern matching 2``() =
    let src =
        """
module TestModule

#[intrinsic("print")]
print(__oly_object): ()

class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

#[open]
module OptionPatterns<T> =

    pattern Some(value: Option<T>): T =
        print("Some was hit")
        value.Value

test(x: Option<__oly_utf16>): () =
    match (x)
    | Some(_) => ()

main(): () =
    test(Option("test"))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Some was hit"
    |> ignore

[<Fact>]
let ``Regression - slow option type pattern matching 3``() =
    let src =
        """
module TestModule

#[intrinsic("print")]
print(__oly_object): ()

class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

#[open]
module OptionPatterns<T> =

    pattern Some(value: Option<T>): T =
        print("Some was hit")
        value.Value

test(x: Option<__oly_utf16>): () =
    match (x)
    | Some(y) => ()

main(): () =
    test(Option("test"))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Some was hit"
    |> ignore

[<Fact>]
let ``Regression - slow option type pattern matching 4``() =
    let src =
        """
module TestModule

#[intrinsic("print")]
print(__oly_object): ()

class Option<T> =
    public field Value: T
    new(value: T) = this { Value = value }

#[open]
module OptionPatterns<T> =

    pattern None(value: Option<T>): () =
        print("None was hit")
        ()

test(x: Option<__oly_utf16>): () =
    match (x)
    | None => ()
    | _ => ()

main(): () =
    test(Option("test"))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "None was hit"
    |> ignore

[<Fact>]
let ``Regression - unit as type argument should work``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[inline(never)]
f<T>(): T = unchecked default

main(): () =
    print(f<()>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "()"
    |> ignore

[<Fact>]
let ``Regression - unit as type argument should work 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[inline(never)]
f<T>(): T = unchecked default

#[inline(never)]
getBool(): __oly_bool = false

main(): () =
    print(if (getBool()) f<()>() else ())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "()"
    |> ignore

[<Fact>]
let ``Regression - unit as type argument should work 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[inline(never)]
f<T>(): T = unchecked default

#[inline(never)]
getBool(): __oly_bool = true

main(): () =
    print(if (getBool()) f<()>() else ())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "()"
    |> ignore

[<Fact>]
let ``Regression - pattern match for nested tuple should work``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = 5
    match ((x, (x, x)))
    | (y, _) => 
        print("pass")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "pass"
    |> ignore

[<Fact>]
let ``Regression - pattern match should work with Or cases when binding a value``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = 5
    match (x, x, x)
    | _, y, _
    | _, _, y
    | y, _, _ when (true) => 
        print("pass")
        print(y)
    | _ => 
        print("fail")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "pass5"
    |> ignore

[<Fact>]
let ``Regression - pattern match (large) should work with Or cases when binding a value``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[intrinsic("int32")]
alias int32

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = 5
    match (x, x, x, x, x, x, x, x, x, x)
    | _, y, _, _, _, _, _, _, _, _
    | _, _, y, _, _, _, _, _, _, _
    | y, _, _, _, _, _, _, _, _, _
    | _, _, _, _, y, _, _, _, _, _ 
    | _, _, _, _, _, _, _, y, _, _ when (y == y) => 
        print("pass")
        print(y)
    | _ => 
        print("fail")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "pass5"
    |> ignore

[<Fact>]
let ``Regression - pattern match (large) should work with Or cases when binding a value 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = 5
    match (x, x, x, x, x, x, x, x, x, x)
    | _, y, _, _, _, _, 5, 5, _, _
    | 5, _, y, _, _, _, _, _, _, 5
    | y, 5, _, 5, _, _, _, _, _, _
    | _, _, _, _, y, _, _, 5, _, _ 
    | _, 5, 5, _, _, _, _, y, _, _ when (true) => 
        print("pass")
        print(y)
    | _ => 
        print("fail")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "pass5"
    |> ignore

[<Fact>]
let ``Regression - pattern match should work with Or cases when binding a value 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = 5
    match ((x, x, x))
    | (2, y, 10)
    | (8, 3, y)
    | (y, 1, 1) => 
        print("fail")
        print(y)
    | _ => 
        print("pass")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "pass"
    |> ignore

[<Fact>]
let ``Enum should work with extensions``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[open]
enum X64GeneralPurposeRegister32 =
    | EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | EBP
    | ESP

#[open]
extension X64GeneralPurposeRegister32Extensions =
    inherits X64GeneralPurposeRegister32

    RegisterEncoding: __oly_uint8
        get() =
            match (this)
            | _ => 255

main(): () =
    print(EAX.get_RegisterEncoding())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "255"
    |> ignore

[<Fact>]
let ``Enum should work with extensions 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[open]
enum X64GeneralPurposeRegister32 =
    | EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | EBP
    | ESP

#[open]
extension X64GeneralPurposeRegister32Extensions =
    inherits X64GeneralPurposeRegister32

    RegisterEncoding: __oly_uint8
        get() =
            match (this)
            | _ => 255

main(): () =
    let x = EAX
    print(x.get_RegisterEncoding())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "255"
    |> ignore

[<Fact>]
let ``Enum should work with extensions 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[open]
enum X64GeneralPurposeRegister32 =
    | EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | EBP
    | ESP

#[open]
extension X64GeneralPurposeRegister32Extensions =
    inherits X64GeneralPurposeRegister32

    RegisterEncoding: __oly_uint8
        get() =
            match (this)
            | _ => 255

main(): () =
    print(EAX.RegisterEncoding)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "255"
    |> ignore

[<Fact>]
let ``Enum should work with extensions 4``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[open]
enum X64GeneralPurposeRegister32 =
    | EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | EBP
    | ESP

#[open]
extension X64GeneralPurposeRegister32Extensions =
    inherits X64GeneralPurposeRegister32

    RegisterEncoding: __oly_uint8
        get() =
            match (this)
            | _ => 255

main(): () =
    let x = EAX
    print(x.RegisterEncoding)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "255"
    |> ignore

[<Fact>]
let ``Enum should work when using pattern matching``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[open]
enum X64GeneralPurposeRegister32 =
    | EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | EBP
    | ESP

printRegister(r: X64GeneralPurposeRegister32): () =
    match (r)
    | X64GeneralPurposeRegister32.EAX => print("eax")
    | X64GeneralPurposeRegister32.EBX => print("ebx")
    | _ => print("failed")

main(): () =
    printRegister(EBX)
    printRegister(EAX)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "ebxeax"
    |> ignore

[<Fact>]
let ``Enum should work when using pattern matching 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[open]
enum X64GeneralPurposeRegister32 =
    | EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | EBP
    | ESP

printRegister(r: X64GeneralPurposeRegister32): () =
    match (r)
    | EAX => print("eax")
    | EBX => print("ebx")
    | _ => print("failed")

main(): () =
    printRegister(EBX)
    printRegister(EAX)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "ebxeax"
    |> ignore

[<Fact>]
let ``Weird one``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int

#[intrinsic("utf16")]
alias string

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("add")]
(+)(int, int): int

#[intrinsic("unsafe_cast")]
unsafeCast<T>(__oly_object): T

interface IMoveable =
    Position: int get, set
   
class Item =
    implements IMoveable

    Name: string get, set = "Bar"

    Position: int
        get() =
            print("Get - ")
            print(this.Name)
            print(" ")
            0
        set(value) =
            print("Set - ")
            print(this.Name)
            print(" ")

GetOffset<T>(item: byref<T>): int =
    let item2 = Item()
    item2.Name <- "Bar"
    item <- unsafeCast(item2)
    0

Shift<T>(mutable item : T): () where T: IMoveable, not struct =
    item.Position <- item.Position + (GetOffset(&item))

main(): () =
    let item = Item()
    item.Name <- "Goo"
    Shift(item)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Get - Goo Set - Goo "
    |> ignore

[<Fact>]
let ``Weird one 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int

#[intrinsic("utf16")]
alias string

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("add")]
(+)(int, int): int

#[intrinsic("unsafe_cast")]
unsafeCast<T>(__oly_object): T

interface IMoveable =
    Position: int get, set
   
struct Item =
    implements IMoveable

    Name: string set, get = "Bar"

    Position: int
        #[inline(never)]
        get() =
            print("Get - ")
            print(this.Name)
            print(" ")
            0
        #[inline(never)]
        set(value) =
            print("Set - ")
            print(this.Name)
            print(" ")

#[inline(never)]
GetOffset<T>(item: byref<T>): int =
    let mutable item2 = Item()
    item2.Name <- "Bar"
    item <- unsafeCast(item2)
    0

#[inline(never)]
Shift<T>(mutable item : T): () where T: IMoveable, struct =
    item.Position <- item.Position + (GetOffset(&item))

main(): () =
    let mutable item = Item()
    item.Name <- "Goo"
    Shift(item)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Get - Goo Set - Bar "
    |> ignore

[<Fact>]
let ``Weird one 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int

#[intrinsic("utf16")]
alias string

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("add")]
(+)(int, int): int

#[intrinsic("unsafe_cast")]
unsafeCast<T>(__oly_object): T

interface IMoveable =
    Position: int get, set
   
struct Item =
    implements IMoveable

    Name: string set, get = "Bar"

    Position: int
        #[inline]
        get() =
            print("Get - ")
            print(this.Name)
            print(" ")
            0
        #[inline]
        set(value) =
            print("Set - ")
            print(this.Name)
            print(" ")

#[inline]
GetOffset<T>(item: byref<T>): int =
    let mutable item2 = Item()
    item2.Name <- "Bar"
    item <- unsafeCast(item2)
    0

#[inline]
Shift<T>(mutable item : T): () where T: IMoveable, struct =
    item.Position <- item.Position + (GetOffset(&item))

main(): () =
    let mutable item = Item()
    item.Name <- "Goo"
    Shift(item)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Get - Goo Set - Bar "
    |> ignore

[<Fact>]
let ``Extension on object should work``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

struct Vector3

#[open]
extension ObjectExt =
    inherits object

    Test(): () = print("test")

main(): () =
    let v = Vector3()
    v.Test()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Extension on object should work 2``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

struct Vector3

#[open]
extension ObjectExt =
    inherits object

    static Test(): () = print("test")

Test<T>(): () where T: { static Test(): () } =
    T.Test()

main(): () =
    Test<Vector3>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Extension on object should work 3``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

struct Vector3

struct Matrix

#[open]
extension Ops =
    inherits object
    
    static op_Multiply(x: Matrix, y: Vector3): Matrix = 
        print("test")
        default

multiply<T1, T2, T3, W>(x: T1, y: T2): T3 where W: { static op_Multiply(T1, T2): T3 } =
    W.op_Multiply(x, y)

main() : () =
    let result = multiply<Matrix, Vector3, Matrix, object>(default, default)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Extension on object should work 4``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

struct Vector3

struct Matrix

#[open]
extension Ops =
    inherits object
    
    static op_Multiply(x: Matrix, y: Vector3): Matrix = 
        print("test")
        default

multiply<T1, T2, T3, W>(x: T1, y: T2): T3 where W: { static op_Multiply(T1, T2): T3 } =
    W.op_Multiply(x, y)

main() : () =
    let result = multiply<Matrix, Vector3, Matrix, Vector3>(default, default)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Regression - extension of array should pass``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("get_length")]
getLength<T>(mutable T[]): int32

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

#[intrinsic("print")]
print(object): ()

#[open]
extension ArrayExtensions<T> =
    inherits mutable T[]

    #[inline]
    GetLength(): int32 = getLength(this)

main(): () =
    let arr = mutable [123]
    let result = arr.GetLength()
    print(result)
    print(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1123"
    |> ignore

[<Fact>]
let ``Regression - extension of array should pass 2``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("get_length")]
getLength<T>(mutable T[]): int32

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

#[intrinsic("print")]
print(object): ()

#[open]
extension ArrayExtensions<T> =
    inherits mutable T[]

    Length: int32 
        #[inline]
        get() = getLength(this)

main(): () =
    let arr = mutable [123]
    let result = arr.Length
    print(result)
    print(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1123"
    |> ignore

[<Fact>]
let ``Multiple type parameters on extension with shape constraint should compile``() =
    let src =
        """
#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("add")]
(+)(float32, float32): float32

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static (+)<T4, T5, T6>(T4, T5): T6 where T4: { static op_Addition(T4, T5): T6 } }, { static op_Addition(T1, T2): T3 } = 
    T1.(+)<T1, T2, T3>(x, y)

struct Vector3 =

    public field mutable X: float32 = 0
    public field mutable Y: float32 = 0
    public field mutable Z: float32 = 0

    static op_Addition(v1: Vector3, v2: Vector3): Vector3 = 
        let mutable v3 = Vector3()
        v3.X <- v1.X + v2.X
        v3.Y <- v1.Y + v2.Y
        v3.Z <- v1.Z + v2.Z
        v3

#[open]
extension AddExtension =
    inherits Vector3

    static (+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Addition(T1, T2): T3 } = 
        T1.op_Addition(x, y)

main(): () =
    let mutable v = Vector3()
    v.X <- 1
    v.Y <- 2
    v.Z <- 3
    
    let result: Vector3 = v + v

    print(result.X)
    print(result.Y)
    print(result.Z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "246"
    |> ignore

[<Fact>]
let ``Multiple type parameters on extension with shape constraint should compile 2``() =
    let src =
        """
#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("add")]
(+)(float32, float32): float32

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static (+)<T4, T5, T6>(T4, T5): T6 where T4: { static op_Addition(T4, T5): T6 } }, { static op_Addition(T1, T2): T3 } = 
    T1.(+)<T1, T2, T3>(x, y)

struct Vector3 =

    public field mutable X: float32 = 0
    public field mutable Y: float32 = 0
    public field mutable Z: float32 = 0

    static op_Addition(v1: Vector3, v2: Vector3): Vector3 = 
        let mutable v3 = Vector3()
        v3.X <- v1.X + v2.X
        v3.Y <- v1.Y + v2.Y
        v3.Z <- v1.Z + v2.Z
        v3

#[open]
extension AddExtension =
    inherits object

    static (+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Addition(T1, T2): T3 } = 
        T1.op_Addition(x, y)

main(): () =
    let mutable v = Vector3()
    v.X <- 1
    v.Y <- 2
    v.Z <- 3
    
    let result: Vector3 = v + v

    print(result.X)
    print(result.Y)
    print(result.Z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "246"
    |> ignore

[<Fact>]
let ``Integral does not equal zero should succeed``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("not_equal")]
(!=)(int32, int32): bool

test1(value: int32): () =
    if (value != 0)
        print("passed")
    else
        print("failed")

main(): () =
    test1(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Integral does not equal value should succeed``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("not_equal")]
(!=)(int32, int32): bool

test1(value: int32): () =
    if (value != 256)
        print("passed")
    else
        print("failed")

main(): () =
    test1(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Object does equal null should succeed``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("equal")]
(===)<T>(T, T): bool where T: not struct

#[null]
class Test1

test1(value: Test1): () =
    if (value === null)
        print("failed")
    else
        print("passed")

main(): () =
    test1(Test1())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Object does equal null should succeed 2``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("equal")]
(===)<T>(T, T): bool where T: not struct

#[intrinsic("and")]
(&&)(bool, bool): bool

#[null]
class Test1

test1(value1: Test1, value2: Test1): () =
    if (value1 === null && value2 === null)
        print("failed")
    else
        print("passed")

main(): () =
    test1(Test1(), Test1())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Object does equal null should succeed 3``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("equal")]
(===)<T>(T, T): bool where T: not struct

#[intrinsic("and")]
(&&)(bool, bool): bool

#[null]
class Test1

test1(value1: Test1, value2: Test1, value3: Test1): () =
    if (value1 === null && value2 === null && value3 === null)
        print("passed")
    else
        print("failed")

main(): () =
    test1(null, null, null)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Overload with newtype similar to actual type``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

newtype NewInt32 =
    field Value: int32

test(x: int32): () =
    print(x)

test(x: NewInt32): () =
    print(x)
    print("newtype")

test2(x: int32): int32 =
    x

test2(x: NewInt32): NewInt32 =
    x

main(): () =
    test(88)
    test(NewInt32(77))
    test(88)
    test(88)
    test(NewInt32(77))
    test(test2(NewInt32(55)))
    test(test2(22))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "8877newtype888877newtype55newtype22"
    |> ignore


[<Fact>]
let ``Overload with newtype similar to actual type 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

newtype NewInt32 =
    field Value: int32

interface ITest =
    test(x: int32): ()
    test(x: NewInt32): ()

class TestClass =
    implements ITest

    test(x: int32): () =
        print(x)

    test(x: NewInt32): () =
        print("newtype")
        print(x)

main(): () =
    let x = 88
    let y = 77

    let t = TestClass()

    t.test(x)
    t.test(NewInt32(77))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "88newtype77"
    |> ignore

[<Fact>]
let ``Overload with byref/inref``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

test(x: byref<int32>): () =
    print(x)
    print("byref")

test(x: inref<int32>): () =
    print(x)
    print("inref")

main(): () =
    let mutable x = 88
    let y = 77
    test(&x)
    test(&y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "88byref77inref"
    |> ignore

[<Fact>]
let ``Overload with byref/inref 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

test(x: byref<int32>): byref<int32> =
    print("byref")
    &x

test(x: inref<int32>): inref<int32> =
    print("inref")
    &x

main(): () =
    let mutable x = 88
    let y = 77
    let by = &test(&y)
    let bx = &test(&x)
    bx <- 22
    print(x)
    print(bx)
    print(by)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "inrefbyref222277"
    |> ignore


[<Fact>]
let ``Overload with byref/inref 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

interface ITest =
    test(x: byref<int32>): ()
    test(x: inref<int32>): ()

class TestClass =
    implements ITest

    test(x: byref<int32>): () =
        print("byref")
        print(x)

    test(x: inref<int32>): () =
        print("inref")
        print(x)

main(): () =
    let mutable x = 88
    let y = 77

    let t = TestClass()

    t.test(&x)
    t.test(&y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "byref88inref77"
    |> ignore

[<Fact>]
let ``Simple character literal``() =
    let src =
        """
#[intrinsic("char16")]
alias char

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = 'y'
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "y"
    |> ignore

[<Fact>]
let ``Mutable struct captured in lambda should pass``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct TestStruct =
    public field mutable X: int32 = 0

    mutable MutateX(): () =
        this.X <- 123

main(): () =
    let mutable t = TestStruct()
    let f() =
        t.MutateX()
    f()
    print(t.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Mutable value array should work with shadowing``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let mutable values = [1]
    let mutable shadowedValues = values
    print("should work")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "should work"
    |> ignore

[<Fact>]
let ``Static lambda and local lambda expression should work``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

main(): () =
    static let testStaticLocal() =
        print("hello")

    print(" ")

    let mutable value = 4

    let testLocal() =
        match (value)
        | 3 => print("3")
        | 4 when (value == 4) =>
            print(" world")
        | _ =>
            ()

    testStaticLocal()
    testLocal()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput " hello world"
    |> ignore

[<Fact>]
let ``Qualifying dots with indexer should work with correct mutation``() =
    let src =
        """
module TestModule

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

#[intrinsic("print")]
print(__oly_object): ()

struct S1 =
    public field mutable X: int32 = 0

main(): () =
    let mutable xs = mutable [S1()]
    xs[0].X <- 123
    print(xs[0].X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Qualifying dots with indexer should work with correct mutation 2``() =
    let src =
        """
module TestModule

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

#[intrinsic("print")]
print(__oly_object): ()

struct S1 =
    public field mutable X: int32 = 0

class C1 =
    public field XS: mutable S1[] = mutable [S1()]

main(): () =
    let c1 = C1()
    c1.XS[0].X <- 456
    print(c1.XS[0].X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Qualifying dots with indexer should work with correct mutation 3``() =
    let src =
        """
module TestModule

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

#[intrinsic("print")]
print(__oly_object): ()

struct S1 =
    public field mutable X: int32 = 0

class C1 =
    public field XS: mutable S1[] = mutable [S1()]

class C2 =
    public field C: C1 = C1()

main(): () =
    let c2 = C2()
    c2.C.XS[0].X <- 789
    print(c2.C.XS[0].X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "789"
    |> ignore

[<Fact>]
let ``Witness pass should work for unrelated T``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[open]
extension VkResultExtensions =
    inherits VkResult

    #[inline(never)]
    static op_Equality(result1: VkResult, result2: VkResult): bool =
        true

#[inline(never)]
(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

enum VkResult =
    | VK_SUCCESS
    | VK_FAILURE

struct TestStruct

#[inline(never)]
update<T>(): () =
    if (VkResult.VK_SUCCESS == VkResult.VK_SUCCESS)
        print("passed")

main(): () =
    update<TestStruct>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Recursive type arguments but unused``() =
    let src =
        """
namespace Test

module Test1 =
    #[intrinsic("print")]
    print(__oly_object): ()

module Test2 =

    main(): () =
        let m = M()
        Test1.print("passed")

    struct N<T>

    struct M =
        field mutable E: N<M> = default
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Newtype with a static function``() =
    let src =
        """
#[open]
newtype Option<T> =
    field value: T

    static Some(value: T): Option<T> = Option(value)

    GetValue(): T = this.value

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = Some("passed")
    print(x.GetValue())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Newtype option API``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(===)(o1: __oly_object, o2: __oly_object): bool
#[intrinsic("not_equal")]
(!==)(o1: __oly_object, o2: __oly_object): bool

#[open]
newtype Option<T> where T: not struct =
    field value: T

    pattern Some(option: Option<T>): T when (option.value !== unchecked default) =>
        option.value

    pattern None(option: Option<T>): () when (option.value === unchecked default) =>
        ()

    static Some(value: T): Option<T> = Option(value)

    static None: Option<T>
        get() = Option(unchecked default)

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = Some("passed")
    match (x)
    | Some(text) =>
        print(text)
    | _ =>
        ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Newtype option API - different member order``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(===)(o1: __oly_object, o2: __oly_object): bool
#[intrinsic("not_equal")]
(!==)(o1: __oly_object, o2: __oly_object): bool

#[open]
newtype Option<T> where T: not struct =
    field value: T

    static Some(value: T): Option<T> = Option(value)

    static None: Option<T>
        get() = Option(unchecked default)

    pattern Some(option: Option<T>): T when (option.value !== unchecked default) =>
        option.value

    pattern None(option: Option<T>): () when (option.value === unchecked default) =>
        ()

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = Some("passed")
    match (x)
    | Some(text) =>
        print(text)
    | _ =>
        ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Newtype option API 2``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(===)(o1: __oly_object, o2: __oly_object): bool
#[intrinsic("not_equal")]
(!==)(o1: __oly_object, o2: __oly_object): bool

#[open]
newtype Option<T> where T: not struct =
    field value: T

    pattern Some(option: Option<T>): T when (option.value !== unchecked default) =>
        option.value

    pattern None(option: Option<T>): () when (option.value === unchecked default) =>
        ()

    static Some(value: T): Option<T> = Option(value)

    static None: Option<T>
        get() = Option(unchecked default)

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x: Option<__oly_utf16> = None
    match (x)
    | Some(text) =>
        print(text)
    | _ =>
        print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Regression - Should not crash for pattern matching enum``() =
    let src =
        """
#[intrinsic("char16")]
alias char

#[intrinsic("uint16")]
alias uint16

#[intrinsic("unsafe_cast")]
uint16(char): uint16

enum Key =
    | Unknown
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z

    | Esc
    | Tilde

TranslateKey(c: char): Key =
    match (c)
    | 'A' => Key.A
    | 'B' => Key.B
    | 'C' => Key.C
    | 'D' => Key.D
    | 'E' => Key.E
    | 'F' => Key.F
    | 'G' => Key.G
    | 'H' => Key.H
    | 'I' => Key.I
    | 'J' => Key.J
    | 'K' => Key.K
    | 'L' => Key.L
    | 'M' => Key.M
    | 'N' => Key.N
    | 'O' => Key.O
    | 'P' => Key.P
    | 'Q' => Key.Q
    | 'R' => Key.R
    | 'S' => Key.S
    | 'T' => Key.T
    | 'U' => Key.U
    | 'V' => Key.V
    | 'W' => Key.W
    | 'X' => Key.X
    | 'Y' => Key.Y
    | 'Z' => Key.Z
    | _ => Key.Unknown

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = TranslateKey('A')
    match (x)
    | Key.A =>
        print("passed")
    | Key.Esc =>
        print("failed")
    | Key.Tilde =>
        print("failed")
    | _ => 
        print("failed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Regression - Should not crash for pattern matching enum 2``() =
    let src =
        """
#[intrinsic("char16")]
alias char

#[intrinsic("uint16")]
alias uint16

#[intrinsic("unsafe_cast")]
uint16(char): uint16

enum Key =
    | Unknown
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z

    | Esc
    | Tilde

TranslateKey(c: char): Key =
    match (c)
    | 'A' => Key.A
    | 'B' => Key.B
    | 'C' => Key.C
    | 'D' => Key.D
    | 'E' => Key.E
    | 'F' => Key.F
    | 'G' => Key.G
    | 'H' => Key.H
    | 'I' => Key.I
    | 'J' => Key.J
    | 'K' => Key.K
    | 'L' => Key.L
    | 'M' => Key.M
    | 'N' => Key.N
    | 'O' => Key.O
    | 'P' => Key.P
    | 'Q' => Key.Q
    | 'R' => Key.R
    | 'S' => Key.S
    | 'T' => Key.T
    | 'U' => Key.U
    | 'V' => Key.V
    | 'W' => Key.W
    | 'X' => Key.X
    | 'Y' => Key.Y
    | 'Z' => Key.Z
    | x =>
        match (uint16(x))
        | 192 => Key.Esc
        | 27 => Key.Tilde
        | _ => Key.Unknown

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = TranslateKey('A')
    match (x)
    | Key.A =>
        print("passed")
    | Key.Esc =>
        print("failed")
    | Key.Tilde =>
        print("failed")
    | _ => 
        print("failed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Regression - Should not crash for pattern matching enum 3``() =
    let src =
        """
#[intrinsic("char16")]
alias char

#[intrinsic("uint16")]
alias uint16

#[intrinsic("unsafe_cast")]
uint16(char): uint16

enum Key =
    | Unknown
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z

    | Esc
    | Tilde

TranslateKey(c: char): Key =
    match (c, c)
    | 'A', _ => Key.A
    | 'B', _ => Key.B
    | 'C', _ => Key.C
    | 'D', _ => Key.D
    | 'E', _ => Key.E
    | 'F', _ => Key.F
    | 'G', _ => Key.G
    | 'H', _ => Key.H
    | 'I', 'I' => Key.I
    | 'J', _ => Key.J
    | 'K', _ => Key.K
    | 'L', _ => Key.L
    | 'M', _ => Key.M
    | 'N', 'N' => Key.N
    | 'O', _ => Key.O
    | 'P', _ => Key.P
    | 'Q', _ => Key.Q
    | 'R', _ => Key.R
    | 'S', _ => Key.S
    | 'T', _ => Key.T
    | 'U', _ => Key.U
    | 'V', _ => Key.V
    | 'W', _ => Key.W
    | 'X', _ => Key.X
    | 'Y', _ => Key.Y
    | 'Z', _ => Key.Z
    | x, y =>
        match (uint16(x))
        | 192 => Key.Esc
        | 27 => Key.Tilde
        | _ => Key.Unknown

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = TranslateKey('A')
    match (x)
    | Key.A =>
        print("passed")
    | Key.Esc =>
        print("failed")
    | Key.Tilde =>
        print("failed")
    | _ => 
        print("failed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Generic interface that handles functions that have ambiguity``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IA<T> =

    Test(x: T): ()
    Test(x: int32): ()

class Test =
    implements IA<int32>

    Test(x: int32): () =
        print(x)

main(): () =
    let t = Test()
    t.Test(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Generic interface that handles functions that have ambiguity 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IA<T> =

    default Test(x: T): () =
        print("failed_T")

    default Test(x: int32): () =
        print("failed_int32")

class Test =
    implements IA<int32>

    Test(x: int32): () =
        print(x)

main(): () =
    let t = Test()
    let t = t: IA<int32>
    t.Test(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Generic interface that handles functions that have ambiguity 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias string

#[intrinsic("print")]
print(__oly_object): ()

interface IA<T> =

    Test(x: T): ()
    Test(x: int32): ()

class Test =
    implements IA<int32>

    Test(x: int32): () =
        print(x)

class Test2 =
    implements IA<string>

    Test(x: int32): () = ()

    Test(x: string): () = 
        print(x)

main(): () =
    let t2 = Test2()
    let t2 = t2: IA<string>
    t2.Test(456)
    t2.Test("hello")

    let t = Test()
    let t = t: IA<int32>
    t.Test(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello123"
    |> ignore

[<Fact>]
let ``Generic class that handles functions that have ambiguity``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

abstract class A<T> =

    abstract default Test(x: T): () =
        print("failed_T")

    abstract default Test(x: int32): () =
        print("failed_int32")

class Test =
    inherits A<int32>

    overrides Test(x: int32): () =
        print(x)

main(): () =
    let t = Test()
    let t = t: A<int32>
    t.Test(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Generic class that handles functions that have ambiguity 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias string

#[intrinsic("print")]
print(__oly_object): ()

abstract class A<T> =

    abstract default Test(x: T): () =
        print("Test_T_")

    abstract default Test(x: int32): () =
        print("Test_int32_")

class Test =
    inherits A<string>

class Test2 =
    inherits A<int32>

    overrides Test(x: int32): () =
        print(x)

main(): () =
    let t = Test()
    let t = t: A<string>
    t.Test("test")
    t.Test(456)

    let t = Test2()
    let t = t: A<int32>
    t.Test(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test_T_Test_int32_123"
    |> ignore

[<Fact>]
let ``Abstract class should work``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

abstract class C1 =

   abstract default M(): () = ()

   abstract M2(): ()

class C2 =
   inherits C1

   overrides M2(): () = print("M")

test(x: C1): () =
   x.M2()

main(): () =
   let c2 = C2()
   test(c2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "M"
    |> ignore

[<Fact>]
let ``Array extension should work``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface ITest =

    Test(): ()

#[open]
extension ArrayTestExtension<T> =
    inherits T[]
    implements ITest

    Test(): () = print("test")

test<T>(xs: T): () where T: trait ITest =
    xs.Test()

main(): () =
    let xs = [1]
    test(xs)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"
    |> ignore

[<Fact>]
let ``Multiple constructors where one calls the other``() =
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
        this {
            value1 = value1
            value2 = value2
        }       

main(): () =
    let c = C("hello")
    print(c.value1)
    print(c.value2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hellopassed"
    |> ignore

[<Fact>]
let ``Let pattern binding should pass``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let (x, y) = (1, 2)
    print(x)
    print(y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "12"
    |> ignore

[<Fact>]
let ``Modify Vector X from a class``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct Vector2 =
    public field mutable X: int32 = 0
    public field mutable Y: int32 = 0

class C =
    public field mutable X: Vector2 = Vector2()

main(): () =
    let c = C()
    c.X.X <- 123
    print(c.X.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Cannot modify Vector X from a class because of property``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct Vector2 =
    public field mutable X: int32 = 0
    public field mutable Y: int32 = 0

class C =
    public X: Vector2 get = Vector2()

main(): () =
    let c = C()
    c.X.X <- 123
    print(c.X.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Closure over trying to get a function pointer``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("native_int")]
alias nint

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("unsafe_cast")]
nint<TResult, TArguments...>(static (TArguments...) -> TResult): nint

#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

class Delegate =

    Invoke(): () = ()

main(): () =
    // This function is necessary as the bug appeared only when there was a local function before it.
    let test(x: int32) =
        ()

    let d = Delegate()
    let f =
        () ->
            let f = nint(&&d.Invoke)
    
    print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Passing witnesses to ctor type arguments by only by function type arguments``() =
    """
namespace Test

#[intrinsic("int32")]
alias int32

interface IComponent =

    static abstract GetValue(): int32

interface IArchetypeReference =

    ArchetypedIndex: int32 get

class ArchetypeReference<T0> where T0: unmanaged, trait IComponent =
    implements IArchetypeReference

    ArchetypedIndex: int32 get

    new() =
        this {
            ArchetypedIndex = T0.GetValue()
        }

struct S1

struct S2

#[open]
extension S1Component =
    inherits S1
    implements IComponent

    static overrides GetValue(): int32 = 11

#[open]
extension S2Component =
    inherits S2
    implements IComponent

    static overrides GetValue(): int32 = 22

module TestModule =
    #[intrinsic("print")]
    print(__oly_object): ()

    GetIndex<T>(): int32 where T: unmanaged, trait IComponent =
        // 'T' might have a witness and it needs to be passed to type-ctor 'ArchetypeReference'.
        let r = ArchetypeReference<T>()
        r.ArchetypedIndex

    main(): () =
        let value1 = GetIndex<S1>()
        let value2 = GetIndex<S2>()
        print(value1)
        print(value2)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1122"
    |> ignore

[<Fact>]
let ``Passing witnesses to ctor type arguments by only by function type arguments 2``() =
    """
namespace Test

#[intrinsic("int32")]
alias int32

interface IComponent =

    static abstract GetValue(): int32

interface IArchetypeReference =

    ArchetypedIndex: int32 get

class ArchetypeReference<T0> where T0: unmanaged, trait IComponent =
    implements IArchetypeReference

    ArchetypedIndex: int32 get() = T0.GetValue()

struct S1

struct S2

#[open]
extension S1Component =
    inherits S1
    implements IComponent

    static overrides GetValue(): int32 = 11

#[open]
extension S2Component =
    inherits S2
    implements IComponent

    static overrides GetValue(): int32 = 22

module TestModule =
    #[intrinsic("print")]
    print(__oly_object): ()

    GetIndex<T>(): int32 where T: unmanaged, trait IComponent =
        // 'T' might have a witness and it needs to be passed to type-ctor 'ArchetypeReference'.
        let r = ArchetypeReference<T>()
        r.ArchetypedIndex

    main(): () =
        let value1 = GetIndex<S1>()
        let value2 = GetIndex<S2>()
        print(value1)
        print(value2)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1122"
    |> ignore

[<Fact>]
let ``Passing witnesses to ctor type arguments by only by function type arguments 3``() =
    """
namespace Test

#[intrinsic("int32")]
alias int32

interface IComponent =

    static abstract GetValue(): int32

interface IComponent2 =

    static abstract GetValue2(): int32

interface IArchetypeReference =

    ArchetypedIndex: int32 get

class ArchetypeReference<T0> where T0: unmanaged, trait IComponent, trait IComponent2 =
    implements IArchetypeReference

    ArchetypedIndex: int32 get

    new() =
        this {
            ArchetypedIndex = __oly_add(T0.GetValue(), T0.GetValue2())
        }

struct S1

struct S2

#[open]
extension S1Component =
    inherits S1
    implements IComponent

    static overrides GetValue(): int32 = 11

#[open]
extension S1Component2 =
    inherits S1
    implements IComponent2

    static overrides GetValue2(): int32 = 22

#[open]
extension S2Component =
    inherits S2
    implements IComponent

    static overrides GetValue(): int32 = 22

#[open]
extension S2Component2 =
    inherits S2
    implements IComponent2

    static overrides GetValue2(): int32 = 33

module TestModule =
    #[intrinsic("print")]
    print(__oly_object): ()

    GetIndex<T>(): int32 where T: unmanaged, trait IComponent, trait IComponent2 =
        // 'T' might have a witness and it needs to be passed to type-ctor 'ArchetypeReference'.
        let r = ArchetypeReference<T>()
        r.ArchetypedIndex

    main(): () =
        let value1 = GetIndex<S1>()
        let value2 = GetIndex<S2>()
        print(value1)
        print(value2)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "3355"
    |> ignore

[<Fact>]
let ``Passing witnesses to ctor type arguments by only by function type arguments 4``() =
    """
namespace Test

#[intrinsic("int32")]
alias int32

interface IComponent =

    static abstract GetValue(): int32

interface IComponent2 =

    static abstract GetValue2(): int32

interface IArchetypeReference =

    ArchetypedIndex: int32 get

class ArchetypeReference<T0> where T0: unmanaged, trait IComponent, trait IComponent2 =
    implements IArchetypeReference

    ArchetypedIndex: int32 get() = __oly_add(T0.GetValue(), T0.GetValue2())

struct S1

struct S2

#[open]
extension S1Component =
    inherits S1
    implements IComponent

    static overrides GetValue(): int32 = 11

#[open]
extension S1Component2 =
    inherits S1
    implements IComponent2

    static overrides GetValue2(): int32 = 22

#[open]
extension S2Component =
    inherits S2
    implements IComponent

    static overrides GetValue(): int32 = 22

#[open]
extension S2Component2 =
    inherits S2
    implements IComponent2

    static overrides GetValue2(): int32 = 33

module TestModule =
    #[intrinsic("print")]
    print(__oly_object): ()

    GetIndex<T>(): int32 where T: unmanaged, trait IComponent, trait IComponent2 =
        // 'T' might have a witness and it needs to be passed to type-ctor 'ArchetypeReference'.
        let r = ArchetypeReference<T>()
        r.ArchetypedIndex

    main(): () =
        let value1 = GetIndex<S1>()
        let value2 = GetIndex<S2>()
        print(value1)
        print(value2)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "3355"
    |> ignore

[<Fact>]
let ``Passing witnesses to ctor type arguments by only by function type arguments 5``() =
    """
namespace Test

#[intrinsic("int32")]
alias int32

interface IComponent =

    static abstract GetValue(): int32

interface IArchetypeReference =

    ArchetypedIndex: int32 get

abstract class A

abstract default class ArchetypeReference<T0> where T0: unmanaged, trait IComponent =
    inherits A
    implements IArchetypeReference

    ArchetypedIndex: int32 get() = T0.GetValue()

struct S1

struct S2

#[open]
extension S1Component =
    inherits S1
    implements IComponent

    static overrides GetValue(): int32 = 11

#[open]
extension S2Component =
    inherits S2
    implements IComponent

    static overrides GetValue(): int32 = 22

module TestModule =
    #[intrinsic("print")]
    print(__oly_object): ()

    GetIndex<T>(): int32 where T: unmanaged, trait IComponent =
        // 'T' might have a witness and it needs to be passed to type-ctor 'ArchetypeReference'.
        let r = ArchetypeReference<T>()
        r.ArchetypedIndex

    main(): () =
        let value1 = GetIndex<S1>()
        let value2 = GetIndex<S2>()
        print(value1)
        print(value2)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1122"
    |> ignore

[<Fact>]
let ``Passing witnesses to ctor type arguments by only by function type arguments 6``() =
    """
namespace Test

#[intrinsic("int32")]
alias int32

interface IComponent =

    static abstract GetValue(): int32

interface IArchetypeReference =

    ArchetypedIndex: int32 get

abstract class A

abstract default class ArchetypeReference<T0> where T0: unmanaged, trait IComponent =
    inherits A
    implements IArchetypeReference

    ArchetypedIndex: int32 get() = T0.GetValue()

struct S1

module TestModule =
    #[open]
    extension S1Component =
        inherits S1
        implements IComponent

        static overrides GetValue(): int32 = 11

    #[intrinsic("print")]
    print(__oly_object): ()

    GetIndex<T>(): int32 where T: unmanaged, trait IComponent =
        // 'T' might have a witness and it needs to be passed to type-ctor 'ArchetypeReference'.
        let r = ArchetypeReference<T>()
        r.ArchetypedIndex

    Test(): () =
        let value1 = GetIndex<S1>()
        print(value1)

module Program =
    #[open]
    extension S1Component2 =
        inherits S1
        implements IComponent

        static overrides GetValue(): int32 = 22

    #[intrinsic("print")]
    print(__oly_object): ()

    main(): () =
        TestModule.Test()
        let value1 = TestModule.GetIndex<S1>()
        print(value1)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1122"
    |> ignore

[<Fact>]
let ``Passing witnesses to ctor type arguments by only by function type arguments - generic``() =
    """
namespace Test

#[intrinsic("int32")]
alias int32

interface IComponent =

    static abstract GetValue(): int32

interface IArchetypeReference<T> =

    ArchetypedIndex: int32 get

class ArchetypeReference<T0> where T0: unmanaged, trait IComponent =
    implements IArchetypeReference<T0>

    ArchetypedIndex: int32 get

    new() =
        this {
            ArchetypedIndex = T0.GetValue()
        }

struct S1

struct S2

#[open]
extension S1Component =
    inherits S1
    implements IComponent

    static overrides GetValue(): int32 = 11

#[open]
extension S2Component =
    inherits S2
    implements IComponent

    static overrides GetValue(): int32 = 22

module TestModule =
    #[intrinsic("print")]
    print(__oly_object): ()

    GetIndex<T>(): int32 where T: unmanaged, trait IComponent =
        // 'T' might have a witness and it needs to be passed to type-ctor 'ArchetypeReference'.
        let r = ArchetypeReference<T>()
        r.ArchetypedIndex

    main(): () =
        let value1 = GetIndex<S1>()
        let value2 = GetIndex<S2>()
        print(value1)
        print(value2)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1122"
    |> ignore

[<Fact>]
let ``Shape constraint can have a constructor``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

class A =

    Test(): () = print("passed")

test<T>(): () where T: { new(); Test(): () } =
    let t = T()
    t.Test()

main(): () =
    test<A>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Static abstract for witness should work``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =
    static abstract default M(): () = print("failed")

struct S

#[open]
extension SA =
    inherits S
    implements IA

    static overrides M(): () = print("passed")

Test<T>(): () where T: trait IA =
    T.M()

main(): () =
    Test<S>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Static abstract for witness should work 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =
    static abstract default M(): () = print("failed")

struct S

#[open]
extension SA =
    inherits S
    implements IA

    static overrides M(): () = print("passed")

Test<T>(): () where T: trait IA =
    let f = () -> T.M()
    f()

main(): () =
    Test<S>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Static abstract for witness should work 3``() =
    let src =
        """
module Prelude

open extension Prelude.SA2

#[intrinsic("print")]
print(__oly_object): ()

interface IA =
    static abstract default M(): () = print("failed")

struct S

extension SA =
    inherits S
    implements IA

    static overrides M(): () = print("passed")

extension SA2 =
    inherits S
    implements IA

    static overrides M(): () = print("passed2")

Test<T>(): () where T: trait IA =
    let f = () -> T.M()
    f()

Test2(): () =
    Test<S>()
        """
    let src2 =
        """
open static Prelude

open extension Prelude.SA

main(): () =
    Test<S>()
    Test2()
        """
    OlyTwo src2 src
    |> withCompile
    |> shouldRunWithExpectedOutput "passedpassed2"
    |> ignore

[<Fact>]
let ``Abstract for witness should work``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =
    default M(): () = print("failed")

struct S

#[open]
extension SA =
    inherits S
    implements IA

    M(): () = print("passed")

Test<T>(s: T): () where T: trait IA =
    s.M()

main(): () =
    Test<S>(S())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Abstract for witness should work 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =
    default M(): () = print("failed")

struct S

#[open]
extension SA =
    inherits S
    implements IA

    M(): () = print("passed")

Test<T>(s: T): () where T: trait IA =
    let f = () -> s.M()
    f()

main(): () =
    Test<S>(S())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore


[<Fact>]
let ``Abstract for witness should work 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =
    default M(): () = print("failed")

    X: __oly_int32 get

struct S =

    public field mutable x: __oly_int32 = 0

#[open]
extension SA =
    inherits S
    implements IA

    X: __oly_int32
        get() =
            this.x
        set(value) =
            this.x <- value

    mutable M(): () =
        this.X <- __oly_add(this.X, 1)

Test<T>(s: T): () where T: trait IA =
    let f = 
        () -> 
            s.M()
            s.M()
            print(s.X)
    f()
    print(s.X)

main(): () =
    Test<S>(S())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "20"
    |> ignore

[<Fact>]
let ``Abstract for witness should work 4``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =
    default M(): () = print("failed")

    X: __oly_int32 get

struct S =

    public field mutable x: __oly_int32 = 0

#[open]
extension SA =
    inherits S
    implements IA

    X: __oly_int32
        get() =
            this.x
        set(value) =
            this.x <- value

    mutable M(): () =
        this.X <- __oly_add(this.X, 1)

Test(mutable s: S): ()=
    s.M()
    s.M()
    print(s.X)

main(): () =
    Test(S())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Abstract for witness should work 5``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

interface IA =
    default M(): () = print("failed")

    X: __oly_int32 get

struct S =

    public field mutable x: __oly_int32 = 0

#[open]
extension SA =
    inherits S
    implements IA

    X: __oly_int32
        get() =
            this.x
        set(value) =
            this.x <- value

    mutable M(): () =
        this.X <- __oly_add(this.X, 1)

Test(mutable s: S): ()=
    let f = 
        () -> 
            s.M()
            s.M()
            print(s.X)
    f()

main(): () =
    Test(S())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore


[<Fact>]
let ``Witness passing regression``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

interface IComponent =
    static abstract GetSize(): int32

class EntityQuery<T0, T1> =

    SayHello(): () = print("hello")

class EntityDb =

    CreateQuery<T0, T1>(): EntityQuery<T0, T1> where T0: trait IComponent where T1: trait IComponent =
        let query = EntityQuery<T0, T1>()
        query.SayHello()
        print(this.GetSize<T0>())
        print(this.GetSize<T1>())
        query

    GetSize<T>(): int32 where T: trait IComponent =
        T.GetSize()

struct S

struct S2

#[open]
extension SComponent =
    inherits S
    implements IComponent

    static overrides GetSize(): int32 = 123

#[open]
extension SComponent2 =
    inherits S2
    implements IComponent

    static overrides GetSize(): int32 = 456

main(): () =
    let db = EntityDb()
    let _ = db.CreateQuery<S, S2>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello123456"
    |> ignore

[<Fact>]
let ``Witness passing regression 2``() =
    let src =
        """
module Prelude

open extension Prelude.SComponent
open extension Prelude.SComponent2

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

interface IComponent =
    static abstract GetSize(): int32

class EntityQuery<T0, T1> =

    SayHello(): () = print("hello")

class EntityDb =

    CreateQuery<T0, T1>(): EntityQuery<T0, T1> where T0: trait IComponent where T1: trait IComponent =
        let query = EntityQuery<T0, T1>()
        query.SayHello()
        print(this.GetSize<T0>())
        print(this.GetSize<T1>())
        query

    GetSize<T>(): int32 where T: trait IComponent =
        T.GetSize()

struct S

struct S2


extension SComponent =
    inherits S
    implements IComponent

    static overrides GetSize(): int32 = 123


extension SComponent2 =
    inherits S2
    implements IComponent

    static overrides GetSize(): int32 = 456

testComponents(): () =
    let db = EntityDb()
    let _ = db.CreateQuery<S, S2>()
        """
    let src2 =
        """
open static Prelude

#[open]
extension SComponent3 =
    inherits S
    implements IComponent

    static overrides GetSize(): int32 = 899

#[open]
extension SComponent4 =
    inherits S2
    implements IComponent

    static overrides GetSize(): int32 = 988

main(): () =
    testComponents()
    let db = EntityDb()
    let _ = db.CreateQuery<S, S2>()
        """
    OlyTwo src2 src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello123456hello899988"
    |> ignore

[<Fact>]
let ``Call getter of class property that returns a struct``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct Vector2 =
    public field mutable X: int32
    new(x: int32) = this { X = x }

struct MouseInfo =
    X: int32 set, get = 123
    Y: int32 set, get = 0
    RelativeX: int32 set, get = 0
    RelativeY: int32 set, get = 0

    Delta: Vector2 get() = Vector2(this.X)

class InputSnapshot =
    MouseInfo: MouseInfo get = MouseInfo()

Test(snapshot: InputSnapshot): int32 =
    snapshot.MouseInfo.X <- 456
    let v = snapshot.MouseInfo.Delta
    v.X

main(): () =
    let value = Test(InputSnapshot())
    print(value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Call getter of class property that returns a struct who has a field of a function``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct MouseInfo =
    field mutable X: () -> () = () -> print("passed")
    mutable Delta: () -> () get() = this.X

class InputSnapshot =
    MouseInfo: MouseInfo get = MouseInfo()

Test(snapshot: InputSnapshot): () =
    snapshot.MouseInfo.Delta()

main(): () =
    Test(InputSnapshot())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"
    |> ignore

[<Fact>]
let ``Get field of class that returns a struct``() =
    let src =
        """
module TestModule

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct Vector2 =
    public field mutable X: int32
    new(x: int32) = this { X = x }

struct MouseInfo =
    X: int32 set, get = 123
    Y: int32 set, get = 0
    RelativeX: int32 set, get = 0
    RelativeY: int32 set, get = 0

    Delta: Vector2 get() = Vector2(this.X)

class InputSnapshot =
    public field mutable MouseInfo: MouseInfo = MouseInfo()

Test(snapshot: InputSnapshot): int32 =
    snapshot.MouseInfo.X <- 456
    let v = snapshot.MouseInfo.Delta
    v.X

main(): () =
    let value = Test(InputSnapshot())
    print(value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Base constructor is called before setting rest of the fields``() =
    let src =
        """
#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

abstract class A =

    new() =
        print("A")
        this { }

class B =
    inherits A

    field X: int32

    new() =
        print("before ")
        base() {
            X =
                print(" after")
                1
        }

main(): () =
    let _ = B()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "before A after"
    |> ignore

[<Fact>]
let ``Static ctor evaluation order for fields``() =
    let src =
        """
#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module MD =

    field X: int32 = 5
    public field Y: int32 = X + 4

main(): () =
    print(MD.Y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Static ctor evaluation order for fields 2``() =
    let src =
        """
#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

module MD =

    public field Y: int32 = X + 4
    field X: int32 = 5

main(): () =
    print(MD.Y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"
    |> ignore

[<Fact>]
let ``Simple value binding of object type with an int32``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias obj

main(): () =
    let x: obj = 1234
    print(x)

#[intrinsic("print")]
print(__oly_object): ()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1234"
    |> ignore

[<Fact>]
let ``Implicit copy should occur``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =

    X: int32 get

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

M<T>(x: inref<T>): int32 where T: IA =
    x.SideEffect()
    x.SideEffect()
    x.X

main(): () =
    let s = S()
    let result = M(&s)
    print(result)
    print("_")
    print(s.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1_1"
    |> ignore

[<Fact>]
let ``Implicit copy should occur 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =

    X: int32 get

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

interface IA2 =

    S: S get

struct S2 =
    implements IA2

    S: S get, set = S()

M<T>(x: inref<T>): int32 where T: IA2 =
    x.S.SideEffect()
    x.S.SideEffect()
    x.S.X

main(): () =
    let mutable s = S2()
    let result = M(&s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1_1"
    |> ignore

[<Fact>]
let ``Implicit copy should occur 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =

    X: int32 get

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

interface IA2 =

    S: S get

struct S2 =
    implements IA2

    S: S get, set = S()

M<T>(x: byref<T>): int32 where T: IA2 =
    x.S.SideEffect()
    x.S.SideEffect()
    x.S.X

main(): () =
    let mutable s = S2()
    let result = M(&s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1_1"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

struct S =

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

M(mutable x: S): int32 =
    x.SideEffect()
    x.SideEffect()
    x.X

main(): () =
    let s = S()
    let result = M(s)
    print(result)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =

    X: int32 get

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

M(x: IA): int32 =
    x.SideEffect()
    x.SideEffect()
    x.X

main(): () =
    let s = S()
    let result = M(s)
    print(result)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =

    X: int32 get

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

M<T>(x: T): int32 where T: IA =
    x.SideEffect()
    x.SideEffect()
    x.X

main(): () =
    let s = S()
    let result = M(s)
    print(result)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "9"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 4``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =

    X: int32 get

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

M<T>(x: byref<T>): int32 where T: IA =
    x.SideEffect()
    x.SideEffect()
    x.X

main(): () =
    let mutable s = S()
    let result = M(&s)
    print(result)
    print("_")
    print(s.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "9_9"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 5``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =

    X: int32 get

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

interface IA2 =

    S: S get

struct S2 =
    implements IA2

    S: S get, set = S()

M<T>(x: T): int32 where T: IA2 =
    x.S.SideEffect()
    x.S.SideEffect()
    x.S.X

main(): () =
    let mutable s = S2()
    let result = M(s)
    print(result)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 6``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =

    X: int32 get

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

interface IA2 =

    S: S get

class S2 =
    implements IA2

    S: S get, set = S()

M<T>(x: T): int32 where T: IA2 =
    x.S.SideEffect()
    x.S.SideEffect()
    x.S.X

main(): () =
    let mutable s = S2()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1_1"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 7``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

struct S =

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

class S2 =

    S: S get, set = S()

M(x: S2): int32 =
    x.S.SideEffect()
    x.S.SideEffect()
    x.S.X

main(): () =
    let mutable s = S2()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1_1"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 8``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

struct S =

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

class S2 =

    public field mutable S: S = S()

M(x: S2): int32 =
    x.S.SideEffect()
    x.S.SideEffect()
    x.S.X

main(): () =
    let mutable s = S2()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "9_9"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 9``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

struct S =

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

class S2<T> =

    public field mutable S: T = unchecked default

M(x: S2<S>): int32 =
    x.S.SideEffect()
    x.S.SideEffect()
    x.S.X

main(): () =
    let mutable s = S2()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "8_8"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 10``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =
    
    X: int32 get, set

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

class S2<T> where T: IA =

    public field mutable S: T = unchecked default

    GetX(): int32 = this.S.X

    SetX(x: int32): () = this.S.X <- x

M(x: S2<S>): int32 =
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

M2(x: S2<S>): int32 =
    x.SetX(1)
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

main(): () =
    let mutable s = S2()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    let result = M2(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "8_89_9"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 11``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =
    
    X: int32 get, set

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

class S2<T> where T: IA =

    public field S: T = unchecked default

    GetX(): int32 = this.S.X

    SetX(x: int32): () = this.S.X <- x

M<T>(x: S2<T>): int32 where T: IA =
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

M2<T>(x: S2<T>): int32 where T: IA =
    x.SetX(1)
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

main(): () =
    let mutable s = S2<S>()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    let result = M2(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "8_89_9"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 12``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =
    
    X: int32 get, set

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

struct S2<T> where T: IA =

    public field S: T = unchecked default

    GetX(): int32 = this.S.X

    SetX(x: int32): () = this.S.X <- x

M<T>(x: S2<T>): int32 where T: IA =
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

M2<T>(x: S2<T>): int32 where T: IA =
    x.SetX(1)
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

main(): () =
    let mutable s = S2<S>()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    let result = M2(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "8_09_0"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 13``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =
    
    X: int32 get, set

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

struct S2<T> where T: IA =
    implements IA

    public field S: T = unchecked default

    X: int32
        get() = this.S.X
        set(value) = this.S.X <- value

    SideEffect(): () =
        this.S.SideEffect()

struct S3<T> where T: IA =

    public field S: T = unchecked default

    GetX(): int32 = this.S.X

    SetX(x: int32): () = this.S.X <- x

M<T>(x: S3<T>): int32 where T: IA =
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

M2<T>(x: S3<T>): int32 where T: IA =
    x.SetX(1)
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

main(): () =
    let mutable s = S3<S2<S>>()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    let result = M2(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "8_09_0"
    |> ignore

[<Fact>]
let ``Implicit copy should not occur 16``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("add")]
(+)(int32, int32): int32

interface IA =
    
    X: int32 get, set

    SideEffect(): ()

struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

struct S2<T> where T: IA =
    implements IA

    public field S: T = unchecked default

    X: int32
        get() = this.S.X
        set(value) = this.S.X <- value

    SideEffect(): () =
        this.S.SideEffect()

struct S3<T> where T: IA =

    public field S: S2<T> = unchecked default

    GetX(): int32 = this.S.S.X

    SetX(x: int32): () = this.S.S.X <- x

M<T>(x: S3<T>): int32 where T: IA =
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

M2<T>(x: S3<T>): int32 where T: IA =
    x.SetX(1)
    x.S.SideEffect()
    x.S.SideEffect()
    x.GetX()

main(): () =
    let mutable s = S3<S2<S>>()
    let result = M(s)
    print(result)
    print("_")
    print(s.S.X)
    let result = M2(s)
    print(result)
    print("_")
    print(s.S.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "8_09_0"
    |> ignore

[<Fact>]
let ``Scoped lambda 1``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

M(f: scoped int32 -> int32): int32 = f(50)

main(): () =
    let result = M(x -> x)
    print(result)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "50"
    |> ignore

[<Fact>]
let ``Scoped lambda 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

M(f: scoped int32 -> int32): int32 = f(50)

main(): () =
    let mutable y = 78
    let result = 
        M(x -> 
            y <- x
            x
        )
    print(y)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "50"
    |> ignore

[<Fact>]
let ``Scoped lambda 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

M(f: scoped int32 -> int32): int32 = f(50)

M2(f: scoped int32 -> int32): int32 =
    M(x -> f(x))

main(): () =
    let mutable y = 78
    let result = 
        M2(x -> 
            y <- x
            x
        )
    print(y)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "50"
    |> ignore

[<Fact>]
let ``ByRef should pass as it can be captured inside a scoped lambda``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

M(f: scoped () -> int32): int32 = f()

M2(x: byref<int32>): int32 =
    M(() -> x)

main(): () =
    let mutable y = 5
    let result = M2(&y)
    print(result)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``ByRef should pass as it can be captured inside a scoped lambda 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct S =

    public field mutable X: int32 = 0

    mutable Call(): () =
        this.X <- 5

M(f: scoped () -> ()): () = f()

M2(x: byref<S>): () =
    M(() -> x.Call())

main(): () =
    let mutable y = S()
    M2(&y)
    print(y.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``ByRef should pass as it can be captured inside a scoped lambda 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct S =

    public field mutable X: int32 = 0

    mutable Call(): () =
        this.X <- 5

M(f: scoped () -> ()): () = f()

M2(f: scoped () -> ()): () = M(f)

M3(x: byref<S>): () =
    M2(() -> x.Call())

main(): () =
    let mutable y = S()
    M3(&y)
    print(y.X)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Scoped lambda should close properly``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct S =

    public field mutable X: int32 = 0

M(f: scoped byref<S> -> ()): () = 
    let mutable s = S()
    f(&s)
    print(s.X)

M2(x: int32): () =
    M(s -> s.X <- x)

main(): () =
    M2(123)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Scoped lambda should close properly 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("uint32")]
alias uint32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct S =

    public field mutable X: int32 = 0
    public field mutable EntityId: EntityId = EntityId()

struct EntityId =

    public field mutable Index: int32 = 0
    public field mutable Version: uint32 = 0

M(entId: EntityId, f: scoped byref<S> -> ()): () = 
    let mutable s = S()
    f(&s)
    print(s.X)

M2(entId: EntityId): () =
    M(entId, 
        s -> 
            s.EntityId <- entId
            s.X <- 123
    )

main(): () =
    M2(EntityId())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Scoped lambda should close properly 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("uint32")]
alias uint32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct S =

    public field mutable X: int32 = 0
    public field mutable EntityId: EntityId = EntityId()

struct EntityId =

    public field mutable Index: int32 = 0
    public field mutable Version: uint32 = 0

M(entId: EntityId, f: scoped byref<S> -> ()): () = 
    let mutable s = S()
    f(&s)
    print(s.X)

M2(mutable entId: EntityId): () =
    M(entId, 
        s -> 
            s.EntityId <- entId
            s.X <- 123
    )

main(): () =
    M2(EntityId())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Scoped lambda should close properly 4``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("uint32")]
alias uint32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct S =

    public field mutable X: int32 = 0
    public field mutable EntityId: EntityId = EntityId()

struct EntityId =

    public field mutable Index: int32 = 0
    public field mutable Version: uint32 = 0

    #[inline(never)]
    Encode(): int32 = 7

M(entId: EntityId, f: scoped byref<S> -> ()): () = 
    let mutable s = S()
    f(&s)
    print(s.X)

M2(mutable entId: EntityId): () =
    M(entId, 
        s -> 
            s.EntityId <- entId
            s.X <- entId.Encode()
    )

main(): () =
    M2(EntityId())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Scoped lambda should close properly 5``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("uint32")]
alias uint32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

struct S =

    public field mutable X: int32 = 0
    public field mutable EntityId: EntityId = EntityId()

struct EntityId =

    public field mutable Index: int32 = 0
    public field mutable Version: uint32 = 0

    #[inline(never)]
    Encode(): int32 = 7

M(entId: EntityId, f: scoped byref<S> -> ()): () = 
    let mutable s = S()
    f(&s)
    print(s.X)

M2(mutable entId: EntityId): () =
    M(entId, 
        s -> 
            M(entId,
                s ->
                    s.EntityId <- entId
                    s.X <- entId.Encode()
            )
    )

main(): () =
    M2(EntityId())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "70"
    |> ignore

[<Fact>]
let ``Complex witness arrangement``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent

class EntityQuery<T> where T: unmanaged, trait IComponent

class EntityDatabase =

    CreateQuery<T>(): EntityQuery<T> where T: unmanaged, trait IComponent =
        let query = EntityQuery<T>()
        query

    M<T>(): () where T: unmanaged, trait IComponent =
        let _ = this.CreateQuery<T>()

struct S

#[open]
extension SComponent =
    inherits S
    implements IComponent

main(): () =
    let db = EntityDatabase()
    db.M<S>()
    print("worked")
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "worked"
    |> ignore

[<Fact>]
let ``Complex witness arrangement 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent<T> where T: unmanaged

class EntityQuery<T> where T: unmanaged, trait IComponent<T>

class EntityDatabase =

    CreateQuery<T>(): EntityQuery<T> where T: unmanaged, trait IComponent<T> =
        let query = EntityQuery<T>()
        query

    M<T>(): () where T: unmanaged, trait IComponent<T> =
        let _ = this.CreateQuery<T>()

struct S

#[open]
extension SComponent =
    inherits S
    implements IComponent<S>

main(): () =
    let db = EntityDatabase()
    db.M<S>()
    print("worked")
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "worked"
    |> ignore

[<Fact>]
let ``Complex witness arrangement 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent<T> where T: unmanaged

class EntityQuery<T> where T: unmanaged, trait IComponent<T>

class EntityDatabase =

    CreateQuery<T>(): EntityQuery<T> where T: unmanaged, trait IComponent<T> =
        let query = EntityQuery<T>()
        query

    M<T>(): () where T: unmanaged, trait IComponent<T> =
        let _ = this.CreateQuery<T>()

struct S<U>

#[open]
extension SComponent =
    inherits S<int32>
    implements IComponent<S<int32>>

main(): () =
    let db = EntityDatabase()
    db.M<S<int32>>()
    print("worked")
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "worked"
    |> ignore

[<Fact>]
let ``Complex witness arrangement 4``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent<T> where T: unmanaged =

    Process<U>(): ()

class EntityQuery<T> where T: unmanaged, trait IComponent<T> =

    Execute(s: T): () = s.Process<int32>()

class EntityDatabase =

    CreateQuery<T>(): EntityQuery<T> where T: unmanaged, trait IComponent<T> =
        let query = EntityQuery<T>()
        query

    M<T>(s: T): () where T: unmanaged, trait IComponent<T> =
        let query = this.CreateQuery<T>()
        query.Execute(s)

struct S<U>

#[open]
extension SComponent =
    inherits S<int32>
    implements IComponent<S<int32>>

    Process<U>(): () = print("worked")

main(): () =
    let db = EntityDatabase()
    db.M<S<int32>>(S())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "worked"
    |> ignore

[<Fact>]
let ``Scoped lambda capturing a normal lambda``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("add")]
(+)(int32, int32): int32

#[unmanaged(allocation_only)]
#[intrinsic("less_than")]
(<)(int32, int32): bool

#[inline]
For(count: int32, #[inline] f: scoped int32 -> ()): () =
    let mutable i = 0
    while (i < count)
        f(i)
        i <- i + 1

M(g: int32 -> ()): () =
    let a() =
        For(1, 
            x -> 
                let b() = g(x)
                b()
        )
    a()

main(): () =
    M(x -> print(x))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Scoped lambda capturing a normal lambda 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("add")]
(+)(int32, int32): int32

#[unmanaged(allocation_only)]
#[intrinsic("less_than")]
(<)(int32, int32): bool

#[inline]
For(count: int32, #[inline] f: scoped int32 -> ()): () =
    let mutable i = 0
    while (i < count)
        f(i)
        i <- i + 1

M(g: int32 -> ()): () =
    let mutable a =
        () ->
            For(1, 
                x -> 
                    let mutable b =
                        () ->
                            g(x)
                    b()
            )
    a()

main(): () =
    M(x -> print(x))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Pass function types from closures``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class A =

    public field X: int32 -> ()
    public field Y: (int32, int32) -> ()

    new(x: int32 -> (), y: (int32, int32) -> ()) =
        this {
            X = x
            Y = y
        }

class B =

    public field A: A

    new() =
        let f(x: int32) = print(x)
        let g(x: int32, /* inference */ y) =
            print(x)
        this {
            A = A(f, (x: int32, y: int32) -> g(x, y))
        }

main(): () =
    let b = B()
    let a = b.A

    let f = a.X
    let g = a.Y

    f(1)
    g(2, 3)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "12"
    |> ignore

[<Fact>]
let ``Generic class with a static field``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class Option<T> =
    Value: T get = unchecked default

    static None: Option<T> get = Option()

main(): () =
    let x = Option<int32>.None
    print(x.Value)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Newtype should work with static auto-property backed by a static field``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[open]
newtype Option<T> =
    public field Value: T

    static Some(value: T): Option<T> = Option(value)

    static None: Option<T> get = unchecked default: Option<T>

main(): () =
    let x = Option<int32>.None
    print(x.Value)
        """
    src
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
    |> ignore

[<Fact>]
let ``Override static function within an interface``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent =

    static abstract GetValue(): int32

interface IComponent<N, T> where N: constant int32 where T: unmanaged =
    inherits IComponent

    static overrides GetValue(): int32 = N

class A =
    implements IComponent<7, int32>

M<N, T>(): () where T: IComponent<N, int32> where N: constant int32 =
    print(T.GetValue())

main(): () =
    M<7, A>()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Override static function within an interface 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent =

    static abstract GetValue(): int32

interface IComponent<N, T> where N: constant int32 where T: unmanaged =
    inherits IComponent

    static overrides GetValue(): int32 = N

class A

#[open]
extension AExtension =
    inherits A
    implements IComponent<7, int32>

M<N, T>(): () where T: trait IComponent<N, int32> where N: constant int32 =
    print(T.GetValue())

main(): () =
    M<7, A>()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Override static function within an interface 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent =

    static abstract GetValue(): int32

interface IComponent<N, T> where N: constant int32 where T: unmanaged =
    inherits IComponent

    static overrides GetValue(): int32 = N

class A =
    implements IComponent<7, int32>

M<N, T>(): () where T: IComponent =
    print(T.GetValue())

main(): () =
    M<7, A>()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Override static function within an interface 4``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent =

    static abstract GetValue(): int32

interface IComponent<N, T> where N: constant int32 where T: unmanaged =
    inherits IComponent

    static overrides GetValue(): int32 = N

class A

#[open]
extension AExtension =
    inherits A
    implements IComponent<7, int32>

M<N, T>(): () where T: trait IComponent =
    print(T.GetValue())

main(): () =
    M<7, A>()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Override function within an interface``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent =

    GetValue(): int32

interface IComponent<N, T> where N: constant int32 where T: unmanaged =
    inherits IComponent

    overrides GetValue(): int32 = N

class A =
    implements IComponent<7, int32>

M<N, T>(a: T): () where T: IComponent<N, int32> where N: constant int32 =
    print(a.GetValue())

main(): () =
    M<7, A>(A())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent

#[open]
extension Int32Component =
    inherits int32
    implements IComponent

field mutable F: () -> () = unchecked default

#[inline(never)]
M2<T>(): () = print("asdf")

#[inline(never)]
M<T>(): () where T: trait IComponent =
    let f() =
        M2<T>()
    f()

main(): () =
    M<int32>()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "asdf"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent

#[open]
extension Int32Component =
    inherits int32
    implements IComponent

field mutable F: () -> () = unchecked default

#[inline(never)]
M2<T>(): () = print("asdf")

#[inline(never)]
M<T>(): () where T: trait IComponent =
    let f() =
        M2<T>()
    F <- f

main(): () =
    M<int32>()
    F()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "asdf"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 2-1``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent

#[open]
extension Int32Component =
    inherits int32
    implements IComponent

field mutable F: () -> () = unchecked default

#[inline(never)]
M2<T>(): () = print("asdf")

#[inline(never)]
M<T>(): () where T: trait IComponent =
    F <- () -> M2<T>()

main(): () =
    M<int32>()
    F()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "asdf"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 2-2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent

#[open]
extension Int32Component =
    inherits int32
    implements IComponent

field mutable F: () -> () = unchecked default

#[inline(never)]
M2<T>(): () where T: trait IComponent = print("asdf")

#[inline(never)]
M<T>(): () where T: trait IComponent =
    F <- () -> M2<T>()

main(): () =
    M<int32>()
    F()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "asdf"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 2-3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent =

    static abstract default Print(): () = print("asdf")

#[open]
extension Int32Component =
    inherits int32
    implements IComponent

field mutable F: () -> () = unchecked default

#[inline(never)]
M2<T>(): () where T: trait IComponent = T.Print()

#[inline(never)]
M<T>(): () where T: trait IComponent =
    F <- () -> M2<T>()

main(): () =
    M<int32>()
    F()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "asdf"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct

interface IComponent

#[open]
extension TestStructComponent =
    inherits TestStruct
    implements IComponent

M2<T>(f: scoped byref<T> -> ()): () where T: trait IComponent =
    let mutable x = unchecked default
    f(&x)

#[inline(never)]
M(): () =
    M2<TestStruct>(
        refTestStruct ->
            print("asdf")
    )

main(): () =
    M()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "asdf"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 4``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct

interface IComponent

#[open]
extension TestStructComponent =
    inherits TestStruct
    implements IComponent

M2<T>(f: scoped byref<T> -> ()): () where T: trait IComponent =
    let mutable x = unchecked default
    f(&x)

#[inline(never)]
M(): () =
    M2<TestStruct>(
        refTestStruct ->
            M2<TestStruct>(
                refTestStruct2 ->
                    refTestStruct2 <- refTestStruct
                    print("asdf")
            )
    )

main(): () =
    M()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "asdf"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 5``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct =
    public field mutable X: int32 = 0

struct TestStruct2 =
    public field mutable Y: int32 = 0

interface IComponent

#[open]
extension TestStructComponent =
    inherits TestStruct
    implements IComponent

#[open]
extension TestStruct2Component =
    inherits TestStruct2
    implements IComponent

M2<T>(f: scoped byref<T> -> ()): () where T: trait IComponent =
    let mutable x = unchecked default
    f(&x)

#[inline(never)]
M(): () =
    M2<TestStruct>(
        refTestStruct ->
            refTestStruct.X <- 456
            M2<TestStruct2>(
                refTestStruct2 ->
                    refTestStruct2.Y <- refTestStruct.X
                    print(refTestStruct2.Y)
            )
    )

main(): () =
    M()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 6``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct =
    public field mutable X: int32 = 0

struct TestStruct2 =
    public field mutable Y: int32 = 0

interface IComponent

#[open]
extension TestStructComponent =
    inherits TestStruct
    implements IComponent

#[open]
extension TestStruct2Component =
    inherits TestStruct2
    implements IComponent

struct TestG<T> =
    public field mutable G: T = unchecked default

M3<T>(f: scoped byref<TestG<T>> -> ()): () where T: trait IComponent =
    let mutable x = unchecked default
    f(&x)

M2<T>(f: scoped byref<T> -> ()): () where T: trait IComponent =
    M3<T>(
        g ->
            f(&g.G)   
    )

#[inline(never)]
M(): () =
    M2<TestStruct>(
        refTestStruct ->
            refTestStruct.X <- 456
            M2<TestStruct2>(
                refTestStruct2 ->
                    refTestStruct2.Y <- refTestStruct.X
                    print(refTestStruct2.Y)
            )
    )

main(): () =
    M()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Lambda captures type parameter and witness 6 - except its without the witness via no IComponent constraint``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct TestStruct =
    public field mutable X: int32 = 0

struct TestStruct2 =
    public field mutable Y: int32 = 0

interface IComponent

#[open]
extension TestStructComponent =
    inherits TestStruct
    implements IComponent

#[open]
extension TestStruct2Component =
    inherits TestStruct2
    implements IComponent

struct TestG<T> =
    public field mutable G: T = unchecked default

M3<T>(f: scoped byref<TestG<T>> -> ()): () =
    let mutable x = unchecked default
    f(&x)

M2<T>(f: scoped byref<T> -> ()): () =
    M3<T>(
        g ->
            f(&g.G)   
    )

#[inline(never)]
M(): () =
    M2<TestStruct>(
        refTestStruct ->
            refTestStruct.X <- 456
            M2<TestStruct2>(
                refTestStruct2 ->
                    refTestStruct2.Y <- refTestStruct.X
                    print(refTestStruct2.Y)
            )
    )

main(): () =
    M()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

[<Fact>]
let ``Scoped closure should work when captured by a normal closure and mutating a local outside the scope``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

M(f: scoped () -> ()): () = f()

main(): () =
    let mutable x = 0

    let f() =
        M(() -> x <- 5)
        
    f()
    print(x)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "5"
    |> ignore

[<Fact>]
let ``Property as a function type alias``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

alias PropFunc = (int32, int32, int32) -> () 

class C =

    F: PropFunc
        get = 
            (x: int32, y: int32, z: int32) -> 
                print(x)
                print(y)
                print(z)
                print("hello")

main(): () =
    let c = C()
    c.F(1, 2, 3)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123hello"
    |> ignore

[<Fact>]
let ``Mutable value should be mutated in a match case that is in a closure``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

pattern P(x: int32): () when (x == 5) =>
    ()

M(f: () -> ()): () =
    f()

main(): () =
    let x = 5

    let mutable abc = 0

    M(
        () ->
            match (x)
            | P =>
                abc <- 13
            | _ =>
                ()
    )

    print(abc)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "13"

[<Fact>]
let ``Array of tuples in a ForEach loop funcion should work``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

ForEach<T>(xs: T[], f: T -> ()): () =
    print("hello")

main(): () =
    let xs = [(1, 2)]
    ForEach(xs, x -> ())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Array of tuples in a ForEach loop funcion should work 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("add")]
(+)(int32, int32): int32

#[unmanaged(allocation_only)]
#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

#[inline]
For(count: int32, #[inline] f: scoped int32 -> ()): () =
    let mutable i = 0
    while (i < count)
        f(i)
        i <- i + 1

#[intrinsic("get_length")]
private getLength<T>(T[]): int32

ForEach<T>(xs: T[], f: (T, int32) -> ()): () =
    For(getLength(xs), i -> f(xs[i], 5))

main(): () =
    let xs = [(1, 2)]
    ForEach(xs, 
        ((x, y), _) ->
            print(x)
            print(y)
    )
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "12"

[<Fact>]
let ``Array of tuples in a ForEach loop funcion should work 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("add")]
(+)(int32, int32): int32

#[unmanaged(allocation_only)]
#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

#[inline]
For(count: int32, #[inline] f: scoped int32 -> ()): () =
    let mutable i = 0
    while (i < count)
        f(i)
        i <- i + 1

#[intrinsic("get_length")]
private getLength<T>(T[]): int32

ForEach<T>(xs: T[], f: T -> ()): () =
    For(getLength(xs), i -> f(xs[i]))

main(): () =
    let xs = [((1, 2), (3, 4))]
    ForEach(xs, 
        (((x, y), (z, w))) ->
            print(x)
            print(y)
            print(z)
            print(w)
    )
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1234"

[<Fact>]
let ``Array of tuples in a ForEach loop funcion should work 4``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("add")]
(+)(int32, int32): int32

#[unmanaged(allocation_only)]
#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

#[inline]
For(count: int32, #[inline] f: scoped int32 -> ()): () =
    let mutable i = 0
    while (i < count)
        f(i)
        i <- i + 1

#[intrinsic("get_length")]
private getLength<T>(T[]): int32

ForEach<T>(xs: T[], f: T -> ()): () =
    For(getLength(xs), i -> f(xs[i]))

main(): () =
    let xs = [(1, (2, 3))]
    ForEach(xs, 
        ((x, (y, z))) ->
            print(x)
            print(y)
            print(z)
    )
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Partial application unit to unit regression``() =
    """
#[intrinsic("print")]
print(__oly_object): ()

class A =

    Test(): () = print("hello")

class B =

    Add(f: () -> ()): () =
        f()

main(): () =
    let a = A()
    let b = B()
    b.Add(a.Test)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Partial application unit to unit regression 2``() =
    """
#[intrinsic("print")]
print(__oly_object): ()

class A =

    Test(): () = print("hello")

    Test(x: __oly_object): () = print("wrong")

class B =

    Add(f: () -> ()): () =
        f()

main(): () =
    let a = A()
    let b = B()
    b.Add(a.Test)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Partial application unit to unit regression 3``() =
    """
#[intrinsic("print")]
print(__oly_object): ()

class A =

    Test(): () = print("hello")

    Test(x: ()): () = print("wrong")

class B =

    Add(f: () -> ()): () =
        f()

main(): () =
    let a = A()
    let b = B()
    b.Add(a.Test)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Partial application unit to unit regression 4``() =
    """
#[intrinsic("print")]
print(__oly_object): ()

class A =

    Test(): () = print("hello")

    Test(x: ()): () = print("wrong")

    Add(f: () -> ()): () =
        f()

    Call(): () =
        this.Add(this.Test)

main(): () =
    let a = A()
    a.Call()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Partial application unit to unit regression 5``() =
    """
#[intrinsic("print")]
print(__oly_object): ()

M<T>(z: T, f: T -> ()): () =
    f(z)

Test(x: ()): () = print("hello")

main(): () =
    M((), Test)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Partial application unit to unit regression 6``() =
    """
#[intrinsic("print")]
print(__oly_object): ()

M<T>(z: T, f: T -> ()): () =
    f(z)

main(): () =
    M((), x -> print("hello"))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Partial application unit to unit regression 7``() =
    """
#[intrinsic("print")]
print(__oly_object): ()

M<T>(z: T, f: T -> ()): () =
    f(z)

main(): () =
    M((), (x: ()) -> print("hello"))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Regression - should infer 15 as uint32``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("uint32")]
alias uint32

#[intrinsic("print")]
print(__oly_object): ()

get_uint32(): uint32 = 25

call_indirect_uint32: (uint32, uint32) -> () get = (x, y) -> print(y)

main(): () =
    call_indirect_uint32(get_uint32(), 15)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "15"

[<Fact>]
let ``Regression - should infer 15 as uint32 - 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("uint32")]
alias uint32

#[intrinsic("print")]
print(__oly_object): ()

get_uint32(): uint32 = 25

call_direct_uint32(x: uint32, y: uint32): () = print(y)

main(): () =
    call_direct_uint32(get_uint32(), 15)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "15"

[<Fact>]
let ``Native int should work on most platforms``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("native_int")]
alias nint

#[intrinsic("unsafe_cast")]
nint(int32): nint

#[intrinsic("unsafe_cast")]
int32(nint): int32

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    print(nint(123))
    print(int32(nint(456)))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123456"

[<Fact>]
let ``Native uint should work on most platforms``() =
    """
#[intrinsic("uint32")]
alias uint32

#[intrinsic("native_int")]
alias nuint

#[intrinsic("unsafe_cast")]
nuint(uint32): nuint

#[intrinsic("unsafe_cast")]
uint32(nuint): uint32

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    print(nuint(123))
    print(uint32(nuint(456)))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123456"

[<Fact>]
let ``Tuple uses an interface and we try to pass a concrete type``() =
    """
interface IA =

    X: __oly_int32 get

class A =
    implements IA

    X: __oly_int32 get = 5

#[intrinsic("print")]
print(__oly_object): ()

getTuple(): (IA, __oly_int32) =
    (A() : IA, 9) // 'A' is the concrete type

main(): () =
    let (a, v) = getTuple()
    print(a.X)
    print(v)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "59"

[<Fact>]
let ``Regression - closure and pattern should succeed``() =
    let src =
        """
#[intrinsic("unsafe_cast")]
unsafeCast<T>(__oly_object): T

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("utf16")]
alias string

#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32) : bool

abstract class Command =
    Tag: int32 get
    Name: string get
    new(tag: int32, name: string) = this { Tag = tag; Name = name }

module FieldCommand<T> =

    interface IFieldCommand =
        Getter: () -> T get
        Setter: T -> () get

    class Impl =
        inherits Command
        implements IFieldCommand
        Getter: () -> T get
        Setter: T -> () get
        new(name: string, getter: () -> T, setter: T -> ()) = base(10000, name) { Getter = getter; Setter = setter }

    pattern FieldCommand(cmd: Command): (getter: () -> T, setter: T -> ()) when (cmd.Tag == 10000) =>
        let cmd = unsafeCast<IFieldCommand>(cmd)
        (cmd.Getter, cmd.Setter)

class FunctionCommand =
    inherits Command
    Action: () -> () get
    new(name: string, action: () -> ()) = base(0, name) { Action = action }

pattern FunctionCommand(cmd: Command): (() -> ()) when (cmd.Tag == 0) =>
    let cmd = unsafeCast<FunctionCommand>(cmd)
    cmd.Action

init(f: (scoped () -> ()) -> ()): () =
    let cmd: Command = FieldCommand<bool>.Impl("test", () -> true, x -> f(() -> print("hello")))

    match (cmd)
    | FunctionCommand(action) =>
        action()
    | FieldCommand<bool>.FieldCommand(getter, setter) =>
        setter(false)
    | _ =>
        ()

main(): () =
    init(f -> f())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Newtype - take address of principal field should succeed``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

struct S =
    public field mutable X: int32 = 1

class C =
    public field mutable X: int32 = 5

newtype NewtypeS =
    public field mutable S: S

newtype NewtypeC =
    public field C: C

main(): () =
    let mutable ns = NewtypeS(S())
    print(ns.S.X)
    ns.S.X <- 9
    print(ns.S.X)

    let nc = NewtypeC(C())
    print(nc.C.X)
    nc.C.X <- 3
    print(nc.C.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1953"

[<Fact>]
let ``Newtype - take address of principal field should succeed 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct S =
    public field mutable X: int32 = 1

newtype NewtypeS =
    public field mutable S: S

main(): () =
    let mutable ns = NewtypeS(S())
    let x = &ns.S.X

    x <- 7

    print(ns.S.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "7"

[<Fact>]
let ``Newtype - take address of principal field should succeed 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct S =
    public field mutable X: int32 = 1

newtype NewtypeS =
    public field mutable S: S

main(): () =
    let mutable ns = NewtypeS(S())
    let x = &ns.S

    let mutable s = S()
    s.X <- 8
    x <- s

    print(ns.S.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "8"

[<Fact>]
let ``Newtype - take address of principal field should succeed 4``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

class S =
    public field mutable X: int32 = 1

newtype NewtypeS =
    public field S: S

main(): () =
    let mutable ns = NewtypeS(S())
    let x = &ns.S.X

    x <- 7

    print(ns.S.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "7"

[<Fact>]
let ``Struct value will not mutate from interface constraint for inref``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

interface IA =
    X: int32 get, set

struct S =
    implements IA

    X: int32 get, set = 0

M<T>(s: inref<T>): () where T: IA =
    s.X <- 99

main(): () =
    let s = S()
    print(s.X)
    M(&s)
    print(s.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "00"

[<Fact>]
let ``Struct value will not mutate from shape constraint for inref``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct S =
    field mutable x: int32 = 0
    GetX(): int32 = this.x
    mutable SetX(x: int32): () = 
        this.x <- x

M<T>(s: inref<T>): () where T: { SetX(int32): () } =
    s.SetX(99)

main(): () =
    let s = S()
    print(s.GetX())
    M(&s)
    print(s.GetX())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "00"

[<Fact>]
let ``Able to mutate value field of a newtype``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

newtype NewInt =
    public field mutable Value: int32

M(x: byref<NewInt>): () =
    x.Value <- 5

main(): () =
    let mutable ni = NewInt(1)
    print(ni)
    M(&ni)
    print(ni)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "15"

[<Fact>]
let ``Able to mutate value field of a newtype 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct S =
    public field mutable N: int32 = 7

newtype NewS =
    public field mutable Value: S

M(x: byref<NewS>): () =
    x.Value.N <- 2

M2(x: byref<NewS>): () =
    x.Value <- S()

M3(x: byref<NewS>, y: byref<S>): () =
    x.Value <- y

main(): () =
    let mutable ns = NewS(S())
    print(ns.Value.N)
    M(&ns)
    print(ns.Value.N)
    M2(&ns)
    print(ns.Value.N)

    let mutable s = S()
    s.N <- 9

    M3(&ns, &s)

    print(ns.Value.N)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "7279"

[<Fact>]
let ``Able to mutate value field of a newtype 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

class S =
    public field mutable N: int32 = 7

newtype NewS =
    public field Value: S

M(x: NewS): () =
    x.Value.N <- 2

main(): () =
    let ns = NewS(S())
    print(ns.Value.N)
    M(ns)
    print(ns.Value.N)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "72"

[<Fact>]
let ``Able to mutate value field of a newtype 4``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

class S =
    public field mutable N: int32 = 7

newtype NewS =
    public field Value: S

M(x: inref<NewS>): () =
    x.Value.N <- 2

main(): () =
    let ns = NewS(S())
    print(ns.Value.N)
    M(&ns)
    print(ns.Value.N)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "72"

[<Fact>]
let ``Newtype should work with a shape trait constraint``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

newtype NewInt =
    public field Value: int32

    Doot(): int32 = 8

M<T>(x: T): () where T: trait { Doot(): int32 } =
    print(x.Doot())

main(): () =
    let ns = NewInt(2)
    M(ns)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "8"

[<Fact>]
let ``Outref should pass``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("by_ref_write_only")]
alias outref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(outValue: outref<int32>): () =
    outValue <- 4

main(): () =
    let mutable x = 1
    M(&x)
    print(x)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "4"

[<Fact>]
let ``Anonymous shape constraint with members that have generics with constraints``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class C =

    GetX<U>(x: U): U where U: struct = x

M<T>(x: T): int32 where T: { GetX<U>(U): U where U: struct } =
    x.GetX(5)

main(): () =
    let c = C()
    print(M(c))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "5"

[<Fact>]
let ``Constraint { new() } and T() should work for struct``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IExample =

    M<T>(): T where T: { new() }

class Example =
    implements IExample

    M<T>(): T where T: { new() } = T()

struct S =

    X: int32 get = 123

main(): () =
    let example = Example()
    let s = example.M<S>()
    print(s.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Constraint { new() } and T() should work for struct 2 - no parameterless constructor``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IExample =

    M<T>(): T where T: { new() }

class Example =
    implements IExample

    M<T>(): T where T: { new() } = T()

struct S =

    new(x: int32) = this { X = x }
    X: int32 get

main(): () =
    let example = Example()
    let s = example.M<S>()
    print(s.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"

[<Fact>]
let ``Constraint { new() } and T() should work for class``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IExample =

    M<T>(): T where T: { new() }

class Example =
    implements IExample

    M<T>(): T where T: { new() } = T()

class C =

    X: int32 get = 123

main(): () =
    let example = Example()
    let c = example.M<C>()
    print(c.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Constraint { new() } and T() should work for class 2 - has a static constructor``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IExample =

    M<T>(): T where T: { new() }

class Example =
    implements IExample

    M<T>(): T where T: { new() } = T()

class C =

    static Y: int32 get = 
        print("static")
        456

    X: int32 get = 123

main(): () =
    let example = Example()
    let c = example.M<C>()
    print(c.X)
    let _ = C.Y
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123static"

[<Fact>]
let ``Constraint { new() } and T() should work for class 3 - has a static constructor``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IExample =

    M<T>(): T where T: { new() }

class Example =
    implements IExample

    M<T>(): T where T: { new() } = T()

class C =

    static Y: int32 get = 
        print("static")
        456

    X: int32 get = 123

main(): () =
    let example = Example()
    let c = example.M<C>()
    let _ = C.Y
    print(c.X)
    let _ = C.Y
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "static123"

[<Fact>]
let ``Higher-kind with byref types``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("address_of")]
(&)<T>(T): __oly_by_ref_read_only<T>

#[intrinsic("address_of")]
(&)<T>(T): __oly_by_ref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
M<Ref1<_>, T1>(#[inline] f: scoped (int32, Ref1<T1>) -> ()): () where Ref1: scoped =
    f(0, unchecked default)

main(): () =
    M<_, int32>((_, x: __oly_by_ref_read_only<int32>) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Higher-kind with byref types 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
M<Ref1<_>, T1>(#[inline] f: scoped (int32, Ref1<T1>) -> ()): () where Ref1: scoped =
    f(0, unchecked default)

main(): () =
    M<_, int32>((_, x: inref<int32>) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Higher-kind with tuple type``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
M<Ref1<_>, T...>(#[inline] f: (Ref1<T...>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: __oly_tuple<(int32, float32)>) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Higher-kind with tuple type 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
M<Ref1<_, _>, T...>(#[inline] f: (Ref1<int32, T...>) -> ()): () =
    f(unchecked default)

class C<R, T...>

main(): () =
    M<_, (int32, float32)>((x: C<int32, __oly_tuple<(int32, float32)>>) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"


[<Fact>]
let ``Higher-kind with tuple type 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
M<Ref1<_>, T...>(#[inline] f: (Ref1<T...>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: (int32, float32)) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Higher-kind with tuple type 4``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
M<Ref1<_>, T...>(#[inline] f: (Ref1<T...>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, int32, float32>((x: (int32, float32)) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Higher-kind with tuple type 5``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
M<Ref1<_>, T...>(#[inline] f: (Ref1<T...>) -> ()): () where Ref1<_>: __oly_tuple =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: __oly_tuple<(int32, float32)>) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Higher-kind with tuple type 6``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

abstract class C<T...>

#[inline]
M<Ref1<_>, T>(#[inline] f: Ref1<T> -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: C<(int32, float32)>) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Higher-kind with tuple type 7``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

abstract class C<T>

#[inline]
M<Ref1<_>, T>(#[inline] f: Ref1<T> -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: C<(int32, float32)>) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Higher-kind against class with variadic type parameter``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("print")]
print(__oly_object): ()

class C<T...>

#[inline]
M<Ref1<_>, T...>(#[inline] f: (Ref1<T...>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: C<(int32, float32)>) -> ())
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Constructor immutable field assignment should pass with 'let' expression``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

M(x: inref<S>): () =
    print(x.X)

struct S =

    public field mutable X: int32 = 0

class C =

    field s: S

    new() =
        this {
            s =
                let mutable s = S()
                s.X <- 123
                s
        }

    Call(): () =
        M(&this.s)

main(): () =
    let c = C()
    c.Call()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Able to have constructor overloads when combined from other files``() =
    let src1 =
        """
namespace Oly.Test

#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias string

class C<T1> =
    new(x: T1) = this { }

    static Create(): C<T1> = C(unchecked default)

module Helpers =

    #[intrinsic("print")]
    print(__oly_object): ()
        """

    let src2 =
        """
namespace Oly.Test

class C<T1, T2> =
    new(x: T1) = this { }
    new(x: T1, y: T2) = this { }

    static Create(): C<T1, T2> = C(unchecked default)
        """

    let src =
        """
open Oly.Test
open static Oly.Test.Helpers

module M =
    main(): () =
        let c = C<int32>(1)
        let c2 = C(1, "hello")
        print(123)
        """

    OlyThree src src1 src2
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``'new' should work on the concrete implementation``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IExample =

    M(): int32

abstract class BaseExample =

    M(): int32 = 123

class Example =
    inherits BaseExample
    implements IExample

    new M(): int32 = 456

main(): () =
    let example: IExample = Example()
    print(example.M())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
