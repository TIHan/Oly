module MiscTests

open Xunit
open TestUtilities
open Oly.Compiler.Compilation

[<Fact>]
let ``Value has nested values`` () =
    let src = """
f() : () =
    let x: () =
        let y: __oly_int32 = 1
              """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Value implements inference has nested values`` () =
    let src = """
f() : () =
    let x =
        let y: __oly_int32 = 1
              """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``No input`` () =
    let src = """f() : __oly_int32 = 1"""
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Simple application`` () =
    let src = """
f() : __oly_int32 = 1

z() : () =
    let x: __oly_int32 = f()
    let g: () -> __oly_int32 = f
              """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Simple interface implementation and infix operator +`` () =
    let src = 
        """
open extension Int32AddExtension

interface Add<T> =

    static abstract add(value1: T, value2: T) : T

(+)<T>(value1: T, value2: T) : T where T : trait Add<T> =
    T.add(value1, value2)

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(value1: __oly_int32, value2: __oly_int32) : __oly_int32 =
        __oly_add(value1, value2)

f() : () =
    let x = 1 
             + 
             1
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Basic new type`` () =
    let src = """
#[intrinsic("int32")]
alias int32

class Test =
    field value: int32 = 0

test(x: __oly_int32) : () = ()
test2(x: int32) : () =
    test(x)
              """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Generic new type`` () =
    let src = """
class Test<T> =
    field value: T

    new(x: T) = { value = x }

f() : () =
    let x = Test<_>(1)
              """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Example 1 compiles`` () =
    let src = """
open extension Int32AddExtension
open extension Utf16AddExtension

#[import("PLATFORM", "", "IntrinsicStringConcat")]
IntrinsicStringConcat(value1: __oly_utf16, value2: __oly_utf16) : __oly_utf16

#[import("PLATFORM", "", "IntrinsicPrint")]
IntrinsicPrint<T>(value: T) : ()

interface Add<T> =

    static abstract add(value1: T, value2: T) : (T)

(+)<T>(value1: T, value2: T) : (T) where T : trait Add<T> =
    T.add(value1, value2)

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(value1: __oly_int32, value2: __oly_int32) : __oly_int32 =
        __oly_add(value1, value2)

extension Utf16AddExtension =
    inherits __oly_utf16
    implements Add<__oly_utf16>

    static overrides add(value1: __oly_utf16, value2: __oly_utf16) : __oly_utf16 =
        IntrinsicStringConcat(value1, value2)

f() : () =
    let x = 1 + 1
    let y = "hello " + "world" + "zooot"
    IntrinsicPrint<_>(x)
    IntrinsicPrint<_>(y)

g() : () =
    let z = "beef" + "beef"

    IntrinsicPrint<_>(z)
              """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Type implements interfaces compiles`` () =
    let src =
        """
#[import("PLATFORM", "", "IntrinsicPrint")]
IntrinsicPrint<T>(value: T) : ()

class Test =
    field value: __oly_int32

    new(value: __oly_int32) = { value = value }

interface Add<T> =

    static abstract add(value1: T, value2: T) : (T)

(+)<T>(value1: T, value2: T) : (T) where T : Add<T> =
    T.add(value1, value2)

extension TestAddExtension =
    inherits Test
    implements Add<Test>

    static overrides add(value1: Test, value2: Test) : Test = Test(500)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Get a function reference`` () =
    let src =
        """
#[import("PLATFORM", "", "IntrinsicPrint")]
IntrinsicPrint<T>(value: T) : ()

f(doot: __oly_int32) : (__oly_int32) = doot

z() : () =
    let g: (__oly_int32) -> __oly_int32 = f

    let y = g(500)
    IntrinsicPrint<_>(y)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Example 2`` () =
    let src =
        """
open extension Int32AddExtension

#[import("PLATFORM", "", "IntrinsicPrint")]
IntrinsicPrint<T>(value: T) : ()

interface Add<T> =

   static abstract add(value1: T, value2: T) : T

(+)<T>(value1: T, value2: T) : (T) where T : Add<T> =
   T.add(value1, value2)

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, value2: __oly_int32) : (__oly_int32) =
        __oly_add(x, value2)

class Doot =
    field x: __oly_int32 = 0
    field y: __oly_int32 = 0

f() : () =
    IntrinsicPrint<_>(2)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Example 3``() =
    let src =
        """
open extension Int32AddInt32Extension
open extension Int32AddFloat64Extension

#[import("CLR", "System", "Console")]
class Console =

    static WriteLine<T>(value: T): ()

interface Add<T> =

   static abstract add(value1: T, value2: T): T

(+)<T>(value1: T, value2: T): T where T: trait Add<T> =
   T.add(value1, value2)

extension Int32AddInt32Extension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, value2: __oly_int32): __oly_int32 =
      __oly_add(x, value2)

extension Int32AddFloat64Extension =
    inherits __oly_int32
    implements Add<__oly_float64>

    static overrides add(x: __oly_float64, value2: __oly_float64): __oly_float64 =
      2.0

f<T>(x: T, y: T): T where T: trait Add<T> = 
   class X<U> =
       public field x: T
       field y: U

       new(x: T, y: U) = { x = x; y = y }
   let doot = X<__oly_float64>(x, 9.0)
   doot.x + y

test<T>(x: T, y: T): T where T: trait Add<T> =
   T.add(x, y)

main (): () =
    Console.WriteLine<_>(f<_>(40, 9))
    Console.WriteLine<_>(test<_>(1, 2))

    extension Int32AddFloat64Extension2 =
        inherits __oly_int32
        implements Add<__oly_float64>

        static overrides add(x: __oly_float64, value2: __oly_float64): __oly_float64 =
            10.0

    Console.WriteLine<_>(test<_>(1, 2))
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Example 4``() =
    let src =
        """
open extension MaybeMonadExtension<_>
open extension Int32AddInt32Extension

#[intrinsic("int32")]
alias int32

extension Int32AddInt32Extension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, value2: __oly_int32): __oly_int32 =
      __oly_add(x, value2)

#[import("CLR", "System", "Console")]
class Console =

    static WriteLine<T>(value: T): ()

interface Add<T> =

   static abstract add(x: T, y: T): T

(+)<T>(x: T, y: T): T where T: trait Add<T> = T.add(x, y)

interface Functor<F<_>> =

   static abstract map<A, B>(fa: F<A>, f: A -> B): F<B>

interface Monad<M<_>> =

   static abstract Bind<A, B>(ma: M<A>, f: A -> M<B>): M<B>

   static abstract Return<A>(a: A): M<A>

class Maybe<T> =
    public field value: T

    new(value: T) = { value = value }

extension MaybeMonadExtension<T> =
    inherits Maybe<T>
    implements Monad<Maybe>

    static overrides Bind<A, B>(ma: Maybe<A>, f: A -> Maybe<B>): Maybe<B> =
        let res = ma.value
        f(res)

    static overrides Return<A>(a: A): Maybe<A> =
        Maybe<_>(a)

(>>=)<Toot<_>, A, B>(ma: Toot<A>, f: A -> Toot<B>): Toot<B> where Toot: trait Monad<Toot>  =
   Toot.Bind<_, _>(ma, f)

transform (x: __oly_int32): Maybe<__oly_float64> = Maybe<_>(228888.45)

example(): () =
   let m = Maybe<_>(1)
   let res = transform(1)
   let res: Maybe<__oly_float64> = m >>= transform
   Console.WriteLine<_>(res.value)

class Hoot<T> =
    public field value: T

    new(value: T) = { value = value }

extension HootAddExtension<T> where T: trait Add<T> =
    inherits Hoot<T>
    implements Add<Hoot<T>>

    static overrides add(x: Hoot<T>, y: Hoot<T>): Hoot<T> =
        let v1 = x.value
        let v2 = y.value
        let result = v1 + v2
        Hoot<_>(result)

f(x: __oly_int32): __oly_int32 = x + 1

g<T>(x: T, y: T): T where T: trait Add<T> = 
   class X<U> =
    public field x: T
    field y: U

    new(x: T, y: U) = { x = x; y = y }
   let doot = X<__oly_float64>(x, 9.0)
   doot.x + y
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Example 5``() =
    let src =
        """
open extension Int32AddExtension

#[import("CLR", "System", "Console")]
class Console =

    static WriteLine<T>(value: T): ()

interface Add<T> =

   static abstract add(x: T, y: T): T

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32): __oly_int32 =
      __oly_add(x, y)

(+)<T>(x: T, y: T): T where T: trait Add<T> =
   T.add(x, y)

main (): () =
   let result = 1 + 5
   Console.WriteLine<_>("hello world")
   Console.WriteLine<_>(result)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Example 6``() =
    let src =
        """
open extension Int32AddExtension

#[import("CLR", "System", "Console")]
class Console =

    static WriteLine<T>(value: T): ()

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2): T3

interface Add<T> =
    inherits Add<T, T, T>

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32): __oly_int32 =
        __oly_add(x, y)

(+)<T>(x: T, y: T): T where T: trait Add<T> = T.add(x, y)

main(): () =
   let x = 1
   Console.WriteLine<_>(x + x)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Example 7``() =
    let src =
        """
open extension Int32AddExtension

#[import("CLR", "System", "Console")]
class Console =

    static WriteLine<T>(value: T): ()

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2): T3

interface Add<T> =
   inherits Add<T, T, T>

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32): __oly_int32 =
        __oly_add(x, y)

extension Int32AddInt32Int16Int32Extension =
    inherits __oly_int32
    implements Add<__oly_int32, __oly_int16, __oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int16): __oly_int32 =
        __oly_add(x, y)

extension Int16AddExtension =
    inherits __oly_int32
    implements Add<__oly_int16>
    
    static overrides add(x: __oly_int16, y: __oly_int16): __oly_int16 =
        __oly_add(x, y)

(+)<T1, T2, T3>(x: T1, y: T2) : T3 where T1 : trait Add<T1, T2, T3> = T1.add(x, y)

main () : () =
   let x = 6
   let res = x + x
   Console.WriteLine<_>(res)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``HigherTypeArgument example 1``() =
    let src =
        """
class Hoot<T> =
    field value: T

    new(value: T) = { value = value }

interface Functor<F<_>> =

   static abstract Map<A, B>(fa: F<A>, f: A -> B) : F<B>
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Should pass potential witnesses``() =
    let src =
        """
interface Add<T> =

   static abstract add(x: T, y: T) : T

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32) : __oly_int32 =
      __oly_add(x, y)

(+)<T>(x: T, y: T) : T where T : Add<T> = T.add(x, y)

add<T>(x: T, y: T) : T where T : Add<T> =
   x + y
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Should pass implements higher kinded type on trait implements generic implements trait``() =
    let src =
        """
open extension Int32AddExtension

interface Add<T> =

   static abstract add(x: T, y: T): T

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32): __oly_int32 =
        __oly_add(x, y)

interface Monad<M<_>> =

   static abstract Bind<A, B>(ma: M<A>, f: A -> M<B>): M<B> where B: Add<__oly_int32>

   static abstract Return<A>(a: A): M<A>

class Maybe<T> =
    public field value: T

    new(value: T) = { value = value }

extension MaybeMonadExtension<T> =
    inherits Maybe<T>
    implements Monad<Maybe>

    static overrides Bind<A, B>(ma: Maybe<A>, f: A -> Maybe<B>): Maybe<B> where B: Add<__oly_int32> =
      let res = ma.value
      f(res)

    static overrides Return<A>(a: A): Maybe<A> =
      Maybe<_>(a)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Super-interfaces compile``() =
    let src =
        """
open extension Int32TestExtension

interface Add<T> =

   static abstract add(value1: T, value2: T) : T

interface Test =
    inherits Add<__oly_int32>

    static abstract boot(value1: __oly_int32, value2: __oly_int32) : __oly_int32

extension Int32TestExtension =
    inherits __oly_int32
    implements Test

    static overrides boot(value1: __oly_int32, value2: __oly_int32) : __oly_int32 = 1

    static overrides add(value1: __oly_int32, value2: __oly_int32) : __oly_int32 = 2

(+)<T>(x: T, y: T) : T where T : trait Add<T> = T.add(x, y)

f() : () =
    let x = 1 + 1
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Prefix application``() =
    let src =
        """
(^)(x: __oly_int32) : __oly_int32 = x

f() : () =
    let y = ^ 1
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Simple function reference``() =
    let src =
        """
f() : __oly_int32 = 1

main() : __oly_int32 =
    f()
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Simple function reference with parameter``() =
    let src =
        """
f(x: __oly_int32) : __oly_int32 = x

main() : __oly_int32 =
    f(1)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Simple function reference with parameter with inference``() =
    let src =
        """
f(x: __oly_int32) : __oly_int32 = x

main() : __oly_int32 =
    f(1)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Simple type with constructor``() =
    let src =
        """
class Test =
    field x: __oly_int32

    new(x: __oly_int32) = { x = x }
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Generic type that is nested within a function``() =
    let src =
        """
test<T>(x: T) : () =
    class X<U> =
      field x: T
      field y: U

      new(x: T, y: U) = { x = x; y = y }

    let y = X<__oly_int32>(x, 1)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Overloaded functions on type``() =
    let src =
        """
class Test =

    static M(x: __oly_int32) : __oly_int32 = x
    static M(x: __oly_utf16) : __oly_utf16 = x

test() : () =
    let f = Test.M(1i)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Non-let top-level declarations do not require order``() =
    let src =
        """
main() : () =
    let z = test(1)

test(x: __oly_int32) : __oly_int32 = x
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Namespaces are valid``() =
    let src =
        """
namespace Test1.Test2

class Test =
    field x: __oly_int32 = 0
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Should not crash with an unknown indexer``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

// Immutable array
#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

// Mutable array
#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

#[intrinsic("equal")]
(==)(int32, int32): bool

main(): () =
    if (Data[0].Value == Data[0])
        ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Identifier 'Data' not found in scope.",
                """
    if (Data[0].Value == Data[0])
        ^^^^
"""
            )
            ("Identifier 'Data' not found in scope.",
                """
    if (Data[0].Value == Data[0])
                         ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should get documentation summary for type``() =
    let src =
        """
// This is a test
class ~^~Test
        """
    let symbolInfo = getSymbolByCursor src
    Assert.Equal("This is a test", symbolInfo.Symbol.AsType.Documentation)

[<Fact>]
let ``Should not get documentation summary for type``() =
    let src =
        """
// This is a test

class ~^~Test
        """
    let symbolInfo = getSymbolByCursor src
    Assert.Equal("", symbolInfo.Symbol.AsType.Documentation)
