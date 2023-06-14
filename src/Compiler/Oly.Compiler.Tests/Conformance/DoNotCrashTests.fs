module DoNotCrashTests

open Xunit
open TestUtilities

[<Fact>]
let ``Test 1``() =
    """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test(f: () -> Wrapper<() -> __oly_int32>): () = ()
test(f: () -> Wrapper<Wrapper<() -> >>): () = ()
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 2``() =
    """
interface Functor<F<_>> =

    static abstract fmap<A, B>(ab: A -> B, fa: F<A>): F<B>

fmap<F<_>, A, B>(ab: A -> B, fa: F<A>): F<B> where F: Functor<F> =
    F.fmap(ab, fa)

fmap<F<_, A>(aa: A -> A, fa: F<A>): F<A> where F: Functor<F> =
    fmap<_, _, _>(aa, fa)
    """
    |> Oly
    |> withSyntaxErrorHelperTextDiagnostics [
        ("Expected '>'.",
        """
fmap<F<_, A>(aa: A -> A, fa: F<A>): F<A> where F: Functor<F> =
            ^
"""
        )
    ]

[<Fact>]
let ``Test 3``() =
    """
#[intrinsic("base_object")]
alias object

#intrinsic("print")]
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
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 4``() =
    """
#[intrinsic("bool")]
alias bool

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

struct TestStruct =

    static op_Equality(x TestStruct, y: TestStruct): bool = true

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
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 5``() =
    """
module TestModule

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

pattern Some<T>(value: Option<T>): __oly_bool =
    false

pattern Some<T>(value: Option<T>): T =
    value.Value

test(value: Option<__oly_int32>): () =
    match(
    """
    |> Oly
    |> withErrorHelperTextDiagnostics [
        ("Expected 'expressions to match' after '('.",
        """
    match(
         ^
"""
        )
    ]

[<Fact>]
let ``Test 6``() =
    """
interface Monad<M<_>, M2<_>> =

    static abstract bind<A, B>(ma: MA>, f: A -> M2<B>) : M2<B>

class Maybe<T> =
    public field value: T

    new(value: T) = { value = value }

class Maybe2<T> =
    public field value: T

    new(value: T) = { value = value }

extension MaybeMonadExtension<T> =
    inherits Maybe<T>
    implements Monad<Maybe, Maybe2>

    static overrides bind<A, B>(ma: Maybe<A>, f: A -> Maybe2<B>) : Maybe2<B> =
        let res = ma.value
        f(res)

(>>=)<Toot, W<_>, W2<_>, X, Y>(ma: W<X>, f: X -> W2<Y>) : W2<Y> where Toot : Monad<W, W2> =
    Toot.bind<_, _>(ma, f)
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 7``() =
    """
interface Monad<M<_>, M2<_>> =

    static abstract bind<A, B>(ma: M<A>, f: A  M2<B>) : M2<B>

class Maybe<T> =
    public field value: T

    new(value: T) = { value = value }

class Maybe2<T> =
    public field value: T

    new(value: T) = { value = value }

extension MaybeExtension<T> =
    inherits Maybe<T>
    implements Monad<Maybe, Maybe2>

    static overrides bind<A, B>(ma: Maybe<A>, f: A -> Maybe2<B>) : Maybe2<B> =
        let res = ma.value
        f(res)

(>>=)<Toot, W<_>, W2<_>, X, Y>(ma: W<X>, f: X -> W2<Y>) : W2<Y> where Toot : Monad<W, W2> =
    Toot.bind<_, _>(ma, f)
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 8``() =
    """
interface Test<T> =

    static abstract test() : )

interface Test2<T<_>> where T<_> : Test =

    static abstract test2<A, B>(x: T<A>, y: B) : T<B>

class TestType<T> =
    public field value: T

    new(value: T) = { value = value }

extension Int32Extension =
    inherits __oly_int32
    implements Test2<TestType>

    static overrides test2<A, B>(x: TestType<A>, y: B): TestType<B> =
        TestType<_>(y)
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 9``() =
    """
open extension TestTypeExtension<_>

interface Test<T> =

    static abstract test() : ()

interface Test2<T<_>> where T<_> : Test =

    static abstract test2<A, B>(x: T<A>, y A -> B) : T<B>

class TestType<T> =
    public field value: T

    new(value: T) = { value = value }

extension TestTypeExtension<T> =
    inherits TestType<T>
    implements Test<TestType<T>>

    static overrides test() : () = ()

extension Int32Extension =
    inherits __oly_int32
    implements Test2<TestType>

    static overrides test2<A, B>(x: TestType<A>, y: A -> B): TestType<B> =
        TestType<_>(y(x.value))
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 10``() =
    """
open extension MaybeMonadExtension<int32>
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

(+)<T>(x: T, y: T): T where T: Add<T> = T.add(x, y)

interface Functor<F<_>> =

   static abstract map<A, B>(fa: F<A>, f: A  B): F<B>

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

(>>=)<Toot<_>, A, B>(ma: Toot<A>, f: A -> Toot<B>): Toot<B> where Toot: Monad<Toot>  =
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

extension HootAddExtension<T> where T: Add<T> =
    inherits Hoot<T>
    implements Add<Hoot<T>>

    static overrides add(x: Hoot<T>, y: Hoot<T>): Hoot<T> =
        let v1 = x.value
        let v2 = y.value
        let result = v1 + v2
        Hoot<_>(result)

f(x: __oly_int32): __oly_int32 = x + 1

g<T>(x: T, y: T): T where T: Add<T> = 
   class X<U> =
    public field x: T
    field y: U

    new(x: T, y: U) = { x = x; y = y }
   let doot = X<__oly_float64>(x, 9.0)
   doot.x + y
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 11``() =
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

   static abstract Bind<A, B>(ma M<A>, f: A -> M<B>): M<B> where B: Add<__oly_int32>

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
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 12``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("multiply")]
(+)(int32, int32): int32

#[intrinsic("multiply")]
(*)(int32, int32): int32

#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

#[intrinsic("get_length")]
private getLength<T>(mutable T[]): int32

#[open]
extension MutableArrayExtensions<T> =
    inherits mutable T[]

    Length: int32 
        #[inline]
        get() = getLength(this)

module Array =

    #[intrinsic("new_array")]
    ZeroCreate<T>(size: int32): mutable T[]

    Flatten<T, U>(arr: mutable T[], f: T -> (U, U, U)): mutable U[] =
        let newArr = ZeroCreate<U>(arr.Length * 3)
        let mutable i = 0
        let mutable j = 0
        while (
    """
    |> Oly
    |> withErrorHelperTextDiagnostics [
        ("Expected 'condition expression' after '('.",
        """
        while (
              ^
"""
        )
    ]
