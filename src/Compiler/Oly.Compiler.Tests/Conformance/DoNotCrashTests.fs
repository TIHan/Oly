/// These tests are only temporarily useful when making sure the front-end compiler doesn't crash when syntax is in a partial state.
/// Language changes will not represent these tests very well.
/// Therefore, at some point we can remove them.
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
    |> containsErrorHelperTextDiagnostics [
        ("Partial instantiation of type-constructor 'IMemory<T, TKey, TValue, T>' not valid.",
        """
struct Test<TMemory<_>> where TMemory<_>: IMemory =
                                          ^^^^^^^
"""
        )
    ]

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
    |> containsErrorHelperTextDiagnostics [
        ("Expected ')'.",
        """
    static op_Equality(x TestStruct, y: TestStruct): bool = true
                        ^
"""
        )
    ]

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
    |> containsErrorHelperTextDiagnostics [
        ("Expected ')'.",
        """
    static abstract bind<A, B>(ma: MA>, f: A -> M2<B>) : M2<B>
                                     ^
"""
        )
    ]

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
    |> containsErrorHelperTextDiagnostics [
        ("Expected ')'.",
        """
    static abstract bind<A, B>(ma: M<A>, f: A  M2<B>) : M2<B>
                                             ^
"""
        )
    ]

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

[<Fact>]
let ``Test 13``() =
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
    |> containsErrorHelperTextDiagnostics [
        ("Partial instantiation of type-constructor 'IMemory<T, TKey, TValue, T>' not valid.",
        """
struct Test<TMemory<_>> where TMemory<_>: IMemory =
                                          ^^^^^^^
"""
        )
    ]

[<Fact>]
let ``Test 14``() =
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
    |> containsErrorHelperTextDiagnostics [
        ("Expected ')'.",
        """
    static op_Equality(x TestStruct, y: TestStruct): bool = true
                        ^
"""
        )
    ]

[<Fact>]
let ``Test 15``() =
    """
interface ITest =

    static abstract default M(): () = ()

class TestT>

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

test<T<_>>(): () where T<_>: Test =
    T<()>.M()

main(): () =
    test<Test>()
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 16``() =
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
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(, value)

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
let ``Test 17``() =
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

(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): ) where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)

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
let ``Test 18``() =
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
#intrinsic("set_element")]
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
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 19``() =
    """
module Oly.Entities

#[intrinsic("int32")]
alias int32

#intrinsic("by_ref_read_write")]
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
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 20``() =
    """
#[intrinsic("int32")]
alias int32

alias AliasObject<T1, T2, T3> = int32

#[open]
extension AdditionExtends<T1, T2, T3> =
    inherits AliasObjectT1, T2, T3>
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 21``() =
    """
main(): () =
    let loop() = ()
    let loop() = )
    loop()
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 22``() =
    """
interface ITest<T> where T: struct

class C

class Test<T<_, _>> where T<_, _>: ITest =

    M(x: T<C>): () = ()
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 23``() =
    """
#[intrinsic("void")]
alias void

module Module =

    class TestT>

f(): Module.Test<void> = unchecked default
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 24``() =
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
    set_Item(index: int32 item: T): () = this.Buffer[index] <- item

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
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 25``() =
    """
class Test 

    class NestedTest

test(x: Test.NestedTest): () = ()
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 26``() =
    """
class Test =

    class NestedTest =

        class 

test(x: Test.NestedTest.NestedTest2): () = ()
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 27``() =
    """
class Test 

    class NestedTest<T>

test(x: Test.NestedTest<__oly_int32>): () = ()
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 28``() =
    """
class Test<T> =

    class NestedTest<U>

test(x: Test<__oly_int32>.<__oly_float32>): () = ()
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 29``() =
    """
#[intrinsic("bool")]
alias bool

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

struct TestStruct =

    static op_Equality(x: TestStruct, y: TestStruct): bool = true

struct TestStruct2 =
    
    public field S: TestStruct = default

Find<T>(arr: T[], predicate: T -> bool): T = unchecked default

main(): ) =
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
let ``Test 30``() =
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
(`[]`)T>(mutable T[], index: int32): T

test(xs: mutable int32[]): () =
    let z: byref<int32> = &xs[0]
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 31``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool 

test(f: int32 -> int32): () = ()

main(): () =
    let x = 1

    test(
        if (x == 3)
            let y = 1
            x -> y
        else if (x == 1)
            let y = 5
            ()
            ()
            x -> y

            let z = 5
            ()
            let x = 2
            ()
            x -> z
    )
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 32``() =
    """
class C1

test(c: C1): ) = ()

main(): () =
    test(
        let f = C1
        f()
    )
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 33``() =
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
    let result = getResult(cast<ITest>123))
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 34``() =
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
    getResult(cast<ITest>x))
    
#[intrinsic("cast")]
cast<T>(__oly_object): T
    
main(): () =
    let result = getResult(cast<ITest>(123))
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 35``() =
    """
open extension TestTypeExtension<_>

interface Test<T> =

    static abstract test() : )

interface Test2<T<_>> where T<_> : Test =

    static abstract test2<A, B>(x: T<A>, y: A -> B) : T<B>

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
let ``Test 36``() =
    """
#[intrinsic("int32")]
    alias int32
    
    #[intrinsic("bool")]
    alias bool
    
    #[intrinsic("equal")]
    (==)(int32, int32): bool 
    
    test(f: int32 -> int32): () = ()
    
    main(): () =
        let x = 1
    
        test(
            if (x == 3)
                let y = 1
                x -> y
            else if x == 1)
                let y = 5
                ()
                ()
                x -> y
            else
                let z = 5
                ()
                let x = 2
                ()
                x -> z
        )
    """
    |> Oly
    |> hasErrorDiagnostics

[<Fact>]
let ``Test 37``() =
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

#[import(, "System", "Console")]
class Console =

    static WriteLine<T>(value: T): ()

interface Add<T> =

   static abstract add(x: T, y: T): T

(+)<T>(x: T, y: T): T where T: Add<T> = T.add(x, y)

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
let ``Test 38``() =
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
         Monad<Maybe>
    
        static overrides Bind<A, B>(ma: Maybe<A>, f: A -> Maybe<B>): Maybe<B> where B: Add<__oly_int32> =
          let res = ma.value
          f(res)
    
        static overrides Return<A>(a: A): Maybe<A> =
          Maybe<_>(a)
    """
    |> Oly
    |> hasErrorDiagnostics