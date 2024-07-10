module Conformance.Types.InterfaceTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Interface with default implementation``() =
    let src =
        """
interface Test =

    static abstract default test() : __oly_int32 = 1
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Interface inherits has correct symbols``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
    inherits ~^~Add<T, T, T>
        """

    src |> hasSymbolSignatureTextByCursor "Add<T, T, T>"

[<Fact>]
let ``Interface inherits has correct symbols 2``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
    inherits Add<T, ~^~T, T>
        """

    src |> hasSymbolSignatureTextByCursor "T"

[<Fact>]
let ``Interface implements has correct symbols``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static add(x: T1, y: T2) : T1 = x 

class Add<T1, T2> =
    implements ~^~Add<T1, T2, T2>

    static add(x: T1, y: T2) : T2 = y
        """

    src |> hasSymbolSignatureTextByCursor "Add<T1, T2, T2>"

[<Fact>]
let ``Interface implements has correct symbols 2``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static add(x: T1, y: T2) : T1 = x 

class Add<T1, T2> =
    implements Add<T1, ~^~T2, T2>

    static add(x: T1, y: T2) : T2 = y
        """

    src |> hasSymbolSignatureTextByCursor "T2"

[<Fact>]
let ``Static interface function requires an implementation``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static add(x: T1, y: T2) : T3
        """

    Oly src
    |> withErrorDiagnostics
        [
            "The function 'add' must have an implementation."
        ]
    |> ignore

[<Fact>]
let ``Interface requires explicit implementation``() =
    let src =
        """
class Test =

    add(x: __oly_int32, y: __oly_int32) : __oly_int32

interface Add<T1, T2, T3> =

    add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

extension TestAddExtension =
    inherits Test
    implements Add<__oly_int32>
        """

    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("The function 'add' must have an implementation.", "
    add(x: __oly_int32, y: __oly_int32) : __oly_int32
    ^^^
");
            ("The function 'add(x: __oly_int32, y: __oly_int32): __oly_int32' is not implemented for 'Add<__oly_int32, __oly_int32, __oly_int32>' on 'TestAddExtension'.", "
extension TestAddExtension =
          ^^^^^^^^^^^^^^^^
")
        ]
    |> ignore

[<Fact>]
let ``Complex interface``() =
    let src =
        """
interface Monad<M<_>, M2<_>> =

    static abstract bind<A, B>(ma: M<A>, f: A -> M2<B>) : M2<B>

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
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Complex interface via explicit extension``() =
    let src =
        """
interface Monad<M<_>, M2<_>> =

    static abstract bind<A, B>(ma: M<A>, f: A -> M2<B>) : M2<B>

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
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Type must implement static interface function``() =
    let src =
        """
interface TestInterface =
 
    static abstract test() : __oly_int32

class Test =
    implements TestInterface

    static test() : __oly_int32 = 123
        """
    Oly src
    |> withErrorDiagnostics
        [
            "The member 'test' will hide over its base."
            "The function 'static test(): __oly_int32' is not implemented for 'TestInterface' on 'Test'."
        ]
    |> ignore

[<Fact>]
let ``Static abstract on an interface must not include an implementation without 'default'``() =
    let src =
        """
interface TestInterface =
 
    static abstract test() : __oly_int32 = 1
        """
    Oly src
    |> withErrorDiagnostics
        [
            "An abstract member with an implementation must have a 'default' modifier."
        ]
    |> ignore

[<Fact>]
let ``Static abstract default interface must require an implementation``() =
    let src =
        """
interface TestInterface =
 
    static abstract default test() : __oly_int32
        """
    Oly src
    |> withErrorDiagnostics
        [
            "The function 'test' must have an implementation."
        ]
    |> ignore

[<Fact>]
let ``Static abstract default interface should compile``() =
    let src =
        """
interface TestInterface =
 
    static abstract default test() : __oly_int32 = 1
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Interface inherits more than one interface``() =
    let src =
        """
interface IA
interface IB
interface IC =
    inherits IA, IB
        """
    Oly src
    |> withCompile
    |> ignore


[<Fact>]
let ``Generic interface should error if the type instantiations don't match``() =
    let src =
        """
interface Test<T> =

    static abstract test() : ()

test1<T>() : () where T : Test<__oly_int32> =
    T.test()

test2<T>() : () where T : Test<__oly_float64> =
    test1<T>()
        """
    Oly src
    |> withErrorDiagnostics ["Type instantiation 'T' is missing the constraint 'Test<__oly_int32>'."]
    |> ignore

[<Fact>]
let ``Generic interface should error if the type instantiations don't match - 2``() =
    let src =
        """
interface Test<T> =

    static abstract test() : ()

test1<T>() : () where T : Test<__oly_int32> =
    T.test()

test2<T>() : () where T : Test<T> =
    test1<T>()
        """
    Oly src
    |> withErrorDiagnostics ["Type instantiation 'T' is missing the constraint 'Test<__oly_int32>'."]
    |> ignore

[<Fact>]
let ``Type argument is missing interfaces from when implementing an interface with a higher kind``() =
    let src =
        """
interface Test<T> =

    static abstract test() : ()

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
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation 'TestType<T>' is missing the constraint 'Test<T>'."
        ]
    |> ignore

[<Fact>]
let ``Type argument is missing interfaces from when implementing an interface with a higher kind - reverse order``() =
    let src =
        """
extension Int32Extension =
    inherits __oly_int32
    implements Test2<TestType>

    static overrides test2<A, B>(x: TestType<A>, y: B) : TestType<B> =
        TestType<_>(y)

class TestType<T> =
    public field value: T

    new(value: T) = { value = value }

interface Test2<T<_>> where T<_> : Test =

    static abstract test2<A, B>(x: T<A>, y: B) : T<B>

interface Test<T> =

    static abstract test() : ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation 'TestType<T>' is missing the constraint 'Test<T>'."
        ]
    |> ignore

[<Fact>]
let ``Type argument is missing the interface as we do not allow extensions implementing interfaces to be resolved for type constructors``() =
    let src =
        """
open extension TestTypeExtension<_>

interface Test<T> =

    static abstract test() : ()

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
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation 'TestType<T>' is missing the constraint 'Test<T>'."
        ]
    |> ignore

[<Fact>]
let ``Resolve type inference on the + operator``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

(+)<T1, T2, T3>(x: T1, y: T2) : T3 where T1 : Add<T1, T2, T3> = T1.add(x, y)

f<T, U>(x: T) : U where T : Add<T, __oly_int16, U> = x + 1
        """

    Oly src
    |> withErrorDiagnostics
        [
           "Type instantiation 'T' is missing the constraint 'Add<T, __oly_int32, U>'."
        ]
    |> ignore

[<Fact>]
let ``Interface implementation has correct symbols``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

extension Int32AddExtension =
    inherits ~^~__oly_int32
    implements Add<__oly_int32, __oly_int32, __oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32): __oly_int32 = 1
        """
    src |> hasSymbolSignatureTextByCursor "__oly_int32"

[<Fact>]
let ``Interface implementation has correct symbols 2``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

extension Int32AddExtension =
    inherits __oly_int32
    implements ~^~Add<__oly_int32, __oly_int32, __oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32) : __oly_int32 = 1
        """
    src |> hasSymbolSignatureTextByCursor "Add<__oly_int32, __oly_int32, __oly_int32>"

[<Fact>]
let ``Order does not matter``() =
    let src =
        """
open extension Int32AddExtension

~^~test<T>(x: T): T where T: Add<T> = T.add(x, x)

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32): __oly_int32 = 1

interface Add<T> =
    inherits Add<T, T, T>

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3
        """
    src |> hasSymbolSignatureTextByCursor "static test<T>(x: T): T where T: Add<T>"

[<Fact>]
let ``Order does not matter 2``() =
    let src =
        """
open extension Int32AddExtension

test2() : () =
    let x = ~^~test<_>(1)

test<T>(x: T) : T where T : trait Add<T> = T.add(x, x)

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32) : __oly_int32 = 1

interface Add<T> =
    inherits Add<T, T, T>

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3
        """
    src |> hasSymbolSignatureTextByCursor "static test<T>(x: T): T where T: trait Add<T>"

[<Fact>]
let ``Order does not matter 3``() =
    let src =
        """
open extension Int32AddExtension

test2(): () =
    let x = ~^~test<_>(1)

test<T>(x: T): T where T: trait Add<T, T, T> = T.add(x, x)

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32): __oly_int32 = 1

interface Add<T> =
    inherits Add<T, T, T>

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2): T3
        """
    src |> hasSymbolSignatureTextByCursor "static test<T>(x: T): T where T: trait Add<T, T, T>"

[<Fact>]
let ``Order does not matter - but should fail due to missing interface``() =
    let src =
        """
test2() : () =
    let x = test<_>(1)

test<T>(x: T) : T where T : Add<T, T, T> = T.add(x, x)

interface Add<T> =
    inherits Add<T, T, T>

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3
        """
    Oly src
    |> withErrorDiagnostics
        [
           "Type instantiation '__oly_int32' is missing the constraint 'Add<__oly_int32, __oly_int32, __oly_int32>'."
        ]
    |> ignore

[<Fact>]
let ``Able to use a static member from a interface when implemented via an extension``() =
    let src =
        """
open extension Int32AddExtension

interface Add<T> =

   static abstract add(x: T, y: T) : T

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32) : __oly_int32 = x

test() : () =
    let x = __oly_int32.add(1, 2)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Able to use an instance member from a interface when implemented via an extension``() =
    let src =
        """
open extension Int32AddExtension

interface Add<T> =

   add(x: T, y: T) : T

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    add(x: __oly_int32, y: __oly_int32) : __oly_int32 = x

test() : () =
    let y = 1
    let x = y.add(1, 2)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Not able to use a default static member from a interface when implemented via an extension``() =
    let src =
        """
open extension Int32AddExtension

interface Add<T> =

   static abstract default add(x: T, y: T) : T =  x

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

test() : () =
    let x = __oly_int32.add(1, 2)
        """
    Oly src
    |> withErrorDiagnostics
        [
           "Member 'add' does not exist on type '__oly_int32'."
        ]
    |> ignore

[<Fact>]
let ``Not able to use a default instance member from a interface when implemented via an extension``() =
    let src =
        """
interface Add<T> =

   default add(x: T, y: T) : T = x

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

test() : () =
    let y = 1
    let x = y.add(1, 2)
        """
    Oly src
    |> withErrorDiagnostics
        [
           "Member 'add' does not exist on type '__oly_int32'."
        ]
    |> ignore

[<Fact>]
let ``Able to use an instance member with the same name implemented via an extension``() =
    let src =
        """
open extension TestAddExtension

class Test =

    field x: __oly_int32
    new(x: __oly_int32) = { x = x }

    add(x: __oly_int32, y: __oly_int32) : __oly_int32 = y

interface Add<T> =

   add(x: T, y: T) : T

extension TestAddExtension =
    inherits Test
    implements Add<__oly_int32>

    add(x: __oly_int32, y: __oly_int32) : __oly_int32 = x

test() : () =
    let y = Test(567)
    let x = y.add(1, 2)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Multiple implements on explicit extension``() =
    let src =
        """
interface Add<T> =

   static abstract add(value1: T, value2: T): T

(+)<T>(value1: T, value2: T): T where T: Add<T> =
   T.add(value1, value2)

extension Int32Extensions =
    inherits __oly_int32
    implements Add<__oly_int32>, Add<__oly_float64>

    static overrides add(x: __oly_int32, value2: __oly_int32): __oly_int32 =
        __oly_add(x, value2)

    static overrides add(x: __oly_float64, value2: __oly_float64): __oly_float64 =
        __oly_add(x, value2)
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Recursive implements should error``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static add(x: T1, y: T2) : T1 = x 

class Add<T1, T2> =
    implements Add<T1, T2>
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot implement non-interfaces.",
                """
class Add<T1, T2> =
      ^^^
"""
            )
            ("'Add<T1, T2>' is recursively implementing itself.",
                """
    implements Add<T1, T2>
    ^^^^^^^^^^^^^^^^^^^^^^
"""
            )
        ]

[<Fact>]
let ``Overrides should fail if there is nothing to override for the interface``() =
    """
#[intrinsic("int32")]
alias int32

interface IComponent

interface IComponent<N, T> where N: constant int32 where T: unmanaged =
    inherits IComponent

    static overrides GetValue(): int32 = N
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("The function 'static GetValue(): int32' cannot find a function to override.",
                """
    static overrides GetValue(): int32 = N
                     ^^^^^^^^
"""
            )
        ]

[<Fact>]
let ``Interface cannot implement an interface``() =
    """
interface IA

interface IB =
    implements IA
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Interfaces cannot implement interfaces.",
                """
interface IB =
          ^^
"""
            )
        ]

[<Fact>]
let ``Interface implementation not valid as the constraints are not the same - testing trait constraints``() =
    """
interface ISee

interface IA =

    M<T>(): () where T: trait ISee

class A =
    implements IA

    M<T>(): () where T: ISee = ()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("'ISee' constraint does not exist on the overriden function's type parameter 'T'.",
                """
    M<T>(): () where T: ISee = ()
    ^
"""
            )
        ]

[<Fact>]
let ``Interface implementation not valid as the constraints are not the same - testing trait constraints 2``() =
    """
interface ISee

interface IA =

    M<T>(): () where T: ISee

class A =
    implements IA

    M<T>(): () where T: trait ISee = ()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("'trait ISee' constraint does not exist on the overriden function's type parameter 'T'.",
                """
    M<T>(): () where T: trait ISee = ()
    ^
"""
            )
        ]

[<Fact>]
let ``Cannot implement a non-interface``() =
    """
class A

class B =
    implements A
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot implement non-interfaces.",
                """
class B =
      ^
"""
            )
        ]