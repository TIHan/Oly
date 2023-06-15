module Conformance.Constraints.ConstraintTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Constraint should fail``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

f<T, U, V>(x: V, y: U) : V where U : Add<V, U, V> =
    U.add(x, x)
    """
    Oly src
    |> withErrorDiagnostics
        [
            "Expected type 'U' but is 'V'."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 2``() =
    let src =
        """
f<T>(x: T): () where T: not struct =
    ()

test(): () =
    f(1)
    """
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation '__oly_int32' is missing the constraint 'not struct'."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 3``() =
    let src =
        """
class Test<T> where T: not struct

test(): () =
    let result = Test<__oly_int32>()
    """
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation '__oly_int32' is missing the constraint 'not struct'."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 4``() =
    let src =
        """
class Test<T> where T: not struct

test(x: Test<__oly_int32>): () =
    ()
    """
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation '__oly_int32' is missing the constraint 'not struct'."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 5``() =
    let src =
        """
f<T>(x: T): () where T: not struct =
    ()

test(): () =
    let g = x -> f(x)
    g(1)
    """
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation 'T' is missing the constraint 'not struct'."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 6``() =
    let src =
        """
f<T>(x: T): () where T: not struct =
    ()

g<T>(x: T): () =
    f(x)
    """
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation 'T' is missing the constraint 'not struct'."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 7``() =
    let src =
        """
interface IA =
    
    Test<T>(): () where T: not struct

class B =
    implements IA

    Test<T>(): () = ()

    """
    Oly src
    |> withErrorDiagnostics
        [
            "'Test' type parameter constraints do not match its overriden function."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 8``() =
    let src =
        """
interface IA =
    
    Test<T>(): () where T: not struct

class B =
    implements IA

    Test<T>(): () where T: struct = ()

    """
    Oly src
    |> withErrorDiagnostics
        [
            "'struct' constraint does not exist on the overriden function's type parameter 'T'."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 9``() =
    let src =
        """
test<N>(): () where N: constant __oly_object = ()
    """
    Oly src
    |> withErrorDiagnostics
        [
            "'__oly_object' is not a supported constant type."
        ]
    |> ignore

[<Fact>]
let ``Constraint should fail 10``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

test<N>(): () where N: constant int32 = ()

main(): () =
    test<int32>()
    """
    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation 'int32' is missing the constraint 'constant int32'."
        ]
    |> ignore

[<Fact>]
let ``Constraint should compile``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

f<T, U, V>(x: V, y: U) : V where U : Add<V, U, V> =
    U.add(x, y)
    """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile 2``() =
    let src =
        """
f<T>(x: T): () where T: not struct =
    ()

test(): () =
    f("")
    """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile and right signature``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

f<T, U, V>(x: V, y: U) : V where U : Add<V, U, V> =
    ~^~U.add(x, y)
    """
    src |> hasSymbolSignatureTextByCursor "U"

[<Fact>]
let ``Constraint should compile 3``() =
    let src =
        """
open extension MaybeMonadExtension<int32>

#[intrinsic("int32")]
alias int32

interface Monad<M<_>> =

   static abstract Bind<A, B>(ma: M<A>, f: A -> M<B>) : M<B>

   static abstract Return<A>(a: A) : M<A>

class Maybe<T> =
    public field value: T

    new(value: T) = { value = value }

extension MaybeMonadExtension<T> =
    inherits Maybe<T>
    implements Monad<Maybe>

    static overrides Bind<A, B>(ma: Maybe<A>, f: A -> Maybe<B>) : Maybe<B> =
      let res = ma.value
      f(res)

    static overrides Return<A>(a: A) : Maybe<A> =
      Maybe<_>(a)

(>>=)<Test<_>, X, Y>(ma: Test<X>, f: X -> Test<Y>) : Test<Y> where Test: Monad<Test>  =
   Test.Bind<_, _>(ma, f)

transform (x: __oly_int32) : Maybe<__oly_float64> = Maybe<_>(228888.45)

example() : () =
   let m = Maybe<_>(1)
   let res = transform(1)
   let res: Maybe<__oly_float64> = m >>= transform
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile 4``() =
    let src =
        """
class Test<T> where T: not struct

test(x: Test<__oly_utf16>): () =
    ()
    """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile 5``() =
    let src =
        """
class Test<T> where T: not struct

test(x: Test<(__oly_int32, __oly_int32)>): () =
    ()
    """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile 6``() =
    let src =
        """
class Test<T> where T: not struct

test(): () =
    let result = Test<(__oly_int32, __oly_int32)>()
    """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile 7``() =
    let src =
        """
f<T>(x: T): () where T: not struct =
    ()

g<U>(x: U): () where U: not struct =
    f(x)
    """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile 8``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

test<N>(): () where N: constant int32 = ()
    """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile 9``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

test<N>(): () where N: constant int32 = ()

main(): () =
    test<1>()
    """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should compile and have correct instance symbol type``() =
    let src =
        """
interface TTrait<T> =

    test() : ()

class Test<T> =

    field x: __oly_int32 = 0

extension TestExtension<W> =
    inherits Test<W>
    implements TTrait<W>

    test() : () =
        let x = ~^~this
        """
    src |> hasSymbolSignatureTextByCursor "this: Test<W>"

[<Fact>]
let ``Constraint should compile and have correct instance symbol type 2``() =
    let src =
        """
interface TTrait<T> =

    test() : ()

class Test<T> =

    field x: __oly_int32 = 0

extension TestExtension<W> =
    inherits Test<W>
    implements TTrait<W>

    test() : () =
        let ~^~x = this
        """
    src |> hasSymbolSignatureTextByCursor "x: Test<W>"

[<Fact>]
let ``Constraint should compile and have correct instance symbol type 4``() =
    let src =
        """
interface TTrait<T<_>> =

    test() : ()

class Test<T> =

    field x: __oly_int32 = 0

extension TestExtension<W> =
    inherits Test<W>
    implements TTrait<Test>

    test() : () =
        let x = ~^~this
        """
    src |> hasSymbolSignatureTextByCursor "this: Test<W>"

[<Fact>]
let ``Automatic generalization should compile``() =
    let src =
        """
main() : () =
    let f(x) = x
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Calling a function that returns an output that we haven't solved at the callsite should fail``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

(+)<T1, T2, T3>(x: T1, y: T2) : T3 where T3 : Add<T1, T2, T3> = T3.add(x, y)

main () : () =
    let f(x: __oly_int32) = 8 + x
    let x = 50
    let z = f(x)
        """

    Oly src
    |> withErrorDiagnostics
        [
            "Type instantiation 'T' is missing the constraint 'Add<__oly_int32, __oly_int32, T>'."
            "Type parameter '?T' was unable to be inferred."
        ]
    |> ignore

[<Fact>]
let ``Automatic generalization should get the correct symbol``() =
    """
open extension Int32AddExtension

interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

(+)<T1, T2, T3>(x: T1, y: T2) : T3 where T1 : Add<T1, T2, T3> = T1.add(x, y)

extension Int32AddExtension =
    inherits __oly_int32
    implements Add<__oly_int32>

    static overrides add(x: __oly_int32, y: __oly_int32) : __oly_int32 =
        __oly_add(x, y)

main() : () =
    let ~^~f(x) = 1 + x
    """
    |> hasSymbolSignatureTextByCursor "f(x: __oly_int32): __oly_int32"

[<Fact>]
let ``Function has correct constraint symbols``() =
    """
interface TestTrait =

    static abstract default f() : __oly_int32 = 1

test<T>() : __oly_int32 where T : ~^~TestTrait =
    1
    """
    |> hasSymbolSignatureTextByCursor "TestTrait"

[<Fact>]
let ``Function has correct constraint symbols 2``() =
    """
interface TestTrait =

    static abstract default f() : __oly_int32 = 1

test<T>() : __oly_int32 where ~^~T : TestTrait =
    1
    """
    |> hasSymbolSignatureTextByCursor "T"

[<Fact>]
let ``Constraint is missing type parameters for a second-order generic - should pass``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

test<T<_>>() : () where T<_> : Add = ()
        """

    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Constraint should not constrain a type``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
   inherits Add<T, T, T>

test<T<_>>() : () where __oly_int32 : Add<__oly_int32> = ()
        """

    Oly src
    |> withErrorDiagnostics
        [
            // TODO: The error should probably be "'__oly_int32' is not a type parameter."
            "Expected a type parameter for the constraint."
        ]
    |> ignore

[<Fact>]
let ``Adding a interface constraint on a type definition should compile``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

class Test<T> where T : Add<T, T, T> =

    field x: T

    new(x: T) = { x = x }

    test(y: T) : T = T.add(y, y) 
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Cannot add additional constraints to nested functions``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

test<T>() : () =
    let test2<U>() where U : Add<U, U, U>; where T : Add<T, T, T> = ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Cannot add additional constraints to the captured type parameter 'T'."
        ]
    |> ignore

[<Fact>]
let ``Cannot add additional constraints to nested functions 2``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
    inherits Add<T, T, T>

test<T<_>>() : () =
    let test2<U>() where U : Add<U, U, U>; where T<_> : Add = ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Cannot add additional constraints to the captured type parameter 'T<_>'."
        ]
    |> ignore

[<Fact>]
let ``Cannot add additional constraints to type functions``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

class test<T> =
    test2<U>() : () where U : Add<U, U, U>; where T : Add<T, T, T> = ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Cannot add additional constraints to the captured type parameter 'T'."
        ]
    |> ignore

[<Fact>]
let ``Cannot add additional constraints to type functions 2``() =
    let src =
        """
interface Add<T1, T2, T3> =

   static abstract add(x: T1, y: T2) : T3

interface Add<T> =
    inherits Add<T, T, T>

class test<T<_>> =
    test2<U>() : () where U : Add<U, U, U>; where T<_> : Add = ()
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Cannot add additional constraints to the captured type parameter 'T<_>'."
        ]
    |> ignore

[<Fact>]
let ``Nested constraint should fail``() =
    let src =
        """
open extension Int32AddExtension

main() : () =
    let x = Invoke<int32>(1)

Invoke<T>(x: T) : () where T : Add<T, T, T> =
    let call2<A, B>(a: A, b: B) = x
    let call<A, X>(a: A) : (X -> T) where X : Add<X, T, T> = 
        (x: X) -> x + call2(a, x)

extension Int32AddExtension =
    inherits int32
    implements Add<int32, int32, int32>

    static overrides (+)(x: int32, y: int32) : int32 =
        __oly_add(x, y)

(+)<T>(x: T, y: T) : T where T : Add<T, T, T> = T.(+)(x, y)

interface Add<T1, T2, T3> =

    static abstract (+)(T1, T2) : T3

#[intrinsic("int32")]
alias int32
        """
    Oly src
    |> withErrorDiagnostics
        [
            "Expected type 'X' but is 'T'."
            "Type instantiation 'X' is missing the constraint 'Add<X, X, X>'."
            "Expected type 'X -> T' but is 'X -> X'."
        ]
    |> ignore

[<Fact>]
let ``Nested constraint should fail 1``() =
    let src =
        """
open extension Int32AddExtension

main() : () =
    let x = Invoke<int32>(1)

Invoke<T>(x: T) : () where T : Add<T, T, T> =
    let call2<A, B>(a: A, b: B) = x
    let call<A, X>(a: A) : (X -> T) where X : Add<X, T, T> = 
        let f = (x: X) -> x + call2(a, x)
        f

extension Int32AddExtension =
    inherits int32
    implements Add<int32, int32, int32>

    static overrides (+)(x: int32, y: int32) : int32 =
        __oly_add(x, y)

(+)<T>(x: T, y: T) : T where T : Add<T, T, T> = T.(+)(x, y)

interface Add<T1, T2, T3> =

    static abstract (+)(T1, T2) : T3

#[intrinsic("int32")]
alias int32
        """

    Oly src
    |> withErrorDiagnostics
        [
            "Expected type 'X' but is 'T'."
            "Type instantiation 'X' is missing the constraint 'Add<X, X, X>'."
        ]
    |> ignore

[<Fact>]
let ``Second-order generic constraint specified twice but one is its type constructor should pass``() =
    let src =
        """
interface ITest<T>

class Test<T<_>> where T<_>: ITest; where T: struct
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Second-order generic constraint specified twice but one is its type constructor should pass 2``() =
    let src =
        """
interface ITest<T>

class Test<T<_>> where T<_>: ITest; where T: struct

struct Impl<T> =
    implements ITest<T>

test(x: Test<Impl>): () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor should pass``() =
    let src =
        """
interface ITest =

    static abstract M(): ()

class Test<T> =
    implements ITest

    static overrides M(): () = ()

test<T<_>>(): () where T: ITest; where T<_>: Test =
    T.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor should pass 2``() =
    let src =
        """
interface ITest =

    static abstract M(): ()

class Test<T> =
    implements ITest

    static overrides M(): () = ()

test<T<_>>(): () where T: ITest; where T<_>: Test =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor should pass 3``() =
    let src =
        """
interface ITest =

    static abstract M(): ()

class Test<T> =
    implements ITest

    static overrides M(): () = ()

test<T<_>>(): () where T: ITest =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor should pass 4``() =
    let src =
        """
interface ITest =

    static abstract M(): ()

class Test<T> =
    implements ITest

    static overrides M(): () = ()

test<T<_>>(): () where T: ITest =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor should pass 5``() =
    let src =
        """
interface ITest =

    static abstract default M(): () = ()

class Test<T> =
    implements ITest

test<T<_>>(): () where T: ITest =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor should pass 6``() =
    let src =
        """
interface ITest =

    static abstract default M(): () = ()

class Test<T> =
    implements ITest

test<T<_>>(): () where T: ITest =
    T.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor with extension should pass``() =
    let src =
        """
interface ITest =

    static abstract M(): ()

class Test<T>

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    static overrides M(): () = ()

test<T<_>>(): () where T: ITest; where T<_>: Test =
    T.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor with extension should pass 2``() =
    let src =
        """
interface ITest =

    static abstract M(): ()

class Test<T>

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    static overrides M(): () = ()

test<T<_>>(): () where T: ITest; where T<_>: Test =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor with extension should pass 3``() =
    let src =
        """
interface ITest =

    static abstract M(): ()

class Test<T>

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    static overrides M(): () = ()

test<T<_>>(): () where T: ITest =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor with extension should pass 4``() =
    let src =
        """
interface ITest =

    static abstract M(): ()

class Test<T>

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

    static overrides M(): () = ()

test<T<_>>(): () where T: ITest =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor with extension should pass 5``() =
    let src =
        """
interface ITest =

    static abstract default M(): () = ()

class Test<T>

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

test<T<_>>(): () where T: ITest =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor with extension should pass 6``() =
    let src =
        """
interface ITest =

    static abstract default M(): () = ()

class Test<T>

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

test<T<_>>(): () where T: ITest =
    T.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor should fail``() =
    let src =
        """
interface ITest =

    static abstract default M(): () = ()

class Test<T> =
    implements ITest

test<T<_>>(): () where T<_>: Test =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Member 'M' does not exist on type 'T<()>'.",
                """
    T<()>.M()
          ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Ambiguous-looking generic type constructor with extension should fail``() =
    let src =
        """
interface ITest =

    static abstract default M(): () = ()

class Test<T>

#[open]
extension TestExtension<T> =
    inherits Test<T>
    implements ITest

test<T<_>>(): () where T<_>: Test =
    T<()>.M()

main(): () =
    test<Test>()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Member 'M' does not exist on type 'T<()>'.",
                """
    T<()>.M()
          ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Second-order generic constraint specified twice but one is its type constructor should fail``() =
    let src =
        """
interface ITest<T>

class Test<T<_>> where T<_>: ITest; where T: struct

class Impl<T> =
    implements ITest<T>

test(x: Test<Impl>): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'Impl<T>' is missing the constraint 'struct'.",
                """
test(x: Test<Impl>): () = ()
             ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Second-order generic constraint specified twice but one is its type constructor should fail 2``() =
    let src =
        """
interface ITest<T>

class Test<T<_>> where T<_>: ITest; where T: struct

struct Impl<T>

test(x: Test<Impl>): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'Impl<T>' is missing the constraint 'ITest<T>'.",
                """
test(x: Test<Impl>): () = ()
             ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Second-order generic constraint should fail because of constraints on generic type constructor``() =
    let src =
        """
interface ITest<T> where T: struct

class C

class Test<T<_>> where T<_>: ITest =

    M(x: T<C>): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'C' is missing the constraint 'struct'.",
                """
    M(x: T<C>): () = ()
           ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Second-order generic constraint should fail because of constraints on generic type constructor 2``() =
    let src =
        """
interface ITest2<T> where T: not struct
interface ITest<T<_>> where T<_>: ITest2

struct C

class Test<T<_>> where T<_>: ITest =

    M(x: T<C>): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'ITest<T>' cannot be a partially applied constraint as it contains second-order generic type parameters.",
                """
class Test<T<_>> where T<_>: ITest =
                             ^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Second-order generic constraint should fail because of constraints on generic type constructor 3``() =
    let src =
        """
interface ITest<T1, T2> where T1: struct

class C

class Test<T<_, _>> where T<_, _>: ITest =

    M(x: T<C, C>): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'C' is missing the constraint 'struct'.",
                """
    M(x: T<C, C>): () = ()
           ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Second-order generic constraint should fail because of constraints on generic type constructor 4``() =
    let src =
        """
interface ITest<T1, T2> where T2: struct

class C

class Test<T<_, _>> where T<_, _>: ITest =

    M(x: T<C, C>): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type instantiation 'C' is missing the constraint 'struct'.",
                """
    M(x: T<C, C>): () = ()
              ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Recursive constraint interface should compile``() =
    let src =
        """
interface ITest<Z> where Z: ITest<Z>

test<T>(): () where T: ITest<T> = ()
        """
    Oly src
    |> shouldCompile
    |> ignore
