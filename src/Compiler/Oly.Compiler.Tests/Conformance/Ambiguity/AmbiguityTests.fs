module Conformance.AmbiguityTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Should compile 1``() =
    let src =
        """
interface Functor<F<_>> =

    static abstract fmap<A, B>(ab: A -> B, fa: F<A>): F<B>

fmap<F<_>, A, B>(ab: A -> B, fa: F<A>): F<B> where F: Functor<F> =
    F.fmap<_, _>(ab, fa)

fmap<F<_>, A>(aa: A -> A, fa: F<A>): F<A> where F: Functor<F> =
    fmap<F, A, A>(aa, fa)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should compile 2``() =
    let src =
        """
interface Functor<F<_>> =

    static abstract fmap<A, B>(ab: A -> B, fa: F<A>): F<B>

fmap<F<_>, A, B>(ab: A -> B, fa: F<A>): F<B> where F: Functor<F> =
    F.fmap<_, _>(ab, fa)

fmap<F<_>, A>(aa: A -> A, fa: F<A>): F<A> where F: Functor<F> =
    fmap<_, _, _>(aa, fa)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should compile 3``() =
    let src =
        """
class Test =
    field x: __oly_int32

    new() = { x = 1 }

Test() : () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should compile 4``() =
    let src =
        """
interface IA =

    a(): ()

class B =
    implements IA

    a(): () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should compile 5``() =
    let src =
        """
interface Functor<F<_>> =

    static abstract fmap<A, B>(ab: A -> B, fa: F<A>): F<B>

fmap<F<_>, A, B>(ab: A -> B, fa: F<A>): F<B> where F: Functor<F> =
    F.fmap(ab, fa)

fmap<F<_>, A>(aa: A -> A, fa: F<A>): F<A> where F: Functor<F> =
    fmap<_, _, _>(aa, fa)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Should not compile 1``() =
    let src =
        """
class Test =
    field x: __oly_int32
    field x: __oly_int32

    new() = { x = 1 }
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'field x: __oly_int32' has duplicate member definitions.",
            """
    field x: __oly_int32
          ^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Should not compile 2``() =
    let src =
        """
class Test =
    field x: __oly_int32
    
    x() : ()

    new() = { x = 1 }
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("The function 'x' must have an implementation.",
            """
    x() : ()
    ^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Should not compile 3``() =
    let src =
        """
interface Functor<F<_>> =

    static abstract fmap<A, B>(ab: A -> B, fa: F<A>) : F<B>

fmap<F<_>, A, B>(ab: A -> B, fa: F<A>) : F<B> where F<_> : Functor =
    F<_>.fmap<_, _>(ab, fa)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Functor<F>' cannot be a partially applied constraint as it contains second-order generic type parameters.",
            """
fmap<F<_>, A, B>(ab: A -> B, fa: F<A>) : F<B> where F<_> : Functor =
                                                           ^^^^^^^
"""
        )
        ("Type parameter '?F<_>' was unable to be inferred.",
            """
    F<_>.fmap<_, _>(ab, fa)
    ^^^^^^^^^^^^^^^
"""
        )
        ("Unable to infer type at this location.",
            """
    F<_>.fmap<_, _>(ab, fa)
         ^^^^^^^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Should not compile 4``() =
    let src =
        """
Test<T>(): () = ()
Test<T>(): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'static Test<T>(): ()' has duplicate member definitions.",
            """
Test<T>(): () = ()
^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Should not compile 5``() =
    let src =
        """
abstract class A =

    a(): () = ()

class B =
    inherits A

    a(): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'a(): ()' has duplicate member definitions.",
            """
    a(): () = ()
    ^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Should not compile 6``() =
    let src =
        """
class B =
    inherits A

    a(): () = ()

abstract class A =

    a(): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'a(): ()' has duplicate member definitions.",
            """
    a(): () = ()
    ^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Complex overload should compile``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test(f: () -> Wrapper<() -> __oly_int32>): () = ()

main(): () =
    test(() -> Wrapper(1))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 2``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test(f: () -> Wrapper<() -> __oly_int32>): () = ()

main(): () =
    test(() -> Wrapper(() -> 1))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 3``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test(f: () -> Wrapper<() -> __oly_int32>): () = ()
test(f: () -> Wrapper<Wrapper<() -> __oly_int32>>): () = ()

main(): () =
    test(() -> Wrapper(1))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 4``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test(f: () -> Wrapper<() -> __oly_int32>): () = ()
test(f: () -> Wrapper<Wrapper<() -> __oly_int32>>): () = ()

main(): () =
    test(() -> Wrapper(() -> 1))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 5``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test(f: () -> Wrapper<() -> __oly_int32>): () = ()
test(f: () -> Wrapper<Wrapper<() -> __oly_int32>>): () = ()

main(): () =
    test(() -> Wrapper(Wrapper(() -> 1)))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 6``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test(f: () -> Wrapper<() -> __oly_int32>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    test(() -> Wrapper(Wrapper(() -> 1)))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 7``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test<T>(f: () -> Wrapper<() -> T>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    test(() -> Wrapper(Wrapper(() -> 1)))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 7 - test symbol``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test<T>(f: () -> Wrapper<() -> T>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    ~^~test(() -> Wrapper(Wrapper(() -> 1)))
        """
    src
    |> hasSymbolSignatureTextByCursor "static test<T>(f: () -> Wrapper<Wrapper<() -> T>>): ()"

[<Fact>]
let ``Complex overload should compile 8``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test<T>(f: () -> Wrapper<T>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    test(() -> Wrapper(Wrapper(() -> 1)))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 8 - test symbol``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test<T>(f: () -> Wrapper<T>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    ~^~test(() -> Wrapper(Wrapper(() -> 1)))
        """
    src
    |> hasSymbolSignatureTextByCursor "static test<T>(f: () -> Wrapper<Wrapper<() -> T>>): ()"

[<Fact>]
let ``Complex overload should compile 9``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test<T>(f: () -> Wrapper<T>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    test(() -> Wrapper(1))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 9 - test symbol``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test<T>(f: () -> Wrapper<T>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    ~^~test(() -> Wrapper(1))
        """
    src
    |> hasSymbolSignatureTextByCursor "static test(f: () -> Wrapper<__oly_int32>): ()"

[<Fact>]
let ``Complex overload should compile 10``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test<T>(f: () -> Wrapper<T>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    test(() -> Wrapper(1))
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Complex overload should compile 10 - test symbol``() =
    let src =
        """
module Test

class Wrapper<T> =

    field Value: T

    new(value: T) = { Value = value }

test(f: () -> Wrapper<__oly_int32>): () = ()
test<T>(f: () -> Wrapper<T>): () = ()
test<T>(f: () -> Wrapper<Wrapper<() -> T>>): () = ()

main(): () =
    ~^~test(() -> Wrapper(1.0))
        """
    src
    |> hasSymbolSignatureTextByCursor "static test<T>(f: () -> Wrapper<T>): ()"

[<Fact>]
let ``Generic overload should not compile``() =
    let src =
        """
module Test

#[intrinsic("int32")]
alias int32

#[null]
class Option<T> =
    public field Value: T
    new(value: T) = { Value = value }

Some<T>(value: T): Option<T> =
    Option(value)

None<T>(): Option<T> =
    null

test(option: Option<int32>): () =
    ()

test(option: Option<Option<int32>>): () =
    () 

main(): () =
    test(None())
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("Type parameter '?T' was unable to be inferred.",
            """
    test(None())
         ^^^^
"""
        )
        ("'test' has ambiguous functions.",
            """
    test(None())
    ^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Indexer operator example with struct byref and mutable byrefs``() =
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
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Indexer operator example with struct byref and mutable byrefs 2``() =
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
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let mutable s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Indexer operator example with struct byref and mutable byrefs 3``() =
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
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

main(): () =
    let s = Test<__oly_int32>()
    let x = s[0]
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ambiguous patterns found``() =
    let src =
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
    match(value)
    | Some(x) => ()
    | _ => ()

main(): () =
    test(Some(1))
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Some' has ambiguous functions.",
            """
    | Some(x) => ()
      ^^^^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Overloaded prefix (*) should compile``() =
    let src =
        """
#[intrinsic("void")]
alias void

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("unsafe_cast")]
(*)<T>(void*): byref<T>

#[intrinsic("unsafe_cast")]
(*)<T>(T*): byref<T>

f(ptr: void*): () =
    let x: int32 = *ptr
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Type declaration should error``() =
    let src =
        """
class (*)<T>

class (*)<T> // this one
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'T*' has already been declared.",
            """
class (*)<T> // this one
       ^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Type declaration should error 2``() =
    let src =
        """
class (*)<T>

class (*)<T> // this one

class (*)<T> // this other one
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'T*' has already been declared.",
            """
class (*)<T> // this one
       ^
"""
        )
        ("'T*' has already been declared.",
            """
class (*)<T> // this other one
       ^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Type declaration should error 3``() =
    let src =
        """
#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("native_ptr")]
alias (*)<T>
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'T*' has already been declared.",
            """
alias (*)<T>
       ^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Overloaded infix (+) on use with un-annotated int literal should compile``() =
    let src =
        """
#[intrinsic("void")]
alias void

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

#[intrinsic("unsafe_cast")]
uint32(int32): uint32

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

#[intrinsic("unsafe_cast")]
nint<T>(T*): nint

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("unsafe_cast")]
nuint(int32): nuint

#[intrinsic("unsafe_cast")]
nuint(uint32): nuint

#[intrinsic("unsafe_cast")]
nuint(uint8): nuint

#[intrinsic("unsafe_cast")]
nuint(void*): nuint

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

#[intrinsic("unsafe_cast")]
ToVoidPtr(int32): void*

#[intrinsic("unsafe_cast")]
ToVoidPtr(nuint): void*

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
#[intrinsic("add")]
(+)(nuint, nuint): nuint

(+)(ptr: void*, value: nuint): void* =
    unchecked default

(+)(ptr: void*, value: int32): void* =
    unchecked default

(+)(ptr: void*, value: uint32): void* =
    unchecked default

(+)(ptr: void*, value: uint8): void* =
    unchecked default

f(ptr: void*): void* =
    ptr + 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Overloaded infix (+) on use with un-annotated int literal should compile 2``() =
    let src =
        """
#[intrinsic("void")]
alias void

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

#[intrinsic("unsafe_cast")]
uint32(int32): uint32

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

#[intrinsic("unsafe_cast")]
nint<T>(T*): nint

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("unsafe_cast")]
nuint(int32): nuint

#[intrinsic("unsafe_cast")]
nuint(uint32): nuint

#[intrinsic("unsafe_cast")]
nuint(uint8): nuint

#[intrinsic("unsafe_cast")]
nuint(void*): nuint

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

#[intrinsic("unsafe_cast")]
ToVoidPtr(int32): void*

#[intrinsic("unsafe_cast")]
ToVoidPtr(nuint): void*

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
#[intrinsic("add")]
(+)(nuint, nuint): nuint

(+)(ptr: void*, value: nuint): void* =
    unchecked default

(+)(ptr: void*, value: uint8): void* =
    unchecked default

f(ptr: void*): void* =
    ptr + 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Overload should have ambiguity when used for inference``() =
    let src =
        """
struct Vector3

struct Matrix

#[open]
module Ops =
    
    op_Multiply(x: Matrix, y: Vector3): Matrix = default

    op_Multiply(x: Vector3, y: Matrix): Vector3 = default

main() : () =
    let f(x, y) = op_Multiply(x, y)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'op_Multiply' has ambiguous functions.",
            """
    let f(x, y) = op_Multiply(x, y)
                  ^^^^^^^^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Overload should have ambiguity when used for inference for witness``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

struct Vector3

struct Matrix

#[open]
extension Ops =
    inherits object
    
    static op_Multiply(x: Matrix, y: Vector3): Matrix = default

    static op_Multiply(x: Vector3, y: Matrix): Vector3 = default

multiply<T1, T2, T3, W>(x: T1, y: T2): T3 where W: { static op_Multiply(T1, T2): T3 } =
    W.op_Multiply(x, y)

main() : () =
    let f(x, y) = multiply<_, _, _, object>(x, y)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'op_Multiply' has ambiguous functions.",
            """
    let f(x, y) = multiply<_, _, _, object>(x, y)
                                    ^^^^^^
"""
        )
        ("'op_Multiply' has ambiguous functions.",
            """
    let f(x, y) = multiply<_, _, _, object>(x, y)
                                    ^^^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Overload should have ambiguity when used for inference for witness 2``() =
    let src =
        """
#[intrinsic("base_object")]
alias object

struct Vector3

struct Matrix

#[open]
extension Ops =
    inherits object
    
    static op_Multiply(x: Matrix, y: Vector3): Matrix = default

#[open]
extension Ops2 =
    inherits object

    static op_Multiply(x: Vector3, y: Matrix): Vector3 = default

multiply<T1, T2, T3, W>(x: T1, y: T2): T3 where W: { static op_Multiply(T1, T2): T3 } =
    W.op_Multiply(x, y)

main() : () =
    let f(x, y) = multiply<_, _, _, object>(x, y)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'op_Multiply' has ambiguous functions.",
            """
    let f(x, y) = multiply<_, _, _, object>(x, y)
                                    ^^^^^^
"""
        )
        ("'op_Multiply' has ambiguous functions.",
            """
    let f(x, y) = multiply<_, _, _, object>(x, y)
                                    ^^^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Void* overload should work``() =
    let src =
        """
#[intrinsic("void")]
alias void

#[intrinsic("base_object")]
alias object

#[intrinsic("uint32")]
alias uint32

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("print")]
print(object): ()

#[intrinsic("unsafe_cast")]
(*)<T>(void*): byref<T>

#[intrinsic("unsafe_cast")]
(*)<T>(T*): byref<T>

#[intrinsic("unsafe_cast")]
UnsafeCast<T>(object): T

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

main(): () =
    let mutable result: uint32 = 123
    let resultAddr: int32* = UnsafeCast(&&result)
    let result: int32 = *resultAddr
    print(result)
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Pattern match ambiguity with constructor``() =
    let src =
        """
class A =

    class B

    pattern B(x: A): A = x

main(): () =
    let a = A()
    match (a)
    | A.B(y) => ()
    | _ => ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Correct overload selected for load_function_ptr intrinsic``() =
    let src =
        """
#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

test(): () = ()

main(): () =
    let ~^~ptr = &&test
        """
    src
    |> hasSymbolSignatureTextByCursor "ptr: static () -> ()"

[<Fact>]
let ``Correct intrinsic overload selected for enum``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("equal")]
(==)<T>(T, T): bool where T: struct

(==)<T1, T2, T3>(value1: T1, value2: T2): T3 = unchecked default

enum E =
    | A
    | B
    | C

main(): () =
    let x = E.A ~^~== E.A
        """
    src
    |> hasSymbolSignatureTextByCursor "(==)(int32, int32): bool"

[<Fact>]
let ``Correct overloaded selected for function pointer``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[intrinsic("and")]
(&&)(bool, bool): bool

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("native_ptr")]
alias (*)<T>

#[blittable]
testFunc(): () = ()

main(): () =
    let ~^~ptr = &&testFunc
        """
    src
    |> hasSymbolSignatureTextByCursor "ptr: static blittable () -> ()"

[<Fact>]
let ``Correct overloaded selected for function pointer 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("and")]
(&&)(bool, bool): bool

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("native_ptr")]
alias (*)<T>

#[blittable]
testFunc(x: int32): int32 = x

main(): () =
    let ~^~ptr = &&testFunc
        """
    src
    |> hasSymbolSignatureTextByCursor "ptr: static blittable int32 -> int32"

[<Fact>]
let ``Correct overloaded selected for function pointer 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("and")]
(&&)(bool, bool): bool

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("native_ptr")]
alias (*)<T>

#[blittable]
testFunc(x: int32, y: int32): int32 = x

main(): () =
    let ~^~ptr = &&testFunc
        """
    src
    |> hasSymbolSignatureTextByCursor "ptr: static blittable (int32, int32) -> int32"

[<Fact>]
let ``Generic class that handles functions that have ambiguity should error``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

abstract default class A<T, U> =

    abstract default Test(x: T): () = ()

    abstract default Test(x: U): () = ()

class Test =
    inherits A<int32, int32>

    overrides Test(x: int32): () = ()

main(): () =
    let t = Test()
    let t = t: A<int32, int32>
    t.Test(123)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Test' has ambiguous functions.",
            """
    t.Test(123)
      ^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Generic class that handles functions that have ambiguity should not error``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

abstract default class A<T, U> =

    abstract default Test(x: T): () = ()

    abstract default Test(x: U): () = ()

class Test =
    inherits A<int32, int32>

    overrides Test(x: int32): () = ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Multiple type extensions on the same type should error``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

interface IA

class A

#[open]
extension AExtension =
    inherits A
    implements IA

#[open]
extension AExtension2 =
    inherits A
    implements IA

test<T>(t: T): () where T: IA = ()

main(): () =
    let a = A()
    test(a)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("Unable to solve due to ambiguity of the possibly resolved constraints:\n    AExtension\n    AExtension2\n\nUse explicit type annotations to disambiguate.",
            """
    test(a)
    ^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Multiple type extensions on the same type with same function should error``() =
    let src =
        """
class A

#[open]
extension AExtension =
    inherits A

    Test(): () = ()

#[open]
extension AExtension2 =
    inherits A

    Test(): () = ()

main(): () =
    let a = A()
    a.Test()
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Test' has ambiguous functions.",
            """
    a.Test()
      ^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Multiple type extensions on the same type with same function should error 2``() =
    let src =
        """
class A

#[open]
extension AExtension =
    inherits A

    Test(): () = ()

#[open]
extension AExtension2 =
    inherits A

    Test(): () = ()

test<T>(t: T): () where T: { Test(): () } = ()

main(): () =
    let a = A()
    test(a)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Test' has ambiguous functions.",
            """
    test(a)
    ^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Multiple type extensions on the same type with same function should error 3``() =
    let src =
        """
interface IA

class A

#[open]
extension AExtension =
    inherits A

    Test(): () = ()

#[open]
extension AExtension2 =
    inherits A
    implements IA

    Test(): () = ()

main(): () =
    let a = A()
    a.Test()
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Test' has ambiguous functions.",
            """
    a.Test()
      ^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Multiple type extensions on the same type with same function should error 4``() =
    let src =
        """
interface IA

class A

#[open]
extension AExtension =
    inherits A

    Test(): () = ()

#[open]
extension AExtension2 =
    inherits A
    implements IA

    Test(): () = ()

test<T>(t: T): () where T: { Test(): () } = ()

main(): () =
    let a = A()
    test(a)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Test' has ambiguous functions.",
            """
    test(a)
    ^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Multiple type extensions on the same type with same function should error 5``() =
    let src =
        """
interface IA

class A

#[open]
extension AExtension =
    inherits A

    Test(): () = ()

#[open]
extension AExtension2 =
    inherits A
    implements IA

    Test(): () = ()

test<T>(t: T): () where T: IA, { Test(): () } = ()

main(): () =
    let a = A()
    test(a)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Test' has ambiguous functions.",
            """
    test(a)
    ^^^^
"""
        )
    ]
    |> ignore

[<Fact>]
let ``Multiple type extensions on the same type with same function should error 6``() =
    let src =
        """
interface IA

class A

#[open]
extension AExtension =
    inherits A

    Test(): () = ()

#[open]
extension AExtension2 =
    inherits A
    implements IA

    Test(): () = ()

test<T>(t: T): () where T: { Test(): () }, IA = ()

main(): () =
    let a = A()
    test(a)
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'Test' has ambiguous functions.",
            """
    test(a)
    ^^^^
"""
        )
    ]
    |> ignore