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
    x: __oly_int32

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
    x: __oly_int32
    x: __oly_int32

    new() = { x = 1 }
        """
    Oly src
    |> withErrorHelperTextDiagnostics [
        ("'x: __oly_int32' has duplicate member definitions.",
            """
    x: __oly_int32
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
    x: __oly_int32
    
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
        ("'x(): ()' has duplicate member definitions.",
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
class A =

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

class A =

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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

    Value: T

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
    Value: T
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
    Value: T
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

#[intrinsic("cast")]
(*)<T>(void*): byref<T>

#[intrinsic("cast")]
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

#[intrinsic("cast")]
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

#[intrinsic("cast")]
nint<T>(T*): nint

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("cast")]
nuint(int32): nuint

#[intrinsic("cast")]
nuint(uint32): nuint

#[intrinsic("cast")]
nuint(uint8): nuint

#[intrinsic("cast")]
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

#[intrinsic("cast")]
ToVoidPtr(int32): void*

#[intrinsic("cast")]
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

#[intrinsic("cast")]
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

#[intrinsic("cast")]
nint<T>(T*): nint

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("cast")]
nuint(int32): nuint

#[intrinsic("cast")]
nuint(uint32): nuint

#[intrinsic("cast")]
nuint(uint8): nuint

#[intrinsic("cast")]
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

#[intrinsic("cast")]
ToVoidPtr(int32): void*

#[intrinsic("cast")]
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

#[intrinsic("cast")]
(*)<T>(void*): byref<T>

#[intrinsic("cast")]
(*)<T>(T*): byref<T>

#[intrinsic("cast")]
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