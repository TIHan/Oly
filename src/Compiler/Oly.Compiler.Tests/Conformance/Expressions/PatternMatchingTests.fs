module Conformance.Expressions.PatternMatchingTests

open Xunit
open TestUtilities

[<Fact>]
let ``Simple pattern match should fail``() =
    let src =
        """
test(x: __oly_int32): () =
    match (x)
    | 1 => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Match is not exhaustive.", "
    match (x)
    ^^^^^
")
        ]
    |> ignore

[<Fact>]
let ``Simple pattern match should fail 2``() =
    let src =
        """
test(x: __oly_int32): () =
    match (x)
    | _ => ()
    | 1 => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Specified pattern will never be matched.", "
    | 1 => ()
      ^
")
        ]
    |> ignore

[<Fact>]
let ``Simple pattern match should fail 3``() =
    let src =
        """
test(x: __oly_int32, y: __oly_int32): () =
    match (x)
    | _ when (__oly_equal(y, 3)) => ()
    | 1 => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Match is not exhaustive.", "
    match (x)
    ^^^^^
")
        ]
    |> ignore

[<Fact>]
let ``Simple pattern match should fail 4``() =
    let src =
        """
test(x: __oly_int32, y: __oly_int32): () =
    match (x, y)
    | 1, 6 => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Match is not exhaustive.", "
    match (x, y)
    ^^^^^
");
        ]
    |> ignore

[<Fact>]
let ``Simple pattern match should fail 5``() =
    let src =
        """
test(x: __oly_int32, y: __oly_int32): () =
    match (x, y)
    | 1, 6 => ()
    | _, 8 => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Match is not exhaustive.", "
    match (x, y)
    ^^^^^
");
        ]
    |> ignore

[<Fact>]
let ``Simple pattern match should fail 6``() =
    let src =
        """
test(x: __oly_int32, y: __oly_int32): () =
    match (x, y)
    | 1, 6 => ()
    | _, 8 => ()
    | 4, _ => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Match is not exhaustive.", "
    match (x, y)
    ^^^^^
");
        ]
    |> ignore

[<Fact>]
let ``Active pattern should get correct symbol``() =
    let src =
        """
pattern addOne(x: __oly_int32): __oly_int32 = __oly_add(x, 1)
test(x: __oly_int32): () =
    match (x)
    | addOne(~^~y) => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "y: __oly_int32"

[<Fact>]
let ``Active pattern should get correct symbol 2``() =
    let src =
        """
pattern addOne(x: __oly_int32): __oly_int32 = __oly_add(x, 1)
test(x: __oly_int32): () =
    match (x)
    | ~^~addOne(y) => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "pattern addOne(x: __oly_int32): __oly_int32"

[<Fact>]
let ``Active pattern should get correct symbol 3``() =
    let src =
        """
pattern getFloat(x: __oly_int32): __oly_float32 = 5
test(x: __oly_int32): () =
    match (x)
    | getFloat(~^~y) => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "y: __oly_float32"

[<Fact>]
let ``Active pattern should get correct symbol 4``() =
    let src =
        """
pattern getFloat(x: __oly_int32): __oly_float32 = 5
test(x: __oly_int32): () =
    match (x)
    | ~^~getFloat(y) => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "pattern getFloat(x: __oly_int32): __oly_float32"

[<Fact>]
let ``Active pattern should compile``() =
    let src =
        """
pattern addOne(x: __oly_int32): __oly_int32 = __oly_add(x, 1)
test(x: __oly_int32): () =
    match (x)
    | addOne(y) => ()
    | _ => ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Use of pattern should error due to type arguments``() =
    let src =
        """
#[intrinsic("not_equal")]
(!==)<T>(T, T): __oly_bool where T: not struct, null

#[null]
class Option<T> =
    Value: T
    new(value: T) = { Value = value }

pattern Some<T>(option: Option<T>): T when (option !== null) =>
    option.Value

test(x: Option<__oly_int32>): () =
    match (x)
    | Some<__oly_utf16>(_) => ()
    | _ => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'Option<__oly_utf16>' but is 'Option<__oly_int32>'.", """
    | Some<__oly_utf16>(_) => ()
      ^^^^
""")
        ]
    |> ignore

[<Fact>]
let ``Use of pattern should error due to type arguments 2``() =
    let src =
        """
#[intrinsic("not_equal")]
(!==)<T>(T, T): __oly_bool where T: not struct, null

#[null]
class Option<T> =
    Value: T
    new(value: T) = { Value = value }

pattern Some<T>(option: Option<T>): T when (option !== null) =>
    option.Value

pattern Some<T>(option: Option<T>): (T, T) when SomeGuardTuple(option !== null) =>
    (option.Value, option.Value)

test(x: Option<__oly_int32>): () =
    match (x)
    | Some<__oly_utf16>(_) => ()
    | _ => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'Option<__oly_utf16>' but is 'Option<__oly_int32>'.", """
    | Some<__oly_utf16>(_) => ()
      ^^^^^^^^^^^^^^^^^
""")
        ]
    |> ignore

[<Fact>]
let ``Pattern definitions should error because guard has same name``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[null]
class Option<T> =
    Value: T
    new(value: T) = { Value = value }

pattern Some<T>(value: Option<T>): T when (value !== null) => value.Value

pattern Some<T>(value: Option<T>): (T, T) when (value !== null) =>
    (value.Value, value.Value)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'static guard_Some<T>(value: Option<T>): bool' has duplicate member definitions.", """
pattern Some<T>(value: Option<T>): (T, T) when (value !== null) =>
        ^^^^
""")
        ]
    |> ignore

[<Fact>]
let ``Literal pattern match should give the correct symbol``() =
    let src =
        """
test(t: __oly_int32): () =
    match (t)
    | ~^~1 => ()
    | _ => ()
        """
    let symbol = getSymbolByCursor src
    Assert.Equal(1, symbol.AsConstant.Value.AsInt32)

[<Fact>]
let ``Enum pattern match should give the correct symbol``() =
    let src =
        """
#[open]
enum Test =
    | ABC
    | DEF

test(t: Test): () =
    match (t)
    | ~^~ABC => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "constant ABC: Test = 0"

[<Fact>]
let ``Enum pattern match should give the correct symbol 2``() =
    let src =
        """
enum Test =
    | ABC
    | DEF

test(t: Test): () =
    match (t)
    | Test.~^~ABC => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "constant ABC: Test = 0"

[<Fact>]
let ``Enum pattern match should give the correct symbol 3``() =
    let src =
        """
enum Test =
    | ABC
    | DEF

test(t: Test): () =
    match (t)
    | ~^~Test.ABC => ()
    | _ => ()
        """
    src |> hasSymbolSignatureTextByCursor "Test"

[<Fact>]
let ``A non-pattern function definition with the use of 'when' should error``() =
    let src =
        """
abc(): () when (true) => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected 'pattern' with 'when'.", """
abc(): () when (true) => ()
^^^
""")
        ]
    |> ignore