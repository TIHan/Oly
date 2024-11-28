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
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Use of pattern should error due to type arguments``() =
    let src =
        """
#[intrinsic("not_equal")]
(!==)<T>(T, T): __oly_bool where T: not struct, null

#[null]
class Option<T> =
    public field Value: T
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
    public field Value: T
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
      ^^^^^^^^^^^^^^^^^^^^
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
    public field Value: T
    new(value: T) = { Value = value }

pattern Some<T>(value: Option<T>): T when (value !== null) => value.Value

pattern Some<T>(value: Option<T>): (T, T) when (value !== null) =>
    (value.Value, value.Value)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'static pattern_guard_Some<T>(value: Option<T>): bool' has duplicate member definitions.", """
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

[<Fact>]
let ``Let discard pattern should give correct signature of expressions below``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

main(): () =
    let _ = 1
    let ~^~x = 2
        """
    src |> hasSymbolSignatureTextByCursor "x: int32"

[<Fact>]
let ``Use of bare option type for pattern matching should error as it is not exhaustive``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[unmanaged(allocation_only)]
#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct

#[unmanaged(allocation_only)]
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[open]
newtype Option<T> where T: not struct =
    field Value: T

    static Some(value: T): Option<T> = Option(value)
    static None: Option<T> get = Option(unchecked default)

    pattern Some(option: Option<T>): T when (option.Value !== unchecked default) =>
        option.Value

    pattern None(option: Option<T>): () when (option.Value === unchecked default) =>
        ()

class C

main(): () =
    let cOpt = Some(C())
    match (cOpt)
    | Some(_) => ()
    | None => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Match is not exhaustive.", """
    match (cOpt)
    ^^^^^
""")
        ]
    |> ignore

[<Fact>]
let ``Use of bare option type for pattern matching should error as it is not exhaustive 2``() =
    let src =
        """
#[intrinsic("bool")]
alias bool

#[unmanaged(allocation_only)]
#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct

#[unmanaged(allocation_only)]
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[open]
newtype Option<T> where T: not struct =
    field Value: T

    static Some(value: T): Option<T> = Option(value)
    static None: Option<T> get = Option(unchecked default)

    pattern Some(option: Option<T>): T when (option.Value !== unchecked default) =>
        option.Value

    pattern None(option: Option<T>): () when (option.Value === unchecked default) =>
        ()

class C

main(): () =
    let cOpt = Some(C())
    match (cOpt)
    | None => ()
    | Some(_) => ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Match is not exhaustive.", """
    match (cOpt)
    ^^^^^
""")
        ]
    |> ignore

[<Fact>]
let ``Ignore multiple patterns should compile``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

pattern P(x: int32): (x: int32, y: int32) when (true) =>
    (x, x)

main(): () =
    match (1)
    | P(y, _) =>
        ()
    | _ =>
        ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ignore multiple patterns should compile 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

pattern P(x: int32): (x: int32, y: int32, z: int32) when (true) =>
    (x, x, x)

main(): () =
    match (1)
    | P(y, _, _) =>
        ()
    | _ =>
        ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Ignore multiple patterns should compile 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

pattern P(x: int32): (x: int32, y: int32, z: int32, w: int32) when (true) =>
    (x, x, x, x)

main(): () =
    match (1)
    | P(y, _, _, _) =>
        ()
    | _ =>
        ()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Pattern local in lambda should error on same local name``() =
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
        ((x, y), x) ->
            print(x)
            print(y)
    )
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("'x' has already been declared in the pattern set.", """
        ((x, y), x) ->
          ^
""")
        ]
    |> ignore

[<Fact>]
let ``Pattern local in lambda should error on same local name 2``() =
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

ForEach<T>(xs: T[], f: (int32, T) -> ()): () =
    For(getLength(xs), i -> f(5, xs[i]))

main(): () =
    let xs = [(1, 2)]
    ForEach(xs, 
        (x, (x, y)) ->
            print(x)
            print(y)
    )
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("'x' has already been declared in the pattern set.", """
        (x, (x, y)) ->
             ^
""")
        ]
    |> ignore


[<Fact>]
let ``Pattern local in lambda should error on same local name 3``() =
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

ForEach<T>(xs: T[], f: (T, T) -> ()): () =
    For(getLength(xs), i -> f(xs[i], xs[i]))

main(): () =
    let xs = [(1, 2)]
    ForEach(xs, 
        ((x, y), (x, y)) ->
            print(x)
            print(y)
    )
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("'x' has already been declared in the pattern set.", """
        ((x, y), (x, y)) ->
                  ^
""");
            ("'y' has already been declared in the pattern set.", """
        ((x, y), (x, y)) ->
                     ^
""")
        ]
    |> ignore

[<Fact>]
let ``Tuple pattern in signature but is actually a type but should fail as this is recognized as a pattern``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print((int32, int32)): ()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Patterns are not allowed for signatures.", """
print((int32, int32)): ()
      ^^^^^^^^^^^^^^
""")
        ]
    |> ignore

[<Fact>]
let ``Discard pattern in signature should compile``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(_: (int32, int32)): ()
    """
    |> Oly
    |> shouldCompile