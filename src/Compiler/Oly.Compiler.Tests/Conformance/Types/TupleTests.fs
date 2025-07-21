module Conformance.Types.TupleTests

open Xunit
open TestUtilities

[<Fact>]
let ``Higher-kind with tuple type should compile``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

class C<T>

M<Ref1<_>, T>(f: (Ref1<T>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x:C<(int32, float32)>) -> ())
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``Higher-kind with tuple type should compile 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

M<Ref1<_>, T...>(f: (Ref1<T...>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: __oly_tuple<(int32, float32)>) -> ())
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``Higher-kind with tuple type should compile 3``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

M<Ref1<_>, T...>(f: (Ref1<T...>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: (int32, float32)) -> ())
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``Higher-kind with tuple type should compile 4``() =
    let src =
        """
M<Ref1<_>, T>(f: (Ref1<T>) -> ()): () where Ref1<_>: __oly_tuple =
    f(unchecked default)
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``Higher-kind with tuple type should compile 5``() =
    let src =
        """
M<Ref1<_>, T>(f: (Ref1<(T, T)>) -> ()): () where Ref1<_>: __oly_tuple =
    f(unchecked default)
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``Higher-kind with tuple type should error``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

M<Ref1<_>, T>(f: (Ref1<T>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: __oly_tuple<(int32, float32)>) -> ())
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '(?Ref1<__oly_tuple>) -> ()' but is '((int32, float32)) -> ()'.",
                """
    M<_, (int32, float32)>((x: __oly_tuple<(int32, float32)>) -> ())
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Higher-kind with tuple type should error 2``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

M<Ref1<_>, T>(f: (Ref1<T>) -> ()): () =
    f(unchecked default)

main(): () =
    M<_, (int32, float32)>((x: (int32, float32)) -> ())
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '(?Ref1<__oly_tuple>) -> ()' but is '((int32, float32)) -> ()'.",
                """
    M<_, (int32, float32)>((x: (int32, float32)) -> ())
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

// REVIEW: Because Ref1 is constrained to construct of __oly_tuple that has a a variadic type parameter,
//         we might consider it legal to do Ref1<T, T> or more type arguments as it is constrained to a variadic.
[<Fact>]
let ``Higher-kind with tuple type should error 3``() =
    let src =
        """
M<Ref1<_>, T>(f: (Ref1<T, T>) -> ()): () where Ref1<_>: __oly_tuple =
    f(unchecked default)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Second-order type parameter 'Ref1<_>' expected to have an arity of 1 but got 2.",
                """
M<Ref1<_>, T>(f: (Ref1<T, T>) -> ()): () where Ref1<_>: __oly_tuple =
                  ^^^^
"""
            )
            ("Expected '1' type argument(s) but got '2'.",
                """
M<Ref1<_>, T>(f: (Ref1<T, T>) -> ()): () where Ref1<_>: __oly_tuple =
                      ^^^^^^
"""
            )
        ]
    |> ignore