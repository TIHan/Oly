module Conformance.Types.HigherKindTests

open Xunit
open TestUtilities

[<Fact>]
let ``Higher-kind constrained to a type with a variadic type parameter should pass``() =
    let src =
        """
abstract class C<T...>

M<Ref1<_>, T>(f: (Ref1<T>) -> ()): () where Ref1<_>: C =
    f(unchecked default)
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``Higher-kind constrained to a type with a variadic type parameter should pass 2``() =
    let src =
        """
abstract class C<T...>

M<Ref1<_>, T>(f: (Ref1<(T, T)>) -> ()): () where Ref1<_>: C =
    f(unchecked default)
        """
    Oly src
    |> shouldCompile

// REVIEW: Because Ref1 is constrained to construct of C that has a a variadic type parameter,
//         we might consider it legal to do Ref1<T, T> or more type arguments as it is constrained to a variadic.
[<Fact>]
let ``Higher-kind constrained to a type with a variadic type parameter should fail``() =
    let src =
        """
abstract class C<T...>

M<Ref1<_>, T>(f: (Ref1<T, T>) -> ()): () where Ref1<_>: C =
    f(unchecked default)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Second-order type parameter 'Ref1<_>' expected to have an arity of 1 but got 2.",
                """
M<Ref1<_>, T>(f: (Ref1<T, T>) -> ()): () where Ref1<_>: C =
                  ^^^^
"""
            )
            ("Expected '1' type argument(s) but got '2'.",
                """
M<Ref1<_>, T>(f: (Ref1<T, T>) -> ()): () where Ref1<_>: C =
                      ^^^^^^
"""
            )
        ]
    |> ignore