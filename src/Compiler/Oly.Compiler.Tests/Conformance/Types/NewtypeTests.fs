module Conformance.Types.NewtypeTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Newtype should pass``() =
    let src =
        """
newtype A =
    field Value: __oly_int32
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Newtype should never allow getting base type methods``() =
    let src =
        """
struct S =
    GetValue(): __oly_int32 = 0

newtype A =
    field Value: S

main(): () =
    let a = A(S())
    let value = a.GetValue()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Member 'GetValue' does not exist on type 'A'.",
                """
    let value = a.GetValue()
                  ^^^^^^^^
"""
            )
        ]
    |> ignore