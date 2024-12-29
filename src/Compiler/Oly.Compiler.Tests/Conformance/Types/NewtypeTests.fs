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
    |> withCompile
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

[<Fact>]
let ``Newtype should never allow attributes on the pincipal field``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

newtype A =
    #[open]
    public field Value: int32

main(): () =
    ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Attributes are not allowed on the principal field for a newtype.",
                """
    #[open]
    ^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Newtype should never allow getting access to underlying types fields``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

struct TestStruct =
    public field X: int32 = 0

newtype A =
    field value: TestStruct

main(): () =
    let mutable a = A(TestStruct())
    let value = a.X
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Member 'X' does not exist on type 'A'.",
                """
    let value = a.X
                  ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Newtype without a field should fail``() =
    let src =
        """
newtype A
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected field definition signature for newtype 'A' as the first expression.",
             """
newtype A
        ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Newtype should work with a non-trait shape constraint - NOTE - THIS USED TO NOT WORK``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

newtype NewInt =
    public field Value: int32

    Doot(): int32 = 8

M<T>(x: T): () where T: { Doot(): int32 } =
    print(x.Doot())

main(): () =
    let ns = NewInt(2)
    M(ns)
        """
    Oly src
    |> shouldCompile
