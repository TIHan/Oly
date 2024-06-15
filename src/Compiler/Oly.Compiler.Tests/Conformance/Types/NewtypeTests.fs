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
let ``Newtype should never allow getting address of field``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

newtype A =
    public field Value: int32

main(): () =
    let mutable a = A(1)
    let value = &a.Value
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            // TODO: Fix compiler to not duplicate the error.
            ("Newtypes do not allow getting the address of its field.",
                """
    let value = &a.Value
                ^^^^^^^^
"""
            )
            ("Newtypes do not allow getting the address of its field.",
                """
    let value = &a.Value
                ^^^^^^^^
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
