module Conformance.PrivateByDefaultTests

open Xunit
open TestUtilities

[<Fact>]
let ``Should error as it is private by default``() =
    """
#default_accessor "private"

struct S =

    ThisIsNowPrivate(): () = ()

main(): () =
    let s = S()
    s.ThisIsNowPrivate()
    """
    |> OlyPrivateByDefault
    |> hasErrorHelperTextDiagnostics
        [
            ("Member 'ThisIsNowPrivate' does not exist on type 'S'.",
                """
    s.ThisIsNowPrivate()
      ^^^^^^^^^^^^^^^^
"""
            )
        ]

[<Fact>]
let ``Should not error as it is public by default``() =
    """
#default_accessor "public"

struct S =

    ThisIsNowPrivate(): () = ()

main(): () =
    let s = S()
    s.ThisIsNowPrivate()
    """
    |> Oly
    |> shouldCompile