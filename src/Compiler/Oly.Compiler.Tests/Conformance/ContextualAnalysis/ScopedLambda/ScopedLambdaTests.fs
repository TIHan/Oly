module Conformance.ContextualAnalysis.ScopedLambdaTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Scoped lambda should fail as it cannot be captured inside a non-scoped lambda``() =
    """
M(f: () -> ()): () = f()

M2(f: scoped () -> ()): () =
    M(() -> f())

main(): () = ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Value cannot be captured.",
                """
    M(() -> f())
            ^
"""
            )
        ]
