module Oly.Targets.DotNet.AnalyzerTests

open Xunit
open WorkspaceUtilities

let buildHasErrors expected src =
    buildHasErrorsWithPrefix (DotNetTarget()) expected src

[<Fact>]
let ``Should error with ambiguous overloads for exported function for inref/outref/byref``() =
    """
#target "dotnet: netstandard2.1"
#library

namespace Test

class A =

    M(x: byref<int32>): () = ()
    M(x: inref<int32>): () = ()
    M(x: outref<int32>): () = ()

#[export]
class ExportedA =

    M(y: byref<int32>): () = ()
    M(y: inref<int32>): () = ()

#[export]
class ExportedA2 =

    M(z: byref<int32>): () = ()
    M(z: outref<int32>): () = ()

#[export]
class ExportedA3 =

    M(w: inref<int32>): () = ()
    M(w: outref<int32>): () = ()
    """
    |> buildHasErrors
        [
            ("DotNet: Unable to disambiguate types on function 'M(y: byref<int32>): ()'.",
                """
    M(y: byref<int32>): () = ()
    ^
"""
            )
            ("DotNet: Unable to disambiguate types on function 'M(y: inref<int32>): ()'.",
                """
    M(y: inref<int32>): () = ()
    ^
"""
            )
            ("DotNet: Unable to disambiguate types on function 'M(z: byref<int32>): ()'.",
                """
    M(z: byref<int32>): () = ()
    ^
"""
            )
            ("DotNet: Unable to disambiguate types on function 'M(z: outref<int32>): ()'.",
                """
    M(z: outref<int32>): () = ()
    ^
"""
            )
            ("DotNet: Unable to disambiguate types on function 'M(w: inref<int32>): ()'.",
                """
    M(w: inref<int32>): () = ()
    ^
"""
            )
            ("DotNet: Unable to disambiguate types on function 'M(w: outref<int32>): ()'.",
                """
    M(w: outref<int32>): () = ()
    ^
"""
            )
        ]


