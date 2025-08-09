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
            ("DOTNET: Unable to disambiguate types on function 'M(y: byref<int32>): ()'.",
                """
    M(y: byref<int32>): () = ()
    ^
"""
            )
            ("DOTNET: Unable to disambiguate types on function 'M(y: inref<int32>): ()'.",
                """
    M(y: inref<int32>): () = ()
    ^
"""
            )
            ("DOTNET: Unable to disambiguate types on function 'M(z: byref<int32>): ()'.",
                """
    M(z: byref<int32>): () = ()
    ^
"""
            )
            ("DOTNET: Unable to disambiguate types on function 'M(z: outref<int32>): ()'.",
                """
    M(z: outref<int32>): () = ()
    ^
"""
            )
            ("DOTNET: Unable to disambiguate types on function 'M(w: inref<int32>): ()'.",
                """
    M(w: inref<int32>): () = ()
    ^
"""
            )
            ("DOTNET: Unable to disambiguate types on function 'M(w: outref<int32>): ()'.",
                """
    M(w: outref<int32>): () = ()
    ^
"""
            )
        ]

[<Fact>]
let ``Calling an UnmanagedCallersOnly should fail``() =
    let src =
        """
#target "dotnet: net8"

namespace A

open System.Runtime.InteropServices
open System.Runtime.CompilerServices

#[open]
module Test =

    #[intrinsic("constant")]
    #[import("intrinsic-CLR", "", "typeof")]
    typeof<T>: System.Type

    #[UnmanagedCallersOnly() { CallConvs = [typeof<CallConvCdecl>] }]
    test(x: __oly_int32): __oly_int32 = x

module Main =

    #[intrinsic("print")]
    print(__oly_object): ()

    main(): () =
        let result = test(1234)
        print(result)
        """
    src |> buildHasErrors
        [
            ("DOTNET: Cannot call an 'UnmanagedCallersOnly' function.",
                """
        let result = test(1234)
                     ^^^^
"""
            )
        ]

[<Fact>]
let ``Getting pointer of UnmanagedCallersOnly should pass``() =
    let src =
        """
#target "dotnet: net8"

namespace A

open System.Runtime.InteropServices
open System.Runtime.CompilerServices

#[open]
module Test =

    #[intrinsic("constant")]
    #[import("intrinsic-CLR", "", "typeof")]
    typeof<T>: System.Type

    #[UnmanagedCallersOnly() { CallConvs = [typeof<CallConvCdecl>] }]
    test(x: __oly_int32): __oly_int32 = x

module Main =

    #[intrinsic("native_ptr")]
    alias (*)<T>

    #[intrinsic("unsafe_address_of")]
    (&&)<T>(T): T*

    main(): () =
        let fnptr = &&test
        """
    src |> buildHasErrors []