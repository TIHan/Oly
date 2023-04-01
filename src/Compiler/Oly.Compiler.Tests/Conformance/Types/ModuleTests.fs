module Conformance.Types.ModuleTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Top-level module with nothing in it should pass``() =
    let src =
        """
module Test
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Top-level module with type parameters should pass``() =
    let src =
        """
module Test<T>
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Top-level module with type parameters should pass 2``() =
    let src =
        """
module Test<T>

class Test2<U>
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Top-level module with type parameters should pass 3``() =
    let src =
        """
module Test<T> where T: struct
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Top-level module with type parameters should pass 4``() =
    let src =
        """
module Test<T> where T: struct

class Test2<U> where U: struct
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Top-level module with type parameters should pass 5``() =
    let src =
        """
namespace Oly.Entities

module Entities<T1> =

    struct Test<T2>
        where T2: struct
        =

        mutable X: __oly_int32 = 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Module with private members cannot be accessed``() =
    let src =
        """
module Test1 =

    private M(): () = ()

    Other(): () =
        M()

module Test2 =

    M2(): () =
        Test1.M()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Member 'M' does not exist on type 'Test1'.",
                """
        Test1.M()
              ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Module with private members cannot be accessed 2``() =
    let src =
        """
#[open]
module Test1 =

    private M(): () = ()

module Test2 =

    M2(): () =
        M()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Identifier 'M' not found in scope.",
                """
        M()
        ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Module with private members cannot be accessed 3``() =
    let src =
        """
module Test1 =

    private M: __oly_int32 = 1

module Test2 =

    M2(): __oly_int32 =
        Test1.M
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Member 'M' does not exist on type 'Test1'.",
                """
        Test1.M
              ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Module with private members cannot be accessed 4``() =
    let src =
        """
#[open]
module Test1 =

    private M: __oly_int32 = 1

module Test2 =

    M2(): __oly_int32 =
        M
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Identifier 'M' not found in scope.",
                """
        M
        ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Module with private members cannot be accessed 5``() =
    let src =
        """
module Test1 =

    M: __oly_int32 private get = 1

module Test2 =

    M2(): __oly_int32 =
        Test1.M
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Member 'M' does not exist on type 'Test1'.",
                """
        Test1.M
              ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Module with private members cannot be accessed 6``() =
    let src =
        """
#[open]
module Test1 =

    M: __oly_int32 private get = 1

module Test2 =

    M2(): __oly_int32 =
        M
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Identifier 'M' not found in scope.",
                """
        M
        ^
"""
            )
        ]
    |> ignore