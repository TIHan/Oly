module Conformance.ContextualAnalysis.ByRefTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``ByRef should fail as it goes out of scope``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

main(): () =
    let y =
        let x = 1
        &x
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'x' as it might escape its scope at this point.",
                """
        &x
         ^
"""
            )
        ]

[<Fact>]
let ``ByRef should fail as it goes out of scope 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

main(): () =
    let _ =
        let x = 1
        &x
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'x' as it might escape its scope at this point.",
                """
        &x
         ^
"""
            )
        ]

[<Fact>]
let ``ByRef should fail as it cannot be captured inside a non-scoped lambda``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

M(f: () -> int32): int32 = f()

M2(x: byref<int32>): int32 =
    M(() -> x)

main(): () = ()
    """
    |> Oly
    |> hasErrorHelperTextDiagnostics
        [
            ("'x' is an address and cannot be captured.",
                """
    M(() -> x)
            ^
"""
            )
        ]

[<Fact>]
let ``ByRef should pass as it can be captured inside a scoped lambda``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

M(f: scoped () -> int32): int32 = f()

M2(x: byref<int32>): int32 =
    M(() -> x)

main(): () = ()
    """
    |> Oly
    |> shouldCompile

[<Fact>]
let ``Should not be allowed to box a byref``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let mutable x = 1
    print(&x)
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("'byref<int32>' is scoped and cannot be boxed.",
                """
    print(&x)
          ^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should be out-of-scope``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<int32>): byref<int32> = &x

M2(): byref<int32> =
    let mutable y = 1
    &M(&y)

main(): () =
    let result = M2()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Expression is scoped and might escape its scope at this point.",
                """
    &M(&y)
    ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should be out-of-scope 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<int32>): byref<int32> = &x

M2(): byref<int32> =
    let mutable y = 1
    let result = &M(&y)
    &result

main(): () =
    let result = M2()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'result' as it might escape its scope at this point.",
                """
    &result
     ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should be out-of-scope 3``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<int32>): byref<int32> = &x

M2(): byref<int32> =
    let mutable y = 1
    &M(&M(&y))

main(): () =
    let result = M2()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Expression is scoped and might escape its scope at this point.",
                """
    &M(&M(&y))
    ^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should be out-of-scope 4``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<int32>): byref<int32> = &x

M2(): byref<int32> =
    let mutable y = 1
    let result = &M(&M(&y))
    &result

main(): () =
    let result = M2()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'result' as it might escape its scope at this point.",
                """
    &result
     ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should be out-of-scope 5``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<int32>): byref<int32> = &x

struct A =
    public field mutable X: int32 = 0

M2(): byref<int32> =
    let mutable a = A()
    let result = &M(&M(&a.X))
    &result

main(): () =
    let result = M2()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'result' as it might escape its scope at this point.",
                """
    &result
     ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should be out-of-scope 6``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<int32>): byref<int32> = &x

struct A =
    public field mutable X: int32 = 0

    P: byref<int32>
        mutable get() =
            &this.X

M2(): byref<int32> =
    let mutable a = A()
    let result = &M(&M(&a.P))
    &result

main(): () =
    let result = M2()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'result' as it might escape its scope at this point.",
                """
    &result
     ^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should be able to return from inside a struct``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<int32>): byref<int32> = &x

struct A =
    public field mutable X: int32 = 0

    mutable M2(): byref<int32> =
        let result = &M(&M(&this.X))
        &result

main(): () =
    let a = A()
    """
    |> Oly
    |> shouldCompile

[<Fact>]
let ``Byref return should error if a struct is returning itself``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct A =
    public field mutable X: int32 = 0

    mutable M(): byref<A> =
        &this

main(): () =
    let a = A()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'this' as it might escape its scope at this point.",
                """
        &this
         ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should error if a struct is returning itself 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<A>): byref<A> = &x

struct A =
    public field mutable X: int32 = 0

    mutable M2(): byref<A> =
        &M(&M(&this))

main(): () =
    let a = A()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Expression is scoped and might escape its scope at this point.",
                """
        &M(&M(&this))
        ^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should error if a struct is returning itself from an extension function``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct A =
    public field mutable X: int32 = 0

#[open]
extension AExtensions =
    inherits A

    mutable M(): byref<A> =
        &this

main(): () =
    let a = A()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Cannot take the address of 'this' as it might escape its scope at this point.",
                """
        &this
         ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Byref return should error if a struct is returning itself from an extrension function 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

M(x: byref<A>): byref<A> = &x

struct A =
    public field mutable X: int32 = 0

#[open]
extension AExtensions =
    inherits A

    mutable M2(): byref<A> =
        &M(&M(&this))

main(): () =
    let a = A()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Expression is scoped and might escape its scope at this point.",
                """
        &M(&M(&this))
        ^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore