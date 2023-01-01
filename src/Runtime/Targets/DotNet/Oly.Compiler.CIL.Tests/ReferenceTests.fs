module ReferenceTests

open Xunit
open TestUtilities
open Utilities

[<Fact>]
let ``Basic reference should compile and run``() =
    let refSrc =
        """
module ReferenceTest

#[intrinsic("print")]
print(__oly_object): ()

RefTest(): () = print("from a reference")
        """
    let src =
        """
open static ReferenceTest

main(): () =
    RefTest()
        """
    OlyWithReference refSrc src
    |> runWithExpectedOutput "from a reference"

[<Fact>]
let ``Witness pass subsumption``() =
    let refSrc =
        """
module ReferenceTest

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3>

test<T>(): () where T: Add<T, T, T> = print("from a reference")
        """
    let src =
        """
open extension Int32AddExtension
open static ReferenceTest

interface Add<T> =
    inherits Add<T, T, T>

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

main(): () =
    test<int32>()
        """
    OlyWithReference refSrc src
    |> runWithExpectedOutput "from a reference"

[<Fact>]
let ``Witness pass subsumption 2``() =
    let refSrc =
        """
module ReferenceTest

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3>
        """
    let src =
        """
open extension Int32AddExtension
open static ReferenceTest

interface Add<T> =
    inherits Add<T, T, T>

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

test<T>(): () where T: Add<T, T, T> = print("not from a reference")

main(): () =
    test<int32>()
        """
    OlyWithReference refSrc src
    |> runWithExpectedOutput "not from a reference"

[<Fact>]
let ``Witness pass subsumption 3``() =
    let refSrc =
        """
module ReferenceTest

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

    static abstract test(): ()

test<T>(): () where T: Add<T, T, T> = 
    T.test()
    print("from a reference")

        """
    let src =
        """
open extension Int32AddExtension
open static ReferenceTest

interface Add<T> =
    inherits Add<T, T, T>

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides test(): () = ()

main(): () =
    test<int32>()
        """
    OlyWithReference refSrc src
    |> runWithExpectedOutput "from a reference"

[<Fact>]
let ``Witness pass subsumption 4``() =
    let refSrc =
        """
module ReferenceTest

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface Add<T1, T2, T3> =

    static abstract test(): ()

        """
    let src =
        """
open extension Int32AddExtension
open static ReferenceTest

interface Add<T> =
    inherits Add<T, T, T>

extension Int32AddExtension =
    inherits int32
    implements Add<int32>

    static overrides test(): () = ()

test<T>(): () where T: Add<T, T, T> = 
    T.test()
    print("not from a reference")

main(): () =
    test<int32>()
        """
    OlyWithReference refSrc src
    |> runWithExpectedOutput "not from a reference"

[<Fact>]
let ``Basic namespace reference should compile and run``() =
    let refSrc =
        """
namespace Test

module TestModule =

    #[intrinsic("print")]
    print(__oly_object): ()

    RefTest(): () = print("from a reference")
        """
    let src =
        """
namespace Test

module TestMain =

    main(): () =
        TestModule.RefTest()
        """
    OlyWithReference refSrc src
    |> runWithExpectedOutput "from a reference"

[<Fact>]
let ``Basic namespace reference should compile and run 2``() =
    let refSrc =
        """
namespace Test.Inner

module TestModule =

    #[intrinsic("print")]
    print(__oly_object): ()

    RefTest(): () = print("from a reference")
        """
    let src =
        """
namespace Test.Inner

module TestMain =

    main(): () =
        TestModule.RefTest()
        """
    OlyWithReference refSrc src
    |> runWithExpectedOutput "from a reference"

[<Fact>]
let ``Basic module reference should compile and run``() =
    let refSrc =
        """
module TestModule

#[intrinsic("print")]
print(__oly_object): ()

RefTest(): () = print("from a reference")
        """
    let src =
        """
namespace Test.Inner

open static TestModule

module TestMain =

    main(): () =
        RefTest()
        """
    OlyWithReference refSrc src
    |> runWithExpectedOutput "from a reference"

[<Fact>]
let ``Basic types referencing one another in the same compilation``() =
    let src1 =
        """
namespace Test

module TestModule =

    #[intrinsic("print")]
    print(__oly_object): ()

    test(): () =
        let a = A()
        a.Call()

        """
    let src2 =
        """
namespace Test

open static Test.TestModule

class A =

    Call(): () = print("call")

module TestMain =

    main(): () =
        print("hello")
        test()
        """
    OlyTwo src1 src2
    |> runWithExpectedOutput "hellocall"
