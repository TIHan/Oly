module Conformance.Types.ClassTests

open Xunit
open TestUtilities
open Oly.Compiler

[<Fact>]
let ``Class 1``() =
    let src =
        """
class A =
    field value: __oly_int32 = 0
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class 2``() =
    let src =
        """
class A =
    field value: __oly_int32 = 0

class B =
    inherits A
    field value2: __oly_int32 = 0
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class 3``() =
    let src =
        """
interface IA =
    value(): __oly_int32

class A =
    implements IA

    value(): __oly_int32 = 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class 4 - should fail due to not providing a property field``() =
    let src =
        """
interface IA =
    value: __oly_int32 get

class A =
    implements IA
        """
    Oly src
    |> withErrorDiagnostics [
        "The function 'get_value(): __oly_int32' is not implemented for 'IA' on 'A'."
    ]
    |> ignore

[<Fact>]
let ``Class 5 - should fail``() =
    let src =
        """
interface IA =
    value(): __oly_int32

class A =
    value(): __oly_int32 = 1

class B =
    inherits A
    implements IA
        """
    Oly src
    |> withErrorDiagnostics [
        "The function 'value(): __oly_int32' is not implemented for 'IA' on 'B'."
    ]
    |> ignore

[<Fact>]
let ``Class has correct field symbol``() =
    """
class Test =
    field ~^~x: __oly_int32 = 0
    """
    |> hasSymbolSignatureTextByCursor "field x: __oly_int32"

[<Fact>]
let ``Class has correct type symbol``() =
    """
class ~^~Test =
    field x: __oly_int32 = 0
    """
    |> hasSymbolSignatureTextByCursor "Test"

[<Fact>]
let ``Class has correct generic type symbol``() =
    """
class ~^~Test<T> =
    field x: __oly_int32 = 0
    """
    |> hasSymbolSignatureTextByCursor "Test<T>"

[<Fact>]
let ``Class has correct generic type parameter symbol``() =
    """
class Test<~^~T> =
    field x: __oly_int32 = 0
    """
    |> hasSymbolSignatureTextByCursor "T"

[<Fact>]
let ``Class has correct parameter type on method``() =
    """
class Test =
    field x: __oly_int32 = 0

    test(x: ~^~__oly_int32) : __oly_int32 = 1
    """
    |> hasSymbolSignatureTextByCursor "__oly_int32"

[<Fact>]
let ``Class has correct parameter on constructor``() =
    """
class Test =
    field x: __oly_int32

    new(~^~x: __oly_int32) = { x = x }
    """
    |> hasSymbolSignatureTextByCursor "x: __oly_int32"

[<Fact>]
let ``Class has correct parameter type on constructor``() =
    """
class Test =
    field x: __oly_int32

    new(x: ~^~__oly_int32) = { x = x }
    """
    |> hasSymbolSignatureTextByCursor "__oly_int32"

[<Fact>]
let ``Correct symbol on identifier followed by a qualified function name``() =
    """
class Test =
    field x: __oly_int32 = 0

    static test(x: __oly_int32) : __oly_int32 = x

f() : () =
    let x = ~^~Test.test(1)
    """
    |> hasSymbolSignatureTextByCursor "Test"

[<Fact>]
let ``Correct symbol on identifier followed by a qualified field name``() =
    """
class Test =
    public field x: __oly_int32 = 0

    static test(x: __oly_int32) : __oly_int32 = x

f(testValue: Test) : () =
    let y = testValue.~^~x
    """
    |> hasSymbolSignatureTextByCursor "field x: __oly_int32"

[<Fact>]
let ``Correct symbol on identifier followed by a qualified field name 2``() =
    """
class Test =
    public field x: __oly_int32 = 0

    static test(x: __oly_int32) : __oly_int32 = x

f(testValue: Test) : () =
    let y = ~^~testValue.x
    """
    |> hasSymbolSignatureTextByCursor "testValue: Test"

[<Fact>]
let ``Source with class has the correct symbols``() =
    let src =
        """
class Test<T> =
    field x: __oly_int32 = 0
        """
    let symbols = getAllSymbols src
    
    Assert.Equal(5, symbols.Length)
    Assert.Equal("Test<T>", symbols.[0].SignatureText)
    Assert.Equal("T", symbols.[1].SignatureText)
    Assert.Equal("field x: __oly_int32", symbols.[2].SignatureText)
    Assert.Equal("__oly_int32", symbols.[3].SignatureText)
    Assert.Equal("", symbols.[4].SignatureText)

[<Fact>]
let ``Source with class has the correct symbols 2``() =
    let src =
        """
class Test =
    x: __oly_int32 = 0

    static test(x: __oly_int32) : __oly_int32 = x

test() : () =
    let x = Test.test(1)
        """
    let symbols = getAllSymbols src
    
    Assert.Equal(14, symbols.Length)

[<Fact>]
let ``Expected error for a member function``() =
    let src =
        """
class Test<T> =

    field x: T

    new(x: T) = { x = x }

main() : () =
    let x = Test<__oly_int32>(123.0f)
        """
    Oly src
    |> withErrorDiagnostics [
        "Expected type '__oly_int32' but is '__oly_float32'."
    ]
    |> ignore

[<Fact>]
let ``Overloading in anonymous module works``() =
    let src =
        """
test(x: __oly_int32) : () = ()
test(x: __oly_float64) : () = ()

main() : () =
    ~^~test(1)
        """
    src |> hasSymbolSignatureTextByCursor "static test(x: __oly_int32): ()"

[<Fact>]
let ``Overloading in anonymous module works 2``() =
    let src =
        """
test(x: __oly_int32) : () = ()
test(x: __oly_float64) : () = ()

main() : () =
    ~^~test(1.0)
        """
    src |> hasSymbolSignatureTextByCursor "static test(x: __oly_float64): ()"

[<Fact>]
let ``Overloading in anonymous module works 3``() =
    let src =
        """
test() : () = ()
test(x: __oly_float64) : () = ()

main() : () =
    ~^~test()
        """
    src |> hasSymbolSignatureTextByCursor "static test(): ()"

[<Fact>]
let ``Overloading in anonymous module works 4``() =
    let src =
        """
test() : () = ()
test(x: __oly_float64) : () = ()

main() : () =
    ~^~test(10.0)
        """
    src |> hasSymbolSignatureTextByCursor "static test(x: __oly_float64): ()"

[<Fact>]
let ``Undecided overload should fail``() =
    let src =
        """
test(x: __oly_int32) : () = ()
test(x: __oly_float64) : () = ()

main() : () =
    let f(x) = test(x)
        """
    Oly src
    |> withErrorDiagnostics [
        "'test' has ambiguous functions."
    ]
    |> ignore

[<Fact>]
let ``Undecided overload should fail 2``() =
    let src =
        """
test(x: __oly_int32) : () = ()
test(x: __oly_float64) : () = ()

main() : () =
    let x = test
        """
    Oly src
    |> withErrorDiagnostics [
        "'test' has ambiguous functions."
    ]
    |> ignore

[<Fact>]
let ``Undecided overload should fail 3``() =
    let src =
        """
test(x: __oly_int32) : () = ()
test(x: __oly_float64, y: __oly_float64) : () = ()

main() : () =
    let x = test
        """
    Oly src
    |> withErrorDiagnostics [
        "'test' has ambiguous functions."
    ]
    |> ignore

[<Fact>]
let ``Undecided overload should fail 4``() =
    let src =
        """
test() : () = ()
test(x: __oly_float64, y: __oly_float64) : () = ()

main() : () =
    let x = test
        """
    Oly src
    |> withErrorDiagnostics [
        "'test' has ambiguous functions."
    ]
    |> ignore

[<Fact>]
let ``Able to access class constructor after it was defined``() =
    """
f() : () =
    let ~^~x = Test(1)

class Test =
    field x: __oly_int32

    new(x: __oly_int32) = { x = x }
    """
    |> hasSymbolSignatureTextByCursor "x: Test"

[<Fact>]
let ``Nested class``() =
    """
class Test =
    field x: __oly_int32

    class Test2 =
        field ~^~y: __oly_float32 = 0
    new(x: __oly_int32) = { x = x }
    """
    |> hasSymbolSignatureTextByCursor "field y: __oly_float32"

[<Fact>]
let ``Nested class instantiation``() =
    """
class Test =
    field x: __oly_int32

    class Test2 =
        field y: __oly_float32
        new(y: __oly_float32) = { y = y }
    new(x: __oly_int32) = { x = x }

test() : () =
    let x = Test.~^~Test2(2.0f)
    """
    |> hasSymbolSignatureTextByCursor "new(y: __oly_float32): Test2"

[<Fact>]
let ``Should error with duplicate type parameter names``() =
    let src =
        """
test<T, T>() : () = ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Type parameter 'T' has already been declared."
    ]
    |> ignore

[<Fact>]
let ``Should error with duplicate type parameter names 2``() =
    let src =
        """
test<T>() : () =
    let test2<T>() : () = ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Type parameter 'T' has already been declared."
    ]
    |> ignore

[<Fact>]
let ``Should error with duplicate type parameter names 3``() =
    let src =
        """
class Test<T> =
    test<T>() : () = ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Type parameter 'T' has already been declared."
    ]
    |> ignore

[<Fact>]
let ``Should error with duplicate type parameter names 4``() =
    let src =
        """
interface TestTrait =
    test() : ()

class Test<T> =
    field x: __oly_int32 = 0

f<T>() : () =
    extension TestImpl<T> =
        inherits Test<T>
        implements TestTrait
    
        test() : () = ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Type parameter 'T' has already been declared."
    ]
    |> ignore

[<Fact>]
let ``Should error with duplicate type parameter names 5``() =
    let src =
        """
module Test<T>
test<T>() : () = ()
        """
    Oly src
    |> withErrorDiagnostics [
        "Type parameter 'T' has already been declared."
    ]
    |> ignore


[<Fact>]
let ``Class with no fields with a constructor``() =
    let src =
        """
class Test =

    new() = { }

test() : () =
    let t = Test()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class inherits class``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    new() = { x = 1 }

class Class2 =
    inherits Class1

    new() = base()

test() : () =
    let t = Class2()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class inherits class - base ctor should pass``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    new() = { x = 1 }

class Class2 =
    inherits Class1

    new() = 
        let x = 1
        { }

test() : () =
    let t = Class2()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class inherits class - base ctor should pass 2``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    new() = { x = 1 }

class Class2 =
    inherits Class1

    field y: __oly_int32

    new() = base() with { y = 1 }

test() : () =
    let t = Class2()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class inherits class - should as forgot to assign field y ``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    new() = { x = 1 }

class Class2 =
    inherits Class1

    field y: __oly_int32

    new() = base()

test() : () =
    let t = Class2()
        """
    Oly src
    |> withErrorDiagnostics [
        "'y' is not initialized."
    ]
    |> ignore

[<Fact>]
let ``Class inherits class - base ctor should fail if not the last thing called``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    new() = { x = 1 }

class Class2 =
    inherits Class1

    new() = 
        let x = base()
        { }

test() : () =
    let t = Class2()
        """
    Oly src
    |> withErrorDiagnostics [
        "The base constructor call is only allowed as the last expression of a branch."
    ]
    |> ignore

[<Fact>]
let ``Class inherits class should fail and must call super``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    new(x: __oly_int32) = { x = x }

class Class2 =
    inherits Class1

    new() = { }

test() : () =
    let t = Class2()
        """
    Oly src
    |> withErrorDiagnostics [
        "Cannot implicitly call parameterless base constructor for type 'Class2' as it does not exist."
    ]
    |> ignore

[<Fact>]
let ``Abstract class should compile``() =
    let src =
        """
abstract class Class1 =

    field x: __oly_int32 = 0
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Abstract class should not compile due constructor call``() =
    let src =
        """
abstract class Class1 =

    field x: __oly_int32

    new() = { x = 1 }

test() : () =
    let x = Class1() // should fail
        """
    Oly src
    |> withErrorDiagnostics [
        "The constructor call is not allowed as the enclosing type 'Class1' is abstract."
    ]
    |> ignore

[<Fact>]
let ``Class inherits class with virtual function``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    abstract default VirtualTest() : () = ()

    new() = { x = 1 }

class Class2 =
    inherits Class1

    overrides VirtualTest() : () = ()

test() : () =
    let t = Class2()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class inherits class with function should error as function is non-virtual by default``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    Test() : () = ()

    new() = { x = 1 }

class Class2 =
    inherits Class1

    overrides Test() : () = () // Error

test() : () =
    let t = Class2()
        """
    Oly src
    |> withErrorDiagnostics [
        "The function 'Test(): ()' cannot find a function to override."
    ]
    |> ignore

[<Fact>]
let ``Class inherits class with function should error as function has same name as one of its super class' function``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    Test() : () = ()

    new() = { x = 1 }

class Class2 =
    inherits Class1

    Test() : () = () // Error - same name

test() : () =
    let t = Class2()
        """
    Oly src
    |> withErrorDiagnostics [
        "'Test(): ()' has duplicate member definitions."
    ]
    |> ignore

[<Fact>]
let ``Class inherits class with function should compile as function has same name as one of its super class' function will hide using the 'new' keyword``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    Test() : () = ()

    new() = { x = 1 }

class Class2 =
    inherits Class1

    new Test() : () = () // Should compile

test() : () =
    let t = Class2()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Class inherits class with virtual function should compile as function has same name as one of its super class' function will hide using the 'new' keyword``() =
    let src =
        """
class Class1 =

    field x: __oly_int32

    abstract default Test() : () = ()

    new() = { x = 1 }

class Class2 =
    inherits Class1

    new Test() : () = () // Should compile

test() : () =
    let t = Class2()
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Basic property with getter should compile``() =
    let src =
        """
class A =

   X: __oly_int32
      get() = 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Basic property with overrides getter should compile``() =
    let src =
        """
interface IA =

    static X: __oly_int32 abstract get

class A =
    implements IA

    static X: __oly_int32 
        overrides get() = 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Basic property with overrides getter should compile 2``() =
    let src =
        """
interface IA =

    X: __oly_int32 get

class A =
    implements IA

    X: __oly_int32 
        get() = 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Basic property with overrides getter should compile 3``() =
    let src =
        """
abstract class B =

    static X: __oly_int32 abstract default get = 5

class A =
    inherits B

    static X: __oly_int32 
        overrides get() = 1
        """
    Oly src
    |> shouldCompile
    |> ignore

[<Fact>]
let ``Basic property with overrides getter should compile 4``() =
    let src =
        """
interface IA =

    static X: __oly_int32 abstract get 

class A =
    implements IA

    static X: __oly_int32 
        overrides get() = 1
        set(value) = ()
        """
    Oly src
    |> shouldCompile
    |> ignore