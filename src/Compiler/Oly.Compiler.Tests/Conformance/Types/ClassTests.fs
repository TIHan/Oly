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
    |> withCompile
    |> ignore

[<Fact>]
let ``Class 2``() =
    let src =
        """
abstract class A =
    field value: __oly_int32 = 0

class B =
    inherits A
    field value2: __oly_int32 = 0
        """
    Oly src
    |> withCompile
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
    |> withCompile
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

abstract class A =
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
    Assert.Equal("0: __oly_int32", symbols.[4].SignatureText)

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
    |> withCompile
    |> ignore

[<Fact>]
let ``Class inherits class``() =
    let src =
        """
abstract class Class1 =

    field x: __oly_int32

    new() = { x = 1 }

class Class2 =
    inherits Class1

    new() = base()

test() : () =
    let t = Class2()
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Class inherits class - base ctor should pass``() =
    let src =
        """
abstract class Class1 =

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
    |> withCompile
    |> ignore

[<Fact>]
let ``Class inherits class - base ctor should pass 2``() =
    let src =
        """
abstract class Class1 =

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
    |> withCompile
    |> ignore

[<Fact>]
let ``Class inherits class - should fail as forgot to assign field y ``() =
    let src =
        """
abstract class Class1 =

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
        "Field 'y' is not initialized."
    ]
    |> ignore

[<Fact>]
let ``Class inherits class - base ctor should fail if not the last thing called``() =
    let src =
        """
abstract class Class1 =

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
abstract class Class1 =

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
    |> withCompile
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
abstract class Class1 =

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
    |> withCompile
    |> ignore

[<Fact>]
let ``Class inherits class with function should error as function is non-virtual by default``() =
    let src =
        """
abstract class Class1 =

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
abstract class Class1 =

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
abstract class Class1 =

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
    |> withCompile
    |> ignore

[<Fact>]
let ``Class inherits class with virtual function should compile as function has same name as one of its super class' function will hide using the 'new' keyword``() =
    let src =
        """
abstract class Class1 =

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
    |> withCompile
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
    |> withCompile
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
    |> withCompile
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
    |> withCompile
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
    |> withCompile
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
    |> withCompile
    |> ignore

[<Fact>]
let ``Abstract class should error with missing override``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

abstract class C1 =

   abstract default M(): () = ()

   abstract M2(): ()

class C2 =
   inherits C1

test(x: C1): () =
   x.M2()

main(): () =
   let c2 = C2()
   test(c2)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("The function 'M2(): ()' is not implemented for 'C1' on 'C2'.",
                """
class C2 =
      ^^
"""
            )
        ]
    |> ignore

[<Fact(Skip = "Not enabled yet")>]
let ``Class constructor does not need to initialize field``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

class C =

    field x: int32 = 0

    new() = { }
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Can use 'base'``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

abstract default class A =

    abstract default M(): () = ()

class B =
    inherits A

    Test(): () =
        base.M()
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Can use 'base' property getter``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

abstract default class A =

    P: int32 abstract default get() = 123

class B =
    inherits A

    Test(): () =
        let result = base.P
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Can use 'base' property setter``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

abstract default class A =

    P: int32 abstract default set(value) = ()

class B =
    inherits A

    Test(): () =
        base.P <- 123
        """
    Oly src
    |> withCompile
    |> ignore

[<Fact>]
let ``Cannot use 'base' inside a lambda``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

abstract default class A =

    abstract default M(): () = ()

class B =
    inherits A

    Test(): () =
        let f() =
            base.M()
        f()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Improper use of 'base'.",
                """
            base.M()
            ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot assign 'base' to a new value``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

abstract default class A =

    abstract default M(): () = ()

class B =
    inherits A

    Test(): () =
        let x = base
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Improper use of 'base'.",
                """
        let x = base
                ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot access fields from 'base'``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

abstract default class A =

    public field X: int32 = 0

    abstract default M(): () = ()

class B =
    inherits A

    Test(): () =
        let x = base.X
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Improper use of 'base'.",
                """
        let x = base.X
                ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Cannot use property setter as it is not defined``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

class A =

    P: int32 get() = 123

    Test(): () =
        this.P <- 123
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Property 'P' cannot be set.",
                """
        this.P <- 123
             ^
"""
            )
            ("Unable to set property value as 'P' does not have a setter.",
                """
        this.P <- 123
             ^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Must implement a body for the non-abstract method in an abstract class``() =
    let src =
        """
abstract class BaseExample =

    GenericExample<T>(T): ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("TODO:",
                """
    GenericExample<T>(T): ()
    ^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Must implement a body for the non-abstract method in an abstract class - using an export``() =
    let src =
        """
namespace Test

#[export]
abstract class BaseExample =

    GenericExample<T>(T): ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("TODO:",
                """
    GenericExample<T>(T): ()
    ^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Must specify 'new' on the concrete implementation``() =
    let src =
        """
interface IExample =

    GenericExample<T>(x: T): ()

abstract class BaseExample =

    GenericExample<T>(x: T): () = ()

class Example =
    inherits BaseExample
    implements IExample

    GenericExample<T>(x: T): () =
        base.GenericExample(x)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("TODO:",
                """
    GenericExample<T>(x: T): () =
    ^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Must implement the interface member even though it is provided in the base class``() =
    let src =
        """
interface IExample =

    GenericExample<T>(x: T): ()

abstract class BaseExample =

    GenericExample<T>(x: T): () = ()

class Example =
    inherits BaseExample
    implements IExample
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("The function 'GenericExample<T>(x: T): ()' is not implemented for 'IExample' on 'Example'.",
                """
class Example =
      ^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Must implement the interface member even though it is provided in the base class 2``() =
    let src =
        """
interface IExample =

    GenericExample<T>(x: T): ()

abstract class BaseExample =

    abstract GenericExample<T>(x: T): ()

class Example =
    inherits BaseExample
    implements IExample
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("The function 'GenericExample<T>(x: T): ()' is not implemented for 'IExample' on 'Example'.",
                """
class Example =
      ^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Must implement the interface member even though it is provided in the base class 3``() =
    let src =
        """
interface IExample =

    GenericExample<T>(x: T): ()

abstract default class BaseExample =

    abstract default GenericExample<T>(x: T): () = ()

class Example =
    inherits BaseExample
    implements IExample
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("The function 'GenericExample<T>(x: T): ()' is not implemented for 'IExample' on 'Example'.",
                """
class Example =
      ^^^^^^^
"""
            )
        ]
    |> ignore