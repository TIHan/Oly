module SyntaxTreeTests

open System.Threading
open Xunit
open Oly.Compiler
open Oly.Compiler.Text
open TestUtilities

[<Fact>]
let ``Single comment`` () =
    let src = """/* test */"""
    Oly src
    |> ignore

[<Fact>]
let ``Value with leading and trailing trivia`` () =
    let src = """
/* test1 */
x: int32
/* test2 */
              """
    Oly src
    |> ignore

[<Fact>]
let ``Simple interface with leading and trailing trivia`` () =
    let src = """
/* test1 */
interface Test = 
    static test: int32
/* test2 */
              """
    Oly src
    |> ignore

[<Fact>]
let ``My test`` () =
    let src =
        """
/* test */
x: int32
/* test */
interface Equality =
/* test */
    static equals<T>(value: T) : (bool, bool) where T : Equality
/* test */
        """
    Oly src
    |> ignore

[<Fact>]
let ``My test 2`` () =
    let src =
        """
/* test */
interface Equality =
/* test */
    static equals<T>(value: T) : (bool, bool) where T : Equality
/* test */
x: int32
/* test */
        """
    Oly src
    |> ignore

[<Fact>]
let ``Simple interface`` () =
    let src = "
interface Test = 
    static test: int32"
    Oly src
    |> ignore

[<Fact>]
let ``Interface with parameters`` () =
    let src = "
interface Equality =
    static equals<T>(value: T) : (bool) where T : Equality"
    Oly src
    |> ignore

[<Fact>]
let ``Value`` () =
    let src = "
f() : () =
    let x: int32 = 1"
    Oly src
    |> ignore

[<Fact>]
let ``Value inference`` () =
    let src = "
f() : () =
    let x = 1"
    Oly src
    |> ignore

[<Fact>]
let ``Simple interface and value`` () =
    let src = "
interface Test = 
    static test: int32
    
x: int32"
    Oly src
    |> ignore

[<Fact>]
let ``Value and simple interface`` () =
    let src = "
x: int32

interface Test = 
    static test: int32
    "
    Oly src
    |> ignore

[<Fact>]
let ``Interface with parameters and value`` () =
    let src = "
interface Equality =
    static equals<T>(value: T) : (bool) where T : Equality
x: int32"
    Oly src
    |> ignore

[<Fact>]
let ``Value and interface with parameters`` () =
    let src = "
x: int32
interface Equality =
    static equals<T>(value: T) : (bool) where T : Equality"
    Oly src
    |> ignore

[<Fact>]
let ``Many values and interfaces with parameters`` () =
    let src = "
x: int32

interface Equality =
    static equals<T>(value: T) : (bool) where T : Equality

y: int32

interface AnotherTrait =
    static test1<T>(value: T) : (bool) where T : AnotherTrait and T : Equality
    static test2<T>(value: T) : (bool) where T : Equality
    
z: int32 /* return */"
    Oly src
    |> ignore

[<Fact>]
let ``Value has nested values`` () =
    let src ="""
f() : () =
    let x: int32 =
        let y: int32 = 1
"""
    Oly src
    |> ignore

[<Fact>]
let ``Single input and single output`` () =
    let src ="""
f(z: int) : (int) = 1
"""
    Oly src
    |> ignore

[<Fact>]
let ``No input`` () =
    let src ="""
g() : () =
    let f() : (int) = 1
                       5
"""
    Oly src
    |> ignore

[<Fact>]
let ``Let with explicit unit`` () =
    let src ="""
g() : () =
    let f: () = let x = 1
"""
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Let with explicit unit 2`` () =
    let src ="""
g() : () =
    let f() : () = let x = 1
"""
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Stack overflow regression``() =
    let src =
        """
//type Hoot<T> = { x: T; y: int32 }
type Hoot<int32>
        """
    Oly src
    |> ignore

[<Fact>]
let ``Should error with expected type parameter`` () =
    let src = "f<>() : (int) = 1"
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Expected 'type parameter'."
        ]

[<Fact>]
let ``Using plus operator with two applications`` () =
    let src =
        """
f() : () =
    let x = Test(1) + Test(2)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Using plus operator with two applications with parenthesis`` () =
    let src =
        """
f() : () =
    let x = (Test(1)) + Test(2)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple type definition with two fields`` () =
    let src =
        """
class Test =
    x: int32
    y: int32
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple type definition with two fields with flex formatting`` () =
    let src =
        """
class Test =
    x: int32
    y: int32
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple sequence builder`` () =
    let src =
        """
f(): () =
    let xs = [1;2;3;4;5]
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple sequence type`` () =
    let src =
        """
f(xs: int32[]): () = ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple hash directive`` () =
    let src =
        """
#load "prelude.oly"
        """
    Oly src
    |> ignore

[<Fact>]
let ``Simple open declaration`` () =
    let src =
        """
open Oly.Core.Prelude
        """
    Oly src
    |> ignore

[<Fact>]
let ``Simple if expression`` () =
    let src =
        """
test() : () =
    if (true)
        let x = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple if/else expression`` () =
    let src =
        """
test() : () =
    if (true)
        let x = 1
    else
        let y = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple if/else expression 2`` () =
    let src =
        """
test() : () = if (true) let x = 1 else let y = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple if/else if expression`` () =
    let src =
        """
test() : () =
    if (true)
        let x = 1
    else if (false)
        let y = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple if/else if/else expression`` () =
    let src =
        """
test() : () =
    if (true)
        let x = 1
    else if (false)
        let y = 1
    else
        let z = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple if/else if/else expression with more expressions`` () =
    let src =
        """
test() : () =
    if (true)
        let x = 1
    else if (false)
        let y = 1
    else
        let z = 1

    let w = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple if/else if/else expression with more expressions 2`` () =
    let src =
        """
test() : () =
    if (true)
        let x = 1
    else 
        if (false)
            let y = 1
        else
            let z = 1

    let w = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Invalid syntax test`` () =
    let src =
        """
open Hpl2
let
        """
    Oly src
    |> ignore

[<Fact>]
let ``Interface subsumption`` () =
    let src =
        """
interface Add =

    static test() : ()

interface SubAdd =
    inherits Add

    static test2() : ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Generic interface subsumption`` () =
    let src =
        """
interface Add<T1, T2, T3> =

   static add(x: T1, y: T2) : T3

interface Add<T> =
    inherits Add<T, T, T>

x: int32
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Interface multiple subsumption`` () =
    let src =
        """
interface Add =

    static test() : ()

interface Subtract =

    static test2() : ()

interface SubAdd =
    inherits Add, Substract

    static test3() : ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Operator with parenthesis`` () =
    let src =
        """
f() : () =
    let x = (+)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Operator with parenthesis but with calling args`` () =
    let src =
        """
f() : () =
    let x = (+)(1, 2)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Prefix application``() =
    let src =
        """
(^)(x: int32) = x

f() : () =
    let y = ^ 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Invalid syntax when defining a interface``() =
    let src =
        """
interface Functor<F<_>> =
inherits Add<int32>

    static map<A, B>(fa: F<A>, f: A -> B) : F<B>
        """
    Oly src
    |> ignore

[<Fact>]
let ``Construct type``() =
    let src =
        """
class Test =
    x: int32
    y: int32

    new(x, y) = { x = x; y = y }
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Valid syntax``() =
    let src =
        """

class Test =
    x: int32
    
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Valid constraint syntax``() =
    let src =
        """
interface Test =

    static test() : ()

f<T>() : () where T : Test = T.test()
    
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Valid constraint syntax 2``() =
    let src =
        """
interface Test =

    static test() : ()

class A<T> where T : Test =
    value: T
    
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Valid interop type``() =
    let src =
        """
#[import("CLR", "System")]
class Console =

    static Write(str: string) : ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Valid attribute on function type``() =
    let src =
        """
#[test]
test() : int32 = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Valid attribute on function type 2``() =
    let src =
        """
#[test] #[test2]
test() : int32 = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Valid attribute on function type 3``() =
    let src =
        """
#[test]#[test2]
test() : int32 = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Empty attribute on function type``() =
    let src =
        """
#[]
test() : int32 = 1
        """
    Oly src
    |> ignore

[<Fact>]
let ``Member access from generic``() =
    let src =
        """
test () : () =
    let x = Test<int32>.x
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Member access from generic 2``() =
    let src =
        """
test () : () =
    let x = Test<int32>.x()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Qualified function is valid``() =
    let src =
        """
f() : () =
    Test.test(1)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Qualified function is valid 2``() =
    let src =
        """
f() : () =
    Test.test(1).test(2)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Qualified generic function is valid``() =
    let src =
        """
f() : () =
    Test.test<int32>(1).test<int32>(2)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Qualified generic function is valid 2``() =
    let src =
        """
f() : () =
    Test.test<int32>(1).test<int32>(2).test<int32>(3)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Class is valid``() =
    let src =
        """
class Test =
    x: int32

    static test(x: int32) : int32 = x

f() : () =
    let x = Test.test(1)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Lambda expression is valid``() =
    let src =
        """
f() : () =
    let x = (y: int32, z: int32) -> z
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Lambda expression is valid 2``() =
    let src =
        """
f(y: int32) : () =
    let x = () -> y
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Lambda expression is valid 3``() =
    let src =
        """
f() : () =
    let x = (y) -> y
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Lambda expression is valid 4``() =
    let src =
        """
f() : () =
    let x = y -> y
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Static lambda expression is valid``() =
    let src =
        """
f() : () =
    let x = static (y: int32, z: int32) -> z
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Static lambda expression is valid 2``() =
    let src =
        """
f(y: int32) : () =
    let x = static () -> y
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Static lambda expression is valid 3``() =
    let src =
        """
f() : () =
    let x = static (y) -> y
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Static local is valid``() =
    let src =
        """
f() : () =
    static let x = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Local let mutable is valid``() =
    let src =
        """
f() : () =
    let mutable x = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Type with mutable fields is valid``() =
    let src =
        """
class Test =
    mutable x: int32
    mutable y: int32
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Setting a mutable let bound value``() =
    let src =
        """
f() : () =
    let mutable x = 1
    x <- 5
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Interface default implementation``() =
    let src =
        """
interface TestTrait =

    default test() : () = ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Chain call functions``() =
    let src =
        """
test() : () =
    chain1().chain2()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Chain call function with field``() =
    let src =
        """
test() : () =
    chain1().chainField
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Chain call function with set field``() =
    let src =
        """
test() : () =
    chain1().chainField <- 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Chain call function with set function``() =
    let src =
        """
test() : () =
    chain1().chain2() <- 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Intrinsic attribute``() =
    let src =
        """
#[intrinsic("__oly_and")]
(&&)(x: bool, y: bool) : bool = __oly_and(x, y)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Curried function as a parameter``() =
    let src =
        """
test(f: int32 -> int32 -> int32) : () = ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Tuple 1``() =
    let src =
        """
test() : () =
    let x = (1, 2)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Tuple 2``() =
    let src =
        """
test() : () =
    let x = (1, 2
        """
    Oly src
    |> ignore

[<Fact>]
let ``Tuple 3``() =
    let src =
        """
test() : () =
    let x = (1,
        """
    Oly src
    |> ignore

[<Fact>]
let ``Tuple 4``() =
    let src =
        """
test() : () =
    let x = (1, 2, 3, 4)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Empty type definition body``() =
    let src =
        """
struct Test
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Empty struct definition body``() =
    let src =
        """
struct Test
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Basic extension``() =
    let src =
        """
extension Int32Extension =
    inherits int32

    test() : ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Should have no syntax diagnostics``() =
    let src =
        """
test(y)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Module with a load hash directive``() =
    let src =
        """
module Test

#load "test.oly"
        """
    Oly src
    |> ignore

[<Fact>]
let ``Module with type parameters``() =
    let src =
        """
module Test<T>
        """
    Oly src
    |> ignore

[<Fact>]
let ``Simple pattern match``() =
    let src =
        """
test(x: __oly_int32) : __oly_int32 =
    match (x)
    | 1 => 5
    | _ => 2
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple pattern match 2``() =
    let src =
        """
test(x: __oly_int32) : __oly_int32 =
    match (x)
    | 1 => 5
    | _ when (true) => 6
    | _ => 2
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple pattern match 3``() =
    let src =
        """
test(x: __oly_int32) : __oly_int32 =
    match (x)
    | _ when (true) => 6
    | _ => 2
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple pattern match 4``() =
    let src =
        """
test(x: __oly_int32) : __oly_int32 =
    match (x)
    | 1
    | 2 
    | 3 => 6
    | _ => 2
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple pattern match 5``() =
    let src =
        """
test(x: __oly_int32) : __oly_int32 =
    match (x)
    | 1
    | 2 
    | 3 when (true) => 6
    | _ => 2
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple pattern match 6``() =
    let src =
        """
test(x: __oly_int32) : __oly_int32 =
    match (x)
    | 1 | 2 | 3 => 6
    | _ => 2
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple pattern match 7``() =
    let src =
        """
test(x: __oly_int32) : __oly_int32 =
    match (x)
    | 1 | 2 | 3 when (true) => 6
    | _ => 2
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple get property``() =
    let src =
        """
Value: __oly_int32
    get() = 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple set property``() =
    let src =
        """
Value: __oly_int32
    set(value) = ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple get/set property``() =
    let src =
        """
Value: __oly_int32
    get() = 1
    set(value) = ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Should not throw``() =
    let src =
        """
#load "prelude.oly"

open Prelude

(test)(x: int32) : int32 = 1

main() : () =
    Console.Write("Hello World!")
let λ = 1
    Console.Write(λ)
    ()
        """
    Oly src
    |> ignore

[<Fact>]
let ``Should give a syntax error``() =
    let src =
        """
main() : () =
    test(2,)
        """
    Oly src
    |> withSyntaxErrorDiagnostics 
        [
            "Expected 'expression' after ','."
        ]
    |> ignore

[<Fact>]
let ``Index getter call``() =
    let src =
        """
test(xs: int32[]): () =
    let x = xs[0]
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Index setter call``() =
    let src =
        """
test(xs: int32[]): () =
    xs[0] <- 1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Enum declaration syntax``() =
    let src =
        """
enum Test =
    | X = 1
    | Y
    | z
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Error recovery for 'if'``() =
    let src =
        """
test(): int32 =
    if(true == true
        let x = 1
    1
        """
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Expected ')'."
        ]

[<Fact>]
let ``Error recovery for 'if' 2``() =
    let src =
        """
test(): int32 =
    if(true == true
        if(true == true)
            let x = 1
    1
        """
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Expected ')'."
        ]

[<Fact>]
let ``Error recovery for 'if' 3``() =
    let src =
        """
test(): int32 =
    if(true == true
        if(true == true)
            test2()
    1
        """
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Expected ')'."
        ]

[<Fact>]
let ``Error recovery for 'if' 4``() =
    let src =
        """
test(x: int32): bool =
    if(1 == x
        test2()
        if(2 == x)
            test2()
    false
        """
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Expected ')'."
        ]

[<Fact>]
let ``Error recovery for 'if' 5``() =
    let src =
        """
test(x: int32): bool =
    if(1 == x
        test2()
        if(2 == x)
            test2()
        true
    else
        false
        """
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Expected ')'."
        ]

[<Fact>]
let ``Error recovery for 'if' 6``() =
    let src =
        """
test(x: int32): bool =
    if(1 == x
        ()
        if(2 == x)
            ()
        true
    else
        false
        """
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Expected ')'."
        ]

[<Fact>]
let ``Parameter with attribute``() =
    let src =
        """
test(#[inline] x: int32): () =
    ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Parameter with attribute 2``() =
    let src =
        """
test(#[inline]): () =
    ()
        """
    Oly src
    |> ignore

[<Fact>]
let ``Parameter with attribute 3``() =
    let src =
        """
test(#[inline] #[export] x: int32): () =
    ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Parameter with attribute 4``() =
    let src =
        """
#[inline]
test(#[inline] x: int32 -> ()): () =
    ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Member access from indexer should pass``() =
    let src =
        """
struct Test =

    mutable SetComponentBit(entId: EntityId, bit: int32, value: bool): () =
        this.ComponentBits[entId.Index].Set(bit, value)
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Precedence for == &&``() =
    let src =
        """
Equals(cbits1: ComponentBits, cbits2: ComponentBits): bool =
    cbits1.Page0 == cbits2.Page0 && 
    cbits1.Page1 == cbits2.Page1
        """
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Expected 'right-side argument expression' after '&&'."
        ]

[<Fact>]
let ``Precedence for == && - 2``() =
    let src =
        """
Equals(cbits1: ComponentBits, cbits2: ComponentBits): bool =
    cbits1.Page0 == cbits2.Page0 && 
     cbits1.Page1 == cbits2.Page1
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Precedence for == && - 3``() =
    let src =
        """
Equals(cbits1: ComponentBits, cbits2: ComponentBits): bool =
    cbits1.Page0 == cbits2.Page0 && 
     cbits1.Page1 == cbits2.Page1 &&
     cbits1.Page2 == cbits2.Page2
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Calling a function with a string should pass offside rules``() =
    let src =
        """
main(): () =
    print("testtesttest
"   )
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Calling a function with a string should fail offside rules``() =
    let src =
        """
main(): () =
    print("testtesttest
"  )
        """
    Oly src
    |> withSyntaxErrorDiagnostics
        [
            "Offsides by 1 space(s) from the right."
        ]

[<Fact>]
let ``Directives should fail when not declared at the top``() =
    let src =
        """
module A

#library
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Non-conditional directives must be declared at the top.",
                """
#library
^^^^^^^^
"""         )
        ]

[<Fact>]
let ``Directives should fail when not declared at the top 2``() =
    let src =
        """
module A

#target "test"
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Non-conditional directives must be declared at the top.",
                """
#target "test"
^^^^^^^^^^^^^^
"""         )
        ]

[<Fact>]
let ``Directives should fail when indented``() =
    let src =
        """
 #library
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Directives may not be indented.",
                """
 #library
 ^^^^^^^^
"""        )
        ]

[<Fact>]
let ``Directives should fail when indented 2``() =
    let src =
        """
 #target "test"
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Directives may not be indented.",
                """
 #target "test"
 ^^^^^^^^^^^^^^
"""        )
        ]

[<Fact>]
let ``Directives should fail with invalid value due to keyword``() =
    let src =
        """
#target let
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Invalid directive value.",
                """
#target let
^^^^^^^^^^^
"""        )
        ]

[<Fact>]
let ``Pattern match tuple should pass syntax``() =
    let src =
        """
test(x: int32, y: int32): () =
    match((x, y))
    | (z, w) =>
        print(z)
        print(w)
    | _ => 
        ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Pattern with implicit match should pass``() =
    let src =
        """
pattern Some<T>(value: FSharpOption<T>): T when (value !== null) =>
    value.Value
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Invalid binary should not crash``() =
    let src =
        """
test(): () =
    let x = 0b
        """
    Oly src
    |> hasErrorDiagnostics

[<Fact>]
let ``Invalid numeric literal``() =
    let src =
        """
test(): () =
    let x = 1_1_
        """
    Oly src
    |> hasErrorDiagnostics

[<Fact(Skip = "This is not simple and the error condition in this test is totally not right")>]
let ``Pattern match recoverability``() =
    let src =
        """
test(x: int32): () =
    match (x)
    | 1 =>
        let result = test2()
    let result = test3()
    | 2 =>
        let result = test2()
    | _ =>
        let result = test3()
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Non-conditional directives must be declared at the top.",
                """
#target "test"
^^^^^^^^^^^^^^
"""         )
        ]

[<Fact>]
let ``Simple conditional define``() =
    let src =
        """
#if TEST
test(): () =
    let x =
#end
        """
    OlyWithConditionalDefines [] src
    |> withNoSyntaxDiagnostics

[<Fact>]
let ``Simple conditional define 2``() =
    // This is testing that the #end is not needed at the EndOfSource.
    let src =
        """
#if TEST
test(): () =
    let x =
        """
    OlyWithConditionalDefines [] src
    |> withNoSyntaxDiagnostics

[<Fact>]
let ``Simple conditional define 3``() =
    let src =
        """
#if TEST
test(): () =
    let x =
#end
        """
    OlyWithConditionalDefines ["TEST"] src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Expected 'expression' after '='.",
                """
    let x =
          ^
"""         )
        ]

[<Fact>]
let ``Simple conditional define 4``() =
    let src =
        """
#if TEST
test(): () =
    let x =
#end

test2(): () = ()
        """
    OlyWithConditionalDefines [] src
    |> withNoSyntaxDiagnostics

[<Fact>]
let ``Simple conditional define should error``() =
    let src =
        """
#if TEST
test(): () =
    let x =
 #end
        """
    OlyWithConditionalDefines [] src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Directives may not be indented.",
                """
 #end
 ^^^^
"""         )
        ]

[<Fact>]
let ``Simple conditional define should error 2``() =
    let src =
        """
 #if TEST
test(): () =
    let x =
#end
        """
    OlyWithConditionalDefines [] src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Directives may not be indented.",
                """
 #if TEST
 ^^^
"""         )
        ]

[<Fact>]
let ``Simple conditional define should error 3``() =
    // Errors because there is no corresponding #if.
    let src =
        """
test(): () =
    let x = 1
#end
        """
    OlyWithConditionalDefines [] src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("No corresponding conditional directive was found.",
                """
#end
^^^^
"""         )
        ]

[<Fact>]
let ``Simple conditional define should error 4``() =
    // Errors because there is no corresponding #if.
    let src =
        """
#if TEST
test(): () =
    let x = 1
#end
#end
        """
    OlyWithConditionalDefines [] src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("No corresponding conditional directive was found.",
                """
#end
^^^^
"""         )
        ]

[<Fact>]
let ``Extends type on same line``() =
    let src =
        """
alias byte = uint8
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Extends type on same line 2``() =
    let src =
        """
alias byte =uint8
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Char literal example``() =
    let src =
        """
main(): () =
    let x = 'x'
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Char literal example should fail``() =
    let src =
        """
main(): () =
    let x = 'x
'
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("New-lines are not valid in character literals.",
                """
    let x = 'x
            ^^^^^
"""         )
        ]

[<Fact>]
let ``Char literal example should fail 2``() =
    let src =
        """
main(): () =
    let x = 'x"""
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Character literal reached end-of-source.",
                """
    let x = 'x
            ^^
"""         )
        ]

[<Fact>]
let ``String literal example should pass``() =
    let src =
        """
main(): () =
    let x = "x"""
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple Try expression``() =
    let src =
        """
main(): () =
    try
        DoWork()
    finally
        FinallyDoWork()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple Try expression 2``() =
    let src =
        """
main(): () =
    try
        DoWork()
    catch (ex: Exception) =>
        ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple Try expression 3``() =
    let src =
        """
main(): () =
    try
        DoWork()
    catch (ex: Exception) =>
        ()
    catch (ex: Exception) =>
        ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple Try expression 4``() =
    let src =
        """
main(): () =
    try
        DoWork()
    catch (ex: Exception) =>
        ()
    catch (ex: Exception) =>
        ()
    finally
        ()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Simple Try expression should fail``() =
    let src =
        """
main(): () =
    try
        DoWork()
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Expected ''catch' or 'finally'' after ''try' body expression'.",
                """
    try
    ^^^
"""         )
        ]

[<Fact>]
let ``Function that is offsides should fail``() =
    let src =
        """
test(): () =
    ()

        #[inline]
        test2(): () =
            ()
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Offsides by 4 space(s).",
                """
        #[inline]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"""         )
        ]

[<Fact>]
let ``Function that is offsides should fail 2``() =
    let src =
        """
test(): () =
    ()

        test2(): () =
            ()
        """
    Oly src
    |> withSyntaxErrorHelperTextDiagnostics 
        [
            ("Offsides by 4 space(s).",
                """
        test2(): () =
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"""         )
        ]

[<Fact>]
let ``Qualifying dots with indexer should parse``() =
    let src =
        """
main(): () =
    world.Archetype_Light.Buffer2[i].Position <- 123
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Let pattern binding``() =
    let src =
        """
main(): () =
    let (x, y) = call()
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Multi-line comment no end``() =
    let src =
        """
/*
        """
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Multi-line comment no end 2``() =
    let src =
        """
/*"""
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore

[<Fact>]
let ``Multi-line comment no end 3``() =
    let src =
        """
main(): () =
    ()
/*"""
    Oly src
    |> withNoSyntaxDiagnostics
    |> ignore