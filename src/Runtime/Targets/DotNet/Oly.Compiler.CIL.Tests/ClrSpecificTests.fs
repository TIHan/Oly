module ClrSpecificTests

open System.IO
open Xunit
open TestUtilities
open Utilities
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Targets.DotNet
open Oly.Core
open System.Runtime.Loader
open System

let OlyWithCSharp csSrc src (callback: TestCompilation -> unit): unit =
    let tmp = Path.GetTempFileName()
    let dllTmp = Path.Combine(Environment.CurrentDirectory, Path.ChangeExtension(Path.GetFileNameWithoutExtension(tmp), ".dll"))

    try

    let csAsm = DotNetTarget.CompileCSharp(Path.GetFileNameWithoutExtension(dllTmp), csSrc, getDefaultReferences() |> ImArray.map (fun x -> x.Path), System.Threading.CancellationToken.None)
    let csRef = OlyCompilationReference.Create(OlyPath.Create(dllTmp), 42UL, Importer.Import(Path.GetFileNameWithoutExtension(dllTmp), csAsm))
    csAsm.Position <- 0L
    File.WriteAllBytes(dllTmp, csAsm.ToArray())

    callback(OlyWithCRef csRef src)

    finally
        try File.Delete dllTmp with _ -> ()
        try File.Delete tmp with _ -> ()

let coreLib =
    """
// Interfaces

interface Add<T1, T2, T3> =

    static abstract (+)(T1, T2) : T3

interface Subtract<T1, T2, T3> =

    static abstract (-)(T1, T2) : T3

interface Multiply<T1, T2, T3> =

    static abstract (*)(T1, T2) : T3

interface Divide<T1, T2, T3> =

    static abstract (/)(T1, T2) : T3

interface Remainder<T1, T2, T3> =

    static abstract (%)(T1, T2) : T3

interface Equal<T1, T2, T3> =

    static abstract (==)(T1, T2) : T3

interface NotEqual<T1, T2, T3> =

    static abstract (!=)(T1, T2) : T3

interface GreaterThan<T1, T2, T3> =

    static abstract (>)(T1, T2) : T3

interface GreaterThanOrEqual<T1, T2, T3> =

    static abstract (>=)(T1, T2) : T3

interface LessThan<T1, T2, T3> =

    static abstract (<)(T1, T2) : T3

interface LessThanOrEqual<T1, T2, T3> =

    static abstract (<=)(T1, T2) : T3

// Operators

#[intrinsic("add")]
(+)(int32, int32) : int32
#[intrinsic("add")]
(+)(float32, float32) : float32
(+)<T>(x: T, y: T) : T where T : Add<T, T, T> = T.(+)(x, y)

#[intrinsic("subtract")]
(-)(int32, int32) : int32
(-)<T>(x: T, y: T) : T where T : Subtract<T, T, T> = T.(-)(x, y)

#[intrinsic("multiply")]
(*)(int32, int32) : int32
#[intrinsic("multiply")]
(*)(float32, float32) : float32
(*)<T>(x: T, y: T) : T where T : Multiply<T, T, T> = T.(*)(x, y)

#[intrinsic("divide")]
(/)(int32, int32) : int32
(/)<T>(x: T, y: T) : T where T : Divide<T, T, T> = T.(/)(x, y)

#[intrinsic("remainder")]
(%)(int32, int32) : int32
(%)<T>(x: T, y: T) : T where T : Remainder<T, T, T> = T.(%)(x, y)

#[intrinsic("equal")]
(==)(int32, int32) : bool
(==)<T>(x: T, y: T) : bool where T : Equal<T, T, bool> = T.(==)(x, y)

#[intrinsic("not_equal")]
(!=)(int32, int32) : bool
(!=)<T>(x: T, y: T) : bool where T : NotEqual<T, T, bool> = T.(!=)(x, y)

#[intrinsic("greater_than")]
(>)(int32, int32) : bool
(>)<T>(x: T, y: T) : bool where T : GreaterThan<T, T, bool> = T.(>)(x, y)

#[intrinsic("greater_than_or_equal")]
(>=)(int32, int32) : bool
(>=)<T>(x: T, y: T) : bool where T : GreaterThanOrEqual<T, T, bool> = T.(>=)(x, y)

#[intrinsic("less_than")]
(<)(int32, int32) : bool
(<)<T>(x: T, y: T) : bool where T : LessThan<T, T, bool> = T.(<)(x, y)

#[intrinsic("less_than_or_equal")]
(<=)(int32, int32) : bool
(<=)<T>(x: T, y: T) : bool where T : LessThanOrEqual<T, T, bool> = T.(<=)(x, y)

#[intrinsic("and")]
(&&)(bool, bool) : bool

#[intrinsic("or")]
(||)(bool, bool) : bool
"""

let run expectedOutput src =
    OlySharp (coreLib + src)
    |> withCompile
    |> shouldRunWithExpectedOutput expectedOutput
    |> ignore

[<Fact>]
let ``Defining and using Vector3``() =
    let src =
        """
#[import("CLR:System.Numerics", "System.Numerics", "Vector3")]
struct Vector3 =

    public field mutable X: float32
    public field mutable Y: float32
    public field mutable Z: float32

    new(x: float32, y: float32, z: float32)

dot(v1: Vector3, v2: Vector3) : float32 =
    v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

main() : () =
    let v = Vector3(3.0f, 4.0f, 5.0f)
    print(dot(v, v))
        """

    run "50" src

[<Fact>]
let ``CLR import generic type``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[import("CLR:mscorlib", "System.Collections.Generic", "List`1")]
class List<T> =

    get_Item(__oly_int32): T

    new()

    Add(item: T) : ()

main() : () =
    let xs : List<__oly_int32> = List<__oly_int32>()
    xs.Add(123)
    print(xs.get_Item(0))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``CLR import generic type 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[import("CLR:mscorlib", "System.Collections.Generic", "List`1")]
class List<T> =
    inherits System.Object

    get_Count() : __oly_int32
    get_Item(__oly_int32): T

    new()

    Add(item: T) : ()

    overrides ToString() : __oly_utf16

main() : () =
    let xs : List<__oly_int32> = List<__oly_int32>()
    print(xs.get_Count())
    xs.Add(123)
    print(xs.get_Count())
    xs.Add(456)
    print(xs.get_Count())
    print(xs.get_Item(0))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "012123"

[<Fact>]
let ``Custom CLR import intrinsic int32``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
#[import("CLR:mscorlib", "System", "Int32")]
struct CustomInt32 =
    inherits System.ValueType

    static Parse(s: __oly_utf16) : CustomInt32

    overrides ToString() : __oly_utf16

main() : () =
    let x = CustomInt32.Parse("123")
    print(x)
    print(9)
    print(x.ToString())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1239123"

[<Fact>]
let ``Simple immutable array``() =
    let src =
        """
open System
open System.Collections.Immutable

main(): () =
    let x = ImmutableArray.Create<_>(7)
    Console.Write(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Witness pass subsumption with IDisposable``() =
    let src =
        """
open extension Int32DisposableExtension
open System

interface IDoot =
  inherits IDisposable

extension Int32DisposableExtension =
    inherits Int32
    implements IDoot

    Dispose(): () = ()

test<T>(x: T): () where T: trait IDisposable = x.Dispose()

class Test =
  implements IDisposable

  new() = this { }

  Dispose(): () = 
    let x = 1
    ()

main(): () =
  let tt = Test()
  let lookup = System.Collections.Generic.Dictionary<Int32, Int32>()
  lookup.Add(2, 3)
  let result = lookup.get_Item(2)
  let xs = System.Collections.Generic.List<Int32>()
  xs.Add(500)
  test<_>(1)
  test<_>(tt)
  Console.Write(result)
  Console.Write("Hello World!")
  Console.Write(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3Hello World!3"

[<Fact>]
let ``Witness pass subsumption with IDisposable 2``() =
    let src =
        """
open extension Int32DisposableExtension
open System

interface IDoot =
  inherits IDisposable

extension Int32DisposableExtension =
    inherits Int32
    implements IDoot

    Dispose(): () = ()

test<T>(x: T): () where T: trait IDisposable = x.Dispose()

class Test =
  implements IDisposable

  new() = this { }

  Dispose(): () = 
    let x = 1
    ()

main(): () =
  let tt = Test()
  test<_>(tt)
  test<_>(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Partial call``() =
    let src =
        """
open System

main(): () =
    let xsf = System.Collections.Generic.List<Int32>: Int32 -> _
    let xs = xsf(1223)
    Console.Write(xs.Capacity)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1223"

[<Fact>]
let ``GetHashCode``() =
    let src =
        """
open System
open System.Diagnostics
open System.Numerics

#[intrinsic("by_ref")]
alias (&)<T>

#[intrinsic("address_of")]
(&)<T>(T): T&

struct TestStruct =

    x: Int32 get, set

    new(x: Int32) = this { x = x }

test<T>(x: T&): Int32 where T: { x: Int32 get, set } = 
    let mutable result = Int32.Parse("123")
    x.x <- result.GetHashCode()
    x.x

main(): () =
    let s = Stopwatch()
    Console.Write(s.Elapsed.TotalMilliseconds)
    let mutable testStruct = TestStruct(123)
    let result = test<_>(&testStruct)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"

[<Fact>]
let ``GetHashCode shape constraint``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

hash<T>(mutable x: T): int32 where T: { GetHashCode(): int32 } =
    x.GetHashCode()

main(): () =
    print(hash<Int32>(123))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``GetHashCode shape constraint 2``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

hash<T>(mutable x: T): int32 where T: { GetHashCode(): int32 } =
    x.GetHashCode()

main(): () =
    print(hash(123))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``GetHashCode shape constraint 3``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

struct Hash<T> where T: { GetHashCode(): int32 } =
    
    private field mutable item: T

    new(item: T) = this { item = item }

    mutable GetValue(): int32 =
        this.item.GetHashCode()

main(): () =
    let mutable h = Hash<System.Int32>(123)
    print(h.GetValue())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``GetHashCode shape constraint 4``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

struct Hash<T> where T: { GetHashCode(): int32 } =
    
    private field mutable item: T

    new(item: T) = this { item = item }

    mutable GetValue(): int32 =
        this.item.GetHashCode()

main(): () =
    let mutable h = Hash(123)
    print(h.GetValue())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``GetHashCode shape constraint 5``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

struct Hash<T> where T: { GetHashCode(): int32 } =
    
    private field mutable item: T

    new(item: T) = this { item = item }

    #[inline(never)]
    mutable GetValue(): int32 =
        this.item.GetHashCode()

struct Hash2<T> where T: { GetHashCode(): int32 } =

    private field mutable item: Hash<T>

    new(item: Hash<T>) = this { item = item }

    #[inline(never)]
    mutable GetValue(): int32 =
        this.item.GetValue().GetHashCode()

main(): () =
    let h = Hash(123)
    let mutable h2 = Hash2(h)
    print(h2.GetValue())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``GetHashCode shape constraint 6``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface IHash<T> where T: { GetHashCode(): int32 } =

    default GetValue(mutable x: T): int32 =
        x.GetHashCode()

struct Hash<T> where T: { GetHashCode(): int32 } =
    implements IHash<T>
    
    private field mutable item: T

    new(item: T) = this { item = item }

    #[inline(never)]
    GetValue(): int32 =
        let x: IHash<T> = this
        x.GetValue(this.item)

struct Hash2<T> where T: { GetHashCode(): int32 } =

    private field mutable item: Hash<T>

    new(item: Hash<T>) = this { item = item }

    #[inline(never)]
    GetValue(): int32 =
        this.item.GetValue().GetHashCode()

main(): () =
    let h = Hash(123)
    let h2 = Hash2(h)
    print(h2.GetValue())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``GetHashCode shape constraint 7``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

interface IHash<T> where T: { GetHashCode(): int32 } =

    default GetValue(mutable x: T): int32 =
        x.GetHashCode()

struct Hash<T> where T: { GetHashCode(): int32 } =
    implements IHash<T>
    
    public field mutable item: T

    new(item: T) = this { item = item }

    #[inline(never)]
    GetValue(): int32 =
        let x: IHash<T> = this
        x.GetValue(this.item)

struct Hash2<T> where T: { GetHashCode(): int32 } =
    implements IHash<T>

    public field mutable item: Hash<T>

    new(item: Hash<T>) = this { item = item }

    #[inline(never)]
    GetValue(x: T): int32 =
        let h: IHash<T> = this.item
        h.GetValue(x)

    #[inline(never)]
    GetValue(): int32 =
        this.GetValue(this.item.item)

main(): () =
    let h = Hash(123)
    let h2 = Hash2(h)
    print(h2.GetValue())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``GetHashCode shape constraint 8``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

hash<T>(mutable x: T): int32 where T: { GetHashCode(): int32 } =
    x.GetHashCode()

main(): () =
    print(hash(123))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Complex test and csharp source``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}

public interface IExample3
{
    void GenericExample<T>(T x) where T : IExample, IExample2;
}
        """

    let src =
        """
open System

test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

class Example =
  implements IExample

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () = 
      Console.Write("Example")

class Example2 =
  implements IExample2

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
                |> withErrorHelperTextDiagnostics
                    [
            ("Type parameter 'U' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
      test<_>(x)
      ^^^^^^^
"""
            )
                    ]
                    |> ignore
        )

[<Fact>]
let ``Complex test and csharp source 2``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}

public interface IExample3
{
    void GenericExample<T>(T x) where T : IExample, IExample2;
}
        """

    let src =
        """
open System

test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

struct Example =
  implements IExample

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () = 
      Console.Write("Example")

struct Example2 =
  implements IExample2

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
                |> withErrorHelperTextDiagnostics
                    [
            ("Type parameter 'U' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
      test<_>(x)
      ^^^^^^^
"""
            )
                    ]
                    |> ignore
        )

[<Fact>]
let ``Complex test and csharp source 3``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}

public interface IExample3
{
    void GenericExample<T>(T x) where T : IExample, IExample2;
}
        """

    let src =
        """
open System

test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

struct Example<Z> =
  implements IExample

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () = 
      Console.Write("Example")

struct Example2<Z> =
  implements IExample2

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example<__oly_int32>()
    let t2 = Example2<__oly_int32>()

    t2.GenericExample<_>(t)
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
                |> withErrorHelperTextDiagnostics
                    [
            ("Type parameter 'U' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
      test<_>(x)
      ^^^^^^^
"""
            )
                    ]
                    |> ignore
        )

[<Fact>]
let ``Complex test and csharp source 4 - using an export``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}

public interface IExample3
{
    void GenericExample<T>(T x) where T : IExample, IExample2;
}
        """

    let src =
        """
namespace Test

open System

#[open]
module Test =

    test<T>(x: T): () where T: IExample =
        Console.Write("test")
        x.GenericExample<T>(x)

    main(): () =
        let t = Example<__oly_int32>()
        let t2 = Example2<__oly_int32>()

        t2.GenericExample<_>(t)

struct Example<Z> =
  implements IExample

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
struct Example2<Z> =
  implements IExample2

  new() = this { }

  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
                |> withErrorHelperTextDiagnostics
                    [
            ("Type parameter 'U' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
      test<_>(x)
      ^^^^^^^
"""
            )
                    ]
                    |> ignore
        )

[<Fact>]
let ``Complex test and csharp source 5``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}
        """

    let src =
        """
open System

abstract default class BaseExample =

    abstract default GenericExample<T>(x: T): () =
        Console.Write(x)

class Example =
    inherits BaseExample
    implements IExample

main(): () =
    let example = Example()
    let example2 = example: IExample
    example2.GenericExample(123)
    example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
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
        )

[<Fact>]
let ``Complex test and csharp source 5 - but explicit``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}
        """

    let src =
        """
open System

abstract default class BaseExample =

    abstract default GenericExample<T>(x: T): () =
        Console.Write("failed")

class Example =
    inherits BaseExample
    implements IExample

    #[export]
    new GenericExample<T>(x: T): () =
        Console.Write(x)

main(): () =
    let example = Example()
    let example2 = example: IExample
    example2.GenericExample(123)
    example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123test"
        )

[<Fact>]
let ``Complex test and csharp source 6``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}
        """

    let src =
        """
namespace Test

open System

module Test =

    main(): () =
        let example = Example()
        let example2 = example: IExample
        example2.GenericExample(123)
        example2.GenericExample("test")

#[export]
abstract default class BaseExample =

    abstract default GenericExample<T>(x: T): () =
        Console.Write(x)

#[export]
class Example =
    inherits BaseExample
    implements IExample
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123test"
        )

[<Fact>]
let ``Complex test and csharp source 7``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}
        """

    let src =
        """
open System

abstract class BaseExample =

    abstract GenericExample<T>(T): ()

class Example =
    inherits BaseExample
    implements IExample

    overrides GenericExample<T>(x: T): () =
        Console.Write(x)

main(): () =
    let example = Example()
    let example2 = example: IExample
    example2.GenericExample(123)
    example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
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
        )

[<Fact>]
let ``Complex test and csharp source 8 - using an export``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}
        """

    let src =
        """
namespace Test

open System

module Test =

    main(): () =
        let example = Example()
        let example2 = example: IExample
        example2.GenericExample(123)
        example2.GenericExample("test")

#[export]
abstract class BaseExample =

    abstract GenericExample<T>(T): ()

#[export]
class Example =
    inherits BaseExample
    implements IExample

    overrides GenericExample<T>(x: T): () =
        Console.Write(x)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123test"
        )

[<Fact>]
let ``Complex test and csharp source 9``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}
        """

    let src =
        """
open System

abstract class BaseExample =

    GenericExample<T>(x: T): () = Console.Write("failed")

class Example =
    inherits BaseExample
    implements IExample

    #[export]
    new GenericExample<T>(x: T): () = Console.Write(x)

main(): () =
    let example = Example()
    let example2 = example: IExample
    example2.GenericExample(123)
    example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123test"
        )

[<Fact>]
let ``Complex test and csharp source 10 - cannot use a non-exported class inside a function that cannot be erased``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}
        """

    let src =
        """
open System

class NonExportedClass<T> =

    Value: T get, set
    new(value: T) = this { Value = value }

abstract class BaseExample =

    GenericExample<T>(x: T): () = Console.Write("failed")

class Example =
    inherits BaseExample
    implements IExample

    #[export]
    new GenericExample<T>(x: T): () =
        let x = NonExportedClass(x)
        Console.Write(x.Value)

main(): () =
    let example = Example()
    let example2 = example: IExample
    example2.GenericExample(123)
    example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
                |> withErrorHelperTextDiagnostics
                    [
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        let x = NonExportedClass(x)
                ^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        let x = NonExportedClass(x)
                ^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        let x = NonExportedClass(x)
                ^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        let x = NonExportedClass(x)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        Console.Write(x.Value)
                      ^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        Console.Write(x.Value)
                      ^^^^^^^
"""
            )
                    ]
                    |> ignore
        )

[<Fact>]
let ``Complex test and csharp source 11``() =
    let csSrc =
        """
public abstract class BaseExample
{
    public virtual void GenericExample<T>(T x)
    {
        System.Console.Write("failed");
    }
}
        """

    let src =
        """
open System

interface IExample =

    GenericExample<T>(x: T): ()

class Example =
    inherits BaseExample
    implements IExample

main(): () =
    let example = Example()
    let example2 = example: IExample
    example2.GenericExample(123)
    example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
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
        )

[<Fact>]
let ``Complex test and csharp source 11 - but explicit``() =
    let csSrc =
        """
public abstract class BaseExample
{
    public virtual void GenericExample<T>(T x)
    {
        System.Console.Write("failed");
    }
}
        """

    let src =
        """
open System

interface IExample =

    GenericExample<T>(x: T): ()

class Example =
    inherits BaseExample
    implements IExample

    new GenericExample<T>(x: T): () =
        Console.Write(x)

main(): () =
    let example = Example()
    let example2 = example: IExample
    example2.GenericExample(123)
    example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123test"
        )

[<Fact>]
let ``Complex test and csharp source 11 - but explicit without new``() =
    let csSrc =
        """
public abstract class BaseExample
{
    public virtual void GenericExample<T>(T x)
    {
        System.Console.Write("failed");
    }
}
        """

    let src =
        """
open System

interface IExample =

    GenericExample<T>(x: T): ()

class Example =
    inherits BaseExample
    implements IExample

    GenericExample<T>(x: T): () =
        Console.Write(x)

main(): () =
    let example = Example()
    let example2 = example: IExample
    example2.GenericExample(123)
    example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
                |> withErrorHelperTextDiagnostics
                    [
            ("The member 'GenericExample' will hide over its base.",
                """
    GenericExample<T>(x: T): () =
    ^^^^^^^^^^^^^^
"""
            )
            ("The function 'GenericExample<T>(x: T): ()' is not implemented for 'IExample' on 'Example'.",
                """
class Example =
      ^^^^^^^
"""
            )
                    ]
                    |> ignore
        )

[<Fact>]
let ``Complex test and csharp source 11 - but with export``() =
    let csSrc =
        """
public abstract class BaseExample
{
    public virtual void GenericExample<T>(T x)
    {
        System.Console.Write(x);
    }
}
        """

    let src =
        """
namespace Test

open System

#[export]
interface IExample =

    GenericExample<T>(x: T): ()

class Example =
    inherits BaseExample
    implements IExample

module Main =
    main(): () =
        let example = Example()
        let example2 = example: IExample
        example2.GenericExample(123)
        example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123test"
        )

[<Fact>]
let ``Complex test and csharp source 11 - but with export on function only``() =
    let csSrc =
        """
public abstract class BaseExample
{
    public virtual void GenericExample<T>(T x)
    {
        System.Console.Write(x);
    }
}
        """

    let src =
        """
namespace Test

open System

interface IExample =

    #[export]
    GenericExample<T>(x: T): ()

class Example =
    inherits BaseExample
    implements IExample

module Main =
    main(): () =
        let example = Example()
        let example2 = example: IExample
        example2.GenericExample(123)
        example2.GenericExample("test")
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123test"
        )

[<Fact>]
let ``Complex test and csharp source 12 - using export attributes``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}
        """

    let src =
        """
open System

#[export]
test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

#[export]
class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
class Example2 =
  implements IExample2

  new() = this { }

  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "testExample"
        )

[<Fact>]
let ``Complex test and csharp source 13 - using export attributes and wrapped in a lambda``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}
        """

    let src =
        """
open System

#[export]
test<Z>(x: Z): () where Z: IExample =
  Console.Write("test")
  x.GenericExample<Z>(x)

#[export]
class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
class Example2 =
  implements IExample2

  new() = this { }

  GenericExample<U>(x: U): () where U: IExample = 
      let f() =
          test<_>(x)
      f()

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "testExample"
        )

[<Fact>]
let ``Complex test and csharp source 14 - partially using export attributes``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}
        """

    let src =
        """
open System

#[export]
test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

class Example =
  implements IExample

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () = 
      Console.Write("Example")

class Example2 =
  implements IExample2

  new() = this { }
  
  #[export]
  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "testExample"
        )

[<Fact>]
let ``Complex test and csharp source 15 - using export attributes and local class``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}
        """

    let src =
        """
open System

#[export]
test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

#[export]
class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
class Example2 =
  implements IExample2

  new() = this { }

  GenericExample<U>(x: U): () where U: IExample = 
      class LocalClass =
        M(): () = Console.Write("LocalClass")
      let c = LocalClass()
      c.M()
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "LocalClasstestExample"
        )

[<Fact>]
let ``Complex test and csharp source 16 - using export attributes and local class using static``() =
    let csSrc =
        """
public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}
        """

    let src =
        """
open System

#[export]
test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

#[export]
class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
class Example2 =
  implements IExample2

  new() = this { }

  GenericExample<U>(x: U): () where U: IExample = 
      class LocalClass =
        static M(): () = Console.Write("LocalClass")
      LocalClass.M()
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "LocalClasstestExample"
        )

[<Fact>]
let ``Complex test and NO-csharp source - using no export attributes``() =
    let src =
        """
open System

interface IExample =
    
    GenericExample<T>(T): ()

interface IExample2 =

    GenericExample<T>(T): () where T: IExample

test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

class Example2 =
  implements IExample2

  new() = this { }
  
  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "testExample"

[<Fact>]
let ``Complex test and NO-csharp source 2 - using export attributes``() =
    let src =
        """
open System

#[export]
interface IExample =
    
    GenericExample<T>(T): ()

#[export]
interface IExample2 =

    GenericExample<T>(T): () where T: IExample

#[export]
test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

#[export]
class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
class Example2 =
  implements IExample2

  new() = this { }
  
  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "testExample"
    
[<Fact>]
let ``Complex test and NO-csharp source 3 - using export attributes and wrapped in a lambda``() =
    let src =
        """
open System

#[export]
interface IExample =
    
    GenericExample<T>(T): ()

#[export]
interface IExample2 =

    GenericExample<T>(T): () where T: IExample

#[export]
test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

#[export]
class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
class Example2 =
  implements IExample2

  new() = this { }
  
  GenericExample<U>(x: U): () where U: IExample = 
      let f() =
          test<_>(x)
      f()

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "testExample"

[<Fact>]
let ``Complex test and NO-csharp source 4 - partially using export attributes``() =
    let src =
        """
open System

#[export]
interface IExample =
    
    GenericExample<T>(T): ()

#[export]
interface IExample2 =

    GenericExample<T>(T): () where T: IExample

#[export]
test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

class Example =
  implements IExample

  new() = this { }

  #[export]
  GenericExample<U>(x: U): () = 
      Console.Write("Example")

class Example2 =
  implements IExample2

  new() = this { }
  
  #[export]
  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "testExample"

[<Fact>]
let ``Property test and csharp source``() =
    let csSrc =
        """
public interface IExample
{
    int X { get; }
}
        """

    let src =
        """
open System

class Test =
    implements IExample

    X: Int32 get
    new() = this { X = 123 }

main(): () =
    let t = Test()
    Console.Write(t.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123"
        )

[<Fact>]
let ``Multiple C# interfaces for different type extensions should fail because no trait``() =
    let csSrc =
        """
using System;

public interface IExample
{
    int X { get; }
}

public interface IDoot
{
    int Y { get; }
}

public static class Program
{
    public static void Test<T>(T x) where T: IExample, IDoot
    {
        Console.Write(x.X);
        Console.Write(x.Y);
    }
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Test

#[open]
extension ExampleTest =
    inherits Test
    implements IExample

    X: int32 get() = 123

#[open]
extension DootTest =
    inherits Test
    implements IDoot

    Y: int32 get() = 456

main(): () =
    Program.Test<Test>(Test())
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
                |> withErrorHelperTextDiagnostics
                    [
            ("Type instantiation 'Test' is missing the constraint 'IExample'.",
                """
    Program.Test<Test>(Test())
    ^^^^^^^^^^^^^^^^^^
"""
            )
            ("Type instantiation 'Test' is missing the constraint 'IDoot'.",
                """
    Program.Test<Test>(Test())
    ^^^^^^^^^^^^^^^^^^
"""
            )
                    ]
                    |> ignore
        )

[<Fact>]
let ``Multiple C# interfaces for different type extensions should pass because we casted to create a witness instance``() =
    let csSrc =
        """
using System;

public interface IExample
{
    int X { get; }
}

public interface IDoot
{
    int Y { get; }
}

public static class Program
{
    public static void Test<T>(T x) where T: IExample, IDoot
    {
        Console.Write(typeof(T).Name);
        Console.Write(x.GetType().Name);
        Console.Write(x.X);
        Console.Write(x.Y);
    }
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Test

module Helpers =
    #[intrinsic("cast")]
    Cast<T>(__oly_object): T

interface IDootExample =
    inherits IDoot, IExample

#[open]
extension DootExampleTest =
    inherits Test
    implements IDootExample

    X: int32 get() = 123

    Y: int32 get() = 456

main(): () =
    Program.Test(Helpers.Cast<IDootExample>(Test()))
        """
    OlyWithCSharp csSrc src
        (
            withCompile >>
            shouldRunWithExpectedOutput "IDootExample__oly_instance123456"
        )

[<Fact>]
let ``Second order generic on List``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[import("CLR:mscorlib", "System.Collections.Generic", "List`1")]
class List<T> =

    get_Item(__oly_int32): T

    new()

    Add(item: T) : ()

test<T<_>>(x: T<int32>): () where T<_>: List = x.Add(1)

main(): () =
    let x = List<int32>()
    test<_>(x)
    print(x.get_Item(0))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Second order generic on ICollection``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[import("CLR:mscorlib", "System.Collections.Generic", "ICollection`1")]
interface ICollection<T> =

    get_Item(__oly_int32): T

    Add(item: T) : ()

#[import("CLR:mscorlib", "System.Collections.Generic", "List`1")]
class List<T> =
    implements ICollection<T>

    get_Item(__oly_int32): T

    new()

    Add(item: T) : ()

test<T<_>>(x: T<int32>): () where T<_>: ICollection = x.Add(1)

main(): () =
    let x = List<int32>()
    test<_>(x)
    print(x.get_Item(0))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Second order generic on ICollection 2``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[import("CLR:mscorlib", "System.Collections.Generic", "ICollection`1")]
interface ICollection<T> =

    get_Item(__oly_int32): T

    Add(item: T) : ()

#[import("CLR:mscorlib", "System.Collections.Generic", "List`1")]
class List<T> =
    implements ICollection<T>

    get_Item(__oly_int32): T

    new()

    Add(item: T) : ()

test<T<_>>(x: T<int32>): () where T<_>: ICollection = x.Add(1)

main(): () =
    let x = List<int32>()
    let z = test<List>
    z(x)
    print(x.get_Item(0))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``String concat with fully qualified calls``() =
    let src =
        """
#[intrinsic("utf16")]
alias string

(+)(str1: string, str2: string): string = System.String.Concat(str1, str2)

main(): () =
    System.Console.Write("Hello" + "World")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "HelloWorld"

[<Fact>]
let ``Mappable example``() =
    let src =
        """
open System
open System.Collections.Generic
open extension ListMappable<_>

#[intrinsic("add")]
(+)(Int32, Int32): Int32

#[intrinsic("less_than")]
(<)(Int32, Int32): Boolean

interface IMappable<T<_>> =

    static abstract Map<A, B>(ta: T<A>, f: A -> B): T<B>

extension ListMappable<T> =
    inherits List<T>
    implements IMappable<List>

    static overrides Map<A, B>(list: List<A>, f: A -> B): List<B> =
        let newList = List<B>(list.Count)
        let loop(i) =
            if (i < list.Count)
                newList.Add(f(list.get_Item(i)))
                loop(i + 1)
        loop(0)
        newList

map<T<_>, A, B>(ta: T<A>, f: A -> B): T<B> where T: trait IMappable<T> =
    T.Map<A, B>(ta, f)

main(): () =
    let xs = List<Int32>()
    xs.Add(1)
    xs.Add(2)
    let xs2 = map(xs, x -> x + 100)
    Console.Write(xs2.get_Item(0))
    Console.Write(xs2.get_Item(1))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "101102"

[<Fact>]
let ``MemoryStream API that takes an array``() =
    let src =
        """
open System
open System.IO

main(): () =
    let ms = MemoryStream(mutable [])
    Console.Write("test")
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"

[<Fact>]
let ``typeof``() =
    let src =
        """
open System

#[intrinsic("constant")]
#[import("intrinsic-CLR", "", "typeof")]
typeof<require T>: Type

main(): () =
    let x = typeof<__oly_int32>
    Console.Write(x.Name)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Int32"

[<Fact>]
let ``typeof as a partial call``() =
    let src =
        """
open System

#[intrinsic("constant")]
#[import("intrinsic-CLR", "", "typeof")]
typeof<T>(): Type

main(): () =
    let x = typeof<__oly_int32>
    Console.Write(x().Name)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Int32"

[<Fact>]
let ``Call ToString on newly defined class``() =
    let src =
        """
class Test

main(): () =
    let x = Test()
    System.Console.Write(x.ToString())
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "__oly_gen_0+Test"

[<Fact>]
let ``Call ToString on newly defined class with overriding ToString``() =
    let src =
        """
class Test =

    overrides ToString(): __oly_utf16 = "overriding ToString"

main(): () =
    let x = Test()
    System.Console.Write(x.ToString())
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "overriding ToString"

[<Fact>]
let ``Call ToString on newly defined class with overriding ToString 2``() =
    let src =
        """
class Test =

    overrides ToString(): __oly_utf16 = "overriding ToString"

main(): () =
    let x = Test()
    let y: __oly_object = x
    System.Console.Write(y.ToString())
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "overriding ToString"

[<Fact>]
let ``Call ToString via witness on newly defined class``() =
    let src =
        """
class Test

test<T>(x: T): () where T: { ToString(): __oly_utf16 } = 
    System.Console.Write(x.ToString())

main(): () =
    let x = Test()
    test(x)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "__oly_gen_0+Test"

[<Fact>]
let ``Call ToString via witness on newly defined class 2``() =
    let src =
        """
class Test =

    overrides ToString(): __oly_utf16 = "overriding ToString"

test<T>(x: T): () where T: { ToString(): __oly_utf16 } = 
    System.Console.Write(x.ToString())

main(): () =
    let x = Test()
    test(x)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "overriding ToString"

[<Fact>]
let ``Call ToString via witness on newly defined class 3``() =
    let src =
        """
class Test =

    overrides ToString(): __oly_utf16 = "overriding ToString"

test<T>(x: T): () where T: { ToString(): __oly_utf16; GetType(): System.Type } = 
    System.Console.Write(x.ToString())
    System.Console.Write(x.GetType())

main(): () =
    let x = Test()
    test(x)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "overriding ToString__oly_gen_0+Test"

[<Fact>]
let ``Use of '()' should pass 1``() =
    let src =
        """
open System

#[intrinsic("base_object")]
alias object

print(o: object): () = Console.Write(o)
   
test(): (()) =
    ()

main(): () =
    print(test())
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "()"

[<Fact>]
let ``Use of '()' should pass 2``() =
    let src =
        """
open System

#[intrinsic("base_object")]
alias object

print(o: object): () = Console.Write(o)
   
test(): () =
    ()

main(): () =
    print(test())
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "()"

[<Fact>]
let ``Type extension of a generic type``() =
    let src =
        """
open System
open System.IO
open System.Collections.Generic
open extension ListExtras<_>

print(o: Object): () = Console.Write(o)

extension ListExtras<T> =
   inherits List<T>

   Doot(x: T): () = print(x)

   
main(): () =
   let x = List<Int32>()
   x.Doot(1)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Type extension of a generic type with #[export]``() =
    let src =
        """
namespace A

open System
open System.IO
open System.Collections.Generic
open extension A.ListExtras<_>

#[export]
extension ListExtras<T> =
   inherits List<T>

   Doot(x: T): () = print(x)

#[open]
module Test =

    print(o: Object): () = Console.Write(o)
   
    main(): () =
       let x = List<Int32>()
       x.Doot(1)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Implicit default ctors should output correctly``() =
    let src =
        """
open System

abstract class Test =

   public field mutable X: Int32 =
      let x = 123
      x

class Test2 =
   inherits Test

main(): () =
   let t = Test2()
   Console.Write(t.X)
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Index getter/setter call``() =
    let src =
        """
open System
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

shape DotNetIndexGetter<TKey, TValue> =

    get_Item(TKey): TValue

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: DotNetIndexGetter<TKey, TValue> = x.get_Item(key)

shape DotNetIndexSetter<TKey, TValue> =

    set_Item(TKey, TValue): ()

(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: DotNetIndexSetter<TKey, TValue> = x.set_Item(key, value)

main(): () =
    let lookup = Dictionary<__oly_utf16, int32>()
    lookup["hello"] <- 123
    Console.Write(lookup["hello"])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Index getter/setter call 2``() =
    let src =
        """
open System
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

shape DotNetIndexGetter<TKey, TValue> =

    get_Item(TKey): TValue

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: DotNetIndexGetter<TKey, TValue> = x.get_Item(key)

shape DotNetIndexSetter<TKey, TValue> =

    set_Item(TKey, TValue): ()

(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: DotNetIndexSetter<TKey, TValue> = x.set_Item(key, value)

main(): () =
    let strs = List<__oly_utf16>()
    strs.Add("hello")
    Console.Write(strs[0])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``get_Item for ReadOnlySpan``() =
    let src =
        """
open System
open System.Collections.Generic

main(): () =
    let xs = mutable [1;2;3]
    let mutable xs = ReadOnlySpan(xs)
    Console.Write(xs.get_Item(1))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"

[<Fact>]
let ``Intrinsic alias should be able to get mscorlib methods``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

main(): () =
    let mutable x: int32 = 1
    Console.Write(x.ToString())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Get enumerator should pass``() =
    let src =
        """
open System
open System.Collections.Generic

main(): () =
    let xs = List<__oly_int32>()
    let e = xs.GetEnumerator()
    Console.Write("123")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Task Run example``() =
    let src =
        """
open System
open System.Threading.Tasks

main(): () =
    let task = Task.Run(() -> ())
    ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Task Run example 2``() =
    let src =
        """
open System
open System.Threading.Tasks

main(): () =
    let task = Task.Run<__oly_int32>(() -> Task.FromResult(1))
    ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Task Run example 3``() =
    let src =
        """
open System
open System.Threading.Tasks

main(): () =
    let task = Task.Run(() -> Task.FromResult(1))
    ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Task Run example 4``() =
    // We are testing the overloads here as 'null' is for the TaskScheduler.
    let src =
        """
open System
open System.Threading.Tasks

main(): () =
    let task = Task.Run(() -> ())
    let w = task.ContinueWith(x -> (), null)
        """
    Oly src
    |> shouldCompile

[<Fact>]
let ``State machine example``() =
    let src =
        """
open System
open System.Threading
open System.Threading.Tasks
open System.Runtime.CompilerServices

struct StateMachine<A, B> =
    implements IAsyncStateMachine

    mutable MoveNext(): () = ()

    mutable SetStateMachine(stateMachine: IAsyncStateMachine): () = ()

main(): () =
    let stateMachine = default: StateMachine<__oly_int32, __oly_int32>
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``State machine example 2``() =
    let src =
        """
open System
open System.Threading
open System.Threading.Tasks
open System.Runtime.CompilerServices

#[intrinsic("not")]
(!)(__oly_bool): __oly_bool

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct StateMachine<A, B> =
    implements IAsyncStateMachine

    public field mutable f: A -> Task<B> = unchecked default

    public field mutable state: B = unchecked default

    public field mutable builder: AsyncTaskMethodBuilder<B> = unchecked default

    public field mutable t: Task<A> = unchecked default

    field mutable u: TaskAwaiter<A> = unchecked default

    mutable MoveNext(): () =
        let mutable value = this.state
        let mutable result = unchecked default: B

        let mutable awaiter = this.t.GetAwaiter(): TaskAwaiter<A>
        if(!awaiter.IsCompleted)
          this.u <- awaiter
       //   this.builder.AwaitUnsafeOnCompleted(&awaiter, &this)
        else
          ()
         // result <- this.f(awaiter.GetResult())

    mutable SetStateMachine(stateMachine: IAsyncStateMachine): () =
      this.builder.SetStateMachine(stateMachine)

(let!)<A, B>(a: Task<A>, f: A -> Task<B>): Task<B> =
    let mutable stateMachine = unchecked default: StateMachine<A, B>
    stateMachine.builder <- AsyncTaskMethodBuilder<B>.Create()
    stateMachine.t <- a
    stateMachine.state <- unchecked default
    stateMachine.builder.Start(&stateMachine)
    stateMachine.builder.Task
  
(return)<A>(a: A): Task<A> = Task.FromResult(a)

test(t: Task<__oly_int32>): Task<__oly_int32> =
  let! result = t
  return result

main(): () =
  let x = Task.FromResult(123)
  let result = test(x)
  ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``AsyncTaskMethodBuilder example``() =
    let src =
        """
open System
open System.Threading
open System.Threading.Tasks
open System.Runtime.CompilerServices

#[intrinsic("not")]
(!)(__oly_bool): __oly_bool

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>


main(): () =
  let x = AsyncTaskMethodBuilder<__oly_int32>.Create()
  ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Export example 1``() =
    let src =
        """
namespace Test

#[export]
class GenericType<Z> where Z: struct =

    Test<U>(u: U): Z = default

module Test =
    main(): () =
      let x = GenericType<__oly_int32>()
      ()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Indexer operator example with struct with export``() =
    let src =
        """
namespace A

#[intrinsic("by_ref")]
alias byref<T>

#[export]
struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

struct Test2 =

    public field mutable s: Test<__oly_int32> = default

module Test =
    #[intrinsic("print")]
    print(__oly_object): ()

    (`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

    main(): () =
        let mutable s = Test2()
        let x = s.s[0]
        print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"

[<Fact>]
let ``Simple functions and private ones with export``() =
    let src =
        """
namespace A

#[open]
module Prelude =

    #[intrinsic("print")]
    print(__oly_object): ()

    main() : () =
        Test.test()

#[export]
module Test =

    private test_private(): () =
        print("test")
        print("_private")

    test(): () = Test.test_private()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test_private"

[<Fact>]
let ``Use mscorlib type to pass the shape constraint``() =
    let src =
        """
open System
open System.Diagnostics

test<T>(x: T): () where T: { Elapsed: TimeSpan get } = ()

main() : () =
    let s = Stopwatch()
    test(s)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Using SystemValueType subsumption``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let result = 1234
    let result2 = result: ValueType
    print(result2)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1234"

[<Fact>]
let ``Using SystemValueType subsumption 2``() =
    let src =
        """
open System

struct Test

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let result = Test()
    let result2 = result: ValueType
    print("1234")
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1234"

[<Fact>]
let ``Overrides ToString()``() =
    let src =
        """
class Test =
    inherits System.Object

    overrides ToString(): __oly_utf16 = "overrides"

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let result: System.Object = Test()
    print(result.ToString())
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "overrides"

[<Fact>]
let ``Overrides ToString() 2``() =
    let src =
        """
class Test =

    overrides ToString(): __oly_utf16 = "overrides"

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let result: System.Object = Test()
    print(result.ToString())
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "overrides"

[<Fact>]
let ``System Convert ToUInt64``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    print(System.Convert.ToUInt64(123))
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``StructLayout``() =
    let src =
        """
open System.Runtime.InteropServices

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

struct Test =
    public field X: int32 = 123
    public field Y: int32 = 456

main(): () =
    let t = Test()
    print(t.X)
    print(t.Y)

    let size = System.Runtime.InteropServices.Marshal.SizeOf(default: Test)
    print(size)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1234568"

[<Fact>]
let ``Regression Vector3 with witness multiply``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

module Main =
    main() : () =
        let v1 = Vector3.Zero
        print(v1 * v1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<0, 0, 0>"

[<Fact>]
let ``Regression Vector3 with witness multiply 2``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

class AClass =

    Mtd(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<0, 0, 0>"

[<Fact>]
let ``Regression Vector3 with witness multiply 3``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

#[export]
class AClass =

    Mtd(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<0, 0, 0>"

[<Fact>]
let ``Regression Vector3 with witness multiply 4``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

#[export]
class AClass =

    Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd<__oly_object>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<0, 0, 0>"

[<Fact>]
let ``Regression Vector3 with witness multiply 5``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

#[export]
abstract class BClass =

    abstract default Mtd<U>(): Vector3 = Vector3.One

#[export]
class AClass =
    inherits BClass

    overrides Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd<__oly_object>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<0, 0, 0>"

[<Fact>]
let ``Regression Vector3 with witness multiply 6``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

#[export]
abstract class BClass<T> =

    abstract default Mtd<U>(): Vector3 = Vector3.One

#[export]
class AClass =
    inherits BClass<__oly_utf16>

    overrides Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd<__oly_object>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<0, 0, 0>"

[<Fact>]
let ``Regression Vector3 with witness multiply 7``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

#[export]
abstract class BClass =

    abstract default Mtd<U>(): Vector3 = Vector3.One

#[export]
class AClass<T> =
    inherits BClass

    overrides Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass<__oly_utf16>()
        print(a.Mtd<__oly_object>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<0, 0, 0>"

[<Fact>]
let ``Regression Vector3 with witness multiply 8``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

#[export]
abstract class BClass<T> =

    abstract default Mtd<U>(): Vector3 = Vector3.One

#[export]
class AClass<T> =
    inherits BClass<T>

    overrides Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass<__oly_utf16>()
        print(a.Mtd<__oly_object>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<0, 0, 0>"

[<Fact>]
let ``Regression Vector3 with witness multiply 9``() =
    let src =
        """
namespace Test

open System.Numerics

#[open]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()

    (*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)

#[export]
abstract class BClass =

    abstract default Mtd<U>(): Vector3 = Vector3.One * Vector3.One

#[export]
class AClass<T> =
    inherits BClass

module Main =
    main() : () =
        let a = AClass<__oly_utf16>()
        print(a.Mtd<__oly_object>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "<1, 1, 1>"

[<Fact(Skip = "This test is weird because running it with dotnet.exe explicitly will result in Goo Goo for release. For this test in process, it is Goo Bar for release.")>]
let ``Weird one``() =
    let src =
        """
namespace Test

#[open]
module Prelude =

    #[intrinsic("print")]
    print(__oly_object): ()
    
    #[intrinsic("int32")]
    alias int
    
    #[intrinsic("utf16")]
    alias string
    
    #[intrinsic("by_ref")]
    alias byref<T>
    
    #[intrinsic("address_of")]
    (&)<T>(T): byref<T>
    
    #[intrinsic("add")]
    (+)(int, int): int
    
    #[intrinsic("unsafe_cast")]
    unsafeCast<T>(__oly_object): T

#[export]
module M =

    interface IMoveable =
        Position: int get, set
   
    class Item =
        implements IMoveable

        Name: string get, set = "Bar" 

        Position: int
            get() =
                print("Get - ")
                print(this.Name)
                print(" ")
                0
            set(value) =
                print("Set - ")
                print(this.Name)
                print(" ")

    GetOffset<T>(item: byref<T>): int =
        let item2 = Item()
        item2.Name <- "Bar"
        item <- unsafeCast(item2)
        0

    Shift<T>(mutable item : T): () where T: IMoveable, not struct =
        item.Position <- item.Position + (GetOffset(&item))

    main(): () =
        let item = Item()
        item.Name <- "Goo"
        Shift(item)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Get - Goo Set - Goo "

[<Fact>]
let ``Weird one 2``() =
    let src =
        """
namespace Test

#[open]
module Prelude =

    #[intrinsic("print")]
    print(__oly_object): ()
    
    #[intrinsic("int32")]
    alias int
    
    #[intrinsic("utf16")]
    alias string
    
    #[intrinsic("by_ref")]
    alias byref<T>
    
    #[intrinsic("address_of")]
    (&)<T>(T): byref<T>
    
    #[intrinsic("add")]
    (+)(int, int): int
    
    #[intrinsic("unsafe_cast")]
    unsafeCast<T>(__oly_object): T

#[export]
module M =

    interface IMoveable =
        Position: int get, set

    struct Item =
        implements IMoveable

        Name: string get, set = "Bar"

        Position: int
            get() =
                print("Get - ")
                print(this.Name)
                print(" ")
                0
            set(value) =
                print("Set - ")
                print(this.Name)
                print(" ")

    GetOffset<T>(item: byref<T>): int where T: IMoveable =
        let mutable item2 = Item()
        item2.Name <- "Bar"
        item <- unsafeCast(item2)
        0

    Shift<T>(mutable item : T): () where T: IMoveable, struct =
        item.Position <- item.Position + (GetOffset(&item))

    main(): () =
        let mutable item = Item()
        item.Name <- "Goo"
        Shift(item)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Get - Goo Set - Bar "

[<Fact>]
let ``Set a pointer with a value``() =
    let src =
        """
#[intrinsic("void")]
alias void

#[intrinsic("uint8")]
alias uint8

#[intrinsic("int32")]
alias int32

#[intrinsic("uint64")]
alias uint64

#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("unsafe_cast")]
unsafeCast<T>(__oly_object): T

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("print")]
print(__oly_object): ()

alloc(numberOfBytes: int32): void* =
    unsafeCast(System.Runtime.InteropServices.Marshal.AllocHGlobal(numberOfBytes))

free(pBytes: void*): () =
    System.Runtime.InteropServices.Marshal.FreeHGlobal(unsafeCast(pBytes))

module X64 =
    
    Emit(dat: uint64, size: uint8, buffer: void*): () =
        unsafeCast<byref<uint64>>(buffer) <- dat

main(): () =
    let m = alloc(128)
    X64.Emit(123, 8, m)
    print("test")
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"

[<Fact>]
let ``Set a value with a null ptr``() =
    let src =
        """
#[intrinsic("void")]
alias void

#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("load_null_ptr")]
nullptr<T>: T*

test(x: void*): () = print("test")

main(): () =
    test(nullptr)
    """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "test"


[<Fact>]
let ``Simple throw should give the right exception``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("throw")]
(throw)<TResult>(System.Exception): TResult

test(): __oly_int32 =
    throw System.Exception("a message for throw")

main(): () =
    try
        let result = test()
        print("should not happen")
    catch (ex: System.Exception) =>
        print(ex.Message)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "a message for throw"

[<Fact(Skip = "Should work but we should switch to something that is not a message box")>]
let ``Simple PInvoke``() =
    let src =
        """
open System
open System.Runtime.InteropServices

#[intrinsic("uint32")]
alias uint32

#[intrinsic("int32")]
alias int32

#[intrinsic("utf16")]
alias string

#[intrinsic("native_int")]
alias nint

#[import("C", "user32.dll", "MessageBox")]
MessageBox(hWnd: nint, lpText: string, lpCaption: string, uType: uint32): int32

main(): () =
    let result = MessageBox(default, "Command-line message box", "Attention!", 0)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Void* overload should work``() =
    let src =
        """
#[intrinsic("void")]
alias void

#[intrinsic("base_object")]
alias object

#[intrinsic("uint32")]
alias uint32

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("print")]
print(object): ()

#[intrinsic("unsafe_cast")]
(*)<T>(void*): byref<T>

#[intrinsic("unsafe_cast")]
(*)<T>(T*): byref<T>

#[intrinsic("unsafe_cast")]
UnsafeCast<T>(object): T

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

main(): () =
    let mutable result: uint32 = 123
    let resultAddr: int32* = UnsafeCast(&&result)
    let result: int32 = *resultAddr
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Multiple type parameters on System_Numerics_Vector3 extension with shape constraint should compile``() =
    let src =
        """
open System.Numerics

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("add")]
(+)(float32, float32): float32

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static (+)<T4, T5, T6>(T4, T5): T6 where T4: { static op_Addition(T4, T5): T6 } }, { static op_Addition(T1, T2): T3 } = 
    T1.(+)<T1, T2, T3>(x, y)

#[open]
extension AddExtension =
    inherits Vector3

    static (+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Addition(T1, T2): T3 } = 
        T1.op_Addition(x, y)

main(): () =
    let mutable v = Vector3(0)
    v.X <- 1
    v.Y <- 2
    v.Z <- 3
    
    let result: Vector3 = v + v

    print(result.X)
    print(result.Y)
    print(result.Z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "246"

[<Fact>]
let ``Multiple type parameters on System_Numerics_Vector3 extension with shape constraint should compile 2``() =
    let src =
        """
open System.Numerics

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("add")]
(+)(float32, float32): float32

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static (+)<T4, T5, T6>(T4, T5): T6 where T4: { static op_Addition(T4, T5): T6 } }, { static op_Addition(T1, T2): T3 } = 
    T1.(+)<T1, T2, T3>(x, y)

#[open]
extension AddExtension =
    inherits object

    static (+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Addition(T1, T2): T3 } = 
        T1.op_Addition(x, y)

main(): () =
    let mutable v = Vector3(0)
    v.X <- 1
    v.Y <- 2
    v.Z <- 3
    
    let result: Vector3 = v + v

    print(result.X)
    print(result.Y)
    print(result.Z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "246"

[<Fact>]
let ``Multiple type parameters on System_Numerics_Vector3 extension with shape constraint should compile 3``() =
    let src =
        """
open System.Numerics

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("add")]
(+)(float32, float32): float32

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static (+)<T4, T5, T6>(T4, T5): T6 where T4: { static op_Addition(T4, T5): T6 } }, { static op_Addition(T1, T2): T3 } = 
    T1.(+)<T1, T2, T3>(x, y)

#[open]
extension AddExtension =
    inherits object

    static (+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Addition(T1, T2): T3 } = 
        T1.op_Addition(x, y)

main(): () =
    let mutable v = Vector3(0)
    v.X <- 1
    v.Y <- 2
    v.Z <- 3
    
    let result = v + v // must infer correctly

    print(result.X)
    print(result.Y)
    print(result.Z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "246"

[<Fact>]
let ``Multiple type parameters on System_Numerics_Vector3 extension with shape constraint should compile 4``() =
    let refSrc =
        """
#[open]
module RefModule

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("add")]
(+)(float32, float32): float32

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static (+)<T4, T5, T6>(T4, T5): T6 where T4: { static op_Addition(T4, T5): T6 } }, { static op_Addition(T1, T2): T3 } = 
    T1.(+)<T1, T2, T3>(x, y)

#[open]
extension AddExtension =
    inherits object

    static (+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Addition(T1, T2): T3 } = 
        T1.op_Addition(x, y)
        """
    let src =
        """
open System.Numerics

main(): () =
    let mutable v = Vector3(0)
    v.X <- 1
    v.Y <- 2
    v.Z <- 3
    
    let result: Vector3 = v + v

    print(result.X)
    print(result.Y)
    print(result.Z)
        """
    OlyWithRef refSrc src
    |> withCompile
    |> shouldRunWithExpectedOutput "246"

[<Fact>]
let ``Extension Vector3 should work``() =
    let refSrc =
        """
#[open]
module RefModule

#[intrinsic("float32")]
alias float32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("multiply")]
(*)(float32, float32): float32

(*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)
        """
    let src =
        """
open System.Numerics

main(): () =
    let mutable v = Vector3(0)
    v.X <- 1
    v.Y <- 2
    v.Z <- 3
    
    let result = v * v

    print(result.X)
    print(result.Y)
    print(result.Z)
        """
    OlyWithRef refSrc src
    |> withCompile
    |> shouldRunWithExpectedOutput "149"

[<Fact>]
let ``Test various branch sizes``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(int32, int32): bool

#[inline(never)]
test1(x: int32): () =
    if (x == 1)
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
    else
        print("failed")

#[inline(never)]
test2(x: int32): () =
    if (x == 1)
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
        print("")
    else
        print("failed")

main(): () =
    test1(1)
    test2(1)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""
    |> ignore

[<Fact>]
let ``Use 'Length' property from string``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("utf16")]
alias string

main(): () =
    let str: System.String = "asdf"
    print(str.Length)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"

[<Fact>]
let ``Use 'Length' property from string 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("utf16")]
alias string

main(): () =
    let str = "asdf"
    print(str.Length)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"

[<Fact>]
let ``Use 'Length' property from string 3``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("utf16")]
alias string

main(): () =
    print("asdf".Length)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "4"

[<Fact>]
let ``Use 'MaxValue' from uint16``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("uint16")]
alias uint16

main(): () =
    print(uint16.MaxValue)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "65535"

[<Fact>]
let ``Simple Try expression``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("throw")]
(throw)<TResult>(Exception): TResult

main(): () =
    try
        try
            throw Exception("an exception")
        catch (ex: Exception) =>
            ()
    finally
        print("printing the finally")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "printing the finally"

[<Fact>]
let ``Simple Try expression 2``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("throw")]
(throw)<TResult>(Exception): TResult

main(): () =
    try
        throw Exception("an exception")
    catch (ex: Exception) =>
        ()
    finally
        print("printing the finally")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "printing the finally"

[<Fact>]
let ``Simple Try expression 3``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("throw")]
(throw)<TResult>(Exception): TResult

main(): () =
    try
        throw Exception("an exception")
    catch (ex: Exception) =>
        print(ex.Message)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "an exception"

[<Fact>]
let ``Marshal sizeof should work``() =
    let src =
        """
open System.Runtime.InteropServices

#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

struct TestData =
    public field X: int32 = 1
    public field Y: int32 = 2

sizeof<require T>: int32 =
    Marshal.SizeOf(unchecked default: T)

#[inline(never)]
test<T>(x: T): int32 =
    sizeof<T>

main(): () =
    let x = test(1)
    let y = test(TestData())
    print(x)
    print(y)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "48"

[<Fact>]
let ``Indexing into a ReadOnlySpan of a ref type should give the correct value``() =
    let src =
        """
open System

#[intrinsic("uint64")]
alias uint64

#[intrinsic("print")]
print(__oly_object): ()

class TestData =
    public field X: uint64 = 123456789

main(): () =
    let mutable xs = ReadOnlySpan(mutable [TestData()])
    print(xs.get_Item(0).X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123456789"

[<Fact>]
let ``Cast to SystemEnum``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

enum E =
    | A

main(): () =
    let x: Enum = E.A
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "A"

[<Fact>]
let ``Cast to SystemValueType``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

enum E =
    | A

main(): () =
    let x: ValueType = E.A
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "A"

[<Fact>]
let ``Enum equality overload check``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("bool")]
alias bool

#[intrinsic("equal")]
(==)(value1: Enum, value2: Enum): bool

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

#[intrinsic("or")]
(||)(bool, bool): bool

enum E =
    | A
    | B
    | C

#[open]
extension EExtensions =
    inherits E

    static op_Equality(value1: E, value2: E): bool =
        print("passed")
        true

main(): () =
    let x = E.A
    if (x == E.A)
        ()
    else
        print("failed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``typeof can be usesd in attribute``() =
    let src =
        """
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("constant")]
#[import("intrinsic-CLR", "", "typeof")]
typeof<require T>: System.Type

#[blittable]
#[UnmanagedCallersOnly() { CallConvs = [typeof<CallConvCdecl>] }]
test(): () =
    ()

main(): () =
    print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Compile prelude``() =
    let src =
        """
#[open]
module OlyPrelude

#[intrinsic("void")]
alias void

#[intrinsic("uint8")]
alias byte

alias uint8 = byte

#[intrinsic("int8")]
alias int8

#[intrinsic("uint16")]
alias uint16

#[intrinsic("int16")]
alias int16

#[intrinsic("uint32")]
alias uint32

#[intrinsic("int32")]
alias int32

#[intrinsic("uint64")]
alias uint64

#[intrinsic("int64")]
alias int64

#[intrinsic("float32")]
alias float32

#[intrinsic("float64")]
alias float64

#[intrinsic("native_int")]
alias nint

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("bool")]
alias bool

#[intrinsic("utf16")]
alias string

#[intrinsic("char16")]
alias char

#[intrinsic("base_object")]
alias object

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("native_ptr")]
alias (*)<T>

// Cast Operations


#[intrinsic("unsafe_cast")]
uint16(byte): uint16

#[intrinsic("unsafe_cast")]
uint32(uint16): uint32

#[intrinsic("unsafe_cast")]
uint32(int32): uint32

#[intrinsic("unsafe_cast")]
uint32(int64): uint32

#[intrinsic("unsafe_cast")]
uint32(uint64): uint32

#[intrinsic("unsafe_cast")]
uint32(float32): uint32

#[intrinsic("unsafe_cast")]
int32(byte): int32

#[intrinsic("unsafe_cast")]
int32(uint16): int32

#[intrinsic("unsafe_cast")]
int32(uint32): int32

#[intrinsic("unsafe_cast")]
int32(int64): int32

#[intrinsic("unsafe_cast")]
int32(uint64): int32

#[intrinsic("unsafe_cast")]
int32(float32): int32

#[intrinsic("unsafe_cast")]
int32(float64): int32

#[intrinsic("unsafe_cast")]
uint64(uint32): uint64

#[intrinsic("unsafe_cast")]
uint64(int32): uint64

#[intrinsic("unsafe_cast")]
uint64(int64): uint64

#[intrinsic("unsafe_cast")]
int64(int32): int64

#[intrinsic("unsafe_cast")]
int64(uint32): int64

#[intrinsic("unsafe_cast")]
int64(uint64): int64

#[intrinsic("unsafe_cast")]
int64(float64): int64

#[intrinsic("unsafe_cast")]
float32(byte): float32

#[intrinsic("unsafe_cast")]
float32(uint16): float32

#[intrinsic("unsafe_cast")]
float32(uint32): float32

#[intrinsic("unsafe_cast")]
float32(int32): float32

#[intrinsic("unsafe_cast")]
float32(int64): float32

#[intrinsic("unsafe_cast")]
float32(float64): float32

#[intrinsic("unsafe_cast")]
float64(uint8): float64

#[intrinsic("unsafe_cast")]
float64(uint16): float64

#[intrinsic("unsafe_cast")]
float64(int32): float64

#[intrinsic("unsafe_cast")]
float64(int64): float64

#[intrinsic("unsafe_cast")]
float64(float32): float64

#[intrinsic("unsafe_cast")]
char(nuint): char

#[intrinsic("unsafe_cast")]
nint<T>(T*): nint

#[intrinsic("unsafe_cast")]
nint(void*): nint

#[intrinsic("unsafe_cast")]
nint(int32): nint

#[intrinsic("unsafe_cast")]
nint<TResult, TArguments...>(static blittable (TArguments...) -> TResult): nint

#[intrinsic("unsafe_cast")]
nuint(int32): nuint

#[intrinsic("unsafe_cast")]
nuint(uint32): nuint

#[intrinsic("unsafe_cast")]
nuint(uint64): nuint

#[intrinsic("unsafe_cast")]
nuint(uint8): nuint

#[intrinsic("unsafe_cast")]
nuint(void*): nuint

#[intrinsic("unsafe_cast")]
nuint<TResult, TArguments...>(static blittable (TArguments...) -> TResult): nuint

#[intrinsic("unsafe_cast")]
ToVoidPtr(int32): void*

#[intrinsic("unsafe_cast")]
ToVoidPtr(nuint): void*

#[intrinsic("unsafe_cast")]
UnsafeCast<T>(object): T

#[intrinsic("unsafe_cast")]
ToPtr<T>(int32): T*

#[intrinsic("unsafe_cast")]
ToPtr<T>(void*): T*

#[intrinsic("unsafe_cast")]
ToPtr<T>(nint): T*

#[intrinsic("unsafe_cast")]
ToPtr<T, TResult>(T*): TResult*

#[intrinsic("unsafe_cast")]
(*)<T>(void*): byref<T>

#[intrinsic("unsafe_cast")]
(*)<T>(T*): byref<T>

// Other Operations


#[intrinsic("load_null_ptr")]
nullptr<T>: T*

#[intrinsic("add")]
(+)(uint8, uint8): uint8
#[intrinsic("add")]
(+)(int8, int8): int8
#[intrinsic("add")]
(+)(uint16, uint16): uint16
#[intrinsic("add")]
(+)(int16, int16): int16
#[intrinsic("add")]
(+)(uint32, uint32): uint32
#[intrinsic("add")]
(+)(int32, int32): int32
#[intrinsic("add")]
(+)(uint64, uint64): uint64
#[intrinsic("add")]
(+)(int64, int64): int64
#[intrinsic("add")]
(+)(float32, float32): float32
#[intrinsic("add")]
(+)(float64, float64): float64
#[intrinsic("add")]
(+)(nuint, nuint): nuint

#[inline]
(+)(ptr: void*, value: nuint): void* =
    ToVoidPtr(nuint(ptr) + value)

#[inline]
(+)(ptr: void*, value: uint64): void* =
    ptr + nuint(value)

#[intrinsic("subtract")]
(-)(uint8, uint8): uint8
#[intrinsic("subtract")]
(-)(int8, int8): int8
#[intrinsic("subtract")]
(-)(uint16, uint16): uint16
#[intrinsic("subtract")]
(-)(int16, int16): int16
#[intrinsic("subtract")]
(-)(uint32, uint32): uint32
#[intrinsic("subtract")]
(-)(int32, int32): int32
#[intrinsic("subtract")]
(-)(uint64, uint64): uint64
#[intrinsic("subtract")]
(-)(int64, int64): int64
#[intrinsic("subtract")]
(-)(float32, float32): float32
#[intrinsic("subtract")]
(-)(float64, float64): float64

#[intrinsic("multiply")]
(*)(uint8, uint8): uint8
#[intrinsic("multiply")]
(*)(int8, int8): int8
#[intrinsic("multiply")]
(*)(uint16, uint16): uint16
#[intrinsic("multiply")]
(*)(int16, int16): int16
#[intrinsic("multiply")]
(*)(uint32, uint32): uint32
#[intrinsic("multiply")]
(*)(int32, int32): int32
#[intrinsic("multiply")]
(*)(uint64, uint64): uint64
#[intrinsic("multiply")]
(*)(int64, int64): int64
#[intrinsic("multiply")]
(*)(float32, float32): float32
#[intrinsic("multiply")]
(*)(float64, float64): float64

#[intrinsic("divide")]
(/)(uint8, uint8): uint8
#[intrinsic("divide")]
(/)(int8, int8): int8
#[intrinsic("divide")]
(/)(uint16, uint16): uint16
#[intrinsic("divide")]
(/)(int16, int16): int16
#[intrinsic("divide")]
(/)(uint32, uint32): uint32
#[intrinsic("divide")]
(/)(int32, int32): int32
#[intrinsic("divide")]
(/)(uint64, uint64): uint64
#[intrinsic("divide")]
(/)(int64, int64): int64
#[intrinsic("divide")]
(/)(float32, float32): float32
#[intrinsic("divide")]
(/)(float64, float64): float64

#[intrinsic("remainder")]
(%)(uint8, uint8): uint8
#[intrinsic("remainder")]
(%)(int8, int8): int8
#[intrinsic("remainder")]
(%)(uint16, uint16): uint16
#[intrinsic("remainder")]
(%)(int16, int16): int16
#[intrinsic("remainder")]
(%)(uint32, uint32): uint32
#[intrinsic("remainder")]
(%)(int32, int32): int32
#[intrinsic("remainder")]
(%)(uint64, uint64): uint64
#[intrinsic("remainder")]
(%)(int64, int64): int64
#[intrinsic("remainder")]
(%)(float32, float32): float32
#[intrinsic("remainder")]
(%)(float64, float64): float64

#[intrinsic("less_than")]
(<)(uint8, uint8): bool
#[intrinsic("less_than")]
(<)(int8, int8): bool
#[intrinsic("less_than")]
(<)(uint16, uint16): bool
#[intrinsic("less_than")]
(<)(int16, int16): bool
#[intrinsic("less_than")]
(<)(uint32, uint32): bool
#[intrinsic("less_than")]
(<)(int32, int32): bool
#[intrinsic("less_than")]
(<)(uint64, uint64): bool
#[intrinsic("less_than")]
(<)(int64, int64): bool
#[intrinsic("less_than")]
(<)(float32, float32): bool
#[intrinsic("less_than")]
(<)(float64, float64): bool

#[intrinsic("less_than_or_equal")]
(<=)(uint8, uint8): bool
#[intrinsic("less_than_or_equal")]
(<=)(int8, int8): bool
#[intrinsic("less_than_or_equal")]
(<=)(uint16, uint16): bool
#[intrinsic("less_than_or_equal")]
(<=)(int16, int16): bool
#[intrinsic("less_than_or_equal")]
(<=)(uint32, uint32): bool
#[intrinsic("less_than_or_equal")]
(<=)(int32, int32): bool
#[intrinsic("less_than_or_equal")]
(<=)(uint64, uint64): bool
#[intrinsic("less_than_or_equal")]
(<=)(int64, int64): bool
#[intrinsic("less_than_or_equal")]
(<=)(float32, float32): bool
#[intrinsic("less_than_or_equal")]
(<=)(float64, float64): bool

#[intrinsic("greater_than")]
(>)(uint8, uint8): bool
#[intrinsic("greater_than")]
(>)(int8, int8): bool
#[intrinsic("greater_than")]
(>)(uint16, uint16): bool
#[intrinsic("greater_than")]
(>)(int16, int16): bool
#[intrinsic("greater_than")]
(>)(uint32, uint32): bool
#[intrinsic("greater_than")]
(>)(int32, int32): bool
#[intrinsic("greater_than")]
(>)(uint64, uint64): bool
#[intrinsic("greater_than")]
(>)(int64, int64): bool
#[intrinsic("greater_than")]
(>)(float32, float32): bool
#[intrinsic("greater_than")]
(>)(float64, float64): bool

#[intrinsic("greater_than_or_equal")]
(>=)(uint8, uint8): bool
#[intrinsic("greater_than_or_equal")]
(>=)(int8, int8): bool
#[intrinsic("greater_than_or_equal")]
(>=)(uint16, uint16): bool
#[intrinsic("greater_than_or_equal")]
(>=)(int16, int16): bool
#[intrinsic("greater_than_or_equal")]
(>=)(uint32, uint32): bool
#[intrinsic("greater_than_or_equal")]
(>=)(int32, int32): bool
#[intrinsic("greater_than_or_equal")]
(>=)(uint64, uint64): bool
#[intrinsic("greater_than_or_equal")]
(>=)(int64, int64): bool
#[intrinsic("greater_than_or_equal")]
(>=)(float32, float32): bool
#[intrinsic("greater_than_or_equal")]
(>=)(float64, float64): bool

#[intrinsic("equal")]
(==)(uint8, uint8): bool
#[intrinsic("equal")]
(==)(int8, int8): bool
#[intrinsic("equal")]
(==)(uint16, uint16): bool
#[intrinsic("equal")]
(==)(int16, int16): bool
#[intrinsic("equal")]
(==)(uint32, uint32): bool
#[intrinsic("equal")]
(==)(int32, int32): bool
#[intrinsic("equal")]
(==)(uint64, uint64): bool
#[intrinsic("equal")]
(==)(int64, int64): bool
#[intrinsic("equal")]
(==)(float32, float32): bool
#[intrinsic("equal")]
(==)(float64, float64): bool
#[intrinsic("equal")]
(==)(bool, bool): bool

#[intrinsic("not_equal")]
(!=)(uint8, uint8): bool
#[intrinsic("not_equal")]
(!=)(int8, int8): bool
#[intrinsic("not_equal")]
(!=)(uint16, uint16): bool
#[intrinsic("not_equal")]
(!=)(int16, int16): bool
#[intrinsic("not_equal")]
(!=)(uint32, uint32): bool
#[intrinsic("not_equal")]
(!=)(int32, int32): bool
#[intrinsic("not_equal")]
(!=)(uint64, uint64): bool
#[intrinsic("not_equal")]
(!=)(int64, int64): bool
#[intrinsic("not_equal")]
(!=)(float32, float32): bool
#[intrinsic("not_equal")]
(!=)(float64, float64): bool
#[intrinsic("not_equal")]
(!=)(bool, bool): bool

#[intrinsic("negate")]
(-)(int8): int8
#[intrinsic("negate")]
(-)(int16): int16
#[intrinsic("negate")]
(-)(int32): int32
#[intrinsic("negate")]
(-)(int64): int64
#[intrinsic("negate")]
(-)(float32): float32
#[intrinsic("negate")]
(-)(float64): float64

#[intrinsic("or")]
(||)(bool, bool): bool

#[intrinsic("and")]
(&&)(bool, bool): bool

#[intrinsic("not")]
(!)(bool): bool

#[intrinsic("print")]
print(object): ()

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T
#[intrinsic("get_element")]
(`[,]`)<T>(T[,], index1: int32, index2: int32): T

#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()
#[intrinsic("get_element")]
(`[,]`)<T>(mutable T[,], index1: int32, index2: int32): T
#[intrinsic("set_element")]
(`[,]`)<T>(mutable T[,], index1: int32, index2: int32, T): ()

#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

#[intrinsic("bitwise_and")]
(&)(uint8, uint8): uint8
#[intrinsic("bitwise_and")]
(&)(uint16, uint16): uint16
#[intrinsic("bitwise_and")]
(&)(int32, int32): int32
#[intrinsic("bitwise_and")]
(&)(uint32, uint32): uint32
#[intrinsic("bitwise_and")]
(&)(uint64, uint64): uint64

#[intrinsic("bitwise_or")]
(|)(uint8, uint8): uint8
#[intrinsic("bitwise_or")]
(|)(uint32, uint32): uint32
#[intrinsic("bitwise_or")]
(|)(int32, int32): int32
#[intrinsic("bitwise_or")]
(|)(uint64, uint64): uint64

#[intrinsic("bitwise_not")]
(~)(int32): int32
#[intrinsic("bitwise_not")]
(~)(uint32): uint32
#[intrinsic("bitwise_not")]
(~)(uint64): uint64

#[intrinsic("bitwise_shift_left")]
(<<)(byte, int32): byte
#[intrinsic("bitwise_shift_left")]
(<<)(int32, int32): int32
#[intrinsic("bitwise_shift_left")]
(<<)(uint32, int32): uint32
#[intrinsic("bitwise_shift_left")]
(<<)(uint64, int32): uint64

#[intrinsic("bitwise_shift_right")]
(>>)(uint32, int32): uint32
#[intrinsic("bitwise_shift_right")]
(>>)(int32, int32): int32
#[intrinsic("bitwise_shift_right")]
(>>)(uint64, int32): uint64

#[intrinsic("get_tuple_element")]
GetTupleElement<N, T...>(__oly_tuple<T...>): T...[N] where N: constant int32

// Array specific operations

#[intrinsic("get_length")]
private getLength<T>(T[]): int32

#[intrinsic("get_length")]
private getLength<T>(mutable T[]): int32

#[open]
extension MutableArrayExtensions<T> =
    inherits mutable T[]

    Length: int32 
        #[inline]
        get() = getLength(this)

#[open]
extension ArrayExtensions<T> =
    inherits T[]

    Length: int32
        #[inline]
        get() = getLength(this)

#[open]
extension ArrayExtensions =
    inherits System.Array

    #[intrinsic("new_array")]
    static ZeroCreate<T>(size: int32): mutable T[]

    static Initialize<T>(size: int32, f: int32 -> T): mutable T[] =
        let newArr = System.Array.ZeroCreate<T>(size)
        let mutable i = 0
        while (i < newArr.Length)
            newArr[i] <- f(i)
            i <- i + 1
        newArr

    static Map<T, U>(arr: mutable T[], f: T -> U): mutable U[] =
        let newArr = System.Array.ZeroCreate<U>(arr.Length)
        let mutable i = 0
        while (i < arr.Length)
            newArr[i] <- f(arr[i])
            i <- i + 1
        newArr

    static Map<T, U>(arr: T[], f: T -> U): U[] =
        let newArr = System.Array.ZeroCreate<U>(arr.Length)
        let mutable i = 0
        while (i < arr.Length)
            newArr[i] <- f(arr[i])
            i <- i + 1
        UnsafeCast(newArr)

    static MapAsMutable<T, U>(arr: T[], f: T -> U): mutable U[] =
        let newArr = System.Array.ZeroCreate<U>(arr.Length)
        let mutable i = 0
        while (i < arr.Length)
            newArr[i] <- f(arr[i])
            i <- i + 1
        newArr

    static DistinctBy<T, U>(arr: T[], f: T -> U): T[] =
        let found = System.Collections.Generic.HashSet<U>()
        let indicesToInclude = System.Collections.Generic.List<int32>()
        let mutable i = 0
        while (i < arr.Length)
            let item = arr[i]
            if (found.Add(f(item)))
                indicesToInclude.Add(i)
            i <- i + 1
        
        let newArr = System.Array.ZeroCreate<T>(indicesToInclude.Count)
        let mutable i = 0
        while (i < newArr.Length)
            newArr[i] <- arr[indicesToInclude[i]]
            i <- i + 1
        UnsafeCast(newArr)

    static MaxByUInt32<T>(arr: T[], f: T -> uint32): T =
        let mutable maxValue = 0: uint32
        let mutable found = -1
        let mutable i = 0
        while (i < arr.Length)
            let item = arr[i]
            let value = f(item)
            if (value > maxValue)
                maxValue <- value
                found <- i
            i <- i + 1

        if (found == -1)
            fail("Empty array.")
        arr[found]

    static Filter<T>(arr: T[], f: T -> bool): T[] =
        let indicesToInclude = System.Collections.Generic.List<int32>()
        let mutable i = 0
        while (i < arr.Length)
            let item = arr[i]
            if (f(item))
                indicesToInclude.Add(i)
            i <- i + 1
        
        let newArr = System.Array.ZeroCreate<T>(indicesToInclude.Count)
        let mutable i = 0
        while (i < newArr.Length)
            newArr[i] <- arr[indicesToInclude[i]]
            i <- i + 1
        UnsafeCast(newArr)

    static Find<T>(arr: T[], predicate: T -> bool): T =
        let mutable result: T = unchecked default
        let mutable isFound = false
        let mutable i = 0
        while (i < arr.Length && !isFound)
            let item = arr[i]
            if (predicate(item))
                result <- item
                isFound <- true
            else
                i <- i + 1
        if (isFound)
            result
        else
            fail("Unable to find item.")

    static FindIndex<T>(arr: T[], predicate: T -> bool): int32 =
        let mutable isFound = false
        let mutable i = 0
        while (i < arr.Length && !isFound)
            let item = arr[i]
            if (predicate(item))
                isFound <- true
            else
                i <- i + 1
        if (isFound)
            i
        else
            fail("Unable to find item index.")

    static FindIndex<T>(arr: T[], predicate: (index: int32, T) -> bool): int32 =
        let mutable isFound = false
        let mutable i = 0
        while (i < arr.Length && !isFound)
            let item = arr[i]
            if (predicate(i, item))
                isFound <- true
            else
                i <- i + 1
        if (isFound)
            i
        else
            fail("Unable to find item index.")

    static Flatten<T, U>(arr: mutable T[], f: T -> (U, U, U)): mutable U[] =
        let newArr = System.Array.ZeroCreate<U>(arr.Length * 3)
        let mutable i = 0
        let mutable j = 0
        while (i < arr.Length)
            let tup = f(arr[i])
            newArr[j] <- GetTupleElement<0, _>(tup)
            newArr[j + 1] <- GetTupleElement<1, _>(tup)
            newArr[j + 2] <- GetTupleElement<2, _>(tup)
            i <- i + 1
            j <- j + 3
        newArr

// DotNet Specific

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = 
    x.get_Item(key)
#[inline]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = 
    x.get_Item(key)
#[inline]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = 
    x.get_Item(key)
#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = 
    x.set_Item(key, value)
#[inline]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = 
    x.set_Item(key, value)

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Addition(T1, T2): T3 } = T1.op_Addition(x, y)
(-)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Subtraction(T1, T2): T3 } = T1.op_Subtraction(x, y)
(*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)
(/)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Division(T1, T2): T3 } = T1.op_Division(x, y)
(%)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Remainder(T1, T2): T3 } = T1.op_Remainder(x, y)
(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)
(!=)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Inequality(T1, T2): T3 } = T1.op_Inequality(x, y)
(-)<T1, T2>(x: T1): T2 where T1: { static op_UnaryNegation(T1): T2 } = T1.op_UnaryNegation(x)

#[intrinsic("constant")]
#[import("intrinsic-CLR", "", "typeof")]
typeof<require T>: System.Type

// TODO: make this an actual intrinsic like 'typeof'.
sizeof<require T>: int32 =
    System.Runtime.InteropServices.Marshal.SizeOf(unchecked default: T)

printLine<T>(value: T): () where T: { ToString(): string } =
    print(value.ToString() + "\n")

printLine(value: uint32): () =
    print(value)
    print("\n")

printLine(value: int32): () =
    print(value)
    print("\n")

(+)(str1: string, str2: string): string = System.String.Concat(str1, str2)
(==)(str1: string, str2: string): bool = System.String.Equals(str1, str2)
(!=)(str1: string, str2: string): bool = !System.String.Equals(str1, str2)

#[intrinsic("throw")]
(throw)<TResult>(System.Exception): TResult

fail<TResult>(msg: string): TResult =
    throw System.Exception(msg)

main(): () =
    print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Generic class that handles functions that have ambiguity - Exported``() =
    let src =
        """
namespace TestNamespace

open System

#[export]
abstract class A<T> =

    abstract default Test(x: T): () =
        Program.print("Test_T_")

    abstract default Test(x: Int32): () =
        Program.print("Test_int32_")

#[export]
class Test =
    inherits A<String>

#[export]
class Test2 =
    inherits A<Int32>

    overrides Test(x: Int32): () =
        Program.print(x)

module Program =
    #[intrinsic("print")]
    print(__oly_object): ()

    main(): () =
        let t = Test()
        let t = t: A<String>
        t.Test("test")
        t.Test(456)

        let t = Test2()
        let t = t: A<Int32>
        t.Test(123)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test_T_Test_int32_123"

[<Fact>]
let ``Should get correct GetEnumerator``() =
    let src =
        """
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

ForEach<T>(xs: System.Collections.Generic.IEnumerable<T>, f: T -> ()): () =
    let xse = xs.GetEnumerator()
    if (xse.MoveNext())
        f(xse.Current)

main(): () =
    let xs = List<int32>()
    xs.Add(123)

    ForEach(xs, x -> print(x))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Should get correct IEnumerable extension for array``() =
    let src =
        """
open System.Collections
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[open]
extension ArrayEnumerableExtension<T> =
    inherits T[]
    implements IEnumerable

    GetEnumerator(): IEnumerator =
        print("passed")
        unchecked default

test<T>(xs: T): () where T: trait IEnumerable =
    let x = xs.GetEnumerator()

main(): () =
    let xs = [1]
    test(xs)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Should get correct IEnumerable extension for array 2``() =
    let src =
        """
open System.Collections
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[open]
extension ArrayEnumerableExtension<T> =
    inherits T[]
    implements IEnumerable<T>

    GetEnumerator(): IEnumerator =
        print("failed")
        unchecked default

    GetEnumerator(): IEnumerator<T> =
        print("passed")
        unchecked default

test<T>(xs: T): () where T: trait IEnumerable<int32> =
    let x = xs.GetEnumerator()

main(): () =
    let xs = [1]
    test(xs)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Should get correct IEnumerable extension for array 3``() =
    let src =
        """
open System.Collections
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[open]
extension ArrayEnumerableExtension<T> =
    inherits T[]
    implements IEnumerable<T>

    private GetEnumerator(): IEnumerator =
        print("failed")
        unchecked default

    GetEnumerator(): IEnumerator<T> =
        print("passed")
        unchecked default

main(): () =
    let xs = [1]
    let x = xs.GetEnumerator()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Should get correct IEnumerable extension for array 4``() =
    let src =
        """
open System.Collections
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[open]
extension ArrayEnumerableExtension<T> =
    inherits T[]
    implements IEnumerable<T>

    private GetEnumerator(): IEnumerator =
        print("failed")
        unchecked default

    GetEnumerator(): IEnumerator<T> =
        print("passed")
        unchecked default

#[inline(always)]
ForEach<T<_>, U>(xs: T<U>, #[inline(always)] f: U -> ()): () where T<_>: trait System.Collections.Generic.IEnumerable =
    let xse = xs.GetEnumerator()

test<T>(xs: T): () where T: trait IEnumerable<int32> =
    ForEach(xs, x -> print(x))

main(): () =
    let xs = [1]
    test(xs)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Should get correct IEnumerable extension for array 5``() =
    let src =
        """
open System.Collections
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[open]
extension ArrayEnumerableExtension<T> =
    inherits T[]
    implements IEnumerable<T>

    private GetEnumerator(): IEnumerator =
        print("failed")
        (this.GetEnumerator(): IEnumerator<T>)

    GetEnumerator(): IEnumerator<T> =
        print("passed")
        unchecked default

#[inline(always)]
ForEach<T<_>, U>(xs: T<U>, #[inline(always)] f: U -> ()): () where T<_>: trait System.Collections.Generic.IEnumerable =
    let xse = xs.GetEnumerator()

Do(f: () -> ()): () =
    f()

test<T>(xs: T): () where T: trait IEnumerable<int32> =
    Do(() ->
        ForEach(xs, x -> print(x))
    )

main(): () =
    let xs = [1]
    test(xs)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Should get correct IEnumerable extension for array 6``() =
    let src =
        """
open System.Collections
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[open]
extension ArrayEnumerableExtension<T> =
    inherits T[]
    implements IEnumerable<T>

    private GetEnumerator(): IEnumerator =
        print("failed")
        (this.GetEnumerator(): IEnumerator<T>)

    GetEnumerator(): IEnumerator<T> =
        print("passed")
        unchecked default

#[inline(always)]
ForEach<T<_>, U>(xs: T<U>, #[inline(always)] f: U -> ()): () where T<_>: trait System.Collections.Generic.IEnumerable =
    let xse = xs.GetEnumerator()

Do(f: () -> ()): () =
    f()

test<T>(xs: T): () where T: trait IEnumerable<int32> =
    Do(() ->
        ForEach(xs, x -> print(x))
    )

main(): () =
    test([])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Get ReadOnlySpan index``() =
    let src =
        """
open System

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

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = 
    x.get_Item(key)

main(): () =
    let xs = mutable [1;2;3]
    let mutable rospan = ReadOnlySpan(xs)
    let x = rospan[1]
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"

[<Fact>]
let ``Implement IEquatable``() =
    let src =
        """
open System

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

struct Test =
   implements IEquatable<Test>

   mutable Equals(x: Test): bool =
      true

   static op_Equality(x: Test, y: Test): bool =
      print("passed")
      true

main(): () =
   let t = Test()
   print(t == t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passedTrue"

[<Fact>]
let ``Implement IEquatable with extension``() =
    let src =
        """
open System

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

#[open]
extension IEquatableExtensions<T> =
   inherits IEquatable<T>

   static op_Equality(x: T, y: T): bool =
      print("passed")
      true

struct Test =
   implements IEquatable<Test>

   mutable Equals(x: Test): bool =
      true

main(): () =
   let t = Test()
   print(t == t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passedTrue"

[<Fact>]
let ``Implement IEquatable with extension 2``() =
    let src =
        """
open System

#[intrinsic("bool")]
alias bool

#[intrinsic("print")]
print(__oly_object): ()

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)

#[open]
extension IEquatableExtensions<T> =
   inherits IEquatable<T>

   static op_Equality(x: T, y: T): bool =
      print("failed")
      true

struct Test =
   implements IEquatable<Test>

   mutable Equals(x: Test): bool =
      true

   static op_Equality(x: Test, y: Test): bool =
      print("passed")
      true

main(): () =
   let t = Test()
   print(t == t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passedTrue"

[<Fact>]
let ``Always choose most specific implementation for extension``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

class C =
   implements IDisposable

   Dispose(): () = print("passed")

#[open]
extension CExtension =
   inherits C
   implements IDisposable

   Dispose(): () = print("failed")

test<T>(x: T): () where T: IDisposable = x.Dispose()

main(): () =
   let c = C()
   c.Dispose()
   test(c)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passedpassed"

[<Fact>]
let ``Always choose most specific implementation for extension 2``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

abstract default class C

class Z =
   inherits C

#[open]
extension CExtension =
   inherits C
   implements IDisposable

   Dispose(): () = print("failed")

#[open]
extension ZExtension =
   inherits Z
   implements IDisposable

   Dispose(): () = print("passed")

test<T>(x: T): () where T: trait IDisposable = x.Dispose()

main(): () =
   let c = Z()
   c.Dispose()
   test(c)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passedpassed"

[<Fact>]
let ``Always choose most specific implementation for extension 3``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

abstract default class C

class Z =
   inherits C

#[open]
extension CExtension =
   inherits C
   implements IDisposable

   Dispose(): () = print("passed")

test<T>(x: T): () where T: trait IDisposable = x.Dispose()

main(): () =
   let c = Z()
   c.Dispose()
   test(c)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passedpassed"

[<Fact>]
let ``Override finalizer``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

class C =

    protected overrides Finalize(): () =
        print("finalize")

#[inline(never)]
test(): () =
    let _ = C()

main(): () =
    test()
    GC.Collect(2)
    GC.WaitForPendingFinalizers()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "finalize"

[<Fact>]
let ``Field array of byte pointers should work``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("uint8")]
alias byte

#[intrinsic("native_ptr")]
alias (*)<T>

field XS: (byte*)[] = []

main(): () =
    print(XS)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "System.UIntPtr[]"

[<Fact>]
let ``Lambda uses a pointer type should work``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("uint8")]
alias byte

#[intrinsic("native_ptr")]
alias (*)<T>

#[inline(never)]
f(g: byte* -> byte*): () = 
    let _ = g(default)

main(): () =
    f(x -> x)
    print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Span used as a parameter for a lambda``() =
    let src =
        """
open System

#[intrinsic("uint8")]
alias byte

#[intrinsic("print")]
print(__oly_object): ()

#[inline(never)]
test(f: Span<byte> -> ()): () =
    let s = Span(mutable []: mutable byte[])
    f(s)

main(): () =
    test(s -> ())
    print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``ReadOnlySpan used as a parameter for a lambda``() =
    let src =
        """
open System

#[intrinsic("uint8")]
alias byte

#[intrinsic("print")]
print(__oly_object): ()

#[inline(never)]
test(f: ReadOnlySpan<byte> -> ()): () =
    let s = ReadOnlySpan(mutable []: mutable byte[])
    f(s)

main(): () =
    test(s -> ())
    print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Span extension should not have an ambiguous overload``() =
    let src =
        """
open System
open System.Runtime.InteropServices

#[intrinsic("uint8")]
alias byte

#[intrinsic("print")]
print(__oly_object): ()

struct S =
    field value: byte = 1

#[open]
extension MutableArrayExtensions<T> =
    inherits mutable T[]

    AsSpan(): Span<T> = Span(this)

#[open]
extension MutableArrayCastExtensions<T> where T: struct, ValueType, { new() } =
    inherits mutable T[]

    AsSpan<TCast>(): Span<TCast> where TCast: struct, ValueType, { new() } = MemoryMarshal.Cast(Span(this))

main(): () =
    let xs = mutable [S()]
    let span = xs.AsSpan()
    print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Span extension should not have an ambiguous overload 2``() =
    let src =
        """
open System
open System.Runtime.InteropServices

#[intrinsic("uint8")]
alias byte

#[intrinsic("print")]
print(__oly_object): ()

#[open]
extension MutableArrayExtensions<T> =
    inherits mutable T[]

    AsSpan(): Span<T> = Span(this)

#[open]
extension MutableArrayCastExtensions<T> where T: struct, ValueType, { new() } =
    inherits mutable T[]

    AsSpan<TCast>(): Span<TCast> where TCast: struct, ValueType, { new() } = MemoryMarshal.Cast(Span(this))

test(xs: Span<byte>): () = ()
test<T>(xs: T): () = ()

main(): () =
    let xs = mutable [1: byte]
    test(xs.AsSpan())
    print("passed")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Passing witnesses to ctor type arguments by only by function type arguments - with exported interface``() =
    let src =
        """
namespace Test

#[intrinsic("int32")]
alias int32

interface IComponent =

    static abstract GetValue(): int32

#[export]
interface IArchetypeReference =

    ArchetypedIndex: int32 get

class ArchetypeReference<T0> where T0: unmanaged, trait IComponent =
    implements IArchetypeReference

    ArchetypedIndex: int32 get

    new() =
        this {
            ArchetypedIndex = T0.GetValue()
        }

struct S1

struct S2

#[open]
extension S1Component =
    inherits S1
    implements IComponent

    static overrides GetValue(): int32 = 11

#[open]
extension S2Component =
    inherits S2
    implements IComponent

    static overrides GetValue(): int32 = 22

module TestModule =
    #[intrinsic("print")]
    print(__oly_object): ()

    GetIndex<T>(): int32 where T: unmanaged, trait IComponent =
        // 'T' might have a witness and it needs to be passed to type-ctor 'ArchetypeReference'.
        let r = ArchetypeReference<T>()
        r.ArchetypedIndex

    main(): () =
        let value1 = GetIndex<S1>()
        let value2 = GetIndex<S2>()
        print(value1)
        print(value2)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1122"

[<Fact>]
let ``Transform should work``() =
    let src =
        """
open System
open System.Numerics

#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

(+)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_Addition(T1, T2): T3 } = T1.op_Addition(x, y)
(-)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_Subtraction(T1, T2): T3 } = T1.op_Subtraction(x, y)
(*)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_Multiply(T1, T2): T3 } = T1.op_Multiply(x, y)
(/)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_Division(T1, T2): T3 } = T1.op_Division(x, y)
(%)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_Remainder(T1, T2): T3 } = T1.op_Remainder(x, y)
(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)
(!=)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_Inequality(T1, T2): T3 } = T1.op_Inequality(x, y)
(-)<T1, T2>(x: T1): T2 where T1: trait { static op_UnaryNegation(T1): T2 } = T1.op_UnaryNegation(x)
(|)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_BitwiseOr(T1, T2): T3 } = T1.op_BitwiseOr(x, y)
(&)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_BitwiseAnd(T1, T2): T3 } = T1.op_BitwiseAnd(x, y)

#[open]
extension Vector3Extensions =
    inherits Vector3

    static Forward: Vector3 get() = -Vector3.UnitZ
    static Back: Vector3 get() = Vector3.UnitZ
    static Left: Vector3 get() = -Vector3.UnitX
    static Right: Vector3 get() = Vector3.UnitX

struct Transform =
    public field mutable Matrix: Matrix4x4

    new(matrix: Matrix4x4) = this { Matrix = matrix }

    Position: Vector3
        get() = this.Matrix.Translation
        set(value) = this.Matrix.Translation <- value

    Rotation: Quaternion
        get() = Quaternion.CreateFromRotationMatrix(this.Matrix)

    Scale: Vector3
        get() = 
            let mutable scale = Vector3.Zero
            let mutable rotation = Quaternion.Identity
            let mutable position = Vector3.Zero
            if (Matrix4x4.Decompose(this.Matrix, &scale, &rotation, &position))
                scale
            else
                Vector3.Zero

    Forward: Vector3
        get() = Vector3.Transform(Vector3.Forward, this.Rotation)

    Back: Vector3
        get() = Vector3.Transform(Vector3.Back, this.Rotation)

    Left: Vector3
        get() = Vector3.Transform(Vector3.Left, this.Rotation)

    Right: Vector3
        get() = Vector3.Transform(Vector3.Right, this.Rotation)

    WorldToLocalMatrix: Matrix4x4
        get() =
            let mutable inverted = Matrix4x4.Identity
            let didSucceed = Matrix4x4.Invert(this.Matrix, &inverted)
            inverted

    static Create(position: Vector3, rotation: Quaternion, scale: Vector3): Transform =
        let rotationMatrix = Matrix4x4.CreateFromQuaternion(rotation)
        let mutable scaleMatrix = Matrix4x4.CreateScale(scale)
        scaleMatrix.Translation <- position
        Transform(rotationMatrix * scaleMatrix)

struct Camera =
    public field mutable Transform: Transform = default
    public field mutable Projection: Matrix4x4 = default

    field mutable yaw: float32 = default
    field mutable pitch: float32 = default

interface IComponent =

    static abstract GetSize(): int32

#[open]
extension TransformComponent =
    inherits Transform
    implements IComponent

    static overrides GetSize(): int32 = sizeof<Transform>

#[open]
extension CameraComponent =
    inherits Camera
    implements IComponent

    static overrides GetSize(): int32 = sizeof<Camera>

sizeof<require T>: int32 =
    System.Runtime.InteropServices.Marshal.SizeOf(unchecked default: T)

GetComponentSize<T>(): int32 where T: unmanaged, trait IComponent =
    T.GetSize()

class ComponentRegistry =
    Register<T>(): () where T: unmanaged, trait IComponent = 
        print(T.GetSize())

newtype TransformLerp =
    public field Value: Transform

class Database =

    field registry: ComponentRegistry = ComponentRegistry()

    Register<T>(): () where T: unmanaged, trait IComponent =
        this.registry.Register<T>()

main(): () =
    let db = Database()
    let x = sizeof<TransformLerp>
    let x = Transform.Create(default, default, default)
    let a = x.Position
    let b = x.Rotation
    let c = x.Scale
    let l = x.WorldToLocalMatrix
    let g = Camera()
    let z = sizeof<Transform>
    let y = sizeof<Camera>
    print(z)
    print(y)
    print(GetComponentSize<Camera>())
    print(GetComponentSize<Transform>())
    db.Register<Transform>()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "641361366464"

[<Fact>]
let ``Basic Observable``() =
    let src =
        """
open System
open System.Numerics

#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

shape DotNetIndexGetter<TKey, TValue> =

    get_Item(TKey): TValue

(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: DotNetIndexGetter<TKey, TValue> = x.get_Item(key)

shape DotNetIndexSetter<TKey, TValue> =

    set_Item(TKey, TValue): ()

(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: DotNetIndexSetter<TKey, TValue> = x.set_Item(key, value)

#[intrinsic("equal")]
(==)(int32, int32) : bool

#[inline(always)]
ForEach<T>(xs: System.Collections.Generic.IEnumerable<T>, #[inline(always)] f: T -> ()): () =
    let xse = xs.GetEnumerator()
    while (xse.MoveNext())
        f(xse.Current)

class Subscription<T> =
    implements IDisposable

    private Unsubscribe: () -> () get

    new(unsubscribe: () -> ()) =
        this {
            Unsubscribe = unsubscribe
        }

    Dispose(): () = this.Unsubscribe()

class Observable<T> =

    field subscribers: System.Collections.Concurrent.ConcurrentDictionary<T -> (), ()>
    field mutable value: T

    Subscribe(callback: T -> ()): Subscription<T> =
        let _ = this.subscribers[callback] <- ()
        Subscription(
            () -> 
                let mutable value = unchecked default
                let _ = this.subscribers.TryRemove(callback, &value)
        )

    Value: T
        get() = this.value
        set(value) =
            this.value <- value
            ForEach(this.subscribers, (mutable pair) -> pair.Key(value))

    new(value: T) = this { value = value; subscribers = System.Collections.Concurrent.ConcurrentDictionary() }

main(): () =
    let var = Observable<int32>(0)
    let subscription = var.Subscribe(x -> if (x == 123) print("passed") else print("failed"))
    var.Value <- 123
    subscription.Dispose()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "passed"

[<Fact>]
let ``Span get_Item simple test``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let mutable span = Span(mutable [567])
    let value = span.get_Item(0)
    print(value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "567"

[<Fact>]
let ``Observer example``() =
    let src =
        """
open System
open System.Collections.Concurrent

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("throw")]
(throw)<TResult>(Exception): TResult

#[inline(always)]
ForEach<T>(xs: System.Collections.Generic.IEnumerable<T>, #[inline(always)] f: T -> ()): () =
    let xse = xs.GetEnumerator()
    while (xse.MoveNext())
        f(xse.Current)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = 
    x.set_Item(key, value)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = 
    x.set_Item(key, value)

private class Subscription =
    implements IDisposable

    private Unsubscribe: () -> () get

    new(unsubscribe: () -> ()) =
        this {
            Unsubscribe = unsubscribe
        }

    Dispose(): () = this.Unsubscribe()

private class Observer<T> =
    implements IObserver<T>

    field callback: T -> ()

    new(callback: T -> ()) = this { callback = callback }

    OnCompleted(): () = ()

    OnError(error: Exception): () =
        throw error

    OnNext(value: T): () =
        this.callback(value)

class Observable<T> =
    implements IObservable<T>

    field subscribers: ConcurrentDictionary<IObserver<T>, ()>
    field mutable value: T

    Subscribe(callback: T -> ()): IDisposable =
        this.Subscribe(Observer(callback))

    Subscribe(observer: IObserver<T>): IDisposable =
        this.subscribers[observer] <- ()
        Subscription(
            () -> 
                let mutable value = unchecked default
                let _ = this.subscribers.TryRemove(observer, &value)
        )

    Value: T
        get() = this.value
        set(value) =
            this.value <- value
            ForEach(this.subscribers, (mutable pair) -> pair.Key.OnNext(value))

    new(value: T) = this { value = value; subscribers = ConcurrentDictionary() }

main(): () =
    let o = Observable(123)
    o.Value <- 456
    print(o.Value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"

[<Fact>]
let ``Observer example 2``() =
    let src =
        """
open System
open System.Collections.Concurrent

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("throw")]
(throw)<TResult>(Exception): TResult

#[inline(always)]
ForEach<T>(xs: System.Collections.Generic.IEnumerable<T>, #[inline(always)] f: T -> ()): () =
    let xse = xs.GetEnumerator()
    while (xse.MoveNext())
        f(xse.Current)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = 
    x.set_Item(key, value)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = 
    x.set_Item(key, value)

private class Subscription =
    implements IDisposable

    private Unsubscribe: () -> () get

    new(unsubscribe: () -> ()) =
        this {
            Unsubscribe = unsubscribe
        }

    Dispose(): () = this.Unsubscribe()

private class Observer<T> =
    implements IObserver<T>

    field callback: T -> ()

    new(callback: T -> ()) = this { callback = callback }

    OnCompleted(): () = ()

    OnError(error: Exception): () =
        throw error

    OnNext(value: T): () =
        this.callback(value)

class Observable =
    implements IObservable<int32>

    field subscribers: ConcurrentDictionary<IObserver<int32>, ()>
    field mutable value: int32

    Subscribe(callback: int32 -> ()): IDisposable =
        this.Subscribe(Observer(callback))

    Subscribe(observer: IObserver<int32>): IDisposable =
        this.subscribers[observer] <- ()
        Subscription(
            () -> 
                let mutable value = unchecked default
                let _ = this.subscribers.TryRemove(observer, &value)
        )

    Value: int32
        get() = this.value
        set(value) =
            this.value <- value
            ForEach(this.subscribers, (mutable pair) -> pair.Key.OnNext(value))

    new(value: int32) = this { value = value; subscribers = ConcurrentDictionary() }

main(): () =
    let o = Observable(123)
    o.Value <- 456
    print(o.Value)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"

[<Fact>]
let ``Lock example``() =
    let src =
        """
open System
open System.Collections.Concurrent

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

lock<T>(lockObj: Object, f: () -> T): T =
    let mutable lockTaken = false
    try
        System.Threading.Monitor.Enter(lockObj, &lockTaken)
        f()
    finally
        if (lockTaken)
            System.Threading.Monitor.Exit(lockObj)

main(): () =
    let o = Object()
    lock(o,
        () -> print("hello")
    )
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Custom delegate``() =
    let src =
        """
namespace N

#[intrinsic("base_object")]
alias object

#[intrinsic("utf16")]
alias string

#[intrinsic("uint32")]
alias uint32

#[intrinsic("native_int")]
alias nint

#[export]
class Callback =

    Invoke(bodyId1: uint32, bodyId2: uint32): () =
        M.print(bodyId1)
        M.print(bodyId2)

module Unsafe =

    #[intrinsic("unsafe_cast")]
    Cast<T>(object): T

module M =

    #[intrinsic("print")]
    print(object): ()

    #[intrinsic("load_function_ptr")]
    (&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

    #[intrinsic("load_function_ptr")]
    (&&)<TFunctionPtr, TParameters...>(TParameters... -> ()): TFunctionPtr

    #[intrinsic("constant")]
    #[import("intrinsic-CLR", "", "typeof")]
    typeof<require T>: System.Type

    #[import("intrinsic-CLR", "", "CreateDelegate")]
    CreateDelegate<TReturn, TParameters...>(object, static TParameters... -> TReturn): System.Delegate

    #[import("intrinsic-CLR", "", "CreateDelegate")]
    CreateDelegate<TParameters...>(object, static TParameters... -> ()): System.Delegate

    main(): () =
        let callback = Callback()
        let del = CreateDelegate(callback, &&callback.Invoke)
        let x = 5: uint32
        let y = 8: uint32
        let _ = del.DynamicInvoke(mutable [x: object; y: object])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "58"

[<Fact>]
let ``Custom delegate should error due to physical unit type``() =
    let src =
        """
namespace N

#[intrinsic("base_object")]
alias object

#[intrinsic("utf16")]
alias string

#[intrinsic("uint32")]
alias uint32

#[intrinsic("native_int")]
alias nint

#[export]
class Callback =

    Invoke(bodyId1: uint32, bodyId2: uint32): () = ()

module Unsafe =

    #[intrinsic("unsafe_cast")]
    Cast<T>(object): T

module M =

    #[intrinsic("print")]
    print(object): ()

    #[intrinsic("load_function_ptr")]
    (&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

    #[intrinsic("constant")]
    #[import("intrinsic-CLR", "", "typeof")]
    typeof<require T>: System.Type

    #[import("intrinsic-CLR", "", "CreateDelegate")]
    CreateDelegate<TReturn, TParameters...>(object, static TParameters... -> TReturn): System.Delegate

    main(): () =
        let callback = Callback()
        let del = CreateDelegate(callback, &&callback.Invoke)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Invalid use of 'LoadFunctionPtr'.",
                """
        let del = CreateDelegate(callback, &&callback.Invoke)
                                           ^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Custom delegate should error due to physical unit type 2``() =
    let src =
        """
namespace N

#[intrinsic("base_object")]
alias object

#[intrinsic("utf16")]
alias string

#[intrinsic("uint32")]
alias uint32

#[intrinsic("native_int")]
alias nint

#[export]
class Callback =

    Invoke(bodyId1: uint32, bodyId2: uint32): () = ()

module Unsafe =

    #[intrinsic("unsafe_cast")]
    Cast<T>(object): T

module M =

    #[intrinsic("print")]
    print(object): ()

    #[intrinsic("load_function_ptr")]
    (&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

    #[intrinsic("constant")]
    #[import("intrinsic-CLR", "", "typeof")]
    typeof<require T>: System.Type

    #[import("intrinsic-CLR", "", "CreateDelegate")]
    CreateDelegate<TReturn, TParameters...>(object, static TParameters... -> TReturn): System.Delegate

    main(): () =
        let callback = Callback()
        let ptr = &&callback.Invoke
        let del = CreateDelegate(callback, ptr)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Invalid use of 'LoadFunctionPtr'.",
                """
        let ptr = &&callback.Invoke
                  ^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Custom delegate should not error due to physical unit type``() =
    let src =
        """
namespace N

#[intrinsic("base_object")]
alias object

#[intrinsic("utf16")]
alias string

#[intrinsic("uint32")]
alias uint32

#[intrinsic("native_int")]
alias nint

#[export]
class Callback =

    Invoke(bodyId1: uint32, bodyId2: uint32): () = 
        M.print("hello")
        M.print(bodyId1)
        M.print(bodyId2)

module Unsafe =

    #[intrinsic("unsafe_cast")]
    Cast<T>(object): T

module M =

    #[intrinsic("print")]
    print(object): ()

    #[intrinsic("load_function_ptr")]
    (&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

    #[intrinsic("load_function_ptr")]
    (&&)<TFunctionPtr, TParameters...>(TParameters... -> ()): TFunctionPtr

    #[intrinsic("constant")]
    #[import("intrinsic-CLR", "", "typeof")]
    typeof<require T>: System.Type

    #[import("intrinsic-CLR", "", "CreateDelegate")]
    CreateDelegate<TReturn, TParameters...>(object, static TParameters... -> TReturn): System.Delegate

    #[import("intrinsic-CLR", "", "CreateDelegate")]
    CreateDelegate<TParameters...>(object, static TParameters... -> ()): System.Delegate

    main(): () =
        let callback = Callback()
        let del = CreateDelegate(callback, &&callback.Invoke)
        let _ = del.DynamicInvoke(mutable [4: uint32; 2: uint32])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello42"

[<Fact>]
let ``Custom delegate with return type``() =
    let src =
        """
namespace N

#[intrinsic("base_object")]
alias object

#[intrinsic("utf16")]
alias string

#[intrinsic("uint32")]
alias uint32

#[intrinsic("native_int")]
alias nint

#[export]
class Callback =

    Invoke(bodyId1: uint32, bodyId2: uint32): uint32 =
        M.print(bodyId1)
        M.print(bodyId2)
        789

module Unsafe =

    #[intrinsic("unsafe_cast")]
    Cast<T>(object): T

module M =

    #[intrinsic("print")]
    print(object): ()

    #[intrinsic("load_function_ptr")]
    (&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

    #[intrinsic("constant")]
    #[import("intrinsic-CLR", "", "typeof")]
    typeof<require T>: System.Type

    #[import("intrinsic-CLR", "", "CreateDelegate")]
    CreateDelegate<TReturn, TParameters...>(object, static TParameters... -> TReturn): System.Delegate

    #[import("intrinsic-CLR", "", "CreateDelegate")]
    CreateDelegate<TParameters...>(object, static TParameters... -> ()): System.Delegate

    main(): () =
        let callback = Callback()
        let del = CreateDelegate(callback, &&callback.Invoke)
        let x = 5: uint32
        let y = 8: uint32
        let result = del.DynamicInvoke(mutable [x: object; y: object])
        print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "58789"

[<Fact>]
let ``Should choose correct overload``() =
    let src =
        """
open System

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(object): ()

class C =

    Write<T>(mutable value: T): () where T: unmanaged, ValueType =
        print("T")
        this.Write(&value)
    Write<T>(value: byref<T>): () where T: unmanaged, ValueType =
        print("byref")

main(): () =
    let c = C()
    c.Write<int32>(0)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Tbyref"
    

[<Fact>]
let ``Should choose correct overload 2``() =
    let src =
        """
open System

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("print")]
print(object): ()

class C =

    Write<T>(value: T): () where T: unmanaged, ValueType =
        print("T")
        this.Write(&value)
    Write<T>(value: inref<T>): () where T: unmanaged, ValueType =
        print("inref")

main(): () =
    let c = C()
    c.Write<int32>(0)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Tinref"

[<Fact>]
let ``Should choose correct overload 3``() =
    let src =
        """
open System

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(object): ()

class C =

    Write<T>(mutable value: T): () where T: unmanaged, ValueType =
        print("T")
        this.Write(&value)
    Write<T>(value: byref<T>): () where T: unmanaged, ValueType =
        print("byref")

main(): () =
    let c = C()
    c.Write<int32>(0: int32)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Tbyref"

[<Fact>]
let ``Should choose correct overload 4``() =
    let src =
        """
open System

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(object): ()

class C =

    Write<T>(mutable value: T): () where T: unmanaged, ValueType =
        print("T")
        this.Write(&value)
    Write<T>(value: byref<T>): () where T: unmanaged, ValueType =
        print("byref")

main(): () =
    let c = C()
    c.Write(0: int32)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Tbyref"

[<Fact>]
let ``Should choose correct overload 5``() =
    let src =
        """
open System

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(object): ()

class C =

    Write<T>(mutable value: T): () where T: unmanaged, ValueType =
        print("T")
        this.Write(&value)
    Write<T>(value: byref<T>): () where T: unmanaged, ValueType =
        print("byref")

main(): () =
    let c = C()
    c.Write(0)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Tbyref"

[<Fact>]
let ``Exported type with a generic field type``() =
    let src =
        """
namespace NMSPC1

#[open]
module Prelude =

    #[intrinsic("int32")]
    alias int32

    #[intrinsic("print")]
    print(__oly_object): ()

#[export]
struct S<T> =

    public field X: int32 = 5

#[export]
module Program =

    field X: S<int32> = S()

    main(): () =
        print(X.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "5"

[<Fact>]
let ``Complex use of fields and structs and mutation with exported``() =
    let src =
        """
namespace NMSPC1

#[open]
module Prelude =
    #[intrinsic("int32")]
    alias int32

    #[intrinsic("by_ref")]
    alias byref<T>

    #[intrinsic("address_of")]
    (&)<T>(T): byref<T>

    #[intrinsic("print")]
    print(__oly_object): ()

    #[intrinsic("add")]
    (+)(int32, int32): int32

#[export]
interface IA =
    
    X: int32 get, set

    SideEffect(): ()

#[export]
struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

#[export]
struct S2<T> where T: IA =
    implements IA

    public field S: T = unchecked default

    X: int32
        get() = this.S.X
        set(value) = this.S.X <- value

    SideEffect(): () =
        this.S.SideEffect()

#[export]
struct S3<T> where T: IA =

    public field S: T = unchecked default

    GetX(): int32 = this.S.X

    SetX(x: int32): () = this.S.X <- x


#[export]
module Program =

    M<T>(x: S3<T>): int32 where T: IA =
        x.S.SideEffect()
        x.S.SideEffect()
        x.GetX()

    M2<T>(x: S3<T>): int32 where T: IA =
        x.SetX(1)
        x.S.SideEffect()
        x.S.SideEffect()
        x.GetX()

    main(): () =
        let mutable s = S3<S2<S>>()
        let result = M(s)
        print(result)
        print("_")
        print(s.S.X)
        let result = M2(s)
        print(result)
        print("_")
        print(s.S.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "8_09_0"

[<Fact>]
let ``Complex use of fields and structs and mutation with exported 2``() =
    let src =
        """
namespace NMSPC1

#[open]
module Prelude =
    #[intrinsic("int32")]
    alias int32

    #[intrinsic("by_ref")]
    alias byref<T>

    #[intrinsic("address_of")]
    (&)<T>(T): byref<T>

    #[intrinsic("print")]
    print(__oly_object): ()

    #[intrinsic("add")]
    (+)(int32, int32): int32

#[export]
interface IA =
    
    X: int32 get, set

    SideEffect(): ()

#[export]
struct S =
    implements IA

    X: int32 get, set = 1

    mutable SideEffect(): () =
        this.X <- this.X + 4

#[export]
struct S2<T> where T: IA =
    implements IA

    public field S: T = unchecked default

    X: int32
        get() = this.S.X
        set(value) = this.S.X <- value

    SideEffect(): () =
        this.S.SideEffect()

#[export]
struct S3<T> where T: IA =

    public field S: S2<T> = unchecked default

    GetX(): int32 = this.S.S.X

    SetX(x: int32): () = this.S.S.X <- x


#[export]
module Program =

    M<T>(x: S3<T>): int32 where T: IA =
        x.S.SideEffect()
        x.S.SideEffect()
        x.GetX()

    M2<T>(x: S3<T>): int32 where T: IA =
        x.SetX(1)
        x.S.SideEffect()
        x.S.SideEffect()
        x.GetX()

    main(): () =
        let mutable s = S3<S2<S>>()
        let result = M(s)
        print(result)
        print("_")
        print(s.S.X)
        let result = M2(s)
        print(result)
        print("_")
        print(s.S.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "8_09_0"

[<Fact>]
let ``Complex lambda``() =
    """
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class C =

    public field funcs: List<() -> ()> = List()

    #[inline]
    M2<T, U, Z>(#[inline(never)] f: T -> ()): () =
        f(unchecked default)
    
    #[inline]
    M<T>(#[inline] f: T -> ()): () =
        this.funcs.Add(
            () ->
                this.M2<_, int32, int32>(
                    x -> 
                        print("hello")
                        f(x)
                )
        )

main(): () =
    let c = C()
    c.M<int32>(x -> print(" world"))
    let f = c.funcs.get_Item(0)
    f()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello world"
    |> ignore

[<Fact>]
let ``Complex lambda 2``() =
    """
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class C =

    public field funcs: List<() -> ()> = List()
    public field moreFuncs: List<() -> ()> = List()

    #[inline]
    M2<T, U, Z>(#[inline(never)] f: T -> ()): () =
        f(unchecked default)
    
    #[inline]
    M<T>(#[inline] f: T -> ()): () =
        this.funcs.Add(
            () ->
                this.M2<_, int32, int32>(
                    x -> 
                        this.moreFuncs.Add(
                            () ->
                                print("hello")
                                f(x)
                        )
                )
        )

main(): () =
    let c = C()
    c.M<int32>(x -> print(" world"))
    let f = c.funcs.get_Item(0)
    f()
    let f = c.moreFuncs.get_Item(0)
    f()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello world"
    |> ignore

[<Fact>]
let ``Complex lambda 3``() =
    """
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class C =

    public field funcs: List<() -> ()> = List()
    public field moreFuncs: List<() -> ()> = List()

    #[inline]
    M2<T, U, Z>(#[inline(never)] f: T -> ()): () =
        f(unchecked default)
    
    #[inline]
    M<T, W, A>(#[inline] f: T -> ()): () =
        this.funcs.Add(
            () ->
                this.M2<_, int32, int32>(
                    x -> 
                        this.moreFuncs.Add(
                            () ->
                                print("hello")
                                f(x)
                        )
                )
        )

main(): () =
    let c = C()
    c.M<int32, __oly_object, __oly_object>(x -> print(" world"))
    let f = c.funcs.get_Item(0)
    f()
    let f = c.moreFuncs.get_Item(0)
    f()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello world"
    |> ignore

[<Fact>]
let ``Complex lambda 4``() =
    """
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class C =

    public field funcs: List<() -> ()> = List()
    public field moreFuncs: List<() -> ()> = List()

    #[inline]
    M2<T, U, Z>(#[inline(never)] f: T -> ()): () =
        f(unchecked default)
    
    #[inline]
    M<T, W, A, B, C>(#[inline] f: (T, B) -> ()): () =
        this.funcs.Add(
            () ->
                this.M2<_, int32, int32>(
                    x -> 
                        this.moreFuncs.Add(
                            () ->
                                print("hello")
                                f(x, unchecked default)
                        )
                )
        )

main(): () =
    let c = C()
    c.M<int32, __oly_object, __oly_object, int32, int32>((x, y) -> print(" world"))
    let f = c.funcs.get_Item(0)
    f()
    let f = c.moreFuncs.get_Item(0)
    f()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "hello world"
    |> ignore

[<Fact>]
let ``Complex lambda should fail``() =
    """
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class C =

    public field funcs: List<() -> ()> = List()
    public field moreFuncs: List<() -> ()> = List()

    #[inline]
    M2<T, U, Z>(#[inline(never)] f: T -> ()): () =
        f(unchecked default)
    
    #[inline]
    M<T, W, A, B, C>(#[inline] f: (T, B) -> ()): () =
        this.funcs.Add(
            () ->
                this.M2<_, int32, int32>(
                    x -> 
                        this.moreFuncs.Add(
                            () ->
                                print("hello")
                                f(x, unchecked default)
                        )
                )
        )

main(): () =
    let c = C()
    c.M<int32, __oly_object, __oly_object, int32, int32>(x -> print(" world"))
    let f = c.funcs.get_Item(0)
    f()
    let f = c.moreFuncs.get_Item(0)
    f()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type '(int32, int32) -> ()' but is '? -> ()'.",
                """
    c.M<int32, __oly_object, __oly_object, int32, int32>(x -> print(" world"))
                                                         ^^^^^^^^^^^^^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Static local function returning a pointer``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

#[unmanaged(allocation_only)]
#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TParameters...>(TParameters... -> ()): TFunctionPtr

main(): () =

    static let f(x: int32) = print(x)

    let ptr = &&f
    ptr(7)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "7"
    |> ignore

[<Fact>]
let ``Pattern match regression``() =
    """
#[intrinsic("uint8")]
alias byte

#[intrinsic("uint16")]
alias uint16

#[intrinsic("int32")]
alias int32

#[unmanaged(allocation_only)]
#[intrinsic("constant")]
#[import("intrinsic-CLR", "", "sizeof")]
sizeof<require T>: int32

#[intrinsic("print")]
print(__oly_object): ()

enum PacketKind =
    | Invalid

    | Heartbeat
    | ConnectionRequested
    | ConnectionAccepted
    | Disconnect

    | Unreliable
    | UnreliableSequenced

struct PacketUnreliableHeader =
    public field mutable Kind: PacketKind = PacketKind.Invalid
    public field mutable Channel: byte = 0
    public field mutable SequenceId: uint16 = 0
    public field mutable FragmentIndex: uint16 = 0
    public field mutable FragmentCount: uint16 = 0
    public field mutable TotalDataSize: int32 = 0

getHeaderSize(kind: PacketKind): int32 =
    match (kind)
    | PacketKind.Heartbeat
    | PacketKind.ConnectionRequested
    | PacketKind.ConnectionAccepted
    | PacketKind.Disconnect => sizeof<PacketKind>
    | PacketKind.Unreliable => sizeof<PacketUnreliableHeader>
    | PacketKind.UnreliableSequenced => sizeof<PacketUnreliableHeader>
    | _ => -1

getHeaderSize2(kind: PacketKind): int32 =
    match (kind)
    | PacketKind.Heartbeat => sizeof<PacketKind>
    | PacketKind.ConnectionRequested => sizeof<PacketKind>
    | PacketKind.ConnectionAccepted => sizeof<PacketKind>
    | PacketKind.Disconnect => sizeof<PacketKind>
    | PacketKind.Unreliable => sizeof<PacketUnreliableHeader>
    | PacketKind.UnreliableSequenced => sizeof<PacketUnreliableHeader>
    | _ => -1

main(): () =
    let size = getHeaderSize(PacketKind.Unreliable)
    print(size)
    let size = getHeaderSize2(PacketKind.Unreliable)
    print(size)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "1616"
    |> ignore

[<Fact>]
let ``Mutable array extension should work``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("get_length")]
private getLength<T>(mutable T[]): int32

#[open]
extension MutableArrayExtensions<T> =
    inherits mutable T[]

    Length: int32 
        #[inline]
        #[unmanaged(allocation_only)]
        get() = getLength(this)

main(): () =
    let xs = mutable [0;0]
    print(xs.Length)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Mutable array extension should work from reference``() =
    let refSrc =
        """
module M

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("get_length")]
private getLength<T>(mutable T[]): int32

#[open]
extension MutableArrayExtensions<T> =
    inherits mutable T[]

    Length: int32 
        #[inline]
        #[unmanaged(allocation_only)]
        get() = getLength(this)
        """

    """
open static M

main(): () =
    let xs = mutable [0;0]
    print(xs.Length)
    """
    |> OlyWithRef refSrc
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Mutable array enumerable extension should work``() =
    """
#[intrinsic("bool")]
alias bool

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("equal")]
(==)(int32, int32): bool

#[intrinsic("less_than")]
(<)(int32, int32): bool

#[intrinsic("add")]
(+)(int32, int32): int32

#[intrinsic("or")]
(||)(bool, bool): bool

#[unmanaged(allocation_only)]
#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

#[unmanaged(allocation_only)]
#[intrinsic("set_element")]
(`[]`)<T>(mutable T[], index: int32, T): ()

#[intrinsic("cast")]
Cast<T>(object): T

#[unmanaged(allocation_only)]
#[intrinsic("get_length")]
private getLength<T>(mutable T[]): int32

#[open]
extension MutableArrayExtensions<T> =
    inherits mutable T[]

    Length: int32 
        #[inline]
        #[unmanaged(allocation_only)]
        get() = getLength(this)

#[open]
extension MutableArrayEnumerableExtension<T> =
    inherits mutable T[]
    implements System.Collections.Generic.IEnumerable<T>

    private GetEnumerator(): System.Collections.IEnumerator =
        this.GetEnumerator(): System.Collections.Generic.IEnumerator<T>  

    GetEnumerator(): System.Collections.Generic.IEnumerator<T> =
        class Impl =
            implements System.Collections.Generic.IEnumerator<T>

            field mutable arr: mutable T[]
            field mutable currentIndex: int32
            field mutable current: object
            field mutable currentTyped: T

            new(arr: mutable T[]) =
                this {
                    arr = arr
                    currentIndex = -1
                    current = unchecked default
                    currentTyped = unchecked default
                }

            private Current: object get() = this.current

            Current: T get() = this.currentTyped

            MoveNext(): bool =
                if (this.arr.Length == 0)
                    false
                else if ((this.currentIndex == -1) || (this.currentIndex < this.arr.Length))
                    if (this.currentIndex == -1)
                        this.currentIndex <- 0
                    this.current <- this.arr[this.currentIndex]
                    this.currentTyped <- this.arr[this.currentIndex]
                    this.currentIndex <- this.currentIndex + 1
                    true
                else
                    false

            Reset(): () =
                this.currentIndex <- -1
                this.current <- unchecked default
                this.currentTyped <- unchecked default

            Dispose(): () = ()

        Impl(this)


Test<T>(xs: System.Collections.Generic.IEnumerable<T>): () =
    print(System.Linq.Enumerable.Count(xs))

main(): () =
    let xs = mutable [0;0]
    Test<int32>(Cast(xs))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "2"
    |> ignore

[<Fact>]
let ``Try/catch should work in a lambda``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("throw")]
(throw)<TResult>(System.Exception): TResult

main(): () =
    let localLambda() =
        try
            throw System.Exception("success")
            1
        catch (ex: System.Exception) =>
            print(ex.Message)
            2
    print(localLambda())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "success2"
    |> ignore

[<Fact>]
let ``Try/catch should work in a static lambda``() =
    """
#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("throw")]
(throw)<TResult>(System.Exception): TResult

main(): () =
    static let localLambda() =
        try
            throw System.Exception("success")
            1
        catch (ex: System.Exception) =>
            print(ex.Message)
            2
    print(localLambda())
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "success2"
    |> ignore

[<Fact>]
let ``Try/catch should work in a match``() =
    """
#[intrinsic("bool")]
alias bool

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[unmanaged(allocation_only)]
#[intrinsic("equal")]
(===)<T>(o1: T, o2: T): bool where T: not struct

#[unmanaged(allocation_only)]
#[intrinsic("not_equal")]
(!==)<T>(o1: T, o2: T): bool where T: not struct

#[open]
newtype Option<T> where T: not struct =
    field Value: T

    static Some(value: T): Option<T> = Option(value)
    static None: Option<T> get = Option(unchecked default)

    pattern Some(option: Option<T>): T when (option.Value !== unchecked default) =>
        option.Value

    pattern None(option: Option<T>): () when (option.Value === unchecked default) =>
        ()

#[intrinsic("throw")]
(throw)<TResult>(System.Exception): TResult

class C

M(cOpt: Option<C>): int32 =
    match (cOpt)
    | Some(c) =>
        try
            throw System.Exception("success")
            1
        catch (ex: System.Exception) =>
            print(ex.Message)
            2
    | _ =>
        0

main(): () =
    print(M(Some(C())))
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "success2"
    |> ignore

[<Fact>]
let ``Get assembly``() =
    """
open System

#[intrinsic("constant")]
#[import("intrinsic-CLR", "", "typeof")]
typeof<require T>: Type

class A

#[intrinsic("print")]
print(__oly_object): ()

#[inline(never)]
consume(x: __oly_utf16): () = ()

main(): () =
    let ty = typeof<A>
    let loc = ty.Assembly.Location
    consume(loc)
    print(123)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``Int32 ToString via a shape abstraction``() =
    """
open System

#[intrinsic("utf16")]
alias string

#[intrinsic("int32")]
alias int32

printSpecial<T>(mutable value: T): () where T: { ToString(): string } =
    print(value.ToString())

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let x = 456
    printSpecial(x)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "456"
    |> ignore

let AdaptiveMonadSrc =
    """
open System
open System.Collections.Generic
open System.Collections.Concurrent

#[intrinsic("int32")]
alias int32

#[intrinsic("float32")]
alias float32

#[intrinsic("bool")]
alias bool

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("throw")]
(throw)<TResult>(Exception): TResult

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("not")]
(!)(bool): bool

#[inline]
ForEach<T>(xs: System.Collections.Generic.IEnumerable<T>, #[inline] f: scoped T -> ()): () =
    let xse = xs.GetEnumerator()
    while (xse.MoveNext())
        f(xse.Current)

#[inline]
(let!)<M<_>, A, B>(ma: M<A>, f: A -> M<B>): M<B> where M: trait Monad<M> =
    M.Bind(ma, f)

#[inline]
(return)<M<_>, A>(value: A): M<A> where M: trait Monad<M> where A: struct =
    M.Return(value)

interface Monad<M<_>> =

    static abstract Bind<A, B>(ma: M<A>, f: A -> M<B>) : M<B>

    static abstract Return<A>(a: A) : M<A>

private class Subscription =
    implements IDisposable

    private Unsubscribe: () -> () get

    new(unsubscribe: () -> ()) =
        this {
            Unsubscribe = unsubscribe
        }

    Dispose(): () = this.Unsubscribe()

private class Observer<T> =
    implements IObserver<T>

    field callback: T -> ()

    new(callback: T -> ()) = this { callback = callback }

    OnCompleted(): () = ()

    OnError(error: Exception): () =
        throw error

    OnNext(value: T): () =
        this.callback(value)

class Observable<T> =
    implements IObservable<T>

    field subscribers: ConcurrentDictionary<IObserver<T>, ()>
    field mutable value: T

    Subscribe(callback: T -> ()): IDisposable =
        this.Subscribe(Observer(callback))

    Subscribe(observer: IObserver<T>): IDisposable =
        if (!this.subscribers.TryAdd(observer, ()))
            throw InvalidOperationException("Observer already subscribed")
        Subscription(
            () -> 
                let mutable value = unchecked default
                let _ = this.subscribers.TryRemove(observer, &value)
        )

    Value: T
        get() = this.value
        set(value) =
            this.value <- value
            ForEach(this.subscribers, (mutable pair) -> pair.Key.OnNext(value))

    new(value: T) = this { value = value; subscribers = ConcurrentDictionary() }

#[open]
newtype Adaptive<T> =
    public field Value: () -> Observable<T>

    #[open]
    extension NestedMonad =
        inherits Adaptive<T>
        implements Monad<Adaptive>

        static overrides Bind<A, B>(ma: Adaptive<A>, f: A -> Adaptive<B>): Adaptive<B> =

            // This is to make sure we captured the T properly.
            // In the test use case, the T will be object.
            let xs = List<T>()
            print(xs.GetType().Name)

            let valueA = ma.Value()
            let valueB = f(valueA.Value).Value()
            let subscription =
                valueA.Subscribe(
                    t -> valueB.Value <- f(t).Value().Value
                )
            Adaptive<B>(
                () ->
                    valueB
            )

        static overrides Return<A>(a: A): Adaptive<A> =
            let observable = Observable(a)
            Adaptive<A>(
                () -> observable
            )
    """

[<Fact>]
let ``Adaptive monad test``() =
    $"""
{AdaptiveMonadSrc}
main(): () =
    let aval: Adaptive<int32> = return 123
    let f =
        () ->
            let! result = aval
            return 456
    print(f().Value().Value)

    let aval: Adaptive<float32> = return (78: float32)
    let f =
        () ->
            let! result = aval
            return (9.1: float32)
    print(f().Value().Value)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "List`1456List`19.1"
    |> ignore

[<Fact>]
let ``Adaptive monad test 2``() =
    $"""
{AdaptiveMonadSrc}
main(): () =
    let g() =
        let aval: Adaptive<int32> = return 123
        let f =
            () ->
                let! result = aval
                return 456
        print(f().Value().Value)

        let aval: Adaptive<float32> = return (78: float32)
        let f =
            () ->
                let! result = aval
                return (9.1: float32)
        print(f().Value().Value)
    g()
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "List`1456List`19.1"
    |> ignore

[<Fact>]
let ``Adaptive monad test with a reference``() =
    let refSrc =
        $"""
module Prelude

{AdaptiveMonadSrc}
        """

    let src =
        """
open static Prelude

main(): () =
    let aval: Adaptive<int32> = return 123
    let f =
        () ->
            let! result = aval
            return 456
    print(f().Value().Value)

    let aval: Adaptive<float32> = return (78: float32)
    let f =
        () ->
            let! result = aval
            return (9.1: float32)
    print(f().Value().Value)
        """

    OlyWithRef refSrc src
    |> withCompile
    |> shouldRunWithExpectedOutput "List`1456List`19.1"
    |> ignore

[<Fact>]
let ``Adaptive monad test with a reference 2``() =
    let refSrc =
        $"""
module Prelude

{AdaptiveMonadSrc}
        """

    let src =
        """
open static Prelude

main(): () =
    let g() =
        let aval: Adaptive<int32> = return 123
        let f =
            () ->
                let! result = aval
                return 456
        print(f().Value().Value)

        let aval: Adaptive<float32> = return (78: float32)
        let f =
            () ->
                let! result = aval
                return (9.1: float32)
        print(f().Value().Value)
    g()
        """

    OlyWithRef refSrc src
    |> withCompile
    |> shouldRunWithExpectedOutput "List`1456List`19.1"
    |> ignore

[<Fact>]
let ``Adaptive monad test with a reference 3``() =
    let refSrc =
        $"""
module Prelude

{AdaptiveMonadSrc}
        """

    let src =
        """
open static Prelude

main(): () =
    let g() =
        let aval: Adaptive<int32> = return 123
        let f =
            () ->
                let! result = aval
                let! result = aval
                return 456
        print(f().Value().Value)

        let aval: Adaptive<float32> = return (78: float32)
        let f =
            () ->
                let! result = aval
                let! result = aval
                return (9.1: float32)
        print(f().Value().Value)
    g()
        """

    OlyWithRef refSrc src
    |> withCompile
    |> shouldRunWithExpectedOutput "List`1List`1456List`1List`19.1"
    |> ignore

[<Fact>]
let ``byref captured in scoped lambda``() =
    let src =
        """
open System.Numerics

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[inline(never)]
M(f: scoped () -> ()): () =
    f()

#[inline(never)]
M2(m: byref<Matrix4x4>): () =
    M(() -> print(m.M11))

#[inline(never)]
M3(): () =
    let mutable m = Matrix4x4.Identity
    M2(&m)

main(): () =
    M3()
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``inref captured in scoped lambda``() =
    let src =
        """
open System.Numerics

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[inline(never)]
M(f: scoped () -> ()): () =
    f()

#[inline(never)]
M2(m: inref<Matrix4x4>): () =
    M(() -> print(m.M11))

#[inline(never)]
M3(): () =
    let m = Matrix4x4.Identity
    M2(&m)

main(): () =
    M3()
        """

    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"
    |> ignore

[<Fact>]
let ``Should properly infer lambda argument against overloads``() =
    let src =
        """
module Test

open System.Collections.Generic

class A =

    X: __oly_int32 get = 1

class B =

    Y: __oly_int32 get = 2

M(xs: A[], f: A -> ()): () = ()
M<T>(xs: T[], f: T -> ()): () = ()
M<T>(xs: IEnumerable<T>, f: T -> ()): () = ()

Consume(o: __oly_object): () = ()

main(): () =
    let xs1 = []: A[]
    let xs2 = []: B[]
    let xs3 = List<B>()
    M(xs1, x -> Consume(x.X))
    M(xs2, x -> Consume(x.Y))
    M(xs3, x -> Consume(x.Y))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""

[<Fact>]
let ``Should properly infer scoped lambda argument against overloads``() =
    let src =
        """
module Test

open System.Collections.Generic

class A =

    X: __oly_int32 get = 1

class B =

    Y: __oly_int32 get = 2

M<T>(xs: mutable T[], f: scoped T -> ()): () = ()
M<T>(xs: T[], f: scoped T -> ()): () = ()
M<T>(xs: IEnumerable<T>, f: scoped T -> ()): () = ()

Consume(o: __oly_object): () = ()

main(): () =
    let xs4 = Dictionary<__oly_int32, B>()
    M(xs4, (mutable pair) -> Consume(pair.Value.Y))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput ""


[<Fact>]
let ``Cannot clear a span of a class type alias as no overloads exist for it``() =
    """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class A

alias AAlias = A

class B =

    A: AAlias get = A()

    M(): () =
        Span(this.A).Clear()

main(): () =
    ()
    """
    |> Oly
    |> withErrorHelperTextDiagnostics
        [
            // TODO: Honestly, this shouldn't even report ambiguous functions.
            //       It should actually say that there is no valid overload.
            ("'.ctor' has ambiguous functions.",
                """
        Span(this.A).Clear()
        ^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Should choose the correct overload for AsSpan``() =
    """
open System
open System.Runtime.InteropServices

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[unmanaged(allocation_only)]
#[intrinsic("unsafe_cast")]
private _cast<T>(object): T

#[unmanaged(allocation_only)]
AsMutable<T>(arr: T[]): mutable T[] =
    _cast(arr)

#[unmanaged(allocation_only)]
#[intrinsic("get_element")]
(`[]`)<T>(mutable T[], index: int32): T

#[open]
extension MutableArrayDotNetExtensions<T> =
    inherits mutable T[]

    AsSpan(): Span<T> = Span(this)
    AsReadOnlySpan(): ReadOnlySpan<T> = ReadOnlySpan(this)

#[open]
extension ArrayDotNetExtensions<T> =
    inherits T[]

    AsSpan(): ReadOnlySpan<T> = AsMutable(this).AsReadOnlySpan()

#[open]
extension ArrayCastDotNetExtensions<T> where T: struct, ValueType, { new() } =
    inherits T[]

    AsSpan<TCast>(): ReadOnlySpan<TCast> where TCast: struct, ValueType, { new() } = 
        Span<_>.op_Implicit(MemoryMarshal.Cast(AsMutable(this).AsSpan()))

main(): () =
    let xs = [1;2;3]
    let xs2 = mutable [0;0;0]
    xs.AsSpan().CopyTo(Span(xs2))
    print(xs2[0])
    print(xs2[1])
    print(xs2[2])
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Scoped lambda captures a mutable struct``() =
    """
open System
open System.Collections.Generic

#[intrinsic("int32")]
alias int32

#[intrinsic("base_object")]
alias object

#[intrinsic("print")]
print(object): ()

#[intrinsic("by_ref")]
alias (&)<T>

#[intrinsic("address_of")]
(&)<T>(T): T&

#[inline(never)]
M<T>(f: scoped () -> T): T =
    f()

struct Item =
    public field Object: object = Object()
    public field Object2: object = Object()

class Collection =

    Items: List<Item> get = List()

    Add(): () =
        this.Items.Add(Item())

    Get(index: int32): Item =
        let mutable item = this.Items.get_Item(index)
        M(
            () ->
                print(item.Object2)
                item
        )

main(): () =
    let xs = Collection()
    xs.Add()
    let item = xs.Get(0)
    print(item.Object2)
    """
    |> Oly
    |> withCompile
    |> shouldRunWithExpectedOutput "System.ObjectSystem.Object"
    |> ignore

[<Fact>]
let ``Catching an exception and returning -1 should be the right value``() =
    let src =
        """
#[intrinsic("int32")]
alias int32

#[intrinsic("bool")]
alias bool

#[intrinsic("utf16")]
alias string

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("throw")]
(throw)<TResult>(System.Exception): TResult

#[System.Diagnostics.DebuggerHiddenAttribute()]
fail<TResult>(msg: string): TResult =
    throw System.Exception(msg)

#[inline(never)]
ThrowSomething(cond: bool): int32 =
    if (cond)
        9
    else
        fail("a message for throw")

test(): int32 =
    try
        ThrowSomething(false)
    catch (ex: System.Exception) =>
        -1

main(): () =
    let x = test()
    print(x)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "-1"

[<Fact>]
let ``Able to use DotNet generic math``() =
    let src =
        """
open System.Numerics

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

Test<T>(): T where T: INumberBase<T> =
    T.One

main(): () =
    print(Test<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Able to use DotNet generic math 2``() =
    let src =
        """
namespace TestOly

open System.Numerics

#[open]
module Prelude =
    #[intrinsic("int32")]
    alias int32

    #[intrinsic("print")]
    print(__oly_object): ()

#[export]
module Exported =

    Test<T>(): T where T: INumberBase<T> =
        T.One

    main(): () =
        print(Test<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Able to use DotNet generic math 3``() =
    let src =
        """
open System.Numerics

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

Test<T>(): T where T: IBinaryInteger<T> =
    T.One

main(): () =
    print(Test<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Able to use DotNet generic math 4``() =
    let src =
        """
open System.Numerics

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    print(int32.One)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Able to use DotNet generic math 5``() =
    let src =
        """
open System.Numerics

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

Test<T>(): T where T: IBinaryInteger<T> =
    T.get_One()

main(): () =
    print(Test<int32>())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Able to use DotNet generic math 6``() =
    let src =
        """
open System.Numerics

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    print(int32.get_One())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "1"

[<Fact>]
let ``Overloading regression on AddByteOffset``() =
    let src =
        """
#[intrinsic("uint8")]
alias uint8

#[intrinsic("int32")]
alias int32

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("unsafe_cast")]
nuint(int32): nuint

#[intrinsic("unsafe_cast")]
nuint(uint8): nuint

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

do_not_run_this(): () =
    let mutable x: int32 = 5
    let result = &System.Runtime.CompilerServices.Unsafe.AddByteOffset(&x, nuint(4))

main(): () =
    print("hello")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Enum to object then to enum``() =
    let src =
        """
#[intrinsic("unsafe_cast")]
unsafeCast<T>(__oly_object): T

#[intrinsic("print")]
print(__oly_object): ()

enum E =
    | A
    | B 

main(): () =
    let x = E.B
    let y = x: System.Enum
    let z = unsafeCast<E>(y)
    print(z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "B"

[<Fact>]
let ``Newtype inherits ValueType properly``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

newtype A =
    field Value: __oly_int32

main(): () =
    let a = A(24)
    let v = a: ValueType
    print(a)
    print(v.GetType().Name)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "24Int32"

[<Fact>]
let ``Newtype inherits Enum properly``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

enum E =
    | A
    | B 

newtype A =
    field Value: E

main(): () =
    let a = A(E.B)
    let v = a: Enum
    print(a)
    print(v.GetType().Name)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "BE"

[<Fact>]
let ``Newtype of a newtype inherits ValueType properly``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

newtype B =
    field Value: __oly_int32

newtype A =
    field Value: B

main(): () =
    let a = A(B(42))
    let v = a: ValueType
    print(a)
    print(v.GetType().Name)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "42Int32"

[<Fact>]
let ``Newtype should error when trying to override method ToString``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

newtype A =
    field Value: __oly_int32

    overrides ToString(): __oly_utf16 =
        "A"

main(): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("'ToString' cannot be overriden in a newtype declaration.",
                """
    overrides ToString(): __oly_utf16 =
              ^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Newtype should error when NOT trying to hide method ToString``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

newtype A =
    field Value: __oly_int32

    ToString(): __oly_utf16 =
        "Test"

main(): () = ()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("The member 'ToString' will hide over its base.",
                """
    ToString(): __oly_utf16 =
    ^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Newtype should pass when trying to hide method ToString``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

newtype A =
    field Value: __oly_int32

    new ToString(): __oly_utf16 =
        "Test"

main(): () =
    let a = A(32)
    print(a.ToString())
    let v = a: ValueType
    print(v.ToString())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Test32"

[<Fact>]
let ``Newtype of Vector512 - regression - should not crash runtime``() =
    let src =
        """
open System
open System.IO
open System.Text
open System.Numerics
open System.Diagnostics
open System.Security.Cryptography
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.Runtime.Intrinsics
open System.Collections.Generic
open System.Collections.Concurrent

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("add")]
(+)(int32, int32) : int32

shape DotNetIndexGetter<TKey, TValue> =

    get_Item(TKey): TValue

(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: DotNetIndexGetter<TKey, TValue> = x.get_Item(key)

newtype BitSet512 =
    field value: Vector512<int32>

    GetSomeValue(): int32 = this.value[0] + this.value[1]

    GetSomething(): int32 =
        let valueIndex = this.GetSomeValue()
        valueIndex

main(): () =
    let x = BitSet512(default)
    print(x.GetSomeValue())
    print(x.GetSomething())
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "00"

[<Fact>]
let ``Able to use exported type inside an exported function``() =
    let src =
        """
namespace Test

#[open]
module OlyPrelude =
    #[intrinsic("int32")]
    alias int32

    #[intrinsic("print")]
    print(__oly_object): ()

    #[intrinsic("get_element")]
    (`[]`)<T>(mutable T[], index: int32): T

#[export]
class ExportedClass<T> =

    Value: mutable T[] get

    new(xs: mutable T[]) =
        this {
            Value = xs
        }

#[export]
module TestModule =

    Run<T>(input: mutable T[]): ExportedClass<T> =
        let xs = ExportedClass<T>(input)
        xs

module Main =

    main(): () =
        let result = TestModule.Run(mutable [1;2;3;4]).Value
        print(result[0])
        print(result[3])
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "14"

[<Fact>]
let ``Not able to use non-exported type inside an exported function``() =
    let src =
        """
namespace Test

#[open]
module OlyPrelude =
    #[intrinsic("int32")]
    alias int32

    #[intrinsic("print")]
    print(__oly_object): ()

    #[intrinsic("get_element")]
    (`[]`)<T>(mutable T[], index: int32): T

class NonExportedClass<T> =

    Value: mutable T[] get

    new(xs: mutable T[]) =
        this {
            Value = xs
        }

#[export]
module TestModule =

    Run<T>(input: mutable T[]): mutable T[] =
        let xs = NonExportedClass<T>(input)
        xs.Value

module Main =

    main(): () =
        let result = TestModule.Run(mutable [1;2;3;4])
        print(result[0])
        print(result[3])
        """
    Oly src
        |> withErrorHelperTextDiagnostics
        [
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        let xs = NonExportedClass<T>(input)
                 ^^^^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        let xs = NonExportedClass<T>(input)
                 ^^^^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        let xs = NonExportedClass<T>(input)
                 ^^^^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        let xs = NonExportedClass<T>(input)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        xs.Value
        ^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
        xs.Value
        ^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Not able to use non-exported type inside an exported function 2``() =
    let src =
        """
namespace Test

#[open]
module OlyPrelude =
    #[intrinsic("int32")]
    alias int32

    #[intrinsic("print")]
    print(__oly_object): ()

    #[intrinsic("get_element")]
    (`[]`)<T>(mutable T[], index: int32): T

class NonExportedClass<T> =

    Value: mutable T[] get

    new(xs: mutable T[]) =
        this {
            Value = xs
        }

#[export]
module TestModule =

    Run<T>(input: mutable T[]): mutable T[] =
        let f() =
            let xs = NonExportedClass(input)
            xs.Value
        f()

module Main =

    main(): () =
        let result = TestModule.Run(mutable [1;2;3;4])
        print(result[0])
        print(result[3])
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
            let xs = NonExportedClass(input)
                     ^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
            let xs = NonExportedClass(input)
                     ^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
            let xs = NonExportedClass(input)
                     ^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
            let xs = NonExportedClass(input)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
            xs.Value
            ^^
"""
            )
            ("Type parameter 'T' cannot be used in this vanilla construct. Yes this error message is terrible. TODO:",
                """
            xs.Value
            ^^^^^^^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Property has the specified attribute``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

class Testing1Attribute
class Testing2Attribute

class A

class B =

    #[Testing1]
    P0: int32 get, set = 123

    P1: int32 get, set = 123
    
    #[Testing2]
    P2: int32 get, set = 123

class C

ForEach<T>(xs: System.Collections.Generic.IEnumerable<T>, f: T -> ()): () =
    let xse = xs.GetEnumerator()
    if (xse.MoveNext())
        f(xse.Current)

main(): () =
    let a = A()
    let b = B()
    let c = C()
    ForEach(b.GetType().GetProperty("P0").CustomAttributes,
        attr ->
            print(attr.Constructor.DeclaringType.Name)
    )
    print("--")
    ForEach(b.GetType().GetProperty("P1").CustomAttributes,
        attr ->
            print(attr.Constructor.DeclaringType.Name)
    )
    print("--")
    ForEach(b.GetType().GetProperty("P2").CustomAttributes,
        attr ->
            print(attr.Constructor.DeclaringType.Name)
    )
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "Testing1Attribute----Testing2Attribute"

[<Fact>]
let ``Able to use newtype when a constraint requires ValueType``() =
    let refSrc =
        """
#[open]
module RefModule

open System

#[intrinsic("print")]
print(__oly_object): ()

M<T>(): () where T: ValueType = ()

class C<T> where T: ValueType

struct S
        """
    let src =
        """
newtype NewS =
    public field Value: S

main(): () =
    M<NewS>()
    let _ = C<NewS>()
    print("hello")
        """
    OlyWithRef refSrc src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Able to use newtype when a constraint requires ValueType 2``() =
    let refSrc =
        """
#[open]
module RefModule

open System

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

interface IComponent

interface IComponent<N, T> where N: constant int32 where T: blittable, struct, ValueType, { new() } =
    inherits IComponent
        """
    let src =
        """
struct S

newtype NewS =
    public field Value: S

#[open]
extension NewSExtensions =
    inherits NewS
    implements IComponent<1, NewS>

main(): () =
    print("hello")
        """
    OlyWithRef refSrc src
    |> withCompile
    |> shouldRunWithExpectedOutput "hello"

[<Fact>]
let ``Equality operator should work for nint``() =
    let refSrc =
        """
#[open]
module RefModule

open System

#[intrinsic("int32")]
alias int32

#[intrinsic("native_int")]
alias nint

#[intrinsic("unsafe_cast")]
nint(int32): nint

#[intrinsic("print")]
print(__oly_object): ()

(==)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_Equality(T1, T2): T3 } = T1.op_Equality(x, y)
        """
    let src =
        """
main(): () =
    print(nint(0) == nint(0))
        """
    OlyWithRef refSrc src
    |> withCompile
    |> shouldRunWithExpectedOutput "True"

[<Fact>]
let ``C# abstract generic class should work``() =
    let csSrc =
        """
public abstract class BaseA<T>
{
}
        """

    let src =
        """
class A =
    inherits BaseA<__oly_int32>

    new() = base()

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let a = A()
    print("hello")
        """
    OlyWithCSharp csSrc src
        (
            fun c ->
                c
                |> withCompile
                |> shouldRunWithExpectedOutput "hello"
        )

[<Fact>]
let ``Regression - defined exports with interfaces should work``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

#[export]
interface IExample =

    GenericExample<T>(x: T): ()

#[export]
interface IExample2 =

    GenericExample<T>(x: T): () where T: IExample

#[export]
test<Z>(x: Z): () where Z: IExample =
  Console.Write("test")
  x.GenericExample<Z>(x)

#[export]
class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
class Example2 =
  implements IExample2

  new() = this { }

  GenericExample<U>(x: U): () where U: IExample = 
      let f() =
          test<_>(x)
      f()

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "testExample"

[<Fact>]
let ``Regression - defined exports with interfaces but reversed should work``() =
    let src =
        """
open System

#[intrinsic("print")]
print(__oly_object): ()

#[export]
test<Z>(x: Z): () where Z: IExample =
  Console.Write("test")
  x.GenericExample<Z>(x)

#[export]
class Example =
  implements IExample

  new() = this { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
class Example2 =
  implements IExample2

  new() = this { }

  GenericExample<U>(x: U): () where U: IExample = 
      let f() =
          test<_>(x)
      f()

#[export]
interface IExample =

    GenericExample<T>(x: T): ()

#[export]
interface IExample2 =

    GenericExample<T>(x: T): () where T: IExample

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "testExample"

[<Fact>]
let ``Regression - should not require type annotation for "task"``() =
    let src =
        """
open System
open System.Threading
open System.Threading.Tasks

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let task = // should not need a type annotation
        Task.Factory.StartNew(
            () ->
                Thread.Sleep(100)
                123
        )
    print(task.Result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Regression - should not require type annotation for "task" 2``() =
    let src =
        """
open System
open System.Threading
open System.Threading.Tasks

#[intrinsic("utf16")]
alias strbg

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    let task = // should not need a type annotation
        Task.Factory.StartNew(
            () ->
                Thread.Sleep(100)
                "123"
        )
    print(task.Result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Regression - should not require type annotation for "task" 3``() =
    let src =
        """
open System
open System.Threading
open System.Threading.Tasks

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

M<T>(x: T): T = x

M2<T>(x: T): () =
    let task = // should not need a type annotation
        Task.Factory.StartNew(
            () ->
                Thread.Sleep(100)
                M(x)                
        )
    print(task.Result)

main(): () =
    M2(456)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"

[<Fact>]
let ``Get index of a span``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[unmanaged(allocation_only)]
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[unmanaged(allocation_only)]
#[intrinsic("bitwise_and")]
(&)(int32, int32): int32

(&)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_BitwiseAnd(T1, T2): T3 } = T1.op_BitwiseAnd(x, y)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: trait { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: trait { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

main(): () =
    let mutable s = Span<int32>(mutable [1;2;3])
    let x = &s[0]
    print(x)
    let y = &s[1]
    print(y)
    let z = &s[2]
    print(z)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "123"

[<Fact>]
let ``Get index of a span 2``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[unmanaged(allocation_only)]
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[unmanaged(allocation_only)]
#[intrinsic("bitwise_and")]
(&)(int32, int32): int32

(&)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_BitwiseAnd(T1, T2): T3 } = T1.op_BitwiseAnd(x, y)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: trait { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: trait { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

M(mutable s: Span<int32>, f: byref<int32> -> ()): () =
    f(&s[2])

main(): () =
    let mutable s = Span<int32>(mutable [1;2;3])
    M(s, x -> print(x))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "3"

[<Fact>]
let ``Get index of a span 3``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("print")]
print(__oly_object): ()

#[unmanaged(allocation_only)]
#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[unmanaged(allocation_only)]
#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[unmanaged(allocation_only)]
#[intrinsic("bitwise_and")]
(&)(int32, int32): int32

(&)<T1, T2, T3>(x: T1, y: T2): T3 where T1: trait { static op_BitwiseAnd(T1, T2): T3 } = T1.op_BitwiseAnd(x, y)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: trait { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
#[System.Diagnostics.DebuggerHiddenAttribute()]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: trait { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

M(s: byref<Span<int32>>): byref<int32> =
    &s[1]

main(): () =
    let mutable s = Span<int32>(mutable [1;2;3])
    let result = M(&s)
    print(result)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "2"

[<Fact>]
let ``Should choose right overload when used as receiver``() =
    let src =
        """
open System
open System.Numerics

#[intrinsic("int32")]
alias int32

#[intrinsic("subtract")]
(-)(int32, int32) : int32
(-)<T1, T2, T3>(x: T1, y: T2): T3 where T1: { static op_Subtraction(T1, T2): T3 } = T1.op_Subtraction(x, y)

main(): () =
    Console.Write(Math.Abs((Vector3.Zero - Vector3.Zero).Length()))
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"

[<Fact>]
let ``Should resolve address-of correctly``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("unsafe_cast")]
nuint(int32): nuint

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = 
    x.get_Item(key)

struct S<T> =
    public field mutable X: T
    new(x: T) = this { X = x }

M(): () =
    let mutable s = S(S(456))
    let z = &s.X
    let res = &System.Runtime.CompilerServices.Unsafe.AddByteOffset(&z, nuint(0))
    print(res.X)

main(): () =
    M()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"

[<Fact>]
let ``Should resolve address-of correctly 2``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("unsafe_cast")]
nuint(int32): nuint

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = 
    x.get_Item(key)

struct S<T> =
    public field mutable X: T
    new(x: T) = this { X = x }

M(): () =
    let mutable s = S(S(456))
    let z = &s.X
    let res = 
        &System.Runtime.CompilerServices.Unsafe.AddByteOffset(
            if (true)
                let w = 1
                &z
            else
                let a = 1
                &z, 
            nuint(0)
         )
    print(res.X)

main(): () =
    M()
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "456"

[<Fact>]
let ``Should fail address-of correctly because field is not mutable``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("native_uint")]
alias nuint

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T> 

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("unsafe_cast")]
nuint(int32): nuint

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } where TValue: scoped = 
    x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = 
    x.get_Item(key)

struct S<T> =
    public field X: T
    new(x: T) = this { X = x }

M(): () =
    let mutable s = S(S(456))
    let z = &s.X
    let res = &System.Runtime.CompilerServices.Unsafe.AddByteOffset(&z, nuint(0))
    print(res.X)

main(): () =
    M()
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Expected type 'byref<S<int32>>' but is 'inref<S<int32>>'.",
                """
    let res = &System.Runtime.CompilerServices.Unsafe.AddByteOffset(&z, nuint(0))
                                                                    ^^
"""
            )
        ]
    |> ignore


[<Fact>]
let ``Should resolve the default correctly``() =
    let src =
        """
open System
open System.Numerics

#[intrinsic("void")]
alias void

#[intrinsic("base_object")]
alias object

#[intrinsic("int32")]
alias int32

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

#[intrinsic("native_ptr")]
alias (*)<T>

#[intrinsic("print")]
print(object): ()

#[intrinsic("unsafe_cast")]
(*)<T>(void*): byref<T>

#[intrinsic("unsafe_cast")]
(*)<T>(T*): byref<T>

#[intrinsic("unsafe_address_of")]
(&&)<T>(T): T*

#[intrinsic("load_function_ptr")]
(&&)<TFunctionPtr, TReturn, TParameters...>(TParameters... -> TReturn): TFunctionPtr

M(_dummy: int32, res: Quaternion*): () =
    *res <- Quaternion.Identity

main(): () =
    let mutable res = default
    M(0, &&res)
    print(res)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "{X:0 Y:0 Z:0 W:1}"

[<Fact>]
let ``Should not crash and should error``() =
    let src =
        """
open System.Collections.Generic

main(): () =
    let xs = List()
    xs.Add(xs)
        """
    Oly src
    |> withErrorHelperTextDiagnostics
        [
            ("Detected a cycle in inference: '?T' cannot be solved with 'List<?T>'.",
                """
    xs.Add(xs)
           ^^
"""
            )
        ]
    |> ignore

[<Fact>]
let ``Dotnet - Should still get shape member even though it has a subsumption``() =
    let src = 
        """
open System
open System.Collections.Concurrent

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

shape DotNetIndexSetter<TKey, TValue> =

    set_Item(TKey, TValue): ()

(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: DotNetIndexSetter<TKey, TValue> = x.set_Item(key, value)

abstract default class A

class B =
    inherits A

main(): () =
    let dict = ConcurrentDictionary<A, ()>()
    let b = B()
    dict[b] <- ()
    print("doot")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "doot"
    |> ignore

[<Fact>]
let ``Dotnet - Should still get shape member even though it has a subsumption 2``() =
    let src = 
        """
open System
open System.Collections.Concurrent

#[intrinsic("int32")]
alias int32

#[intrinsic("print")]
print(__oly_object): ()

#[inline]
(`[]`)<T, TKey, TValue>(mutable x: T, key: TKey, value: TValue): () where T: trait { set_Item(TKey, TValue): () } = 
    x.set_Item(key, value)

abstract default class A

class B =
    inherits A

main(): () =
    let dict = ConcurrentDictionary<A, ()>()
    let b = B()
    dict[b] <- ()
    print("doot")
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "doot"
    |> ignore