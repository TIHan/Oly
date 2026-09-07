module DotNet.Conformance.ConstraintTests

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

[<Fact>]
let ``C# new() constraint``() =
    let csSrc =
        """
public static class ExampleModule
{
    public static T M<T>() where T : new()
    {
        return new T();
    }
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    X: int32 get
    new() = this { X = 123 }

main(): () =
    let example = ExampleModule.M<Example>()
    Console.Write(example.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123"
        )

[<Fact>]
let ``C# new() constraint on interface method``() =
    let csSrc =
        """
public interface IExample
{
    T M<T>() where T : new();
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    implements IExample

    #[export]
    M<T>(): T where T: { new() } =
        T()

class C =

    X: int32 get = 123

main(): () =
    let example = Example()
    let c = example.M<C>()
    Console.Write(c.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123"
        )

[<Fact>]
let ``C# new() constraint on interface method 2 - uses a struct``() =
    let csSrc =
        """
public interface IExample
{
    T M<T>() where T : new();
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    implements IExample

    #[export]
    M<T>(): T where T: { new() } =
        T()

struct C =

    X: int32 get = 123

main(): () =
    let example = Example()
    let c = example.M<C>()
    Console.Write(c.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123"
        )

[<Fact>]
let ``C# new() constraint on interface method 3 - uses a struct with no parameterless constructor``() =
    let csSrc =
        """
public interface IExample
{
    T M<T>() where T : new();
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    implements IExample

    #[export]
    M<T>(): T where T: { new() } =
        T()

struct C =

    new(x: int32) = this { X = x }
    X: int32 get

main(): () =
    let example = Example()
    let c = example.M<C>()
    Console.Write(c.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "0"
        )

[<Fact>]
let ``C# new() constraint on interface method 4 - uses a struct with a static constructor``() =
    let csSrc =
        """
public interface IExample
{
    T M<T>() where T : new();
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    implements IExample

    #[export]
    M<T>(): T where T: { new() } =
        T()

#[export]
struct C =

    static Y: int32 get =
        Console.Write("static")
        456

    X: int32 get = 123

main(): () =
    let example = Example()
    let c = example.M<C>()
    Console.Write(c.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123"
        )

[<Fact>]
let ``C# new() constraint on interface method 5 - uses a class with a static constructor``() =
    let csSrc =
        """
public interface IExample
{
    T M<T>() where T : new();
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    implements IExample

    #[export]
    M<T>(): T where T: { new() } =
        T()

#[export]
class C =

    public static field Y: int32 =
        Console.Write("static")
        456

    X: int32 get = 123

main(): () =
    let example = Example()
    let _ = C()
    let c = example.M<C>()
    Console.Write(c.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "123"
        )

[<Fact>]
let ``C# struct constraint on interface method``() =
    let csSrc =
        """
public interface IExample
{
    T M<T>() where T : struct;
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    implements IExample

    #[export]
    M<T>(): T where T: ValueType, struct, { new() } =
        default

struct S =

    X: int32 get = 123

main(): () =
    let example = Example()
    let s = example.M<S>()
    Console.Write(s.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "0"
        )

[<Fact>]
let ``C# class constraint on interface method``() =
    let csSrc =
        """
public interface IExample
{
    void M<T>() where T : class;
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    implements IExample

    #[export]
    M<T>(): () where T: not struct = ()

class C =

    X: int32 get = 123

main(): () =
    let example = Example()
    let c = example.M<C>()
    Console.Write("hello")
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "hello"
        )

[<Fact>]
let ``C# unmanaged constraint on interface method``() =
    let csSrc =
        """
public interface IExample
{
    T M<T>() where T : unmanaged;
}
        """

    let src =
        """
open System

#[intrinsic("int32")]
alias int32

class Example =
    implements IExample

    #[export]
    M<T>(): T where T: ValueType, struct, unmanaged, { new() } =
        default

struct S =

    X: int32 get = 123

main(): () =
    let example = Example()
    let s = example.M<S>()
    Console.Write(s.X)
        """
    OlyWithCSharp csSrc src
        (
        withCompile
        >> shouldRunWithExpectedOutput "0"
        )

[<Fact>]
let ``Unmanaged constraint on interface method``() =
    let src =
        """
open System

#[intrinsic("int32")]
alias int32

#[export]
interface IExample =

    M<T>(): T where T: ValueType, struct, unmanaged, { new() }

class Example =
    implements IExample

    #[export]
    M<T>(): T where T: ValueType, struct, unmanaged, { new() } =
        default

struct S =

    X: int32 get = 123

main(): () =
    let example = Example()
    let s = example.M<S>()
    Console.Write(s.X)
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "0"
