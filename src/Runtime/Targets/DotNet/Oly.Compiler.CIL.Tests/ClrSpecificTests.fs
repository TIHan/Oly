module ClrSpecificTests

open System.IO
open Xunit
open TestUtilities
open Utilities
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime.Target.DotNet
open Oly.Core
open System.Runtime.Loader
open System

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

let dotnetTarget = DotNetTarget()
let targetName = "net7"
let targetInfo = OlyTargetInfo(targetName, OlyOutputKind.Library, Some "System.ValueType", Some "System.Enum")
let projectPath = OlyPath.Create "olytest"
let projectPath2 = OlyPath.Create "olytest2"
let dotnetReferences =
    dotnetTarget.ResolveReferencesAsync(projectPath, targetInfo, ImArray.empty, ImArray.empty, System.Threading.CancellationToken.None).Result.Paths
    |> ImArray.map (fun x -> 
        match dotnetTarget.ImportReferenceAsync(projectPath, targetInfo, x, System.Threading.CancellationToken.None).Result with
        | Ok(Some res) -> OlyProjectReference.Create res.CompilationReference
        | Ok(None) -> failwith "None"
        | Error msg -> failwith msg
    )
let workspace = OlyWorkspace.Create([dotnetTarget])
let lazyInfo =
    lazy
        let config = OlyProjectConfiguration("olytest", ImArray.empty, false)
        let sol, _ = workspace.Solution.CreateProject(projectPath, config, "dotnet", OlyTargetInfo(targetName, OlyOutputKind.Executable, Some "System.ValueType", Some "System.Enum"), System.Threading.CancellationToken.None)
        let sol, proj = sol.UpdateReferences(projectPath, dotnetReferences, System.Threading.CancellationToken.None)
        let syntaxTree = OlySyntaxTree.Parse(OlyPath.Create "olytest", (fun _ -> OlySourceText.Create("")), parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true })
        let (sol: OlySolution), (proj: OlyProject), (doc: OlyDocument) = sol.UpdateDocument(proj.Path, OlyPath.Create "olytest", syntaxTree, ImArray.empty)
        sol, proj, doc
let lazyInfo2 =
    lazy
        let config = OlyProjectConfiguration("olytest2", ImArray.empty, false)
        let sol, _ = workspace.Solution.CreateProject(projectPath2, config, "dotnet", OlyTargetInfo(targetName, OlyOutputKind.Library, Some "System.ValueType", Some "System.Enum"), System.Threading.CancellationToken.None)
        let sol, proj = sol.UpdateReferences(projectPath2, dotnetReferences, System.Threading.CancellationToken.None)
        let syntaxTree = OlySyntaxTree.Parse(OlyPath.Create "olytest2", (fun _ -> OlySourceText.Create("")), parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true })
        let (sol: OlySolution), (proj: OlyProject), (doc: OlyDocument) = sol.UpdateDocument(proj.Path, OlyPath.Create "olytest2", syntaxTree, ImArray.empty)
        sol, proj, doc
let getProject src =
    let sol, proj, doc = lazyInfo.Value
    let syntaxTree = OlySyntaxTree.Parse(OlyPath.Create "olytest", (fun _ -> OlySourceText.Create(src)), parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true })
    let _, proj, _ = sol.UpdateDocument(proj.Path, doc.Path, syntaxTree, ImArray.empty)
    proj

let getProjectWithReferenceProject refSrc src =
    let sol, proj, doc = lazyInfo2.Value
    let syntaxTree = OlySyntaxTree.Parse(OlyPath.Create "olytest2", (fun _ -> OlySourceText.Create(refSrc)), parsingOptions = OlyParsingOptions.Default)
    let sol, refProj, _ = sol.UpdateDocument(proj.Path, doc.Path, syntaxTree, ImArray.empty)

    let sol, proj, doc = lazyInfo.Value
    let syntaxTree = OlySyntaxTree.Parse(OlyPath.Create "olytest", (fun _ -> OlySourceText.Create(src)), parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true })
    let sol, proj, _ = sol.UpdateDocument(proj.Path, doc.Path, syntaxTree, ImArray.empty)
    let _, proj = sol.UpdateReferences(projectPath, dotnetReferences.Add(OlyProjectReference.Create(OlyCompilationReference.Create(projectPath2, fun () -> refProj.Compilation))), System.Threading.CancellationToken.None)
    proj

let run expectedOutput src =
    OlySharp (coreLib + src)
    |> shouldCompile
    |> shouldRunWithExpectedOutput expectedOutput
    |> ignore

[<Fact>]
let ``Defining and using Vector3``() =
    let src =
        """
#[import("CLR:System.Numerics", "System.Numerics", "Vector3")]
struct Vector3 =

    public mutable field X: float32
    public mutable field Y: float32
    public mutable field Z: float32

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
    |> shouldCompile
    |> shouldRunWithExpectedOutput "123"
    |> ignore

[<Fact>]
let ``CLR import generic type 2``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[import("CLR:mscorlib", "System.Collections.Generic", "List`1")]
class List<T> =

    get_Count() : __oly_int32
    get_Item(__oly_int32): T

    new()

    Add(item: T) : ()

    ToString() : __oly_utf16

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
    |> shouldCompile
    |> shouldRunWithExpectedOutput "012123"
    |> ignore

[<Fact>]
let ``Custom CLR import intrinsic int32``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("int32")]
#[import("CLR:mscorlib", "System", "Int32")]
struct CustomInt32 =

    static Parse(s: __oly_utf16) : CustomInt32

    ToString() : __oly_utf16

main() : () =
    let x = CustomInt32.Parse("123")
    print(x)
    print(9)
    print(x.ToString())
        """
    Oly src
    |> shouldCompile
    |> shouldRunWithExpectedOutput "1239123"
    |> ignore

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

test<T>(x: T): () where T: IDisposable = x.Dispose()

class Test =
  implements IDisposable

  new() = {}

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "3Hello World!3"

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

test<T>(x: T): () where T: IDisposable = x.Dispose()

class Test =
  implements IDisposable

  new() = {}

  Dispose(): () = 
    let x = 1
    ()

main(): () =
  let tt = Test()
  test<_>(tt)
  test<_>(1)
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "1223"

[<Fact>]
let ``GetHashCode``() =
    let src =
        """
open System
open System.Diagnostics
open System.Numerics

#[intrinsic("by_ref_read_write")]
alias (&)<T>

#[intrinsic("address_of")]
(&)<T>(T): T&

struct TestStruct =

    x: Int32 get, set

    new(x: Int32) = { x = x }

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "0"

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

hash<T>(x: T): int32 where T: { mutable GetHashCode(): int32 } =
    x.GetHashCode()

main(): () =
    print(hash<Int32>(123))
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

hash<T>(x: T): int32 where T: { mutable GetHashCode(): int32 } =
    x.GetHashCode()

main(): () =
    print(hash(123))
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

struct Hash<T> where T: { mutable GetHashCode(): int32 } =
    
    private field item: T

    new(item: T) = { item = item }

    GetValue(): int32 =
        this.item.GetHashCode()

main(): () =
    let h = Hash<System.Int32>(123)
    print(h.GetValue())
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

struct Hash<T> where T: { mutable GetHashCode(): int32 } =
    
    private field item: T

    new(item: T) = { item = item }

    GetValue(): int32 =
        this.item.GetHashCode()

main(): () =
    let h = Hash(123)
    print(h.GetValue())
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

struct Hash<T> where T: { mutable GetHashCode(): int32 } =
    
    private field item: T

    new(item: T) = { item = item }

    #[inline(never)]
    GetValue(): int32 =
        this.item.GetHashCode()

struct Hash2<T> where T: { mutable GetHashCode(): int32 } =

    private field item: Hash<T>

    new(item: Hash<T>) = { item = item }

    #[inline(never)]
    GetValue(): int32 =
        this.item.GetValue().GetHashCode()

main(): () =
    let h = Hash(123)
    let h2 = Hash2(h)
    print(h2.GetValue())
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

interface IHash<T> where T: { mutable GetHashCode(): int32 } =

    default GetValue(x: T): int32 =
        x.GetHashCode()

struct Hash<T> where T: { mutable GetHashCode(): int32 } =
    implements IHash<T>
    
    private field item: T

    new(item: T) = { item = item }

    #[inline(never)]
    GetValue(): int32 =
        let x: IHash<T> = this
        x.GetValue(this.item)

struct Hash2<T> where T: { mutable GetHashCode(): int32 } =

    private field item: Hash<T>

    new(item: Hash<T>) = { item = item }

    #[inline(never)]
    GetValue(): int32 =
        this.item.GetValue().GetHashCode()

main(): () =
    let h = Hash(123)
    let h2 = Hash2(h)
    print(h2.GetValue())
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

interface IHash<T> where T: { mutable GetHashCode(): int32 } =

    default GetValue(x: T): int32 =
        x.GetHashCode()

struct Hash<T> where T: { mutable GetHashCode(): int32 } =
    implements IHash<T>
    
    public field item: T

    new(item: T) = { item = item }

    #[inline(never)]
    GetValue(): int32 =
        let x: IHash<T> = this
        x.GetValue(this.item)

struct Hash2<T> where T: { mutable GetHashCode(): int32 } =
    implements IHash<T>

    public field item: Hash<T>

    new(item: Hash<T>) = { item = item }

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

hash<T>(x: T): int32 where T: { mutable GetHashCode(): int32 } =
    x.GetHashCode()

main(): () =
    print(hash(123))
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

    let origTmp = Path.GetTempFileName()
    try File.Delete origTmp with | _ -> ()
    let tmp = Path.ChangeExtension(Path.GetTempFileName(), ".cs")
    use _disposable = 
        { new System.IDisposable with
            member _.Dispose() =
                try File.Delete(tmp) with | _ -> ()
        }
    File.WriteAllText(tmp, csSrc)

    let csRef = 
        match dotnetTarget.ImportReferenceAsync(projectPath, targetInfo, OlyPath.Create tmp, System.Threading.CancellationToken.None).Result with
        | Ok(Some csRef) -> OlyProjectReference.Create csRef.CompilationReference
        | Ok(None) -> failwith "None"
        | Error msg -> failwith msg

    let src =
        """
open System

test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

class Example =
  implements IExample

  new() = { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

class Example2 =
  implements IExample2

  new() = { }

  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """

    let proj = getProject src
    let _, proj = proj.Solution.UpdateReferences(proj.Path, proj.References.Add(csRef), System.Threading.CancellationToken.None)
    proj.Compilation.GetILAssembly(System.Threading.CancellationToken.None) |> ignore
    let csDll = Path.Combine(Environment.CurrentDirectory, Path.ChangeExtension(Path.GetFileName(tmp), ".dll"))
    try
        let csMs = dotnetTarget.GetCSharpOutput(OlyPath.Create tmp)
        csMs.Position <- 0L
        let fs = File.Create(csDll)
        fs.Write(csMs.GetBuffer().AsSpan())
        fs.Dispose()
        proj.Compilation
        |> runWithExpectedOutput "testExample"
    finally
        try File.Delete csDll with _ -> ()

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

    let origTmp = Path.GetTempFileName()
    try File.Delete origTmp with | _ -> ()
    let tmp = Path.ChangeExtension(Path.GetTempFileName(), ".cs")
    use _disposable = 
        { new System.IDisposable with
            member _.Dispose() =
                try File.Delete(tmp) with | _ -> ()
        }
    File.WriteAllText(tmp, csSrc)

    let csRef = 
        match dotnetTarget.ImportReferenceAsync(projectPath, targetInfo, OlyPath.Create tmp, System.Threading.CancellationToken.None).Result with
        | Ok(Some csRef) -> OlyProjectReference.Create csRef.CompilationReference
        | Ok(None) -> failwith "None"
        | Error msg -> failwith msg

    let src =
        """
open System

test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

struct Example =
  implements IExample

  new() = { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

struct Example2 =
  implements IExample2

  new() = { }

  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example()
    let t2 = Example2()

    t2.GenericExample<_>(t)
        """

    let proj = getProject src
    let _, proj = proj.Solution.UpdateReferences(proj.Path, proj.References.Add(csRef), System.Threading.CancellationToken.None)
    proj.Compilation.GetILAssembly(System.Threading.CancellationToken.None) |> ignore
    let csDll = Path.Combine(Environment.CurrentDirectory, Path.ChangeExtension(Path.GetFileName(tmp), ".dll"))
    try
        let csMs = dotnetTarget.GetCSharpOutput(OlyPath.Create tmp)
        csMs.Position <- 0L
        let fs = File.Create(csDll)
        fs.Write(csMs.GetBuffer().AsSpan())
        fs.Dispose()
        proj.Compilation
        |> runWithExpectedOutput "testExample"
    finally
        try File.Delete csDll with _ -> ()

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

    let origTmp = Path.GetTempFileName()
    try File.Delete origTmp with | _ -> ()
    let tmp = Path.ChangeExtension(Path.GetTempFileName(), ".cs")
    use _disposable = 
        { new System.IDisposable with
            member _.Dispose() =
                try File.Delete(tmp) with | _ -> ()
        }
    File.WriteAllText(tmp, csSrc)

    let csRef = 
        match dotnetTarget.ImportReferenceAsync(projectPath, targetInfo, OlyPath.Create tmp, System.Threading.CancellationToken.None).Result with
        | Ok(Some csRef) -> OlyProjectReference.Create csRef.CompilationReference
        | Ok(None) -> failwith "None"
        | Error msg -> failwith msg

    let src =
        """
open System

test<T>(x: T): () where T: IExample =
  Console.Write("test")
  x.GenericExample<T>(x)

struct Example<Z> =
  implements IExample

  new() = { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

struct Example2<Z> =
  implements IExample2

  new() = { }

  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)

main(): () =
    let t = Example<__oly_int32>()
    let t2 = Example2<__oly_int32>()

    t2.GenericExample<_>(t)
        """

    let proj = getProject src
    let _, proj = proj.Solution.UpdateReferences(proj.Path, proj.References.Add(csRef), System.Threading.CancellationToken.None)
    proj.Compilation.GetILAssembly(System.Threading.CancellationToken.None) |> ignore
    let csDll = Path.Combine(Environment.CurrentDirectory, Path.ChangeExtension(Path.GetFileName(tmp), ".dll"))
    try
        let csMs = dotnetTarget.GetCSharpOutput(OlyPath.Create tmp)
        csMs.Position <- 0L
        let fs = File.Create(csDll)
        fs.Write(csMs.GetBuffer().AsSpan())
        fs.Dispose()
        proj.Compilation
        |> runWithExpectedOutput "testExample"
    finally
        try File.Delete csDll with _ -> ()

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

    let origTmp = Path.GetTempFileName()
    try File.Delete origTmp with | _ -> ()
    let tmp = Path.ChangeExtension(Path.GetTempFileName(), ".cs")
    use _disposable = 
        { new System.IDisposable with
            member _.Dispose() =
                try File.Delete(tmp) with | _ -> ()
        }
    File.WriteAllText(tmp, csSrc)

    let csRef = 
        match dotnetTarget.ImportReferenceAsync(projectPath, targetInfo, OlyPath.Create tmp, System.Threading.CancellationToken.None).Result with
        | Ok(Some csRef) -> OlyProjectReference.Create csRef.CompilationReference
        | Ok(None) -> failwith "None"
        | Error msg -> failwith msg

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

  new() = { }

  GenericExample<U>(x: U): () = 
      Console.Write("Example")

#[export]
struct Example2<Z> =
  implements IExample2

  new() = { }

  GenericExample<U>(x: U): () where U: IExample = 
      test<_>(x)
        """

    let proj = getProject src
    let _, proj = proj.Solution.UpdateReferences(proj.Path, proj.References.Add(csRef), System.Threading.CancellationToken.None)
    proj.Compilation.GetILAssembly(System.Threading.CancellationToken.None) |> ignore
    let csDll = Path.Combine(Environment.CurrentDirectory, Path.ChangeExtension(Path.GetFileName(tmp), ".dll"))
    try
        let csMs = dotnetTarget.GetCSharpOutput(OlyPath.Create tmp)
        csMs.Position <- 0L
        let fs = File.Create(csDll)
        fs.Write(csMs.GetBuffer().AsSpan())
        fs.Dispose()
        proj.Compilation
        |> runWithExpectedOutput "testExample"
    finally
        try File.Delete csDll with _ -> ()

[<Fact>]
let ``Property test and csharp source``() =
    let csSrc =
        """
public interface IExample
{
    int X { get; }
}
        """

    let origTmp = Path.GetTempFileName()
    try File.Delete origTmp with | _ -> ()
    let tmp = Path.ChangeExtension(Path.GetTempFileName(), ".cs")
    use _disposable = 
        { new System.IDisposable with
            member _.Dispose() =
                try File.Delete(tmp) with | _ -> ()
        }
    File.WriteAllText(tmp, csSrc)

    let csRef = 
        match dotnetTarget.ImportReferenceAsync(projectPath, targetInfo, OlyPath.Create tmp, System.Threading.CancellationToken.None).Result with
        | Ok(Some csRef) -> OlyProjectReference.Create csRef.CompilationReference
        | Ok(None) -> failwith "None"
        | Error msg -> failwith msg

    let src =
        """
open System

class Test =
    implements IExample

    X: Int32 get
    new() = { X = 123 }

main(): () =
    let t = Test()
    Console.Write(t.X)
        """

    let proj = getProject src
    let _, proj = proj.Solution.UpdateReferences(proj.Path, proj.References.Add(csRef), System.Threading.CancellationToken.None)
    proj.Compilation.GetILAssembly(System.Threading.CancellationToken.None) |> ignore
    let csDll = Path.Combine(Environment.CurrentDirectory, Path.ChangeExtension(Path.GetFileName(tmp), ".dll"))
    try
        let csMs = dotnetTarget.GetCSharpOutput(OlyPath.Create tmp)
        csMs.Position <- 0L
        let fs = File.Create(csDll)
        fs.Write(csMs.GetBuffer().AsSpan())
        fs.Dispose()
        proj.Compilation
        |> runWithExpectedOutput "123"
    finally
        try File.Delete csDll with _ -> ()

[<Fact>]
let ``Second order generic on List``() =
    """
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
    |> run "1"

[<Fact>]
let ``Second order generic on ICollection``() =
    """
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
    |> run "1"

[<Fact>]
let ``Second order generic on ICollection 2``() =
    """
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
    |> run "1"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "HelloWorld"

[<Fact>]
let ``Mappable example``() =
    let src =
        """
open System
open System.Collections.Generic
open extension ListMappable<System.Int32>

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

map<T<_>, A, B>(ta: T<A>, f: A -> B): T<B> where T: IMappable<T> =
    T.Map<A, B>(ta, f)

main(): () =
    let xs = List<Int32>()
    xs.Add(1)
    xs.Add(2)
    let xs2 = map(xs, x -> x + 100)
    Console.Write(xs2.get_Item(0))
    Console.Write(xs2.get_Item(1))
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "101102"

[<Fact>]
let ``MemoryStream API that takes an array``() =
    let src =
        """
open System
open System.IO

main(): () =
    let ms = MemoryStream([||])
    Console.Write("test")
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "test"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "Int32"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "Int32"

[<Fact>]
let ``Call ToString on newly defined class``() =
    let src =
        """
class Test

main(): () =
    let x = Test()
    System.Console.Write(x.ToString())
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "__oly_gen_0+Test"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "overriding ToString"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "overriding ToString"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "__oly_gen_0+Test"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "overriding ToString"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "overriding ToString__oly_gen_0+Test"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "1"

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

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "1"

[<Fact>]
let ``Implicit default ctors should output correctly``() =
    let src =
        """
open System

abstract class Test =

   public mutable field X: Int32 =
      let x = 123
      x

class Test2 =
   inherits Test

main(): () =
   let t = Test2()
   Console.Write(t.X)
        """

    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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
(`[]`)<T>(T[||], index: int32): T

#[intrinsic("set_element")]
(`[]`)<T>(T[||], index: int32, T): ()

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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
(`[]`)<T>(T[||], index: int32): T

#[intrinsic("set_element")]
(`[]`)<T>(T[||], index: int32, T): ()

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "hello"

[<Fact>]
let ``get_Item for ReadOnlySpan``() =
    let src =
        """
open System
open System.Collections.Generic

main(): () =
    let xs = [|1;2;3|]
    let mutable xs = ReadOnlySpan(xs)
    Console.Write(xs.get_Item(1))
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "2"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "1"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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
    let proj = getProject src
    let diags = proj.Compilation.GetDiagnostics(System.Threading.CancellationToken.None)
    Assert.Empty(diags)

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

struct StateMachine<A, B> =
    implements IAsyncStateMachine

    public mutable field f: A -> Task<B> = unchecked default

    public mutable field state: B = unchecked default

    public mutable field builder: AsyncTaskMethodBuilder<B> = unchecked default

    public mutable field t: Task<A> = unchecked default

    mutable field u: TaskAwaiter<A> = unchecked default

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>


main(): () =
  let x = AsyncTaskMethodBuilder<__oly_int32>.Create()
  ()
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

[<Fact>]
let ``Indexer operator example with struct with export``() =
    let src =
        """
namespace A

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[export]
struct Test<T> where T: struct =

    get_Item(index: __oly_int32): T = default

struct Test2 =

    public mutable field s: Test<__oly_int32> = default

module Test =
    #[intrinsic("print")]
    print(__oly_object): ()

    (`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

    main(): () =
        let mutable s = Test2()
        let x = s.s[0]
        print(x)
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "0"

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

    #[export]
    private test_private(): () =
        print("test")
        print("_private")

    #[export]
    test(): () = Test.test_private()
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "test_private"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

// Mono.Cecil tests

open Mono.Cecil

[<Fact(Skip = "fix paths")>]
let ``Debug info reading``() =
    let src =
        """
main(): () = ()
        """
    let proj = getProject src
    match dotnetTarget.BuildProjectAsync(proj, System.Threading.CancellationToken.None).Result with
    | Ok(str) ->
        let pars = ReaderParameters()
        pars.ReadSymbols <- true
        pars.ThrowIfSymbolsAreNotMatching <- true
        let mcAsm = AssemblyDefinition.ReadAssembly(str, pars)
        ()
    | Error(str) ->
        failwith str

[<Fact(Skip = "We cannot call 'test' as it is marked as unmanaged callers only, we need a new test. Passes in Release vs Debug - weird .NET behavior.")>]
let ``UnmanagedCallersOnly example``() =
    let src =
        """
namespace A

open System.Runtime.InteropServices
open System.Runtime.CompilerServices

#[open]
module Test =

    #[intrinsic("constant")]
    #[import("intrinsic-CLR", "", "typeof")]
    typeof<T>(): System.Type

    #[UnmanagedCallersOnly(CallConvs = [typeof<CallConvCdecl>()])]
    test(x: __oly_int32): __oly_int32 = x

module Main =

    #[intrinsic("print")]
    print(__oly_object): ()

    main(): () =
        let result = test(1234)
        print(result)
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "1234"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "1234"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "1234"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "overrides"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "overrides"

[<Fact>]
let ``System Convert ToUInt64``() =
    let src =
        """
#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    print(System.Convert.ToUInt64(123))
    """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "1234568"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<0, 0, 0>"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<0, 0, 0>"

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

    #[export]
    Mtd(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd())
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<0, 0, 0>"

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

    #[export]
    Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd<__oly_object>())
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<0, 0, 0>"

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

    #[export]
    abstract default Mtd<U>(): Vector3 = Vector3.One

#[export]
class AClass =
    inherits BClass

    #[export]
    overrides Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd<__oly_object>())
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<0, 0, 0>"

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

    #[export]
    abstract default Mtd<U>(): Vector3 = Vector3.One

#[export]
class AClass =
    inherits BClass<__oly_utf16>

    #[export]
    overrides Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass()
        print(a.Mtd<__oly_object>())
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<0, 0, 0>"

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

    #[export]
    abstract default Mtd<U>(): Vector3 = Vector3.One

#[export]
class AClass<T> =
    inherits BClass

    #[export]
    overrides Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass<__oly_utf16>()
        print(a.Mtd<__oly_object>())
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<0, 0, 0>"

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

    #[export]
    abstract default Mtd<U>(): Vector3 = Vector3.One

#[export]
class AClass<T> =
    inherits BClass<T>

    #[export]
    overrides Mtd<U>(): Vector3 =
        let v1 = Vector3.Zero
        v1 * v1

module Main =
    main() : () =
        let a = AClass<__oly_utf16>()
        print(a.Mtd<__oly_object>())
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<0, 0, 0>"

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

    #[export]
    abstract default Mtd<U>(): Vector3 = Vector3.One * Vector3.One

#[export]
class AClass<T> =
    inherits BClass

module Main =
    main() : () =
        let a = AClass<__oly_utf16>()
        print(a.Mtd<__oly_object>())
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "<1, 1, 1>"

[<Fact(Skip = "Different behavior in Debug and Release - this is a .NET JIT bug.")>]
let ``Weird one``() =
    let src =
        """
namespace Test

#[export]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()
    
    #[intrinsic("int32")]
    alias int
    
    #[intrinsic("utf16")]
    alias string
    
    #[intrinsic("by_ref_read_write")]
    alias byref<T>
    
    #[intrinsic("address_of")]
    (&)<T>(T): byref<T>
    
    #[intrinsic("add")]
    (+)(int, int): int
    
    #[intrinsic("cast")]
    unsafeCast<T>(__oly_object): T

    #[export]
    interface IMoveable =
        get set Position: int
   
    #[export]
    class Item =
        implements IMoveable

        get set Name: string = "Bar"

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

    #[export]
    GetOffset<T>(item: byref<T>): int =
        let item2 = Item()
        item2.Name <- "Bar"
        item <- unsafeCast(item2)
        0

    #[export]
    Shift<T>(mutable item : T): () where T: IMoveable, not struct =
        item.Position <- item.Position + (GetOffset(&item))

    main(): () =
        let item = Item()
        item.Name <- "Goo"
        Shift(item)
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "Get - Goo Set - Goo "

[<Fact(Skip = "breaks in net7")>]
let ``Weird one 2``() =
    let src =
        """
namespace Test

#[export]
module M =
    #[intrinsic("print")]
    print(__oly_object): ()
    
    #[intrinsic("int32")]
    alias int
    
    #[intrinsic("utf16")]
    alias string
    
    #[intrinsic("by_ref_read_write")]
    alias byref<T>
    
    #[intrinsic("address_of")]
    (&)<T>(T): byref<T>
    
    #[intrinsic("add")]
    (+)(int, int): int
    
    #[intrinsic("cast")]
    unsafeCast<T>(__oly_object): T

    #[export]
    interface IMoveable =
        get set Position: int
   
    #[export]
    struct Item =
        implements IMoveable

        get set Name: string = "Bar"

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

    #[export]
    GetOffset<T>(item: byref<T>): int =
        let mutable item2 = Item()
        item2.Name <- "Bar"
        item <- unsafeCast(item2)
        0

    #[export]
    Shift<T>(mutable item : T): () where T: IMoveable, struct =
        item.Position <- item.Position + (GetOffset(&item))

    main(): () =
        let mutable item = Item()
        item.Name <- "Goo"
        Shift(item)
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "Get - Goo Set - Bar "

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

#[intrinsic("by_ref_read_write")]
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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "test"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "test"


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
    let result = test()
    print("should not happen")
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedExceptionMessage "a message for throw"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput ""

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

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "246"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "246"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "246"

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
    let proj = getProjectWithReferenceProject refSrc src
    proj.Compilation
    |> runWithExpectedOutput "246"

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
    let proj = getProjectWithReferenceProject refSrc src
    proj.Compilation
    |> runWithExpectedOutput "149"

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
    |> shouldCompile
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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "4"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "4"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "4"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "65535"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "printing the finally"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "printing the finally"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "an exception"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "48"

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
    let mutable xs = ReadOnlySpan([|TestData()|])
    print(xs.get_Item(0).X)
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123456789"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "A"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "A"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "passed"

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
#[UnmanagedCallersOnly(CallConvs = [typeof<CallConvCdecl>])]
test(): () =
    ()

main(): () =
    print("passed")
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "passed"


/// TODO: We should instead compile the actual prelude file instead of embedding this here.
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

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
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
(`[]`)<T>(T[||], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(T[||], index: int32, T): ()
#[intrinsic("get_element")]
(`[,]`)<T>(T[|,|], index1: int32, index2: int32): T
#[intrinsic("set_element")]
(`[,]`)<T>(T[|,|], index1: int32, index2: int32, T): ()

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
private getLength<T>(T[||]): int32

#[open]
extension MutableArrayExtensions<T> =
    inherits T[||]

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
    static ZeroCreate<T>(size: int32): T[||]

    static Initialize<T>(size: int32, f: int32 -> T): T[||] =
        let newArr = System.Array.ZeroCreate<T>(size)
        let mutable i = 0
        while (i < newArr.Length)
            newArr[i] <- f(i)
            i <- i + 1
        newArr

    static Map<T, U>(arr: T[||], f: T -> U): U[||] =
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

    static MapAsMutable<T, U>(arr: T[], f: T -> U): U[||] =
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

    static Flatten<T, U>(arr: T[||], f: T -> (U, U, U)): U[||] =
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
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = 
    x.get_Item(key)
#[inline]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = 
    x.get_Item(key)
#[inline]
(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = 
    x.get_Item(key)
#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = 
    x.set_Item(key, value)
#[inline]
(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = 
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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "passed"

[<Fact>]
let ``Generic class that handles functions that have ambiguity - Exported``() =
    let src =
        """
namespace TestNamespace

open System

#[export]
abstract class A<T> =

    #[export]
    abstract default Test(x: T): () =
        Program.print("Test_T_")

    #[export]
    abstract default Test(x: Int32): () =
        Program.print("Test_int32_")

#[export]
class Test =
    inherits A<String>

#[export]
class Test2 =
    inherits A<Int32>

    #[export]
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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "Test_T_Test_int32_123"

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
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "123"

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

test<T>(xs: T): () where T: IEnumerable =
    let x = xs.GetEnumerator()

main(): () =
    let xs = [1]
    test(xs)
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "passed"

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

test<T>(xs: T): () where T: IEnumerable<int32> =
    let x = xs.GetEnumerator()

main(): () =
    let xs = [1]
    test(xs)
        """
    let proj = getProject src
    proj.Compilation
    |> runWithExpectedOutput "passed"
