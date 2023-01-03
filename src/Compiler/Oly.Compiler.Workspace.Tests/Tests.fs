module Tests

open System
open System.Threading
open Xunit
open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Compiler.Workspace.Extensions

let createWorkspace() =
    OlyWorkspace.Create([Oly.Runtime.Target.Interpreter.InterpreterTarget()])

let createWorkspaceWith(f) =
    let rs =
        {
            new IOlyWorkspaceResourceService with
        
                member _.LoadSourceText(filePath) =
                    f filePath
        
                member _.GetTimeStamp(filePath) = DateTime()
        
                member _.FindSubPaths(dirPath) =
                    ImArray.empty
        
                member _.LoadProjectConfigurationAsync(_projectFilePath: OlyPath, ct: CancellationToken) =
                    backgroundTask {
                        ct.ThrowIfCancellationRequested()
                        return OlyProjectConfiguration(String.Empty, ImArray.empty, false)
                    }
        }
    OlyWorkspace.Create([Oly.Runtime.Target.Interpreter.InterpreterTarget()], rs)

let createProject src (workspace: OlyWorkspace) =
    let path = OlyPath.Create "olytest.olyx"
    workspace.UpdateDocument(path, OlySourceText.Create(src), CancellationToken.None)
    workspace.GetDocumentsAsync(path, CancellationToken.None).Result[0].Project  

[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let createProjectWeakReference src workspace =
    let proj = createProject src workspace
    let comp = proj.Compilation
    let syntaxTree = proj.Documents[0].SyntaxTree
    WeakReference<OlyProject>(proj), WeakReference<OlyCompilation>(comp), WeakReference<OlySyntaxTree>(syntaxTree)

let createDocument src (workspace: OlyWorkspace) =
#if DEBUG
    let isDebuggable = true
#else
    let isDebuggable = false
#endif
    let projOptions = OlyProjectConfiguration("olytest", ImArray.empty, isDebuggable)
    let sol, proj = workspace.Solution.CreateProject(OlyPath.Create "olytest", projOptions, "dotnet", OlyTargetInfo("net7", OlyOutputKind.Executable), CancellationToken.None)
    let syntaxTree = OlySyntaxTree.Parse(OlyPath.Create "olytest", (fun _ -> OlySourceText.Create(src)))
    let sol, proj, doc = sol.UpdateDocument(proj.Path, OlyPath.Create "olytest", syntaxTree, ImArray.empty)
    doc

let updateDocument path src (workspace: OlyWorkspace) =
    workspace.UpdateDocumentAsync(path, OlySourceText.Create(src), CancellationToken.None).Result
    |> ignore

let shouldCompile (proj: OlyProject) =
    let diags = proj.Compilation.GetDiagnostics(CancellationToken.None)
    Assert.Empty(diags)

let getCompletionLabels (srcWithCursor: string) =
    let cursorPosition = srcWithCursor.IndexOf("~^~")
    let src = srcWithCursor.Replace("~^~", "")
    let doc =
        createWorkspace()
        |> createDocument src

    let completionLabels = 
        doc.GetCompletions(cursorPosition, CancellationToken.None)
        |> Seq.groupBy (fun x -> x.Label)
        |> Seq.map (fun (label, xs) -> System.Collections.Generic.KeyValuePair(label, xs))
       
    let completionLabels = 
        System.Collections.Generic.Dictionary(completionLabels)
        |> System.Collections.ObjectModel.ReadOnlyDictionary
        :> System.Collections.Generic.IReadOnlyDictionary<_, _>
    completionLabels

let containsCompletionLabelsByCursor (expectedCompletionLabels: string seq) (srcWithCursor: string) =
    let completionLabels = getCompletionLabels srcWithCursor
    expectedCompletionLabels
    |> Seq.iter (fun expected ->
        Assert.Contains(expected, completionLabels)
        |> ignore
    )

let doesNotContainCompletionLabelsByCursor (expectedCompletionLabels: string seq) (srcWithCursor: string) =
    let completionLabels = getCompletionLabels srcWithCursor
    expectedCompletionLabels
    |> Seq.iter (fun expected ->
        Assert.DoesNotContain(expected, completionLabels)
        |> ignore
    )

[<Fact>]
let ``Simple workspace with hello world project should compile`` () =
    let src =
        """
#target "i: default"

module Test

#[intrinsic("print")]
print(__oly_object): ()

main(): () =
    print("Hello World!")
        """
    createWorkspace()
    |> createProject src
    |> shouldCompile

[<Fact>]
let ``By cursor, get completions of local variable 'x'`` () =
    """
#target "i: default"

module Test

main(): () =
    let x = 1
    ~^~
    """
    |> containsCompletionLabelsByCursor ["x"]

[<Fact>]
let ``By cursor, get completions of local variable 'x' 2`` () =
    """
#target "i: default"

module Test

main(): () =
    let x = 1
    let y = 1
    ~^~
    """
    |> containsCompletionLabelsByCursor ["x"]

[<Fact>]
let ``By cursor, get completions of local variable 'x' 3`` () =
    """
#target "i: default"

module Test

main(): () =
    let x = 1
    ~^~
    let y = 1
    """
    |> containsCompletionLabelsByCursor ["x"]

[<Fact>]
let ``By cursor, get completions of local variable 'x' 4`` () =
    """
#target "i: default"

module Test

main(): () =
    let x = 1
    ~^~
    ()
    """
    |> containsCompletionLabelsByCursor ["x"]

[<Fact>]
let ``By cursor, get completions of local variable 'x' 5`` () =
    """
#target "i: default"

module Test

main(): () =
    let x = 1
~^~
    """
    |> doesNotContainCompletionLabelsByCursor ["x"]

[<Fact>]
let ``By cursor, get completions of local variable 'x' 6`` () =
    """
#target "i: default"

module Test

main(): () =
    let x = 1
 ~^~
    """
    |> doesNotContainCompletionLabelsByCursor ["x"]

[<Fact>]
let ``By cursor, get completions of local variable 'x' 7`` () =
    """
#target "i: default"

module Test

main(): () =
    let x = 1
  ~^~
    """
    |> doesNotContainCompletionLabelsByCursor ["x"]

[<Fact>]
let ``By cursor, get completions of local variable 'x' 8`` () =
    """
#target "i: default"

module Test

main(): () =
    let x = 1
   ~^~
    """
    |> doesNotContainCompletionLabelsByCursor ["x"]

[<Fact>]
let ``By cursor, get completions from a local variable`` () =
    """
#target "i: default"

struct Test =
    
    mutable X: __oly_int32 = 3

main(): () =
    let t = Test()
    t.~^~
    """
    |> containsCompletionLabelsByCursor ["X"]

[<Fact>]
let ``By cursor, get completions from a namespace`` () =
    """
namespace TestNamespace

#target "i: default"

struct Test =
    
    mutable X: __oly_int32 = 3

main(): () =
    let t = TestNamespace.~^~
    """
    |> containsCompletionLabelsByCursor ["Test"]

[<Fact>]
let ``By cursor, get completions from a cast`` () =
    """
#target "i: default"

#[intrinsic("print")]
print(__oly_object): ()

interface IA =

    B(): ()

    default A(): () =
        this.B()

struct Test =
    implements IA
    
    mutable X: __oly_int32 = 3

    mutable B(): () =
        this.X <- 5

main(): () =
    let x = Test()
    (x: IA).~^~
    print(x.X)
    """
    |> containsCompletionLabelsByCursor ["A";"B"]

let clearSolution (workspace: OlyWorkspace) =
    workspace.ClearSolutionAsync(CancellationToken.None).Result

let mutable workspaceGC_stub = Unchecked.defaultof<OlyWorkspace>

[<Fact>]
let ``Project should be GC'ed``() =
    let src =
        """
#target "i: default"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
    
main(): () =
    print("Hello World!")
        """
    let workspace = createWorkspace()
    workspaceGC_stub <- workspace
    let projWeak, compWeak, treeWeak = createProjectWeakReference src workspace

    let mutable proj = Unchecked.defaultof<_>
    let mutable comp = Unchecked.defaultof<_>
    let mutable tree = Unchecked.defaultof<_>

    for _i = 1 to 10 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()

    Assert.True(projWeak.TryGetTarget(&proj))
    Assert.True(compWeak.TryGetTarget(&comp))
    Assert.True(treeWeak.TryGetTarget(&tree))

    proj <- Unchecked.defaultof<_>
    comp <- Unchecked.defaultof<_>
    tree <- Unchecked.defaultof<_>

    clearSolution workspace

    for _i = 1 to 10 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()

    Assert.False(projWeak.TryGetTarget(&proj))
    Assert.False(compWeak.TryGetTarget(&comp))
    Assert.False(treeWeak.TryGetTarget(&tree))

    workspaceGC_stub <- Unchecked.defaultof<_>

let clearSolution2 (path: OlyPath) (workspace: OlyWorkspace) =
    let mutable currentDocs = workspace.GetDocumentsAsync(path, CancellationToken.None).Result
    Assert.True(currentDocs.Length = 1)
    workspace.ClearSolutionAsync(CancellationToken.None).Result
    currentDocs <- workspace.GetDocumentsAsync(path, CancellationToken.None).Result
    Assert.True(currentDocs.Length = 0)

[<Fact>]
let ``Project should be GC'ed 2``() =
    let src =
        """
#target "i: default"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
    
main(): () =
    print("Hello World!")
        """
    let workspace = createWorkspace()
    workspaceGC_stub <- workspace
    let projWeak, compWeak, treeWeak = createProjectWeakReference src workspace

    let mutable proj = Unchecked.defaultof<_>
    let mutable comp = Unchecked.defaultof<_>
    let mutable tree = Unchecked.defaultof<_>

    for _i = 1 to 10 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()

    Assert.True(projWeak.TryGetTarget(&proj))
    Assert.True(compWeak.TryGetTarget(&comp))
    Assert.True(treeWeak.TryGetTarget(&tree))

    let path = proj.Path

    proj <- Unchecked.defaultof<_>
    comp <- Unchecked.defaultof<_>
    tree <- Unchecked.defaultof<_>

    clearSolution2 path workspace

    for _i = 1 to 10 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()

    Assert.False(projWeak.TryGetTarget(&proj))
    Assert.False(compWeak.TryGetTarget(&comp))
    Assert.False(treeWeak.TryGetTarget(&tree))

    workspaceGC_stub <- Unchecked.defaultof<_>

[<Fact>]
let ``Project should be GC'ed 3``() =
    let src =
        """
#target "i: default"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
    
main(): () =
    print("Hello World!")
        """
    let workspace = createWorkspace()
    workspaceGC_stub <- workspace
    let projWeak, compWeak, treeWeak = createProjectWeakReference src workspace

    let mutable proj = Unchecked.defaultof<_>
    let mutable comp = Unchecked.defaultof<_>
    let mutable tree = Unchecked.defaultof<_>

    for _i = 1 to 10 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()

    Assert.True(projWeak.TryGetTarget(&proj))
    Assert.True(compWeak.TryGetTarget(&comp))
    Assert.True(treeWeak.TryGetTarget(&tree))

    let path = proj.Path

    proj <- Unchecked.defaultof<_>
    comp <- Unchecked.defaultof<_>
    tree <- Unchecked.defaultof<_>

    updateDocument path (src + " ") workspace

    for _i = 1 to 10 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()

    Assert.False(projWeak.TryGetTarget(&proj))
    Assert.False(compWeak.TryGetTarget(&comp))
    Assert.False(treeWeak.TryGetTarget(&tree))

    workspaceGC_stub <- Unchecked.defaultof<_>

[<Fact>]
let ``Project should not be GC'ed as the source text is the same``() =
    let src =
        """
#target "i: default"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
    
main(): () =
    print("Hello World!")
        """
    let workspace = createWorkspace()
    workspaceGC_stub <- workspace
    let projWeak, compWeak, treeWeak = createProjectWeakReference src workspace

    let mutable proj = Unchecked.defaultof<_>
    let mutable comp = Unchecked.defaultof<_>
    let mutable tree = Unchecked.defaultof<_>

    for _i = 1 to 10 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()

    Assert.True(projWeak.TryGetTarget(&proj), "proj - before")
    Assert.True(compWeak.TryGetTarget(&comp), "comp - before")
    Assert.True(treeWeak.TryGetTarget(&tree), "tree - before")

    let path = proj.Path

    proj <- Unchecked.defaultof<_>
    comp <- Unchecked.defaultof<_>
    tree <- Unchecked.defaultof<_>

    updateDocument path src workspace

    for _i = 1 to 10 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()

    Assert.True(projWeak.TryGetTarget(&proj), "proj - after")
    Assert.True(compWeak.TryGetTarget(&comp), "comp - after")
    Assert.True(treeWeak.TryGetTarget(&tree), "tree - after")

    workspaceGC_stub <- Unchecked.defaultof<_>

[<Fact>]
let ``Project should work with multiple dots in the name``() =
    let src =
        """
#target "i: default"

#load "../fakepath/*.oly"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
    
main(): () =
    print("Hello World!")
        """
    let workspace = createWorkspace()
    let path = OlyPath.Create("Oly.Runtime.Target.Example.olyx")
    workspace.UpdateDocument(path, OlySourceText.Create(src), CancellationToken.None)
    let proj = workspace.GetDocumentsAsync(path, CancellationToken.None).Result[0].Project
    Assert.Equal("Oly.Runtime.Target.Example", proj.Name)
    Assert.Equal("Oly.Runtime.Target.Example", proj.Compilation.AssemblyName)
    let doc = proj.Documents[0]
    let symbols = doc.GetAllSymbols(CancellationToken.None)
    Assert.NotEqual(0, symbols.Length)

[<Fact>]
let ``Project should fail when trying to reference a Oly file``() =
    let src =
        """
#target "i: default"

#reference "fake.oly"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
    
main(): () =
    print("Hello World!")
        """
    let workspace = createWorkspace()
    let path = OlyPath.Create("Test.olyx")
    workspace.UpdateDocument(path, OlySourceText.Create(src), CancellationToken.None)
    let proj = workspace.GetDocumentsAsync(path, CancellationToken.None).Result[0].Project
    
    let diags = proj.Compilation.GetDiagnostics(CancellationToken.None)
    Assert.Equal(1, diags.Length)
    Assert.Equal("Cannot reference Oly file(s) 'fake.oly'. Use '#load' instead.", diags[0].Message)

[<Fact>]
let ``Project should fail when trying to reference a Oly file 2``() =
    let src =
        """
#target "i: default"

#reference "*.oly"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
    
main(): () =
    print("Hello World!")
        """
    let workspace = createWorkspace()
    let path = OlyPath.Create("Test.olyx")
    workspace.UpdateDocument(path, OlySourceText.Create(src), CancellationToken.None)
    let proj = workspace.GetDocumentsAsync(path, CancellationToken.None).Result[0].Project
    
    let diags = proj.Compilation.GetDiagnostics(CancellationToken.None)
    Assert.Equal(1, diags.Length)
    Assert.Equal("Cannot reference Oly file(s) '*.oly'. Use '#load' instead.", diags[0].Message)

[<Fact>]
let ``Project reference another project``() =
    let src1 =
        """
#target "i: default"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
        """

    let src2 =
        """
#target "i: default"

#reference "fakepath/Test.olyx"

open static Test

main(): () =
    print("Hello World!")
        """

    let path1 = OlyPath.Create("fakePath/Test.olyx")
    let path2 = OlyPath.Create("main.olyx")
    let text1 = OlySourceText.Create(src1)
    let text2 = OlySourceText.Create(src2)
    let workspace = createWorkspaceWith(fun x -> if x = path1 then text1 else failwith "Invalid path")
    workspace.UpdateDocument(path2, text2, CancellationToken.None)
    let proj = workspace.GetDocumentsAsync(path2, CancellationToken.None).Result[0].Project
    let doc = proj.Documents[0]
    let symbols = doc.GetAllSymbols(CancellationToken.None)
    Assert.Empty(proj.Compilation.GetDiagnostics(CancellationToken.None))
    Assert.NotEqual(0, symbols.Length)

[<Fact>]
let ``Project reference another project 2``() =
    let src1 =
        """
#target "i: default"

module Test
    
#[intrinsic("print")]
print(__oly_object): ()
        """

    let src2 =
        """
#target "i: default"

#reference "fakepath/Test.olyx"

main(): () =
    Test.print("Hello World!")
        """

    let path1 = OlyPath.Create("fakePath/Test.olyx")
    let path2 = OlyPath.Create("main.olyx")
    let text1 = OlySourceText.Create(src1)
    let text2 = OlySourceText.Create(src2)
    let workspace = createWorkspaceWith(fun x -> if x = path1 then text1 else failwith "Invalid path")
    workspace.UpdateDocument(path2, text2, CancellationToken.None)
    let proj = workspace.GetDocumentsAsync(path2, CancellationToken.None).Result[0].Project
    let doc = proj.Documents[0]
    let symbols = doc.GetAllSymbols(CancellationToken.None)
    Assert.Empty(proj.Compilation.GetDiagnostics(CancellationToken.None))
    Assert.NotEqual(0, symbols.Length)

[<Fact>]
let ``Project reference another project 3``() =
    let src1 =
        """
#target "i: default"

#[open]
module Test
    
#[intrinsic("print")]
print(__oly_object): ()
        """

    let src2 =
        """
#target "i: default"

#reference "fakepath/Test.olyx"

main(): () =
    print("Hello World!")
        """

    let path1 = OlyPath.Create("fakePath/Test.olyx")
    let path2 = OlyPath.Create("main.olyx")
    let text1 = OlySourceText.Create(src1)
    let text2 = OlySourceText.Create(src2)
    let workspace = createWorkspaceWith(fun x -> if x = path1 then text1 else failwith "Invalid path")
    workspace.UpdateDocument(path2, text2, CancellationToken.None)
    let proj = workspace.GetDocumentsAsync(path2, CancellationToken.None).Result[0].Project
    let doc = proj.Documents[0]
    let symbols = doc.GetAllSymbols(CancellationToken.None)
    Assert.Empty(proj.Compilation.GetDiagnostics(CancellationToken.None))
    Assert.NotEqual(0, symbols.Length)
