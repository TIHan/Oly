module DotNet.Conformance.WorkspaceTests

open Xunit
open System
open System.Threading
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Workspace
open Oly.Targets.DotNet

let rs = 
    let rs = OlyWorkspaceResourceSnapshot.Create(OlyPath.Empty)
    let fileInfo = System.IO.FileInfo("prelude.oly")
    let rs = rs.SetResourceAsCopy(OlyPath.Create(fileInfo.FullName), new System.IO.MemoryStream(System.IO.File.ReadAllBytes(fileInfo.FullName)))
    let fileInfo = System.IO.FileInfo("prelude_dotnet.olyx")
    let rs = rs.SetResourceAsCopy(OlyPath.Create(fileInfo.FullName), new System.IO.MemoryStream(System.IO.File.ReadAllBytes(fileInfo.FullName)))
    let fileInfo = System.IO.FileInfo("numerics_dotnet.oly")
    rs.SetResourceAsCopy(OlyPath.Create(fileInfo.FullName), new System.IO.MemoryStream(System.IO.File.ReadAllBytes(fileInfo.FullName)))

let createWorkspace() =
    OlyWorkspace.Create(([DotNetTarget()]: OlyBuild seq), OlyPath.Empty, rs)

let createProject src (workspace: OlyWorkspace) =
    let path = OlyPath.Create "olytest.olyx"
    workspace.UpdateDocument(path, OlySourceText.Create(src), CancellationToken.None)
    (workspace, workspace.GetDocumentsAsync(path, CancellationToken.None).Result[0].Project)

let shouldCompile (proj: OlyProject) =
    let diags = proj.Compilation.GetDiagnostics(CancellationToken.None)
    Assert.Empty(diags)

let build (workspace: OlyWorkspace, proj: OlyProject) =
    let result = workspace.BuildProjectAsync(proj.Path, CancellationToken.None).Result
    match result with
    | Ok(program) -> program
    | Error(diags) ->
        let builder = System.Text.StringBuilder()
        diags
        |> ImArray.iter (fun diag -> builder.AppendLine(diag.ToString()) |> ignore)
        failwith (builder.ToString())

let shouldHaveBuildError (expectedOutput: string) (workspace: OlyWorkspace, proj: OlyProject) =
    let result = workspace.BuildProjectAsync(proj.Path, CancellationToken.None).Result
    match result with
    | Ok(_) -> failwith "Expected build error."
    | Error(diags) ->
        OlyAssert.False(diags.IsEmpty)
        let builder = System.Text.StringBuilder()
        diags
        |> ImArray.iteri (fun i diag ->
            if i = diags.Length - 1 then
                builder.Append(diag.ToString()) |> ignore
            else
                builder.AppendLine(diag.ToString()) |> ignore
        ) 
        Assert.Equal(expectedOutput.ReplaceLineEndings("\n"), builder.ToString().ReplaceLineEndings("\n"))

let run (expectedOutput: string) (program: OlyProgram) =
    Assert.Equal(expectedOutput.ReplaceLineEndings("\n"), program.Run([||]).ReplaceLineEndings("\n"))

[<Fact>]
let ``Simple hello world project should compile`` () =
    let src =
        """
#target "dotnet: net8"

main(): () =
    print("Hello World!")
        """
    createWorkspace()
    |> createProject src
    |> build
    |> run "Hello World!\n"

[<Fact>]
let ``Simple ReadyToRun hello world project should compile`` () =
    let src =
        """
#target "dotnet: net8"

#property "r2r" true

main(): () =
    print("Hello World!")
        """
    createWorkspace()
    |> createProject src
    |> build
    |> run "Hello World!\n"


[<Fact>]
let ``Simple NativeAOT hello world project should compile`` () =
    let src =
        """
#target "dotnet: net8_aot"

main(): () =
    print("Hello World!")
        """
    createWorkspace()
    |> createProject src
    |> build
    |> run "Hello World!\n"

[<Fact>]
let ``Recursive generics should give a build error``() =
    let src = """
#target "dotnet: net8"

class B<T>

class D<T>

class E

class A =

    M<T>(): () =
        this.M2<D<T>>()

    M2<U>(): () =
        this.M<B<U>>()

main(): () =
    A().M<E>()
    """
    createWorkspace()
    |> createProject src
    |> shouldHaveBuildError """olytest.olyx(13,9): error OLY9999: Generic recursion limit reached: D<B<D<B<D<B<D<B<D<B<D<B<D<B<D<B<D<E>>>>>>>>>>>>>>>>>
        this.M2<D<T>>()
        ^^^^^^^^^^^^^^^"""
