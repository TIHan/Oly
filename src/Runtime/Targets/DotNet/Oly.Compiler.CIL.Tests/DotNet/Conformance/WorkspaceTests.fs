module DotNet.Conformance.WorkspaceTests

open Xunit
open System
open System.Threading
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Workspace
open Oly.Runtime.Target.DotNet

let rs = 
    let rs = OlyWorkspaceResourceSnapshot.Create(OlyPath.Empty)
    let fileInfo = System.IO.FileInfo("prelude.oly")
    let rs = rs.SetResourceAsCopy(OlyPath.Create(fileInfo.FullName), new System.IO.MemoryStream(System.IO.File.ReadAllBytes(fileInfo.FullName)))
    let fileInfo = System.IO.FileInfo("prelude_dotnet.olyx")
    let rs = rs.SetResourceAsCopy(OlyPath.Create(fileInfo.FullName), new System.IO.MemoryStream(System.IO.File.ReadAllBytes(fileInfo.FullName)))
    let fileInfo = System.IO.FileInfo("numerics_dotnet.oly")
    rs.SetResourceAsCopy(OlyPath.Create(fileInfo.FullName), new System.IO.MemoryStream(System.IO.File.ReadAllBytes(fileInfo.FullName)))

let updateText (path: OlyPath) (src: string) (rs: OlyWorkspaceResourceSnapshot) =
    let ms = new System.IO.MemoryStream()
    ms.Write(System.Text.Encoding.Default.GetBytes(src))
    ms.Position <- 0
    let result = rs.SetResourceAsCopy(path, ms)
    ms.Dispose()
    result

let createWorkspace() =
    OlyWorkspace.Create([DotNetTarget()])

let createProject src (workspace: OlyWorkspace) =
    let path = OlyPath.Create "olytest.olyx"
    (workspace, workspace.UpdateDocumentAsync(rs, path, OlySourceText.Create(src), CancellationToken.None).Result[0].Project)

let shouldCompile (proj: OlyProject) =
    let diags = proj.Compilation.GetDiagnostics(CancellationToken.None)
    Assert.Empty(diags)

let build (workspace: OlyWorkspace, proj: OlyProject) =
    let result = workspace.BuildProjectAsync(rs, proj.Path, CancellationToken.None).Result
    match result with
    | Ok(program) -> program
    | Error(diags) ->
        let builder = System.Text.StringBuilder()
        diags
        |> ImArray.iter (fun diag -> builder.AppendLine(diag.ToString()) |> ignore)
        failwith (builder.ToString())


let run (expectedOutput: string) (program: OlyProgram) =
    Assert.Equal(expectedOutput + Environment.NewLine, program.Run([||]))

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
    |> run "Hello World!"

[<Fact>]
let ``Simple ReadyToRun hello world project should compile`` () =
    let src =
        """
#target "dotnet: net8_r2r"

main(): () =
    print("Hello World!")
        """
    createWorkspace()
    |> createProject src
    |> build
    |> run "Hello World!"


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
    |> run "Hello World!"
