namespace Oly.Benchmarks

open System
open System.IO
open System.Text
open System.Threading.Tasks
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Runtime.Target.DotNet
open Oly.Runtime.Clr
open Oly.Runtime.CodeGen

[<MemoryDiagnoser>]
type DotNetCompile() =

    let benchmarkPath = OlyPath.Create("benchmark.olyx")
    let workspace = OlyWorkspace.Create([DotNetTarget(true)])
    let rs = OlyWorkspaceResourceState.Create()
    let rs = rs.SetResource(benchmarkPath, new MemoryStream(File.ReadAllBytes(benchmarkPath.ToString())), DateTime.UtcNow)
    do
        workspace.UpdateDocument(rs, benchmarkPath, OlySourceText.FromFile(benchmarkPath.ToString()), System.Threading.CancellationToken.None)

    let text =
        OlySourceText.FromFile("""C:\work\Evergreen\src\managed\Collections\EntityDatabase.oly""")

    let test (text: IOlySourceText) =
        let root = OlySyntaxTree.Parse(OlyPath.Create("benchmark"), text).GetRoot(Unchecked.defaultof<_>)
        if root.Tree.GetDiagnostics(System.Threading.CancellationToken.None).IsEmpty |> not then
            failwith "Errors"
        let rec loop (node: OlySyntaxNode) =
            node.Children
            |> ImArray.iter loop
        loop root
        root.FullTextSpan
        |> ignore

    [<GlobalSetup>]
    member _.Setup() = ()

    [<Benchmark>]
    member _.Benchmark() =
        test text
        //let task = workspace.BuildProjectAsync(benchmarkPath, System.Threading.CancellationToken.None)
        //let result = task.Result
        //match result with
        //| Ok _ -> ()
        //| Error(diags) -> failwithf "Errors %A" diags
        //for _ = 1 to 10 do
        //    test vulkanText

    [<IterationCleanup>]
    member _.Cleanup() = ()

module Program =

    [<EntryPoint>]
    let main(argv: string[]): int =
        BenchmarkRunner.Run<DotNetCompile>() |> ignore
        100