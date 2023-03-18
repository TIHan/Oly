namespace Oly.Benchmarks

open System
open System.IO
open System.Text
open System.Threading.Tasks
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax

[<MemoryDiagnoser>]
type ParsingLargeText() =

    [<GlobalSetup>]
    member _.Setup() = ()

    [<Benchmark>]
    member _.Run() =
        let root = OlySyntaxTree.Parse(OlyPath.Create("benchmark"), LargeSyntaxExample.Text).GetRoot(Unchecked.defaultof<_>)
        if root.Tree.GetDiagnostics(System.Threading.CancellationToken.None).IsEmpty |> not then
            failwith "Errors"
        let rec loop (node: OlySyntaxNode) =
            node.Children
            |> ImArray.iter loop
        loop root
        root.FullTextSpan
        |> ignore

    [<IterationCleanup>]
    member _.Cleanup() = ()

module Program =

    [<EntryPoint>]
    let main(argv: string[]): int =
        // System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
        BenchmarkRunner.Run<ParsingLargeText>() |> ignore
        100