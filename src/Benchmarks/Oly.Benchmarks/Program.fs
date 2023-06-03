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
type ParsingText() =

    let vulkanText =
        OlySourceText.FromFile("../../../../../../../../../../examples/Evergreen/src/Graphics/Backend/Vulkan.oly")

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
    member _.Run() =
        for _ = 1 to 10 do
            test vulkanText

    [<IterationCleanup>]
    member _.Cleanup() = ()

module Program =

    [<EntryPoint>]
    let main(argv: string[]): int =
        BenchmarkRunner.Run<ParsingText>() |> ignore
        100