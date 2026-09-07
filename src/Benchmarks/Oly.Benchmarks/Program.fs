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
open Oly.Targets.DotNet
open Oly.Runtime.CodeGen

[<MemoryDiagnoser>]
type DotNetCompile() =

    [<GlobalSetup>]
    member _.Setup() = 
        ()

    [<Benchmark>]
    member _.Benchmark() =
        // TODO: Actually implement a good benchmark.
        ()

    [<IterationCleanup>]
    member _.Cleanup() = 
        ()

module Program =

    [<EntryPoint>]
    let main(argv: string[]): int =
        BenchmarkRunner.Run<DotNetCompile>() |> ignore
        100