module TestPlatform

open System.IO
open System.Text
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.Target.Spirv.Emitter
open Oly.Core
open Xunit
open Spirv
open Spirv.SpirvModule
open Spirv.TestHelpers

let globalSetup() =
    ()

let createEmitter(asm: OlyILAssembly) =
    SpirvEmitter(1u, 0u, ExecutionModel.Vertex) // TODO: What to do about fragment, compute, etc.?

let configureRuntime(vm: OlyRuntime<SpirvType, SpirvFunction, SpirvField>) =
    ()

let emitterWrite(emitter: SpirvEmitter, isDebuggable) =
    emitter.EmitOutput(isDebuggable)

let run (spvOutput: SpirvOutput, expectedOutput: string) : unit =
    raise(System.NotImplementedException())