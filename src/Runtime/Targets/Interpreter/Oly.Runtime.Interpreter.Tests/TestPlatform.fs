module TestPlatform

open System.IO
open System.Text
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.Interpreter
open Oly.Core
open Xunit

let globalSetup() =
    ()

let createEmitter(asm: OlyILAssembly) =
    InterpreterRuntimeEmitter()

let configureRuntime(vm: OlyRuntime<InterpreterType, InterpreterFunction, InterpreterField>) =
    ()

let emitterWrite(emitter: InterpreterRuntimeEmitter, _isDebuggable) =
    emitter.Run(ImArray.empty)
    let output = emitter.StandardOut
    let ms = new MemoryStream()
    let s = new StreamWriter(ms, leaveOpen = true)
    s.Write(output)
    s.Flush()
    s.Dispose()
    ms.Position <- 0L
    ms

let run (ms: MemoryStream, expectedOutput: string) : unit =
    use s = new StreamReader(ms)
    let actualOutput = s.ReadToEnd()
    Assert.Equal(expectedOutput, actualOutput)