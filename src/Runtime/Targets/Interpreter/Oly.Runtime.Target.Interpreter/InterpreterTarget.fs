namespace Oly.Runtime.Target.Interpreter

open System
open System.IO
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Runtime.Interpreter
open Oly.Runtime.Target.Core

[<Sealed>]
type InterpreterTarget() =
    inherit OlyTargetOutputOnly<InterpreterRuntimeEmitter, InterpreterType, InterpreterFunction, InterpreterField>("interpreter")

    let mutable emitterOpt: InterpreterRuntimeEmitter option = None

    member this.Run(args) =
        match emitterOpt with
        | Some emitter -> 
            emitterOpt <- None
            emitter.Run(args)
        | _ ->
            failwith "No successful build found"

    override this.CreateEmitter() = InterpreterRuntimeEmitter(Console.Out)

    override this.EmitOutput(_, _, emitter: InterpreterRuntimeEmitter, _) = emitter.Run(ImArray.empty)

    override this.IsValidTargetName targetInfo = targetInfo.Name = "default"
