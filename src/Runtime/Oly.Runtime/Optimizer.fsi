[<AutoOpen>]
module internal Oly.Runtime.CodeGen.Internal.Optimizer

open Oly.Core
open Oly.Metadata
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns

val OptimizeFunctionBody<'Type, 'Function, 'Field> :
    tryGetFunctionBody: (RuntimeFunction -> OlyIRFunctionBody<'Type, 'Function, 'Field> option) ->
    emitFunction: ((RuntimeFunction * RuntimeFunction) -> 'Function) ->
    func: RuntimeFunction -> 
    irArgFlags: OlyIRLocalFlags imarray ->
    irLocalFlags: OlyIRLocalFlags imarray ->
    E<'Type, 'Function, 'Field> -> 
    GenericContext ->
    isDebuggable: bool ->
    OlyIRFunctionBody<'Type, 'Function, 'Field>
