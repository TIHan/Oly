[<AutoOpen>]
module internal Oly.Runtime.CodeGen.Internal.Optimizer

open Oly.Core
open Oly.Metadata
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns

val OptimizeFunctionBody<'Type, 'Function, 'Field> :
    tryGetFunctionBody: (RuntimeFunction -> OlyIRFunctionBody<'Type, 'Function, 'Field> option) ->
    emitFunction: (RuntimeFunction -> 'Function) ->
    emitType: (RuntimeType -> 'Type) ->
    irArgFlags: OlyIRLocalFlags [] ->
    irLocalFlags: OlyIRLocalFlags [] ->
    E<'Type, 'Function, 'Field> -> 
    GenericContext ->
    irTier: OlyIRFunctionTier ->
    OlyIRFunctionBody<'Type, 'Function, 'Field>
