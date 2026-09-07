module internal Oly.Runtime.CodeGen.Internal.InlineFunctions

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable
open System.Diagnostics
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns
open Oly.Metadata
open Oly.Core
open Oly.Core.TaskExtensions

val InlineFunctions : optenv<'Type, 'Function, 'Field> -> irExpr: E<'Type, 'Function, 'Field> -> E<'Type, 'Function, 'Field>