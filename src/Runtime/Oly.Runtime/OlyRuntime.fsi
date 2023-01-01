[<AutoOpen>]
module rec Oly.Runtime.Implementation

open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Metadata

[<Sealed>]
type OlyRuntime<'Type, 'Function, 'Field> =

    new : IOlyRuntimeEmitter<'Type, 'Function, 'Field> -> OlyRuntime<'Type, 'Function, 'Field>

    /// Loads an assembly into the runtime.
    member ImportAssembly : OlyILReadOnlyAssembly -> unit

    /// Emit the entry point.
    /// If multiple entry points are found, it will raise an error.
    member EmitEntryPoint : unit -> unit

    /// Emit all non-generic entities and functions ahead of time.
    /// Generic entities and functions can still be emitted if they are instantiated within the non-generic entities and functions.
    /// Generic functions that override external generic functions will be emitted ahead of time - requires the target runtime to support generics.
    member EmitAheadOfTime : unit -> unit

    member TryGetIROfFunction : fullyQualifiedTypeName: string * funcName: string -> Result<OlyIRFunctionBody<'Type, 'Function, 'Field>, string>