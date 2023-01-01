
[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.Lowering.CommonLowering

open System.Threading
open Oly.Compiler.Internal.BoundTree

/// Generally not for optimizing.
/// Handles transformations for:
///     - Logical And/Or calls transforms to IfElse expressions
///     - Sequential normalization
///     - Auto-properties
///     - Get/Set property calls
///     - Static field constant uses (transforms to literals)
///     - Implicit default constructors
///     - Cast/Typed removals - REVIEW: Should we actually do this here? Should analyzers actually see these even if they are redundant?
///     - LoadFunctionPtr lambda removal
/// TODO:
///    Consider moving static constructs, static local functions and entity definitions, in local contexts
///    to be moved to the beginning of the function. That normalization would allow analyzing and optimization
///    to not have any interference.
val Lower : ct: CancellationToken -> BoundTree -> BoundTree