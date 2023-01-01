[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.Lowering.LambdaLifting

open Oly.Compiler.Internal.BoundTree

/// Transforms Lambda expressions into closures.
/// This is required before code-gen.
val Lower : BoundTree -> BoundTree