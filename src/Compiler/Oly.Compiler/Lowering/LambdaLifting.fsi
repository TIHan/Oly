[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.Lowering.LambdaLifting

open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.SymbolEnvironments

/// Transforms Lambda expressions into closures.
/// This is required before code-gen.
val Lower : g: g -> tree: BoundTree -> BoundTree