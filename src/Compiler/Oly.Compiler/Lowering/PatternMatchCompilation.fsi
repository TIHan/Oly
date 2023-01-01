[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.Lowering.PatternMatchCompilation

open System.Threading
open Oly.Compiler.Internal.BoundTree

/// Transforms the 'Match' expression into a series of 'If/IfElse' expressions.
/// Extra locals will be created, but later optimizaations will clean this up especially for
/// generated locals.
val Lower : ct: CancellationToken -> BoundTree -> BoundTree