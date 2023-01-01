[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.Lowering.Optimizer

open System.Threading
open Oly.Compiler.Internal.BoundTree

type OptimizerSettings =
    {
        LocalValueElimination: bool
        BranchElimination: bool
    }

/// Optimizes the tree in two passes.
/// Will always optimize generated expressions if it can.
val Lower : ct: CancellationToken -> OptimizerSettings -> BoundTree -> BoundTree