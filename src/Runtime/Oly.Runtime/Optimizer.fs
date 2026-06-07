module rec Oly.Runtime.CodeGen.Internal.Optimizer

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

open Oly.Runtime.CodeGen.Internal.InlineFunctions
open Oly.Runtime.CodeGen.Internal.Optimizations.GeneralOptimizer
open Oly.Runtime.CodeGen.Internal.Optimizations.CopyPropagation
open Oly.Runtime.CodeGen.Internal.Optimizations.CommonSubexpressionElimination
open Oly.Runtime.CodeGen.Internal.Optimizations.AssertionPropagation
open Oly.Runtime.CodeGen.Internal.Optimizations.DeadCodeElimination
open Oly.Runtime.CodeGen.Internal.LocalNormalization

let OptimizeFunctionBody<'Type, 'Function, 'Field> 
        (tryGetFunctionBody: RuntimeFunction -> OlyIRFunctionBody<'Type, 'Function, 'Field> option) 
        (emitFunction: RuntimeFunction -> 'Function)
        (emitType: RuntimeType -> 'Type)
        (irArgFlags: OlyIRLocalFlags [])
        (irLocalFlags: OlyIRLocalFlags [])
        (irExpr: E<'Type, 'Function, 'Field>)
        (genericContext: GenericContext)
        (irTier: OlyIRFunctionTier)
        (enclosingTyName: string)
        (funcName: string) 
        =
    let argLocalManager =
        ArgumentLocalManager(irArgFlags, ResizeArray irLocalFlags)

    let optenv: optenv<'Type, 'Function, 'Field> =
        {
            tryGetFunctionBody = tryGetFunctionBody
            emitFunction = emitFunction
            emitType = emitType
            argLocalManager = argLocalManager
            inlineSet = Dictionary()
            irTier = irTier
            genericContext = genericContext
            ssaenv = ssaenv(argLocalManager)
            isSsa = false
        }

    /// In Debug or Checked builds, this checks the IR to make sure its valid.
    let inline checkExpr name optenv expr =
#if DEBUG || CHECKED
        try
            let _ = NormalizeLocals optenv expr
            ()
        with
        | _ ->
            Debug.WriteLine(Dump.DumpExpression expr)
            reraise()

#endif
        expr
        
    let optimizationPass (optenv: optenv<_, _, _>) irExpr =
        irExpr
        |> OptimizeExpression optenv  
        |> checkExpr "OptimizeExpression" optenv
        |> CopyPropagation optenv
        |> checkExpr "CopyPropagation" optenv
        |> CommonSubexpressionElimination optenv
        |> checkExpr "CommonSubexpressionElimination" optenv
        |> AssertionPropagation optenv
        |> checkExpr "AssertionPropagation" optenv
        |> DeadCodeElimination optenv
        |> checkExpr "DeadCodeElimination" optenv

    let irOptimizedExpr = 
        let mutable irNewExpr = InlineFunctions optenv irExpr |> checkExpr "InlineFunctions" optenv
        if optenv.IsDebuggable |> not then
            for _ = 1 to 3 do // 3 passes
                irNewExpr <- optimizationPass optenv irNewExpr
        irNewExpr

    let irOptimizedExpr, optenv = 
        irOptimizedExpr
        |> NormalizeLocals optenv

    let irLocalFlags = optenv.GetLocalFlags()
    let irArgFlags = optenv.GetArgumentFlags()

    //if optenv.IsDebuggable then
    //    System.IO.File.WriteAllText($"{enclosingTyName}_{funcName}_debug.oly-ir", Dump.DumpExpression irOptimizedExpr)
    //else
    //    System.IO.File.WriteAllText($"{enclosingTyName}_{funcName}.oly-ir", Dump.DumpExpression irOptimizedExpr)

    OlyIRFunctionBody<'Type, 'Function, 'Field>(irOptimizedExpr, irArgFlags, irLocalFlags)