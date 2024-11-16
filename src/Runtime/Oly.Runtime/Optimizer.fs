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

(* Template ************************************************************************************************************************************************
let handleOperation irExpr =
    match irExpr with
    | E.Operation(irTextRange, irOp) ->
        irExpr
    | _ ->
        OlyAssert.Fail("Expected opertion")

let rec handleExpression irExpr : E<_, _, _> =
    match irExpr with
    | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
        let irNewRhsExpr = handleExpression irRhsExpr
        let irNewBodyExpr = handleExpression irBodyExpr

        if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
            irExpr
        else
            E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

    | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
        let irNewConditionExpr = handleExpression irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.True, _)) ->
            handleExpression irTrueTargetExpr

        | E.Value(value=V.Constant(C.False, _)) ->
            handleExpression irFalseTargetExpr

        | _ ->
            let irNewTrueTargetExpr = handleExpression irTrueTargetExpr
            let irNewFalseTargetExpr = handleExpression irFalseTargetExpr

            if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                irExpr
            else
                E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

    | E.While(irConditionExpr, irBodyExpr, resultTy) ->
        let irNewConditionExpr = handleExpression irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.False, _)) ->
            E.None(resultTy)
        | _ ->
            let irNewBodyExpr = handleExpression irBodyExpr

            if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                irExpr
            else
                E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

    | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
        let irNewBodyExpr = handleExpression irBodyExpr

        let mutable didChange = false
        let irNewCatchCases =
            irCatchCases
            |> ImArray.map (fun irCatchCase ->
                match irCatchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                    let irNewCaseBodyExpr = handleExpression irCaseBodyExpr

                    if irNewCaseBodyExpr = irCaseBodyExpr then
                        irCatchCase
                    else
                        didChange <- true
                        OlyIRCatchCase.CatchCase(localName, localIndex, irNewCaseBodyExpr, catchTy)
            )

        let irNewFinallyBodyExprOpt =
            irFinallyBodyExprOpt
            |> Option.map (fun irExpr ->
                let irNewExpr = handleExpression irExpr
                if irNewExpr = irExpr then
                    irExpr
                else
                    didChange <- true
                    irNewExpr
            )

        if irNewBodyExpr = irBodyExpr && not didChange then
            irExpr
        else
            E.Try(irNewBodyExpr, irNewCatchCases, irNewFinallyBodyExprOpt, resultTy)

    | E.Sequential(irExpr1, irExpr2) ->
        let irNewExpr1 = handleExpression irExpr1
        let irNewExpr2 = handleExpression irExpr2

        if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
            irExpr
        else
            E.Sequential(irNewExpr1, irNewExpr2)

    | E.Operation(irTextRange, irOp) ->
        let irNewArgExprs = irOp.MapArguments(fun _ irArgExpr -> handleExpression irArgExpr)
        let mutable areSame = true
        irOp.ForEachArgument(fun i irArgExpr ->
            if irNewArgExprs[i] <> irArgExpr then
                areSame <- false
        )
        if areSame then
            handleOperation irExpr
        else
            let irNewOp = irOp.ReplaceArguments(irNewArgExprs) 
            E.Operation(irTextRange, irNewOp)
            |> handleOperation

    | _ ->
        irExpr

handleExpression irExpr

*)



// -------------------------------------------------------------------------------------------------------------

// -------------------------------------------------------------------------------------------------------------

let OptimizeFunctionBody<'Type, 'Function, 'Field> 
        (tryGetFunctionBody: RuntimeFunction -> OlyIRFunctionBody<'Type, 'Function, 'Field> option) 
        (emitFunction: RuntimeFunction * RuntimeFunction -> 'Function)
        (emitType: RuntimeType -> 'Type)
        (func: RuntimeFunction) 
        (irArgFlags: OlyIRLocalFlags [])
        (irLocalFlags: OlyIRLocalFlags [])
        (irExpr: E<'Type, 'Function, 'Field>)
        (genericContext: GenericContext)
        (irTier: OlyIRFunctionTier) =
#if DEBUG || CHECKED
    Log(
        let witnesses = func.Witnesses
        let witnessText = 
            if witnesses.IsEmpty then
                ""
            else
                let text = witnesses |> ImArray.map (fun x -> x.TypeExtension.Name.ToString()) |> (String.concat "\n")
                $" - Witnesses: {text}"
        $"Optimizing Function: {func.EnclosingType.Name}.{func.Name}{witnessText}"
    )
#endif
    let argLocalManager =
        ArgumentLocalManager(irArgFlags, ResizeArray irLocalFlags)

    let optenv: optenv<'Type, 'Function, 'Field> =
        {
            tryGetFunctionBody = tryGetFunctionBody
            emitFunction = emitFunction
            emitType = emitType
            func = func
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
        // TODO: We could expand this, but NormalizeLocals is good enough.
        try
            let _ = NormalizeLocals optenv expr
            ()
        with
        | ex ->
            raise(AggregateException($"Error in '{name}'.", ex))
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

    //if optenv.IsDebuggable then
    //    System.IO.File.WriteAllText($"{optenv.func.EnclosingType.Name}_{optenv.func.Name}_debug_before.oly-ir", Dump.DumpExpression irExpr)
    //else
    //    System.IO.File.WriteAllText($"{optenv.func.EnclosingType.Name}_{optenv.func.Name}_before.oly-ir", Dump.DumpExpression irExpr)

    let irOptimizedExpr = 
        let mutable irNewExpr = InlineFunctions optenv irExpr |> checkExpr "InlineFunctions" optenv
        if optenv.IsDebuggable |> not then
            // Run an optimize expression pass before SSA.
            irNewExpr <-
                irNewExpr
                |> OptimizeExpression optenv  
                |> checkExpr "OptimizeExpression" optenv
           // irNewExpr <- SSA.ToSSA optenv SSA.SsaUsage.Default irNewExpr |> fst |> checkExpr "ToSSA" optenv

            for _ = 1 to 3 do // 3 passes
                irNewExpr <- optimizationPass optenv irNewExpr

          //  irNewExpr <- SSA.FromSSA optenv ImmutableHashSet.Empty irNewExpr |> checkExpr "FromSSA" optenv
            irNewExpr <- DeadCodeElimination optenv irNewExpr |> checkExpr "DeadCodeElimination after FromSSA" optenv
        irNewExpr

    let irOptimizedExpr, optenv = 
        irOptimizedExpr
        |> NormalizeLocals optenv

    let irLocalFlags = optenv.GetLocalFlags()
    let irArgFlags = optenv.GetArgumentFlags()

    //if optenv.IsDebuggable then
    //    System.IO.File.WriteAllText($"{optenv.func.EnclosingType.Name}_{optenv.func.Name}_debug.oly-ir", Dump.DumpExpression irOptimizedExpr)
    //else
    //    System.IO.File.WriteAllText($"{optenv.func.EnclosingType.Name}_{optenv.func.Name}.oly-ir", Dump.DumpExpression irOptimizedExpr)

    OlyIRFunctionBody<'Type, 'Function, 'Field>(irOptimizedExpr, irArgFlags, irLocalFlags)