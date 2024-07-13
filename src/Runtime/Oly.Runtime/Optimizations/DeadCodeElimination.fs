module internal rec Oly.Runtime.CodeGen.Internal.Optimizations.DeadCodeElimination

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

open Oly.Runtime.CodeGen.Internal

// -------------------------------------------------------------------------------------------------------------

let DeadCodeElimination optenv (irExpr: E<_, _, _>) =
    let doNotRemove = HashSet()
    
    let rec analyzeExpression inCandidate irExpr : unit =
        match irExpr with
        | E.Let(_, localIndex, irRhsExpr, irBodyExpr) ->
            analyzeExpression inCandidate irBodyExpr

            if doNotRemove.Contains(localIndex) || hasSideEffect optenv irRhsExpr then
                doNotRemove.Add(localIndex) |> ignore
                analyzeExpression inCandidate irRhsExpr
    
        | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, _) ->
            analyzeExpression inCandidate irConditionExpr
            analyzeExpression inCandidate irTrueTargetExpr
            analyzeExpression inCandidate irFalseTargetExpr
    
        | E.While(irConditionExpr, irBodyExpr, _) ->
            analyzeExpression inCandidate irConditionExpr
            analyzeExpression inCandidate irBodyExpr

        | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, _) ->
            analyzeExpression inCandidate irBodyExpr

            irCatchCases
            |> ImArray.iter (fun irCatchCase ->
                match irCatchCase with
                | OlyIRCatchCase.CatchCase(_, localIndex, irCaseBodyExpr, _) ->
                    doNotRemove.Add(localIndex) |> ignore
                    analyzeExpression inCandidate irCaseBodyExpr
            )

            irFinallyBodyExprOpt
            |> Option.iter (fun irExpr ->
                analyzeExpression inCandidate irExpr
            )
    
        | E.Sequential(irExpr1, irExpr2) ->
            analyzeExpression inCandidate irExpr1
            analyzeExpression inCandidate irExpr2

        | E.Operation(op=irOp) ->
            irOp.ForEachArgument(fun _ irArgExpr ->
                analyzeExpression inCandidate irArgExpr
            )
            match irOp with
            | O.Store(localIndex, _, _) ->
                doNotRemove.Add(localIndex) |> ignore
            | _ ->
                ()

        | E.Value(value=irValue) ->
            match irValue with
            | V.Local(localIndex, _)
            | V.LocalAddress(localIndex, _, _) ->
                doNotRemove.Add(localIndex) |> ignore
            | _ ->
                ()
    
        | _ ->
            ()
    
    analyzeExpression false irExpr

    let rec handleExpression irExpr =
        StackGuard.Do(fun () ->
            handleExpressionAuxDeadCodeElimination irExpr
        )

    and handleExpressionAuxDeadCodeElimination irExpr : E<_, _, _> =
        match irExpr with
        | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
            if doNotRemove.Contains(localIndex) then
                let irNewRhsExpr = handleExpression irRhsExpr
                let irNewBodyExpr = handleExpression irBodyExpr

                if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                    irExpr
                else
                    E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)
            else
                handleExpression irBodyExpr

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
                E.None(NoRange, resultTy)
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
                        OlyAssert.True(doNotRemove.Contains(localIndex))

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
            //let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> handleExpression argExpr)
            //if newOp = irOp then
            //    irExpr
            //else
            //    E.Operation(irTextRange, newOp)
            let irNewArgExprs = irOp.MapArguments(fun _ irArgExpr -> handleExpression irArgExpr)
            let mutable areSame = true
            irOp.ForEachArgument(fun i irArgExpr ->
                if irNewArgExprs[i] <> irArgExpr then
                    areSame <- false
            )
            if areSame then
                irExpr
            else                    
                let irNewOp = irOp.ReplaceArguments(irNewArgExprs)
                E.Operation(irTextRange, irNewOp)

        | _ ->
            irExpr

    handleExpression irExpr

