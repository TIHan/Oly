module internal rec Oly.Runtime.CodeGen.Internal.Optimizations.GeneralOptimizer

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
open Oly.Runtime.CodeGen.Internal.InlineFunctions

let private And arg1 arg2 resultTy =
    E.IfElse(arg1, arg2, E.Value(NoRange, V.Constant(C.False, resultTy)), resultTy)

let private Or arg1 arg2 resultTy =
    E.IfElse(arg1, E.Value(NoRange, V.Constant(C.True, resultTy)), arg2, resultTy)

let private optimizeOperation optenv irExpr : E<_, _, _> =
    match irExpr with
    | E.Operation(irTextRange, irOp) ->
        let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> optimizeExpression optenv argExpr)
        if newOp = irOp then
            optimizeOperationSecondPass optenv irExpr
        else
            E.Operation(irTextRange, newOp)
            |> optimizeOperationSecondPass optenv
    | _ ->
        failwith "invalid expression"

let private optimizeOperationSecondPass optenv irExpr : E<_, _, _> =
    match irExpr with
    // Makes optimizations easier
    | And(And(argx1, argx2, _), arg2, resultTy) ->
        And argx1 (And argx2 arg2 resultTy) resultTy
        |> optimizeExpression optenv

    // Makes optimizations easier
    | Or(Or(argx1, argx2, _), arg2, resultTy) ->
        Or argx1 (Or argx2 arg2 resultTy) resultTy
        |> optimizeExpression optenv

    | E.Operation(irTextRange, irOp) ->
        match irOp with
        | O.Ignore(irArgExpr, resultTy) ->
            if hasSideEffect optenv irArgExpr then
                irExpr
            else
                E.None(NoRange, resultTy)

        | O.StoreToAddress(E.Value(_, V.LocalAddress(localIndex, irByRefKind, _)), irRhsExpr, resultTy) ->
            match irByRefKind with
            | OlyIRByRefKind.ReadOnly ->
                OlyAssert.Fail("Expected read-write byref type.")
            | _ ->
                ()
            E.Operation(irTextRange, O.Store(localIndex, irRhsExpr, resultTy))

        | O.StoreToAddress(E.Value(_, V.ArgumentAddress(argIndex, irByRefKind, _)), irRhsExpr, resultTy) ->
            match irByRefKind with
            | OlyIRByRefKind.ReadOnly ->
                OlyAssert.Fail("Expected read-write byref type.")
            | _ ->
                ()
            E.Operation(irTextRange, O.StoreArgument(argIndex, irRhsExpr, resultTy))

        | O.LoadFromAddress(E.Value(_, V.LocalAddress(localIndex, _, _)), resultTy) ->
            E.Value(irTextRange, V.Local(localIndex, resultTy))

        | O.LoadFromAddress(E.Value(_, V.ArgumentAddress(argIndex, _, _)), resultTy) ->
            E.Value(irTextRange, V.Argument(argIndex, resultTy))

        // Constant folding
        | _ ->
            OptimizeImmediateConstantFolding irExpr
                   
    | _ ->
        OlyAssert.Fail("Expected opertion")

let private optimizeExpression optenv irExpr : E<_, _, _> =
    StackGuard.Do(fun () ->
        optimizeExpressionCore optenv irExpr
    )

let private optimizeExpressionCore optenv irExpr : E<_, _, _> =
    match irExpr with
    // Inconsequential loop
    | SimpleForLoop(_, rhsExpr, condExpr, bodyExpr, resultTy)
            when not(hasSideEffect optenv rhsExpr) &&
                    not(hasSideEffect optenv condExpr) &&
                    not(hasSideEffect optenv bodyExpr) ->
        E.None(OlyIRDebugSourceTextRange.Empty, resultTy)

    // Normalize sequential expressions
    | E.Let(name, localIndex, E.Sequential(expr1, expr2), bodyExpr) ->
        E.Sequential(
            expr1,
            E.Let(name, localIndex, expr2, bodyExpr)
        )
        |> optimizeExpression optenv
    | E.Let(name, localIndex, E.Let(name2, localIndex2, rhsExpr2, bodyExpr2), bodyExpr) ->
        E.Let(
            name2,
            localIndex2,
            rhsExpr2,
            E.Let(name, localIndex, bodyExpr2, bodyExpr)
        )
        |> optimizeExpression optenv

    | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
        let irNewRhsExpr = optimizeExpression optenv irRhsExpr
        let irNewBodyExpr = optimizeExpression optenv irBodyExpr

        if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
            irExpr
        else
            E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

    | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
        let irNewConditionExpr = optimizeExpression optenv irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.True, _)) ->
            optimizeExpression optenv irTrueTargetExpr

        | E.Value(value=V.Constant(C.False, _)) ->
            optimizeExpression optenv irFalseTargetExpr

        | _ ->
            let irNewTrueTargetExpr = optimizeExpression optenv irTrueTargetExpr
            let irNewFalseTargetExpr = optimizeExpression optenv irFalseTargetExpr

            match irNewTrueTargetExpr, irNewFalseTargetExpr with
            | E.Value(value=V.Constant(C.True, _)), E.Value(value=V.Constant(C.False, _)) -> 
                irNewConditionExpr
            | E.Value(value=V.Constant(C.False, _)), E.Value(value=V.Constant(C.False, _)) when hasSideEffect optenv irNewConditionExpr |> not ->
                E.Value(NoRange, V.Constant(C.False, resultTy))

            | E.Operation(op=O.Call(irFunc1, irArgExprs1, _)), E.Operation(op=O.Call(irFunc2, irArgExprs2, _))
                    when 
                    irFunc1.RuntimeFunction = irFunc2.RuntimeFunction && irArgExprs1.Length = irArgExprs2.Length &&
                    (irArgExprs1, irArgExprs2) 
                    ||> ImArray.forall2 (fun x y ->
                        match x, y with
                        | E.Value(value=V.Argument(index1, _)), E.Value(value=V.Argument(index2, _)) ->
                            index1 = index2
                        | E.Value(value=V.Local(index1, _)), E.Value(value=V.Local(index2, _)) ->
                            index1 = index2
                        | _ ->
                            false
                    ) && not(hasSideEffect optenv irNewConditionExpr) ->
                irNewTrueTargetExpr

            | E.IfElse(irNestedConditionExpr, irNestedTrueTargetExpr, E.Operation(op=O.Call(irFunc1, irArgExprs1, _)), _), E.Operation(op=O.Call(irFunc2, irArgExprs2, _))
                    when irFunc1.RuntimeFunction = irFunc2.RuntimeFunction && irArgExprs1.Length = irArgExprs2.Length &&
                            // This is conservative. We could allow mutable locals if we know the conditional expressions do not have side effects.
                            (irArgExprs1, irArgExprs2) 
                            ||> ImArray.forall2 (fun x y ->
                            match x, y with
                            | E.Value(value=V.Argument(index1, _)), E.Value(value=V.Argument(index2, _)) ->
                                not(optenv.IsArgumentMutable(index1)) && index1 = index2
                            | E.Value(value=V.Local(index1, _)), E.Value(value=V.Local(index2, _)) ->
                                not(optenv.IsLocalMutable(index1)) && index1 = index2
                            | _ ->
                                false
                            ) ->
                    E.IfElse(And irNewConditionExpr irNestedConditionExpr irNewConditionExpr.ResultType, irNestedTrueTargetExpr, irNewFalseTargetExpr, resultTy)
                    |> optimizeExpression optenv

            | E.Let(name, localIndex, irRhsExpr, E.IfElse(irNestedConditionExpr, irNestedTrueTargetExpr, E.Operation(op=O.Call(irFunc1, irArgExprs1, _)), _)), E.Operation(op=O.Call(irFunc2, irArgExprs2, _)) 
                    when irFunc1.RuntimeFunction = irFunc2.RuntimeFunction && irArgExprs1.Length = irArgExprs2.Length &&
                    // This is conservative. We could allow mutable locals if we know the conditional expressions do not have side effects.
                    (irArgExprs1, irArgExprs2) 
                    ||> ImArray.forall2 (fun x y ->
                        match x, y with
                        | E.Value(value=V.Argument(index1, _)), E.Value(value=V.Argument(index2, _)) ->
                            not(optenv.IsArgumentMutable(index1)) && index1 = index2
                        | E.Value(value=V.Local(index1, _)), E.Value(value=V.Local(index2, _)) ->
                            index1 <> localIndex && not(optenv.IsLocalMutable(index1)) && index1 = index2
                        | _ ->
                            false
                    ) && not(hasSideEffect optenv irRhsExpr) && (
                        match irNestedTrueTargetExpr with
                        | E.Value(value=V.Local(index, _)) ->
                            index <> localIndex
                        | E.Operation(op=O.Call(_, irArgExprs3, _)) ->
                            irArgExprs3
                            |> ImArray.forall (fun x ->
                                match x with
                                | E.Value(value=V.Local(index, _)) ->
                                    localIndex <> index
                                | _ ->
                                    false
                            )
                        | _ ->
                            false
                    ) ->

                let irNewConditionExpr =
                    And irNewConditionExpr (E.Let(name, localIndex, irRhsExpr, irNestedConditionExpr)) irNewConditionExpr.ResultType

                E.IfElse(irNewConditionExpr,
                    irNestedTrueTargetExpr,
                    irNewFalseTargetExpr,
                    resultTy
                )
                |> optimizeExpression optenv

            | _ ->
                if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                    irExpr
                else
                    E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

    | E.While(irConditionExpr, irBodyExpr, resultTy) ->
        let irNewConditionExpr = optimizeExpression optenv irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.False, _)) ->
            E.None(NoRange, resultTy)
        | _ ->
            let irNewBodyExpr = optimizeExpression optenv irBodyExpr

            if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                irExpr
            else
                E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

    | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
        let irNewBodyExpr = optimizeExpression optenv irBodyExpr

        let mutable didChange = false
        let irNewCatchCases =
            irCatchCases
            |> ImArray.map (fun irCatchCase ->
                match irCatchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                    let irNewCaseBodyExpr = optimizeExpression optenv irCaseBodyExpr

                    if irNewCaseBodyExpr = irCaseBodyExpr then
                        irCatchCase
                    else
                        didChange <- true
                        OlyIRCatchCase.CatchCase(localName, localIndex, irNewCaseBodyExpr, catchTy)
            )

        let irNewFinallyBodyExprOpt =
            irFinallyBodyExprOpt
            |> Option.map (fun irExpr ->
                let irNewExpr = optimizeExpression optenv irExpr
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
        let irNewExpr1 = optimizeExpression optenv irExpr1
        let irNewExpr2 = optimizeExpression optenv irExpr2

        if hasSideEffect optenv irNewExpr1 then
            if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
                irExpr
            else
                E.Sequential(irNewExpr1, irNewExpr2)
        else
            irNewExpr2

    | E.Operation _ ->
        optimizeOperation optenv irExpr

    | _ ->
        irExpr

let OptimizeExpression (optenv: optenv<_, _, _>) (irExpr: E<_, _, _>) : E<_, _, _> =
    optimizeExpression optenv irExpr
