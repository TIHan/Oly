module rec Oly.Runtime.CodeGen.Internal.SSA

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

let internal ToSSA (optenv: optenv<_, _, _>) (expr: E<_, _, _>) =
    StackGuard.Do(fun () -> ToSSAAux optenv expr)
let private ToSSAAux (optenv: optenv<_, _, _>) (expr: E<_, _, _>) =
    match expr with
    | E.None _ -> expr
    | E.Value(textRange, v) ->
        match v with
        | V.Local(localIndex, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromLocal(localIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.Local(ssaIndex, resultTy))
            | _ ->
                expr
        | V.LocalAddress(localIndex, kind, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromLocal(localIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.LocalAddress(ssaIndex, kind, resultTy))
            | _ ->
                expr
        | V.Argument(argIndex, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromArgument(argIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.Local(ssaIndex, resultTy))
            | _ ->
                expr
        | V.ArgumentAddress(argIndex, kind, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromArgument(argIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.LocalAddress(ssaIndex, kind, resultTy))
            | _ ->
                expr
        | _ ->
            expr

    | E.Let(name, localIndex, rhsExpr, bodyExpr) ->
        E.Let(name, localIndex, ToSSA optenv rhsExpr, ToSSA optenv bodyExpr)

    | E.Sequential(expr1, expr2) ->
        let newExpr1 = ToSSA optenv expr1
        let newExpr2 = ToSSA optenv expr2

        if newExpr1 = expr1 && newExpr2 = expr2 then
            expr
        else
            E.Sequential(newExpr1, newExpr2)

    | E.Operation(textRange, op) ->
        let newOp = op.MapAndReplaceArguments(fun _ expr -> ToSSA optenv expr)

        match newOp with
        | O.Store(localIndex, rhsExpr, resultTy) ->
            E.Let("ssa_local", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex), rhsExpr, E.None(OlyIRDebugSourceTextRange.Empty, resultTy))

        | O.StoreArgument(argIndex, rhsExpr, resultTy) ->
            E.Let("ssa_arg", optenv.ssaenv.CreateSsaIdexFromArgument(argIndex), rhsExpr, E.None(OlyIRDebugSourceTextRange.Empty, resultTy))

        | _ ->
            if newOp = op then
                expr
            else
                E.Operation(textRange, op)
    
    | E.While(conditionExpr, bodyExpr, resultTy) ->
        let newConditionExpr = ToSSA optenv conditionExpr
        let newBodyExpr = ToSSA optenv bodyExpr

        if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
            expr
        else
            E.While(newConditionExpr, newBodyExpr, resultTy)

    | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
        let newConditionExpr = ToSSA optenv conditionExpr
        let newTrueTargetExpr = ToSSA optenv trueTargetExpr
        let newFalseTargetExpr = ToSSA optenv falseTargetExpr

        if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
            expr
        else
            E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)

    | E.Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy) ->
        let newBodyExpr = ToSSA optenv bodyExpr
        let newCatchCases =
            catchCases
            |> ImArray.map (fun catchCase ->
                match catchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, bodyExpr, catchTy) ->
                    OlyIRCatchCase.CatchCase(localName, localIndex, ToSSA optenv bodyExpr, catchTy)
            )
        let newFinallyBodyExprOpt = finallyBodyExprOpt |> Option.map (ToSSA optenv)
        E.Try(newBodyExpr, newCatchCases, newFinallyBodyExprOpt, resultTy)

let internal FromSSA (optenv: optenv<_, _, _>) (expr: E<_, _, _>) =
    StackGuard.Do(fun () -> FromSSAAux optenv expr)
let private FromSSAAux (optenv: optenv<_, _, _>) (expr: E<_, _, _>) =
    match expr with
    | E.None _ -> expr
    | E.Value(textRange, v) ->
        match v with
        | V.Local(localIndex, resultTy) ->
            match optenv.ssaenv.GetValue(localIndex) with
            | SsaValue.Local(localIndex) ->
                E.Value(textRange, V.Local(localIndex, resultTy))
            | SsaValue.Argument(argIndex) ->
                E.Value(textRange, V.Argument(argIndex, resultTy))
            | SsaValue.None ->
                expr
        | V.LocalAddress(localIndex, kind, resultTy) ->
            match optenv.ssaenv.GetValue(localIndex) with
            | SsaValue.Local(localIndex) ->
                E.Value(textRange, V.LocalAddress(localIndex, kind, resultTy))
            | SsaValue.Argument(argIndex) ->
                E.Value(textRange, V.ArgumentAddress(argIndex, kind, resultTy))
            | SsaValue.None ->
                expr
        | _ ->
            expr

    | E.Let(name, localIndex, rhsExpr, bodyExpr) ->
        let newRhsExpr = FromSSA optenv rhsExpr
        let newBodyExpr = FromSSA optenv bodyExpr
        match optenv.ssaenv.GetValue(localIndex) with
        | SsaValue.Local(localIndex) ->
            E.Sequential(
                E.Operation((* TODO: bad range *) OlyIRDebugSourceTextRange.Empty, O.Store(localIndex, newRhsExpr, optenv.emitType(RuntimeType.Void))),
                newBodyExpr
            )
        | SsaValue.Argument(argIndex) ->
            E.Sequential(
                E.Operation((* TODO: bad range *) OlyIRDebugSourceTextRange.Empty, O.StoreArgument(argIndex, newRhsExpr, optenv.emitType(RuntimeType.Void))),
                newBodyExpr
            )
        | SsaValue.None ->
            if newRhsExpr = rhsExpr && newBodyExpr = bodyExpr then
                expr
            else
                E.Let(name, localIndex, newRhsExpr, newBodyExpr)

    | E.Sequential(expr1, expr2) ->
        let newExpr1 = FromSSA optenv expr1
        let newExpr2 = FromSSA optenv expr2

        if newExpr1 = expr1 && newExpr2 = expr2 then
            expr
        else
            E.Sequential(newExpr1, newExpr2)

    | E.Operation(textRange, op) ->
        let newOp = op.MapAndReplaceArguments(fun _ expr -> FromSSA optenv expr)

        if newOp = op then
            expr
        else
            E.Operation(textRange, op)
    
    | E.While(conditionExpr, bodyExpr, resultTy) ->
        let newConditionExpr = FromSSA optenv conditionExpr
        let newBodyExpr = FromSSA optenv bodyExpr

        if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
            expr
        else
            E.While(newConditionExpr, newBodyExpr, resultTy)

    | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
        let newConditionExpr = FromSSA optenv conditionExpr
        let newTrueTargetExpr = FromSSA optenv trueTargetExpr
        let newFalseTargetExpr = FromSSA optenv falseTargetExpr

        if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
            expr
        else
            E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)

    | E.Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy) ->
        let newBodyExpr = FromSSA optenv bodyExpr
        let newCatchCases =
            catchCases
            |> ImArray.map (fun catchCase ->
                match catchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, bodyExpr, catchTy) ->
                    OlyIRCatchCase.CatchCase(localName, localIndex, FromSSA optenv bodyExpr, catchTy)
            )
        let newFinallyBodyExprOpt = finallyBodyExprOpt |> Option.map (FromSSA optenv)
        E.Try(newBodyExpr, newCatchCases, newFinallyBodyExprOpt, resultTy)
