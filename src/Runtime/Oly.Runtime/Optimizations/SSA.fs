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

let internal ToSSA (optenv: optenv<_, _, _>) (scopes: SsaScopes) (expr: E<_, _, _>) =
    StackGuard.Do(fun () -> ToSSAAux optenv scopes expr)
let private ToSSAAux (optenv: optenv<_, _, _>) (scopes: SsaScopes) (expr: E<_, _, _>) =
    match expr with
    | E.None _ 
    | E.Phi _ -> expr, scopes
    | E.Value(textRange, v) ->
        match v with
        | V.Local(localIndex, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromLocal(localIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.Local(ssaIndex, resultTy)), scopes
            | _ ->
                expr, scopes
        | V.LocalAddress(localIndex, kind, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromLocal(localIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.LocalAddress(ssaIndex, kind, resultTy)), scopes
            | _ ->
                expr, scopes
        | V.Argument(argIndex, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromArgument(argIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.Local(ssaIndex, resultTy)), scopes
            | _ ->
                expr, scopes
        | V.ArgumentAddress(argIndex, kind, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromArgument(argIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.LocalAddress(ssaIndex, kind, resultTy)), scopes
            | _ ->
                expr, scopes
        | _ ->
            expr, scopes

    | E.Let(name, localIndex, rhsExpr, bodyExpr) ->
        let newRhsExpr, used = ToSSA optenv scopes rhsExpr

        let used = { hashSet = used.hashSet.Add(localIndex) }
        let newBodyExpr, used = ToSSA optenv used bodyExpr

        E.Let(name, localIndex, newRhsExpr, newBodyExpr), used

    | E.Operation(textRange, op) ->
        let mutable used = scopes
        let newOp = 
            op.MapAndReplaceArguments(fun _ expr -> 
                let newExpr, newUsed = ToSSA optenv used expr
                used <- newUsed
                newExpr
            )

        match newOp with
        | O.Store(localIndex, rhsExpr, resultTy) ->
            E.Let("ssa_local", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex), rhsExpr, E.None(OlyIRDebugSourceTextRange.Empty, resultTy)), used

        | O.StoreArgument(argIndex, rhsExpr, resultTy) ->
            E.Let("ssa_arg", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex), rhsExpr, E.None(OlyIRDebugSourceTextRange.Empty, resultTy)), used

        | _ ->
            if newOp = op then
                expr, used
            else
                E.Operation(textRange, op), used

    | E.Sequential(E.Operation(_, O.Store(localIndex, rhsExpr, _)), expr2) ->
        E.Let("ssa_local", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex), rhsExpr, expr2), scopes

    | E.Sequential(E.Operation(_, O.StoreArgument(argIndex, rhsExpr, _)), expr2) ->
        E.Let("ssa_arg", optenv.ssaenv.CreateSsaIndexFromLocal(argIndex), rhsExpr, expr2), scopes

    | E.Sequential(E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy), expr2) ->
        let newConditionExpr, used = ToSSA optenv scopes conditionExpr
        let newTrueTargetExpr, used = ToSSA optenv used trueTargetExpr
        let newFalseTargetExpr, used = ToSSA optenv used falseTargetExpr

        if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
            expr, used
        else
            let newExpr = E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)
            
            let phiExprs =
                (expr2, used.hashSet)
                ||> Seq.fold (fun expr ssaIndex ->
                    match optenv.ssaenv.GetValue(ssaIndex) with
                    | SsaValue.Local(localIndex) ->
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                    | SsaValue.Argument(argIndex) ->
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                    | SsaValue.None ->
                        expr
                )
            E.Sequential(
                newExpr,
                phiExprs
            ), used

    | E.Sequential(E.Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy), expr2) ->
        let newBodyExpr, used = ToSSA optenv scopes bodyExpr
        let mutable used = used
        let newCatchCases =
            catchCases
            |> ImArray.map (fun catchCase ->
                match catchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, bodyExpr, catchTy) ->
                    let newBodyExpr, newUsed = ToSSA optenv scopes bodyExpr
                    used <- newUsed
                    OlyIRCatchCase.CatchCase(localName, localIndex, newBodyExpr, catchTy)
            )
        let newFinallyBodyExprOpt = 
            finallyBodyExprOpt 
            |> Option.map (fun finallyBodyExpr ->
                let newFinallyBodyExpr, newUsed = ToSSA optenv used finallyBodyExpr
                used <- newUsed
                newFinallyBodyExpr
            )
        let newExpr = E.Try(newBodyExpr, newCatchCases, newFinallyBodyExprOpt, resultTy)
        let phiExprs =
            (expr2, used.hashSet)
            ||> Seq.fold (fun expr ssaIndex ->
                match optenv.ssaenv.GetValue(ssaIndex) with
                | SsaValue.Local(localIndex) ->
                    E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                | SsaValue.Argument(argIndex) ->
                    E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                | SsaValue.None ->
                    expr
            )
        E.Sequential(
            newExpr,
            phiExprs
        ), used

    | E.Sequential(expr1, expr2) ->
        let newExpr1, used = ToSSA optenv scopes expr1
        let newExpr2, used = ToSSA optenv used expr2

        if newExpr1 = expr1 && newExpr2 = expr2 then
            expr, used
        else
            E.Sequential(newExpr1, newExpr2), used
    
    | E.While(conditionExpr, bodyExpr, resultTy) ->
        let newConditionExpr, used = ToSSA optenv scopes conditionExpr
        let newBodyExpr, used = ToSSA optenv used bodyExpr

        if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
            expr, used
        else
            let newExpr = E.While(newConditionExpr, newBodyExpr, resultTy)
            let phiExprs =
                (E.None(OlyIRDebugSourceTextRange.Empty, optenv.emitType(RuntimeType.Void)), used.hashSet)
                ||> Seq.fold (fun expr ssaIndex ->
                    match optenv.ssaenv.GetValue(ssaIndex) with
                    | SsaValue.Local(localIndex) ->
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                    | SsaValue.Argument(argIndex) ->
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                    | SsaValue.None ->
                        expr
                )
            E.Sequential(
                newExpr,
                phiExprs
            ), used

    | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
        let newConditionExpr, used = ToSSA optenv scopes conditionExpr
        let newTrueTargetExpr, used = ToSSA optenv used trueTargetExpr
        let newFalseTargetExpr, used = ToSSA optenv used falseTargetExpr

        if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
            expr, used
        else
            let newExpr = E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)
            
            let phiExprs =
                (E.None(OlyIRDebugSourceTextRange.Empty, newExpr.ResultType), used.hashSet)
                ||> Seq.fold (fun expr ssaIndex ->
                    match optenv.ssaenv.GetValue(ssaIndex) with
                    | SsaValue.Local(localIndex) ->
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                    | SsaValue.Argument(argIndex) ->
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                    | SsaValue.None ->
                        expr
                )
            E.Sequential(
                newExpr,
                phiExprs
            ), used

    | E.Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy) ->
        let newBodyExpr, used = ToSSA optenv scopes bodyExpr
        let mutable used = used
        let newCatchCases =
            catchCases
            |> ImArray.map (fun catchCase ->
                match catchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, bodyExpr, catchTy) ->
                    let newBodyExpr, newUsed = ToSSA optenv scopes bodyExpr
                    used <- newUsed
                    OlyIRCatchCase.CatchCase(localName, localIndex, newBodyExpr, catchTy)
            )
        let newFinallyBodyExprOpt = 
            finallyBodyExprOpt 
            |> Option.map (fun finallyBodyExpr ->
                let newFinallyBodyExpr, newUsed = ToSSA optenv used finallyBodyExpr
                used <- newUsed
                newFinallyBodyExpr
            )
        let newExpr = E.Try(newBodyExpr, newCatchCases, newFinallyBodyExprOpt, resultTy)
        let phiExprs =
            (E.None(OlyIRDebugSourceTextRange.Empty, newExpr.ResultType), used.hashSet)
            ||> Seq.fold (fun expr ssaIndex ->
                match optenv.ssaenv.GetValue(ssaIndex) with
                | SsaValue.Local(localIndex) ->
                    E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                | SsaValue.Argument(argIndex) ->
                    E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex), E.Phi(optenv.emitType(RuntimeType.Void)), expr)
                | SsaValue.None ->
                    expr
            )
        E.Sequential(
            newExpr,
            phiExprs
        ), used

let internal FromSSA (optenv: optenv<_, _, _>) (expr: E<_, _, _>) =
    StackGuard.Do(fun () -> FromSSAAux optenv expr)
let private FromSSAAux (optenv: optenv<_, _, _>) (expr: E<_, _, _>) =
    match expr with
    | E.None _ 
    | E.Phi _ -> expr
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

    | E.Let(_, _, E.Phi _, bodyExpr) ->
        FromSSA optenv bodyExpr

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
