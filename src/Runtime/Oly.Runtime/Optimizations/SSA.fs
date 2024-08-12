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

[<NoComparison;NoEquality>]
type SsaUsage =
    {
        localUsed: ImmutableDictionary<int, int>
        argUsed: ImmutableDictionary<int, int>
    }

    static member Default = { localUsed = ImmutableDictionary.Empty; argUsed = ImmutableDictionary.Empty }

    member this.GetSsaLocal(localIndex: int) =
        match this.localUsed.TryGetValue localIndex with
        | true, ssaIndex -> ssaIndex
        | _ -> localIndex

    member this.GetSsaArgument(argIndex: int) =
        match this.argUsed.TryGetValue argIndex with
        | true, ssaIndex -> ssaIndex
        | _ -> argIndex

    member this.SetLocal(localIndex: int, ssaIndex: int) =
        { this with localUsed = this.localUsed.SetItem(localIndex, ssaIndex) }

    member this.SetArgument(argIndex: int, ssaIndex: int) =
        { this with argUsed = this.argUsed.SetItem(argIndex, ssaIndex) }

let internal ToSSA (optenv: optenv<_, _, _>) (used: SsaUsage) (expr: E<_, _, _>) =
    StackGuard.Do(fun () -> ToSSAAux optenv used expr)
let private ToSSAAux (optenv: optenv<_, _, _>) (used: SsaUsage) (expr: E<_, _, _>) =
    match expr with
    | E.Let _
    | E.Sequential _ ->
        handleLinearToSSA optenv used expr

    | E.None _ 
    | E.Phi _ -> expr, used

    | E.Value(textRange, v) ->
        match v with
        | V.Local(localIndex, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromLocal(localIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.Local(ssaIndex, resultTy)), used
            | _ ->
                expr, used
        | V.LocalAddress(localIndex, kind, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromLocal(localIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.LocalAddress(ssaIndex, kind, resultTy)), used
            | _ ->
                expr, used
        | V.Argument(argIndex, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromArgument(argIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.Local(ssaIndex, resultTy)), used
            | _ ->
                expr, used
        | V.ArgumentAddress(argIndex, kind, resultTy) ->
            match optenv.ssaenv.TryGetSsaIndexFromArgument(argIndex) with
            | ValueSome ssaIndex ->
                E.Value(textRange, V.LocalAddress(ssaIndex, kind, resultTy)), used
            | _ ->
                expr, used
        | _ ->
            expr, used

    | E.Operation(_, O.Store(localIndex, rhsExpr, resultTy)) ->
        let rhsExpr2, used = ToSSA optenv used rhsExpr
        let newLocalIndex = optenv.ssaenv.CreateSsaIndexFromLocal(localIndex, rhsExpr2.ResultType)
        let used = used.SetLocal(localIndex, newLocalIndex)
        E.Let("ssa_local", newLocalIndex, rhsExpr2, E.None(OlyIRDebugSourceTextRange.Empty, resultTy)), used

    | E.Operation(_, O.StoreArgument(argIndex, rhsExpr, resultTy)) ->
        let rhsExpr2, used = ToSSA optenv used rhsExpr
        let newLocalIndex = optenv.ssaenv.CreateSsaIndexFromArgument(argIndex, rhsExpr2.ResultType)
        let used = used.SetArgument(argIndex, newLocalIndex)
        E.Let("ssa_arg", newLocalIndex, rhsExpr2, E.None(OlyIRDebugSourceTextRange.Empty, resultTy)), used

    | E.Operation(textRange, op) ->
        let mutable used = used
        let newOp = 
            op.MapAndReplaceArguments(fun _ expr -> 
                let newExpr, newUsed = ToSSA optenv used expr
                used <- newUsed
                newExpr
            )

        if newOp = op then
            expr, used
        else
            E.Operation(textRange, op), used
    
    | E.While(conditionExpr, bodyExpr, resultTy) ->
        let newConditionExpr, used = ToSSA optenv used conditionExpr
        let newBodyExpr, used = ToSSA optenv used bodyExpr

        if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
            expr, used
        else
            E.While(newConditionExpr, newBodyExpr, resultTy), used

    | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
        let newConditionExpr, used = ToSSA optenv used conditionExpr
        let newTrueTargetExpr, used = ToSSA optenv used trueTargetExpr
        let newFalseTargetExpr, used = ToSSA optenv used falseTargetExpr

        if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
            expr, used
        else
            E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy), used

    | E.Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy) ->
        let newBodyExpr, used = ToSSA optenv used bodyExpr
        let mutable used = used
        let newCatchCases =
            catchCases
            |> ImArray.map (fun catchCase ->
                match catchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, bodyExpr, catchTy) ->
                    let newBodyExpr, newUsed = ToSSA optenv used bodyExpr
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
        E.Try(newBodyExpr, newCatchCases, newFinallyBodyExprOpt, resultTy), used

let private handleLinearToSSA (optenv: optenv<_, _, _>) (used: SsaUsage) (expr: E<_, _, _>) =
    match expr with
    | E.Let(name, localIndex, rhsExpr, bodyExpr) ->
        let newRhsExpr, used = ToSSA optenv used rhsExpr

        let used = 
            match optenv.ssaenv.GetValue(localIndex) with
            | SsaValue.UseLocal(nonSsaLocalIndex, _) ->
                used.SetLocal(nonSsaLocalIndex, localIndex)
            | SsaValue.UseArgument(argIndex, _) ->
                used.SetArgument(argIndex, localIndex)
            | SsaValue.Definition ->
                used.SetLocal(localIndex, localIndex)
        let newBodyExpr, used = ToSSA optenv used bodyExpr

        E.Let(name, localIndex, newRhsExpr, newBodyExpr), used

    | E.Sequential(E.Operation(_, O.Store(localIndex, rhsExpr, _)), expr2) ->
        let rhsExpr2, used = ToSSA optenv used rhsExpr
        let newLocalIndex = optenv.ssaenv.CreateSsaIndexFromLocal(localIndex, rhsExpr2.ResultType)
        let used = used.SetLocal(localIndex, newLocalIndex)
        let newExpr2, used = ToSSA optenv used expr2
        E.Let("ssa_local", newLocalIndex, rhsExpr2, newExpr2), used

    | E.Sequential(E.Operation(_, O.StoreArgument(argIndex, rhsExpr, _)), expr2) ->
        let rhsExpr2, used = ToSSA optenv used rhsExpr
        let newLocalIndex = optenv.ssaenv.CreateSsaIndexFromArgument(argIndex, rhsExpr2.ResultType)
        let used = used.SetArgument(argIndex, newLocalIndex)
        let newExpr2, used = ToSSA optenv used expr2
        E.Let("ssa_arg", newLocalIndex, rhsExpr2, newExpr2), used

    | E.Sequential(E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy), expr2) ->
        let newConditionExpr, used = ToSSA optenv used conditionExpr
        let newTrueTargetExpr, usedBranch = ToSSA optenv used trueTargetExpr
        let newFalseTargetExpr, used = ToSSA optenv usedBranch falseTargetExpr

        if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
            expr, used
        else
            let newExpr = E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)
            
            let phiExprs, used =
                (expr2, Seq.append used.argUsed.Values used.localUsed.Values)
                ||> Seq.fold (fun expr ssaIndex ->
                    match optenv.ssaenv.GetValue(ssaIndex) with
                    | SsaValue.UseLocal(localIndex, resultTy) ->
                        let values =
                            seq {
                                usedBranch.GetSsaLocal(localIndex)
                                used.GetSsaLocal(localIndex)
                            }
                            |> Seq.distinct
                            |> ImArray.ofSeq
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex, resultTy), E.Phi(values, resultTy), expr)
                    | SsaValue.UseArgument(argIndex, resultTy) ->
                        let values =
                            seq {
                                usedBranch.GetSsaArgument(argIndex)
                                used.GetSsaArgument(argIndex)
                            }
                            |> Seq.distinct
                            |> ImArray.ofSeq
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex, resultTy), E.Phi(values, resultTy), expr)
                    | SsaValue.Definition ->
                        expr
                )
                |> ToSSA optenv used
            E.Sequential(
                newExpr,
                phiExprs
            ), used

    | E.Sequential(E.Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy), expr2) ->
        let newBodyExpr, used = ToSSA optenv used bodyExpr
        let manyUsed = ResizeArray()
        let mutable used = used
        let newCatchCases =
            catchCases
            |> ImArray.map (fun catchCase ->
                match catchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, bodyExpr, catchTy) ->
                    let newBodyExpr, newUsed = ToSSA optenv used bodyExpr
                    used <- newUsed
                    manyUsed.Add(used)
                    OlyIRCatchCase.CatchCase(localName, localIndex, newBodyExpr, catchTy)
            )
        let newFinallyBodyExprOpt = 
            finallyBodyExprOpt 
            |> Option.map (fun finallyBodyExpr ->
                let newFinallyBodyExpr, newUsed = ToSSA optenv used finallyBodyExpr
                used <- newUsed
                manyUsed.Add(used)
                newFinallyBodyExpr
            )
        let newExpr = E.Try(newBodyExpr, newCatchCases, newFinallyBodyExprOpt, resultTy)
        let phiExprs, used =
            (expr2, Seq.append used.argUsed.Values used.localUsed.Values)
            ||> Seq.fold (fun expr ssaIndex ->
                match optenv.ssaenv.GetValue(ssaIndex) with
                | SsaValue.UseLocal(localIndex, resultTy) ->
                    let values =
                        seq {
                            for usedBranch in manyUsed do
                                usedBranch.GetSsaLocal(localIndex)
                            used.GetSsaLocal(localIndex)
                        }
                        |> Seq.distinct
                        |> ImArray.ofSeq
                    E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex, resultTy), E.Phi(values, resultTy), expr)
                | SsaValue.UseArgument(argIndex, resultTy) ->
                    let values =
                        seq {
                            for usedBranch in manyUsed do
                                usedBranch.GetSsaArgument(argIndex)
                            used.GetSsaLocal(argIndex)
                        }
                        |> Seq.distinct
                        |> ImArray.ofSeq
                    E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex, resultTy), E.Phi(values, resultTy), expr)
                | SsaValue.Definition ->
                    expr
            ) |> ToSSA optenv used
        E.Sequential(
            newExpr,
            phiExprs
        ), used

    | E.Sequential(E.While(conditionExpr, bodyExpr, resultTy), expr2) ->
        let newConditionExpr, used = ToSSA optenv used conditionExpr
        let newBodyExpr, usedBranch = ToSSA optenv used bodyExpr
        let used = usedBranch

        if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
            expr, used
        else
            let newExpr = E.While(newConditionExpr, newBodyExpr, resultTy)
            let phiExprs, used =
                (expr2, Seq.append used.argUsed.Values used.localUsed.Values)
                ||> Seq.fold (fun expr ssaIndex ->
                    match optenv.ssaenv.GetValue(ssaIndex) with
                    | SsaValue.UseLocal(localIndex, resultTy) ->
                        let values =
                            seq {
                                usedBranch.GetSsaLocal(localIndex)
                                used.GetSsaLocal(localIndex)
                            }
                            |> Seq.distinct
                            |> ImArray.ofSeq
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromLocal(localIndex, resultTy), E.Phi(values, resultTy), expr)
                    | SsaValue.UseArgument(argIndex, resultTy) ->
                        let values =
                            seq {
                                usedBranch.GetSsaArgument(argIndex)
                                used.GetSsaArgument(argIndex)
                            }
                            |> Seq.distinct
                            |> ImArray.ofSeq
                        E.Let("ssa_phi", optenv.ssaenv.CreateSsaIndexFromArgument(argIndex, resultTy), E.Phi(values, resultTy), expr)
                    | SsaValue.Definition ->
                        expr
                ) |> ToSSA optenv used
            E.Sequential(
                newExpr,
                phiExprs
            ), used

    | E.Sequential(expr1, expr2) ->
        let newExpr1, used = ToSSA optenv used expr1
        let newExpr2, used = ToSSA optenv used expr2

        if newExpr1 = expr1 && newExpr2 = expr2 then
            expr, used
        else
            E.Sequential(newExpr1, newExpr2), used

    | _ ->
        failwith "Invalid linear expression"


// ------------------------------------------------------

let internal FromSSA (optenv: optenv<_, _, _>) (localDefs: ImmutableHashSet<int>) (expr: E<_, _, _>) =
    StackGuard.Do(fun () -> FromSSAAux optenv localDefs expr)
let private FromSSAAux (optenv: optenv<_, _, _>) (localDefs: ImmutableHashSet<int>) (expr: E<_, _, _>) =
    match expr with
    | E.Let _
    | E.Sequential _ ->
        handleLinearFromSSA optenv localDefs expr

    | E.None _ 
    | E.Phi _ -> expr
    | E.Value(textRange, v) ->
        match v with
        | V.Local(localIndex, resultTy) ->
            match optenv.ssaenv.GetValue(localIndex) with
            | SsaValue.UseLocal(nonSsaLocalIndex, _) ->
                OlyAssert.True(optenv.ssaenv.IsSsaLocal(localIndex))
                OlyAssert.True(localDefs.Contains(nonSsaLocalIndex))
                E.Value(textRange, V.Local(nonSsaLocalIndex, resultTy))
            | SsaValue.UseArgument(argIndex, _) ->
                OlyAssert.True(optenv.ssaenv.IsSsaLocal(localIndex))
                E.Value(textRange, V.Argument(argIndex, resultTy))
            | SsaValue.Definition ->
                OlyAssert.False(optenv.ssaenv.IsSsaLocal(localIndex))
                expr
        | V.LocalAddress(localIndex, kind, resultTy) ->
            match optenv.ssaenv.GetValue(localIndex) with
            | SsaValue.UseLocal(nonSsaLocalIndex, _) ->
                OlyAssert.True(optenv.ssaenv.IsSsaLocal(localIndex))
                OlyAssert.True(localDefs.Contains(nonSsaLocalIndex))
                E.Value(textRange, V.LocalAddress(nonSsaLocalIndex, kind, resultTy))
            | SsaValue.UseArgument(argIndex, _) ->
                OlyAssert.True(optenv.ssaenv.IsSsaLocal(localIndex))
                E.Value(textRange, V.ArgumentAddress(argIndex, kind, resultTy))
            | SsaValue.Definition ->
                OlyAssert.False(optenv.ssaenv.IsSsaLocal(localIndex))
                expr
        | _ ->
            expr

    | E.Operation(textRange, op) ->
        let newOp = op.MapAndReplaceArguments(fun _ expr -> FromSSA optenv localDefs expr)

        if newOp = op then
            expr
        else
            E.Operation(textRange, newOp)
    
    | E.While(conditionExpr, bodyExpr, resultTy) ->
        let newConditionExpr = FromSSA optenv localDefs conditionExpr
        let newBodyExpr = FromSSA optenv localDefs bodyExpr

        if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
            expr
        else
            E.While(newConditionExpr, newBodyExpr, resultTy)

    | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
        let newConditionExpr = FromSSA optenv localDefs conditionExpr
        let newTrueTargetExpr = FromSSA optenv localDefs trueTargetExpr
        let newFalseTargetExpr = FromSSA optenv localDefs falseTargetExpr

        if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
            expr
        else
            E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)

    | E.Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy) ->
        let newBodyExpr = FromSSA optenv localDefs bodyExpr
        let newCatchCases =
            catchCases
            |> ImArray.map (fun catchCase ->
                match catchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, bodyExpr, catchTy) ->
                    OlyIRCatchCase.CatchCase(localName, localIndex, FromSSA optenv localDefs bodyExpr, catchTy)
            )
        let newFinallyBodyExprOpt = finallyBodyExprOpt |> Option.map (FromSSA optenv localDefs)
        E.Try(newBodyExpr, newCatchCases, newFinallyBodyExprOpt, resultTy)

let private handleLinearFromSSA (optenv: optenv<_, _, _>) (localDefs: ImmutableHashSet<int>) (expr: E<_, _, _>) =
    match expr with
    | E.Let(_, localIndex, E.Phi _, bodyExpr) ->
        OlyAssert.True(optenv.ssaenv.IsSsaLocal(localIndex))
        FromSSA optenv localDefs bodyExpr

    | E.Let(name, localIndex, rhsExpr, bodyExpr) ->
        let newRhsExpr = FromSSA optenv localDefs rhsExpr
        match optenv.ssaenv.GetValue(localIndex) with
        | SsaValue.UseLocal(nonSsaLocalIndex, _) ->
            OlyAssert.True(optenv.ssaenv.IsSsaLocal(localIndex))
            OlyAssert.True(localDefs.Contains(nonSsaLocalIndex))
            let newBodyExpr = FromSSA optenv localDefs bodyExpr
            if localDefs.Contains nonSsaLocalIndex then
                let storeExpr = E.Operation((* TODO: bad range *) OlyIRDebugSourceTextRange.Empty, O.Store(nonSsaLocalIndex, newRhsExpr, optenv.emitType(RuntimeType.Void)))
                match newBodyExpr with
                | E.None _ ->
                    storeExpr
                | _ ->
                    E.Sequential(
                        storeExpr,
                        newBodyExpr
                    )
            else
                E.Let("tmp_from_ssa", optenv.argLocalManager.CreateLocal(optenv.argLocalManager.GetLocalFlags(nonSsaLocalIndex)), newRhsExpr, newBodyExpr)
        | SsaValue.UseArgument(argIndex, _) ->
            OlyAssert.True(optenv.ssaenv.IsSsaLocal(localIndex))
            let newBodyExpr = FromSSA optenv localDefs bodyExpr
            let storeExpr = E.Operation((* TODO: bad range *) OlyIRDebugSourceTextRange.Empty, O.StoreArgument(argIndex, newRhsExpr, optenv.emitType(RuntimeType.Void)))
            match newBodyExpr with
            | E.None _ ->
                storeExpr
            | _ ->
                E.Sequential(
                    storeExpr,
                    newBodyExpr
                )
        | SsaValue.Definition ->
            OlyAssert.False(optenv.ssaenv.IsSsaLocal(localIndex))
            OlyAssert.False(localDefs.Contains(localIndex))
            let newBodyExpr = FromSSA optenv (localDefs.Add(localIndex)) bodyExpr
            if newRhsExpr = rhsExpr && newBodyExpr = bodyExpr then
                expr
            else
                E.Let(name, localIndex, newRhsExpr, newBodyExpr)

    | E.Sequential(expr1, expr2) ->
        let newExpr1 = FromSSA optenv localDefs expr1
        let newExpr2 = FromSSA optenv localDefs expr2

        if newExpr1 = expr1 && newExpr2 = expr2 then
            expr
        else
            E.Sequential(newExpr1, newExpr2)

    | _ ->
        failwith "Invalid linear expression"
