module internal rec Oly.Runtime.CodeGen.Internal.LocalNormalization

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

let private addLocal (optenv: optenv<_, _, _>) (normalizedLocals: ArgumentLocalManager) (localToNormalizedLocalMap: Dictionary<int, int>) localIndex =
    OlyAssert.False(optenv.ssaenv.IsSsa(localIndex))
    let newLocalIndex = normalizedLocals.CreateLocal(optenv.GetLocalFlags(localIndex))
    localToNormalizedLocalMap.Add(localIndex, newLocalIndex)
    newLocalIndex

let private getLocal (localScope: ImmutableHashSet<int>) (localToNormalizedLocalMap: Dictionary<int, int>) localIndex =
    let newLocalIndex = localToNormalizedLocalMap[localIndex]
    if localScope.Contains(newLocalIndex) |> not then
        failwith $"Local '{newLocalIndex}' not in scope."
    newLocalIndex

let private handleOperation (optenv: optenv<_, _, _>) (localScope: ImmutableHashSet<int>) (normalizedLocals: ArgumentLocalManager) (localToNormalizedLocalMap: Dictionary<int, int>) origExpr : E<_, _, _> =
    match origExpr with
    | E.Operation(irTextRange, irOp) ->
#if DEBUG || CHECKED
        match irOp with
        | O.Call(irFunc, argExprs, _) ->
            let func = irFunc.RuntimeFunction
            // Verify use of 'base' calls.
            if func.Flags.IsInstance && func.Flags.IsVirtual && not func.Flags.IsFinal && argExprs.Length > 0 && not(func.EnclosingType.IsAnyStruct) then
                match argExprs[0] with
                | E.Operation(op=O.Upcast(arg=E.Value(value=V.Argument(index=0)))) when func.EnclosingType <> optenv.func.EnclosingType && subsumesType func.EnclosingType optenv.func.EnclosingType ->
                    ()
                | _ ->
                    failwith "Invalid base call after inlining. This is a bug in the runtime."
        | _ ->
            ()
#endif
        let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap argExpr)
        if newOp = irOp then
            let newOp = handleOperationSecondPass localScope localToNormalizedLocalMap irOp
            if newOp = irOp then
                origExpr
            else
                E.Operation(irTextRange, newOp)
        else
            let newOp = 
                newOp
                |> handleOperationSecondPass localScope localToNormalizedLocalMap
            E.Operation(irTextRange, newOp)
    | _ ->
        failwith "invalid expression"

let private handleOperationSecondPass (localScope: ImmutableHashSet<int>) (localToNormalizedLocalMap: Dictionary<int, int>) origOp : O<_, _, _> =
    match origOp with
    | O.Store(localIndex, irRhsExpr, resultTy) ->
        O.Store(getLocal localScope localToNormalizedLocalMap localIndex, irRhsExpr, resultTy)
    | _ ->
        origOp

let private handleLinearExpression (optenv: optenv<_, _, _>) (localScope: ImmutableHashSet<int>) (normalizedLocals: ArgumentLocalManager) (localToNormalizedLocalMap: Dictionary<int, int>) (origExpr: E<_, _, _>) : E<_, _, _> =
    StackGuard.Do(fun () ->
        match origExpr with
        | E.Let(irTextRange, localIndex, irRhsExpr, irBodyExpr) ->
            let irNewRhsExpr =
                match irRhsExpr with
                | E.Let _
                | E.Sequential _ ->
                    handleLinearExpression optenv localScope normalizedLocals localToNormalizedLocalMap irRhsExpr
                | _ ->
                    handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irRhsExpr

            let newLocalIndex = addLocal optenv normalizedLocals localToNormalizedLocalMap localIndex
            let localScope = localScope.Add(newLocalIndex)

            let irNewBodyExpr =
                match irBodyExpr with
                | E.Let _
                | E.Sequential _ ->
                    handleLinearExpression optenv localScope normalizedLocals localToNormalizedLocalMap irBodyExpr
                | _ ->
                    handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irBodyExpr

            if newLocalIndex = localIndex && irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                origExpr
            else
                E.Let(irTextRange, newLocalIndex, irNewRhsExpr, irNewBodyExpr)

        | E.Sequential(irExpr1, irExpr2) ->
            let irNewExpr1 =
                match irExpr1 with
                | E.Let _
                | E.Sequential _ ->
                    handleLinearExpression optenv localScope normalizedLocals localToNormalizedLocalMap irExpr1
                | _ ->
                    handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irExpr1

            let irNewExpr2 =
                match irExpr2 with
                | E.Let _
                | E.Sequential _ ->
                    handleLinearExpression optenv localScope normalizedLocals localToNormalizedLocalMap irExpr2
                | _ ->
                    handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irExpr2

            if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
                origExpr
            else
                E.Sequential(irNewExpr1, irNewExpr2)

        | _ ->
            failwith "Invalid linear expression"
    )

let private handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap origExpr =
    StackGuard.Do(fun () ->
        handleExpressionAuxNormalizeLocals optenv localScope  normalizedLocals localToNormalizedLocalMap origExpr
    )

let private handleExpressionAuxNormalizeLocals (optenv: optenv<_, _, _>) (localScope: ImmutableHashSet<int>) normalizedLocals localToNormalizedLocalMap origExpr : E<_, _, _> =
    match origExpr with
    | E.Let _
    | E.Sequential _ ->
        handleLinearExpression optenv localScope normalizedLocals localToNormalizedLocalMap origExpr

    | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
        let irNewConditionExpr = handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.True, _)) ->
            handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irTrueTargetExpr

        | E.Value(value=V.Constant(C.False, _)) ->
            handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irFalseTargetExpr

        | _ ->
            let irNewTrueTargetExpr = handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irTrueTargetExpr
            let irNewFalseTargetExpr = handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irFalseTargetExpr

            if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                origExpr
            else
                E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

    | E.While(irConditionExpr, irBodyExpr, resultTy) ->
        let irNewConditionExpr = handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.False, _)) ->
            E.None(NoRange, resultTy)
        | _ ->
            let irNewBodyExpr = handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irBodyExpr

            if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                origExpr
            else
                E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

    | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
        let irNewBodyExpr = handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irBodyExpr

        let mutable didChange = false
        let irNewCatchCases =
            irCatchCases
            |> ImArray.map (fun irCatchCase ->
                match irCatchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                    let newLocalIndex = addLocal optenv normalizedLocals localToNormalizedLocalMap localIndex
                    let localScope = localScope.Add(newLocalIndex)
                    let irNewCaseBodyExpr = handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irCaseBodyExpr

                    if newLocalIndex = localIndex && irNewCaseBodyExpr = irCaseBodyExpr then
                        irCatchCase
                    else
                        didChange <- true
                        OlyIRCatchCase.CatchCase(localName, newLocalIndex, irNewCaseBodyExpr, catchTy)
            )

        let irNewFinallyBodyExprOpt =
            irFinallyBodyExprOpt
            |> Option.map (fun irExpr ->
                let irNewExpr = handleExpression optenv localScope normalizedLocals localToNormalizedLocalMap irExpr
                if irNewExpr = irExpr then
                    irExpr
                else
                    didChange <- true
                    irNewExpr
            )

        if irNewBodyExpr = irBodyExpr && not didChange then
            origExpr
        else
            E.Try(irNewBodyExpr, irNewCatchCases, irNewFinallyBodyExprOpt, resultTy)

    | E.Operation _ ->
        handleOperation optenv localScope normalizedLocals localToNormalizedLocalMap origExpr

    | E.Value(irTextRange, V.Local(localIndex, resultTy)) ->
        E.Value(irTextRange, V.Local(getLocal localScope localToNormalizedLocalMap localIndex, resultTy))
    | E.Value(irTextRange, V.LocalAddress(localIndex, irByRefKind, resultTy)) ->
        E.Value(irTextRange, V.LocalAddress(getLocal localScope localToNormalizedLocalMap localIndex, irByRefKind, resultTy))

    | _ ->
        origExpr

let NormalizeLocals (optenv: optenv<_, _, _>) (principalExpr: E<_, _, _>) =
    let normalizedLocals          = ArgumentLocalManager(optenv.GetArgumentFlags(), ResizeArray())
    let localToNormalizedLocalMap = Dictionary<int, int>()

    let finalExpr = handleExpression optenv ImmutableHashSet.Empty normalizedLocals localToNormalizedLocalMap principalExpr
    finalExpr, { optenv with argLocalManager = normalizedLocals }

