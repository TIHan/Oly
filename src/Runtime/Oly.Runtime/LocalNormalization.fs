module internal Oly.Runtime.CodeGen.Internal.LocalNormalization

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

let NormalizeLocals (optenv: optenv<_, _, _>) (principalExpr: E<_, _, _>) =
    let normalizedLocals          = ArgumentLocalManager(optenv.GetArgumentFlags(), ResizeArray())
    let localToNormalizedLocalMap = Dictionary<int, int>()

    let addLocal localIndex =
        let newLocalIndex = normalizedLocals.CreateLocal(optenv.GetLocalFlags(localIndex))
        localToNormalizedLocalMap.Add(localIndex, newLocalIndex)
        newLocalIndex

    let getLocal (localScope: ImmutableHashSet<int>) localIndex =
        let newLocalIndex = localToNormalizedLocalMap[localIndex]
        if localScope.Contains(newLocalIndex) |> not then
            failwith $"Local '{newLocalIndex}' not in scope."
        newLocalIndex

    let handleOperation (localScope: ImmutableHashSet<int>) origOp : O<_, _, _> =
        match origOp with
        | O.Store(localIndex, irRhsExpr, resultTy) ->
            O.Store(getLocal localScope localIndex, irRhsExpr, resultTy)
        | _ ->
            origOp

    let rec handleLinearExpression (localScope: ImmutableHashSet<int>) origExpr : E<_, _, _> =
        StackGuard.Do(fun () ->
            match origExpr with
            | E.Let(irTextRange, localIndex, irRhsExpr, irBodyExpr) ->
                let irNewRhsExpr =
                    match irRhsExpr with
                    | E.Let _
                    | E.Sequential _ ->
                        handleLinearExpression localScope irRhsExpr
                    | _ ->
                        handleExpression localScope irRhsExpr

                let newLocalIndex = addLocal localIndex
                let localScope = localScope.Add(newLocalIndex)

                let irNewBodyExpr =
                    match irBodyExpr with
                    | E.Let _
                    | E.Sequential _ ->
                        handleLinearExpression localScope irBodyExpr
                    | _ ->
                        handleExpression localScope irBodyExpr

                if newLocalIndex = localIndex && irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                    origExpr
                else
                    E.Let(irTextRange, newLocalIndex, irNewRhsExpr, irNewBodyExpr)

            | E.Sequential(irExpr1, irExpr2) ->
                let irNewExpr1 =
                    match irExpr1 with
                    | E.Let _
                    | E.Sequential _ ->
                        handleLinearExpression localScope irExpr1
                    | _ ->
                        handleExpression localScope irExpr1

                let irNewExpr2 =
                    match irExpr2 with
                    | E.Let _
                    | E.Sequential _ ->
                        handleLinearExpression localScope irExpr2
                    | _ ->
                        handleExpression localScope irExpr2

                if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
                    origExpr
                else
                    E.Sequential(irNewExpr1, irNewExpr2)

            | _ ->
                failwith "Invalid linear expression"
        )

    and handleExpression localScope origExpr =
        StackGuard.Do(fun () ->
            handleExpressionAuxNormalizeLocals localScope origExpr
        )

    and handleExpressionAuxNormalizeLocals (localScope: ImmutableHashSet<int>) origExpr : E<_, _, _> =
        match origExpr with
        | E.Let _
        | E.Sequential _ ->
            handleLinearExpression localScope origExpr

        | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
            let irNewConditionExpr = handleExpression localScope irConditionExpr

            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.True, _)) ->
                handleExpression localScope irTrueTargetExpr

            | E.Value(value=V.Constant(C.False, _)) ->
                handleExpression localScope irFalseTargetExpr

            | _ ->
                let irNewTrueTargetExpr = handleExpression localScope irTrueTargetExpr
                let irNewFalseTargetExpr = handleExpression localScope irFalseTargetExpr

                if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                    origExpr
                else
                    E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

        | E.While(irConditionExpr, irBodyExpr, resultTy) ->
            let irNewConditionExpr = handleExpression localScope irConditionExpr

            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.False, _)) ->
                E.None(NoRange, resultTy)
            | _ ->
                let irNewBodyExpr = handleExpression localScope irBodyExpr

                if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                    origExpr
                else
                    E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

        | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
            let irNewBodyExpr = handleExpression localScope irBodyExpr

            let mutable didChange = false
            let irNewCatchCases =
                irCatchCases
                |> ImArray.map (fun irCatchCase ->
                    match irCatchCase with
                    | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                        let newLocalIndex = addLocal localIndex
                        let localScope = localScope.Add(newLocalIndex)
                        let irNewCaseBodyExpr = handleExpression localScope irCaseBodyExpr

                        if newLocalIndex = localIndex && irNewCaseBodyExpr = irCaseBodyExpr then
                            irCatchCase
                        else
                            didChange <- true
                            OlyIRCatchCase.CatchCase(localName, newLocalIndex, irNewCaseBodyExpr, catchTy)
                )

            let irNewFinallyBodyExprOpt =
                irFinallyBodyExprOpt
                |> Option.map (fun irExpr ->
                    let irNewExpr = handleExpression localScope irExpr
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
            //let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> handleExpression localScope argExpr)
            //if newOp = irOp then
            //    let newOp = handleOperation localScope irOp
            //    if newOp = irOp then
            //        origExpr
            //    else
            //        E.Operation(irTextRange, newOp)
            //else
            //    let newOp = 
            //        newOp
            //        |> handleOperation localScope
            //    E.Operation(irTextRange, newOp)
            let irNewArgExprs = irOp.MapArguments(fun _ irArgExpr -> handleExpression localScope irArgExpr)
            let mutable areSame = true
            irOp.ForEachArgument(fun i irArgExpr ->
                if irNewArgExprs[i] <> irArgExpr then
                    areSame <- false
            )
            if areSame then
                let irNewOp = handleOperation localScope irOp
                if irOp = irNewOp then
                    origExpr
                else
                    E.Operation(irTextRange, irNewOp)
            else                    
                let irNewOp = 
                    irOp.ReplaceArguments(irNewArgExprs)
                    |> handleOperation localScope
                E.Operation(irTextRange, irNewOp)

        | E.Value(irTextRange, V.Local(localIndex, resultTy)) ->
            E.Value(irTextRange, V.Local(getLocal localScope localIndex, resultTy))
        | E.Value(irTextRange, V.LocalAddress(localIndex, irByRefKind, resultTy)) ->
            E.Value(irTextRange, V.LocalAddress(getLocal localScope localIndex, irByRefKind, resultTy))

        | _ ->
            origExpr

    let finalExpr = handleExpression ImmutableHashSet.Empty principalExpr
    finalExpr, { optenv with argLocalManager = normalizedLocals }

