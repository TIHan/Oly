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

let And arg1 arg2 resultTy =
    E.IfElse(arg1, arg2, E.Value(NoRange, V.Constant(C.False, resultTy)), resultTy)

let Or arg1 arg2 resultTy =
    E.IfElse(arg1, E.Value(NoRange, V.Constant(C.True, resultTy)), arg2, resultTy)

// -------------------------------------------------------------------------------------------------------------

let OptimizeExpression (optenv: optenv<_, _, _>) (irExpr: E<_, _, _>) : E<_, _, _> =

    let rec optimizeOperation irExpr =
        match irExpr with
        // Makes optimizations easier
        | And(And(argx1, argx2, _), arg2, resultTy) ->
            And argx1 (And argx2 arg2 resultTy) resultTy
            |> optimizeExpression

        // Makes optimizations easier
        | Or(Or(argx1, argx2, _), arg2, resultTy) ->
            Or argx1 (Or argx2 arg2 resultTy) resultTy
            |> optimizeExpression

        | E.Operation(irTextRange, irOp) ->
            match irOp with
            | O.Ignore(irArgExpr, resultTy) ->
                if hasSideEffect optenv irArgExpr then
                    E.Operation(irTextRange, irOp)
                else
                    E.None(NoRange, resultTy)

            | O.StoreToAddress(E.Value(_, V.LocalAddress(localIndex, irByRefKind, _)), irRhsExpr, resultTy) ->
                match irByRefKind with
                | OlyIRByRefKind.Read ->
                    OlyAssert.Fail("Expected read-write byref type.")
                | _ ->
                    ()
                E.Operation(irTextRange, O.Store(localIndex, irRhsExpr, resultTy))

            | O.StoreToAddress(E.Value(_, V.ArgumentAddress(argIndex, irByRefKind, _)), irRhsExpr, resultTy) ->
                match irByRefKind with
                | OlyIRByRefKind.Read ->
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

//#if DEBUG
    and optimizeExpression irExpr : E<_, _, _> =
        DebugStackGuard.Do(fun () ->
            optimizeExpressionCore irExpr
        )

    and optimizeExpressionCore irExpr : E<_, _, _> =
//#else
//    and optimizeExpression irExpr : E<_, _, _> =
//#endif
        match irExpr with
        // Normalize sequential expressions
        | E.Let(name, localIndex, E.Sequential(expr1, expr2), bodyExpr) ->
            E.Sequential(
                expr1,
                E.Let(name, localIndex, expr2, bodyExpr)
            )
            |> optimizeExpression
        | E.Let(name, localIndex, E.Let(name2, localIndex2, rhsExpr2, bodyExpr2), bodyExpr) ->
            E.Let(
                name2,
                localIndex2,
                rhsExpr2,
                E.Let(name, localIndex, bodyExpr2, bodyExpr)
            )
            |> optimizeExpression

        | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
            let irNewRhsExpr = optimizeExpression irRhsExpr
            let irNewBodyExpr = optimizeExpression irBodyExpr

            if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                irExpr
            else
                E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

        | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
            let irNewConditionExpr = optimizeExpression irConditionExpr

            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.True, _)) ->
                optimizeExpression irTrueTargetExpr

            | E.Value(value=V.Constant(C.False, _)) ->
                optimizeExpression irFalseTargetExpr

            | _ ->
                let irNewTrueTargetExpr = optimizeExpression irTrueTargetExpr
                let irNewFalseTargetExpr = optimizeExpression irFalseTargetExpr

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
                        |> optimizeExpression

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
                        ) && not(hasSideEffect optenv irRhsExpr) ->

                    let irNewConditionExpr =
                        And irNewConditionExpr (E.Let(name, localIndex, irRhsExpr, irNestedConditionExpr)) irNewConditionExpr.ResultType

                    E.IfElse(irNewConditionExpr,
                        irNestedTrueTargetExpr,
                        irNewFalseTargetExpr,
                        resultTy
                    )
                    |> optimizeExpression

                | _ ->
                    if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                        irExpr
                    else
                        E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

        | E.While(irConditionExpr, irBodyExpr, resultTy) ->
            let irNewConditionExpr = optimizeExpression irConditionExpr

            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.False, _)) ->
                E.None(NoRange, resultTy)
            | _ ->
                let irNewBodyExpr = optimizeExpression irBodyExpr

                if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                    irExpr
                else
                    E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

        | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
            let irNewBodyExpr = optimizeExpression irBodyExpr

            let mutable didChange = false
            let irNewCatchCases =
                irCatchCases
                |> ImArray.map (fun irCatchCase ->
                    match irCatchCase with
                    | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                        let irNewCaseBodyExpr = optimizeExpression irCaseBodyExpr

                        if irNewCaseBodyExpr = irCaseBodyExpr then
                            irCatchCase
                        else
                            didChange <- true
                            OlyIRCatchCase.CatchCase(localName, localIndex, irNewCaseBodyExpr, catchTy)
                )

            let irNewFinallyBodyExprOpt =
                irFinallyBodyExprOpt
                |> Option.map (fun irExpr ->
                    let irNewExpr = optimizeExpression irExpr
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
            let irNewExpr1 = optimizeExpression irExpr1
            let irNewExpr2 = optimizeExpression irExpr2

            if hasSideEffect optenv irNewExpr1 then
                if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
                    irExpr
                else
                    E.Sequential(irNewExpr1, irNewExpr2)
            else
                irNewExpr2

        | E.Operation(irTextRange, irOp) ->
            let irNewArgExprs = irOp.MapArguments(fun _ irArgExpr -> optimizeExpression irArgExpr)
            let mutable areSame = true
            irOp.ForEachArgument(fun i irArgExpr ->
                if irNewArgExprs[i] <> irArgExpr then
                    areSame <- false
            )
            if areSame then
                optimizeOperation irExpr
            else
                let irNewOp = irOp.ReplaceArguments(irNewArgExprs) 
                E.Operation(irTextRange, irNewOp)
                |> optimizeOperation

        | _ ->
            irExpr

    optimizeExpression irExpr

// -------------------------------------------------------------------------------------------------------------

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type CopyPropagationItem<'Type, 'Function, 'Field> =
    | Constant of C<'Type, 'Function>
    | Local of localIndex: int
    | LocalAddress of localIndex: int * irByRefKind: OlyIRByRefKind
    | Argument of argIndex: int
    | LoadField of irField: OlyIRField<'Type, 'Function, 'Field> * irReceiverExpr: E<'Type, 'Function, 'Field>
    | LoadFunction of irFunc: OlyIRFunction<'Type, 'Function, 'Field> * irArgExpr: E<'Type, 'Function, 'Field>
    | NewTuple of CopyPropagationItem<'Type, 'Function, 'Field> option imarray

let tryGetPropagatedExpressionByLocal optenv (items: Dictionary<int, CopyPropagationItem<_, _, _>>) (localIndex: int, resultTy) =
    match items.TryGetValue localIndex with
    | true, item ->
        let rec tryGet item =
            match item with
            | CopyPropagationItem.Constant c ->
                E.Value(NoRange, V.Constant(c, resultTy))
                |> Some
            | CopyPropagationItem.Local(localIndex) ->
                match tryGetPropagatedExpressionByLocal optenv items (localIndex, resultTy) with
                | Some(irExpr) -> irExpr |> Some
                | _ ->
                    E.Value(NoRange, V.Local(localIndex, resultTy))
                    |> Some
            | CopyPropagationItem.Argument(argIndex) ->
                E.Value(NoRange, V.Argument(argIndex, resultTy))
                |> Some
            | CopyPropagationItem.LoadField(irField, irReceiverExpr) ->
                let irReceiverExpr = copyPropagationOptimizeExpression optenv items irReceiverExpr
                E.Operation(NoRange, O.LoadField(irField, irReceiverExpr, resultTy))
                |> Some
            | CopyPropagationItem.LocalAddress _ ->
                // Conservative
                None
            | CopyPropagationItem.NewTuple _
            | CopyPropagationItem.LoadFunction _ ->
                None
        tryGet item
    | _ ->
        None

let tryGetPropagatedExpressionByLocalAddress (items: Dictionary<int, CopyPropagationItem<_, _, _>>) (localIndex: int, irByRefKind, resultTy) =
    match items.TryGetValue localIndex with
    | true, item ->
        match item with
        | CopyPropagationItem.Local(localIndex) ->
            match tryGetPropagatedExpressionByLocalAddress items (localIndex, irByRefKind, resultTy) with
            | Some(irExpr) -> irExpr |> Some
            | _ ->
                E.Value(NoRange, V.LocalAddress(localIndex, irByRefKind, resultTy))
                |> Some
        | CopyPropagationItem.Argument(argIndex) ->
            E.Value(NoRange, V.ArgumentAddress(argIndex, irByRefKind, resultTy))
            |> Some
        | CopyPropagationItem.LocalAddress _ ->
            // Conservative
            None
        | CopyPropagationItem.LoadField _ ->
            None // Conservative
        | CopyPropagationItem.NewTuple _
        | CopyPropagationItem.Constant _ 
        | CopyPropagationItem.LoadFunction _ ->
            None
    | _ ->
        None

let tryGetPropagatedExpressionByLoadFromAddressLocal optenv (items: Dictionary<int, CopyPropagationItem<_, _, _>>) (localIndex: int, resultTy) =
    match items.TryGetValue localIndex with
    | true, item ->
        match item with
        | CopyPropagationItem.LocalAddress(localIndex, OlyIRByRefKind.Read) ->
            tryGetPropagatedExpressionByLocal optenv items (localIndex, resultTy)
        | _ ->
            None
    | _ ->
        None

let copyPropagationOptimizeExpression optenv (items: Dictionary<int, CopyPropagationItem<_, _, _>>) irExpr =
    match irExpr with
    | E.Value(value=V.Local(localIndex, resultTy)) ->
        match tryGetPropagatedExpressionByLocal optenv items (localIndex, resultTy) with
        | Some irExpr -> irExpr
        | _ -> irExpr

    | E.Value(value=V.LocalAddress(localIndex, irByRefKind, resultTy)) ->
        match tryGetPropagatedExpressionByLocalAddress items (localIndex, irByRefKind, resultTy) with
        | Some irExpr -> irExpr
        | _ -> irExpr

    | E.Operation(op=O.LoadTupleElement(E.Value(value=V.Local(localIndex, _)), index, resultTy)) ->
        match items.TryGetValue(localIndex) with
        | true, CopyPropagationItem.NewTuple(innerItems) ->
            match innerItems[index] with
            | Some(CopyPropagationItem.Local(localIndex2)) ->
                match tryGetPropagatedExpressionByLocal optenv items (localIndex2, resultTy) with
                | Some(irNewExpr) -> irNewExpr
                | _ -> E.Value(NoRange, V.Local(localIndex2, resultTy))
            | Some(CopyPropagationItem.Argument(argIndex)) ->
                E.Value(NoRange, V.Argument(argIndex, resultTy))
            | Some(CopyPropagationItem.LoadField(irField, irReceiverExpr)) ->
                let irReceiverExpr = OptimizeExpression optenv irReceiverExpr
                E.Operation(NoRange, O.LoadField(irField, irReceiverExpr, resultTy))
            | Some(CopyPropagationItem.Constant(c)) ->
                E.Value(NoRange, OlyIRValue.Constant(c, resultTy))
            | _ ->
                irExpr
        | _ ->
            irExpr

    | _ ->
        irExpr

let CopyPropagation (optenv: optenv<_, _, _>) (irExpr: E<_, _, _>) =
    let items = Dictionary<int, CopyPropagationItem<_, _, _>>()

    let handleOperation irExpr : E<_, _, _> =
        match irExpr with
        | E.Operation(op=irOp) ->
            match irOp with
            | O.LoadFromAddress(E.Value(value=V.Local(localIndex, _)), resultTy) ->
                match tryGetPropagatedExpressionByLoadFromAddressLocal optenv items (localIndex, resultTy) with
                | Some(irNewExpr) ->
                    irNewExpr
                | _ ->
                    irExpr
            | _ ->
                irExpr
        | _ ->
            OlyAssert.Fail("Expected operation")

    let rec handleExpression irExpr : E<_, _, _> =
        DebugStackGuard.Do(fun () ->
            handleExpressionAux irExpr
        )

    and handleExpressionAux irExpr : E<_, _, _> =
        let irExpr = copyPropagationOptimizeExpression optenv items irExpr
        match irExpr with
        | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
            let irNewRhsExpr = handleExpression irRhsExpr

            if optenv.IsLocalMutable(localIndex) |> not then
                match irNewRhsExpr with
                | E.Value(value=V.Local(localIndexToPropagate, _)) when optenv.CanPropagateLocal localIndexToPropagate ->
                    items.Add(localIndex, CopyPropagationItem.Local(localIndexToPropagate))
                    handleExpression irBodyExpr

                | E.Value(value=V.Argument(argIndexToPropagate, _)) when optenv.CanPropagateArgument argIndexToPropagate ->
                    items.Add(localIndex, CopyPropagationItem.Argument(argIndexToPropagate))
                    handleExpression irBodyExpr

                | E.Value(value=V.Constant(c, _)) ->
                    items.Add(localIndex, CopyPropagationItem.Constant(c))
                    let irNewBodyExpr = handleExpression irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                | E.Operation(op=O.Upcast(irArgExpr, castTy)) ->
                    match irArgExpr with

                    // Handles the case where we have a constant enum type being upcasted to its base type.
                    // TODO: Maybe we should just have an IR ConstantEnum value like in IL.
                    | E.Value(value=V.Constant(c, _)) ->
                        items.Add(localIndex, CopyPropagationItem.Constant(c))
                        let irNewBodyExpr = handleExpression irBodyExpr

                        if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                            irExpr
                        else
                            E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                    | _ ->
                        let irNewBodyExpr = handleExpression irBodyExpr

                        if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                            irExpr
                        else
                            E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                | E.Operation(op=O.LoadField(irField, irReceiverExpr, _)) when not irField.IsMutable ->
                    match irReceiverExpr with
                    | E.Value(value=V.Local(localIndexToPropagate, _)) when optenv.CanPropagateLocal localIndexToPropagate ->
                        items.Add(localIndex, CopyPropagationItem.LoadField(irField, irReceiverExpr))
                        let irNewBodyExpr = handleExpression irBodyExpr

                        if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                            irExpr
                        else
                            E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                    | E.Value(value=V.Argument(argIndexToPropagate, _)) when optenv.CanPropagateArgument argIndexToPropagate ->
                        items.Add(localIndex, CopyPropagationItem.LoadField(irField, irReceiverExpr))
                        let irNewBodyExpr = handleExpression irBodyExpr

                        if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                            irExpr
                        else
                            E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                    | _ ->
                        let irNewBodyExpr = handleExpression irBodyExpr

                        if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                            irExpr
                        else
                            E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                | E.Operation(op=O.NewTuple(_, irArgExprs, _)) ->
                    let itemsToAdd =
                        irArgExprs
                        |> ImArray.map (fun irArgExpr ->
                            match irArgExpr with
                            | E.Value(value=V.Local(localIndexToPropagate, _)) when optenv.CanPropagateLocal localIndexToPropagate ->
                                Some(CopyPropagationItem.Local(localIndexToPropagate))
                            | E.Value(value=V.Argument(argIndexToPropagate, _)) when optenv.CanPropagateArgument argIndexToPropagate ->
                                Some(CopyPropagationItem.Argument(argIndexToPropagate))
                            | E.Operation(op=O.LoadField(irField, irReceiverExpr, _)) when not irField.IsMutable ->
                                match irReceiverExpr with
                                | E.Value(value=V.Local(localIndexToPropagate, _)) when optenv.CanPropagateLocal localIndexToPropagate ->
                                    Some(CopyPropagationItem.LoadField(irField, irReceiverExpr))

                                | E.Value(value=V.Argument(argIndexToPropagate, _)) when optenv.CanPropagateArgument argIndexToPropagate ->
                                    Some(CopyPropagationItem.LoadField(irField, irReceiverExpr))

                                | E.Operation(op=O.LoadField(irField2, irReceiverExpr2, _)) when not irField2.IsMutable ->
                                    match irReceiverExpr2 with
                                    | E.Value(value=V.Local(localIndexToPropagate, _)) when optenv.CanPropagateLocal localIndexToPropagate ->
                                        Some(CopyPropagationItem.LoadField(irField, irReceiverExpr))

                                    | E.Value(value=V.Argument(argIndexToPropagate, _)) when optenv.CanPropagateArgument argIndexToPropagate ->
                                        Some(CopyPropagationItem.LoadField(irField, irReceiverExpr))

                                    | _ ->
                                        None

                                | _ ->
                                    None

                            | E.Value(value=V.Constant(c, _)) ->
                                Some(CopyPropagationItem.Constant(c))

                            | _ ->
                                None
                        )
                    items.Add(localIndex, CopyPropagationItem.NewTuple(itemsToAdd))

                    let irNewBodyExpr = handleExpression irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                | E.Operation(op=O.LoadFunction(irFunc, irArgExpr, _)) when not(hasSideEffect optenv irArgExpr) ->
                    items.Add(localIndex, CopyPropagationItem.LoadFunction(irFunc, irArgExpr))

                    let irNewBodyExpr = handleExpression irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                | _ ->
                    let irNewBodyExpr = handleExpression irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)
            else
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
                irExpr
                |> handleOperation
            else
                let irNewOp = irOp.ReplaceArguments(irNewArgExprs)
                E.Operation(irTextRange, irNewOp)
                |> handleOperation

        | _ ->
            irExpr

    handleExpression irExpr

// -------------------------------------------------------------------------------------------------------------

let cseEquals irExpr1 irExpr2 =
    match irExpr1, irExpr2 with
    | E.Value(value=irValue1), E.Value(value=irValue2) ->
        match irValue1, irValue2 with
        | V.Local(localIndex1, _), V.Local(localIndex2, _) ->
            localIndex1 = localIndex2
        | V.Argument(argIndex1, _), V.Argument(argIndex2, _) ->
            argIndex1 = argIndex2
        | _ ->
            false
    | E.Operation(op=irOp1), E.Operation(op=irOp2) ->
        match irOp1, irOp2 with
        | O.Equal(E.Value(value=V.Local(localIndex1, _)), E.Value(value=V.Null _), _), 
          O.Equal(E.Value(value=V.Local(localIndex2, _)), E.Value(value=V.Null _), _) ->
            localIndex1 = localIndex2

        | O.NotEqual(E.Value(value=V.Local(localIndex1, _)), E.Value(value=V.Null _), _), 
          O.NotEqual(E.Value(value=V.Local(localIndex2, _)), E.Value(value=V.Null _), _) ->
            localIndex1 = localIndex2

        | O.Equal(E.Value(value=V.Argument(argIndex1, _)), E.Value(value=V.Null _), _), 
          O.Equal(E.Value(value=V.Argument(argIndex2, _)), E.Value(value=V.Null _), _) ->
            argIndex1 = argIndex2

        | O.NotEqual(E.Value(value=V.Argument(argIndex1, _)), E.Value(value=V.Null _), _), 
          O.NotEqual(E.Value(value=V.Argument(argIndex2, _)), E.Value(value=V.Null _), _) ->
            argIndex1 = argIndex2

        | O.LoadField(irField1, irReceiverExpr1, _),
          O.LoadField(irField2, irReceiverExpr2, _) when OlyIRField.AreEqual(irField1, irField2) ->
          cseEquals irReceiverExpr1 irReceiverExpr2


        //| O.New(func1, argExprs1, _),
        //  O.New(func2, argExprs2, _) 
        //        when func1.IsClosureInstanceConstructor && func2.IsClosureInstanceConstructor &&
        //             func1.RuntimeFunction = func2.RuntimeFunction &&
        //             argExprs1.Length = argExprs2.Length &&
        //             (argExprs1, argExprs2)
        //             ||> ImArray.forall2 (cseEquals) ->
        //    true

        | _ ->
            false
    | _ ->
        false

let cseComparer<'Type, 'Function, 'Field> =
    { new IEqualityComparer<E<'Type, 'Function, 'Field>> with
        member _.GetHashCode(irExpr) =
            0
        member _.Equals(irExpr1, irExpr2) =
            cseEquals irExpr1 irExpr2
    }

[<NoEquality;NoComparison>]
type CseEnv<'Type, 'Function, 'Field> =
    {
        Subexpressions: ImmutableDictionary<E<'Type, 'Function, 'Field>, E<'Type, 'Function, 'Field>>
    }

    static member Default =
        let comparer = cseComparer<'Type, 'Function, 'Field>
        {
            Subexpressions = ImmutableDictionary.Create<_, _>(keyComparer = comparer)
        }

    member this.Contains(irExpr) =
        this.Subexpressions.Contains(irExpr)

    member this.Set(irExpr, irSafeExpr) =
        { this with
            Subexpressions = this.Subexpressions.SetItem(irExpr, irSafeExpr)         
        }

    member this.TryGetValue(irExpr, value: outref<_>) =
        this.Subexpressions.TryGetValue(irExpr, &value)

let CommonSubexpressionElimination (optenv: optenv<_, _, _>) (irExpr: E<_, _, _>) =
    
    let rec handleOperation (env: CseEnv<_, _, _>) irExpr =
        match irExpr with
        | E.Operation(_, irOp) ->
            match irOp with
            | O.LoadField _ ->
                match env.TryGetValue(irExpr) with
                | true, irNewExpr -> 
                    handleExpression env irNewExpr
                | _ -> 
                    irExpr

            //| O.New _ ->
            //    match env.TryGetValue(irExpr) with
            //    | true, irNewExpr ->
            //        handleExpression env irNewExpr
            //    | _ ->
            //        irExpr

            | _ ->
                irExpr
        | _ ->
            OlyAssert.Fail("Expected opertion")

    and handleExpression irExpr =
        DebugStackGuard.Do(fun () ->
            handleExpressionAux irExpr
        )
    
    and handleExpressionAux (env: CseEnv<_, _, _>) irExpr : E<_, _, _> =
        match irExpr with
        | E.Let(name, letLocalIndex, irRhsExpr, irBodyExpr) ->
            let irNewRhsExpr = handleExpression env irRhsExpr

            let newEnv =
                if hasSideEffect optenv irNewRhsExpr then
                    env
                else
                    match irNewRhsExpr with
                    | E.Operation(op=O.LoadField(irField, irReceiverExpr, resultTy)) when not irField.IsMutable ->
                        match irReceiverExpr with
                        | E.Value(value=V.Local(localIndex, _)) when optenv.IsLocalMutable(localIndex) |> not ->
                            env.Set(irNewRhsExpr, E.Value(NoRange, V.Local(letLocalIndex, resultTy)))
                        | E.Value(value=V.Argument(argIndex, _)) when optenv.IsArgumentMutable(argIndex) |> not ->
                            env.Set(irNewRhsExpr, E.Value(NoRange, V.Local(letLocalIndex, resultTy)))
                        | E.Operation(op=O.LoadField(irField2, irReceiverExpr2, _)) when not irField2.IsMutable ->
                            match irReceiverExpr2 with
                            | E.Value(value=V.Local(localIndex, _)) when optenv.IsLocalMutable(localIndex) |> not ->
                                env.Set(irNewRhsExpr, E.Value(NoRange, V.Local(letLocalIndex, resultTy)))
                            | E.Value(value=V.Argument(argIndex, _)) when optenv.IsArgumentMutable(argIndex) |> not ->
                                env.Set(irNewRhsExpr, E.Value(NoRange, V.Local(letLocalIndex, resultTy)))
                            | _ ->
                                env
                        | _ ->
                            env
                    | _ ->
                        env

            let irNewBodyExpr = handleExpression newEnv irBodyExpr
    
            if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                irExpr
            else
                E.Let(name, letLocalIndex, irNewRhsExpr, irNewBodyExpr)
    
        | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
            let irNewConditionExpr = handleExpression env irConditionExpr
    
            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.True, _)) ->
                handleExpression env irTrueTargetExpr
    
            | E.Value(value=V.Constant(C.False, _)) ->
                handleExpression env irFalseTargetExpr
    
            | _ ->
                let irNewTrueTargetExpr = handleExpression env irTrueTargetExpr
                let irNewFalseTargetExpr = handleExpression env irFalseTargetExpr
    
                if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                    irExpr
                else
                    E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)
    
        | E.While(irConditionExpr, irBodyExpr, resultTy) ->
            let irNewConditionExpr = handleExpression env irConditionExpr
    
            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.False, _)) ->
                E.None(NoRange, resultTy)
            | _ ->
                let irNewBodyExpr = handleExpression env irBodyExpr
    
                if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                    irExpr
                else
                    E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

        | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
            let irNewBodyExpr = handleExpression env irBodyExpr

            let mutable didChange = false
            let irNewCatchCases =
                irCatchCases
                |> ImArray.map (fun irCatchCase ->
                    match irCatchCase with
                    | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                        let irNewCaseBodyExpr = handleExpression env irCaseBodyExpr

                        if irNewCaseBodyExpr = irCaseBodyExpr then
                            irCatchCase
                        else
                            didChange <- true
                            OlyIRCatchCase.CatchCase(localName, localIndex, irNewCaseBodyExpr, catchTy)
                )

            let irNewFinallyBodyExprOpt =
                irFinallyBodyExprOpt
                |> Option.map (fun irExpr ->
                    let irNewExpr = handleExpression env irExpr
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
            let hoist = List()

            let irNewExpr1 = handleExpression env irExpr1

            //let env, irNewExpr1 =
            //    match irNewExpr1 with
            //    | E.Operation(irTextRange, op) ->
            //        let mutable env = env
            //        let irNewArgExprs =
            //            op.MapArguments(fun _ irArgExpr ->
            //                match irArgExpr with
            //                | E.Operation(_, op) ->
            //                    match op with
            //                    | O.New(func, _, _) when func.IsClosureInstanceConstructor && canSafelyPropagateForNewClosure optenv irArgExpr ->
            //                        match env.TryGetValue(irArgExpr) with
            //                        | true, irNewExpr ->
            //                            handleExpression env irNewExpr
            //                        | _ ->
            //                            let localIndex = optenv.CreateLocal(OlyIRLocalFlags.None)
            //                            let localExpr = E.Value(NoRange, V.Local(localIndex, irExpr.ResultType))
            //                            hoist.Add((localIndex, irArgExpr))
            //                            env <- env.Set(irArgExpr, localExpr)
            //                            localExpr
            //                    | _ ->
            //                        irArgExpr
            //                | _ ->
            //                    irArgExpr
            //            )
            //        env, E.Operation(irTextRange, op.ReplaceArguments(irNewArgExprs))
            //    | _ ->
            //        env, irNewExpr1

            let irNewExpr1 =
                (hoist, irNewExpr1)
                ||> Seq.foldBack (fun (localIndex, rhsExpr) expr ->
                    E.Let("cse", localIndex, rhsExpr,
                        expr
                    )
                )

            let irNewExpr2 = handleExpression env irExpr2
    
            if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
                irExpr
            else
                E.Sequential(irNewExpr1, irNewExpr2)
    
        | E.Operation(irTextRange, irOp) ->
            let irNewArgExprs = irOp.MapArguments(fun _ irArgExpr -> handleExpression env irArgExpr)
            let mutable areSame = true
            irOp.ForEachArgument(fun i irArgExpr ->
                if irNewArgExprs[i] <> irArgExpr then
                    areSame <- false
            )
            if areSame then
                handleOperation env irExpr
            else
                let irNewOp = irOp.ReplaceArguments(irNewArgExprs) 
                E.Operation(irTextRange, irNewOp)
                |> handleOperation env
    
        | _ ->
            irExpr    
    handleExpression CseEnv.Default irExpr

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
        DebugStackGuard.Do(fun () ->
            handleExpressionAux irExpr
        )

    and handleExpressionAux irExpr : E<_, _, _> =
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

// -------------------------------------------------------------------------------------------------------------

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

    and handleExpression (localScope: ImmutableHashSet<int>) origExpr : E<_, _, _> =
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
#if DEBUG
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
        }
        
    let optimizationPass (optenv: optenv<_, _, _>) irExpr =
        if optenv.IsDebuggable then
            irExpr
        else
            irExpr
            |> OptimizeExpression optenv  
            |> CopyPropagation optenv
            |> CommonSubexpressionElimination optenv
            |> AssertionPropagation optenv
            |> DeadCodeElimination optenv

    let irOptimizedExpr = 
        let mutable irNewExpr = InlineFunctions optenv irExpr
        for i = 1 to 3 do // 3 passes
            irNewExpr <- optimizationPass optenv irNewExpr
        irNewExpr

    let irOptimizedExpr, optenv = 
        NormalizeLocals optenv irOptimizedExpr

    let irLocalFlags = optenv.GetLocalFlags()
    let irArgFlags = optenv.GetArgumentFlags()

    //if optenv.IsDebuggable then
    //    System.IO.File.WriteAllText($"{optenv.func.EnclosingType.Name}_{optenv.func.Name}_debug.oly-ir", Dump.DumpExpression irOptimizedExpr)
    //else
    //    System.IO.File.WriteAllText($"{optenv.func.EnclosingType.Name}_{optenv.func.Name}.oly-ir", Dump.DumpExpression irOptimizedExpr)

    OlyIRFunctionBody<'Type, 'Function, 'Field>(irOptimizedExpr, irArgFlags, irLocalFlags)

// -------------------------------------------------------------------------------------------------------------

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type AssertionPropagationKey<'Type, 'Function, 'Field> =
    | Local of localIndex: int
    | Argument of argIndex: int
    | LocalLoadField of localIndex: int * irField: OlyIRField<'Type, 'Function, 'Field>

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type AssertionPropagationValue<'Type, 'Function> =
    | IsNull
    | IsNotNull
    | IsEqualTo of C<'Type, 'Function>
    | IsNotEqualTo of C<'Type, 'Function>

let assertionPropagationComparer<'Type, 'Function, 'Field> =
    { new IEqualityComparer<AssertionPropagationKey<'Type, 'Function, 'Field>> with
        member _.GetHashCode(_) =
            0 // TODO: handle this for perf reasons
        member _.Equals(x, y) =
            match x, y with
            | AssertionPropagationKey.Local(localIndex1), AssertionPropagationKey.Local(localIndex2) ->
                localIndex1 = localIndex2
            | AssertionPropagationKey.Argument(argIndex1), AssertionPropagationKey.Argument(argIndex2) ->
                argIndex1 = argIndex2
            | AssertionPropagationKey.LocalLoadField(localIndex1, irField1), AssertionPropagationKey.LocalLoadField(localIndex2, irField2) ->
                localIndex1 = localIndex2 &&
                OlyIRField.AreEqual(irField1, irField2)
            | _ ->
                false
    }

[<NoEquality;NoComparison>]
type AssertionEnv<'Type, 'Function, 'Field> =
    {
        Assertions: ImmutableDictionary<AssertionPropagationKey<'Type, 'Function, 'Field>, AssertionPropagationValue<'Type, 'Function>>
    }

    static member Create() =
        let comparer = assertionPropagationComparer<'Type, 'Function, 'Field>
        let env =
            {
                Assertions = ImmutableDictionary.Create<_, _>(keyComparer = comparer)
            }
        env

    member this.Contains(irExpr) =
        this.Assertions.ContainsKey(irExpr)

    member this.Set(irExpr, irSafeExpr) =
        { this with
            Assertions = this.Assertions.SetItem(irExpr, irSafeExpr)         
        }

    member this.TryGetValue(irExpr, value: outref<_>) =
        this.Assertions.TryGetValue(irExpr, &value)

let AssertionPropagation (optenv: optenv<'Type, 'Function, 'Field>) (irExpr: E<'Type, 'Function, 'Field>) : E<'Type, 'Function, 'Field> =
    let env = AssertionEnv<'Type, 'Function, 'Field>.Create()
    assertionPropagateExpression optenv env irExpr

let tryGetAssertionKeyAndValue (optenv: optenv<'Type, 'Function, 'Field>) (irOp: O<'Type, 'Function, 'Field>) =
    match irOp with
    | O.Equal(E.Value(value=V.Local(localIndex, _)), E.Value(value=V.Null _), _) when optenv.IsLocalMutable(localIndex) |> not ->
        (AssertionPropagationKey.Local(localIndex), AssertionPropagationValue.IsNull)
        |> Some

    | O.Equal(E.Value(value=V.Argument(argIndex, _)), E.Value(value=V.Null _), _) when optenv.IsArgumentMutable(argIndex) |> not ->
        (AssertionPropagationKey.Argument(argIndex), AssertionPropagationValue.IsNull)
        |> Some

    | O.NotEqual(E.Value(value=V.Local(localIndex, _)), E.Value(value=V.Null _), _) when optenv.IsLocalMutable(localIndex) |> not ->
        (AssertionPropagationKey.Local(localIndex), AssertionPropagationValue.IsNotNull)
        |> Some

    | O.NotEqual(E.Value(value=V.Argument(argIndex, _)), E.Value(value=V.Null _), _) when optenv.IsArgumentMutable(argIndex) |> not ->
        (AssertionPropagationKey.Argument(argIndex), AssertionPropagationValue.IsNotNull)
        |> Some

    | O.Equal(irArgExpr1, irArgExpr2, _) ->
        match irArgExpr1 with
        | E.Value(value=V.Local(localIndex, _)) when optenv.IsLocalMutable(localIndex) |> not ->
            match irArgExpr2 with
            | E.Value(value=V.Constant(c, _)) ->
                match c with
                | C.Int32 _ ->
                    (AssertionPropagationKey.Local(localIndex), AssertionPropagationValue.IsEqualTo(c))
                    |> Some
                | _ ->
                    None
            | _ ->
                None

        | E.Operation(op=O.LoadField(irField, E.Value(value=V.Local(localIndex, _)), _)) when not irField.IsMutable && not(optenv.IsLocalMutable(localIndex)) ->
            match irArgExpr2 with
            | E.Value(value=V.Constant(c, _)) ->
                match c with
                | C.Int32 _ ->
                    (AssertionPropagationKey.LocalLoadField(localIndex, irField), AssertionPropagationValue.IsEqualTo(c))
                    |> Some
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None

    | _ ->
        None

let recordConditionExpression (optenv: optenv<'Type, 'Function, 'Field>) (env: AssertionEnv<'Type, 'Function, 'Field>) (irExpr: E<'Type, 'Function, 'Field>) : (AssertionEnv<'Type, 'Function, 'Field> * AssertionEnv<'Type, 'Function, 'Field>) =
    match irExpr with
    | E.Operation(op=irOp) ->
        match tryGetAssertionKeyAndValue optenv irOp with
        | Some(key, value) ->
            let trueEnv = env.Set(key, value)
            let falseEnv =
                let value =
                    match value with
                    | AssertionPropagationValue.IsNull ->
                        AssertionPropagationValue.IsNotNull
                    | AssertionPropagationValue.IsNotNull ->
                        AssertionPropagationValue.IsNull
                    | AssertionPropagationValue.IsEqualTo(c) ->
                        AssertionPropagationValue.IsNotEqualTo(c)
                    | AssertionPropagationValue.IsNotEqualTo(c) ->
                        AssertionPropagationValue.IsEqualTo(c)
                env.Set(key, value)
            trueEnv, falseEnv
        | _ ->
            env, env
    | _ ->
        env, env

let optimizeAssertionPropagateExpression (optenv: optenv<'Type, 'Function, 'Field>) (origEnv: AssertionEnv<'Type, 'Function, 'Field>) (irExpr: E<'Type, 'Function, 'Field>) : E<'Type, 'Function, 'Field> =
    match irExpr with
    | E.Operation(op=irOp) ->
        match tryGetAssertionKeyAndValue optenv irOp with
        | Some(key, currentValue) ->
            match origEnv.TryGetValue(key) with
            | true, value ->
                match currentValue, value with
                | AssertionPropagationValue.IsNull, AssertionPropagationValue.IsNull
                | AssertionPropagationValue.IsNotNull, AssertionPropagationValue.IsNotNull ->
                    E.Value(NoRange, V.Constant(C.True, irOp.ResultType))
                | AssertionPropagationValue.IsNull, AssertionPropagationValue.IsNotNull
                | AssertionPropagationValue.IsNotNull, AssertionPropagationValue.IsNull ->
                    E.Value(NoRange, V.Constant(C.False, irOp.ResultType))
                | AssertionPropagationValue.IsEqualTo(expectedC), AssertionPropagationValue.IsEqualTo(c)
                | AssertionPropagationValue.IsNotEqualTo(expectedC), AssertionPropagationValue.IsNotEqualTo(c)->
                    match expectedC, c with
                    | C.Int32(expectedValue), C.Int32(value) ->
                        if expectedValue = value then
                            E.Value(NoRange, V.Constant(C.True, irOp.ResultType))
                        else
                            E.Value(NoRange, V.Constant(C.False, irOp.ResultType))
                    | _ ->
                        irExpr

                | AssertionPropagationValue.IsEqualTo(expectedC), AssertionPropagationValue.IsNotEqualTo(c)
                | AssertionPropagationValue.IsNotEqualTo(expectedC), AssertionPropagationValue.IsEqualTo(c)->
                    match expectedC, c with
                    | C.Int32(expectedValue), C.Int32(value) ->
                        if expectedValue = value then
                            E.Value(NoRange, V.Constant(C.False, irOp.ResultType))
                        else
                            irExpr
                    | _ ->
                        irExpr

                | _ ->
                    irExpr
            | _ ->
                irExpr
        | _ ->
            irExpr
    | _ ->
        irExpr  

let assertionPropagateExpression (optenv: optenv<'Type, 'Function, 'Field>) (origEnv: AssertionEnv<'Type, 'Function, 'Field>) (irExpr: E<'Type, 'Function, 'Field>) : E<'Type, 'Function, 'Field> =
    let irExpr = optimizeAssertionPropagateExpression optenv origEnv irExpr
    match irExpr with
    | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
        let irNewConditionExpr = 
            assertionPropagateExpression optenv origEnv irConditionExpr 
            |> OptimizeExpression optenv
        let newEnvTrue, newEnvFalse = recordConditionExpression optenv origEnv irNewConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.True, _)) ->
            assertionPropagateExpression optenv  newEnvTrue irTrueTargetExpr

        | E.Value(value=V.Constant(C.False, _)) ->
            assertionPropagateExpression optenv  newEnvFalse irFalseTargetExpr

        | _ ->
            let irNewTrueTargetExpr = assertionPropagateExpression optenv  newEnvTrue irTrueTargetExpr
            let irNewFalseTargetExpr = assertionPropagateExpression optenv  newEnvFalse irFalseTargetExpr

            if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                irExpr
            else
                E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

    | E.While(irConditionExpr, irBodyExpr, resultTy) ->
        let irNewConditionExpr = assertionPropagateExpression optenv origEnv irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.False, _)) ->
            E.None(NoRange, resultTy)
        | _ ->
            let irNewBodyExpr = assertionPropagateExpression optenv origEnv irBodyExpr

            if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                irExpr
            else
                E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

    | E.Sequential(irExpr1, irExpr2) ->
        let irNewExpr1 = assertionPropagateExpression optenv  origEnv irExpr1
        let irNewExpr2 = assertionPropagateExpression optenv  origEnv irExpr2

        if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
            irExpr
        else
            E.Sequential(irNewExpr1, irNewExpr2)

    | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
        let irNewRhsExpr = assertionPropagateExpression optenv  origEnv irRhsExpr
        let irNewBodyExpr = assertionPropagateExpression optenv  origEnv irBodyExpr

        if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
            irExpr
        else
            E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

    | E.Operation(irTextRange, irOp) ->
        let irNewArgExprs = irOp.MapArguments(fun _ irArgExpr -> assertionPropagateExpression optenv origEnv irArgExpr)
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