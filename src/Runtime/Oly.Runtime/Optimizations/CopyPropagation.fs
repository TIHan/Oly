module internal rec Oly.Runtime.CodeGen.Internal.Optimizations.CopyPropagation

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
open Oly.Runtime.CodeGen.Internal.Optimizations.GeneralOptimizer

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type private CopyPropagationItem<'Type, 'Function, 'Field> =
    | Constant of C<'Type, 'Function>
    | Local of localIndex: int
    | LocalAddress of localIndex: int * irByRefKind: OlyIRByRefKind
    | Argument of argIndex: int
    | LoadField of irField: OlyIRField<'Type, 'Function, 'Field> * irReceiverExpr: E<'Type, 'Function, 'Field>
    | LoadFunction of irFunc: OlyIRFunction<'Type, 'Function, 'Field> * irArgExpr: E<'Type, 'Function, 'Field>
    | NewTuple of CopyPropagationItem<'Type, 'Function, 'Field> option imarray

let private tryGetPropagatedExpressionByLocal optenv (items: Dictionary<int, CopyPropagationItem<_, _, _>>) (localIndex: int, resultTy) =
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

let private tryGetPropagatedExpressionByLocalAddress (items: Dictionary<int, CopyPropagationItem<_, _, _>>) (localIndex: int, irByRefKind, resultTy) =
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

let private tryGetPropagatedExpressionByLoadFromAddressLocal optenv (items: Dictionary<int, CopyPropagationItem<_, _, _>>) (localIndex: int, resultTy) =
    match items.TryGetValue localIndex with
    | true, item ->
        match item with
        | CopyPropagationItem.LocalAddress(localIndex, OlyIRByRefKind.Read) ->
            tryGetPropagatedExpressionByLocal optenv items (localIndex, resultTy)
        | _ ->
            None
    | _ ->
        None

let private copyPropagationOptimizeExpression optenv (items: Dictionary<int, CopyPropagationItem<_, _, _>>) irExpr =
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

let private handleOperation optenv items irExpr : E<_, _, _> =
    match irExpr with
    | E.Operation(irTextRange, irOp) ->
        let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> handleExpression optenv items argExpr)
        if newOp = irOp then
            handleOperationSecondPass optenv items irExpr
        else
            E.Operation(irTextRange, newOp)
            |> handleOperationSecondPass optenv items
    | _ ->
        OlyAssert.Fail("Expected operation")

let private handleOperationSecondPass optenv items irExpr : E<_, _, _> =
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

let private handleExpression optenv items irExpr : E<_, _, _> =
    StackGuard.Do(fun () ->
        handleExpressionAuxCopyPropagation optenv items irExpr
    )

let private handleExpressionAuxCopyPropagation optenv items irExpr : E<_, _, _> =
    let irExpr = copyPropagationOptimizeExpression optenv items irExpr
    match irExpr with
    | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
        let irNewRhsExpr = handleExpression optenv items irRhsExpr

        if optenv.IsLocalMutable(localIndex) |> not then
            match irNewRhsExpr with
            | E.Value(value=V.Local(localIndexToPropagate, _)) when optenv.CanPropagateLocal localIndexToPropagate ->
                items.Add(localIndex, CopyPropagationItem.Local(localIndexToPropagate))
                handleExpression optenv items irBodyExpr

            | E.Value(value=V.Argument(argIndexToPropagate, _)) when optenv.CanPropagateArgument argIndexToPropagate ->
                items.Add(localIndex, CopyPropagationItem.Argument(argIndexToPropagate))
                handleExpression optenv items irBodyExpr

            | E.Value(value=V.Constant(c, _)) ->
                items.Add(localIndex, CopyPropagationItem.Constant(c))
                let irNewBodyExpr = handleExpression optenv items irBodyExpr

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
                    let irNewBodyExpr = handleExpression optenv items irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                | _ ->
                    let irNewBodyExpr = handleExpression optenv items irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

            | E.Operation(op=O.LoadField(irField, irReceiverExpr, _)) when not irField.IsMutable ->
                match irReceiverExpr with
                | E.Value(value=V.Local(localIndexToPropagate, _)) when optenv.CanPropagateLocal localIndexToPropagate ->
                    items.Add(localIndex, CopyPropagationItem.LoadField(irField, irReceiverExpr))
                    let irNewBodyExpr = handleExpression optenv items irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                | E.Value(value=V.Argument(argIndexToPropagate, _)) when optenv.CanPropagateArgument argIndexToPropagate ->
                    items.Add(localIndex, CopyPropagationItem.LoadField(irField, irReceiverExpr))
                    let irNewBodyExpr = handleExpression optenv items irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

                | _ ->
                    let irNewBodyExpr = handleExpression optenv items irBodyExpr

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

                let irNewBodyExpr = handleExpression optenv items irBodyExpr

                if irNewRhsExpr = irRhsExpr && irNewRhsExpr = irBodyExpr then
                    irExpr
                else
                    E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

            | E.Operation(op=O.LoadFunction(irFunc, irArgExpr, _)) when not(hasSideEffect optenv irArgExpr) ->
                items.Add(localIndex, CopyPropagationItem.LoadFunction(irFunc, irArgExpr))

                let irNewBodyExpr = handleExpression optenv items irBodyExpr

                if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                    irExpr
                else
                    E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

            | _ ->
                let irNewBodyExpr = handleExpression optenv items irBodyExpr

                if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                    irExpr
                else
                    E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)
        else
            let irNewBodyExpr = handleExpression optenv items irBodyExpr

            if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                irExpr
            else
                E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

    | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
        let irNewConditionExpr = handleExpression optenv items irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.True, _)) ->
            handleExpression optenv items irTrueTargetExpr

        | E.Value(value=V.Constant(C.False, _)) ->
            handleExpression optenv items irFalseTargetExpr

        | _ ->
            let irNewTrueTargetExpr = handleExpression optenv items irTrueTargetExpr
            let irNewFalseTargetExpr = handleExpression optenv items irFalseTargetExpr

            if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                irExpr
            else
                E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

    | E.While(irConditionExpr, irBodyExpr, resultTy) ->
        let irNewConditionExpr = handleExpression optenv items irConditionExpr

        match irNewConditionExpr with
        | E.Value(value=V.Constant(C.False, _)) ->
            E.None(NoRange, resultTy)
        | _ ->
            let irNewBodyExpr = handleExpression optenv items irBodyExpr

            if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                irExpr
            else
                E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

    | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
        let irNewBodyExpr = handleExpression optenv items irBodyExpr

        let mutable didChange = false
        let irNewCatchCases =
            irCatchCases
            |> ImArray.map (fun irCatchCase ->
                match irCatchCase with
                | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                    let irNewCaseBodyExpr = handleExpression optenv items irCaseBodyExpr

                    if irNewCaseBodyExpr = irCaseBodyExpr then
                        irCatchCase
                    else
                        didChange <- true
                        OlyIRCatchCase.CatchCase(localName, localIndex, irNewCaseBodyExpr, catchTy)
            )

        let irNewFinallyBodyExprOpt =
            irFinallyBodyExprOpt
            |> Option.map (fun irExpr ->
                let irNewExpr = handleExpression optenv items irExpr
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
        let irNewExpr1 = handleExpression optenv items irExpr1
        let irNewExpr2 = handleExpression optenv items irExpr2

        if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
            irExpr
        else
            E.Sequential(irNewExpr1, irNewExpr2)

    | E.Operation _ ->
        handleOperation optenv items irExpr

    | _ ->
        irExpr

let CopyPropagation (optenv: optenv<_, _, _>) (irExpr: E<_, _, _>) =
    let items = Dictionary<int, CopyPropagationItem<_, _, _>>()
    handleExpression optenv items irExpr