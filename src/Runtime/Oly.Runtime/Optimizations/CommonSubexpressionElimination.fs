module internal rec Oly.Runtime.CodeGen.Internal.Optimizations.CommonSubexpressionElimination

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


// -------------------------------------------------------------------------------------------------------------

let private cseEquals irExpr1 irExpr2 =
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


        | O.New(func1, argExprs1, _),
          O.New(func2, argExprs2, _) 
                when func1.IsClosureInstanceConstructor && func2.IsClosureInstanceConstructor &&
                     func1.RuntimeFunction = func2.RuntimeFunction &&
                     argExprs1.Length = argExprs2.Length &&
                     (argExprs1, argExprs2)
                     ||> ImArray.forall2 (cseEquals) ->
            true

        | _ ->
            false
    | _ ->
        false

let private cseComparer<'Type, 'Function, 'Field> =
    { new IEqualityComparer<E<'Type, 'Function, 'Field>> with
        member _.GetHashCode(irExpr) =
            0
        member _.Equals(irExpr1, irExpr2) =
            cseEquals irExpr1 irExpr2
    }

[<NoEquality;NoComparison>]
type private CseEnv<'Type, 'Function, 'Field> =
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

            | O.New _ ->
                match env.TryGetValue(irExpr) with
                | true, irNewExpr ->
                    handleExpression env irNewExpr
                | _ ->
                    irExpr

            | _ ->
                irExpr
        | _ ->
            OlyAssert.Fail("Expected opertion")

    and handleSequentialExpression1 (env: CseEnv<_, _, _>) (hoist: List<_>) irNewExpr1 =
        match irNewExpr1 with
        | E.Operation(irTextRange, op) ->
            let mutable env = env
            let irNewArgExprs =
                op.MapArguments(fun _ irArgExpr ->
                    match irArgExpr with
                    | E.Operation(_, op) ->
                        match op with
                        | O.New(func, _, _) when func.IsClosureInstanceConstructor && canSafelyPropagateForNewClosure optenv irArgExpr ->
                            match env.TryGetValue(irArgExpr) with
                            | true, irNewExpr ->
                                handleExpression env irNewExpr
                            | _ ->
                                let localIndex = optenv.CreateLocal(OlyIRLocalFlags.None)
                                let localExpr = E.Value(NoRange, V.Local(localIndex, irExpr.ResultType))
                                hoist.Add((localIndex, irArgExpr))
                                env <- env.Set(irArgExpr, localExpr)
                                localExpr
                        | _ ->
                            irArgExpr
                    | _ ->
                        irArgExpr
                )
            env, E.Operation(irTextRange, op.ReplaceArguments(irNewArgExprs))
        | _ ->
            env, irNewExpr1

    and handleExpression irExpr =
        StackGuard.Do(fun () ->
            handleExpressionAuxCse irExpr
        )
    
    and handleExpressionAuxCse (env: CseEnv<_, _, _>) irExpr : E<_, _, _> =
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

            let env, irNewExpr1 = handleSequentialExpression1 env hoist irNewExpr1

            let irNewExpr2 = handleExpression env irExpr2
    
            if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
                irExpr
            else
                let newExpr = E.Sequential(irNewExpr1, irNewExpr2)
                (hoist, newExpr)
                ||> Seq.foldBack (fun (localIndex, rhsExpr) expr ->
                    E.Let("cse", localIndex, rhsExpr,
                        expr
                    )
                )
    
        | E.Operation(irTextRange, irOp) ->
            //let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> handleExpression env argExpr)
            //if newOp = irOp then
            //    handleOperation env irExpr
            //else
            //    E.Operation(irTextRange, newOp)
            //    |> handleOperation env
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