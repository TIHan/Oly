module internal rec Oly.Runtime.CodeGen.Internal.Optimizations.AssertionPropagation

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

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type private AssertionPropagationKey<'Type, 'Function, 'Field> =
    | Local of localIndex: int
    | Argument of argIndex: int
    | LocalLoadField of localIndex: int * irField: OlyIRField<'Type, 'Function, 'Field>

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type private AssertionPropagationValue<'Type, 'Function> =
    | IsNull
    | IsNotNull
    | IsEqualTo of C<'Type, 'Function>
    | IsNotEqualTo of C<'Type, 'Function>

let private assertionPropagationComparer<'Type, 'Function, 'Field> =
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
type private AssertionEnv<'Type, 'Function, 'Field> =
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

let private tryGetAssertionKeyAndValue (optenv: optenv<'Type, 'Function, 'Field>) (irOp: O<'Type, 'Function, 'Field>) =
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

let private recordConditionExpression (optenv: optenv<'Type, 'Function, 'Field>) (env: AssertionEnv<'Type, 'Function, 'Field>) (irExpr: E<'Type, 'Function, 'Field>) : (AssertionEnv<'Type, 'Function, 'Field> * AssertionEnv<'Type, 'Function, 'Field>) =
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

let private optimizeAssertionPropagateExpression (optenv: optenv<'Type, 'Function, 'Field>) (origEnv: AssertionEnv<'Type, 'Function, 'Field>) (irExpr: E<'Type, 'Function, 'Field>) : E<'Type, 'Function, 'Field> =
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

let private assertionPropagateExpression optenv origEnv irExpr =
    StackGuard.Do(fun() ->
        assertionPropagateExpressionAux optenv origEnv irExpr
    )

let private assertionPropagateExpressionAux (optenv: optenv<'Type, 'Function, 'Field>) (origEnv: AssertionEnv<'Type, 'Function, 'Field>) (irExpr: E<'Type, 'Function, 'Field>) : E<'Type, 'Function, 'Field> =
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
        //let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> assertionPropagateExpression optenv origEnv argExpr)
        //if newOp = irOp then
        //    irExpr
        //else
        //    E.Operation(irTextRange, newOp)
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

