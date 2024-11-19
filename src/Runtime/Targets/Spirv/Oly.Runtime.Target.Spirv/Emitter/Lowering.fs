namespace Oly.Runtime.Target.Spirv.Emitter

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading
open Oly.Core
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.CodeGen
open Spirv
open Spirv.SpirvModule

module rec Lowering =

    let private EmptyTextRange = OlyIRDebugSourceTextRange.Empty

    [<NoEquality;NoComparison>]
    type cenv =
        {
            Module: SpirvModuleBuilder
            Function: SpirvFunctionBuilder
        }

        member this.IsEntryPoint = this.Function.IsEntryPoint

    [<NoEquality;NoComparison>]
    type private env =
        {
            IsReturnable: bool
        }

        member this.NotReturnable =
            if this.IsReturnable then
                { this with IsReturnable = false }
            else
                this

    let private LowerLinearExpression (cenv: cenv) (env: env) (expr: E) =
        match expr with
        | E.Sequential(expr1, expr2) ->
            let newExpr1 = LowerLinearExpression cenv env expr1
            let newExpr2 = LowerLinearExpression cenv env expr2
            if newExpr1 = expr1 && newExpr2 = expr2 then
                expr
            else
                E.Sequential(newExpr1, newExpr2)
        
        | E.Let(name, localIndex, rhsExpr, bodyExpr) ->
            let newRhsExpr = LowerLinearExpression cenv env rhsExpr
            let newBodyExpr = LowerLinearExpression cenv env bodyExpr
            if newRhsExpr = rhsExpr && newBodyExpr = bodyExpr then
                expr
            else
                E.Let(name, localIndex, newRhsExpr, newBodyExpr)

        | _ ->
            LowerExpression cenv env expr

    let private LowerExpressionAux (cenv: cenv) (env: env) (expr: E) =
        match expr with
        | E.None _
        | E.Phi _ -> expr

        | E.Sequential _
        | E.Let _ ->
            LowerLinearExpression cenv env expr

        | E.Value(_, _value) ->
            expr

        | E.Operation(textRange, op) ->
            let newOp =
                let env = env.NotReturnable
                op.MapAndReplaceArguments (fun _ argExpr ->
                    LowerExpression cenv env argExpr
                )
            if newOp = op then
                expr
            else
                E.Operation(textRange, newOp)

        | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
            let newConditionExpr = LowerExpression cenv env.NotReturnable conditionExpr
            let newTrueTargetExpr = LowerExpression cenv env trueTargetExpr
            let newFalseTargetExpr = LowerExpression cenv env falseTargetExpr

            if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
                expr
            else
                E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)

        | E.While(conditionExpr, bodyExpr, resultTy) ->
            let newConditionExpr = LowerExpression cenv env.NotReturnable conditionExpr
            let newBodyExpr = LowerExpression cenv env.NotReturnable bodyExpr

            if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
                expr
            else
                E.While(newConditionExpr, newBodyExpr, resultTy)

        | E.Try _ ->
            raise(NotSupportedException())

    let private LowerExpression (cenv: cenv) (env: env) (expr: E) =
        let expr = LowerExpressionAux cenv env expr
        if cenv.IsEntryPoint && env.IsReturnable then
            match expr with
            | E.Let _
            | E.Sequential _
            | E.IfElse _ -> expr
            | _ ->
                match cenv.Function.LogicalReturnType with
                | SpirvType.Void _ ->
                    expr

                | SpirvType.Vector4 _ ->
                    // gl_Position
                    let result = cenv.Function.OutputParameterIdResults[0]
                    let setVarFunc = SpirvFunction.SetVariable(result.TypePointerOfBlockIdResult, result.VariableOfPointerOfBlockIdResult)
                    E.Operation(EmptyTextRange, O.Call(OlyIRFunction(setVarFunc), ImArray.createOne expr, cenv.Module.GetTypeVoid()))

                // Vertex only
                | SpirvType.Tuple(_, itemTys, _) when itemTys.Length > 1 && (match itemTys[0] with SpirvType.Vector4 _ -> true | _ -> false) ->
                    let itemExprs =
                        match expr with
                        | E.Operation(op=O.NewTuple(args=argExprs)) ->
                            argExprs
                        | _ ->
                            raise(System.NotImplementedException())
                    OlyAssert.Equal(itemTys.Length, itemExprs.Length)
                    let exprs =
                        [
                            // gl_Position
                            let result = cenv.Function.OutputParameterIdResults[0]
                            let setVarFunc = SpirvFunction.SetVariable(result.TypePointerOfBlockIdResult, result.VariableOfPointerOfBlockIdResult)
                            E.Operation(EmptyTextRange, O.Call(OlyIRFunction(setVarFunc), ImArray.createOne itemExprs[0], cenv.Module.GetTypeVoid()))

                            // Output Location 0-n
                            let result = cenv.Function.OutputParameterIdResults[1]
                            let setVarFunc = SpirvFunction.SetVariable(result.TypePointerOfBlockIdResult, result.VariableOfPointerOfBlockIdResult)
                            E.Operation(EmptyTextRange, O.Call(OlyIRFunction(setVarFunc), ImArray.createOne itemExprs[1], cenv.Module.GetTypeVoid()))
                        ]
                    E.CreateSequential(exprs |> ImArray.ofSeq)

            | _ ->
                raise(NotImplementedException())
        else
            expr

    let Lower cenv (expr: E) =
        let env = { IsReturnable = true }
        LowerExpression cenv env expr
            