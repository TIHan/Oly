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

    type cenv() = class end
    type env() = class end

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

    let private LowerExpression (cenv: cenv) (env: env) (expr: E) =
        match expr with
        | E.None _
        | E.Value _
        | E.Phi _ -> expr

        | E.Sequential _
        | E.Let _ ->
            LowerLinearExpression cenv env expr

        | E.Operation(textRange, op) ->
            let newOp =
                op.MapAndReplaceArguments (fun _ argExpr ->
                    LowerExpression cenv env argExpr
                )
            if newOp = op then
                expr
            else
                E.Operation(textRange, newOp)

        | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
            let newConditionExpr = LowerExpression cenv env conditionExpr
            let newTrueTargetExpr = LowerExpression cenv env trueTargetExpr
            let newFalseTargetExpr = LowerExpression cenv env falseTargetExpr

            if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
                expr
            else
                E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)

        | E.While(conditionExpr, bodyExpr, resultTy) ->
            let newConditionExpr = LowerExpression cenv env conditionExpr
            let newBodyExpr = LowerExpression cenv env bodyExpr

            if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
                expr
            else
                E.While(newConditionExpr, newBodyExpr, resultTy)

        | E.Try _ ->
            raise(NotSupportedException())

    let Lower (expr: E) =
        let cenv = cenv()
        let env = env()
        LowerExpression cenv env expr
            