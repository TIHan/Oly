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

    let private StorageClassToOlyIRByRefKind storageClass =
        match storageClass with
        | StorageClass.Output -> OlyIRByRefKind.WriteOnly
        | StorageClass.Input -> OlyIRByRefKind.ReadOnly
        | _ -> OlyIRByRefKind.ReadWrite

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

        | E.Value(textRange, value) ->
            match value with
            | V.Argument(index, _) when cenv.Function.IsEntryPoint ->
                E.Value(textRange, V.Argument(index, cenv.Function.EntryPointParameters[index].Type))
            | V.ArgumentAddress _ ->
                raise(NotImplementedException()) // TODO: We will need to handle entrypoint like the above Argument
            | _ ->
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

    let private LowerOperationExpression (cenv: cenv) (env: env) (expr: E) =
        match expr with
        | E.Operation(textRange, op) ->
            match op with
            | O.LoadField(field, receiverExpr, resultTy) ->
                OlyAssert.Equal(field.EmittedField.Type, resultTy)
                match receiverExpr.ResultType with
                | SpirvType.NativePointer(storageClass=storageClass) ->
                    if storageClass = StorageClass.Output then
                        raise(InvalidOperationException())

                    let loadFieldAddrExpr =
                        E.Operation(EmptyTextRange, 
                            O.LoadFieldAddress(
                                field, 
                                receiverExpr, 
                                StorageClassToOlyIRByRefKind storageClass, 
                                cenv.Module.GetTypePointer(storageClass, field.EmittedField.Type)
                            )
                        )

                    E.Operation(textRange,
                        O.LoadFromAddress(loadFieldAddrExpr, field.EmittedField.Type)
                    )
                | _ ->
                    raise(InvalidOperationException())

            | O.StoreField(field, receiverExpr, rhsExpr, resultTy) ->
                match receiverExpr.ResultType with
                | SpirvType.NativePointer(storageClass=storageClass) ->
                    if storageClass = StorageClass.Input then
                        raise(InvalidOperationException())

                    let loadFieldAddrExpr =
                        E.Operation(EmptyTextRange, 
                            O.LoadFieldAddress(
                                field, 
                                receiverExpr, 
                                StorageClassToOlyIRByRefKind storageClass, 
                                cenv.Module.GetTypePointer(storageClass, field.EmittedField.Type)
                            )
                        )

                    E.Operation(textRange,
                        O.StoreToAddress(loadFieldAddrExpr, rhsExpr, resultTy)
                    )
                | _ ->
                    raise(InvalidOperationException())

            | O.StoreArrayElement(receiverExpr, indexExprs, rhsExpr, resultTy) ->
                OlyAssert.Equal(1, indexExprs.Length)
                match receiverExpr.ResultType with
                | SpirvType.Array(_, kind, elementTy) ->
                    let irByRefKind =
                        match kind with
                        | SpirvArrayKind.Immutable ->
                            OlyIRByRefKind.ReadOnly
                        | SpirvArrayKind.Mutable ->
                            OlyIRByRefKind.ReadWrite

                    let loadArrayElementAddrExpr =
                        E.Operation(EmptyTextRange, 
                            O.LoadArrayElementAddress(
                                receiverExpr,
                                indexExprs,
                                irByRefKind,
                                cenv.Module.GetTypePointer(StorageClass.Function, elementTy)
                            )
                        )

                    E.Operation(textRange,
                        O.StoreToAddress(loadArrayElementAddrExpr, rhsExpr, resultTy)
                    )
                | _ ->
                    raise(InvalidOperationException())
            | _ ->
                expr
        | _ ->
            expr

    let private LowerExpression (cenv: cenv) (env: env) (expr: E) =
        let expr = LowerExpressionAux cenv env expr
        let expr =
            match expr with
            | E.Let _
            | E.Sequential _ -> expr
            | _ ->
                match expr.ResultType with
                | SpirvType.ByRef(elementTy=elementTy) ->
                    expr.WithResultType(cenv.Module.GetTypePointer(StorageClass.Function, elementTy))
                | _ ->
                    expr

        let expr = LowerOperationExpression cenv env expr

        if env.IsReturnable then
            match expr with
            | E.Let _
            | E.Sequential _
            | E.IfElse _ -> expr
            | _ ->
                // TODO: Do we need to do anything here?
                expr
        else
            expr

    let Lower cenv (expr: E) =
        let env = { IsReturnable = true }
        LowerExpression cenv env expr
            