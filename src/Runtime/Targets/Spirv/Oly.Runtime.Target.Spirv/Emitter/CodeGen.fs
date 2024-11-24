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

[<NoEquality;NoComparison>]
type cenv =
    {
        Function: SpirvFunctionBuilder
        Module: SpirvModuleBuilder
        Instructions: List<Instruction>
        Locals: List<IdResult>
        LocalTypes: List<SpirvType>
    }

    member this.IsEntryPoint =
        this.Function.IsEntryPoint

    member this.GetArgumentIdRef(argIndex: int32) =
        if this.Function.IsEntryPoint then
            this.Function.EntryPointParameters[argIndex].VariableIdRef
        else
            this.Function.Parameters[argIndex].VariableIdRef

    member this.GetLocalPointerIdRef(localIndex: int32) : IdRef =
        this.Locals[localIndex]

    member this.GetLocalPointerElementType(localIndex: int32) : SpirvType =
        match this.LocalTypes[localIndex] with
        | SpirvType.NativePointer(elementTy=elementTy) -> elementTy
        | _ -> failwith "Unable to get local pointer element type."

[<NoEquality;NoComparison>]
type env =
    {
        IsReturnable: bool
//        BlockScope: int
        BlockLabel: uint32
    }

    member this.NotReturnable =
        if this.IsReturnable then
            { this with IsReturnable = false }
        else
            this

module rec CodeGen =

    let private IdRef0 : IdRef = 0u

    let private emitInstruction cenv instr =
        cenv.Instructions.Add(instr)

    let private emitInstructions cenv instrs =
        cenv.Instructions.AddRange(instrs)

    let Gen (cenv: cenv) (expr: E) =
        let blockLabel= cenv.Module.NewIdResult()
        OpLabel(blockLabel) |> emitInstruction cenv

        let newCenv = { cenv with Instructions = List() }

        let env = { IsReturnable = true; BlockLabel = blockLabel }
        GenExpression newCenv env expr
        |> ignore

        for i = 0 to cenv.Locals.Count - 1 do
            let localIdResult = cenv.Locals[i]
            let localTy = cenv.LocalTypes[i]
            OpVariable(localTy.IdResult, localIdResult, StorageClass.Function, None)
            |> emitInstruction cenv

        emitInstructions cenv newCenv.Instructions

    let GenLinearExpression (cenv: cenv) (env: env) (expr: E) : IdResult =
        match expr with
        | E.Sequential(expr1, expr2) ->
            GenLinearExpression cenv env.NotReturnable expr1 |> ignore
            GenLinearExpression cenv env expr2

        | E.Let(_, localIndex, rhsExpr, bodyExpr) ->
            let rhsIdRef = GenLinearExpression cenv env.NotReturnable rhsExpr
            let idResult = cenv.Module.NewIdResult()
            cenv.Locals[localIndex] <- idResult
            cenv.LocalTypes[localIndex] <- cenv.Module.GetTypePointer(StorageClass.Function, rhsExpr.ResultType)
            OpStore(idResult, rhsIdRef, None) |> emitInstruction cenv
            GenLinearExpression cenv env bodyExpr

        | _ ->
            GenExpression cenv env expr

    let GenOperation (cenv: cenv) (env: env) (op: O) : IdRef =
        match op with
        | O.New(func, argExprs, _) ->
            match func.EmittedFunction with
            | SpirvFunction.BuiltIn(builtInFunc) ->
                match builtInFunc.Data with
                | BuiltInFunctionData.Intrinsic(create) ->
                    let args =
                        let env = env.NotReturnable
                        argExprs
                        |> ImArray.map (function
                            | E.Value(value=V.Constant(cns, _)) ->
                                Choice2Of2(cns)
                            | argExpr ->
                                Choice1Of2(GenExpression cenv env argExpr)
                        )
                    let idRef, instrs = create cenv.Module args
                    emitInstructions cenv instrs
                    idRef
                | _ ->
                    raise(InvalidOperationException())
            | _ ->
               raise(NotImplementedException(op.ToString()))

        | O.Call(irFunc, argExprs, resultTy) ->
            match irFunc.EmittedFunction with
            | SpirvFunction.AccessChain ->
                let receiverExpr = argExprs[0]
                match receiverExpr with
                | E.Operation(op=O.LoadFromAddress(body=E.Operation(op=O.LoadFieldAddress(field, innerReceiverExpr, _, _)))) ->
                    let indexExprs = argExprs |> ImArray.skip 1
                    let envNotReturnable = env.NotReturnable
                    let receiverIdRef = GenExpression cenv envNotReturnable innerReceiverExpr
                    let indexIdRefs =
                        indexExprs
                        |> ImArray.map (GenExpression cenv envNotReturnable)
                        |> ImArray.prependOne
                            (
                                cenv.Module.GetConstantInt32(field.EmittedField.Index)
                            )
                        |> List.ofSeq
                    let idResult = cenv.Module.NewIdResult()
                    OpAccessChain(resultTy.IdResult, idResult, receiverIdRef, indexIdRefs)
                    |> emitInstruction cenv
                    idResult
                | _ ->
                    raise(InvalidOperationException())

            | SpirvFunction.BuiltIn(builtInFunc) ->
                match builtInFunc.Data with
                | BuiltInFunctionData.Intrinsic _ ->
                    raise(NotImplementedException())
                | _ ->
                    raise(InvalidOperationException())

            | _ ->
                raise(NotImplementedException(op.ToString()))

        | op ->
            let argCount = op.ArgumentCount
            let idRefs = Array.zeroCreate argCount
            let envForArg = env.NotReturnable
            op.ForEachArgument(fun i argExpr ->
                idRefs[i] <- GenExpression cenv envForArg argExpr
            )
            match op with
            | O.Store(localIndex, _, _) ->
                let rhsIdRef = idRefs[0]

                OpStore(cenv.GetLocalPointerIdRef(localIndex), rhsIdRef, None) |> emitInstruction cenv
                IdRef0

            | O.LoadFromAddress(_, resultTy) ->
                let bodyIdRef = idRefs[0]

                let idResult = cenv.Module.NewIdResult()
                OpLoad(resultTy.IdResult, idResult, bodyIdRef, None) |> emitInstruction cenv
                idResult

            | O.StoreToAddress _ ->
                let argIdRef = idRefs[0]
                let rhsIdRef = idRefs[1]

                OpStore(argIdRef, rhsIdRef, None) |> emitInstruction cenv
                IdRef0

            | O.LoadFieldAddress(field, receiverExpr, _,_) ->
                let receiverIdRef = idRefs[0]

                match receiverExpr.ResultType with
                | SpirvType.NativePointer(storageClass=storageClass) ->
                    let idResult1 = cenv.Module.NewIdResult()
                    let idResultTy1 = cenv.Module.GetTypePointer(storageClass, field.EmittedField.Type).IdResult
                    OpAccessChain(idResultTy1, idResult1, receiverIdRef, [cenv.Module.GetConstantInt32(field.EmittedField.Index)]) 
                    |> emitInstruction cenv
                    idResult1

                | SpirvType.ByRef _ ->
                    raise(InvalidOperationException("ByRef should be lowered."))

                | _ ->
                    raise(InvalidOperationException())

            | O.LoadArrayElementAddress(receiverExpr, _, _, _) ->
                OlyAssert.Equal(2, idRefs.Length)
                let receiverIdRef = idRefs[0]
                let indexIdRef = idRefs[1]

                match receiverExpr.ResultType with
                | SpirvType.Array(elementTy=elementTy) ->
                    let idResult = cenv.Module.NewIdResult()
                    OpAccessChain(
                        cenv.Module.GetTypePointer(StorageClass.Function, elementTy).IdResult,
                        idResult,
                        receiverIdRef,
                        [indexIdRef]
                    ) |> emitInstruction cenv
                    idResult
                | _ ->
                    raise(InvalidOperationException())

            | O.Equal(arg1Expr, _, resultTy) ->
                let arg1IdRef = idRefs[0]
                let arg2IdRef = idRefs[1]

                match arg1Expr.ResultType with
                | SpirvType.UInt8 _
                | SpirvType.Int8 _
                | SpirvType.UInt16 _
                | SpirvType.Int16 _
                | SpirvType.UInt32 _
                | SpirvType.Int32 _
                | SpirvType.UInt64 _
                | SpirvType.Int64 _
                | SpirvType.Bool _ ->
                    // TODO: What if the value for bool is something other than 0 or 1?
                    let idResult = cenv.Module.NewIdResult()
                    OpIEqual(resultTy.IdResult, idResult, arg1IdRef, arg2IdRef) |> emitInstruction cenv
                    idResult
                | SpirvType.Float32 _
                | SpirvType.Float64 _
                | SpirvType.Vec2 _
                | SpirvType.Vec3 _
                | SpirvType.Vec4 _ ->
                    let idResult = cenv.Module.NewIdResult()
                    OpFOrdEqual(resultTy.IdResult, idResult, arg1IdRef, arg2IdRef) |> emitInstruction cenv
                    idResult
                | _ ->
                    raise(NotImplementedException())

            | O.Cast(argExpr, castToTy) ->
                let idRef1 = idRefs[0]

                let castFromTy = argExpr.ResultType

                let idResult = cenv.Module.NewIdResult()
                match castFromTy, castToTy with
                | SpirvType.UInt32 _, SpirvType.Int32 castToTyId ->
                    OpSConvert(castToTyId, idResult, idRef1) |> emitInstruction cenv
                | SpirvType.Int32 _, SpirvType.UInt32 castToTyId ->
                    OpUConvert(castToTyId, idResult, idRef1) |> emitInstruction cenv
                | _ ->
                    raise(NotImplementedException())
                idResult

            | O.StoreField _
            | O.LoadField _
            | O.StoreArrayElement _
            | O.LoadArrayElement _ ->
                raise(NotSupportedException("Should have lowered: \n" + op.ToString()))

            | _ ->
                raise(NotImplementedException(op.ToString()))

    let GenConstant (cenv: cenv) (env: env) (cns: C) =
        match cns with
        | C.Int32 value ->
            cenv.Module.GetConstantInt32(value)
        | C.Float32 value ->
            cenv.Module.GetConstantFloat32(value)
        | C.True ->
            cenv.Module.GetConstantUInt32(1u)
        | C.False ->
            cenv.Module.GetConstantUInt32(0u)
        | _ ->
            raise(NotImplementedException(cns.ToString()))

    let GenValue (cenv: cenv) (env: env) (value: V) =
        match value with
        | V.Constant(cns, _) ->
            GenConstant cenv env cns

        | V.Argument(argIndex, _) ->
            cenv.GetArgumentIdRef(argIndex)

        | V.Local(localIndex, _) ->
            let idResult = cenv.Module.NewIdResult()
            OpLoad(cenv.GetLocalPointerElementType(localIndex).IdResult, idResult, cenv.GetLocalPointerIdRef(localIndex), None) |> emitInstruction cenv
            idResult

        | V.LocalAddress(localIndex, _, _) ->
            cenv.GetLocalPointerIdRef(localIndex)

        | _ ->
            raise(NotImplementedException(value.ToString()))

    let private GenExpressionAux (cenv: cenv) (env: env) (expr: E) : IdResult =
        match expr with
        | E.Sequential _
        | E.Let _ ->
            GenLinearExpression cenv env expr

        | E.None _ ->
            IdRef0 // Nothing

        | E.Value(value=value) ->
            GenValue cenv env value

        | E.Operation(op=op) ->
            GenOperation cenv env op

        | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
            let conditionIdRef = GenExpression cenv env.NotReturnable conditionExpr

            let contLabel = cenv.Module.NewIdResult()
            let trueTargetLabel = cenv.Module.NewIdResult()
            let falseTargetLabel = cenv.Module.NewIdResult()

            [
                OpSelectionMerge(contLabel, SelectionControl.None)
                OpBranchConditional(conditionIdRef, trueTargetLabel, falseTargetLabel, [])
            ]
            |> emitInstructions cenv

            OpLabel(trueTargetLabel) |> emitInstruction cenv
            let trueTargetIdRef = GenExpression cenv { env with BlockLabel = trueTargetLabel } trueTargetExpr
            OpBranch(contLabel) |> emitInstruction cenv

            OpLabel(falseTargetLabel) |> emitInstruction cenv
            let falseTargetIdRef = GenExpression cenv { env with BlockLabel = falseTargetLabel } falseTargetExpr
            OpBranch(contLabel) |> emitInstruction cenv

            OpLabel(contLabel) |> emitInstruction cenv
            
            match resultTy with
            | SpirvType.Void _ ->
                OlyAssert.Equal(IdRef0, trueTargetIdRef)
                OlyAssert.Equal(IdRef0, falseTargetIdRef)
                IdRef0
            | _ ->
                let idResult = cenv.Module.NewIdResult()
                OpPhi(resultTy.IdResult, idResult, [PairIdRefIdRef(trueTargetIdRef, trueTargetLabel);PairIdRefIdRef(falseTargetIdRef, falseTargetLabel)]) |> emitInstruction cenv
                idResult

        | E.While _ ->
            raise(NotImplementedException())

        | E.Phi _ ->
            raise(NotImplementedException())

        | E.Try _ ->
            raise(NotSupportedException("Try"))

    let GenExpression (cenv: cenv) (env: env) (expr: E) : IdResult =
        let idRef : IdRef = GenExpressionAux cenv env expr
        if env.IsReturnable then
            match expr with
            | E.Let _
            | E.Sequential _
            | E.IfElse _ -> ()
            | _ ->
                if idRef = IdRef0 then
                    OpReturn |> emitInstruction cenv
                else
                    raise(NotImplementedException("returnable value"))
        idRef
