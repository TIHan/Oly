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
        Locals: IReadOnlyList<IdResult>
        LocalTypes: IReadOnlyList<SpirvType>

        mutable PredecessorBlockLabel: IdRef
    }

    member this.IsEntryPoint =
        this.Function.IsEntryPoint

    member this.GetArgumentIdRef(argIndex: int32) : IdRef =
        if this.Function.IsEntryPoint then
            this.Function.EntryPointParameters[argIndex].VariableIdRef
        else
            this.Function.Parameters[argIndex].VariableIdRef

    member this.GetLocalIdRef(localIndex: int32) : IdRef =
        this.Locals[localIndex]

[<NoEquality;NoComparison>]
type env =
    {
        IsReturnable: bool
        CurrentBlockLabel: IdRef
    }

    member this.NotReturnable =
        if this.IsReturnable then
            { this with IsReturnable = false }
        else
            this

module rec SpirvCodeGen =

    let private IdRef0 : IdRef = 0u

    let private emitInstruction cenv instr =
        cenv.Instructions.Add(instr)

    let private emitInstructions cenv instrs =
        cenv.Instructions.AddRange(instrs)

    let Gen (cenv: cenv) (expr: E) =
        let blockLabel = cenv.Module.NewIdResult()
        OpLabel(blockLabel) |> emitInstruction cenv

        let newCenv = { cenv with Instructions = List() }

        let env = { IsReturnable = true; CurrentBlockLabel = blockLabel }
        GenExpression newCenv env expr
        |> ignore

        for i = 0 to cenv.Locals.Count - 1 do
            let localIdResult = cenv.Locals[i]
            let localTy = cenv.LocalTypes[i]
            match localTy with
            | SpirvType.Pointer(pointerTyIdResult, StorageClass.Function, _) ->
                OpVariable(pointerTyIdResult, localIdResult, StorageClass.Function, None)
                |> emitInstruction cenv
            | _ ->
                raise(InvalidOperationException("Expected a pointer type of storage class 'Function'."))

        emitInstructions cenv newCenv.Instructions

    let GenLinearExpression (cenv: cenv) (env: env) (expr: E) : IdResult =
        match expr with
        | E.Sequential(expr1, expr2) ->
            GenLinearExpression cenv env.NotReturnable expr1 |> ignore
            let env = { env with CurrentBlockLabel = cenv.PredecessorBlockLabel }
            GenLinearExpression cenv env expr2

        | _ ->
            GenExpression cenv env expr

    let GenOperation (cenv: cenv) (env: env) (op: O) : IdRef =
        match op with
        | O.New(func, argExprs, _) ->
            match func.EmittedFunction with
            | SpirvFunction.BuiltIn(builtInFunc) ->
#if DEBUG || CHECKED
                OlyAssert.True(argExprs.Length >= 1)
                argExprs
                |> ImArray.iter (fun argExpr ->
                    match argExpr.ResultType with
                    | SpirvType.Pointer _ ->
                        raise(InvalidOperationException("Did not expect a pointer."))
                    | _ ->
                        ()
                )
#endif

                match builtInFunc.Data with
                | BuiltInFunctionData.Intrinsic(create) ->
                    let args =
                        let env = env.NotReturnable
                        argExprs
                        |> ImArray.map (function
                            | E.Value(value=V.Constant(cns, _)) ->
                                Choice2Of2(cns)
                            | argExpr ->
                                Choice1Of2(argExpr.ResultType, GenExpression cenv env argExpr)
                        )
                    let idRef, instrs = create cenv.Module args
                    emitInstructions cenv instrs
                    idRef
                | _ ->
                    raise(InvalidOperationException())
            | _ ->
               raise(NotImplementedException(op.ToString()))

        | BuiltInOperations.AccessChain(_, baseExpr, indexExprs, resultTy) ->
            match resultTy with
            | SpirvType.Pointer _ -> ()
            | _ -> raise(InvalidOperationException("Expected a pointer type."))

#if DEBUG || CHECKED
            match baseExpr.ResultType, resultTy with
            | SpirvType.Pointer(storageClass=expectedStorageClass), SpirvType.Pointer(storageClass=storageClass) ->
                if expectedStorageClass <> storageClass then
                    raise(InvalidOperationException($"Storage classes do not match up for access chain. Expected: {expectedStorageClass}, Actual: {storageClass}"))
            | _ -> 
                raise(InvalidOperationException("Expected a pointer type."))
#endif

            let envNotReturnable = env.NotReturnable
            let baseIdRef = GenExpression cenv envNotReturnable baseExpr
            let indexIdRefs =
                indexExprs
                |> ROMem.mapAsList (GenExpression cenv envNotReturnable)

            let idResult = cenv.Module.NewIdResult()
            OpAccessChain(resultTy.IdResult, idResult, baseIdRef, indexIdRefs)
            |> emitInstruction cenv
            idResult

        | BuiltInOperations.PtrAccessChain(_, baseExpr, elementExpr, indexExprs, resultTy) ->
            match resultTy with
            | SpirvType.Pointer _ -> ()
            | _ -> raise(InvalidOperationException("Expected a pointer type."))

#if DEBUG || CHECKED
            match baseExpr.ResultType, resultTy with
            | SpirvType.Pointer(storageClass=expectedStorageClass), SpirvType.Pointer(storageClass=storageClass) ->
                if expectedStorageClass <> storageClass then
                    raise(InvalidOperationException($"Storage classes do not match up for access chain. Expected: {expectedStorageClass}, Actual: {storageClass}"))
            | _ -> 
                raise(InvalidOperationException("Expected a pointer type."))
#endif

            let envNotReturnable = env.NotReturnable
            let baseIdRef = GenExpression cenv envNotReturnable baseExpr
            let elementIdRef = GenExpression cenv envNotReturnable elementExpr
            let indexIdRefs =
                indexExprs
                |> ROMem.mapAsList (GenExpression cenv envNotReturnable)

#if DEBUG || CHECKED
            indexExprs
            |> ROMem.iter (fun indexExpr ->
                match indexExpr.ResultType with
                | SpirvType.UInt32 _ -> ()
                | _ -> raise(InvalidOperationException("Expected an 'uint32' type for the index."))
            )
#endif

            let idResult = cenv.Module.NewIdResult()
            OpPtrAccessChain(resultTy.IdResult, idResult, baseIdRef, elementIdRef, indexIdRefs)
            |> emitInstruction cenv
            idResult

        | op ->
            let argCount = op.ArgumentCount
            let idRefs = Array.zeroCreate argCount
            let envForArg = env.NotReturnable
            op.ForEachArgument(fun i argExpr ->
                idRefs[i] <- GenExpression cenv envForArg argExpr
            )
            match op with
            | O.StoreToAddress(lhsExpr, rhsExpr, _) ->
                let argIdRef = idRefs |> Array.head
                let rhsIdRef = idRefs[1]

#if DEBUG || CHECKED
                match lhsExpr.ResultType with
                | SpirvType.Pointer(elementTy=elementTy) ->
                    OlyAssert.Equal(rhsExpr.ResultType.IdResult, elementTy.IdResult)
                | _ ->
                    raise(InvalidOperationException("Expected a pointer type."))
#endif

                OpStore(argIdRef, rhsIdRef, None) |> emitInstruction cenv
                IdRef0

            | O.LoadFromAddress(bodyExpr, resultTy) ->
                let bodyIdRef = idRefs |> Array.head

#if DEBUG || CHECKED
                match bodyExpr.ResultType with
                | SpirvType.Pointer(_, _, elementTy) ->
                    match elementTy with
                    | SpirvType.RuntimeArray _ ->
                        raise(InvalidOperationException("Cannot load a runtime array from an address."))
                    | _ ->
                        ()
                | _ ->
                    raise(InvalidOperationException("Expected a pointer"))
#endif

                let idResult = cenv.Module.NewIdResult()
                OpLoad(resultTy.IdResult, idResult, bodyIdRef, None) |> emitInstruction cenv
                idResult

            | O.Equal(arg1Expr, _, resultTy) ->
                let arg1IdRef = idRefs |> Array.head
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
                    raise(NotSupportedException(arg1Expr.ResultType.ToString()))

            | O.Cast(argExpr, castToTy) ->
                let idRef1 = idRefs |> Array.head

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

            | O.Store _
            | O.StoreField _
            | O.LoadField _
            | O.LoadFieldAddress _
            | O.StoreArrayElement _
            | O.LoadArrayElement _
            | O.LoadArrayElementAddress _ ->
                raise(NotSupportedException($"Should have been lowered:\n{op}"))

            | O.Call(func=irFunc) ->
                let func = irFunc.EmittedFunction
                let funcBuilder = func.AsBuilder

                let idResult = cenv.Module.NewIdResult()
                OpFunctionCall(funcBuilder.ReturnType.IdResult, idResult, funcBuilder.IdResult, idRefs |> List.ofArray)
                |> emitInstruction cenv
                idResult

            | _ ->
                raise(NotImplementedException(op.ToString()))

    let GenConstant (cenv: cenv) (env: env) (cns: C) =
        match cns with
        | C.Int32 value ->
            cenv.Module.GetConstantInt32(value)
        | C.UInt32 value ->
            cenv.Module.GetConstantUInt32(value)
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
            cenv.GetLocalIdRef(localIndex)

        | V.ArgumentAddress _
        | V.LocalAddress _ ->
            raise(NotSupportedException(value.ToString()))

        | _ ->
            raise(NotImplementedException(value.ToString()))

    let private GenExpressionAux (cenv: cenv) (env: env) (expr: E) : IdResult =
        match expr with
        | E.Sequential _ ->
            GenLinearExpression cenv env expr

        | E.None _ ->
            IdRef0 // Nothing

        | E.Value(value=value) ->
            GenValue cenv env value

        | E.Operation(op=op) ->
            GenOperation cenv env op

        | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
            let conditionIdRef = GenExpression cenv env.NotReturnable conditionExpr

#if DEBUG || CHECKED
            OlyAssert.Equal(trueTargetExpr.ResultType, falseTargetExpr.ResultType)
            OlyAssert.Equal(trueTargetExpr.ResultType, resultTy)
#endif

            let contLabel = cenv.Module.NewIdResult()
            let trueTargetLabel = cenv.Module.NewIdResult()
            let falseTargetLabel = cenv.Module.NewIdResult()

            [
                OpSelectionMerge(contLabel, SelectionControl.None)
                OpBranchConditional(conditionIdRef, trueTargetLabel, falseTargetLabel, [])
            ]
            |> emitInstructions cenv

            OpLabel(trueTargetLabel) |> emitInstruction cenv
            let trueTargetIdRef = GenExpression cenv { env with CurrentBlockLabel = trueTargetLabel } trueTargetExpr
            let trueTargetPredecessorBlockLabel = cenv.PredecessorBlockLabel
            OpBranch(contLabel) |> emitInstruction cenv

            OpLabel(falseTargetLabel) |> emitInstruction cenv
            let falseTargetIdRef = GenExpression cenv { env with CurrentBlockLabel = falseTargetLabel } falseTargetExpr
            let falseTargetPredecessorBlockLabel = cenv.PredecessorBlockLabel
            OpBranch(contLabel) |> emitInstruction cenv

            OpLabel(contLabel) |> emitInstruction cenv
            cenv.PredecessorBlockLabel <- contLabel
            
            match resultTy with
            | SpirvType.Void _ ->
                OlyAssert.Equal(IdRef0, trueTargetIdRef)
                OlyAssert.Equal(IdRef0, falseTargetIdRef)
                IdRef0
            | _ ->
                let idResult = cenv.Module.NewIdResult()
                OpPhi(resultTy.IdResult, idResult, 
                    [
                        PairIdRefIdRef(trueTargetIdRef, trueTargetPredecessorBlockLabel)
                        PairIdRefIdRef(falseTargetIdRef, falseTargetPredecessorBlockLabel)
                    ]
                ) |> emitInstruction cenv
                idResult

        | E.While _ ->
            raise(NotImplementedException())

        | E.Phi _ ->
            raise(NotImplementedException())

        | E.Let _ ->
            raise(InvalidOperationException($"Should have been lowered:\n{expr}"))

        | E.Try _ ->
            raise(NotSupportedException("Try"))

    let GenExpression (cenv: cenv) (env: env) (expr: E) : IdResult =
        let idRef : IdRef = GenExpressionAux cenv env expr
        match expr with
        | E.Let _
        | E.Sequential _
        | E.IfElse _ -> ()
        | _ ->
            cenv.PredecessorBlockLabel <- env.CurrentBlockLabel
        if env.IsReturnable then
            match expr with
            | E.Let _
            | E.Sequential _
            | E.IfElse _ -> ()
            | _ ->
                if idRef = IdRef0 then
                    OpReturn |> emitInstruction cenv
                else
                    OpReturnValue idRef |> emitInstruction cenv
        idRef
