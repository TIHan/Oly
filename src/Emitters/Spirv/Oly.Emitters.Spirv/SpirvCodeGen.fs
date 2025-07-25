﻿namespace Oly.Emitters.Spirv

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
        IsDebuggable: bool
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
        this.Function.Parameters[argIndex].IdResult

    member this.GetLocalIdRef(localIndex: int32) : IdRef =
        this.Locals[localIndex]

    member this.GetExtendedInstructionSet(set: string) =
        match set with
        | "GLSL.std.450" -> 1u // TODO: this is hard-coded!
        | _ -> invalidOp "Invalid extended instruction set."

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

        let localSet = HashSet()
        for i = 0 to cenv.Locals.Count - 1 do
            let localIdResult = cenv.Locals[i]
            let localTy = cenv.LocalTypes[i]
            if localSet.Add(localIdResult) && not localTy.IsInvalid then
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
        | BuiltInOperations.AccessChain(baseExpr, indexExprs, resultTy) ->
            match resultTy with
            | SpirvType.Pointer(elementTy=elementTy) ->
                OlyAssert.False(elementTy.IsPointer)
            | _ -> 
                raise(InvalidOperationException("Expected a pointer type."))

#if DEBUG || CHECKED
            match baseExpr.ResultType, resultTy with
            | SpirvType.Pointer(storageClass=expectedStorageClass), SpirvType.Pointer(storageClass=storageClass) ->
                if expectedStorageClass <> storageClass then
                    raise(InvalidOperationException($"Storage classes do not match up for access chain. Expected: {expectedStorageClass}, Actual: {storageClass}"))
            | _ -> 
                raise(InvalidOperationException("Expected a pointer type."))
#endif

            match baseExpr with
            // Optimization: Combine access chains into one.
            | E.Operation(op=BuiltInOperations.AccessChain(innerBaseExpr, innerIndexExprs, _)) when not cenv.IsDebuggable ->
                let combinedAccessChainExpr =
                    BuiltInExpressions.AccessChain(
                        innerBaseExpr, 
                        (innerIndexExprs |> ROMem.toImArray).AddRange(indexExprs |> ROMem.toImArray),
                        resultTy
                    )
                GenExpression cenv env combinedAccessChainExpr
            | _ ->
                let envNotReturnable = env.NotReturnable
                let baseIdRef = GenExpression cenv envNotReturnable baseExpr
                let indexIdRefs =
                    indexExprs
                    |> ROMem.mapAsList (GenExpression cenv envNotReturnable)

                let idResult = cenv.Module.NewIdResult()
                OpAccessChain(resultTy.IdResult, idResult, baseIdRef, indexIdRefs)
                |> emitInstruction cenv
                idResult

        | BuiltInOperations.PtrAccessChain(baseExpr, elementExpr, indexExprs, resultTy) ->
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
            let idTypeRefs = Array.zeroCreate argCount
            let envForArg = env.NotReturnable
            op.ForEachArgument(fun i argExpr ->
                idRefs[i] <- GenExpression cenv envForArg argExpr
                idTypeRefs[i] <- argExpr.ResultType
            )
            match op with
            | O.StoreToAddress(lhsExpr, rhsExpr, _) ->
                let argIdRef = idRefs |> Array.head
                let rhsIdRef = idRefs[1]

#if DEBUG || CHECKED
                match lhsExpr.ResultType with
                | SpirvType.Pointer(elementTy=elementTy) ->
                    OlyAssert.False(elementTy.IsPointer)
                    OlyAssert.Equal(rhsExpr.ResultType.IdResult, elementTy.IdResult)
                | _ ->
                    raise(InvalidOperationException("Expected a pointer type."))

                OlyAssert.False(rhsExpr.ResultType.IsPointer)
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

                OlyAssert.NotEqual(IdRef0, bodyIdRef)

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
                | SpirvType.Vec _ ->
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
                    OpSatConvertUToS(castToTyId, idResult, idRef1) |> emitInstruction cenv
                | SpirvType.Int32 _, SpirvType.UInt32 castToTyId ->
                    OpSatConvertSToU(castToTyId, idResult, idRef1) |> emitInstruction cenv
                | _ ->
                    raise(NotImplementedException())
                idResult

            | O.New _
            | O.Store _
            | O.StoreField _
            | O.LoadField _
            | O.LoadFieldAddress _
            | O.StoreArrayElement _
            | O.LoadArrayElement _
            | O.LoadArrayElementAddress _ ->
                raise(NotSupportedException($"Should have been lowered:\n{op}"))

            | O.Call(func=irFunc;args=argExprs;resultTy=resultTy) ->
                let func = irFunc.EmittedFunction

                let handleVariable (var: SpirvVariable) =
#if DEBUG || CHECKED
                    match var.Type with
                    | SpirvType.Pointer _ ->
                        ()
                    | _ ->
                        raise(InvalidOperationException("Expected a pointer type."))
#endif
                    if idRefs.Length = 0 then
                        // Get
                        var.IdResult
                    else
                        // Set
                        OlyAssert.Equal(1, idRefs.Length)
#if DEBUG || CHECKED
                        match var.Type with
                        | SpirvType.Pointer(elementTy=elementTy) ->
                            OlyAssert.Equal(elementTy, op.GetArgument(0).ResultType)
                        | _ ->
                            raise(InvalidOperationException("Expected a pointer type."))
#endif
                        OpStore(var.IdResult, idRefs |> Array.head, None)
                        |> emitInstruction cenv
                        IdRef0

                match func with
                | SpirvFunction.Variable var ->
                    handleVariable var

                | SpirvFunction.LazyVariable lazyVar ->
                    handleVariable lazyVar.Value

                | SpirvFunction.AccessChain
                | SpirvFunction.PtrAccessChain ->
                    raise(InvalidOperationException("already handled"))

                | SpirvFunction.BuiltIn builtInFunc ->
                    match builtInFunc.Data with
                    | SpirvBuiltInFunctionData.Intrinsic(create) ->
                        let args =
                            idRefs
                            |> Array.mapi (fun i idRef ->
                                idTypeRefs[i], idRef
                            )
                            |> ImArray.ofSeq
                        let idRef, instrs = create cenv.Module args op.ResultType
                        emitInstructions cenv instrs
                        if resultTy.IsVoid then
                            IdRef0
                        else
                            idRef
                    | _ ->
                        raise(InvalidOperationException())

                | SpirvFunction.Function(funcBuilder) ->
                    let idResult = cenv.Module.NewIdResult()
                    argExprs
                    |> ImArray.iter (fun x ->
                        match x.ResultType with
                        | SpirvType.Pointer _ -> ()
                        | _ -> invalidOp "Expected pointer type."
                    )
                    OpFunctionCall(funcBuilder.ReturnType.IdResult, idResult, funcBuilder.IdResult, idRefs |> List.ofArray)
                    |> emitInstruction cenv
                    if resultTy.IsVoid then
                        IdRef0
                    else
                        idResult

                | SpirvFunction.ExtendedInstruction(set, n) ->
                    let idResult = cenv.Module.NewIdResult()
                    OpExtInst(resultTy.IdResult, idResult, cenv.GetExtendedInstructionSet(set), n, idRefs |> List.ofArray)
                    |> emitInstruction cenv
                    if resultTy.IsVoid then
                        IdRef0
                    else
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

        | _ ->
            raise(NotImplementedException(value.ToString()))

    let private GenExpressionAux (cenv: cenv) (env: env) (expr: E) : IdResult =
        match expr with
        | E.Sequential _ ->
            GenLinearExpression cenv env expr

        | E.None _ ->
            IdRef0 // Nothing

        | E.Value(textRange, value) ->
            if cenv.IsDebuggable && not textRange.IsEmpty then
                OpLine(cenv.Module.GetStringIdRef(textRange.Path.ToString()), uint32(textRange.StartLine), uint32(textRange.StartColumn))
                |> emitInstruction cenv
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
                    match expr.ResultType with
                    | SpirvType.Pointer _ -> invalidOp "Return type cannot be a pointer in Logical addressing model."
                    | _ -> ()
                    OpReturnValue idRef |> emitInstruction cenv
        idRef
