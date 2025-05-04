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

module rec SpirvLowering =

    let private EmptyTextRange = OlyIRDebugSourceTextRange.Empty

    [<NoEquality;NoComparison>]
    type cenv =
        {
            Module: SpirvModuleBuilder
            Function: SpirvFunctionBuilder
            Locals: List<IdResult>
            LocalTypes: List<SpirvType>
        }

        member this.IsEntryPoint = this.Function.IsEntryPoint

        member this.SetLocal(localIndex: int32, ty: SpirvType, idResult: IdResult) =
            let ty =
                match ty with
                | SpirvType.Pointer(elementTy=elementTy) -> 
                    this.Module.GetTypePointer(StorageClass.Function, elementTy)
                | _ -> 
                    raise(InvalidOperationException("Expected a pointer type."))
            this.Locals[localIndex] <- idResult
            this.LocalTypes[localIndex] <- ty
            ty

        member this.SetLocal(localIndex: int32, ty: SpirvType) =
            this.SetLocal(localIndex, ty, this.Module.NewIdResult())

        member this.AddLocal(ty: SpirvType) =
            let localIndex = this.Locals.Count
            this.Locals.Add(0u) // placeholder
            this.LocalTypes.Add(SpirvType.Invalid) // placeholder
            let ty = this.SetLocal(localIndex, ty)
            ty, localIndex

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

    let private AssertPointerType (expr: E) =
#if DEBUG || CHECKED
        match expr.ResultType with
        | SpirvType.Pointer _ -> ()
        | _ -> raise(InvalidOperationException("Expected a pointer type."))
#endif
        ()

    let private CheckArgumentOrLocalType resultTy =
        match resultTy with
        | SpirvType.Pointer(elementTy=SpirvType.RuntimeArray _) 
        | SpirvType.OlyByRef(elementTy=SpirvType.RuntimeArray _) ->
            raise(InvalidOperationException("Invalid use of runtime array."))
        | SpirvType.Pointer _
        | SpirvType.OlyByRef _ ->
            resultTy
        | _ ->
            raise(InvalidOperationException("Expected pointer or by-ref type."))

    /// Use this in post-lowering.
    let private AutoDereferenceLoweredExpressionIfPossible (expr: E) =
        match expr.ResultType with
        | SpirvType.Pointer(elementTy=elementTy) ->
            E.Operation(EmptyTextRange, O.LoadFromAddress(expr, elementTy))
        | SpirvType.OlyByRef _ ->
            raise(InvalidOperationException("Did not expect a by-ref type."))
        | _ ->
            expr

    let private LowerLinearExpression (cenv: cenv) (env: env) (expr: E) =
        match expr with
        | E.Sequential(expr1, expr2) ->
            let newExpr1 = LowerLinearExpression cenv env expr1
            let newExpr2 = LowerLinearExpression cenv env expr2
            if newExpr1 = expr1 && newExpr2 = expr2 then
                expr
            else
                E.Sequential(newExpr1, newExpr2)
        
        | E.Let(_, localIndex, rhsExpr, bodyExpr) ->
            let envNotReturnable = env.NotReturnable
            let loweredRhsExpr = LowerLinearExpression cenv envNotReturnable rhsExpr
            let resultTy = loweredRhsExpr.ResultType

            match resultTy with
            | SpirvType.Pointer _ ->
                cenv.SetLocal(localIndex, resultTy)
                |> ignore
            | _ ->
                cenv.SetLocal(localIndex, cenv.Module.GetTypePointer(StorageClass.Function, resultTy))
                |> ignore
            E.Sequential(
                // Lower Store to StoreToAddress.
                E.Operation(EmptyTextRange, O.Store(localIndex, loweredRhsExpr, cenv.Module.GetTypeVoid()))
                |> LowerExpression cenv envNotReturnable,
                LowerLinearExpression cenv env bodyExpr
            )

        | _ ->
            LowerExpression cenv env expr

    let private LowerArgument (cenv: cenv) (_env: env) textRange argIndex =
        let resultTy = cenv.Function.Parameters[argIndex].Type
        E.Value(textRange, V.Argument(argIndex, CheckArgumentOrLocalType resultTy))

    let private LowerArgumentAddress (cenv: cenv) (env: env) textRange argIndex byRefKind =
        LowerArgument cenv env textRange argIndex
        //let resultTy = cenv.Function.Parameters[argIndex].Type
        //let resultTy = SpirvType.Pointer(0u, StorageClass.Function, resultTy) // fake pointer of pointer - logical addressing
        //E.Value(textRange, V.ArgumentAddress(argIndex, byRefKind, CheckArgumentOrLocalType resultTy))
        
    let private LowerLocal (cenv: cenv) (_env: env) textRange localIndex =
        let resultTy = cenv.LocalTypes[localIndex]
        E.Value(textRange, V.Local(localIndex, CheckArgumentOrLocalType resultTy))

    let private LowerLocalAddress (cenv: cenv) (env: env) textRange localIndex byRefKind =
        LowerLocal cenv env textRange localIndex
        //let resultTy = cenv.LocalTypes[localIndex]
        //let resultTy = SpirvType.Pointer(0u, StorageClass.Function, resultTy) // fake pointer of pointer - logical addressing
        //E.Value(textRange, V.LocalAddress(localIndex, byRefKind, CheckArgumentOrLocalType resultTy))

    let private LowerValue (cenv: cenv) (env: env) origExpr textRange value =
        match value with
        | V.Argument(argIndex, _) ->
            LowerArgument cenv env textRange argIndex
        | V.ArgumentAddress(argIndex, byRefKind, _) ->
            LowerArgumentAddress cenv env textRange argIndex byRefKind          
        | V.Local(localIndex, _) ->
            LowerLocal cenv env textRange localIndex
        | V.LocalAddress(localIndex, byRefKind, _) ->
            LowerLocalAddress cenv env textRange localIndex byRefKind
        | _ ->
            origExpr

    let private CopyLoweredArgument (cenv: cenv) (loweredArgExpr: E) =
        OlyAssert.False(loweredArgExpr.ResultType.IsOlyByRef)
        let loweredArgExpr =
            if loweredArgExpr.ResultType.IsPointer then
                E.Operation(
                    EmptyTextRange, 
                    O.LoadFromAddress(loweredArgExpr, loweredArgExpr.ResultType.ElementType)
                )
            else
                loweredArgExpr
        let localTy, localIndex = 
            cenv.AddLocal(
                cenv.Module.GetTypePointer(StorageClass.Function, loweredArgExpr.ResultType)
            )
        let valueExpr = E.Value(EmptyTextRange, V.Local(localIndex, localTy))
        E.Sequential(
            E.Operation(EmptyTextRange, O.StoreToAddress(valueExpr, loweredArgExpr, cenv.Module.GetTypeVoid())),
            valueExpr
        )

    let private LowerOperation (cenv: cenv) (env: env) origExpr textRange (origOp: O) =
        let loweredOp =
            let env = env.NotReturnable

            let op =
                match origOp with
                | O.New(irFunc, argExprs, resultTy) ->
                    match irFunc.EmittedFunction with
                    | SpirvFunction.BuiltIn _ ->
                        O.Call(irFunc, argExprs, resultTy)
                    | _ ->
                        origOp
                | _ ->
                    origOp

            op.MapAndReplaceArguments (fun i origArgExpr ->
                let loweredArgExpr = LowerExpression cenv env origArgExpr
                match op with
                | O.Store(localIndex, _, _) ->
                    let localTy = cenv.LocalTypes[localIndex]
                    if localTy.IsPointer && localTy.ElementType.IsPointer then
                        failwith "Invalid local"
                    else
                        AutoDereferenceLoweredExpressionIfPossible loweredArgExpr

                | O.StoreArgument(argIndex, _, _) ->
                    let parTy = cenv.Function.Parameters[argIndex].Type
                    if parTy.IsPointer && parTy.ElementType.IsPointer then
                        failwith "Invalid argument"
                    else
                        AutoDereferenceLoweredExpressionIfPossible loweredArgExpr

                | O.StoreField _ ->
                    if i = 0 then
                        AssertPointerType loweredArgExpr
                        loweredArgExpr
                    else
                        AutoDereferenceLoweredExpressionIfPossible loweredArgExpr

                | O.StoreToAddress _ ->
                    if i = 1 then
                        AutoDereferenceLoweredExpressionIfPossible loweredArgExpr
                    else
                        AssertPointerType loweredArgExpr
                        loweredArgExpr

                | O.StoreArrayElement _ ->
                    // In Spirv, the index must always be an uint32.
                    if i = 0 then
                        AssertPointerType loweredArgExpr
                        loweredArgExpr
                    elif i = 1 then // 1 dim - the index
                        let newArgExpr = AutoDereferenceLoweredExpressionIfPossible loweredArgExpr 
                        match newArgExpr.ResultType with
                        | SpirvType.UInt32 _ ->
                            newArgExpr
                        | _ ->
                            E.Operation(EmptyTextRange, O.Cast(newArgExpr, cenv.Module.GetTypeUInt32()))
                    else
                        AutoDereferenceLoweredExpressionIfPossible loweredArgExpr

                | O.LoadArrayElement _ ->
                    if i = 0 then
                        AssertPointerType loweredArgExpr
                        loweredArgExpr
                    else
                        let newArgExpr = AutoDereferenceLoweredExpressionIfPossible loweredArgExpr 
                        match newArgExpr.ResultType with
                        | SpirvType.UInt32 _ ->
                            newArgExpr
                        | _ ->
                            E.Operation(EmptyTextRange, O.Cast(newArgExpr, cenv.Module.GetTypeUInt32()))

                | O.LoadField _
                | O.LoadFieldAddress _
                | O.LoadFromAddress _ ->
                    AssertPointerType loweredArgExpr
                    loweredArgExpr
                
                | O.Equal _
                | O.Cast _ ->
                    AutoDereferenceLoweredExpressionIfPossible loweredArgExpr

                | BuiltInOperations.AccessChain _ ->
                    if i = 0 then
                        AssertPointerType loweredArgExpr
                        loweredArgExpr
                    else
                        AutoDereferenceLoweredExpressionIfPossible loweredArgExpr

                | BuiltInOperations.PtrAccessChain _ ->
                    if i = 0 then
                        AssertPointerType loweredArgExpr
                        loweredArgExpr
                    else
                        AutoDereferenceLoweredExpressionIfPossible loweredArgExpr

                | O.Call(irFunc, _, _) ->
                    let canAutoDereference =
                        match irFunc.EmittedFunction with
                        | SpirvFunction.BuiltIn(builtInFunc) ->
                            builtInFunc.Flags.HasFlag(SpirvBuiltInFunctionFlags.AutoDereferenceArguments)
                        | _ ->
                            false
                    if canAutoDereference then
                        AutoDereferenceLoweredExpressionIfPossible loweredArgExpr
                    else
                        match origArgExpr.ResultType with
                        | SpirvType.OlyByRef _
                        | SpirvType.Pointer _ -> 
                            OlyAssert.True(loweredArgExpr.ResultType.IsPointer)
                            loweredArgExpr
                        | _ ->
                            CopyLoweredArgument cenv loweredArgExpr

                | _ ->
                    raise(NotImplementedException(op.ToString()))
            )

        let handleCallVariable (var: SpirvVariable) (irFunc: OlyIRFunction<_, _, _>) argExprs resultTy =
            match resultTy with
            | SpirvType.Void _ ->
                O.Call(irFunc, argExprs, resultTy)
            | _ ->
                O.Call(irFunc, argExprs, var.Type)

        let loweredOp =
            match loweredOp with
            | O.Call(irFunc, argExprs, resultTy) ->
                match irFunc.EmittedFunction with
                | SpirvFunction.Variable var ->
                    let argExprs = argExprs |> ImArray.map AutoDereferenceLoweredExpressionIfPossible
                    handleCallVariable var irFunc argExprs resultTy
                | SpirvFunction.LazyVariable lazyVar ->
                    let argExprs = argExprs |> ImArray.map AutoDereferenceLoweredExpressionIfPossible
                    handleCallVariable lazyVar.Value irFunc argExprs resultTy
                | SpirvFunction.ExtendedInstruction _ ->
                    O.Call(irFunc, argExprs |> ImArray.map AutoDereferenceLoweredExpressionIfPossible, resultTy)
                | _ ->
                    loweredOp
            | _ ->
                loweredOp

        let loweredExpr =
            match loweredOp with
            | O.LoadFromAddress(bodyExpr, _) ->
                bodyExpr
            | O.New(ctor, argExprs, resultTy) ->
                match ctor.EmittedFunction with
                | SpirvFunction.BuiltIn _ ->
                    E.Operation(textRange, O.Call(ctor, argExprs, resultTy))

                | SpirvFunction.Function(funcBuilder) ->
                    OlyAssert.Equal(cenv.Module.GetTypeVoid(), funcBuilder.ReturnType)
                    let localTy, localIndex = cenv.AddLocal(cenv.Module.GetTypePointer(StorageClass.Function, resultTy))
                    let valueExpr = E.Value(EmptyTextRange, V.Local(localIndex, localTy))
                    E.CreateSequential(
                        [
                            E.Operation(textRange,
                                O.Call(ctor,
                                    argExprs
                                    |> ImArray.prependOne valueExpr,
                                    funcBuilder.ReturnType
                                )
                            )
                            valueExpr
                        ]
                        |> ImArray.ofSeq
                    )

                | _ ->
                    raise(NotImplementedException(loweredOp.ToString()))
            | _ ->
                if loweredOp = origOp then
                    origExpr
                else
                    E.Operation(textRange, loweredOp)

        let theAshleyTransform loweredExpr =
            match loweredExpr with
            | E.Operation(textRange, (O.Call(func, _, _) as loweredOp)) ->
                match func.EmittedFunction with
                | SpirvFunction.Function _ ->
                    let copyRestoreExprs = ResizeArray()
                    let loweredOp =
                        loweredOp.MapAndReplaceArguments (fun _ loweredArgExpr ->
                            match loweredArgExpr with
                            | E.Operation(op=BuiltInOperations.AccessChain(baseExpr, indexExprs, _)) ->
#if DEBUG || CHECKED
                                let isValid =
                                    match baseExpr with
                                    | E.Value _ ->
                                        indexExprs
                                        |> ROMem.forall (function E.Value _ -> true | _ -> false)
                                    | _ ->
                                        false
                                OlyAssert.True(isValid)
#endif
                                let copiedLoweredArgExpr = CopyLoweredArgument cenv loweredArgExpr
                                copyRestoreExprs.Add(loweredArgExpr, copiedLoweredArgExpr)
                                copiedLoweredArgExpr
                            | _ ->
                                loweredArgExpr
                        )

                    (E.Operation(textRange, loweredOp), copyRestoreExprs)
                    ||> Seq.fold (fun loweredExpr ((loweredArgExpr, copiedLoweredArgExpr)) ->
                        let restoreExpr =
                            let valueExpr = 
                                let exprs = copiedLoweredArgExpr.GetExpressions()
                                exprs[exprs.Length - 1] // get last expression
#if DEBUG || CHECKED
                            match valueExpr with
                            | E.Value _ -> ()
                            | _ -> OlyAssert.Fail("Expected value expression.")
#endif
                            OlyAssert.True(valueExpr.ResultType.IsPointer)
                            let loadValueExpr = E.Operation(EmptyTextRange, O.LoadFromAddress(valueExpr, valueExpr.ResultType.ElementType))
                            E.Operation(EmptyTextRange, O.StoreToAddress(loweredArgExpr, loadValueExpr, cenv.Module.GetTypeVoid()))
                        E.Sequential(
                            loweredExpr,
                            restoreExpr
                        )
                    )
                | _ ->
                    loweredExpr
            | _ ->
                loweredExpr

        theAshleyTransform loweredExpr

    let private LowerIfElse (cenv: cenv) (env: env) origExpr conditionExpr trueTargetExpr falseTargetExpr resultTy =
        let newConditionExpr = LowerExpression cenv env.NotReturnable conditionExpr
        let newTrueTargetExpr = LowerExpression cenv env trueTargetExpr
        let newFalseTargetExpr = LowerExpression cenv env falseTargetExpr

        let isPointer =
            match resultTy with
            | SpirvType.Pointer _ -> true
            | SpirvType.OlyByRef _ -> raise(NotImplementedException())
            | _ -> false

        let newTrueTargetExpr = if isPointer then newTrueTargetExpr else AutoDereferenceLoweredExpressionIfPossible newTrueTargetExpr
        let newFalseTargetExpr = if isPointer then newFalseTargetExpr else AutoDereferenceLoweredExpressionIfPossible newFalseTargetExpr

        if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
            origExpr
        else
            E.IfElse(newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, resultTy)

    let private LowerWhile (cenv: cenv) (env: env) origExpr conditionExpr bodyExpr resultTy =
        let newConditionExpr = LowerExpression cenv env.NotReturnable conditionExpr
        let newBodyExpr = LowerExpression cenv env.NotReturnable bodyExpr

        if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
            origExpr
        else
            E.While(newConditionExpr, newBodyExpr, resultTy)

    let private LowerExpressionAux (cenv: cenv) (env: env) (expr: E) =
        match expr with
        | E.None _ -> expr

        | E.Sequential _
        | E.Let _ ->
            LowerLinearExpression cenv env expr

        | E.Value(textRange, value) ->
            LowerValue cenv env expr textRange value

        | E.Operation(textRange, op) ->
            LowerOperation cenv env expr textRange op

        | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
            LowerIfElse cenv env expr conditionExpr trueTargetExpr falseTargetExpr resultTy

        | E.While(conditionExpr, bodyExpr, resultTy) ->
            LowerWhile cenv env expr conditionExpr bodyExpr resultTy

        | E.Phi _ ->
            raise(NotImplementedException())

        | E.Try _ ->
            raise(NotSupportedException("Exception handling is not supported in SPIR-V."))

    let private TryPostOrderLowerOperationExpression (cenv: cenv) (env: env) (expr: E) =
        match expr with
        | E.Operation(textRange, op) ->
            match op with
            | O.LoadArrayElementAddress _ ->
                raise(NotImplementedException(op.ToString()))

            | O.LoadField(field, receiverExpr, resultTy)
            | O.LoadFieldAddress(field, receiverExpr, _, SpirvType.Pointer(elementTy=resultTy)) ->
                let field = field.EmittedField

                match receiverExpr.ResultType with
                | SpirvType.Pointer(storageClass=storageClass) ->
                    if storageClass = StorageClass.Output then
                        raise(InvalidOperationException())

                    let resultTy = 
                        //if resultTy.IsPointer then
                        //    SpirvType.Pointer(0u, storageClass, resultTy) // fake pointer of pointer - logical addressing
                        //else
                            cenv.Module.GetTypePointer(storageClass, resultTy)

                    BuiltInExpressions.AccessChain(
                        receiverExpr, 
                        BuiltInExpressions.IndexConstantFromField cenv.Module field |> ImArray.createOne,
                        resultTy
                    )
                | _ ->
                    raise(InvalidOperationException())

            | O.Store(localIndex, rhsExpr, resultTy) ->
                let localTy = cenv.LocalTypes[localIndex]
                let localExpr = E.Value(EmptyTextRange, V.Local(localIndex, localTy))
                AssertPointerType localExpr
                E.Operation(textRange, O.StoreToAddress(localExpr, rhsExpr, resultTy))

            | O.StoreArgument(argIndex, rhsExpr, resultTy) ->
                let argTy = cenv.Function.Parameters[argIndex].Type
                let argExpr = E.Value(EmptyTextRange, V.Argument(argIndex, argTy))
                AssertPointerType argExpr
                E.Operation(textRange, O.StoreToAddress(argExpr, rhsExpr, resultTy))

            | O.StoreField(field, receiverExpr, rhsExpr, resultTy) ->
                let field = field.EmittedField
                OlyAssert.Equal(field.Type, rhsExpr.ResultType)

                match receiverExpr.ResultType with
                | SpirvType.Pointer(storageClass=storageClass) ->
                    if storageClass = StorageClass.Input then
                        raise(InvalidOperationException())

                    let accessChainExpr =
                        BuiltInExpressions.AccessChain(
                            receiverExpr, 
                            BuiltInExpressions.IndexConstantFromField cenv.Module field |> ImArray.createOne,
                            cenv.Module.GetTypePointer(storageClass, field.Type)
                        )

                    let rhsExpr = AutoDereferenceLoweredExpressionIfPossible rhsExpr
                    E.Operation(textRange,
                        O.StoreToAddress(accessChainExpr, rhsExpr, resultTy)
                    )
                | _ ->
                    raise(InvalidOperationException())

            | O.StoreArrayElement(receiverExpr, indexExprs, rhsExpr, resultTy) ->
                OlyAssert.Equal(1, indexExprs.Length) // 1-dim for now
                match receiverExpr.ResultType with
                | SpirvType.Pointer(_, storageClass, SpirvType.Struct(structBuilder)) when structBuilder.IsRuntimeArray ->
                    let indexExprs = List(indexExprs)
                    indexExprs.Insert(0, BuiltInExpressions.IndexConstantFromField cenv.Module (structBuilder.GetRuntimeArrayField()))
                    let accessChainExpr =
                        BuiltInExpressions.AccessChain(receiverExpr, ImArray.ofSeq indexExprs, cenv.Module.GetTypePointer(storageClass, rhsExpr.ResultType))
                    E.Operation(textRange, 
                        O.StoreToAddress(accessChainExpr, rhsExpr, resultTy)
                    )

                | _ ->
                    raise(InvalidOperationException())

            | O.LoadArrayElement(receiverExpr, indexExprs, resultTy) ->
                OlyAssert.Equal(1, indexExprs.Length) // 1-dim for now
                match receiverExpr.ResultType with
                | SpirvType.Pointer(_, storageClass, SpirvType.Struct(structBuilder)) when structBuilder.IsRuntimeArray ->
                    let indexExprs = List(indexExprs)
                    indexExprs.Insert(0, BuiltInExpressions.IndexConstantFromField cenv.Module (structBuilder.GetRuntimeArrayField()))
                    let accessChainExpr =
                        BuiltInExpressions.AccessChain(receiverExpr, ImArray.ofSeq indexExprs, cenv.Module.GetTypePointer(storageClass, resultTy))
                    E.Operation(textRange, 
                        O.LoadFromAddress(accessChainExpr, resultTy)
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
                | SpirvType.OlyByRef(elementTy=elementTy) ->
                    let resultTy = cenv.Module.GetTypePointer(StorageClass.Function, elementTy)
                    //let resultTy = SpirvType.Pointer(0u, StorageClass.Function, resultTy) // fake pointer of pointer - logical addressing
                    expr.WithResultType(resultTy)
                | SpirvType.RuntimeArray _ ->
                    raise(InvalidOperationException($"Runtime array is not used correctly:\n{expr}"))
                | _ ->
                    expr

        let expr = TryPostOrderLowerOperationExpression cenv env expr

        if env.IsReturnable then
            match expr with
            | E.Let _
            | E.Sequential _
            | E.IfElse _ -> expr
            | _ ->
                // We do this because we cannot return a pointer in Logical addressing model
                AutoDereferenceLoweredExpressionIfPossible expr
        else
            expr

    let Lower cenv (expr: E) =
        let env = { IsReturnable = true }
        LowerExpression cenv env expr
            