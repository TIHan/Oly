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

        member this.SetLocal(localIndex: int32, ty: SpirvType) =
            let ty =
                match ty with
                | SpirvType.Pointer(elementTy=elementTy) -> 
                    this.Module.GetTypePointer(StorageClass.Function, elementTy)
                | _ -> 
                    raise(InvalidOperationException("Expected a pointer type."))

            let idResult = this.Module.NewIdResult()
            this.Locals[localIndex] <- idResult
            this.LocalTypes[localIndex] <- ty
            ty

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
    let private AutoDereferenceIfPossible (expr: E) =
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
            let newRhsExpr = LowerLinearExpression cenv envNotReturnable rhsExpr
            let resultTy = newRhsExpr.ResultType
            match resultTy with
            | SpirvType.Pointer _ ->
                cenv.SetLocal(localIndex, resultTy)
                |> ignore
            | _ ->
                cenv.SetLocal(localIndex, cenv.Module.GetTypePointer(StorageClass.Function, resultTy))
                |> ignore
            E.Sequential(
                // Lower Store to StoreToAddress.
                E.Operation(EmptyTextRange, O.Store(localIndex, newRhsExpr, cenv.Module.GetTypeVoid()))
                |> LowerExpression cenv envNotReturnable,
                LowerLinearExpression cenv env bodyExpr
            )

        | _ ->
            LowerExpression cenv env expr

    let private LowerArgument (cenv: cenv) (_env: env) textRange argIndex =
        let resultTy = cenv.Function.Parameters[argIndex].Type
        E.Value(textRange, V.Argument(argIndex, CheckArgumentOrLocalType resultTy))
        
    let private LowerLocal (cenv: cenv) (_env: env) textRange localIndex =
        let resultTy = cenv.LocalTypes[localIndex]
        E.Value(textRange, V.Local(localIndex, CheckArgumentOrLocalType resultTy))

    let private LowerValue (cenv: cenv) (env: env) origExpr textRange value =
        match value with
        | V.Argument(argIndex, _)
        | V.ArgumentAddress(argIndex, _, _)->
            LowerArgument cenv env textRange argIndex            
        | V.Local(localIndex, _)
        | V.LocalAddress(localIndex, _, _) ->
            LowerLocal cenv env textRange localIndex
        | _ ->
            origExpr

    let private LowerOperation (cenv: cenv) (env: env) origExpr textRange (op: O) =
        let newOp =
            let env = env.NotReturnable
            op.MapAndReplaceArguments (fun i argExpr ->
                let newArgExpr = LowerExpression cenv env argExpr
                match op with
                | O.Store _ ->
                    AutoDereferenceIfPossible newArgExpr

                | O.StoreArgument _ ->
                    raise(NotImplementedException(op.ToString()))

                | O.StoreField _ ->
                    if i = 0 then
                        AssertPointerType newArgExpr
                        newArgExpr
                    else
                        AutoDereferenceIfPossible newArgExpr

                | O.StoreToAddress _ ->
                    if i = 1 then
                        AutoDereferenceIfPossible newArgExpr
                    else
                        AssertPointerType newArgExpr
                        newArgExpr

                | O.StoreArrayElement _ ->
                    // In Spirv, the index must always be an uint32.
                    if i = 0 then
                        AssertPointerType newArgExpr
                        newArgExpr
                    elif i = 1 then // 1 dim - the index
                        let newArgExpr = AutoDereferenceIfPossible newArgExpr 
                        match newArgExpr.ResultType with
                        | SpirvType.UInt32 _ ->
                            newArgExpr
                        | _ ->
                            E.Operation(EmptyTextRange, O.Cast(newArgExpr, cenv.Module.GetTypeUInt32()))
                    else
                        AutoDereferenceIfPossible newArgExpr

                | O.LoadArrayElement _ ->
                    if i = 0 then
                        AssertPointerType newArgExpr
                        newArgExpr
                    else
                        let newArgExpr = AutoDereferenceIfPossible newArgExpr 
                        match newArgExpr.ResultType with
                        | SpirvType.UInt32 _ ->
                            newArgExpr
                        | _ ->
                            E.Operation(EmptyTextRange, O.Cast(newArgExpr, cenv.Module.GetTypeUInt32()))

                | O.LoadField _
                | O.LoadFieldAddress _
                | O.LoadFromAddress _ ->
                    AssertPointerType newArgExpr
                    newArgExpr
                
                | O.Equal _
                | O.Cast _
                | BuiltInOperations.NewVec4 _ ->
                    AutoDereferenceIfPossible newArgExpr

                | BuiltInOperations.AccessChain _ ->
                    if i = 0 then
                        AssertPointerType newArgExpr
                        newArgExpr
                    else
                        AutoDereferenceIfPossible newArgExpr

                | BuiltInOperations.PtrAccessChain _ ->
                    if i = 0 then
                        AssertPointerType newArgExpr
                        newArgExpr
                    else
                        AutoDereferenceIfPossible newArgExpr

                | O.Call _ ->
                    match newArgExpr.ResultType with
                    | SpirvType.Pointer _ -> newArgExpr
                    | resultTy ->
                        let localTy, localIndex = cenv.AddLocal(cenv.Module.GetTypePointer(StorageClass.Function, resultTy))
                        let valueExpr = E.Value(EmptyTextRange, V.Local(localIndex, localTy))
                        E.Sequential(
                            E.Operation(EmptyTextRange, O.StoreToAddress(valueExpr, newArgExpr, cenv.Module.GetTypeVoid())),
                            valueExpr
                        )                    

                | _ ->
                    raise(NotImplementedException(op.ToString()))
            )

        let handleCallVariable (var: SpirvVariable) (irFunc: OlyIRFunction<_, _, _>) argExprs resultTy =
            match resultTy with
            | SpirvType.Void _ ->
                O.Call(irFunc, argExprs, resultTy)
            | _ ->
                O.Call(irFunc, argExprs, var.Type)

        let newOp =
            match newOp with
            | O.Call(irFunc, argExprs, resultTy) ->
                match irFunc.EmittedFunction with
                | SpirvFunction.Variable var ->
                    let argExprs = argExprs |> ImArray.map AutoDereferenceIfPossible
                    handleCallVariable var irFunc argExprs resultTy
                | SpirvFunction.LazyVariable lazyVar ->
                    let argExprs = argExprs |> ImArray.map AutoDereferenceIfPossible
                    handleCallVariable lazyVar.Value irFunc argExprs resultTy
                | SpirvFunction.ExtendedInstruction _ ->
                    O.Call(irFunc, argExprs |> ImArray.map AutoDereferenceIfPossible, resultTy)
                | _ ->
                    newOp
            | _ ->
                newOp

        match newOp with
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
                raise(NotImplementedException(newOp.ToString()))
        | _ ->
            if newOp = op then
                origExpr
            else
                E.Operation(textRange, newOp)

    let private LowerIfElse (cenv: cenv) (env: env) origExpr conditionExpr trueTargetExpr falseTargetExpr resultTy =
        let newConditionExpr = LowerExpression cenv env.NotReturnable conditionExpr
        let newTrueTargetExpr = LowerExpression cenv env trueTargetExpr
        let newFalseTargetExpr = LowerExpression cenv env falseTargetExpr

        let isPointer =
            match resultTy with
            | SpirvType.Pointer _ -> true
            | SpirvType.OlyByRef _ -> raise(NotImplementedException())
            | _ -> false

        let newTrueTargetExpr = if isPointer then newTrueTargetExpr else AutoDereferenceIfPossible newTrueTargetExpr
        let newFalseTargetExpr = if isPointer then newFalseTargetExpr else AutoDereferenceIfPossible newFalseTargetExpr

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
                OlyAssert.Equal(field.Type, resultTy)

                match receiverExpr.ResultType with
                | SpirvType.Pointer(storageClass=storageClass) ->
                    if storageClass = StorageClass.Output then
                        raise(InvalidOperationException())

                    BuiltInExpressions.AccessChain(
                        receiverExpr, 
                        BuiltInExpressions.IndexConstantFromField cenv.Module field |> ImArray.createOne,
                        cenv.Module.GetTypePointer(storageClass, field.Type)
                    )
                | _ ->
                    raise(InvalidOperationException())

            | O.Store(localIndex, rhsExpr, resultTy) ->
                let localTy = cenv.LocalTypes[localIndex]
                let localExpr = E.Value(EmptyTextRange, V.Local(localIndex, localTy))
                AssertPointerType localExpr
                E.Operation(textRange, O.StoreToAddress(localExpr, rhsExpr, resultTy))

            | O.StoreArgument _ ->
                raise(NotImplementedException(op.ToString()))

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

                    E.Operation(textRange,
                        O.StoreToAddress(accessChainExpr, AutoDereferenceIfPossible rhsExpr, resultTy)
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
                    expr.WithResultType(cenv.Module.GetTypePointer(StorageClass.Function, elementTy))
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
                AutoDereferenceIfPossible expr
        else
            expr

    let Lower cenv (expr: E) =
        let env = { IsReturnable = true }
        LowerExpression cenv env expr
            