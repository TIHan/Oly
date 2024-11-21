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

type E = OlyIRExpression<SpirvType, SpirvFunction, SpirvField>
type O = OlyIROperation<SpirvType, SpirvFunction, SpirvField>
type V = OlyIRValue<SpirvType, SpirvFunction, SpirvField>
type C = OlyIRConstant<SpirvType, SpirvFunction>

[<NoEquality;NoComparison>]
type cenv =
    {
        Function: SpirvFunctionBuilder
        Module: SpirvModuleBuilder
        Instructions: List<Instruction>
    }

    member this.IsEntryPoint =
        this.Function.IsEntryPoint

    member this.GetArgumentIdRef(argIndex: int32) =
        this.Function.Parameters[argIndex].VariableIdRef

[<NoEquality;NoComparison>]
type env =
    {
        IsReturnable: bool
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

    let Gen (cenv: cenv) (expr: E) =
        let env = { IsReturnable = true }
        GenExpression cenv env expr
        |> ignore

    let GenLinearExpression (cenv: cenv) (env: env) (expr: E) : IdResult =
        match expr with
        | E.Sequential(expr1, expr2) ->
            GenLinearExpression cenv env.NotReturnable expr1 |> ignore
            GenLinearExpression cenv env expr2

        | E.Let _ ->
            raise(NotImplementedException(expr.ToString()))

        | _ ->
            GenExpression cenv env expr

    let GenOperation (cenv: cenv) (env: env) (op: O) : IdRef =
        match op with
        | O.New(func, argExprs, _) ->
            match func.EmittedFunction with
            | SpirvFunction.NewVector4(argTys) ->
                OlyAssert.Equal(argTys.Length, argExprs.Length)
                match argTys.Length with
                | 1 ->
                    match argExprs[0] with
                    | E.Value(value=V.Constant(C.Float32 value, _)) ->
                        cenv.Module.GetConstantVector4Float32(value, value, value, value)
                    | _ ->
                        raise(NotImplementedException(op.ToString()))

                | 3 ->
                    match argTys[0], argTys[1], argTys[2] with
                    | SpirvType.Vector2 _, SpirvType.Float32 _, SpirvType.Float32 _ ->
                        let idRefs =
                            let env = env.NotReturnable
                            argExprs
                            |> ImArray.map (GenExpression cenv env)

                        let arg0IdResult = cenv.Module.NewIdResult()
                        OpCompositeExtract(cenv.Module.GetTypeFloat32().IdResult, arg0IdResult, idRefs[0], [0u]) |> emitInstruction cenv

                        let arg1IdResult = cenv.Module.NewIdResult()
                        OpCompositeExtract(cenv.Module.GetTypeFloat32().IdResult, arg1IdResult, idRefs[0], [1u]) |> emitInstruction cenv

                        let arg2IdRef = idRefs[1]
                        let arg3IdRef = idRefs[2]

                        let idResult = cenv.Module.NewIdResult()
                        OpCompositeConstruct(cenv.Module.GetTypeVector4Float32().IdResult, idResult, [arg0IdResult; arg1IdResult; arg2IdRef; arg3IdRef]) |> emitInstruction cenv
                        idResult
                    | _ ->
                        raise(NotImplementedException(op.ToString()))
                | _ ->
                    raise(NotImplementedException(op.ToString()))
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
            | O.StoreField(field, receiverExpr, _, _) ->
                let receiverIdRef = idRefs[0]
                let rhsIdRef = idRefs[1]

                let idResult = cenv.Module.NewIdResult()
                OpAccessChain(receiverExpr.ResultType.IdResult, idResult, receiverIdRef, [cenv.Module.GetConstantInt32(field.EmittedField.Index)]) |> emitInstruction cenv
                OpStore(idResult, rhsIdRef, None) |> emitInstruction cenv
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

            | _ ->
                raise(NotImplementedException(op.ToString()))

    let GenConstant (cenv: cenv) (env: env) (cns: C) =
        match cns with
        | C.Int32 value ->
            cenv.Module.GetConstantInt32(value)
        | C.Float32 value ->
            cenv.Module.GetConstantFloat32(value)
        | _ ->
            raise(NotImplementedException(cns.ToString()))

    let GenValue (cenv: cenv) (env: env) (value: V) =
        match value with
        | V.Constant(cns, _) ->
            GenConstant cenv env cns

        | V.Argument(argIndex, _) ->
            cenv.GetArgumentIdRef(argIndex)

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

        | E.IfElse _ ->
            raise(NotImplementedException())

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
