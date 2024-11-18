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
type g =
    {
        TypeVoid: SpirvType
        TypeInt32: SpirvType
        TypeFloat32: SpirvType
    }

[<NoEquality;NoComparison>]
type cenv =
    {
        Function: SpirvFunctionBuilder
        Module: SpirvModuleBuilder
        Instructions: List<Instruction>
        g: g
    }

    member this.IsEntryPoint =
        this.Function.IsEntryPoint

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

    let GenOperation (cenv: cenv) (env: env) (op: O) =
        match op with
        | O.New(func, argExprs, _) ->
            match func.EmittedFunction with
            | SpirvFunction.NewVector4(argTys) ->
                OlyAssert.Equal(argTys.Length, argExprs.Length)
                match argTys.Length with
                | 1 ->
                    match argExprs[0] with
                    | E.Value(value=V.Constant(C.Float32 value, _)) ->
                        cenv.Module.CreateConstantVector4Float32(value, value, value, value)
                    | _ ->
                        raise(NotImplementedException(op.ToString()))  
                | _ ->
                    raise(NotImplementedException(op.ToString()))                  
            | _ ->
               raise(NotImplementedException(op.ToString())) 
        | _ ->
            raise(NotImplementedException(op.ToString()))

    let GenConstant (cenv: cenv) (env: env) (cns: C) =
        match cns with
        | C.Int32 value ->
            cenv.Module.CreateConstantInt32(value)
        | C.Float32 value ->
            cenv.Module.CreateConstantFloat32(value)
        | _ ->
            raise(NotImplementedException(cns.ToString()))

    let GenValue (cenv: cenv) (env: env) (value: V) =
        match value with
        | V.Constant(cns, _) ->
            GenConstant cenv env cns
        | _ ->
            raise(NotImplementedException(value.ToString()))


    let private GenExpressionAux (cenv: cenv) (env: env) (expr: E) : IdResult =
        match expr with
        | E.Sequential _
        | E.Let _ ->
            GenLinearExpression cenv env expr

        | E.None _ ->
            0u // Nothing

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
            if idRef = 0u then
                OpReturn |> emitInstruction cenv
            else
                //if cenv.IsEntryPoint then
                //    // Special!
                //    match cenv.Function.ReturnType with
                //    | SpirvType.Vector4 _ ->
                //        let idResult = cenv.Module.NewIdResult()
                //        let outputPar1 = cenv.Function.OutputParameterIdResults[0]
                //        OpAccessChain(outputPar1.TypePointerOfBlockIdResult, idResult, outputPar1.VariableOfPointerOfBlockIdResult, [cenv.Module.CreateConstantInt32(0)]) |> emitInstruction cenv
                //        OpStore(idResult, idRef, None) |> emitInstruction cenv
                //    | _ ->
                //        raise(NotImplementedException("returnable value for entry-point"))
                //    OpReturn |> emitInstruction cenv
                //else
                    raise(NotImplementedException("returnable value"))
        idRef
