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
type SpirvOutput =
    {
        Module: SpirvModule
    }

[<AutoOpen>]
module private Helpers =

    let NotSupported() =
        raise(NotSupportedException("Construct not supported in Spirv."))

    let InvalidOperation() =
        raise(InvalidOperationException())

[<Sealed>]
type SpirvEmitter() =

    let builder = SpirvModuleBuilder()

    member this.EmitOutput(_isDebuggable: bool) =
        {
            Module = builder.Build()
        }

    interface IOlyRuntimeEmitter<SpirvType, SpirvFunction, SpirvField> with

        member this.EmitExternalType(externalPlatform: string, externalPath: string imarray, externalName: string, enclosing: Choice<string imarray,SpirvType>, kind: OlyILEntityKind, flags: OlyIRTypeFlags, name: string, tyParCount: int): SpirvType = 
            if externalPlatform <> "spirv" then InvalidOperation()
            if externalPath.Length <> 1 then InvalidOperation()
            if externalPath[0] <> "__oly_spirv_" then InvalidOperation()

            match externalName with
            | "vec2" -> 
                builder.GetTypeVector2Float32()
            | "vec3" -> 
                builder.GetTypeVector3Float32()
            | "vec4" -> 
                builder.GetTypeVector4Float32()
            | _ ->
                InvalidOperation()

        member this.EmitField(enclosingTy: SpirvType, flags: OlyIRFieldFlags, name: string, ty: SpirvType, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray, constValueOpt: OlyIRConstant<SpirvType,SpirvFunction> option): SpirvField = 
            raise (System.NotImplementedException())

        member this.EmitFieldInstance(enclosingTy: SpirvType, formalField: SpirvField): SpirvField = 
            NotSupported()

        member this.EmitFunctionBody(body: Lazy<OlyIRFunctionBody<SpirvType,SpirvFunction,SpirvField>>, tier: OlyIRFunctionTier, func: SpirvFunction): unit = 
            let body = body.Value

            let cenv = { Instructions = List(); Module = builder; Function = (match func with SpirvFunction.Function(x) -> x | _ -> failwith "Invalid func") }
            let loweringCenv = { Lowering.cenv.Module = builder; Lowering.cenv.Function = cenv.Function }
            CodeGen.Gen cenv (Lowering.Lower loweringCenv body.Expression)

            match func with
            | SpirvFunction.Function(funcBuilder) ->
                funcBuilder.Instructions <-
                    [
                        OpFunction(funcBuilder.ReturnType.IdResult, funcBuilder.IdResult, FunctionControl.None, funcBuilder.Type.IdResult)

                        OpLabel(builder.NewIdResult())
                        yield! cenv.Instructions

                        OpFunctionEnd
                    ]
            | _ ->
                InvalidOperation()

        member this.EmitFunctionDefinition(externalInfoOpt: OlyIRFunctionExternalInfo option, enclosingTy: SpirvType, flags: OlyIRFunctionFlags, name: string, tyPars: OlyIRTypeParameter<SpirvType> imarray, pars: OlyIRParameter<SpirvType> imarray, returnTy: SpirvType, overrides: SpirvFunction option, sigKey: OlyIRFunctionSignatureKey, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray): SpirvFunction = 
            if tyPars.Length > 0 then NotSupported()
            if overrides.IsSome then NotSupported()
            if attrs.IsEmpty |> not then NotSupported()

            match externalInfoOpt with
            | Some(info) ->
                if info.Platform <> "spirv" then InvalidOperation()
                match info.Path |> Seq.toList with
                | ["__oly_spirv_"; "vec4"] when flags.IsInstance && flags.IsConstructor -> 
                    SpirvFunction.NewVector4(pars |> ImArray.map (fun x -> x.Type))
                | _ -> 
                    InvalidOperation()
            | _ ->
                let funcBuilder = SpirvFunctionBuilder(builder, builder.NewIdResult(), enclosingTy, name, flags, pars, returnTy, builder.GetTypeVoid())
                let func = SpirvFunction.Function(funcBuilder)
                builder.AddFunctionBuilder(funcBuilder)
                func

        member this.EmitFunctionInstance(enclosingTy: SpirvType, formalFunc: SpirvFunction, tyArgs: SpirvType imarray): SpirvFunction = 
            NotSupported()

        member this.EmitFunctionReference(enclosingTy: SpirvType, formalFunc: SpirvFunction): SpirvFunction = 
            NotSupported()

        member this.EmitProperty(enclosingTy: SpirvType, name: string, ty: SpirvType, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray, getterOpt: SpirvFunction option, setterOpt: SpirvFunction option): unit = 
            ()

        member this.EmitTypeArray(elementTy: SpirvType, rank: int, kind: OlyIRArrayKind): SpirvType = 
            NotSupported()

        member this.EmitTypeBaseObject(): SpirvType = 
            NotSupported()

        member this.EmitTypeBool(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeByRef(elementTy: SpirvType, kind: OlyIRByRefKind): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeChar16(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeConstantInt32(value: int32): SpirvType = 
            NotSupported()

        member this.EmitTypeDefinition(enclosing: Choice<string imarray,SpirvType>, kind: OlyILEntityKind, flags: OlyIRTypeFlags, name: string, tyParCount: int): SpirvType = 
            if tyParCount > 0 then NotSupported()
            match kind with
            | OlyILEntityKind.Module ->
                SpirvType.Module(enclosing, name)
            | OlyILEntityKind.Struct ->
                let namedTy =
                    {
                        IdResult = builder.NewIdResult()
                        Enclosing = enclosing
                        Name = name
                        Fields = List()
                    }
                let ty = SpirvType.Named(namedTy)
                builder.AddType(ty)
                ty
            | _ ->
                NotSupported()

        member this.EmitTypeDefinitionInfo(ty: SpirvType, enclosing: Choice<string imarray,SpirvType>, kind: OlyILEntityKind, flags: OlyIRTypeFlags, name: string, tyPars: OlyIRTypeParameter<SpirvType> imarray, extends: SpirvType imarray, implements: SpirvType imarray, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray, runtimeTyOpt: SpirvType option): unit = 
            ()

        member this.EmitTypeFloat32(): SpirvType = 
            builder.GetTypeFloat32()

        member this.EmitTypeFloat64(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeFunction(argTys: SpirvType imarray, returnTy: SpirvType, kind: OlyIRFunctionKind): SpirvType = 
            NotSupported()

        member this.EmitTypeGenericInstance(ty: SpirvType, tyArgs: SpirvType imarray): SpirvType = 
            NotSupported()

        member this.EmitTypeHigherVariable(index: int32, tyInst: SpirvType imarray, kind: OlyIRTypeVariableKind): SpirvType = 
            NotSupported()

        member this.EmitTypeInt16(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeInt32(): SpirvType = 
            builder.GetTypeInt32()

        member this.EmitTypeInt64(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeInt8(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeNativeFunctionPtr(arg1: OlyILCallingConvention, argTys: SpirvType imarray, returnTy: SpirvType): SpirvType = 
            NotSupported()

        member this.EmitTypeNativeInt(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeNativePtr(elementTy: SpirvType): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeNativeUInt(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeRefCell(ty: SpirvType): SpirvType = 
            NotSupported()

        member this.EmitTypeTuple(elementTys: SpirvType imarray, elementNames: string imarray): SpirvType = 
            let ty = SpirvType.Tuple(builder.NewIdResult(), elementTys, elementNames)
            builder.AddType(ty)
            ty

        member this.EmitTypeUInt16(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUInt32(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUInt64(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUInt8(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUnit(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUtf16(): SpirvType = 
            NotSupported()

        member this.EmitTypeVariable(index: int32, kind: OlyIRTypeVariableKind): SpirvType = 
            NotSupported()

        member this.EmitTypeVoid(): SpirvType = 
            builder.GetTypeVoid()

        member this.Initialize(vm: IOlyVirtualMachine<SpirvType,SpirvFunction,SpirvField>): unit = 
            ()

