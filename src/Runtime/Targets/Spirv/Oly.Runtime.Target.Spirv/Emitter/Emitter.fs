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

[<NoEquality;NoComparison>]
type g =
    {
        TypeVoid: SpirvType
    }

[<Sealed>]
type SpirvEmitter() =

    let mutable g = Unchecked.defaultof<g>

    let builder = SpirvModuleBuilder()

    let mutable cachedFloat32Ty = Unchecked.defaultof<SpirvType>
    let getFloat32Type() =
        if isNull (box cachedFloat32Ty) then
            cachedFloat32Ty <- SpirvType.Float32(builder.NewIdResult())
            builder.AddType(cachedFloat32Ty)
        cachedFloat32Ty

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
                let ty = SpirvType.Vector2(builder.NewIdResult(), getFloat32Type())
                builder.AddType(ty)
                ty
            | "vec3" -> 
                let ty = SpirvType.Vector3(builder.NewIdResult(), getFloat32Type())
                builder.AddType(ty)
                ty
            | "vec4" -> 
                let ty = SpirvType.Vector4(builder.NewIdResult(), getFloat32Type())
                builder.AddType(ty)
                ty
            | _ ->
                InvalidOperation()

        member this.EmitField(enclosingTy: SpirvType, flags: OlyIRFieldFlags, name: string, ty: SpirvType, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray, constValueOpt: OlyIRConstant<SpirvType,SpirvFunction> option): SpirvField = 
            raise (System.NotImplementedException())

        member this.EmitFieldInstance(enclosingTy: SpirvType, formalField: SpirvField): SpirvField = 
            NotSupported()

        member this.EmitFunctionBody(body: Lazy<OlyIRFunctionBody<SpirvType,SpirvFunction,SpirvField>>, tier: OlyIRFunctionTier, func: SpirvFunction): unit = 
            let body = body.Value

            match func with
            | SpirvFunction.Builder(funcBuilder) ->
                let returnTy =
                    match funcBuilder.Type with
                    | SpirvType.Function(returnTy=returnTy) -> returnTy // This is the real return type!
                    | _ -> InvalidOperation()
                funcBuilder.Instructions <-
                    [
                        OpFunction(returnTy.IdResult, funcBuilder.IdResult, FunctionControl.None, funcBuilder.Type.IdResult)

                        OpLabel(builder.NewIdResult())
                        // TODO: Convert 'body' to instructions.
                        OpReturn 

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
                let funcBuilder = SpirvFunctionBuilder(builder, builder.NewIdResult(), enclosingTy, name, flags, pars, returnTy, g.TypeVoid)
                let func = SpirvFunction.Builder(funcBuilder)
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
            let ty = SpirvType.Bool(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeByRef(elementTy: SpirvType, kind: OlyIRByRefKind): SpirvType = 
            let ty =
                match kind with
                | OlyIRByRefKind.ReadWrite ->
                    SpirvType.ByRef(builder.NewIdResult(), elementTy, SpirvByRefKind.ReadWrite)
                | OlyIRByRefKind.Read ->
                    SpirvType.ByRef(builder.NewIdResult(), elementTy, SpirvByRefKind.ReadOnly)
            builder.AddType(ty)
            ty

        member this.EmitTypeChar16(): SpirvType = 
            let ty = SpirvType.Char16(builder.NewIdResult())
            builder.AddType(ty)
            ty

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
            getFloat32Type()

        member this.EmitTypeFloat64(): SpirvType = 
            let ty = SpirvType.Float64(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeFunction(argTys: SpirvType imarray, returnTy: SpirvType, kind: OlyIRFunctionKind): SpirvType = 
            NotSupported()

        member this.EmitTypeGenericInstance(ty: SpirvType, tyArgs: SpirvType imarray): SpirvType = 
            NotSupported()

        member this.EmitTypeHigherVariable(index: int32, tyInst: SpirvType imarray, kind: OlyIRTypeVariableKind): SpirvType = 
            NotSupported()

        member this.EmitTypeInt16(): SpirvType = 
            let ty = SpirvType.Int16(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeInt32(): SpirvType = 
            let ty = SpirvType.Int32(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeInt64(): SpirvType = 
            let ty = SpirvType.Int64(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeInt8(): SpirvType = 
            let ty = SpirvType.Int8(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeNativeFunctionPtr(arg1: OlyILCallingConvention, argTys: SpirvType imarray, returnTy: SpirvType): SpirvType = 
            NotSupported()

        member this.EmitTypeNativeInt(): SpirvType = 
            let ty = SpirvType.NativeInt(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeNativePtr(elementTy: SpirvType): SpirvType = 
            let ty = SpirvType.NativePointer(builder.NewIdResult(), elementTy)
            builder.AddType(ty)
            ty

        member this.EmitTypeNativeUInt(): SpirvType = 
            let ty = SpirvType.NativeUInt(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeRefCell(ty: SpirvType): SpirvType = 
            NotSupported()

        member this.EmitTypeTuple(elementTys: SpirvType imarray, elementNames: string imarray): SpirvType = 
            let ty = SpirvType.Tuple(builder.NewIdResult(), elementTys, elementNames)
            builder.AddType(ty)
            ty

        member this.EmitTypeUInt16(): SpirvType = 
            let ty = SpirvType.UInt16(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeUInt32(): SpirvType = 
            let ty = SpirvType.UInt32(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeUInt64(): SpirvType = 
            let ty = SpirvType.UInt64(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeUInt8(): SpirvType = 
            let ty = SpirvType.UInt8(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeUnit(): SpirvType = 
            let ty = SpirvType.Unit(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.EmitTypeUtf16(): SpirvType = 
            NotSupported()

        member this.EmitTypeVariable(index: int32, kind: OlyIRTypeVariableKind): SpirvType = 
            NotSupported()

        member this.EmitTypeVoid(): SpirvType = 
            let ty = SpirvType.Void(builder.NewIdResult())
            builder.AddType(ty)
            ty

        member this.Initialize(vm: IOlyVirtualMachine<SpirvType,SpirvFunction,SpirvField>): unit = 
            g <-
                {
                    TypeVoid = vm.GetTypeVoid()
                }

