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

[<Sealed>]
type SpirvEmitter(executionModel) =

    let builder = SpirvModuleBuilder(executionModel)

    member this.EmitOutput(_isDebuggable: bool) =
        {
            Module = builder.Build()
        }

    interface IOlyRuntimeEmitter<SpirvType, SpirvFunction, SpirvField> with

        member this.EmitExternalType(externalPlatform: string, externalPath: string imarray, externalName: string, _enclosing: Choice<string imarray,SpirvType>, _kind: OlyILEntityKind, _flags: OlyIRTypeFlags, _name: string, _tyParCount: int): SpirvType = 
            if externalPlatform <> "spirv" then raise(InvalidOperationException())
            if externalPath.Length <> 1 then raise(InvalidOperationException())
            if externalPath[0] <> "__oly_spirv_" then raise(InvalidOperationException())

            match BuiltInTypes.TryGetByName(builder, externalName) with
            | ValueSome ty -> ty
            | _ ->
                if BuiltInFunctions.IsValid(externalName) then
                    SpirvType.Invalid
                else
                    raise(InvalidOperationException())

        member this.EmitField(enclosingTy: SpirvType, flags: OlyIRFieldFlags, name: string, ty: SpirvType, index: int32, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray, constValueOpt: OlyIRConstant<SpirvType,SpirvFunction> option): SpirvField = 
            if enclosingTy.IsStructBuilder then
                let namedTyBuilder = enclosingTy.AsStructBuilder

                let attrs =
                    attrs
                    |> ImArray.choose (fun attr ->
                        match attr with
                        | OlyIRAttribute(ctor, _, _) ->
                            match ctor.TryGetBuiltIn() with
                            | ValueSome(builtInFunc) -> Some(builtInFunc)
                            | _ -> None
                    )

                let field = 
                    {
                        Index = index
                        Name = name
                        Type = ty
                        Flags = SpirvFieldFlags.None
                        Attributes = attrs
                    }

                namedTyBuilder.Fields.Add(field)
                field
            else
                match enclosingTy with
                | SpirvType.Vec4 _
                | SpirvType.Vec3 _
                | SpirvType.Vec2 _ ->
                    {
                        Index = index
                        Name = name
                        Type = ty
                        Flags = SpirvFieldFlags.None
                        Attributes = ImArray.empty
                    }
                | _ ->
                    raise(NotImplementedException())

        member this.EmitFieldInstance(enclosingTy: SpirvType, formalField: SpirvField): SpirvField = 
            raise(NotSupportedException())

        member this.EmitFunctionBody(body: Lazy<OlyIRFunctionBody<SpirvType,SpirvFunction,SpirvField>>, _tier: OlyIRFunctionTier, func: SpirvFunction): unit = 
            let body = body.Value

            let cenv = 
                { 
                    Instructions = List()
                    Module = builder
                    Function = (match func with SpirvFunction.Function(x) -> x | _ -> failwith "Invalid func")
                    Locals = List(Array.zeroCreate body.LocalCount)
                    LocalTypes = List(Array.zeroCreate body.LocalCount)
                }
            let loweringCenv = { Lowering.cenv.Module = builder; Lowering.cenv.Function = cenv.Function }
            CodeGen.Gen cenv (Lowering.Lower loweringCenv body.Expression)

            match func with
            | SpirvFunction.Function(funcBuilder) ->
                funcBuilder.Instructions <-
                    [
                        let pars =
                            if funcBuilder.IsEntryPoint then
                                funcBuilder.EntryPointParameters
                            else
                                funcBuilder.Parameters

                        for par in pars do
                            match par.Type with
                            | SpirvType.NativePointer(_, storageClass, _) ->
                                yield! par.DecorateInstructions
                                OpVariable(par.Type.IdResult, par.VariableIdRef, storageClass, None)
                            | _ ->
                                OpVariable(par.Type.IdResult, par.VariableIdRef, StorageClass.Private, None)

                        OpFunction(funcBuilder.ReturnType.IdResult, funcBuilder.IdResult, FunctionControl.None, funcBuilder.Type.IdResult)

                        yield! cenv.Instructions

                        OpFunctionEnd
                    ]
            | _ ->
                raise(InvalidOperationException())

        member this.EmitFunctionDefinition(externalInfoOpt: OlyIRFunctionExternalInfo option, enclosingTy: SpirvType, flags: OlyIRFunctionFlags, name: string, tyPars: OlyIRTypeParameter<SpirvType> imarray, pars: OlyIRParameter<SpirvType, SpirvFunction> imarray, returnTy: SpirvType, overrides: SpirvFunction option, sigKey: OlyIRFunctionSignatureKey, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray): SpirvFunction = 
            if tyPars.Length > 0 then raise(NotSupportedException())
            if overrides.IsSome then raise(NotSupportedException())
            if attrs.IsEmpty |> not then raise(NotSupportedException())

            match externalInfoOpt with
            | Some(info) ->
                if info.Platform <> "spirv" then raise(InvalidOperationException())
                match BuiltInFunctions.TryGetBuiltInFunction(info.Path, pars |> ImArray.map (fun x -> x.Type), returnTy, flags) with
                | Some func -> func
                | _ -> raise(InvalidOperationException())
            | _ ->
                let funcBuilder = builder.CreateFunctionBuilder(enclosingTy, name, flags, pars, returnTy)
                funcBuilder.AsFunction

        member this.EmitFunctionInstance(enclosingTy: SpirvType, formalFunc: SpirvFunction, tyArgs: SpirvType imarray): SpirvFunction = 
            raise(NotSupportedException())

        member this.EmitFunctionReference(enclosingTy: SpirvType, formalFunc: SpirvFunction): SpirvFunction = 
            raise(NotSupportedException())

        member this.EmitProperty(enclosingTy: SpirvType, name: string, ty: SpirvType, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray, getterOpt: SpirvFunction option, setterOpt: SpirvFunction option): unit = 
            ()

        member this.EmitTypeArray(elementTy: SpirvType, rank: int, kind: OlyIRArrayKind): SpirvType = 
            if rank > 1 || rank <= 0 then
                raise(InvalidOperationException())
            match kind with
            | OlyIRArrayKind.Immutable ->
                builder.GetTypeArray(SpirvArrayKind.Immutable, elementTy)
            | OlyIRArrayKind.Mutable ->
                builder.GetTypeArray(SpirvArrayKind.Mutable, elementTy)

        member this.EmitTypeBaseObject(): SpirvType = 
            SpirvType.Invalid // TODO: What should we do here? Ideally, we should not ever emit an 'object' type in the first place. Structs always inherits from 'object', but maybe we should not do that.

        member this.EmitTypeBool(): SpirvType = 
            builder.GetTypeBool()

        member this.EmitTypeByRef(elementTy: SpirvType, kind: OlyIRByRefKind): SpirvType = 
            match kind with
            | OlyIRByRefKind.ReadWrite ->
                SpirvType.ByRef(SpirvByRefKind.ReadWrite, elementTy)
            | OlyIRByRefKind.ReadOnly ->
                SpirvType.ByRef(SpirvByRefKind.ReadOnly, elementTy)
            | OlyIRByRefKind.WriteOnly ->
                SpirvType.ByRef(SpirvByRefKind.WriteOnly, elementTy)

        member this.EmitTypeChar16(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeConstantInt32(value: int32): SpirvType = 
            raise(NotSupportedException())

        member this.EmitTypeDefinition(enclosing: Choice<string imarray,SpirvType>, kind: OlyILEntityKind, flags: OlyIRTypeFlags, name: string, tyParCount: int): SpirvType = 
            if tyParCount > 0 then raise(NotSupportedException())
            match kind with
            | OlyILEntityKind.Module ->
                SpirvType.Module(enclosing, name)
            | OlyILEntityKind.Struct ->
                let namedTy = builder.CreateTypeStructBuilder(enclosing, name)
                namedTy.AsType
            | _ ->
                raise(NotSupportedException())

        member this.EmitTypeDefinitionInfo(ty: SpirvType, enclosing: Choice<string imarray,SpirvType>, kind: OlyILEntityKind, flags: OlyIRTypeFlags, name: string, tyPars: OlyIRTypeParameter<SpirvType> imarray, extends: SpirvType imarray, implements: SpirvType imarray, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray, runtimeTyOpt: SpirvType option): unit = 
            if ty.IsStructBuilder then
                ty.AsStructBuilder.Attributes <-
                    attrs
                    |> ImArray.choose (fun attr ->
                        match attr with
                        | OlyIRAttribute(ctor=ctor) ->
                            match ctor.TryGetBuiltIn() with
                            | ValueSome(builtInFunc) -> Some(builtInFunc)
                            | _ ->
                                None
                    )

        member this.OnTypeDefinitionEmitted(ty) =
            if ty.IsStructBuilder then
                builder.AddTypeStructBuilder(ty.AsStructBuilder)

        member this.EmitTypeFloat32(): SpirvType = 
            builder.GetTypeFloat32()

        member this.EmitTypeFloat64(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeFunction(argTys: SpirvType imarray, returnTy: SpirvType, kind: OlyIRFunctionKind): SpirvType = 
            raise(NotSupportedException())

        member this.EmitTypeGenericInstance(ty: SpirvType, tyArgs: SpirvType imarray): SpirvType = 
            raise(NotSupportedException())

        member this.EmitTypeHigherVariable(index: int32, tyInst: SpirvType imarray, kind: OlyIRTypeVariableKind): SpirvType = 
            raise(NotSupportedException())

        member this.EmitTypeInt16(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeInt32(): SpirvType = 
            builder.GetTypeInt32()

        member this.EmitTypeInt64(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeInt8(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeNativeFunctionPtr(arg1: OlyILCallingConvention, argTys: SpirvType imarray, returnTy: SpirvType): SpirvType = 
            raise(NotSupportedException())

        member this.EmitTypeNativeInt(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeNativePtr(elementTy: SpirvType): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeNativeUInt(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeRefCell(ty: SpirvType): SpirvType = 
            raise(NotSupportedException())

        member this.EmitTypeTuple(elementTys: SpirvType imarray, elementNames: string imarray): SpirvType = 
            builder.GetTypeTuple(elementTys, elementNames)

        member this.EmitTypeUInt16(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUInt32(): SpirvType = 
            builder.GetTypeUInt32()

        member this.EmitTypeUInt64(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUInt8(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUnit(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeUtf16(): SpirvType = 
            raise(NotSupportedException())

        member this.EmitTypeVariable(index: int32, kind: OlyIRTypeVariableKind): SpirvType = 
            raise(NotSupportedException())

        member this.EmitTypeVoid(): SpirvType = 
            builder.GetTypeVoid()

        member this.Initialize(vm: IOlyVirtualMachine<SpirvType,SpirvFunction,SpirvField>): unit = 
            ()

