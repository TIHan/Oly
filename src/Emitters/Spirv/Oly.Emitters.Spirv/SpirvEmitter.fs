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
type SpirvEmitter(majorVersion: uint, minorVersion: uint, executionModel) =

    let builder = SpirvModuleBuilder(majorVersion, minorVersion, executionModel)

    member this.EmitOutput(_isDebuggable: bool) =
        {
            Module = builder.Build()
        }

    interface IOlyRuntimeEmitter<SpirvType, SpirvFunction, SpirvField> with

        member this.EmitExternalType(externalPlatform: string, externalPath: string imarray, externalName: string, _enclosing: Choice<string imarray,SpirvType>, _kind: OlyILEntityKind, _flags: OlyIRTypeFlags, _name: string, _tyParCount: int): SpirvType = 
            if externalPlatform <> "spirv" then raise(InvalidOperationException())
            if externalPath.Length <> 1 then raise(InvalidOperationException())
            if (ImArray.head externalPath) <> "std" then raise(InvalidOperationException())

            match BuiltInTypes.TryGetByName(builder, externalName) with
            | ValueSome ty -> ty
            | _ ->
                if BuiltInFunctions.IsValid(externalName) then
                    SpirvType.Invalid
                else
                    raise(InvalidOperationException())

        member this.EmitFieldDefinition(enclosingTy: SpirvType, flags: OlyIRFieldFlags, name: string, ty: SpirvType, index: int32, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray, constValueOpt: OlyIRConstant<SpirvType,SpirvFunction> option): SpirvField = 
            if constValueOpt.IsSome then
                raise(NotImplementedException("field constants"))

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
                | SpirvType.Vec _ ->
                    {
                        Index = index
                        Name = name
                        Type = ty
                        Flags = SpirvFieldFlags.None
                        Attributes = ImArray.empty
                    }
                | _ ->
                    raise(NotImplementedException())

        member this.EmitFieldReference(enclosingTy: SpirvType, formalField: SpirvField): SpirvField = 
            raise(NotSupportedException())

        member this.EmitFunctionBody(body: Lazy<OlyIRFunctionBody<SpirvType,SpirvFunction,SpirvField>>, tier: OlyIRFunctionTier, func: SpirvFunction): unit = 
            let funcBuilder = (match func with SpirvFunction.Function(x) -> x | _ -> raise(InvalidOperationException()))
            let body = body.Value

            let loweringCenv = 
                { 
                    SpirvLowering.cenv.Module = builder
                    SpirvLowering.cenv.Function = funcBuilder
                    SpirvLowering.cenv.Locals = List(Array.zeroCreate body.LocalCount)
                    SpirvLowering.cenv.LocalTypes = List(Array.zeroCreate body.LocalCount)
                    SpirvLowering.cenv.LocalsIsMutable = body.LocalFlags |> ImArray.ofSeq |> ImArray.map (fun x -> x.HasFlag(OlyIRLocalFlags.Mutable))
                    SpirvLowering.cenv.ForwardSubLocalExpressions = Dictionary()
                }
            let codeGenCenv = 
                { 
                    IsDebuggable = tier.HasMinimalOptimizations
                    Instructions = List()
                    Module = builder
                    Function = funcBuilder
                    Locals = loweringCenv.Locals
                    LocalTypes = loweringCenv.LocalTypes
                    PredecessorBlockLabel = 0u
                }
            SpirvCodeGen.Gen codeGenCenv (SpirvLowering.Lower loweringCenv body.Expression)

            funcBuilder.Instructions <-
                [
                    let pars = funcBuilder.Parameters

                    OpFunction(funcBuilder.ReturnType.IdResult, funcBuilder.IdResult, FunctionControl.None, funcBuilder.Type.IdResult)

                    if not funcBuilder.IsEntryPoint then
                        for par in pars do
                            match par.Type with
                            | SpirvType.Pointer _ ->
                                OlyAssert.True(par.DecorateInstructions.IsEmpty)
                                OpFunctionParameter(par.Type.IdResult, par.IdResult)
                            | _ ->
                                raise(InvalidOperationException("Expected a pointer type."))                       

                    yield! codeGenCenv.Instructions

                    OpFunctionEnd
                ]

        member this.EmitFunctionDefinition(externalInfoOpt: OlyIRFunctionExternalInfo option, enclosingTy: SpirvType, flags: OlyIRFunctionFlags, name: string, tyPars: OlyIRTypeParameter<SpirvType> imarray, pars: OlyIRParameter<SpirvType, SpirvFunction> imarray, returnTy: SpirvType, overrides: SpirvFunction option, sigKey: OlyIRFunctionSignatureKey, attrs: OlyIRAttribute<SpirvType,SpirvFunction> imarray): SpirvFunction =
            if overrides.IsSome then raise(NotSupportedException())

            match externalInfoOpt with
            | Some(info) ->
                match info.Platform with
                | "" when info.Path.IsEmpty ->
                    if flags.IsStatic then
                        let isLazy =
                            attrs
                            |> ImArray.exists (fun irAttr ->
                                match irAttr with
                                | OlyIRAttribute(ctor=ctor) ->
                                    match ctor.TryGetBuiltIn() with
                                    | ValueSome builtInFunc ->
                                        match builtInFunc.Name with
                                        | "position"
                                        | "global_invocation_id" -> true
                                        | _ -> false
                                    | _ ->
                                        false
                            )
                        if pars.IsEmpty then
                            if returnTy = builder.GetTypeVoid() then
                                raise(InvalidOperationException())
                            let createVar() =
                                builder.CreateVariable((* global *) true, StorageClass.Input, attrs, returnTy)
                            if isLazy then
                                SpirvFunction.LazyVariable (lazy createVar())
                            else
                                SpirvFunction.Variable (createVar())
                        elif pars.Length = 1 && returnTy = builder.GetTypeVoid() then
                            let createVar() =
                                builder.CreateVariable((* global *) true, StorageClass.Output, attrs, pars[0].Type)
                            if isLazy then
                                SpirvFunction.LazyVariable (lazy createVar())
                            else
                                SpirvFunction.Variable (createVar())
                        else
                            raise(InvalidOperationException())                           
                    else
                        raise(InvalidOperationException())
                | _ ->
                    if info.Platform <> "spirv" then raise(InvalidOperationException())
                    let path = info.Path
                    let name = info.Name
                    let name =
                        if name.StartsWith("get_") then
                            name.Replace("get_", "")
                        elif name.StartsWith("set_") then
                            name.Replace("set_", "")
                        else
                            name
                    let name =
                        if flags.IsConstructor then
                            let name = path[1]
                            if name.StartsWith("vec") then
                                "vec"
                            else
                                name
                        else
                            name
                    match BuiltInFunctions.TryGetBuiltInFunction(path, name) with
                    | Some func -> func
                    | _ -> raise(InvalidOperationException())
            | _ ->
                let funcBuilder = builder.CreateFunctionBuilder(enclosingTy, name, flags, pars, returnTy)
                funcBuilder.AsFunction

        member this.EmitFunctionInstance(enclosingTy: SpirvType, formalFunc: SpirvFunction, tyArgs: SpirvType imarray): SpirvFunction = 
            formalFunc

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

        member this.EmitTypeFixedArray(elementTy: SpirvType, length: int, kind: OlyIRArrayKind): SpirvType = 
            if length <= 0 then
                raise(InvalidOperationException())
            raise(NotImplementedException())

        member this.EmitTypeBaseObject(): SpirvType = 
            SpirvType.Invalid // TODO: What should we do here? Ideally, we should not ever emit an 'object' type in the first place. Structs always inherits from 'object', but maybe we should not do that.

        member this.EmitTypeBool(): SpirvType = 
            builder.GetTypeBool()

        member this.EmitTypeByRef(elementTy: SpirvType, kind: OlyIRByRefKind): SpirvType = 
            match kind with
            | OlyIRByRefKind.ReadWrite ->
                SpirvType.OlyByRef(kind, elementTy)
            | OlyIRByRefKind.ReadOnly ->
                SpirvType.OlyByRef(kind, elementTy)
            | OlyIRByRefKind.WriteOnly ->
                SpirvType.OlyByRef(kind, elementTy)

        member this.EmitTypeChar16(): SpirvType = 
            raise(NotImplementedException())

        member this.EmitTypeConstantInt32(value: int32): SpirvType = 
            SpirvType.OlyConstantInt32 value

        member this.EmitTypeDefinition(enclosing: Choice<string imarray,SpirvType>, kind: OlyILEntityKind, flags: OlyIRTypeFlags, name: string, tyParCount: int): SpirvType =
            match kind with
            | OlyILEntityKind.Module ->
                SpirvType.Module(enclosing, name)
            | OlyILEntityKind.Struct ->
                let namedTy = builder.CreateTypeStructBuilder(enclosing, name)
                namedTy.AsType
            | OlyILEntityKind.TypeExtension ->
                SpirvType.Invalid
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
            match tyArgs[0], ty with
            | SpirvType.OlyConstantInt32 n, SpirvType.Vec(0u, 0u, SpirvType.Invalid) ->
                if not(n >= 2 && n <= 4) || not(tyArgs.Length = 2) then
                    invalidOp "Bad type arguments."
                builder.GetTypeVec(uint32(n), tyArgs[1])
            | _ ->
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
            SpirvType.Invalid

        member this.EmitTypeVoid(): SpirvType = 
            builder.GetTypeVoid()

        member this.Initialize(vm: IOlyVirtualMachine<SpirvType,SpirvFunction,SpirvField>): unit = 
            ()

