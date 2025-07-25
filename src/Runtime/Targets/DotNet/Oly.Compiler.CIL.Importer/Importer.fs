﻿namespace Oly.Targets.DotNet

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable

open Oly.Core
open Oly.Metadata
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Workspace

[<AutoOpen>]
module internal rec Helpers =

    let importCallingConvention (callConv: SignatureCallingConvention) =
        let ilCallConv =
            if callConv.HasFlag(SignatureCallingConvention.Unmanaged) then
                OlyILCallingConvention.Blittable
            else
                OlyILCallingConvention.Default

        if callConv.HasFlag(SignatureCallingConvention.CDecl) then
            ilCallConv ||| OlyILCallingConvention.CDecl
        elif callConv.HasFlag(SignatureCallingConvention.StdCall) then
            ilCallConv ||| OlyILCallingConvention.StdCall
        elif callConv.HasFlag(SignatureCallingConvention.ThisCall) then
            ilCallConv ||| OlyILCallingConvention.ThisCall
        elif callConv.HasFlag(SignatureCallingConvention.FastCall) then
            ilCallConv ||| OlyILCallingConvention.FastCall
        elif callConv.HasFlag(SignatureCallingConvention.VarArgs) then
            ilCallConv ||| OlyILCallingConvention.VarArgs
        else
            ilCallConv

    let mapDotNetTypeToOlyType namespac name tyParCount olyTy (olyTyArgs: _ imarray) =
        match namespac, name with
        | "System", "Action" when tyParCount = 0 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(ImArray.empty, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`1" when tyParCount = 1 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`2" when tyParCount = 2 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`3" when tyParCount = 3 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`4" when tyParCount = 4 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`5" when tyParCount = 5 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`6" when tyParCount = 6 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`7" when tyParCount = 7 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`8" when tyParCount = 8 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`9" when tyParCount = 9 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`10" when tyParCount = 10 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`11" when tyParCount = 11 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`12" when tyParCount = 12 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`13" when tyParCount = 13 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`14" when tyParCount = 14 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`15" when tyParCount = 15 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)
        | "System", "Action`16" when tyParCount = 16 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs, OlyILTypeVoid, OlyILFunctionKind.Normal)

        | "System", "Func`1" when tyParCount = 1 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(ImArray.empty, olyTyArgs[0], OlyILFunctionKind.Normal)
        | "System", "Func`2" when tyParCount = 2 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(1), olyTyArgs[1], OlyILFunctionKind.Normal)
        | "System", "Func`3" when tyParCount = 3 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(2), olyTyArgs[2], OlyILFunctionKind.Normal)
        | "System", "Func`4" when tyParCount = 4 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(3), olyTyArgs[3], OlyILFunctionKind.Normal)
        | "System", "Func`5" when tyParCount = 5 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(4), olyTyArgs[4], OlyILFunctionKind.Normal)
        | "System", "Func`6" when tyParCount = 6 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(5), olyTyArgs[5], OlyILFunctionKind.Normal)
        | "System", "Func`7" when tyParCount = 7 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(6), olyTyArgs[6], OlyILFunctionKind.Normal)
        | "System", "Func`8" when tyParCount = 8 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(7), olyTyArgs[7], OlyILFunctionKind.Normal)
        | "System", "Func`9" when tyParCount = 9 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(8), olyTyArgs[8], OlyILFunctionKind.Normal)
        | "System", "Func`10" when tyParCount = 10 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(9), olyTyArgs[9], OlyILFunctionKind.Normal)
        | "System", "Func`11" when tyParCount = 11 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(10), olyTyArgs[10], OlyILFunctionKind.Normal)
        | "System", "Func`12" when tyParCount = 12 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(11), olyTyArgs[11], OlyILFunctionKind.Normal)
        | "System", "Func`13" when tyParCount = 13 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(12), olyTyArgs[12], OlyILFunctionKind.Normal)
        | "System", "Func`14" when tyParCount = 14 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(13), olyTyArgs[13], OlyILFunctionKind.Normal)
        | "System", "Func`15" when tyParCount = 15 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(14), olyTyArgs[14], OlyILFunctionKind.Normal)
        | "System", "Func`16" when tyParCount = 16 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeFunction(olyTyArgs.RemoveAt(15), olyTyArgs[15], OlyILFunctionKind.Normal)

        // TODO: Figure out what the item names can be for tuples. C# allows to express the item names for a tuple.
        | "System", "ValueTuple`2" when tyParCount = 2 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeTuple(olyTyArgs, ImArray.empty)
        | "System", "ValueTuple`3" when tyParCount = 3 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeTuple(olyTyArgs, ImArray.empty)
        | "System", "ValueTuple`4" when tyParCount = 4 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeTuple(olyTyArgs, ImArray.empty)
        | "System", "ValueTuple`5" when tyParCount = 5 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeTuple(olyTyArgs, ImArray.empty)
        | "System", "ValueTuple`6" when tyParCount = 6 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeTuple(olyTyArgs, ImArray.empty)
        | "System", "ValueTuple`7" when tyParCount = 7 ->
            OlyAssert.Equal(tyParCount, olyTyArgs.Length)
            OlyILTypeTuple(olyTyArgs, ImArray.empty)

        | _ ->
            olyTy

    type internal OlyTypeName =

        static member Check(olyTy, olyAsm: OlyILAssembly, namespacePathWithDots: string, name: string) =
            let namespacePathWithDots2, name2 =
                match olyTy with
                | OlyILTypeEntity(OlyILEntityInstance(olyEntDefOrRefHandle, _)) when olyEntDefOrRefHandle.Kind = OlyILTableKind.EntityReference ->
                    let olyEntRef = olyAsm.GetEntityReference(olyEntDefOrRefHandle)
                    if olyEntRef.TypeParameterCount = 0 then
                        let olyEnclosing = olyEntRef.Enclosing
                        let olyName = olyEntRef.NameHandle
                        match olyEnclosing with
                        | OlyILEnclosing.Namespace(path, _) -> 
                            (   
                                path |> ImArray.map (fun x -> olyAsm.GetStringOrEmpty(x)) |> String.concat ".",
                                olyAsm.GetStringOrEmpty(olyName)
                            )
                        | _ ->
                            "", ""
                    else
                        "", ""
                | _ ->
                    "", ""

            namespacePathWithDots = namespacePathWithDots2 && name = name2

    let invalidType cenv msg =
        OlyILTypeInvalid(importRawString cenv msg)

    [<Sealed>]
    type internal OlySignatureTypeProvider (cenv: cenv) =

        let tryGetNamespaceAndName(olyEntDefOrRefHandle: OlyILEntityDefinitionOrReferenceHandle) =
            if olyEntDefOrRefHandle.Kind = OlyILTableKind.EntityReference then
                let olyEntRef = cenv.olyAsm.GetEntityReference(olyEntDefOrRefHandle)
                match olyEntRef.Enclosing with
                | OlyILEnclosing.Namespace(olyPath, _) ->
                    Some(olyPath, olyEntRef.NameHandle)
                | _ ->
                    None
            else
                let olyEntDef = cenv.olyAsm.GetEntityDefinition(olyEntDefOrRefHandle)
                match olyEntDef.Enclosing with
                | OlyILEnclosing.Namespace(olyPath, _) ->
                    Some(olyPath, olyEntDef.NameHandle)
                | _ ->
                    None
        
        interface ISignatureTypeProvider<OlyILType, int> with

            member this.GetArrayType(olyElementTy, shape) = 
                if shape.LowerBounds |> ImArray.forall (fun x -> x = 0) then
                    OlyILTypeArray(olyElementTy, shape.Rank, OlyILArrayKind.Mutable)
                else
                    invalidType cenv "Unsupported .NET type."

            member this.GetByReferenceType(olyTy) =
                OlyILTypeByRef(olyTy, OlyILByRefKind.ReadWrite)

            member this.GetGenericInstantiation(olyTy, olyTyArgs) =
                match olyTy with
                | OlyILTypeEntity(OlyILEntityInstance(olyTyHandle, _)) ->
                    OlyILTypeEntity(OlyILEntityInstance(olyTyHandle, olyTyArgs))
                | OlyILTypeRefCell _ ->
                    OlyILTypeRefCell(olyTyArgs.[0])
                | OlyILTypeArray(_, rank, kind) ->
                    OlyILTypeArray(olyTyArgs.[0], rank, kind)
                | OlyILTypeFixedArray(_, length, kind) ->
                    OlyILTypeFixedArray(olyTyArgs.[0], length, kind)
                | OlyILTypeNativePtr _ ->
                    OlyILTypeNativePtr(olyTyArgs.[0])
                | OlyILTypeByRef(_, kind) ->
                    OlyILTypeByRef(olyTyArgs.[0], kind)
                | OlyILTypeTuple(_, names) ->
                    OlyILTypeTuple(olyTyArgs, names)

                | OlyILTypeFunction(olyArgTys, olyReturnTy, olyKind) ->
                    let olyArgTys =
                        olyArgTys
                        |> ImArray.map (function
                            | OlyILTypeVariable(i, _) -> olyTyArgs[i]
                            | olyTy -> olyTy
                        )
                    let olyReturnTy = 
                        match olyReturnTy with
                        | OlyILTypeVariable(i, _) -> olyTyArgs[i]
                        | olyTy -> olyTy
                    OlyILTypeFunction(olyArgTys, olyReturnTy, olyKind)

                | _ ->
                    olyTy

            member this.GetGenericMethodParameter(offset, index) =
                OlyILTypeVariable(index, OlyILTypeVariableKind.Function)

            member this.GetGenericTypeParameter(offset, index) =
                OlyILTypeVariable(index, OlyILTypeVariableKind.Type)

            member this.GetModifiedType(olyModifier, olyUnmodifiedType, isRequired) =
                if isRequired then
                    match olyModifier with
                    | OlyILTypeEntity(olyEntInst) ->
                        match olyEntInst with
                        | OlyILEntityInstance(olyEntDefOrRefHandle, _) ->
                            match tryGetNamespaceAndName olyEntDefOrRefHandle with
                            | Some(olyPath, name) ->
                                let path1 = cenv.olyAsm.GetStringOrEmpty(olyPath[0])
                                let path2 = cenv.olyAsm.GetStringOrEmpty(olyPath[1])
                                let path3 = cenv.olyAsm.GetStringOrEmpty(olyPath[2])
                                let name = cenv.olyAsm.GetStringOrEmpty(name)
                                if (path1 = "System") && (path2 = "Runtime") && (path3 = "InteropServices") then
                                    match name with
                                    | "InAttribute" ->
                                        match olyUnmodifiedType with
                                        | OlyILTypeByRef(olyElementTy, _) ->
                                            OlyILTypeByRef(olyElementTy, OlyILByRefKind.ReadOnly)
                                        | _ ->
                                            invalidType cenv "Unsupported .NET type."

                                    | "UnmanagedType" ->
                                        match olyUnmodifiedType with
                                        | OlyILTypeEntity(olyEntInst) ->
                                            match olyEntInst with
                                            | OlyILEntityInstance(olyEntDefOrRefHandle, olyTyArgs) when olyTyArgs.IsEmpty && (olyEntDefOrRefHandle.Kind = OlyILTableKind.EntityDefinition) ->
                                                let olyEntRef = cenv.olyAsm.GetEntityDefinition(olyEntDefOrRefHandle)
                                                let name = cenv.olyAsm.GetStringOrEmpty(olyEntRef.NameHandle)
                                                // TODO: Check namespace...
                                                if name = "ValueType" then                                         
                                                    OlyILTypeModified(olyModifier, olyUnmodifiedType)
                                                else
                                                    invalidType cenv "Unsupported .NET type."

                                            | OlyILEntityInstance(olyEntDefOrRefHandle, olyTyArgs) when olyTyArgs.IsEmpty && (olyEntDefOrRefHandle.Kind = OlyILTableKind.EntityReference) ->
                                                let olyEntRef = cenv.olyAsm.GetEntityReference(olyEntDefOrRefHandle)
                                                let name = cenv.olyAsm.GetStringOrEmpty(olyEntRef.NameHandle)
                                                // TODO: Check namespace...
                                                if name = "ValueType" then                                         
                                                    OlyILTypeModified(olyModifier, olyUnmodifiedType)
                                                else
                                                    invalidType cenv "Unsupported .NET type."

                                            | _ ->
                                                invalidType cenv "Unsupported .NET type."
                                                
                                        | _ ->
                                            invalidType cenv "Unsupported .NET type."

                                    | _ ->
                                        invalidType cenv "Unsupported .NET type."
                                else
                                    invalidType cenv "Unsupported .NET type."
                            | _ ->
                                invalidType cenv "Unsupported .NET type."
                        | _ ->
                            invalidType cenv "Unsupported .NET type."
                    | _ ->
                        invalidType cenv "Unsupported .NET type."
                else
                    olyUnmodifiedType

            member this.GetPinnedType(olyElementTy) = 
                olyElementTy

            member this.GetPointerType(olyElementTy) =
                OlyILTypeNativePtr(olyElementTy)

            member this.GetPrimitiveType(typeCode) =
                match typeCode with
                | PrimitiveTypeCode.Boolean -> OlyILTypeBool
                | PrimitiveTypeCode.Byte -> OlyILTypeUInt8
                | PrimitiveTypeCode.SByte -> OlyILTypeInt8
                | PrimitiveTypeCode.UInt16 -> OlyILTypeUInt16
                | PrimitiveTypeCode.Int16 -> OlyILTypeInt16
                | PrimitiveTypeCode.UInt32 -> OlyILTypeUInt32
                | PrimitiveTypeCode.Int32 -> OlyILTypeInt32
                | PrimitiveTypeCode.UInt64 -> OlyILTypeUInt64
                | PrimitiveTypeCode.Int64 -> OlyILTypeInt64
                | PrimitiveTypeCode.Char -> OlyILTypeChar16
                | PrimitiveTypeCode.Single -> OlyILTypeFloat32
                | PrimitiveTypeCode.Double -> OlyILTypeFloat64
                | PrimitiveTypeCode.Void -> OlyILTypeVoid
                | PrimitiveTypeCode.TypedReference -> invalidType cenv "Unsupported .NET type."
                | PrimitiveTypeCode.String -> OlyILTypeUtf16
                | PrimitiveTypeCode.IntPtr -> OlyILTypeNativeInt
                | PrimitiveTypeCode.UIntPtr -> OlyILTypeNativeUInt
                | PrimitiveTypeCode.Object -> OlyILTypeBaseObject
                | _ -> failwith "Invalid primitive type code."

            member this.GetSZArrayType(olyElementTy) = 
                OlyILTypeArray(olyElementTy, 1, OlyILArrayKind.Mutable)

            member this.GetTypeFromDefinition(reader, handle, rawTypeKind) =
                let olyEntDefHandle, fullTyParCount = importTypeDefinitionAsOlyILEntityDefinition cenv handle
                let olyTyArgs = ImArray.init fullTyParCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))
                let olyTy = OlyILTypeEntity(OlyILEntityInstance(olyEntDefHandle, olyTyArgs))

                let tyDef = reader.GetTypeDefinition(handle)
                let name = reader.GetString(tyDef.Name)
                let namespac = reader.GetString(tyDef.Namespace)
                let tyParCount = tyDef.GetGenericParameters().Count

                mapDotNetTypeToOlyType namespac name tyParCount olyTy olyTyArgs

            member this.GetTypeFromReference(reader, handle, rawTypeKind) =
                let olyEntRefHandle = importTypeReferenceAsOlyILEntityReference cenv handle
                let olyEntRef = cenv.olyAsm.GetEntityReference(olyEntRefHandle)
                let olyTyArgs = ImArray.init olyEntRef.FullTypeParameterCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))
                let olyTy = OlyILTypeEntity(OlyILEntityInstance(olyEntRefHandle, olyTyArgs))

                let tyRef = reader.GetTypeReference(handle)
                let name = reader.GetString(tyRef.Name)
                let namespac = reader.GetString(tyRef.Namespace)
                let tyParCount = olyEntRef.FullTypeParameterCount

                mapDotNetTypeToOlyType namespac name tyParCount olyTy olyTyArgs

            member this.GetTypeFromSpecification(reader, genericContext, handle, rawTypeKind) =
                let olyTy = importTypeSpecificationAsOlyILType cenv handle
             
                let tySpec = reader.GetTypeSpecification(handle)
                let sigg = tySpec.DecodeSignature(OlySignatureTypeProvider(cenv), 0)

                match sigg with
                | OlyILTypeEntity(OlyILEntityInstance(olyEntDefOrRefHandle, olyTyArgs)) ->
                    let tyParCount, name, namespac =
                        if olyEntDefOrRefHandle.Kind = OlyILTableKind.EntityDefinition then
                            let olyEntDef = cenv.olyAsm.GetEntityDefinition(olyEntDefOrRefHandle)
                            let name = cenv.olyAsm.GetStringOrEmpty(olyEntDef.NameHandle)
                            let namespac = 
                                match olyEntDef.Enclosing with
                                | OlyILEnclosing.Namespace(namespaceParts, _) ->
                                    namespaceParts
                                    |> ImArray.map (fun x -> cenv.olyAsm.GetStringOrEmpty(x))
                                    |> String.concat "."
                                | _ ->
                                    ""
                            let tyParCount = olyEntDef.FullTypeParameterCount
                            tyParCount, name, namespac
                        else
                            let olyEntRef = cenv.olyAsm.GetEntityReference(olyEntDefOrRefHandle)
                            0, "", ""

                    match namespac, name, tyParCount with
                    | "System", "Action", 0 ->
                        OlyILTypeFunction(ImArray.empty, OlyILTypeVoid, OlyILFunctionKind.Normal)
                    | "System", "Func", 1 ->
                        OlyILTypeFunction(ImArray.empty, olyTyArgs[0], OlyILFunctionKind.Normal)
                    | _ ->
                        olyTy

                | _ ->
                    olyTy

            member _.GetFunctionPointerType(methSig) =
                let olyArgTys = methSig.ParameterTypes

                let olyReturnTy =
                    methSig.ReturnType

                let ilCallConv = importCallingConvention methSig.Header.CallingConvention
                OlyILTypeNativeFunctionPtr(ilCallConv, olyArgTys, olyReturnTy)

    type cenv =
        {
            olyAsm: OlyILAssembly
            reader: MetadataReader
            olyDefaultCtorConstr: OlyILConstraint ref
            stringCache: Dictionary<string, OlyILStringHandle>
            stringToOlyStringCache: Dictionary<StringHandle, OlyILStringHandle>
            tyDefToOlyEntRefCache: Dictionary<TypeDefinitionHandle, OlyILEntityReferenceHandle>
            tyDefToOlyEntDefCache: Dictionary<TypeDefinitionHandle, OlyILEntityDefinitionHandle * int>
            tyRefToOlyEntRefCache: Dictionary<TypeReferenceHandle, OlyILEntityReferenceHandle>
            tySpecToOlyTypeCache: Dictionary<TypeSpecificationHandle, OlyILType>
            exportedTyToOlyEntRefCache: Dictionary<ExportedTypeHandle, OlyILEntityReferenceHandle>
            methDefToOlyFuncDefCache: Dictionary<MethodDefinitionHandle, OlyILEntityDefinitionHandle>
        }

    let unmangleName (name: string) =
        match name.IndexOf("`") with
        | -1 -> name
        | index -> name.Substring(0, index)

    let importString (cenv: cenv) (strHandle: StringHandle) =
        match cenv.stringToOlyStringCache.TryGetValue strHandle with
        | true, olyStrHandle -> olyStrHandle
        | _ ->
            let reader = cenv.reader

            let str = reader.GetString(strHandle)
            match cenv.stringCache.TryGetValue str with
            | true, olyStrHandle ->
                cenv.stringToOlyStringCache[strHandle] <- olyStrHandle
                olyStrHandle
            | _ ->
                let olyStrHandle = cenv.olyAsm.AddStringOrNilString(str)
                cenv.stringToOlyStringCache[strHandle] <- olyStrHandle
                cenv.stringCache[str] <- olyStrHandle
                olyStrHandle

    let importRawString (cenv: cenv) (str: string) =
        match cenv.stringCache.TryGetValue str with
        | true, olyStrHandle ->
            olyStrHandle
        | _ ->
            let olyStrHandle = cenv.olyAsm.AddStringOrNilString(str)
            cenv.stringCache[str] <- olyStrHandle
            olyStrHandle

    let importTypeSpecificationAsOlyILType (cenv: cenv) (tySpecHandle: TypeSpecificationHandle) =
        match cenv.tySpecToOlyTypeCache.TryGetValue tySpecHandle with
        | true, olyTy -> olyTy
        | _ ->
            let reader = cenv.reader

            let tySpec = reader.GetTypeSpecification(tySpecHandle)
            let olyTy = tySpec.DecodeSignature(OlySignatureTypeProvider(cenv), 0)
            cenv.tySpecToOlyTypeCache.[tySpecHandle] <- olyTy
            olyTy

    let importAsOlyILType (cenv: cenv) (entHandle: EntityHandle) =
        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        match entHandle.Kind with
        | HandleKind.TypeDefinition ->
            let olyEntDefHandle, fullTyParCount = importTypeDefinitionAsOlyILEntityDefinition cenv (TypeDefinitionHandle.op_Explicit entHandle)
            OlyILTypeEntity(OlyILEntityInstance(olyEntDefHandle, ImArray.init fullTyParCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))))
        | HandleKind.TypeReference ->
            let olyEntRefHandle = importTypeReferenceAsOlyILEntityReference cenv (TypeReferenceHandle.op_Explicit entHandle)
            let olyEntRef = olyAsm.GetEntityReference(olyEntRefHandle)
            OlyILTypeEntity(OlyILEntityInstance(olyEntRefHandle, ImArray.init olyEntRef.FullTypeParameterCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))))
        | HandleKind.TypeSpecification ->
            importTypeSpecificationAsOlyILType cenv (TypeSpecificationHandle.op_Explicit entHandle)
        | _ ->
            failwith "Invalid handle kind."

    let importConstraintAsOlyILConstraints (cenv: cenv) (constrHandle: GenericParameterConstraintHandle) : OlyILConstraint imarray =
        let reader = cenv.reader

        let constr = reader.GetGenericParameterConstraint(constrHandle)
        let olyTy = importAsOlyILType cenv constr.Type

        match olyTy with
        | OlyILTypeModified(olyModifierTy, olyTy) ->
            if OlyTypeName.Check(olyModifierTy, cenv.olyAsm, "System.Runtime.InteropServices", "UnmanagedType") &&
                OlyTypeName.Check(olyTy, cenv.olyAsm, "System", "ValueType") then
                (OlyILConstraint.SubtypeOf(olyTy), OlyILConstraint.Struct, OlyILConstraint.Unmanaged)
                |||> ImArray.createThree
                
            else
                OlyILConstraint.SubtypeOf(olyTy)
                |> ImArray.createOne
        | _ ->
            if OlyTypeName.Check(olyTy, cenv.olyAsm, "System", "ValueType") then
                (OlyILConstraint.SubtypeOf(olyTy), OlyILConstraint.Struct)
                ||> ImArray.createTwo
            else
                OlyILConstraint.SubtypeOf(olyTy)
                |> ImArray.createOne

    let importGenericParametersAsOlyILTypeParameters (cenv: cenv) (genericParHandles: GenericParameterHandleCollection) =
        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        seq {
            for i = 0 to genericParHandles.Count - 1 do
                let genericPar = reader.GetGenericParameter(genericParHandles.[i])

                let olyConstrs =
                    genericPar.GetConstraints().ToImmutableArray()
                    |> ImArray.map (fun constrHandle ->
                        importConstraintAsOlyILConstraints cenv constrHandle
                    )
                    |> ImArray.concat

                let olyConstrs =
                    if genericPar.Attributes.HasFlag(GenericParameterAttributes.ReferenceTypeConstraint) then
                        olyConstrs.Add(OlyILConstraint.NotStruct)
                    else
                        olyConstrs

                let olyConstrs =
                    if genericPar.Attributes.HasFlag(GenericParameterAttributes.DefaultConstructorConstraint) then
                        olyConstrs.Add(cenv.olyDefaultCtorConstr.contents)
                    else
                        olyConstrs

                let olyTyPar =
                    OlyILTypeParameter(
                        importString cenv genericPar.Name,
                        0, // Will always be zero as .NET does not support second-order generics.
                        false,
                        olyConstrs
                    )
                yield olyTyPar
        }
        |> ImArray.ofSeq

    let importMethodDefinitionAsOlyILFunctionSpecification (cenv: cenv) (name: string) tyParOffset (methDefHandle: MethodDefinitionHandle) =
        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        let meth = reader.GetMethodDefinition(methDefHandle)

        let olyTyPars = importGenericParametersAsOlyILTypeParameters cenv (meth.GetGenericParameters())

        let pars = meth.GetParameters().ToImmutableArray() |> ImArray.map (fun x -> reader.GetParameter(x))

        let sigg = meth.DecodeSignature(OlySignatureTypeProvider(cenv), tyParOffset)
        let olyPars =
            seq {
                for i = 0 to sigg.ParameterTypes.Length - 1 do
                    let parTy = sigg.ParameterTypes.[i]

                    let olyAttrs, olyNameHandle = 
                        match pars |> ImArray.tryFind (fun x -> x.SequenceNumber = i + 1) with
                        | Some par ->
                            let olyAttrs =
                                par.GetCustomAttributes().ToImmutableArray()
                                |> ImArray.choose (tryImportAttributeAsOlyILAttribute cenv)

                            if par.Name.IsNil then olyAttrs, OlyILTableIndex(OlyILTableKind.String, -1)
                            else olyAttrs, importString cenv par.Name
                        | _ ->
                            ImArray.empty, OlyILTableIndex(OlyILTableKind.String, -1)

                    yield OlyILParameter(olyAttrs, olyNameHandle, parTy, false)
            }
            |> ImArray.ofSeq

        let res =
            OlyILFunctionSpecification(
                (meth.Attributes &&& MethodAttributes.Static <> MethodAttributes.Static),
                importCallingConvention sigg.Header.CallingConvention,
                importRawString cenv name,
                olyTyPars,
                olyPars,
                sigg.ReturnType
            )
            |> olyAsm.AddFunctionSpecification
        res

    let getEnclosingInfo (cenv: cenv) (path: string imarray) (entHandle: EntityHandle) =
        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        match entHandle.Kind with
        | HandleKind.AssemblyReference ->
            let asmRef = reader.GetAssemblyReference(AssemblyReferenceHandle.op_Explicit(entHandle))
            let asmName = asmRef.Name |> reader.GetString
            let olyAsmIdentity = OlyILAssemblyIdentity(asmName, "dotnet")
            OlyILEnclosing.Namespace(path |> ImArray.map (importRawString cenv), olyAsmIdentity)
        | _ ->
            if path.IsEmpty then
                match entHandle.Kind with
                | HandleKind.TypeReference ->
                    let olyEnclosingEntRefHandle = importTypeReferenceAsOlyILEntityReference cenv (TypeReferenceHandle.op_Explicit entHandle)
                    let olyEnclosingEntRef = olyAsm.GetEntityReference(olyEnclosingEntRefHandle)
                    let olyTyArgs = ImArray.init olyEnclosingEntRef.FullTypeParameterCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))
                    OlyILEnclosing.Entity(OlyILEntityInstance(olyEnclosingEntRefHandle, olyTyArgs))
                | HandleKind.ExportedType ->
                    let olyEnclosingEntRefHandle = importExportedTypeAsOlyILEntityReference cenv (ExportedTypeHandle.op_Explicit entHandle)
                    let olyEnclosingEntRef = olyAsm.GetEntityReference(olyEnclosingEntRefHandle)
                    let olyTyArgs = ImArray.init olyEnclosingEntRef.FullTypeParameterCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))
                    OlyILEnclosing.Entity(OlyILEntityInstance(olyEnclosingEntRefHandle, olyTyArgs))
                | _ ->
                    failwithf "Handle kind '%A' not handled." entHandle.Kind
            else
                failwith "Invalid type reference."

    let importExportedTypeAsOlyILEntityReference (cenv: cenv) (exportedTyHandle: ExportedTypeHandle) =
        match cenv.exportedTyToOlyEntRefCache.TryGetValue exportedTyHandle with
        | true, x -> x
        | _ ->
            let olyAsm = cenv.olyAsm
            let reader = cenv.reader

            let exportedTy = reader.GetExportedType(exportedTyHandle)
            
            let mangledName = reader.GetString(exportedTy.Name)
            let name = unmangleName(mangledName)

            let path = 
                let path = reader.GetString(exportedTy.Namespace)
                if String.IsNullOrWhiteSpace path then
                    ImArray.empty
                else
                    reader.GetString(exportedTy.Namespace).Split(".")
                    |> ImArray.ofSeq

            let olyEnclosing = getEnclosingInfo cenv path exportedTy.Implementation

            let tyParCount =
                // We assume the mangledName contains the type parameter count.
                // Technically this may not be true but it is almost always the case.
                let index = mangledName.IndexOf("`")
                if index = -1 then
                    0
                else
                    mangledName.Substring(index + 1)
                    |> Int32.Parse
            
            let olyEntRefHandle =
                OlyILEntityReference(
                    olyEnclosing,
                    importRawString cenv name,
                    tyParCount
                )
                |> olyAsm.AddEntityReference
            cenv.exportedTyToOlyEntRefCache.[exportedTyHandle] <- olyEntRefHandle
            olyEntRefHandle

    let importTypeReferenceAsOlyILEntityReference (cenv: cenv) (tyRefHandle: TypeReferenceHandle) =
        match cenv.tyRefToOlyEntRefCache.TryGetValue tyRefHandle with
        | true, x -> x
        | _ ->
            let olyAsm = cenv.olyAsm
            let reader = cenv.reader

            let tyRef = reader.GetTypeReference(tyRefHandle)
            
            let mangledName = reader.GetString(tyRef.Name)
            let name = unmangleName(mangledName)

            let path = 
                let path = reader.GetString(tyRef.Namespace)
                if String.IsNullOrWhiteSpace path then
                    ImArray.empty
                else
                    reader.GetString(tyRef.Namespace).Split(".")
                    |> ImArray.ofSeq

            let olyEnclosing = getEnclosingInfo cenv path tyRef.ResolutionScope

            let tyParCount =
                // We assume the mangledName contains the type parameter count.
                // Technically this may not be true but it is almost always the case.
                let index = mangledName.IndexOf("`")
                if index = -1 then
                    0
                else
                    mangledName.Substring(index + 1)
                    |> Int32.Parse
            
            let olyEntRefHandle =
                OlyILEntityReference(
                    olyEnclosing,
                    importRawString cenv name,
                    tyParCount
                )
                |> olyAsm.AddEntityReference
            cenv.tyRefToOlyEntRefCache.[tyRefHandle] <- olyEntRefHandle
            olyEntRefHandle

    let private getFullTypeParameterCount (cenv: cenv) (tyDefHandle: TypeDefinitionHandle) =
        cenv.reader.GetTypeDefinition(tyDefHandle).GetGenericParameters().Count

    let hasReadOnlyAttribute cenv (handles: CustomAttributeHandleCollection) =
        let reader = cenv.reader

        handles.ToImmutableArray()
        |> ImArray.exists (fun x ->
            let attr = reader.GetCustomAttribute(x)
            match attr.Constructor.Kind with
            | HandleKind.MethodDefinition ->
                let methDef = reader.GetMethodDefinition(MethodDefinitionHandle.op_Explicit attr.Constructor)
                let name = reader.GetString(methDef.Name)
                if name = ".ctor" then
                    let tyDef = reader.GetTypeDefinition(methDef.GetDeclaringType())
                    let name = reader.GetString(tyDef.Name)
                    let namesp =
                        if tyDef.Namespace.IsNil then
                            String.Empty
                        else
                            reader.GetString(tyDef.Namespace)

                    namesp = "System.Runtime.CompilerServices" && name = "IsReadOnlyAttribute"
                else
                    false

            | HandleKind.MemberReference ->
                let memRef = reader.GetMemberReference(MemberReferenceHandle.op_Explicit attr.Constructor)
                let name = reader.GetString(memRef.Name)
                if name = ".ctor" then
                    match memRef.Parent.Kind with
                    | HandleKind.TypeReference ->
                        let tyRef = reader.GetTypeReference(TypeReferenceHandle.op_Explicit memRef.Parent)
                        let name = reader.GetString(tyRef.Name)
                        let namesp =
                            if tyRef.Namespace.IsNil then
                                String.Empty
                            else
                                reader.GetString(tyRef.Namespace)
                            
                        namesp = "System.Runtime.CompilerServices" && name = "IsReadOnlyAttribute"
                    | _ ->
                        false
                else
                    false

            | _ ->
                false
        )

    let hasIsByRefLikeAttribute cenv (handles: CustomAttributeHandleCollection) =
        let reader = cenv.reader

        handles.ToImmutableArray()
        |> ImArray.exists (fun x ->
            let attr = reader.GetCustomAttribute(x)
            match attr.Constructor.Kind with
            | HandleKind.MethodDefinition ->
                let methDef = reader.GetMethodDefinition(MethodDefinitionHandle.op_Explicit attr.Constructor)
                let name = reader.GetString(methDef.Name)
                if name = ".ctor" then
                    let tyDef = reader.GetTypeDefinition(methDef.GetDeclaringType())
                    let name = reader.GetString(tyDef.Name)
                    let namesp =
                        if tyDef.Namespace.IsNil then
                            String.Empty
                        else
                            reader.GetString(tyDef.Namespace)

                    namesp = "System.Runtime.CompilerServices" && name = "IsByRefLikeAttribute"
                else
                    false

            | HandleKind.MemberReference ->
                let memRef = reader.GetMemberReference(MemberReferenceHandle.op_Explicit attr.Constructor)
                let name = reader.GetString(memRef.Name)
                if name = ".ctor" then
                    match memRef.Parent.Kind with
                    | HandleKind.TypeReference ->
                        let tyRef = reader.GetTypeReference(TypeReferenceHandle.op_Explicit memRef.Parent)
                        let name = reader.GetString(tyRef.Name)
                        let namesp =
                            if tyRef.Namespace.IsNil then
                                String.Empty
                            else
                                reader.GetString(tyRef.Namespace)
                            
                        namesp = "System.Runtime.CompilerServices" && name = "IsByRefLikeAttribute"
                    | _ ->
                        false
                else
                    false

            | _ ->
                false
        )

    let tryImportFieldDefinitionAsOlyILFieldDefinition (cenv: cenv) (fieldDefHandle: FieldDefinitionHandle) =
        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        let fieldDef = reader.GetFieldDefinition(fieldDefHandle)

        if fieldDef.Attributes &&& FieldAttributes.FieldAccessMask <> FieldAttributes.Public then None
        else
        
        if fieldDef.Name.IsNil then None
        else

        let name = reader.GetString(fieldDef.Name)

        if String.IsNullOrWhiteSpace name then None
        else

        let nameHandle = importRawString cenv name

        let olyMemberFlags =
            if fieldDef.Attributes &&& FieldAttributes.Static = FieldAttributes.Static then
                OlyILMemberFlags.Static
            else
                OlyILMemberFlags.None

        let olyMemberFlags =
            // TODO: Handle more accessors.
            if fieldDef.Attributes &&& FieldAttributes.FieldAccessMask = FieldAttributes.Public then
                olyMemberFlags ||| OlyILMemberFlags.Public
            else
                olyMemberFlags

        let olyTy = fieldDef.DecodeSignature(OlySignatureTypeProvider(cenv), 0)

        let isLiteral = fieldDef.Attributes &&& FieldAttributes.Literal = FieldAttributes.Literal
        if isLiteral then
            let constantHandle = fieldDef.GetDefaultValue()
            let constant = reader.GetConstant(constantHandle)

            match constant.TypeCode with
            | ConstantTypeCode.Byte ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.UInt8(blob.ReadByte())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.SByte ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.Int8(blob.ReadSByte())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.Int16 ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.Int16(blob.ReadInt16())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.UInt16 ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.UInt16(blob.ReadUInt16())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.Int32 ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.Int32(blob.ReadInt32())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.UInt32 ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.UInt32(blob.ReadUInt32())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.Int64 ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.Int64(blob.ReadInt64())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.UInt64 ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.UInt64(blob.ReadUInt64())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.Single ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.Float32(blob.ReadSingle())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | ConstantTypeCode.Double ->
                let mutable blob = reader.GetBlobReader(constant.Value)
                let c = OlyILConstant.Float64(blob.ReadDouble())
                let olyConstant = OlyILFieldConstant(nameHandle, olyTy, c, olyMemberFlags)
                olyAsm.AddFieldDefinition(olyConstant)
                |> Some
            | _ ->
                None
        else

        let olyAttrs =
            seq {
                OlyILAttribute.Import(importRawString cenv "CLR", ImArray.empty, importRawString cenv name)
            }
            |> ImArray.ofSeq

        let olyFieldFlags =
            if (fieldDef.Attributes &&& FieldAttributes.InitOnly = FieldAttributes.InitOnly) || isLiteral then
                OlyILFieldFlags.None
            else
                OlyILFieldFlags.Mutable

        let olyFieldDef =
            OlyILFieldDefinition(
                olyAttrs,
                nameHandle,
                olyTy,
                olyFieldFlags,
                olyMemberFlags
            )

        olyAsm.AddFieldDefinition(olyFieldDef)
        |> Some

    let importMemberReferenceAsOlyILFunctionReference (cenv: cenv) (memRefHandle: MemberReferenceHandle) : OlyILFunctionReference =
        // TODO: Caching
        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        let memRef = reader.GetMemberReference(memRefHandle)
        let olyEnclosingTy = importAsOlyILType cenv memRef.Parent   
        let tyParOffset = olyEnclosingTy.TypeArguments.Length
        let si = memRef.DecodeMethodSignature(OlySignatureTypeProvider(cenv), tyParOffset)

        let name = reader.GetString(memRef.Name)

        let isInstance = false
        let tyPars = 
            ImArray.init si.GenericParameterCount (fun _ ->
                OlyILTypeParameter(OlyILTableIndex.CreateString(-1), 0, false, ImArray.empty)
            )
        let pars = 
            si.ParameterTypes
            |> ImArray.map (fun olyTy ->
                OlyILParameter(ImArray.empty, OlyILTableIndex.CreateString(-1), olyTy, false)
            )
        let returnTy = si.ReturnType

        let olyFuncSpec =
            OlyILFunctionSpecification(
                isInstance,
                importCallingConvention si.Header.CallingConvention,
                importRawString cenv (unmangleName name),
                tyPars,
                pars,
                returnTy
            )
        let olyFuncSpecHandle = olyAsm.AddFunctionSpecification(olyFuncSpec)

        let olyEnclosing =
            match olyEnclosingTy with
            | OlyILTypeEntity(ilEntInst) ->
                OlyILEnclosing.Entity(ilEntInst)
            | _ ->
                failwith "Expected entity."

        OlyILFunctionReference(olyEnclosing, olyFuncSpecHandle)

    let importMethodDefinitionAsOlyILFunctionDefinition (cenv: cenv) olyEntDefHandle tyParOffset (methOverrides: ImmutableDictionary<EntityHandle, OlyILFunctionReference>) (methDefHandle: MethodDefinitionHandle) : OlyILFunctionDefinitionHandle voption =
        match cenv.methDefToOlyFuncDefCache.TryGetValue methDefHandle with
        | true, res -> ValueSome res
        | _ ->

        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        let meth = reader.GetMethodDefinition(methDefHandle)

        let isStatic = meth.Attributes &&& MethodAttributes.Static = MethodAttributes.Static
        let isVirtual = meth.Attributes &&& MethodAttributes.Virtual = MethodAttributes.Virtual
        let isAbstract = meth.Attributes &&& MethodAttributes.Abstract = MethodAttributes.Abstract
        let isNewSlot = meth.Attributes &&& MethodAttributes.NewSlot = MethodAttributes.NewSlot
        let isFinal = meth.Attributes &&& MethodAttributes.Final = MethodAttributes.Final

        if (meth.Attributes &&& MethodAttributes.MemberAccessMask = MethodAttributes.Private) && not isVirtual then ValueNone
        else

        let name = if meth.Name.IsNil then "" else reader.GetString(meth.Name)

        if name = ".cctor" then ValueNone
        else

        let olyFuncSpecHandle = 
            importMethodDefinitionAsOlyILFunctionSpecification cenv name tyParOffset methDefHandle

        let olyFuncFlags = 
            if name = ".ctor" then
                OlyILFunctionFlags.Constructor
            else
                OlyILFunctionFlags.None

        let olyFuncFlags =
            if hasReadOnlyAttribute cenv (meth.GetCustomAttributes()) then
                olyFuncFlags
            else
                olyFuncFlags ||| OlyILFunctionFlags.Mutable

        let olyMemberFlags =
            if (meth.Attributes &&& MethodAttributes.MemberAccessMask = MethodAttributes.Public) then
                OlyILMemberFlags.Public
            elif (meth.Attributes &&& MethodAttributes.MemberAccessMask = MethodAttributes.Family) then
                OlyILMemberFlags.Protected
            elif (meth.Attributes &&& MethodAttributes.MemberAccessMask = MethodAttributes.FamORAssem) then
                OlyILMemberFlags.Protected
            elif (meth.Attributes &&& MethodAttributes.MemberAccessMask = MethodAttributes.Assembly) then
                OlyILMemberFlags.Internal
            else
                OlyILMemberFlags.Private

        let olyMemberFlags = 
            if isStatic then
                olyMemberFlags ||| OlyILMemberFlags.Static
            else
                olyMemberFlags

        let olyMemberFlags =
            if isVirtual then
                olyMemberFlags ||| OlyILMemberFlags.Virtual
            else
                olyMemberFlags

        let olyMemberFlags =
            if isAbstract then
                olyMemberFlags ||| OlyILMemberFlags.Abstract
            else
                olyMemberFlags

        let olyMemberFlags =
            if isNewSlot then
                olyMemberFlags ||| OlyILMemberFlags.NewSlot
            else
                olyMemberFlags

        let olyMemberFlags =
            if isFinal then
                olyMemberFlags ||| OlyILMemberFlags.Final
            else
                olyMemberFlags

        let olyAttrs =
            seq {
                OlyILAttribute.Import(importRawString cenv "CLR", ImArray.empty, importRawString cenv name)
            }
            |> ImArray.ofSeq

        let olyOverrides =
            match methOverrides.TryGetValue(MethodDefinitionHandle.op_Implicit(methDefHandle)) with
            | true, olyFuncRef ->
                Some(olyFuncRef)
            | _ ->
                None

        let olyFuncDef =
            OlyILFunctionDefinition(
                olyFuncFlags,
                olyMemberFlags,
                olyAttrs,
                olyFuncSpecHandle,
                olyOverrides,
                ref None
            )

        let res = olyAsm.AddFunctionDefinition(olyEntDefHandle, olyFuncDef)
        cenv.methDefToOlyFuncDefCache.[methDefHandle] <- res
        ValueSome res

    let tryImportAttributeAsOlyILAttribute (cenv: cenv) (attrHandle: CustomAttributeHandle) =
        // TODO: Implement.
        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        let attr = reader.GetCustomAttribute(attrHandle)

        None

    let importTypeDefinitionAsOlyILEntityDefinition (cenv: cenv) (tyDefHandle: TypeDefinitionHandle) : OlyILEntityDefinitionHandle * int =
        match cenv.tyDefToOlyEntDefCache.TryGetValue tyDefHandle with
        | true, result -> result
        | _ ->

        let olyAsm = cenv.olyAsm
        let reader = cenv.reader

        let olyEntDefHandle = olyAsm.NextEntityDefinitionHandle()
        let fullTyParCount = getFullTypeParameterCount cenv tyDefHandle
        cenv.tyDefToOlyEntDefCache.[tyDefHandle] <- (olyEntDefHandle, fullTyParCount)
     
        let tyDef = reader.GetTypeDefinition(tyDefHandle)
        let name = reader.GetString(tyDef.Name)
        let unmangledName = unmangleName(name)

        let path, olyTyPars, olyEnclosing = 
            if tyDef.IsNested then
                let enclosingTyDefHandle = tyDef.GetDeclaringType()
                let olyEnclosingEntDefHandle, enclosingTyParCount = importTypeDefinitionAsOlyILEntityDefinition cenv enclosingTyDefHandle
                let olyEnclosingTyArgs = ImArray.init enclosingTyParCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))

                let olyTyPars = 
                    importGenericParametersAsOlyILTypeParameters cenv (tyDef.GetGenericParameters())
                    |> Seq.skip enclosingTyParCount
                    |> ImArray.ofSeq

                ImArray.empty, olyTyPars, OlyILEnclosing.Entity(OlyILEntityInstance(olyEnclosingEntDefHandle, olyEnclosingTyArgs))
            else
                let path =
                    if tyDef.Namespace.IsNil then
                        ImArray.empty
                    else
                        reader.GetString(tyDef.Namespace).Split(".")
                        |> ImArray.ofSeq
                let olyTyPars = importGenericParametersAsOlyILTypeParameters cenv (tyDef.GetGenericParameters())
                path, olyTyPars, OlyILEnclosing.Namespace(path |> ImArray.map (importRawString cenv), olyAsm.Identity)

        let isSealed = tyDef.Attributes &&& TypeAttributes.Sealed = TypeAttributes.Sealed

        let isKind kindName =
            if tyDef.BaseType.IsNil then
                false
            else
                match tyDef.BaseType.Kind with
                | HandleKind.TypeDefinition ->
                    let tyDef = reader.GetTypeDefinition(TypeDefinitionHandle.op_Explicit tyDef.BaseType)
                    let name = if tyDef.Name.IsNil then "" else reader.GetString(tyDef.Name)
                    let namespacePath = if tyDef.Namespace.IsNil then "" else reader.GetString(tyDef.Namespace)
                    (name = kindName) && namespacePath = "System"
                | HandleKind.TypeReference ->
                    let tyRef = reader.GetTypeReference(TypeReferenceHandle.op_Explicit tyDef.BaseType)
                    let name = if tyRef.Name.IsNil then "" else reader.GetString(tyRef.Name)
                    let namespacePath = if tyRef.Namespace.IsNil then "" else reader.GetString(tyRef.Namespace)
                    (name = kindName) && namespacePath = "System"
                | _ ->
                    false

        let isStruct = isKind "ValueType"

        let isEnum = isKind "Enum"

        let isInterface = tyDef.Attributes &&& TypeAttributes.Interface = TypeAttributes.Interface
        let isAbstract = tyDef.Attributes &&& TypeAttributes.Abstract = TypeAttributes.Abstract

        let olyIntrinsicAttrOpt =
            if path.Length = 1 && path.[0] = "System" then
                match unmangledName with
                | "Object" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "base_object"), OlyILTypeBaseObject)
                    |> ValueSome
                | "Byte" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "uint8"), OlyILTypeUInt8)
                    |> ValueSome
                | "SByte" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "int8"), OlyILTypeInt8)
                    |> ValueSome
                | "UInt16" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "uint16"), OlyILTypeUInt16)
                    |> ValueSome
                | "Int16" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "int16"), OlyILTypeInt16)
                    |> ValueSome
                | "UInt32" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "uint32"), OlyILTypeUInt32)
                    |> ValueSome
                | "Int32" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "int32"), OlyILTypeInt32)
                    |> ValueSome
                | "UInt64" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "uint64"), OlyILTypeUInt64)
                    |> ValueSome
                | "Int64" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "int64"), OlyILTypeInt64)
                    |> ValueSome
                | "Single" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "float32"), OlyILTypeFloat32)
                    |> ValueSome
                | "Double" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "float64"), OlyILTypeFloat64)
                    |> ValueSome
                | "Char" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "char16"), OlyILTypeChar16)
                    |> ValueSome
                | "Boolean" -> 
                    (OlyILAttribute.Intrinsic(importRawString cenv "bool"), OlyILTypeBool)
                    |> ValueSome
                | "String" ->
                    (OlyILAttribute.Intrinsic(importRawString cenv "utf16"), OlyILTypeUtf16)
                    |> ValueSome
                | "IntPtr" ->
                    (OlyILAttribute.Intrinsic(importRawString cenv "native_int"), OlyILTypeNativeInt)
                    |> ValueSome
                | "UIntPtr" ->
                    (OlyILAttribute.Intrinsic(importRawString cenv "native_uint"), OlyILTypeNativeUInt)
                    |> ValueSome
                | "Void" ->
                    (OlyILAttribute.Intrinsic(importRawString cenv "void"), OlyILTypeVoid)
                    |> ValueSome
                | _ ->
                    ValueNone
            else
                ValueNone

        let olyEntKind =
            if isStruct then
                if path.Length = 1 && path[0] = "System" && name = "Enum" then
                    OlyILEntityKind.Class
                else
                    OlyILEntityKind.Struct
            elif isInterface then
                OlyILEntityKind.Interface
            elif isEnum then
                if path.Length = 1 && path[0] = "System" && name = "Enum" then
                    OlyILEntityKind.Class
                else
                    OlyILEntityKind.Enum
            else
                OlyILEntityKind.Class // TODO:

        let olyEntFlags = OlyILEntityFlags.None

        let olyEntFlags =
            if isSealed then
                olyEntFlags ||| OlyILEntityFlags.Final
            else
                olyEntFlags

        let olyEntFlags =
            if isAbstract then
                olyEntFlags ||| OlyILEntityFlags.Abstract
            else
                olyEntFlags

        let olyEntFlags =
            if not isEnum && not isStruct then
                // By default, all reference types imported are assumed to be nullable.
                olyEntFlags ||| OlyILEntityFlags.Nullable
            else
                olyEntFlags

        let olyEntFlags =
            if (olyEntKind = OlyILEntityKind.Struct) && hasIsByRefLikeAttribute cenv (tyDef.GetCustomAttributes()) then
                olyEntFlags ||| OlyILEntityFlags.Scoped
            else
                olyEntFlags

        let asmName = reader.GetAssemblyDefinition().GetAssemblyName()

        let olyAttrs =
            seq {
                yield OlyILAttribute.Import(importRawString cenv ("CLR:" + asmName.FullName), path |> ImArray.map (importRawString cenv), importRawString cenv name)
                match olyIntrinsicAttrOpt with
                | ValueSome(olyAttr, _) -> yield olyAttr
                | _ -> ()
            }
            |> ImArray.ofSeq

        let methImpls = ImmutableDictionary.CreateBuilder()
        for methImplHandle in tyDef.GetMethodImplementations().ToImmutableArray() do
            let methImpl = reader.GetMethodImplementation(methImplHandle)

            let import (handle: EntityHandle) =
                match handle.Kind with
                | HandleKind.MethodDefinition ->
                    let handle = MethodDefinitionHandle.op_Explicit(handle)
                    let methDef = reader.GetMethodDefinition(handle)
                    let unmangledName = unmangleName (reader.GetString(methDef.Name))
                    let olySpecHandle = importMethodDefinitionAsOlyILFunctionSpecification cenv unmangledName fullTyParCount handle
                    let olyEntDef, fullTyParCount = importTypeDefinitionAsOlyILEntityDefinition cenv (methDef.GetDeclaringType())
                    let olyEnclosing =
                        let olyTyArgs = ImArray.init fullTyParCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))
                        OlyILEnclosing.Entity(OlyILEntityInstance(olyEntDef, olyTyArgs))
                    ValueSome(OlyILFunctionReference(olyEnclosing, olySpecHandle))
                | HandleKind.MemberReference ->
                    let handle = MemberReferenceHandle.op_Explicit(handle)
                    ValueSome(importMemberReferenceAsOlyILFunctionReference cenv handle)
                | kind ->
                    failwith $"{kind} not handled."
            let overridesOpt = import methImpl.MethodDeclaration
            let overridenByOpt = import methImpl.MethodBody
            match overridesOpt, overridenByOpt with
            | ValueSome(overrides), ValueSome(overridenBy) ->
                match methImpl.MethodBody.Kind with
                | HandleKind.MethodDefinition -> ()
                | kind -> failwith $"Expected method definition kind, but got {kind}."
                methImpls.TryAdd(methImpl.MethodBody, overrides) |> ignore
            | _ ->
                ()

        let methImpls = methImpls.ToImmutable()

        let olyFuncs =
            let handles = tyDef.GetMethods().ToImmutableArray()
            seq {
                for i = 0 to handles.Length - 1 do
                    match importMethodDefinitionAsOlyILFunctionDefinition cenv olyEntDefHandle fullTyParCount methImpls handles.[i] with
                    | ValueNone -> ()
                    | ValueSome x -> yield x
            }
            |> ImArray.ofSeq

        let olyFields =
            tyDef.GetFields().ToImmutableArray()
            |> ImArray.choose (fun fieldDefHandle ->
                tryImportFieldDefinitionAsOlyILFieldDefinition cenv fieldDefHandle
            )

        let olyNestedEntDefHandles =
            tyDef.GetNestedTypes()
            |> ImArray.choose (fun tyDefHandle ->
                let nestedTyDef = reader.GetTypeDefinition(tyDefHandle)
                if nestedTyDef.IsNested && (nestedTyDef.Attributes &&& TypeAttributes.VisibilityMask = TypeAttributes.NestedPublic) then
                    let result, _ = importTypeDefinitionAsOlyILEntityDefinition cenv tyDefHandle
                    Some(result)
                else
                    None
            )

        let olyImplements = 
            tyDef.GetInterfaceImplementations().ToImmutableArray()
            |> ImArray.map (fun handle ->
                let impl = reader.GetInterfaceImplementation(handle)
                importAsOlyILType cenv impl.Interface
            )

        let olyInherits =
            let baseTy = tyDef.BaseType
            if baseTy.IsNil then
                ImArray.empty
            else
                importAsOlyILType cenv baseTy
                |> ImArray.createOne

        let olyProps =
            tyDef.GetProperties().ToImmutableArray()
            |> ImArray.choose (fun handle ->
                let propDef = reader.GetPropertyDefinition(handle)

                let olyAttrs = ImArray.empty // TODO:

                let name = propDef.Name |> reader.GetString

                let propAccessor = propDef.GetAccessors()
                let getter = propAccessor.Getter
                let setter = propAccessor.Setter
                if getter.IsNil && setter.IsNil then
                    None
                else
                    let olyGetterOpt =
                        if getter.IsNil then
                            None
                        else
                            match importMethodDefinitionAsOlyILFunctionDefinition cenv olyEntDefHandle fullTyParCount methImpls getter with
                            | ValueSome olyFuncDefHandle -> 
                                let olyFuncDef = olyAsm.GetFunctionDefinition(olyFuncDefHandle)
                                let olyFuncSpec = olyAsm.GetFunctionSpecification(olyFuncDef.SpecificationHandle)
                                match olyFuncSpec with
                                | OlyILFunctionSpecification(pars=olyPars) ->
                                    if olyPars.IsEmpty then
                                        Some olyFuncDefHandle
                                    else
                                        None
                            | _ -> 
                                None

                    let olySetterOpt =
                        if setter.IsNil then
                            None
                        else
                            match importMethodDefinitionAsOlyILFunctionDefinition cenv olyEntDefHandle fullTyParCount methImpls setter with
                            | ValueSome olyFuncDefHandle -> 
                                let olyFuncDef = olyAsm.GetFunctionDefinition(olyFuncDefHandle)
                                let olyFuncSpec = olyAsm.GetFunctionSpecification(olyFuncDef.SpecificationHandle)
                                match olyFuncSpec with
                                | OlyILFunctionSpecification(pars=olyPars) ->
                                    if olyPars.Length = 1 then
                                        Some olyFuncDefHandle
                                    else
                                        None
                            | _ -> 
                                None

                    let olyPropTyOpt = 
                        match olyGetterOpt with
                        | Some olyGetter -> 
                            let olyGetterDef = olyAsm.GetFunctionDefinition(olyGetter)
                            let olyFuncSpecHandle = olyGetterDef.SpecificationHandle
                            let olyFuncSpec = olyAsm.GetFunctionSpecification(olyFuncSpecHandle)
                            if olyFuncSpec.Parameters.IsEmpty then
                                olyFuncSpec.ReturnType
                                |> Some
                            else
                                None
                        | _ ->
                            match olySetterOpt with
                            | Some olySetter ->
                                let olySetterDef = olyAsm.GetFunctionDefinition(olySetter)
                                let olyFuncSpecHandle = olySetterDef.SpecificationHandle
                                let olyFuncSpec = olyAsm.GetFunctionSpecification(olyFuncSpecHandle)
                                let olyPars = olyFuncSpec.Parameters
                                if olyPars.Length = 1 then
                                    olyPars.[0].Type
                                    |> Some
                                else
                                    None
                            | _ ->
                                None

                    match olyPropTyOpt with
                    | Some olyPropTy -> 
                        OlyILPropertyDefinition(
                            olyAttrs,
                            importRawString cenv name,
                            olyPropTy,
                            olyGetterOpt |> Option.defaultValue (OlyILTableIndex(OlyILTableKind.FunctionDefinition, -1)),
                            olySetterOpt |> Option.defaultValue (OlyILTableIndex(OlyILTableKind.FunctionDefinition, -1))
                        )
                        |> olyAsm.AddPropertyDefinition
                        |> Some
                    | _ ->
                        None
            )

        // We may never need this, except for fsharp patterns maybe?
        let olyPatDefs = ImArray.empty

        let olyEntDef =
            OlyILEntityDefinition(
                olyEntKind,
                olyEntFlags,
                olyAttrs,
                olyEnclosing,
                importRawString cenv unmangledName,
                olyTyPars,
                olyFuncs,
                olyFields,
                olyProps,
                olyPatDefs,
                olyNestedEntDefHandles,
                (if isInterface then ImArray.empty else olyImplements),
                (if isInterface then olyImplements else olyInherits)
            )
        
        olyAsm.SetEntityDefinition(olyEntDefHandle, olyEntDef)

        match olyIntrinsicAttrOpt with
        | ValueSome(_, olyBuiltInTy) ->
            olyAsm.AddPrimitiveType(olyBuiltInTy, olyEntDefHandle)
        | _ ->
            ()

        olyEntDefHandle, fullTyParCount

[<Sealed>]
type Importer private (name: string, peReader: PEReader) =

    let reader = peReader.GetMetadataReader()
            
    member private this.Compute() =
        let key = "dotnet"
        let olyAsm = OlyILAssembly.Create(name, key, false)

        let cenv =
            {
                olyAsm = olyAsm
                reader = reader
                olyDefaultCtorConstr = ref Unchecked.defaultof<_>
                stringCache = Dictionary()
                stringToOlyStringCache = Dictionary()
                tyDefToOlyEntRefCache = Dictionary()
                tyDefToOlyEntDefCache = Dictionary()
                tyRefToOlyEntRefCache = Dictionary()
                tySpecToOlyTypeCache = Dictionary()
                exportedTyToOlyEntRefCache = Dictionary()
                methDefToOlyFuncDefCache = Dictionary()
            }

        let olyDefaultCtorConstr =
            let olyEntDefHandle = olyAsm.NextEntityDefinitionHandle()
            OlyAssert.Equal(0, olyEntDefHandle.Index)

            let olyCtorAttrs =
                OlyILAttribute.Export
                |> ImArray.createOne

            let olyEntAttrs =
                OlyILAttribute.Export
                |> ImArray.createOne

            let olyDefaultCtorHandle =
                let olyFuncSpec =
                    OlyILFunctionSpecification(
                        true,
                        OlyILCallingConvention.Default,
                        importRawString cenv "__oly_ctor",
                        ImArray.empty,
                        ImArray.empty,
                        OlyILTypeVoid
                    )

                let olyFuncSpecHandle = olyAsm.AddFunctionSpecification(olyFuncSpec)
                let olyFuncDef =
                    OlyILFunctionDefinition(
                        OlyILFunctionFlags.Constructor,
                        OlyILMemberFlags.Abstract,
                        olyCtorAttrs,
                        olyFuncSpecHandle,
                        None,
                        ref None
                    )
                olyAsm.AddFunctionDefinition(olyEntDefHandle, olyFuncDef)

            let olyEntDef =
                OlyILEntityDefinition(
                    OlyILEntityKind.Shape,
                    OlyILEntityFlags.Abstract,
                    olyEntAttrs,
                    OlyILEnclosing.Namespace(ImArray.empty, olyAsm.Identity),
                    OlyILStringHandle.GetNil(OlyILTableKind.String),
                    ImArray.empty,
                    ImArray.createOne olyDefaultCtorHandle,
                    ImArray.empty,
                    ImArray.empty,
                    ImArray.empty,
                    ImArray.empty,
                    ImArray.empty,
                    ImArray.empty
                )
            olyAsm.SetEntityDefinition(olyEntDefHandle, olyEntDef)
            let olyEntInst = OlyILEntityInstance(olyEntDefHandle, ImArray.empty)
            OlyILConstraint.SubtypeOf(OlyILTypeEntity(olyEntInst))

        cenv.olyDefaultCtorConstr.contents <- olyDefaultCtorConstr

        reader.TypeDefinitions
        |> Seq.iter (fun x ->
            let tyDef = reader.GetTypeDefinition(x)
            if not tyDef.IsNested && (tyDef.Attributes &&& TypeAttributes.VisibilityMask = TypeAttributes.Public) then
                importTypeDefinitionAsOlyILEntityDefinition cenv x |> ignore
        )
        
        let exportedTys = reader.ExportedTypes.ToImmutableArray()
        for i = 0 to exportedTys.Length - 1 do
            importExportedTypeAsOlyILEntityReference cenv exportedTys.[i] |> ignore

        olyAsm

    static member Import(name, stream: Stream) =
        let s = System.Diagnostics.Stopwatch.StartNew()
        use peReader = new PEReader(stream, PEStreamOptions.LeaveOpen)
        try
            let result = Importer(name, peReader).Compute()
            OlyTrace.Log($"[DotNet] (MSIL => OlyIL) Completed: {name} - {s.Elapsed.TotalMilliseconds}ms")
            result
        with
        | ex ->
            OlyTrace.LogError($"[DotNet] (MSIL => OlyIL) Failed: {name} - Exception:\n{ex.ToString()}")
            reraise()

