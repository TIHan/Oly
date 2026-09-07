module internal Oly.Emitters.DotNet.Extensions

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Oly.Core
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns
open DotNet.Metadata
open DotNet.Metadata.ClrPatterns

type BlobBuilder with

    member b.WriteSerializedUTF8(value: string) =
        b.WriteByte(byte value.Length)
        b.WriteUTF8(value)

    member b.WriteTypeOfC(asmBuilder: ClrAssemblyBuilder, x: C<ClrTypeInfo, ClrMethodInfo>) =
        match x with
        | C.Int8 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceByte)
        | C.UInt8 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceSByte)
        | C.Int16 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceInt16)
        | C.UInt16 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceUInt16)
        | C.Int32 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceInt32)
        | C.UInt32 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceUInt32)
        | C.Int64 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceInt64)
        | C.UInt64 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceUInt64)
        | C.Float32 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceSingle)
        | C.Float64 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceDouble)

        | C.True -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceBoolean)

        | C.False -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceBoolean)

        | C.Array(elementTy, _) ->
            let mutable encoder = CustomAttributeArrayTypeEncoder(b)
            if elementTy.Handle = asmBuilder.TypeReferenceObject then
                encoder.ObjectArray()
            else
                asmBuilder.EncodeAttributeElementType(encoder.ElementType(), elementTy.Handle)

        | C.Char16 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceChar)

        | C.Utf16 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceString)

        | C.Variable _ ->
            raise(System.NotSupportedException("constant variable"))

        | C.External(func) -> 
            let func = func.AsDefinition
            match func.specialKind with
            | ClrMethodSpecialKind.TypeOf ->
                if func.ReturnType.IsStruct then
                    ClrElementTypes.ValueType
                    |> b.WriteByte
                else
                    ClrElementTypes.Class
                    |> b.WriteByte
            | ClrMethodSpecialKind.SizeOf ->
                if func.tyInst.Length = 1 && func.tyInst[0].IsStruct then
                    raise(System.NotSupportedException("sizeof constant"))
                    //ClrElementTypes.ValueType
                    //|> b.WriteByte
                else
                    failwith "Invalid use of SizeOf."
            | ClrMethodSpecialKind.FunctionPointer ->
                SignatureTypeCode.FunctionPointer
                |> byte
                |> b.WriteByte
            | _ ->
                raise(System.NotSupportedException($"Constant function '{func.name}'."))

    member b.WriteValueOfC(x: C<ClrTypeInfo, ClrMethodInfo>, asCountedUtf8) =
        match x with
        | C.UInt8(value) -> b.WriteByte(value)
        | C.Int8(value) -> b.WriteSByte(value)
        | C.UInt16(value) -> b.WriteUInt16(value)
        | C.Int16(value) -> b.WriteInt16(value)
        | C.UInt32(value) -> b.WriteUInt32(value)
        | C.Int32(value) -> b.WriteInt32(value)
        | C.UInt64(value) -> b.WriteUInt64(value)
        | C.Int64(value) -> b.WriteInt64(value)
        | C.Float32(value) -> b.WriteSingle(value)
        | C.Float64(value) -> b.WriteDouble(value)
        | C.Char16(value) -> b.WriteUInt16(uint16 value)
        | C.Utf16(value) -> 
            if asCountedUtf8 then
                b.WriteSerializedUTF8(value)
            else
                b.WriteSerializedString(value)
        | C.True -> b.WriteBoolean(true)
        | C.False -> b.WriteBoolean(false)
        | C.Array(ty: ClrTypeInfo, elements) ->
            b.WriteUInt32(uint32 elements.Length)
            elements
            |> ImArray.iter (fun x -> b.WriteValueOfC(x, false))
        | C.Variable _ ->
            raise(System.NotSupportedException("constant variable"))
        | C.External(func) -> 
            let func = func.AsDefinition
            match func.specialKind with
            | ClrMethodSpecialKind.TypeOf when func.tyInst.Length = 1 ->
                let ty = func.tyInst.[0]
                b.WriteSerializedString(ty.FullyQualifiedName)
            | ClrMethodSpecialKind.SizeOf when func.tyInst.Length = 1 ->
                raise(System.NotSupportedException("constant sizeof"))
            | _ ->
                failwith "Invalid external constant."

type ClrTypeDefinitionBuilder with

    member tyDefBuilder.CreateMethodDefinitionBuilderEx(enclosingTy: ClrTypeInfo, flags: OlyIRFunctionFlags, methodName, tyPars, cilParameters, cilReturnTy: ClrTypeHandle, isStatic, isCtor) =
        let methDefBuilder = tyDefBuilder.CreateMethodDefinitionBuilder(methodName, tyPars, cilParameters, cilReturnTy, not isStatic)

        let methAttrs =
            if flags.IsPublic then
                MethodAttributes.Public
            elif flags.IsProtected then
                MethodAttributes.Family
            else
                if flags.IsInternal || (not flags.IsExported) then
                    // We always emit "internal" due to inlining.
                    // Exported functions will never be inlined by Oly.
                    MethodAttributes.Assembly
                else
                    MethodAttributes.Private

        let methAttrs = methAttrs ||| MethodAttributes.HideBySig

        let methAttrs =
            if isStatic then
                methAttrs ||| MethodAttributes.Static
            else
                methAttrs

        let methAttrs =
            if isCtor then
                methAttrs ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName
            else
                methAttrs

        let methAttrs =
            if flags.IsVirtual && not isStatic then
                methAttrs ||| MethodAttributes.Virtual
            else
                methAttrs

        let methAttrs =
            if flags.IsAbstract && not isStatic && not(flags.IsInstance && flags.AreGenericsErased && enclosingTy.IsTypeDefinitionInterface) then
                methAttrs ||| MethodAttributes.Abstract
            else
                methAttrs

        let methAttrs =
            if flags.IsNewSlot && not isStatic then
                methAttrs ||| MethodAttributes.NewSlot
            else
                methAttrs

        let methAttrs =
            if flags.IsFinal && not isStatic then
                methAttrs ||| MethodAttributes.Final ||| MethodAttributes.Virtual
            else
                methAttrs

        methDefBuilder.Attributes <- methAttrs
        methDefBuilder.ImplementationAttributes <- MethodImplAttributes.IL ||| MethodImplAttributes.Managed

        if flags.IsAbstract && isStatic then
            // We do this as it's a stub for a static abstract method.
            methDefBuilder.BodyInstructions <-
                [
                    I.Ldnull
                    I.Throw
                    I.Ret
                ]
                |> ImArray.ofSeq

        methDefBuilder