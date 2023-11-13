module Oly.Runtime.Clr.Emitter

open System
open Oly.Core
open Oly.Platform.Clr.Metadata
open System.Reflection
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns
open ClrPatterns

[<RequireQualifiedAccess>]
type ClrMethodSpecialKind =
    | None
    | External
    | TypeOf
    | SizeOf
    | FunctionPointer
    | CreateDelegate

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type ClrTypeDefinitionInfo =
    {
        mutable enumBaseTyOpt: ClrTypeInfo option
        mutable typeExtensionInfo: (ClrTypeInfo * ClrTypeInfo * ClrFieldHandle) voption
    }

    static member Default =
        {
            enumBaseTyOpt = None
            typeExtensionInfo = ValueNone
        }

and [<RequireQualifiedAccess;NoComparison;NoEquality>] ClrTypeInfo = 
    | TypeGenericInstance of ClrTypeInfo * tyArgs: ClrTypeHandle imarray * appliedTyHandle: ClrTypeHandle
    | TypeReference of assembly: ClrAssemblyBuilder * ClrTypeHandle * isReadOnly: bool * isStruct: bool
    | TypeDefinition of assembly: ClrAssemblyBuilder * ClrTypeDefinitionBuilder * isReadOnly: bool * isInterface: bool * isStruct: bool * isClosure: bool * info: ClrTypeDefinitionInfo

    member this.FullyQualifiedName =
        match this with
        | TypeDefinition(_, tyDefBuilder, _, _, _, _, _) ->
            tyDefBuilder.FullyQualifiedName
        | TypeReference(_, tyHandle, _, _) ->
            tyHandle.FullyQualifiedName
        | TypeGenericInstance(formalTy, _, _) ->
            formalTy.FullyQualifiedName

    member this.IsDefinitionEnum =
        match this with
        | TypeDefinition(info=info) -> info.enumBaseTyOpt.IsSome
        | _ -> false

    member this.TryGetEnumBaseType() =
        match this with
        | TypeDefinition(info=info) -> info.enumBaseTyOpt
        | _ -> None

    member this.IsReadOnly =
        match this with
        | TypeGenericInstance(x, _, _) -> x.IsReadOnly
        | TypeReference(isReadOnly=isReadOnly)
        | TypeDefinition(isReadOnly=isReadOnly) -> isReadOnly

    member this.IsStruct =
        match this with
        | TypeGenericInstance(x, _, _) -> x.IsStruct
        | TypeReference(isStruct=isStruct) -> isStruct
        | TypeDefinition(isStruct=isStruct;info=info) -> 
            if isStruct then true
            else
                (match info.enumBaseTyOpt with Some ty -> ty.IsStruct | _ -> false)

    member this.IsTypeVariable =
        match this with
        | TypeReference(_, handle, _, _) ->
            handle.IsVariable
        | _ ->
            false

    member this.IsByRefOfTypeVariable =
        match this with
        | TypeReference(_, handle, _, _) ->
            handle.IsByRef_t &&
            match handle.TryElementType with
            | ValueSome elementHandle -> elementHandle.IsVariable
            | _ -> false
        | _ ->
            false

    member this.IsByRefOfStruct =
        match this with
        | TypeReference(_, handle, _, _) ->
            handle.IsByRef_t &&
            match handle.TryElementType with
            | ValueSome elementHandle -> elementHandle.IsValueType
            | _ -> false
        | _ ->
            false

    member this.IsByRef =
        match this with
        | TypeReference(_, handle, _, _) ->
            handle.IsByRef_t
        | _ ->
            false

    member this.TryByRefElementType =
        match this with
        | TypeReference(asmBuilder, handle, _, _) ->
            match handle.TryElementType with
            | ValueSome elementHandle -> ValueSome(ClrTypeInfo.TypeReference(asmBuilder, elementHandle, false, false))
            | _ -> ValueNone
        | _ ->
            ValueNone

    member this.IsNativePointer =
        match this with
        | TypeGenericInstance(_, _, handle) ->
            handle.IsNativePointer_t
        | _ ->
            false

    member this.TryTypeVariable =
        match this with
        | TypeReference(asmBuilder, handle, _, _) ->
            handle.TryTypeVariable
        | _ ->
            ValueNone

    member this.IsClosure =
        match this with
        | TypeGenericInstance(x, _, _) -> x.IsClosure
        | TypeReference _ -> false
        | TypeDefinition(isClosure=isClosure) -> isClosure

    member this.IsTypeDefinitionInterface =
        match this with
        | TypeDefinition(isInterface=isInterface) -> isInterface
        | _ -> false

    member this.IsTypeDefinition_t =
        match this with
        | TypeDefinition _ -> true
        | _ -> false

    member this.Handle =
        match this with
        | TypeGenericInstance(_, _, x) -> x
        | TypeReference(_, handle, _, _) -> handle
        | TypeDefinition(_, tyDefBuilder, _, _, _, _, _) -> tyDefBuilder.Handle

    member this.TypeArguments: ClrTypeHandle imarray =
        match this with
        | TypeGenericInstance(tyArgs=tyArgs) -> tyArgs
        | TypeReference(_, ty, _, _) ->
            match ty with
            | ClrTypeHandle.TypeSpecification(tyInst=tyInst) -> tyInst
            | _ -> ImArray.empty
        | _ -> ImArray.empty

    member this.TryTypeExtensionType =
        match this with
        | TypeDefinition(_, _, _, _, _, _, info) -> info.typeExtensionInfo |> ValueOption.map (fun (x) -> x)
        | _ -> ValueNone

    member this.Assembly =
        match this with
        | TypeGenericInstance(x, _, _) -> x.Assembly
        | TypeReference(assembly=asm)
        | TypeDefinition(assembly=asm) -> asm

    member this.TypeDefinitionBuilder =
        match this with
        | TypeDefinition(_, tyDefBuilder, _, _, _, _, _) ->
            tyDefBuilder
        | _ ->
            OlyAssert.Fail("Not a type definition.")

    member this.TypeDefinitionInfo =
        match this with
        | TypeDefinition(_, _, _, _, _, _, info) ->
            info
        | _ ->
            OlyAssert.Fail("Not a type definition.")

    member this.MethodDefinitionBuilders: ClrMethodDefinitionBuilder seq =
        match this with
        | TypeDefinition(_, tyDefBuilder, _, _, _, _, _) ->
            tyDefBuilder.MethodDefinitionBuilders
        | _ ->
            ImArray.empty

[<ReferenceEquality;NoComparison>]
type ClrFieldInfo = 
    { 
        handle: ClrFieldHandle
        isMutable: bool } with

    member this.Handle: ClrFieldHandle = this.handle

and [<ReferenceEquality;NoComparison>] ClrMethodInfo =
    {
        isEnclosingClosure: bool
        enclosingTyHandle: ClrTypeHandle
        handle: ClrMethodHandle
        builder: ClrMethodDefinitionBuilder option
        name: string
        isStatic: bool
        isConstructor: bool
        returnTy: ClrTypeInfo
        pars: (string * ClrTypeInfo) imarray
        tyInst: ClrTypeInfo imarray
        specialKind: ClrMethodSpecialKind
        tyParCount: int
    }

    member this.IsStatic = this.isStatic
    member this.IsInstance = not this.IsStatic

    member this.ReturnType = this.returnTy

    member this.Parameters = this.pars

type BlobBuilder with

    member b.WriteSerializedUTF8(value: string) =
        b.WriteByte(byte value.Length)
        b.WriteUTF8(value)

    member b.WriteTypeOfC(asmBuilder: ClrAssemblyBuilder, x: C<ClrTypeInfo, _>) =
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

        | C.True _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceBoolean)

        | C.False _ -> 
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

    member b.WriteValueOfC(x, asCountedUtf8) =
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
            match func.specialKind with
            | ClrMethodSpecialKind.TypeOf when func.tyInst.Length = 1 ->
                let ty = func.tyInst.[0]
                b.WriteSerializedString(ty.FullyQualifiedName)
            | ClrMethodSpecialKind.SizeOf when func.tyInst.Length = 1 ->
                raise(System.NotSupportedException("constant sizeof"))
            | _ ->
                failwith "Invalid external constant."

module rec ClrCodeGen =

    type cenv =
        {
            assembly: ClrAssemblyBuilder
            emitTailCalls: bool
            buffer: imarrayb<I>
            locals: System.Collections.Generic.Dictionary<int, string * ClrTypeInfo>
            dups: System.Collections.Generic.HashSet<int>
            irTier: OlyIRFunctionTier
            debugLocalsInScope: System.Collections.Generic.Dictionary<int, ClrDebugLocal>
            newUniqueId: unit -> int64
            mutable localCount: int ref
            mutable nextLabelId: int32 ref
            mutable seqPointCount: int ref
            mutable retEmitted: bool ref
        }

        member this.NewBuffer() =
            { this with
                buffer = ImArray.builder ()
            }

        member this.NewLocal(ty: ClrTypeInfo) =
            let localIndex = this.localCount.contents
            this.localCount.contents <- this.localCount.contents + 1
            this.locals.[localIndex] <- (String.Empty, ty)
            localIndex

        member this.NewLabel() =
            let labelId = this.nextLabelId.contents
            this.nextLabelId.contents <- this.nextLabelId.contents + 1
            labelId

        member this.IsDebuggable =
            match this.irTier with
            | OlyIRFunctionTier.Tier0 true -> true
            | _ -> false

        member this.GetSequencePointCount() =
            this.seqPointCount.contents

        member this.IncrementSequencePointCount() =
            this.seqPointCount.contents <- this.seqPointCount.contents + 1

        member this.DecrementSequencePointCount() =
            this.seqPointCount.contents <- this.seqPointCount.contents - 1

        member this.SetRetEmitted() =
            this.retEmitted.contents <- true

        member this.UnsetRetEmitted() =
            this.retEmitted.contents <- false

        member this.IsRetEmitted =
            this.retEmitted.contents

    type SequencePointBehavior =
        | EnableSequencePoint
        | DisableSequencePoint

    type env =
        {
            isInWhileLoop: bool
            isReturnable: bool
            spb: SequencePointBehavior
        }

    let isByRefLike (asmBuilder: ClrAssemblyBuilder) (x: ClrTypeInfo) =
        if x.IsByRef then
            true
        else
            let result =
                match asmBuilder.tr_Span with
                | Some(tr) ->
                    match x with
                    | ClrTypeInfo.TypeDefinition _ ->                
                        x.Handle = tr
                    | ClrTypeInfo.TypeReference(_, handle, _, _) ->
                        handle = tr
                    | ClrTypeInfo.TypeGenericInstance(info, _, _) ->
                        info.Handle = tr                  
                | _ ->
                    false

            if result then
                true
            else
                match asmBuilder.tr_ReadOnlySpan with
                | Some(tr) ->
                    match x with
                    | ClrTypeInfo.TypeDefinition _ ->                
                        x.Handle = tr
                    | ClrTypeInfo.TypeReference(_, handle, _, _) ->
                        handle = tr
                    | ClrTypeInfo.TypeGenericInstance(info, _, _) ->
                        info.Handle = tr
                | _ ->
                    false

    let writeAttributeArguments (asmBuilder: ClrAssemblyBuilder) (irArgs: C<ClrTypeInfo, ClrMethodInfo> imarray) (irNamedArgs: OlyIRAttributeNamedArgument<ClrTypeInfo, ClrMethodInfo> imarray) =
        let b = BlobBuilder()

        // Prolog
        b.WriteByte(1uy)
        b.WriteByte(0uy)

        irArgs
        |> ImArray.iter (fun x -> b.WriteValueOfC(x, false))

        // NumNamed
        let numNamed = irNamedArgs.Length
        b.WriteByte(byte numNamed)
        b.WriteByte(0uy)
        
        irNamedArgs
        |> ImArray.iter (fun { Kind = kind; Name = name; Constant = x } ->
            match kind with
            | OlyIRAttributeNamedArgumentKind.Property ->
                b.WriteByte(byte 0x54) // PROPERTY
            | OlyIRAttributeNamedArgumentKind.Field ->
                b.WriteByte(byte 0x53) // FIELD

            b.WriteTypeOfC(asmBuilder, x)
            b.WriteSerializedUTF8(name)
            b.WriteValueOfC(x, true)
        )

        asmBuilder.AddBlob(b)

    let createMulticastDelegateTypeDefinition (asmBuilder: ClrAssemblyBuilder) name invokeParTys invokeReturnTy =

        let tyDef = asmBuilder.CreateTypeDefinitionBuilder(ClrTypeHandle.Empty, "", name, 0, false)

        tyDef.Attributes <- TypeAttributes.Sealed
        tyDef.BaseType <- asmBuilder.MulticastDelegate

        let parTys = ImArray.createTwo ("", asmBuilder.TypeReferenceObject) ("", asmBuilder.TypeReferenceIntPtr)
        let ctor = tyDef.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, parTys, asmBuilder.TypeReferenceVoid, true)
        ctor.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName
        ctor.ImplementationAttributes <- MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed

        let invoke = tyDef.CreateMethodDefinitionBuilder("Invoke", ImArray.empty, invokeParTys, invokeReturnTy, true)

        let mutable strictAttr = 0x00000200
        invoke.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual ||| (System.Runtime.CompilerServices.Unsafe.As(&strictAttr))
        invoke.ImplementationAttributes <- MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed

        match (asmBuilder.tr_UnmanagedFunctionPointerAttribute, asmBuilder.tr_CallingConvention) with
        | Some(attr), Some(callConv) ->

            let attrTy = ClrTypeInfo.TypeReference(asmBuilder, attr, false, false)
            let callConvTy = ClrTypeInfo.TypeReference(asmBuilder, callConv, false, true)

            let ctorArgs =
                // TODO: VERY BAD - for now we are defaulting to cdecl, which isn't great
                OlyIRConstant.Int32((* cdecl *) 2)
                |> ImArray.createOne
            let ctorHandle = 
                asmBuilder.CreateMethodHandle(
                    attrTy.Handle,
                    ".ctor",
                    true,
                    ImArray.createOne(callConvTy.Handle),
                    asmBuilder.TypeReferenceVoid
                )

            asmBuilder.AddTypeAttribute(tyDef.Handle, ctorHandle, ClrCodeGen.writeAttributeArguments asmBuilder ctorArgs ImArray.empty)
        | _ -> 
            ()
        tyDef

    let createMulticastDelegateConstructor (asmBuilder: ClrAssemblyBuilder) (enclosingTy: ClrTypeInfo) =
        (enclosingTy.MethodDefinitionBuilders |> Seq.item 0).Handle

    let createMulticastDelegateInvoke (asmBuilder: ClrAssemblyBuilder) (enclosingTy: ClrTypeInfo) =
        (enclosingTy.MethodDefinitionBuilders |> Seq.item 1).Handle

    let createAnonymousFunctionConstructor (asmBuilder: ClrAssemblyBuilder) enclosingTy =
        asmBuilder.AddAnonymousFunctionConstructor(enclosingTy)

    let createAnonymousFunctionInvoke (asmBuilder: ClrAssemblyBuilder) enclosingTy tyArgs parTys returnTy =
        asmBuilder.AddAnonymousFunctionInvoke(enclosingTy, tyArgs, handleTypeArguments asmBuilder parTys, handleTypeArgument asmBuilder returnTy)

    let createAnonymousFunctionType (asmBuilder: ClrAssemblyBuilder) parTys returnTy =
        let handle, _ = asmBuilder.AddAnonymousFunctionType(handleTypeArguments asmBuilder parTys, handleTypeArgument asmBuilder returnTy)
        handle

    let handleTypeArgument (asmBuilder: ClrAssemblyBuilder) (tyArgHandle: ClrTypeHandle) =
        if tyArgHandle.IsNativePointer_t then
            asmBuilder.TypeReferenceIntPtr
        else
            tyArgHandle

    let handleTypeArguments (asmBuilder: ClrAssemblyBuilder) (tyArgHandles: ClrTypeHandle imarray) =
        tyArgHandles
        |> ImArray.map (handleTypeArgument asmBuilder)

    let private tryGetLastInstruction cenv =
        if cenv.buffer.Count > 0 then
            cenv.buffer[cenv.buffer.Count - 1]
            |> ValueSome
        else
            ValueNone

    let private tryGetSecondToLastInstruction cenv =
        if cenv.buffer.Count > 1 then
            cenv.buffer[cenv.buffer.Count - 2]
            |> ValueSome
        else
            ValueNone

    let private setLastInstruction cenv instr =
        if cenv.buffer.Count > 0 then
            cenv.buffer[cenv.buffer.Count - 1] <- instr
        else
            OlyAssert.Fail("Cannot set last instruction as no instructions have been emitted.")

    let private setSecondToLastInstruction cenv instr =
        if cenv.buffer.Count > 1 then
            cenv.buffer[cenv.buffer.Count - 2] <- instr
        else
            OlyAssert.Fail("Cannot set second-to-last instruction as not enough instructions have been emitted.")

    let private setNotReturnable env =
        if env.isReturnable then { env with isReturnable = false }
        else env

    let private setDisableSequencePoint env =
        match env.spb with
        | DisableSequencePoint -> env
        | _ -> { env with spb = DisableSequencePoint }

    let private setEnableSequencePoint env =
        match env.spb with
        | EnableSequencePoint -> env
        | _ -> { env with spb = EnableSequencePoint }

    let private getPrimitiveTypeCode cenv (ty: ClrTypeInfo) =
        if ty.IsDefinitionEnum then
            getPrimitiveTypeCode cenv (ty.TryGetEnumBaseType().Value)
        else

        if not ty.Handle.HasEntityHandle then
            PrimitiveTypeCode.Object

        elif ty.Handle = cenv.assembly.TypeReferenceByte then
            PrimitiveTypeCode.Byte

        elif ty.Handle = cenv.assembly.TypeReferenceSByte then
            PrimitiveTypeCode.SByte

        elif ty.Handle = cenv.assembly.TypeReferenceUInt16 then
            PrimitiveTypeCode.UInt16

        elif ty.Handle = cenv.assembly.TypeReferenceInt16 then
            PrimitiveTypeCode.Int16

        elif ty.Handle = cenv.assembly.TypeReferenceUInt32 then
            PrimitiveTypeCode.UInt32

        elif ty.Handle = cenv.assembly.TypeReferenceInt32 then
            PrimitiveTypeCode.Int32

        elif ty.Handle = cenv.assembly.TypeReferenceUInt64 then
            PrimitiveTypeCode.UInt64

        elif ty.Handle = cenv.assembly.TypeReferenceInt64 then
            PrimitiveTypeCode.Int64

        elif ty.Handle = cenv.assembly.TypeReferenceSingle then
            PrimitiveTypeCode.Single

        elif ty.Handle = cenv.assembly.TypeReferenceDouble then
            PrimitiveTypeCode.Double

        elif ty.Handle = cenv.assembly.TypeReferenceChar then
            PrimitiveTypeCode.Char

        elif ty.Handle = cenv.assembly.TypeReferenceString then
            PrimitiveTypeCode.String

        elif ty.Handle = cenv.assembly.TypeReferenceBoolean then
            PrimitiveTypeCode.Boolean

        elif ty.Handle = cenv.assembly.TypeReferenceIntPtr then
            PrimitiveTypeCode.IntPtr

        elif ty.Handle = cenv.assembly.TypeReferenceUIntPtr then
            PrimitiveTypeCode.UIntPtr

        elif ty.Handle = cenv.assembly.TypeReferenceVoid then
            PrimitiveTypeCode.Void

        // TODO: Handle TypedReferenced? maybe we don't really need it...

        else
            PrimitiveTypeCode.Object

    let emitInstruction (cenv: cenv) (instr: I) =
        match instr with
        | I.Ret ->
            cenv.SetRetEmitted()
        | I.BeginLocalScope _
        | I.EndLocalScope
        | I.SequencePoint _
        | I.HiddenSequencePoint _ -> ()
        | _ ->
            cenv.UnsetRetEmitted()
        cenv.buffer.Add(instr)

    let emitInstructions cenv (instrs: imarrayb<_>) =
        cenv.buffer.AddRange(instrs)

    let emitConv cenv tyCode =
        match tyCode with
        | PrimitiveTypeCode.UIntPtr ->
            I.Conv_u |> emitInstruction cenv
        | PrimitiveTypeCode.Byte ->
            I.Conv_u1 |> emitInstruction cenv
        | PrimitiveTypeCode.UInt16 ->
            I.Conv_u2 |> emitInstruction cenv
        | PrimitiveTypeCode.UInt32 ->
            I.Conv_u4 |> emitInstruction cenv
        | PrimitiveTypeCode.UInt64 ->
            I.Conv_u8 |> emitInstruction cenv

        | PrimitiveTypeCode.IntPtr ->
            I.Conv_i |> emitInstruction cenv
        | PrimitiveTypeCode.SByte ->
            I.Conv_i1 |> emitInstruction cenv
        | PrimitiveTypeCode.Int16 ->
            I.Conv_i2 |> emitInstruction cenv
        | PrimitiveTypeCode.Int32 ->
            I.Conv_i4 |> emitInstruction cenv
        | PrimitiveTypeCode.Int64 ->
            I.Conv_i8 |> emitInstruction cenv

        | PrimitiveTypeCode.Single ->
            I.Conv_r4 |> emitInstruction cenv
        | PrimitiveTypeCode.Double ->
            I.Conv_r8 |> emitInstruction cenv

        | _ -> () // possible unsafe cast

    let emitConvForOp cenv ty =
        let tyCode = getPrimitiveTypeCode cenv ty
        match tyCode with
        | PrimitiveTypeCode.Byte
        | PrimitiveTypeCode.SByte
        | PrimitiveTypeCode.UInt16
        | PrimitiveTypeCode.Int16 ->
            emitConv cenv tyCode
        | PrimitiveTypeCode.UInt32 ->
            I.Conv_i4 |> emitInstruction cenv
        | _ ->
            ()

    let GenArgumentExpression (cenv: cenv) (env: env) expr =
        match expr with
        | E.Value _ -> 
            GenExpression cenv (setDisableSequencePoint env) expr
        | _ ->
            GenExpression cenv env expr

    let GenOperation (cenv: cenv) prevEnv (irOp: O<ClrTypeInfo, _, _>) =
        let env = { prevEnv with isReturnable = false }
        match irOp with
        | O.LoadFunction(irFunc: OlyIRFunction<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>, receiverExpr, funcTy) ->
            if receiverExpr.ResultType.IsStruct then
                let boxTy = ClrTypeInfo.TypeReference(cenv.assembly, cenv.assembly.TypeReferenceObject, false, false)
                GenArgumentExpression cenv env (E.Operation(NoRange, O.Box(receiverExpr, boxTy)))
            else
                GenArgumentExpression cenv env receiverExpr

            emitInstruction cenv (I.Ldftn(irFunc.EmittedFunction.handle))

            let ctor = 
                if funcTy.IsTypeDefinition_t then
                    ClrCodeGen.createMulticastDelegateConstructor cenv.assembly funcTy
                else
                    ClrCodeGen.createAnonymousFunctionConstructor cenv.assembly funcTy.Handle
            I.Newobj(ctor, irFunc.EmittedFunction.Parameters.Length - 1) |> emitInstruction cenv

        | O.CallStaticConstructor _ ->
            // .NET already handles static constructor invocation.
            // No need to do it ourselves; just simply skip it.
            ()

        | O.LoadArrayLength(irReceiver, rank, _) ->
            GenArgumentExpression cenv env irReceiver

            if rank > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                emitInstruction cenv I.Ldlen

        | O.LoadArrayElement(irReceiver, irIndexArgs, resultTy) ->
            GenArgumentExpression cenv env irReceiver
            if irIndexArgs.Length > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                GenArgumentExpression cenv env irIndexArgs[0]
                emitInstruction cenv (I.Ldelem resultTy.Handle)

        | O.LoadArrayElementAddress(irReceiver, irIndexArgs, _, resultTy) ->
            GenArgumentExpression cenv env irReceiver
            if irIndexArgs.Length > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                GenArgumentExpression cenv env irIndexArgs[0]
                match resultTy.TryByRefElementType with
                | ValueSome resultTy ->
                    emitInstruction cenv (I.Ldelema resultTy.Handle)
                | _ ->
                    OlyAssert.Fail("Expected ByRef type.")

        | O.StoreArrayElement(irReceiver, irIndexArgs, irRhsArg, _) ->
            let tyHandle =
                match irReceiver.ResultType.Handle.TryElementType with
                | ValueSome ty -> ty
                | _ -> failwith "Expecting a type with an element."

            GenArgumentExpression cenv env irReceiver
            if irIndexArgs.Length > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                GenArgumentExpression cenv env irIndexArgs[0]
                GenArgumentExpression cenv env irRhsArg
                emitInstruction cenv (I.Stelem tyHandle)

        | O.BitwiseNot(irArg, resultTy) ->
            GenArgumentExpression cenv env irArg
            emitInstruction cenv I.Not
            emitConvForOp cenv resultTy

        | O.BitwiseOr(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            emitInstruction cenv I.Or
            emitConvForOp cenv resultTy

        | O.BitwiseExclusiveOr(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            emitInstruction cenv I.Xor
            emitConvForOp cenv resultTy

        | O.BitwiseAnd(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            emitInstruction cenv I.And
            emitConvForOp cenv resultTy

        | O.BitwiseShiftLeft(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            emitInstruction cenv I.Shl
            emitConvForOp cenv resultTy

        | O.BitwiseShiftRight(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            let ty = getPrimitiveTypeCode cenv resultTy
            match ty with
            | PrimitiveTypeCode.Byte
            | PrimitiveTypeCode.UInt16
            | PrimitiveTypeCode.UInt32
            | PrimitiveTypeCode.UInt64
            | PrimitiveTypeCode.UIntPtr ->
                emitInstruction cenv I.Shr_un
            | _ ->
                emitInstruction cenv I.Shr  
            emitConvForOp cenv resultTy

        | O.Witness(irBody, witnessTy, _) ->
            GenExpression cenv env irBody
            match witnessTy with
            | ClrTypeInfo.TypeDefinition(info = { typeExtensionInfo=ValueSome(_, instanceTyInfo, _) }) ->
                let ctorHandle =
                    match instanceTyInfo with
                    | ClrTypeInfo.TypeDefinition(_, instanceTyDefBuilder, _, _, _, _, _) ->
                        if irBody.ResultType.Handle.IsByRef_t then
                            match instanceTyDefBuilder.TryGetSingleByRefConstructor() with
                            | Some(ctorDefBuilder) -> ctorDefBuilder.Handle
                            | _ -> failwith "Expected a single constructor."
                        else
                            match instanceTyDefBuilder.TryGetSingleNonByRefConstructor() with
                            | Some(ctorDefBuilder) -> ctorDefBuilder.Handle
                            | _ -> failwith "Expected a single constructor."
                    | _ ->
                        failwith "Expected a type definition."
                emitInstruction cenv (I.Newobj(ctorHandle, 1))
            | _ ->
                failwith "Expected a type definition."

        | O.Upcast(irArg, _) ->
            GenArgumentExpression cenv env irArg

        | O.Box(irArg, _) ->
            GenArgumentExpression cenv env irArg
            emitInstruction cenv (I.Box(irArg.ResultType.Handle))

        | O.Unbox(irArg, resultTy) ->
            GenArgumentExpression cenv env irArg
            emitInstruction cenv (I.Unbox_any(resultTy.Handle))

        | O.Print(E.Operation(op=O.Box(irArg, _)), _) when irArg.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ->
            GenArgumentExpression cenv env irArg
            I.Call(cenv.assembly.ConsoleWriteMethod_Int32.Value, 1) |> emitInstruction cenv

        | O.Print(E.Operation(op=O.Upcast(irArg, _)), _) when irArg.ResultType.Handle = cenv.assembly.TypeReferenceString ->
            GenArgumentExpression cenv env irArg
            I.Call(cenv.assembly.ConsoleWriteMethod_String.Value, 1) |> emitInstruction cenv

        | O.Print(irArg, _) ->
            GenArgumentExpression cenv env irArg
            I.Call(cenv.assembly.ConsoleWriteMethod.Value, 1) |> emitInstruction cenv

        | O.Cast(irArg, resultTy) ->
            GenArgumentExpression cenv env irArg

            if resultTy.IsTypeVariable then
                I.Box(irArg.ResultType.Handle) |> emitInstruction cenv
                I.Unbox_any(resultTy.Handle) |> emitInstruction cenv
            else

            let castFromTy = irArg.ResultType |> getPrimitiveTypeCode cenv
            let castToTy = resultTy |> getPrimitiveTypeCode cenv
            
            match castFromTy with
            | PrimitiveTypeCode.Object
            | PrimitiveTypeCode.TypedReference
            | PrimitiveTypeCode.String
            | PrimitiveTypeCode.Void -> () // unsafe cast
            | PrimitiveTypeCode.Byte
            | PrimitiveTypeCode.UInt16
            | PrimitiveTypeCode.UInt32
            | PrimitiveTypeCode.UInt64 
            | PrimitiveTypeCode.UIntPtr ->
                match castToTy with
                | PrimitiveTypeCode.Single ->
                    I.Conv_r_un |> emitInstruction cenv
                    I.Conv_r4 |> emitInstruction cenv
                | PrimitiveTypeCode.Double ->
                    I.Conv_r_un |> emitInstruction cenv
                    I.Conv_r4 |> emitInstruction cenv
                | _ ->
                    emitConv cenv castToTy
            | _ ->
                emitConv cenv castToTy     

        | O.Throw(irArg, _) ->
            GenArgumentExpression cenv env irArg
            I.Throw |> emitInstruction cenv

        | O.Ignore(irArg, _) ->
            GenArgumentExpression cenv env irArg
            match irArg with
            | E.Operation(op=O.Throw _) -> () // do not emit a pop for a Throw.
            | _ ->
                I.Pop |> emitInstruction cenv

        | O.Add(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            I.Add |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Subtract(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            I.Sub |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Multiply(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            I.Mul |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Divide(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            I.Div |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Remainder(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            I.Rem |> emitInstruction cenv
            emitConvForOp cenv resultTy

        | O.Not(irArg, _) ->
            GenArgumentExpression cenv env irArg
            I.LdcI4 0 |> emitInstruction cenv
            I.Ceq |> emitInstruction cenv

        | O.Negate(irArg, _) ->
            GenArgumentExpression cenv env irArg
            I.Neg |> emitInstruction cenv

        | O.Equal(irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            I.Ceq |> emitInstruction cenv
        | O.NotEqual(irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            I.Ceq |> emitInstruction cenv
            I.LdcI4 0 |> emitInstruction cenv
            I.Ceq |> emitInstruction cenv

        | O.Utf16Equal(irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            let methHandle = cenv.assembly.String_Equals.Value
            I.Call(methHandle, 2) |> emitInstruction cenv

        | O.GreaterThan(irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            if irArg1.ResultType.Handle = cenv.assembly.TypeReferenceSByte ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt16 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt64 then
                I.Cgt |> emitInstruction cenv
            else
                I.Cgt_un |> emitInstruction cenv
        | O.GreaterThanOrEqual(irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            if irArg1.ResultType.Handle = cenv.assembly.TypeReferenceSByte ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt16 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt64 then
                I.Clt |> emitInstruction cenv
                I.LdcI4 0 |> emitInstruction cenv
                I.Ceq |> emitInstruction cenv
            else
                I.Clt_un |> emitInstruction cenv
                I.LdcI4 0 |> emitInstruction cenv
                I.Ceq |> emitInstruction cenv
        | O.LessThan(irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            if irArg1.ResultType.Handle = cenv.assembly.TypeReferenceSByte ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt16 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt64 then
                I.Clt |> emitInstruction cenv
            else
                I.Clt_un |> emitInstruction cenv
        | O.LessThanOrEqual(irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            if irArg1.ResultType.Handle = cenv.assembly.TypeReferenceSByte ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt16 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt64 then               
                I.Cgt |> emitInstruction cenv
                I.LdcI4 0 |> emitInstruction cenv
                I.Ceq |> emitInstruction cenv
            else
                I.Cgt_un |> emitInstruction cenv
                I.LdcI4 0 |> emitInstruction cenv
                I.Ceq |> emitInstruction cenv

        | O.LoadRefCellContents(irArg, resultTy) ->
            GenArgumentExpression cenv env irArg
            I.LdcI4 0 |> emitInstruction cenv
            I.Ldelem(resultTy.Handle) |> emitInstruction cenv

        | O.LoadRefCellContentsAddress(irArg, _, resultTy) ->
            GenArgumentExpression cenv env irArg
            I.LdcI4 0 |> emitInstruction cenv

            match resultTy.TryByRefElementType with
            | ValueSome resultTy ->
                emitInstruction cenv (I.Ldelema resultTy.Handle)
            | _ ->
                OlyAssert.Fail("Expected ByRef type.")

        | O.LoadTupleElement(irArg, index, _) ->
            GenArgumentExpression cenv env irArg
            let methHandle =
                match irArg.ResultType with
                | ClrTypeInfo.TypeReference(asmBuilder, tyHandle, _, _) ->
                    match tyHandle with
                    | ClrTypeHandle.TypeSpecification(tyRefHandle=formalTyHandle) ->
                        let methHandle =
                            if formalTyHandle = asmBuilder.``TypeReferenceTuple`2`` then
                                asmBuilder.``Tuple`2_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`3`` then
                                asmBuilder.``Tuple`3_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`4`` then
                                asmBuilder.``Tuple`4_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`5`` then
                                asmBuilder.``Tuple`5_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`6`` then
                                asmBuilder.``Tuple`6_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`7`` then
                                asmBuilder.``Tuple`7_ItemMethods``.Value[index]
                            else
                                failwith "Invalid LoadTupleItem"
                        asmBuilder.CreateMethodSpecification(tyHandle, methHandle, ImArray.empty)
                    | _ ->
                        failwith "Invalid LoadTupleItem"
                | _ ->
                    failwith "Invalid LoadTupleItem"
            I.Call(methHandle, 1) |> emitInstruction cenv

        | O.StoreRefCellContents(irArg1, irArg2, _) ->
            let tyHandle =
                match irArg1.ResultType.Handle.TryElementType with
                | ValueSome ty -> ty
                | _ -> failwith "Expecting a type with an element."

            GenArgumentExpression cenv env irArg1
            I.LdcI4 0 |> emitInstruction cenv
            GenArgumentExpression cenv env irArg2
            I.Stelem(tyHandle) |> emitInstruction cenv

        | O.Store(n, irArg1, _) ->
            GenArgumentExpression cenv env irArg1
            I.Stloc n |> emitInstruction cenv

        | O.StoreArgument(n, irArg1, _) ->
            GenArgumentExpression cenv env irArg1
            I.Starg n |> emitInstruction cenv

        | O.StoreToAddress(irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            let ty = irArg2.ResultType
            if ty.Handle = cenv.assembly.TypeReferenceInt32 then
                I.Stind_i4 |> emitInstruction cenv
            elif ty.Handle = cenv.assembly.TypeReferenceInt64 then
                I.Stind_i8 |> emitInstruction cenv
            elif ty.Handle = cenv.assembly.TypeReferenceUInt64 then
                I.Stind_i8 |> emitInstruction cenv
            else
                match irArg1.ResultType.TryByRefElementType with
                | ValueSome(elementTy) when not elementTy.IsStruct ->
                    I.Stobj(elementTy.Handle) |> emitInstruction cenv
                | _ ->
                    I.Stind_ref |> emitInstruction cenv

        | O.StoreField(irField: OlyIRField<_, _, ClrFieldInfo>, irArg1, irArg2, _) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            I.Stfld irField.EmittedField.Handle |> emitInstruction cenv

        | O.StoreStaticField(irField, irArg, _) ->
            GenArgumentExpression cenv env irArg
            I.Stsfld irField.EmittedField.Handle |> emitInstruction cenv

        | O.LoadFromAddress(irArg, returnTy) ->
            GenArgumentExpression cenv env irArg
            if returnTy.IsStruct then
                if returnTy.Handle = cenv.assembly.TypeReferenceInt32 then
                    I.Ldind_i4 |> emitInstruction cenv
                elif returnTy.Handle = cenv.assembly.TypeReferenceInt64 then
                    I.Ldind_i8 |> emitInstruction cenv
                elif returnTy.Handle = cenv.assembly.TypeReferenceUInt64 then
                    I.Ldind_i8 |> emitInstruction cenv
                else
                    I.Ldobj(returnTy.Handle) |> emitInstruction cenv
            else
                I.Ldobj(returnTy.Handle) |> emitInstruction cenv

        | O.LoadField(irField, irArg, _) ->
            GenArgumentExpression cenv env irArg
            I.Ldfld irField.EmittedField.Handle |> emitInstruction cenv

        | O.LoadFieldAddress(irField, irArg, _, _) ->
            GenArgumentExpression cenv env irArg
            I.Ldflda irField.EmittedField.Handle |> emitInstruction cenv

        | O.NewTuple(itemTys, irArgs, _) ->
            irArgs |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)
            let tyArgs =
                itemTys
                |> ImArray.map (fun x -> x.Handle)
            I.Newobj(cenv.assembly.AddTupleConstructor(tyArgs), irArgs.Length) |> emitInstruction cenv
            
        | O.NewMutableArray(elementTy, irSizeArgExpr, _) ->
            GenArgumentExpression cenv env irSizeArgExpr
            I.Newarr(elementTy.Handle) |> emitInstruction cenv

        | O.NewArray(elementTy, _irKind, irArgExprs, _) ->
            I.LdcI4 (irArgExprs.Length) |> emitInstruction cenv
            I.Newarr(elementTy.Handle) |> emitInstruction cenv
            for i = 0 to irArgExprs.Length - 1 do
                I.Dup |> emitInstruction cenv
                I.LdcI4 i |> emitInstruction cenv
                GenArgumentExpression cenv env irArgExprs[i]
                I.Stelem(elementTy.Handle) |> emitInstruction cenv

        | O.NewRefCell(contentTy, irArg, _) ->
            I.LdcI4 1 |> emitInstruction cenv
            I.Newarr(contentTy.Handle) |> emitInstruction cenv
            I.Dup |> emitInstruction cenv
            I.LdcI4 0 |> emitInstruction cenv
            GenArgumentExpression cenv env irArg
            I.Stelem(contentTy.Handle) |> emitInstruction cenv

        | O.New(irFunc, irArgs, _) ->
            GenNew cenv env irFunc.EmittedFunction irArgs

        | O.Call(irFunc, irArgs, _) ->
            GenCall cenv env prevEnv.isReturnable irFunc.EmittedFunction irArgs false

        | O.CallVirtual(irFunc, irArgs, _) ->
            GenCall cenv env prevEnv.isReturnable irFunc.EmittedFunction irArgs true

        | O.CallIndirect(runtimeArgTys, irFunArg, irArgs, runtimeResultTy) ->
            GenCallIndirect cenv env irFunArg irArgs runtimeArgTys runtimeResultTy

    let GenValue cenv env (irValue: V<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        match irValue with
        | V.Unit _ -> I.Ldnull |> emitInstruction cenv
        | V.Null _ -> I.Ldnull |> emitInstruction cenv
        | V.Constant(irConstant, _) ->
            match irConstant with
            | C.True _ -> I.LdcI4(1) |> emitInstruction cenv
            | C.False _ -> I.LdcI4(0) |> emitInstruction cenv
            | C.Int8(v) -> I.LdcI4(int32 v) |> emitInstruction cenv
            | C.UInt8(v) -> 
                I.LdcI4(int32 v) |> emitInstruction cenv
            | C.Int16(v) -> I.LdcI4(int32 v) |> emitInstruction cenv
            | C.UInt16(v) -> 
                I.LdcI4(int32 v) |> emitInstruction cenv
            | C.Int32(v) -> I.LdcI4(v) |> emitInstruction cenv
            | C.UInt32(v) -> 
                I.LdcI4(int32 v) |> emitInstruction cenv
            | C.Int64(v) -> I.LdcI8(v) |> emitInstruction cenv
            | C.UInt64(v) -> 
                I.LdcI8(int64 v) |> emitInstruction cenv
            | C.Float32(v) -> I.LdcR4(v) |> emitInstruction cenv
            | C.Float64(v) -> I.LdcR8(v) |> emitInstruction cenv
            | C.Char16(v) -> I.LdcI4(int32 v) |> emitInstruction cenv
            | C.Utf16(v) -> I.Ldstr v |> emitInstruction cenv
            | C.Array _ ->
                raise(System.NotImplementedException())
            | C.External _ ->
                raise(System.NotImplementedException())
            | C.Variable _ ->
                raise(System.NotSupportedException())

        | V.StaticField(irField, _) ->
            I.Ldsfld irField.EmittedField.Handle |> emitInstruction cenv

        | V.StaticFieldAddress(irField, _, _) ->
            I.Ldsflda irField.EmittedField.Handle |> emitInstruction cenv

        | V.Local(n, _) ->
            if cenv.dups.Contains(n) |> not then
                I.Ldloc n |> emitInstruction cenv

        | V.LocalAddress(n, _, _) ->
            I.Ldloca n |> emitInstruction cenv

        | V.Argument(n, _) ->
            I.Ldarg n |> emitInstruction cenv

        | V.ArgumentAddress(n, _, _) ->
            I.Ldarga n |> emitInstruction cenv

        | V.FunctionPtr(methInfo, _) ->
            emitInstruction cenv (I.Ldftn(methInfo.handle))

        | V.Function(irFunc, funcTy) ->
            let methInfo = irFunc.EmittedFunction

            I.Ldnull |> emitInstruction cenv
            emitInstruction cenv (I.Ldftn(methInfo.handle))

            let ctor = 
                if funcTy.IsTypeDefinition_t then
                    ClrCodeGen.createMulticastDelegateConstructor cenv.assembly funcTy
                else
                    ClrCodeGen.createAnonymousFunctionConstructor cenv.assembly funcTy.Handle
            I.Newobj(ctor, methInfo.Parameters.Length) |> emitInstruction cenv

        | V.DefaultStruct(ty) ->
            if ty.IsStruct then
                match ty.Handle with
                | ClrTypeHandle.NativePointer _
                | ClrTypeHandle.FunctionPointer _ ->
                    I.LdcI4(0) |> emitInstruction cenv
                    I.Conv_u |> emitInstruction cenv
                | _ ->
                    let localIndex = cenv.NewLocal(ty)
                    emitInstruction cenv (I.Ldloca(localIndex))
                    emitInstruction cenv (I.Initobj(ty.Handle))
                    emitInstruction cenv (I.Ldloc(localIndex))
            else
                OlyAssert.Fail("Expected struct type.")

    let canTailCall cenv (func: ClrMethodInfo) =
        cenv.emitTailCalls && 
      //  func.ReturnType.Handle <> cenv.assembly.TypeReferenceVoid && 
        not(func.Parameters |> ImArray.exists (fun (_, x) -> isByRefLike cenv.assembly x)) &&
        not(isByRefLike cenv.assembly func.ReturnType)

    let GenCall (cenv: cenv) env isReturnable (func: ClrMethodInfo) irArgs isVirtual =
        match func.specialKind with
        | ClrMethodSpecialKind.FunctionPointer ->
            raise(System.NotImplementedException("Clr FunctionPointer"))
        | _ ->

        irArgs 
        |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)

        match func.specialKind with
        | ClrMethodSpecialKind.None
        | ClrMethodSpecialKind.External ->
            if func.IsStatic || not isVirtual then
                if isReturnable && canTailCall cenv func then
                    I.Tail |> emitInstruction cenv
                I.Call(func.handle, irArgs.Length) |> emitInstruction cenv
            else
                let argTy0 = irArgs.[0].ResultType
                if isVirtual && func.IsInstance then
                    if argTy0.IsByRefOfTypeVariable || (argTy0.IsByRefOfStruct && (not func.enclosingTyHandle.IsValueType || (func.enclosingTyHandle = cenv.assembly.TypeReferenceValueType))) then
                        match argTy0.TryByRefElementType with
                        | ValueSome(elementTy) ->
                            I.Constrained(elementTy.Handle) |> emitInstruction cenv
                        | _ ->
                            failwith "Expected by-ref element type."
                I.Callvirt(func.handle, irArgs.Length) |> emitInstruction cenv

        | ClrMethodSpecialKind.TypeOf ->
            I.Ldtoken func.tyInst.[0].Handle |> emitInstruction cenv
            I.Call(cenv.assembly.GetTypeFromHandleMethod.Value, 1) |> emitInstruction cenv

        | ClrMethodSpecialKind.SizeOf ->
            I.Sizeof func.tyInst.[0].Handle |> emitInstruction cenv

        | ClrMethodSpecialKind.CreateDelegate when irArgs.Length = 2 && func.tyInst.Length > 0 ->
            
            let name = "__oly_delegate_" + (cenv.newUniqueId()).ToString()
            let returnTy = 
                if func.tyInst.Length = 1 then
                    cenv.assembly.TypeReferenceVoid
                else
                    func.tyInst[0].Handle

            // tuple type representing variadic type arguments
            // TODO: add some sort of verification for this.
            // we skip 1 as it is the instance type
            let parTys =
                let tyArgs =
                    if func.tyInst.Length = 1 then
                        func.tyInst[0].TypeArguments
                    else
                        func.tyInst[1].TypeArguments
                tyArgs
                |> ImArray.skip 1
                |> ImArray.map (fun x -> ("", x))
            // TODO: We should cache this.
            let tyDef = ClrCodeGen.createMulticastDelegateTypeDefinition cenv.assembly name parTys returnTy
            let ctor = (tyDef.MethodDefinitionBuilders |> Seq.item 0).Handle
            I.Newobj(ctor, parTys.Length) |> emitInstruction cenv

        | _ ->
            failwith "Invalid special method."

    let GenNew cenv env (func: ClrMethodInfo) irArgs =
        irArgs |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)
        I.Newobj(func.handle, irArgs.Length) |> emitInstruction cenv

    let GenCallIndirect (cenv: cenv) env (irFunArg: E<ClrTypeInfo, _, _>) (irArgs: E<_, _, _> imarray) (runtimeArgTys: ClrTypeInfo imarray) (runtimeReturnTy: ClrTypeInfo) =
        GenArgumentExpression cenv env irFunArg

        let funcTy = irFunArg.ResultType
        match funcTy.Handle with
        | ClrTypeHandle.FunctionPointer(cc, parTys, returnTy) ->            
            let localIndex = cenv.NewLocal(irFunArg.ResultType)
            I.Stloc localIndex |> emitInstruction cenv
            irArgs |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)
            I.Ldloc localIndex |> emitInstruction cenv
            I.Calli(cc, parTys, returnTy) |> emitInstruction cenv
        | _ ->
            irArgs |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)

            let argTys =
                runtimeArgTys
                |> ImArray.map (fun x -> x.Handle)

            let invoke =
                if funcTy.IsTypeDefinition_t then
                    createMulticastDelegateInvoke cenv.assembly funcTy
                else
                    createAnonymousFunctionInvoke cenv.assembly funcTy.Handle funcTy.TypeArguments argTys runtimeReturnTy.Handle
            I.Callvirt(invoke, irArgs.Length) |> emitInstruction cenv

    // TODO: We could just do this in the front-end when optimizations are enabled.
    let rec MorphExpression (cenv: cenv) (expr: E<ClrTypeInfo, _, _>) =
        if cenv.IsDebuggable then expr
        else
            match expr with
            // Makes optimizations easier
            | And(And(argx1, argx2, _), arg2, resultTy) ->
                And argx1 (And argx2 arg2 resultTy) resultTy
                |> MorphExpression cenv

            // Makes optimizations easier
            | Or(Or(argx1, argx2, _), arg2, resultTy) ->
                Or argx1 (Or argx2 arg2 resultTy) resultTy
                |> MorphExpression cenv

            | _ ->
                expr

    let private isIntegral (cenv: cenv) (expr: E<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        let handle = expr.ResultType.Handle
        handle = cenv.assembly.TypeReferenceInt32 ||
        handle = cenv.assembly.TypeReferenceSByte ||
        handle = cenv.assembly.TypeReferenceInt16 ||
        handle = cenv.assembly.TypeReferenceInt64 ||
        handle = cenv.assembly.TypeReferenceByte ||
        handle = cenv.assembly.TypeReferenceUInt16 ||
        handle = cenv.assembly.TypeReferenceUInt32 ||
        handle = cenv.assembly.TypeReferenceUInt64

    let private isUnsigned (cenv: cenv) (expr: E<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        let handle = expr.ResultType.Handle
        handle = cenv.assembly.TypeReferenceByte ||
        handle = cenv.assembly.TypeReferenceUInt16 ||
        handle = cenv.assembly.TypeReferenceUInt32 ||
        handle = cenv.assembly.TypeReferenceUInt64

    let canEmitDebugNop cenv env =
        cenv.irTier.HasMinimalOptimizations

    let createSequencePointInstruction (textRange: inref<OlyIRDebugSourceTextRange>) =
        if not(String.IsNullOrWhiteSpace (textRange.Path.ToString())) then
            I.SequencePoint(textRange.Path.ToString(), textRange.StartLine + 1, textRange.EndLine + 1, textRange.StartColumn + 1, textRange.EndColumn + 1)
        else
            I.HiddenSequencePoint

    let emitSequencePointIfPossible cenv (env: env) (textRange: inref<OlyIRDebugSourceTextRange>) =
        match env.spb with
        | EnableSequencePoint ->
            if not(String.IsNullOrWhiteSpace (textRange.Path.ToString())) then
                I.SequencePoint(textRange.Path.ToString(), textRange.StartLine + 1, textRange.EndLine + 1, textRange.StartColumn + 1, textRange.EndColumn + 1) |> emitInstruction cenv
                cenv.IncrementSequencePointCount()
            else
                I.HiddenSequencePoint |> emitInstruction cenv
                cenv.IncrementSequencePointCount()
        | _ ->
            ()

    let emitHiddenSequencePointIfPossible cenv (env: env) =
        match env.spb with
        | EnableSequencePoint ->
            I.HiddenSequencePoint |> emitInstruction cenv
            cenv.IncrementSequencePointCount()
        | _ ->
            ()

    let emitDebugNopIfPossible cenv env : unit =
        if canEmitDebugNop cenv env then
            match tryGetLastInstruction cenv with
            | ValueSome(I.Nop) -> ()
            | _ ->
                emitInstruction cenv I.Nop

    let emitDebugNopIfTypeVoid cenv env (ty: ClrTypeInfo) =
        if canEmitDebugNop cenv env then
            match tryGetLastInstruction cenv with
            | ValueSome(I.Nop)
            | ValueSome(I.HiddenSequencePoint)
            | ValueSome(I.SequencePoint _) -> ()
            | _ ->
                emitInstruction cenv I.Nop

    let GenConditionExpressionForFalseTarget (cenv: cenv) env falseTargetLabelId (conditionExpr: E<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        OlyAssert.False(env.isReturnable)

        if cenv.IsDebuggable then
            GenArgumentExpression cenv env conditionExpr
            I.Brfalse falseTargetLabelId |> emitInstruction cenv
        else
            match conditionExpr with
            | And(E.Operation(_, O.Equal _) as arg1, arg2, _) ->
                GenConditionExpressionForFalseTarget cenv env falseTargetLabelId arg1
                GenConditionExpressionForFalseTarget cenv env falseTargetLabelId arg2

            | E.Operation(_, O.NotEqual(arg1, arg2, _)) ->
                match arg1, arg2 with
                | _, Null
                | _, IntegralZero ->
                    GenArgumentExpression cenv env arg1
                    I.Brfalse falseTargetLabelId |> emitInstruction cenv

                | _ ->
                    GenArgumentExpression cenv env arg1
                    GenArgumentExpression cenv env arg2
                    I.Beq falseTargetLabelId |> emitInstruction cenv

            | E.Operation(_, O.Equal(arg1, arg2, _)) ->                        
                match arg1, arg2 with
                | _, Null
                | _, IntegralZero ->
                    GenArgumentExpression cenv env arg1
                    I.Brtrue falseTargetLabelId |> emitInstruction cenv

                | _ ->
                    GenArgumentExpression cenv env arg1
                    GenArgumentExpression cenv env arg2
                    I.Bne_un falseTargetLabelId |> emitInstruction cenv

            | _ ->
                GenArgumentExpression cenv env conditionExpr
                I.Brfalse falseTargetLabelId |> emitInstruction cenv       

    let GenConditionExpressionForTrueTarget (cenv: cenv) env trueTargetLabelId (conditionExpr: E<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        OlyAssert.False(env.isReturnable)

        if cenv.IsDebuggable then
            GenArgumentExpression cenv env conditionExpr
            I.Brtrue trueTargetLabelId |> emitInstruction cenv
        else
            match conditionExpr with
            | E.Operation(_, O.NotEqual(arg1, arg2, _)) ->
                match arg1, arg2 with
                | _, Null
                | _, IntegralZero ->
                    GenArgumentExpression cenv env arg1
                    I.Brtrue trueTargetLabelId |> emitInstruction cenv

                | _ ->
                    GenArgumentExpression cenv env arg1
                    GenArgumentExpression cenv env arg2
                    I.Bne_un trueTargetLabelId |> emitInstruction cenv

            | E.Operation(_, O.Equal(arg1, arg2, _)) ->                        
                match arg1, arg2 with
                | _, Null
                | _, IntegralZero ->
                    GenArgumentExpression cenv env arg1
                    I.Brfalse trueTargetLabelId |> emitInstruction cenv

                | _ ->
                    GenArgumentExpression cenv env arg1
                    GenArgumentExpression cenv env arg2
                    I.Beq trueTargetLabelId |> emitInstruction cenv

            | _ ->
                GenArgumentExpression cenv env conditionExpr
                I.Brtrue trueTargetLabelId |> emitInstruction cenv  

    let private GenConditionOrExpression cenv env continuationLabelIdOpt expr1 expr2 trueTargetLabelId falseTargetLabelId =
        GenConditionExpressionForTrueTarget cenv (setNotReturnable env) trueTargetLabelId expr1

        match expr2 with
        | Or(expr1, expr2, _) ->
            GenConditionOrExpression cenv env continuationLabelIdOpt expr1 expr2 trueTargetLabelId falseTargetLabelId
        | _ ->
            GenConditionExpressionForFalseTarget cenv (setNotReturnable env) falseTargetLabelId expr2

    let GenExpressionAux (cenv: cenv) env (irExpr: E<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        match irExpr with
        | E.None(textRange, resultTy) ->
            // We only want to emit actual nop and sequence points if the result type is void.
            if resultTy.Handle = cenv.assembly.TypeReferenceVoid then
                emitSequencePointIfPossible cenv (setEnableSequencePoint env) &textRange
                emitDebugNopIfPossible cenv env

        | E.Let(name, n, irRhsExpr, irBodyExpr) ->
            let hasNoDup = cenv.IsDebuggable || cenv.dups.Contains(n) |> not
            
            if hasNoDup then
                let rhsExprTy = irRhsExpr.ResultType
                cenv.locals.[n] <- (name, rhsExprTy)
            GenExpression cenv (setNotReturnable env) irRhsExpr
            if hasNoDup then
                I.Stloc n |> emitInstruction cenv
            else
                I.Dup |> emitInstruction cenv

            if cenv.IsDebuggable then
                let newDebugLocal = ClrDebugLocal(name, n)
                let debugLocals =
                    // Shadowing support
                    cenv.debugLocalsInScope.Values
                    |> Seq.map (fun debugLocal ->
                        if debugLocal.Name = name then
                            ClrDebugLocal(debugLocal.Name + " (shadowed)", debugLocal.Index)
                        else
                            debugLocal
                    )
                    |> Seq.append (seq { newDebugLocal })
                    |> ImArray.ofSeq
                cenv.debugLocalsInScope.Add(n, newDebugLocal)
                I.BeginLocalScope debugLocals |> emitInstruction cenv

            GenExpression cenv env irBodyExpr

            if cenv.IsDebuggable then
                cenv.debugLocalsInScope.Remove(n) |> ignore
                I.EndLocalScope |> emitInstruction cenv

        | E.Value(textRange, irValue) ->
            emitSequencePointIfPossible cenv env &textRange
            emitDebugNopIfPossible cenv env
            GenValue cenv env irValue

        | E.Operation(textRange, irOp) ->
            emitSequencePointIfPossible cenv env &textRange
            emitDebugNopIfPossible cenv env

            GenOperation cenv env irOp

            emitDebugNopIfTypeVoid cenv env irOp.ResultType

        | E.Sequential(irExpr1, irExpr2) ->
            GenExpression cenv (setNotReturnable env) irExpr1
            GenExpression cenv env irExpr2

        | E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy) ->
            let continuationLabelIdOpt =
                if not env.isReturnable then
                    cenv.NewLabel() |> ValueSome
                else
                    ValueNone

            // 'debugTmp' is only emitted in debugging and its purpose is to provide a better debugging experience if you have this code:
            //     let result =
            //         if (condition)
            //             1
            //         else
            //             2
            // Without using the 'debugTmp' and a hidden sequence point, 
            //     stepping through the code when breaked on '1' will result to breaking on '2' which is not right.
            let debugTmpOpt =
                if cenv.IsDebuggable && not env.isReturnable && (resultTy.Handle <> cenv.assembly.TypeReferenceVoid) then
                    cenv.NewLocal(resultTy) |> Some
                else
                    None

            let falseTargetLabelId = cenv.NewLabel()

            let conditionExpr = MorphExpression cenv conditionExpr
            match conditionExpr with
            | Or(expr1, expr2, _) ->
                let trueTargetLabelId = cenv.NewLabel()

                GenConditionOrExpression 
                    cenv 
                    env 
                    continuationLabelIdOpt 
                    expr1 
                    expr2 
                    trueTargetLabelId 
                    falseTargetLabelId

                I.Label trueTargetLabelId |> emitInstruction cenv
                GenExpression cenv env trueTargetExpr
            | _ ->
                GenConditionExpressionForFalseTarget cenv (setNotReturnable env) falseTargetLabelId conditionExpr          
                GenExpression cenv env trueTargetExpr

            match debugTmpOpt with
            | Some(debugTmp) ->
                I.Stloc debugTmp |> emitInstruction cenv
            | _ ->
                ()

            match continuationLabelIdOpt with
            | ValueSome(contLabelId) ->
                I.Br(contLabelId) |> emitInstruction cenv
            | _ ->
                ()

            I.Label falseTargetLabelId |> emitInstruction cenv
            GenExpression cenv env falseTargetExpr

            match debugTmpOpt with
            | Some(debugTmp) ->
                emitHiddenSequencePointIfPossible cenv env
                emitDebugNopIfPossible cenv env
                I.Stloc debugTmp |> emitInstruction cenv
            | _ ->
                ()

            match continuationLabelIdOpt with
            | ValueSome(contLabelId) ->
                I.Label(contLabelId) |> emitInstruction cenv
            | _ ->
                ()

            match debugTmpOpt with
            | Some(debugTmp) ->
                I.Ldloc debugTmp |> emitInstruction cenv
            | _ ->
                ()

        | E.While(conditionExpr, bodyExpr, _) ->
            let envLoop = 
                { env with 
                    isInWhileLoop = true
                    isReturnable = false }

            let loopStartLabelId = cenv.NewLabel()
            let loopEndLabelId = cenv.NewLabel()

            emitHiddenSequencePointIfPossible cenv env
            I.Label loopStartLabelId |> emitInstruction cenv

            GenArgumentExpression cenv envLoop conditionExpr
            I.Brfalse loopEndLabelId |> emitInstruction cenv

            GenExpression cenv envLoop bodyExpr
            I.Br loopStartLabelId |> emitInstruction cenv

            I.Label loopEndLabelId  |> emitInstruction cenv

        | E.Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy) ->
            let theVeryEndLabelId = cenv.NewLabel()

            let env = setNotReturnable env

            let tryStartLabelId = cenv.NewLabel()
            let tryEndLabelId = cenv.NewLabel()

            I.Label tryStartLabelId |> emitInstruction cenv

            GenExpression cenv env bodyExpr

            let resultLocalOpt =
                if (resultTy.Handle = cenv.assembly.TypeReferenceVoid) then
                    None
                else
                    cenv.NewLocal(resultTy)
                    |> Some

            resultLocalOpt
            |> Option.iter (fun returnLocal ->
                I.Stloc returnLocal |> emitInstruction cenv
            )

            I.Leave theVeryEndLabelId |> emitInstruction cenv
            I.Label tryEndLabelId |> emitInstruction cenv

            catchCases
            |> ImArray.iter (function
                | OlyIRCatchCase.CatchCase(localName, localIndex, bodyExpr, catchTy) ->
                    let handlerStartLabelId = cenv.NewLabel()
                    let handlerEndLabelId = cenv.NewLabel()

                    emitHiddenSequencePointIfPossible cenv env
                    I.Label handlerStartLabelId |> emitInstruction cenv

                    // This is sort of hacky, but it does make it easier to handle introducing locals
                    // for debugging purposes on try/catch/finally.
                    // At this point, the exception is on the stack and so using a Let expression will emit a store.
                    E.Let(localName, localIndex, E.None(NoRange, catchTy), bodyExpr)
                    |> GenExpression cenv env

                    resultLocalOpt
                    |> Option.iter (fun returnLocal ->
                        I.Stloc returnLocal |> emitInstruction cenv
                    )

                    I.Leave theVeryEndLabelId |> emitInstruction cenv
                    I.Label handlerEndLabelId |> emitInstruction cenv

                    I.CatchRegion(tryStartLabelId, tryEndLabelId, handlerStartLabelId, handlerEndLabelId, catchTy.Handle) |> emitInstruction cenv
            )

            finallyBodyExprOpt
            |> Option.iter (fun finallyBodyExpr ->
                let finallyEndLabelId =
                    if not catchCases.IsEmpty then
                        let catchCasesEndLabelId = cenv.NewLabel()
                        I.Label catchCasesEndLabelId |> emitInstruction cenv
                        catchCasesEndLabelId
                    else
                        tryEndLabelId

                let handlerStartLabelId = cenv.NewLabel()
                let handlerEndLabelId = cenv.NewLabel()

                I.Label handlerStartLabelId |> emitInstruction cenv

                GenExpression cenv env finallyBodyExpr

                I.Endfinally |> emitInstruction cenv
                I.Label handlerEndLabelId |> emitInstruction cenv

                I.FinallyRegion(tryStartLabelId, finallyEndLabelId, handlerStartLabelId, handlerEndLabelId) |> emitInstruction cenv
            )

            I.Label theVeryEndLabelId |> emitInstruction cenv
            resultLocalOpt
            |> Option.iter (fun returnLocal ->
                I.Ldloc returnLocal |> emitInstruction cenv
            )

    let GenExpression cenv env irExpr =
        GenExpressionAux cenv env irExpr
        if env.isReturnable then
            // If the last emitted instruction is a return, then we do not need to emit another one.
            if cenv.IsRetEmitted then ()
            else
                emitHiddenSequencePointIfPossible cenv env
                I.Ret |> emitInstruction cenv

let createMethod (enclosingTy: ClrTypeInfo) (flags: OlyIRFunctionFlags) methodName tyPars cilParameters (cilReturnTy: ClrTypeHandle) isStatic isCtor (tyDefBuilder: ClrTypeDefinitionBuilder) =
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

[<Sealed>]
type OlyRuntimeClrEmitter(assemblyName, isExe, primaryAssembly, consoleAssembly) =

    let asmBuilder = ClrAssemblyBuilder(assemblyName, isExe, primaryAssembly, consoleAssembly)

    let getEnclosingInfo (enclosing: Choice<string imarray, ClrTypeInfo>) =
        match enclosing with
        | Choice1Of2 path ->
            let namespac = path |> String.concat "."
            ClrTypeHandle.Empty, namespac
        | Choice2Of2(ty) ->
            ty.Handle, String.Empty

    let transformName (name: string) (tyParCount: int) =
        if tyParCount > 0 then
            name + "`" + tyParCount.ToString()
        else
            name

    let newUniqueId =
        let i = ref 0L
        fun () -> System.Threading.Interlocked.Increment i

    let newUniquePrivateTypeName () =
        "__oly_unique_" + string (newUniqueId ())

    let convertTyPars (tyPars: OlyIRTypeParameter<ClrTypeInfo> imarray) =
        tyPars
        |> ImArray.map (fun x -> 
            {
                Name = x.Name
                Constraints =
                    x.Constraints
                    |> ImArray.choose (function
                        | OlyIRConstraint.SubtypeOf(ty) ->
                            ClrTypeConstraint.SubtypeOf(ty.Handle)
                            |> Some
                        | _ ->
                            None
                    )
            } : ClrTypeParameter
        )

    let createMethodName 
            (externalInfoOpt: OlyIRFunctionExternalInfo option)
            (flags: OlyIRFunctionFlags) 
            (name: string) 
            (tyPars: OlyIRTypeParameter<ClrTypeInfo> imarray) 
            (originalOverridesOpt: ClrMethodInfo option)
            (pars: OlyIRParameter<ClrTypeInfo> imarray) 
            (returnTy: ClrTypeInfo) =

        // Exported functions will always use the name represented in the source.
        // TODO: How will we solve generic interface implementations?
        if flags.IsExported || flags.IsExternal then
            OlyAssert.False(flags.AreGenericsErased)
        
            if flags.IsExported && flags.SignatureUsesNewType then
                OlyAssert.Fail($"Method '{name}' cannot be exported as it uses newtypes in its signature.")

            match externalInfoOpt with
            | Some(externalInfo) ->
                if externalInfo.Platform = "C" then
                    name
                else
                    externalInfo.Name
            | _ ->
                name
        else
            if flags.SignatureUsesNewType || flags.AreGenericsErased then
                name + newUniquePrivateTypeName()
            else
                match originalOverridesOpt with
                | Some originalOverrides when originalOverrides.specialKind = ClrMethodSpecialKind.External ->
                    name
                | _ ->
                    // We do not need to check the type arguments for byref/inref because
                    // they are not legal in the CLR, therefore the generics should be erased.
                    let mutable readOnlyByRefCount = 0
                    pars
                    |> ImArray.iter (fun x ->
                        if x.Type.IsByRef && x.Type.IsReadOnly then
                            readOnlyByRefCount <- readOnlyByRefCount + 1
                    )
                    if returnTy.IsByRef && returnTy.IsReadOnly then
                        readOnlyByRefCount <- readOnlyByRefCount + 1
            
                    if readOnlyByRefCount > 0 then
                        name + "__oly_read_only_" + readOnlyByRefCount.ToString()
                    else
                        name

    let voidTy = ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceVoid, false, false)

    let addGenericInstanceTypeReference(tyHandle, tyArgs: ClrTypeHandle imarray) =
        let tyArgs = ClrCodeGen.handleTypeArguments asmBuilder tyArgs
        asmBuilder.AddGenericInstanceTypeReference(tyHandle, tyArgs)
    
    let createMulticastDelegateTypeDefinition (asmBuilder: ClrAssemblyBuilder) enclosingTyHandle invokeParTys invokeReturnTy =
        let name = "__oly_delegate_" + (newUniqueId().ToString())
        let tyDef = asmBuilder.CreateTypeDefinitionBuilder(enclosingTyHandle, "", name, 0, false)

        let parTys = ImArray.createTwo ("", asmBuilder.TypeReferenceObject) ("", asmBuilder.TypeReferenceIntPtr)
        let ctor = tyDef.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, parTys, asmBuilder.TypeReferenceVoid, true)
        ctor.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName
        ctor.ImplementationAttributes <- MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed

        let invoke = tyDef.CreateMethodDefinitionBuilder("Invoke", ImArray.empty, invokeParTys, invokeReturnTy, true)
        invoke.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual
        invoke.ImplementationAttributes <- MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed

        tyDef

    member this.Write(stream, pdbStream, isDebuggable) =
        asmBuilder.Write(stream, pdbStream, isDebuggable)

    interface IOlyRuntimeEmitter<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo> with

        member this.Initialize(vm) =
            asmBuilder.tr_InAttribute <- vm.TryFindType("System.Runtime.InteropServices.InAttribute") |> Option.map (fun x -> x.Handle)
            asmBuilder.tr_IsReadOnlyAttribute <- vm.TryFindType("System.Runtime.CompilerServices.IsReadOnlyAttribute") |> Option.map (fun x -> x.Handle)
            asmBuilder.tr_Span <- vm.TryFindType("System.Span", 1) |> Option.map (fun x -> x.Handle)
            asmBuilder.tr_ReadOnlySpan <- vm.TryFindType("System.ReadOnlySpan", 1) |> Option.map (fun x -> x.Handle)
            asmBuilder.tr_UnmanagedFunctionPointerAttribute <- vm.TryFindType("System.Runtime.InteropServices.UnmanagedFunctionPointerAttribute") |> Option.map (fun x -> x.Handle)
            asmBuilder.tr_CallingConvention <- vm.TryFindType("System.Runtime.InteropServices.CallingConvention") |> Option.map (fun x -> x.Handle)

        member this.EmitTypeArray(elementTy: ClrTypeInfo, rank, _): ClrTypeInfo =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddArrayType(elementTy.Handle, rank), false, false)  

        member this.EmitTypeNativeInt(): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceIntPtr, false, true)

        member this.EmitTypeNativePtr(elementTy: ClrTypeInfo): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddNativePointer(elementTy.Handle), false, true)

        member this.EmitTypeNativeFunctionPtr(ilCallConv, parTys, returnTy): ClrTypeInfo =
            let parTys =
                parTys
                |> ImArray.map (fun x -> x.Handle)
            let returnTy =
                returnTy.Handle

            let callConv =
                if ilCallConv.HasFlag(OlyILCallingConvention.Blittable) then
                    SignatureCallingConvention.Unmanaged
                else
                    SignatureCallingConvention.Default

            let callConv =
                if ilCallConv.HasFlag(OlyILCallingConvention.CDecl) then
                    callConv ||| SignatureCallingConvention.CDecl
                elif ilCallConv.HasFlag(OlyILCallingConvention.StdCall) then
                    callConv ||| SignatureCallingConvention.StdCall
                elif ilCallConv.HasFlag(OlyILCallingConvention.ThisCall) then
                    callConv ||| SignatureCallingConvention.ThisCall
                elif ilCallConv.HasFlag(OlyILCallingConvention.FastCall) then
                    callConv ||| SignatureCallingConvention.FastCall
                elif ilCallConv.HasFlag(OlyILCallingConvention.VarArgs) then
                    callConv ||| SignatureCallingConvention.VarArgs
                else
                    callConv

            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddFunctionPointer(callConv, parTys, returnTy), false, true)

        member this.EmitTypeNativeUInt(): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceUIntPtr, false, true)

        member this.EmitTypeByRef(ty, byRefKind) =
            match byRefKind with
            | OlyIRByRefKind.ReadWrite ->
                ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateByRef(ty.Handle), false, true)     
            | OlyIRByRefKind.Read ->
                let ty = 
                    match asmBuilder.tr_InAttribute with
                    | Some tr_InAttribute ->
                        ClrTypeHandle.ModReq(tr_InAttribute, ClrTypeHandle.CreateByRef(ty.Handle))
                    | _ ->
                        ClrTypeHandle.CreateByRef(ty.Handle)
                ClrTypeInfo.TypeReference(asmBuilder, ty, true, true)     

        member this.EmitTypeRefCell(ty) =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddArrayType(ty.Handle, 1), false, false)

        member _.EmitTypeVariable(index, irKind) =
            let kind =
                if irKind = OlyIRTypeVariableKind.Type then
                    ClrTypeVariableKind.Type
                else
                    ClrTypeVariableKind.Method
            ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateVariable(index, kind), false, false)

        member _.EmitTypeHigherVariable(_, _, _) =
            raise (System.NotSupportedException("CLR does not support higher kinds."))

        member this.EmitTypeBaseObject() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceObject, false, false)

        member this.EmitTypeVoid() =
            voidTy

        member this.EmitTypeUnit() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceObject, false, false)

        member this.EmitTypeInt8() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceSByte, true, true)

        member this.EmitTypeUInt8() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceByte, true, true)

        member this.EmitTypeInt16() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceInt16, true, true)

        member this.EmitTypeUInt16() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceUInt16, true, true)

        member this.EmitTypeInt32() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceInt32, true, true)

        member this.EmitTypeUInt32() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceUInt32, true, true)

        member this.EmitTypeInt64() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceInt64, true, true)

        member this.EmitTypeUInt64() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceUInt64, true, true)

        member this.EmitTypeFloat32() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceSingle, true, true)

        member this.EmitTypeFloat64() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceDouble, true, true)

        member this.EmitTypeBool() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceBoolean, true, true)

        member this.EmitTypeChar16() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceChar, true, true)

        member this.EmitTypeUtf16() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceString, true, false)

        member this.EmitTypeTuple(tyInst, _) =   
            let tyInst =
                tyInst 
                |> ImArray.map (fun x -> x.Handle)
                |> ClrCodeGen.handleTypeArguments asmBuilder
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddTupleType(tyInst), true, false)

        member this.EmitTypeConstantInt32 _ = raise (System.NotSupportedException())

        member this.EmitTypeFunction(inputTys, outputTy) =
            let mutable mustCreateDelegate = false
            let argTyHandles =
                inputTys
                |> ImArray.map (fun x -> 
                    if ClrCodeGen.isByRefLike asmBuilder x then
                        mustCreateDelegate <- true
                    x.Handle
                )

            if mustCreateDelegate then
                let name = "__oly_delegate_" + (newUniqueId()).ToString()
                let parTys =
                    argTyHandles
                    |> ImArray.map (fun x -> ("", x))
                let tyDef = ClrCodeGen.createMulticastDelegateTypeDefinition asmBuilder name parTys outputTy.Handle
                ClrTypeInfo.TypeDefinition(asmBuilder, tyDef, false, false, false, false, ClrTypeDefinitionInfo.Default)
            else
                let handle = ClrCodeGen.createAnonymousFunctionType asmBuilder argTyHandles outputTy.Handle
                ClrTypeInfo.TypeReference(asmBuilder, handle, true, false)

        member this.EmitExternalType(externalPlatform, externalPath, externalName, enclosing, kind, flags, _, tyParCount) =
            let isStruct = kind = OlyILEntityKind.Struct || kind = OlyILEntityKind.Enum
            let isReadOnly = flags.IsReadOnly

            let isNestedInExternalType =
                match enclosing with
                | Choice1Of2 _ -> false
                | Choice2Of2(enclosingTy) ->
                    match enclosingTy with
                    | ClrTypeInfo.TypeDefinition _ -> false
                    | ClrTypeInfo.TypeGenericInstance _ -> true
                    | _ -> true

            let enclosingTyHandle, namespac =
                if isNestedInExternalType then
                    match enclosing with
                    | Choice2Of2(enclosingTy) ->
                        enclosingTy.Handle, ""
                    | _ ->
                        failwith "Expected an enclosing type."
                else
                    let path = externalPath |> String.concat "."
                    let asmFullName = externalPlatform.Replace("CLR:", "")
                    let asmName = AssemblyName(asmFullName)
                    asmBuilder.AddAssemblyReference(asmName), path

            let enclosingTyHandle =
                match enclosingTyHandle with
                | ClrTypeHandle.TypeSpecification(tyRefHandle=tyRefHandle) -> tyRefHandle
                | _ -> enclosingTyHandle

            let tyHandle = asmBuilder.AddTypeReference(enclosingTyHandle, namespac, externalName, isStruct)

            let tyHandle =
                if tyParCount > 0 then
                    let tyArgs =
                        ImArray.init tyParCount (fun i -> ClrTypeHandle.CreateVariable(i, ClrTypeVariableKind.Type))
                    addGenericInstanceTypeReference(tyHandle, tyArgs)
                else
                    tyHandle

            ClrTypeInfo.TypeReference(asmBuilder, tyHandle, isReadOnly, isStruct)

        member this.EmitTypeGenericInstance(ty, tyArgs) =
            let tyArgHandles =
                tyArgs 
                |> ImArray.map (fun x -> x.Handle)
            let appliedTyHandle = addGenericInstanceTypeReference(ty.Handle, tyArgHandles)
            ClrTypeInfo.TypeGenericInstance(ty, tyArgHandles, appliedTyHandle)

        member this.EmitTypeDefinitionInfo(tyDef, enclosing, kind, flags, _, irTyPars, extends, implements, irAttrs, runtimeTyOpt) =
            let isStruct = kind = OlyILEntityKind.Struct
            let isEnum = kind = OlyILEntityKind.Enum
            let isNewtype = kind = OlyILEntityKind.Newtype
            let isReadOnly = flags.IsReadOnly 
            let isInterface = kind = OlyILEntityKind.Interface
            let isTypeExtension = kind = OlyILEntityKind.TypeExtension
            let isAbstract = flags.IsAbstract
            let isFinal = flags.IsFinal

            let enumRuntimeTyOpt =
                if isEnum then
                    runtimeTyOpt
                else
                    None

            tyDef.TypeDefinitionInfo.enumBaseTyOpt <- enumRuntimeTyOpt

            let isAnyStruct =
                isStruct ||
                (match enumRuntimeTyOpt with Some ty -> ty.IsStruct | _ -> false)

            let inherits =
                if isNewtype then ImArray.empty
                else 
                    // If structs or enums extend an object, just use the proper type to inherit.
                    // Struct -> System.ValueType
                    // Enum -> System.Enum
                    if isStruct && extends.Length = 1 && extends[0].Handle = asmBuilder.TypeReferenceObject then
                        ImArray.createOne (ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceValueType, false, false))
                    elif isEnum && extends.Length = 1 && extends[0].Handle = asmBuilder.TypeReferenceObject then
                        ImArray.createOne (ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceEnum, false, false))
                    else
                        extends

            let isExported = flags.IsExported

            let tyPars = convertTyPars irTyPars

            let tyDefBuilder = tyDef.TypeDefinitionBuilder

            tyDefBuilder.SetTypeParameters(tyPars)

            irAttrs
            |> ImArray.iter (fun x ->
                match x with
                | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                    asmBuilder.AddTypeAttribute(tyDefBuilder.Handle, ctor.handle, ClrCodeGen.writeAttributeArguments asmBuilder irArgs irNamedArgs)
            )

            let extends =
                if kind = OlyILEntityKind.Closure then
                    ImArray.empty
                else
                    inherits

            let layoutKind =
                irAttrs
                |> ImArray.tryPick (fun x ->
                    match x with
                    | OlyIRAttribute(ctor, irArgs, irNamedArgs) 
                            when irArgs.Length = 1 && 
                                 ctor.name = ".ctor" &&
                                 ctor.enclosingTyHandle.IsNamed &&
                                 ctor.enclosingTyHandle.FullyQualifiedName.StartsWith("System.Runtime.InteropServices.StructLayoutAttribute") ->
                        let irArg = irArgs[0]
                        match irArg with
                        | C.Int16(0s) -> Some System.Runtime.InteropServices.LayoutKind.Sequential
                        | C.Int32(x) ->
                            match x with
                            | 3 -> Some System.Runtime.InteropServices.LayoutKind.Auto
                            | 2 -> Some System.Runtime.InteropServices.LayoutKind.Explicit
                            | _ -> Some System.Runtime.InteropServices.LayoutKind.Sequential
                        | _ -> None
                    | _ ->
                        None
                )
                |> Option.defaultValue System.Runtime.InteropServices.LayoutKind.Sequential

            let tyAttrs = 
                if isAnyStruct then
                    let layout =
                        if isEnum then
                            TypeAttributes.AutoLayout
                        else
                            match layoutKind with
                            | System.Runtime.InteropServices.LayoutKind.Auto ->
                                TypeAttributes.AutoLayout
                            | System.Runtime.InteropServices.LayoutKind.Explicit ->
                                TypeAttributes.ExplicitLayout
                            | _ ->
                                TypeAttributes.SequentialLayout

                    layout ||| TypeAttributes.BeforeFieldInit
                elif isInterface then
                    tyDefBuilder.BaseType <- ClrTypeHandle.Empty
                    TypeAttributes.Interface
                else
                    TypeAttributes.Class ||| TypeAttributes.BeforeFieldInit

            let tyAttrs =
                if isAbstract then
                    tyAttrs ||| TypeAttributes.Abstract
                else
                    tyAttrs

            let tyAttrs =
                if isFinal then
                    tyAttrs ||| TypeAttributes.Sealed
                else
                    tyAttrs

            let tyAttrs =
                match enclosing with
                | Choice1Of2 _ ->
                    if isExported then
                        tyAttrs ||| TypeAttributes.Public
                    else
                        tyAttrs
                | _ ->
                    if flags.IsPublic then
                        tyAttrs ||| TypeAttributes.NestedPublic
                    else
                        // We always emit "internal" due to erasing generics on functions and types.
                        //     This means those functions and types must be able to be accessed.
                        tyAttrs ||| TypeAttributes.NestedAssembly

            tyDefBuilder.Attributes <- tyAttrs

            match enumRuntimeTyOpt with
            | Some enumBaseTy ->
                tyDefBuilder.AddFieldDefinition(
                    FieldAttributes.Public ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName,
                    "value__",
                    enumBaseTy.Handle,
                    None
                )
                |> ignore
            | _ ->
                ()

            if isInterface then
                if not extends.IsEmpty then
                    tyDefBuilder.InterfaceImplementations <- extends |> ImArray.map (fun x -> x.Handle)
            else
                if extends.Length > 1 && not isTypeExtension then
                    failwith "Multiple inheritance not supported on the CLR."

                if extends.Length = 1 && not isTypeExtension then
                    tyDefBuilder.BaseType <- extends.[0].Handle

                if not implements.IsEmpty && not isTypeExtension then
                    tyDefBuilder.InterfaceImplementations <- implements |> ImArray.map (fun x -> x.Handle)

            if isTypeExtension then
                let extendedTy = extends |> Seq.exactlyOne
                // ** INSTANCE GENERATION **

                let instanceTyDefBuilder = asmBuilder.CreateTypeDefinitionBuilder(tyDefBuilder.Handle, "", "__oly_instance", tyPars.Length, false)
                instanceTyDefBuilder.SetTypeParameters(tyPars)
                instanceTyDefBuilder.Attributes <- TypeAttributes.Class ||| TypeAttributes.NestedPublic
                if not implements.IsEmpty then
                    instanceTyDefBuilder.InterfaceImplementations <- implements |> ImArray.map (fun x -> x.Handle)

                let ctorParTyHandle =
                    if extendedTy.IsStruct then
                        ClrTypeHandle.CreateByRef(extendedTy.Handle)
                    else
                        extendedTy.Handle
                let methDefBuilder = instanceTyDefBuilder.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, ImArray.createOne("", ctorParTyHandle), asmBuilder.TypeReferenceVoid, true)
                let methAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

                let methAttrs = methAttrs ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName

                let instanceFieldHandle = instanceTyDefBuilder.AddFieldDefinition(FieldAttributes.Private, "contents", extendedTy.Handle, None)

                methDefBuilder.Attributes <- methAttrs
                methDefBuilder.ImplementationAttributes <- MethodImplAttributes.IL ||| MethodImplAttributes.Managed
                methDefBuilder.Locals <- ImArray.empty
                methDefBuilder.BodyInstructions <-
                    [
                        I.Ldarg 0
                        I.Ldarg 1
                        if extendedTy.IsStruct then
                            I.Ldobj(extendedTy.Handle)
                        I.Stfld instanceFieldHandle
                        I.Ret
                    ]
                    |> ImArray.ofSeq

                if extendedTy.IsStruct then
                    // Add second constructor if the extending type is a struct.
                    // The second constructor is the same as the first except it doesn't take a byref.
                    let methDefBuilder = instanceTyDefBuilder.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, ImArray.createOne("", extendedTy.Handle), asmBuilder.TypeReferenceVoid, true)
                    let methAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

                    let methAttrs = methAttrs ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName

                    methDefBuilder.Attributes <- methAttrs
                    methDefBuilder.ImplementationAttributes <- MethodImplAttributes.IL ||| MethodImplAttributes.Managed
                    methDefBuilder.Locals <- ImArray.empty
                    methDefBuilder.BodyInstructions <-
                        [
                            I.Ldarg 0
                            I.Ldarg 1
                            I.Stfld instanceFieldHandle
                            I.Ret
                        ]
                        |> ImArray.ofSeq

                let instanceTyInfo =
                    ClrTypeInfo.TypeDefinition(asmBuilder, instanceTyDefBuilder, isReadOnly, false, false, false, ClrTypeDefinitionInfo.Default)

                tyDef.TypeDefinitionInfo.typeExtensionInfo <- ValueSome(extendedTy, instanceTyInfo, instanceFieldHandle)

        member this.EmitTypeDefinition(enclosing, kind, flags, name, tyParCount) =
            let name =
                if flags.IsGenericsErased then
                    name + newUniquePrivateTypeName()
                else
                    name
            let name = transformName name tyParCount

            let isClosure = kind = OlyILEntityKind.Closure
            let isStruct = kind = OlyILEntityKind.Struct || (isClosure && flags.IsScoped)
            let isEnum = kind = OlyILEntityKind.Enum
            let isReadOnly = flags.IsReadOnly 
            let isInterface = kind = OlyILEntityKind.Interface

            let enclosingTyHandle, namespac = getEnclosingInfo enclosing

            let tyDefBuilder = 
                if isEnum then
                    asmBuilder.CreateEnumTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyParCount)
                else
                    asmBuilder.CreateTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyParCount, isStruct)

            ClrTypeInfo.TypeDefinition(asmBuilder, tyDefBuilder, isReadOnly, isInterface, isStruct, isClosure, ClrTypeDefinitionInfo.Default)

        member this.EmitField(enclosingTy, flags: OlyIRFieldFlags, name: string, fieldTy: ClrTypeInfo, irAttrs, irConstValueOpt): ClrFieldInfo = 
            let isStatic = flags.IsStatic
            let fieldTyHandle = fieldTy.Handle

            match enclosingTy with
            | ClrTypeInfo.TypeDefinition(_, tyDefBuilder, _, isInterface, _, _, _) ->
                let attrs = 
                    if flags.IsPublic then
                        FieldAttributes.Public
                    elif flags.IsProtected then
                        FieldAttributes.Family
                    else
                        if flags.IsInternal || (not flags.IsExported) then
                            // We always emit "internal" due to inlining.
                            // Exported functions will never be inlined by Oly.
                            FieldAttributes.Assembly
                        else    
                            FieldAttributes.Private
                let attrs =
                    if isStatic then
                        attrs ||| FieldAttributes.Static
                    else
                        attrs

                let attrs =
                    if flags.IsMutable || enclosingTy.IsDefinitionEnum then
                        attrs
                    else
                        attrs ||| FieldAttributes.InitOnly

                let attrs =
                    if irConstValueOpt.IsSome then
                        attrs ||| FieldAttributes.Literal ||| FieldAttributes.HasDefault
                    else
                        attrs

                let constValueOpt =
                    irConstValueOpt
                    |> Option.map (fun irConstValue ->
                        match irConstValue with
                        | C.Int8(value=value) -> value :> obj
                        | C.UInt8(value=value) -> value :> obj
                        | C.Int16(value=value) -> value :> obj
                        | C.UInt16(value=value) -> value :> obj
                        | C.Int32(value=value) -> value :> obj
                        | C.UInt32(value=value) -> value :> obj
                        | C.Int64(value=value) -> value :> obj
                        | C.UInt64(value=value) -> value :> obj
                        | C.Float32(value=value) -> value :> obj
                        | C.Float64(value=value) -> value :> obj
                        | C.Array(_, elements) -> elements |> Array.ofSeq :> obj
                        | C.True -> true :> obj
                        | C.False -> false :> obj
                        | C.Char16(value=value) -> value :> obj
                        | C.Utf16(value=value) -> value :> obj
                        | C.Variable _ ->
                            raise(System.NotSupportedException("constant variable"))
                        | C.External _ -> failwith "Invalid constant in CLR Emitter."
                    )

                let fieldHandle = tyDefBuilder.AddFieldDefinition(attrs, name, fieldTyHandle, constValueOpt)

                irAttrs
                |> ImArray.iter (fun x ->
                    match x with
                    | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                        asmBuilder.AddFieldAttribute(fieldHandle, ctor.handle, ClrCodeGen.writeAttributeArguments asmBuilder irArgs irNamedArgs)
                )

                {
                    handle = fieldHandle
                    isMutable = flags.IsMutable
                }

            | ClrTypeInfo.TypeReference(_, handle, _, _) ->
                let fieldHandle = asmBuilder.AddFieldReference(handle, name, fieldTyHandle)
                {
                    handle = fieldHandle
                    isMutable = flags.IsMutable
                }

            | _ ->
                failwith "Invalid type info."

        member this.EmitFieldInstance(enclosingTy, field) =
            let fieldHandle = asmBuilder.AddFieldReference(enclosingTy.Handle, field.Handle)
            {
                handle = fieldHandle
                isMutable = field.isMutable
            }

        member this.EmitExportedProperty(enclosingTy, name, ty, attrs, getterOpt, setterOpt) =
            match enclosingTy with
            | ClrTypeInfo.TypeDefinition(_, tyDefBuilder, _, _, _, _, _) ->
                let canEmitProperty, isInstance =
                    match getterOpt, setterOpt with
                    | Some getter, Some setter ->
                        getter.IsInstance = setter.IsInstance, getter.IsInstance
                    | None, None ->
                        false, false
                    | Some getter, None ->
                        true, getter.IsInstance
                    | None, Some setter ->
                        true, setter.IsInstance

                if canEmitProperty then
                    tyDefBuilder.CreatePropertyDefinitionBuilder(
                        name, 
                        ty.Handle, 
                        isInstance, 
                        getterOpt |> Option.map (fun x -> x.handle), 
                        setterOpt |> Option.map (fun x -> x.handle)
                    )
                    |> ignore
            | _ ->
                OlyAssert.Fail("Expected type definition.")

        member this.EmitFunctionInstance(enclosingTy, func: ClrMethodInfo, tyArgs: imarray<ClrTypeInfo>): ClrMethodInfo = 
            let newHandle =
                if enclosingTy.TypeArguments.IsEmpty && tyArgs.IsEmpty then
                    failwith "Expected type arguments."
                else
                    let tyArgHandles = tyArgs |> ImArray.map (fun x -> x.Handle)
                    if enclosingTy.Handle.EntityHandle.Equals(func.enclosingTyHandle.EntityHandle) then
                        asmBuilder.AddMethodSpecification(func.handle, tyArgHandles)
                    else
                        asmBuilder.CreateMethodSpecification(enclosingTy.Handle, func.handle, tyArgHandles)
            { func with handle = newHandle; tyInst = tyArgs }

        member this.EmitFunctionReference(enclosingTy, func: ClrMethodInfo): ClrMethodInfo =
            let newHandle =
                let parTys = 
                    if func.IsInstance && not func.isConstructor then
                        func.Parameters.RemoveAt(0) |> ImArray.map (fun x -> (snd x).Handle)
                    else
                        func.Parameters |> ImArray.map (fun x -> (snd x).Handle)
                        
                asmBuilder.AddMethodReference(
                    SignatureCallingConvention.Default, 
                    func.IsInstance, 
                    enclosingTy.Handle, 
                    func.name, 
                    func.tyParCount, 
                    parTys,
                    func.ReturnType.Handle
                )
            { func with handle = newHandle }

        member this.EmitFunctionDefinition(externalInfoOpt, enclosingTy, flags, name, tyPars, pars, returnTy, originalOverridesOpt, _, irAttrs) =
            let name =
                createMethodName
                    externalInfoOpt
                    flags
                    name
                    tyPars
                    originalOverridesOpt
                    pars
                    returnTy
            let isCtor = flags.IsConstructor
            let isTypeExtension =
                match enclosingTy with
                | ClrTypeInfo.TypeDefinition(info=info) -> info.typeExtensionInfo.IsSome
                | _ -> false
            let isStatic = flags.IsStatic || isTypeExtension
            let overridesOpt =
                if isStatic then
                    None
                else
                    originalOverridesOpt

            let pars =
                pars
                |> ImArray.map (fun x -> (x.Name, x.Type))

            let pars =
                if isTypeExtension && not flags.IsStatic then
                    let cilTy =
                        let cilTy = enclosingTy
                        match cilTy.TryTypeExtensionType with
                        | ValueSome(cilTy, _, _) -> 
                            if cilTy.IsStruct then
                                ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateByRef(cilTy.Handle), false, false)
                            else
                                cilTy
                        | _ -> cilTy
                    Seq.append
                        (seq { "this", cilTy })
                        pars
                    |> ImArray.ofSeq
                else
                    pars

            let parTys =
                pars
                |> ImArray.map (fun (_, x) -> x.Handle)

            let parHandles =
                pars
                |> ImArray.map (fun (name, x) -> 
                    (name, x.Handle)
                )

            let cilReturnTy = returnTy

            let methodName =
                if isCtor then
                    if flags.IsStatic then
                        ".cctor"
                    else
                        ".ctor"
                else
                    name

            let cilReturnTy2 =
                if isCtor then
                    voidTy
                else
                    cilReturnTy
                        
            let isInstance = not isStatic && not isTypeExtension        

            let tyPars = convertTyPars tyPars
                
            let methRefHandle, methDefBuilderOpt =
                match enclosingTy with
                | ClrTypeInfo.TypeGenericInstance(_, _, enclosingTyHandle)
                | ClrTypeInfo.TypeReference(_, enclosingTyHandle, _, _) ->
                    let convention = SignatureCallingConvention.Default
                    asmBuilder.AddMethodReference(convention, isInstance, enclosingTyHandle, methodName, tyPars.Length, parTys, cilReturnTy2.Handle), None
                | ClrTypeInfo.TypeDefinition(_, tyDefBuilder, _, _, _, _, info) ->
                    let tyExtInfoOpt = info.typeExtensionInfo
                    if flags.IsExternal && (externalInfoOpt.IsNone || externalInfoOpt.Value.Platform <> "C") then
                        ClrMethodHandle.None, None
                    else
                        let methDefBuilder = createMethod enclosingTy flags methodName tyPars parHandles cilReturnTy2.Handle isStatic isCtor tyDefBuilder
                        overridesOpt
                        |> Option.iter (fun x ->
                            methDefBuilder.Overrides <- Some x.handle
                        )

                        if isTypeExtension && not flags.IsStatic then
                            match tyExtInfoOpt with
                            | ValueSome(extendedTy, (ClrTypeInfo.TypeDefinition(_, instanceTyDefBuilder, _, _, _, _, _) as enclosingTy), instanceFieldHandle) ->
                                let instanceMethDefBuilder = createMethod enclosingTy flags methodName tyPars (parHandles.RemoveAt(0)) cilReturnTy2.Handle false false instanceTyDefBuilder
                                match originalOverridesOpt with
                                | Some overrides ->
                                   instanceMethDefBuilder.Overrides <- Some overrides.handle
                                | _ ->
                                    ()

                                if not flags.IsAbstract then
                                    instanceMethDefBuilder.BodyInstructions <-
                                        let ldargInstrs =
                                            Array.init 
                                                (pars.Length) 
                                                (fun i -> I.Ldarg(i))
                                            |> Array.collect (fun instr ->
                                                match instr with
                                                | I.Ldarg(0) ->
                                                    [|
                                                        instr
                                                        if extendedTy.IsStruct then
                                                            I.Ldflda(instanceFieldHandle)
                                                        else
                                                            I.Ldfld(instanceFieldHandle)
                                                    |]
                                                | _ ->
                                                    [|instr|]
                                            )

                                        Array.append
                                            ldargInstrs
                                            [|
                                                I.Call(methDefBuilder.Handle, ldargInstrs.Length)
                                                I.Ret
                                            |]
                                        |> ImArray.ofSeq
                            | _ ->
                                ()

                        if flags.IsInstance && flags.AreGenericsErased && enclosingTy.IsTypeDefinitionInterface then
                            // stub
                            methDefBuilder.BodyInstructions <-
                                [
                                    I.Ldnull
                                    I.Throw
                                    I.Ret
                                ]
                                |> ImArray.ofSeq

                        if externalInfoOpt.IsSome && externalInfoOpt.Value.Platform = "C" then
                            //methDefBuilder.PInvokeInfo <-
                            //    irAttrs
                            //    |> ImArray.tryPick (fun x ->
                            //        match x with
                            //        | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                            //            match ctor.name with
                            //            | ".ctor" ->
                            //                if ctor.enclosingTyHandle.FullyQualifiedName.StartsWith("System.Runtime.InteropServices.DllImportAttribute,") && irArgs.Length > 0 then
                            //                    match irArgs[0] with
                            //                    | OlyIRConstant.Utf16(value) ->
                            //                        Some value
                            //                    | _ ->
                            //                        None
                            //                else
                            //                    None
                            //            | _ ->
                            //                None                                    
                            //    )
                            methDefBuilder.PInvokeInfo <- Some (externalInfoOpt.Value.Path[0], externalInfoOpt.Value.Name)
                            methDefBuilder.ImplementationAttributes <- methDefBuilder.ImplementationAttributes ||| MethodImplAttributes.PreserveSig
                            methDefBuilder.Attributes <- methDefBuilder.Attributes ||| MethodAttributes.PinvokeImpl

                        methDefBuilder.Handle, Some methDefBuilder

            let pars =
                if isStatic || isCtor then 
                    //if isTypeExtension && not flags.IsStatic then
                    //    let enclosingTy =
                    //        match enclosingTy.TryTypeExtensionType with
                    //        | ValueSome(enclosingTy, _, _) -> enclosingTy
                    //        | _ -> enclosingTy
                    //    ImArray.createOne("this", enclosingTy).AddRange(pars)
                    //else
                        pars
                else
                    if enclosingTy.IsStruct then
                        // Perhaps, the OlyRuntime can give all parameters even for instance functions instead of us
                        // having to do this manually.
                        let thisTy = ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateByRef(enclosingTy.Handle), false, true)  
                        ImArray.createOne("this", thisTy).AddRange(pars)
                    else
                        ImArray.createOne("this", enclosingTy).AddRange(pars)

            match methDefBuilderOpt with
            | Some(methDefBuilder) ->
                let handle = methDefBuilder.Handle
                irAttrs
                |> ImArray.iter (fun x ->
                    match x with
                    | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                        asmBuilder.AddMethodAttribute(handle, ctor.handle, ClrCodeGen.writeAttributeArguments asmBuilder irArgs irNamedArgs)
                )

                if flags.IsEntryPoint then
                    asmBuilder.EntryPoint <- methDefBuilder.Handle
            | _ ->
                ()

            let specialKind =
                match externalInfoOpt with
                | Some externalInfo when externalInfo.Platform = "intrinsic-CLR" ->
                    match externalInfo.Name with
                    | "typeof" when pars.Length = 0 && tyPars.Length = 1 ->
                        ClrMethodSpecialKind.TypeOf
                    | "sizeof" when pars.Length = 0 && tyPars.Length = 1 ->
                        ClrMethodSpecialKind.SizeOf
                    | "__ldftn" ->
                        ClrMethodSpecialKind.FunctionPointer
                    | "CreateDelegate" ->
                        ClrMethodSpecialKind.CreateDelegate
                    | _ ->
                        failwith "Invalid CLR intrinsic"
                | Some _ ->
                    ClrMethodSpecialKind.External
                | _ ->
                    ClrMethodSpecialKind.None

            {
                isEnclosingClosure = enclosingTy.IsClosure
                enclosingTyHandle = enclosingTy.Handle
                handle = methRefHandle
                builder = methDefBuilderOpt
                name = methodName
                isStatic = isStatic
                isConstructor = isCtor
                returnTy = cilReturnTy // do not use cilReturnTy2 as it may be void due to constructor
                pars = pars
                tyInst = ImArray.empty
                specialKind = specialKind
                tyParCount = tyPars.Length
            }

        member this.EmitFunctionBody(irFuncBody, irTier, func) =
            let output = ImArray.builder()

            let bodyResult = 
#if DEBUG
                // We do this in Debug to prevent stack overflows due to lack of tail calls.
                // It would be a fair amount of work to do so otherwise since we rely on recursive patterns.
                async { 
                    do! Async.SwitchToThreadPool()
                    return irFuncBody.Value 
                }
                |> Async.RunSynchronously
#else
                irFuncBody.Value
#endif

            let expr = bodyResult.Expression

            let cenv = 
                {
                    assembly = asmBuilder
                    irTier = irTier
                    emitTailCalls = not irTier.HasMinimalOptimizations
                    buffer = output
                    locals = System.Collections.Generic.Dictionary()
                    dups = System.Collections.Generic.HashSet()
                    debugLocalsInScope = System.Collections.Generic.Dictionary()
                    newUniqueId = newUniqueId
                    localCount = ref bodyResult.LocalCount
                    nextLabelId = ref 0
                    seqPointCount = ref 0
                    retEmitted = ref false
                } : ClrCodeGen.cenv

            let env =
                {
                    isInWhileLoop = false
                    isReturnable = true
                    spb = ClrCodeGen.EnableSequencePoint
                } : ClrCodeGen.env

            ClrCodeGen.GenExpression cenv env expr

            match func.builder with
            | Some methDefBuilder ->
                methDefBuilder.Locals <-
                    cenv.locals
                    |> Seq.sortBy (fun x -> x.Key)
                    |> Seq.map (fun x -> 
                        let name, ty = x.Value
                        ClrLocal(name, ty.Handle))
                    |> ImArray.ofSeq
                methDefBuilder.BodyInstructions <- cenv.buffer.ToImmutable()

            | _ ->
                failwith "Expected method definition builder."

