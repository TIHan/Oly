module Oly.Runtime.Clr.Emitter

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
open Oly.Platform.Clr.Metadata
open ClrPatterns

[<RequireQualifiedAccess>]
type ClrMethodSpecialKind =
    | None
    | External
    | TypeOf
    | SizeOf
    | IsSubtypeOf
    | FunctionPointer
    | CreateDelegate

[<RequireQualifiedAccess>]
type ClrTypeFlags =
    | None           = 0x00000
    | Struct         = 0x00001
    | ByRefLike      = 0x00011
    | ScopedFunction = 0x00111

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type ClrTypeDefinitionInfo =
    {
        mutable enumBaseTyOpt: ClrTypeInfo option
        mutable typeExtensionInfo: (ClrTypeInfo * ClrTypeInfo * ClrFieldHandle) voption
        mutable isFixedArray: bool
    }

    static member Create() =
        {
            enumBaseTyOpt = None
            typeExtensionInfo = ValueNone
            isFixedArray = false
        }

and [<RequireQualifiedAccess;NoComparison;NoEquality;System.Diagnostics.DebuggerDisplay("{FullyQualifiedName}")>] ClrTypeInfo = 
    | TypeGenericInstance of ClrTypeInfo * tyArgs: ClrTypeHandle imarray * appliedTyHandle: ClrTypeHandle
    | TypeReference of ClrTypeHandle * isReadOnly: bool * isStruct: bool
    | TypeDefinition of ClrTypeDefinitionBuilder * isReadOnly: bool * isInterface: bool * flags: ClrTypeFlags * isClosure: bool * info: ClrTypeDefinitionInfo
    | ByRef of ClrTypeInfo * kind: OlyIRByRefKind * appliedTyHandle: ClrTypeHandle
    | Shape // only for { new() } on constraints

    member this.IsFixedArray =
        match this with
        | TypeDefinition(info=info) -> info.isFixedArray
        | _ -> false

    member this.IsShape_t =
        match this with
        | Shape -> true
        | _ -> false

    member this.FullyQualifiedName =
        match this with
        | TypeDefinition(tyDefBuilder, _, _, _, _, _) ->
            tyDefBuilder.FullyQualifiedName
        | TypeReference(tyHandle, _, _) ->
            tyHandle.FullyQualifiedName
        | TypeGenericInstance(formalTy, _, _) ->
            formalTy.FullyQualifiedName
        | ByRef _
        | Shape ->
            failwith "Not a named typed."

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
        | ByRef(kind=kind) -> kind = OlyIRByRefKind.ReadOnly
        | Shape -> false

    member this.IsWriteOnly =
        match this with
        | ByRef(kind=kind) -> kind = OlyIRByRefKind.WriteOnly
        | _ -> false

    member this.IsStruct =
        match this with
        | TypeGenericInstance(x, _, _) -> x.IsStruct
        | TypeReference(isStruct=isStruct) -> isStruct
        | TypeDefinition(flags=flags;info=info) -> 
            if flags.HasFlag(ClrTypeFlags.Struct) then true
            else
                (match info.enumBaseTyOpt with Some ty -> ty.IsStruct | _ -> false)
        | ByRef _
        | Shape ->
            false

    member this.IsTypeVariable =
        match this with
        | TypeReference(handle, _, _) ->
            handle.IsVariable
        | _ ->
            false

    member this.IsByRefOfTypeVariable =
        match this with
        | ByRef(elementTy, _, _) ->
            elementTy.IsTypeVariable
        | _ ->
            false

    member this.IsByRefOfStruct =
        match this with
        | ByRef(elementTy, _, _) ->
            elementTy.IsStruct
        | _ ->
            false

    member this.IsByRef_t =
        match this with
        | ByRef _ ->
            true
        | _ ->
            false

    member this.IsByRefLike =
        match this with
        | ByRef _ -> true
        | TypeDefinition(flags=flags) -> flags.HasFlag(ClrTypeFlags.ByRefLike)
        | _ -> false

    member this.IsScopedFunction =
        match this with
        | TypeDefinition(flags=flags) -> flags.HasFlag(ClrTypeFlags.ScopedFunction)
        | _ -> false

    member this.TryByRefElementType =
        match this with
        | ByRef(elementTy, _, _) ->
            ValueSome(elementTy)
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
        | TypeReference(handle, _, _) ->
            handle.TryTypeVariable
        | _ ->
            ValueNone

    member this.IsClosure =
        match this with
        | TypeGenericInstance(x, _, _) -> x.IsClosure
        | TypeReference _ -> false
        | TypeDefinition(isClosure=isClosure) -> isClosure
        | ByRef _ 
        | Shape -> false

    member this.IsScopedClosure = this.IsClosure && this.IsStruct

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
        | TypeReference(handle, _, _) -> handle
        | TypeDefinition(tyDefBuilder, _, _, _, _, _) -> tyDefBuilder.Handle
        | ByRef(_, _, handle) -> handle
        | Shape -> failwith "Handle not valid"

    member this.TypeArguments: ClrTypeHandle imarray =
        match this with
        | TypeGenericInstance(tyArgs=tyArgs) -> tyArgs
        | TypeReference(ty, _, _) ->
            match ty with
            | ClrTypeHandle.TypeSpecification(tyInst=tyInst) -> tyInst
            | _ -> ImArray.empty
        | _ -> ImArray.empty

    member this.TryTypeExtensionType =
        match this with
        | TypeDefinition(_, _, _, _, _, info) -> info.typeExtensionInfo |> ValueOption.map (fun (x) -> x)
        | _ -> ValueNone

    member this.TypeDefinitionBuilder =
        match this with
        | TypeDefinition(tyDefBuilder, _, _, _, _, _) ->
            tyDefBuilder
        | _ ->
            OlyAssert.Fail("Not a type definition.")

    member this.TypeDefinitionInfo =
        match this with
        | TypeDefinition(_, _, _, _, _, info) ->
            info
        | _ ->
            OlyAssert.Fail("Not a type definition.")

    member this.MethodDefinitionBuilders: ClrMethodDefinitionBuilder seq =
        match this with
        | TypeDefinition(tyDefBuilder, _, _, _, _, _) ->
            tyDefBuilder.MethodDefinitionBuilders
        | _ ->
            ImArray.empty

[<ReferenceEquality;NoComparison>]
type ClrFieldInfo = 
    { 
        handle: ClrFieldHandle
        isMutable: bool } with

    member this.Handle: ClrFieldHandle = this.handle

and [<ReferenceEquality;NoComparison>] ClrMethodInfoDefinition =
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

and [<RequireQualifiedAccess;ReferenceEquality;NoComparison>] ClrMethodInfo =
    | Definition of ClrMethodInfoDefinition
    | DefaultConstructorConstraint

    member this.AsDefinition = 
        match this with
        | Definition d -> d
        | _ -> failwith "Not a method definition"

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

module rec ClrCodeGen =

    

    [<NoEquality;NoComparison>]
    type g =
        {
            ``Attribute``:    ClrTypeInfo
            ``Enum``:         ClrTypeInfo
            ``IsUnmanagedAttribute``: ClrTypeHandle
            ``UnmanagedType``: ClrTypeInfo
            ``ValueType``:    ClrTypeInfo
            ``ValueTuple``:   ClrTypeInfo
            ``ValueTuple`1``: ClrTypeInfo
            ``ValueTuple`2``: ClrTypeInfo
            ``ValueTuple`3``: ClrTypeInfo
            ``ValueTuple`4``: ClrTypeInfo
            ``ValueTuple`5``: ClrTypeInfo
            ``ValueTuple`6``: ClrTypeInfo
            ``ValueTuple`7``: ClrTypeInfo
            ``ValueTuple`8``: ClrTypeInfo

            ``ValueTuple`1_GetItemFields``: ClrFieldHandle imarray
            ``ValueTuple`2_GetItemFields``: ClrFieldHandle imarray
            ``ValueTuple`3_GetItemFields``: ClrFieldHandle imarray
            ``ValueTuple`4_GetItemFields``: ClrFieldHandle imarray
            ``ValueTuple`5_GetItemFields``: ClrFieldHandle imarray
            ``ValueTuple`6_GetItemFields``: ClrFieldHandle imarray
            ``ValueTuple`7_GetItemFields``: ClrFieldHandle imarray
            ``ValueTuple`8_GetItemFields``: ClrFieldHandle imarray

            ``Object_.ctor``: ClrMethodHandle

            ``DebuggerBrowsable.ctor``: ClrMethodHandle

            ``Activator_CreateInstance`1``: ClrMethodHandle

            ``ArgumentOutOfRangeExceptionCtor``: ClrMethodHandle

            ``InternalsVisibleToAttribute.ctor``: ClrMethodHandle

            // Caches
            ActivatorCreateInstanceCacheLock: obj
            ActivatorCreateInstanceCache: ConcurrentDictionary<ClrTypeHandle, ClrMethodHandle>
        }

        member this.GetActivatorCreateInstance(asm: ClrAssemblyBuilder, tyHandle: ClrTypeHandle) =
            match this.ActivatorCreateInstanceCache.TryGetValue(tyHandle) with
            | true, result -> result
            | _ ->
                lock this.ActivatorCreateInstanceCacheLock (fun () ->
                    match this.ActivatorCreateInstanceCache.TryGetValue(tyHandle) with
                    | true, result -> result
                    | _ ->
                        let result = asm.AddMethodSpecification(this.``Activator_CreateInstance`1``, ImArray.createOne tyHandle)
                        this.ActivatorCreateInstanceCache[tyHandle] <- result
                        result
                )

    [<NoEquality;NoComparison>]
    type cenv =
        {
            g: g
            assembly: ClrAssemblyBuilder
            emitTailCalls: bool
            buffer: imarrayb<I>
            locals: System.Collections.Generic.Dictionary<int, string * ClrTypeInfo * bool>
            dups: System.Collections.Generic.HashSet<int>
            irTier: OlyIRFunctionTier
            debugLocalsInScope: System.Collections.Generic.Dictionary<int, ClrDebugLocal>
            newUniqueId: unit -> int64
            mutable localCount: int ref
            mutable nextLabelId: int32 ref
            mutable seqPointCount: int ref
            mutable retEmitted: bool ref
        }

        member this.NewLocal(ty: ClrTypeInfo) =
            let localIndex = this.localCount.contents
            this.localCount.contents <- this.localCount.contents + 1
            this.locals.[localIndex] <- (String.Empty, ty, false)
            localIndex

        member this.NewLocalPinned(ty: ClrTypeInfo) =
            let localIndex = this.localCount.contents
            this.localCount.contents <- this.localCount.contents + 1
            this.locals.[localIndex] <- (String.Empty, ty, true)
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
        if x.IsByRefLike then
            true
        else
            let result =
                match asmBuilder.tr_Span with
                | Some(tr) ->
                    match x with
                    | ClrTypeInfo.TypeDefinition _ ->                
                        x.Handle = tr
                    | ClrTypeInfo.TypeReference(handle, _, _) ->
                        handle = tr
                    | ClrTypeInfo.TypeGenericInstance(info, _, _) ->
                        info.Handle = tr
                    | ClrTypeInfo.ByRef _
                    | ClrTypeInfo.Shape ->
                        false
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
                    | ClrTypeInfo.TypeReference(handle, _, _) ->
                        handle = tr
                    | ClrTypeInfo.TypeGenericInstance(info, _, _) ->
                        info.Handle = tr
                    | ClrTypeInfo.ByRef _
                    | ClrTypeInfo.Shape ->
                        false
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

    let createScopedFunctionTypeDefinition (g: g) (asmBuilder: ClrAssemblyBuilder) name =
        let tyDef = asmBuilder.CreateTypeDefinitionBuilder(ClrTypeHandle.Empty, "", name, 0, true, g.ValueType.Handle)

        tyDef.Attributes <- TypeAttributes.Sealed ||| TypeAttributes.SequentialLayout

        let parTys = ImArray.createTwo ("", asmBuilder.TypeReferenceIntPtr) ("", asmBuilder.TypeReferenceIntPtr)
        let ctor = tyDef.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, parTys, asmBuilder.TypeReferenceVoid, true)
        ctor.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName
        ctor.ImplementationAttributes <- MethodImplAttributes.Managed

        let thisptrField = tyDef.AddFieldDefinition(FieldAttributes.Public, "thisptr", asmBuilder.TypeReferenceIntPtr, None)
        let fnptrField = tyDef.AddFieldDefinition(FieldAttributes.Public, "fnptr", asmBuilder.TypeReferenceIntPtr, None)

        ctor.BodyInstructions <-
            [

                (I.Ldarg 0)
                (I.Ldarg 1)
                (I.Stfld thisptrField)

                (I.Ldarg 0)
                (I.Ldarg 2)
                (I.Stfld fnptrField)

                I.Ret
            ]
            |> ImArray.ofSeq

        tyDef

    let createMulticastDelegateTypeDefinition (asmBuilder: ClrAssemblyBuilder) name invokeParTys invokeReturnTy =

        let tyDef = asmBuilder.CreateTypeDefinitionBuilder(ClrTypeHandle.Empty, "", name, 0, false, asmBuilder.MulticastDelegate)

        tyDef.Attributes <- TypeAttributes.Sealed

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

            let attrTy = ClrTypeInfo.TypeReference(attr, false, false)
            let callConvTy = ClrTypeInfo.TypeReference(callConv, false, true)

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

    let createFixedArrayType g (asmBuilder: ClrAssemblyBuilder) name elementTyHandle length =
        OlyAssert.True(length >= 1)

        let tyDef = asmBuilder.CreateTypeDefinitionBuilder(ClrTypeHandle.Empty, "", name, 0, true, g.ValueType.Handle)
        tyDef.Attributes <- TypeAttributes.Sealed

        let fields =
            ImArray.init 
                length
                (fun i ->
                    tyDef.AddFieldDefinition(FieldAttributes.Public, "I" + i.ToString(), elementTyHandle, None)
                )

        // .ctor
        let ctorParTys = ImArray.init length (fun i -> ("i" + i.ToString(), elementTyHandle))
        let ctor = tyDef.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, ctorParTys, asmBuilder.TypeReferenceVoid, true)
        ctor.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig
        ctor.ImplementationAttributes <- MethodImplAttributes.IL ||| MethodImplAttributes.Managed

        let instrs = ImArray.builder()
        for i = 0 to length - 1 do
            instrs.Add(I.Ldarg 0)
            instrs.Add(I.Ldarg (i + 1))
            instrs.Add(I.Stfld fields[i])
        instrs.Add(I.Ret)
        ctor.BodyInstructions <- instrs.ToImmutable()

        // get_Item
        let getItemMethReturnTy = ClrTypeHandle.ByRef(elementTyHandle)
        let getItemMethParTys = ImArray.createOne ("index", asmBuilder.TypeReferenceInt32)
        let getItemMeth = tyDef.CreateMethodDefinitionBuilder("get_Item", ImArray.empty, getItemMethParTys, getItemMethReturnTy, true)

        getItemMeth.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig
        getItemMeth.ImplementationAttributes <- MethodImplAttributes.IL ||| MethodImplAttributes.Managed

        let mutable nextLabelId = 0
        let newLabelId() =
            let result = nextLabelId
            nextLabelId <- nextLabelId + 1
            result

        let instrs = ImArray.builder()
        for index = 0 to length - 1 do
            let labelId = newLabelId()
            instrs.Add(I.Ldarg 1)
            instrs.Add(I.LdcI4 index)
            instrs.Add(I.Ceq)
            instrs.Add(I.Brfalse labelId)
            instrs.Add(I.Ldarg 0)
            instrs.Add(I.Ldflda fields[index])
            instrs.Add(I.Ret)

            instrs.Add(I.Label labelId)

        instrs.Add(I.Newobj(g.``ArgumentOutOfRangeExceptionCtor``, 0))
        instrs.Add(I.Throw)
        instrs.Add(I.Ret)
        getItemMeth.BodyInstructions <- instrs.ToImmutable()

        tyDef

    let getFixedArrayConstructorHandle (fixedArrayTy: ClrTypeInfo) =
        (fixedArrayTy.TypeDefinitionBuilder.MethodDefinitionBuilders |> Seq.item 0).Handle

    let getFixedArrayGetItemMethodHandle (fixedArrayTy: ClrTypeInfo) =
        (fixedArrayTy.TypeDefinitionBuilder.MethodDefinitionBuilders |> Seq.item 1).Handle

    let handleTypeArgument (asmBuilder: ClrAssemblyBuilder) (tyArgHandle: ClrTypeHandle) =
        if tyArgHandle.IsNativePointer_t then
            asmBuilder.TypeReferenceIntPtr
        else
            tyArgHandle

    let handleTypeArguments (asmBuilder: ClrAssemblyBuilder) (tyArgHandles: ClrTypeHandle imarray) =
        tyArgHandles
        |> ImArray.map (handleTypeArgument asmBuilder)

    let private tryGetLastInstructionSkipLabelsAndSequencePoints cenv =
        let rec tryGet i =
            if i >= 0 && i < cenv.buffer.Count then
                let prev = cenv.buffer[i]
                match prev with
                | I.Label _
                | I.SequencePoint _
                | I.HiddenSequencePoint ->
                    tryGet (i - 1)
                | _ ->
                    ValueSome prev
            else
                ValueNone
        tryGet (cenv.buffer.Count - 1)

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
        | I.HiddenSequencePoint -> ()
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

    let createByRef (asmBuilder: ClrAssemblyBuilder) byRefKind ty =
        match byRefKind with
        | OlyIRByRefKind.ReadWrite ->
            ClrTypeInfo.ByRef(ty, OlyIRByRefKind.ReadWrite, ClrTypeHandle.CreateByRef(ty.Handle))    
        | OlyIRByRefKind.WriteOnly ->
            // TODO: Do we need to do something special with 'WriteOnly/outref'?
            ClrTypeInfo.ByRef(ty, OlyIRByRefKind.WriteOnly, ClrTypeHandle.CreateByRef(ty.Handle))     
        | OlyIRByRefKind.ReadOnly ->
            let handle = 
                match asmBuilder.tr_InAttribute with
                | Some tr_InAttribute ->
                    ClrTypeHandle.ModReq(tr_InAttribute, ClrTypeHandle.CreateByRef(ty.Handle))
                | _ ->
                    ClrTypeHandle.CreateByRef(ty.Handle)
            ClrTypeInfo.ByRef(ty, OlyIRByRefKind.ReadOnly, handle) 

    let addressOf cenv byRefKind (expr: E<ClrTypeInfo, _, _>) =
        let exprTy = expr.ResultType
        OlyAssert.False(exprTy.IsByRef_t)
        let byRefOfExprTy = createByRef cenv.assembly byRefKind exprTy

        let defaultCase() =
            let newLocalIndex = cenv.NewLocal(exprTy)
            let valueExpr = E.Value(NoRange, V.LocalAddress(newLocalIndex, byRefKind, byRefOfExprTy))
            E.Let("tmp", newLocalIndex, expr, valueExpr)

        match expr with
        | E.Value(textRange, value) ->
            match value with
            | V.Local(localIndex, _) ->
                E.Value(textRange, V.LocalAddress(localIndex, byRefKind, byRefOfExprTy))
            | V.Argument(argIndex, _) ->
                E.Value(textRange, V.ArgumentAddress(argIndex, byRefKind, byRefOfExprTy))
            | V.StaticField(field, _) ->
                E.Value(textRange, V.StaticFieldAddress(field, byRefKind, byRefOfExprTy))
            | _ ->
                defaultCase()

        | E.Operation(textRange, op) ->
            match op with
            | O.LoadField(field, receiverExpr, _) ->
                E.Operation(textRange, O.LoadFieldAddress(field, receiverExpr, byRefKind, byRefOfExprTy))
            | _ ->
                defaultCase()

        | _ ->
            defaultCase()

    let GenArgumentExpression (cenv: cenv) (env: env) expr =
        match expr with
        | E.Value _ -> 
            GenExpression cenv (setDisableSequencePoint env) expr
        | _ ->
            GenExpression cenv env expr

    let GenOperation (cenv: cenv) prevEnv (irOp: O<ClrTypeInfo, _, _>) =
        let env = { prevEnv with isReturnable = false }
        match irOp with
        | O.NewFixedArray(args=argExprs;resultTy=resultTy) ->
            if not resultTy.IsFixedArray then
                invalidOp "Invalid new fixed array op."
            argExprs
            |> ImArray.iter (GenArgumentExpression cenv env)
            I.Newobj(getFixedArrayConstructorHandle resultTy, argExprs.Length)
            |> emitInstruction cenv

        | O.LoadFunction(irFunc: OlyIRFunction<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>, receiverExpr, funcTy) ->
            OlyAssert.False(receiverExpr.ResultType.IsStruct)

            GenArgumentExpression cenv env receiverExpr

            // Converts a byref to a native int.
            if receiverExpr.ResultType.IsByRef_t then
                OlyAssert.True(receiverExpr.ResultType.TryByRefElementType.Value.IsStruct)
                I.Conv_u |> emitInstruction cenv

            emitInstruction cenv (I.Ldftn(irFunc.EmittedFunction.AsDefinition.handle))

            let ctor = 
                if funcTy.IsTypeDefinition_t then
                    ClrCodeGen.createMulticastDelegateConstructor cenv.assembly funcTy
                else
                    ClrCodeGen.createAnonymousFunctionConstructor cenv.assembly funcTy.Handle
            I.Newobj(ctor, irFunc.EmittedFunction.AsDefinition.Parameters.Length - 1) |> emitInstruction cenv

        | O.CallStaticConstructor _ ->
            // .NET already handles static constructor invocation.
            // No need to do it ourselves; just simply skip it.
            ()

        | O.LoadArrayLength(irReceiver, rank, resultTy) ->
            GenArgumentExpression cenv env irReceiver

            if rank > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                emitInstruction cenv I.Ldlen
                emitConv cenv (getPrimitiveTypeCode cenv resultTy)

        | O.LoadArrayElement(irReceiver, irIndexArgs, resultTy) ->
            GenArgumentExpression cenv env irReceiver
            match irReceiver.ResultType with
            | ClrTypeInfo.ByRef(fixedArrayTy, _, _) when fixedArrayTy.IsFixedArray ->
                if irIndexArgs.Length <> 1 then
                    invalidOp "Invalid number of arguments"
                irIndexArgs
                |> ImArray.iter (GenArgumentExpression cenv env)
                I.Call(getFixedArrayGetItemMethodHandle fixedArrayTy, irIndexArgs.Length) |> emitInstruction cenv
                I.Ldobj(resultTy.Handle) |> emitInstruction cenv
            | _ ->
                if irIndexArgs.Length > 1 then
                    failwith "clr emit rank greater than zero not yet supported."
                else
                    GenArgumentExpression cenv env irIndexArgs[0]
                    emitInstruction cenv (I.Ldelem resultTy.Handle)

        | O.LoadArrayElementAddress(irReceiver, irIndexArgs, _, resultTy) ->
            GenArgumentExpression cenv env irReceiver
            match irReceiver.ResultType with
            | ClrTypeInfo.ByRef(fixedArrayTy, _, _) when fixedArrayTy.IsFixedArray ->
                irIndexArgs
                |> ImArray.iter (GenArgumentExpression cenv env)
                match irIndexArgs.Length with
                | 1 -> I.LdcI4(1) |> emitInstruction cenv
                | 2 -> ()
                | _ -> invalidOp "Too many arguments"
                I.Call(getFixedArrayGetItemMethodHandle fixedArrayTy, 2) |> emitInstruction cenv
            | _ ->
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
                    | ClrTypeInfo.TypeDefinition(instanceTyDefBuilder, _, _, _, _, _) ->
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
            if isUnsigned cenv resultTy then
                I.Div_un |> emitInstruction cenv
            else
                I.Div |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Remainder(irArg1, irArg2, resultTy) ->
            GenArgumentExpression cenv env irArg1
            GenArgumentExpression cenv env irArg2
            if isUnsigned cenv resultTy then
                I.Rem_un |> emitInstruction cenv
            else
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
            | ValueSome elementTy ->
                // IMPORTANT: THIS IS VERY CRITICAL!!!
                //            We must create a temporary variable that stores
                //            the byref so we can pin it to prevent the GC
                //            from moving it around.
                //            This can happen as a RefCell is created when a mutable value is captured by a closure,
                //            and then later the address of the value is taken and used as an unsafe pointer.
                //
                //            This is conservative because we do it all the time.
                //            We technically only need to do it if we are passing
                //            the byref as an unsafe pointer.
                //
                //            There is an optimization opportunity where 
                //            we only need to pin this once in a method body if 'irArg' is a local.
                //            However, that may or may not be trivial.
                //
                //            REVIEW: Should this actually be illegal? 
                //                    Should we add analysis to the frontend? 
                //                    Should we add a special node like "PinAddress" to be used in OlyIL and let the front-end explicitly code-gen it?
                emitInstruction cenv (I.Ldelema elementTy.Handle)
                let tmpToPin = cenv.NewLocalPinned(resultTy)
                I.Stloc tmpToPin |> emitInstruction cenv
                I.Ldloc tmpToPin |> emitInstruction cenv
            | _ ->
                OlyAssert.Fail("Expected ByRef type.")

        | O.LoadTupleElement(irArg, index, _) ->
            let tupleTy = irArg.ResultType
            OlyAssert.False(tupleTy.IsByRef_t)

            let irArg =
                if irArg.ResultType.IsStruct then
                    addressOf cenv OlyIRByRefKind.ReadOnly irArg
                else
                    irArg

            GenArgumentExpression cenv env irArg

            let fieldHandle =
                match tupleTy with
                | ClrTypeInfo.TypeGenericInstance(formalTy, _, _) ->
                    let formalTyHandle = formalTy.Handle
                    let g = cenv.g

                    if formalTyHandle = g.``ValueTuple`1``.Handle then
                        g.``ValueTuple`1_GetItemFields``[index]
                    elif formalTyHandle = g.``ValueTuple`2``.Handle then
                        g.``ValueTuple`2_GetItemFields``[index]
                    elif formalTyHandle = g.``ValueTuple`3``.Handle then
                        g.``ValueTuple`3_GetItemFields``[index]
                    elif formalTyHandle = g.``ValueTuple`4``.Handle then
                        g.``ValueTuple`4_GetItemFields``[index]
                    elif formalTyHandle = g.``ValueTuple`5``.Handle then
                        g.``ValueTuple`5_GetItemFields``[index]
                    elif formalTyHandle = g.``ValueTuple`6``.Handle then
                        g.``ValueTuple`6_GetItemFields``[index]
                    elif formalTyHandle = g.``ValueTuple`7``.Handle then
                        g.``ValueTuple`7_GetItemFields``[index]
                    elif formalTyHandle = g.``ValueTuple`8``.Handle then
                        g.``ValueTuple`8_GetItemFields``[index]
                    else
                        failwith "Invalid LoadTupleItem"
                | _ ->
                    failwith "Invalid LoadTupleItem"

            I.Ldfld(cenv.assembly.AddFieldReference(tupleTy.Handle, fieldHandle)) |> emitInstruction cenv

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
                | ValueSome(elementTy) ->
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
            if irArg.ResultType.IsStruct then
                failwith "Expected an address."
            match tryGetLastInstructionSkipLabelsAndSequencePoints cenv with
            | ValueSome(I.Ldloc(i)) when irArg.ResultType.IsByRefOfStruct && not((let (_, x, _) = cenv.locals[i] in x).IsByRefOfStruct) ->
                failwith "Local address mis-matching."
            | _ ->
                ()
            I.Ldflda irField.EmittedField.Handle |> emitInstruction cenv

        | O.NewTuple(itemTys, irArgs, _) ->
            OlyAssert.Equal(itemTys.Length, irArgs.Length)

            irArgs |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)

            let rec emitNewTuple (itemTys: ClrTypeHandle imarray) isRest =
                if (itemTys.Length > 8) || (not isRest && itemTys.Length = 8) then
                    let take7 = itemTys |> ImArray.take 7
                    let skip7 = itemTys |> ImArray.skip 7
                    let skip7Handle = emitNewTuple skip7 false
                    emitNewTuple (take7.Add(skip7Handle)) true
                else
                    let tyHandle, methHandle = cenv.assembly.AddValueTupleConstructor(itemTys)
                    I.Newobj(methHandle, itemTys.Length) |> emitInstruction cenv
                    tyHandle

            emitNewTuple (itemTys |> ImArray.map (fun x -> x.Handle)) false
            |> ignore
            
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
            GenNew cenv env irFunc.EmittedFunction.AsDefinition irArgs

        | O.Call(irFunc, irArgs, _) ->
            GenCall cenv env prevEnv.isReturnable irFunc.EmittedFunction.AsDefinition irArgs false ValueNone

        | O.CallVirtual(irFunc, irArgs, _) ->
            GenCall cenv env prevEnv.isReturnable irFunc.EmittedFunction.AsDefinition irArgs true ValueNone

        | O.CallIndirect(runtimeArgTys, irFunArg, irArgs, runtimeResultTy) ->
            GenCallIndirect cenv env irFunArg irArgs runtimeArgTys runtimeResultTy

        | O.CallConstrained(constrainedTy, irFunc, irArgs, _) ->
            GenCall cenv env prevEnv.isReturnable irFunc.EmittedFunction.AsDefinition irArgs false (ValueSome constrainedTy)

        | O.NewOrDefaultOfTypeVariable(resultTy) ->
            OlyAssert.True(resultTy.IsTypeVariable)
            let methHandle = cenv.g.GetActivatorCreateInstance(cenv.assembly, resultTy.Handle)
            I.Call(methHandle, 0) |> emitInstruction cenv

    let GenValue (cenv: cenv) env (irValue: V<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        match irValue with
        | V.Unit ty ->
            let localIndex = cenv.NewLocal(ty)
            emitInstruction cenv (I.Ldloca(localIndex))
            emitInstruction cenv (I.Initobj(ty.Handle))
            emitInstruction cenv (I.Ldloc(localIndex))
        | V.Null _ -> I.Ldnull |> emitInstruction cenv
        | V.Constant(irConstant, _) ->
            match irConstant with
            | C.True -> I.LdcI4(1) |> emitInstruction cenv
            | C.False -> I.LdcI4(0) |> emitInstruction cenv
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
            emitInstruction cenv (I.Ldftn(methInfo.AsDefinition.handle))

        | V.Function(irFunc, funcTy) ->
            let methInfo = irFunc.EmittedFunction

            I.Ldnull |> emitInstruction cenv
            emitInstruction cenv (I.Ldftn(methInfo.AsDefinition.handle))

            let ctor = 
                if funcTy.IsTypeDefinition_t then
                    ClrCodeGen.createMulticastDelegateConstructor cenv.assembly funcTy
                else
                    ClrCodeGen.createAnonymousFunctionConstructor cenv.assembly funcTy.Handle
            I.Newobj(ctor, methInfo.AsDefinition.Parameters.Length) |> emitInstruction cenv

        | V.Default(ty) ->
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

    let canTailCall cenv (func: ClrMethodInfoDefinition) =
        cenv.emitTailCalls && 
      //  func.ReturnType.Handle <> cenv.assembly.TypeReferenceVoid && 
        not(func.Parameters |> ImArray.exists (fun (_, x) -> isByRefLike cenv.assembly x)) &&
        not(isByRefLike cenv.assembly func.ReturnType)

    let GenCall (cenv: cenv) env isReturnable (func: ClrMethodInfoDefinition) (irArgs: E<_, _, _> imarray) isVirtual (constrainedTyOpt: ClrTypeInfo voption) =
#if DEBUG || CHECKED
        if func.IsInstance && func.isConstructor then
            OlyAssert.Equal(func.pars.Length + 1, irArgs.Length)
        else
            OlyAssert.Equal(func.pars.Length, irArgs.Length)
#endif

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
                match constrainedTyOpt with
                | ValueSome(constrainedTy) ->
                    I.Constrained(constrainedTy.Handle) |> emitInstruction cenv
                | _ ->
                    if isReturnable && canTailCall cenv func then
                        I.Tail |> emitInstruction cenv

                I.Call(func.handle, irArgs.Length) |> emitInstruction cenv
            else
                let argTy0 = irArgs.[0].ResultType
                if isVirtual && func.IsInstance then
                    if argTy0.IsByRefOfTypeVariable || (argTy0.IsByRefOfStruct && (not func.enclosingTyHandle.IsValueType || (func.enclosingTyHandle = cenv.g.ValueType.Handle))) then
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

        | ClrMethodSpecialKind.IsSubtypeOf ->
            OlyAssert.Equal(func.tyParCount, func.tyInst.Length)
            OlyAssert.Equal(func.Parameters.Length, irArgs.Length)
            I.Isinst func.tyInst[0].Handle |> emitInstruction cenv
            I.Ldnull |> emitInstruction cenv
            I.Cgt_un |> emitInstruction cenv

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

    let GenNew cenv env (func: ClrMethodInfoDefinition) irArgs =
        irArgs |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)
        I.Newobj(func.handle, irArgs.Length) |> emitInstruction cenv

    let GenCallIndirect (cenv: cenv) env (thisArgExpr: E<ClrTypeInfo, _, _>) (argExprs: E<_, _, _> imarray) (parTys: ClrTypeInfo imarray) (returnTy: ClrTypeInfo) =

        let funcTy = thisArgExpr.ResultType
        let funcTy =
            match funcTy.TryByRefElementType with
            | ValueSome(elementTy) ->
                elementTy
            | _ ->
                funcTy

        GenArgumentExpression cenv env thisArgExpr

        match funcTy.Handle with
        | ClrTypeHandle.FunctionPointer(cc, parTys, returnTy) ->            
            let localIndex = cenv.NewLocal(thisArgExpr.ResultType)
            I.Stloc localIndex |> emitInstruction cenv
            argExprs |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)
            I.Ldloc localIndex |> emitInstruction cenv
            I.Calli(cc, parTys, returnTy) |> emitInstruction cenv
        | _ ->

            let localIndex =
                if funcTy.IsScopedFunction then
                    let localIndex = cenv.NewLocal(thisArgExpr.ResultType)
                    I.Stloc localIndex |> emitInstruction cenv
                    localIndex
                else
                    -1

            if funcTy.IsScopedFunction then
                I.Ldloc localIndex |> emitInstruction cenv
                I.Ldfld (funcTy.TypeDefinitionBuilder.GetFieldByIndex(0)) |> emitInstruction cenv

            argExprs |> ImArray.iter (fun x -> GenArgumentExpression cenv env x)

            let argTys =
                parTys
                |> ImArray.map (fun x -> x.Handle)

            if funcTy.IsScopedFunction then
                I.Ldloc localIndex |> emitInstruction cenv
                I.Ldfld (funcTy.TypeDefinitionBuilder.GetFieldByIndex(1)) |> emitInstruction cenv

                I.Calli(SignatureCallingConvention.Default, argTys |> ImArray.prependOne cenv.assembly.TypeReferenceIntPtr, returnTy.Handle)
                |> emitInstruction cenv
            else
                let invoke =
                    if funcTy.IsTypeDefinition_t then
                        createMulticastDelegateInvoke cenv.assembly funcTy
                    else
                        createAnonymousFunctionInvoke cenv.assembly funcTy.Handle funcTy.TypeArguments argTys returnTy.Handle

                I.Callvirt(invoke, argExprs.Length) |> emitInstruction cenv

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

    let private isUnsigned (cenv: cenv) (ty: ClrTypeInfo) =
        let handle = ty.Handle
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
            match tryGetLastInstructionSkipLabelsAndSequencePoints cenv with
            | ValueSome(I.Nop) -> ()
            | _ ->
                emitInstruction cenv I.Nop

    let emitDebugNopIfTypeVoid cenv env (ty: ClrTypeInfo) =
        if canEmitDebugNop cenv env then
            match tryGetLastInstructionSkipLabelsAndSequencePoints cenv with
            | ValueSome(I.Nop) -> ()
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
                cenv.locals.[n] <- (name, rhsExprTy, false)
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

        | E.Phi _ ->
            failwith "Phi not supported."

    let GenExpression cenv env irExpr =
        GenExpressionAux cenv env irExpr
        if env.isReturnable then
            // If the last emitted instruction is a return, then we do not need to emit another one.
            if cenv.IsRetEmitted then ()
            else
                match irExpr with
                | E.Let _
                | E.Sequential _
                | E.IfElse _ -> ()
                | _ ->
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

    let mutable g = Unchecked.defaultof<_>

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
        "__oly_unique" + string (newUniqueId ())

    let convertTyPars (g: ClrCodeGen.g) (tyPars: OlyIRTypeParameter<ClrTypeInfo> imarray) =
        tyPars
        |> ImArray.map (fun x -> 
            let mutable flags = GenericParameterAttributes.None
            let constrs =
                x.Constraints
                |> ImArray.choose (function
                    | OlyIRConstraint.SubtypeOf(ty) ->
                        if ty.IsShape_t then
                            flags <- flags ||| GenericParameterAttributes.DefaultConstructorConstraint
                            None
                        else
                            if g.ValueType.Handle = ty.Handle then
                                let isUnmanaged =
                                    x.Constraints
                                    |> ImArray.exists (fun x -> match x with OlyIRConstraint.Unmanaged -> true | _ -> false)
                                if isUnmanaged then
                                    ClrTypeConstraint.SubtypeOf(ClrTypeHandle.ModReq(g.UnmanagedType.Handle, ty.Handle))
                                    |> Some
                                else
                                    ClrTypeConstraint.SubtypeOf(ty.Handle)
                                    |> Some
                            else
                                ClrTypeConstraint.SubtypeOf(ty.Handle)
                                |> Some
                    | OlyIRConstraint.Struct ->
                        flags <- flags ||| GenericParameterAttributes.NotNullableValueTypeConstraint
                        None
                    | OlyIRConstraint.NotStruct ->
                        flags <- flags ||| GenericParameterAttributes.ReferenceTypeConstraint
                        None
                    | _ ->
                        None
                )
            {
                Name = x.Name
                Constraints = constrs
                Flags = flags
            } : ClrTypeParameter
        )

    let createMethodName 
            (externalInfoOpt: OlyIRFunctionExternalInfo option)
            (flags: OlyIRFunctionFlags) 
            (name: string) 
            (tyPars: OlyIRTypeParameter<ClrTypeInfo> imarray) 
            (originalOverridesOpt: ClrMethodInfoDefinition option)
            (pars: OlyIRParameter<ClrTypeInfo, ClrMethodInfo> imarray) 
            (returnTy: ClrTypeInfo) =

        // Exported functions will always use the name represented in the source.
        // TODO: How will we solve generic interface implementations?
        if flags.IsExported || flags.IsExternal then
          //  OlyAssert.False(flags.AreGenericsErased)
        
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
                    // We do not need to check the type arguments for byref/inref/outref because
                    // they are not legal in the CLR, therefore the generics should be erased.
                    // This is to make the method name unique.
                    let mutable readOnlyByRefCount = 0
                    let mutable writeOnlyByRefCount = 0
                    pars
                    |> ImArray.iter (fun x ->
                        if x.Type.IsByRef_t then
                            if x.Type.IsReadOnly then
                                readOnlyByRefCount <- readOnlyByRefCount + 1
                            elif x.Type.IsWriteOnly then
                                writeOnlyByRefCount <- writeOnlyByRefCount + 1
                    )
                    if returnTy.IsByRef_t then
                        if returnTy.IsReadOnly then
                            readOnlyByRefCount <- readOnlyByRefCount + 1
                        elif returnTy.IsWriteOnly then
                            writeOnlyByRefCount <- writeOnlyByRefCount + 1
            
                    if readOnlyByRefCount > 0 || writeOnlyByRefCount > 0 then
                        name + "__oly_by_ref_" + readOnlyByRefCount.ToString() + "_" + writeOnlyByRefCount.ToString()
                    else
                        name

    let voidTy = ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceVoid, false, false)

    let addGenericInstanceTypeReference(tyHandle, tyArgs: ClrTypeHandle imarray) =
        let tyArgs = ClrCodeGen.handleTypeArguments asmBuilder tyArgs
        asmBuilder.AddGenericInstanceTypeReference(tyHandle, tyArgs)
    
    let createMulticastDelegateTypeDefinition (asmBuilder: ClrAssemblyBuilder) enclosingTyHandle invokeParTys invokeReturnTy =
        let name = "__oly_delegate" + (newUniqueId().ToString())
        let tyDef = asmBuilder.CreateTypeDefinitionBuilder(enclosingTyHandle, "", name, 0, false, asmBuilder.TypeReferenceObject)

        let parTys = ImArray.createTwo ("", asmBuilder.TypeReferenceObject) ("", asmBuilder.TypeReferenceIntPtr)
        let ctor = tyDef.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, parTys, asmBuilder.TypeReferenceVoid, true)
        ctor.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName
        ctor.ImplementationAttributes <- MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed

        let invoke = tyDef.CreateMethodDefinitionBuilder("Invoke", ImArray.empty, invokeParTys, invokeReturnTy, true)
        invoke.Attributes <- MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual
        invoke.ImplementationAttributes <- MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed

        tyDef

    let lazyScopedFunc =
        lazy
            let name = "__oly_scoped_func"
            let tyDef = ClrCodeGen.createScopedFunctionTypeDefinition g asmBuilder name
            ClrTypeInfo.TypeDefinition(tyDef, false, false, ClrTypeFlags.ScopedFunction, false, ClrTypeDefinitionInfo.Create())
    
    let createTypeGenericInstance (formalTy: ClrTypeInfo) (tyArgs: ClrTypeInfo imarray) =
        let formalTyHandle = formalTy.Handle
        let tyArgHandles = 
            tyArgs 
            |> ImArray.map (fun x -> x.Handle)
            |> ClrCodeGen.handleTypeArguments asmBuilder
        ClrTypeInfo.TypeGenericInstance(
            formalTy, 
            tyArgHandles, 
            addGenericInstanceTypeReference(formalTyHandle, tyArgHandles)
        )

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

            asmBuilder.tr_IsByRefLikeAttribute <- vm.TryFindType("System.Runtime.CompilerServices.IsByRefLikeAttribute") |> Option.map (fun x -> x.Handle)

            match asmBuilder.tr_IsByRefLikeAttribute with
            | Some(isByRefLikeAttrHandle) ->
                asmBuilder.tr_IsByRefLikeAttributeConstructor <- asmBuilder.CreateConstructor(isByRefLikeAttrHandle) |> Some
            | _ ->
                failwith "System.Runtime.CompilerServices.IsByRefLikeAttribute not found."

            match vm.TryFindType("System.ValueTuple", 2) with
            | None -> failwith "System.ValueTuple not found."
            | Some(result) ->

            let ``Attribute`` = vm.TryFindType("System.Attribute").Value
            let ``Enum`` = vm.TryFindType("System.Enum").Value
            let ``UnmanagedType`` = vm.TryFindType("System.Runtime.InteropServices.UnmanagedType").Value
            let ``ValueType`` = vm.TryFindType("System.ValueType").Value
            let ``ValueTuple`` = vm.TryFindType("System.ValueTuple", 0).Value
            let ``ValueTuple`1`` = vm.TryFindType("System.ValueTuple", 1).Value
            let ``ValueTuple`2`` = result
            let ``ValueTuple`3`` = vm.TryFindType("System.ValueTuple", 3).Value
            let ``ValueTuple`4`` = vm.TryFindType("System.ValueTuple", 4).Value
            let ``ValueTuple`5`` = vm.TryFindType("System.ValueTuple", 5).Value
            let ``ValueTuple`6`` = vm.TryFindType("System.ValueTuple", 6).Value
            let ``ValueTuple`7`` = vm.TryFindType("System.ValueTuple", 7).Value
            let ``ValueTuple`8`` = vm.TryFindType("System.ValueTuple", 8).Value

            let createTuple_GetItemField count index =
                vm.TryFindField("System.ValueTuple", count, $"Item{index + 1}").Value.Handle

            let createTuple_GetItemFields count =
                if count = 8 then
                    let fields =
                        ImArray.init 7
                            (fun i ->
                                createTuple_GetItemField count i
                            )
                    fields.Add(vm.TryFindField("System.ValueTuple", count, $"Rest").Value.Handle)
                else
                    ImArray.init count
                        (fun i ->
                            createTuple_GetItemField count i
                        )

            let objectCtor =
                match vm.TryFindFunction(("System.Object", 0), ".ctor", 0, ImArray.empty, ("System.Void", 0), OlyFunctionKind.Instance) with
                | Some x -> x.AsDefinition.handle
                | _ -> failwith "Unable to find 'System.Object..ctor'"

            let debuggerBrowsableCtor =
                let pars = ImArray.createOne("System.Diagnostics.DebuggerBrowsableState", 0)
                match vm.TryFindFunction(("System.Diagnostics.DebuggerBrowsableAttribute", 0), ".ctor", 0, pars, ("System.Void", 0), OlyFunctionKind.Instance) with
                | Some x -> x.AsDefinition.handle
                | _ -> failwith "Unable to find 'System.Diagnostics.DebuggerBrowsableAttribute..ctor'"


            let ``IsUnmanagedAttribute`` =
                // TODO: This isn't finished.
                let tyDef = 
                    asmBuilder.CreateTypeDefinitionBuilder(
                        ClrTypeHandle.Empty, 
                        "System.Runtime.CompilerServices", 
                        "IsUnmanagedAttribute", 
                        0, 
                        false,
                        ``Attribute``.Handle
                    )
                tyDef.Attributes <- TypeAttributes.Sealed ||| TypeAttributes.AnsiClass ||| TypeAttributes.BeforeFieldInit
                tyDef.Handle

            let ``Activator_CreateInstance`1`` =
                vm.TryFindFunction(("System.Activator", 0), "CreateInstance", 1, 0, OlyFunctionKind.Static).Value.AsDefinition.handle

            let ``ArgumentOutOfRangeExceptionCtor`` =
                vm.TryFindFunction(("System.ArgumentOutOfRangeException", 0), ".ctor", 0, 0, OlyFunctionKind.Instance).Value.AsDefinition.handle

            let ``InternalsVisibleToAttribute.ctor`` =
                vm.TryFindFunction(("System.Runtime.CompilerServices.InternalsVisibleToAttribute", 0), ".ctor", 0, 1, OlyFunctionKind.Instance)
                    .Value
                    .AsDefinition
                    .handle

            // Adds an InternalsVisibleTo attribute to the assembly with the same name
            // as the assembly project.
            asmBuilder.AddAssemblyAttribute(
                ``InternalsVisibleToAttribute.ctor``,
                ClrCodeGen.writeAttributeArguments 
                    asmBuilder 
                    (ImArray.createOne(C.Utf16(assemblyName.Replace("__oly_internal", "")))) 
                    ImArray.empty
            )

            g <-
                {
                    ``Attribute`` = ``Attribute``
                    ``Enum`` = ``Enum``
                    ``IsUnmanagedAttribute`` = ``IsUnmanagedAttribute``
                    ``UnmanagedType`` = ``UnmanagedType``
                    ``ValueType`` = ``ValueType``
                    ``ValueTuple`` = ``ValueTuple``
                    ``ValueTuple`1`` = ``ValueTuple`1``
                    ``ValueTuple`2`` = ``ValueTuple`2``
                    ``ValueTuple`3`` = ``ValueTuple`3``
                    ``ValueTuple`4`` = ``ValueTuple`4``
                    ``ValueTuple`5`` = ``ValueTuple`5``
                    ``ValueTuple`6`` = ``ValueTuple`6``
                    ``ValueTuple`7`` = ``ValueTuple`7``
                    ``ValueTuple`8`` = ``ValueTuple`8``

                    ``ValueTuple`1_GetItemFields`` = createTuple_GetItemFields 1
                    ``ValueTuple`2_GetItemFields`` = createTuple_GetItemFields 2
                    ``ValueTuple`3_GetItemFields`` = createTuple_GetItemFields 3
                    ``ValueTuple`4_GetItemFields`` = createTuple_GetItemFields 4
                    ``ValueTuple`5_GetItemFields`` = createTuple_GetItemFields 5
                    ``ValueTuple`6_GetItemFields`` = createTuple_GetItemFields 6
                    ``ValueTuple`7_GetItemFields`` = createTuple_GetItemFields 7
                    ``ValueTuple`8_GetItemFields`` = createTuple_GetItemFields 8

                    ``Object_.ctor`` = objectCtor
                    ``DebuggerBrowsable.ctor`` = debuggerBrowsableCtor
                    ``Activator_CreateInstance`1`` = ``Activator_CreateInstance`1``

                    ``ArgumentOutOfRangeExceptionCtor`` = ``ArgumentOutOfRangeExceptionCtor``

                    ``InternalsVisibleToAttribute.ctor`` = ``InternalsVisibleToAttribute.ctor``

                    ActivatorCreateInstanceCacheLock = obj()
                    ActivatorCreateInstanceCache = ConcurrentDictionary()
                } : ClrCodeGen.g

        member this.EmitTypeArray(elementTy: ClrTypeInfo, rank, _kind): ClrTypeInfo =
            // Immutable and Mutable arrays are the exact same type.
            ClrTypeInfo.TypeReference(asmBuilder.AddArrayType(elementTy.Handle, rank), false, false)

        member this.EmitTypeFixedArray(elementTy: ClrTypeInfo, length: int, kind: OlyIRArrayKind): ClrTypeInfo = 
            if length <= 0 then
                raise(InvalidOperationException())

            let tyDefBuilder =
                match kind with
                | OlyIRArrayKind.Immutable ->
                    ClrCodeGen.createFixedArrayType g asmBuilder ("__oly_fixed_array" + newUniqueId().ToString()) elementTy.Handle length
                | OlyIRArrayKind.Mutable ->
                    ClrCodeGen.createFixedArrayType g asmBuilder ("__oly_mutable_fixed_array" + newUniqueId().ToString()) elementTy.Handle length

            let tyDefInfo = ClrTypeDefinitionInfo.Create()
            tyDefInfo.isFixedArray <- true
            ClrTypeInfo.TypeDefinition(tyDefBuilder, false, false, ClrTypeFlags.Struct, false, tyDefInfo)

        member this.EmitTypeNativeInt(): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceIntPtr, false, true)

        member this.EmitTypeNativePtr(elementTy: ClrTypeInfo): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder.AddNativePointer(elementTy.Handle), false, true)

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

            ClrTypeInfo.TypeReference(asmBuilder.AddFunctionPointer(callConv, parTys, returnTy), false, true)

        member this.EmitTypeNativeUInt(): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceUIntPtr, false, true)

        member this.EmitTypeByRef(ty, byRefKind) =
            ClrCodeGen.createByRef asmBuilder byRefKind ty    

        member this.EmitTypeRefCell(ty) =
            ClrTypeInfo.TypeReference(asmBuilder.AddArrayType(ty.Handle, 1), false, false)

        member _.EmitTypeVariable(index, irKind) =
            let kind =
                if irKind = OlyIRTypeVariableKind.Type then
                    ClrTypeVariableKind.Type
                else
                    ClrTypeVariableKind.Method
            ClrTypeInfo.TypeReference(ClrTypeHandle.CreateVariable(index, kind), false, false)

        member _.EmitTypeHigherVariable(_, _, _) =
            raise (System.NotSupportedException("CLR does not support higher kinds."))

        member this.EmitTypeBaseObject() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceObject, false, false)

        member this.EmitTypeVoid() =
            voidTy

        member this.EmitTypeUnit() =
            g.ValueTuple

        member this.EmitTypeInt8() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceSByte, true, true)

        member this.EmitTypeUInt8() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceByte, true, true)

        member this.EmitTypeInt16() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceInt16, true, true)

        member this.EmitTypeUInt16() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceUInt16, true, true)

        member this.EmitTypeInt32() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceInt32, true, true)

        member this.EmitTypeUInt32() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceUInt32, true, true)

        member this.EmitTypeInt64() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceInt64, true, true)

        member this.EmitTypeUInt64() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceUInt64, true, true)

        member this.EmitTypeFloat32() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceSingle, true, true)

        member this.EmitTypeFloat64() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceDouble, true, true)

        member this.EmitTypeBool() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceBoolean, true, true)

        member this.EmitTypeChar16() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceChar, true, true)

        member this.EmitTypeUtf16() =
            ClrTypeInfo.TypeReference(asmBuilder.TypeReferenceString, true, false)

        member this.EmitTypeTuple(itemTys, _) =
            OlyAssert.True(itemTys.Length > 1)

            // TODO: Should we cache these?
            let rec emitTypeTuple (itemTys: ClrTypeInfo imarray) =
                let itemTys =
                    if itemTys.Length >= 8 then
                        let take7 = itemTys |> ImArray.take 7
                        take7.Add(emitTypeTuple (itemTys |> ImArray.skip 7))
                    else
                        itemTys

                let formalTy =
                    match itemTys.Length with
                    | 1 -> g.``ValueTuple`1``
                    | 2 -> g.``ValueTuple`2``
                    | 3 -> g.``ValueTuple`3``
                    | 4 -> g.``ValueTuple`4``
                    | 5 -> g.``ValueTuple`5``
                    | 6 -> g.``ValueTuple`6``
                    | 7 -> g.``ValueTuple`7``
                    | 8 -> g.``ValueTuple`8``
                    | _ -> OlyAssert.Unreached()

                // TODO: Cache these as to avoid duplicates. Could we do anything in the VM?
                createTypeGenericInstance formalTy itemTys

            emitTypeTuple itemTys

        member this.EmitTypeConstantInt32 _ = raise (System.NotSupportedException())

        member this.EmitTypeFunction(inputTys, outputTy, kind) =
            if kind = OlyIRFunctionKind.Normal then
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
                    ClrTypeInfo.TypeDefinition(tyDef, false, false, ClrTypeFlags.None, false, ClrTypeDefinitionInfo.Create())
                else
                    let handle = ClrCodeGen.createAnonymousFunctionType asmBuilder argTyHandles outputTy.Handle
                    ClrTypeInfo.TypeReference(handle, true, false)
            else
                lazyScopedFunc.Value

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
            ClrTypeInfo.TypeReference(tyHandle, isReadOnly, isStruct)

        member this.EmitTypeGenericInstance(ty, tyArgs) =
            let tyArgHandles =
                tyArgs 
                |> ImArray.map (fun x -> x.Handle)
            let appliedTyHandle = addGenericInstanceTypeReference(ty.Handle, tyArgHandles)
            ClrTypeInfo.TypeGenericInstance(ty, tyArgHandles, appliedTyHandle)

        member this.EmitTypeDefinitionInfo(tyDef, enclosing, kind, flags, _, irTyPars, extends, implements, irAttrs, runtimeTyOpt) =
            if kind = OlyILEntityKind.Shape then
                ()
            else

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
                    // TODO: We should not have to do this.
                    if isStruct && extends.Length = 1 && extends[0].Handle = asmBuilder.TypeReferenceObject then
                        ImArray.createOne (ClrTypeInfo.TypeReference(g.ValueType.Handle, false, false))
                    elif isEnum && extends.Length = 1 && extends[0].Handle = asmBuilder.TypeReferenceObject then
                        ImArray.createOne (ClrTypeInfo.TypeReference(g.Enum.Handle, false, false))
                    else
                        extends

            let isExported = flags.IsExported

            let tyPars = convertTyPars g irTyPars

            let tyDefBuilder = tyDef.TypeDefinitionBuilder

            tyDefBuilder.SetTypeParameters(tyPars)

            irAttrs
            |> ImArray.iter (fun x ->
                match x with
                | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                    asmBuilder.AddTypeAttribute(tyDefBuilder.Handle, ctor.AsDefinition.handle, ClrCodeGen.writeAttributeArguments asmBuilder irArgs irNamedArgs)
            )

            if flags.IsScoped then
                match asmBuilder.tr_IsByRefLikeAttributeConstructor with
                | Some(isByRefLikeAttrCtorHandle) ->
                    asmBuilder.AddTypeAttribute(tyDefBuilder.Handle, isByRefLikeAttrCtorHandle, ClrCodeGen.writeAttributeArguments asmBuilder ImArray.empty ImArray.empty)
                | _ ->
                    ()

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
                                 ctor.AsDefinition.name = ".ctor" &&
                                 ctor.AsDefinition.enclosingTyHandle.IsNamed &&
                                 ctor.AsDefinition.enclosingTyHandle.FullyQualifiedName.StartsWith("System.Runtime.InteropServices.StructLayoutAttribute") ->
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

                let instanceTyDefBuilder = asmBuilder.CreateTypeDefinitionBuilder(tyDefBuilder.Handle, "", "__oly_instance", tyPars.Length, false, asmBuilder.TypeReferenceObject)
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
                        I.Call(g.``Object_.ctor``, 0)

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
                            I.Call(g.``Object_.ctor``, 0)

                            I.Ldarg 0
                            I.Ldarg 1
                            I.Stfld instanceFieldHandle

                            I.Ret
                        ]
                        |> ImArray.ofSeq

                let instanceTyInfo =
                    ClrTypeInfo.TypeDefinition(instanceTyDefBuilder, isReadOnly, false, ClrTypeFlags.None, false, ClrTypeDefinitionInfo.Create())

                tyDef.TypeDefinitionInfo.typeExtensionInfo <- ValueSome(extendedTy, instanceTyInfo, instanceFieldHandle)

        member this.EmitTypeDefinition(enclosing, ilKind, irTyFlags, name, tyParCount) =
            if ilKind = OlyILEntityKind.Shape then
                ClrTypeInfo.Shape
            else

            match enclosing with
            | Choice2Of2(tyInfo) ->
                match tyInfo.Handle with
                | ClrTypeHandle.TypeSpecification _ ->
                    failwith "Unexpected type specification."
                | _ ->
                    ()
            | _ ->
                ()

            let name =
                if irTyFlags.IsGenericsErased then
                    name + newUniquePrivateTypeName()
                else
                    name
            let name = transformName name tyParCount

            let isClosure = ilKind = OlyILEntityKind.Closure
            let isStruct = ilKind = OlyILEntityKind.Struct || (isClosure && irTyFlags.IsScoped)
            let isEnum = ilKind = OlyILEntityKind.Enum
            let isReadOnly = irTyFlags.IsReadOnly 
            let isInterface = ilKind = OlyILEntityKind.Interface

            let enclosingTyHandle, namespac = getEnclosingInfo enclosing

            let tyDefBuilder = 
                if isEnum then
                    asmBuilder.CreateEnumTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyParCount, g.Enum.Handle)
                elif isStruct then
                    asmBuilder.CreateTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyParCount, true, g.ValueType.Handle)
                elif isInterface then
                    asmBuilder.CreateTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyParCount, isStruct, ClrTypeHandle.Empty)
                else
                    asmBuilder.CreateTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyParCount, isStruct, asmBuilder.TypeReferenceObject)

            let tyFlags = 
                if isStruct then
                    ClrTypeFlags.Struct
                else
                    ClrTypeFlags.None

            let tyFlags =
                if irTyFlags.IsScoped then
                    tyFlags ||| ClrTypeFlags.ByRefLike
                else
                    tyFlags

            ClrTypeInfo.TypeDefinition(tyDefBuilder, isReadOnly, isInterface, tyFlags, isClosure, ClrTypeDefinitionInfo.Create())

        member this.OnTypeDefinitionEmitted(_) =
            ()

        member this.EmitFieldDefinition(enclosingTy, flags: OlyIRFieldFlags, name: string, fieldTy: ClrTypeInfo, _index: int32, irAttrs, irConstValueOpt): ClrFieldInfo =
            if not flags.IsStatic && enclosingTy.IsStruct && enclosingTy.Handle = fieldTy.Handle then
                OlyAssert.Fail($"Invalid struct {enclosingTy.FullyQualifiedName}")

            let isStatic = flags.IsStatic

            let fieldTy =
                // If the field type is a byref, then emit it as a native-pointer if IsByRefLike doesn't exist.
                match fieldTy.TryByRefElementType with
                | ValueSome elementTy ->
                    if asmBuilder.tr_IsByRefLikeAttributeConstructor.IsNone then
                        ClrTypeInfo.TypeReference(asmBuilder.AddNativePointer(elementTy.Handle), false, true)
                    elif fieldTy.IsReadOnly then
                        // IMPORTANT: We cannot emit a read-only byref as a field, so emit it as a normal byref.
                        ClrTypeInfo.ByRef(elementTy, OlyIRByRefKind.ReadWrite, ClrTypeHandle.CreateByRef(elementTy.Handle))
                    else
                        fieldTy
                | _ ->
                    fieldTy
            let fieldTyHandle = fieldTy.Handle

            match enclosingTy with
            | ClrTypeInfo.TypeDefinition(tyDefBuilder, _, isInterface, _, _, _) ->
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

                let attrs =
                    if enclosingTy.IsDefinitionEnum && not isStatic then
                        attrs ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName
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
                        asmBuilder.AddFieldAttribute(fieldHandle, ctor.AsDefinition.handle, ClrCodeGen.writeAttributeArguments asmBuilder irArgs irNamedArgs)
                )

                // REVIEW: Should we actually mark fields in OlyIL as not browsable?
                if name.StartsWith("@") then
                    let irArgs =
                        ImArray.createOne(OlyIRConstant.Int32(0))
                    asmBuilder.AddFieldAttribute(fieldHandle, g.``DebuggerBrowsable.ctor``, ClrCodeGen.writeAttributeArguments asmBuilder irArgs ImArray.empty)

                {
                    handle = fieldHandle
                    isMutable = flags.IsMutable
                }

            | ClrTypeInfo.TypeReference(handle, _, _) ->
                let fieldHandle = asmBuilder.AddFieldReference(handle, name, fieldTyHandle)
                {
                    handle = fieldHandle
                    isMutable = flags.IsMutable
                }

            | _ ->
                failwith "Invalid type info."

        member this.EmitFieldReference(enclosingTy, field) =
            let fieldHandle = asmBuilder.AddFieldReference(enclosingTy.Handle, field.Handle)
            {
                handle = fieldHandle
                isMutable = field.isMutable
            }

        member this.EmitProperty(enclosingTy, name, ty, attrs, getterOpt, setterOpt) =
            match enclosingTy with
            | ClrTypeInfo.TypeDefinition(tyDefBuilder, _, _, _, _, _) ->
                let canEmitProperty, isInstance =
                    match getterOpt, setterOpt with
                    | Some getter, Some setter ->
                        let getter = getter.AsDefinition
                        let setter = setter.AsDefinition
                        if (not getter.enclosingTyHandle.IsTypeDefinition) then
                            failwith "expected type definition"
                        if (not setter.enclosingTyHandle.IsTypeDefinition) then
                            failwith "expected type definition"
                        getter.IsInstance = setter.IsInstance, getter.IsInstance
                    | None, None ->
                        false, false
                    | Some getter, None ->
                        let getter = getter.AsDefinition
                        if (not getter.enclosingTyHandle.IsTypeDefinition) then
                            failwith "expected type definition"
                        true, getter.IsInstance
                    | None, Some setter ->
                        let setter = setter.AsDefinition
                        if (not setter.enclosingTyHandle.IsTypeDefinition) then
                            failwith "expected type definition"
                        true, setter.IsInstance

                if canEmitProperty then
                    let propDef =
                        tyDefBuilder.CreatePropertyDefinitionBuilder(
                            name, 
                            ty.Handle, 
                            isInstance, 
                            getterOpt |> Option.map (fun x -> x.AsDefinition.handle), 
                            setterOpt |> Option.map (fun x -> x.AsDefinition.handle)
                        )
                    attrs
                    |> ImArray.iter (fun x ->
                        match x with
                        | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                             asmBuilder.AddPropertyAttribute(propDef, ctor.AsDefinition.handle, ClrCodeGen.writeAttributeArguments asmBuilder irArgs irNamedArgs)
                    )
            | _ ->
                OlyAssert.Fail($"Expected type definition. {enclosingTy}")

        member this.EmitFunctionInstance(enclosingTy, func: ClrMethodInfo, tyArgs: imarray<ClrTypeInfo>): ClrMethodInfo = 
            let funcDef = func.AsDefinition
            let newHandle =
                if enclosingTy.TypeArguments.IsEmpty && tyArgs.IsEmpty then
                    failwith "Expected type arguments."
                else
                    let tyArgHandles = tyArgs |> ImArray.map (fun x -> x.Handle)
                    if enclosingTy.Handle.EntityHandle.Equals(funcDef.enclosingTyHandle.EntityHandle) then
                        asmBuilder.AddMethodSpecification(funcDef.handle, tyArgHandles)
                    else
                        asmBuilder.CreateMethodSpecification(enclosingTy.Handle, funcDef.handle, tyArgHandles)
            ClrMethodInfo.Definition({ funcDef with handle = newHandle; tyInst = tyArgs })

        member this.EmitFunctionReference(enclosingTy, func: ClrMethodInfo): ClrMethodInfo =
            let funcDef = func.AsDefinition
            let newHandle =
                let parTys = 
                    if funcDef.IsInstance && not funcDef.isConstructor then
                        funcDef.Parameters.RemoveAt(0) |> ImArray.map (fun x -> (snd x).Handle)
                    else
                        funcDef.Parameters |> ImArray.map (fun x -> (snd x).Handle)
                        
                asmBuilder.AddMethodReference(
                    SignatureCallingConvention.Default, 
                    funcDef.IsInstance, 
                    enclosingTy.Handle, 
                    funcDef.name, 
                    funcDef.tyParCount, 
                    parTys,
                    funcDef.ReturnType.Handle
                )
            ClrMethodInfo.Definition({ funcDef with handle = newHandle })

        member this.EmitFunctionDefinition(externalInfoOpt, enclosingTy, flags, name, tyPars, pars, returnTy, originalOverridesOpt, _, irAttrs) =
            match enclosingTy with
            | ClrTypeInfo.Shape ->
                ClrMethodInfo.DefaultConstructorConstraint
            | _ ->

            // Note on Scoped Closures:
            //      - For scoped closures, its Invoke function cannot actually be an instance function. Instead, force it to be a static function.
            let isCtor = flags.IsConstructor
            // Make sure not to include functions that were originally static.
            let isMorphedScopedClosureStatic = enclosingTy.IsScopedClosure && not isCtor && not flags.IsStatic

            let name =
                createMethodName
                    externalInfoOpt
                    flags
                    name
                    tyPars
                    (originalOverridesOpt |> Option.map (_.AsDefinition))
                    pars
                    returnTy
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
                                ClrTypeInfo.ByRef(cilTy, OlyIRByRefKind.ReadWrite, ClrTypeHandle.CreateByRef(cilTy.Handle))
                            else
                                cilTy
                        | _ -> cilTy
                    Seq.append
                        (seq { "this", cilTy })
                        pars
                    |> ImArray.ofSeq
                else if isMorphedScopedClosureStatic then
                    let cilTy =
                        let cilTy = enclosingTy
                        ClrTypeInfo.ByRef(cilTy, OlyIRByRefKind.ReadWrite, ClrTypeHandle.CreateByRef(cilTy.Handle))
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
            
            let isInstance =
                if isMorphedScopedClosureStatic then
                    false
                else
                    isInstance

            let isStatic =
                if isMorphedScopedClosureStatic then
                    true
                else
                    isStatic

            let tyPars = convertTyPars g tyPars
                
            let methRefHandle, methDefBuilderOpt =
                match enclosingTy with
                | ClrTypeInfo.ByRef _
                | ClrTypeInfo.Shape -> failwith "Invalid enclosing type."
                | ClrTypeInfo.TypeGenericInstance(_, _, enclosingTyHandle)
                | ClrTypeInfo.TypeReference(enclosingTyHandle, _, _) ->
                    let convention = SignatureCallingConvention.Default
                    asmBuilder.AddMethodReference(convention, isInstance, enclosingTyHandle, methodName, tyPars.Length, parTys, cilReturnTy2.Handle), None
                | ClrTypeInfo.TypeDefinition(tyDefBuilder, _, _, _, _, info) ->
                    let tyExtInfoOpt = info.typeExtensionInfo
                    if flags.IsExternal && (externalInfoOpt.IsNone || externalInfoOpt.Value.Platform <> "C") then
                        ClrMethodHandle.None, None
                    else
                        let methDefBuilder = createMethod enclosingTy flags methodName tyPars parHandles cilReturnTy2.Handle isStatic isCtor tyDefBuilder
                        overridesOpt
                        |> Option.iter (fun x ->
                            methDefBuilder.Overrides <- Some x.AsDefinition.handle
                        )

                        if isTypeExtension && not flags.IsStatic then
                            match tyExtInfoOpt with
                            | ValueSome(extendedTy, (ClrTypeInfo.TypeDefinition(instanceTyDefBuilder, _, _, _, _, _) as enclosingTy), instanceFieldHandle) ->
                                let instanceMethDefBuilder = createMethod enclosingTy flags methodName tyPars (parHandles.RemoveAt(0)) cilReturnTy2.Handle false false instanceTyDefBuilder
                                match originalOverridesOpt with
                                | Some overrides ->
                                   instanceMethDefBuilder.Overrides <- Some overrides.AsDefinition.handle
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
                    pars
                else
                    if enclosingTy.IsStruct then
                        // Perhaps, the OlyRuntime can give all parameters even for instance functions instead of us
                        // having to do this manually.
                        let thisTy = ClrTypeInfo.ByRef(enclosingTy, OlyIRByRefKind.ReadWrite, ClrTypeHandle.CreateByRef(enclosingTy.Handle))  
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
                        asmBuilder.AddMethodAttribute(handle, ctor.AsDefinition.handle, ClrCodeGen.writeAttributeArguments asmBuilder irArgs irNamedArgs)
                )

                if flags.IsEntryPoint then
                    asmBuilder.EntryPoint <- methDefBuilder.Handle
            | _ ->
                ()

            let specialKind =
                match externalInfoOpt with
                | Some externalInfo when externalInfo.Platform = "intrinsic-CLR" ->
                    match externalInfo.Name with
                    | "typeof" when isStatic && pars.Length = 0 && tyPars.Length = 1 ->
                        ClrMethodSpecialKind.TypeOf
                    | "sizeof" when isStatic && pars.Length = 0 && tyPars.Length = 1 ->
                        ClrMethodSpecialKind.SizeOf
                    | "is" when isStatic && pars.Length = 1 && tyPars.Length = 1 &&
                            ((pars[0] |> snd).Handle = asmBuilder.TypeReferenceObject) &&
                            returnTy.Handle = asmBuilder.TypeReferenceBoolean ->
                        ClrMethodSpecialKind.IsSubtypeOf
                    | "__ldftn" when isStatic ->
                        ClrMethodSpecialKind.FunctionPointer
                    | "CreateDelegate" when isStatic ->
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
            |> ClrMethodInfo.Definition

        member this.EmitFunctionBody(irFuncBody, irTier, func) =
            let output = ImArray.builder()

            let bodyResult = 
#if DEBUG || CHECKED
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
                    g = g
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

            match func.AsDefinition.builder with
            | Some methDefBuilder ->
                methDefBuilder.Locals <-
                    cenv.locals
                    |> Seq.sortBy (fun x -> x.Key)
                    |> Seq.map (fun x -> 
                        let name, ty, isPinned = x.Value
                        ClrLocal(name, ty.Handle, isPinned))
                    |> ImArray.ofSeq
                methDefBuilder.BodyInstructions <- cenv.buffer.ToImmutable()

            | _ ->
                failwith "Expected method definition builder."

