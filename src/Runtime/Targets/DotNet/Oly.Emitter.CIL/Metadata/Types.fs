namespace rec Oly.Platform.Clr.Metadata

open System
open System.Reflection
open Oly.Core
open System.Reflection
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Collections.Immutable

[<AutoOpen>]
module internal Helpers =

    let inline internal getInlineCache (valueCache: byref<'T voption>) (f: unit -> 'T) =
        match valueCache with
        | ValueSome value -> value
        | _ ->
            let value = f ()
            valueCache <- ValueSome value
            value

(*************************************************************************************************)

type internal cenv =
    {
        reader: MetadataReader
    }

[<RequireQualifiedAccess>]
type ClrTypeVariableKind =
    | Type
    | Method

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type ClrTypeHandle =
    internal
    | None
    | AssemblyReference of AssemblyReferenceHandle * qualifiedName: string
    | TypeVariable of index: int32 * kind: ClrTypeVariableKind
    | Array of elementTyHandle: ClrTypeHandle * rank: int32
    | ByRef of ty: ClrTypeHandle
    | ModReq of modifier: ClrTypeHandle * ty: ClrTypeHandle
    | NativePointer of ty: ClrTypeHandle
    | FunctionPointer of cc: SignatureCallingConvention * parTys: ClrTypeHandle imarray * returnTy: ClrTypeHandle
    | TypeReference of handle: TypeReferenceHandle * isValueType: bool * qualifiedName: string
    | TypeSpecification of handle: TypeSpecificationHandle * isValueType: bool * tyRefHandle: ClrTypeHandle * tyInst: ClrTypeHandle imarray
    | LazyTypeDefinition of realHandle: Lazy<TypeDefinitionHandle> * isValueType: bool * fakeHandle: TypeDefinitionHandle * qualifiedName: string

    static member Empty = ClrTypeHandle.None

    static member CreateVariable(index: int32, kind) =
        TypeVariable(index, kind)

    static member CreateByRef(ty) =
        ByRef(ty)

    static member CreateArray(elementTy) =
        Array(elementTy)

    member this.StripModifiers() =
        match this with
        | ModReq(_, ty) -> ty
        | _ -> this

    member this.IsNamed =
        match this.StripModifiers() with
        | TypeReference _
        | TypeSpecification _
        | LazyTypeDefinition _ -> true
        | _ -> false

    member this.IsTypeDefinition =
        match this with
        | ClrTypeHandle.LazyTypeDefinition _ -> true
        | _ -> false

    member this.IsTypeSpecificationHandle =
        match this.StripModifiers() with
        | ClrTypeHandle.TypeSpecification _ -> true
        | _ -> false

    member this.IsValueType =
        match this.StripModifiers() with
        | None -> false
        | AssemblyReference _ 
        | Array _ 
        | TypeVariable _ 
        | ByRef _ -> false
        | TypeReference(isValueType=isValueType)
        | TypeSpecification(isValueType=isValueType)
        | LazyTypeDefinition(isValueType=isValueType) -> isValueType
        | FunctionPointer _ 
        | NativePointer _ -> true
        | ModReq _ -> false
        
    member this.TryElementType =
        match this.StripModifiers() with
        | Array(elementTyHandle, _)
        | ByRef elementTyHandle 
        | NativePointer elementTyHandle -> elementTyHandle |> ValueSome
        | _ -> ValueNone

    member this.HasEntityHandle =
        match this.StripModifiers() with
        | Array _
        | TypeVariable _ 
        | ByRef _ 
        | FunctionPointer _ 
        | NativePointer _ -> false
        | _ -> true

    member this.IsByRef_t =
        match this.StripModifiers() with
        | ByRef _ -> true
        | _ -> false

    member this.IsNativePointer_t =
        match this.StripModifiers() with
        | NativePointer _ -> true
        | _ -> false

    member this.IsVariable =
        match this.StripModifiers() with
        | TypeVariable _ -> true
        | _ -> false

    member this.TryTypeVariable =
        match this.StripModifiers() with
        | TypeVariable(index, kind) -> ValueSome(struct(index, kind))
        | _ -> ValueNone

    member this.FullyQualifiedName =
        match this.StripModifiers() with
        | TypeReference(qualifiedName=qualifiedName) ->
            qualifiedName
        | LazyTypeDefinition(qualifiedName=qualifiedName) ->
            qualifiedName
        | TypeSpecification(tyRefHandle=tyRefHandle) ->
            tyRefHandle.FullyQualifiedName
        | _ ->
            failwith "Expected a named type."

    member this.EntityHandle: EntityHandle =
        match this.StripModifiers() with
        | None -> EntityHandle()
        | AssemblyReference(handle, _) -> AssemblyReferenceHandle.op_Implicit handle
        | TypeReference(handle=handle) -> 
            TypeReferenceHandle.op_Implicit handle
        | TypeSpecification(handle=handle) ->
            TypeSpecificationHandle.op_Implicit handle
        | LazyTypeDefinition(fakeHandle=handle) -> 
            TypeDefinitionHandle.op_Implicit handle
        | Array _ ->
           failwith "Array does not have a handle."
        | TypeVariable _ ->
            failwith "Type variable does not have a handle."
        | ByRef _ ->
            failwith "ByReference does not have a handle."
        | NativePointer _ ->
            failwith "NativePointer does not have a handle."
        | FunctionPointer _ ->
            failwith "FunctionPointer does not have a handle."
        | ModReq _ ->
            failwith "ModReq does not have a handle."

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type ClrFieldHandle =
    internal
    | None
    | MemberReference of handle: MemberReferenceHandle * name: StringHandle * signature: BlobHandle
    | LazyFieldDefinition of realHandle: Lazy<FieldDefinitionHandle> * handle: Lazy<FieldDefinitionHandle> * name: StringHandle * signature: BlobHandle

    static member Empty = None

    member internal this.EntityHandle: EntityHandle =
        match this with
        | None -> failwith "Invalid handle."
        | MemberReference(handle=handle) ->
            MemberReferenceHandle.op_Implicit handle
        | LazyFieldDefinition(handle=handle) -> 
            FieldDefinitionHandle.op_Implicit handle.Value

    member internal this.NameHandle =
        match this with
        | None -> failwith "Invalid handle."
        | MemberReference(name=name)
        | LazyFieldDefinition(name=name) -> name

    member internal this.Signature =
        match this with
        | None -> failwith "Invalid handle."
        | MemberReference(signature=signature)
        | LazyFieldDefinition(signature=signature) -> signature

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type ClrMethodHandle =
    internal
    | None
    | MemberReference of handle: MemberReferenceHandle * name: StringHandle * signature: BlobHandle
    | MethodSpecification of handle: Lazy<MethodSpecificationHandle> * name: StringHandle * signature: BlobHandle
    | LazyMethodDefinition of realHandle: Lazy<MethodDefinitionHandle> * handle: Lazy<MethodDefinitionHandle> * name: StringHandle * signature: BlobHandle

    /// Only call this right before we add the entity handle to the SRM metadata.
    member internal this.UnsafeLazilyEvaluateEntityHandle() =
        match this with
        | None -> EntityHandle()
        | MemberReference(handle=handle) -> 
            MemberReferenceHandle.op_Implicit handle
        | MethodSpecification(handle=handle) ->
            MethodSpecificationHandle.op_Implicit handle.Value
        | LazyMethodDefinition(handle=handle) -> 
            MethodDefinitionHandle.op_Implicit handle.Value

    member internal this.Signature: BlobHandle =
        match this with
        | None -> BlobHandle()
        | MemberReference(signature=s) -> s 
        | MethodSpecification(signature=s) -> s
        | LazyMethodDefinition(signature=s) -> s 

    member internal this.Name: StringHandle =
        match this with
        | None -> StringHandle()
        | MemberReference(name=name) 
        | MethodSpecification(name=name)
        | LazyMethodDefinition(name=name) -> name 

[<Sealed>]
type ClrDebugLocal(name: string, index: int) =
    member _.Name = name
    member _.Index = index

[<NoComparison;ReferenceEquality;RequireQualifiedAccess>]
type ClrInstruction =
    | Nop
    | Pop
    | Ret
    | Leave of labelId: int32
    | Endfinally

    | Stloc of int32
    | Starg of int32

    | Ldloc of int32
    | Ldloca of int32
    | Ldarg of int32
    | Ldarga of int32
    | Ldind_i4
    | Ldind_i8
    | LdindRef

    | LdcI4 of int32
    | LdcI8 of int64
    | LdcR4 of float32
    | LdcR8 of float
    | Ldstr of string

    | Conv_i
    | Conv_i1
    | Conv_i2
    | Conv_i4
    | Conv_i8

    | Conv_u
    | Conv_u1
    | Conv_u2
    | Conv_u4
    | Conv_u8

    | Conv_r_un
    | Conv_r4
    | Conv_r8

    | Ldfld of ClrFieldHandle
    | Ldflda of ClrFieldHandle
    | Ldsfld of ClrFieldHandle
    | Ldsflda of ClrFieldHandle
    | Stfld of ClrFieldHandle
    | Stsfld of ClrFieldHandle

    | Stind_i4
    | Stind_i8
    | Stind_ref
    | Stobj of ClrTypeHandle

    | Add
    | Sub
    | Mul
    | Div
    | Div_un
    | Rem
    | Rem_un
    | Tail
    | Call of ClrMethodHandle * argCount: int
    | Calli of cc: SignatureCallingConvention * parTys: ClrTypeHandle imarray * returnTy: ClrTypeHandle
    | Callvirt of ClrMethodHandle * argCount: int
    | Newobj of ClrMethodHandle * argCount: int
    | Initobj of ClrTypeHandle
    | Ldtoken of ClrTypeHandle
    | Ldnull
    | Ldobj of ClrTypeHandle
    | Ldftn of ClrMethodHandle
    | Sizeof of ClrTypeHandle

    | Not
    | And
    | Or
    | Xor
    | Shl
    | Shr
    | Shr_un
    | Neg

    | Ceq
    | Cgt
    | Cgt_un
    | Clt
    | Clt_un

    | Box of ClrTypeHandle
    | Unbox of ClrTypeHandle
    | Unbox_any of ClrTypeHandle
    | Constrained of ClrTypeHandle

    | Ldelem of ClrTypeHandle
    | Ldelema of ClrTypeHandle
    | Stelem of ClrTypeHandle
    | Ldlen
    | Newarr of ClrTypeHandle

    | Throw

    | Dup

    // Branches

    /// Branch if equal
    | Beq of labelId: int32
    /// Branch if greater than or equal
    | Bge of labelId: int32
    /// Branch if greater than or equal (unsigned)
    | Bge_un of labelId: int32
    /// Branch if greater than
    | Bgt of labelId: int32
    /// Branch if greater than (unsigned)
    | Bgt_un of labelId: int32
    /// Branch if less than or equal
    | Ble of labelId: int32
    /// Branch if less than or equal (unsigned)
    | Ble_un of labelId: int32
    /// Branch if less than
    | Blt of labelId: int32
    /// Branch if less than (unsigned)
    | Blt_un of labelId: int32
    /// Branch if not equal
    | Bne_un of labelId: int32
    /// Branch if true
    | Brtrue of labelId: int32
    /// Branch if false
    | Brfalse of labelId: int32
    /// Branch
    | Br of labelId: int32
    /// Branch label marker, not a real instruction.
    | Label of labelId: int32

    // Not real instructions
    | CatchRegion of tryStartLabelId: int32 * tryEndLabelId: int32 * handlerStartLabelId: int32 * handlerEndLabelId: int32 * catchTy: ClrTypeHandle
    | FinallyRegion of tryStartLabelId: int32 * tryEndLabelId: int32 * handlerStartLabelId: int32 * handlerEndLabelId: int32

    // Debugger only - not real instructions.
    | SequencePoint of documentPath: string * startLine: int * endLine: int * startColumn: int * endColumn: int
    | HiddenSequencePoint
    | BeginLocalScope of ClrDebugLocal imarray
    | EndLocalScope

    | Skip

    member this.IsBranch =
        match this with
        | Beq _
        | Bge _
        | Bge_un _
        | Bgt _
        | Bgt_un _
        | Ble _
        | Ble_un _
        | Blt _
        | Ble_un _
        | Bne_un _
        | Brtrue _
        | Brfalse _
        | Br _ 
        | Leave _ ->
            true
        | _ ->
            false

    member this.LabelId =
        match this with
        | Beq labelId
        | Bge labelId
        | Bge_un labelId
        | Bgt labelId
        | Bgt_un labelId
        | Ble labelId
        | Ble_un labelId
        | Blt labelId
        | Ble_un labelId
        | Bne_un labelId
        | Brtrue labelId
        | Brfalse labelId
        | Br labelId
        | Leave labelId ->
            labelId
        | _ ->
            failwith "Instruction does not have a label."

[<RequireQualifiedAccess>]
module ClrElementTypes =

    [<Literal>]
    let End = 0x00uy

    [<Literal>]
    let Void = 0x01uy

    [<Literal>]
    let Boolean = 0x02uy

    [<Literal>]
    let Char = 0x03uy

    [<Literal>]
    let I1 = 0x04uy

    [<Literal>]
    let U1 = 0x05uy

    [<Literal>]
    let I2 = 0x06uy

    [<Literal>]
    let U2 = 0x07uy

    [<Literal>]
    let I4 = 0x08uy

    [<Literal>]
    let U4 = 0x09uy

    [<Literal>]
    let I8 = 0x0auy

    [<Literal>]
    let U8 = 0x0buy

    [<Literal>]
    let R4 = 0x0cuy

    [<Literal>]
    let R8 = 0x0duy

    [<Literal>]
    let String = 0x0euy

    /// Followed by type
    [<Literal>]
    let Ptr = 0x0fuy

    /// Followed by type
    [<Literal>]
    let Byref = 0x10uy

    /// Followed by TypeDef or TypeRef token
    [<Literal>]
    let ValueType = 0x11uy

    /// Followed by TypeDef or TypeRef token
    [<Literal>]
    let Class = 0x12uy

module ClrPatterns =

    type I = ClrInstruction