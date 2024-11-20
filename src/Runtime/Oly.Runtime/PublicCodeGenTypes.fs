namespace rec Oly.Runtime.CodeGen

open System
open Oly.Core
open Oly.Metadata

[<Flags>]
type OlyIRLocalFlags =
    | None                          = 0b00000000
    | Mutable                       = 0b00000001
                                       
    | ByRefType                     = 0b00000010
    | ReadOnlyByRefType             = 0b00000110
    | ReadWriteByRefType            = 0b00001010
    
    | AddressExposed                = 0b00010000

[<RequireQualifiedAccess>]
type OlyIRTypeVariableKind =
    | Type
    | Function

[<RequireQualifiedAccess;NoComparison>]
type OlyIRTypeModifier =
    | Int32 of name: string * value: int32

type internal IOlyIRTypeKey =

    abstract IsEqualTo: IOlyIRTypeKey -> bool

[<NoComparison;CustomEquality;RequireQualifiedAccess>]
type OlyIRFunctionSignatureKey =
    internal {
        Name: string
        TypeArguments: IOlyIRTypeKey imarray
        ParameterTypes: IOlyIRTypeKey imarray
        ReturnType: IOlyIRTypeKey
        IsStatic: bool
        IsConstructor: bool
    }

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? OlyIRFunctionSignatureKey as o ->
            this.Name = o.Name &&
            this.TypeArguments.Length = o.TypeArguments.Length &&
            this.ParameterTypes.Length = o.ParameterTypes.Length &&
            this.IsStatic = o.IsStatic &&
            this.IsConstructor = o.IsConstructor &&
            this.ReturnType.IsEqualTo(o.ReturnType) &&
            (this.ParameterTypes, o.ParameterTypes)
            ||> ImArray.forall2 (fun parTy1 parTy2 -> parTy1.IsEqualTo(parTy2)) &&
            (this.TypeArguments, o.TypeArguments)
            ||> ImArray.forall2 (fun tyArg1 tyArg2 -> tyArg1.IsEqualTo(tyArg2))
        | _ ->
            false

[<Struct>]
[<RequireQualifiedAccess>]
type OlyIRByRefKind =
    | ReadWrite
    | ReadOnly
    | WriteOnly

[<Struct>]
[<RequireQualifiedAccess>]
type OlyIRArrayKind =
    | Immutable
    | Mutable

[<Struct>]
[<RequireQualifiedAccess>]
type OlyIRFunctionKind =
    | Normal
    | Scoped

// TODO: Rename this to RuntimeTypeFlags?
type internal RuntimeTypeFlags =
    | None =           0x000000000
    | ReadOnly =       0x000000001
    | GenericsErased = 0x000000010
    | Exported       = 0x000000100

type internal RuntimeFunctionFlags =
    | None                 = 0b00000000
    | Exported             = 0b00000010
    | Inlineable           = 0b00000100
    | External             = 0b00001000
    | GenericsErased       = 0b00010000
    | SignatureUsesNewType = 0b00100000
    | EntryPoint           = 0b01000000

[<Sealed>]
type OlyIRFunctionExternalInfo internal (platform: string, path: string imarray, name: string) =
    
    member _.Platform = platform

    member _.Path = path

    member _.Name = name

[<Sealed>]
type OlyIRTypeFlags internal (ilEntFlags: OlyILEntityFlags, tyFlags: RuntimeTypeFlags) =

    member _.IsGenericsErased = tyFlags &&& RuntimeTypeFlags.GenericsErased = RuntimeTypeFlags.GenericsErased

    member _.IsReadOnly = tyFlags &&& RuntimeTypeFlags.ReadOnly = RuntimeTypeFlags.ReadOnly

    member _.IsExported = tyFlags &&& RuntimeTypeFlags.Exported = RuntimeTypeFlags.Exported

    member _.IsAbstract = ilEntFlags &&& OlyILEntityFlags.Abstract = OlyILEntityFlags.Abstract

    member _.IsFinal = ilEntFlags &&& OlyILEntityFlags.Final = OlyILEntityFlags.Final

    member _.IsPublic = ilEntFlags &&& OlyILEntityFlags.AccessorMask = OlyILEntityFlags.Public

    member _.IsInternal = ilEntFlags &&& OlyILEntityFlags.AccessorMask = OlyILEntityFlags.Internal

    member _.IsPrivate = ilEntFlags &&& OlyILEntityFlags.AccessorMask = OlyILEntityFlags.Private

    member _.IsScoped = ilEntFlags &&& OlyILEntityFlags.Scoped = OlyILEntityFlags.Scoped

[<Struct>]
type OlyIRFunctionFlags internal (ilFuncFlags: OlyILFunctionFlags, ilMemberFlags: OlyILMemberFlags, funcFlags: RuntimeFunctionFlags) =

    member internal _.SetGenericsErased() =
        OlyIRFunctionFlags(ilFuncFlags, ilMemberFlags, funcFlags ||| RuntimeFunctionFlags.GenericsErased)

    member internal _.SetStatic() =
        OlyIRFunctionFlags(ilFuncFlags, ilMemberFlags ||| OlyILMemberFlags.Static, funcFlags)

    member internal _.SetInlineable() =
        OlyIRFunctionFlags(ilFuncFlags, ilMemberFlags, funcFlags ||| RuntimeFunctionFlags.Inlineable)

    member internal _.SetSignatureUsesNewType() =
        OlyIRFunctionFlags(ilFuncFlags, ilMemberFlags, funcFlags ||| RuntimeFunctionFlags.SignatureUsesNewType)

    /// Function is able to be inlined in all situations.
    member _.IsInlineable = funcFlags &&& RuntimeFunctionFlags.Inlineable = RuntimeFunctionFlags.Inlineable

    member _.IsConstructor = ilFuncFlags &&& OlyILFunctionFlags.Constructor = OlyILFunctionFlags.Constructor

    member _.IsReadOnly = ilFuncFlags &&& OlyILFunctionFlags.Mutable <> OlyILFunctionFlags.Mutable

    member _.IsStatic = ilMemberFlags &&& OlyILMemberFlags.Static = OlyILMemberFlags.Static

    member this.IsInstance = not this.IsStatic

    member _.IsVirtual = ilMemberFlags &&& OlyILMemberFlags.Virtual = OlyILMemberFlags.Virtual

    member _.IsAbstract = ilMemberFlags &&& OlyILMemberFlags.Abstract = OlyILMemberFlags.Abstract

    member _.IsFinal = ilMemberFlags &&& OlyILMemberFlags.Final = OlyILMemberFlags.Final

    member _.IsNewSlot = ilMemberFlags &&& OlyILMemberFlags.NewSlot = OlyILMemberFlags.NewSlot

    member _.AreGenericsErased = funcFlags &&& RuntimeFunctionFlags.GenericsErased = RuntimeFunctionFlags.GenericsErased

    member _.SignatureUsesNewType = funcFlags &&& RuntimeFunctionFlags.SignatureUsesNewType = RuntimeFunctionFlags.SignatureUsesNewType

    member _.IsEntryPoint = funcFlags &&& RuntimeFunctionFlags.EntryPoint = RuntimeFunctionFlags.EntryPoint

    member _.IsExported = funcFlags &&& RuntimeFunctionFlags.Exported = RuntimeFunctionFlags.Exported

    member _.IsExternal = funcFlags &&& RuntimeFunctionFlags.External = RuntimeFunctionFlags.External

    member _.IsPublic = ilMemberFlags &&& OlyILMemberFlags.AccessorMask = OlyILMemberFlags.Public

    member _.IsPrivate = ilMemberFlags &&& OlyILMemberFlags.AccessorMask = OlyILMemberFlags.Private

    member _.IsInternal = ilMemberFlags &&& OlyILMemberFlags.AccessorMask = OlyILMemberFlags.Internal

    member _.IsProtected = ilMemberFlags &&& OlyILMemberFlags.AccessorMask = OlyILMemberFlags.Protected

[<Struct>]
type OlyIRFieldFlags internal (ilFieldFlags: OlyILFieldFlags, ilMemberFlags: OlyILMemberFlags, isExported: bool) =

    member internal _.ILFieldFlags = ilFieldFlags

    member internal _.ILMemberFlags = ilMemberFlags

    member _.IsMutable = ilFieldFlags &&& OlyILFieldFlags.Mutable = OlyILFieldFlags.Mutable

    member _.IsStatic = ilMemberFlags &&& OlyILMemberFlags.Static = OlyILMemberFlags.Static

    member _.IsPublic = ilMemberFlags &&& OlyILMemberFlags.AccessorMask = OlyILMemberFlags.Public

    member _.IsPrivate = ilMemberFlags &&& OlyILMemberFlags.AccessorMask = OlyILMemberFlags.Private

    member _.IsInternal = ilMemberFlags &&& OlyILMemberFlags.AccessorMask = OlyILMemberFlags.Internal

    member _.IsProtected = ilMemberFlags &&& OlyILMemberFlags.AccessorMask = OlyILMemberFlags.Protected

    member this.IsInstance = not this.IsStatic

    member _.IsExported = isExported
