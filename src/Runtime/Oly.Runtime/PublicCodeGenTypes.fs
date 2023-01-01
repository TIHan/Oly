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
    | UsedOnlyOnce                  = 0b00100000

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
            this.ParameterTypes.Length = o.ParameterTypes.Length &&
            this.IsStatic = o.IsStatic &&
            this.IsConstructor = o.IsConstructor &&
            this.ReturnType.IsEqualTo(o.ReturnType) &&
            (this.ParameterTypes, o.ParameterTypes)
            ||> ImArray.forall2 (fun parTy1 parTy2 -> parTy1.IsEqualTo(parTy2))
        | _ ->
            false

[<Struct>]
[<RequireQualifiedAccess>]
type OlyIRByRefKind =
    | ReadWrite
    | Read

[<Struct>]
[<RequireQualifiedAccess>]
type OlyIRArrayKind =
    | Immutable
    | Mutable

// TODO: Rename this to RuntimeTypeFlags?
type OlyIRTypeFlags =
    | None =           0x000000000
    | ReadOnly =       0x000000001
    | GenericsErased = 0x000000010
    | Exported       = 0x000000100

type internal RuntimeFunctionFlags =
    | None =             0x00000000
    | Exported =         0x00000010
    | Inlineable =       0x00000100
    | External =         0x00100000
    | GenericsErased =   0x01000000
    | EntryPoint =       0x10000000

[<Sealed>]
type OlyIRFunctionExternalInfo internal (platform: string, path: string imarray, name: string) =
    
    member _.Platform = platform

    member _.Path = path

    member _.Name = name

[<Struct>]
type OlyIRFunctionFlags internal (ilFuncFlags: OlyILFunctionFlags, ilMemberFlags: OlyILMemberFlags, irFuncFlags: RuntimeFunctionFlags) =

    member internal _.SetGenericsErased() =
        OlyIRFunctionFlags(ilFuncFlags, ilMemberFlags, irFuncFlags ||| RuntimeFunctionFlags.GenericsErased)

    member internal _.SetStatic() =
        OlyIRFunctionFlags(ilFuncFlags, ilMemberFlags ||| OlyILMemberFlags.Static, irFuncFlags)

    member internal _.SetInlineable() =
        OlyIRFunctionFlags(ilFuncFlags, ilMemberFlags, irFuncFlags ||| RuntimeFunctionFlags.Inlineable)

    /// Function is able to be inlined in all situations.
    member _.IsInlineable = irFuncFlags &&& RuntimeFunctionFlags.Inlineable = RuntimeFunctionFlags.Inlineable

    member _.IsConstructor = ilFuncFlags &&& OlyILFunctionFlags.Constructor = OlyILFunctionFlags.Constructor

    member _.IsReadOnly = ilFuncFlags &&& OlyILFunctionFlags.Mutable <> OlyILFunctionFlags.Mutable

    member _.IsStatic = ilMemberFlags &&& OlyILMemberFlags.Static = OlyILMemberFlags.Static

    member this.IsInstance = not this.IsStatic

    member _.IsVirtual = ilMemberFlags &&& OlyILMemberFlags.Virtual = OlyILMemberFlags.Virtual

    member _.IsAbstract = ilMemberFlags &&& OlyILMemberFlags.Abstract = OlyILMemberFlags.Abstract

    member _.IsFinal = ilMemberFlags &&& OlyILMemberFlags.Final = OlyILMemberFlags.Final

    member _.IsNewSlot = ilMemberFlags &&& OlyILMemberFlags.NewSlot = OlyILMemberFlags.NewSlot

    member _.AreGenericsErased = irFuncFlags &&& RuntimeFunctionFlags.GenericsErased = RuntimeFunctionFlags.GenericsErased

    member _.IsEntryPoint = irFuncFlags &&& RuntimeFunctionFlags.EntryPoint = RuntimeFunctionFlags.EntryPoint

    member _.IsExported = irFuncFlags &&& RuntimeFunctionFlags.Exported = RuntimeFunctionFlags.Exported

    member _.IsExternal = irFuncFlags &&& RuntimeFunctionFlags.External = RuntimeFunctionFlags.External

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
