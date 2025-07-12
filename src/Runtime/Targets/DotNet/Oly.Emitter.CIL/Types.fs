namespace Oly.Emitters.DotNet

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
