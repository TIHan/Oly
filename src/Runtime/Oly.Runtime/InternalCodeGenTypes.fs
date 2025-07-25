﻿[<AutoOpen>]
module internal rec Oly.Runtime.CodeGen.InternalTypes

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open Oly.Runtime
open Oly.Metadata
open Oly.Core
open Oly.Core.TaskExtensions

module private GenericContextDefault =
    let Instance =
        {
            enclosingTyArgs = ImArray.empty
            funcTyArgs = ImArray.empty
            isTyErasing = false
            isFuncErasing = false
            passedWitnesses = ImArray.empty
        }

    let InstanceTypeErasing =
        {
            enclosingTyArgs = ImArray.empty
            funcTyArgs = ImArray.empty
            isTyErasing = true
            isFuncErasing = false
            passedWitnesses = ImArray.empty
        }

[<NoEquality;NoComparison>]
type GenericContext =
    private {
        enclosingTyArgs: RuntimeType imarray
        funcTyArgs: RuntimeType imarray
        isTyErasing: bool
        isFuncErasing: bool
        passedWitnesses: RuntimeWitness imarray
    }

    static member Default = GenericContextDefault.Instance

    static member CreateFromEnclosingType(enclosingTy: RuntimeType, funcTyArgs: _ imarray) =
        let enclosingTyArgs = enclosingTy.TypeArguments
        if enclosingTyArgs.IsEmpty && funcTyArgs.IsEmpty then
            OlyAssert.True(enclosingTy.Witnesses.IsEmpty)
            GenericContext.Default
        else
            {
                enclosingTyArgs = enclosingTyArgs
                funcTyArgs = funcTyArgs
                isTyErasing = false
                isFuncErasing = false
                passedWitnesses = enclosingTy.Witnesses
            }

    static member Create(enclosingTyArgs: _ imarray) =
        if enclosingTyArgs.IsEmpty then
            GenericContext.Default
        else
            {
                enclosingTyArgs = enclosingTyArgs
                funcTyArgs = ImArray.empty
                isTyErasing = false
                isFuncErasing = false
                passedWitnesses = ImArray.empty
            }

    static member Create(enclosingTyArgs: _ imarray, funcTyArgs: _ imarray) =
        if enclosingTyArgs.IsEmpty && funcTyArgs.IsEmpty then
            GenericContext.Default
        else
            {
                enclosingTyArgs = enclosingTyArgs
                funcTyArgs = funcTyArgs
                isTyErasing = false
                isFuncErasing = false
                passedWitnesses = ImArray.empty
            }

    static member CreateErasing(enclosingTyArgs: _ imarray) =
        if enclosingTyArgs.IsEmpty then
            GenericContextDefault.InstanceTypeErasing
        else
            {
                enclosingTyArgs = enclosingTyArgs
                funcTyArgs = ImArray.empty
                isTyErasing = true
                isFuncErasing = false
                passedWitnesses = ImArray.empty
            }

    member this.PassedWitnesses = this.passedWitnesses

    member this.SetPassedWitnesses(witnesses: RuntimeWitness imarray) =
        if witnesses.IsEmpty then
            this
        else
            { this with passedWitnesses = witnesses }

    member this.FunctionTypeArguments =
        this.funcTyArgs

    member this.Set(enclosingTyArgs: _ imarray, funcTyArgs: _ imarray) =
        if this.enclosingTyArgs.IsEmpty && this.funcTyArgs.IsEmpty && enclosingTyArgs.IsEmpty && funcTyArgs.IsEmpty then
            this
        else
            { this with
                enclosingTyArgs = enclosingTyArgs
                funcTyArgs = funcTyArgs
            }

    member this.GetTypeArgument(index, ilKind) =
        match ilKind with
        | OlyILTypeVariableKind.Type ->
            this.enclosingTyArgs[index]
        | OlyILTypeVariableKind.Function ->
            this.funcTyArgs[index]
        | _ ->
            OlyAssert.Fail("Invalid type variable kind.")

    member this.AddFunctionTypeArguments(funcTyArgs: _ imarray) =
        if this.isFuncErasing && not this.funcTyArgs.IsEmpty then
            failwith "Function is erasing."

        { this with
            funcTyArgs = this.funcTyArgs.AddRange(funcTyArgs)
            isFuncErasing = false
        }

    member this.AddErasingFunctionTypeArguments(funcTyArgs: _ imarray) =
        if not this.isFuncErasing && not this.funcTyArgs.IsEmpty then
            failwith "Function is not erasing."

        { this with
            funcTyArgs = this.funcTyArgs.AddRange(funcTyArgs)
            isFuncErasing = true
        }

    member this.CanErase(i, ilKind) =
        match ilKind with
        | OlyILTypeVariableKind.Type ->
            // Context has no type arguments so any call to this will return false.
            if this.enclosingTyArgs.IsEmpty then false
            else
                if i < 0 || i >= this.enclosingTyArgs.Length then
                    raise(IndexOutOfRangeException())
                this.IsErasingType

        | OlyILTypeVariableKind.Function ->
            // Context has no type arguments so any call to this will return false.
            if this.funcTyArgs.IsEmpty then false
            else
                if i < 0 || i >= this.funcTyArgs.Length then
                    raise(IndexOutOfRangeException())
                this.IsErasingFunction

        | _ ->
            OlyAssert.Fail("Invalid type variable kind.")

    member this.GetErasedTypeArgument(i, ilKind) =
        if this.CanErase(i, ilKind) |> not then
            failwith $"Index {i} is not erasable."
        match ilKind with
        | OlyILTypeVariableKind.Type ->
            this.enclosingTyArgs[i]
        | OlyILTypeVariableKind.Function ->
            this.funcTyArgs[i]
        | _ ->
            OlyAssert.Fail("Invalid type variable kind.")

    member this.IsErasing = this.isTyErasing || this.isFuncErasing

    member this.Length = this.enclosingTyArgs.Length + this.funcTyArgs.Length

    member this.EnclosingTypeArguments = this.enclosingTyArgs

    member this.TypeArguments = this.enclosingTyArgs.AddRange(this.funcTyArgs)

    member this.IsErasingType = this.isTyErasing

    member this.IsErasingFunction = this.isFuncErasing

    member this.IsEmpty = this.enclosingTyArgs.IsEmpty && this.funcTyArgs.IsEmpty

[<NoComparison;CustomEquality;RequireQualifiedAccess>]
type RuntimeEnclosing =
    | Namespace of string imarray
    | Type of RuntimeType
    | Witness of realTy: RuntimeType * ty: RuntimeType * witness: RuntimeWitness option

    member this.IsExported =
        match this with
        | Type(ty) -> ty.IsExported
        | Witness(_, ty, _) -> ty.IsExported
        | _ -> false

    member this.AsType =
        match this with
        | Type(ty) -> ty
        | Witness(_, ty, _) -> ty
        | _ -> failwith "Expected type."

    member this.TryAbstractType =
        match this with
        | Witness(_, ty, _) -> ValueSome ty
        | _ -> ValueNone

    member this.TypeArguments =
        match this with
        | Namespace _ -> ImArray.empty
        | Type(ty) -> ty.TypeArguments
        | Witness(_, ty, _) -> ty.TypeArguments

    member this.TypeParameters =
        match this with
        | Namespace _ -> ImArray.empty
        | Type(ty) -> ty.TypeParameters
        | Witness(_, ty, _) -> ty.TypeParameters

    member this.Substitute(genericContext: GenericContext) =
        match this with
        | Namespace _ -> this
        | Type(ty) -> Type(ty.Substitute(genericContext))
        | Witness _ ->
            failwith "Witness cannot be an enclosing for a type."

    override this.GetHashCode() =
        match this with
        | Namespace path -> path.GetHashCode()
        | Type(ty) -> ty.GetHashCode()
        | Witness(ty, _, _) -> ty.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? RuntimeEnclosing as o ->
            match this, o with
            | Namespace(path1), Namespace(path2) ->
                path1.Length = path2.Length &&
                (path1, path2)
                ||> ImArray.forall2 (=)

            | _ ->
                this.AsType = o.AsType
        | _ ->
            false

[<NoComparison;CustomEquality;RequireQualifiedAccess>]
type RuntimeTypeParameter =
    {
        Name: string
        Arity: int
        IsVariadic: bool
        ILConstraints: OlyILConstraint imarray
        mutable ConstraintSubtypes: RuntimeType imarray Lazy
        mutable ConstraintTraits: RuntimeType imarray Lazy
    }

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? RuntimeTypeParameter as o ->
            (this : IEquatable<_>).Equals(o)
        | _ ->
            false

    interface IEquatable<RuntimeTypeParameter> with

        member this.Equals(o: RuntimeTypeParameter) =
            if obj.ReferenceEquals(this, o) then true
            else

            this.Name = o.Name &&
            this.Arity = o.Arity &&
            this.IsVariadic = o.IsVariadic

[<NoComparison;CustomEquality;RequireQualifiedAccess>]
type RuntimeParameter =
    {
        Attributes: RuntimeAttribute imarray
        Name: string
        Type: RuntimeType
    }

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        if obj.ReferenceEquals(this, o) then true
        else

        match o with
        | :? RuntimeParameter as o ->
            this.Name = o.Name &&
            this.Type = o.Type
        | _ ->
            false

[<Flags>]
type RuntimeEntityFlags =
    | None            = 0b000000
    | Intrinsic       = 0b000001

[<NoComparison;NoEquality;RequireQualifiedAccess>]
type RuntimeEntityInfo =
    {
        Name: string
        ILAssembly: OlyILReadOnlyAssembly
        ILEntityDefinitionHandle: OlyILEntityDefinitionHandle
        ILEntityKind: OlyILEntityKind
        ILEntityFlags: OlyILEntityFlags
        ILPropertyDefinitionLookup: ImmutableDictionary<OlyILFunctionDefinitionHandle, OlyILPropertyDefinitionHandle>

        mutable Flags: RuntimeEntityFlags
        mutable Formal: RuntimeEntity
        mutable Attributes: RuntimeAttribute imarray
        mutable StaticConstructor: RuntimeFunction option
    }

[<NoComparison;CustomEquality;RequireQualifiedAccess>]
type RuntimeEntity =
    {
        Enclosing: RuntimeEnclosing
        TypeParameters: RuntimeTypeParameter imarray
        TypeArguments: RuntimeType imarray
        mutable Witnesses: RuntimeWitness imarray
        mutable ExtendsLazy: RuntimeType imarray Lazy
        mutable ImplementsLazy: RuntimeType imarray Lazy
        mutable RuntimeTypeLazy: RuntimeType option Lazy
        mutable FieldsLazy: RuntimeField imarray Lazy
        mutable AsType: RuntimeType

        Info: RuntimeEntityInfo
    }

    member this.Name = this.Info.Name
    member this.ILAssembly = this.Info.ILAssembly
    member this.ILEntityDefinitionHandle = this.Info.ILEntityDefinitionHandle
    member this.ILEntityKind = this.Info.ILEntityKind
    member this.ILEntityFlags = this.Info.ILEntityFlags
    member this.ILPropertyDefinitionLookup = this.Info.ILPropertyDefinitionLookup
    member this.Formal = this.Info.Formal
    member this.Attributes = this.Info.Attributes
    member this.StaticConstructor = this.Info.StaticConstructor

    member this.Extends = this.ExtendsLazy.Value
    member this.Implements = this.ImplementsLazy.Value
    member this.Fields = this.FieldsLazy.Value
    member this.RuntimeType = this.RuntimeTypeLazy.Value

    member private this.FilterWitnesses(witnesses: RuntimeWitness imarray) =
        (this.TypeArguments, this.TypeParameters)
        ||> ImArray.mapi2 (fun i tyArg tyPar ->
            witnesses
            |> ImArray.choose (fun (witness: RuntimeWitness) ->
                if witness.Type.StripAlias() = tyArg.StripAlias() then
                    let tyExt = witness.TypeExtension
                    let exists = 
                        tyPar.ConstraintTraits.Value
                        |> ImArray.exists (fun superTy ->
                            subsumesType superTy tyExt 
                        )
                    if exists then
                        match witness.TypeVariableKind with
                        | OlyILTypeVariableKind.Type when i = witness.TypeVariableIndex ->
                            Some witness
                        | OlyILTypeVariableKind.Function ->
                            RuntimeWitness(i, OlyILTypeVariableKind.Type, witness.Type, witness.TypeExtension, witness.AbstractFunction)
                            |> Some
                        | _ ->
                            None
                    else
                        None
                else
                    None
            )
        )
        |> ImArray.concat
        |> ImArray.distinct

    member this.SetWitnesses(witnesses: RuntimeWitness imarray) =
        // Imported types do not support witnesses.
        if ((witnesses.IsEmpty && this.Witnesses.IsEmpty) || this.TypeArguments.IsEmpty || this.IsImported) then
            this
        else
            let entNew =
                { this with Witnesses = this.FilterWitnesses(witnesses) }

            entNew.FieldsLazy <-
                lazy
                    let enclosingTy = RuntimeType.Entity(entNew)
                    this.Fields
                    |> ImArray.map (fun x ->
                        x.Substitute(enclosingTy)
                    )
            entNew

    member this.AssemblyIdentity = this.ILAssembly.Identity

    member this.IsAnyStruct =
        if this.IsEnumOrNewtype then
            this.RuntimeType.Value.IsAnyStruct
        else
            this.ILEntityKind = OlyILEntityKind.Struct ||
            this.IsScopedClosure ||
            (
                (this.IsTypeExtension || this.IsAlias) && not this.Extends.IsEmpty && this.Extends.[0].IsAnyStruct
            )

    member this.IsEnum =
        this.ILEntityKind = OlyILEntityKind.Enum

    member this.IsNewtype =
        this.ILEntityKind = OlyILEntityKind.Newtype

    member this.IsEnumOrNewtype =
        this.IsEnum || this.IsNewtype

    member this.IsTypeExtension =
        this.ILEntityKind = OlyILEntityKind.TypeExtension

    member this.IsClosure =
        this.ILEntityKind = OlyILEntityKind.Closure

    member this.IsScoped =
        this.ILEntityFlags.HasFlag(OlyILEntityFlags.Scoped)

    // TODO: Do we really want this?
    member this.IsScopedClosure =
        this.IsClosure && this.IsScoped

    member this.IsInterface =
        this.ILEntityKind = OlyILEntityKind.Interface

    member this.IsModule =
        this.ILEntityKind = OlyILEntityKind.Module

    member this.IsClass =
        this.ILEntityKind = OlyILEntityKind.Class

    member this.IsAlias =
        this.ILEntityKind = OlyILEntityKind.Alias

    member this.IsShape =
        this.ILEntityKind = OlyILEntityKind.Shape

    member this.IsAbstract =
        this.ILEntityFlags &&& OlyILEntityFlags.Abstract = OlyILEntityFlags.Abstract

    member this.IsFinal =
        this.ILEntityFlags &&& OlyILEntityFlags.Final = OlyILEntityFlags.Final

    member this.IsFormal = obj.ReferenceEquals(this, this.Formal)

    member this.IsTypeConstructor =
        not this.TypeParameters.IsEmpty && this.IsFormal

    member this.IsImported =
        let entDef = this.ILAssembly.GetEntityDefinition(this.ILEntityDefinitionHandle)
        entDef.Attributes
        |> ImArray.exists (function OlyILAttribute.Import _ -> true | _ -> false)

    member this.TryImportedInfo =
        let entDef = this.ILAssembly.GetEntityDefinition(this.ILEntityDefinitionHandle)
        entDef.Attributes
        |> ImArray.tryPick (function OlyILAttribute.Import(platform, path, name) -> Some(platform, path, name) | _ -> None)

    member this.IsExported =
        let entDef = this.ILAssembly.GetEntityDefinition(this.ILEntityDefinitionHandle)
        entDef.Attributes
        |> ImArray.exists (function OlyILAttribute.Export -> true | _ -> false)

    member this.IsObjectType =
        if this.IsIntrinsic then
            let entDef = this.ILAssembly.GetEntityDefinition(this.ILEntityDefinitionHandle)
            entDef.Attributes
            |> ImArray.exists (function 
                | OlyILAttribute.Intrinsic(nameHandle) ->
                    this.ILAssembly.GetStringOrEmpty(nameHandle) = "base_object"
                | _ -> 
                    false
            )
        else
            false

    member this.IsIntrinsic =
        this.Info.Flags.HasFlag(RuntimeEntityFlags.Intrinsic)

    member this.TryGetIntrinsicTypeInfo() =
        if this.IsIntrinsic then
            let entDef = this.ILAssembly.GetEntityDefinition(this.ILEntityDefinitionHandle)
            entDef.Attributes
            |> ImArray.tryPick (function 
                | OlyILAttribute.Intrinsic(nameHandle) -> 
                    Some(this.ILAssembly.GetStringOrEmpty(nameHandle))
                | _ -> 
                    None
            )
        else
            None

    member this.Substitute(genericContext: GenericContext) =
        if genericContext.IsEmpty then
            this
        else
            let origTyArgs = this.TypeArguments
            let tyArgs =
                origTyArgs
                |> ImArray.map (fun x -> x.Substitute(genericContext))

            let isSame =
                (origTyArgs, tyArgs)
                ||> ImArray.forall2 (=)

            // If the type arguments did not change, then return as we do not want to allocate another Entity.
            if isSame then
                this
            else

            let tyPars =
                this.TypeParameters
                |> ImArray.map (fun x ->
                    if x.ConstraintSubtypes.Value.IsEmpty && x.ConstraintTraits.Value.IsEmpty then
                        x
                    else
                        { x with 
                            ConstraintSubtypes = lazy (x.ConstraintSubtypes.Value |> ImArray.map (fun x -> x.Substitute(genericContext)))
                            ConstraintTraits = lazy (x.ConstraintTraits.Value |> ImArray.map (fun x -> x.Substitute(genericContext)))
                        }
                )

            let entNew =
                // TODO: What about witnesses?
                { this with
                    Enclosing = this.Enclosing.Substitute(genericContext)
                    ExtendsLazy =
                        lazy
                            this.Extends
                            |> ImArray.map (fun x ->
                                x.Substitute(genericContext)
                            )
                    ImplementsLazy =
                        lazy
                            this.Implements
                            |> ImArray.map (fun x ->
                                x.Substitute(genericContext)
                            )
                    RuntimeTypeLazy =
                        lazy
                            this.RuntimeType
                            |> Option.map (fun x -> x.Substitute(genericContext))
                    TypeArguments = tyArgs
                    TypeParameters = tyPars }

            let witnessesToSet =
                genericContext.PassedWitnesses
                |> ImArray.filter (fun witness ->
                    this.TypeArguments
                    |> ImArray.exists (fun tyArg ->
                        match tyArg with
                        | RuntimeType.Variable(index, ilKind)
                        | RuntimeType.HigherVariable(index, _, ilKind) ->
                            witness.TypeVariableIndex = index &&
                            witness.TypeVariableKind = ilKind
                        | _ ->
                            false
                    )
                )

            entNew.Witnesses <- entNew.FilterWitnesses(witnessesToSet)

            entNew.FieldsLazy <-
                lazy
                    let enclosingTy = RuntimeType.Entity(entNew)
                    this.Fields
                    |> ImArray.map (fun x ->
                        x.Substitute(enclosingTy)
                    )

            entNew

    member this.Apply(tyArgs: RuntimeType imarray) : RuntimeEntity =
        if this.TypeParameters.Length <> tyArgs.Length then
            failwith "Type argument count does not match type parameter count when applying."

        if not this.IsFormal then
            failwith "Expected formal entity."

        if tyArgs.IsEmpty then
            this
        else
            let genericContext = GenericContext.Create(tyArgs)

            let tyPars =
                this.TypeParameters
                |> ImArray.map (fun x ->
                    if x.ConstraintSubtypes.Value.IsEmpty && x.ConstraintTraits.Value.IsEmpty then
                        x
                    else
                        { x with 
                            ConstraintSubtypes = lazy (x.ConstraintSubtypes.Value |> ImArray.map (fun x -> x.Substitute(genericContext))) 
                            ConstraintTraits = lazy (x.ConstraintTraits.Value |> ImArray.map (fun x -> x.Substitute(genericContext))) 
                        }
                )

            let entNew =
                { this with
                    Enclosing = this.Enclosing.Substitute(genericContext)
                    ExtendsLazy =
                        lazy
                            this.Extends
                            |> ImArray.map (fun x ->
                                x.Substitute(genericContext)
                            )
                    ImplementsLazy = 
                        lazy
                            this.Implements
                            |> ImArray.map (fun x ->
                                x.Substitute(genericContext)
                            )
                    RuntimeTypeLazy =
                        lazy
                            this.RuntimeType
                            |> Option.map (fun x -> x.Substitute(genericContext))
                    TypeArguments = tyArgs }

            entNew.FieldsLazy <- 
                lazy
                    let enclosingTy = RuntimeType.Entity(entNew)
                    this.Fields
                    |> ImArray.map (fun x ->
                        x.Apply(enclosingTy)
                    )
            { entNew with TypeParameters = tyPars }

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? RuntimeEntity as o ->
            (this : IEquatable<_>).Equals(o)
        | _ ->
            false

    interface IEquatable<RuntimeEntity> with

        member this.Equals(o: RuntimeEntity) =
            if obj.ReferenceEquals(this, o) then true
            else

            obj.ReferenceEquals(this.Formal, o.Formal) && this.TypeArguments.Length = o.TypeArguments.Length &&
            (
                (this.TypeArguments, o.TypeArguments)
                ||> ImArray.forall2 (=)
            ) && this.Witnesses.Length = o.Witnesses.Length &&
            (
                (this.Witnesses, o.Witnesses)
                ||> ImArray.forall2 (=)
            )

let emptyEnclosing = RuntimeEnclosing.Namespace(ImArray.empty)

[<NoComparison;CustomEquality;RequireQualifiedAccess;DebuggerDisplay("{DebugText}")>]
type RuntimeType =
    | BaseObject

    | Void
    | Unit
    | UInt8
    | Int8
    | UInt16
    | Int16
    | UInt32
    | Int32
    | UInt64
    | Int64
    | Float32
    | Float64
    | Bool
    | Char16
    | Utf16
    | NativeInt
    | NativeUInt
    | NativePtr of elementTy: RuntimeType
    | Tuple of tyArgs: RuntimeType imarray * string imarray
    | ReferenceCell of elementTy: RuntimeType
    | Array of elementTy: RuntimeType * rank: int * isMutable: bool // TODO: Instead of 'isMutable', use 'kind: OlyIRArrayKind'
    | FixedArray of elementTy: RuntimeType * lengthTy: RuntimeType * isMutable: bool // TODO: Instead of 'isMutable', use 'kind: OlyIRArrayKind'
    | Function of argTys: RuntimeType imarray * returnTy: RuntimeType * kind: OlyIRFunctionKind
    | NativeFunctionPtr of OlyILCallingConvention * argTys: RuntimeType imarray * returnTy: RuntimeType
    | Entity of RuntimeEntity
    | Variable of index: int * ilKind: OlyILTypeVariableKind
    | HigherVariable of index: int * tyArgs: RuntimeType imarray * ilKind: OlyILTypeVariableKind
    | ByRef of elementTy: RuntimeType * kind: OlyIRByRefKind

    | ForAll of tyPars: RuntimeTypeParameter imarray * innerTy: RuntimeType

    // TODO: We should generalize constant types.
    | ConstantInt32 of value: int32

    member this.SetWitnesses(witnesses: RuntimeWitness imarray) =
        OlyAssert.False(this.IsAlias)
        match this with
        | Entity(ent) ->
            RuntimeType.Entity(ent.SetWitnesses(witnesses))
        | _ ->
            this

    member this.TryGetStaticConstructor() =
        OlyAssert.True(this.IsFormal)
        match this with
        | Entity(ent) -> ent.StaticConstructor
        | _ -> None

    member this.IsUtf16_t =
        match this.StripAlias() with
        | Utf16 -> true
        | _ -> false

    member this.IsByRef_t =
        match this.StripAlias() with
        | ByRef _ -> true
        | _ -> false

    member this.IsInteger =
        match this.StripAlias() with
        | UInt8
        | Int8
        | UInt16
        | Int16
        | UInt32
        | Int32
        | UInt32
        | Int64
        | UInt64
        | NativeInt
        | NativeUInt -> true
        | _ -> false

    member this.IsAnyPtr =
        match this.StripAlias() with
        | NativePtr _
        | NativeFunctionPtr _ -> true
        | _ -> false

    /// NativeInt or NativeUInt
    member this.IsAnyNativeInt =
        match this.StripAlias() with
        | NativeInt
        | NativeUInt -> true
        | _ -> false

    member this.IsUnit_t =
        match this.StripAlias() with
        | Unit -> true
        | _ -> false

    member this.IsVoid_t =
        match this.StripAlias() with
        | Void -> true
        | _ -> false

    member this.IsReadWriteByRef =
        match this.StripAlias() with
        | ByRef(_, OlyIRByRefKind.ReadWrite) -> true
        | _ -> false

    member this.IsReadOnlyByRef =
        match this.StripAlias() with
        | ByRef(_, OlyIRByRefKind.ReadOnly) -> true
        | _ -> false

    member this.IsWriteOnlyByRef =
        match this.StripAlias() with
        | ByRef(_, OlyIRByRefKind.WriteOnly) -> true
        | _ -> false

    member this.Witnesses : RuntimeWitness imarray =
        match this.StripAlias() with
        | Entity(ent) -> ent.Witnesses
        | _ -> ImArray.empty

    member this.IsExported =
        match this with
        | Entity(ent) -> ent.IsExported
        | _ -> false

    member this.IsShape =
        match this with
        | Entity(ent) -> ent.IsShape
        | _ -> false

    member this.IsEnum =
        match this with
        | Entity(ent) -> ent.IsEnum
        | _ -> false

    member this.IsNewtype =
        match this with
        | Entity(ent) -> ent.IsNewtype
        | _ -> false
        
    member this.IsEnumOrNewtype =
        this.IsEnum || this.IsNewtype

    member this.IsTypeVariable =
        match this with
        | Variable _
        | HigherVariable _ -> true
        | _ -> false

    member this.IsByRefOfVariable =
        match this with
        | ByRef(elementTy, _) ->
            elementTy.IsTypeVariable
        | _ ->
            false

    member this.TryExternal =
        match this with
        | Entity(ent) -> ent.TryImportedInfo
        | _ -> None

    member this.TryGetImportInfo() =
        match this with
        | Entity(ent) -> 
            match ent.TryImportedInfo with
            | Some(platform, path, name) ->
                let ilAsm = ent.ILAssembly
                let platform = ilAsm.GetStringOrEmpty(platform)
                let path = path |> ImArray.map ilAsm.GetStringOrEmpty
                let name = ilAsm.GetStringOrEmpty(name)
                Some(platform, path, name)
            | _ ->
                None
        | _ -> 
            None

    member this.Enclosing =
        match this with
        | Entity(ent) -> ent.Enclosing
        | _ -> emptyEnclosing

    member this.IsBuiltIn =
        match this with
        | Entity _ -> false
        | _ -> true

    member this.IsPrimitive =
        match this with
        | Entity _
        | Variable _
        | HigherVariable _ -> false
        | _ -> true

    member this.IsIntrinsic =
        match this with
        | Entity(ent) -> ent.IsIntrinsic
        | _ -> false

    member this.AssemblyIdentity =
        match this with
        | Entity(ent) -> ent.AssemblyIdentity
        | _ -> failwith "Unable to get assembly identity of built-in type."

    member this.ILEntityDefinitionHandle =
        match this with
        | Entity(ent) -> ent.ILEntityDefinitionHandle
        | _ -> failwith "Unable to get entity handle of built-in type."

    member this.Formal =
        match this with
        | Entity(ent) -> ent.Formal.AsType
        | ForAll _ -> this
        | _ -> this // TODO:

    member this.IsFormal =
        match this, this.Formal with
        | Entity(ent1), Entity(ent2) -> obj.ReferenceEquals(ent1, ent2)
        | ForAll _, _ -> true
        | _ -> true // TODO:

    member this.Fields =
        match this with
        | Entity(ent) -> ent.Fields
        | _ -> ImArray.empty

    member this.IsAbstract =
        match this with
        | Entity(ent) -> ent.IsAbstract
        | _ -> false

    member this.Name =
        match this with
        | Void -> "__oly_void"
        | Unit -> "__oly_unit"
        | Int8 -> "__oly_int8"
        | UInt8 -> "__oly_uint8"
        | Int16 -> "__oly_int16"
        | UInt16 -> "__oly_uint16"
        | Int32 -> "__oly_int32"
        | UInt32 -> "__oly_uint32"
        | Int64 -> "__oly_int64"
        | UInt64 -> "__oly_uint64"
        | Float32 -> "__oly_float32"
        | Float64 -> "__oly_float64"
        | NativeInt -> "__oly_native_int"
        | NativeUInt -> "__oly_native_uint"
        | NativePtr _ -> "__oly_native_pointer"
        | NativeFunctionPtr _ -> "__oly_func_pointer"
        | Bool -> "__oly_bool"
        | Tuple _ -> "__oly_tuple"
        | Utf16 -> "__oly_utf16"
        | Char16 -> "__oly_char16"
        | ReferenceCell _ -> "__oly_reference_cell"
        | Function(kind=kind) -> 
            match kind with
            | OlyIRFunctionKind.Normal ->
                "__oly_func"
            | OlyIRFunctionKind.Scoped ->
                "__oly_scoped_func"
        | Array _ -> "__oly_array"
        | FixedArray _ -> "__oly_fixed_array"
        | BaseObject -> "__oly_base_object"
        | ForAll _ -> "__oly_for_all"
        | Entity(ent) -> ent.Name
        | Variable(index, ilKind) -> $"__oly_type_variable_{index}_{ilKind}"
        | HigherVariable(index, _, ilKind) -> $"__oly_higher_variable_{index}_{ilKind}"
        | ByRef(elementTy, _) -> $"__oly_by_reference_{elementTy.Name}"
        | ConstantInt32(value) -> value.ToString()

    member this.TypeArguments : RuntimeType imarray =
        match this with
        | Entity(ent) -> ent.TypeArguments
        | HigherVariable(tyArgs=tyArgs) -> tyArgs
        | Tuple(tyArgs, _) -> tyArgs
        | ReferenceCell(elementTy)
        | ByRef(elementTy, _)
        | NativePtr(elementTy) 
        | Array(elementTy, _, _) -> ImArray.createOne elementTy
        | FixedArray(elementTy, lengthTy, _) -> ImArray.createTwo elementTy lengthTy
        | Function(argTys, returnTy, _) 
        | NativeFunctionPtr(_, argTys, returnTy) ->
            argTys.Add(returnTy)
        | _ -> 
            ImArray.empty

    member this.IsAnyStruct =
        match this with
        | UInt8
        | Int8
        | UInt16
        | Int16
        | UInt32
        | Int32
        | UInt64
        | Int64
        | Float32
        | Float64
        | Bool
        | Char16
        | NativeInt
        | NativeUInt
        | NativePtr _ 
        | NativeFunctionPtr _
        | Unit
        | Tuple _ 
        | FixedArray _ -> true
        | Function(kind=OlyIRFunctionKind.Scoped) -> true
        | Entity(ent) -> ent.IsAnyStruct
        | _ -> false

    member this.IsArrayOfStruct =
        match this with
        | Array(elementTy, _, _) -> elementTy.IsAnyStruct
        | _ -> false

    member this.IsTypeExtension =
        match this with
        | Entity(ent) -> ent.IsTypeExtension
        | _ -> false

    member this.IsModule=
        match this with
        | Entity(ent) -> ent.IsModule
        | _ -> false

    member this.IsClosure =
        match this with
        | Entity(ent) -> ent.IsClosure
        | _ -> false

    member this.IsScoped =
        match this.StripAll() with
        | ByRef _ -> true
        | Entity(ent) -> ent.IsScoped
        | Function(kind=OlyIRFunctionKind.Scoped) -> true
        | _ -> false

    member this.IsInterface =
        match this with
        | Entity(ent) -> ent.IsInterface
        | _ -> false

    member this.IsAlias =
        match this with
        | Entity(ent) -> ent.IsAlias
        | _ -> false

    member this.IsTypeConstructor =
        match this with
        | Entity(ent) -> ent.IsTypeConstructor
        | ForAll _ -> true
        | _ -> false

    member this.IsFinal =
        match this with
        | Entity(ent) -> ent.IsFinal
        | _ -> false

    member this.Extends: RuntimeType imarray =
        match this with
        | Entity(ent) -> ent.Extends
        | _ -> ImArray.empty

    member this.Implements: RuntimeType imarray =
        match this with
        | Entity(ent) -> ent.Implements
        | _ -> ImArray.empty

    member this.RuntimeType: RuntimeType option =
        match this with
        | Entity(ent) -> ent.RuntimeType
        | _ -> None

    member this.TypeParameters: RuntimeTypeParameter imarray =
        match this with
        | ForAll(tyPars, _) -> tyPars
        | Entity(ent) -> ent.TypeParameters
        | ReferenceCell _
        | Array _
        | ByRef _ 
        | NativePtr _ -> ImArray.createOne({ Name = ""; Arity = 0; IsVariadic = false; ILConstraints = ImArray.empty; ConstraintSubtypes = Lazy<_>.CreateFromValue(ImArray.empty); ConstraintTraits = Lazy<_>.CreateFromValue(ImArray.empty) })
        | Tuple _ -> ImArray.createOne({ Name = ""; Arity = 0; IsVariadic = true; ILConstraints = ImArray.empty; ConstraintSubtypes = Lazy<_>.CreateFromValue(ImArray.empty); ConstraintTraits = Lazy<_>.CreateFromValue(ImArray.empty) })
        | Function _ 
        | NativeFunctionPtr _ ->
            ImArray.init this.TypeArguments.Length (fun i -> { Name = ""; Arity = 0; IsVariadic = false; ILConstraints = ImArray.empty; ConstraintSubtypes = Lazy<_>.CreateFromValue(ImArray.empty); ConstraintTraits = Lazy<_>.CreateFromValue(ImArray.empty) })
        | FixedArray _ ->
            (
                ({ Name = ""; Arity = 0; IsVariadic = false; ILConstraints = ImArray.empty; ConstraintSubtypes = Lazy<_>.CreateFromValue(ImArray.empty); ConstraintTraits = Lazy<_>.CreateFromValue(ImArray.empty) }: RuntimeTypeParameter),
                ({ Name = "RowRank"; Arity = 0; IsVariadic = false; ILConstraints = ImArray.createOne (OlyILConstraint.ConstantType(OlyILType.OlyILTypeInt32)); ConstraintSubtypes = Lazy<_>.CreateFromValue(ImArray.empty); ConstraintTraits = Lazy<_>.CreateFromValue(ImArray.empty) }: RuntimeTypeParameter),
                ({ Name = "ColumnRank"; Arity = 0; IsVariadic = false; ILConstraints = ImArray.createOne (OlyILConstraint.ConstantType(OlyILType.OlyILTypeInt32)); ConstraintSubtypes = Lazy<_>.CreateFromValue(ImArray.empty); ConstraintTraits = Lazy<_>.CreateFromValue(ImArray.empty) }: RuntimeTypeParameter)
            )
            |||> ImArray.createThree
        | _ -> 
            ImArray.empty

    member this.IsObjectType: bool =
        match this with
        | BaseObject -> true
        | Entity(ent) -> ent.IsObjectType
        | _ -> false

    member this.IsExternal = 
        match this with
        | Entity(ent) -> ent.IsImported
        | _ -> false

    member this.CanGenericsBeErased = not this.IsExternal && not this.IsExported

    member this.Substitute(genericContext: GenericContext): RuntimeType =
        if genericContext.IsEmpty then
            this
        else

        match this with
        | HigherVariable(index, tyArgs, ilKind) ->
            let tyArgsNew = tyArgs |> ImArray.map (fun x -> x.Substitute(genericContext))
            let ty = genericContext.GetTypeArgument(index, ilKind)
            match ty with
            | Variable(index, ilKind) ->
                HigherVariable(index, tyArgs, ilKind)
            | _ ->
                if not ty.IsTypeConstructor then
                    failwith "Expected type constructor."
                ty.Apply(tyArgsNew)
        | Variable(index, ilKind) -> genericContext.GetTypeArgument(index, ilKind)
        | Tuple(tyArgs, names) ->
            let tyArgsNew = tyArgs |> ImArray.map (fun x -> x.Substitute(genericContext))
            Tuple(tyArgsNew, names)
        | Function(argTys, returnTy, kind) ->
            Function(argTys |> ImArray.map (fun x -> x.Substitute(genericContext)), returnTy.Substitute(genericContext), kind)
        | NativeFunctionPtr(ilCc, argTys, returnTy) ->
            NativeFunctionPtr(ilCc, argTys |> ImArray.map (fun x -> x.Substitute(genericContext)), returnTy.Substitute(genericContext))
        | ReferenceCell(elementTy) ->
            ReferenceCell(elementTy.Substitute(genericContext))
        | Array(elementTy, rank, isMutable) ->
            Array(elementTy.Substitute(genericContext), rank, isMutable)
        | FixedArray(elementTy, lengthTy, isMutable) ->
            FixedArray(elementTy.Substitute(genericContext), lengthTy, isMutable)
        | Entity(ent) ->
            if ent.IsTypeConstructor then
                this
            else
                if ent.TypeParameters.IsEmpty then
                    this
                else
                    let newEnt = ent.Substitute(genericContext)
                    if obj.ReferenceEquals(ent, newEnt) then
                        this
                    else
                        Entity(newEnt)
        | ByRef(elementTy, kind) ->
            ByRef(elementTy.Substitute(genericContext), kind)
        | NativePtr(elementTy) ->
            NativePtr(elementTy.Substitute(genericContext))
        | _ ->
            this

    member this.IsEntity_t =
        match this with
        | Entity _ -> true
        | _ -> false

    member this.AsEntity =
        match this with
        | Entity(ent) -> ent
        | _ -> failwith "Expected entity"

    member this.Apply(tyArgs: RuntimeType imarray) =
#if DEBUG || CHECKED
        OlyAssert.Equal(this.TypeParameters.Length, tyArgs.Length)
#endif

        if tyArgs.IsEmpty then
            this
        else

        match this with
        | Variable(index, ilKind) ->
            HigherVariable(index, tyArgs, ilKind)
        | Entity(ent) ->
            if not this.IsTypeConstructor then
                failwith "requires type constructor"
            Entity(ent.Apply(tyArgs))
        | ForAll(_, ty) ->
            ty.Substitute(GenericContext.Create(tyArgs))
        | _ ->
            this

    member this.IsVariadicTypeParameter(tyPars: RuntimeTypeParameter imarray) =
        match this with
        | Variable(index, _) ->
            tyPars[index].IsVariadic
        | _ ->
            false

    member this.TryGetVariadicTypeArgument() =
        let mutable lastVariadicTyParIndex = -1
        this.TypeParameters
        |> ImArray.iteri (fun i tyPar ->
            if tyPar.IsVariadic then
                lastVariadicTyParIndex <- i
        )
        if lastVariadicTyParIndex = -1 then
            ValueNone
        else
            ValueSome(this.TypeArguments[lastVariadicTyParIndex])

    member this.Strip() =
        match this with
        | Entity(ent) ->
            match ent.TryGetIntrinsicTypeInfo() with
            | Some name ->
                match name with
                | "base_object" -> RuntimeType.BaseObject
                | "utf16" -> RuntimeType.Utf16
                | "char16" -> RuntimeType.Char16
                | "int8" -> RuntimeType.Int8
                | "uint8" -> RuntimeType.UInt8
                | "int16" -> RuntimeType.Int16
                | "uint16" -> RuntimeType.UInt16
                | "int32" -> RuntimeType.Int32
                | "uint32" -> RuntimeType.UInt32
                | "int64" -> RuntimeType.Int64
                | "uint64" -> RuntimeType.UInt64
                | "float32" -> RuntimeType.Float32
                | "float64" -> RuntimeType.Float64
                | "bool" -> RuntimeType.Bool
                | "native_uint" -> RuntimeType.NativeUInt
                | "native_int" -> RuntimeType.NativeInt
                | "native_ptr" -> RuntimeType.NativePtr(this.TypeArguments[0])
                | "void" -> RuntimeType.Void
                | _ ->
                    raise(System.NotImplementedException("intrinsic type stripping: " + name))
            | _ ->
                this
        | _ ->
            this

    member this.StripExtension() =
        if this.IsTypeExtension then
            this.Extends[0]
        else
            this

    interface IOlyIRTypeKey with

        member ty1.IsEqualTo(ty2) = 
            match ty2 with
            | :? RuntimeType as ty2 ->
                ty1 = ty2
            | _ ->
                false

    member this.DebugText =
        if this.TypeArguments.IsEmpty then
            this.Name
        elif this.IsTypeConstructor then
            let tyArgsText = this.TypeArguments |> Seq.map (fun x -> "_") |> String.concat ","
            $"(type constructor) {this.Name}<{tyArgsText}>" 
        else
            let tyArgsText = this.TypeArguments |> Seq.map (fun x -> x.DebugText) |> String.concat ","
            $"{this.Name}<{tyArgsText}>" 

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? RuntimeType as o ->
            (this : IEquatable<_>).Equals(o)
        | _ ->
            false

    interface IEquatable<RuntimeType> with

        member this.Equals(o: RuntimeType) =
            if obj.ReferenceEquals(this, o) then true
            else

            match this.Strip(), o.Strip() with
            | Void, Void
            | Unit, Unit
            | UInt8, UInt8
            | Int8, Int8
            | UInt16, UInt16
            | Int16, Int16
            | UInt32, UInt32
            | Int32, Int32
            | UInt64, UInt64
            | Int64, Int64
            | Float32, Float32
            | Float64, Float64
            | Bool, Bool -> true
            | Utf16, Utf16 -> true
            | BaseObject, BaseObject -> true
            | NativeInt, NativeInt
            | NativeUInt, NativeUInt -> true
            | NativePtr(elementTy1), NativePtr(elementTy2) -> elementTy1 = elementTy2
            | ReferenceCell(elementTy1), ReferenceCell(elementTy2) -> elementTy1 = elementTy2
            | Array(elementTy1, rank1, isMutable1), Array(elementTy2, rank2, isMutable2) -> elementTy1 = elementTy2 && rank1 = rank2 && isMutable1 = isMutable2
            | FixedArray(elementTy1, lengthTy1, isMutable1), FixedArray(elementTy2, lengthTy2, isMutable2) -> lengthTy1 = lengthTy2 && elementTy1 = elementTy2 && isMutable1 = isMutable2
            | Tuple(tyArgs1, _), Tuple(tyArgs2, _) when tyArgs1.Length = tyArgs2.Length ->
                (tyArgs1, tyArgs2)
                ||> ImArray.forall2 (=)

            | ByRef(elementTy1, kind1), ByRef(elementTy2, kind2) -> elementTy1 = elementTy2 && kind1 = kind2

            | Function(inputTys1, outputTy1, kind1), Function(inputTys2, outputTy2, kind2) when inputTys1.Length = inputTys2.Length ->
                outputTy1 = outputTy2 &&
                kind1 = kind2 &&
                (inputTys1, inputTys2)
                ||> ImArray.forall2 (=)

            | NativeFunctionPtr(ilCc1, inputTys1, outputTy1), NativeFunctionPtr(ilCc2, inputTys2, outputTy2) when inputTys1.Length = inputTys2.Length ->
                outputTy1 = outputTy2 &&
                ilCc1 = ilCc2 &&
                (inputTys1, inputTys2)
                ||> ImArray.forall2 (=)

            | Entity ent1, Entity ent2 -> ent1 = ent2
            | Variable(index1, ilKind1), Variable(index2, ilKind2) ->
                index1 = index2 && ilKind1 = ilKind2
            | HigherVariable(index1, tyArgs1, ilKind1), HigherVariable(index2, tyArgs2, ilKind2) ->
                index1 = index2 && ilKind1 = ilKind2 &&
                tyArgs1.Length = tyArgs2.Length &&
                (tyArgs1, tyArgs2)
                ||> ImArray.forall2 (=)

            | ForAll(tyPars1, ty1), ForAll(tyPars2, ty2) when tyPars1.Length = tyPars2.Length ->
                (tyPars1, tyPars2)
                ||> ImArray.forall2 (=) &&
                ty1 = ty2

            | ConstantInt32(value1), ConstantInt32(value2) ->
                value1 = value2

            | _ -> 
                false

[<Sealed;DebuggerDisplay("(witness) {Type} {TypeExtension}")>]
type RuntimeWitness(tyVarIndex: int, tyVarKind: OlyILTypeVariableKind, ty: RuntimeType, tyExt: RuntimeType, abstractFuncOpt: RuntimeFunction option) =

    member _.TypeVariableIndex = tyVarIndex

    member _.TypeVariableKind = tyVarKind

    member _.Type: RuntimeType = ty

    member _.TypeExtension: RuntimeType = tyExt

    member _.AbstractFunction = abstractFuncOpt

    override this.GetHashCode() = this.TypeExtension.GetHashCode()
    
    override this.Equals(o) =
        match o with
        | :? RuntimeWitness as witness ->
            (this : IEquatable<_>).Equals(witness)
        | _ ->
            false

    interface IEquatable<RuntimeWitness> with

        member this.Equals(witness) =
            if obj.ReferenceEquals(this, witness) then true
            else

            this.TypeVariableIndex = witness.TypeVariableIndex &&
            (
                match this.TypeVariableKind, witness.TypeVariableKind with
                | OlyILTypeVariableKind.Type, OlyILTypeVariableKind.Type
                | OlyILTypeVariableKind.Function, OlyILTypeVariableKind.Function -> true
                | _ -> false
            ) &&
            witness.Type = this.Type &&
            witness.TypeExtension = this.TypeExtension

type RuntimeFunctionKind =
    | Formal
    | Reference
    | Instance

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type internal RuntimeFunctionState =
    {
        mutable Formal: RuntimeFunction
        Enclosing: RuntimeEnclosing
        Name: string
        TypeArguments: RuntimeType imarray
        TypeParameters: RuntimeTypeParameter imarray
        Parameters: RuntimeParameter imarray
        ReturnType: RuntimeType
        Flags: OlyIRFunctionFlags
        Attributes: RuntimeAttribute imarray

        Overrides : RuntimeFunction option
        Witnesses : RuntimeWitness imarray
        Kind : RuntimeFunctionKind

        ILAssembly: OlyILReadOnlyAssembly
        ILFunctionDefinitionHandle: OlyILFunctionDefinitionHandle
    }

[<DebuggerDisplay("'{Name}' - {Kind}")>]
type RuntimeFunction internal (state: RuntimeFunctionState) =

    member _.State = state
    member _.Formal = state.Formal
    member _.Enclosing = state.Enclosing
    member _.Name = state.Name
    member _.TypeArguments = state.TypeArguments
    member _.TypeParameters = state.TypeParameters

    /// Does not include the receiver.
    member _.Parameters = state.Parameters

    member _.ReturnType = state.ReturnType
    member _.Flags = state.Flags
    member _.Attributes = state.Attributes
    member _.Overrides = state.Overrides
    member _.Witnesses: RuntimeWitness imarray = state.Witnesses
    member _.ILAssembly = state.ILAssembly
    member _.ILFunctionDefinitionHandle = state.ILFunctionDefinitionHandle
    member _.Kind = state.Kind

    member this.IsFormal = state.Kind = RuntimeFunctionKind.Formal

    member this.HasILFunctionBody =
        let ilFuncDef = state.ILAssembly.GetFunctionDefinition(state.ILFunctionDefinitionHandle)
        ilFuncDef.BodyHandle.contents.IsSome

    member this.GetILFunctionBody() =
        let ilFuncDef = state.ILAssembly.GetFunctionDefinition(state.ILFunctionDefinitionHandle)
        state.ILAssembly.GetFunctionBody(ilFuncDef.BodyHandle.contents.Value)

    member this.TryGetExternalInfo() =
        let ilAsm = state.ILAssembly
        let ilFuncDef = ilAsm.GetFunctionDefinition(state.ILFunctionDefinitionHandle)
        ilFuncDef.Attributes
        |> ImArray.tryPick (function
            | OlyILAttribute.Import(platform, path, name) ->
                let platform = ilAsm.GetStringOrEmpty(platform)
                let path = path |> ImArray.map ilAsm.GetStringOrEmpty
                let name = ilAsm.GetStringOrEmpty(name)
                Some(OlyIRFunctionExternalInfo(platform, path, name))
            | _ ->
                None
        )

    member this.AssemblyIdentity = state.ILAssembly.Identity

    member this.IsExternal = state.Flags.IsExternal

    member this.IsExported =
        if this.Flags.IsConstructor then
            this.EnclosingType.IsExported
        else
            let ilFuncDef = state.ILAssembly.GetFunctionDefinition(state.ILFunctionDefinitionHandle)
            ilFuncDef.Attributes
            |> ImArray.exists (function OlyILAttribute.Export -> true | _ -> false)

    member this.IsMutable =
        let ilFuncDef = state.ILAssembly.GetFunctionDefinition(state.ILFunctionDefinitionHandle)
        ilFuncDef.Flags.HasFlag(OlyILFunctionFlags.Mutable)

    member this.IsIntrinsic =
        let ilFuncDef = state.ILAssembly.GetFunctionDefinition(state.ILFunctionDefinitionHandle)
        ilFuncDef.Attributes
        |> ImArray.exists (function OlyILAttribute.Intrinsic _ -> true | _ -> false)

    member this.GetArgumentType(argIndex: int) =
        if this.Flags.IsInstance then
            if argIndex = 0 then
                if this.EnclosingType.IsAnyStruct then
                    if this.IsMutable then
                        RuntimeType.ByRef(this.EnclosingType, OlyIRByRefKind.ReadWrite)
                    else
                        RuntimeType.ByRef(this.EnclosingType, OlyIRByRefKind.ReadOnly)
                else
                    this.EnclosingType
            else
                this.Parameters[argIndex - 1].Type
        else
            this.Parameters[argIndex].Type

    member this.IsArgumentByRefType(argIndex: int) =
        if this.Flags.IsInstance then
            if argIndex = 0 then
                this.EnclosingType.IsAnyStruct
            else
                this.Parameters[argIndex - 1].Type.IsByRef_t
        else
            this.Parameters[argIndex].Type.IsByRef_t

    member this.IsArgumentReadWriteByRefType(argIndex: int) =
        if this.Flags.IsInstance then
            if argIndex = 0 then
                this.EnclosingType.IsAnyStruct && this.IsMutable
            else
                this.Parameters[argIndex - 1].Type.IsReadWriteByRef
        else
            this.Parameters[argIndex].Type.IsReadWriteByRef

    member this.IsArgumentReadOnlyByRefType(argIndex: int) =
        if this.Flags.IsInstance then
            if argIndex = 0 then
                this.EnclosingType.IsAnyStruct && not this.IsMutable
            else
                this.Parameters[argIndex - 1].Type.IsReadOnlyByRef
        else
            this.Parameters[argIndex].Type.IsReadOnlyByRef

    member this.IsArgumentWriteOnlyByRefType(argIndex: int) =
        if this.Flags.IsInstance then
            if argIndex = 0 then
                this.EnclosingType.IsAnyStruct && not this.IsMutable
            else
                this.Parameters[argIndex - 1].Type.IsWriteOnlyByRef
        else
            this.Parameters[argIndex].Type.IsWriteOnlyByRef

    member this.MakeInstance(enclosing: RuntimeEnclosing, funcTyArgs: RuntimeType imarray) =
        let enclosingTy = enclosing.AsType

        if not this.IsFormal then
            failwith "Expected formal function."

        //if enclosingTy.Formal <> this.EnclosingType.Formal then
        //    failwith "Invalid enclosing type."

        if state.TypeParameters.Length <> funcTyArgs.Length then
            failwith "Type argument count does not match type parameter count when applying."

        if enclosingTy.TypeParameters.IsEmpty && funcTyArgs.IsEmpty then
            this
        else if not enclosingTy.TypeParameters.IsEmpty && funcTyArgs.IsEmpty then
            OlyAssert.True(this.TypeParameters.IsEmpty)
            this.MakeReference(enclosingTy)
        else

        let genericContext = GenericContext.CreateFromEnclosingType(enclosingTy, funcTyArgs)

        { state with
            Enclosing = enclosing
            TypeArguments = funcTyArgs
            Parameters = state.Parameters |> ImArray.map (fun x -> { x with Type = x.Type.Substitute(genericContext) })
            ReturnType = state.ReturnType.Substitute(genericContext)
            Kind = RuntimeFunctionKind.Instance
        }
        |> RuntimeFunction

    member this.MakeInstance(enclosingTy: RuntimeType, funcTyArgs) =
        this.MakeInstance(RuntimeEnclosing.Type(enclosingTy), funcTyArgs)

    member this.MakeInstance(enclosingTy: RuntimeType) =
        this.MakeInstance(enclosingTy, this.TypeArguments)

    member this.MakeReference(enclosingTy: RuntimeType) =
        if not this.IsFormal then
            failwith "Expected formal function."

        //if enclosingTy.Formal <> this.EnclosingType.Formal then
        //    failwith "Invalid enclosing type."

        if enclosingTy.TypeParameters.IsEmpty then
            this
        else

        let genericContext = GenericContext.CreateFromEnclosingType(enclosingTy, this.TypeArguments)

        { state with
            Enclosing = RuntimeEnclosing.Type(enclosingTy)
            Parameters = state.Parameters |> ImArray.map (fun x -> { x with Type = x.Type.Substitute(genericContext) })
            ReturnType = state.ReturnType.Substitute(genericContext)
            Kind = RuntimeFunctionKind.Reference
        }
        |> RuntimeFunction

    /// Forces the function be considered the formal.
    member this.MakeFormal() =
        if this.IsFormal then this
        else        
            let resultState =
                { state with
                    Kind = RuntimeFunctionKind.Formal
                }
            let result = RuntimeFunction(resultState)
            resultState.Formal <- result
            result

    /// Forces the function be considered the formal.
    member this.MakeFormal(enclosingTy) =
        if this.IsFormal then this
        else        
            let resultState =
                { state with
                    Enclosing = RuntimeEnclosing.Type(enclosingTy)
                    Kind = RuntimeFunctionKind.Formal
                }
            let result = RuntimeFunction(resultState)
            resultState.Formal <- result
            result

    member this.EraseGenerics() =
        if this.TypeArguments.IsEmpty then this
        else
            if this.IsFormal then
                failwith "Unexpected formal function."

            let resultState =
                { state with
                    Kind = RuntimeFunctionKind.Formal
                    TypeArguments = ImArray.empty
                    TypeParameters = ImArray.empty
                }
            let result = RuntimeFunction(resultState)
            resultState.Formal <- result
            result

    member this.EnclosingType: RuntimeType = this.Enclosing.AsType

    member this.IsOverridesExternal =
        this.Flags.IsVirtual &&
        (
            match this.Overrides with
            | Some(func) -> func.IsExternal
            | _ -> false
        )

    member this.CanGenericsBeErased = not this.IsExternal && not this.IsExported

    /// REVIEW: Consider caching this?
    member this.ComputeSignatureKey() =
        let func = this
        let name = func.Name
        {
            Name = name
            TypeArguments = func.TypeArguments |> ImArray.map (fun x -> x :> IOlyIRTypeKey)
            ParameterTypes = func.Parameters |> ImArray.map (fun x -> x.Type)
            ReturnType = func.ReturnType
            IsStatic = func.Flags.IsStatic
            IsConstructor = func.Flags.IsConstructor
        } : OlyIRFunctionSignatureKey

    member this.SetWitnesses(witnesses: RuntimeWitness imarray) =
        if witnesses.IsEmpty || (this.EnclosingType.TypeParameters.IsEmpty && this.TypeArguments.IsEmpty) then
            // If the function is not generic, then we do not need to set its witnesses
            //    since witnesses require that a function has at least one type parameter.
            this
        else
            if this.IsFormal then
                failwith "Unexpected formal function."

            let filteredWitnesses =
                this.TypeArguments
                |> ImArray.mapi (fun i tyArg ->
                    witnesses
                    |> ImArray.choose (fun (witness: RuntimeWitness) ->
                        if witness.Type.StripAlias() = tyArg.StripAlias() then
                            RuntimeWitness(i, OlyILTypeVariableKind.Function, witness.Type, witness.TypeExtension, witness.AbstractFunction)
                            |> Some
                        else
                            None
                    )
                )
                |> ImArray.concat
                |> ImArray.distinct
            if filteredWitnesses.IsEmpty then
                this
            else
                let state = this.State

                { state with 
                    Witnesses = filteredWitnesses
                    Parameters = 
                        state.Parameters 
                        |> ImArray.map (fun x -> { x with Type = x.Type.SetWitnesses(filteredWitnesses) })
                    ReturnType = state.ReturnType.SetWitnesses(filteredWitnesses)
                }
                |> RuntimeFunction

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? RuntimeFunction as func ->
            (this : IEquatable<_>).Equals(func)
        | _ ->
            false

    interface IEquatable<RuntimeFunction> with
        
        member this.Equals(func: RuntimeFunction) =
            if obj.ReferenceEquals(this, func) then true
            else
            // REVIEW: This is fine since it is correct, though we could only rely on reference equality
            //         if the runtime was maintaining single instances of generic instantiations, which it does not.
            this.EnclosingType = func.EnclosingType &&
            (this.ComputeSignatureKey() = func.ComputeSignatureKey()) &&
            this.Witnesses.Length = func.Witnesses.Length &&
            (
                (this.Witnesses, func.Witnesses)
                ||> ImArray.forall2 (=)
            )

[<ReferenceEquality;NoComparison;RequireQualifiedAccess;DebuggerDisplay("{Name}")>]
type RuntimeAttribute =
    {
        Constructor: RuntimeFunction
        Arguments: OlyILConstant imarray
        NamedArguments: OlyILAttributeNamedArgument imarray
    }

    member this.Name = this.Constructor.Name
    
[<CustomEquality;NoComparison;RequireQualifiedAccess;DebuggerDisplay("{Name}")>]
type RuntimeField =
    {
        mutable Formal: RuntimeField
        EnclosingType: RuntimeType
        Name: string
        Flags: OlyIRFieldFlags
        Type: RuntimeType
        Attributes: RuntimeAttribute imarray
        Index: int

        ILConstant: OlyILConstant option
        ILAssembly: OlyILReadOnlyAssembly
        ILFieldDefinitionHandle : OlyILFieldDefinitionHandle
    }

    member this.IsFormal =
        obj.ReferenceEquals(this, this.Formal)

    member this.AssemblyIdentity = this.ILAssembly.Identity

    member this.IsExternal =
        let ilFieldDef = this.ILAssembly.GetFieldDefinition(this.ILFieldDefinitionHandle)
        match ilFieldDef with
        | OlyILFieldDefinition.OlyILFieldDefinition(attrs=ilAttrs) -> ilAttrs
        | _ ->
            ImArray.empty

    member this.IsStatic =
        not this.Flags.IsInstance

    member this.IsMutable = this.Flags.IsMutable

    member this.ILConstantValueOption =
        let ilFieldDef = this.ILAssembly.GetFieldDefinition(this.ILFieldDefinitionHandle)
        match ilFieldDef with
        | OlyILFieldDefinition.OlyILFieldConstant(constant=constant) -> Some constant
        | _ -> None

    member this.Substitute(enclosingTy: RuntimeType) =
        this.Formal.Apply(enclosingTy)

    member this.Apply(enclosingTy: RuntimeType) =
        if not this.IsFormal then
            failwith "Expected formal field."

        if enclosingTy.IsFormal then
            this
        else
            let genericContext = GenericContext.Create(enclosingTy.TypeArguments).SetPassedWitnesses(enclosingTy.Witnesses)

            { this with
                EnclosingType = enclosingTy
                Type = this.Type.Substitute(genericContext)
            }

    member this.TryGetImportInfo() =
        let resultOpt =
            let fieldDef = this.ILAssembly.GetFieldDefinition(this.ILFieldDefinitionHandle)
            fieldDef.Attributes
            |> ImArray.tryPick (function OlyILAttribute.Import(platform, path, name) -> Some(platform, path, name) | _ -> None)
        match resultOpt with
        | Some(platform, path, name) ->
            let ilAsm = this.ILAssembly
            let name = ilAsm.GetStringOrEmpty(name)
            Some(name)
        | _ ->
            None

    override this.GetHashCode() = this.Index

    override this.Equals(o) =
        match o with
        | :? RuntimeField as field ->
            (this : IEquatable<_>).Equals(field)
        | _ ->
            false

    interface IEquatable<RuntimeField> with

        member this.Equals(field: RuntimeField) =
            if obj.ReferenceEquals(this, field) then true
            else

            // We do not need to check the field type as they should be the same if the following below are true.
            this.Index = field.Index &&
            this.Name = field.Name &&
            this.EnclosingType = field.EnclosingType
#if DEBUG || CHECKED
            &&
            (
                OlyAssert.True(this.Type = field.Type)
                true
            )
#endif
type RuntimeType with

    member this.StripAll() =
        this.StripAliasAndNewtypeAndEnum()

    member this.StripAlias(): RuntimeType =
        if this.IsAlias && this.Extends.Length = 1 then
            this.Extends.[0].StripAlias()
        else
            this

    member this.StripAliasAndNewtype(): RuntimeType =
        if this.IsAlias && this.Extends.Length = 1 then
            this.Extends.[0].StripAliasAndNewtype()
        elif this.IsNewtype then
            this.RuntimeType.Value.StripAliasAndNewtype()
        else
            this

    member this.StripAliasAndNewtypeAndEnum(): RuntimeType =
        if this.IsAlias && this.Extends.Length = 1 then
            this.Extends.[0].StripAliasAndNewtypeAndEnum()
        elif this.IsEnumOrNewtype then
            this.RuntimeType.Value.StripAliasAndNewtypeAndEnum()
        else
            this

let getAllDistinctImplements (ty: RuntimeType) : RuntimeType imarray =
    let result = ty.Implements
    let result2 =
        result
        |> ImArray.map (fun x ->
            getAllDistinctImplements x
        )
        |> Seq.concat
    Seq.append result result2
    |> Seq.distinct
    |> ImArray.ofSeq

let getAllDistinctExtends (ty: RuntimeType) : RuntimeType imarray =
    let result = ty.Extends
    let result2 =
        result
        |> ImArray.map (fun x ->
            getAllDistinctExtends x
        )
        |> Seq.concat
    Seq.append result result2
    |> Seq.distinct
    |> ImArray.ofSeq

let getAllDistinctInheritsAndImplements (ty: RuntimeType) : RuntimeType imarray =
    let result = ty.Extends.AddRange(ty.Implements)
    let result2 =
        result
        |> ImArray.map (fun x ->
            getAllDistinctInheritsAndImplements x
        )
        |> Seq.concat
    Seq.append result result2
    |> Seq.distinct
    |> ImArray.ofSeq

let subsumesType (superTy: RuntimeType) (ty: RuntimeType) =
    if superTy.StripAlias().IsObjectType then true
    else
        if superTy = ty then true
        else
            let possibleTys = getAllDistinctInheritsAndImplements ty
            possibleTys
            |> ImArray.exists (fun x -> superTy = x)

[<Sealed>]
type RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Value>() =  
    static let comparer =
        { new IEqualityComparer<RuntimeType imarray> with
            member _.GetHashCode(x) = x.Length
            member _.Equals(x1, x2) =
                if x1.Length = x2.Length then
                    (x1, x2)
                    ||> ImArray.forall2 (fun x1 x2 ->
                        x1 = x2
                    )
                else
                    false
        }

    let table = Dictionary<RuntimeType imarray, 'Value>(comparer)

    member this.Item 
        with get key = table.[key]
        and set key value =
            table.[key] <- value

    member this.TryGetValue(key) =
        match table.TryGetValue(key) with
        | true, res -> ValueSome res
        | _ -> ValueNone

[<Sealed>]
type RuntimeEntityDefinitionTypeArgumentWitnessListTable<'Type, 'Function, 'Field, 'Value>() =  
    static let comparer =
        { new IEqualityComparer<struct(RuntimeType imarray * RuntimeWitness imarray)> with
            member _.GetHashCode((tyArgs, witnesses)) = tyArgs.Length + witnesses.Length
            member _.Equals((tyArgs1, witnesses1), (tyArgs2, witnesses2)) =
                if tyArgs1.Length = tyArgs2.Length && witnesses1.Length = witnesses2.Length then
                    let tyArgsAreEqual =
                        (tyArgs1, tyArgs2)
                        ||> ImArray.forall2 (=)

                    let witnessesAreEqual =
                        (witnesses1, witnesses2)
                        ||> ImArray.forall2 (fun witness1 witness2 ->
                            witness1.Type = witness2.Type &&
                            witness1.TypeExtension = witness2.TypeExtension
                        )

                    tyArgsAreEqual && witnessesAreEqual
                else
                    false
        }

    let table = Dictionary<struct(RuntimeType imarray * RuntimeWitness imarray), 'Value>(comparer)

    member this.Item 
        with get key = table.[key]
        and set key value =
            table.[key] <- value

    member this.TryGetValue(key) =
        match table.TryGetValue(key) with
        | true, res -> ValueSome res
        | _ -> ValueNone

[<Sealed>]
type RuntimeTypeArgumentWitnessListTable<'Type, 'Function, 'Field, 'Value>() =  
    static let comparer =
        { new IEqualityComparer<struct(bool * RuntimeType * RuntimeType imarray * RuntimeWitness imarray * bool)> with
            member _.GetHashCode((_, enclosingTy, tyArgs, witnesses, _)) = enclosingTy.TypeArguments.Length + tyArgs.Length + witnesses.Length
            member _.Equals((isErased1, enclosingTy1, tyArgs1, witnesses1, isFormal1), (isErased2, enclosingTy2, tyArgs2, witnesses2, isFormal2)) =
                if isFormal1 = isFormal2 && isErased1 = isErased2 && tyArgs1.Length = tyArgs2.Length && witnesses1.Length = witnesses2.Length && enclosingTy1 = enclosingTy2 then
                    let tyArgsAreEqual =
                        (tyArgs1, tyArgs2)
                        ||> ImArray.forall2 (=)

                    let witnessesAreEqual =
                        (witnesses1, witnesses2)
                        ||> ImArray.forall2 (fun witness1 witness2 ->
                            witness1.Type = witness2.Type &&
                            witness1.TypeExtension = witness2.TypeExtension
                        )

                    tyArgsAreEqual && witnessesAreEqual
                else
                    false
        }

    let table = Dictionary<struct(bool * RuntimeType * RuntimeType imarray * RuntimeWitness imarray * bool), 'Value>(comparer)

    member this.Item 
        with get key = table.[key]
        and set key value =
            table.[key] <- value

    member this.TryGetValue(key) =
        match table.TryGetValue(key) with
        | true, res -> ValueSome res
        | _ -> ValueNone

[<Sealed>]
type RuntimeFieldReferenceCache<'Type, 'Function, 'Field>() =  
    static let comparer =
        { new IEqualityComparer<RuntimeField> with
            member _.GetHashCode(x) = x.ILFieldDefinitionHandle.Index
            member _.Equals(x1, x2) =
                if obj.ReferenceEquals(x1, x2) then
                    true
                elif x1.EnclosingType = x2.EnclosingType then
                    x1.Name = x2.Name &&
                    x1.Type = x2.Type
                else
                    false
        }

    let table = Dictionary<RuntimeField, RuntimeField>(comparer)

    member this.Intern(field: RuntimeField) =
        match table.TryGetValue(field) with
        | true, result -> result
        | _ ->
            table[field] <- field
            field

exception GenericRecursionLimitReached of message: string