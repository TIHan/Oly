[<AutoOpen>]
module internal rec Oly.Runtime.CodeGen.InternalTypes

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable
open System.Diagnostics
open Oly.Runtime
open Oly.Metadata
open Oly.Core
open Oly.Core.TaskExtensions

[<NoEquality;NoComparison>]
type GenericContext =
    private {
        enclosingTyArgs: RuntimeType imarray
        funcTyArgs: RuntimeType imarray
        isTyErasing: bool
        isFuncErasing: bool
        passedWitnesses: RuntimeWitness imarray
    }

    static member Default =
        {
            enclosingTyArgs = ImArray.empty
            funcTyArgs = ImArray.empty
            isTyErasing = false
            isFuncErasing = false
            passedWitnesses = ImArray.empty
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
            { GenericContext.Default with isTyErasing = true }
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

    member this.Set(enclosingTyArgs, funcTyArgs) =
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
                this.isTyErasing

        | OlyILTypeVariableKind.Function ->
            // Context has no type arguments so any call to this will return false.
            if this.funcTyArgs.IsEmpty then false
            else
                if i < 0 || i >= this.funcTyArgs.Length then
                    raise(IndexOutOfRangeException())
                this.isFuncErasing

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
    }

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        if obj.ReferenceEquals(this, o) then true
        else

        match o with
        | :? RuntimeTypeParameter as o ->
            this.Name = o.Name &&
            this.Arity = o.Arity &&
            this.IsVariadic = o.IsVariadic
        | _ ->
            false

[<NoComparison;CustomEquality;RequireQualifiedAccess>]
type RuntimeParameter =
    {
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

[<NoComparison;CustomEquality;RequireQualifiedAccess>]
type RuntimeEntity =
    {
        Enclosing: RuntimeEnclosing
        Name: string
        TypeParameters: RuntimeTypeParameter imarray
        TypeArguments: RuntimeType imarray
        Witnesses: RuntimeWitness imarray
        mutable Extends: RuntimeType imarray
        mutable Implements: RuntimeType imarray
        mutable RuntimeType: RuntimeType option
        mutable Formal: RuntimeEntity
        mutable Fields: RuntimeField imarray
        mutable Attributes: RuntimeAttribute imarray

        mutable StaticConstructor: RuntimeFunction option

        ILAssembly: OlyILReadOnlyAssembly
        ILEntityDefinitionHandle: OlyILEntityDefinitionHandle
        ILEntityKind: OlyILEntityKind
        ILEntityFlags: OlyILEntityFlags
        ILPropertyDefinitionLookup: ImmutableDictionary<OlyILFunctionDefinitionHandle, OlyILPropertyDefinitionHandle>
    }

    member this.SetWitnesses(witnesses: RuntimeWitness imarray) =
        // Imported types do not support witnesses.
        if ((witnesses.IsEmpty && this.Witnesses.IsEmpty) || this.TypeArguments.IsEmpty || this.IsImported) then
            this
        else
            let filteredWitnesses =
                this.TypeArguments
                |> ImArray.mapi (fun i tyArg ->
                    witnesses
                    |> ImArray.choose (fun (witness: RuntimeWitness) ->
                        if witness.Type.StripAlias() = tyArg.StripAlias() then
                            match witness.TypeVariableKind with
                            | OlyILTypeVariableKind.Type when i = witness.TypeVariableIndex ->
                                Some witness
                            | _ ->
                                None
                        else
                            None
                    )
                )
                |> ImArray.concat
                |> ImArray.distinct

            let entNew =
                { this with Witnesses = filteredWitnesses }

            let fields =
                let enclosingTy = RuntimeType.Entity(entNew)
                this.Fields
                |> ImArray.map (fun x ->
                    x.Substitute(enclosingTy)
                )

            entNew.Fields <- fields
            entNew

    member this.AssemblyIdentity = this.ILAssembly.Identity

    member this.IsAnyStruct =
        if this.IsEnum then
            this.RuntimeType.Value.IsAnyStruct
        else
            this.ILEntityKind = OlyILEntityKind.Struct ||
            (
                (this.IsTypeExtension || this.IsNewtype || this.IsAlias) && not this.Extends.IsEmpty && this.Extends.[0].IsAnyStruct
            )

    member this.IsEnum =
        this.ILEntityKind = OlyILEntityKind.Enum

    member this.IsNewtype =
        this.ILEntityKind = OlyILEntityKind.Newtype

    member this.IsTypeExtension =
        this.ILEntityKind = OlyILEntityKind.TypeExtension

    member this.IsClosure =
        this.ILEntityKind = OlyILEntityKind.Closure

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
        |> ImArray.exists (function OlyILAttribute.Export _ -> true | _ -> false)

    member this.IsObjectType =
        let entDef = this.ILAssembly.GetEntityDefinition(this.ILEntityDefinitionHandle)
        entDef.Attributes
        |> ImArray.exists (function 
            | OlyILAttribute.Intrinsic(nameHandle) ->
                this.ILAssembly.GetStringOrEmpty(nameHandle) = "base_object"
            | _ -> 
                false
        )

    member this.IsIntrinsic =
        let entDef = this.ILAssembly.GetEntityDefinition(this.ILEntityDefinitionHandle)
        entDef.Attributes
        |> ImArray.exists (function OlyILAttribute.Intrinsic _ -> true | _ -> false)

    member this.TryGetIntrinsicTypeInfo() =
        let entDef = this.ILAssembly.GetEntityDefinition(this.ILEntityDefinitionHandle)
        entDef.Attributes
        |> ImArray.tryPick (function 
            | OlyILAttribute.Intrinsic(nameHandle) -> 
                Some(this.ILAssembly.GetStringOrEmpty(nameHandle))
            | _ -> 
                None
        )

    member this.Substitute(genericContext: GenericContext) =
        if genericContext.IsEmpty then
            this
        else
            let tyArgs =
                this.TypeArguments
                |> ImArray.map (fun x -> x.Substitute(genericContext))

            let extends =
                this.Extends
                |> ImArray.map (fun x ->
                    x.Substitute(genericContext)
                )

            let implements =
                this.Implements
                |> ImArray.map (fun x ->
                    x.Substitute(genericContext)
                )

            let entNew =
                { this with
                    Enclosing = this.Enclosing.Substitute(genericContext)
                    Extends = extends
                    Implements = implements
                    TypeArguments = tyArgs }

            let fields =
                let enclosingTy = RuntimeType.Entity(entNew)
                this.Fields
                |> ImArray.map (fun x ->
                    x.Substitute(enclosingTy)
                )

            entNew.Fields <- fields
            entNew

    member this.Apply(tyArgs: RuntimeType imarray) =
        if this.TypeParameters.Length <> tyArgs.Length then
            failwith "Type argument count does not match type parameter count when applying."

        if not this.IsFormal then
            failwith "Expected formal entity."

        if tyArgs.IsEmpty then
            this
        else
            let genericContext = GenericContext.Create(tyArgs)
            let extends =
                this.Extends
                |> ImArray.map (fun x ->
                    x.Substitute(genericContext)
                )

            let implements =
                this.Implements
                |> ImArray.map (fun x ->
                    x.Substitute(genericContext)
                )

            let entNew =
                { this with
                    Enclosing = this.Enclosing.Substitute(genericContext)
                    Extends = extends
                    Implements = implements
                    TypeArguments = tyArgs }

            let fields =
                let enclosingTy = RuntimeType.Entity(entNew)
                this.Fields
                |> ImArray.map (fun x ->
                    x.Apply(enclosingTy)
                )

            entNew.Fields <- fields
            entNew

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? RuntimeEntity as o ->
            obj.ReferenceEquals(this.Formal, o.Formal) && this.TypeArguments.Length = o.TypeArguments.Length &&
            (
                (this.TypeArguments, o.TypeArguments)
                ||> ImArray.forall2 (=)
            ) && this.Witnesses.Length = o.Witnesses.Length &&
            (
                (this.Witnesses, o.Witnesses)
                ||> ImArray.forall2 (=)
            )
        | _ ->
            false

let emptyEnclosing = RuntimeEnclosing.Namespace(ImArray.empty)

[<NoComparison;CustomEquality;RequireQualifiedAccess;DebuggerDisplay("{Name}")>]
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
    | Array of elementTy: RuntimeType * rank: int * isMutable: bool
    | Function of argTys: RuntimeType imarray * returnTy: RuntimeType
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

    member this.IsByRefLike = this.IsByRef_t

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

    member this.IsReadOnlyByRef =
        match this.StripAlias() with
        | ByRef(_, OlyIRByRefKind.Read) -> true
        | _ -> false

    member this.IsReadWriteByRef =
        match this.StripAlias() with
        | ByRef(_, OlyIRByRefKind.ReadWrite) -> true
        | _ -> false

    member this.Witnesses =
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
        | Entity(ent) -> Entity(ent.Formal)
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
        | Utf16 _ -> "__oly_utf16"
        | Char16 -> "__oly_char16"
        | ReferenceCell _ -> "__oly_reference_cell"
        | Function _ -> "__oly_func"
        | Array _ -> "__oly_array"
        | BaseObject -> "__oly_base_object"
        | ForAll _ -> "__oly_for_all"
        | Entity(ent) -> ent.Name
        | Variable(index, ilKind) -> $"__oly_type_variable_{index}_{ilKind}"
        | HigherVariable(index, _, ilKind) -> $"__oly_higher_variable_{index}_{ilKind}"
        | ByRef(elementTy, _) -> $"__oly_by_reference_{elementTy.Name}"
        | ConstantInt32(value) -> value.ToString()

    member this.TypeArguments =
        match this with
        | Entity(ent) -> ent.TypeArguments
        | HigherVariable(tyArgs=tyArgs) -> tyArgs
        | Tuple(tyArgs, _) -> tyArgs
        | ReferenceCell(elementTy)
        | ByRef(elementTy, _)
        | NativePtr(elementTy) -> ImArray.createOne elementTy
        | Array(elementTy, _, _) -> ImArray.createOne elementTy
        | Function(argTys, returnTy) 
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
        | NativeInt _
        | NativeUInt _
        | NativePtr _ 
        | NativeFunctionPtr _ -> true
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
        | NativePtr _ -> ImArray.createOne({ Name = ""; Arity = 0; IsVariadic = false; ILConstraints = ImArray.empty })
        | Tuple _ -> ImArray.createOne({ Name = ""; Arity = 0; IsVariadic = true; ILConstraints = ImArray.empty })
        | Function _ 
        | NativeFunctionPtr _ ->
            ImArray.init this.TypeArguments.Length (fun i -> { Name = ""; Arity = 0; IsVariadic = false; ILConstraints = ImArray.empty })
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
        | Function(argTys, returnTy) ->
            Function(argTys |> ImArray.map (fun x -> x.Substitute(genericContext)), returnTy.Substitute(genericContext))
        | NativeFunctionPtr(ilCc, argTys, returnTy) ->
            NativeFunctionPtr(ilCc, argTys |> ImArray.map (fun x -> x.Substitute(genericContext)), returnTy.Substitute(genericContext))
        | ReferenceCell(elementTy) ->
            ReferenceCell(elementTy.Substitute(genericContext))
        | Array(elementTy, rank, isMutable) ->
            Array(elementTy.Substitute(genericContext), rank, isMutable)
        | Entity(ent) ->
            if ent.IsTypeConstructor then
                this
            else
                if ent.TypeParameters.IsEmpty then
                    Entity(ent)
                else
                    Entity(ent.Substitute(genericContext))
        | ByRef(elementTy, kind) ->
            ByRef(elementTy.Substitute(genericContext), kind)
        | NativePtr(elementTy) ->
            NativePtr(elementTy.Substitute(genericContext))
        | _ ->
            this

    member this.Apply(tyArgs: RuntimeType imarray) =
#if DEBUG
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

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        if obj.ReferenceEquals(this, o) then true
        else

        match o with
        | :? RuntimeType as o ->
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
            | Tuple(tyArgs1, _), Tuple(tyArgs2, _) when tyArgs1.Length = tyArgs2.Length ->
                (tyArgs1, tyArgs2)
                ||> ImArray.forall2 (=)

            | ByRef(elementTy1, kind1), ByRef(elementTy2, kind2) -> elementTy1 = elementTy2 && kind1 = kind2

            | Function(inputTy1, outputTy1), Function(inputTy2, outputTy2) ->
                inputTy1 = inputTy2 &&
                outputTy1 = outputTy2

            | NativeFunctionPtr(ilCc1, inputTy1, outputTy1), NativeFunctionPtr(ilCc2, inputTy2, outputTy2) ->
                ilCc1 = ilCc2 &&
                inputTy1 = inputTy2 &&
                outputTy1 = outputTy2

            | Entity ent1, Entity ent2 -> ent1 = ent2
            | Variable(index1, ilKind1), Variable(index2, ilKind2) ->
                index1 = index2 && ilKind1 = ilKind2
            | HigherVariable(index1, tyArgs1, ilKind1), HigherVariable(index2, tyArgs2, ilKind2) ->
                index1 = index2 && ilKind1 = ilKind2 &&
                tyArgs1.Length = tyArgs2.Length &&
                (tyArgs1, tyArgs2)
                ||> ImArray.forall2 (=)

            | _ -> 
                false

        | _ ->
            false

[<Sealed;DebuggerDisplay("(witness) {Type} {TypeExtension}")>]
type RuntimeWitness(tyVarIndex: int, tyVarKind: OlyILTypeVariableKind, ty: RuntimeType, tyExt: RuntimeType, abstractFuncOpt: RuntimeFunction option) =

    member _.TypeVariableIndex = tyVarIndex

    member _.TypeVariableKind = tyVarKind

    member _.Type: RuntimeType = ty

    member _.TypeExtension = tyExt

    member _.AbstractFunction = abstractFuncOpt

    override this.GetHashCode() = this.TypeExtension.GetHashCode()
    
    override this.Equals(o) =
        match o with
        | :? RuntimeWitness as witness ->
            this.TypeVariableIndex = witness.TypeVariableIndex &&
            (
                match this.TypeVariableKind, witness.TypeVariableKind with
                | OlyILTypeVariableKind.Type, OlyILTypeVariableKind.Type
                | OlyILTypeVariableKind.Function, OlyILTypeVariableKind.Function -> true
                | _ -> false
            ) &&
            witness.Type = this.Type &&
            witness.TypeExtension = this.TypeExtension
        | _ ->
            false

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
    member _.Witnesses = state.Witnesses
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

    member this.IsExported = state.Enclosing.IsExported

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
                        RuntimeType.ByRef(this.EnclosingType, OlyIRByRefKind.Read)
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

    member this.IsArgumentReadOnlyByRefType(argIndex: int) =
        if this.Flags.IsInstance then
            if argIndex = 0 then
                this.EnclosingType.IsAnyStruct && not this.IsMutable
            else
                this.Parameters[argIndex - 1].Type.IsReadOnlyByRef
        else
            this.Parameters[argIndex].Type.IsReadOnlyByRef

    member this.IsArgumentReadWriteByRefType(argIndex: int) =
        if this.Flags.IsInstance then
            if argIndex = 0 then
                this.EnclosingType.IsAnyStruct && this.IsMutable
            else
                this.Parameters[argIndex - 1].Type.IsReadWriteByRef
        else
            this.Parameters[argIndex].Type.IsReadWriteByRef

    member this.MakeInstance(enclosing: RuntimeEnclosing, funcTyArgs: RuntimeType imarray) =
        let enclosingTy = enclosing.AsType

        if not this.IsFormal then
            failwith "Expected formal function."

        if enclosingTy.Formal <> this.EnclosingType.Formal then
            failwith "Invalid enclosing type."

        if state.TypeParameters.Length <> funcTyArgs.Length then
            failwith "Type argument count does not match type parameter count when applying."

        if enclosingTy.TypeParameters.IsEmpty && funcTyArgs.IsEmpty then
            this
        else if not enclosingTy.TypeParameters.IsEmpty && funcTyArgs.IsEmpty then
            OlyAssert.True(this.TypeParameters.IsEmpty)
            this.MakeReference(enclosingTy)
        else

        let genericContext = 
            GenericContext.Create(enclosingTy.TypeArguments, funcTyArgs)

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

        if enclosingTy.Formal <> this.EnclosingType.Formal then
            failwith "Invalid enclosing type."

        if enclosingTy.TypeParameters.IsEmpty then
            this
        else

        let genericContext = 
            GenericContext.Create(enclosingTy.TypeArguments, this.TypeArguments)

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

    member this.CanGenericsBeErased =
        not this.IsExternal && (not (this.IsOverridesExternal && this.Witnesses.IsEmpty)) &&
        this.EnclosingType.CanGenericsBeErased

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

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(o) =
        if obj.ReferenceEquals(this, o) then
            true
        else
            // REVIEW: This is fine since it is correct, though we could only rely on reference equality
            //         if the runtime was maintaining single instances of generic instantiations, which it does not.
            match o with
            | :? RuntimeFunction as func ->
                this.EnclosingType = func.EnclosingType &&
                (this.ComputeSignatureKey() = func.ComputeSignatureKey()) &&
                this.Witnesses.Length = func.Witnesses.Length &&
                (
                    (this.Witnesses, func.Witnesses)
                    ||> ImArray.forall2 (=)
                )
            | _ ->
                false

[<ReferenceEquality;NoComparison;RequireQualifiedAccess;DebuggerDisplay("{Name}")>]
type RuntimeAttribute =
    {
        Constructor: RuntimeFunction
        Arguments: OlyILConstant imarray
        NamedArguments: OlyILAttributeNamedArgument imarray
    }

    member this.Name = this.Constructor.Name
    
[<ReferenceEquality;NoComparison;RequireQualifiedAccess;DebuggerDisplay("{Name}")>]
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

        let genericContext = GenericContext.Create(enclosingTy.TypeArguments)

        { this with
            EnclosingType = enclosingTy
            Type = this.Type.Substitute(genericContext)
        }

type RuntimeType with

    member this.StripAlias(): RuntimeType =
        if this.IsAlias && this.Extends.Length = 1 then
            this.Extends.[0].StripAlias()
        else
            this

    member this.StripAliasAndNewtype(): RuntimeType =
        if (this.IsAlias || this.IsNewtype) && this.Extends.Length = 1 then
            this.Extends.[0].StripAliasAndNewtype()
        else
            this

    member this.StripAliasAndNewtypeAndEnum(): RuntimeType =
        if (this.IsAlias || this.IsNewtype) && this.Extends.Length = 1 then
            this.Extends.[0].StripAliasAndNewtypeAndEnum()
        elif this.IsEnum then
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
    if ty.IsNewtype then
        ImArray.empty
    else
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
        let superTy = superTy.SetWitnesses(ImArray.empty) // TODO: We do this so the subsumption passes. We should instead not use 'subsumesType' at the site where the types have witnesses.
        if superTy = ty then true
        else
            let possibleTys = getAllDistinctInheritsAndImplements ty
            possibleTys
            |> ImArray.exists (fun x -> superTy = x)

[<Sealed>]
type RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Value>() =  
    let comparer =
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

    let table = ConcurrentDictionary<RuntimeType imarray, 'Value>(comparer)

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
    let comparer =
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

    let table = ConcurrentDictionary<struct(RuntimeType imarray * RuntimeWitness imarray), 'Value>(comparer)

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
    let comparer =
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

    let table = ConcurrentDictionary<struct(bool * RuntimeType * RuntimeType imarray * RuntimeWitness imarray * bool), 'Value>(comparer)

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
    let comparer =
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

    let table = ConcurrentDictionary<RuntimeField, RuntimeField>(comparer)

    member this.Intern(field: RuntimeField) =
        match table.TryGetValue(field) with
        | true, result -> result
        | _ ->
            table[field] <- field
            field