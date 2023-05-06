namespace rec Oly.Metadata

open System
open System.Collections.Concurrent
open Oly.Core

type OlyILTableKind =
    | String
    | EntityReference
    | EntityDefinition
    | FunctionSpecification
    | FunctionDefinition
    | FieldDefinition
    | PropertyDefinition
    | PatternDefinition

    | DebugSource

    | FunctionBody

[<Struct>]
type OlyILTableIndex = OlyILTableIndex of OlyILTableKind * index: int32 with

    member this.IsNil =
        this.Index = -1

    member this.Kind =
        match this with
        | OlyILTableIndex(kind, _) -> kind

    member this.Index =
        match this with
        | OlyILTableIndex(index=index) -> index

    static member CreateString(index) =
        OlyILTableIndex(OlyILTableKind.String, index)

    static member CreateEntityReference(index) =
        OlyILTableIndex(OlyILTableKind.EntityReference, index)

    static member CreateEntityDefinition(index) =
        OlyILTableIndex(OlyILTableKind.EntityDefinition, index)

    static member CreateFunctionSpecification(index) =
        OlyILTableIndex(OlyILTableKind.FunctionSpecification, index)

    static member CreateFunctionDefinition(index) =
        OlyILTableIndex(OlyILTableKind.FunctionDefinition, index)

    static member CreateFunctionBody(index) =
        OlyILTableIndex(OlyILTableKind.FunctionBody, index)

    static member CreateFieldDefinition(index) =
        OlyILTableIndex(OlyILTableKind.FieldDefinition, index)

type OlyILStringHandle = OlyILTableIndex
type OlyILEntityReferenceHandle = OlyILTableIndex
type OlyILEntityDefinitionHandle = OlyILTableIndex
type OlyILEntityDefinitionOrReferenceHandle = OlyILTableIndex
type OlyILFunctionSpecificationHandle = OlyILTableIndex
type OlyILFunctionDefinitionHandle = OlyILTableIndex
type OlyILFieldDefinitionHandle = OlyILTableIndex
type OlyILPropertyDefinitionHandle = OlyILTableIndex
type OlyILPatternDefinitionHandle = OlyILTableIndex

type OlyILDebugSourceHandle = OlyILTableIndex

type OlyILFunctionBodyHandle = OlyILTableIndex

[<RequireQualifiedAccess>]
type OlyILEntityKind =
    | Module
    | TypeExtension
    | Alias
    | Class
    | Interface
    | Shape
    | Struct
    | Enum
    | Closure
    | Attribute
    | Newtype

[<Flags>]
type OlyILEntityFlags =
    | None         = 0x0000000

    | Public       = 0x00000000
    | Internal     = 0x00000001
    | Private      = 0x00000002
    | AccessorMask = 0x00000007

    | Abstract     = 0x00000100
    | Final        = 0x00001000
    | AutoOpen     = 0x00010000
    | Nullable     = 0x00100000

[<Flags>]
type OlyILMemberFlags =
    | None =              0x00000000

    | Public            = 0x00000000
    | Internal          = 0x00000001
    | Private           = 0x00000002
    | Protected         = 0x00000003
    | AccessorMask      = 0x00000007

    | Abstract =          0x00000110
    | Virtual =           0x00000100
    | Static =            0x00001000
    | Final =             0x00010000
    | NewSlot =           0x00100000

[<Flags>]
type OlyILFunctionFlags =
    | None                          = 0x000000
    | Constructor                   = 0x000001
    | Inline                        = 0x000010
    | InlineNever                   = 0x000020
    | InlineAlways                  = 0x000030
    | InlineMask                    = 0x000070

    /// Marks the function as 'mutable'.
    /// Runtime only cares about this flag if the enclosing type is a struct or shape, and the function is an instance member and not a constructor,
    ///     otherwise, it will ignore it.
    | Mutable                       = 0x000100
    | Pure                          = 0x000200
    | RequiresExplicitTypeArguments = 0x001000
    | ParameterLess                 = 0x010000
    | StackEmplace                  = 0x100030

[<Flags>]
type OlyILFieldFlags =
    | None =        0x000000
    | Mutable =     0x000001

[<Flags>]
type OlyILCallingConvention =
  | Default   = 0uy
  | CDecl     = 1uy
  | StdCall   = 2uy
  | ThisCall  = 3uy
  | FastCall  = 4uy
  | VarArgs   = 5uy
  | Blittable = 9uy

[<Struct;NoEquality;NoComparison>]
type OlyILEntityReference(enclosing: OlyILEnclosing, nameHandle: OlyILStringHandle, tyParCount: int) =

    member _.Enclosing = enclosing
    member _.NameHandle = nameHandle
    member _.TypeParameterCount = tyParCount

    member _.FullTypeParameterCount =
        enclosing.TypeArgumentCount + tyParCount

[<RequireQualifiedAccess>]
type OlyILAttributeNamedArgumentKind =
    | Property
    | Field

[<RequireQualifiedAccess;NoEquality;NoComparison>]
type OlyILAttributeNamedArgument =
    {
        Kind: OlyILAttributeNamedArgumentKind
        NameHandle: OlyILStringHandle
        Constant: OlyILConstant
    }

[<NoEquality;NoComparison>]
[<RequireQualifiedAccess>]
type OlyILAttribute =
    | Import of platform: OlyILStringHandle * path: OlyILStringHandle imarray * name: OlyILStringHandle
    | Export
    | Intrinsic of name: OlyILStringHandle
    | Constructor of OlyILFunctionInstance * args: OlyILConstant imarray * namedArgs: OlyILAttributeNamedArgument imarray

[<NoEquality;NoComparison>]
type OlyILEntityDefinition =
    | OlyILEntityDefinition of 
        kind: OlyILEntityKind * 
        flags: OlyILEntityFlags *
        attrs: OlyILAttribute imarray *
        enclosing: OlyILEnclosing * 
        name: OlyILStringHandle * 
        tyPars: OlyILTypeParameter imarray *
        funcDefs: OlyILFunctionDefinitionHandle imarray *
        fieldDefs: OlyILFieldDefinitionHandle imarray * 
        propDefs: OlyILPropertyDefinitionHandle imarray *
        patDefs: OlyILPatternDefinitionHandle imarray *
        entDefs: OlyILEntityDefinitionHandle imarray *
        implements: OlyILType imarray *
        extends: OlyILType imarray *
        runtimeTyOpt: OlyILType option

    member this.UpdateKind(kind: OlyILEntityKind) =
        match this with
        | OlyILEntityDefinition(_, flags, attrs, enclosing, name, tyPars, funcDefs, fieldDefs, propDefs, patDefs, entDefs, implements, inherits, runtimeTyOpt) ->
            OlyILEntityDefinition(kind, flags, attrs, enclosing, name, tyPars, funcDefs, fieldDefs, propDefs, patDefs, entDefs, implements, inherits, runtimeTyOpt)

    member this.EntityDefinitionHandles =
        match this with
        | OlyILEntityDefinition(entDefs=entDefs) -> entDefs

    member this.FieldDefinitionHandles =
        match this with
        | OlyILEntityDefinition(fieldDefs=fieldDefs) -> fieldDefs

    member this.PropertyDefinitionHandles =
        match this with
        | OlyILEntityDefinition(propDefs=propDefs) -> propDefs

    member this.Extends =
        match this with
        | OlyILEntityDefinition(extends=extends) -> extends

    member this.Implements =
        match this with
        | OlyILEntityDefinition(implements=implements) -> implements

    member this.NameHandle =
        match this with
        | OlyILEntityDefinition(name=name) -> name

    member this.Enclosing =
        match this with
        | OlyILEntityDefinition(enclosing=enclosing) -> enclosing

    member this.TypeParameters =
        match this with
        | OlyILEntityDefinition(tyPars=tyPars) -> tyPars

    member this.FullTypeParameterCount =
        match this with
        | OlyILEntityDefinition(tyPars=tyPars) ->   
            tyPars.Length + this.Enclosing.TypeArgumentCount

    member this.Kind =
        match this with
        | OlyILEntityDefinition(kind=kind) -> kind

    member this.Flags =
        match this with
        | OlyILEntityDefinition(flags=flags) -> flags

    member this.FunctionHandles =
        match this with
        | OlyILEntityDefinition(funcDefs=funcDefs) -> funcDefs

    member this.Attributes =
        match this with
        | OlyILEntityDefinition(attrs=attrs) -> attrs
        
    member this.IsExternal =
        this.Attributes
        |> ImArray.exists (function OlyILAttribute.Import _ -> true | _ -> false)

    member this.IsExported =
        this.Attributes
        |> ImArray.exists (function OlyILAttribute.Export _ -> true | _ -> false)

    member this.IsIntrinsic =
        this.Attributes
        |> ImArray.exists (function OlyILAttribute.Intrinsic _ -> true | _ -> false)

    member this.RuntimeType =
        match this with
        | OlyILEntityDefinition(runtimeTyOpt=runtimeTyOpt) -> runtimeTyOpt

[<NoEquality;NoComparison>]
type OlyILEntityInstance =
    | OlyILEntityInstance of defOrRefHandle: OlyILEntityDefinitionOrReferenceHandle * tyArgs: OlyILType imarray
    | OlyILEntityConstructor of defOrRefHandle: OlyILEntityDefinitionOrReferenceHandle

    member this.DefinitionOrReferenceHandle =
        match this with
        | OlyILEntityInstance(defOrRefHandle=defOrRefHandle)
        | OlyILEntityConstructor(defOrRefHandle=defOrRefHandle) -> defOrRefHandle

    member this.TypeArguments =
        match this with
        | OlyILEntityInstance(tyArgs=tyArgs) -> tyArgs
        | OlyILEntityConstructor _ -> ImArray.empty

    member this.AsType = OlyILTypeEntity(this)

[<NoEquality;NoComparison>]
type OlyILParameter =
    | OlyILParameter of name: OlyILStringHandle * ty: OlyILType * isMutable: bool * canInlineClosure: bool

    member this.NameHandle =
        match this with
        | OlyILParameter(name=name) -> name

    member this.Type =
        match this with
        | OlyILParameter(ty=ty) -> ty

    member this.IsMutable =
        match this with
        | OlyILParameter(isMutable=isMutable) -> isMutable

    member this.CanInlineClosure =
        match this with
        | OlyILParameter(canInlineClosure=canInlineClosure) -> canInlineClosure

[<RequireQualifiedAccess>]
[<NoEquality;NoComparison>]
type OlyILConstraint =
    | Null
    | Struct
    | NotStruct
    | Unmanaged
    | ConstantType of OlyILType
    | SubtypeOf of OlyILType

[<NoEquality;NoComparison>]
type OlyILTypeParameter =
    | OlyILTypeParameter of name: OlyILStringHandle * arity: int * isVariadic: bool * constrs: OlyILConstraint imarray

    member this.NameHandle =
        match this with
        | OlyILTypeParameter(name=name) -> name

    member this.Arity =
        match this with
        | OlyILTypeParameter(arity=arity) -> arity

    member this.IsVariadic =
        match this with
        | OlyILTypeParameter(isVariadic=isVariadic) -> isVariadic

    member this.Constraints =
        match this with
        | OlyILTypeParameter(constrs=constrs) -> constrs

[<RequireQualifiedAccess>]
[<NoEquality;NoComparison>]
type OlyILEnclosing =
    | Entity of entInst: OlyILEntityInstance
    | Witness of ty: OlyILType * abstractEntInst: OlyILEntityInstance
    | Namespace of path: OlyILStringHandle imarray * identity: OlyILAssemblyIdentity

    member this.TypeArgumentCount =
        match this with
        | Entity(OlyILEntityInstance(_, tyArgs)) -> tyArgs.Length
        | _ -> 0

[<RequireQualifiedAccess>]
type OlyILWitness =
    | Implementation of index: int * kind: OlyILTypeVariableKind * OlyILEntityInstance * specificAbstractFuncInstOpt: OlyILFunctionReference option

[<NoEquality;NoComparison>]
type OlyILFunctionSpecification =
    | OlyILFunctionSpecification of 
        isInstance: bool * 
        callConv: OlyILCallingConvention *
        name: OlyILStringHandle * 
        tyPars: OlyILTypeParameter imarray * 
        pars: OlyILParameter imarray * 
        returnTy: OlyILType with

    member this.IsInstance =
        match this with
        | OlyILFunctionSpecification(isInstance=isInstance) -> isInstance

    member this.CallingConvention =
        match this with
        | OlyILFunctionSpecification(callConv=callConv) -> callConv

    member this.NameHandle =
        match this with
        | OlyILFunctionSpecification(name=name) -> name

    member this.TypeParameters =
        match this with
        | OlyILFunctionSpecification(tyPars=tyPars) -> tyPars

    member this.Parameters =
        match this with
        | OlyILFunctionSpecification(pars=pars) -> pars

    member this.ReturnType =
        match this with
        | OlyILFunctionSpecification(returnTy=returnTy) -> returnTy

[<NoEquality;NoComparison>]
type OlyILFunctionDefinition =
    | OlyILFunctionDefinition of 
        flags: OlyILFunctionFlags * 
        memberFlags: OlyILMemberFlags * 
        attrs: OlyILAttribute imarray * 
        specHandle: OlyILFunctionSpecificationHandle * 
        overrides: OlyILFunctionReference option * 
        bodyHandle: OlyILFunctionBodyHandle option ref

    member this.Flags =
        match this with
        | OlyILFunctionDefinition(flags=flags) -> flags

    member this.MemberFlags =
        match this with
        | OlyILFunctionDefinition(memberFlags=memberFlags) -> memberFlags

    member this.SpecificationHandle =
        match this with
        | OlyILFunctionDefinition(specHandle=specHandle) -> specHandle

    member this.BodyHandle =
        match this with
        | OlyILFunctionDefinition(bodyHandle=bodyHandle) -> bodyHandle

    member this.Overrides =
        match this with
        | OlyILFunctionDefinition(overrides=overrides) -> overrides

    member this.Attributes =
        match this with
        | OlyILFunctionDefinition(attrs=attrs) -> attrs

    member this.IsImported =
        this.Attributes
        |> ImArray.exists (function OlyILAttribute.Import _ -> true | _ -> false)

    member this.IsExported =
        this.Attributes
        |> ImArray.exists (function OlyILAttribute.Export _ -> true | _ -> false)

    member this.IsIntrinsic =
        this.Attributes
        |> ImArray.exists (function OlyILAttribute.Intrinsic _ -> true | _ -> false)

    member this.IsStatic =
        this.MemberFlags &&& OlyILMemberFlags.Static = OlyILMemberFlags.Static

    member this.IsAbstract =
        this.MemberFlags &&& OlyILMemberFlags.Abstract = OlyILMemberFlags.Abstract

    member this.IsVirtual =
        this.MemberFlags &&& OlyILMemberFlags.Virtual = OlyILMemberFlags.Virtual

    member this.IsSealed =
        this.MemberFlags &&& OlyILMemberFlags.Final = OlyILMemberFlags.Final

    member this.IsConstructor =
        this.Flags &&& OlyILFunctionFlags.Constructor = OlyILFunctionFlags.Constructor

    static member NilHandle = OlyILTableIndex.CreateFunctionDefinition(-1)

[<NoEquality;NoComparison>]
type OlyILFunctionInstance =
    | OlyILFunctionInstance of enclosing: OlyILEnclosing * specHandle: OlyILFunctionSpecificationHandle * tyArgs: OlyILType imarray * witnesses: OlyILWitness imarray

    member this.Enclosing =
        match this with
        | OlyILFunctionInstance(enclosing=enclosing) -> enclosing

    member this.SpecificationHandle =
        match this with
        | OlyILFunctionInstance(specHandle=specHandle) -> specHandle

    member this.TypeArguments =
        match this with
        | OlyILFunctionInstance(tyArgs=tyArgs) -> tyArgs

    member this.GetEnclosingType() =
        match this.Enclosing with
        | OlyILEnclosing.Entity(entInst) -> entInst.AsType
        | OlyILEnclosing.Witness(ty, _) -> ty
        | _ -> failwith "Expected enclosing type."

[<NoEquality;NoComparison>]
type OlyILFunctionReference =
    | OlyILFunctionReference of enclosing: OlyILEnclosing * specHandle: OlyILFunctionSpecificationHandle

    member this.Enclosing =
        match this with
        | OlyILFunctionReference(enclosing=enclosing) -> enclosing

    member this.SpecificationHandle =
        match this with
        | OlyILFunctionReference(specHandle=specHandle) -> specHandle

    member this.GetEnclosingType() =
        match this.Enclosing with
        | OlyILEnclosing.Entity(entInst) -> entInst.AsType
        | OlyILEnclosing.Witness(ty, _) -> ty
        | _ -> failwith "Expected enclosing type."

type OlyILTypeVariableKind =
    | Type      = 0
    | Function  = 1

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type OlyILConstant =
    | Int8 of value: int8
    | UInt8 of value: uint8
    | Int16 of value: int16
    | UInt16 of value: uint16
    | Int32 of value: int32
    | UInt32 of value: uint32
    | Int64 of value: int64
    | UInt64 of value: uint64
    | Float32 of value: float32
    | Float64 of value: float
    | True
    | False
    | Array of elementTy: OlyILType * value: OlyILConstant imarray
    | Char16 of value: char
    | Utf16 of value: string
    | TypeVariable of index: int32 * kind: OlyILTypeVariableKind
    | External of funcInst: OlyILFunctionInstance * returnTy: OlyILType

    member this.Type =
        match this with
        | Int8 _ -> OlyILTypeInt8
        | UInt8 _ -> OlyILTypeUInt8
        | Int16 _ -> OlyILTypeInt16
        | UInt16 _ -> OlyILTypeUInt16
        | Int32 _ -> OlyILTypeInt32
        | UInt32 _ -> OlyILTypeUInt32
        | Int64 _ -> OlyILTypeInt64
        | UInt64 _ -> OlyILTypeUInt64
        | Float32 _ -> OlyILTypeFloat32
        | Float64 _ -> OlyILTypeFloat64
        | True _
        | False _ -> OlyILTypeBool
        | Array(elementTy, _) -> OlyILTypeArray(elementTy, 1, OlyILArrayKind.Immutable)
        | Char16 _ -> OlyILTypeChar16
        | Utf16 _ -> OlyILTypeUtf16
        | TypeVariable(index, kind) -> OlyILTypeVariable(index, kind)
        | External(_, ilTy) -> ilTy

[<NoEquality;NoComparison>]
type OlyILFieldDefinition =
    | OlyILFieldDefinition of attrs: OlyILAttribute imarray * name: OlyILStringHandle * ty: OlyILType * flags: OlyILFieldFlags * memberFlags: OlyILMemberFlags
    | OlyILFieldConstant of name: OlyILStringHandle * ty: OlyILType * constant: OlyILConstant * memberFlags: OlyILMemberFlags

    member this.IsConstant =
        match this with
        | OlyILFieldConstant _ -> true
        | _ -> false

    member this.NameHandle =
        match this with
        | OlyILFieldDefinition(name=nameHandle) -> nameHandle
        | OlyILFieldConstant(name=nameHandle) -> nameHandle

    member this.Type =
        match this with
        | OlyILFieldDefinition(ty=ty) -> ty
        | OlyILFieldConstant(ty=ty) -> ty

    member this.Flags =
        match this with
        | OlyILFieldDefinition(flags=flags) -> flags
        | OlyILFieldConstant _ -> OlyILFieldFlags.None

    member this.MemberFlags =
        match this with
        | OlyILFieldDefinition(memberFlags=memberFlags) -> memberFlags
        | OlyILFieldConstant(memberFlags=memberFlags) -> memberFlags

    member this.IsMutable =
        this.Flags &&& OlyILFieldFlags.Mutable = OlyILFieldFlags.Mutable

    member this.Attributes =
        match this with
        | OlyILFieldDefinition(attrs=attrs) -> attrs
        | _ -> ImArray.empty

    member this.IsExternal =
        this.Attributes
        |> ImArray.exists (function OlyILAttribute.Import _ -> true | _ -> false)

[<NoEquality;NoComparison>]
type OlyILPropertyDefinition =
    | OlyILPropertyDefinition of attrs: OlyILAttribute imarray * nameHandle: OlyILStringHandle * ty: OlyILType * getterHandleOpt: OlyILFunctionDefinitionHandle * setterHandleOpt: OlyILFunctionDefinitionHandle

    member this.Attributes =
        match this with
        | OlyILPropertyDefinition(attrs=attrs) -> attrs

    member this.NameHandle =
        match this with
        | OlyILPropertyDefinition(nameHandle=nameHandle) -> nameHandle

    member this.Type =
        match this with
        | OlyILPropertyDefinition(ty=ty) -> ty

    member this.Getter =
        match this with
        | OlyILPropertyDefinition(getterHandleOpt=getterHandleOpt) -> getterHandleOpt

    member this.Setter =
        match this with
        | OlyILPropertyDefinition(setterHandleOpt=setterHandleOpt) -> setterHandleOpt

[<NoEquality;NoComparison>]
type OlyILPatternDefinition =
    | OlyILPatternDefinition of attrs: OlyILAttribute imarray * name: OlyILStringHandle * funcDefHandle: OlyILFunctionDefinitionHandle * guardDefHandleOpt: OlyILFunctionDefinitionHandle

    member this.NameHandle =
        match this with
        | OlyILPatternDefinition(name=nameHandle) -> nameHandle

    member this.FunctionDefinitionHandle =
        match this with
        | OlyILPatternDefinition(funcDefHandle=funcDefHandle) -> funcDefHandle

    member this.GuardDefinitionHandleOption =
        match this with
        | OlyILPatternDefinition(guardDefHandleOpt=guardDefHandleOpt) -> guardDefHandleOpt

[<NoEquality;NoComparison>]
type OlyILFieldReference =
    | OlyILFieldReference of enclosing: OlyILEnclosing * name: OlyILStringHandle * ty: OlyILType

    member this.Type =
        match this with
        | OlyILFieldReference(ty=ty) -> ty

    member this.GetEnclosingType() =
        match this with
        | OlyILFieldReference(enclosing=enclosing) ->
            match enclosing with
            | OlyILEnclosing.Entity(entInst) -> entInst.AsType
            | OlyILEnclosing.Witness(ty, _) -> ty
            | _ -> failwith "Invalid enclosing."

[<RequireQualifiedAccess>]
type OlyILByRefKind =
    | ReadWrite
    | Read

[<RequireQualifiedAccess>]
type OlyILArrayKind =
    | Immutable
    | Mutable

[<NoEquality;NoComparison>]
type OlyILType =
    | OlyILTypeModified of modifierTy: OlyILType * ty: OlyILType
    | OlyILTypeInvalid of msg: OlyILStringHandle

    | OlyILTypeBaseObject
    | OlyILTypeBaseStruct
    | OlyILTypeBaseStructEnum
    | OlyILTypeBaseAttribute

    | OlyILTypeVoid
    | OlyILTypeUnit
    | OlyILTypeInt8
    | OlyILTypeUInt8
    | OlyILTypeInt16
    | OlyILTypeUInt16
    | OlyILTypeInt32
    | OlyILTypeUInt32
    | OlyILTypeInt64
    | OlyILTypeUInt64
    | OlyILTypeFloat32
    | OlyILTypeFloat64
    | OlyILTypeBool
    | OlyILTypeChar16
    | OlyILTypeUtf16

    | OlyILTypeByRef of OlyILType * kind: OlyILByRefKind

    | OlyILTypeEntity of entInst: OlyILEntityInstance
    | OlyILTypeForAll of tyPars: OlyILTypeParameter imarray * ty: OlyILType

    | OlyILTypeTuple of elementTys: OlyILType imarray * elementNames: OlyILStringHandle imarray
    | OlyILTypeRefCell of ty: OlyILType
    | OlyILTypeFunction of argTys: OlyILType imarray * returnTy: OlyILType
    | OlyILTypeNativeInt
    | OlyILTypeNativeUInt
    | OlyILTypeNativePtr of elementTy: OlyILType
    | OlyILTypeNativeFunctionPtr of cc: OlyILCallingConvention * argTys: OlyILType imarray * returnTy: OlyILType
    | OlyILTypeArray of elementTy: OlyILType * rank: int * kind: OlyILArrayKind
    | OlyILTypeVariable of index: int32 * kind: OlyILTypeVariableKind
    | OlyILTypeHigherVariable of index: int32 * tyInst: OlyILType imarray * kind: OlyILTypeVariableKind
    | OlyILTypeConstantInt32 of value: int32

    | OlyILTypeDependentIndexer of inputValueTy: OlyILType * innerTy: OlyILType

    member this.IsTypeVariable =
        match this with
        | OlyILTypeVariable _
        | OlyILTypeHigherVariable _ -> true
        | _ -> false

    member this.TypeArguments =
        match this with
        | OlyILTypeInvalid _
        | OlyILTypeModified _
        | OlyILTypeVoid
        | OlyILTypeUnit
        | OlyILTypeInt8
        | OlyILTypeUInt8
        | OlyILTypeInt16
        | OlyILTypeUInt16
        | OlyILTypeInt32
        | OlyILTypeUInt32
        | OlyILTypeInt64
        | OlyILTypeUInt64
        | OlyILTypeFloat32
        | OlyILTypeFloat64
        | OlyILTypeBool
        | OlyILTypeChar16
        | OlyILTypeUtf16 
        | OlyILTypeTuple _
        | OlyILTypeConstantInt32 _
        | OlyILTypeVariable _ 
        | OlyILTypeBaseObject
        | OlyILTypeBaseStruct
        | OlyILTypeBaseAttribute
        | OlyILTypeBaseStructEnum
        | OlyILTypeNativeInt
        | OlyILTypeNativeUInt -> imarray.Empty
        | OlyILTypeFunction(argTys, returnTy)
        | OlyILTypeNativeFunctionPtr(_, argTys, returnTy) -> argTys.Add(returnTy)
        | OlyILTypeNativePtr(ty) -> ImArray.createOne ty
        | OlyILTypeArray(ty, _, _) -> ImArray.createOne ty
        | OlyILTypeRefCell(ty) -> ImArray.createOne ty
        | OlyILTypeEntity(entRef) -> entRef.TypeArguments
        | OlyILTypeForAll _ -> ImArray.empty
        | OlyILTypeHigherVariable(_, tyArgs, _) -> tyArgs
        | OlyILTypeByRef(ty, _) -> ImArray.createOne ty
        | OlyILTypeDependentIndexer _ -> ImArray.empty

    member this.IsFunction =
        match this with
        | OlyILTypeFunction _ -> true
        | _ -> false

    // 0 - 63 reserved for built-in types
    member this.FormalId =
        match this with
        | OlyILTypeVoid -> 0
        | OlyILTypeUnit -> 1
        | OlyILTypeInt8 -> 2
        | OlyILTypeUInt8 -> 3
        | OlyILTypeInt16 -> 4
        | OlyILTypeUInt16 -> 5
        | OlyILTypeInt32 -> 6
        | OlyILTypeUInt32 -> 7
        | OlyILTypeInt64 -> 8
        | OlyILTypeUInt64 -> 9
        | OlyILTypeFloat32 -> 10
        | OlyILTypeFloat64 -> 11
        | OlyILTypeBool -> 12
        | OlyILTypeChar16 -> 13
        | OlyILTypeUtf16 -> 14
        | OlyILTypeTuple _ -> 15
        | OlyILTypeRefCell _ -> 16
        | OlyILTypeFunction _ -> 17
        | OlyILTypeArray _ -> 18
        | OlyILTypeBaseObject -> 19
        | OlyILTypeConstantInt32 _ -> 20
        | OlyILTypeVariable _ -> 21
        | OlyILTypeHigherVariable _ -> 22
        | OlyILTypeByRef(_, kind) ->
            match kind with
            | OlyILByRefKind.Read ->
                23
            | OlyILByRefKind.ReadWrite ->
                24
        | OlyILTypeNativeInt -> 25
        | OlyILTypeNativeUInt -> 26
        | OlyILTypeNativePtr _ -> 27
        | OlyILTypeNativeFunctionPtr _ -> 28
        | OlyILTypeBaseStruct _ -> 31
        | OlyILTypeBaseStructEnum _ -> 32
        | OlyILTypeBaseAttribute _ -> 33
        | OlyILTypeDependentIndexer _ -> 34
        | OlyILTypeForAll _ -> 35
        | OlyILTypeInvalid _ -> 36
        | OlyILTypeModified _ -> 37
        | OlyILTypeEntity(ilEntInst) -> 64 + ilEntInst.DefinitionOrReferenceHandle.Index

    member this.IsBuiltIn =
        match this with
        | OlyILTypeInvalid _
        | OlyILTypeModified _
        | OlyILTypeVoid
        | OlyILTypeUnit
        | OlyILTypeInt8
        | OlyILTypeUInt8
        | OlyILTypeInt16
        | OlyILTypeUInt16
        | OlyILTypeInt32
        | OlyILTypeUInt32
        | OlyILTypeInt64
        | OlyILTypeUInt64
        | OlyILTypeFloat32
        | OlyILTypeFloat64
        | OlyILTypeBool
        | OlyILTypeChar16
        | OlyILTypeUtf16
        | OlyILTypeTuple _
        | OlyILTypeFunction _
        | OlyILTypeRefCell _
        | OlyILTypeVariable _
        | OlyILTypeHigherVariable _
        | OlyILTypeConstantInt32 _ 
        | OlyILTypeBaseObject
        | OlyILTypeBaseStruct
        | OlyILTypeBaseStructEnum _ 
        | OlyILTypeBaseAttribute
        | OlyILTypeByRef _
        | OlyILTypeNativeInt
        | OlyILTypeNativeUInt
        | OlyILTypeNativePtr _
        | OlyILTypeNativeFunctionPtr _
        | OlyILTypeArray _
        | OlyILTypeDependentIndexer _ 
        | OlyILTypeForAll _ -> true
        | OlyILTypeEntity _ -> false          

type OlyILLocalFlags =
    | None           = 0x0000
    | Mutable        = 0x0001

[<NoEquality;NoComparison>]
type OlyILLocal =
    | OlyILLocal of index: int32 * nameHandle: OlyILStringHandle * ty: OlyILType * flags: OlyILLocalFlags

    member this.Index =
        match this with
        | OlyILLocal(index, _, _, _) -> index

    member this.NameHandle =
        match this with
        | OlyILLocal(_, nameHandle, _, _) -> nameHandle

    member this.Type =
        match this with
        | OlyILLocal(_, _, ty, _) -> ty

    member this.IsMutable =
        match this with
        | OlyILLocal(_, _, _, flags) -> flags &&& OlyILLocalFlags.Mutable = OlyILLocalFlags.Mutable

[<NoEquality;NoComparison>]
type OlyILFunctionBody =
    | OlyILFunctionBody of 
        locals: OlyILLocal imarray *
        bodyExpr: OlyILExpression

    member this.Locals =
        match this with
        | OlyILFunctionBody(locals, _) -> locals

    member this.BodyExpression =
        match this with
        | OlyILFunctionBody(_, bodyExpr) -> bodyExpr

[<NoEquality;NoComparison>]
[<RequireQualifiedAccess>]
type OlyILValue =
    | Unit
    | Null of ty: OlyILType
    | Default of ty: OlyILType
    | FunctionPtr of funcInst: OlyILFunctionInstance
    | Function of funcInst: OlyILFunctionInstance
    | Local of n: int32
    | LocalAddress of n: int32 * kind: OlyILByRefKind
    | Argument of n: int32
    | ArgumentAddress of n: int32 * kind: OlyILByRefKind
    | StaticField of OlyILFieldReference
    | StaticFieldAddress of OlyILFieldReference * kind: OlyILByRefKind
    | Constant of OlyILConstant
    | ConstantEnum of OlyILConstant * enumTy: OlyILType

[<NoEquality;NoComparison>]
[<RequireQualifiedAccess>]
type OlyILOperation =
    | Add of arg1: OlyILExpression * arg2: OlyILExpression
    | Subtract of arg1: OlyILExpression * arg2: OlyILExpression
    | Multiply of arg1: OlyILExpression * arg2: OlyILExpression
    | Divide of arg1: OlyILExpression * arg2: OlyILExpression
    | Remainder of arg1: OlyILExpression * arg2: OlyILExpression

    | BitwiseNot of arg: OlyILExpression
    | BitwiseAnd of arg1: OlyILExpression * arg2: OlyILExpression
    | BitwiseOr of arg1: OlyILExpression * arg2: OlyILExpression
    | BitwiseExclusiveOr of arg1: OlyILExpression * arg2: OlyILExpression
    | BitwiseShiftLeft of arg1: OlyILExpression * arg2: OlyILExpression
    | BitwiseShiftRight of arg1: OlyILExpression * arg2: OlyILExpression
    | Not of arg: OlyILExpression
    | Negate of arg: OlyILExpression

    | Equal of arg1: OlyILExpression * arg2: OlyILExpression
    | NotEqual of arg1: OlyILExpression * arg2: OlyILExpression

    | GreaterThan of arg1: OlyILExpression * arg2: OlyILExpression
    | GreaterThanOrEqual of arg1: OlyILExpression * arg2: OlyILExpression
    | LessThan of arg1: OlyILExpression * arg2: OlyILExpression
    | LessThanOrEqual of arg1: OlyILExpression * arg2: OlyILExpression

    | LoadRefCellContents of elementTy: OlyILType * arg: OlyILExpression
    | LoadRefCellContentsAddress of elementTy: OlyILType * arg: OlyILExpression * kind: OlyILByRefKind
    | StoreRefCellContents of arg1: OlyILExpression * arg2: OlyILExpression

    | Print of arg: OlyILExpression
    | Throw of arg: OlyILExpression * resultTy: OlyILType
    | Cast of arg: OlyILExpression * resultTy: OlyILType

    | Store of n: int32 * arg: OlyILExpression
    | StoreArgument of n: int32 * arg: OlyILExpression
    | LoadFromAddress of arg: OlyILExpression
    | StoreToAddress of arg1: OlyILExpression * arg2: OlyILExpression
    | LoadField of OlyILFieldReference * arg: OlyILExpression
    | LoadFieldAddress of OlyILFieldReference * arg: OlyILExpression * kind: OlyILByRefKind
    | StoreField of OlyILFieldReference * arg1: OlyILExpression * arg2: OlyILExpression
    | StoreStaticField of OlyILFieldReference * arg: OlyILExpression

    | LoadArrayElement of receiver: OlyILExpression * indexArgs: OlyILExpression imarray
    | LoadArrayElementAddress of receiver: OlyILExpression * indexArgs: OlyILExpression imarray * kind: OlyILByRefKind
    | StoreArrayElement of receiver: OlyILExpression * indexArgs: OlyILExpression imarray * arg: OlyILExpression
    | LoadArrayLength of receiver: OlyILExpression

    | LoadTupleElement of arg: OlyILExpression * index: int32

    | LoadFunction of OlyILFunctionInstance * arg: OlyILExpression

    | Call of OlyILFunctionInstance * args: OlyILExpression imarray
    | CallVirtual of OlyILFunctionInstance * args: OlyILExpression imarray
    | CallIndirect of funArg: OlyILExpression * args: OlyILExpression imarray
    | New of OlyILFunctionInstance * args: OlyILExpression imarray
    | NewTuple of tyInst: OlyILType imarray * args: OlyILExpression imarray * names: OlyILStringHandle imarray
    | NewArray of elementTy: OlyILType * kind: OlyILArrayKind * args: OlyILExpression imarray
    | NewMutableArray of elementTy: OlyILType * sizeArg: OlyILExpression
    | NewRefCell of ty: OlyILType * arg: OlyILExpression

    | Witness of body: OlyILExpression * witnessArg: OlyILType * ty: OlyILType
    | Ignore of arg: OlyILExpression

[<Sealed>]
type OlyILDebugSource(path: OlyPath) =

    member _.Path = path

[<Struct>]
type OlyILDebugSourceTextRange(debugSourceHandle: OlyILDebugSourceHandle, startLine: int, startColumn: int, endLine: int, endColumn: int) =

    member _.DebugSourceHandle = debugSourceHandle
    member _.StartLine = startLine
    member _.StartColumn = startColumn
    member _.EndLine = endLine
    member _.EndColumn = endColumn

    static member Empty =
        OlyILDebugSourceTextRange(OlyILTableIndex(OlyILTableKind.DebugSource, -1), 0, 0, 0, 0)

[<NoEquality;NoComparison>]
[<RequireQualifiedAccess>]
type OlyILCatchCase =
    | CatchCase of localIndex: int32 * bodyExpr: OlyILExpression

[<NoEquality;NoComparison>]
[<RequireQualifiedAccess>]
type OlyILExpression =
    | None of textRange: OlyILDebugSourceTextRange
    | Let of localIndex: int32 * rhsExpr: OlyILExpression * bodyExpr: OlyILExpression
    | Value of textRange: OlyILDebugSourceTextRange * value: OlyILValue
    | Operation of textRange: OlyILDebugSourceTextRange * op: OlyILOperation
    | IfElse of conditionExpr: OlyILExpression * trueTargetExpr: OlyILExpression * falseTargetExpr: OlyILExpression
    | Sequential of expr1: OlyILExpression * expr2: OlyILExpression
    
    /// Cannot be used in a non-imperative context.
    /// TODO: The check for "non-imperative" context is not implemented.
    | While of conditionExpr: OlyILExpression * bodyExpr: OlyILExpression

    | Try of bodyExpr: OlyILExpression * catchCases: OlyILCatchCase imarray * finallyBodyExprOpt: OlyILExpression option

// Not concurrency safe.
type private Table<'Value>(kind: OlyILTableKind) =

    let mutable nextIndex = 0
    let mutable state = Array.zeroCreate<'Value> 10

    member private _.NextIndex() =
        let index = nextIndex
        nextIndex <- nextIndex + 1
        index

    member this.Get(tableIndex: OlyILTableIndex) =
        if tableIndex.Kind <> kind then
            failwithf "Invalid kind: %A. Expected: %A" tableIndex.Kind kind
        if tableIndex.Index < 0 || tableIndex.Index >= nextIndex then
            failwithf "Unable to find an entry into the table '%A'." kind
        state.[tableIndex.Index]

    member this.TryGet(tableIndex: OlyILTableIndex) =
        if tableIndex.Kind <> kind then
            failwithf "Invalid kind: %A. Expected: %A" tableIndex.Kind kind
        if tableIndex.Index < 0 || tableIndex.Index >= nextIndex then
            None
        else
            Some(state.[tableIndex.Index])

    member this.Contains(tableIndex: OlyILTableIndex) =
        if tableIndex.Kind <> kind then
            failwithf "Invalid kind: %A. Expected: %A" tableIndex.Kind kind
        if tableIndex.Index < 0 || tableIndex.Index >= nextIndex then
            false
        else
            true

    member this.Next() =
        let index = this.NextIndex()
        OlyILTableIndex(kind, index)

    member this.Add(v) =
        let index = this.NextIndex()
        this.Set(index, v)
        OlyILTableIndex(kind, index)
    
    member private this.Set(i, v) =
        if i >= state.Length then
            let newState = Array.zeroCreate (state.Length * 2) 
            state.CopyTo(newState, 0)
            state <- newState
            this.Set(i, v)
        else
            state.[i] <- v

    member this.Set(index: OlyILTableIndex, v) =
        if index.Kind <> kind then
            failwith "Invalid kind to set."
        this.Set(index.Index, v)

    member this.Values =
        state
        |> Seq.take this.Count

    member this.Count = nextIndex

module internal OlyILAssemblyIdentityHelpers =
    
    let Comparer =
        { new System.Collections.Generic.IEqualityComparer<OlyILAssemblyIdentity> with
            member _.GetHashCode(x: OlyILAssemblyIdentity) = x.Name.GetHashCode(StringComparison.OrdinalIgnoreCase)
            member _.Equals(x: OlyILAssemblyIdentity, y: OlyILAssemblyIdentity) =
                x.Name.Equals(y.Name, StringComparison.OrdinalIgnoreCase) &&
                x.Key.Equals(y.Key)
        }

[<Struct;CustomEquality;NoComparison>]
type OlyILAssemblyIdentity =
    | OlyILAssemblyIdentity of name: string * key: string

    member this.Name: string =
        match this with
        | OlyILAssemblyIdentity(name, _) -> name

    member this.Key =
        match this with
        | OlyILAssemblyIdentity(_, key) -> key

    static member Comparer = OlyILAssemblyIdentityHelpers.Comparer

    interface IEquatable<OlyILAssemblyIdentity> with

        member this.Equals(o) =
            this.Name.Equals(o.Name, StringComparison.OrdinalIgnoreCase) &&
            this.Key.Equals(o.Key)

[<Sealed>]
type OlyILEntityDefinitionDocumentation() = class end

[<Sealed>]
type OlyILFunctionDefinitionDocumentation() = class end

[<Sealed>]
type OlyILFieldDefinitionDocumentation() = class end

[<Sealed>]
type OlyILPropertyDefinitionDocumentation() = class end

// Read-only Thread safe, but be careful writing multiple entries at the same time.
[<NoComparison;ReferenceEquality>]
type OlyILAssembly =
    private {
        stringSet: ConcurrentDictionary<string, OlyILStringHandle> // this is a helper, not serialized
        entDefSet: ConcurrentDictionary<string, ConcurrentDictionary<OlyILEntityDefinitionHandle, byte>> // this is a helper, not serialized
        entRefSet: ConcurrentDictionary<string, ConcurrentDictionary<OlyILEntityReferenceHandle, byte>> // this is a helper, not serialized

        // Signature metadata.
        identity: OlyILAssemblyIdentity
        strings: Table<string>
        entRefs: Table<OlyILEntityReference>
        entDefs: Table<OlyILEntityDefinition>
        funcSpecs: Table<OlyILFunctionSpecification>
        funcDefs: Table<OlyILFunctionDefinition>
        fieldDefs: Table<OlyILFieldDefinition>
        propDefs: Table<OlyILPropertyDefinition>
        patDefs: Table<OlyILPatternDefinition>
        mutable entryPoint: (OlyILType * OlyILFunctionDefinitionHandle) option
        isDebuggable: bool
        primitiveTypes: ResizeArray<(OlyILType * OlyILEntityDefinitionHandle)>

        // Documentation metadata.
        entDefDocs: Table<OlyILEntityDefinitionDocumentation>
        funcDefDocs: Table<OlyILFunctionDefinitionDocumentation>
        fieldDefDocs: Table<OlyILFieldDefinitionDocumentation>
        propDefDocs: Table<OlyILPropertyDefinitionDocumentation>

        // Debug metadata.
        dbgSrcs: Table<OlyILDebugSource>

        // Implementation metadata.
        funcBodies: Table<OlyILFunctionBody>
    }

    member this.Identity = this.identity
    member this.Name = this.Identity.Name
    member this.IsDebuggable = this.isDebuggable

    member this.AddDebugSource(dbgSrc: OlyILDebugSource) : OlyILDebugSourceHandle =
        this.dbgSrcs.Add(dbgSrc)

    member this.GetDebugSource(handle: OlyILDebugSourceHandle) =
        this.dbgSrcs.Get(handle)

    member this.AddString(str: string) : OlyILStringHandle =
        if System.String.IsNullOrWhiteSpace str then
            invalidArg (nameof(str)) "String cannot be null or whitespace-only."

        match this.stringSet.TryGetValue str with
        | true, handle -> handle
        | _ ->
            let handle = this.strings.Add(str)
            this.stringSet.[str] <- handle
            handle

    member this.AddStringOrNilString(str: string) : OlyILStringHandle =
        if System.String.IsNullOrWhiteSpace str then
            Unchecked.defaultof<_>
        else
            match this.stringSet.TryGetValue str with
            | true, handle -> handle
            | _ ->
                let handle = this.strings.Add(str)
                this.stringSet.[str] <- handle
                handle

    member private this.SetEntityDefinitionLookup(handle: OlyILEntityDefinitionHandle, entDef: OlyILEntityDefinition) =
        let name = this.GetStringOrEmpty(entDef.NameHandle)
        if String.IsNullOrWhiteSpace name |> not then
            let handles =
                match this.entDefSet.TryGetValue(name) with
                | true, handles -> handles
                | _ ->
                    let handles = ConcurrentDictionary()
                    this.entDefSet.[name] <- handles
                    handles
            handles[handle] <- 0uy       
            
    member private this.SetEntityReferenceLookup(handle: OlyILEntityReferenceHandle, entRef: OlyILEntityReference) =
        let name = this.GetStringOrEmpty(entRef.NameHandle)
        if String.IsNullOrWhiteSpace name |> not then
            let handles =
                match this.entRefSet.TryGetValue(name) with
                | true, handles -> handles
                | _ ->
                    let handles = ConcurrentDictionary()
                    this.entRefSet.[name] <- handles
                    handles
            handles[handle] <- 0uy 

    member this.AddEntityReference(entRef: OlyILEntityReference) : OlyILEntityReferenceHandle =
        let handle = this.entRefs.Add(entRef)
        this.SetEntityReferenceLookup(handle, entRef)
        handle

    member this.AddEntityDefinition(entDef: OlyILEntityDefinition) : OlyILEntityDefinitionHandle =
        let handle = this.entDefs.Add(entDef)
        this.SetEntityDefinitionLookup(handle, entDef)
        handle

    member this.NextEntityDefinition() : OlyILEntityDefinitionHandle =
        this.entDefs.Next()

    member this.SetEntityDefinition(handle: OlyILEntityDefinitionHandle, entDef: OlyILEntityDefinition) =
        this.entDefs.Set(handle, entDef)
        this.SetEntityDefinitionLookup(handle, entDef)

    member this.AddFunctionSpecification(funcSpec: OlyILFunctionSpecification) : OlyILFunctionSpecificationHandle =
        this.funcSpecs.Add(funcSpec)

    member this.SetFunctionSpecification(handle: OlyILFunctionSpecificationHandle, funcSpec: OlyILFunctionSpecification) =
        this.funcSpecs.Set(handle, funcSpec)

    member this.AddFunctionDefinition(funcDef: OlyILFunctionDefinition) : OlyILFunctionDefinitionHandle =
        this.funcDefs.Add(funcDef)

    member this.SetFunctionDefinition(handle: OlyILFunctionDefinitionHandle, funcDef) =
        this.funcDefs.Set(handle, funcDef)

    member this.AddFieldDefinition(fieldDef: OlyILFieldDefinition) : OlyILFieldDefinitionHandle =
        this.fieldDefs.Add(fieldDef)

    member this.AddFunctionBody(funcBody: OlyILFunctionBody): OlyILFunctionBodyHandle =
        this.funcBodies.Add(funcBody)

    member this.GetStringOrEmpty(handle: OlyILStringHandle) : string =
        if handle.IsNil && handle.Kind = OlyILTableKind.String then
            System.String.Empty
        else
            this.strings.Get(handle)

    member this.GetEntityDefinition(handle: OlyILEntityDefinitionHandle) : OlyILEntityDefinition =
        if handle.Kind = OlyILTableKind.EntityDefinition then
            this.entDefs.Get(handle)
        else
            failwith "Incorrect table kind."

    member this.GetEntityReference(handle: OlyILEntityReferenceHandle) : OlyILEntityReference =
        if handle.Kind = OlyILTableKind.EntityReference then
            this.entRefs.Get(handle)
        else
            failwith "Incorrect table kind."

    member this.GetFunctionSpecification(handle: OlyILFunctionSpecificationHandle) =
        this.funcSpecs.Get(handle)

    member this.GetFunctionDefinition(handle: OlyILFunctionDefinitionHandle) =
        this.funcDefs.Get(handle)

    member this.GetFieldDefinition(handle: OlyILFieldDefinitionHandle) =
        this.fieldDefs.Get(handle)

    member this.GetFunctionBody(handle: OlyILFunctionBodyHandle) =
        this.funcBodies.Get(handle)

    member this.GetPropertyDefinition(handle: OlyILPropertyDefinitionHandle) =
        this.propDefs.Get(handle)

    member this.GetPatternDefinition(handle: OlyILPatternDefinitionHandle) =
        this.patDefs.Get(handle)

    member this.AddPropertyDefinition(propDef) =
        this.propDefs.Add(propDef)

    member this.AddPatternDefinition(patDef) =
        this.patDefs.Add(patDef)

    member this.HasFunctionBody(handle: OlyILFunctionBodyHandle) =
        this.funcBodies.Contains(handle)

    member this.EntityDefinitions: (OlyILEntityDefinitionHandle * OlyILEntityDefinition) seq =
        this.entDefs.Values
        |> Seq.mapi (fun i x ->
            OlyAssert.NotNull(x)
            (OlyILTableIndex(OlyILTableKind.EntityDefinition, i), x)
        )

    member this.EntityReferences: (OlyILEntityReferenceHandle * OlyILEntityReference) seq =
        this.entRefs.Values
        |> Seq.mapi (fun i x ->
            (OlyILTableIndex(OlyILTableKind.EntityReference, i), x)
        )

    member this.FunctionDefinitions: (OlyILFunctionDefinitionHandle * OlyILFunctionDefinition) seq =
        this.funcDefs.Values
        |> Seq.mapi (fun i x ->
            (OlyILTableIndex(OlyILTableKind.FunctionDefinition, i), x)
        )

    member this.EntryPoint
        with get() = this.entryPoint
        and set value = this.entryPoint <- value

    member this.FindEntityDefinitions(name: string) =
        match this.entDefSet.TryGetValue(name) with
        | true, handles -> handles.Keys |> ImArray.ofSeq
        | _ -> ImArray.empty

    member this.FindEntityReferences(name: string) =
        match this.entRefSet.TryGetValue(name) with
        | true, handles -> handles.Keys |> ImArray.ofSeq
        | _ -> ImArray.empty

    member this.AddPrimitiveType(builtInTy, entDefHandle) =
        this.primitiveTypes.Add(builtInTy, entDefHandle)

    member this.ForEachPrimitiveType(f) =
        for i = 0 to this.primitiveTypes.Count - 1 do
            f this.primitiveTypes[i]

    member this.GetAssemblyIdentity(ilEntRef: OlyILEntityReference) =
        match ilEntRef.Enclosing with
        | OlyILEnclosing.Namespace(_, identity) -> identity
        | OlyILEnclosing.Entity(ilEntInst) -> this.GetAssemblyIdentity(ilEntInst)
        | OlyILEnclosing.Witness(_, ilEntInst) -> this.GetAssemblyIdentity(ilEntInst)

    member this.GetAssemblyIdentity(ilEntInst: OlyILEntityInstance) =
        match ilEntInst with
        | OlyILEntityInstance.OlyILEntityInstance(ilDefOrRefHandle, _)
        | OlyILEntityInstance.OlyILEntityConstructor(ilDefOrRefHandle) ->
            if ilDefOrRefHandle.Kind = OlyILTableKind.EntityDefinition then
                this.Identity
            else
                let ilEntRef = this.GetEntityReference(ilDefOrRefHandle)
                this.GetAssemblyIdentity(ilEntRef)

    static member Create(name, stamp, isDebuggable) =
        let asm = {
            stringSet = ConcurrentDictionary()
            entDefSet = ConcurrentDictionary()
            entRefSet = ConcurrentDictionary()
            strings = Table(OlyILTableKind.String)
            entRefs = Table(OlyILTableKind.EntityReference)
            entDefs = Table(OlyILTableKind.EntityDefinition)
            funcSpecs = Table(OlyILTableKind.FunctionSpecification)
            funcDefs = Table(OlyILTableKind.FunctionDefinition)
            fieldDefs = Table(OlyILTableKind.FieldDefinition)
            propDefs = Table(OlyILTableKind.PropertyDefinition)
            patDefs = Table(OlyILTableKind.PatternDefinition)
            identity = OlyILAssemblyIdentity(name, stamp)
            entryPoint = None
            isDebuggable = isDebuggable
            primitiveTypes = ResizeArray()

            entDefDocs = Table(OlyILTableKind.EntityDefinition)
            funcDefDocs = Table(OlyILTableKind.FunctionDefinition)
            fieldDefDocs = Table(OlyILTableKind.FieldDefinition)
            propDefDocs = Table(OlyILTableKind.PropertyDefinition)

            dbgSrcs = Table(OlyILTableKind.DebugSource)

            funcBodies = Table(OlyILTableKind.FunctionBody)
        }
        asm

[<Sealed>]
type OlyILReadOnlyAssembly internal (ilAsm: OlyILAssembly) =

    member this.Identity = ilAsm.Identity
    member this.Name = ilAsm.Name
    member this.IsDebuggable = ilAsm.IsDebuggable

    member this.GetDebugSource(handle: OlyILDebugSourceHandle) =
        ilAsm.GetDebugSource(handle)

    member this.GetStringOrEmpty(handle: OlyILStringHandle) : string =
        ilAsm.GetStringOrEmpty(handle)

    member this.GetEntityDefinition(handle: OlyILEntityDefinitionHandle) : OlyILEntityDefinition =
        ilAsm.GetEntityDefinition(handle)

    member this.GetEntityReference(handle: OlyILEntityReferenceHandle) : OlyILEntityReference =
        ilAsm.GetEntityReference(handle)

    member this.GetFunctionSpecification(handle: OlyILFunctionSpecificationHandle) =
        ilAsm.GetFunctionSpecification(handle)

    member this.GetFunctionDefinition(handle: OlyILFunctionDefinitionHandle) =
        ilAsm.GetFunctionDefinition(handle)

    member this.GetFieldDefinition(handle: OlyILFieldDefinitionHandle) =
        ilAsm.GetFieldDefinition(handle)

    member this.GetFunctionBody(handle: OlyILFunctionBodyHandle) =
        ilAsm.GetFunctionBody(handle)

    member this.GetPropertyDefinition(handle: OlyILPropertyDefinitionHandle) =
        ilAsm.GetPropertyDefinition(handle)

    member this.GetPatternDefinition(handle: OlyILPatternDefinitionHandle) =
        ilAsm.GetPatternDefinition(handle)

    member this.HasFunctionBody(handle: OlyILFunctionBodyHandle) =
        ilAsm.HasFunctionBody(handle)

    member this.EntityDefinitions: (OlyILEntityDefinitionHandle * OlyILEntityDefinition) seq =
        ilAsm.EntityDefinitions

    member this.EntityReferences: (OlyILEntityReferenceHandle * OlyILEntityReference) seq =
        ilAsm.EntityReferences

    member this.EntryPoint
        with get() = ilAsm.EntryPoint

    member this.FindEntityDefinitions(name: string) =
        ilAsm.FindEntityDefinitions(name)

    member this.FindEntityReferences(name: string) =
        ilAsm.FindEntityReferences(name)

    member this.ForEachPrimitiveType(f) =
        ilAsm.ForEachPrimitiveType(f)

    member this.GetAssemblyIdentity(ilEntRef: OlyILEntityReference) =
        ilAsm.GetAssemblyIdentity(ilEntRef)

    member this.GetAssemblyIdentity(ilEntInst: OlyILEntityInstance) =
        ilAsm.GetAssemblyIdentity(ilEntInst)

    member private this.GetQualifiedNameForDefinition(ilEntDefHandle: OlyILEntityDefinitionHandle, builder: System.Text.StringBuilder) =
        let ilEntDef = this.GetEntityDefinition(ilEntDefHandle)
        let ilTyParCount = ilEntDef.TypeParameters.Length
        match ilEntDef.Enclosing with
        | OlyILEnclosing.Witness _ ->
            OlyAssert.Fail("Invalid IL.")

        | OlyILEnclosing.Namespace(path, _) ->
            path
            |> ImArray.iteri (fun i x ->
                builder.Append(this.GetStringOrEmpty(x)) |> ignore
                if i <> (path.Length - 1) then
                    builder.Append(".") |> ignore
            )
            builder.Append("::") |> ignore

        | OlyILEnclosing.Entity(OlyILEntityInstance(ilEntDefOrRefHandle, _)) ->
            this.GetQualifiedName(ilEntDefOrRefHandle, builder)

        | OlyILEnclosing.Entity(OlyILEntityConstructor _) ->
            OlyAssert.Fail("Invalid IL.")
                
        builder.Append("::") |> ignore
        builder.Append(this.GetStringOrEmpty(ilEntDef.NameHandle)) |> ignore
        if ilTyParCount > 0 then
            builder.Append("````" + ilTyParCount.ToString()) |> ignore

    member private this.GetQualifiedNameForReference(ilEntRefHandle: OlyILEntityReferenceHandle, builder: System.Text.StringBuilder) =
        let ilEntRef = this.GetEntityReference(ilEntRefHandle)
        let ilTyParCount = ilEntRef.TypeParameterCount
        match ilEntRef.Enclosing with
        | OlyILEnclosing.Witness _ ->
            OlyAssert.Fail("Invalid IL.")

        | OlyILEnclosing.Namespace(path, _) ->
            path
            |> ImArray.iteri (fun i x ->
                builder.Append(this.GetStringOrEmpty(x)) |> ignore
                if i <> (path.Length - 1) then
                    builder.Append(".") |> ignore
            )
            builder.Append("::") |> ignore

        | OlyILEnclosing.Entity(OlyILEntityInstance(ilEntDefOrRefHandle, _)) ->
            this.GetQualifiedName(ilEntDefOrRefHandle, builder)

        | OlyILEnclosing.Entity(OlyILEntityConstructor _) ->
            OlyAssert.Fail("Invalid IL.")
                
        builder.Append("::") |> ignore
        builder.Append(this.GetStringOrEmpty(ilEntRef.NameHandle)) |> ignore
        if ilTyParCount > 0 then
            builder.Append("````" + ilTyParCount.ToString()) |> ignore

    member private this.GetQualifiedName(ilEntDefOrRefHandle: OlyILEntityDefinitionOrReferenceHandle, builder: System.Text.StringBuilder) =
        if ilEntDefOrRefHandle.Kind = OlyILTableKind.EntityReference then
            this.GetQualifiedNameForReference(ilEntDefOrRefHandle, builder)
        else
            this.GetQualifiedNameForDefinition(ilEntDefOrRefHandle, builder)

    member this.GetQualifiedName(ilEntDefOrRefHandle: OlyILEntityDefinitionOrReferenceHandle) =
        let builder = System.Text.StringBuilder()
        this.GetQualifiedName(ilEntDefOrRefHandle, builder)
        builder.ToString()

type OlyILAssembly with

    member this.ToReadOnly() =
        OlyILReadOnlyAssembly(this)
