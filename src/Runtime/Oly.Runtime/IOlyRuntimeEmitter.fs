namespace Oly.Runtime.CodeGen

open Oly.Metadata
open Oly.Core

[<RequireQualifiedAccess>]
type OlyIRFunctionTier =
    | Tier0 of minOpts: bool
    | Tier1
    | Tier2

    member this.HasMinimalOptimizations =
        match this with
        | Tier0 true -> true
        | _ -> false

type OlyFunctionKind =
    | Static
    | Instance

type IOlyVirtualMachine<'Type, 'Function, 'Field> =

    abstract GetTypeVoid : unit -> 'Type
    abstract GetTypeInt32 : unit -> 'Type
    abstract GetTypeFloat32 : unit -> 'Type
    abstract TryFindType : fullyQualifiedTypeName: string -> 'Type option
    abstract TryFindType : fullyQualifiedTypeName: string * tyParCount: int32 -> 'Type option
    abstract TryFindField : fullyQualifiedTypeName: string * tyParCount: int32 * fieldName: string -> 'Field option
    abstract TryFindFunction : enclosingType: (string * int32) * name: string * typeParameterCount: int32 * parameterCount: int32 * kind: OlyFunctionKind -> 'Function option
    abstract TryFindFunction : enclosingType: (string * int32) * name: string * typeParameterCount: int32 * parameterTypes: (string * int32) imarray * returnType: (string * int32) * kind: OlyFunctionKind -> 'Function option

type IOlyRuntimeEmitter<'Type, 'Function, 'Field> =

    abstract Initialize : vm: IOlyVirtualMachine<'Type, 'Function, 'Field> -> unit

    abstract EmitTypeNativeInt      : unit -> 'Type
    abstract EmitTypeNativeUInt     : unit -> 'Type
    abstract EmitTypeNativePtr      : elementTy: 'Type -> 'Type
    abstract EmitTypeNativeFunctionPtr    : OlyILCallingConvention * argTys: 'Type imarray * returnTy: 'Type -> 'Type
    abstract EmitTypeArray          : elementTy: 'Type * rank: int * kind: OlyIRArrayKind -> 'Type
    abstract EmitTypeFixedArray     : length: int * elementTy: 'Type * kind: OlyIRArrayKind -> 'Type
    abstract EmitTypeByRef          : elementTy: 'Type * kind: OlyIRByRefKind -> 'Type
    abstract EmitTypeBaseObject     : unit -> 'Type
    abstract EmitTypeVoid           : unit -> 'Type
    abstract EmitTypeUnit           : unit -> 'Type
    abstract EmitTypeInt8           : unit -> 'Type
    abstract EmitTypeUInt8          : unit -> 'Type
    abstract EmitTypeInt16          : unit -> 'Type
    abstract EmitTypeUInt16         : unit -> 'Type
    abstract EmitTypeInt32          : unit -> 'Type
    abstract EmitTypeUInt32         : unit -> 'Type
    abstract EmitTypeInt64          : unit -> 'Type
    abstract EmitTypeUInt64         : unit -> 'Type
    abstract EmitTypeFloat32        : unit -> 'Type
    abstract EmitTypeFloat64        : unit -> 'Type
    abstract EmitTypeBool           : unit -> 'Type

    abstract EmitTypeChar16         : unit -> 'Type
    abstract EmitTypeUtf16          : unit -> 'Type
    abstract EmitTypeVariable       : index: int32 * kind: OlyIRTypeVariableKind -> 'Type

    abstract EmitTypeHigherVariable : index: int32 * tyInst: 'Type imarray * kind: OlyIRTypeVariableKind -> 'Type
    /// Element type count will always be two or more.
    abstract EmitTypeTuple          : elementTys: 'Type imarray * elementNames: string imarray -> 'Type
    abstract EmitTypeFunction       : argTys: 'Type imarray * returnTy: 'Type * kind: OlyIRFunctionKind -> 'Type
    abstract EmitTypeRefCell  : ty: 'Type -> 'Type
    // TODO: When we expand literal types, we should create a 'kind' to represent the different possibilities
    //       and then use one function "EmitTypeLiteral".
    abstract EmitTypeConstantInt32   : value: int32 -> 'Type

    abstract EmitExternalType : 
        externalPlatform: string * 
        externalPath: string imarray * 
        externalName: string * 
        enclosing: Choice<string imarray, 'Type> * 
        kind: OlyILEntityKind * 
        flags: OlyIRTypeFlags * 
        name: string * 
        tyParCount: int 
            -> 'Type

    abstract EmitTypeDefinition : 
        enclosing: Choice<string imarray, 'Type> * 
        kind: OlyILEntityKind * 
        flags: OlyIRTypeFlags * 
        name: string * 
        tyParCount: int
            -> 'Type
    
    abstract EmitTypeDefinitionInfo : 
        ty: 'Type *
        enclosing: Choice<string imarray, 'Type> * 
        kind: OlyILEntityKind * 
        flags: OlyIRTypeFlags * 
        name: string *
        tyPars: OlyIRTypeParameter<'Type> imarray * 
        extends: 'Type imarray * 
        implements: 'Type imarray * 
        attrs: OlyIRAttribute<'Type, 'Function> imarray *
        runtimeTyOpt: 'Type option
            -> unit

    abstract OnTypeDefinitionEmitted : ty: 'Type -> unit

    abstract EmitTypeGenericInstance : ty: 'Type * tyArgs: 'Type imarray -> 'Type

    abstract EmitFunctionDefinition : 
        externalInfoOpt: OlyIRFunctionExternalInfo option * 
        enclosingTy: 'Type * flags: OlyIRFunctionFlags * 
        name: string * 
        tyPars: OlyIRTypeParameter<'Type> imarray * 
        pars: OlyIRParameter<'Type, 'Function> imarray * 
        returnTy: 'Type * 
        overrides: 'Function option * 
        sigKey: OlyIRFunctionSignatureKey * 
        attrs: OlyIRAttribute<'Type, 'Function> imarray 
            -> 'Function

    abstract EmitFunctionReference : 
        enclosingTy: 'Type * 
        formalFunc: 'Function
            -> 'Function

    abstract EmitFunctionInstance : 
        enclosingTy: 'Type * 
        formalFunc: 'Function * 
        tyArgs: 'Type imarray 
            -> 'Function

    abstract EmitFunctionBody : 
        body: Lazy<OlyIRFunctionBody<'Type, 'Function, 'Field>> *
        tier: OlyIRFunctionTier *
        func: 'Function 
            -> unit
    
    abstract EmitFieldDefinition : 
        enclosingTy: 'Type * 
        flags: OlyIRFieldFlags * 
        name: string * 
        ty: 'Type * 
        index: int32 *
        attrs: OlyIRAttribute<'Type, 'Function> imarray * 
        constValueOpt: OlyIRConstant<'Type, 'Function> option 
            -> 'Field

    abstract EmitFieldReference : 
        enclosingTy: 'Type * 
        formalField: 'Field
            -> 'Field

    abstract EmitProperty :
        enclosingTy: 'Type *
        name: string *
        ty: 'Type *
        attrs: OlyIRAttribute<'Type, 'Function> imarray *
        getterOpt: 'Function option *
        setterOpt: 'Function option
            -> unit

    // TODO: Add EmitPattern, though there are no target runtimes that understand patterns.