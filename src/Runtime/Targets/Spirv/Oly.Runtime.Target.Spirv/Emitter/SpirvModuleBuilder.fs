namespace rec Oly.Runtime.Target.Spirv.Emitter

// This is preventing warnings for recursive objects that we know are delayed
#nowarn "40"

open System
open System.Text
open System.Numerics
open System.Collections.Generic
open System.Collections.Concurrent
open Oly.Core
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.CodeGen
open Spirv
open Spirv.SpirvModule

type E = OlyIRExpression<SpirvType, SpirvFunction, SpirvField>
type O = OlyIROperation<SpirvType, SpirvFunction, SpirvField>
type V = OlyIRValue<SpirvType, SpirvFunction, SpirvField>
type C = OlyIRConstant<SpirvType, SpirvFunction>

type SpirvTypeFlags =
    | None  = 0b00

type SpirvFieldFlags =
    | None      = 0b0000

type SpirvVariableFlags =
    | None          = 0b000000000
    | Uniform       = 0b000000001

[<Sealed>]
type SpirvTypeStructBuilder(idResult: IdResult, enclosing: Choice<string imarray, SpirvType>, name: string, fields: List<SpirvField>, flags: SpirvTypeFlags) as this =

    let ty = SpirvType.Struct(this)

    member val Attributes = ImArray.empty with get, set

    member val Flags = flags with get, set

    member _.IdResult = idResult
    member _.Enclosing = enclosing
    member _.Name = name
    member _.Fields = fields

    member this.AsType = ty

[<RequireQualifiedAccess>]
type SpirvByRefKind =
    | ReadWrite
    | ReadOnly
    | WriteOnly

[<RequireQualifiedAccess>]
type SpirvArrayKind =
    | Immutable
    | Mutable

[<RequireQualifiedAccess>]
type SpirvType =
    | Invalid
    | Void of idResult: IdResult
    | Unit of idResult: IdResult
    | Int8 of idResult: IdResult
    | UInt8 of idResult: IdResult
    | Int16 of idResult: IdResult
    | UInt16 of idResult: IdResult
    | Int32 of idResult: IdResult
    | UInt32 of idResult: IdResult
    | Int64 of idResult: IdResult
    | UInt64 of idResult: IdResult
    | Float32 of idResult: IdResult
    | Float64 of idResult: IdResult
    | Bool of idResult: IdResult
    | Char16 of idResult: IdResult
    | Tuple of idResult: IdResult * itemTys: SpirvType imarray * itemNames: string imarray
    | ByRef of kind: SpirvByRefKind * elementTy: SpirvType
    | NativeInt of idResult: IdResult
    | NativeUInt of idResult: IdResult
    | NativePointer of idResult: IdResult * storageClass: StorageClass * elementTy: SpirvType

    | Vec2 of idResult: IdResult * elementTy: SpirvType
    | Vec3 of idResult: IdResult * elementTy: SpirvType
    | Vec4 of idResult: IdResult * elementTy: SpirvType

    | Function of idResult: IdResult * parTys: SpirvType imarray * returnTy: SpirvType

    | RuntimeArray of idResult: IdResult * kind: SpirvArrayKind * elementTy: SpirvType

    | Struct of SpirvTypeStructBuilder
    | Module of enclosing: Choice<string imarray, SpirvType> * name: string

    member this.GetSizeInBytes(): uint32 =
        match this with
        | Int8 _
        | UInt8 _ -> 1u
        | Int16 _
        | UInt16 _
        | Char16 _ -> 2u
        | Int32 _
        | UInt32 _ 
        | Float32 _ -> 4u
        | Int64 _
        | UInt64 _
        | Float64 _ -> 8u
        | Unit _ ->
            raise(NotImplementedException())
        | Bool _ ->
            raise(NotImplementedException())
        | Tuple _ ->
            raise(NotImplementedException())
        | ByRef _ ->
            raise(NotImplementedException())
        | NativeInt _ ->
            raise(NotImplementedException())
        | NativeUInt _ ->
            raise(NotImplementedException())
        | NativePointer _ ->
            raise(NotImplementedException())
        | Vec2 _ ->
            raise(NotImplementedException())
        | Vec3 _ ->
            raise(NotImplementedException())
        | Vec4 _ ->
            raise(NotImplementedException())
        | Function _ ->
            raise(NotSupportedException())
        | RuntimeArray _ ->
            raise(NotSupportedException())
        | Struct _ ->
            raise(NotImplementedException())
        | Void _
        | Module _
        | Invalid ->
            raise(InvalidOperationException())

    member this.GetDefinitionInstructions() =
        match this with
        | Invalid -> failwith "invalid"
        | Void idResult ->                  [OpTypeVoid(idResult)]
        | Unit idResult ->                  [OpTypeInt(idResult, 32u, 0u)]
        | Int8 idResult ->                  [OpTypeInt(idResult, 8u, 1u)]
        | UInt8 idResult ->                 [OpTypeInt(idResult, 8u, 0u)]
        | Int16 idResult ->                 [OpTypeInt(idResult, 16u, 1u)]
        | UInt16 idResult ->                [OpTypeInt(idResult, 16u, 0u)]
        | Int32 idResult ->                 [OpTypeInt(idResult, 32u, 1u)]
        | UInt32 idResult ->                [OpTypeInt(idResult, 32u, 0u)]
        | Int64 idResult ->                 [OpTypeInt(idResult, 64u, 1u)]
        | UInt64 idResult ->                [OpTypeInt(idResult, 64u, 0u)]
        | Float32 idResult ->               [OpTypeFloat(idResult, 32u, None)]
        | Float64 idResult ->               [OpTypeFloat(idResult, 64u, None)]
        | Bool idResult ->                  [OpTypeBool(idResult)]
        | Char16 idResult ->                [OpTypeInt(idResult, 16u, 0u)]
        | Vec2(idResult, elementTy) ->   [OpTypeVector(idResult, elementTy.IdResult, 2u)]
        | Vec3(idResult, elementTy) ->   [OpTypeVector(idResult, elementTy.IdResult, 3u)]
        | Vec4(idResult, elementTy) ->   [OpTypeVector(idResult, elementTy.IdResult, 4u)]

        | Tuple(idResult, itemTys, _) ->
            [
                OpTypeStruct(idResult, itemTys |> Seq.map (fun x -> x.IdResult) |> List.ofSeq)
            ]

        | ByRef _ ->
            raise(InvalidOperationException("ByRef must be lowered."))

        | NativeInt idResult ->             raise(NotImplementedException())
        | NativeUInt idResult ->            raise(NotImplementedException())
        | NativePointer(idResult, storageClass, elementTy) ->
            [
                OpTypePointer(idResult, storageClass, elementTy.IdResult)
            ]

        | Function(idResult, parTys, returnTy) ->
            [
                OpTypeFunction(idResult, returnTy.IdResult, parTys |> Seq.map (fun x -> x.IdResult) |> List.ofSeq)
            ]

        | RuntimeArray(idResult, _, elementTy) ->
            [
                OpTypeRuntimeArray(idResult, elementTy.IdResult)
                OpDecorate(idResult, Decoration.ArrayStride(elementTy.GetSizeInBytes()))
            ]

        | Struct(namedTy) -> 
            [
                for field in namedTy.Fields do
                    for attr in field.Attributes do
                        match attr.Data with
                        | BuiltInFunctionData.DecorateField(_, create)
                        | BuiltInFunctionData.DecorateFieldAndVariable(_, create, _, _) ->
                            yield! create namedTy.IdResult (uint32(field.Index))
                        | _ ->
                            raise(InvalidOperationException())

                for attr in namedTy.Attributes do
                    match attr.Data with
                    | BuiltInFunctionData.DecorateType(_, create) ->
                        yield! create namedTy.IdResult
                    | _ ->
                        raise(InvalidOperationException())

                OpTypeStruct(namedTy.IdResult, namedTy.Fields |> Seq.map (fun x -> x.Type.IdResult) |> List.ofSeq)
            ]

        | Module _ -> 
            failwith "Invalid type for 'GetDefinitionInstructions'."

    member this.IdResult =
        match this with
        | Invalid -> failwith "invalid"
        | Void idResult
        | Unit idResult
        | Int8 idResult
        | UInt8 idResult
        | Int16 idResult
        | UInt16 idResult
        | Int32 idResult
        | UInt32 idResult
        | Int64 idResult
        | UInt64 idResult
        | Float32 idResult
        | Float64 idResult
        | Bool idResult
        | Char16 idResult
        | Tuple(idResult, _, _)
        | NativeInt idResult
        | NativeUInt idResult
        | NativePointer(idResult, _, _)
        | Vec2(idResult, _)
        | Vec3(idResult, _)
        | Vec4(idResult, _) 
        | Function(idResult, _, _)
        | RuntimeArray(idResult, _, _) -> idResult
        | Struct(namedTy) -> namedTy.IdResult
        | Module _ -> failwith "Invalid type for 'IdResult'."
        | ByRef _ -> raise(InvalidOperationException("ByRef must be lowered."))

    member this.IsModule_t =
        match this with
        | Module _ -> true
        | _ -> false

    member this.IsStructBuilder =
        match this with
        | Struct _ -> true
        | _ -> false

    member this.AsStructBuilder =
        match this with
        | Struct(namedTyBuilder) -> namedTyBuilder
        | _ -> failwith "Expected named type."

[<Sealed>]
type SpirvFunctionBuilder(
    builder: SpirvModuleBuilder, 
    idResult: IdResult, 
    enclosingTy: SpirvType, 
    name: string, 
    irFlags: OlyIRFunctionFlags, 
    irPars: OlyIRParameter<SpirvType, SpirvFunction> imarray, 
    returnTy: SpirvType) as this =

    let func = SpirvFunction.Function(this)

    let realParTys = 
        // Lowering
        irPars 
        |> ImArray.map (fun x -> 
            if irFlags.IsEntryPoint then 
                let checkParameterElementType elementTy =
                    match elementTy with
                    | SpirvType.RuntimeArray _ -> raise(InvalidOperationException("Parameters cannot be runtime array types."))
                    | _ -> ()

                match x.Type with
                | SpirvType.ByRef(SpirvByRefKind.ReadOnly, elementTy) ->
                    checkParameterElementType elementTy

                    let isUniform = 
                        x.Attributes 
                        |> ImArray.exists (fun x -> 
                            match x with
                            | OlyIRAttribute(ctor, _, _) ->
                                match ctor.TryGetBuiltIn() with
                                | ValueSome(builtInFunc) ->
                                    match builtInFunc.Data with
                                    | BuiltInFunctionData.DecorateVariable(varFlags, _)
                                    | BuiltInFunctionData.DecorateFieldAndVariable(_, _, varFlags, _) ->
                                        varFlags.HasFlag(SpirvVariableFlags.Uniform)
                                    | _ ->
                                        false
                                | _ ->
                                    false
                        )

                    if isUniform then
                        builder.GetTypePointer(StorageClass.Uniform, elementTy)
                    else
                        builder.GetTypePointer(StorageClass.Input, elementTy)

                | SpirvType.ByRef(SpirvByRefKind.WriteOnly, elementTy) ->
                    checkParameterElementType elementTy
                    builder.GetTypePointer(StorageClass.Output, elementTy)

                | _ ->
                    failwith "Expected a read-only or write-only by-ref type."
            else 
                match x.Type with
                | SpirvType.ByRef(_, elementTy) ->
                    builder.GetTypePointer(StorageClass.Function, elementTy)
                | ty ->
                    ty
        )

    let parTys =
        // Lowering
        if irFlags.IsEntryPoint then
            ImArray.empty
        else
            realParTys

    let returnTy =
        // Lowering
        if irFlags.IsEntryPoint then
            builder.GetTypeVoid()
        else
            match returnTy with
            | SpirvType.ByRef(elementTy=elementTy) ->
                builder.GetTypePointer(StorageClass.Function, elementTy)
            | _ ->
                returnTy

    let funcTy = SpirvType.Function(builder.NewIdResult(), parTys, returnTy)

    let realPars =
        realParTys
        |> ImArray.mapi (fun i parTy ->

            let varIdRef = (builder.NewIdResult(): IdRef)

            let decorateInstrs = 
                let irPar = irPars[i]
                [
                    for attr in irPar.Attributes do
                        match attr with
                        | OlyIRAttribute(ctor, args, _) ->
                            match ctor.TryGetBuiltIn() with
                            | ValueSome builtInFunc ->
                                match builtInFunc.Data with
                                | BuiltInFunctionData.DecorateVariable(_, create)
                                | BuiltInFunctionData.DecorateFieldAndVariable(_, _, _, create) ->
                                    yield! create varIdRef args
                                | _ ->
                                    raise(InvalidOperationException())
                            | _ ->
                                ()
                ]

            {| VariableIdRef = varIdRef; Type = parTy; DecorateInstructions = decorateInstrs |}
        )

    let pars =
        // Lowering
        if irFlags.IsEntryPoint then
            ImArray.empty
        else
            realPars

    member _.IdResult = idResult
    member _.EnclosingType: SpirvType = enclosingTy
    member _.Name = name
    member val Instructions : Instruction list = [] with get, set
    member _.Type = funcTy
    member _.ReturnType = returnTy
    member _.Parameters = pars

    member _.IsEntryPoint = irFlags.IsEntryPoint
    member this.EntryPointParameters = 
        if not this.IsEntryPoint then
            raise(InvalidOperationException("Expected entry point."))
        realPars

    member _.AsFunction = func

[<RequireQualifiedAccess>]
type SpirvFunction =
    | Function of SpirvFunctionBuilder
    | BuiltIn of BuiltInFunction
    | AccessChain

    member this.TryGetBuiltIn() =
        match this with
        | BuiltIn(builtInFunc) -> ValueSome builtInFunc
        | _ -> ValueNone

type SpirvField =
    {
        Name: string
        Type: SpirvType
        Index: int32
        Flags: SpirvFieldFlags
        Attributes: BuiltInFunction imarray
    }

// --

type BuiltInFunctionParameterType =
    | Void
    | Int32
    | UInt32
    | Float32
    | Vector2Float32
    | Vector3Float32
    | Vector4Float32

    member this.IsValid(ty: SpirvType) =
        match this, ty with
        | Void, SpirvType.Void _
        | Int32, SpirvType.Int32 _
        | UInt32, SpirvType.UInt32 _
        | Float32, SpirvType.Float32 _
        | Vector2Float32, SpirvType.Vec2(elementTy=SpirvType.Float32 _)
        | Vector3Float32, SpirvType.Vec3(elementTy=SpirvType.Float32 _)
        | Vector4Float32, SpirvType.Vec4(elementTy=SpirvType.Float32 _) -> true
        | _ -> false

type BuiltInFunctionFlags =
    | None                          = 0b0000000000
    | Constructor                   = 0b0000000001
    | DynamicParameters      = 0b0000000010 

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type BuiltInFunctionData =
    | DecorateField of SpirvFieldFlags * (IdRef -> uint32 -> Instruction list)
    | DecorateType of SpirvTypeFlags * (IdRef -> Instruction list)
    | DecorateVariable of SpirvVariableFlags * (IdRef -> C imarray -> Instruction list)
    | DecorateFieldAndVariable of fieldFlags: SpirvFieldFlags * createFieldInstrs: (IdRef -> uint32 -> Instruction list) * varFlags: SpirvVariableFlags * createVarInstrs: (IdRef -> C imarray -> Instruction list)
    | Intrinsic of (SpirvModuleBuilder -> Choice<IdRef, C> imarray -> IdRef * Instruction list)

[<NoEquality;NoComparison>]
type BuiltInFunction = 
    { 
        Name: string;
        ParameterTypes: BuiltInFunctionParameterType imarray
        ReturnType: BuiltInFunctionParameterType 
        Flags: BuiltInFunctionFlags
        Data: BuiltInFunctionData
    }

    member this.IsValidParameterTypes(parTys: SpirvType imarray) =
        if this.Flags.HasFlag(BuiltInFunctionFlags.DynamicParameters) then true
        else
            this.ParameterTypes.Length = parTys.Length &&
            (this.ParameterTypes, parTys) 
            ||> ImArray.forall2 (fun x parTy -> x.IsValid(parTy))

    member this.IsValid(name: string, parTys: SpirvType imarray, returnTy: SpirvType, irFlags: OlyIRFunctionFlags) =
        if this.Name <> name then 
            false
        elif not(this.ReturnType.IsValid(returnTy)) then
            false
        elif not(this.IsValidParameterTypes(parTys)) then
            false
        else
            if this.Flags.HasFlag(BuiltInFunctionFlags.Constructor) then
                if irFlags.IsConstructor && irFlags.IsInstance then
                    true
                else
                    false
            else
                false

module BuiltInFunctions =

    let private Lookup = new Dictionary<string, SpirvFunction>()

    let private Add(
            name: string,
            data: BuiltInFunctionData,
            parTys: BuiltInFunctionParameterType list, 
            returnTy: BuiltInFunctionParameterType, 
            flags: BuiltInFunctionFlags) =

        if flags.HasFlag(BuiltInFunctionFlags.DynamicParameters) && not(List.isEmpty parTys) then
            failwith "Dynamic parameters require that the parameter types are empty since they can be dynamic."

        Lookup.Add(
            name,
            {
                Name = name
                Data = data
                ParameterTypes = parTys |> ImArray.ofSeq
                ReturnType = returnTy
                Flags = flags
            } |> SpirvFunction.BuiltIn
        )

    do
        Add("position",                
            BuiltInFunctionData.DecorateFieldAndVariable(
                SpirvFieldFlags.None, 
                (fun tyIdRef index ->
                    [OpMemberDecorate(tyIdRef, index, Decoration.BuiltIn BuiltIn.Position)]
                ), 
                SpirvVariableFlags.None, 
                fun varIdRef args ->
                    [OpDecorate(varIdRef, Decoration.BuiltIn BuiltIn.Position)]
            ),
            [],                                             
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor)
        Add("point_size",            
            BuiltInFunctionData.DecorateFieldAndVariable(
                SpirvFieldFlags.None, 
                (fun tyIdRef index ->
                    [OpMemberDecorate(tyIdRef, index, Decoration.BuiltIn BuiltIn.PointSize)]
                ), 
                SpirvVariableFlags.None, 
                fun varIdRef _ ->
                    [OpDecorate(varIdRef, Decoration.BuiltIn BuiltIn.PointSize)]
            ),        
            [],                                             
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor)
        Add("block",                     
            BuiltInFunctionData.DecorateType(
                SpirvTypeFlags.None, 
                fun tyIdRef ->
                    [OpDecorate(tyIdRef, Decoration.Block)]
            ),      
            [],                                             
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor)
        Add("buffer_block",                     
            BuiltInFunctionData.DecorateType(
                SpirvTypeFlags.None, 
                fun tyIdRef ->
                    [OpDecorate(tyIdRef, Decoration.BufferBlock)]
            ),      
            [],                                             
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor)
        Add("location",             
            BuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.None, 
                fun varIdRef args ->
                    match args[0] with
                    | C.UInt32(value) ->
                        [OpDecorate(varIdRef, Decoration.Location value)]
                    | _ ->
                        raise(InvalidOperationException()) 
            ),     
            [BuiltInFunctionParameterType.UInt32],          
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor)
        Add("uniform",             
            BuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.Uniform, 
                fun _varIdRef _args ->
                    []
            ),     
            [],          
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor) 
        Add("descriptor_set",             
            BuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.None, 
                fun varIdRef args ->
                    match args[0] with
                    | C.UInt32(value) ->
                        [OpDecorate(varIdRef, Decoration.DescriptorSet value)]
                    | _ ->
                        raise(InvalidOperationException()) 
            ),     
            [BuiltInFunctionParameterType.UInt32],          
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor)
        Add("binding",             
            BuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.None, 
                fun varIdRef args ->
                    match args[0] with
                    | C.UInt32(value) ->
                        [OpDecorate(varIdRef, Decoration.Binding value)]
                    | _ ->
                        raise(InvalidOperationException()) 
            ),     
            [BuiltInFunctionParameterType.UInt32],          
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor) 
        Add("global_invocation_id",             
            BuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.None, 
                fun varIdRef _ ->
                    [OpDecorate(varIdRef, Decoration.BuiltIn BuiltIn.GlobalInvocationId)]
            ),     
            [],          
            BuiltInFunctionParameterType.Void, 
            BuiltInFunctionFlags.Constructor) 
        Add("vec4",
            BuiltInFunctionData.Intrinsic(
                fun spvModule args ->
                    match args.Length with
                    | 1 ->
                        match args[0] with
                        | Choice2Of2(C.Float32 value) ->
                            (spvModule.GetConstantVector4Float32(value, value, value, value), [])
                        | Choice1Of2 _ ->
                            raise(NotImplementedException())
                        | _ ->
                            raise(InvalidOperationException())

                    | 3 ->
                            let idRefs =
                                args
                                |> ImArray.map (function
                                    | Choice1Of2(idRef) -> idRef
                                    | Choice2Of2(cns) ->
                                        match cns with
                                        | C.Float32 value ->
                                            spvModule.GetConstantFloat32(value)
                                        | _ ->
                                            raise(InvalidOperationException())
                                )
                            let arg0IdResult = spvModule.NewIdResult()
                            let arg1IdResult = spvModule.NewIdResult()
                            let arg2IdRef = idRefs[1]
                            let arg3IdRef = idRefs[2]
                            let idResult = spvModule.NewIdResult()
                            idResult,
                            [
                                OpCompositeExtract(spvModule.GetTypeFloat32().IdResult, arg0IdResult, idRefs[0], [0u])
                                OpCompositeExtract(spvModule.GetTypeFloat32().IdResult, arg1IdResult, idRefs[0], [1u])
                                OpCompositeConstruct(spvModule.GetTypeVec4().IdResult, idResult, [arg0IdResult; arg1IdResult; arg2IdRef; arg3IdRef])
                            ]
                    | _ ->
                        raise(NotImplementedException())
            ),
            [],
            BuiltInFunctionParameterType.Void,
            BuiltInFunctionFlags.Constructor ||| BuiltInFunctionFlags.DynamicParameters
        )

    let TryGetBuiltInFunction(path: string imarray, parTys: SpirvType imarray, returnTy: SpirvType, irFlags: OlyIRFunctionFlags) : SpirvFunction option =
        if path.Length < 2 || path.Length > 2 then None
        elif path[0] <> "__oly_spirv_" then None
        else          
            let name = path[1]
            match Lookup.TryGetValue(name) with
            | true, func ->
                match func.TryGetBuiltIn() with
                | ValueSome(builtInFunc) when builtInFunc.IsValid(name, parTys, returnTy, irFlags) ->
                    Some func
                | _ ->
                    None
            | _ ->
                None

    let IsValid(name: string) = Lookup.ContainsKey(name)

module BuiltInTypes =

    let TryGetByName(spvModule: SpirvModuleBuilder, name: string) =
        match name with
        | "vec2" -> spvModule.GetTypeVec2() |> ValueSome
        | "vec3" -> spvModule.GetTypeVec3() |> ValueSome
        | "vec4" -> spvModule.GetTypeVec4() |> ValueSome
        | "uvec3" -> spvModule.GetTypeUVec3() |> ValueSome
        | _ -> ValueNone

// --

[<Sealed>]
type SpirvModuleBuilder(executionModel: ExecutionModel) =

    let mutable isBuilding = false

    let types = List<SpirvType>()
    let funcs = List<SpirvFunctionBuilder>()

    let pointerTys = Dictionary<(StorageClass * IdRef), SpirvType>()
    let arrayTys = Dictionary<(SpirvArrayKind * IdRef), SpirvType>()

    let constantsInt32 = Dictionary<int32, IdResult>()
    let constantsUInt32 = Dictionary<uint32, IdResult>()
    let constantsFloat32 = Dictionary<float32, IdResult>()
    let constantsVectorFloat32 = Dictionary<IdRef list, IdResult>()

    let vertexHeaderInstrs =
        [
            OpCapability(Capability.Shader)
            OpExtInstImport(1u, "GLSL.std.450")
            OpMemoryModel(AddressingModel.Logical, MemoryModel.GLSL450)
        ]

    let mutable entryPointInstrs = []

    let mutable varInstrs = []

    (* cached types *)
    let mutable cachedTypeVoid           = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeBool           = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeInt32          = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeUInt32         = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeFloat32        = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeVec2 = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeVec3 = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeVec4 = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeUVec3 = Unchecked.defaultof<SpirvType>
    (**)

    let mutable newIdResultValue = 2u // This is '2u' because 'OpExtInstImport' uses '1u'.
    member _.NewIdResult() =
        let value = newIdResultValue
        newIdResultValue <- value + 1u
        value

    member this.GetConstantInt32(value: int32) =
        match constantsInt32.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeInt32() // eval type
            let idResult = this.NewIdResult()
            constantsInt32[value] <- idResult
            idResult

    member this.GetConstantUInt32(value: uint32) =
        match constantsUInt32.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeUInt32() // eval type
            let idResult = this.NewIdResult()
            constantsUInt32[value] <- idResult
            idResult

    member this.GetConstantFloat32(value: float32) =
        match constantsFloat32.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeFloat32() // eval type
            let idResult = this.NewIdResult()
            constantsFloat32[value] <- idResult
            idResult

    member this.GetConstantVector3Float32(x: float32, y: float32, z: float32) =
        let xIdRef : IdRef = this.GetConstantFloat32(x)
        let yIdRef : IdRef = this.GetConstantFloat32(y)
        let zIdRef : IdRef = this.GetConstantFloat32(z)

        let key = [xIdRef;yIdRef;zIdRef]
        match constantsVectorFloat32.TryGetValue key with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeVec3() // eval type
            let idResult = this.NewIdResult()
            constantsVectorFloat32[key] <- idResult
            idResult

    member this.GetConstantVector4Float32(x: float32, y: float32, z: float32, w: float32) : IdResult =
        let xIdRef : IdRef = this.GetConstantFloat32(x)
        let yIdRef : IdRef = this.GetConstantFloat32(y)
        let zIdRef : IdRef = this.GetConstantFloat32(z)
        let wIdRef : IdRef = this.GetConstantFloat32(w)

        let key = [xIdRef;yIdRef;zIdRef;wIdRef]
        match constantsVectorFloat32.TryGetValue key with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeVec4() // eval type
            let idResult = this.NewIdResult()
            constantsVectorFloat32[key] <- idResult
            idResult

    member this.GetTypePointer(storageClass: StorageClass, elementTy: SpirvType) =
        let key = (storageClass, elementTy.IdResult)
        match pointerTys.TryGetValue key with
        | true, ty -> ty
        | _ ->
            let idResult = this.NewIdResult()
            let ty = SpirvType.NativePointer(idResult, storageClass, elementTy)
            this.AddType(ty)
            pointerTys[key] <- ty
            ty

    member this.GetTypeArray(kind: SpirvArrayKind, elementTy: SpirvType) =
        let key = (kind, elementTy.IdResult)
        match arrayTys.TryGetValue key with
        | true, ty -> ty
        | _ ->
            let idResult = this.NewIdResult()
            let ty = SpirvType.RuntimeArray(idResult, kind, elementTy)
            this.AddType(ty)
            arrayTys[key] <- ty
            ty

    member this.GetTypeVoid() : SpirvType =
        if isNull(box cachedTypeVoid) then
            cachedTypeVoid <- SpirvType.Void(this.NewIdResult())
            this.AddType(cachedTypeVoid)
        cachedTypeVoid

    member this.GetTypeBool() : SpirvType =
        if isNull(box cachedTypeBool) then
            cachedTypeBool <- SpirvType.Bool(this.NewIdResult())
            this.AddType(cachedTypeBool)
        cachedTypeBool

    member this.GetTypeInt32() : SpirvType =
        if isNull(box cachedTypeInt32) then
            cachedTypeInt32 <- SpirvType.Int32(this.NewIdResult())
            this.AddType(cachedTypeInt32)
        cachedTypeInt32

    member this.GetTypeUInt32() : SpirvType =
        if isNull(box cachedTypeUInt32) then
            cachedTypeUInt32 <- SpirvType.UInt32(this.NewIdResult())
            this.AddType(cachedTypeUInt32)
        cachedTypeUInt32

    member this.GetTypeFloat32() : SpirvType =
        if isNull(box cachedTypeFloat32) then
            cachedTypeFloat32 <- SpirvType.Float32(this.NewIdResult())
            this.AddType(cachedTypeFloat32)
        cachedTypeFloat32

    member this.GetTypeVec2() : SpirvType =
        if isNull(box cachedTypeVec2) then
            cachedTypeVec2 <- SpirvType.Vec2(this.NewIdResult(), this.GetTypeFloat32())
            this.AddType(cachedTypeVec2)
        cachedTypeVec2

    member this.GetTypeVec3() : SpirvType =
        if isNull(box cachedTypeVec3) then
            cachedTypeVec3 <- SpirvType.Vec3(this.NewIdResult(), this.GetTypeFloat32())
            this.AddType(cachedTypeVec3)
        cachedTypeVec3

    member this.GetTypeVec4() : SpirvType =
        if isNull(box cachedTypeVec4) then
            cachedTypeVec4 <- SpirvType.Vec4(this.NewIdResult(), this.GetTypeFloat32())
            this.AddType(cachedTypeVec4)
        cachedTypeVec4

    member this.GetTypeUVec3() : SpirvType =
        if isNull(box cachedTypeUVec3) then
            cachedTypeUVec3 <- SpirvType.Vec4(this.NewIdResult(), this.GetTypeUInt32())
            this.AddType(cachedTypeUVec3)
        cachedTypeUVec3

    member this.GetTypeTuple(elementTys: SpirvType imarray, elementNames: string imarray): SpirvType = 
        raise(NotImplementedException())
        //let ty = SpirvType.Tuple(this.NewIdResult(), elementTys, elementNames)
        //this.AddType(ty)
        //ty

    member this.CreateTypeStructBuilder(enclosing: Choice<string imarray, SpirvType>, name: string) =
        SpirvTypeStructBuilder(this.NewIdResult(), enclosing, name, List(), SpirvTypeFlags.None)

    member this.AddTypeStructBuilder(structBuilder: SpirvTypeStructBuilder) =
        if isBuilding then
            failwith "Unable to add type while the module is building."
        types.Add(structBuilder.AsType)

    member private _.AddType(ty) =
        if isBuilding then
            failwith "Unable to add type while the module is building."
        types.Add(ty)

    member this.CreateFunctionBuilder(enclosingTy: SpirvType, name: string, irFlags: OlyIRFunctionFlags, irPars: OlyIRParameter<SpirvType, SpirvFunction> imarray, returnTy: SpirvType) =
        let func = SpirvFunctionBuilder(this, this.NewIdResult(), enclosingTy, name, irFlags, irPars, returnTy)
        funcs.Add(func)

        this.AddType(func.Type) // TODO: Cache this!

        if func.IsEntryPoint then
            match entryPointInstrs with
            | [] ->
                let variableOfPointerOfBlockIdResults =
                    func.EntryPointParameters
                    |> Seq.map (fun par ->
                        par.VariableIdRef
                    )
                    |> List.ofSeq

                entryPointInstrs <-
                    [
                        OpEntryPoint(executionModel, func.IdResult, func.Name, variableOfPointerOfBlockIdResults)
                        if executionModel = ExecutionModel.Fragment then
                            OpExecutionMode(func.IdResult, ExecutionMode.OriginUpperLeft)
                        elif executionModel = ExecutionModel.GLCompute then
                            OpExecutionMode(func.IdResult, ExecutionMode.LocalSize(1u, 1u, 1u)) // TODO: What is this?
                    ]
            | _ ->
                failwith "Entry point already set."
        func

    member this.Build() =
        if isBuilding then
            raise(InvalidOperationException())

        isBuilding <- true
        let instrs =
            vertexHeaderInstrs @
            entryPointInstrs @
            (
                types
                |> Seq.collect (fun x ->
                    x.GetDefinitionInstructions()
                )
                |> List.ofSeq
            ) @
            varInstrs @
            ( 
                constantsInt32 
                |> Seq.map (fun pair -> OpConstant(this.GetTypeInt32().IdResult, pair.Value, uint32(pair.Key))) 
                |> List.ofSeq
            ) @
            ( 
                constantsUInt32 
                |> Seq.map (fun pair -> OpConstant(this.GetTypeUInt32().IdResult, pair.Value, uint32(pair.Key))) 
                |> List.ofSeq
            ) @
            (
                constantsFloat32 
                |> Seq.map (fun pair -> OpConstant(this.GetTypeFloat32().IdResult, pair.Value, System.Runtime.CompilerServices.Unsafe.BitCast(pair.Key))) 
                |> List.ofSeq
            ) @
            (
                constantsVectorFloat32
                |> Seq.map (fun pair -> 
                    let n = pair.Key.Length
                    match n with
                    | 3 ->
                        OpConstantComposite(this.GetTypeVec3().IdResult, pair.Value, pair.Key)
                    | 4 ->
                        OpConstantComposite(this.GetTypeVec4().IdResult, pair.Value, pair.Key)
                    | _ ->
                        raise(NotImplementedException($"Vector{n} of float32 constant"))
                )
                |> List.ofSeq
            ) @
            (
                funcs
                |> Seq.collect (fun x -> x.Instructions)
                |> List.ofSeq
            )

        let normalizedInstrs = Normalization.Normalize instrs

        let version = SpirvModule.CreateVersion(1u, 0u)
        let result = SpirvModule.Create(version, instrs = normalizedInstrs)
        isBuilding <- false
        result

module private Normalization =

    let Normalize (instrs: Instruction list) =
        let headerWithEntryPoint = List()
        let executionModes = List()
        let memberDecorates = List()
        let decorates = List()
        let other = List()

        let mutable hasEntryPoint = false

        instrs
        |> List.iter (fun instr ->
            match instr with
            | OpMemberDecorate _ ->
                if hasEntryPoint |> not then
                    raise(InvalidOperationException())
                memberDecorates.Add(instr)

            | OpDecorate _ ->
                if hasEntryPoint |> not then
                    raise(InvalidOperationException())
                decorates.Add(instr)

            | OpEntryPoint _ ->
                headerWithEntryPoint.Add(instr)
                hasEntryPoint <- true

            | OpExecutionMode _ ->
                if hasEntryPoint |> not then
                    raise(InvalidOperationException())
                executionModes.Add(instr)

            | _ ->
                if hasEntryPoint then
                    other.Add(instr)
                else
                    headerWithEntryPoint.Add(instr)
        )

        [
            yield! headerWithEntryPoint
            yield! executionModes
            yield! memberDecorates
            yield! decorates
            yield! other
        ]
        