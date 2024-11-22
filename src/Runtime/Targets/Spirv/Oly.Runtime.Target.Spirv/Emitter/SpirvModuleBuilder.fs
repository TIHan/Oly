namespace rec Oly.Runtime.Target.Spirv.Emitter

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

type SpirvTypeFlags =
    | None  = 0b00
    | Block = 0b01

[<Sealed>]
type SpirvNamedTypeBuilder(idResult: IdResult, enclosing: Choice<string imarray, SpirvType>, name: string, fields: List<SpirvField>, flags: SpirvTypeFlags) as this =

    let ty = SpirvType.Named(this)

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
    | ByRef of idResult: IdResult * kind: SpirvByRefKind * elementTy: SpirvType
    | NativeInt of idResult: IdResult
    | NativeUInt of idResult: IdResult
    | NativePointer of idResult: IdResult * storageClass: StorageClass * elementTy: SpirvType

    | Vector2 of idResult: IdResult * elementTy: SpirvType
    | Vector3 of idResult: IdResult * elementTy: SpirvType
    | Vector4 of idResult: IdResult * elementTy: SpirvType

    | Function of idResult: IdResult * parTys: SpirvType imarray * returnTy: SpirvType

    | Named of SpirvNamedTypeBuilder
    | Module of enclosing: Choice<string imarray, SpirvType> * name: string

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
        | Bool idResult ->                  [OpTypeInt(idResult, 32u, 1u)]
        | Char16 idResult ->                [OpTypeInt(idResult, 16u, 0u)]
        | Vector2(idResult, elementTy) ->   [OpTypeVector(idResult, elementTy.IdResult, 2u)]
        | Vector3(idResult, elementTy) ->   [OpTypeVector(idResult, elementTy.IdResult, 3u)]
        | Vector4(idResult, elementTy) ->   [OpTypeVector(idResult, elementTy.IdResult, 4u)]

        | Tuple(idResult, itemTys, _) ->
            [
                OpTypeStruct(idResult, itemTys |> Seq.map (fun x -> x.IdResult) |> List.ofSeq)
            ]

        | ByRef(idResult, _, elementTy) ->
            [
                OpTypePointer(idResult, StorageClass.Private, elementTy.IdResult)
            ]
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

        | Named(namedTy) -> 
            [
                for field in namedTy.Fields do
                    if field.Flags.HasFlag(SpirvFieldFlags.Position) then
                        OpMemberDecorate(namedTy.IdResult, uint32(field.Index), Decoration.BuiltIn BuiltIn.Position)
                     //   OpMemberDecorate(namedTy.IdResult, uint32(field.Index + 1), Decoration.BuiltIn BuiltIn.PointSize)
                     //   OpMemberDecorate(namedTy.IdResult, uint32(field.Index + 2), Decoration.BuiltIn BuiltIn.ClipDistance)
                     //   OpMemberDecorate(namedTy.IdResult, uint32(field.Index + 3), Decoration.BuiltIn BuiltIn.CullDistance)

                if namedTy.Flags.HasFlag(SpirvTypeFlags.Block) then
                    OpDecorate(namedTy.IdResult, Decoration.Block)

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
        | ByRef(idResult, _, _)
        | NativeInt idResult
        | NativeUInt idResult
        | NativePointer(idResult, _, _)
        | Vector2(idResult, _)
        | Vector3(idResult, _)
        | Vector4(idResult, _) 
        | Function(idResult, _, _)-> idResult
        | Named(namedTy) -> namedTy.IdResult
        | Module _ -> failwith "Invalid type for 'IdResult'."

    member this.IsModule_t =
        match this with
        | Module _ -> true
        | _ -> false

    member this.IsNamedTypeBuilder =
        match this with
        | Named _ -> true
        | _ -> false

    member this.AsNamedTypeBuilder =
        match this with
        | Named(namedTyBuilder) -> namedTyBuilder
        | _ -> failwith "Expected named type."

[<Sealed>]
type SpirvFunctionBuilder(builder: SpirvModuleBuilder, idResult: IdResult, enclosingTy: SpirvType, name: string, irFlags: OlyIRFunctionFlags, irPars: OlyIRParameter<SpirvType, SpirvFunction> imarray, returnTy: SpirvType) as this =

    let func = SpirvFunction.Function(this)

    let realParTys = 
        irPars 
        |> ImArray.map (fun x -> 
            if irFlags.IsEntryPoint then 
                match x.Type with
                | SpirvType.ByRef(_, SpirvByRefKind.ReadOnly, elementTy) ->
                    builder.GetTypePointer(StorageClass.Input, elementTy)
                | SpirvType.ByRef(_, SpirvByRefKind.WriteOnly, elementTy) ->
                    builder.GetTypePointer(StorageClass.Output, elementTy)
                | _ ->
                    failwith "Expected a read-only or write-only by-ref type."
            else 
                x.Type
        )

    let funcTy = SpirvType.Function(builder.NewIdResult(), realParTys, returnTy)

    let pars =
        realParTys
        |> ImArray.mapi (fun i parTy ->

            let varIdRef = (builder.NewIdResult(): IdRef)

            let decorateInstrs = 
                let irPar = irPars[i]
                [
                    for attr in irPar.Attributes do
                        match attr with
                        | OlyIRAttribute(ctor, args, _) ->
                            match ctor with
                            | SpirvFunction.Attribute_Location _ ->
                                if args.Length = 1 then
                                    match args[0] with
                                    | OlyIRConstant.UInt32(value) ->
                                        OpDecorate(varIdRef, Decoration.Location value)
                                    | _ ->
                                        raise(InvalidOperationException())
                                else
                                    raise(InvalidOperationException())
                            | SpirvFunction.Attribute_Position _ ->
                                if args.IsEmpty then
                                    OpDecorate(varIdRef, Decoration.BuiltIn BuiltIn.Position)
                                else
                                    raise(InvalidOperationException())
                            | SpirvFunction.Attribute_PointSize _ ->
                                if args.IsEmpty then
                                    OpDecorate(varIdRef, Decoration.BuiltIn BuiltIn.PointSize)
                                else
                                    raise(InvalidOperationException())
                            | SpirvFunction.Attribute_Block _ ->
                                raise(InvalidOperationException())
                                
                            | _ ->
                                ()
                ]

            {| VariableIdRef = varIdRef; Type = parTy; DecorateInstructions = decorateInstrs |}
        )

    member _.IdResult = idResult
    member _.EnclosingType: SpirvType = enclosingTy
    member _.Name = name
    member val Instructions : Instruction list = [] with get, set
    member _.Type = funcTy
    member _.ReturnType = returnTy
    member _.Parameters = pars

    member _.IsEntryPoint = irFlags.IsEntryPoint

    member _.AsFunction = func

[<RequireQualifiedAccess>]
type SpirvFunction =
    | Function of SpirvFunctionBuilder
    | NewVector4 of SpirvType imarray
    | Attribute_Position of BuiltInFunction
    | Attribute_PointSize of BuiltInFunction
    | Attribute_Block of BuiltInFunction
    | Attribute_Location of BuiltInFunction

    member this.TryGetBuiltIn() =
        match this with
        | Attribute_Position(builtInFunc)
        | Attribute_PointSize(builtInFunc)
        | Attribute_Block(builtInFunc)
        | Attribute_Location(builtInFunc) -> ValueSome builtInFunc
        | _ -> ValueNone

type SpirvFieldFlags =
    | None      = 0b0000
    | Position  = 0b0001
    | PointSize = 0b0010

type SpirvField =
    {
        Name: string
        Type: SpirvType
        Index: int32
        Flags: SpirvFieldFlags
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
        | Vector2Float32, SpirvType.Vector2(elementTy=SpirvType.Float32 _)
        | Vector3Float32, SpirvType.Vector3(elementTy=SpirvType.Float32 _)
        | Vector4Float32, SpirvType.Vector4(elementTy=SpirvType.Float32 _) -> true
        | _ -> false

type BuiltInFunctionFlags =
    | None              = 0b0000000000
    | Constructor       = 0b0000000001
    | DecorateVariable  = 0b0000000010
    | DecorateField     = 0b0000000100
    | DecorateType      = 0b0000001000

type BuiltInFunction = 
    { 
        Name: string;
        ParameterTypes: BuiltInFunctionParameterType imarray
        ReturnType: BuiltInFunctionParameterType 
        Flags: BuiltInFunctionFlags
        DecorateFieldFlags: SpirvFieldFlags
        DecorateTypeFlags: SpirvTypeFlags
    }

    member this.IsValid(name: string, parTys: SpirvType imarray, returnTy: SpirvType, irFlags: OlyIRFunctionFlags) =
        if this.Name <> name then 
            false
        elif this.ParameterTypes.Length <> parTys.Length then 
            false
        elif not(this.ReturnType.IsValid(returnTy)) then
            false
        elif (this.ParameterTypes, parTys) ||> ImArray.forall2 (fun x parTy -> x.IsValid(parTy)) |> not then
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

    let Lookup = new Dictionary<string, SpirvFunction>()

    let Add(name: string, createFunc: BuiltInFunction -> SpirvFunction, fieldFlags: SpirvFieldFlags, tyFlags: SpirvTypeFlags, parTys: BuiltInFunctionParameterType list, returnTy: BuiltInFunctionParameterType, flags: BuiltInFunctionFlags) =
        if flags.HasFlag(BuiltInFunctionFlags.DecorateField) then
            if fieldFlags = SpirvFieldFlags.None then
                raise(InvalidOperationException())
        else
            if fieldFlags <> SpirvFieldFlags.None then
                raise(InvalidOperationException())

        if flags.HasFlag(BuiltInFunctionFlags.DecorateType) then
            if tyFlags = SpirvTypeFlags.None then
                raise(InvalidOperationException())
        else
            if tyFlags <> SpirvTypeFlags.None then
                raise(InvalidOperationException())

        Lookup.Add(
            name,
            {
                Name = name
                ParameterTypes = parTys |> ImArray.ofSeq
                ReturnType = returnTy
                Flags = flags
                DecorateTypeFlags = tyFlags
                DecorateFieldFlags = fieldFlags
            } |> createFunc
        )

    do
        Add("position",             SpirvFunction.Attribute_Position,       SpirvFieldFlags.Position,    SpirvTypeFlags.None,      [],                                             BuiltInFunctionParameterType.Void, BuiltInFunctionFlags.DecorateField ||| BuiltInFunctionFlags.DecorateVariable ||| BuiltInFunctionFlags.Constructor)
        Add("point_size",           SpirvFunction.Attribute_PointSize,      SpirvFieldFlags.PointSize,   SpirvTypeFlags.None,      [],                                             BuiltInFunctionParameterType.Void, BuiltInFunctionFlags.DecorateField ||| BuiltInFunctionFlags.DecorateVariable ||| BuiltInFunctionFlags.Constructor)
        Add("block",                SpirvFunction.Attribute_Block,          SpirvFieldFlags.None,        SpirvTypeFlags.Block,     [],                                             BuiltInFunctionParameterType.Void, BuiltInFunctionFlags.DecorateType ||| BuiltInFunctionFlags.Constructor)
        Add("location",             SpirvFunction.Attribute_Location,       SpirvFieldFlags.None,        SpirvTypeFlags.None,      [BuiltInFunctionParameterType.UInt32],          BuiltInFunctionParameterType.Void, BuiltInFunctionFlags.DecorateVariable ||| BuiltInFunctionFlags.Constructor)
        

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

// --

[<Sealed>]
type SpirvModuleBuilder() =

    let mutable isBuilding = false

    let types = List<SpirvType>()
    let funcs = List<SpirvFunctionBuilder>()

    let pointerTys = Dictionary<(StorageClass * IdRef), SpirvType>()
    let byRefTys = Dictionary<(SpirvByRefKind * IdRef), SpirvType>()

    let constantsInt32 = Dictionary<int32, IdResult>()
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
    let mutable cachedTypeInt32          = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeUInt32         = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeFloat32        = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeVector2Float32 = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeVector3Float32 = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeVector4Float32 = Unchecked.defaultof<SpirvType>
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
            let _ = this.GetTypeVector3Float32() // eval type
            let idResult = this.NewIdResult()
            constantsVectorFloat32[key] <- idResult
            idResult

    member this.GetConstantVector4Float32(x: float32, y: float32, z: float32, w: float32) =
        let xIdRef : IdRef = this.GetConstantFloat32(x)
        let yIdRef : IdRef = this.GetConstantFloat32(y)
        let zIdRef : IdRef = this.GetConstantFloat32(z)
        let wIdRef : IdRef = this.GetConstantFloat32(w)

        let key = [xIdRef;yIdRef;zIdRef;wIdRef]
        match constantsVectorFloat32.TryGetValue key with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeVector4Float32() // eval type
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

    member this.GetTypeByRef(kind: SpirvByRefKind, elementTy: SpirvType) =
        let key = (kind, elementTy.IdResult)
        match byRefTys.TryGetValue key with
        | true, ty -> ty
        | _ ->
            let idResult = this.NewIdResult()
            let ty = SpirvType.ByRef(idResult, kind, elementTy)
            this.AddType(ty)
            byRefTys[key] <- ty
            ty

    member this.GetTypeVoid() =
        if isNull(box cachedTypeVoid) then
            cachedTypeVoid <- SpirvType.Void(this.NewIdResult())
            this.AddType(cachedTypeVoid)
        cachedTypeVoid

    member this.GetTypeInt32() =
        if isNull(box cachedTypeInt32) then
            cachedTypeInt32 <- SpirvType.Int32(this.NewIdResult())
            this.AddType(cachedTypeInt32)
        cachedTypeInt32

    member this.GetTypeUInt32() =
        if isNull(box cachedTypeUInt32) then
            cachedTypeUInt32 <- SpirvType.UInt32(this.NewIdResult())
            this.AddType(cachedTypeUInt32)
        cachedTypeUInt32

    member this.GetTypeFloat32() =
        if isNull(box cachedTypeFloat32) then
            cachedTypeFloat32 <- SpirvType.Float32(this.NewIdResult())
            this.AddType(cachedTypeFloat32)
        cachedTypeFloat32

    member this.GetTypeVector2Float32() =
        if isNull(box cachedTypeVector2Float32) then
            cachedTypeVector2Float32 <- SpirvType.Vector2(this.NewIdResult(), this.GetTypeFloat32())
            this.AddType(cachedTypeVector2Float32)
        cachedTypeVector2Float32

    member this.GetTypeVector3Float32() =
        if isNull(box cachedTypeVector3Float32) then
            cachedTypeVector3Float32 <- SpirvType.Vector3(this.NewIdResult(), this.GetTypeFloat32())
            this.AddType(cachedTypeVector3Float32)
        cachedTypeVector3Float32

    member this.GetTypeVector4Float32() =
        if isNull(box cachedTypeVector4Float32) then
            cachedTypeVector4Float32 <- SpirvType.Vector4(this.NewIdResult(), this.GetTypeFloat32())
            this.AddType(cachedTypeVector4Float32)
        cachedTypeVector4Float32

    member this.GetTypeTuple(elementTys: SpirvType imarray, elementNames: string imarray): SpirvType = 
        let ty = SpirvType.Tuple(this.NewIdResult(), elementTys, elementNames)
        this.AddType(ty)
        ty

    member this.CreateNameTypedBuilder(enclosing: Choice<string imarray, SpirvType>, name: string) =
        let tyBuilder = SpirvNamedTypeBuilder(this.NewIdResult(), enclosing, name, List(), SpirvTypeFlags.None)
        this.AddType(tyBuilder.AsType)
        tyBuilder

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
                    func.Parameters
                    |> Seq.map (fun par ->
                        par.VariableIdRef
                    )
                    |> List.ofSeq

                entryPointInstrs <-
                    [
                        OpEntryPoint(ExecutionModel.Vertex, func.IdResult, func.Name, variableOfPointerOfBlockIdResults)
                    ]
            | _ ->
                failwith "Entry point already set."
        func

    member this.Build() =
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
                        OpConstantComposite(this.GetTypeVector3Float32().IdResult, pair.Value, pair.Key)
                    | 4 ->
                        OpConstantComposite(this.GetTypeVector4Float32().IdResult, pair.Value, pair.Key)
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

#if DEBUG || CHECKED
        Verification.VerifyInstructions instrs
#endif

        isBuilding <- false

        let version = SpirvModule.CreateVersion(1u, 3u)
        SpirvModule.Create(version, instrs = instrs)

module private Verification =

    [<RequireQualifiedAccess>]
    type VerifyInstructionKind =
        | Type
        | Other

    /// This is not comprehensive.
    let VerifyInstructions (instrs: Instruction list) =
        let defs = Dictionary<IdResult, VerifyInstructionKind>()

        let addType idResult =
            defs.Add(idResult, VerifyInstructionKind.Type)

        let addOther idResult =
            defs.Add(idResult, VerifyInstructionKind.Other)

        let assertType (idRef: IdRef) =
            match defs.TryGetValue(idRef) with
            | true, kind ->
                if kind <> VerifyInstructionKind.Type then
                    failwith $"Expected '{VerifyInstructionKind.Type}' by got '{kind}'."
            | _ ->
                failwith $"Unable to find definition for IdRef: {idRef}"

        instrs
        |> List.iter (function
            | Instruction.OpExtInstImport(idResult, _) ->
                addOther idResult

            | Instruction.OpTypeInt(idResult, _, _)
            | Instruction.OpTypeFloat(idResult, _, _)
            | Instruction.OpTypeVector(idResult, _, _)
            | Instruction.OpTypeStruct(idResult, _)
            | Instruction.OpTypePointer(idResult, _, _) ->
                addType idResult

            | _ ->
                ()
        )

        instrs
        |> List.iter (function
            | Instruction.OpTypeStruct(_, idRefs) ->
                idRefs |> List.iter assertType

            | Instruction.OpTypePointer(_, _, idRef)
            | Instruction.OpTypeVector(_, idRef, _) ->
                assertType idRef

            | Instruction.OpConstant(idRef, _, _) ->
                assertType idRef
                
            | _ ->
                ()
        )






        