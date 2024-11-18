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

type SpirvNamedTypeBuilder =
    {
        IdResult: IdResult
        Enclosing: Choice<string imarray, SpirvType>
        Name: string
        Fields: List<SpirvField>
    }

[<RequireQualifiedAccess>]
type SpirvByRefKind =
    | ReadWrite
    | ReadOnly

[<RequireQualifiedAccess>]
type SpirvType =
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
    | ByRef of idResult: IdResult * elementTy: SpirvType * kind: SpirvByRefKind
    | NativeInt of idResult: IdResult
    | NativeUInt of idResult: IdResult
    | NativePointer of idResult: IdResult * elementTy: SpirvType

    | Vector2 of idResult: IdResult * elementTy: SpirvType
    | Vector3 of idResult: IdResult * elementTy: SpirvType
    | Vector4 of idResult: IdResult * elementTy: SpirvType

    | Function of idResult: IdResult * parTys: SpirvType imarray * returnTy: SpirvType

    | Named of SpirvNamedTypeBuilder
    | Module of enclosing: Choice<string imarray, SpirvType> * name: string

    member this.GetDefinitionInstructions() =
        match this with
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

        | ByRef(idResult, _, _) ->          raise(NotImplementedException())
        | NativeInt idResult ->             raise(NotImplementedException())
        | NativeUInt idResult ->            raise(NotImplementedException())
        | NativePointer(idResult, _) ->     raise(NotImplementedException())

        | Function(idResult, parTys, returnTy) ->
            [
                OpTypeFunction(idResult, returnTy.IdResult, parTys |> Seq.map (fun x -> x.IdResult) |> List.ofSeq)
            ]

        | Named(namedTy) -> 
            [
                OpTypeStruct(namedTy.IdResult, namedTy.Fields |> Seq.map (fun x -> x.Type.IdResult) |> List.ofSeq)
            ]

        | Module _ -> 
            failwith "Invalid type for 'GetDefinitionInstructions'."

    member this.IdResult =
        match this with
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
        | NativePointer(idResult, _)
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

[<Sealed>]
type SpirvFunctionBuilder(builder: SpirvModuleBuilder, idResult: IdResult, enclosingTy: SpirvType, name: string, irFlags: OlyIRFunctionFlags, irPars: OlyIRParameter<SpirvType> imarray, returnTy: SpirvType, voidTy: SpirvType) =

    let funcTy = SpirvType.Function(builder.NewIdResult(), irPars |> ImArray.map (fun x -> x.Type), if irFlags.IsEntryPoint then voidTy else returnTy)

    let parameterTypeIdResults =
        irPars
        |> ImArray.map (fun x -> x.Type.IdResult)

    let parametersIdResults =
        parameterTypeIdResults
        |> Seq.map (fun typeIdResult ->
            builder.NewIdResult(), typeIdResult
        )
        |> List.ofSeq

    let outputParameterIdResults =
        if irFlags.IsEntryPoint then
            match returnTy with
            | SpirvType.Void _ -> []
            | SpirvType.Vector4 _ ->
                let blockIdResult = builder.NewIdResult()
                let typePointerOfBlockIdResult = builder.NewIdResult()
                let variableOfPointerOfBlockIdResult = builder.NewIdResult()

                let memberTys =
                    match returnTy with
                    | SpirvType.Vector4(vec4IdResult, SpirvType.Float32 _) ->
                        [vec4IdResult] // Vertex Shader Only
                    | SpirvType.Tuple(_, itemTys, _) ->
                        itemTys |> Seq.map (fun x -> x.IdResult) |> List.ofSeq
                    | _ ->
                        raise(InvalidOperationException())

                [{| MemberTypes = memberTys; BlockIdResult = blockIdResult; TypePointerOfBlockIdResult = typePointerOfBlockIdResult; VariableOfPointerOfBlockIdResult = variableOfPointerOfBlockIdResult |}]
            | _ ->
                raise(NotImplementedException())
        else
            []

    member _.IdResult = idResult
    member _.EnclosingType = enclosingTy
    member _.Name = name
    member val Instructions : Instruction list = [] with get, set
    member _.Type = funcTy
    member _.ReturnType = returnTy

    member _.ParameterTypeIdResults = parameterTypeIdResults
    member _.ParameterIdResults = parametersIdResults
    member _.OutputParameterIdResults = outputParameterIdResults

    member _.IsEntryPoint = irFlags.IsEntryPoint

[<RequireQualifiedAccess>]
type SpirvFunction =
    | Builder of SpirvFunctionBuilder
    | NewVector4 of SpirvType imarray

type SpirvField =
    {
        Name: string
        Type: SpirvType
        Index: int32
    }

[<Sealed>]
type SpirvModuleBuilder() =

    let types = List<SpirvType>()
    let funcs = List<SpirvFunctionBuilder>()

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

    let mutable deferredTypeInstrs = []

    (* cached types *)
    let mutable cachedTypeInt32          = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeFloat32        = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeVector3Float32 = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeVector4Float32 = Unchecked.defaultof<SpirvType>
    (**)

    let mutable newIdResultValue = 2u // This is '2u' because 'OpExtInstImport' uses '1u'.
    member _.NewIdResult() =
        let value = newIdResultValue
        newIdResultValue <- value + 1u
        value

    member _.TypeInt32 = cachedTypeInt32
    member _.TypeFloat32 = cachedTypeFloat32
    member _.TypeVector3Float32 = cachedTypeVector3Float32
    member _.TypeVector4Float32 = cachedTypeVector4Float32

    member this.CreateConstantInt32(value: int32) =
        match constantsInt32.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let idResult = this.NewIdResult()
            constantsInt32[value] <- idResult
            idResult

    member this.CreateConstantFloat32(value: float32) =
        match constantsFloat32.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let idResult = this.NewIdResult()
            constantsFloat32[value] <- idResult
            idResult

    member this.CreateConstantVector3Float32(x: float32, y: float32, z: float32) =
        let xIdRef : IdRef = this.CreateConstantFloat32(x)
        let yIdRef : IdRef = this.CreateConstantFloat32(y)
        let zIdRef : IdRef = this.CreateConstantFloat32(z)

        let key = [xIdRef;yIdRef;zIdRef]
        match constantsVectorFloat32.TryGetValue key with
        | true, idResult -> idResult
        | _ ->
            let idResult = this.NewIdResult()
            constantsVectorFloat32[key] <- idResult
            idResult

    member this.CreateConstantVector4Float32(x: float32, y: float32, z: float32, w: float32) =
        let xIdRef : IdRef = this.CreateConstantFloat32(x)
        let yIdRef : IdRef = this.CreateConstantFloat32(y)
        let zIdRef : IdRef = this.CreateConstantFloat32(z)
        let wIdRef : IdRef = this.CreateConstantFloat32(w)

        let key = [xIdRef;yIdRef;zIdRef;wIdRef]
        match constantsVectorFloat32.TryGetValue key with
        | true, idResult -> idResult
        | _ ->
            let idResult = this.NewIdResult()
            constantsVectorFloat32[key] <- idResult
            idResult

    member _.AddType(ty) =
        types.Add(ty)
        match ty with
        | SpirvType.Int32 _ ->
            cachedTypeInt32 <- ty
        | SpirvType.Float32 _ ->
            cachedTypeFloat32 <- ty
        | SpirvType.Vector3(_, SpirvType.Float32 _) ->
            cachedTypeVector3Float32 <- ty
        | SpirvType.Vector4(_, SpirvType.Float32 _) ->
            cachedTypeVector4Float32 <- ty
        | _ ->
            ()

    member this.AddFunctionBuilder(func) =
        funcs.Add(func)

        this.AddType(func.Type) // TODO: Cache this!

        if func.IsEntryPoint then
            match entryPointInstrs with
            | [] ->
                let variableOfPointerOfBlockIdResults =
                    func.OutputParameterIdResults
                    |> List.map (fun x -> x.VariableOfPointerOfBlockIdResult)

                entryPointInstrs <-
                    [
                        OpEntryPoint(ExecutionModel.Vertex, func.IdResult, func.Name, variableOfPointerOfBlockIdResults @ (func.ParameterIdResults |> List.map fst))

                        let mutable i = 0
                        for x in func.OutputParameterIdResults do
                            if i > 0 then raise(NotImplementedException())
                            let blockIdResult = x.BlockIdResult
                            OpMemberDecorate(blockIdResult, 0u, Decoration.BuiltIn(BuiltIn.Position))
                            OpMemberDecorate(blockIdResult, 1u, Decoration.BuiltIn(BuiltIn.PointSize))
                            OpMemberDecorate(blockIdResult, 2u, Decoration.BuiltIn(BuiltIn.ClipDistance))
                            OpMemberDecorate(blockIdResult, 3u, Decoration.BuiltIn(BuiltIn.CullDistance))
                            OpDecorate(blockIdResult, Decoration.Block)
                        yield! (
                            func.ParameterIdResults
                            |> List.mapi (fun i (varIdResult, _) -> OpDecorate(varIdResult, Decoration.Location(uint32(i))))
                        )
                    ]

                let outPositionDefInstrs =
                    func.OutputParameterIdResults
                    |> List.collect (fun x ->
                        [
                            OpTypeStruct(x.BlockIdResult, x.MemberTypes)
                            OpTypePointer(x.TypePointerOfBlockIdResult, StorageClass.Output, x.BlockIdResult)
                            OpVariable(x.TypePointerOfBlockIdResult, x.VariableOfPointerOfBlockIdResult, StorageClass.Output, None)
                        ]
                    )

                deferredTypeInstrs <-
                    func.ParameterIdResults
                    |> List.collect (fun (varIdResult, typeIdResult) ->
                        let pointerTypeIdResult = this.NewIdResult()
                        [
                            OpTypePointer(pointerTypeIdResult, StorageClass.Input, typeIdResult)
                            OpVariable(pointerTypeIdResult, varIdResult, StorageClass.Input, None)
                        ]
                    )

                deferredTypeInstrs <-
                    deferredTypeInstrs @ outPositionDefInstrs
            | _ ->
                failwith "Entry point already set."

    member this.Build() =
        let instrs =
            vertexHeaderInstrs @
            entryPointInstrs @
            (
                types
                |> Seq.collect (fun x -> x.GetDefinitionInstructions())
                |> List.ofSeq
            ) @
            deferredTypeInstrs @
            ( 
                constantsInt32 
                |> Seq.map (fun pair -> OpConstant(this.TypeInt32.IdResult, pair.Value, uint32(pair.Key))) 
                |> List.ofSeq
            ) @
            (
                constantsFloat32 
                |> Seq.map (fun pair -> OpConstant(this.TypeFloat32.IdResult, pair.Value, uint32(pair.Key))) 
                |> List.ofSeq
            ) @
            (
                constantsVectorFloat32
                |> Seq.map (fun pair -> 
                    let n = pair.Key.Length
                    match n with
                    | 3 ->
                        OpConstantComposite(this.TypeVector3Float32.IdResult, pair.Value, pair.Key)
                    | 4 ->
                        OpConstantComposite(cachedTypeVector4Float32.IdResult, pair.Value, pair.Key)
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
                
            | _ ->
                ()
        )






        