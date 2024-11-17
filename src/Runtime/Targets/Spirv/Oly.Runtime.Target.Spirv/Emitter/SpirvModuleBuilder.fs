namespace rec Oly.Runtime.Target.Spirv.Emitter

open System
open System.Text
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

    let parameterIdResults =
        irPars
        |> ImArray.map (fun _ -> builder.NewIdResult())

    member _.IdResult = idResult
    member _.EnclosingType = enclosingTy
    member _.Name = name
    member val Instructions : Instruction list = [] with get, set
    member _.Type = funcTy
    member _.ReturnType = returnTy

    member _.ParameterTypeIdResults = parameterIdResults

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

    let vertexHeaderInstrs =
        [
            OpCapability(Capability.Shader)
            OpExtInstImport(1u, "GLSL.std.450")
            OpMemoryModel(AddressingModel.Logical, MemoryModel.GLSL450)
        ]

    let mutable entryPointInstrs = []

    let mutable deferredTypeInstrs = []

    let mutable newIdResultValue = 2u // This is '2u' because 'OpExtInstImport' uses '1u'.
    member _.NewIdResult() =
        let value = newIdResultValue
        newIdResultValue <- value + 1u
        value

    member _.AddType(ty) =
        types.Add(ty)

    member this.AddFunctionBuilder(func) =
        funcs.Add(func)

        this.AddType(func.Type) // TODO: Cache this!

        if func.IsEntryPoint then
            match entryPointInstrs with
            | [] ->
                let blockIdResult = this.NewIdResult()

                let parInstrs =
                    func.ParameterTypeIdResults
                    |> Seq.mapi (fun i x ->
                        OpDecorate(x, Decoration.Location(uint32 i))
                    )
                    |> List.ofSeq

                let pointerOfBlockIdResult = this.NewIdResult()
                let variableOfPointerOfBlockIdResult = this.NewIdResult()

                entryPointInstrs <-
                    [
                        OpEntryPoint(ExecutionModel.Vertex, func.IdResult, func.Name, [variableOfPointerOfBlockIdResult] @ (func.ParameterTypeIdResults |> Seq.toList))
                        OpMemberDecorate(blockIdResult, 0u, Decoration.BuiltIn(BuiltIn.Position))
                        OpMemberDecorate(blockIdResult, 1u, Decoration.BuiltIn(BuiltIn.PointSize))
                        OpMemberDecorate(blockIdResult, 2u, Decoration.BuiltIn(BuiltIn.ClipDistance))
                        OpMemberDecorate(blockIdResult, 3u, Decoration.BuiltIn(BuiltIn.CullDistance))
                        OpDecorate(blockIdResult, Decoration.Block)
                        yield! parInstrs
                    ]

                let outPositionDefInstrs =
                    match func.ReturnType with
                    | SpirvType.Void _ -> []
                    | _ ->
                        let memberTys =
                            match func.ReturnType with
                            | SpirvType.Vector4(vec4IdResult, SpirvType.Float32 _) ->
                                [vec4IdResult] // Vertex Shader Only
                            | SpirvType.Tuple(_, itemTys, _) ->
                                itemTys |> Seq.map (fun x -> x.IdResult) |> List.ofSeq
                            | _ ->
                                raise(InvalidOperationException())
                        [
                            OpTypeStruct(blockIdResult, memberTys)
                            OpTypePointer(pointerOfBlockIdResult, StorageClass.Output, blockIdResult)
                            OpVariable(pointerOfBlockIdResult, variableOfPointerOfBlockIdResult, StorageClass.Output, None)
                        ]

                deferredTypeInstrs <-
                    func.ParameterTypeIdResults
                    |> Seq.collect (fun x ->
                        let pointerTypeIdResult = this.NewIdResult()
                        let variableIdResult = this.NewIdResult()
                        [
                            OpTypePointer(pointerTypeIdResult, StorageClass.Input, x)
                            OpVariable(pointerTypeIdResult, variableIdResult, StorageClass.Input, None)
                        ]
                    )
                    |> List.ofSeq

                deferredTypeInstrs <-
                    deferredTypeInstrs @ outPositionDefInstrs
            | _ ->
                failwith "Entry point already set."

    member _.Build() =
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
                funcs
                |> Seq.collect (fun x -> x.Instructions)
                |> List.ofSeq
            )

        let version = SpirvModule.CreateVersion(1u, 3u)
        SpirvModule.Create(version, instrs = instrs)