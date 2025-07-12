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
    | StorageBuffer = 0b000000010

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

    member this.IsRuntimeArray =
        this.Fields.Count = 1 && (match (this.Fields |> Seq.head).Type with SpirvType.RuntimeArray _ -> true | _ -> false)

    member this.GetRuntimeArrayField() =
        if not this.IsRuntimeArray then
            raise(InvalidOperationException("Expected a runtime array type."))
        this.Fields
        |> Seq.head

/// TODO: Do we actually need this? 
///       We only need it if we need to determine a spirv semantic based on if the Oly array was immutable or mutable.
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
    | Pointer of idResult: IdResult * storageClass: StorageClass * elementTy: SpirvType

    | Vec of idresult: IdResult * n: uint * elementTy: SpirvType

    | Function of idResult: IdResult * parTys: SpirvType imarray * returnTy: SpirvType

    | RuntimeArray of idResult: IdResult * kind: SpirvArrayKind * elementTy: SpirvType

    | Struct of SpirvTypeStructBuilder
    | Module of enclosing: Choice<string imarray, SpirvType> * name: string

    /// ByRefs do not exist in spirv, but it is useful to track it.
    /// Note: We could get rid of this type if we can figure out a better way to handle emitting a byref and 
    //        have the entry-point parameter semantics properly determined without introducing duplicate types to the spirv byte-code.
    | OlyByRef of kind: OlyIRByRefKind * elementTy: SpirvType

    | OlyConstantInt32 of value: int32

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
        | OlyByRef _ ->
            raise(InvalidOperationException("ByRef must be lowered."))
        | OlyConstantInt32 _ ->
            raise(InvalidOperationException("ConstantInt32 must be lowered."))
        | Pointer _ ->
            raise(NotImplementedException())
        | Vec(_, n, elemTy) ->
            n * elemTy.GetSizeInBytes()
        | Function _ ->
            raise(NotSupportedException())
        | RuntimeArray _ ->
            0u // size is unknown, so return 0
        | Struct(structBuilder) ->
            structBuilder.Fields
            |> Seq.sumBy (fun x -> x.Type.GetSizeInBytes())
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
        | Vec(idResult, n, elementTy) ->    [OpTypeVector(idResult, elementTy.IdResult, n)]

        | Tuple(idResult, itemTys, _) ->
            [
                OpTypeStruct(idResult, itemTys |> Seq.map (fun x -> x.IdResult) |> List.ofSeq)
            ]

        | OlyByRef _ 
        | OlyConstantInt32 _ ->
            raise(InvalidOperationException("Type must be lowered."))

        | Pointer(idResult, storageClass, elementTy) ->
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

        | Struct(structBuilder) -> 
            [
                for field in structBuilder.Fields do
                    for attr in field.Attributes do
                        match attr.Data with
                        | SpirvBuiltInFunctionData.DecorateField(_, create)
                        | SpirvBuiltInFunctionData.DecorateFieldAndVariable(_, create, _, _) ->
                            yield! create structBuilder.IdResult (uint32(field.Index))
                        | _ ->
                            raise(InvalidOperationException())

                let mutable currentIndex = 0u
                let mutable currentOffset = 0u
                for field in structBuilder.Fields do
                    OpMemberDecorate(structBuilder.IdResult, currentIndex, Decoration.Offset currentOffset)
                    currentIndex <- currentIndex + 1u
                    currentOffset <- currentOffset + uint(field.Type.GetSizeInBytes())

                for attr in structBuilder.Attributes do
                    match attr.Data with
                    | SpirvBuiltInFunctionData.DecorateType(_, create) ->
                        yield! create structBuilder.IdResult
                    | _ ->
                        raise(InvalidOperationException())

                OpTypeStruct(structBuilder.IdResult, structBuilder.Fields |> Seq.map (fun x -> x.Type.IdResult) |> List.ofSeq)
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
        | Pointer(idResult, _, _)
        | Vec(idResult, _, _)
        | Function(idResult, _, _)
        | RuntimeArray(idResult, _, _) -> idResult
        | Struct(namedTy) -> namedTy.IdResult
        | Module _ -> failwith "Invalid type for 'IdResult'."
        | OlyByRef _ 
        | OlyConstantInt32 _ -> 
            raise(InvalidOperationException("Type must be lowered."))

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
        | Struct(structBuilder) -> structBuilder
        | _ -> failwith "Expected named type."

    member this.IsStructRuntimeArray =
        match this with
        | Struct(structBuilder) -> structBuilder.IsRuntimeArray
        | _ -> false

    member this.IsPointerOfStructRuntimeArray =
        match this with
        | Pointer(elementTy=elementTy) -> elementTy.IsStructRuntimeArray
        | _ -> false

    member this.IsPointerOfPointerOfStructRuntimeArray =
        match this with
        | Pointer(elementTy=elementTy) -> elementTy.IsPointerOfStructRuntimeArray
        | _ -> false

    member this.ElementType =
        match this with
        | Pointer(elementTy=elementTy)
        | OlyByRef(elementTy=elementTy) -> elementTy
        | _ -> raise(InvalidOperationException("Type does not have an element type."))

[<Sealed>]
type SpirvVariable(idResult: IdResult, ty: SpirvType, decorateInstrs: Instruction list) =
    member _.IdResult = idResult
    member _.Type = ty
    member _.DecorateInstructions = decorateInstrs
    member _.StorageClass =
        match ty with
        | SpirvType.Pointer(storageClass=storageClass) -> storageClass
        | _ -> raise(InvalidOperationException("Expected a pointer type."))

[<Sealed>]
type SpirvFunctionBuilder(
    builder: SpirvModuleBuilder, 
    idResult: IdResult, 
    enclosingTy: SpirvType, 
    name: string, 
    irFlags: OlyIRFunctionFlags, 
    irPars: OlyIRParameter<SpirvType, SpirvFunction> imarray, 
    returnTy: SpirvType) as this =

    do
        match returnTy with
        | SpirvType.Pointer _ -> invalidOp "Return type cannot be a pointer in Logical addressing model."
        | _ -> ()

    let func = SpirvFunction.Function(this)

    let pars = 
        if irFlags.IsEntryPoint then
            if not irPars.IsEmpty then
                raise(InvalidOperationException("Entry point must have no parameters."))
            ImArray.empty
        else   
            let pars =
                irPars
                |> ImArray.map (fun irPar ->
                    builder.CreateVariable(false, StorageClass.Function, irPar.Attributes, irPar.Type)
                )
            if irFlags.IsStatic then
                pars
            else
                pars
                |> ImArray.prependOne (builder.CreateVariable(false, StorageClass.Function, ImArray.empty, enclosingTy))

    let parTys =
        pars
        |> ImArray.map _.Type

    do
        if irFlags.IsEntryPoint && returnTy <> builder.GetTypeVoid() then
            raise(InvalidOperationException("Entry point must have a 'void' return type."))

    let funcTy = builder.GetTypeFunction(parTys, returnTy)

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
    | BuiltIn of SpirvBuiltInFunction
    | ExtendedInstruction of set: string * opCode: uint32
    | Variable of SpirvVariable
    | LazyVariable of Lazy<SpirvVariable>
    | AccessChain
    | PtrAccessChain

    member this.AsBuilder =
        match this with
        | Function(builder) -> builder
        | _ -> raise(InvalidOperationException("Expected a function builder."))

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
        Attributes: SpirvBuiltInFunction imarray
    }

// --

type SpirvBuiltInFunctionFlags =
    | None                     = 0b0000000000
    | AutoDereferenceArguments = 0b0000000001

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type SpirvBuiltInFunctionData =
    | DecorateField of SpirvFieldFlags * (IdRef -> uint32 -> Instruction list)
    | DecorateType of SpirvTypeFlags * (IdRef -> Instruction list)
    | DecorateVariable of SpirvVariableFlags * (IdRef -> C imarray -> Instruction list)
    | DecorateFieldAndVariable of fieldFlags: SpirvFieldFlags * createFieldInstrs: (IdRef -> uint32 -> Instruction list) * varFlags: SpirvVariableFlags * createVarInstrs: (IdRef -> C imarray -> Instruction list)
    | Intrinsic of (SpirvModuleBuilder -> (SpirvType * IdRef) imarray -> SpirvType -> IdRef * Instruction list)

[<NoEquality;NoComparison>]
type SpirvBuiltInFunction = 
    { 
        Name: string;
        Flags: SpirvBuiltInFunctionFlags
        Data: SpirvBuiltInFunctionData
    }

module BuiltInFunctions =

    let private Lookup = new Dictionary<string, SpirvFunction>()

    let private Add(
            name: string,
            data: SpirvBuiltInFunctionData,
            flags: SpirvBuiltInFunctionFlags) =

        Lookup.Add(
            name,
            {
                Name = name
                Data = data
                Flags = flags
            } |> SpirvFunction.BuiltIn
        )

    do
        Add("position",                
            SpirvBuiltInFunctionData.DecorateFieldAndVariable(
                SpirvFieldFlags.None, 
                (fun tyIdRef index ->
                    [OpMemberDecorate(tyIdRef, index, Decoration.BuiltIn BuiltIn.Position)]
                ), 
                SpirvVariableFlags.None, 
                fun varIdRef args ->
                    [OpDecorate(varIdRef, Decoration.BuiltIn BuiltIn.Position)]
            ),
            SpirvBuiltInFunctionFlags.None)
        Add("point_size",            
            SpirvBuiltInFunctionData.DecorateFieldAndVariable(
                SpirvFieldFlags.None, 
                (fun tyIdRef index ->
                    [OpMemberDecorate(tyIdRef, index, Decoration.BuiltIn BuiltIn.PointSize)]
                ), 
                SpirvVariableFlags.None, 
                fun varIdRef _ ->
                    [OpDecorate(varIdRef, Decoration.BuiltIn BuiltIn.PointSize)]
            ),        
            SpirvBuiltInFunctionFlags.None)
        Add("block",                     
            SpirvBuiltInFunctionData.DecorateType(
                SpirvTypeFlags.None, 
                fun tyIdRef ->
                    [OpDecorate(tyIdRef, Decoration.Block)]
            ),      
            SpirvBuiltInFunctionFlags.None)
        Add("buffer_block",                     
            SpirvBuiltInFunctionData.DecorateType(
                SpirvTypeFlags.None, 
                fun tyIdRef ->
                    [OpDecorate(tyIdRef, Decoration.BufferBlock)]
            ),      
            SpirvBuiltInFunctionFlags.None)
        Add("location",             
            SpirvBuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.None, 
                fun varIdRef args ->
                    match args |> ImArray.head with
                    | C.UInt32(value) ->
                        [OpDecorate(varIdRef, Decoration.Location value)]
                    | _ ->
                        raise(InvalidOperationException()) 
            ),     
            SpirvBuiltInFunctionFlags.None)
        Add("uniform",             
            SpirvBuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.Uniform, 
                fun _varIdRef _args ->
                    []
            ),     
            SpirvBuiltInFunctionFlags.None)
        Add("storage_buffer",             
            SpirvBuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.StorageBuffer, 
                fun _varIdRef _args ->
                    []
            ),     
            SpirvBuiltInFunctionFlags.None) 
        Add("descriptor_set",             
            SpirvBuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.None, 
                fun varIdRef args ->
                    match args |> ImArray.head with
                    | C.UInt32(value) ->
                        [OpDecorate(varIdRef, Decoration.DescriptorSet value)]
                    | _ ->
                        raise(InvalidOperationException()) 
            ),     
            SpirvBuiltInFunctionFlags.None)
        Add("binding",             
            SpirvBuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.None, 
                fun varIdRef args ->
                    match args |> ImArray.head with
                    | C.UInt32(value) ->
                        [OpDecorate(varIdRef, Decoration.Binding value)]
                    | _ ->
                        raise(InvalidOperationException()) 
            ),     
            SpirvBuiltInFunctionFlags.None) 
        Add("global_invocation_id",             
            SpirvBuiltInFunctionData.DecorateVariable(
                SpirvVariableFlags.None, 
                fun varIdRef _ ->
                    [OpDecorate(varIdRef, Decoration.BuiltIn BuiltIn.GlobalInvocationId)]
            ),     
            SpirvBuiltInFunctionFlags.None)
        Add("vec",
            SpirvBuiltInFunctionData.Intrinsic(
                fun spvModule args returnTy ->
                    let idResult = spvModule.NewIdResult()
                    match args.Length with
                    | 1 ->
                        let argIdRef = args |> ImArray.head |> snd 
                        match returnTy with
                        | SpirvType.Vec(_, n, _) ->
                            let n = int n
                            idResult, [OpCompositeConstruct(returnTy.IdResult, idResult, List.init n (fun _ -> argIdRef))]
                        | _ ->
                            invalidOp "Bad return type."

                    | 2 ->
                        let idRefs = args |> ImArray.map snd
                        match returnTy with
                        | SpirvType.Vec(_, 2u, _) ->
                            idResult, [OpCompositeConstruct(returnTy.IdResult, idResult, [idRefs[0]; idRefs[1]])]
                        | SpirvType.Vec(_, n, elementTy) ->
                            let argIdResults = List.init ((int n) - 1) (fun _ -> spvModule.NewIdResult())
                            let arg1IdRef = idRefs[1]
                            idResult,
                            [
                                let mutable i = 0u
                                for argIdResult in argIdResults do
                                    OpCompositeExtract(elementTy.IdResult, argIdResult, idRefs |> ImArray.head, [i])
                                    i <- i + 1u
                                OpCompositeConstruct(returnTy.IdResult, idResult, argIdResults @ [arg1IdRef])
                            ]
                        | _ ->
                            invalidOp "Bad return type."

                    | 3 ->
                        let idRefs = args |> ImArray.map snd
                        match returnTy with
                        | SpirvType.Vec(_, 3u, _) ->
                            idResult, [OpCompositeConstruct(returnTy.IdResult, idResult, [idRefs[0]; idRefs[1]; idRefs[2]])]
                        | SpirvType.Vec(_, (4u as n), elementTy) ->
                            let argIdResults = List.init ((int n) - 2) (fun _ -> spvModule.NewIdResult())
                            let arg1IdRef = idRefs[1]
                            let arg2IdRef = idRefs[2]
                            idResult,
                            [
                                let mutable i = 0u
                                for argIdResult in argIdResults do
                                    OpCompositeExtract(elementTy.IdResult, argIdResult, idRefs |> ImArray.head, [i])
                                    i <- i + 1u
                                OpCompositeConstruct(returnTy.IdResult, idResult, argIdResults @ [arg1IdRef; arg2IdRef])
                            ]
                        | _ ->
                            invalidOp "Bad return type."

                    | 4 ->
                        let idRefs = args |> ImArray.map snd
                        match returnTy with
                        | SpirvType.Vec(_, 4u, _) ->
                            idResult, [OpCompositeConstruct(returnTy.IdResult, idResult, [idRefs[0]; idRefs[1]; idRefs[2]; idRefs[3]])]
                        | _ ->
                            invalidOp "Bad return type."

                    | _ ->
                        invalidOp "Bad arguments."
                        
            ),
            SpirvBuiltInFunctionFlags.AutoDereferenceArguments
        )
        Add("bitcast",
            SpirvBuiltInFunctionData.Intrinsic(
                fun spvModule args returnTy ->
                    match args.Length with
                    | 1 ->
                        let idRefs =
                            args
                            |> ImArray.map snd
                        let idResult = spvModule.NewIdResult()
                        idResult,
                        [OpBitcast(returnTy.IdResult, idResult, idRefs |> ImArray.head)]
                    | _ ->
                        raise(InvalidOperationException())
            ),
            SpirvBuiltInFunctionFlags.None
        )
        Add("add",
            SpirvBuiltInFunctionData.Intrinsic(
                fun spvModule args returnTy ->
                    match args.Length with
                    | 2 ->
                        let idRefs =
                            args
                            |> ImArray.map snd
                        let idResult = spvModule.NewIdResult()

                        match returnTy with
                        | SpirvType.Float32 _
                        | SpirvType.Float64 _
                        | SpirvType.Vec(elementTy=SpirvType.Float32 _)
                        | SpirvType.Vec(elementTy=SpirvType.Float64 _) ->
                            idResult,
                            [OpFAdd(returnTy.IdResult, idResult, idRefs[0], idRefs[1])]
                        | _ ->
                            invalidOp "Bad return type."
                    | _ ->
                        invalidOp "Bad arguments."
            ),
            SpirvBuiltInFunctionFlags.AutoDereferenceArguments
        )

    let TryGetBuiltInFunction(path: string imarray, name: string) : SpirvFunction option =
        // Extended Instruction Sets
        if path.Length = 1 && path[0] = "GLSL.std.450" then
            SpirvFunction.ExtendedInstruction(path[0], UInt32.Parse(name))
            |> Some
        else

        match ImArray.head path with
        | "std" ->      
            match Lookup.TryGetValue(name) with
            | true, func ->
                match func.TryGetBuiltIn() with
                | ValueSome _ ->
                    Some func
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None

    let IsValid(name: string) = Lookup.ContainsKey(name)

module BuiltInOperations =

    let inline (|AccessChain|_|)(op: O) =
        match op with
        | O.Call(irFunc, argExprs, resultTy) ->
            match irFunc.EmittedFunction with
            | SpirvFunction.AccessChain ->
                OlyAssert.True(argExprs.Length >= 2)
                let baseExpr = 
                    argExprs
                    |> ImArray.head
                let indexExprs = 
                    argExprs 
                    |> ROMem.ofImArray 
                    |> ROMem.skip 1
                Some(baseExpr, indexExprs, resultTy)
            | _ ->
                None
        | _ ->
            None

    let inline (|PtrAccessChain|_|)(op: O) =
        match op with
        | O.Call(irFunc, argExprs, resultTy) ->
            match irFunc.EmittedFunction with
            | SpirvFunction.PtrAccessChain ->
                OlyAssert.True(argExprs.Length >= 3)
                let baseExpr = 
                    argExprs
                    |> ImArray.head
                let elementExpr = 
                    argExprs[1]
                let indexExprs = 
                    argExprs 
                    |> ROMem.ofImArray 
                    |> ROMem.skip 2
                Some(baseExpr, elementExpr, indexExprs, resultTy)
            | _ ->
                None
        | _ ->
            None

module BuiltInExpressions =

    let IndexConstantFromField (builder: SpirvModuleBuilder) (field: SpirvField) =
        E.Value(OlyIRDebugSourceTextRange.Empty, V.Constant(C.UInt32(uint(field.Index)), builder.GetTypeUInt32()))

    let AccessChain(baseExpr: E, indexExprs: E imarray, resultTy: SpirvType) =
        match resultTy with
        | SpirvType.Pointer _ -> ()
        | _ -> raise(InvalidOperationException("Expected a pointer type."))

#if DEBUG || CHECKED
        match baseExpr.ResultType with
        | SpirvType.Pointer _ -> ()
        | _ -> raise(InvalidOperationException("Expected a pointer type."))   

        indexExprs
        |> ImArray.iter (fun indexExpr ->
            match indexExpr.ResultType with
            | SpirvType.UInt32 _ -> ()
            | _ -> raise(InvalidOperationException("Expected an 'uint32' type for the index."))
        )
#endif

        E.Operation(OlyIRDebugSourceTextRange.Empty,
            O.Call(
                OlyIRFunction(SpirvFunction.AccessChain), 
                ImArray.prependOne baseExpr indexExprs, 
                resultTy
            )
        )

    let PtrAccessChain(baseExpr: E, elementExpr: E, indexExprs: E imarray, resultTy: SpirvType) =
        match resultTy with
        | SpirvType.Pointer _ -> ()
        | _ -> raise(InvalidOperationException("Expected a pointer type."))

#if DEBUG || CHECKED
        match baseExpr.ResultType with
        | SpirvType.Pointer _ -> ()
        | _ -> raise(InvalidOperationException("Expected a pointer type."))

        match elementExpr.ResultType with
        | SpirvType.Int32 _ -> ()
        | _ -> raise(InvalidOperationException("Expected an int32 type."))

        indexExprs
        |> ImArray.iter (fun indexExpr ->
            match indexExpr.ResultType with
            | SpirvType.UInt32 _ -> ()
            | _ -> raise(InvalidOperationException("Expected an 'uint32' type for the index."))
        )
#endif

        E.Operation(OlyIRDebugSourceTextRange.Empty,
            O.Call(
                OlyIRFunction(SpirvFunction.PtrAccessChain), 
                ImArray.prependTwo baseExpr elementExpr indexExprs, 
                resultTy
            )
        )

    let Bitcast(argExpr: E, castToType: SpirvType) =
        match BuiltInFunctions.TryGetBuiltInFunction(ImArray.createOne "bitcast", "") with
        | Some func -> 
            E.Operation(OlyIRDebugSourceTextRange.Empty, O.Call(OlyIRFunction(func), ImArray.createOne argExpr, castToType))
        | _ ->
            raise(InvalidOperationException())

module BuiltInTypes =

    let TryGetByName(spvModule: SpirvModuleBuilder, name: string) =
        match name with
        | "vec2" -> spvModule.GetTypeVec(2u, spvModule.GetTypeFloat32()) |> ValueSome
        | "vec3" -> spvModule.GetTypeVec(3u, spvModule.GetTypeFloat32()) |> ValueSome
        | "vec4" -> spvModule.GetTypeVec(4u, spvModule.GetTypeFloat32()) |> ValueSome
        | "dvec2" -> spvModule.GetTypeVec(2u, spvModule.GetTypeFloat64()) |> ValueSome
        | "dvec3" -> spvModule.GetTypeVec(3u, spvModule.GetTypeFloat64()) |> ValueSome
        | "dvec4" -> spvModule.GetTypeVec(4u, spvModule.GetTypeFloat64()) |> ValueSome
        | "ivec2" -> spvModule.GetTypeVec(2u, spvModule.GetTypeInt32()) |> ValueSome
        | "ivec3" -> spvModule.GetTypeVec(3u, spvModule.GetTypeInt32()) |> ValueSome
        | "ivec4" -> spvModule.GetTypeVec(4u, spvModule.GetTypeInt32()) |> ValueSome
        | "uvec2" -> spvModule.GetTypeVec(2u, spvModule.GetTypeUInt32()) |> ValueSome
        | "uvec3" -> spvModule.GetTypeVec(3u, spvModule.GetTypeUInt32()) |> ValueSome
        | "uvec4" -> spvModule.GetTypeVec(4u, spvModule.GetTypeUInt32()) |> ValueSome
        | "bvec2" -> spvModule.GetTypeVec(2u, spvModule.GetTypeBool()) |> ValueSome
        | "bvec3" -> spvModule.GetTypeVec(3u, spvModule.GetTypeBool()) |> ValueSome
        | "bvec4" -> spvModule.GetTypeVec(4u, spvModule.GetTypeBool()) |> ValueSome
        | _ -> ValueNone

// --

[<Sealed>]
type SpirvModuleBuilder(majorVersion: uint, minorVersion: uint, executionModel: ExecutionModel) =

    let isTyEq (ty1: SpirvType) (ty2: SpirvType) =
        match ty1, ty2 with
        | SpirvType.Struct(builder1), SpirvType.Struct(builder2) -> builder1.IdResult = builder2.IdResult
        | _ -> ty1 = ty2

    let functionTypeComparer =
        { new IEqualityComparer<(SpirvType * SpirvType imarray)> with

            member _.GetHashCode (obj: SpirvType * SpirvType imarray): int = 
                let (returnTy, _) = obj
                int(returnTy.IdResult)

            member _.Equals(obj1: SpirvType * SpirvType imarray, obj2: SpirvType * SpirvType imarray): bool =
                let (returnTy1, parTys1) = obj1
                let (returnTy2, parTys2) = obj2

                isTyEq returnTy1 returnTy2 &&
                (
                    if parTys1.Length = parTys2.Length then
                        (parTys1, parTys2)
                        ||> ImArray.forall2 (isTyEq)
                    else
                        false
                )
        }

    let mutable isBuilding = false

    let types = List<SpirvType>()
    let funcs = List<SpirvFunctionBuilder>()
    let globalVars = List<SpirvVariable>()

    let pointerTys = Dictionary<(StorageClass * IdRef), SpirvType>()
    let vecTys = Dictionary<(uint * IdRef), SpirvType>()
    let arrayTys = Dictionary<(SpirvArrayKind * IdRef), SpirvType>()
    let funcTys = Dictionary<(SpirvType * SpirvType imarray), SpirvType>(functionTypeComparer)

    let strings = Dictionary<string, IdResult>()

    let constantsInt32 = Dictionary<int32, IdResult>()
    let constantsUInt32 = Dictionary<uint32, IdResult>()
    let constantsFloat32 = Dictionary<float32, IdResult>()
    let constantsVectorFloat32 = Dictionary<IdRef list, IdResult>()

    let GLSL_std_450 = 1u // TODO: There a way to not make this builtin?

    let headerInstrs =
        [
            OpCapability(Capability.Shader)
            OpExtInstImport(GLSL_std_450, "GLSL.std.450")
            OpMemoryModel(AddressingModel.Logical, MemoryModel.GLSL450)
        ]

    let mutable entryPoint = Unchecked.defaultof<SpirvFunctionBuilder>

    (* cached types *)
    let mutable cachedTypeVoid           = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeBool           = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeInt32          = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeUInt32         = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeFloat32        = Unchecked.defaultof<SpirvType>
    let mutable cachedTypeFloat64        = Unchecked.defaultof<SpirvType>
    (**)

    let mutable newIdResultValue = 2u // This is '2u' because 'OpExtInstImport' uses '1u'.
    member _.NewIdResult() =
        let value = newIdResultValue
        newIdResultValue <- value + 1u
        value

    member this.GetConstantInt32(value: int32) : IdRef =
        match constantsInt32.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeInt32() // eval type
            let idResult = this.NewIdResult()
            constantsInt32[value] <- idResult
            idResult

    member this.GetConstantUInt32(value: uint32) : IdRef =
        match constantsUInt32.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeUInt32() // eval type
            let idResult = this.NewIdResult()
            constantsUInt32[value] <- idResult
            idResult

    member this.GetConstantFloat32(value: float32) : IdRef =
        match constantsFloat32.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeFloat32() // eval type
            let idResult = this.NewIdResult()
            constantsFloat32[value] <- idResult
            idResult

    member this.GetConstantVec3Float32(x: float32, y: float32, z: float32) : IdRef =
        let xIdRef : IdRef = this.GetConstantFloat32(x)
        let yIdRef : IdRef = this.GetConstantFloat32(y)
        let zIdRef : IdRef = this.GetConstantFloat32(z)

        let key = [xIdRef;yIdRef;zIdRef]
        match constantsVectorFloat32.TryGetValue key with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeVec(3u, this.GetTypeFloat32()) // eval type
            let idResult = this.NewIdResult()
            constantsVectorFloat32[key] <- idResult
            idResult

    member this.GetConstantVec4Float32(x: float32, y: float32, z: float32, w: float32) : IdRef =
        let xIdRef : IdRef = this.GetConstantFloat32(x)
        let yIdRef : IdRef = this.GetConstantFloat32(y)
        let zIdRef : IdRef = this.GetConstantFloat32(z)
        let wIdRef : IdRef = this.GetConstantFloat32(w)

        let key = [xIdRef;yIdRef;zIdRef;wIdRef]
        match constantsVectorFloat32.TryGetValue key with
        | true, idResult -> idResult
        | _ ->
            let _ = this.GetTypeVec(4u, this.GetTypeFloat32()) // eval type
            let idResult = this.NewIdResult()
            constantsVectorFloat32[key] <- idResult
            idResult

    member this.GetTypePointer(storageClass: StorageClass, elementTy: SpirvType) =
        match elementTy with
        | SpirvType.OlyByRef _ -> raise(InvalidOperationException("Did not expect a ByRef type."))
        | _ -> ()

        let key = (storageClass, elementTy.IdResult)
        match pointerTys.TryGetValue key with
        | true, ty -> ty
        | _ ->
            let idResult = this.NewIdResult()
            let ty = SpirvType.Pointer(idResult, storageClass, elementTy)
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

    member this.GetTypeVec(n: uint, elementTy: SpirvType) : SpirvType =
        let key = (n, elementTy.IdResult)
        match vecTys.TryGetValue key with
        | true, ty -> ty
        | _ ->
            let idResult = this.NewIdResult()
            let ty = SpirvType.Vec(idResult, n, elementTy)
            this.AddType(ty)
            vecTys[key] <- ty
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

    member this.GetTypeFloat64() : SpirvType =
        if isNull(box cachedTypeFloat64) then
            cachedTypeFloat64 <- SpirvType.Float64(this.NewIdResult())
            this.AddType(cachedTypeFloat64)
        cachedTypeFloat64

    member this.GetTypeFunction(parTys: SpirvType imarray, returnTy: SpirvType) =
        let key = (returnTy, parTys)
        match funcTys.TryGetValue key with
        | true, funcTy -> funcTy
        | _ ->
             let funcTy = SpirvType.Function(this.NewIdResult(), parTys, returnTy)
             funcTys[key] <- funcTy
             this.AddType(funcTy)
             funcTy

    member this.GetTypeTuple(elementTys: SpirvType imarray, elementNames: string imarray): SpirvType = 
        raise(NotImplementedException())

    member this.CreateTypeStructBuilder(enclosing: Choice<string imarray, SpirvType>, name: string) =
        SpirvTypeStructBuilder(this.NewIdResult(), enclosing, name, List(), SpirvTypeFlags.None)

    member this.AddTypeStructBuilder(structBuilder: SpirvTypeStructBuilder) =
        if isBuilding then
            failwith "Unable to add type while the module is building."
        types.Add(structBuilder.AsType)

    member _.AddType(ty) =
        if isBuilding then
            failwith "Unable to add type while the module is building."
        types.Add(ty)

    member this.GetStringIdRef(value: string): IdRef =
        match strings.TryGetValue value with
        | true, idResult -> idResult
        | _ ->
            let idResult = this.NewIdResult()
            strings[value] <- idResult
            idResult

    member private this.CreateVariableType(isGlobal: bool, defaultGlobalStorageClass: StorageClass, irAttrs: OlyIRAttribute<SpirvType, SpirvFunction> imarray, ty: SpirvType) =
        let checkParameterElementType elementTy =
            match elementTy with
            | SpirvType.RuntimeArray _ -> raise(InvalidOperationException("Parameters cannot be runtime array types."))
            | _ -> ()

        let isUniform = 
            irAttrs
            |> ImArray.exists (fun x -> 
                match x with
                | OlyIRAttribute(ctor, _, _) ->
                    match ctor.TryGetBuiltIn() with
                    | ValueSome(builtInFunc) ->
                        match builtInFunc.Data with
                        | SpirvBuiltInFunctionData.DecorateVariable(varFlags, _)
                        | SpirvBuiltInFunctionData.DecorateFieldAndVariable(_, _, varFlags, _) ->
                            varFlags.HasFlag(SpirvVariableFlags.Uniform)
                        | _ ->
                            false
                    | _ ->
                        false
            )

        let isStorageBuffer =
            irAttrs
            |> ImArray.exists (fun x -> 
                match x with
                | OlyIRAttribute(ctor, _, _) ->
                    match ctor.TryGetBuiltIn() with
                    | ValueSome(builtInFunc) ->
                        match builtInFunc.Data with
                        | SpirvBuiltInFunctionData.DecorateVariable(varFlags, _)
                        | SpirvBuiltInFunctionData.DecorateFieldAndVariable(_, _, varFlags, _) ->
                            varFlags.HasFlag(SpirvVariableFlags.StorageBuffer)
                        | _ ->
                            false
                    | _ ->
                        false
            )

        match ty with
        | SpirvType.OlyByRef(_, elementTy) ->
            checkParameterElementType elementTy

            if isUniform then
                this.GetTypePointer(StorageClass.Uniform, elementTy)
            elif isStorageBuffer then
                this.GetTypePointer(StorageClass.StorageBuffer, elementTy)
            else
                if isGlobal then
                    this.GetTypePointer(defaultGlobalStorageClass, elementTy)
                else
                    this.GetTypePointer(StorageClass.Function, elementTy)

        | SpirvType.RuntimeArray(elementTy=elementTy) ->
            checkParameterElementType elementTy
            let structBuilder = SpirvTypeStructBuilder(this.NewIdResult(), Choice2Of2(SpirvType.Invalid), "buffer", List(), SpirvTypeFlags.None)
            structBuilder.Fields.Add(
                { 
                    Name = "runtime_array"
                    Type = ty
                    Index = 0
                    Flags = SpirvFieldFlags.None 
                    Attributes = ImArray.empty
                }
            )
            if isUniform then
                structBuilder.Attributes <-
                    {
                        Name = "buffer_block"
                        Flags = SpirvBuiltInFunctionFlags.None
                        Data =
                            SpirvBuiltInFunctionData.DecorateType(
                                SpirvTypeFlags.None,
                                fun idRef ->
                                    [OpDecorate(idRef, Decoration.BufferBlock)]
                            )
                    }
                    |> ImArray.createOne
            else
                structBuilder.Attributes <-
                    {
                        Name = "block"
                        Flags = SpirvBuiltInFunctionFlags.None
                        Data =
                            SpirvBuiltInFunctionData.DecorateType(
                                SpirvTypeFlags.None,
                                fun idRef ->
                                    [OpDecorate(idRef, Decoration.Block)]
                            )
                    }
                    |> ImArray.createOne
            this.AddTypeStructBuilder(structBuilder)

            if isUniform then
                this.GetTypePointer(StorageClass.Uniform, structBuilder.AsType)
            elif isStorageBuffer then
                this.GetTypePointer(StorageClass.StorageBuffer, structBuilder.AsType)
            else
                raise(InvalidOperationException())

        | SpirvType.Pointer _ ->
            ty

        | ty ->
            if isGlobal then
                this.GetTypePointer(defaultGlobalStorageClass, ty)
            else
                this.GetTypePointer(StorageClass.Function, ty)

    member this.CreateVariable(isGlobal, defaultGlobalStorageClass: StorageClass, irAttrs, ty) : SpirvVariable =
        let ty = this.CreateVariableType(isGlobal, defaultGlobalStorageClass, irAttrs, ty)
        let idResult = this.NewIdResult()

        let decorateInstrs = 
            if isGlobal then
                [
                    for attr in irAttrs do
                        match attr with
                        | OlyIRAttribute(ctor, args, _) ->
                            match ctor.TryGetBuiltIn() with
                            | ValueSome builtInFunc ->
                                match builtInFunc.Data with
                                | SpirvBuiltInFunctionData.DecorateVariable(_, create)
                                | SpirvBuiltInFunctionData.DecorateFieldAndVariable(_, _, _, create) ->
                                    yield! create idResult args
                                | _ ->
                                    raise(InvalidOperationException())
                            | _ ->
                                ()
                ]
            else
                []

        let var = SpirvVariable(idResult, ty, decorateInstrs)
        if isGlobal then
            globalVars.Add(var)
        var

    member this.CreateFunctionBuilder(enclosingTy: SpirvType, name: string, irFlags: OlyIRFunctionFlags, irPars: OlyIRParameter<SpirvType, SpirvFunction> imarray, returnTy: SpirvType) =
        let func = SpirvFunctionBuilder(this, this.NewIdResult(), enclosingTy, name, irFlags, irPars, returnTy)
        funcs.Add(func)
        if func.IsEntryPoint then
            if isNull(box entryPoint) then
                entryPoint <- func
            else
                failwith "Entry point already set."
        func

    member this.Build() =
        if isBuilding then
            raise(InvalidOperationException())

        isBuilding <- true

        let instrs = ImArray.builder()

        instrs.AddRange(headerInstrs)

        let entryPointVarIdRefs =
            globalVars
            |> Seq.choose (fun par ->
                match par.Type with
                | SpirvType.Pointer(_, StorageClass.Input, _) -> (par.IdResult : IdRef) |> Some
                | SpirvType.Pointer(_, StorageClass.Output, _) -> (par.IdResult : IdRef) |> Some
                | _ -> None
            )
            |> List.ofSeq

        instrs.Add(OpEntryPoint(executionModel, entryPoint.IdResult, entryPoint.Name, entryPointVarIdRefs))
        if executionModel = ExecutionModel.Fragment then
            instrs.Add(OpExecutionMode(entryPoint.IdResult, ExecutionMode.OriginUpperLeft))
        elif executionModel = ExecutionModel.GLCompute then
            // TODO: What is this? How do we expose this?
            instrs.Add(OpExecutionMode(entryPoint.IdResult, ExecutionMode.LocalSize(1u, 1u, 1u)))

        for pair in strings do
            instrs.Add(OpString(pair.Value, pair.Key))

        for ty in types do
            instrs.AddRange(ty.GetDefinitionInstructions())

        for globalVar in globalVars do
            instrs.AddRange(globalVar.DecorateInstructions)
            instrs.Add(OpVariable(globalVar.Type.IdResult, globalVar.IdResult, globalVar.StorageClass, None))

        for pair in constantsInt32 do
            instrs.Add(OpConstant(this.GetTypeInt32().IdResult, pair.Value, uint32(pair.Key)))

        for pair in constantsUInt32 do
            instrs.Add(OpConstant(this.GetTypeUInt32().IdResult, pair.Value, uint32(pair.Key)))

        for pair in constantsFloat32 do
            instrs.Add(OpConstant(this.GetTypeFloat32().IdResult, pair.Value,  System.Runtime.CompilerServices.Unsafe.BitCast(pair.Key)))

        for pair in constantsVectorFloat32 do
            let n = pair.Key.Length
            match n with
            | 3 ->
                OpConstantComposite(this.GetTypeVec(3u, this.GetTypeFloat32()).IdResult, pair.Value, pair.Key)
            | 4 ->
                OpConstantComposite(this.GetTypeVec(4u, this.GetTypeFloat32()).IdResult, pair.Value, pair.Key)
            | _ ->
                raise(NotImplementedException($"Vector{n} of float32 constant"))
            |> instrs.Add

        for x in funcs do
            instrs.AddRange(x.Instructions)

        let version = SpirvModule.CreateVersion(majorVersion, minorVersion)
        let normalizedInstrs = Normalization.NormalizeAndCheck version (instrs.ToImmutable())
        let result = SpirvModule.Create(version, instrs = normalizedInstrs)
        isBuilding <- false
        result

module private Normalization =

    let NormalizeAndCheck version (instrs: Instruction imarray): Instruction imarray =
        let struct(majorVersion, minorVersion) = SpirvModule.ExtractVersion(version)

        let checkVersion (o: obj) version =
            let struct(major, minor) = SpirvModule.ExtractVersion(version)
            if (major > majorVersion) || (major = majorVersion && minor > minorVersion) then
                invalidOp $"{o} requires version {major}.{minor} or later but is {majorVersion}{minorVersion}." 

        let capabilities = HashSet()
        let checkCapabilities (o: obj) xs =
            let result =
                xs
                |> List.forall (fun x ->
                    capabilities.Contains(x)
                )
            if not result then
                invalidOp $"{o} requires capabilities {xs |> List.sort} but has {capabilities |> List.ofSeq |> List.sort}."

        let checkInstruction (instr: Instruction) =
            checkVersion instr instr.Version
            checkCapabilities instr instr.Capabilities

        let checkDecoration (decor: Decoration) =
            checkVersion decor decor.Version
            checkCapabilities decor decor.Capabilities

        let checkExecutionMode (mode: ExecutionMode) =
            checkVersion mode mode.Version
            checkCapabilities mode mode.Capabilities

        let headerWithEntryPoint = List()
        let executionModes = List()
        let debugInstrs = List()
        let memberDecorates = List()
        let decorates = List()
        let other = List()

        let mutable hasEntryPoint = false

        instrs
        |> ImArray.iter (fun instr ->
            checkInstruction instr

            match instr with
            | OpCapability capability ->
                if not(capabilities.Add(capability)) then
                    invalidOp $"Capability '{capability}' already added."

                if hasEntryPoint then
                    raise(InvalidOperationException())
                headerWithEntryPoint.Add(instr)

            | OpMemberDecorate(_, _, decor) ->
                checkDecoration decor

                if hasEntryPoint |> not then
                    raise(InvalidOperationException())
                memberDecorates.Add(instr)

            | OpString _ ->
                if hasEntryPoint |> not then
                    raise(InvalidOperationException())
                debugInstrs.Add(instr)

            | OpDecorate(_, decor) ->
                checkDecoration decor

                if hasEntryPoint |> not then
                    raise(InvalidOperationException())
                decorates.Add(instr)

            | OpEntryPoint _ ->
                headerWithEntryPoint.Add(instr)
                hasEntryPoint <- true

            | OpExecutionMode(_, mode) ->
                checkExecutionMode mode

                if hasEntryPoint |> not then
                    raise(InvalidOperationException())
                executionModes.Add(instr)

            | _ ->
                if hasEntryPoint then
                    other.Add(instr)
                else
                    headerWithEntryPoint.Add(instr)
        )

        let builder = 
            ImArray.builderWithSize(
                headerWithEntryPoint.Count + 
                executionModes.Count +
                debugInstrs.Count +
                memberDecorates.Count +
                decorates.Count +
                other.Count
            )

        builder.AddRange(headerWithEntryPoint)
        builder.AddRange(executionModes)
        builder.AddRange(debugInstrs)
        builder.AddRange(memberDecorates)
        builder.AddRange(decorates)
        builder.AddRange(other)
        
        builder.MoveToImmutable()
        