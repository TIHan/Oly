namespace rec Oly.Platform.Clr.Metadata

open System
open System.Reflection
open Oly.Core
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Collections.Immutable
open System.Collections.Generic
open System.Collections.Concurrent
open System.Security.Cryptography
open ClrPatterns

[<RequireQualifiedAccess>]
module private MetadataHelpers =

    let addAssembly assemblyName (metadataBuilder: MetadataBuilder) =
        metadataBuilder.AddAssembly(
            metadataBuilder.GetOrAddString(assemblyName), 
            Version(0, 0, 0, 0), 
            StringHandle(), 
            BlobHandle(), 
            AssemblyFlags.PublicKey, 
            AssemblyHashAlgorithm.Sha1)

    let addModule assemblyName isExe (mvid: Guid) (metadataBuilder: MetadataBuilder) =
        metadataBuilder.AddModule(
            0,
            metadataBuilder.GetOrAddString(assemblyName + if isExe then ".exe" else ".dll"),
            metadataBuilder.GetOrAddGuid(mvid),
            GuidHandle(),
            GuidHandle()
        )

    let addAssemblyReference (asmName: AssemblyName) (metadataBuilder: MetadataBuilder) =
        let cultureNameHandle =
            if String.IsNullOrWhiteSpace asmName.CultureName then 
                StringHandle() 
            else 
                metadataBuilder.GetOrAddString(asmName.CultureName)

        let publicKeyTokenHandle =
            let bytes = asmName.GetPublicKeyToken()
            if isNull bytes || bytes.Length = 0 then
                BlobHandle()
            else
                metadataBuilder.GetOrAddBlob(bytes)

        let version =
            let version = asmName.Version
            if isNull version then
                Version(0, 0, 0, 0)
            else
                version

        let handle =
            metadataBuilder.AddAssemblyReference(
                metadataBuilder.GetOrAddString(asmName.Name),
                version,
                cultureNameHandle,
                publicKeyTokenHandle,
                AssemblyFlags(),
                BlobHandle()
            )
        handle

[<Sealed>]
type ClrLocal(ty: ClrTypeHandle) =

    member _.Type = ty

[<Sealed>]
type ClrMetadataBuilder() =
    let metadataBuilder = MetadataBuilder()
    let pdbMetadataBuilder = MetadataBuilder()

    member _.Metadata = metadataBuilder
    member _.PdbMetadata = pdbMetadataBuilder

    member _.AddAssemblyWithPdb(assemblyName) =
        MetadataHelpers.addAssembly assemblyName metadataBuilder,
        MetadataHelpers.addAssembly assemblyName pdbMetadataBuilder

    member _.AddModuleWithPdb(assemblyName, isExe) =
        let mvid = Guid.NewGuid() // TODO: This is really meant for determinism, but I'm lazy...
        MetadataHelpers.addModule assemblyName isExe mvid metadataBuilder,
        MetadataHelpers.addModule assemblyName isExe mvid pdbMetadataBuilder

    member _.AddAssemblyReference(asmName) =
        MetadataHelpers.addAssemblyReference asmName metadataBuilder

    member _.AddAssemblyReferenceWithPdb(asmName) =
        MetadataHelpers.addAssemblyReference asmName metadataBuilder,
        MetadataHelpers.addAssemblyReference asmName pdbMetadataBuilder

[<Sealed>]
type ClrAssemblyBuilder(assemblyName: string, isExe: bool, primaryAssembly: AssemblyName, consoleAssembly: AssemblyName) as this =

    let tyDefsByQualifiedName = ConcurrentDictionary<string, obj>()
    let ilBuilder = BlobBuilder()

    let md = ClrMetadataBuilder()

    let metadataBuilder = md.Metadata
    let pdbMetadataBuilder = md.PdbMetadata

    do
        md.AddAssemblyWithPdb(assemblyName)
        |> ignore

    // TODO: This assembly name equality is probably not entirely correct.
    let asmRefComparer =
        { new IEqualityComparer<AssemblyName> with
            member this.GetHashCode(x) = x.Name.Length
            member this.Equals(x, y) =
                if x = y then true
                else
                    x.Name = y.Name &&
                    x.Version = y.Version
        }
    let asmRefCache = System.Collections.Concurrent.ConcurrentDictionary<AssemblyName, AssemblyReferenceHandle>(asmRefComparer)

    let processQueue (queue: System.Collections.Generic.Queue<unit -> unit>) =
        while queue.Count > 0 do
            let f = queue.Dequeue()
            f()

    let processPriorityQueue (queue: System.Collections.Generic.PriorityQueue<unit -> unit, int>) =
        while queue.Count > 0 do
            let f = queue.Dequeue()
            f()

    let tyDefQueue = System.Collections.Generic.Queue<unit -> unit>()
    let mutable nextTyDefRowId = 1
    let mutable tyDefRowCount = 0
    let fieldDefQueue = System.Collections.Generic.Queue()
    let mutable nextFieldDefRowId = 1
    let mutable fieldDefRowCount = 0
    let methDefQueue = System.Collections.Generic.Queue()
    let mutable nextMethDefRowId = 1
    let mutable methDefRowCount = 0
    let propDefQueue = System.Collections.Generic.Queue()
    let mutable nextPropDefRowId = 1
    let mutable propDefRowCount = 0
    let overrideQueue = System.Collections.Generic.Queue()
    let attrQueue = System.Collections.Generic.Queue()
    let importMethQueue = System.Collections.Generic.Queue()

    let genericParamsQueue = System.Collections.Generic.PriorityQueue<_, int>()

    let getNextTyDefRowId() =
        let n = nextTyDefRowId
        nextTyDefRowId <- nextTyDefRowId + 1
        n

    let getNextFieldDefRowId() =
        let n = nextFieldDefRowId
        nextFieldDefRowId <- nextFieldDefRowId + 1
        n

    let getNextMethDefRowId() =
        let n = nextMethDefRowId
        nextMethDefRowId <- nextMethDefRowId + 1
        n

    let getNextPropDefRowId() =
        let n = nextMethDefRowId
        nextMethDefRowId <- nextMethDefRowId + 1
        n

    let createTyDef qualifiedName (realWork: Lazy<_>) isValueType =
        let lazyFakeHandle =
            lazy
                let n = MetadataTokens.TypeDefinitionHandle(getNextTyDefRowId())
                tyDefRowCount <- tyDefRowCount + 1
                tyDefQueue.Enqueue(fun () -> 
                    let n2 = realWork.Value
                    if not (n.Equals(n2)) then
                        failwith "Invalid type definition handle."
                    )
                n
        let res = 
            ClrTypeHandle.LazyTypeDefinition(
                realWork, 
                isValueType, 
                lazyFakeHandle,
                qualifiedName
            )
        res

    let createFieldDef (realWork: Lazy<_>) name signature =
        let res = 
            ClrFieldHandle.LazyFieldDefinition(
                realWork, 
                (
                    lazy
                        let n = MetadataTokens.FieldDefinitionHandle(getNextFieldDefRowId())
                        fieldDefRowCount <- fieldDefRowCount + 1
                        fieldDefQueue.Enqueue(fun () ->
                            let n2 = realWork.Value
                            if not (n.Equals(n2)) then
                                failwith "Invalid field definition handle."
                            )
                        n
                ),
                name,
                signature
            )
        res

    let createMethDef (realWork: Lazy<_>) name signature =
        let res = 
            ClrMethodHandle.LazyMethodDefinition(
                realWork, 
                (
                    lazy
                        let n = MetadataTokens.MethodDefinitionHandle(getNextMethDefRowId())
                        methDefRowCount <- methDefRowCount + 1
                        methDefQueue.Enqueue(fun () ->
                            let n2 = realWork.Value
                            if not (n.Equals(n2)) then
                                failwith "Invalid method definition handle."
                            )
                        n
                ),
                name,
                signature
            )
        res

    let createMemRef handle name signature =
        ClrMethodHandle.MemberReference(handle, name, signature)

    let createTyRef qualifiedName isValueType handle =
        ClrTypeHandle.TypeReference(handle, isValueType, qualifiedName)

    let createTySpec isValueType tyRefHandle tyInst handle =
        ClrTypeHandle.TypeSpecification(handle, isValueType, tyRefHandle, tyInst)

    let createAsmRef (asmName: AssemblyName) =
        match asmRefCache.TryGetValue(asmName) with
        | true, handle -> handle
        | _ ->
            let handle = md.AddAssemblyReference(asmName)
            asmRefCache.[asmName] <- handle
            handle

    let primaryAsmRef = createAsmRef primaryAssembly

    let addBuiltInTyRef namespac name =
        metadataBuilder.AddTypeReference(
            AssemblyReferenceHandle.op_Implicit primaryAsmRef,
            metadataBuilder.GetOrAddString(namespac),
            metadataBuilder.GetOrAddString(name)
        )

    let addConsoleTyRef namespac name =
        let consoleAsmRef = createAsmRef (consoleAssembly)
        metadataBuilder.AddTypeReference(
            AssemblyReferenceHandle.op_Implicit consoleAsmRef,
            metadataBuilder.GetOrAddString(namespac),
            metadataBuilder.GetOrAddString(name)
        )

    let sysTy name isValueType =
        createTyRef ("System." + name) isValueType (addBuiltInTyRef "System" name)

    let sysTyObject = sysTy "Object" false

    let consoleSysTy name isValueType =
        createTyRef ("System." + name) isValueType (addConsoleTyRef "System" name)

    let moduleDefHandle, _ = md.AddModuleWithPdb(assemblyName, isExe)

    do
        metadataBuilder.AddTypeDefinition(
            TypeAttributes.Class,
            StringHandle(),
            metadataBuilder.GetOrAddString("<Module>"),
            sysTyObject.EntityHandle, 
            MetadataTokens.FieldDefinitionHandle(1),
            MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        tyDefRowCount <- tyDefRowCount + 1
        nextTyDefRowId <- nextTyDefRowId + 1

    let createConsoleWriteMethod() =
        let consoleTy = consoleSysTy "Console" false
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = false)
        encoder.Parameters(
            1,
            (fun encoder -> encoder.Void()),
            (fun encoder -> encoder.AddParameter().Type().Object())
        )

        let name = metadataBuilder.GetOrAddString("Write")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                consoleTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    let createConsoleWriteMethod_Int32() =
        let consoleTy = consoleSysTy "Console" false
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = false)
        encoder.Parameters(
            1,
            (fun encoder -> encoder.Void()),
            (fun encoder -> encoder.AddParameter().Type().Int32())
        )

        let name = metadataBuilder.GetOrAddString("Write")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                consoleTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    let createConsoleWriteMethod_String() =
        let consoleTy = consoleSysTy "Console" false
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = false)
        encoder.Parameters(
            1,
            (fun encoder -> encoder.Void()),
            (fun encoder -> encoder.AddParameter().Type().String())
        )

        let name = metadataBuilder.GetOrAddString("Write")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                consoleTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    let createGetTypeFromHandleMethod () =
        let typeTy = sysTy "Type" false
        let runtimeTypeHandleTy = sysTy "RuntimeTypeHandle" true
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = false)
        encoder.Parameters(
            1,
            (fun encoder -> encoder.Type().Type(typeTy.EntityHandle, false)),
            (fun encoder -> encoder.AddParameter().Type().Type(runtimeTypeHandleTy.EntityHandle, true))
        )

        let name = metadataBuilder.GetOrAddString("GetTypeFromHandle")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                typeTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    let createStringEqualsMethod () =
        let typeTy = sysTy "String" false
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = false)
        encoder.Parameters(
            2,
            (fun encoder -> encoder.Type().Boolean()),
            (fun encoder -> 
                encoder.AddParameter().Type().String()
                encoder.AddParameter().Type().String()
            )
        )

        let name = metadataBuilder.GetOrAddString("Equals")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                typeTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    let createTupleGetItems count =
        let ty: ClrTypeHandle =
            match count with
            | 2 -> this.``TypeReferenceTuple`2``
            | 3 -> this.``TypeReferenceTuple`3``
            | 4 -> this.``TypeReferenceTuple`4``
            | 5 -> this.``TypeReferenceTuple`5``
            | 6 -> this.``TypeReferenceTuple`6``
            | 7 -> this.``TypeReferenceTuple`7``
            | _ -> failwith "Invalid tuple"

        ImArray.init count (fun index ->
            let itemn = index + 1
            let signature = BlobBuilder()
       
            let mutable encoder = BlobEncoder(signature)
            let mutable encoder = encoder.MethodSignature(isInstanceMethod = true)
            encoder.Parameters(
                0,
                (fun encoder -> encoder.Type().GenericTypeParameter(index)),
                (fun encoder -> ())
            )

            let name = metadataBuilder.GetOrAddString("get_Item" + itemn.ToString())
            let signature = metadataBuilder.GetOrAddBlob(signature)

            let realHandle =
                metadataBuilder.AddMemberReference(
                    ty.EntityHandle,
                    name,
                    signature
                )

            createMemRef realHandle name signature
        )


    member internal _.MetadataBuilder = metadataBuilder
    member internal _.ILBuilder = ilBuilder

    member val ModuleDefinitionHandle = moduleDefHandle

    member val TypeReferenceVoid = sysTy "Void" false
    member val TypeReferenceObject = sysTyObject
    member val TypeReferenceValueType = sysTy "ValueType" false
    member val TypeReferenceEnum = sysTy "Enum" true
    member val TypeReferenceByte = sysTy "Byte" true
    member val TypeReferenceSByte = sysTy "SByte" true
    member val TypeReferenceInt16 = sysTy "Int16" true
    member val TypeReferenceUInt16 = sysTy "UInt16" true
    member val TypeReferenceInt32 = sysTy "Int32" true
    member val TypeReferenceUInt32 = sysTy "UInt32" true
    member val TypeReferenceInt64 = sysTy "Int64" true
    member val TypeReferenceUInt64 = sysTy "UInt64" true
    member val TypeReferenceSingle = sysTy "Single" true
    member val TypeReferenceDouble = sysTy "Double" true
    member val TypeReferenceChar = sysTy "Char" true
    member val TypeReferenceBoolean = sysTy "Boolean" true
    member val TypeReferenceString = sysTy "String" false
    member val TypeReferenceArray = sysTy "Array" false
    member val TypeReferenceIntPtr = sysTy "IntPtr" true
    member val TypeReferenceUIntPtr = sysTy "UIntPtr" true

    member val ``TypeReferenceAttribute`` = sysTy "Attribute" false

    member val ``TypeReferenceType`` = sysTy "Type" false

    member val ``TypeReferenceAction`` = sysTy "Action" false
    member val ``TypeReferenceAction`1`` = sysTy "Action`1" false
    member val ``TypeReferenceAction`2`` = sysTy "Action`2" false
    member val ``TypeReferenceAction`3`` = sysTy "Action`3" false
    member val ``TypeReferenceAction`4`` = sysTy "Action`4" false

    member val ``TypeReferenceFunc`1`` = sysTy "Func`1" false
    member val ``TypeReferenceFunc`2`` = sysTy "Func`2" false
    member val ``TypeReferenceFunc`3`` = sysTy "Func`3" false
    member val ``TypeReferenceFunc`4`` = sysTy "Func`4" false
    member val ``TypeReferenceFunc`5`` = sysTy "Func`5" false

    member val ``TypeReferenceTuple`2`` = sysTy "Tuple`2" false
    member val ``TypeReferenceTuple`3`` = sysTy "Tuple`3" false
    member val ``TypeReferenceTuple`4`` = sysTy "Tuple`4" false
    member val ``TypeReferenceTuple`5`` = sysTy "Tuple`5" false
    member val ``TypeReferenceTuple`6`` = sysTy "Tuple`6" false
    member val ``TypeReferenceTuple`7`` = sysTy "Tuple`7" false

    // 8th type parameter is TRest
    member val ``TypeReferenceTuple`8`` = sysTy "Tuple`8" false

    member val ``Tuple`2_ItemMethods`` = lazy createTupleGetItems 2
    member val ``Tuple`3_ItemMethods`` = lazy createTupleGetItems 3
    member val ``Tuple`4_ItemMethods`` = lazy createTupleGetItems 4
    member val ``Tuple`5_ItemMethods`` = lazy createTupleGetItems 5
    member val ``Tuple`6_ItemMethods`` = lazy createTupleGetItems 6
    member val ``Tuple`7_ItemMethods`` = lazy createTupleGetItems 7

    member val ``String_Equals`` = lazy createStringEqualsMethod ()

    member val ``TypeReferenceValueTuple`2`` = sysTy "ValueTuple`2" true
    member val ``TypeReferenceValueTuple`3`` = sysTy "ValueTuple`3" true
    member val ``TypeReferenceValueTuple`4`` = sysTy "ValueTuple`4" true
    member val ``TypeReferenceValueTuple`5`` = sysTy "ValueTuple`5" true

    member val tr_InAttribute =
        createTyRef 
            ("System.Runtime.InteropServices.InAttribute") 
            false 
            (addBuiltInTyRef "System.Runtime.InteropServices" "InAttribute")

    member val tr_IsReadOnlyAttribute =
        createTyRef 
            ("System.Runtime.CompilerServices.IsReadOnlyAttribute") 
            false 
            (addBuiltInTyRef "System.Runtime.CompilerServices" "IsReadOnlyAttribute")
            

    member val ``ConsoleWriteMethod`` = lazy createConsoleWriteMethod()
    member val ``ConsoleWriteMethod_Int32`` = lazy createConsoleWriteMethod_Int32()
    member val ``ConsoleWriteMethod_String`` = lazy createConsoleWriteMethod_String()
    member val ``GetTypeFromHandleMethod`` = lazy createGetTypeFromHandleMethod()

    member val EntryPoint = ClrMethodHandle.None with get, set

    member this.EncodeReturnType (encoder: ReturnTypeEncoder, returnTy: ClrTypeHandle) =
        if returnTy.HasEntityHandle then
            if returnTy.EntityHandle.Equals(this.TypeReferenceVoid.EntityHandle) then
                encoder.Void()
            else
                match returnTy.TryElementType with
                | ValueSome elementTy when returnTy.IsByRef_t ->
                    this.EncodeType(encoder.Type(true), elementTy)
                | _ ->
                    this.EncodeType(encoder.Type(false), returnTy)
        else
            match returnTy.TryElementType with
            | ValueSome elementTy when returnTy.IsByRef_t ->
                this.EncodeType(encoder.Type(true), elementTy)
            | _ ->
                this.EncodeType(encoder.Type(false), returnTy)

    member this.EncodeParameters (encoder: ParametersEncoder, parTys: ClrTypeHandle imarray) =
        parTys
        |> ImArray.iter (fun parTy ->
            match parTy.TryElementType with
            | ValueSome elementTy when parTy.IsByRef_t ->
                this.EncodeType(encoder.AddParameter().Type(true), elementTy)
            | _ ->
                this.EncodeType(encoder.AddParameter().Type(false), parTy)
        )

    member this.EncodeAttributeElementType (encoder: CustomAttributeElementTypeEncoder, handle: ClrTypeHandle) =
        match handle with
        | ClrTypeHandle.TypeSpecification(_, isValueType, tyRefHandle, tyInst) ->
            failwith "assert"

        | ClrTypeHandle.Array(elementTyHandle, rank) ->
            failwith "assert"
                
        | ClrTypeHandle.TypeVariable(index, kind) ->
            failwith "assert"

        | ClrTypeHandle.ByRef(ty) ->
            failwith "assert"

        | ClrTypeHandle.FunctionPointer(cc, parTys, returnTy) ->
            failwith "assert"

        | ClrTypeHandle.NativePointer(elementTy) ->
            failwith "assert"

        | _ ->

        match handle.EntityHandle with
        | x when x = this.TypeReferenceString.EntityHandle ->
            encoder.String()
        | x when x = this.TypeReferenceObject.EntityHandle ->
            failwith "assert"
        | x when x = this.TypeReferenceBoolean.EntityHandle ->
            encoder.Boolean()
        | x when x = this.TypeReferenceByte.EntityHandle ->
            encoder.Byte()
        | x when x = this.TypeReferenceSByte.EntityHandle ->
            encoder.SByte()
        | x when x = this.TypeReferenceInt16.EntityHandle ->
            encoder.Int16()
        | x when x = this.TypeReferenceUInt16.EntityHandle ->
            encoder.UInt16()
        | x when x = this.TypeReferenceInt32.EntityHandle ->
            encoder.Int32()
        | x when x = this.TypeReferenceUInt32.EntityHandle ->
            encoder.UInt32()
        | x when x = this.TypeReferenceInt64.EntityHandle ->
            encoder.Int64()
        | x when x = this.TypeReferenceUInt64.EntityHandle ->
            encoder.UInt64()
        | x when x = this.TypeReferenceSingle.EntityHandle ->
            encoder.Single()
        | x when x = this.TypeReferenceDouble.EntityHandle ->
            encoder.Double()
        | x when x = this.TypeReferenceChar.EntityHandle ->
            encoder.Char()
        | x when x = this.TypeReferenceIntPtr.EntityHandle ->
            failwith "assert"
        | x when x = this.TypeReferenceUIntPtr.EntityHandle ->
            failwith "assert"
        | x when x = this.TypeReferenceType.EntityHandle ->
            encoder.SystemType()
        | _ ->
            // TODO: Handle enum
            failwith "assert"

    member this.EncodeType (encoder: SignatureTypeEncoder, handle: ClrTypeHandle) =
        match handle with
        | ClrTypeHandle.TypeSpecification(_, isValueType, tyRefHandle, tyInst) ->
            let mutable encoderInst = encoder.GenericInstantiation(tyRefHandle.EntityHandle, tyInst.Length, isValueType)
            tyInst
            |> ImArray.iter (fun ty ->
                this.EncodeType(encoderInst.AddArgument(), ty)
            )
        | ClrTypeHandle.Array(elementTyHandle, rank) ->
            if rank < 1 then failwith "Invalid rank."
            if rank = 1 then
                let mutable elementEncoder = encoder.SZArray()
                this.EncodeType(elementEncoder, elementTyHandle)
            else
                
                let mutable elementEncoder = Unchecked.defaultof<_>
                let mutable arrayShape = Unchecked.defaultof<_>
                encoder.Array(&elementEncoder, &arrayShape)
                arrayShape.Shape(rank, ImArray.empty, ImArray.init rank (fun _ -> 0))
                
        | ClrTypeHandle.TypeVariable(index, kind) ->
            match kind with
            | ClrTypeVariableKind.Type ->
                encoder.GenericTypeParameter(index)
            | ClrTypeVariableKind.Method ->
                encoder.GenericMethodTypeParameter(index)
        | ClrTypeHandle.ByRef(ty) ->
            this.EncodeType(encoder, ty)
        | ClrTypeHandle.ModReq(modifier, ty) ->
            this.EncodeType(encoder, ty)
            encoder.CustomModifiers().AddModifier(modifier.EntityHandle, false)
            |> ignore
        | ClrTypeHandle.FunctionPointer(cc, parTys, returnTy) ->
            let mutable parEncoder = Unchecked.defaultof<_>
            let mutable returnTyEncoder = Unchecked.defaultof<_>
            encoder.FunctionPointer(convention = cc).Parameters(parTys.Length, &returnTyEncoder, &parEncoder)
            this.EncodeReturnType(returnTyEncoder, returnTy)
            this.EncodeParameters(parEncoder, parTys)
        | ClrTypeHandle.NativePointer(elementTy) ->
            if elementTy = this.TypeReferenceVoid then
                encoder.VoidPointer()
            else
                this.EncodeType(encoder.Pointer(), elementTy)

        | _ ->

        let entityHandle = handle.EntityHandle

        if entityHandle.Equals(this.TypeReferenceString.EntityHandle) then
            encoder.String()
        elif entityHandle.Equals(this.TypeReferenceObject.EntityHandle) then
            encoder.Object()
        elif entityHandle.Equals(this.TypeReferenceBoolean.EntityHandle) then
            encoder.Boolean()
        elif entityHandle.Equals(this.TypeReferenceByte.EntityHandle) then
            encoder.Byte()
        elif entityHandle.Equals(this.TypeReferenceSByte.EntityHandle) then
            encoder.SByte()
        elif entityHandle.Equals(this.TypeReferenceInt16.EntityHandle) then
            encoder.Int16()
        elif entityHandle.Equals(this.TypeReferenceUInt16.EntityHandle) then
            encoder.UInt16()
        elif entityHandle.Equals(this.TypeReferenceInt32.EntityHandle) then
            encoder.Int32()
        elif entityHandle.Equals(this.TypeReferenceUInt32.EntityHandle) then
            encoder.UInt32()
        elif entityHandle.Equals(this.TypeReferenceInt64.EntityHandle) then
            encoder.Int64()
        elif entityHandle.Equals(this.TypeReferenceUInt64.EntityHandle) then
            encoder.UInt64()
        elif entityHandle.Equals(this.TypeReferenceSingle.EntityHandle) then
            encoder.Single()
        elif entityHandle.Equals(this.TypeReferenceDouble.EntityHandle) then
            encoder.Double()
        elif entityHandle.Equals(this.TypeReferenceChar.EntityHandle) then
            encoder.Char()
        elif entityHandle.Equals(this.TypeReferenceIntPtr.EntityHandle) then
            encoder.IntPtr()
        elif entityHandle.Equals(this.TypeReferenceUIntPtr.EntityHandle) then
            encoder.UIntPtr()
        else
            encoder.Type(handle.EntityHandle, handle.IsValueType)

    member internal this.CreateMethodSignature(convention, tyParCount: int, isInstance, parTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        let signature = BlobBuilder()
        BlobEncoder(signature).MethodSignature(convention, tyParCount, isInstance).Parameters(
            parTys.Length,
            (fun encoder ->
                this.EncodeReturnType(encoder, returnTy)),
            (fun encoder -> 
                this.EncodeParameters(encoder, parTys))
        )
        signature
        |> metadataBuilder.GetOrAddBlob

    member internal this.CreateStandaloneSignature(locals: ClrLocal imarray) =
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable localVarSignature: LocalVariablesEncoder = encoder.LocalVariableSignature(locals.Length)

        locals
        |> ImArray.iter (fun local ->
            this.EncodeType(localVarSignature.AddVariable().Type(local.Type.IsByRef_t), local.Type)
        )

        signature
        |> metadataBuilder.GetOrAddBlob
        |> metadataBuilder.AddStandaloneSignature

    member internal this.CreatePropertySignature(isInstance, parTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        let signature = BlobBuilder()
        BlobEncoder(signature).PropertySignature(isInstance).Parameters(
            parTys.Length,
            (fun encoder ->
                if returnTy.HasEntityHandle then
                    match returnTy.EntityHandle with
                    | x when x = this.TypeReferenceVoid.EntityHandle -> 
                        encoder.Void()
                    | _ ->
                        this.EncodeType(encoder.Type(returnTy.IsByRef_t), returnTy)
                else
                    this.EncodeType(encoder.Type(returnTy.IsByRef_t), returnTy)),
            (fun encoder -> 
                for parTy in parTys do
                    this.EncodeType(encoder.AddParameter().Type(parTy.IsByRef_t), parTy))
        )
        signature
        |> metadataBuilder.GetOrAddBlob

    member internal this.CreateFieldSignature(ty: ClrTypeHandle) =
        let signature = BlobBuilder()
        this.EncodeType(BlobEncoder(signature).FieldSignature(), ty)
        signature
        |> metadataBuilder.GetOrAddBlob

    member internal this.AddGenericInstanceType(genericTyHandle: ClrTypeHandle, tyInst: ClrTypeHandle imarray) =
        let genericTyHandle =
            match genericTyHandle with
            | ClrTypeHandle.TypeSpecification(tyRefHandle=tyRefHandle) ->
                tyRefHandle
            | _ ->
                genericTyHandle
        let isValueType = genericTyHandle.IsValueType
        let handle = 
            createTySpec isValueType genericTyHandle tyInst <|
                let signature = BlobBuilder()
                let mutable encoder = BlobEncoder(signature)
                let mutable encoder = encoder.TypeSpecificationSignature()

                let mutable encoderInst = 
                    encoder.GenericInstantiation(genericTyHandle.EntityHandle, tyInst.Length, isValueType)
                tyInst
                |> ImArray.iter (fun ty ->
                    this.EncodeType(encoderInst.AddArgument(), ty)
                )

                metadataBuilder.AddTypeSpecification(metadataBuilder.GetOrAddBlob(signature))
        handle

    member internal this.EnqueueOverride(f) =
        overrideQueue.Enqueue(f)
    
    member this.Write(stream: IO.Stream, pdbStream: IO.Stream) =

        while tyDefQueue.Count > 0 do
            processQueue tyDefQueue

        processQueue methDefQueue
        processQueue overrideQueue
        processQueue fieldDefQueue 

        if methDefQueue.Count > 0 || overrideQueue.Count > 0 || fieldDefQueue.Count > 0 || tyDefQueue.Count > 0 then
            failwith "Expected empty queues."

        processPriorityQueue genericParamsQueue
        processQueue attrQueue

        let entryPoint =
            match this.EntryPoint with 
            | ClrMethodHandle.LazyMethodDefinition(realHandle, _, _, _) ->
                if not realHandle.IsValueCreated then
                    failwith "Handle not evaluated."
                realHandle.Value
            | _ ->
                MethodDefinitionHandle()

        let rootBuilder = new MetadataRootBuilder(metadataBuilder)

        let algoName = HashAlgorithmName.SHA256
        let mutable computedHash = Unchecked.defaultof<_>
        let hash (content: Blob seq) =
            use incrHash = IncrementalHash.CreateHash(algoName)
            let bytes =
                content
                |> Seq.map (fun x -> x.GetBytes().Array)
                |> Array.concat
            incrHash.AppendData(bytes)
            computedHash <- incrHash.GetHashAndReset().ToImmutableArray()
            computedHash
        let portablePdbIdProvider =
            fun (content: Blob seq) -> BlobContentId.FromHash(hash content)

        let pdbBuilder = 
            PortablePdbBuilder(
                pdbMetadataBuilder, 
                rootBuilder.Sizes.RowCounts, 
                entryPoint,
                idProvider = portablePdbIdProvider
            )
        let pdbBlob = BlobBuilder()
        let pdbBlobContentId = pdbBuilder.Serialize(pdbBlob)
        pdbBlob.WriteContentTo(pdbStream)

        let peHeaderBuilder = 
            if isExe then
                PEHeaderBuilder(imageCharacteristics = Characteristics.ExecutableImage)
            else
                PEHeaderBuilder(imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll))


        let debugDirBuilder = new DebugDirectoryBuilder()

        debugDirBuilder.AddCodeViewEntry(System.IO.Path.ChangeExtension(assemblyName, ".pdb"), pdbBlobContentId, pdbBuilder.FormatVersion)

        let peBuilder = 
            ManagedPEBuilder(
                peHeaderBuilder, 
                rootBuilder, 
                ilBuilder, 
                flags = CorFlags.ILOnly,
                entryPoint = entryPoint,
                debugDirectoryBuilder = debugDirBuilder)
        let peBlob = BlobBuilder()
        peBuilder.Serialize(peBlob) |> ignore
        peBlob.WriteContentTo(stream)

    member this.AddTypeReference(enclosingTyHandle: ClrTypeHandle, namespac: string, name: string, isValueType) =
        match namespac, name, isValueType with
        | "System", "Type", false ->
            this.TypeReferenceType
        // TODO: Handle other specific namespaces/names.
        | _ ->
            let fullQualifiedName =
                if enclosingTyHandle.IsNamed then
                    enclosingTyHandle.FullyQualifiedName + "+" + name
                else
                    let asmName =
                        match enclosingTyHandle with
                        | ClrTypeHandle.AssemblyReference(_, qualifiedName) -> ", " + qualifiedName
                        | _ -> ""
                    if String.IsNullOrWhiteSpace namespac then
                        name + asmName
                    else
                        namespac + "." + name + asmName
            metadataBuilder.AddTypeReference(
                enclosingTyHandle.EntityHandle, 
                (if String.IsNullOrWhiteSpace namespac then StringHandle() else metadataBuilder.GetOrAddString(namespac)), 
                metadataBuilder.GetOrAddString(name))
            |> createTyRef fullQualifiedName isValueType

    member this.AddGenericInstanceTypeReference(tyHandle: ClrTypeHandle, tyArgs: ClrTypeHandle imarray) =
        this.AddGenericInstanceType(tyHandle, tyArgs)

    member _.AddAssemblyReference(asmName) =
        (createAsmRef asmName, asmName.FullName)
        |> ClrTypeHandle.AssemblyReference

    member this.AddMethodReference(convention, isInstance, enclosingTyHandle: ClrTypeHandle, name, tyParCount: int, parTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        let name = metadataBuilder.GetOrAddString(name)
        let signature = this.CreateMethodSignature(convention, tyParCount, isInstance, parTys, returnTy)       
        let realHandle =
            metadataBuilder.AddMemberReference(
                enclosingTyHandle.EntityHandle,
                name,
                signature
            )
        createMemRef realHandle name signature

    member this.AddMethodSpecification(methHandle: ClrMethodHandle, tyArgs: ClrTypeHandle imarray) =
        match methHandle with
        | ClrMethodHandle.MethodSpecification _ ->
            failwith "Unexpected method specification."
        | _ -> ()

        if tyArgs.IsEmpty then
            methHandle
        else
            let methodSpecSig = new BlobBuilder()
            let methodSpecEncoder = new BlobEncoder(methodSpecSig)
            let mutable encoder = methodSpecEncoder.MethodSpecificationSignature(tyArgs.Length)

            tyArgs
            |> ImArray.iter (fun tyArg ->
                this.EncodeType(encoder.AddArgument(), tyArg)
            )

            let handleSig = metadataBuilder.GetOrAddBlob(encoder.Builder)

            ClrMethodHandle.MethodSpecification(lazy metadataBuilder.AddMethodSpecification(methHandle.UnsafeLazilyEvaluateEntityHandle(), handleSig), methHandle.Name, handleSig)

    member this.CreateMethodSpecification(enclosingTyHandle: ClrTypeHandle, methHandle: ClrMethodHandle, tyArgs: ClrTypeHandle imarray) =
        match methHandle with
        | ClrMethodHandle.MethodSpecification _ ->
            failwith "Unexpected method specification."
        | _ -> ()

        let memRefHandle =
            let handle =
                metadataBuilder.AddMemberReference(
                    enclosingTyHandle.EntityHandle,
                    methHandle.Name,
                    methHandle.Signature
                )

            createMemRef handle methHandle.Name methHandle.Signature

        if tyArgs.IsEmpty then
            memRefHandle
        else
            let methodSpecSig = new BlobBuilder()
            let methodSpecEncoder = new BlobEncoder(methodSpecSig)
            let mutable encoder = methodSpecEncoder.MethodSpecificationSignature(tyArgs.Length)

            tyArgs
            |> ImArray.iter (fun tyArg ->
                this.EncodeType(encoder.AddArgument(), tyArg)
            )

            let handleSig = metadataBuilder.GetOrAddBlob(encoder.Builder)
            ClrMethodHandle.MethodSpecification(lazy metadataBuilder.AddMethodSpecification(memRefHandle.UnsafeLazilyEvaluateEntityHandle(), handleSig), methHandle.Name, handleSig)

    member this.AddAnonymousFunctionType(argTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) : ClrTypeHandle * ClrTypeHandle imarray =
        if returnTy = this.TypeReferenceVoid then
            match argTys.Length with
            | 0 -> this.TypeReferenceAction, ImArray.empty
            | 1 -> this.AddGenericInstanceType(this.``TypeReferenceAction`1``, argTys), argTys
            | 2 -> this.AddGenericInstanceType(this.``TypeReferenceAction`2``, argTys), argTys
            | 3 -> this.AddGenericInstanceType(this.``TypeReferenceAction`3``, argTys), argTys
            | 4 -> this.AddGenericInstanceType(this.``TypeReferenceAction`4``, argTys), argTys
            | _ ->
                raise(System.NotImplementedException())
        else
            match argTys.Length with
            | 0 -> 
                let tyInst = ImArray.createOne returnTy
                this.AddGenericInstanceType(this.``TypeReferenceFunc`1``, tyInst), tyInst
            | 1 -> 
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`2``, tyInst), tyInst
            | 2 -> 
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`3``, tyInst), tyInst
            | 3 -> 
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`4``, tyInst), tyInst
            | 4 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`5``, tyInst), tyInst
            | _ ->
                raise(System.NotImplementedException())

    member this.AddAnonymousFunctionConstructor(argTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        let parent, tyInst = this.AddAnonymousFunctionType(argTys, returnTy)

        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = true)
        encoder.Parameters(
            2,
            (fun encoder -> encoder.Void()),
            (fun encoder -> 
                this.EncodeType(encoder.AddParameter().Type(), this.TypeReferenceObject)
                this.EncodeType(encoder.AddParameter().Type(), this.TypeReferenceIntPtr))
        )

        let name = metadataBuilder.GetOrAddString(".ctor")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                parent.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    member this.AddAnonymousFunctionInvoke(argTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        let parent, tyInst = this.AddAnonymousFunctionType(argTys, returnTy)

        let returnsVoid = returnTy.EntityHandle = this.TypeReferenceVoid.EntityHandle

        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = true)
        encoder.Parameters(
            argTys.Length,
            (fun encoder -> 
                if returnsVoid then
                    encoder.Void()
                else
                    encoder.Type().GenericTypeParameter(tyInst.Length - 1)),
            (fun encoder -> 
                let n = if returnsVoid then 1 else 2
                for i = 0 to tyInst.Length - n do
                    encoder.AddParameter().Type().GenericTypeParameter(i))
        )

        let name = metadataBuilder.GetOrAddString("Invoke")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                parent.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    member this.AddArrayType(elementTy: ClrTypeHandle, rank) =
        ClrTypeHandle.Array(elementTy, rank)

    member this.AddFunctionPointer(cc, parTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        ClrTypeHandle.FunctionPointer(cc, parTys, returnTy)

    member this.AddNativePointer(elementTy: ClrTypeHandle) =
        ClrTypeHandle.NativePointer(elementTy)

    member this.AddValueTupleType(tyInst: ClrTypeHandle imarray) =
        match tyInst.Length with
        | 2 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`2``, tyInst)
        | 3 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`3``, tyInst)
        | 4 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`4``, tyInst)
        | 5 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`5``, tyInst)
        | _ ->
            raise(System.NotImplementedException("other tuple lengths"))

    member this.AddTupleType(tyInst: ClrTypeHandle imarray) =
        match tyInst.Length with
        | 2 -> this.AddGenericInstanceType(this.``TypeReferenceTuple`2``, tyInst)
        | 3 -> this.AddGenericInstanceType(this.``TypeReferenceTuple`3``, tyInst)
        | 4 -> this.AddGenericInstanceType(this.``TypeReferenceTuple`4``, tyInst)
        | 5 -> this.AddGenericInstanceType(this.``TypeReferenceTuple`5``, tyInst)
        | _ ->
            raise(System.NotImplementedException("other tuple lengths"))

    member this.AddValueTupleConstructor(tyInst: ClrTypeHandle imarray) =
        let valueTupleTy = this.AddValueTupleType(tyInst)

        let methTyInst =
            ImArray.init tyInst.Length (fun i -> ClrTypeHandle.CreateVariable(i, ClrTypeVariableKind.Type))

        let name = metadataBuilder.GetOrAddString(".ctor")
        let signature = this.CreateMethodSignature(SignatureCallingConvention.Default, 0, true, methTyInst, this.TypeReferenceVoid)

        let realHandle =
            metadataBuilder.AddMemberReference(
                valueTupleTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    member this.AddTupleConstructor(tyInst: ClrTypeHandle imarray) =
        let tupleTy = this.AddTupleType(tyInst)

        let methTyInst =
            ImArray.init tyInst.Length (fun i -> ClrTypeHandle.CreateVariable(i, ClrTypeVariableKind.Type))

        let name = metadataBuilder.GetOrAddString(".ctor")
        let signature = this.CreateMethodSignature(SignatureCallingConvention.Default, 0, true, methTyInst, this.TypeReferenceVoid)

        let realHandle =
            metadataBuilder.AddMemberReference(
                tupleTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    member this.CreateTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyPars, isStruct) =
        let tyDefBuilder = ClrTypeDefinitionBuilder(this, enclosingTyHandle, namespac, name, tyPars, isStruct, false)
        tyDefQueue.Enqueue(fun () -> tyDefBuilder.Handle.EntityHandle |> ignore)
        tyDefBuilder

    member this.CreateEnumTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyPars) =
        let tyDefBuilder = ClrTypeDefinitionBuilder(this, enclosingTyHandle, namespac, name, tyPars, false, true)
        tyDefQueue.Enqueue(fun () -> tyDefBuilder.Handle.EntityHandle |> ignore)
        tyDefBuilder

    member this.AddFieldReference(parent: ClrTypeHandle, name: string, fieldTy: ClrTypeHandle) =  
        let nameHandle = metadataBuilder.GetOrAddString(name)
        let signature = this.CreateFieldSignature(fieldTy)
        let handle =
            metadataBuilder.AddMemberReference(
                parent.EntityHandle,
                nameHandle,
                signature
            )

        ClrFieldHandle.MemberReference(handle, nameHandle, signature)

    member this.AddFieldInstance(parent: ClrTypeHandle, fieldHandle: ClrFieldHandle) =  
        let handle =
            metadataBuilder.AddMemberReference(
                parent.EntityHandle,
                fieldHandle.NameHandle,
                fieldHandle.Signature
            )

        ClrFieldHandle.MemberReference(handle, fieldHandle.NameHandle, fieldHandle.Signature)

    member this.AddBlob(blobBuilder: BlobBuilder) =
        metadataBuilder.GetOrAddBlob(blobBuilder)

    member _.AddTypeAttribute(parent: ClrTypeHandle, ctor: ClrMethodHandle, blobHandle) =
        attrQueue.Enqueue(fun () ->
            metadataBuilder.AddCustomAttribute(parent.EntityHandle, ctor.UnsafeLazilyEvaluateEntityHandle(), blobHandle) |> ignore
        )

    member _.AddFieldAttribute(parent: ClrFieldHandle, ctor: ClrMethodHandle, blobHandle) =
        attrQueue.Enqueue(fun () ->
            metadataBuilder.AddCustomAttribute(parent.EntityHandle, ctor.UnsafeLazilyEvaluateEntityHandle(), blobHandle) |> ignore
        )

    member _.AddMethodAttribute(parent: ClrMethodHandle, ctor: ClrMethodHandle, blobHandle) =
        attrQueue.Enqueue(fun () ->
            metadataBuilder.AddCustomAttribute(parent.UnsafeLazilyEvaluateEntityHandle(), ctor.UnsafeLazilyEvaluateEntityHandle(), blobHandle) |> ignore
        )

    member internal this.CreateMethodDefinitionHandle(f, name, signature) =
        createMethDef f name signature

    member internal this.CreateTypeDefinitionHandle(f, isValueType, qualifiedName) =
        if tyDefsByQualifiedName.TryAdd(qualifiedName, null) then
            createTyDef qualifiedName f isValueType
        else
            failwith $"'{qualifiedName}' already created for the assembly."

    member internal this.CreateFieldDefinitionHandle(f, name, signature) =
        createFieldDef f name signature

    member internal this.TypeDefinitionRowCount = tyDefRowCount
    member internal this.SetTypeDefinitionRowCount n = tyDefRowCount <- n

    member internal this.FieldDefinitionRowCount = fieldDefRowCount
    member internal this.SetFieldDefinitionRowCount n = fieldDefRowCount <- n

    member internal this.MethodDefinitionRowCount = methDefRowCount
    member internal this.SetMethodDefinitionRowCount n = methDefRowCount <- n

    member internal this.PropertyDefinitionRowCount = propDefRowCount
    member internal this.SetPropertyDefinitionRowCount n = propDefRowCount <- n

    member internal this.GenericParameterQueue = genericParamsQueue

[<Sealed>]
type ClrMethodDefinitionBuilder internal (asmBuilder: ClrAssemblyBuilder, enclosingTyParCount: int, name, tyPars: (string) imarray, pars: (string * ClrTypeHandle) imarray, returnTy: ClrTypeHandle, isInstance: bool) as this =
    
    let mutable cachedHandle = ValueNone
    let parTys =
        pars
        |> ImArray.map snd

    let mutable overridesOpt: ClrMethodHandle option = None
    let mutable pinvokeInfoOpt: (string * string) option = None

    let handle =
        let f = lazy this.Build()
        let nameHandle = asmBuilder.MetadataBuilder.GetOrAddString(name)
        let signature = asmBuilder.CreateMethodSignature(SignatureCallingConvention.Default, tyPars.Length, isInstance, parTys, returnTy)
        asmBuilder.CreateMethodDefinitionHandle(f, nameHandle, signature)

    static let getBranchOpCode instr =
        match instr with
        | I.Beq _ -> ILOpCode.Beq
        | I.Bge _ -> ILOpCode.Bge
        | I.Bge_un _ -> ILOpCode.Bge_un
        | I.Bgt _ -> ILOpCode.Bgt
        | I.Bgt_un _ -> ILOpCode.Bgt_un
        | I.Ble _ -> ILOpCode.Ble
        | I.Ble_un _ -> ILOpCode.Ble_un
        | I.Blt _ -> ILOpCode.Blt
        | I.Blt_un _ -> ILOpCode.Blt_un
        | I.Bne_un _ -> ILOpCode.Bne_un
        | I.Brfalse _ -> ILOpCode.Brfalse
        | I.Brtrue _ -> ILOpCode.Brtrue
        | I.Br _ -> ILOpCode.Br
        | _ -> OlyAssert.Fail("Invalid branch instruction.")

    static let getShortBranchOpCode opCode =
        match opCode with
        | ILOpCode.Beq -> ILOpCode.Beq_s
        | ILOpCode.Bge -> ILOpCode.Bge_un
        | ILOpCode.Bge_un -> ILOpCode.Bge_un_s
        | ILOpCode.Bgt -> ILOpCode.Bgt_s
        | ILOpCode.Bgt_un -> ILOpCode.Bgt_un_s
        | ILOpCode.Ble -> ILOpCode.Ble_s
        | ILOpCode.Ble_un -> ILOpCode.Ble_un_s
        | ILOpCode.Blt -> ILOpCode.Blt_s
        | ILOpCode.Blt_un -> ILOpCode.Blt_un_s
        | ILOpCode.Bne_un -> ILOpCode.Bne_un_s
        | ILOpCode.Brfalse -> ILOpCode.Brfalse_s
        | ILOpCode.Brtrue -> ILOpCode.Brtrue_s
        | ILOpCode.Br -> ILOpCode.Br_s
        | _ -> OlyAssert.Fail("Invalid op code for short branch.")

    static let emitTypeToken (asmBuilder: ClrAssemblyBuilder) (il: byref<InstructionEncoder>) (handle: ClrTypeHandle) =
        match handle.TryTypeVariable with
        | ValueSome(index, kind) ->
            let builder = new BlobBuilder()
            let mutable encoder = new BlobEncoder(builder)
            let mutable specSig = encoder.TypeSpecificationSignature()
            match kind with
            | ClrTypeVariableKind.Type ->
                specSig.GenericTypeParameter(index)
            | ClrTypeVariableKind.Method ->
                specSig.GenericMethodTypeParameter(index)
            let blob = asmBuilder.MetadataBuilder.GetOrAddBlob(builder)
            il.Token(asmBuilder.MetadataBuilder.AddTypeSpecification(blob))
        | _ ->
            match handle with
            | ClrTypeHandle.FunctionPointer _ 
            | ClrTypeHandle.NativePointer _  ->
                il.Token(asmBuilder.TypeReferenceUIntPtr.EntityHandle)
            | _ ->
                il.Token(handle.EntityHandle)

    static let emitInstr (asmBuilder: ClrAssemblyBuilder) (maxStack: byref<int32>) (il: byref<InstructionEncoder>) instr =
        match instr with
        | I.Conv_i ->
            il.OpCode(ILOpCode.Conv_i)
        | I.Conv_i1 ->
            il.OpCode(ILOpCode.Conv_i1)
        | I.Conv_i2 ->
            il.OpCode(ILOpCode.Conv_i2)
        | I.Conv_i4 ->
            il.OpCode(ILOpCode.Conv_i4)
        | I.Conv_i8 ->
            il.OpCode(ILOpCode.Conv_i8)

        | I.Conv_u ->
            il.OpCode(ILOpCode.Conv_u)
        | I.Conv_u1 ->
            il.OpCode(ILOpCode.Conv_u1)
        | I.Conv_u2 ->
            il.OpCode(ILOpCode.Conv_u2)
        | I.Conv_u4 ->
            il.OpCode(ILOpCode.Conv_u4)
        | I.Conv_u8 ->
            il.OpCode(ILOpCode.Conv_u8)

        | I.Conv_r_un ->
            il.OpCode(ILOpCode.Conv_r_un)
        | I.Conv_r4 ->
            il.OpCode(ILOpCode.Conv_r4)
        | I.Conv_r8 ->
            il.OpCode(ILOpCode.Conv_r8)

        | I.Calli(cc, parTys, returnTy) ->
            maxStack <- max maxStack (parTys.Length + 1)
            let signature = BlobBuilder()
            let mutable encoder = BlobEncoder(signature)
            let mutable parEncoder = Unchecked.defaultof<_>
            let mutable returnTyEncoder = Unchecked.defaultof<_>
            encoder.MethodSignature(convention = cc).Parameters(parTys.Length, &returnTyEncoder, &parEncoder)
            asmBuilder.EncodeReturnType(returnTyEncoder, returnTy)
            asmBuilder.EncodeParameters(parEncoder, parTys)
            let handle = asmBuilder.MetadataBuilder.AddStandaloneSignature(asmBuilder.MetadataBuilder.GetOrAddBlob(signature))
            il.CallIndirect(handle)

        | I.Ldlen ->
            il.OpCode(ILOpCode.Ldlen)

        | I.Ldtoken(handle) ->
            il.OpCode(ILOpCode.Ldtoken)
            emitTypeToken asmBuilder &il handle

        | I.Initobj(handle) ->
            match handle with
            | ClrTypeHandle.NativePointer _
            | ClrTypeHandle.FunctionPointer _ ->
                il.OpCode(ILOpCode.Conv_u)
                il.OpCode(ILOpCode.Ldc_i4_0)
            | _ ->
                il.OpCode(ILOpCode.Initobj)
                emitTypeToken asmBuilder &il handle

        | I.Constrained(handle) ->
            il.OpCode(ILOpCode.Constrained)
            emitTypeToken asmBuilder &il handle

        | I.Throw ->
            il.OpCode(ILOpCode.Throw)

        | I.StindRef ->
            il.OpCode(ILOpCode.Stind_ref)
        | I.Stobj(handle) ->
            il.OpCode(ILOpCode.Stobj) 
            emitTypeToken asmBuilder &il handle

        | I.Nop ->
            il.OpCode(ILOpCode.Nop)
        | I.Ret ->
            il.OpCode(ILOpCode.Ret)
        | I.Box handle ->
            il.OpCode(ILOpCode.Box)
            emitTypeToken asmBuilder &il handle
        | I.Unbox handle ->
            il.OpCode(ILOpCode.Unbox)
            emitTypeToken asmBuilder &il handle
        | I.Unbox_any handle ->
            il.OpCode(ILOpCode.Unbox_any)
            emitTypeToken asmBuilder &il handle

        | I.Add ->
            il.OpCode(ILOpCode.Add)
        | I.Sub ->
            il.OpCode(ILOpCode.Sub)
        | I.Mul ->
            il.OpCode(ILOpCode.Mul)
        | I.Div ->
            il.OpCode(ILOpCode.Div)
        | I.Rem ->
            il.OpCode(ILOpCode.Rem)
        | I.Neg ->
            il.OpCode(ILOpCode.Neg)

        | I.Ceq ->
            il.OpCode(ILOpCode.Ceq)
        | I.Cgt ->
            il.OpCode(ILOpCode.Cgt)
        | I.Cgt_un ->
            il.OpCode(ILOpCode.Cgt_un)
        | I.Clt ->
            il.OpCode(ILOpCode.Clt)
        | I.Clt_un ->
            il.OpCode(ILOpCode.Clt_un)

        | I.Ldarg n ->
            il.LoadArgument(n)
        | I.Ldarga n ->
            il.LoadArgumentAddress(n)
        | I.LdindRef ->
            il.OpCode(ILOpCode.Ldind_ref)
        | I.Ldloc n ->
            il.LoadLocal(n)
        | I.Ldloca n ->
            il.LoadLocalAddress(n)
        | I.Starg n ->
            il.StoreArgument(n)
        | I.Stloc n ->
            il.StoreLocal n

        | I.Ldind_i4 ->
            il.OpCode(ILOpCode.Ldind_i4)
        | I.Ldind_i8 ->
            il.OpCode(ILOpCode.Ldind_i8)

        | I.Stind_i4 ->
            il.OpCode(ILOpCode.Stind_i4)
        | I.Stind_i8 ->
            il.OpCode(ILOpCode.Stind_i8)

        | I.Ldelem handle ->
            il.OpCode(ILOpCode.Ldelem)
            emitTypeToken asmBuilder &il handle
        | I.Ldelema handle ->
            il.OpCode(ILOpCode.Ldelema)
            emitTypeToken asmBuilder &il handle
        | I.Stelem handle ->
            il.OpCode(ILOpCode.Stelem)
            emitTypeToken asmBuilder &il handle

        | I.Ldftn handle ->
            il.OpCode(ILOpCode.Ldftn)
            il.Token(handle.UnsafeLazilyEvaluateEntityHandle())
        | I.Ldnull ->
            il.OpCode(ILOpCode.Ldnull)
        | I.Ldobj handle ->
            il.OpCode(ILOpCode.Ldobj)
            emitTypeToken asmBuilder &il handle

        | I.Pop ->
            il.OpCode(ILOpCode.Pop)

        | I.Dup ->
            il.OpCode(ILOpCode.Dup)

        | I.Tail ->
            il.OpCode(ILOpCode.Tail)

        | I.Call(handle, argCount) ->
            maxStack <- max maxStack argCount
            match handle with
            | ClrMethodHandle.None -> failwith "Invalid member handle."
            | ClrMethodHandle.MemberReference(memRefHandle, _, _) ->
                il.Call(memRefHandle)
            | ClrMethodHandle.LazyMethodDefinition(_, methDefHandle, _, _) ->
                il.Call(methDefHandle.Value)
            | ClrMethodHandle.MethodSpecification(methSpecHandle, _, _) ->
                il.Call(methSpecHandle.Value)
        | I.Callvirt(handle, argCount) ->
            maxStack <- max maxStack argCount
            il.OpCode(ILOpCode.Callvirt)
            match handle with
            | ClrMethodHandle.None -> failwith "Invalid member handle."
            | ClrMethodHandle.MemberReference(memRefHandle, _, _) ->
                il.Token(MemberReferenceHandle.op_Implicit memRefHandle : EntityHandle)
            | ClrMethodHandle.LazyMethodDefinition(_, methDefHandle, _, _) ->
                il.Token(MethodDefinitionHandle.op_Implicit methDefHandle.Value : EntityHandle)
            | ClrMethodHandle.MethodSpecification(methSpecHandle, _, _) ->
                il.Token(MethodSpecificationHandle.op_Implicit methSpecHandle.Value : EntityHandle)
        | I.Newobj(handle, argCount) ->
            maxStack <- max maxStack argCount
            il.OpCode(ILOpCode.Newobj)
            il.Token(handle.UnsafeLazilyEvaluateEntityHandle())
        | I.Newarr handle ->
            il.OpCode(ILOpCode.Newarr)
            emitTypeToken asmBuilder &il handle

        | I.Ldstr str ->
            il.LoadString(asmBuilder.MetadataBuilder.GetOrAddUserString(str))
        | I.LdcI4 value ->
            il.LoadConstantI4(value)
        | I.LdcI8 value ->
            il.LoadConstantI8(value)
        | I.LdcR4(value) ->
            il.LoadConstantR4(value)
        | I.LdcR8 value ->
            il.LoadConstantR8(value)

        | I.Ldfld handle ->
            il.OpCode(ILOpCode.Ldfld)
            il.Token(handle.EntityHandle)
        | I.Ldflda handle ->
            il.OpCode(ILOpCode.Ldflda)
            il.Token(handle.EntityHandle)
        | I.Stfld handle ->
            il.OpCode(ILOpCode.Stfld)
            il.Token(handle.EntityHandle)
        | I.Ldsfld handle ->
            il.OpCode(ILOpCode.Ldsfld)
            il.Token(handle.EntityHandle)
        | I.Ldsflda handle ->
            il.OpCode(ILOpCode.Ldsflda)
            il.Token(handle.EntityHandle)
        | I.Stsfld handle ->
            il.OpCode(ILOpCode.Stsfld)
            il.Token(handle.EntityHandle)

        | I.And ->
            il.OpCode(ILOpCode.And)
        | I.Or ->
            il.OpCode(ILOpCode.Or)
        | I.Xor ->
            il.OpCode(ILOpCode.Xor)
        | I.Not ->
            il.OpCode(ILOpCode.Not)
        | I.Shl ->
            il.OpCode(ILOpCode.Shl)
        | I.Shr ->
            il.OpCode(ILOpCode.Shr)
        | I.Shr_un ->
            il.OpCode(ILOpCode.Shr_un)

        | I.Beq _
        | I.Bge _
        | I.Bge_un _
        | I.Bgt _
        | I.Bgt_un _
        | I.Ble _
        | I.Ble_un _
        | I.Blt _
        | I.Blt_un _
        | I.Bne_un _
        | I.Brtrue _
        | I.Brfalse _
        | I.Br _
        | I.Label _ ->
            failwith "Unexpected branch instruction."

    static let estimateSizeOfInstr instr =
        match instr with
        | I.Conv_i ->
            1
        | I.Conv_i1 ->
            1
        | I.Conv_i2 ->
            1
        | I.Conv_i4 ->
            1
        | I.Conv_i8 ->
            1

        | I.Conv_u ->
            1
        | I.Conv_u1 ->
            1
        | I.Conv_u2 ->
            1
        | I.Conv_u4 ->
            1
        | I.Conv_u8 ->
            1

        | I.Conv_r_un ->
            1
        | I.Conv_r4 ->
            1
        | I.Conv_r8 ->
            1

        | I.Calli _ ->
            1 + 4

        | I.Ldlen ->
            1

        | I.Ldtoken _ ->
            1 + 4

        | I.Initobj(handle) ->
            match handle with
            | ClrTypeHandle.NativePointer _
            | ClrTypeHandle.FunctionPointer _ ->
                1 + // Conv_u
                1   // Ldc_i4_0
            | _ ->
                2 + 4

        | I.Constrained _ ->
            2 + 4

        | I.Throw ->
            1

        | I.StindRef ->
            1
        | I.Stobj _ ->
            1 + 4

        | I.Nop ->
            1
        | I.Ret ->
            1
        | I.Box _ ->
            1 + 4
        | I.Unbox _ ->
            1 + 4
        | I.Unbox_any _ ->
            1 + 4

        | I.Add ->
            1
        | I.Sub ->
            1
        | I.Mul ->
            1
        | I.Div ->
            1
        | I.Rem ->
            1
        | I.Neg ->
            1

        | I.Ceq ->
            2
        | I.Cgt ->
            2
        | I.Cgt_un ->
            2
        | I.Clt ->
            2
        | I.Clt_un ->
            2

        | I.Ldarg value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                if value >= 0 && value <= 3 then
                    1
                else
                    1 + 1
            else
                1 + 2
        | I.Ldarga value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                1 + 1
            else
                1 + 2
        | I.LdindRef ->
            1
        | I.Ldloc value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                if value >= 0 && value <= 3 then
                    1
                else
                    1 + 1
            else
                1 + 2
        | I.Ldloca value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                1 + 1
            else
                1 + 2
        | I.Starg value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                1 + 1
            else
                1 + 2
        | I.Stloc value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                if value >= 0 && value <= 3 then
                    1
                else
                    1 + 1
            else
                1 + 2

        | I.Ldind_i4 ->
            1
        | I.Ldind_i8 ->
            1

        | I.Stind_i4 ->
            1
        | I.Stind_i8 ->
            1

        | I.Ldelem _ ->
            1 + 4
        | I.Ldelema _ ->
            1 + 4
        | I.Stelem _ ->
            1 + 4

        | I.Ldftn _ ->
            2 + 4
        | I.Ldnull ->
            1
        | I.Ldobj _ ->
            1 + 4

        | I.Pop ->
            1

        | I.Dup ->
            1

        | I.Tail ->
            2

        | I.Call _ ->
            1 + 4
        | I.Callvirt _ ->
            1 + 4
        | I.Newobj _ ->
            1 + 4
        | I.Newarr _ ->
            1 + 4

        | I.Ldstr _ ->
            1 + 4
        | I.LdcI4(value) ->
            if value <= int SByte.MaxValue && value >= int SByte.MinValue then
                if value >= 0 && value <= 8 then
                    1
                elif value = -1 then
                    1 // ldc.i4.m1
                else
                    1 + 1
            else
                1 + 4
        | I.LdcI8 _ ->
            1 + 8
        | I.LdcR4 _ ->
            1 + 4
        | I.LdcR8 _ ->
            1 + 8

        | I.Ldfld _ ->
            1 + 4
        | I.Ldflda _ ->
            1 + 4
        | I.Stfld _ ->
            1 + 4
        | I.Ldsfld _ ->
            1 + 4
        | I.Ldsflda _ ->
            1 + 4
        | I.Stsfld _ ->
            1 + 4

        | I.And ->
            1
        | I.Or ->
            1
        | I.Xor ->
            1
        | I.Not ->
            1
        | I.Shl ->
            1
        | I.Shr ->
            1
        | I.Shr_un ->
            1

        | I.Beq _
        | I.Bge _
        | I.Bge_un _
        | I.Bgt _
        | I.Bgt_un _
        | I.Ble _
        | I.Ble_un _
        | I.Blt _
        | I.Blt_un _
        | I.Bne_un _
        | I.Brtrue _
        | I.Brfalse _
        | I.Br _ ->
            1 + 4
        | I.Label _ ->
            0

    static let createInstructionEncoder() =
        InstructionEncoder(BlobBuilder(), ControlFlowBuilder())

    static let addLabel (il: byref<InstructionEncoder>) (labels: Dictionary<int, _>) labelId =
        labels.Add(labelId, il.DefineLabel())

    member _.Name = name
    member _.IsInstance = isInstance
    member val Locals: ClrLocal imarray = ImArray.empty with get, set
    member val Convention = SignatureCallingConvention.Default with get, set
    member val Attributes = MethodAttributes() with get, set
    member val ImplementationAttributes = MethodImplAttributes() with get, set
    member val BodyInstructions = ImArray.empty with get, set

    member _.Overrides
        with get() = overridesOpt
        and set value = overridesOpt <- value

    member _.PInvokeInfo
        with get() = pinvokeInfoOpt
        and set value = pinvokeInfoOpt <- value

    member _.ParameterTypes =
        parTys

    member _.Handle: ClrMethodHandle = handle

    member private this.PopulateMethodBody() =
        if this.BodyInstructions.IsEmpty then -1 // no body - this makes the RVA zero
        else

        let instrs = this.BodyInstructions

        //---------------------------------------------------------

#if DEBUG
        let labels = Dictionary<int, _>()

        let mutable dummyIL = createInstructionEncoder()

        for i = 0 to instrs.Length - 1 do
            let instr = instrs[i]

            match instr with
            | I.Label labeId ->
                addLabel &dummyIL labels labeId
            | _ ->
                ()

        let mutable maxStack = 8 // TODO: We could optimize this.

        for i = 0 to instrs.Length - 1 do
            let instr = instrs[i]

            match instr with
            | I.Beq labelId
            | I.Bge labelId
            | I.Bge_un labelId
            | I.Bgt labelId
            | I.Bgt_un labelId
            | I.Ble labelId
            | I.Ble_un labelId
            | I.Blt labelId
            | I.Ble_un labelId
            | I.Bne_un labelId
            | I.Brtrue labelId
            | I.Brfalse labelId
            | I.Br labelId ->
                dummyIL.Branch(getBranchOpCode instr, labels[labelId])

            | I.Label labelId ->
                dummyIL.MarkLabel(labels[labelId])

            | _ ->
                emitInstr asmBuilder &maxStack &dummyIL instr
#endif

        let labels = Dictionary<int, _>()
        let labelOffsets = Dictionary<int, _>()
        let offsets = Array.zeroCreate<int> instrs.Length

        let mutable il = createInstructionEncoder()
        let mutable totalSize = 0
        for i = 0 to instrs.Length - 1 do
            let instr = instrs[i]

            offsets[i] <- totalSize

            match instr with
            | I.Label(labelId) ->
                labelOffsets[labelId] <- totalSize
                addLabel &il labels labelId
            | _ ->
                ()

            totalSize <- totalSize + estimateSizeOfInstr instr

#if DEBUG
        // Assert estimation size
        // In Debug, we emit the IL instructions twice.
        OlyAssert.Equal(dummyIL.Offset, totalSize)
#endif

        let mutable maxStack = 8 // TODO: We could optimize this.

        for i = 0 to instrs.Length - 1 do
            let instr = instrs[i]

            // Handling branches to determine the use of the short form is a little conservative as it will not take into account
            //     other branches that are short form.
            match instr with
            | I.Beq labelId
            | I.Bge labelId
            | I.Bge_un labelId
            | I.Bgt labelId
            | I.Bgt_un labelId
            | I.Ble labelId
            | I.Ble_un labelId
            | I.Blt labelId
            | I.Ble_un labelId
            | I.Bne_un labelId
            | I.Brtrue labelId
            | I.Brfalse labelId
            | I.Br labelId ->
                let opCode = getBranchOpCode instr
                let opCode =
                    let offset = offsets[i] + estimateSizeOfInstr instr
                    let labelOffset = labelOffsets[labelId]
                    let distance = labelOffset - offset
                    if distance >= int SByte.MinValue && distance <= int SByte.MaxValue then
                        getShortBranchOpCode opCode
                    else
                        opCode

                il.Branch(opCode, labels[labelId])

            | I.Label labelId ->
                il.MarkLabel(labels[labelId])

            | _ ->
                emitInstr asmBuilder &maxStack &il instr

        //---------------------------------------------------------


        let ilBuilder = asmBuilder.ILBuilder
        ilBuilder.Align(4)
        let mutable methBodyStream = MethodBodyStreamEncoder(ilBuilder)

        let bodyOffset =
            if this.Locals.IsEmpty then
                methBodyStream.AddMethodBody(il, maxStack = maxStack)
            else
                methBodyStream.AddMethodBody(il, maxStack = maxStack, localVariablesSignature = asmBuilder.CreateStandaloneSignature(this.Locals))

        bodyOffset

    member internal this.Build() =
        getInlineCache &cachedHandle (fun () ->
            let metadataBuilder = asmBuilder.MetadataBuilder

            let newPars = ResizeArray()
            for i = 0 to pars.Length - 1 do
                let nameHandle =  
                    let name = fst pars.[i]
                    if String.IsNullOrWhiteSpace name then
                        StringHandle()
                    else
                        metadataBuilder.GetOrAddString(name)
                metadataBuilder.AddParameter(ParameterAttributes(), nameHandle, i + 1)
                |> newPars.Add

            let parList =
                if newPars.Count = 0 then
                    MetadataTokens.ParameterHandle(metadataBuilder.GetRowCount(TableIndex.Param) + 1)
                else
                    newPars.[0]

            let nameHandle = metadataBuilder.GetOrAddString(name)
           
            let handle =
                metadataBuilder.AddMethodDefinition(
                    this.Attributes,
                    this.ImplementationAttributes,
                    nameHandle,
                    asmBuilder.CreateMethodSignature(this.Convention, tyPars.Length, this.IsInstance, parTys, returnTy),
                    this.PopulateMethodBody(),
                    parList)

            if this.Attributes &&& MethodAttributes.PinvokeImpl = MethodAttributes.PinvokeImpl then
                match pinvokeInfoOpt with
                | Some (pinvokeDllName, pinvokeName) ->                  
                    metadataBuilder.AddMethodImport(
                        handle, 
                        MethodImportAttributes.CallingConventionWinApi, 
                        metadataBuilder.GetOrAddString(pinvokeName),
                        metadataBuilder.AddModuleReference(metadataBuilder.GetOrAddString(pinvokeDllName))
                    )
                    |> ignore
                | _ ->
                    ()

            let castedHandle = MethodDefinitionHandle.op_Implicit handle : EntityHandle
            let index = CodedIndex.TypeOrMethodDef(castedHandle)

            let f = fun () ->
                for i = 0 to tyPars.Length - 1 do
                    let name = metadataBuilder.GetOrAddString(tyPars.[i])
                    metadataBuilder.AddGenericParameter(castedHandle, GenericParameterAttributes.None, name, enclosingTyParCount + i)
                    |> ignore
            if not tyPars.IsEmpty then
                asmBuilder.GenericParameterQueue.Enqueue(f, index)

            handle
        )

[<Sealed>]
type ClrPropertyDefinitionBuilder internal (asmBuilder: ClrAssemblyBuilder, name: string, pars: (string * _) imarray, returnTy: ClrTypeHandle, isInstance: bool) =

    let metadataBuilder = asmBuilder.MetadataBuilder
    let parTys = pars |> ImArray.map snd
    let propSig = asmBuilder.CreatePropertySignature(isInstance, parTys, returnTy)
    let nameHandle = metadataBuilder.GetOrAddString(name)

    let handle =
        lazy
            let result = metadataBuilder.AddProperty(PropertyAttributes.None, metadataBuilder.GetOrAddString(name), propSig)
            asmBuilder.SetPropertyDefinitionRowCount(asmBuilder.PropertyDefinitionRowCount + 1)
            result

    member _.Signature = propSig
    member _.NameHandle = nameHandle

    member internal _.Build() = handle.Value

[<Sealed>]
type ClrTypeDefinitionBuilder internal (asmBuilder: ClrAssemblyBuilder, enclosingTyHandle: ClrTypeHandle, namespac: string, name: string, tyPars: string imarray, isValueType: bool, isEnum: bool) as this =

    let fullyQualifiedName =
        if enclosingTyHandle.IsNamed then
            enclosingTyHandle.FullyQualifiedName + "+" + name
        else
            let asmName =
                match enclosingTyHandle with
                | ClrTypeHandle.AssemblyReference(_, qualifiedName) -> ", " + qualifiedName
                | _ -> ""
            if String.IsNullOrWhiteSpace namespac then
                name + asmName
            else
                namespac + "." + name + asmName

    let mutable cachedHandle = ValueNone

    let handle =
        asmBuilder.CreateTypeDefinitionHandle(lazy this.Build(), isValueType || isEnum, fullyQualifiedName)

    let methDefs = ResizeArray()
    let fieldDefs = ResizeArray<string * ClrFieldHandle>()
    let propDefs = ResizeArray()

    member _.FullyQualifiedName = fullyQualifiedName

    member val Attributes = TypeAttributes() with get, set

    member val BaseType = (if isValueType then asmBuilder.TypeReferenceValueType elif isEnum then asmBuilder.TypeReferenceEnum else asmBuilder.TypeReferenceObject) with get, set
    member val InterfaceImplementations: ClrTypeHandle imarray = ImArray.empty with get, set

    member _.FindField(name: string) = fieldDefs |> Seq.find (fun (x, _) -> x = name) |> snd

    member this.CreateMethodDefinitionBuilder(name: string, methTyPars, pars, returnTy: ClrTypeHandle, isInstance: bool) =
        let methDefBuilder = ClrMethodDefinitionBuilder(asmBuilder, tyPars.Length, name, methTyPars, pars, returnTy, isInstance)
        methDefs.Add(methDefBuilder)
        methDefBuilder

    member this.AddFieldDefinition(attrs, name: string, fieldTy: ClrTypeHandle, constValueOpt: obj option) =
        let metadataBuilder = asmBuilder.MetadataBuilder
        let nameHandle = metadataBuilder.GetOrAddString(name)
        let signature = asmBuilder.CreateFieldSignature(fieldTy)
        let handle =
            lazy
                let fieldHandle =
                    metadataBuilder.AddFieldDefinition(
                        attrs,
                        nameHandle,
                        signature
                    )
                match constValueOpt with
                | Some(constValue) ->
                    metadataBuilder.AddConstant(FieldDefinitionHandle.op_Implicit(fieldHandle), constValue)
                    |> ignore
                | _ ->
                    ()
                fieldHandle
        let handle = asmBuilder.CreateFieldDefinitionHandle(handle, nameHandle, signature)
        fieldDefs.Add(name, handle)
        handle

    member this.TryGetSingleByRefConstructor() =
        methDefs
        |> Seq.filter (fun x ->
            x.Name = ".ctor" && x.ParameterTypes.Length = 1 && x.ParameterTypes.[0].IsByRef_t
        )
        |> Seq.tryExactlyOne

    member this.TryGetSingleNonByRefConstructor() =
        methDefs
        |> Seq.filter (fun x ->
            x.Name = ".ctor" && x.ParameterTypes.Length = 1 && not x.ParameterTypes.[0].IsByRef_t
        )
        |> Seq.tryExactlyOne

    member _.Handle: ClrTypeHandle = handle
    
    member internal this.Build() =
        getInlineCache &cachedHandle (fun () ->
            let metadataBuilder = asmBuilder.MetadataBuilder
            let fieldList =
                if fieldDefs.Count = 0 then
                    MetadataTokens.FieldDefinitionHandle(asmBuilder.FieldDefinitionRowCount + 1)
                else
                    let _, handle = fieldDefs.[0]
                    match handle with
                    | ClrFieldHandle.LazyFieldDefinition(realHandle, fakeHandle, _, _) ->
                        if realHandle.IsValueCreated then
                            failwith "Handle already evaluated."
                        fakeHandle.Value
                    | _ ->
                        failwith "Expected a field definition."

            for i = 1 to fieldDefs.Count - 1 do
                let _, handle = fieldDefs.[i]
                handle.EntityHandle |> ignore

            let methList =
                if methDefs.Count = 0 then
                    MetadataTokens.MethodDefinitionHandle(asmBuilder.MethodDefinitionRowCount + 1)
                else
                    let handle = methDefs.[0].Handle 
                    match handle with
                    | ClrMethodHandle.LazyMethodDefinition(realHandle, fakeHandle, _, _) ->
                        if realHandle.IsValueCreated then
                            failwith "Handle already evaluated."
                        fakeHandle.Value
                    | _ ->
                        failwith "Invalid handle."

            for i = 1 to methDefs.Count - 1 do
                methDefs.[i].Handle.UnsafeLazilyEvaluateEntityHandle() |> ignore

            let tyDefHandle =
                metadataBuilder.AddTypeDefinition(
                    this.Attributes,
                    (if String.IsNullOrWhiteSpace namespac then StringHandle() else metadataBuilder.GetOrAddString(namespac)), 
                    metadataBuilder.GetOrAddString(name), 
                    this.BaseType.EntityHandle, 
                    fieldList, 
                    methList)

            let castedTyDefHandle = TypeDefinitionHandle.op_Implicit tyDefHandle : EntityHandle
            let tyDefIndex = CodedIndex.TypeOrMethodDef(castedTyDefHandle)

            let f = fun () ->
                for i = 0 to tyPars.Length - 1 do
                    let name = metadataBuilder.GetOrAddString(tyPars.[i])
                    metadataBuilder.AddGenericParameter(castedTyDefHandle, GenericParameterAttributes.None, name, i)
                    |> ignore
            if not tyPars.IsEmpty then
                asmBuilder.GenericParameterQueue.Enqueue(f, tyDefIndex)

            match enclosingTyHandle with
            | ClrTypeHandle.LazyTypeDefinition(_, _, fakeEnclosingTyDefHandle, _) ->
                metadataBuilder.AddNestedType(tyDefHandle, fakeEnclosingTyDefHandle.Value)
            | ClrTypeHandle.None ->
                ()
            | _ ->
                failwith "Invalid enclosing type handle."

            this.InterfaceImplementations
            |> ImArray.iter (fun impl ->
                metadataBuilder.AddInterfaceImplementation(tyDefHandle, impl.EntityHandle) |> ignore
            )

            for i = 0 to methDefs.Count - 1 do
                let methDef = methDefs.[i]
                match methDef.Overrides with
                | Some overrides ->
                    let methDefHandle = methDef.Handle
                    asmBuilder.EnqueueOverride(fun () ->
                        metadataBuilder.AddMethodImplementation(tyDefHandle, methDefHandle.UnsafeLazilyEvaluateEntityHandle(), overrides.UnsafeLazilyEvaluateEntityHandle())
                        |> ignore
                    )                 
                | _ ->
                    ()

            //let propList =
            //    if propDefs.Count = 0 then
            //        MetadataTokens.PropertyDefinitionHandle(asmBuilder.PropertyDefinitionRowCount + 1)
            //    else
            //        propDefs.[0].Build()

            //for i = 1 to propDefs.Count - 1 do
            //    propDefs.[i].Build() |> ignore
                
            //if propDefs.Count > 0 then
            //    metadataBuilder.AddPropertyMap(tyDefHandle, propList)

            tyDefHandle
        )
        