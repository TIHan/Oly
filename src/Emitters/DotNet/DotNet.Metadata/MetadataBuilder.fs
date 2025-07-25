﻿namespace rec DotNet.Metadata

open System
open System.Reflection
open System.Diagnostics
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Collections.Immutable
open System.Collections.Generic
open System.Security.Cryptography
open Oly.Core
open ClrPatterns

[<NoEquality;NoComparison>]
type private DebugLocalScope =
    {
        Locals: ClrDebugLocal imarray
        StartOffset: int
        Length: int
    }

[<RequireQualifiedAccess>]
module private MetadataHelpers =

    let addAssembly assemblyName (metadataBuilder: MetadataBuilder) =
        metadataBuilder.AddAssembly(
            metadataBuilder.GetOrAddString(assemblyName), 
            Version(0, 0, 0, 0), 
            StringHandle(), 
            BlobHandle(), 
            AssemblyFlags.PublicKey,
            AssemblyHashAlgorithm.Sha256)

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

    let encodeType (encoder: SignatureTypeEncoder, handle: ClrTypeHandle, asmBuilder: ClrAssemblyBuilder) =
        match handle with
        | ClrTypeHandle.TypeSpecification(_, isValueType, tyRefHandle, tyInst) ->
            let mutable encoderInst = encoder.GenericInstantiation(tyRefHandle.EntityHandle, tyInst.Length, isValueType)
            tyInst
            |> ImArray.iter (fun ty ->
                encodeType(encoderInst.AddArgument(), ty, asmBuilder)
            )
        | ClrTypeHandle.Array(elementTyHandle, rank) ->
            if rank < 1 then failwith "Invalid rank."
            if rank = 1 then
                let mutable elementEncoder = encoder.SZArray()
                encodeType(elementEncoder, elementTyHandle, asmBuilder)
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
            encodeType(encoder, ty, asmBuilder)
        | ClrTypeHandle.ModReq(modifier, ty) ->
            encoder.CustomModifiers().AddModifier(modifier.EntityHandle, false)
            |> ignore
            encodeType(encoder, ty, asmBuilder)
        | ClrTypeHandle.FunctionPointer(cc, parTys, returnTy) ->
            let mutable parEncoder = Unchecked.defaultof<_>
            let mutable returnTyEncoder = Unchecked.defaultof<_>
            encoder.FunctionPointer(convention = cc).Parameters(parTys.Length, &returnTyEncoder, &parEncoder)
            encodeReturnType(returnTyEncoder, returnTy, asmBuilder)
            encodeParameters(parEncoder, parTys, asmBuilder)
        | ClrTypeHandle.NativePointer(elementTy) ->
            if elementTy = asmBuilder.TypeReferenceVoid then
                encoder.VoidPointer()
            else
                encodeType(encoder.Pointer(), elementTy, asmBuilder)

        | _ ->

        let entityHandle = handle.EntityHandle

        if entityHandle.Equals(asmBuilder.TypeReferenceString.EntityHandle) then
            encoder.String()
        elif entityHandle.Equals(asmBuilder.TypeReferenceObject.EntityHandle) then
            encoder.Object()
        elif entityHandle.Equals(asmBuilder.TypeReferenceBoolean.EntityHandle) then
            encoder.Boolean()
        elif entityHandle.Equals(asmBuilder.TypeReferenceByte.EntityHandle) then
            encoder.Byte()
        elif entityHandle.Equals(asmBuilder.TypeReferenceSByte.EntityHandle) then
            encoder.SByte()
        elif entityHandle.Equals(asmBuilder.TypeReferenceInt16.EntityHandle) then
            encoder.Int16()
        elif entityHandle.Equals(asmBuilder.TypeReferenceUInt16.EntityHandle) then
            encoder.UInt16()
        elif entityHandle.Equals(asmBuilder.TypeReferenceInt32.EntityHandle) then
            encoder.Int32()
        elif entityHandle.Equals(asmBuilder.TypeReferenceUInt32.EntityHandle) then
            encoder.UInt32()
        elif entityHandle.Equals(asmBuilder.TypeReferenceInt64.EntityHandle) then
            encoder.Int64()
        elif entityHandle.Equals(asmBuilder.TypeReferenceUInt64.EntityHandle) then
            encoder.UInt64()
        elif entityHandle.Equals(asmBuilder.TypeReferenceSingle.EntityHandle) then
            encoder.Single()
        elif entityHandle.Equals(asmBuilder.TypeReferenceDouble.EntityHandle) then
            encoder.Double()
        elif entityHandle.Equals(asmBuilder.TypeReferenceChar.EntityHandle) then
            encoder.Char()
        elif entityHandle.Equals(asmBuilder.TypeReferenceIntPtr.EntityHandle) then
            encoder.IntPtr()
        elif entityHandle.Equals(asmBuilder.TypeReferenceUIntPtr.EntityHandle) then
            encoder.UIntPtr()
        else
            encoder.Type(handle.EntityHandle, handle.IsValueType)

    let encodeFieldType (encoder: FieldTypeEncoder, handle: ClrTypeHandle, asmBuilder: ClrAssemblyBuilder) =
        match handle with
        | ClrTypeHandle.ByRef(ty) ->
            encodeType(encoder.Type(true), ty, asmBuilder)
        | _ ->
            encodeType(encoder.Type(false), handle, asmBuilder)

    let encodeReturnType (encoder: ReturnTypeEncoder, returnTy: ClrTypeHandle, asmBuilder: ClrAssemblyBuilder) =
        if returnTy.HasEntityHandle then
            if returnTy.EntityHandle.Equals(asmBuilder.TypeReferenceVoid.EntityHandle) then
                encoder.Void()
            else
                match returnTy.TryElementType with
                | ValueSome elementTy when returnTy.IsByRef_t ->
                    match returnTy with
                    | ClrTypeHandle.ModReq(modifierTy, ty) ->
                        encoder.CustomModifiers().AddModifier(modifierTy.EntityHandle, false) |> ignore
                        let mutable encoder = encoder.Type(true)
                        encodeType(encoder, ty, asmBuilder)
                    | _ ->
                        encodeType(encoder.Type(true), elementTy, asmBuilder)
                | _ ->
                    encodeType(encoder.Type(false), returnTy, asmBuilder)
        else
            match returnTy.TryElementType with
            | ValueSome elementTy when returnTy.IsByRef_t ->
                match returnTy with
                | ClrTypeHandle.ModReq(modifierTy, ty) ->
                    encoder.CustomModifiers().AddModifier(modifierTy.EntityHandle, false) |> ignore
                    let mutable encoder = encoder.Type(true)
                    encodeType(encoder, ty, asmBuilder)
                | _ ->
                    encodeType(encoder.Type(true), elementTy, asmBuilder)
            | _ ->
                encodeType(encoder.Type(false), returnTy, asmBuilder)

    let encodeParameters (encoder: ParametersEncoder, parTys: ClrTypeHandle imarray, asmBuilder: ClrAssemblyBuilder) =
        parTys
        |> ImArray.iter (fun parTy ->
            match parTy.TryElementType with
            | ValueSome elementTy when parTy.IsByRef_t ->
                match parTy with
                | ClrTypeHandle.ModReq(modifierTy, ty) ->
                    let mutable parEncoder = encoder.AddParameter()
                    parEncoder.CustomModifiers().AddModifier(modifierTy.EntityHandle, false) |> ignore
                    let mutable parTyEncoder = parEncoder.Type(true)
                    encodeType(parTyEncoder, ty, asmBuilder)
                | _ ->
                    encodeType(encoder.AddParameter().Type(true), elementTy, asmBuilder)
            | _ ->
                encodeType(encoder.AddParameter().Type(false), parTy, asmBuilder)
        )

    let buildMethodSignature(blobBuilder: BlobBuilder, convention, tyParCount: int, isInstance, parTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle, asmBuilder: ClrAssemblyBuilder) =
        BlobEncoder(blobBuilder).MethodSignature(convention, tyParCount, isInstance).Parameters(
            parTys.Length,
            (fun encoder ->
                encodeReturnType(encoder, returnTy, asmBuilder)),
            (fun encoder -> 
                encodeParameters(encoder, parTys, asmBuilder))
        )

    let addConstraints (asmBuilder: ClrAssemblyBuilder) (handle: GenericParameterHandle) (constrs: ClrTypeConstraint imarray) =
        constrs
        |> ImArray.iter (fun constr ->
            match constr with
            | ClrTypeConstraint.SubtypeOf(tyHandle) ->
                let entHandle =
                    match tyHandle with
                    | ClrTypeHandle.ModReq(modifierHandle, tyHandle) ->
                        asmBuilder.AddModReq(modifierHandle, tyHandle)
                    | _ ->
                        tyHandle.EntityHandle
                asmBuilder.MetadataBuilder.AddGenericParameterConstraint(handle, entHandle)
                |> ignore
        )

[<Sealed>]
type ClrLocal(name: string, ty: ClrTypeHandle, isPinned: bool) =

    member _.Name = name
    member _.Type = ty
    member _.IsPinned = isPinned

/// The specified Hash algorithm to use on portable pdb files.
type private HashAlgorithm =
    | Sha1
    | Sha256

[<Sealed>]
type ClrSequencePoint (document: DocumentHandle, offset: int, line: int, endLine: int, column: int, endColumn: int) =

    member _.Document = document

    member val Offset = offset with get, set

    member _.Line = line

    member _.EndLine = endLine

    member _.Column = column

    member _.EndColumn = endColumn

[<Sealed>]
type ClrPdbBuilder() =
    let metadata = MetadataBuilder()

    // Document checksum algorithms
    let guidSha1 = Guid("ff1816ec-aa5e-4d10-87f7-6f4963833460")
    let guidSha2 = Guid("8829d00f-11b8-4213-878b-770e8597ac16")

    let corSymLanguageTypeId =
        Guid(0xAB4F38C9u, 0xB6E6us, 0x43baus, 0xBEuy, 0x3Buy, 0x58uy, 0x08uy, 0x0Buy, 0x2Cuy, 0xCCuy, 0xE3uy)

    let embeddedSourceId =
        Guid(0x0e8a571bu, 0x6926us, 0x466eus, 0xb4uy, 0xaduy, 0x8auy, 0xb0uy, 0x46uy, 0x11uy, 0xf5uy, 0xfeuy)

    let sourceLinkId =
        Guid(0xcc110556u, 0xa091us, 0x4d38us, 0x9fuy, 0xecuy, 0x25uy, 0xabuy, 0x9auy, 0x35uy, 0x1auy, 0x6auy)

    let checkSum (url: string) (checksumAlgorithm: HashAlgorithm) =
        try
            use file = System.IO.File.OpenRead(url)

            let guid, alg =
                match checksumAlgorithm with
                | HashAlgorithm.Sha1 -> guidSha1, SHA1.Create() :> System.Security.Cryptography.HashAlgorithm
                | HashAlgorithm.Sha256 -> guidSha2, SHA256.Create() :> System.Security.Cryptography.HashAlgorithm

            let checkSum = alg.ComputeHash file
            Some(guid, checkSum)
        with _ ->
            None

    let serializeDocumentName (name: string) =
       // let name = PathMap.apply pathMap name

        let count s c =
            s |> Seq.filter (fun ch -> c = ch) |> Seq.length

        let s1, s2 = '/', '\\'

        let separator = if (count name s1) >= (count name s2) then s1 else s2

        let writer = BlobBuilder()
        writer.WriteByte(byte separator)

        for part in name.Split([| separator |]) do
            let partIndex =
                MetadataTokens.GetHeapOffset(BlobHandle.op_Implicit (metadata.GetOrAddBlobUTF8 part))

            writer.WriteCompressedInteger(int partIndex)

        metadata.GetOrAddBlob writer

    let docs = Dictionary()

    member internal _.Internal = metadata

    member _.GetOrAddDocument(name: string) =
        match docs.TryGetValue name with
        | true, handle -> handle
        | _ ->
            let (guid, blob) = Guid.Empty, [||]
                //match checkSum name HashAlgorithm.Sha256 with
                //| Some(guid, blob) -> guid, blob
                //| _ -> Guid.Empty, [||]
            let handle =
                metadata.AddDocument(
                    serializeDocumentName name,
                    metadata.GetOrAddGuid(guid),
                    metadata.GetOrAddBlob(blob),
                    metadata.GetOrAddGuid corSymLanguageTypeId
                )
            docs.Add(name, handle)
            handle

    member private _.BuildSequencePoints(localSigToken, sps: ClrSequencePoint imarray, hasMultipleDocuments: bool) =
        let builder = BlobBuilder()
        builder.WriteCompressedInteger(localSigToken)

        if sps.Length = 0 then
            builder.WriteCompressedInteger(0)
            builder.WriteCompressedInteger(0)
            Unchecked.defaultof<BlobHandle>
        else

            let mutable currentDocument = sps[0].Document
            if hasMultipleDocuments then
                builder.WriteCompressedInteger(
                    MetadataTokens.GetRowNumber(DocumentHandle.op_Implicit (currentDocument))
                )

            let mutable previousNonHiddenStartLine = -1
            let mutable previousNonHiddenStartColumn = 0

            for i in 0 .. (sps.Length - 1) do

                if hasMultipleDocuments && not sps[i].Document.IsNil && sps[i].Document <> currentDocument then
                    currentDocument <- sps[i].Document

                    builder.WriteCompressedInteger(0)

                    builder.WriteCompressedInteger(
                        MetadataTokens.GetRowNumber(DocumentHandle.op_Implicit (currentDocument))
                    )
             //   else

                //=============================================================================================================================================
                // Sequence-point-record
                // Validate these with magic numbers according to the portable pdb spec Sequence point dexcription:
                // https://github.com/dotnet/corefx/blob/master/src/System.Reflection.Metadata/specs/PortablePdb-Metadata.md#methoddebuginformation-table-0x31
                //
                // So the spec is actually bit iffy!!!!! (More like guidelines really.  )
                //  It uses code similar to this to validate the values
                //    if (result < 0 || result >= ushort.MaxValue)  // be errorfull
                // Spec Says 0x10000 and value max = 0xFFFF but it can't even be = to maxvalue, and so the range is 0 .. 0xfffe inclusive
                //=============================================================================================================================================

                let capValue v maxValue =
                    if v < 0 then 0
                    elif v > maxValue then maxValue
                    else v

                let capOffset v = capValue v 0xfffe
                let capLine v = capValue v 0x1ffffffe
                let capColumn v = capValue v 0xfffe

                let offset = capOffset sps[i].Offset
                let startLine = capLine sps[i].Line
                let endLine = capLine sps[i].EndLine
                let startColumn = capColumn sps[i].Column
                let endColumn = capColumn sps[i].EndColumn

                let offsetDelta = // delta from previous offset
                    if i > 0 then
                        offset - capOffset sps[i - 1].Offset
                    else
                        offset

                if i < 1 || offsetDelta > 0 then
                    builder.WriteCompressedInteger offsetDelta

                    // Check for hidden-sequence-point-record
                    if
                        startLine = 0xfeefee
                        || endLine = 0xfeefee
                        || (startColumn = 0 && endColumn = 0)
                        || ((endLine - startLine) = 0 && (endColumn - startColumn) = 0)
                    then
                        // Hidden-sequence-point-record
                        builder.WriteCompressedInteger 0
                        builder.WriteCompressedInteger 0
                    else
                        // Non-hidden-sequence-point-record
                        let deltaLines = endLine - startLine // lines
                        builder.WriteCompressedInteger deltaLines

                        let deltaColumns = endColumn - startColumn // Columns

                        if deltaLines = 0 then
                            builder.WriteCompressedInteger deltaColumns
                        else
                            builder.WriteCompressedSignedInteger deltaColumns

                        if previousNonHiddenStartLine < 0 then // delta Start Line & Column:
                            builder.WriteCompressedInteger startLine
                            builder.WriteCompressedInteger startColumn
                        else
                            builder.WriteCompressedSignedInteger(startLine - previousNonHiddenStartLine)
                            builder.WriteCompressedSignedInteger(startColumn - previousNonHiddenStartColumn)

                        previousNonHiddenStartLine <- startLine
                        previousNonHiddenStartColumn <- startColumn

            metadata.GetOrAddBlob builder

    member this.AddMethodDebugInformation(document, localSigToken, sequencePoints) =
        metadata.AddMethodDebugInformation(document, this.BuildSequencePoints(localSigToken, sequencePoints, false))

    member this.AddMethodDebugInformation(localSigToken, sequencePoints) =
        metadata.AddMethodDebugInformation(Unchecked.defaultof<_>, this.BuildSequencePoints(localSigToken, sequencePoints, true))

    member this.AddEmptyMethodDebugInformation(localSigToken) =
        metadata.AddMethodDebugInformation(Unchecked.defaultof<_>, this.BuildSequencePoints(localSigToken, ImArray.empty, false))

[<Sealed>]
type ClrAssemblyBuilder(assemblyName: string, isExe: bool, primaryAssembly: AssemblyName, consoleAssembly: AssemblyName) =

    let tyDefsByQualifiedName = Dictionary<string, obj>()
    let ilBuilder = BlobBuilder()

    let metadataBuilder = MetadataBuilder()
    let pdbBuilder = ClrPdbBuilder()

    let asmDefHandle = MetadataHelpers.addAssembly assemblyName metadataBuilder

    let typeCache_ModReq = System.Collections.Generic.Dictionary<struct (EntityHandle * EntityHandle), EntityHandle>()

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

    let mutable tyDefRowCount = 0
    let mutable fieldDefRowCount = 0
    let mutable methDefRowCount = 0
    let mutable propDefRowCount = 0

    let mutable nextTyDefRowId = 1
    let tyDefQueue = System.Collections.Generic.Queue<unit -> unit>()

    let mutable nextFieldDefRowId = 1
    let fieldDefQueue = System.Collections.Generic.Queue()

    let mutable nextMethDefRowId = 1
    let methDefQueue = System.Collections.Generic.Queue()
    
    let overrideQueue = System.Collections.Generic.Queue()
    let attrQueue = System.Collections.Generic.Queue()

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
        let fakeHandle = MetadataTokens.TypeDefinitionHandle(getNextTyDefRowId())
        tyDefQueue.Enqueue(fun () -> 
            let realHandle = realWork.Value
            if not (fakeHandle.Equals(realHandle)) then
                failwith "Invalid type definition handle."
            )
        let res = 
            ClrTypeHandle.LazyTypeDefinition(
                realWork, 
                isValueType, 
                fakeHandle,
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
            let handle = MetadataHelpers.addAssemblyReference asmName metadataBuilder
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

    let addDebuggableTyRef () =
        let asmRef = createAsmRef (primaryAssembly)
        metadataBuilder.AddTypeReference(
            AssemblyReferenceHandle.op_Implicit asmRef,
            metadataBuilder.GetOrAddString("System.Diagnostics"),
            metadataBuilder.GetOrAddString("DebuggableAttribute")
        )

    let addDebuggingModesTyRef () =
        let asmRef = createAsmRef (primaryAssembly)
        metadataBuilder.AddTypeReference(
            TypeReferenceHandle.op_Implicit(addDebuggableTyRef()),
            Unchecked.defaultof<_>,
            metadataBuilder.GetOrAddString("DebuggingModes")
        )

    let addDebuggerStepperBoundaryAttributeTyRef () =
        let asmRef = createAsmRef (primaryAssembly)
        metadataBuilder.AddTypeReference(
            AssemblyReferenceHandle.op_Implicit asmRef,
            metadataBuilder.GetOrAddString("System.Diagnostics"),
            metadataBuilder.GetOrAddString("DebuggerStepperBoundaryAttribute")
        )

    let sysTy name isValueType =
        createTyRef ("System." + name) isValueType (addBuiltInTyRef "System" name)

    let sysTyObject = sysTy "Object" false

    let consoleSysTy name isValueType =
        createTyRef ("System." + name) isValueType (addConsoleTyRef "System" name)

    let mvid = Guid.NewGuid() // TODO: This does not work for determinism, fix it.
    let moduleDefHandle = MetadataHelpers.addModule assemblyName isExe mvid metadataBuilder

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

    let createDebuggableConstructor() =
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = true)
        encoder.Parameters(
            1,
            (fun encoder -> encoder.Void()),
            (fun encoder -> encoder.AddParameter().Type().Type(addDebuggingModesTyRef(), true))
        )

        let name = metadataBuilder.GetOrAddString(".ctor")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                addDebuggableTyRef(),
                name,
                signature
            )

        createMemRef realHandle name signature

    let createDebuggerStepperBoundaryAttributeConstructor() =
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = true)
        encoder.Parameters(
            0,
            (fun encoder -> encoder.Void()),
            (fun _ -> ())
        )

        let name = metadataBuilder.GetOrAddString(".ctor")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                addDebuggerStepperBoundaryAttributeTyRef(),
                name,
                signature
            )

        createMemRef realHandle name signature

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

    let signatureBuilder = BlobBuilder()

    member internal _.MetadataBuilder: MetadataBuilder = metadataBuilder
    member internal _.ILBuilder = ilBuilder

    member _.PdbBuilder = pdbBuilder

    member val ModuleDefinitionHandle = moduleDefHandle

    member val TypeReferenceVoid: ClrTypeHandle = sysTy "Void" false
    member val TypeReferenceObject: ClrTypeHandle = sysTyObject
    member val TypeReferenceByte: ClrTypeHandle = sysTy "Byte" true
    member val TypeReferenceSByte: ClrTypeHandle = sysTy "SByte" true
    member val TypeReferenceInt16: ClrTypeHandle = sysTy "Int16" true
    member val TypeReferenceUInt16: ClrTypeHandle = sysTy "UInt16" true
    member val TypeReferenceInt32: ClrTypeHandle = sysTy "Int32" true
    member val TypeReferenceUInt32: ClrTypeHandle = sysTy "UInt32" true
    member val TypeReferenceInt64: ClrTypeHandle = sysTy "Int64" true
    member val TypeReferenceUInt64: ClrTypeHandle = sysTy "UInt64" true
    member val TypeReferenceSingle: ClrTypeHandle = sysTy "Single" true
    member val TypeReferenceDouble: ClrTypeHandle = sysTy "Double" true
    member val TypeReferenceChar: ClrTypeHandle = sysTy "Char" true
    member val TypeReferenceBoolean: ClrTypeHandle = sysTy "Boolean" true
    member val TypeReferenceString: ClrTypeHandle = sysTy "String" false
    member val TypeReferenceArray: ClrTypeHandle = sysTy "Array" false
    member val TypeReferenceIntPtr: ClrTypeHandle = sysTy "IntPtr" true
    member val TypeReferenceUIntPtr: ClrTypeHandle = sysTy "UIntPtr" true

    member val ``TypeReferenceAttribute`` = sysTy "Attribute" false

    member val ``TypeReferenceType`` = sysTy "Type" false

    member val ``TypeReferenceAction`` = sysTy "Action" false
    member val ``TypeReferenceAction`1`` = sysTy "Action`1" false
    member val ``TypeReferenceAction`2`` = sysTy "Action`2" false
    member val ``TypeReferenceAction`3`` = sysTy "Action`3" false
    member val ``TypeReferenceAction`4`` = sysTy "Action`4" false
    member val ``TypeReferenceAction`5`` = sysTy "Action`5" false
    member val ``TypeReferenceAction`6`` = sysTy "Action`6" false
    member val ``TypeReferenceAction`7`` = sysTy "Action`7" false
    member val ``TypeReferenceAction`8`` = sysTy "Action`8" false
    member val ``TypeReferenceAction`9`` = sysTy "Action`9" false
    member val ``TypeReferenceAction`10`` = sysTy "Action`10" false
    member val ``TypeReferenceAction`11`` = sysTy "Action`11" false
    member val ``TypeReferenceAction`12`` = sysTy "Action`12" false
    member val ``TypeReferenceAction`13`` = sysTy "Action`13" false
    member val ``TypeReferenceAction`14`` = sysTy "Action`14" false
    member val ``TypeReferenceAction`15`` = sysTy "Action`15" false
    member val ``TypeReferenceAction`16`` = sysTy "Action`16" false

    member val ``TypeReferenceFunc`1`` = sysTy "Func`1" false
    member val ``TypeReferenceFunc`2`` = sysTy "Func`2" false
    member val ``TypeReferenceFunc`3`` = sysTy "Func`3" false
    member val ``TypeReferenceFunc`4`` = sysTy "Func`4" false
    member val ``TypeReferenceFunc`5`` = sysTy "Func`5" false
    member val ``TypeReferenceFunc`6`` = sysTy "Func`6" false
    member val ``TypeReferenceFunc`7`` = sysTy "Func`7" false
    member val ``TypeReferenceFunc`8`` = sysTy "Func`8" false
    member val ``TypeReferenceFunc`9`` = sysTy "Func`9" false
    member val ``TypeReferenceFunc`10`` = sysTy "Func`10" false
    member val ``TypeReferenceFunc`11`` = sysTy "Func`11" false
    member val ``TypeReferenceFunc`12`` = sysTy "Func`12" false
    member val ``TypeReferenceFunc`13`` = sysTy "Func`13" false
    member val ``TypeReferenceFunc`14`` = sysTy "Func`14" false
    member val ``TypeReferenceFunc`15`` = sysTy "Func`15" false
    member val ``TypeReferenceFunc`16`` = sysTy "Func`16" false

    member val ``String_Equals`` = lazy createStringEqualsMethod ()

    member val ``TypeReferenceValueTuple`1`` = sysTy "ValueTuple`1" true
    member val ``TypeReferenceValueTuple`2`` = sysTy "ValueTuple`2" true
    member val ``TypeReferenceValueTuple`3`` = sysTy "ValueTuple`3" true
    member val ``TypeReferenceValueTuple`4`` = sysTy "ValueTuple`4" true
    member val ``TypeReferenceValueTuple`5`` = sysTy "ValueTuple`5" true
    member val ``TypeReferenceValueTuple`6`` = sysTy "ValueTuple`6" true
    member val ``TypeReferenceValueTuple`7`` = sysTy "ValueTuple`7" true
    member val ``TypeReferenceValueTuple`8`` = sysTy "ValueTuple`8" true

    member val tr_InAttribute: ClrTypeHandle option = None with get, set
    member val tr_IsReadOnlyAttribute: ClrTypeHandle option = None with get, set
    member val tr_UnmanagedFunctionPointerAttribute: ClrTypeHandle option = None with get, set
    member val tr_CallingConvention: ClrTypeHandle option = None with get, set
    member val tr_IsByRefLikeAttribute: ClrTypeHandle option = None with get, set
    member val tr_IsByRefLikeAttributeConstructor: ClrMethodHandle option = None with get, set

    member val tr_Span: ClrTypeHandle option = None with get, set
    member val tr_ReadOnlySpan: ClrTypeHandle option = None with get, set

    member val ``ConsoleWriteMethod`` = lazy createConsoleWriteMethod()
    member val ``ConsoleWriteMethod_Int32`` = lazy createConsoleWriteMethod_Int32()
    member val ``ConsoleWriteMethod_String`` = lazy createConsoleWriteMethod_String()
    member val ``GetTypeFromHandleMethod`` = lazy createGetTypeFromHandleMethod()
    member val ``DebuggableAttributeConstructor`` = lazy createDebuggableConstructor()
    member val ``DebuggerStepperBoundaryAttribute`` = lazy createDebuggerStepperBoundaryAttributeConstructor()

    member val ``MulticastDelegate`` = sysTy "MulticastDelegate" false

    member val EntryPoint = ClrMethodHandle.None with get, set

    member this.CreateConstructor(enclosingTy: ClrTypeHandle): ClrMethodHandle =
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = true)
        encoder.Parameters(
            0,
            (fun encoder -> encoder.Void()),
            (fun _encoder -> ())
        )

        let name = metadataBuilder.GetOrAddString(".ctor")
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                enclosingTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

    member this.CreateMethodHandle(enclosingTy: ClrTypeHandle, methodName: string, isInstance: bool, parTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = isInstance)
        encoder.Parameters(
            parTys.Length,
            (fun encoder -> MetadataHelpers.encodeReturnType(encoder, returnTy, this)),
            (fun encoder ->
                parTys
                |> ImArray.iter (fun parTy ->
                    this.EncodeType(encoder.AddParameter().Type(), parTy)
                )
            )
        )

        let name = metadataBuilder.GetOrAddString(methodName)
        let signature = metadataBuilder.GetOrAddBlob(signature)

        let realHandle =
            metadataBuilder.AddMemberReference(
                enclosingTy.EntityHandle,
                name,
                signature
            )

        createMemRef realHandle name signature

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

    member internal this.CreateMethodSignature(convention, tyParCount: int, isInstance, parTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        MetadataHelpers.buildMethodSignature(signatureBuilder, convention, tyParCount, isInstance, parTys, returnTy, this)
        let blobHandle = metadataBuilder.GetOrAddBlob signatureBuilder
        signatureBuilder.Clear()
        blobHandle

    member internal this.CreateStandaloneSignature(locals: ClrLocal imarray) =
        let mutable encoder = BlobEncoder(signatureBuilder)
        let mutable localVarSignature: LocalVariablesEncoder = encoder.LocalVariableSignature(locals.Length)

        locals
        |> ImArray.iter (fun local ->
            MetadataHelpers.encodeType(localVarSignature.AddVariable().Type(local.Type.IsByRef_t, local.IsPinned), local.Type, this)
        )

        let handle =
            signatureBuilder
            |> metadataBuilder.GetOrAddBlob
            |> metadataBuilder.AddStandaloneSignature
        signatureBuilder.Clear()
        handle

    member internal this.CreatePropertySignature(isInstance, returnTy: ClrTypeHandle) =
        let signature = BlobBuilder()
        BlobEncoder(signature).PropertySignature(isInstance).Parameters(
            0,
            (fun encoder ->
                if returnTy.HasEntityHandle then
                    match returnTy.EntityHandle with
                    | x when x = this.TypeReferenceVoid.EntityHandle -> 
                        encoder.Void()
                    | _ ->
                        MetadataHelpers.encodeType(encoder.Type(returnTy.IsByRef_t), returnTy, this)
                else
                    MetadataHelpers.encodeType(encoder.Type(returnTy.IsByRef_t), returnTy, this)),
            (fun _ -> ())
        )
        signature
        |> metadataBuilder.GetOrAddBlob

    member internal this.CreateFieldSignature(ty: ClrTypeHandle) =
        let signature = BlobBuilder()
        MetadataHelpers.encodeFieldType(BlobEncoder(signature).Field(), ty, this)
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
                    if ty.IsByRef_t || ty.IsNativePointer_t then
                        OlyAssert.Fail("Invalid type argument.")
                    else
                        MetadataHelpers.encodeType(encoderInst.AddArgument(), ty, this)
                )

                metadataBuilder.AddTypeSpecification(metadataBuilder.GetOrAddBlob(signature))
        handle

    member internal this.AddModReq(modifierHandle: ClrTypeHandle, tyHandle: ClrTypeHandle) : EntityHandle =
        let key = struct(modifierHandle.EntityHandle, tyHandle.EntityHandle)
        match typeCache_ModReq.TryGetValue(key) with
        | true, result -> result
        | _ ->
            let signature = BlobBuilder()
            let mutable encoder = BlobEncoder(signature)
            let mutable encoder = encoder.TypeSpecificationSignature()
            encoder.CustomModifiers().AddModifier(modifierHandle.EntityHandle, false)
            |> ignore
            MetadataHelpers.encodeType(encoder, tyHandle, this)
            let result: EntityHandle = 
                TypeSpecificationHandle.op_Implicit(metadataBuilder.AddTypeSpecification(metadataBuilder.GetOrAddBlob(signature)))
            typeCache_ModReq[key] <- result
            result

    member internal this.EnqueueOverride(f) =
        overrideQueue.Enqueue(f)

    member private this.AddDebuggableAttribute(asmDefHandle) =
        let b = BlobBuilder()

        // Prolog
        b.WriteByte(1uy)
        b.WriteByte(0uy)

        b.WriteInt32(1 ||| 256)

        // NumNamed
        b.WriteByte(0uy)
        b.WriteByte(0uy)        

        let blobHandle = this.AddBlob(b)

        metadataBuilder.AddCustomAttribute(asmDefHandle, this.DebuggableAttributeConstructor.Value.UnsafeLazilyEvaluateEntityHandle(), blobHandle)
        |> ignore
    
    member this.Write(stream: IO.Stream, pdbStream: IO.Stream, isDebuggable: bool) =
        if isDebuggable then
            this.AddDebuggableAttribute(asmDefHandle)

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

#if DEBUG || CHECKED
        OlyAssert.Equal(metadataBuilder.GetRowCount(TableIndex.MethodDef), pdbBuilder.Internal.GetRowCount(TableIndex.MethodDebugInformation))
#endif

        let rootBuilder = new MetadataRootBuilder(metadataBuilder)

        let algoName = HashAlgorithmName.SHA256
        let mutable computedHash = Unchecked.defaultof<_>
        let hash (content: Blob seq) =
            use incrHash = IncrementalHash.CreateHash(algoName)
            content
            |> Seq.iter (fun x -> incrHash.AppendData(x.GetBytes().Array))
            computedHash <- incrHash.GetHashAndReset().ToImmutableArray()
            computedHash
        let portablePdbIdProvider =
            fun (content: Blob seq) -> BlobContentId.FromHash(hash content)

        let pdbBuilder = 
            PortablePdbBuilder(
                pdbBuilder.Internal, 
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
                flags = (CorFlags.ILOnly),
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
                MetadataHelpers.encodeType(encoder.AddArgument(), tyArg, this)
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
                MetadataHelpers.encodeType(encoder.AddArgument(), tyArg, this)
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
            | 5 -> this.AddGenericInstanceType(this.``TypeReferenceAction`5``, argTys), argTys
            | 6 -> this.AddGenericInstanceType(this.``TypeReferenceAction`6``, argTys), argTys
            | 7 -> this.AddGenericInstanceType(this.``TypeReferenceAction`7``, argTys), argTys
            | 8 -> this.AddGenericInstanceType(this.``TypeReferenceAction`8``, argTys), argTys
            | 9 -> this.AddGenericInstanceType(this.``TypeReferenceAction`9``, argTys), argTys
            | 10 -> this.AddGenericInstanceType(this.``TypeReferenceAction`10``, argTys), argTys
            | 11 -> this.AddGenericInstanceType(this.``TypeReferenceAction`11``, argTys), argTys
            | 12 -> this.AddGenericInstanceType(this.``TypeReferenceAction`12``, argTys), argTys
            | 13 -> this.AddGenericInstanceType(this.``TypeReferenceAction`13``, argTys), argTys
            | 14 -> this.AddGenericInstanceType(this.``TypeReferenceAction`14``, argTys), argTys
            | 15 -> this.AddGenericInstanceType(this.``TypeReferenceAction`15``, argTys), argTys
            | 16 -> this.AddGenericInstanceType(this.``TypeReferenceAction`16``, argTys), argTys
            | _ ->
                raise(NotSupportedException($"not supported: System.Action type with arg-count: {argTys.Length}"))
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
            | 5 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`6``, tyInst), tyInst
            | 6 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`6``, tyInst), tyInst
            | 7 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`7``, tyInst), tyInst
            | 8 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`8``, tyInst), tyInst
            | 9 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`9``, tyInst), tyInst
            | 10 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`10``, tyInst), tyInst
            | 11 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`11``, tyInst), tyInst
            | 12 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`12``, tyInst), tyInst
            | 13 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`13``, tyInst), tyInst
            | 14 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`14``, tyInst), tyInst
            | 15 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`15``, tyInst), tyInst
            | 16 ->
                let tyInst = argTys.Add(returnTy)
                this.AddGenericInstanceType(this.``TypeReferenceFunc`16``, tyInst), tyInst
            | _ ->
                raise(NotSupportedException($"not supported: System.Func type with arg-count: {argTys.Length}"))

    member this.AddAnonymousFunctionConstructor(parent: ClrTypeHandle) =
        let signature = BlobBuilder()
        let mutable encoder = BlobEncoder(signature)
        let mutable encoder = encoder.MethodSignature(isInstanceMethod = true)
        encoder.Parameters(
            2,
            (fun encoder -> encoder.Void()),
            (fun encoder -> 
                MetadataHelpers.encodeType(encoder.AddParameter().Type(), this.TypeReferenceObject, this)
                MetadataHelpers.encodeType(encoder.AddParameter().Type(), this.TypeReferenceIntPtr, this))
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

    member this.AddAnonymousFunctionInvoke(parent: ClrTypeHandle, tyInst: ClrTypeHandle imarray, argTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        let returnsVoid = returnTy.HasEntityHandle && returnTy.EntityHandle = this.TypeReferenceVoid.EntityHandle

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

    member this.EncodeType(encoder, handle) =
        MetadataHelpers.encodeType(encoder, handle, this)

    member this.AddArrayType(elementTy: ClrTypeHandle, rank) =
        ClrTypeHandle.Array(elementTy, rank)

    member this.AddFunctionPointer(cc, parTys: ClrTypeHandle imarray, returnTy: ClrTypeHandle) =
        ClrTypeHandle.FunctionPointer(cc, parTys, returnTy)

    member this.AddNativePointer(elementTy: ClrTypeHandle) =
        ClrTypeHandle.NativePointer(elementTy)

    member this.AddValueTupleType(tyInst: ClrTypeHandle imarray) =
        match tyInst.Length with
        | 1 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`1``, tyInst)
        | 2 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`2``, tyInst)
        | 3 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`3``, tyInst)
        | 4 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`4``, tyInst)
        | 5 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`5``, tyInst)
        | 6 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`6``, tyInst)
        | 7 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`7``, tyInst)
        | 8 -> this.AddGenericInstanceType(this.``TypeReferenceValueTuple`8``, tyInst)
        | _ ->
            raise(System.NotSupportedException("Tuple item count larger than 8 or zero."))

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

        valueTupleTy, createMemRef realHandle name signature

    member this.CreateTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyParCount: int, isStruct, baseTypeHandle) =
        let tyDefBuilder = ClrTypeDefinitionBuilder(this, enclosingTyHandle, namespac, name, tyParCount, isStruct, false, baseTypeHandle)
        tyDefQueue.Enqueue(fun () -> tyDefBuilder.Handle.EntityHandle |> ignore)
        tyDefBuilder
        
    /// TODO: Remove this and just use CreateTypeDefinitionBuilder.
    member this.CreateEnumTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyParCount: int, baseTypeHandle) =
        let tyDefBuilder = ClrTypeDefinitionBuilder(this, enclosingTyHandle, namespac, name, tyParCount, false, true, baseTypeHandle)
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

    member this.AddFieldReference(parent: ClrTypeHandle, fieldHandle: ClrFieldHandle) =
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

    member _.AddPropertyAttribute(parent: ClrPropertyDefinitionBuilder, ctor: ClrMethodHandle, blobHandle) =
        attrQueue.Enqueue(fun () ->
            metadataBuilder.AddCustomAttribute(parent.UnsafeLazilyEvaluateEntityHandle(), ctor.UnsafeLazilyEvaluateEntityHandle(), blobHandle) |> ignore
        )

    member _.AddMethodAttribute(parent: ClrMethodHandle, ctor: ClrMethodHandle, blobHandle) =
        attrQueue.Enqueue(fun () ->
            metadataBuilder.AddCustomAttribute(parent.UnsafeLazilyEvaluateEntityHandle(), ctor.UnsafeLazilyEvaluateEntityHandle(), blobHandle) |> ignore
        )

    member this.AddAssemblyAttribute(ctor: ClrMethodHandle, blobHandle) =
        attrQueue.Enqueue(fun () ->
            metadataBuilder.AddCustomAttribute(asmDefHandle, ctor.UnsafeLazilyEvaluateEntityHandle(), blobHandle) |> ignore
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

[<RequireQualifiedAccess>]
[<NoEquality;NoComparison>]
type ClrTypeConstraint =
    | SubtypeOf of ClrTypeHandle

[<RequireQualifiedAccess>]
[<NoEquality;NoComparison>]
type ClrTypeParameter =
    {
        Name: string
        Constraints: ClrTypeConstraint imarray
        Flags: GenericParameterAttributes
    }

[<Sealed>]
type ClrMethodDefinitionBuilder internal (asmBuilder: ClrAssemblyBuilder, enclosingTyParCount: int, name, tyPars: ClrTypeParameter imarray, pars: (string * ClrTypeHandle) imarray, returnTy: ClrTypeHandle, isInstance: bool) as this =
    
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
        | I.Leave _ -> ILOpCode.Leave
        | _ -> OlyAssert.Fail("Invalid branch instruction.")

    static let getShortBranchOpCode opCode =
        match opCode with
        | ILOpCode.Beq -> ILOpCode.Beq_s
        | ILOpCode.Bge -> ILOpCode.Bge_s
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
        | ILOpCode.Leave -> ILOpCode.Leave_s
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
            | ClrTypeHandle.Array _ ->
                il.Token(asmBuilder.TypeReferenceArray.EntityHandle)
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
            MetadataHelpers.encodeReturnType(returnTyEncoder, returnTy, asmBuilder)
            MetadataHelpers.encodeParameters(parEncoder, parTys, asmBuilder)
            let handle = asmBuilder.MetadataBuilder.AddStandaloneSignature(asmBuilder.MetadataBuilder.GetOrAddBlob(signature))
            il.CallIndirect(handle)

        | I.Ldlen ->
            il.OpCode(ILOpCode.Ldlen)

        | I.Ldtoken(handle) ->
            il.OpCode(ILOpCode.Ldtoken)
            emitTypeToken asmBuilder &il handle

        | I.Sizeof(handle) ->
            il.OpCode(ILOpCode.Sizeof)
            emitTypeToken asmBuilder &il handle

        | I.Initobj(handle) ->
            match handle with
            | ClrTypeHandle.NativePointer _
            | ClrTypeHandle.FunctionPointer _ ->
                OlyAssert.Fail("Invalid type handle.")
            | _ ->
                il.OpCode(ILOpCode.Initobj)
                emitTypeToken asmBuilder &il handle

        | I.Constrained(handle) ->
            il.OpCode(ILOpCode.Constrained)
            emitTypeToken asmBuilder &il handle

        | I.Throw ->
            il.OpCode(ILOpCode.Throw)

        | I.Stind_ref ->
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
        | I.Div_un ->
            il.OpCode(ILOpCode.Div_un)
        | I.Rem ->
            il.OpCode(ILOpCode.Rem)
        | I.Rem_un ->
            il.OpCode(ILOpCode.Rem_un)
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

        | I.Endfinally ->
            il.OpCode(ILOpCode.Endfinally)

        | I.Isinst handle ->
            il.OpCode(ILOpCode.Isinst)
            emitTypeToken asmBuilder &il handle

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
        | I.Label _
        | I.CatchRegion _
        | I.FinallyRegion _
        | I.SequencePoint _
        | I.HiddenSequencePoint
        | I.BeginLocalScope _
        | I.EndLocalScope
        | I.Leave _
        | I.Skip ->
            failwith "Unexpected instruction."

    static let sizeOfInstr instr =
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

        | I.Sizeof _ ->
            2 + 4

        | I.Initobj _ ->
            2 + 4

        | I.Constrained _ ->
            2 + 4

        | I.Throw ->
            1

        | I.Stind_ref ->
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
        | I.Div_un ->
            1
        | I.Rem ->
            1
        | I.Rem_un ->
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
                4 + 2
        | I.Ldarga value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                1 + 1
            else
                4 + 2
        | I.LdindRef ->
            1
        | I.Ldloc value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                if value >= 0 && value <= 3 then
                    1
                else
                    1 + 1
            else
                4 + 2
        | I.Ldloca value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                1 + 1
            else
                4 + 2
        | I.Starg value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                1 + 1
            else
                4 + 2
        | I.Stloc value ->
            if value <= int Byte.MaxValue && value >= int Byte.MinValue then
                if value >= 0 && value <= 3 then
                    1
                else
                    1 + 1
            else
                4 + 2

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
        | I.Isinst _ ->
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

        | I.Endfinally ->
            1

        | I.Leave _ ->
            1 + 4

        | I.Label _
        | I.CatchRegion _
        | I.FinallyRegion _
        | I.SequencePoint _
        | I.HiddenSequencePoint
        | I.BeginLocalScope _
        | I.EndLocalScope
        | I.Skip ->
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
        let localSig = asmBuilder.CreateStandaloneSignature(this.Locals)
        let localSigToken = MetadataTokens.GetRowNumber(localSig)

        if this.BodyInstructions.IsEmpty then 
            asmBuilder.PdbBuilder.AddEmptyMethodDebugInformation(localSigToken)
            |> ignore
            -1, ImArray.empty // no body - this makes the RVA zero
        else

        let instrs = this.BodyInstructions

        //---------------------------------------------------------

#if DEBUG || CHECKED
        let labels = Dictionary<int, _>()

        let mutable dummyIL = createInstructionEncoder()

        for i = 0 to instrs.Length - 1 do
            let instr = instrs[i]

            match instr with
            | I.Label labeId ->
                addLabel &dummyIL labels labeId
            | _ ->
                ()

        let mutable maxStack = 0

        let dummyOffsets = List()

        if dummyIL.Offset <> 0 then
            failwith "Dummy IL should have started with 0 offset"

        for i = 0 to instrs.Length - 1 do
            let instr = instrs[i]

            dummyOffsets.Add(dummyIL.Offset)

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
            | I.Br labelId
            | I.Leave labelId ->
                dummyIL.Branch(getBranchOpCode instr, labels[labelId])

            | I.Label labelId ->
                dummyIL.MarkLabel(labels[labelId])

            | I.CatchRegion(tryStartLabelId, tryEndLabelId, handlerStartLabelId, handlerEndLabelId, catchTy) ->
                dummyIL.ControlFlowBuilder.AddCatchRegion(
                    labels[tryStartLabelId],
                    labels[tryEndLabelId],
                    labels[handlerStartLabelId],
                    labels[handlerEndLabelId],
                    catchTy.EntityHandle
                )

            | I.FinallyRegion(tryStartLabelId, tryEndLabelId, handlerStartLabelId, handlerEndLabelId) ->
                dummyIL.ControlFlowBuilder.AddFinallyRegion(
                    labels[tryStartLabelId],
                    labels[tryEndLabelId],
                    labels[handlerStartLabelId],
                    labels[handlerEndLabelId]
                )

            | I.SequencePoint _
            | I.HiddenSequencePoint
            | I.BeginLocalScope _
            | I.EndLocalScope
            | I.Skip ->
                ()

            | _ ->
                let originalOffset = dummyIL.Offset
                emitInstr asmBuilder &maxStack &dummyIL instr
                let offset = dummyIL.Offset
                if originalOffset = offset then
                    failwithf "IL emitter should have emitted instruction %A" instr
                elif (offset - originalOffset) <> sizeOfInstr instr then
                    failwithf "Invalid estimated size of instruction - %A - expected: %A, actual: %A, index: %A" instr (offset - originalOffset) (sizeOfInstr instr) i
#endif

        let labels = Dictionary<int, _>()
        let labelOffsets = Dictionary<int, _>()
        let offsets = Array.zeroCreate<int> instrs.Length
        let shorts = Array.zeroCreate<bool> instrs.Length

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

            totalSize <- totalSize + sizeOfInstr instr

#if DEBUG || CHECKED
        // Assert estimation size
        // In Debug, we emit the IL instructions twice.
        OlyAssert.Equal(dummyIL.Offset, totalSize)
#endif

        // Branch tightening
        let mutable totalSize = 0
        for i = 0 to instrs.Length - 1 do
            let instr = instrs[i]

            offsets[i] <- totalSize

            let size =
                match instr with
                | I.Label(labelId) ->
                    labelOffsets[labelId] <- totalSize
                    0
                | _ ->
                    if instr.IsBranch then
                        let offset = offsets[i] + sizeOfInstr instr
                        let labelOffset = labelOffsets[instr.LabelId]
                        let distance = labelOffset - offset
                        if distance >= int SByte.MinValue && distance <= int SByte.MaxValue then
                            shorts[i] <- true
                            2
                        else
                            5
                    else
                        sizeOfInstr instr

            totalSize <- totalSize + size

        let ilBuilder = asmBuilder.ILBuilder
        ilBuilder.Align(4)

        // ----
        let mutable firstDocument = None

        let seqPoints = ImArray.builder()

        let mutable maxStack = 0

        let mutable hasMultipleDocuments = false

        let debugILOffsetStack = Stack()
        let localScopes = ImArray.builder()  

        let inline getLabelOffset labelId =
            labelOffsets[labelId]

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
            | I.Br labelId
            | I.Leave labelId ->
                let isShort = shorts[i]
                let labelOffset = getLabelOffset labelId
                let opCode = getBranchOpCode instr
                let opCode = if isShort then getShortBranchOpCode opCode else opCode
                let target = labelOffset - il.Offset - (if isShort then 2 else 5)
                il.OpCode(opCode)            
                if isShort then
                    il.CodeBuilder.WriteSByte(Checked.sbyte target)
                else
                    il.CodeBuilder.WriteInt32(target)

            | I.Label labelId ->
                il.MarkLabel(labels[labelId])
                if il.Offset <> getLabelOffset labelId then
                    failwith "Bad IL or label offset."

            | I.CatchRegion(tryStartLabelId, tryEndLabelId, handlerStartLabelId, handlerEndLabelId, catchTy) ->
                il.ControlFlowBuilder.AddCatchRegion(
                    labels[tryStartLabelId],
                    labels[tryEndLabelId],
                    labels[handlerStartLabelId],
                    labels[handlerEndLabelId],
                    catchTy.EntityHandle
                )

            | I.FinallyRegion(tryStartLabelId, tryEndLabelId, handlerStartLabelId, handlerEndLabelId) ->
                il.ControlFlowBuilder.AddFinallyRegion(
                    labels[tryStartLabelId],
                    labels[tryEndLabelId],
                    labels[handlerStartLabelId],
                    labels[handlerEndLabelId]
                )

            | I.SequencePoint(documentPath, startLine, endLine, startColumn, endColumn) ->
                if String.IsNullOrWhiteSpace documentPath then
                    ()
                else
                    let document = asmBuilder.PdbBuilder.GetOrAddDocument(documentPath)
                    match firstDocument with
                    | None -> 
                        firstDocument <- Some(documentPath, document)
                        seqPoints.Add(ClrSequencePoint(document, il.Offset, startLine, endLine, startColumn, endColumn))
                    | Some(firstDocumentPath, _) when il.Offset > 0 -> 
                        if not(OlyPath.Equals(documentPath, firstDocumentPath)) then
                            hasMultipleDocuments <- true
                        seqPoints.Add(ClrSequencePoint(document, il.Offset, startLine, endLine, startColumn, endColumn))
                    | _ ->
                        ()

            | I.HiddenSequencePoint ->
                match firstDocument with
                | Some(firstDocumentPath, firstDocument) when il.Offset > 0 ->
                    seqPoints.Add(ClrSequencePoint(firstDocument, il.Offset, 1, 1, 1, 1))
                | _ ->
                    ()

            | I.BeginLocalScope debugLocals ->
                debugILOffsetStack.Push(il.Offset, debugLocals)

            | I.EndLocalScope ->
                match debugILOffsetStack.TryPop() with
                | true, (ilOffset, debugLocals) ->
                    localScopes.Add({ Locals = debugLocals; StartOffset = ilOffset; Length = il.Offset - ilOffset })
                | _ ->
                    OlyAssert.Fail("Unexpected 'EndLocalScope'. No 'BeginLocalScope' instruction was found.")

            | I.Skip ->
                ()

            | _ ->
                emitInstr asmBuilder &maxStack &il instr

        //---------------------------------------------------------

        let mutable methBodyStream = MethodBodyStreamEncoder(ilBuilder)
        let bodyOffset =
            // TODO: 'maxStack + 8' is a little arbitrary, we should correctly count the max number of stacks.
            //       The '8' is just a little extra room to ensure we have enough.
            if this.Locals.IsEmpty then
                methBodyStream.AddMethodBody(il, maxStack = maxStack + 8) 
            else
                methBodyStream.AddMethodBody(il, maxStack = maxStack + 8, localVariablesSignature = localSig)

        match firstDocument with
        | Some(_, document) ->
            if hasMultipleDocuments then
                asmBuilder.PdbBuilder.AddMethodDebugInformation(localSigToken, seqPoints.ToImmutable()) 
                |> ignore
            else
                asmBuilder.PdbBuilder.AddMethodDebugInformation(document, localSigToken, seqPoints.ToImmutable()) 
                |> ignore
        | _ ->
            asmBuilder.PdbBuilder.AddEmptyMethodDebugInformation(localSigToken)
            |> ignore

        // Sort by Length in descending order, then by StartOffset in ascending order.
        // This is a requirement to get debug local variables working.
        let localScopes =
            localScopes.ToImmutable()
            |> Seq.sortByDescending (fun x -> x.Length)
            |> Seq.sortBy (fun x -> x.StartOffset)
            |> ImArray.ofSeq

        bodyOffset, localScopes

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
           
            let handle, debugLocalScopes =
                let methSig = asmBuilder.CreateMethodSignature(this.Convention, tyPars.Length, this.IsInstance, parTys, returnTy)
                let bodyOffset, debugLocalScopes = this.PopulateMethodBody()
                metadataBuilder.AddMethodDefinition(
                    this.Attributes,
                    this.ImplementationAttributes,
                    nameHandle,
                    methSig,
                    bodyOffset,
                    parList), debugLocalScopes

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
                    let tyPar = tyPars.[i]
                    let name = metadataBuilder.GetOrAddString(tyPar.Name)
                    let handle = metadataBuilder.AddGenericParameter(castedHandle, tyPar.Flags, name, enclosingTyParCount + i)                 
                    MetadataHelpers.addConstraints asmBuilder handle tyPar.Constraints
                    
            if not tyPars.IsEmpty then
                asmBuilder.GenericParameterQueue.Enqueue(f, index)

            // ------------------ Local Scopes (Debug info) ------------------

            debugLocalScopes
            |> ImArray.iter (fun debugLocalScope ->
                let importScopeHandle = 
                    Unchecked.defaultof<_>
                
                // Debug Local Variables
                // ------------------
                let localVars = ResizeArray()
                for i = 0 to debugLocalScope.Locals.Length - 1 do
                    let debugLocal = debugLocalScope.Locals[i]
                    let nameHandle =  
                        let name = debugLocal.Name
                        if String.IsNullOrWhiteSpace name then
                            StringHandle()
                        else
                            asmBuilder.PdbBuilder.Internal.GetOrAddString(name)
                
                    let localVarAttrs =
                        if String.IsNullOrWhiteSpace name then
                            LocalVariableAttributes.DebuggerHidden
                        else
                            LocalVariableAttributes.None
                    asmBuilder.PdbBuilder.Internal.AddLocalVariable(
                        localVarAttrs,
                        debugLocal.Index,
                        nameHandle
                    )
                    |> localVars.Add
                
                let localVarListHandle =
                    if localVars.Count = 0 then
                        MetadataTokens.LocalVariableHandle(asmBuilder.PdbBuilder.Internal.GetRowCount(TableIndex.LocalVariable) + 1)
                    else
                        localVars.[0]
                // ------------------
                
                let localConstantListHandle =
                    MetadataTokens.LocalConstantHandle(asmBuilder.PdbBuilder.Internal.GetRowCount(TableIndex.LocalConstant) + 1)
                
                asmBuilder.PdbBuilder.Internal.AddLocalScope(
                    handle,
                    importScopeHandle,
                    localVarListHandle,
                    localConstantListHandle,
                    debugLocalScope.StartOffset,
                    debugLocalScope.Length
                )
                |> ignore
            )

            handle
        )

[<Sealed>]
type ClrPropertyDefinitionBuilder internal (asmBuilder: ClrAssemblyBuilder, name: string, returnTy: ClrTypeHandle, isInstance: bool, getterOpt: ClrMethodHandle option, setterOpt: ClrMethodHandle option) =

    let metadataBuilder = asmBuilder.MetadataBuilder
    let propSig = asmBuilder.CreatePropertySignature(isInstance, returnTy)
    let nameHandle = metadataBuilder.GetOrAddString(name)

    let handle =
        lazy
            let result = metadataBuilder.AddProperty(PropertyAttributes.None, metadataBuilder.GetOrAddString(name), propSig)
            asmBuilder.SetPropertyDefinitionRowCount(asmBuilder.PropertyDefinitionRowCount + 1)
            result

    member _.Signature = propSig
    member _.NameHandle = nameHandle

    member _.GetterOption = getterOpt
    member _.SetterOption = setterOpt

    member internal _.UnsafeLazilyEvaluateEntityHandle() : PropertyDefinitionHandle = handle.Value

[<Sealed>]
[<DebuggerDisplay("{FullyQualifiedName}")>]
type ClrTypeDefinitionBuilder internal (asmBuilder: ClrAssemblyBuilder, enclosingTyHandle: ClrTypeHandle, namespac: string, name: string, tyParCount: int, isValueType: bool, isEnum: bool, baseTypeHandle: ClrTypeHandle) as this =

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
    let propDefs = ResizeArray<ClrPropertyDefinitionBuilder>()

    let mutable tyPars = ImArray.empty

    member _.SetTypeParameters(newTyPars: ClrTypeParameter imarray) =
        if newTyPars.Length <> tyParCount then
            failwith "Invalid amount of type parameters."
        tyPars <- newTyPars

    member _.FullyQualifiedName = fullyQualifiedName

    member val Attributes = TypeAttributes() with get, set
    member val InterfaceImplementations: ClrTypeHandle imarray = ImArray.empty with get, set

    member val BaseType = baseTypeHandle with get, set

    member _.FindField(name: string) = fieldDefs |> Seq.find (fun (x, _) -> x = name) |> snd

    member _.GetFieldByIndex(index: int) = fieldDefs[index] |> snd

    member _.MethodDefinitionBuilders = methDefs :> _ seq

    member this.CreateMethodDefinitionBuilder(name: string, methTyPars, pars, returnTy: ClrTypeHandle, isInstance: bool) =
        let methDefBuilder = ClrMethodDefinitionBuilder(asmBuilder, tyParCount, name, methTyPars, pars, returnTy, isInstance)
        methDefs.Add(methDefBuilder)
        methDefBuilder

    member this.CreatePropertyDefinitionBuilder(name: string, returnTy: ClrTypeHandle, isInstance: bool, getterOpt, setterOpt) =
        let propDefBuilder = ClrPropertyDefinitionBuilder(asmBuilder, name, returnTy, isInstance, getterOpt, setterOpt)
        propDefs.Add(propDefBuilder)
        propDefBuilder

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

            if tyPars.Length <> tyParCount then
                failwith "Invalid amount of type parameters."

            let f = fun () ->
                for i = 0 to tyPars.Length - 1 do
                    let tyPar = tyPars[i]
                    let name = metadataBuilder.GetOrAddString(tyPar.Name)
                    let handle = metadataBuilder.AddGenericParameter(castedTyDefHandle, tyPar.Flags, name, i)
                    MetadataHelpers.addConstraints asmBuilder handle tyPar.Constraints
            if not tyPars.IsEmpty then
                asmBuilder.GenericParameterQueue.Enqueue(f, tyDefIndex)

            match enclosingTyHandle with
            | ClrTypeHandle.LazyTypeDefinition(_, _, fakeEnclosingTyDefHandle, _) ->
                OlyAssert.True(MetadataTokens.GetRowNumber(fakeEnclosingTyDefHandle) < MetadataTokens.GetRowNumber(tyDefHandle))
                metadataBuilder.AddNestedType(tyDefHandle, fakeEnclosingTyDefHandle)
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

            // ---------------------------------------------------------------------------------------------------

            let addProperty (propDef: ClrPropertyDefinitionBuilder) =
                let propDefHandle = propDef.UnsafeLazilyEvaluateEntityHandle()
                
                match propDef.GetterOption with
                | Some getter ->
                    let getterHandle = 
                        let getterHandle = getter.UnsafeLazilyEvaluateEntityHandle()
                        try
                            MethodDefinitionHandle.op_Explicit(getterHandle)
                        with
                        | _ ->
                            failwith $"Invalid cast for getter method definition handle. Handle kind: {getterHandle.Kind}\n Enclosing type: {fullyQualifiedName}"
                    metadataBuilder.AddMethodSemantics(
                        PropertyDefinitionHandle.op_Implicit(propDef.UnsafeLazilyEvaluateEntityHandle()), 
                        MethodSemanticsAttributes.Getter, 
                        getterHandle
                    )
                | _ ->
                    ()
                
                match propDef.SetterOption with
                | Some setter ->
                    let setterHandle = 
                        let setterHandle = setter.UnsafeLazilyEvaluateEntityHandle()
                        try
                            MethodDefinitionHandle.op_Explicit(setter.UnsafeLazilyEvaluateEntityHandle())
                        with
                        | _ ->
                            failwith $"Invalid cast for setter method definition handle. Handle kind: {setterHandle.Kind}\n Enclosing type: {fullyQualifiedName}"
                    metadataBuilder.AddMethodSemantics(
                        PropertyDefinitionHandle.op_Implicit(propDef.UnsafeLazilyEvaluateEntityHandle()), 
                        MethodSemanticsAttributes.Setter, 
                        setterHandle
                    )
                | _ ->
                    ()

                propDefHandle

            let propList =
                if propDefs.Count = 0 then
                    MetadataTokens.PropertyDefinitionHandle(asmBuilder.PropertyDefinitionRowCount + 1)
                else
                    addProperty propDefs[0]

            for i = 1 to propDefs.Count - 1 do
                addProperty propDefs[i] |> ignore
                
            if propDefs.Count > 0 then
                metadataBuilder.AddPropertyMap(tyDefHandle, propList)

            // ---------------------------------------------------------------------------------------------------

            tyDefHandle
        )
        