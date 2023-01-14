module Oly.Runtime.Clr.Emitter

open System
open Oly.Core
open Oly.Platform.Clr.Metadata
open System.Reflection
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns

[<RequireQualifiedAccess>]
type ClrMethodSpecialKind =
    | None
    | TypeOf
    | FunctionPointer
    | PInvoke

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type ClrTypeInfo = 
    | TypeGenericInstance of ClrTypeInfo * tyArgs: ClrTypeHandle imarray * appliedTyHandle: ClrTypeHandle
    | TypeReference of assembly: ClrAssemblyBuilder * ClrTypeHandle * isReadOnly: bool * isStruct: bool
    | TypeDefinition of assembly: ClrAssemblyBuilder * ClrTypeDefinitionBuilder * isReadOnly: bool * isInterface: bool * enumBaseTyOpt: ClrTypeInfo option * typeExtensionInfo: (ClrTypeInfo * ClrTypeInfo * ClrFieldHandle) voption * isStruct: bool * isClosure: bool

    member this.FullyQualifiedName =
        match this with
        | TypeDefinition(_, tyDefBuilder, _, _, _, _, _, _) ->
            tyDefBuilder.FullyQualifiedName
        | TypeReference(_, tyHandle, _, _) ->
            tyHandle.FullyQualifiedName
        | TypeGenericInstance(formalTy, _, _) ->
            formalTy.FullyQualifiedName

    member this.IsDefinitionEnum =
        match this with
        | TypeDefinition(isReadOnly=isReadOnly;enumBaseTyOpt=enumBaseTyOpt) -> enumBaseTyOpt.IsSome
        | _ -> false

    member this.TryGetEnumBaseType() =
        match this with
        | TypeDefinition(enumBaseTyOpt=enumBaseTyOpt) -> enumBaseTyOpt
        | _ -> None

    member this.IsReadOnly =
        match this with
        | TypeGenericInstance(x, _, _) -> x.IsReadOnly
        | TypeReference(isReadOnly=isReadOnly)
        | TypeDefinition(isReadOnly=isReadOnly) -> isReadOnly

    member this.IsStruct =
        match this with
        | TypeGenericInstance(x, _, _) -> x.IsStruct
        | TypeReference(isStruct=isStruct)
        | TypeDefinition(isStruct=isStruct) -> isStruct

    member this.IsByRefOfTypeVariable =
        match this with
        | TypeReference(_, handle, _, _) ->
            handle.IsByRef_t &&
            match handle.TryElementType with
            | ValueSome elementHandle -> elementHandle.IsVariable
            | _ -> false
        | _ ->
            false

    member this.IsByRefOfStruct =
        match this with
        | TypeReference(_, handle, _, _) ->
            handle.IsByRef_t &&
            match handle.TryElementType with
            | ValueSome elementHandle -> elementHandle.IsValueType
            | _ -> false
        | _ ->
            false

    member this.IsByRef =
        match this with
        | TypeReference(_, handle, _, _) ->
            handle.IsByRef_t
        | _ ->
            false

    member this.TryByRefElementType =
        match this with
        | TypeReference(asmBuilder, handle, _, _) ->
            match handle.TryElementType with
            | ValueSome elementHandle -> ValueSome(ClrTypeInfo.TypeReference(asmBuilder, elementHandle, false, false))
            | _ -> ValueNone
        | _ ->
            ValueNone

    member this.TryTypeVariable =
        match this with
        | TypeReference(asmBuilder, handle, _, _) ->
            handle.TryTypeVariable
        | _ ->
            ValueNone

    member this.IsClosure =
        match this with
        | TypeGenericInstance(x, _, _) -> x.IsClosure
        | TypeReference _ -> false
        | TypeDefinition(isClosure=isClosure) -> isClosure

    member this.IsTypeDefinitionInterface =
        match this with
        | TypeDefinition(isInterface=isInterface) -> isInterface
        | _ -> false

    member this.Handle =
        match this with
        | TypeGenericInstance(_, _, x) -> x
        | TypeReference(_, handle, _, _) -> handle
        | TypeDefinition(_, tyDefBuilder, _, _, _, _, _, _) -> tyDefBuilder.Handle

    member this.TypeArguments: ClrTypeHandle imarray =
        match this with
        | TypeGenericInstance(tyArgs=tyArgs) -> tyArgs
        | _ -> ImArray.empty

    member this.TryTypeExtensionType =
        match this with
        | TypeDefinition(_, _, _, _, _, typeExtensionTypeOpt, _, _) -> typeExtensionTypeOpt |> ValueOption.map (fun (x) -> x)
        | _ -> ValueNone

    member this.Assembly =
        match this with
        | TypeGenericInstance(x, _, _) -> x.Assembly
        | TypeReference(assembly=asm)
        | TypeDefinition(assembly=asm) -> asm

[<ReferenceEquality;NoComparison>]
type ClrFieldInfo = 
    { 
        handle: ClrFieldHandle
        isMutable: bool } with

    member this.Handle: ClrFieldHandle = this.handle

and [<ReferenceEquality;NoComparison>] ClrMethodInfo =
    {
        isEnclosingClosure: bool
        enclosingTyHandle: ClrTypeHandle
        handle: ClrMethodHandle
        builder: ClrMethodDefinitionBuilder option
        name: string
        isStatic: bool
        isConstructor: bool
        returnTy: ClrTypeInfo
        pars: (string * ClrTypeInfo) imarray
        tyInst: ClrTypeInfo imarray
        specialKind: ClrMethodSpecialKind
        tyParCount: int
    }

    member this.IsStatic = this.isStatic
    member this.IsInstance = not this.IsStatic

    member this.ReturnType = this.returnTy

    member this.Parameters = this.pars

module rec ClrCodeGen =

    type cenv =
        {
            assembly: ClrAssemblyBuilder
            emitTailCalls: bool
            buffer: ResizeArray<ClrInstruction>
            locals: System.Collections.Generic.Dictionary<int, ClrTypeInfo>
            dups: System.Collections.Generic.HashSet<int>
            mutable localCount: int ref
            mutable nextLabelId: int32 ref
        }

        member this.NewBuffer() =
            { this with
                buffer = ResizeArray()
            }

        member this.NewLocal(ty: ClrTypeInfo) =
            let localIndex = this.localCount.contents
            this.localCount.contents <- this.localCount.contents + 1
            this.locals.[localIndex] <- ty
            localIndex

        member this.NewLabel() =
            let labelId = this.nextLabelId.contents
            this.nextLabelId.contents <- this.nextLabelId.contents + 1
            labelId

    type env =
        {
            isInWhileLoop: bool
            isReturnable: bool
        }

    let getPrimitiveTypeCode cenv (ty: ClrTypeInfo) =
        if ty.IsDefinitionEnum then
            getPrimitiveTypeCode cenv (ty.TryGetEnumBaseType().Value)
        else

        if not ty.Handle.HasEntityHandle then
            PrimitiveTypeCode.Object

        elif ty.Handle = cenv.assembly.TypeReferenceByte then
            PrimitiveTypeCode.Byte

        elif ty.Handle = cenv.assembly.TypeReferenceSByte then
            PrimitiveTypeCode.SByte

        elif ty.Handle = cenv.assembly.TypeReferenceUInt16 then
            PrimitiveTypeCode.UInt16

        elif ty.Handle = cenv.assembly.TypeReferenceInt16 then
            PrimitiveTypeCode.Int16

        elif ty.Handle = cenv.assembly.TypeReferenceUInt32 then
            PrimitiveTypeCode.UInt32

        elif ty.Handle = cenv.assembly.TypeReferenceInt32 then
            PrimitiveTypeCode.Int32

        elif ty.Handle = cenv.assembly.TypeReferenceUInt64 then
            PrimitiveTypeCode.UInt64

        elif ty.Handle = cenv.assembly.TypeReferenceInt64 then
            PrimitiveTypeCode.Int64

        elif ty.Handle = cenv.assembly.TypeReferenceSingle then
            PrimitiveTypeCode.Single

        elif ty.Handle = cenv.assembly.TypeReferenceDouble then
            PrimitiveTypeCode.Double

        elif ty.Handle = cenv.assembly.TypeReferenceChar then
            PrimitiveTypeCode.Char

        elif ty.Handle = cenv.assembly.TypeReferenceString then
            PrimitiveTypeCode.String

        elif ty.Handle = cenv.assembly.TypeReferenceBoolean then
            PrimitiveTypeCode.Boolean

        elif ty.Handle = cenv.assembly.TypeReferenceIntPtr then
            PrimitiveTypeCode.IntPtr

        elif ty.Handle = cenv.assembly.TypeReferenceUIntPtr then
            PrimitiveTypeCode.UIntPtr

        elif ty.Handle = cenv.assembly.TypeReferenceVoid then
            PrimitiveTypeCode.Void

        // TODO: Handle TypedReferenced? maybe we don't really need it...

        else
            PrimitiveTypeCode.Object

    let getInstructionPosition cenv =
        cenv.buffer.Count

    let emitInstruction (cenv: cenv) (instr: ClrInstruction) =
        cenv.buffer.Add(instr)

    let emitFixupInstruction cenv =
        let fixupIndex = cenv.buffer.Count
        cenv.buffer.Add(Unchecked.defaultof<_>)
        fixupIndex

    let emitInstructions cenv instrs =
        cenv.buffer.AddRange(instrs)

    let emitConv cenv tyCode =
        match tyCode with
        | PrimitiveTypeCode.UIntPtr ->
            ClrInstruction.Conv_u |> emitInstruction cenv
        | PrimitiveTypeCode.Byte ->
            ClrInstruction.Conv_u1 |> emitInstruction cenv
        | PrimitiveTypeCode.UInt16 ->
            ClrInstruction.Conv_u2 |> emitInstruction cenv
        | PrimitiveTypeCode.UInt32 ->
            ClrInstruction.Conv_u4 |> emitInstruction cenv
        | PrimitiveTypeCode.UInt64 ->
            ClrInstruction.Conv_u8 |> emitInstruction cenv

        | PrimitiveTypeCode.IntPtr ->
            ClrInstruction.Conv_i |> emitInstruction cenv
        | PrimitiveTypeCode.SByte ->
            ClrInstruction.Conv_i1 |> emitInstruction cenv
        | PrimitiveTypeCode.Int16 ->
            ClrInstruction.Conv_i2 |> emitInstruction cenv
        | PrimitiveTypeCode.Int32 ->
            ClrInstruction.Conv_i4 |> emitInstruction cenv
        | PrimitiveTypeCode.Int64 ->
            ClrInstruction.Conv_i8 |> emitInstruction cenv

        | PrimitiveTypeCode.Single ->
            ClrInstruction.Conv_r4 |> emitInstruction cenv
        | PrimitiveTypeCode.Double ->
            ClrInstruction.Conv_r8 |> emitInstruction cenv

        | _ -> () // possible unsafe cast

    let emitConvForOp cenv ty =
        let tyCode = getPrimitiveTypeCode cenv ty
        match tyCode with
        | PrimitiveTypeCode.Byte
        | PrimitiveTypeCode.SByte
        | PrimitiveTypeCode.UInt16
        | PrimitiveTypeCode.Int16 ->
            emitConv cenv tyCode
        | PrimitiveTypeCode.UInt32 ->
            ClrInstruction.Conv_i4 |> emitInstruction cenv
        | _ ->
            ()

    let GenAnonymousFunctionInvoke cenv (argTys: ClrTypeInfo imarray) (returnTy: ClrTypeInfo) =
        cenv.assembly.AddAnonymousFunctionInvoke(argTys |> Seq.map (fun x -> x.Handle) |> ImArray.ofSeq, returnTy.Handle)

    let GenOperation (cenv: cenv) prevEnv (irOp: O<ClrTypeInfo, _, _>) =
        let env = { prevEnv with isReturnable = false }
        match irOp with
        | O.LoadFunction(irFunc: OlyIRFunction<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>, receiverExpr, _) ->
            let parTys = irFunc.EmittedFunction.Parameters.RemoveAt(0) |> ImArray.map (fun (_, x) -> x.Handle)
            let returnTy = irFunc.EmittedFunction.ReturnType.Handle

            GenExpression cenv env receiverExpr
            emitInstruction cenv (ClrInstruction.Ldftn(irFunc.EmittedFunction.handle))
            ClrInstruction.Newobj(cenv.assembly.AddAnonymousFunctionConstructor(parTys, returnTy), parTys.Length) |> emitInstruction cenv

        | O.CallStaticConstructor _ ->
            // .NET already handles static constructor invocation.
            // No need to do it ourselves; just simply skip it.
            ()

        | O.LoadArrayLength(irReceiver, rank, _) ->
            GenExpression cenv env irReceiver
            if rank > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                emitInstruction cenv ClrInstruction.Ldlen

        | O.LoadArrayElement(irReceiver, irIndexArgs, resultTy) ->
            GenExpression cenv env irReceiver
            if irIndexArgs.Length > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                GenExpression cenv env irIndexArgs[0]
                emitInstruction cenv (ClrInstruction.Ldelem resultTy.Handle)

        | O.LoadArrayElementAddress(irReceiver, irIndexArgs, _, resultTy) ->
            GenExpression cenv env irReceiver
            if irIndexArgs.Length > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                GenExpression cenv env irIndexArgs[0]
                match resultTy.TryByRefElementType with
                | ValueSome resultTy ->
                    emitInstruction cenv (ClrInstruction.Ldelema resultTy.Handle)
                | _ ->
                    failwith "assert"

        | O.StoreArrayElement(irReceiver, irIndexArgs, irRhsArg, _) ->
            let tyHandle =
                match irReceiver.ResultType.Handle.TryElementType with
                | ValueSome ty -> ty
                | _ -> failwith "Expecting a type with an element."

            GenExpression cenv env irReceiver
            if irIndexArgs.Length > 1 then
                failwith "clr emit rank greater than zero not yet supported."
            else
                GenExpression cenv env irIndexArgs[0]
                GenExpression cenv env irRhsArg
                emitInstruction cenv (ClrInstruction.Stelem tyHandle)

        | O.BitwiseNot(irArg, resultTy) ->
            GenExpression cenv env irArg
            emitInstruction cenv ClrInstruction.Not
            emitConvForOp cenv resultTy

        | O.BitwiseOr(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            emitInstruction cenv ClrInstruction.Or
            emitConvForOp cenv resultTy

        | O.BitwiseExclusiveOr(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            emitInstruction cenv ClrInstruction.Xor
            emitConvForOp cenv resultTy

        | O.BitwiseAnd(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            emitInstruction cenv ClrInstruction.And
            emitConvForOp cenv resultTy

        | O.BitwiseShiftLeft(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            emitInstruction cenv ClrInstruction.Shl
            emitConvForOp cenv resultTy

        | O.BitwiseShiftRight(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2

            let ty = getPrimitiveTypeCode cenv resultTy
            match ty with
            | PrimitiveTypeCode.Byte
            | PrimitiveTypeCode.UInt16
            | PrimitiveTypeCode.UInt32
            | PrimitiveTypeCode.UInt64
            | PrimitiveTypeCode.UIntPtr ->
                emitInstruction cenv ClrInstruction.Shr_un
            | _ ->
                emitInstruction cenv ClrInstruction.Shr  
            emitConvForOp cenv resultTy

        | O.Witness(irBody, witnessTy, resultTy) ->
            GenExpression cenv env irBody

            match witnessTy with
            | ClrTypeInfo.TypeDefinition(typeExtensionInfo=ValueSome(_, instanceTyInfo, fieldHandle)) ->
                let ctorHandle =
                    match instanceTyInfo with
                    | ClrTypeInfo.TypeDefinition(_, instanceTyDefBuilder, _, _, _, _, _, _) ->
                        if irBody.ResultType.Handle.IsByRef_t then
                            match instanceTyDefBuilder.TryGetSingleByRefConstructor() with
                            | Some(ctorDefBuilder) -> ctorDefBuilder.Handle
                            | _ -> failwith "Expected a single constructor."
                        else
                            match instanceTyDefBuilder.TryGetSingleNonByRefConstructor() with
                            | Some(ctorDefBuilder) -> ctorDefBuilder.Handle
                            | _ -> failwith "Expected a single constructor."
                    | _ ->
                        failwith "Expected a type definition."
                emitInstruction cenv (ClrInstruction.Newobj(ctorHandle, 1))
            | _ ->
                failwith "Expected a type definition."

        | O.Upcast(irArg, resultTy) ->
            GenExpression cenv env irArg

            // TODO/REVIEW: Should we include this for up-casting?
            //if irArg.ResultType.IsStruct && not resultTy.IsStruct then
            //    emitInstruction cenv (ClrInstruction.Box(irArg.ResultType.Handle))

        | O.Box(irArg, _) ->
            GenExpression cenv env irArg
            emitInstruction cenv (ClrInstruction.Box(irArg.ResultType.Handle))

        | O.Unbox(irArg, resultTy) ->
            GenExpression cenv env irArg
            emitInstruction cenv (ClrInstruction.Unbox_any(resultTy.Handle))

        | O.Print(E.Operation(op=O.Box(irArg, _)), _) when irArg.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ->
            GenExpression cenv env irArg
            ClrInstruction.Call(cenv.assembly.ConsoleWriteMethod_Int32.Value, 1) |> emitInstruction cenv

        | O.Print(E.Operation(op=O.Upcast(irArg, _)), _) when irArg.ResultType.Handle = cenv.assembly.TypeReferenceString ->
            GenExpression cenv env irArg
            ClrInstruction.Call(cenv.assembly.ConsoleWriteMethod_String.Value, 1) |> emitInstruction cenv

        | O.Print(irArg, _) ->
            GenExpression cenv env irArg
            ClrInstruction.Call(cenv.assembly.ConsoleWriteMethod.Value, 1) |> emitInstruction cenv

        | O.Cast(irArg, resultTy) ->
            GenExpression cenv env irArg

            let castFromTy = irArg.ResultType |> getPrimitiveTypeCode cenv
            let castToTy = resultTy |> getPrimitiveTypeCode cenv
            
            match castFromTy with
            | PrimitiveTypeCode.Object
            | PrimitiveTypeCode.TypedReference
            | PrimitiveTypeCode.String
            | PrimitiveTypeCode.Void -> () // unsafe cast
            | PrimitiveTypeCode.Byte
            | PrimitiveTypeCode.UInt16
            | PrimitiveTypeCode.UInt32
            | PrimitiveTypeCode.UInt64 
            | PrimitiveTypeCode.UIntPtr ->
                match castToTy with
                | PrimitiveTypeCode.Single ->
                    ClrInstruction.Conv_r_un |> emitInstruction cenv
                    ClrInstruction.Conv_r4 |> emitInstruction cenv
                | PrimitiveTypeCode.Double ->
                    ClrInstruction.Conv_r_un |> emitInstruction cenv
                    ClrInstruction.Conv_r4 |> emitInstruction cenv
                | _ ->
                    emitConv cenv castToTy
            | _ ->
                emitConv cenv castToTy     

        | O.Throw(irArg, _) ->
            GenExpression cenv env irArg
            ClrInstruction.Throw |> emitInstruction cenv

        | O.Ignore(irArg, _) ->
            GenExpression cenv env irArg

            match irArg with
            | E.Operation(op=O.Throw _) -> () // do not emit a pop for a Throw.
            | _ ->
                ClrInstruction.Pop |> emitInstruction cenv

        | O.Add(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            ClrInstruction.Add |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Subtract(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            ClrInstruction.Sub |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Multiply(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            ClrInstruction.Mul |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Divide(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            ClrInstruction.Div |> emitInstruction cenv
            emitConvForOp cenv resultTy
        | O.Remainder(irArg1, irArg2, resultTy) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            ClrInstruction.Rem |> emitInstruction cenv
            emitConvForOp cenv resultTy

        | O.Not(irArg, _) ->
            GenExpression cenv env irArg
            ClrInstruction.LdcI4 0 |> emitInstruction cenv
            ClrInstruction.Ceq |> emitInstruction cenv

        | O.Negate(irArg, _) ->
            GenExpression cenv env irArg
            ClrInstruction.Neg |> emitInstruction cenv

        | O.Equal(irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            ClrInstruction.Ceq |> emitInstruction cenv
        | O.NotEqual(irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            ClrInstruction.Ceq |> emitInstruction cenv
            ClrInstruction.LdcI4 0 |> emitInstruction cenv
            ClrInstruction.Ceq |> emitInstruction cenv

        | O.Utf16Equal(irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            let methHandle = cenv.assembly.String_Equals.Value
            ClrInstruction.Call(methHandle, 2) |> emitInstruction cenv

        | O.GreaterThan(irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            if irArg1.ResultType.Handle = cenv.assembly.TypeReferenceSByte ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt16 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt64 then
                ClrInstruction.Cgt |> emitInstruction cenv
            else
                ClrInstruction.Cgt_un |> emitInstruction cenv
        | O.GreaterThanOrEqual(irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            if irArg1.ResultType.Handle = cenv.assembly.TypeReferenceSByte ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt16 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt64 then
                ClrInstruction.Clt |> emitInstruction cenv
                ClrInstruction.LdcI4 0 |> emitInstruction cenv
                ClrInstruction.Ceq |> emitInstruction cenv
            else
                ClrInstruction.Clt_un |> emitInstruction cenv
                ClrInstruction.LdcI4 0 |> emitInstruction cenv
                ClrInstruction.Ceq |> emitInstruction cenv
        | O.LessThan(irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            if irArg1.ResultType.Handle = cenv.assembly.TypeReferenceSByte ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt16 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt64 then
                ClrInstruction.Clt |> emitInstruction cenv
            else
                ClrInstruction.Clt_un |> emitInstruction cenv
        | O.LessThanOrEqual(irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            if irArg1.ResultType.Handle = cenv.assembly.TypeReferenceSByte ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt16 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 ||
               irArg1.ResultType.Handle = cenv.assembly.TypeReferenceInt64 then               
                ClrInstruction.Cgt |> emitInstruction cenv
                ClrInstruction.LdcI4 0 |> emitInstruction cenv
                ClrInstruction.Ceq |> emitInstruction cenv
            else
                ClrInstruction.Cgt_un |> emitInstruction cenv
                ClrInstruction.LdcI4 0 |> emitInstruction cenv
                ClrInstruction.Ceq |> emitInstruction cenv

        | O.LoadRefCellContents(irArg, resultTy) ->
            GenExpression cenv env irArg
            ClrInstruction.LdcI4 0 |> emitInstruction cenv
            ClrInstruction.Ldelem(resultTy.Handle) |> emitInstruction cenv

        | O.LoadTupleElement(irArg, index, _) ->
            GenExpression cenv env irArg
            let methHandle =
                match irArg.ResultType with
                | ClrTypeInfo.TypeReference(asmBuilder, tyHandle, _, _) ->
                    match tyHandle with
                    | ClrTypeHandle.TypeSpecification(tyRefHandle=formalTyHandle) ->
                        let methHandle =
                            if formalTyHandle = asmBuilder.``TypeReferenceTuple`2`` then
                                asmBuilder.``Tuple`2_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`3`` then
                                asmBuilder.``Tuple`3_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`4`` then
                                asmBuilder.``Tuple`4_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`5`` then
                                asmBuilder.``Tuple`5_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`6`` then
                                asmBuilder.``Tuple`6_ItemMethods``.Value[index]
                            elif formalTyHandle = asmBuilder.``TypeReferenceTuple`7`` then
                                asmBuilder.``Tuple`7_ItemMethods``.Value[index]
                            else
                                failwith "Invalid LoadTupleItem"
                        asmBuilder.CreateMethodSpecification(tyHandle, methHandle, ImArray.empty)
                    | _ ->
                        failwith "Invalid LoadTupleItem"
                | _ ->
                    failwith "Invalid LoadTupleItem"
            ClrInstruction.Call(methHandle, 1) |> emitInstruction cenv

        | O.StoreRefCellContents(irArg1, irArg2, _) ->
            let tyHandle =
                match irArg1.ResultType.Handle.TryElementType with
                | ValueSome ty -> ty
                | _ -> failwith "Expecting a type with an element."

            GenExpression cenv env irArg1
            ClrInstruction.LdcI4 0 |> emitInstruction cenv
            GenExpression cenv env irArg2
            ClrInstruction.Stelem(tyHandle) |> emitInstruction cenv

        | O.Store(n, irArg1, _) ->
            GenExpression cenv env irArg1
            ClrInstruction.Stloc n |> emitInstruction cenv

        | O.StoreArgument(n, irArg1, _) ->
            GenExpression cenv env irArg1
            ClrInstruction.Starg n |> emitInstruction cenv

        | O.StoreToAddress(irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            let ty = irArg2.ResultType
            if ty.Handle = cenv.assembly.TypeReferenceInt32 then
                ClrInstruction.Stind_i4 |> emitInstruction cenv
            elif ty.Handle = cenv.assembly.TypeReferenceInt64 then
                ClrInstruction.Stind_i8 |> emitInstruction cenv
            elif ty.Handle = cenv.assembly.TypeReferenceUInt64 then
                ClrInstruction.Stind_i8 |> emitInstruction cenv
            else
                if irArg2.ResultType.Handle.IsNamed then
                    ClrInstruction.Stobj(irArg2.ResultType.Handle) |> emitInstruction cenv
                else
                    ClrInstruction.StindRef |> emitInstruction cenv

        | O.StoreField(irField: OlyIRField<_, _, ClrFieldInfo>, irArg1, irArg2, _) ->
            GenExpression cenv env irArg1
            GenExpression cenv env irArg2
            ClrInstruction.Stfld irField.EmittedField.Handle |> emitInstruction cenv

        | O.StoreStaticField(irField, irArg, _) ->
            GenExpression cenv env irArg
            ClrInstruction.Stsfld irField.EmittedField.Handle |> emitInstruction cenv

        | O.LoadFromAddress(irArg, returnTy) ->
            GenExpression cenv env irArg
            if returnTy.IsStruct then
                if returnTy.Handle = cenv.assembly.TypeReferenceInt32 then
                    ClrInstruction.Ldind_i4 |> emitInstruction cenv
                elif returnTy.Handle = cenv.assembly.TypeReferenceInt64 then
                    ClrInstruction.Ldind_i8 |> emitInstruction cenv
                elif returnTy.Handle = cenv.assembly.TypeReferenceUInt64 then
                    ClrInstruction.Ldind_i8 |> emitInstruction cenv
                else
                    ClrInstruction.Ldobj(returnTy.Handle) |> emitInstruction cenv
            else
                ClrInstruction.LdindRef |> emitInstruction cenv

        | O.LoadField(irField, irArg, _) ->
            GenExpression cenv env irArg
            ClrInstruction.Ldfld irField.EmittedField.Handle |> emitInstruction cenv

        | O.LoadFieldAddress(irField, irArg, _, _) ->
            GenExpression cenv env irArg
            ClrInstruction.Ldflda irField.EmittedField.Handle |> emitInstruction cenv

        | O.NewTuple(itemTys, irArgs, _) ->
            irArgs |> ImArray.iter (fun x -> GenExpression cenv env x)
        
            let tyArgs =
                itemTys
                |> ImArray.map (fun x -> x.Handle)

            ClrInstruction.Newobj(cenv.assembly.AddTupleConstructor(tyArgs), irArgs.Length) |> emitInstruction cenv
            
        | O.NewMutableArray(elementTy, irSizeArgExpr, _) ->
            GenExpression cenv env irSizeArgExpr
            ClrInstruction.Newarr(elementTy.Handle) |> emitInstruction cenv

        | O.NewArray(elementTy, _irKind, irArgExprs, _) ->
            ClrInstruction.LdcI4 (irArgExprs.Length) |> emitInstruction cenv
            ClrInstruction.Newarr(elementTy.Handle) |> emitInstruction cenv
            for i = 0 to irArgExprs.Length - 1 do
                ClrInstruction.Dup |> emitInstruction cenv
                ClrInstruction.LdcI4 i |> emitInstruction cenv
                GenExpression cenv env irArgExprs[i]
                ClrInstruction.Stelem(elementTy.Handle) |> emitInstruction cenv

        | O.NewRefCell(contentTy, irArg, _) ->
            ClrInstruction.LdcI4 1 |> emitInstruction cenv
            ClrInstruction.Newarr(contentTy.Handle) |> emitInstruction cenv
            ClrInstruction.Dup |> emitInstruction cenv
            ClrInstruction.LdcI4 0 |> emitInstruction cenv
            GenExpression cenv env irArg
            ClrInstruction.Stelem(contentTy.Handle) |> emitInstruction cenv

        | O.New(irFunc, irArgs, _) ->
            GenNew cenv env irFunc.EmittedFunction irArgs

        | O.Call(irFunc, irArgs, _) ->
            GenCall cenv env prevEnv.isReturnable irFunc.EmittedFunction irArgs false

        | O.CallVirtual(irFunc, irArgs, _) ->
            GenCall cenv env prevEnv.isReturnable irFunc.EmittedFunction irArgs true

        | O.CallIndirect(runtimeArgTys, irFunArg, irArgs, runtimeResultTy) ->
            GenCallIndirect cenv env irFunArg irArgs runtimeArgTys runtimeResultTy

    let GenValue cenv env (irValue: V<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        match irValue with
        | V.Unit _ -> ClrInstruction.Ldnull |> emitInstruction cenv
        | V.Null _ -> ClrInstruction.Ldnull |> emitInstruction cenv
        | V.Constant(irConstant, _) ->
            match irConstant with
            | C.True _ -> ClrInstruction.LdcI4(1) |> emitInstruction cenv
            | C.False _ -> ClrInstruction.LdcI4(0) |> emitInstruction cenv
            | C.Int8(v) -> ClrInstruction.LdcI4(int32 v) |> emitInstruction cenv
            | C.UInt8(v) -> 
                ClrInstruction.LdcI4(int32 v) |> emitInstruction cenv
            | C.Int16(v) -> ClrInstruction.LdcI4(int32 v) |> emitInstruction cenv
            | C.UInt16(v) -> 
                ClrInstruction.LdcI4(int32 v) |> emitInstruction cenv
            | C.Int32(v) -> ClrInstruction.LdcI4(v) |> emitInstruction cenv
            | C.UInt32(v) -> 
                ClrInstruction.LdcI4(int32 v) |> emitInstruction cenv
            | C.Int64(v) -> ClrInstruction.LdcI8(v) |> emitInstruction cenv
            | C.UInt64(v) -> 
                ClrInstruction.LdcI8(int64 v) |> emitInstruction cenv
            | C.Float32(v) -> ClrInstruction.LdcR4(v) |> emitInstruction cenv
            | C.Float64(v) -> ClrInstruction.LdcR8(v) |> emitInstruction cenv
            | C.Char16(v) -> ClrInstruction.LdcI4(int32 v) |> emitInstruction cenv
            | C.Utf16(v) -> ClrInstruction.Ldstr v |> emitInstruction cenv
            | C.Array _ ->
                raise(System.NotImplementedException())
            | C.External _ ->
                raise(System.NotImplementedException())
            | C.Variable _ ->
                raise(System.NotSupportedException())

        | V.StaticField(irField, _) ->
            ClrInstruction.Ldsfld irField.EmittedField.Handle |> emitInstruction cenv

        | V.StaticFieldAddress(irField, _, _) ->
            ClrInstruction.Ldsflda irField.EmittedField.Handle |> emitInstruction cenv

        | V.Local(n, _) ->
            if cenv.dups.Contains(n) |> not then
                ClrInstruction.Ldloc n |> emitInstruction cenv

        | V.LocalAddress(n, _, _) ->
            ClrInstruction.Ldloca n |> emitInstruction cenv

        | V.Argument(n, _) ->
            ClrInstruction.Ldarg n |> emitInstruction cenv

        | V.ArgumentAddress(n, _, _) ->
            ClrInstruction.Ldarga n |> emitInstruction cenv

        | V.FunctionPtr(methInfo, _) ->
            emitInstruction cenv (ClrInstruction.Ldftn(methInfo.handle))

        | V.Function(methInfo, _) ->
            let parTys = methInfo.Parameters |> ImArray.map (fun (_, x) -> x.Handle)
            let returnTy = methInfo.ReturnType.Handle

            ClrInstruction.Ldnull |> emitInstruction cenv
            emitInstruction cenv (ClrInstruction.Ldftn(methInfo.handle))
            ClrInstruction.Newobj(cenv.assembly.AddAnonymousFunctionConstructor(parTys, returnTy), parTys.Length) |> emitInstruction cenv

        | V.Default(ty) ->
            if ty.Handle = cenv.assembly.TypeReferenceByte      ||
               ty.Handle = cenv.assembly.TypeReferenceSByte     ||
               ty.Handle = cenv.assembly.TypeReferenceUInt16    ||
               ty.Handle = cenv.assembly.TypeReferenceInt16     ||
               ty.Handle = cenv.assembly.TypeReferenceUInt32    ||
               ty.Handle = cenv.assembly.TypeReferenceInt32     || 
               ty.Handle = cenv.assembly.TypeReferenceChar      ||
               ty.Handle = cenv.assembly.TypeReferenceBoolean   then
                    ClrInstruction.LdcI4(0) |> emitInstruction cenv
            elif ty.Handle = cenv.assembly.TypeReferenceUInt64 ||
                 ty.Handle = cenv.assembly.TypeReferenceInt64 then
                    ClrInstruction.LdcI8(0) |> emitInstruction cenv
            elif ty.Handle = cenv.assembly.TypeReferenceSingle then
                    ClrInstruction.LdcR4(0.0f) |> emitInstruction cenv
            elif ty.Handle = cenv.assembly.TypeReferenceDouble then
                    ClrInstruction.LdcR8(0.0) |> emitInstruction cenv
            elif ty.IsStruct then
                match ty.Handle with
                | ClrTypeHandle.NativePointer _
                | ClrTypeHandle.FunctionPointer _ ->
                    ClrInstruction.LdcI4(0) |> emitInstruction cenv
                    ClrInstruction.Conv_u |> emitInstruction cenv
                | _ ->
                    let localIndex = cenv.NewLocal(ty)
                    emitInstruction cenv (ClrInstruction.Ldloca(localIndex))
                    emitInstruction cenv (ClrInstruction.Initobj(ty.Handle))
                    emitInstruction cenv (ClrInstruction.Ldloc(localIndex))
            else
                ClrInstruction.Ldnull |> emitInstruction cenv

    let canTailCall cenv (func: ClrMethodInfo) =
        cenv.emitTailCalls && 
      //  func.ReturnType.Handle <> cenv.assembly.TypeReferenceVoid && 
        not(func.Parameters |> ImArray.exists (fun (_, x) -> x.IsByRef)) &&
        not func.ReturnType.IsByRef

    let GenCall cenv env isReturnable (func: ClrMethodInfo) irArgs isVirtual =
        match func.specialKind with
        | ClrMethodSpecialKind.FunctionPointer ->
            raise(System.NotImplementedException("Clr FunctionPointer"))
        | _ ->

        irArgs |> ImArray.iter (fun x -> GenExpression cenv env x)
        match func.specialKind with
        | ClrMethodSpecialKind.None ->
            if func.IsStatic || not isVirtual then
                if isReturnable && canTailCall cenv func then
                    ClrInstruction.Tail |> emitInstruction cenv
                ClrInstruction.Call(func.handle, irArgs.Length) |> emitInstruction cenv
            else
                let argTy0 = irArgs.[0].ResultType
                if isVirtual && func.IsInstance then
                    if argTy0.IsByRefOfTypeVariable || (argTy0.IsByRefOfStruct && (not func.enclosingTyHandle.IsValueType || (func.enclosingTyHandle = cenv.assembly.TypeReferenceValueType))) then
                        match argTy0.TryByRefElementType with
                        | ValueSome(elementTy) ->
                            ClrInstruction.Constrained(elementTy.Handle) |> emitInstruction cenv
                        | _ ->
                            failwith "Expected by-ref element type."
                ClrInstruction.Callvirt(func.handle, irArgs.Length) |> emitInstruction cenv

        | ClrMethodSpecialKind.TypeOf ->
            ClrInstruction.Ldtoken func.tyInst.[0].Handle |> emitInstruction cenv
            ClrInstruction.Call(cenv.assembly.GetTypeFromHandleMethod.Value, 1) |> emitInstruction cenv

        | _ ->
            failwith "Invalid special method."

    let GenNew cenv env (func: ClrMethodInfo) irArgs =
        irArgs |> ImArray.iter (fun x -> GenExpression cenv env x)

        ClrInstruction.Newobj(func.handle, irArgs.Length) |> emitInstruction cenv

    let GenCallIndirect (cenv: cenv) env (irFunArg: E<ClrTypeInfo, _, _>) (irArgs: E<_, _, _> imarray) (runtimeArgTys: ClrTypeInfo imarray) (runtimeReturnTy: ClrTypeInfo) =
        match irFunArg.ResultType.Handle with
        | ClrTypeHandle.FunctionPointer(cc, parTys, returnTy) ->            
            let localIndex = cenv.NewLocal(irFunArg.ResultType)
            GenExpression cenv env irFunArg
            ClrInstruction.Stloc localIndex |> emitInstruction cenv
            irArgs |> ImArray.iter (fun x -> GenExpression cenv env x)
            ClrInstruction.Ldloc localIndex |> emitInstruction cenv
            ClrInstruction.Calli(cc, parTys, returnTy) |> emitInstruction cenv
        | _ ->
            GenExpression cenv env irFunArg
            irArgs |> ImArray.iter (fun x -> GenExpression cenv env x)

            let argTys =
                runtimeArgTys
                |> Seq.map (fun x -> 
                    if x.IsByRef then
                        cenv.assembly.TypeReferenceIntPtr
                    else
                        x.Handle
                )
                |> ImArray.ofSeq

            let methRef = cenv.assembly.AddAnonymousFunctionInvoke(argTys, runtimeReturnTy.Handle)
            ClrInstruction.Callvirt(methRef, irArgs.Length) |> emitInstruction cenv

    /// Builds a new instruction buffer and returns it.
    let NewGenBranchPath (cenv: cenv) env irExpr =
        let cenvNew = cenv.NewBuffer()

        GenExpression cenvNew env irExpr

        cenvNew.buffer

    let (|EqualExpr|_|) irExpr =
        match irExpr with
        | E.Operation(irTextRange, O.Equal(arg1, arg2, resultTy)) ->
            Some(irTextRange, arg1, arg2, resultTy)
        | _ ->
            None

    let GenExpressionAux cenv env (irExpr: E<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
        match irExpr with
        | E.None _ ->
            () // Nop

        | E.Let(_, n, irRhsExpr, irBodyExpr) ->
            let hasNoDup = cenv.dups.Contains(n) |> not
            
            if hasNoDup then
                let rhsExprTy = irRhsExpr.ResultType
                cenv.locals.[n] <- rhsExprTy
            GenExpression cenv { env with isReturnable = false } irRhsExpr
            if hasNoDup then
                ClrInstruction.Stloc n |> emitInstruction cenv
            else
                ClrInstruction.Dup |> emitInstruction cenv

            GenExpression cenv env irBodyExpr

        | E.Value(_, irValue) ->
            GenValue cenv env irValue

        | E.Operation(_, irOp) ->
            GenOperation cenv env irOp

        | E.Sequential(irExpr1, irExpr2) ->
            GenExpression cenv { env with isReturnable = false } irExpr1
            GenExpression cenv env irExpr2

        | E.IfElse(conditionExpr, trueExpr, falseExpr, _) ->
            let falseLabelId = cenv.NewLabel()

            let truePath = NewGenBranchPath cenv env trueExpr
            let falsePath = NewGenBranchPath cenv env falseExpr

            let mustBranch =
                falsePath.Count > 0 && 
                not (match falsePath.[falsePath.Count - 1] with ClrInstruction.Ret -> true | _ -> false)

            let contLabelIdOpt =
                if mustBranch then
                    cenv.NewLabel() |> ValueSome
                else
                    ValueNone

            let andExpr arg1 arg2 resultTy =
                E.IfElse(arg1, arg2, E.Value(NoRange, V.Constant(C.False, resultTy)), resultTy)

            let orExpr arg1 arg2 resultTy =
                E.IfElse(arg1, E.Value(NoRange, V.Constant(C.True, resultTy)), arg2, resultTy)

            let canUseIntrinsicEquality (value1: V<ClrTypeInfo, _, _>) (value2: V<ClrTypeInfo, _, _>) =
                value1.ResultType.Handle = cenv.assembly.TypeReferenceInt32 &&
                value2.ResultType.Handle = cenv.assembly.TypeReferenceInt32

            let rec reduce (conditionExpr: E<ClrTypeInfo, _, _>) =
                match conditionExpr with
                // Makes optimizations easier
                | And(And(argx1, argx2, _), arg2, resultTy) ->
                    andExpr argx1 (andExpr argx2 arg2 resultTy) resultTy
                    |> reduce

                // Makes optimizations easier
                | Or(Or(argx1, argx2, _), arg2, resultTy) ->
                    orExpr argx1 (orExpr argx2 arg2 resultTy) resultTy
                    |> reduce

                //| And(E.Operation(_, O.Equal(argx1, argx2, _)), arg2, _) ->
                //    match argx1, argx2 with
                //    // TODO: There is some code duplication that we can cleanup.
                //    | E.Value(_, value1), E.Value(_, value2) 
                //        when canUseIntrinsicEquality value1 value2 ->
                    
                //        GenExpression cenv { env with isReturnable = false } argx1
                //        GenExpression cenv { env with isReturnable = false } argx2

                //        ClrInstruction.Bne_un(falseLabelId) |> emitInstruction cenv
                //        reduce arg2
                //    | _ ->
                //        GenExpression cenv { env with isReturnable = false } conditionExpr
                //        ClrInstruction.Brfalse(falseLabelId) |> emitInstruction cenv

                //| E.Operation(_, O.Equal(arg1, arg2, _)) ->                        
                //    match arg1, arg2 with
                //    | E.Value(_, value1), E.Value(_, value2) 
                //        when canUseIntrinsicEquality value1 value2  ->
                    
                //        GenExpression cenv { env with isReturnable = false } arg1
                //        GenExpression cenv { env with isReturnable = false } arg2

                //        ClrInstruction.Bne_un(falseLabelId) |> emitInstruction cenv
                //    | _ ->
                //        GenExpression cenv { env with isReturnable = false } conditionExpr
                //        ClrInstruction.Brfalse(falseLabelId) |> emitInstruction cenv

                | _ ->
                    GenExpression cenv { env with isReturnable = false } conditionExpr
                    ClrInstruction.Brfalse(falseLabelId) |> emitInstruction cenv

            reduce conditionExpr
            emitInstructions cenv truePath 
            
            match contLabelIdOpt with
            | ValueSome(contLabelId) ->
                ClrInstruction.Br(contLabelId) |> emitInstruction cenv
            | _ ->
                ()

            ClrInstruction.Label(falseLabelId) |> emitInstruction cenv
            emitInstructions cenv falsePath

            match contLabelIdOpt with
            | ValueSome(contLabelId) ->
                ClrInstruction.Label(contLabelId) |> emitInstruction cenv
            | _ ->
                ()

        | E.While(predicateExpr, bodyExpr, _) ->
            let envLoop = 
                { env with 
                    isInWhileLoop = true
                    isReturnable = false }

            let loopStartLabelId = cenv.NewLabel()
            let loopEndLabelId = cenv.NewLabel()

            let predicatePath = NewGenBranchPath cenv envLoop predicateExpr
            let bodyPath = NewGenBranchPath cenv envLoop bodyExpr

            ClrInstruction.Label(loopStartLabelId) |> emitInstruction cenv

            emitInstructions cenv predicatePath
            emitInstruction cenv (ClrInstruction.Brfalse loopEndLabelId)

            emitInstructions cenv bodyPath
            emitInstruction cenv (ClrInstruction.Br loopStartLabelId)

            ClrInstruction.Label(loopEndLabelId) |> emitInstruction cenv

    let GenExpression cenv env irExpr =
        GenExpressionAux cenv env irExpr
        if env.isReturnable then
            if cenv.buffer.Count > 0 && cenv.buffer.[cenv.buffer.Count - 1] = ClrInstruction.Ret then ()
            else
                ClrInstruction.Ret |> emitInstruction cenv


let createMethod (enclosingTy: ClrTypeInfo) (flags: OlyIRFunctionFlags) methodName tyPars cilParameters (cilReturnTy: ClrTypeHandle) isStatic isCtor (tyDefBuilder: ClrTypeDefinitionBuilder) =
    let methDefBuilder = tyDefBuilder.CreateMethodDefinitionBuilder(methodName, tyPars, cilParameters, cilReturnTy, not isStatic)

    // TODO: maybe we should not emit 'virtual final newslot' for some members?

    let methAttrs =
        if flags.IsPublic then
            MethodAttributes.Public
        elif flags.IsProtected then
            MethodAttributes.Family
        else
            if flags.IsExported then
                MethodAttributes.Private
            else
                MethodAttributes.Assembly

    let methAttrs = methAttrs ||| MethodAttributes.HideBySig

    let methAttrs =
        if isStatic then
            methAttrs ||| MethodAttributes.Static
        else
            methAttrs

    let methAttrs =
        if isCtor then
            methAttrs ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName
        else
            methAttrs

    let methAttrs =
        if flags.IsVirtual && not isStatic then
            methAttrs ||| MethodAttributes.Virtual
        else
            methAttrs

    let methAttrs =
        if flags.IsAbstract && not isStatic && not(flags.IsInstance && flags.AreGenericsErased && enclosingTy.IsTypeDefinitionInterface) then
            methAttrs ||| MethodAttributes.Abstract
        else
            methAttrs

    let methAttrs =
        if flags.IsNewSlot && not isStatic then
            methAttrs ||| MethodAttributes.NewSlot
        else
            methAttrs

    let methAttrs =
        if flags.IsFinal && not isStatic then
            methAttrs ||| MethodAttributes.Final ||| MethodAttributes.Virtual
        else
            methAttrs

    methDefBuilder.Attributes <- methAttrs
    methDefBuilder.ImplementationAttributes <- MethodImplAttributes.IL ||| MethodImplAttributes.Managed

    if flags.IsAbstract && isStatic then
        // We do this as it's a stub for a static abstract method.
        methDefBuilder.BodyInstructions <-
            [
                ClrInstruction.Ldnull
                ClrInstruction.Throw
                ClrInstruction.Ret
            ]

    methDefBuilder

type BlobBuilder with

    member b.WriteSerializedUTF8(value: string) =
        b.WriteByte(byte value.Length)
        b.WriteUTF8(value)

    member b.WriteTypeOfC(asmBuilder: ClrAssemblyBuilder, x: C<ClrTypeInfo, _>) =
        match x with
        | C.Int8 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceByte)
        | C.UInt8 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceSByte)
        | C.Int16 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceInt16)
        | C.UInt16 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceUInt16)
        | C.Int32 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceInt32)
        | C.UInt32 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceUInt32)
        | C.Int64 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceInt64)
        | C.UInt64 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceUInt64)
        | C.Float32 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceSingle)
        | C.Float64 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceDouble)

        | C.True _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceBoolean)

        | C.False _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceBoolean)

        | C.Array(elementTy, _) ->
            let mutable encoder = CustomAttributeArrayTypeEncoder(b)
            if elementTy.Handle = asmBuilder.TypeReferenceObject then
                encoder.ObjectArray()
            else
                asmBuilder.EncodeAttributeElementType(encoder.ElementType(), elementTy.Handle)

        | C.Char16 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceChar)

        | C.Utf16 _ -> 
            let mutable encoder = SignatureTypeEncoder(b)
            asmBuilder.EncodeType(encoder, asmBuilder.TypeReferenceString)

        | C.Variable _ ->
            raise(System.NotSupportedException("constant variable"))

        | C.External(func) -> 
            match func.specialKind with
            | ClrMethodSpecialKind.TypeOf ->
                if func.ReturnType.IsStruct then
                    ClrElementTypes.ValueType
                    |> b.WriteByte
                else
                    ClrElementTypes.Class
                    |> b.WriteByte
            | ClrMethodSpecialKind.FunctionPointer ->
                SignatureTypeCode.FunctionPointer
                |> byte
                |> b.WriteByte
            | ClrMethodSpecialKind.PInvoke ->
                raise(System.NotImplementedException())
            | _ ->
                SignatureTypeCode.Invalid
                |> byte
                |> b.WriteByte

    member b.WriteValueOfC(x, asCountedUtf8) =
        match x with
        | C.UInt8(value) -> b.WriteByte(value)
        | C.Int8(value) -> b.WriteSByte(value)
        | C.UInt16(value) -> b.WriteUInt16(value)
        | C.Int16(value) -> b.WriteInt16(value)
        | C.UInt32(value) -> b.WriteUInt32(value)
        | C.Int32(value) -> b.WriteInt32(value)
        | C.UInt64(value) -> b.WriteUInt64(value)
        | C.Int64(value) -> b.WriteInt64(value)
        | C.Float32(value) -> b.WriteSingle(value)
        | C.Float64(value) -> b.WriteDouble(value)
        | C.Char16(value) -> b.WriteUInt16(uint16 value)
        | C.Utf16(value) -> 
            if asCountedUtf8 then
                b.WriteSerializedUTF8(value)
            else
                b.WriteSerializedString(value)
        | C.True -> b.WriteBoolean(true)
        | C.False -> b.WriteBoolean(false)
        | C.Array(ty: ClrTypeInfo, elements) ->
            b.WriteUInt32(uint32 elements.Length)
            elements
            |> ImArray.iter (fun x -> b.WriteValueOfC(x, false))
        | C.Variable _ ->
            raise(System.NotSupportedException("constant variable"))
        | C.External(func) -> 
            match func.specialKind with
            | ClrMethodSpecialKind.TypeOf when func.tyInst.Length = 1 ->
                let ty = func.tyInst.[0]
                b.WriteSerializedString(ty.FullyQualifiedName)
            | _ ->
                failwith "Invalid external constant."

[<Sealed>]
type OlyRuntimeClrEmitter(assemblyName, isExe, primaryAssembly, consoleAssembly) =

    let asmBuilder = ClrAssemblyBuilder(assemblyName, isExe, primaryAssembly, consoleAssembly)

    let getEnclosingInfo (enclosing: Choice<string imarray, ClrTypeInfo>) =
        match enclosing with
        | Choice1Of2 path ->
            let namespac = path |> String.concat "."
            ClrTypeHandle.Empty, namespac
        | Choice2Of2(ty) ->
            ty.Handle, String.Empty

    let transformName (name: string) (tyParCount: int) =
        if tyParCount > 0 then
            name + "`" + tyParCount.ToString()
        else
            name

    let newUniqueId =
        let i = ref 0L
        fun () -> System.Threading.Interlocked.Increment i

    let newUniquePrivateTypeName () =
        "__oly_private_type_" + string (newUniqueId ())

    let voidTy = ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceVoid, false, false)

    let writeAttrArgs (irArgs: C<ClrTypeInfo, ClrMethodInfo> imarray) (irNamedArgs: OlyIRAttributeNamedArgument<ClrTypeInfo, ClrMethodInfo> imarray) =
        let b = BlobBuilder()

        // Prolog
        b.WriteByte(1uy)
        b.WriteByte(0uy)

        irArgs
        |> ImArray.iter (fun x -> b.WriteValueOfC(x, false))

        // NumNamed
        let numNamed = irNamedArgs.Length
        b.WriteByte(byte numNamed)
        b.WriteByte(0uy)
        
        irNamedArgs
        |> ImArray.iter (fun { Kind = kind; Name = name; Constant = x } ->
            match kind with
            | OlyIRAttributeNamedArgumentKind.Property ->
                b.WriteByte(byte 0x54) // PROPERTY
            | OlyIRAttributeNamedArgumentKind.Field ->
                b.WriteByte(byte 0x53) // FIELD

            b.WriteTypeOfC(asmBuilder, x)
            b.WriteSerializedUTF8(name)
            b.WriteValueOfC(x, true)
        )

        asmBuilder.AddBlob(b)

    member this.Write(stream) =
        asmBuilder.Write(stream)

    interface IOlyRuntimeEmitter<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo> with

        member this.EmitTypeBaseAttribute() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceAttribute, false, false)

        member this.EmitTypeBaseStructEnum() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceEnum, false, false)

        member this.EmitTypeBaseStruct() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceValueType, false, false)

        member this.EmitTypeArray(elementTy: ClrTypeInfo, rank, _): ClrTypeInfo =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddArrayType(elementTy.Handle, rank), false, false)  

        member this.EmitTypeNativeInt(): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceIntPtr, false, true)

        member this.EmitTypeNativePtr(elementTy: ClrTypeInfo): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddNativePointer(elementTy.Handle), false, true)

        member this.EmitTypeNativeFunctionPtr(ilCallConv, parTys, returnTy): ClrTypeInfo =
            let parTys =
                parTys
                |> ImArray.map (fun x -> x.Handle)
            let returnTy =
                returnTy.Handle

            let callConv =
                if ilCallConv.HasFlag(OlyILCallingConvention.Blittable) then
                    SignatureCallingConvention.Unmanaged
                else
                    SignatureCallingConvention.Default

            let callConv =
                if ilCallConv.HasFlag(OlyILCallingConvention.CDecl) then
                    callConv ||| SignatureCallingConvention.CDecl
                elif ilCallConv.HasFlag(OlyILCallingConvention.StdCall) then
                    callConv ||| SignatureCallingConvention.StdCall
                elif ilCallConv.HasFlag(OlyILCallingConvention.ThisCall) then
                    callConv ||| SignatureCallingConvention.ThisCall
                elif ilCallConv.HasFlag(OlyILCallingConvention.FastCall) then
                    callConv ||| SignatureCallingConvention.FastCall
                elif ilCallConv.HasFlag(OlyILCallingConvention.VarArgs) then
                    callConv ||| SignatureCallingConvention.VarArgs
                else
                    callConv

            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddFunctionPointer(callConv, parTys, returnTy), false, true)

        member this.EmitTypeNativeUInt(): ClrTypeInfo = 
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceUIntPtr, false, true)

        member this.EmitTypeByRef(ty, byRefKind) =
            match byRefKind with
            | OlyIRByRefKind.ReadWrite ->
                ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateByRef(ty.Handle), false, true)     
            | OlyIRByRefKind.Read ->
                // TODO: Add this modreq -> ClrTypeHandle.ModReq(asmBuilder.tr_IsReadOnlyAttribute, ClrTypeHandle.CreateByRef(ty.Handle))
                //       Not necessary or required, but it would be nice as it gives more info about the byref in metadata.
                //       We do not do it now because we are not stripping the modreqs in all the places.
                ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateByRef(ty.Handle), true, true)     

        member this.EmitTypeRefCell(ty) =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddArrayType(ty.Handle, 1), false, false)

        member _.EmitTypeVariable(index, irKind) =
            let kind =
                if irKind = OlyIRTypeVariableKind.Type then
                    ClrTypeVariableKind.Type
                else
                    ClrTypeVariableKind.Method
            ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateVariable(index, kind), false, false)

        member _.EmitTypeHigherVariable(_, _, _) =
            raise (System.NotSupportedException("CLR does not support higher kinds."))

        member this.EmitTypeBaseObject() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceObject, false, false)

        member this.EmitTypeVoid() =
            voidTy

        member this.EmitTypeUnit() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceObject, false, false)

        member this.EmitTypeInt8() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceSByte, true, true)

        member this.EmitTypeUInt8() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceByte, true, true)

        member this.EmitTypeInt16() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceInt16, true, true)

        member this.EmitTypeUInt16() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceUInt16, true, true)

        member this.EmitTypeInt32() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceInt32, true, true)

        member this.EmitTypeUInt32() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceUInt32, true, true)

        member this.EmitTypeInt64() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceInt64, true, true)

        member this.EmitTypeUInt64() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceUInt64, true, true)

        member this.EmitTypeFloat32() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceSingle, true, true)

        member this.EmitTypeFloat64() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceDouble, true, true)

        member this.EmitTypeBool() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceBoolean, true, true)

        member this.EmitTypeChar16() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceChar, true, true)

        member this.EmitTypeUtf16() =
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.TypeReferenceString, true, false)

        member this.EmitTypeTuple(tyInst, _) =   
            let tyInst =
                tyInst 
                |> ImArray.map (fun x -> x.Handle)
            ClrTypeInfo.TypeReference(asmBuilder, asmBuilder.AddTupleType(tyInst), true, false)

        member this.EmitTypeConstantInt32 _ = raise (System.NotSupportedException())

        member this.EmitTypeFunction(inputTys, outputTy) =
            let inputTyRefs =
                inputTys
                |> ImArray.map (fun x -> 
                    if x.IsByRef then
                        asmBuilder.TypeReferenceIntPtr
                    else
                        x.Handle
                )

            let handle, tyInst = asmBuilder.AddAnonymousFunctionType(inputTyRefs, outputTy.Handle)
            ClrTypeInfo.TypeReference(asmBuilder, handle, true, false)

        member this.EmitExternalType(externalPlatform, externalPath, externalName, enclosing, kind, flags, name, tyParCount) =
            let isStruct = kind = OlyILEntityKind.Struct || kind = OlyILEntityKind.Enum
            let isReadOnly = flags &&& OlyIRTypeFlags.ReadOnly = OlyIRTypeFlags.ReadOnly

            let isNestedInExternalType =
                match enclosing with
                | Choice1Of2 _ -> false
                | Choice2Of2(enclosingTy) ->
                    match enclosingTy with
                    | ClrTypeInfo.TypeDefinition _ -> false
                    | ClrTypeInfo.TypeGenericInstance _ -> true
                    | _ -> true

            let enclosingTyHandle, namespac =
                if isNestedInExternalType then
                    match enclosing with
                    | Choice2Of2(enclosingTy) ->
                        enclosingTy.Handle, ""
                    | _ ->
                        failwith "Expected an enclosing type."
                else
                    let path = externalPath |> String.concat "."
                    let asmFullName = externalPlatform.Replace("CLR:", "")
                    let asmName = AssemblyName(asmFullName)
                    asmBuilder.AddAssemblyReference(asmName), path

            let enclosingTyHandle =
                match enclosingTyHandle with
                | ClrTypeHandle.TypeSpecification(tyRefHandle=tyRefHandle) -> tyRefHandle
                | _ -> enclosingTyHandle

            let tyHandle = asmBuilder.AddTypeReference(enclosingTyHandle, namespac, externalName, isStruct)

            let tyHandle =
                if tyParCount > 0 then
                    let tyArgs =
                        ImArray.init tyParCount (fun i -> ClrTypeHandle.CreateVariable(i, ClrTypeVariableKind.Type))
                    asmBuilder.AddGenericInstanceTypeReference(tyHandle, tyArgs)
                else
                    tyHandle

            ClrTypeInfo.TypeReference(asmBuilder, tyHandle, isReadOnly, isStruct)

        member this.EmitTypeGenericInstance(ty, tyArgs) =
            let tyArgHandles =
                tyArgs 
                |> ImArray.map (fun x -> x.Handle)
            let appliedTyHandle = asmBuilder.AddGenericInstanceTypeReference(ty.Handle, tyArgHandles)
            ClrTypeInfo.TypeGenericInstance(ty, tyArgHandles, appliedTyHandle)

        member this.EmitTypeDefinition(enclosing, kind, flags, name, irTyPars, inherits, implements, irAttrs) =
            let name =
                if flags &&& OlyIRTypeFlags.GenericsErased = OlyIRTypeFlags.GenericsErased then
                    name + newUniquePrivateTypeName()
                else
                    name
            let name = transformName name irTyPars.Length

            let isStruct = kind = OlyILEntityKind.Struct
            let isEnum = kind = OlyILEntityKind.Enum
            let enumBaseTyOpt =
                if isEnum then
                    Some inherits[0]
                else
                    None
            let isNewtype = kind = OlyILEntityKind.Newtype
            let isReadOnly = flags &&& OlyIRTypeFlags.ReadOnly = OlyIRTypeFlags.ReadOnly
            let isInterface = kind = OlyILEntityKind.Interface
            let isTypeExtension = kind = OlyILEntityKind.TypeExtension
            let isAttribute = kind = OlyILEntityKind.Attribute

            let enumBaseTyOpt =
                if isEnum then
                    inherits[0] |> Some
                else
                    None

            let isAnyStruct =
                isStruct ||
                (match enumBaseTyOpt with Some ty -> ty.IsStruct | _ -> false)

            let inherits =
                // TODO: This assumes enum is an int32, handle other cases.
                if isEnum || isNewtype then ImArray.empty
                else inherits

            let isExported = flags.HasFlag(OlyIRTypeFlags.Exported)

            let enclosingTyHandle, namespac = getEnclosingInfo enclosing

            let tyPars =
                irTyPars
                |> ImArray.map (fun x -> x.Name)
            let tyDefBuilder = 
                if isEnum then
                    asmBuilder.CreateEnumTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyPars)
                else
                    asmBuilder.CreateTypeDefinitionBuilder(enclosingTyHandle, namespac, name, tyPars, isStruct)

            irAttrs
            |> ImArray.iter (fun x ->
                match x with
                | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                    asmBuilder.AddTypeAttribute(tyDefBuilder.Handle, ctor.handle, writeAttrArgs irArgs irNamedArgs)
            )

            let inherits =
                if kind = OlyILEntityKind.Closure then
                    ImArray.empty
                else
                    inherits

            let isStructLayoutSequential =
                irAttrs
                |> ImArray.exists (fun x ->
                    match x with
                    | OlyIRAttribute(ctor, irArgs, irNamedArgs) 
                            when irArgs.Length = 1 && 
                               //  ctor.name = ".ctor" &&
                                 ctor.enclosingTyHandle.IsNamed &&
                                 ctor.enclosingTyHandle.FullyQualifiedName.StartsWith("System.Runtime.InteropServices.StructLayoutAttribute") ->
                        let irArg = irArgs[0]
                        // TODO: Handle others.
                        match irArg with
                        | C.Int16(0s) -> true
                        | _ -> false
                    | _ ->
                        false
                )

            let tyAttrs = 
                if isAnyStruct then
                    if isStructLayoutSequential then
                        TypeAttributes.SequentialLayout ||| TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit
                    else
                        TypeAttributes.AutoLayout ||| TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit
                elif isInterface then
                    tyDefBuilder.BaseType <- ClrTypeHandle.Empty
                    TypeAttributes.Interface ||| TypeAttributes.Abstract
                elif isAttribute then
                    tyDefBuilder.BaseType <- asmBuilder.TypeReferenceAttribute
                    TypeAttributes.Class ||| TypeAttributes.BeforeFieldInit
                else
                    TypeAttributes.Class ||| TypeAttributes.BeforeFieldInit

            let tyAttrs =
                match enclosing with
                | Choice1Of2 _ ->
                    if isExported then
                        tyAttrs ||| TypeAttributes.Public
                    else
                        tyAttrs
                | _ ->
                    if isExported then
                        tyAttrs ||| TypeAttributes.NestedPublic
                    else
                        tyAttrs ||| TypeAttributes.NestedFamORAssem

            tyDefBuilder.Attributes <- tyAttrs

            match enumBaseTyOpt with
            | Some enumBaseTy ->
                tyDefBuilder.AddFieldDefinition(
                    FieldAttributes.Public ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName,
                    "value__",
                    enumBaseTy.Handle,
                    None
                )
                |> ignore
            | _ ->
                ()

            if isInterface then
                if not inherits.IsEmpty then
                    tyDefBuilder.InterfaceImplementations <- inherits |> ImArray.map (fun x -> x.Handle)
            else
                if inherits.Length > 1 && not isTypeExtension then
                    failwith "Multiple inheritance not supported on the CLR."

                if inherits.Length = 1 && not isTypeExtension then
                    tyDefBuilder.BaseType <- inherits.[0].Handle

                if not implements.IsEmpty && not isTypeExtension then
                    tyDefBuilder.InterfaceImplementations <- implements |> ImArray.map (fun x -> x.Handle)

            if isTypeExtension then
                let extendedTy = inherits |> Seq.exactlyOne
                // ** INSTANCE GENERATION **

                let instanceTyDefBuilder = asmBuilder.CreateTypeDefinitionBuilder(tyDefBuilder.Handle, namespac, "__oly_instance", tyPars, false)
                instanceTyDefBuilder.Attributes <- TypeAttributes.Class ||| TypeAttributes.NestedPublic
                if not implements.IsEmpty then
                    instanceTyDefBuilder.InterfaceImplementations <- implements |> ImArray.map (fun x -> x.Handle)

                let ctorParTyHandle =
                    if extendedTy.IsStruct then
                        ClrTypeHandle.CreateByRef(extendedTy.Handle)
                    else
                        extendedTy.Handle
                let methDefBuilder = instanceTyDefBuilder.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, ImArray.createOne("", ctorParTyHandle), asmBuilder.TypeReferenceVoid, true)
                let methAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

                let methAttrs = methAttrs ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName

                let instanceFieldHandle = instanceTyDefBuilder.AddFieldDefinition(FieldAttributes.Private, "contents", extendedTy.Handle, None)

                methDefBuilder.Attributes <- methAttrs
                methDefBuilder.ImplementationAttributes <- MethodImplAttributes.IL ||| MethodImplAttributes.Managed
                methDefBuilder.Locals <- ImArray.empty
                methDefBuilder.BodyInstructions <-
                    [
                        ClrInstruction.Ldarg 0
                        ClrInstruction.Ldarg 1
                        if extendedTy.IsStruct then
                            ClrInstruction.Ldobj(extendedTy.Handle)
                        ClrInstruction.Stfld instanceFieldHandle
                        ClrInstruction.Ret
                    ]

                if extendedTy.IsStruct then
                    // Add second constructor if the extending type is a struct.
                    // The second constructor is the same as the first except it doesn't take a byref.
                    let methDefBuilder = instanceTyDefBuilder.CreateMethodDefinitionBuilder(".ctor", ImArray.empty, ImArray.createOne("", extendedTy.Handle), asmBuilder.TypeReferenceVoid, true)
                    let methAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

                    let methAttrs = methAttrs ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName

                    methDefBuilder.Attributes <- methAttrs
                    methDefBuilder.ImplementationAttributes <- MethodImplAttributes.IL ||| MethodImplAttributes.Managed
                    methDefBuilder.Locals <- ImArray.empty
                    methDefBuilder.BodyInstructions <-
                        [
                            ClrInstruction.Ldarg 0
                            ClrInstruction.Ldarg 1
                            ClrInstruction.Stfld instanceFieldHandle
                            ClrInstruction.Ret
                        ]

                let instanceTyInfo =
                    ClrTypeInfo.TypeDefinition(asmBuilder, instanceTyDefBuilder, isReadOnly, false, None, ValueNone, false, false)

                // *********************************
                ClrTypeInfo.TypeDefinition(asmBuilder, tyDefBuilder, isReadOnly, isInterface, enumBaseTyOpt, ValueSome(extendedTy, instanceTyInfo, instanceFieldHandle), isAnyStruct, false)
            else
                ClrTypeInfo.TypeDefinition(asmBuilder, tyDefBuilder, isReadOnly, isInterface, enumBaseTyOpt, ValueNone, isAnyStruct, kind = OlyILEntityKind.Closure)

        member this.EmitField(enclosingTy, flags: OlyIRFieldFlags, name: string, fieldTy: ClrTypeInfo, irAttrs, irConstValueOpt): ClrFieldInfo = 
            let isStatic = flags.IsStatic

            let fieldTyHandle =
                if enclosingTy.IsClosure && fieldTy.IsByRef then
                    asmBuilder.TypeReferenceIntPtr
                else
                    fieldTy.Handle

            match enclosingTy with
            | ClrTypeInfo.TypeDefinition(_, tyDefBuilder, _, isInterface, _, _, _, _) ->
                let attrs = 
                    if flags.IsPublic then
                        FieldAttributes.Public
                    elif flags.IsProtected then
                        FieldAttributes.Family
                    else
                        if flags.IsExported then
                            FieldAttributes.Private
                        else    
                            FieldAttributes.Assembly
                let attrs =
                    if isStatic then
                        attrs ||| FieldAttributes.Static
                    else
                        attrs

                let attrs =
                    if flags.IsMutable || enclosingTy.IsDefinitionEnum then
                        attrs
                    else
                        attrs ||| FieldAttributes.InitOnly

                let attrs =
                    if irConstValueOpt.IsSome then
                        attrs ||| FieldAttributes.Literal
                    else
                        attrs

                let constValueOpt =
                    irConstValueOpt
                    |> Option.map (fun irConstValue ->
                        match irConstValue with
                        | C.Int8(value=value) -> value :> obj
                        | C.UInt8(value=value) -> value :> obj
                        | C.Int16(value=value) -> value :> obj
                        | C.UInt16(value=value) -> value :> obj
                        | C.Int32(value=value) -> value :> obj
                        | C.UInt32(value=value) -> value :> obj
                        | C.Int64(value=value) -> value :> obj
                        | C.UInt64(value=value) -> value :> obj
                        | C.Float32(value=value) -> value :> obj
                        | C.Float64(value=value) -> value :> obj
                        | C.Array(_, elements) -> elements |> Array.ofSeq :> obj
                        | C.True -> true :> obj
                        | C.False -> false :> obj
                        | C.Char16(value=value) -> value :> obj
                        | C.Utf16(value=value) -> value :> obj
                        | C.Variable _ ->
                            raise(System.NotSupportedException("constant variable"))
                        | C.External _ -> failwith "Invalid constant in CLR Emitter."
                    )

                let fieldHandle = tyDefBuilder.AddFieldDefinition(attrs, name, fieldTyHandle, constValueOpt)

                irAttrs
                |> ImArray.iter (fun x ->
                    match x with
                    | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                        asmBuilder.AddFieldAttribute(fieldHandle, ctor.handle, writeAttrArgs irArgs irNamedArgs)
                )

                {
                    handle = fieldHandle
                    isMutable = flags.IsMutable
                }

            | ClrTypeInfo.TypeReference(_, handle, _, _) ->
                let fieldHandle = asmBuilder.AddFieldReference(handle, name, fieldTyHandle)
                {
                    handle = fieldHandle
                    isMutable = flags.IsMutable
                }

            | _ ->
                failwith "Invalid type info."

        member this.EmitFunctionInstance(enclosingTy, func: ClrMethodInfo, tyArgs: imarray<ClrTypeInfo>): ClrMethodInfo = 
            let newHandle =
                if enclosingTy.TypeArguments.IsEmpty && tyArgs.IsEmpty then
                    failwith "Expected type arguments."
                else
                    let tyArgHandles = tyArgs |> ImArray.map (fun x -> x.Handle)
                    if enclosingTy.Handle.EntityHandle.Equals(func.enclosingTyHandle.EntityHandle) then
                        asmBuilder.AddMethodSpecification(func.handle, tyArgHandles)
                    else
                        asmBuilder.CreateMethodSpecification(enclosingTy.Handle, func.handle, tyArgHandles)
            { func with handle = newHandle; tyInst = tyArgs }

        member this.EmitFunctionReference(enclosingTy, func: ClrMethodInfo): ClrMethodInfo =
            let newHandle =
                let parTys = func.Parameters.RemoveAt(0) |> ImArray.map (fun x -> (snd x).Handle)
                asmBuilder.AddMethodReference(
                    SignatureCallingConvention.Default, 
                    func.IsInstance, 
                    enclosingTy.Handle, 
                    func.name, 
                    func.tyParCount, 
                    parTys,
                    func.ReturnType.Handle
                )
            { func with handle = newHandle }

        member this.EmitFunctionDefinition(externalInfoOpt, enclosingTy, flags, name, tyPars, pars, returnTy, originalOverridesOpt, _, irAttrs) =
            let name =
                if flags.AreGenericsErased then
                    name + "__oly_erased_" + tyPars.Length.ToString()
                else
                    match externalInfoOpt with
                    | Some(externalInfo) ->
                        if externalInfo.Platform = "C" then
                            name
                        else
                            externalInfo.Name
                    | _ ->
                        if not flags.IsExported && returnTy.IsByRef && returnTy.IsReadOnly then
                            name + "__oly_read_only"
                        else
                            name
            let isCtor = flags.IsConstructor
            let isTypeExtension =
                match enclosingTy with
                | ClrTypeInfo.TypeDefinition(typeExtensionInfo=tyOpt) -> tyOpt.IsSome
                | _ -> false
            let isStatic = flags.IsStatic || isTypeExtension
            let overridesOpt =
                if isStatic then
                    None
                else
                    originalOverridesOpt

            let pars =
                pars
                |> ImArray.map (fun x -> (x.Name, x.Type))

            let pars =
                if isTypeExtension && not flags.IsStatic then
                    let cilTy =
                        let cilTy = enclosingTy
                        match cilTy.TryTypeExtensionType with
                        | ValueSome(cilTy, _, _) -> 
                            if cilTy.IsStruct then
                                ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateByRef(cilTy.Handle), false, false)
                            else
                                cilTy
                        | _ -> cilTy
                    Seq.append
                        (seq { "", cilTy })
                        pars
                    |> ImArray.ofSeq
                else
                    pars

            let parTys =
                pars
                |> ImArray.map (fun (_, x) -> x.Handle)

            let parTys =
                if enclosingTy.IsClosure then
                    parTys
                    |> ImArray.map (fun x ->
                        if x.IsByRef_t then
                            asmBuilder.TypeReferenceIntPtr
                        else
                            x
                    )
                else
                    parTys

            let parHandles =
                pars
                |> ImArray.map (fun (name, x) -> 
                    if x.IsByRef && enclosingTy.IsClosure then
                        (name, asmBuilder.TypeReferenceIntPtr)
                    else
                        (name, x.Handle)
                )

            let cilReturnTy = returnTy

            let methodName =
                if isCtor then
                    if flags.IsStatic then
                        ".cctor"
                    else
                        ".ctor"
                else
                    name

            let cilReturnTy2 =
                if isCtor then
                    voidTy
                else
                    cilReturnTy
                        
            let isInstance = not isStatic          

            let tyPars =
                tyPars
                |> Seq.map (fun x -> x.Name)
                |> ImArray.ofSeq
                
            let methRefHandle, methDefBuilderOpt =
                match enclosingTy with
                | ClrTypeInfo.TypeGenericInstance(_, _, enclosingTyHandle)
                | ClrTypeInfo.TypeReference(_, enclosingTyHandle, _, _) ->
                    let convention = SignatureCallingConvention.Default
                    asmBuilder.AddMethodReference(convention, isInstance, enclosingTyHandle, methodName, tyPars.Length, parTys, cilReturnTy2.Handle), None
                | ClrTypeInfo.TypeDefinition(_, tyDefBuilder, _, _, _, tyExtInfoOpt, _, _) ->
                    if flags.IsExternal && (externalInfoOpt.IsNone || externalInfoOpt.Value.Platform <> "C") then
                        ClrMethodHandle.None, None
                    else
                        let methDefBuilder = createMethod enclosingTy flags methodName tyPars parHandles cilReturnTy2.Handle isStatic isCtor tyDefBuilder
                        overridesOpt
                        |> Option.iter (fun x ->
                            methDefBuilder.Overrides <- Some x.handle
                        )

                        if isTypeExtension && not flags.IsStatic then
                            match tyExtInfoOpt with
                            | ValueSome(extendedTy, (ClrTypeInfo.TypeDefinition(_, instanceTyDefBuilder, _, _, _, _, _, _) as enclosingTy), instanceFieldHandle) ->
                                let instanceMethDefBuilder = createMethod enclosingTy flags methodName tyPars (parHandles.RemoveAt(0)) cilReturnTy2.Handle false false instanceTyDefBuilder
                                match originalOverridesOpt with
                                | Some overrides ->
                                   instanceMethDefBuilder.Overrides <- Some overrides.handle
                                | _ ->
                                    ()

                                if not flags.IsAbstract then
                                    instanceMethDefBuilder.BodyInstructions <-
                                        let ldargInstrs =
                                            Array.init 
                                                (pars.Length) 
                                                (fun i -> ClrInstruction.Ldarg(i))
                                            |> Array.collect (fun instr ->
                                                match instr with
                                                | ClrInstruction.Ldarg(0) ->
                                                    [|
                                                        instr
                                                        if extendedTy.IsStruct then
                                                            ClrInstruction.Ldflda(instanceFieldHandle)
                                                        else
                                                            ClrInstruction.Ldfld(instanceFieldHandle)
                                                    |]
                                                | _ ->
                                                    [|instr|]
                                            )

                                        Array.append
                                            ldargInstrs
                                            [|
                                                ClrInstruction.Call(methDefBuilder.Handle, ldargInstrs.Length)
                                                ClrInstruction.Ret
                                            |]
                            | _ ->
                                ()

                        if flags.IsInstance && flags.AreGenericsErased && enclosingTy.IsTypeDefinitionInterface then
                            // stub
                            methDefBuilder.BodyInstructions <-
                                [
                                    ClrInstruction.Ldnull
                                    ClrInstruction.Throw
                                    ClrInstruction.Ret
                                ]

                        if externalInfoOpt.IsSome && externalInfoOpt.Value.Platform = "C" then
                            //methDefBuilder.PInvokeInfo <-
                            //    irAttrs
                            //    |> ImArray.tryPick (fun x ->
                            //        match x with
                            //        | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                            //            match ctor.name with
                            //            | ".ctor" ->
                            //                if ctor.enclosingTyHandle.FullyQualifiedName.StartsWith("System.Runtime.InteropServices.DllImportAttribute,") && irArgs.Length > 0 then
                            //                    match irArgs[0] with
                            //                    | OlyIRConstant.Utf16(value) ->
                            //                        Some value
                            //                    | _ ->
                            //                        None
                            //                else
                            //                    None
                            //            | _ ->
                            //                None                                    
                            //    )
                            methDefBuilder.PInvokeInfo <- Some (externalInfoOpt.Value.Path[0], externalInfoOpt.Value.Name)
                            methDefBuilder.ImplementationAttributes <- methDefBuilder.ImplementationAttributes ||| MethodImplAttributes.PreserveSig
                            methDefBuilder.Attributes <- methDefBuilder.Attributes ||| MethodAttributes.PinvokeImpl

                        methDefBuilder.Handle, Some methDefBuilder

            let pars =
                if isStatic || isCtor then 
                    if isTypeExtension && not flags.IsStatic then
                        let enclosingTy =
                            match enclosingTy.TryTypeExtensionType with
                            | ValueSome(enclosingTy, _, _) -> enclosingTy
                            | _ -> enclosingTy
                        ImArray.createOne("this", enclosingTy).AddRange(pars)
                    else
                        pars
                else
                    if enclosingTy.IsStruct then
                        // Perhaps, the OlyRuntime can give all parameters even for instance functions instead of us
                        // having to do this manually.
                        let thisTy = ClrTypeInfo.TypeReference(asmBuilder, ClrTypeHandle.CreateByRef(enclosingTy.Handle), false, true)  
                        ImArray.createOne("this", thisTy).AddRange(pars)
                    else
                        ImArray.createOne("this", enclosingTy).AddRange(pars)

            match methDefBuilderOpt with
            | Some(methDefBuilder) ->
                let handle = methDefBuilder.Handle
                irAttrs
                |> ImArray.iter (fun x ->
                    match x with
                    | OlyIRAttribute(ctor, irArgs, irNamedArgs) ->
                        asmBuilder.AddMethodAttribute(handle, ctor.handle, writeAttrArgs irArgs irNamedArgs)
                )

                if flags.IsEntryPoint then
                    asmBuilder.EntryPoint <- methDefBuilder.Handle
            | _ ->
                ()

            let specialKind =
                match externalInfoOpt with
                | Some externalInfo when externalInfo.Platform = "intrinsic-CLR" ->
                    match externalInfo.Name with
                    | "typeof" when pars.Length = 0 && tyPars.Length = 1 ->
                        ClrMethodSpecialKind.TypeOf
                    | "__ldftn" ->
                        ClrMethodSpecialKind.FunctionPointer
                    | _ ->
                        failwith "Invalid CLR intrinsic"
                | _ ->
                    ClrMethodSpecialKind.None

            {
                isEnclosingClosure = enclosingTy.IsClosure
                enclosingTyHandle = enclosingTy.Handle
                handle = methRefHandle
                builder = methDefBuilderOpt
                name = methodName
                isStatic = isStatic
                isConstructor = isCtor
                returnTy = cilReturnTy // do not use cilReturnTy2 as it may be void due to constructor
                pars = pars
                tyInst = ImArray.empty
                specialKind = specialKind
                tyParCount = tyPars.Length
            }

        member this.EmitFunctionBody(irFuncBody, func) =
            let output = ResizeArray()

            let bodyResult = 
#if DEBUG
                // We do this in Debug to prevent stack overflows due to lack of tail calls.
                // It would be a fair amount of work to do so otherwise since we rely on recursive patterns.
                async { 
                    do! Async.SwitchToThreadPool()
                    return irFuncBody.Value 
                }
                |> Async.RunSynchronously
#else
                irFuncBody.Value
#endif

            let cenv = 
                {
                    assembly = asmBuilder
                    emitTailCalls = true
                    buffer = output
                    locals = System.Collections.Generic.Dictionary()
                    dups = System.Collections.Generic.HashSet()
                    localCount = ref bodyResult.LocalCount
                    nextLabelId = ref 0
                } : ClrCodeGen.cenv

            let env =
                {
                    isInWhileLoop = false
                    isReturnable = true
                } : ClrCodeGen.env

            ClrCodeGen.GenExpression cenv env bodyResult.Expression

            match func.builder with
            | Some methDefBuilder ->
                methDefBuilder.Locals <-
                    cenv.locals
                    |> Seq.sortBy (fun x -> x.Key)
                    |> Seq.map (fun x -> ClrLocal(x.Value.Handle))
                    |> ImArray.ofSeq
                methDefBuilder.BodyInstructions <- cenv.buffer
            | _ ->
                failwith "Expected method definition builder."

