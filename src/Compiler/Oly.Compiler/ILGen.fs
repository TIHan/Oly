module internal rec Oly.Compiler.Internal.ILGen

open System
open System.Collections.Generic
open System.Threading
open Oly.Core
open Oly.Metadata
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations

let private OlyILExpressionNone = OlyILExpression.None(OlyILDebugSourceTextRange.Empty)

type FunctionEnv =
    {
        mutable localNumber: int32
        mutable nextLocalArgument: int32
        arguments: Dictionary<int64, int32>
        scopedLocals: Dictionary<int64, OlyILLocal>
        ilLocals: imarrayb<OlyILLocal>
        localEntities: HashSet<int64>
    }

    static member Create() =
        {
            localNumber = 0
            nextLocalArgument = 0
            arguments = Dictionary()
            scopedLocals = Dictionary()
            localEntities = HashSet<int64>()
            ilLocals = ImArray.builder()
        }

let addLocalArgument funEnv (value: IValueSymbol) =
    funEnv.arguments.[value.Id] <- funEnv.nextLocalArgument
    funEnv.nextLocalArgument <- funEnv.nextLocalArgument + 1

let rec deterministicEnclosingName (enclosing: EnclosingSymbol) =
    match enclosing with
    | EnclosingSymbol.RootNamespace -> ""
    | EnclosingSymbol.Entity(ent) ->
        deterministicEnclosingName ent.Enclosing + "__oly_" + (deterministicEntityName ent)
    | EnclosingSymbol.Witness _ ->
        failwith "Unexpeced enclosing witness."
    | EnclosingSymbol.Local ->
        failwith "Unexpected enclosing local."

and deterministicEntityName (ent: EntitySymbol) =
    let enclosingName = deterministicEnclosingName ent.Enclosing
    if ent.IsAnonymous then
        let tyNameArgs =
            ent.TypeArguments
            |> ImArray.map (fun x ->
                deterministicTypeName x
            )
        enclosingName + (String.concat "__oly_" tyNameArgs)
    else
        enclosingName + "_" + ent.Name

and deterministicTypeName (ty: TypeSymbol) =
    match ty.TryEntity with
    | ValueSome ent -> deterministicEntityName ent
    | _ -> ty.Name

type cenv =
    {
        syntaxTree: OlySyntaxTree

        genNameNumber: int32 ref
        cachedStrings: Dictionary<string, OlyILStringHandle>
        cachedFuncSpecs: Dictionary<int64, OlyILFunctionSpecificationHandle>
        cachedFuncDefs: Dictionary<int64, OlyILFunctionDefinitionHandle>
        cachedEntRefs: Dictionary<int64, OlyILEntityReferenceHandle>
        cachedEntDefs: Dictionary<int64, OlyILEntityDefinitionHandle>
        cachedFieldDefs: Dictionary<int64, OlyILFieldDefinitionHandle>
        cachedDbgSrcs: Dictionary<OlyPath, OlyILDebugSourceHandle>

        extraFuncDefs: Dictionary<int64, ResizeArray<OlyILFunctionDefinitionHandle>>

        delayedEntityGenQueue: Queue<unit -> unit>

        assembly: OlyILAssembly
        mutable funEnv: FunctionEnv
    }

    member this.NewFunction() =
        { this with
            funEnv = FunctionEnv.Create()
        }

    member this.GenerateName() =
        let newId = this.genNameNumber.contents
        this.genNameNumber.contents <- this.genNameNumber.contents + 1
        "__oly_gen_" + string newId

    static member Create(syntaxTree, asm) =
        {
            syntaxTree = syntaxTree

            genNameNumber = ref 0
            cachedStrings = Dictionary()
            cachedFuncSpecs = Dictionary()
            cachedFuncDefs = Dictionary()
            cachedEntRefs = Dictionary()
            cachedEntDefs = Dictionary()
            cachedFieldDefs = Dictionary()
            cachedDbgSrcs = Dictionary(OlyPathEqualityComparer.Instance)

            extraFuncDefs = Dictionary()

            delayedEntityGenQueue = Queue()

            assembly = asm
            funEnv = FunctionEnv.Create()
        }

let emitPathAsILDebugSourceCached cenv (path: OlyPath) =
    match cenv.cachedDbgSrcs.TryGetValue path with
    | true, ilHandle -> ilHandle
    | _ ->
        let ilHandle = cenv.assembly.AddDebugSource(OlyILDebugSource(path))
        cenv.cachedDbgSrcs[path] <- ilHandle
        ilHandle

let emitTextRange cenv (syntaxNode: OlySyntaxNode) =
    let textRange = syntaxNode.GetTextRange(CancellationToken.None)
    let syntaxTree = syntaxNode.Tree
    let s = textRange.Start
    let e = textRange.End
    OlyILDebugSourceTextRange(emitPathAsILDebugSourceCached cenv syntaxTree.Path, s.Line, s.Column, e.Line, e.Column)

[<RequireQualifiedAccess>]
type LocalContext =
    | Namespace of string seq
    | Entity of EntitySymbol

    member this.RealEntity =
        match this with
        | Entity(ent) -> ent
        | Namespace _ -> failwith "Namespace is not an entity."

type env =
    {
        context: LocalContext
        isReturnable: bool
        isInInstance: bool
        isInConstructor: bool
        isInArg: bool
        locals: System.Collections.Immutable.ImmutableHashSet<int64>
    }

    static member Create() =
        {
            context = LocalContext.Namespace Seq.empty
            isReturnable = false
            isInInstance = false
            isInConstructor = false
            isInArg = false
            locals = System.Collections.Immutable.ImmutableHashSet<_>.Empty
        }

let rec GenWitnessArguments cenv env (witnessArgs: WitnessSolution imarray) =
    witnessArgs
    |> ImArray.choose (fun x -> 
        match x.Solution with
        | Some solution -> 
            match solution with
            | WitnessSymbol.TypeExtension(tyExt, specificAbstractFuncOpt) ->
                let index, ilKind = GenTypeParameterAsILTypeVariableInfo env x.TypeParameter
                let ilTyExt = GenEntityAsILEntityInstanceOrConstructor cenv env tyExt                
                let ilSpecificAbstractFuncInstOpt =
                    // REVIEW: Will this ever have witnesses?
                    specificAbstractFuncOpt
                    |> Option.map (fun x -> GenFunctionAsILFunctionReference cenv env x)
                OlyILWitness.Implementation(index, ilKind, ilTyExt, ilSpecificAbstractFuncInstOpt)
                |> Some
            | WitnessSymbol.TypeParameter _
            | WitnessSymbol.Type _ ->
                None
        | _ ->
            failwith "Internal error: No witnesses found"
    )

and GenValueTypeArgumentsAndWitnessArguments cenv env (value: IValueSymbol) witnessArgs =
    let ilTyInst = 
        value.TypeArguments
        |> ImArray.map (emitILTypeAux cenv env false true)
    let ilWitnesses = GenWitnessArguments cenv env witnessArgs

    ilTyInst, ilWitnesses

and GenFunctionAsILFunctionInstance cenv env (witnessArgs: WitnessSolution imarray) (func: IFunctionSymbol) : OlyILFunctionInstance =
    if func.TypeParameters.Length <> func.TypeArguments.Length then
        failwith "Number of type instantiations do not match the count of the function's type parameters."

    let ilEnclosing = emitILEnclosingForMember cenv env func
    let ilFuncSpecHandle = GenFunctionAsILFunctionSpecification cenv env func

    let ilTyInst, ilWitnesses = GenValueTypeArgumentsAndWitnessArguments cenv env func witnessArgs

    OlyILFunctionInstance(ilEnclosing, ilFuncSpecHandle, ilTyInst, ilWitnesses)

and GenFunctionAsILFunctionReference cenv env (func: IFunctionSymbol) : OlyILFunctionReference =
    if func.TypeParameters.Length <> func.TypeArguments.Length then
        failwith "Number of type instantiations do not match the count of the function's type parameters."

    let ilEnclosing = emitILEnclosingForMember cenv env func
    let ilFuncSpecHandle = GenFunctionAsILFunctionSpecification cenv env func

    OlyILFunctionReference(ilEnclosing, ilFuncSpecHandle)

and GenString cenv (value: string) =
    match cenv.cachedStrings.TryGetValue value with
    | true, handle -> handle
    | _ ->
        let handle = cenv.assembly.AddString(value)
        cenv.cachedStrings.[value] <- handle
        handle

and GenEntityAsILEntityReference (cenv: cenv) env (ent: EntitySymbol) =
    OlyAssert.False(ent.IsAnonymous)
    let ent = ent.Formal
    match cenv.cachedEntRefs.TryGetValue ent.Id with
    | true, handle -> handle
    | _ ->
        let name =
            if ent.IsLocal then
                // TODO: Check for private access.
                if ent.IsAnonymous then
                    cenv.GenerateName()
                else
                    cenv.GenerateName() + "_" + ent.Name
            elif ent.IsAnonymous then
                failwith "Entity must have a name if it is not local."                       
            else
                ent.Name

        let ilEntRef = OlyILEntityReference(emitILEnclosingForEntity cenv env ent, GenString cenv name, ent.LogicalTypeParameterCount)
        let handle = cenv.assembly.AddEntityReference(ilEntRef)
        cenv.cachedEntRefs.[ent.Id] <- handle
        handle

and GenEntityAsILEntityInstance cenv env (ent: EntitySymbol) =
#if DEBUG
    OlyAssert.Equal(ent.TypeParameters.Length, ent.TypeArguments.Length)
    OlyAssert.Equal(ent.LogicalTypeParameters.Length, ent.LogicalTypeArguments.Length)
#endif

    let asmIdentity =
        match ent.ContainingAssembly with
        | Some(asm) -> asm.Identity
        | _ -> failwith "Expected a containing assembly when emitting a entity instance."

    let ilTyInst = emitILTypes cenv env ent.TypeArguments
    if asmIdentity = cenv.assembly.Identity || ent.IsAnonymousShape then
        // Local to the assembly
        OlyILEntityInstance(GenEntityAsILEntityDefinition cenv env ent.Formal, ilTyInst)
    else
        OlyILEntityInstance(GenEntityAsILEntityReference cenv env ent, ilTyInst)

and GenEntityAsILEntityInstanceOrConstructor cenv env (ent: EntitySymbol) =
#if DEBUG
    OlyAssert.Equal(ent.TypeParameters.Length, ent.TypeArguments.Length)
    OlyAssert.Equal(ent.LogicalTypeParameters.Length, ent.LogicalTypeArguments.Length)
#endif

    if ent.IsTypeConstructor then
        OlyAssert.True(ent.IsFormal)

        let asmIdentity =
            match ent.ContainingAssembly with
            | Some(asm) -> asm.Identity
            | _ -> failwith "Expected a containing assembly when emitting a entity instance."

        if asmIdentity = cenv.assembly.Identity || ent.IsAnonymousShape then
            OlyILEntityConstructor(GenEntityAsILEntityDefinition cenv env ent)
        else
            OlyILEntityConstructor(GenEntityAsILEntityReference cenv env ent)
    else
        GenEntityAsILEntityInstance cenv env ent

and GenTupleNames cenv (names: string imarray) =
    names
    |> ImArray.map (fun x ->
        if String.IsNullOrWhiteSpace x then
            Unchecked.defaultof<OlyILStringHandle>
        else
            GenString cenv x
    )

and GenTypeParameterAsILTypeVariableInfo (env: env) (tyPar: TypeParameterSymbol) =
    match tyPar.Kind with
    | TypeParameterKind.Type ->
        tyPar.Index, OlyILTypeVariableKind.Type
    | TypeParameterKind.Function index ->
        index, OlyILTypeVariableKind.Function

and emitILType cenv env ty =
    emitILTypeAux cenv env false true ty

and emitILFunctionTypeInfo cenv env (inputTy: TypeSymbol) (returnTy: TypeSymbol) =
    let argTys =
        match stripTypeEquations inputTy with
        | TypeSymbol.Unit -> ImArray.empty
        | TypeSymbol.Tuple(argTys, _) -> argTys
        | _ -> ImArray.createOne inputTy
    let ilArgTys =
        argTys
        |> ImArray.map (emitILType cenv env)

    let ilReturnTy =
        if returnTy.IsFunction_t then
            emitILType cenv env returnTy
        else
            GenReturnType cenv env returnTy

    ilArgTys, ilReturnTy

and emitILTypeAux cenv env canEmitVoidForUnit canStripBuiltIn (ty: TypeSymbol) =
    let ty =
        if canStripBuiltIn then
            stripTypeEquationsAndBuiltIn ty
        else
            ty
    match ty with
    | TypeSymbol.DependentIndexer(inputValueTy, innerTy) ->
        OlyILTypeDependentIndexer(emitILType cenv env inputValueTy, emitILType cenv env innerTy)
    | TypeSymbol.BaseObject ->
        OlyILTypeBaseObject
    | TypeSymbol.Unit ->
        if canEmitVoidForUnit then
            OlyILTypeVoid
        else
            OlyILTypeUnit
    | TypeSymbol.Void ->
        OlyILTypeVoid
    | TypeSymbol.Int8 ->
        OlyILTypeInt8
    | TypeSymbol.UInt8 ->
        OlyILTypeUInt8
    | TypeSymbol.Int16 ->
        OlyILTypeInt16
    | TypeSymbol.UInt16 ->
        OlyILTypeUInt16
    | TypeSymbol.Int32 ->
        OlyILTypeInt32
    | TypeSymbol.UInt32 ->
        OlyILTypeUInt32
    | TypeSymbol.Int64 ->
        OlyILTypeInt64
    | TypeSymbol.UInt64 ->
        OlyILTypeUInt64
    | TypeSymbol.Float32 ->
        OlyILTypeFloat32
    | TypeSymbol.Float64 ->
        OlyILTypeFloat64
    | TypeSymbol.Bool ->
        OlyILTypeBool
    | TypeSymbol.Char16 ->
        OlyILTypeChar16
    | TypeSymbol.Utf16 ->
        OlyILTypeUtf16
    | TypeSymbol.Entity(ent) ->
        let env = setLocalContextWithEnclosing env ent.Enclosing
        OlyILTypeEntity(GenEntityAsILEntityInstanceOrConstructor cenv env ent)
    | TypeSymbol.Tuple(tyArgs, names) ->
        OlyAssert.True(tyArgs.Length >= 1)
        if tyArgs.Length = 1 then
            emitILType cenv env tyArgs[0]
        else
            let ilNameHandles = GenTupleNames cenv names
            OlyILTypeTuple(tyArgs |> ImArray.map (emitILType cenv env), ilNameHandles)
    | TypeSymbol.RefCell(ty) ->
        OlyILTypeRefCell(emitILType cenv env ty)
    | TypeSymbol.Function(inputTy, outputTy) ->
        let ilArgTys, ilReturnTy = emitILFunctionTypeInfo cenv env inputTy outputTy
        OlyILTypeFunction(ilArgTys, ilReturnTy)

    | TypeSymbol.Variable(tyPar) ->
        let index, ilKind = GenTypeParameterAsILTypeVariableInfo env tyPar
        OlyILTypeVariable(index, ilKind)
    | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
        let index, ilKind = GenTypeParameterAsILTypeVariableInfo env tyPar
        OlyILTypeHigherVariable(index, tyArgs |> ImArray.map (emitILType cenv env), ilKind)
    | TypeSymbol.InferenceVariable _
    | TypeSymbol.HigherInferenceVariable _ ->
        failwith "Internal error: Solution-less inference variables cannot be emitted"
    | TypeSymbol.ConstantInt32(n) ->
        OlyILTypeConstantInt32(n)

    | TypeSymbol.ByRef(innerTy, kind) ->
        match kind with
        | ByRefKind.ReadWrite ->
            OlyILTypeByRef(emitILType cenv env innerTy, OlyILByRefKind.ReadWrite)
        | ByRefKind.Read ->
            OlyILTypeByRef(emitILType cenv env innerTy, OlyILByRefKind.Read)

    | TypeSymbol.NativeInt -> OlyILTypeNativeInt
    | TypeSymbol.NativeUInt -> OlyILTypeNativeUInt
    | TypeSymbol.NativePtr(elementTy) -> 
        OlyILTypeNativePtr(emitILType cenv env elementTy)
    | TypeSymbol.NativeFunctionPtr(ilCc, inputTy, outputTy) ->
        let ilArgTys, ilReturnTy = emitILFunctionTypeInfo cenv env inputTy outputTy
        OlyILTypeNativeFunctionPtr(ilCc, ilArgTys, ilReturnTy)
    | TypeSymbol.Array(elementTy, rank, kind) -> 
        let ilKind =
            match kind with
            | ArrayKind.Immutable -> OlyILArrayKind.Immutable
            | ArrayKind.Mutable -> OlyILArrayKind.Mutable
        OlyILTypeArray(emitILType cenv env elementTy, rank, ilKind)

    | TypeSymbol.EagerInferenceVariable _ ->
        OlyAssert.Fail("Internal error: Unable to code-gen an eager inference variable type.")

    | TypeSymbol.ForAll(tyPars, innerTy) ->
        if innerTy.IsFunction_t then
            OlyILTypeForAll(
                emitILTypeParameters cenv env tyPars,
                emitILType cenv env innerTy
            )
        elif innerTy.IsBuiltIn then
            raise(System.NotSupportedException("For all types are not supported in IL."))
        else
            let ent = innerTy.TryEntity.Value
            OlyILTypeForAll(
                emitILTypeParameters cenv env tyPars,
                OlyILTypeEntity(GenEntityAsILEntityInstance cenv env ent)
            )

    | TypeSymbol.Error _ ->
        failwith "Internal error: Unable to code-gen an error type."

and emitILTypes cenv env (tys: TypeSymbol imarray) =
    tys
    |> ImArray.map (emitILType cenv env)    

and GenReturnType (cenv: cenv) env (ty: TypeSymbol) =
#if DEBUG
    if not ty.TypeParameters.IsEmpty && ty.IsFormal then
        failwith "Unexpected formal type."
#endif
    match stripTypeEquations ty with
    | TypeSymbol.Unit -> OlyILTypeVoid
    | ty -> emitILType cenv env ty

and getAssemblyIdentity (ent: EntitySymbol) =
    let asmIdentity =
        match ent.ContainingAssembly with
        | Some(asm) -> asm.Identity
        | _ -> failwith "Expected a containing assembly when emitting a entity reference."
    asmIdentity

and emitILEnclosingNamespace cenv env asmIdentity (ent: EntitySymbol) =
    OlyAssert.True(ent.IsNamespace)
    let rec loop (enclosing: EnclosingSymbol) acc =
            match enclosing with
            | EnclosingSymbol.Entity(ent) when ent.IsNamespace ->
                loop enclosing.Enclosing ((GenString cenv ent.Name) :: acc)
            | EnclosingSymbol.RootNamespace -> acc |> ImArray.ofSeq
            | _ ->
                failwith "Invalid enclosing."

    OlyILEnclosing.Namespace(loop ent.AsEnclosing [], asmIdentity)

and emitILEnclosingEntityNoNamespace cenv env (ent: EntitySymbol) =
    OlyAssert.False(ent.IsNamespace)
    OlyILEnclosing.Entity(GenEntityAsILEntityInstance cenv env ent)

and emitILEnclosingForMember cenv env (func: IValueSymbol) =
    match func.Enclosing with
    | EnclosingSymbol.RootNamespace ->
        failwith "Expected to be in an enclosing entity context for a function."
    | EnclosingSymbol.Local ->
        match env.context with
        | LocalContext.Namespace _ -> failwith "Expected to be in an enclosing entity context for a function."
        | LocalContext.Entity ent ->
            let ilEntInst = GenEntityAsILEntityInstance cenv env ent
            OlyILEnclosing.Entity(ilEntInst)
    | EnclosingSymbol.Entity(ent) ->
        emitILEnclosingEntityNoNamespace cenv env ent
    | EnclosingSymbol.Witness(concreteTy, tr) ->
        OlyILEnclosing.Witness(emitILType cenv env concreteTy, GenEntityAsILEntityInstance cenv env tr)

and emitILEnclosingForEntity cenv env (ent: EntitySymbol) =
    match ent.Enclosing with
    | EnclosingSymbol.Entity(enclosingEnt) ->
        if enclosingEnt.IsNamespace then
            emitILEnclosingNamespace cenv env (getAssemblyIdentity ent) enclosingEnt
        else
            emitILEnclosingEntityNoNamespace cenv env enclosingEnt
    | EnclosingSymbol.Local ->
        match env.context with
        | LocalContext.Namespace _ -> failwith "Expected to be in an enclosing entity context for a local."
        | LocalContext.Entity ent ->
            let ilEntInst = GenEntityAsILEntityInstance cenv env ent
            OlyILEnclosing.Entity(ilEntInst)
    | EnclosingSymbol.RootNamespace ->
        OlyILEnclosing.Namespace(ImArray.empty, getAssemblyIdentity ent)
    | EnclosingSymbol.Witness(concreteTy, tr) ->
        OlyILEnclosing.Witness(emitILType cenv env concreteTy, GenEntityAsILEntityInstance cenv env tr)

and GenLocalParameters cenv env (pars: ILocalParameterSymbol romem) =
    pars
    |> ROMem.mapAsImArray (fun par ->
        let nameHandle =
            if String.IsNullOrEmpty par.Name then
                OlyILTableIndex(OlyILTableKind.String, -1)
            else
                GenString cenv par.Name
        let canInlineClosure = 
            match tryAttributesInlineArgument par.Attributes with
            | Some(inlineArg) ->
                match inlineArg with
                | InlineArgumentSymbol.Never -> false
                | _ -> true
            | _ ->
                false
        let isMutable = par.IsMutable

        let parTy = par.Type
#if DEBUG
        if not parTy.TypeParameters.IsEmpty && parTy.IsFormal then
            failwith "Unexpected formal type."
#endif

        OlyILParameter(nameHandle, emitILType cenv env parTy, isMutable, canInlineClosure)
    )

and emitILTypeParameters cenv env (typeParameters: TypeParameterSymbol imarray) =
    typeParameters
    |> ImArray.map (fun x ->
        let ilConstrs =
            x.Constraints
            |> ImArray.map (fun x ->
                match x with
                | ConstraintSymbol.Null ->
                    OlyILConstraint.Null
                | ConstraintSymbol.Struct ->
                    OlyILConstraint.Struct
                | ConstraintSymbol.NotStruct ->
                    OlyILConstraint.NotStruct
                | ConstraintSymbol.Unmanaged ->
                    OlyILConstraint.Unmanaged
                | ConstraintSymbol.Scoped ->
                    OlyILConstraint.Scoped
                | ConstraintSymbol.ConstantType(constTy) ->
                    OlyILConstraint.ConstantType(emitILType cenv env constTy.Value)
                | ConstraintSymbol.SubtypeOf(ty) ->
                    let ty = ty.Value
                    OlyILConstraint.SubtypeOf(emitILType cenv env ty)
            )
        OlyILTypeParameter(GenString cenv x.Name, x.Arity, x.IsVariadic, ilConstrs)
    )

and GenFieldAsILFieldReference cenv env (field: IFieldSymbol) =
    OlyILFieldReference(emitILEnclosingForMember cenv env field, GenString cenv field.Name, emitILType cenv env field.Type)

and GenFieldAsILFieldDefinition cenv env (field: IFieldSymbol) =
    OlyAssert.True(field.IsFormal)
    match cenv.cachedFieldDefs.TryGetValue field.Id with
    | true, ilFieldDefHandle -> ilFieldDefHandle
    | _ ->
        if field.IsNewSlot && not field.IsInstance then
            failwith "A static field cannot be marked as distinct."

        let memberFlags =
            if field.IsInstance then
                OlyILMemberFlags.None
            else
                OlyILMemberFlags.Static

        let memberFlags = 
            if field.IsPrivate then
                memberFlags ||| OlyILMemberFlags.Private
            elif field.IsInternal then
                memberFlags ||| OlyILMemberFlags.Internal
            elif field.IsProtected then
                memberFlags ||| OlyILMemberFlags.Protected
            else
                memberFlags

        let flags =
            if field.IsMutable then
                OlyILFieldFlags.Mutable
            else
                OlyILFieldFlags.None

        let ilAttrs =
            field.Attributes
            |> (GenAttributes cenv env)

        let ilFieldDef = 
            match field.Constant with
            | ValueSome(constValue) ->
                OlyILFieldConstant(GenString cenv field.Name, emitILType cenv env field.Type, GenConstant cenv env constValue, memberFlags)
            | _ ->
                OlyILFieldDefinition(ilAttrs, GenString cenv field.Name, emitILType cenv env field.Type, flags, memberFlags)
        cenv.assembly.AddFieldDefinition(ilFieldDef)

and GenFieldsAsILFieldDefinitions cenv env fields =
    fields
    |> ImArray.map (GenFieldAsILFieldDefinition cenv env)

and GenFunctionAsILFunctionSpecificationNoCache (cenv: cenv) (env: env) (func: IFunctionSymbol) =
    if func.Name.StartsWith "__oly" && func.WellKnownFunction <> WellKnownFunction.None then
        failwith "Unable to emit a function specification for an intrinsic function."

    match func.Enclosing with
    | EnclosingSymbol.Entity(ent) when ent.IsModule && func.IsAbstract -> failwith "invalid"
    | _ -> ()
    let name =
        if func.IsLocal then
            cenv.GenerateName() + "_" + func.Name
        else
            func.Name
    let nameHandle =
        name
        |> GenString cenv
    let ilTypeParams = 
        if func.IsConstructor then
            ImArray.empty
        else
            emitILTypeParameters cenv env func.TypeParameters
    let ilPars =
        GenLocalParameters cenv env func.LogicalParameters
    let ilReturnTy = 
        if func.IsConstructor then
            OlyILTypeVoid
        else
            GenReturnType cenv env func.ReturnType

    let ilCallConv =
        if func.IsBlittable then
            OlyILCallingConvention.Blittable
        else
            OlyILCallingConvention.Default

    cenv.assembly.AddFunctionSpecification(OlyILFunctionSpecification(func.IsInstance, ilCallConv, nameHandle, ilTypeParams, ilPars, ilReturnTy))

and GenFunctionAsILFunctionSpecification cenv (env: env) (func: IFunctionSymbol) =
    if func.IsFunctionGroup then
        failwith "Function groups are not allowed to be generated."

    let func = func.Formal :?> IFunctionSymbol // TODO: Get rid of the cast.
    let cached = cenv.cachedFuncSpecs
    match cached.TryGetValue func.Id with
    | true, handle -> handle
    | _ ->
        let handle = GenFunctionAsILFunctionSpecificationNoCache cenv env func
        cached.[func.Id] <- handle
        handle

and GenFunctionAsILFunctionDefinition cenv (env: env) (func: IFunctionSymbol) =
    OlyAssert.True(func.IsFormal)
    let funcId = func.Id
    match cenv.cachedFuncDefs.TryGetValue(funcId) with
    | true, result -> result
    | _ ->
#if DEBUG
        let funcToCheck =
            if func.Enclosing.IsLocalEnclosing then
                func.WithEnclosing(env.context.RealEntity.AsEnclosing) :?> IFunctionSymbol
            else
                func
        assertNoForAllTypes funcToCheck
#endif

        if func.Enclosing.IsNamespace then
            failwithf "Function '%s''s enclosing is a namespace; must be a type." func.Name

        if func.IsConstructor && (func.IsVirtual || func.IsAbstract) then
            failwith "A constructor cannot be virtual or abstract."

        let ilMemberFlags =
            if func.IsInstance then
                OlyILMemberFlags.None
            else
                OlyILMemberFlags.Static

        let ilFuncFlags, ilMemberFlags =
            if func.IsConstructor then
                OlyILFunctionFlags.Constructor, ilMemberFlags
            else
                OlyILFunctionFlags.None,
                let memberFlags =
                    if func.IsAbstract then
                        ilMemberFlags ||| OlyILMemberFlags.Abstract
                    else
                        ilMemberFlags

                if func.IsVirtual then
                    memberFlags ||| OlyILMemberFlags.Virtual
                else
                    memberFlags

        let ilFuncFlags =
            if func.IsStackEmplace then
                ilFuncFlags ||| OlyILFunctionFlags.StackEmplace
            else
                ilFuncFlags

        let ilFuncFlags =
            if func.IsMutable then
                ilFuncFlags ||| OlyILFunctionFlags.Mutable
            else
                ilFuncFlags

        let ilFuncFlags =
            if func.IsParameterLessFunction then
                ilFuncFlags ||| OlyILFunctionFlags.ParameterLess
            else
                ilFuncFlags

        let ilFuncFlags =
            if func.RequiresExplicitTypeArguments then
                ilFuncFlags ||| OlyILFunctionFlags.RequiresExplicitTypeArguments
            else
                ilFuncFlags

        let ilFuncFlags =
            if func.IsInline then
                if func.IsAbstract then
                    failwith "Abstract function cannot be marked as inlineable."
                if func.IsInlineAlways then
                    ilFuncFlags ||| OlyILFunctionFlags.InlineAlways
                else
                    ilFuncFlags ||| OlyILFunctionFlags.Inline
            elif func.IsInlineNever then
                if func.IsAbstract then
                    failwith "Abstract function cannot be marked as not inlineable."
                ilFuncFlags ||| OlyILFunctionFlags.InlineNever
            else
                ilFuncFlags

        let ilMemberFlags =
            if func.IsNewSlot then
                ilMemberFlags ||| OlyILMemberFlags.NewSlot
            else
                ilMemberFlags

        let ilMemberFlags =
            if func.IsFinal then
                ilMemberFlags ||| OlyILMemberFlags.Final
            else
                ilMemberFlags

        let ilMemberFlags = 
            if func.IsPrivate then
                ilMemberFlags ||| OlyILMemberFlags.Private
            elif func.IsInternal then
                ilMemberFlags ||| OlyILMemberFlags.Internal
            elif func.IsProtected then
                ilMemberFlags ||| OlyILMemberFlags.Protected
            else
                ilMemberFlags

        let overrides =
            let enclosing = func.Enclosing
            func.FunctionOverrides 
            |> Option.bind (fun funcOverride ->
                if areEnclosingsEqual enclosing funcOverride.Enclosing then
                    failwith "Invalid overrides."
                if funcOverride.IsFinal then
                    failwith "Function is trying to override a sealed function."
                let result = GenFunctionAsILFunctionReference cenv env funcOverride
                Some result
            )

        let ilAttrs =
            func.Attributes
            |> (GenAttributes cenv env)

        let enclosingEnt = 
            if func.Enclosing.IsLocalEnclosing then
                env.context.RealEntity
            else
                func.Enclosing.AsEntity

        let ilFuncDefHandle = 
            let ilEntDefHandle = GenEntityAsILEntityDefinition cenv env enclosingEnt
            let ilFuncDef = OlyILFunctionDefinition(ilFuncFlags, ilMemberFlags, ilAttrs, GenFunctionAsILFunctionSpecification cenv env func, overrides, ref None)
            cenv.assembly.AddFunctionDefinition(ilEntDefHandle, ilFuncDef)

        cenv.cachedFuncDefs.[funcId] <- ilFuncDefHandle

        if func.IsStaticLocalFunction then
            OlyAssert.True(enclosingEnt.IsFormal)
            let funcDefs =
                match cenv.extraFuncDefs.TryGetValue enclosingEnt.Id with
                | true, funcDefs -> funcDefs
                | _ ->
                    let funcDefs = ResizeArray()
                    cenv.extraFuncDefs.[enclosingEnt.Id] <- funcDefs
                    funcDefs
            funcDefs.Add(ilFuncDefHandle)

        elif (func.FunctionFlags.HasFlag(FunctionFlags.Extra)) then
            OlyAssert.True(enclosingEnt.IsFormal)
            let funcDefs =
                match cenv.extraFuncDefs.TryGetValue enclosingEnt.Id with
                | true, funcDefs -> funcDefs
                | _ ->
                    let funcDefs = ResizeArray()
                    cenv.extraFuncDefs.[enclosingEnt.Id] <- funcDefs
                    funcDefs
            funcDefs.Add(ilFuncDefHandle)

        ilFuncDefHandle

and GenAutoOrSignatureProperty cenv env (prop: IPropertySymbol) =
    OlyAssert.True(prop.IsFormal)
    OlyAssert.False(prop.IsField)

    let ilGetterOpt, ilSetterOpt =
        let ilGetterOpt =
            match prop.Getter with
            | Some getter ->
                GenFunctionAsILFunctionDefinition cenv env getter
            | _ ->
                OlyILFunctionDefinition.NilHandle

        let ilSetterOpt =
            match prop.Setter with
            | Some setter ->
                GenFunctionAsILFunctionDefinition cenv env setter
            | _ ->
                OlyILFunctionDefinition.NilHandle

        ilGetterOpt, ilSetterOpt

    let ilAttrs = GenAttributes cenv env prop.Attributes
    let ilName = GenString cenv prop.Name
    let ilTy = emitILType cenv env prop.Type
    OlyILPropertyDefinition(
        ilAttrs,
        ilName,
        ilTy,
        ilGetterOpt,
        ilSetterOpt
    )
    |> cenv.assembly.AddPropertyDefinition

and GenConstant cenv env (constant: ConstantSymbol) =
    match constant with
    | ConstantSymbol.UInt8(value) -> OlyILConstant.UInt8(value)
    | ConstantSymbol.Int8(value) -> OlyILConstant.Int8(value)
    | ConstantSymbol.UInt16(value) -> OlyILConstant.UInt16(value)
    | ConstantSymbol.Int16(value) -> OlyILConstant.Int16(value)
    | ConstantSymbol.UInt32(value) -> OlyILConstant.UInt32(value)
    | ConstantSymbol.Int32(value) -> OlyILConstant.Int32(value)
    | ConstantSymbol.UInt64(value) -> OlyILConstant.UInt64(value)
    | ConstantSymbol.Int64(value) -> OlyILConstant.Int64(value)
    | ConstantSymbol.Float32(value) -> OlyILConstant.Float32(value)
    | ConstantSymbol.Float64(value) -> OlyILConstant.Float64(value)
    | ConstantSymbol.True -> OlyILConstant.True
    | ConstantSymbol.False -> OlyILConstant.False
    | ConstantSymbol.Array(elementTy, elements) ->
        OlyILConstant.Array(emitILType cenv env elementTy, elements |> ImArray.map (GenConstant cenv env))
    | ConstantSymbol.Char16(value) -> OlyILConstant.Char16(value)
    | ConstantSymbol.Utf16(value) -> OlyILConstant.Utf16(value)
    | ConstantSymbol.TypeVariable(tyPar) -> 
        let index, ilKind = GenTypeParameterAsILTypeVariableInfo env tyPar
        OlyILConstant.TypeVariable(index, ilKind)
    | ConstantSymbol.External(func) -> 
        OlyILConstant.External(GenFunctionAsILFunctionInstance cenv env ImArray.empty func, emitILType cenv env func.ReturnType)
    | ConstantSymbol.Error _ -> failwith "Cannot emit constant."

and GenAttribute (cenv: cenv) (env: env) (attr: AttributeSymbol) =
    match attr with
    | AttributeSymbol.Open 
    | AttributeSymbol.Null
    | AttributeSymbol.Inline _
    | AttributeSymbol.Blittable
    | AttributeSymbol.Pure ->
        None
    | AttributeSymbol.Import(platform, path, name) ->
        let platform = GenString cenv platform
        let path = 
            if path.Length = 1 && String.IsNullOrWhiteSpace path[0] then
                ImArray.empty
            else
                path |> ImArray.map (GenString cenv)
        let name = GenString cenv name
        OlyILAttribute.Import(platform, path, name) |> Some
    | AttributeSymbol.Intrinsic(name) ->
        let name = GenString cenv name
        OlyILAttribute.Intrinsic(name) |> Some
    | AttributeSymbol.Export ->
        OlyILAttribute.Export |> Some
    | AttributeSymbol.Constructor(ctor, args, namedArgs, _flags) ->
        if not ctor.IsInstanceConstructor then
            failwith "Expected instance constructor."

        let ilConstants =
            args
            |> ImArray.map (GenConstant cenv env)

        let ilNamedArgs =
            namedArgs
            |> ImArray.map (fun x ->
                match x with
                | AttributeNamedArgumentSymbol.Field(field, constValue) ->
                    {
                        Kind = OlyILAttributeNamedArgumentKind.Field
                        NameHandle = GenString cenv field.Name
                        Constant = GenConstant cenv env constValue
                    } : OlyILAttributeNamedArgument
                | AttributeNamedArgumentSymbol.Property(prop, constValue) ->
                    {
                        Kind = OlyILAttributeNamedArgumentKind.Property
                        NameHandle = GenString cenv prop.Name
                        Constant = GenConstant cenv env constValue
                    } : OlyILAttributeNamedArgument
            )

        OlyILAttribute.Constructor(GenFunctionAsILFunctionInstance cenv env ImArray.empty ctor, ilConstants, ilNamedArgs)
        |> Some

and GenAttributes cenv env (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.choose (GenAttribute cenv env)

and GenEntityDefinitionNoCache cenv env (ent: EntitySymbol) =
#if DEBUG
    OlyAssert.True(ent.IsFormal)
    if ent.IsShape then
        OlyAssert.False(ent.Enclosing.IsShape && not ent.IsAnonymous)
    elif ent.IsAnonymous then
        match ent.Enclosing with
        | EnclosingSymbol.RootNamespace -> ()
        | _ -> OlyAssert.Fail("Expected RootNamespace for anonymous entity.")
#endif

    let ilEntDefHandleFixup = GenEntityAsILEntityDefinition cenv env ent
    let envWithLocalContext = setLocalContext env ent

    let ilEntDefHandles =
        ent.Entities
        |> ImArray.map (fun ent ->
            GenEntityAsILEntityDefinition cenv envWithLocalContext ent
        )

    // Checks and balances for closures, users should never seen any of this but are here just in case.
    if ent.IsClosure then
        if ent.Functions.Length <> 2 then
            failwith "Closure has too many member functions."

        if not ent.Extends.IsEmpty then
            failwith "Closures are not allowed to extend a type."

        if not ent.Implements.IsEmpty then
            failwith "Closures are not allowed to implement a type."

        let func1 = ent.Functions.[0]
        let func2 = ent.Functions.[1]

        let closureHasValidFunctions =
            func1.IsInstanceConstructor && func2.IsInstanceNotConstructor || 
            func1.IsInstanceNotConstructor && func2.IsInstanceConstructor

        if not closureHasValidFunctions then
            failwith "Not a valid closure. A closure must have a single instance constructor and single instance function."

    let ilExtends =
        ent.Extends 
        |> ImArray.map (emitILType cenv env)

    let ilImplements =
        ent.Implements
        |> ImArray.map (emitILType cenv env)

    let ilEntKind =
        match ent.Kind with
        | EntityKind.Namespace -> failwith "Cannot emit a namespace entity."
        | EntityKind.Class -> OlyILEntityKind.Class
        | EntityKind.Interface -> OlyILEntityKind.Interface
        | EntityKind.Module -> OlyILEntityKind.Module
        | EntityKind.Shape -> OlyILEntityKind.Shape
        | EntityKind.Struct -> OlyILEntityKind.Struct
        | EntityKind.TypeExtension -> OlyILEntityKind.TypeExtension
        | EntityKind.Alias -> OlyILEntityKind.Alias
        | EntityKind.Closure -> OlyILEntityKind.Closure
        | EntityKind.Enum -> OlyILEntityKind.Enum
        | EntityKind.Newtype -> OlyILEntityKind.Newtype

    let ilEntFlags = OlyILEntityFlags.None
    let ilEntFlags = if ent.IsAutoOpenable then ilEntFlags ||| OlyILEntityFlags.AutoOpen else ilEntFlags
    let ilEntFlags = if ent.IsSealed then ilEntFlags ||| OlyILEntityFlags.Final else ilEntFlags
    let ilEntFlags = if ent.IsAbstract then ilEntFlags ||| OlyILEntityFlags.Abstract else ilEntFlags
    let ilEntFlags = 
        if ent.IsPrivate then
            if ent.Enclosing.IsNamespace then
                // Types that are marked 'private' but exist within a namespace,
                // we have to emit them as 'internal'.
                ilEntFlags ||| OlyILEntityFlags.Internal
            else
                ilEntFlags ||| OlyILEntityFlags.Private
        elif ent.IsInternal then
            ilEntFlags ||| OlyILEntityFlags.Internal
        else
            ilEntFlags // Default is public

    let ilFieldDefs = GenFieldsAsILFieldDefinitions cenv envWithLocalContext ent.Fields

    let name =
        if ent.IsLocal then
            // TODO: Check for private access.
            if ent.IsAnonymous then
                cenv.GenerateName()
            else
                cenv.GenerateName() + "_" + ent.Name
        elif ent.IsAnonymous && not ent.IsShape then
            cenv.GenerateName()                    
        else
            ent.Name

    let ilEnclosing = 
        if ent.IsAnonymous then
            OlyILEnclosing.Namespace(ImArray.empty, cenv.assembly.Identity)
        else
            emitILEnclosingForEntity cenv env ent
    let ilName = 
        if ent.IsShape && ent.IsAnonymous then
            OlyILTableIndex(OlyILTableKind.String, -1)
        else
            GenString cenv name
    let ilTyPars = 
        let enclosingTyParCount =
            match ent.Enclosing with
            | EnclosingSymbol.Local ->
                env.context.RealEntity.TypeParameters.Length
            | enclosing -> 
//#if DEBUG
//                match enclosing with
//                | EnclosingSymbol.Entity(ent) when not ent.IsNamespace ->
//                    OlyAssert.Equal(ent.TypeParameters.Length, env.context.RealEntity.TypeParameters.Length)
//                | _ ->
//                    ()
//#endif
                enclosing.TypeParameters.Length 
        let tyPars = 
                ent.TypeParameters |> ImArray.skip enclosingTyParCount
        emitILTypeParameters cenv env tyPars

    cenv.delayedEntityGenQueue.Enqueue(fun () ->         

        let ilPropDefs =
            ent.Properties
            |> ImArray.map (fun prop ->
                let ilAttrs = GenAttributes cenv env prop.Attributes
                let ilName = GenString cenv prop.Name
                let ilTy = emitILType cenv env prop.Type
                let ilGetterOpt =
                    prop.Getter
                    |> Option.map (GenFunctionAsILFunctionDefinition cenv env)
                    |> Option.defaultValue (OlyILFunctionDefinition.NilHandle)
                let ilSetterOpt =
                    prop.Setter
                    |> Option.map (GenFunctionAsILFunctionDefinition cenv env)
                    |> Option.defaultValue (OlyILFunctionDefinition.NilHandle)
                OlyILPropertyDefinition(
                    ilAttrs,
                    ilName,
                    ilTy,
                    ilGetterOpt,
                    ilSetterOpt
                )
                |> cenv.assembly.AddPropertyDefinition
            )

        let ilPatDefs =
            ent.Patterns
            |> ImArray.map (fun pat ->
                let ilAttrs = GenAttributes cenv env pat.Attributes
                let ilName = GenString cenv pat.Name
                let ilPatFuncDefHandle = GenFunctionAsILFunctionDefinition cenv env pat.PatternFunction
                let ilPatGuardFuncDefHandleOpt =
                    pat.PatternGuardFunction
                    |> Option.map (GenFunctionAsILFunctionDefinition cenv env)
                    |> Option.defaultValue OlyILFunctionDefinition.NilHandle
                OlyILPatternDefinition(
                    ilAttrs,
                    ilName,
                    ilPatFuncDefHandle,
                    ilPatGuardFuncDefHandleOpt
                )
                |> cenv.assembly.AddPatternDefinition
            )
        
        let ilFuncDefs =          
            let ilFuncDefs =
                ent.Functions
                |> ImArray.map (fun x -> 
                    OlyAssert.True(x.IsFormal)
                    let ilFuncDefHandle = 
                        GenFunctionAsILFunctionDefinition cenv env x
                    if x.IsEntryPoint then
                        let ilEnclosingTy = emitILType cenv env ent.AsType
                        cenv.assembly.EntryPoint <- Some(ilEnclosingTy, ilFuncDefHandle)
                    ilFuncDefHandle)

            match cenv.extraFuncDefs.TryGetValue ent.Id with
            | true, extra ->
                Seq.append ilFuncDefs extra
                |> ImArray.ofSeq
            | _ ->
                ilFuncDefs
                |> ImArray.ofSeq

        let ilRuntimeTyOpt =
            ent.RuntimeType
            |> Option.map (fun ty -> emitILType cenv env ty)

#if DEBUG
        if ilRuntimeTyOpt.IsSome then   
            OlyAssert.True(ilImplements.IsEmpty)
#endif

        let ilEntDef = 
            OlyILEntityDefinition(
                ilEntKind,
                ilEntFlags,
                (ent.Attributes |> (GenAttributes cenv env)),
                ilEnclosing,
                ilName,
                ilTyPars,
                ilFuncDefs, 
                ilFieldDefs,
                ilPropDefs,
                ilPatDefs,
                ilEntDefHandles,
                ilImplements,
                ilExtends,
                ilRuntimeTyOpt)

        cenv.assembly.SetEntityDefinition(ilEntDefHandleFixup, ilEntDef)
    )

    ilEntDefHandleFixup

and GenEntityAsILEntityDefinition cenv env (ent: EntitySymbol) : OlyILEntityDefinitionHandle =
    OlyAssert.True(ent.IsFormal)
    match cenv.cachedEntDefs.TryGetValue ent.Id with
    | true, res -> res
    | _ ->
        let ilEntDefHandleFixup = cenv.assembly.NextEntityDefinition()
        cenv.cachedEntDefs.Add(ent.Id, ilEntDefHandleFixup)
        if ent.IsAnonymousShape then
            let result = GenEntityDefinitionNoCache cenv env ent
            OlyAssert.Equal(result, ilEntDefHandleFixup)
            result
        else
            ilEntDefHandleFixup

and DoesILEntityDefinitionExists cenv (ent: EntitySymbol) =
    OlyAssert.True(ent.IsFormal)
    cenv.cachedEntDefs.ContainsKey(ent.Id)

and GenValueLiteral cenv env (lit: BoundLiteral) : OlyILValue =
    match lit with
    | BoundLiteral.Constant(ConstantSymbol.Int8(value=value)) ->
        OlyILValue.Constant(OlyILConstant.Int8(value))
    | BoundLiteral.Constant(ConstantSymbol.UInt8(value=value)) ->
        OlyILValue.Constant(OlyILConstant.UInt8(value))
    | BoundLiteral.Constant(ConstantSymbol.Int16(value=value)) ->
        OlyILValue.Constant(OlyILConstant.Int16(value))
    | BoundLiteral.Constant(ConstantSymbol.UInt16(value=value)) ->
        OlyILValue.Constant(OlyILConstant.UInt16(value))
    | BoundLiteral.Constant(ConstantSymbol.Int32(value=value)) ->
        OlyILValue.Constant(OlyILConstant.Int32(value))
    | BoundLiteral.Constant(ConstantSymbol.UInt32(value=value)) ->
        OlyILValue.Constant(OlyILConstant.UInt32(value))
    | BoundLiteral.Constant(ConstantSymbol.Int64(value=value)) ->
        OlyILValue.Constant(OlyILConstant.Int64(value))
    | BoundLiteral.Constant(ConstantSymbol.UInt64(value=value)) ->
        OlyILValue.Constant(OlyILConstant.UInt64(value))
    | BoundLiteral.Constant(ConstantSymbol.Float32(value=value)) ->
        OlyILValue.Constant(OlyILConstant.Float32(value))
    | BoundLiteral.Constant(ConstantSymbol.Float64(value=value)) ->
        OlyILValue.Constant(OlyILConstant.Float64(value))
    | BoundLiteral.Constant(ConstantSymbol.True) ->
        OlyILValue.Constant(OlyILConstant.True)
    | BoundLiteral.Constant(ConstantSymbol.False) ->
        OlyILValue.Constant(OlyILConstant.False)
    | BoundLiteral.Constant(ConstantSymbol.Char16(value=value)) ->
        OlyILValue.Constant(OlyILConstant.Char16(value))
    | BoundLiteral.Constant(ConstantSymbol.Utf16(value=value)) ->
        OlyILValue.Constant(OlyILConstant.Utf16(value))
    | BoundLiteral.NullInference ty ->
        OlyILValue.Null(emitILType cenv env ty)
    | BoundLiteral.NumberInference(lazyValue, _) ->
        match lazyValue.Value with
        | Ok(literal) -> GenValueLiteral cenv env literal
        | _ -> OlyAssert.Fail("Internal ILGen Error: Unexpected error for a lazy literal.")
    | BoundLiteral.DefaultInference(ty, _) ->
        OlyILValue.Default(emitILType cenv env ty)
    | BoundLiteral.ConstantEnum(constant, enumTy) ->
        OlyILValue.ConstantEnum(GenConstant cenv env constant, emitILType cenv env enumTy)
    | BoundLiteral.Constant(ConstantSymbol.TypeVariable(tyPar)) ->
        let index, ilKind = GenTypeParameterAsILTypeVariableInfo env tyPar
        OlyILValue.Constant(OlyILConstant.TypeVariable(index, ilKind))
    | _ ->
        failwith "Internal ILGen Error: Literal is invalid."

and setLocalContext env (ent: EntitySymbol) =
    { env with context = LocalContext.Entity ent }

and setLocalContextWithEnclosing env (enclosing: EnclosingSymbol) =
    match enclosing with
    | EnclosingSymbol.Entity(ent) ->
        if ent.IsNamespace then
            { env with context = LocalContext.Namespace ent.FullNamespacePath }
        else
            { env with context = LocalContext.Entity ent }
    | _ ->
        env

and GenTryExpression (cenv: cenv) (env: env) (bodyExpr: BoundExpression) (catchCases: BoundCatchCase imarray) (finallyBodyExprOpt: BoundExpression option) =
    let ilBodyExpr = GenExpression cenv env bodyExpr
    let ilCatchCases =
        catchCases
        |> ImArray.map (function
            | BoundCatchCase.CatchCase(value, catchBodyExpr) ->
                let ilTy = emitILType cenv env value.Type
                let name = value.Name
                let localId = value.Id
                let ilLocal = 
                    let flags = if value.IsMutable then OlyILLocalFlags.Mutable else OlyILLocalFlags.None
                    OlyILLocal(cenv.funEnv.ilLocals.Count, GenString cenv name, ilTy, flags)

                cenv.funEnv.scopedLocals.Add(localId, ilLocal) // this will protect against accidently adding the same local twice
                cenv.funEnv.ilLocals.Add(ilLocal)

                let ilCatchBodyExpr = GenExpression cenv env catchBodyExpr
                OlyILCatchCase.CatchCase(ilLocal.Index, ilCatchBodyExpr)
        )
    let ilFinallyExprBodyOpt =
        finallyBodyExprOpt
        |> Option.map (GenExpression cenv {env with isReturnable = false })

    OlyILExpression.Try(ilBodyExpr, ilCatchCases, ilFinallyExprBodyOpt)

#if DEBUG
and GenExpression (cenv: cenv) prevEnv (expr: E) : OlyILExpression =
    DebugStackGuard.Do(fun () ->
        GenExpressionAux cenv prevEnv expr
    )
and GenExpressionAux (cenv: cenv) prevEnv (expr: E) : OlyILExpression =
#else
and GenExpression (cenv: cenv) prevEnv (expr: E) : OlyILExpression =
#endif
    let possiblyReturnableEnv = prevEnv
    let env =
        if prevEnv.isReturnable then
            { prevEnv with isReturnable = false }
        else
            prevEnv

    let syntaxNode =
        // We try to get the root name for consistent debugging experiences when using
        // fully-qualified values i.e. 'ModuleA.ModuleB.callingFunction()'
        expr.Syntax.GetRootNameIfPossible()
    let ilTextRange = emitTextRange cenv syntaxNode
    match expr with
    | E.None _ -> OlyILExpressionNone

    | E.Try(_, bodyExpr, catchCases, finallyBodyExprOpt) ->
        GenTryExpression cenv env bodyExpr catchCases finallyBodyExprOpt

    | E.Witness(_, _, _, bodyExpr, witnessArgOptRef, exprTy) ->
        match witnessArgOptRef.contents with
        | None ->
            OlyILExpression.Operation(ilTextRange,
                OlyILOperation.Cast(GenExpression cenv env bodyExpr, emitILType cenv env exprTy)
            )
        | Some(witnessArg) ->
            if not witnessArg.IsTypeExtension && not witnessArg.IsTypeVariable then
                failwith "Expected type extension or type parameter."
            OlyILExpression.Operation(ilTextRange, 
                OlyILOperation.Witness(
                    GenExpression cenv env bodyExpr, 
                    emitILType cenv env witnessArg,
                    emitILType cenv env exprTy
                )
            )

    | E.Sequential(syntaxInfo,
        E.Sequential(_, expr1, expr2, _),
        expr3,
        _
      ) ->
      // TODO: We should throw if we actually encounter this pattern. Should be handled in lowering.
      E.Sequential(syntaxInfo,
        expr1,
        E.Sequential(BoundSyntaxInfo.Generated(cenv.syntaxTree), expr2, expr3, NormalSequential),
        NormalSequential
      )
      |> GenExpression cenv prevEnv

    | E.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
        let syntaxDebugNode =
            match syntaxInfo.Syntax with
            | :? OlySyntaxExpression as syntaxExpr ->
                match syntaxExpr with
                | OlySyntaxExpression.ValueDeclaration(_, _, _, _, _, syntaxBinding) ->
                    match syntaxBinding with
                    | OlySyntaxBinding.Implementation(_, equalToken, _) ->
                        equalToken :> OlySyntaxNode
                    | _ ->
                        cenv.syntaxTree.DummyNode
                | _ ->
                    cenv.syntaxTree.DummyNode
            | _ ->
                cenv.syntaxTree.DummyNode
        GenLetExpression cenv possiblyReturnableEnv syntaxDebugNode bindingInfo rhsExpr (Some bodyExpr)

    | E.Sequential(_, expr1, expr2, _) ->
        match expr1 with
        | E.None _ ->
            GenExpression cenv possiblyReturnableEnv expr2
        | _ ->
            let ilExpr1 = GenExpression cenv env expr1
            let ilExpr2 = GenExpression cenv possiblyReturnableEnv expr2
            OlyILExpression.Sequential(ilExpr1, ilExpr2)

    | NewRefCell(expr) ->
        let ilElementTy = emitILType cenv env expr.Type
        let ilOp = OlyILOperation.NewRefCell(ilElementTy, GenExpression cenv env expr)
        OlyILExpression.Operation(ilTextRange, ilOp)

    | GetRefCellContents(expr) ->
        let ilElementTy = emitILType cenv env expr.Type.TryGetReferenceCellElement.Value
        let ilOp = OlyILOperation.LoadRefCellContents(ilElementTy, GenExpression cenv env expr)
        OlyILExpression.Operation(ilTextRange, ilOp)

    | SetRefCellContents(expr, rhs) ->
        let ilArg1 = GenExpression cenv env expr
        let ilArg2 = GenExpression cenv env rhs
        let ilOp = OlyILOperation.StoreRefCellContents(ilArg1, ilArg2)
        OlyILExpression.Operation(ilTextRange, ilOp)

    | E.Error _ 
    | E.ErrorWithNamespace _
    | E.ErrorWithType _ ->
        failwith "Cannot generate an error expression."

    | E.NewTuple(_, exprs, ty) ->
        if exprs.Length <= 1 then
            failwith "Cannot generate an invalid tuple expression."

        let ilParTys =
            exprs
            |> ImArray.map (fun expr -> emitILType cenv env expr.Type)

        let ilArgExprs =
            exprs
            |> ImArray.map (fun expr -> GenExpression cenv { env with isInArg = true } expr)

        let ilNameHandles =
            match stripTypeEquationsAndBuiltIn ty with
            | TypeSymbol.Tuple(_, names) ->
                GenTupleNames cenv names
            | _ ->
                ImArray.empty

        let ilOp = OlyILOperation.NewTuple(ilParTys, ilArgExprs, ilNameHandles)
        OlyILExpression.Operation(ilTextRange, ilOp)

    | E.NewArray(_, _, exprs, exprTy) ->
        let ilElementTy = emitILType cenv env exprTy.TypeArguments.[0]

        let ilArgExprs =
            exprs
            |> ImArray.map (fun expr -> GenExpression cenv { env with isInArg = true } expr)

        let ilKind =
            match stripTypeEquationsAndBuiltIn exprTy with
            | TypeSymbol.Array(kind=ArrayKind.Immutable) ->
                OlyILArrayKind.Immutable
            | _ ->
                OlyILArrayKind.Mutable

        let ilOp = OlyILOperation.NewArray(ilElementTy, ilKind, ilArgExprs)
        OlyILExpression.Operation(ilTextRange, ilOp)

    | E.Unit _ ->
        OlyILExpression.Value(ilTextRange, OlyILValue.Unit)

    | E.Call(syntaxInfo, receiverOpt, witnessArgs, argExprs, value, isVirtualCall) ->
        OlyAssert.False(value.IsProperty)
        OlyAssert.False(value.IsInvalid)
        GenCallExpression cenv possiblyReturnableEnv syntaxInfo receiverOpt witnessArgs argExprs value isVirtualCall

    | E.EntityDefinition(_, body, ent) ->
        // It may be possible that several local expressions may contain
        // the same local entity definition. If so, only do it once.
        if not ent.Enclosing.IsLocalEnclosing || cenv.funEnv.localEntities.Add(ent.Id) then
            GenEntityDefinitionNoCache cenv env ent
            |> ignore
            GenExpression cenv (setLocalContext env ent) body
        else
            OlyILExpressionNone

    | E.Value(value=value) ->
        match stripTypeEquations value.Type with
        | TypeSymbol.Unit -> OlyILExpressionNone
        | _ -> GenValueExpression cenv possiblyReturnableEnv ilTextRange ValueNone ImArray.empty value ImArray.empty

    | E.Literal(_, lit) ->
        OlyILExpression.Value(ilTextRange, GenValueLiteral cenv env lit)

    | E.MemberDefinition(_, binding) ->
#if DEBUG
        OlyAssert.False(binding.Info.Value.IsLocal)
        OlyAssert.True(binding.Info.Value.IsFormal)
#endif
        match binding with
        | BoundBinding.Implementation(syntaxInfo, bindingInfo, rhs) when not bindingInfo.Value.IsField ->
            let syntaxDebugNode =
                match syntaxInfo.Syntax with
                | :? OlySyntaxBinding as syntaxBinding ->
                    match syntaxBinding with
                    | OlySyntaxBinding.Implementation(_, equalToken, _) ->
                        equalToken :> OlySyntaxNode
                    | _ ->
                        cenv.syntaxTree.DummyNode
                | _ ->
                    cenv.syntaxTree.DummyNode
            GenMemberDefinitionExpression cenv env syntaxDebugNode bindingInfo rhs None

        | BoundBinding.Signature(bindingInfo=bindingInfo) when bindingInfo.Value.IsFunction ->
            GenFunctionAsILFunctionDefinition cenv env (bindingInfo.Value :?> IFunctionSymbol)
            |> ignore
            OlyILExpressionNone

        | BoundBinding.Signature(bindingInfo=bindingInfo) when bindingInfo.Value.IsProperty ->
            match bindingInfo with
            | BindingProperty(_, prop) ->
                GenAutoOrSignatureProperty cenv env prop
                |> ignore
                OlyILExpressionNone
            | _ ->
                OlyAssert.Fail("Expected binding property.")

        | _ ->
            OlyILExpressionNone

    | E.GetField(receiver=receiver;field=field) ->
        GenGetFieldExpression cenv env ilTextRange ValueNone receiver field

    | E.SetField(receiver=receiver;field=field;rhs=rhs) ->
        if (not env.isInConstructor && not field.IsMutable) ||
            // TODO: Does not take into account subsumption
           (env.isInConstructor && not (areEnclosingsEqual env.context.RealEntity.AsEnclosing field.Enclosing)) then
            failwith ($"Field must be mutable. Field: {field.Name}, EnclosingType: {field.Enclosing.AsType.DebugName}")
        GenSetFieldExpression cenv env ilTextRange (Some receiver) field rhs

    | E.SetValue(value=value;rhs=rhs) ->
#if DEBUG
        if value.IsLocal && not value.IsMutable then
            failwith "Value must be mutable."

        OlyAssert.False(value.IsFunction)
        OlyAssert.False(value.Type.IsByRef_t)
#endif
        GenSetValueExpression cenv env ilTextRange value rhs

    | E.SetContentsOfAddress(lhs=lhs;rhs=rhs) ->
        GenSetContentsOfAddressExpression cenv env ilTextRange lhs rhs

    | E.IfElse(_, conditionExpr, trueTargetExpr, falseTargetExpr, _) ->
        let ilConditionExpr = GenExpression cenv env conditionExpr
        let ilTrueTargetExpr = GenExpression cenv possiblyReturnableEnv trueTargetExpr
        let ilFalseTargetExpr = GenExpression cenv possiblyReturnableEnv falseTargetExpr

        OlyILExpression.IfElse(ilConditionExpr, ilTrueTargetExpr, ilFalseTargetExpr)

    | E.While(_, conditionExpr, bodyExpr) ->
        let ilConditionExpr = GenExpression cenv env conditionExpr
        let ilBodyExpr = GenExpression cenv env bodyExpr

        OlyILExpression.While(ilConditionExpr, ilBodyExpr)

    | E.Lambda(syntaxInfo, lambdaFlags, tyPars, pars, lazyBodyExpr, _, _, _) ->
        let bodyExpr = lazyBodyExpr.Expression
        let freeTyVars = expr.GetLogicalFreeTypeVariables()
        let freeLocals = expr.GetLogicalFreeAnyLocals()

        let oldFunEnv = cenv.funEnv
        cenv.funEnv <- FunctionEnv.Create()

        pars
        |> ImArray.iter (fun par ->
            let name = par.Name
            let ilTy = emitILType cenv env par.Type
            let localId = par.Id
            let ilLocal = 
                let flags = if par.IsMutable then OlyILLocalFlags.Mutable else OlyILLocalFlags.None
                OlyILLocal(cenv.funEnv.ilLocals.Count, GenString cenv name, ilTy, flags)

            cenv.funEnv.scopedLocals.Add(localId, ilLocal) // this will protect against accidently adding the same local twice
            cenv.funEnv.ilLocals.Add(ilLocal)
        )

        let ilLocals = cenv.funEnv.ilLocals.ToImmutable()
        let ilTyPars = emitILTypeParameters cenv env tyPars

        freeLocals
        |> ImArray.iter (fun value ->
            let name = value.Name
            let ilTy = emitILType cenv env value.Type
            let localId = value.Id
            let ilLocal = 
                let flags = if value.IsMutable then OlyILLocalFlags.Mutable else OlyILLocalFlags.None
                OlyILLocal(cenv.funEnv.ilLocals.Count, GenString cenv name, ilTy, flags)

            cenv.funEnv.scopedLocals.Add(localId, ilLocal) // this will protect against accidently adding the same local twice
            cenv.funEnv.ilLocals.Add(ilLocal)
        )

        let ilFreeLocals = cenv.funEnv.ilLocals |> Seq.skip ilLocals.Length |> ImArray.ofSeq
        let ilFreeTyPars = emitILTypeParameters cenv env freeTyVars

        let ilBodyExpr = GenExpression cenv env bodyExpr
        cenv.funEnv <- oldFunEnv
        OlyILExpression.Lambda(ilFreeLocals, ilFreeTyPars, ilLocals, ilTyPars, ilBodyExpr)


    | E.Match _
    | E.GetProperty _
    | E.SetProperty _ 
    | E.Typed _ ->
        failwith "Unexpected expression. Should be removed in lowering."

and GenSetFieldExpression (cenv: cenv) env _ilTextRange receiverOpt field rhsExpr =
    let ilReceiverOpt : OlyILExpression option =
        receiverOpt
        |> Option.map (fun receiver ->
            GenExpression cenv { env with isReturnable = false } receiver
        )

    let ilRhsExpr = GenExpression cenv env rhsExpr
    let ilFieldRef = GenFieldAsILFieldReference cenv env field
    let ilOp =
        match ilReceiverOpt with
        | Some ilReceiver ->
            OlyILOperation.StoreField(ilFieldRef, ilReceiver, ilRhsExpr)
        | _ ->
            OlyILOperation.StoreStaticField(ilFieldRef, ilRhsExpr)
    OlyILExpression.Operation(OlyILDebugSourceTextRange.Empty, ilOp)

and GenSetValueExpression (cenv: cenv) env ilTextRange value rhsExpr =
    match value with
    | :? IFieldSymbol as field ->
        GenSetFieldExpression cenv env ilTextRange None field rhsExpr
    | _ ->
        let ilRhsExpr = GenExpression cenv env rhsExpr
        match cenv.funEnv.scopedLocals.TryGetValue value.Id with
        | true, local ->
            let ilOp = OlyILOperation.Store(local.Index, ilRhsExpr)
            OlyILExpression.Operation(OlyILDebugSourceTextRange.Empty, ilOp)
        | _ ->
            match cenv.funEnv.arguments.TryGetValue value.Id with
            | true, n ->
                let ilOp = OlyILOperation.StoreArgument(n, ilRhsExpr)
                OlyILExpression.Operation(OlyILDebugSourceTextRange.Empty, ilOp)
            | _ ->
                failwithf "Local '%s' does not exist" value.Name

and GenSetContentsOfAddressExpression (cenv: cenv) env _ilTextRange lhsExpr rhsExpr =
#if DEBUG
    OlyAssert.True(lhsExpr.Type.IsByRef_t || lhsExpr.Type.IsAnyPtr)
    OlyAssert.False(lhsExpr.Type.IsReadOnlyByRef)
#endif
    let ilLhsExpr = GenExpression cenv env lhsExpr
    let ilRhsExpr = GenExpression cenv env rhsExpr
    let ilOp = OlyILOperation.StoreToAddress(ilLhsExpr, ilRhsExpr)
    OlyILExpression.Operation(OlyILDebugSourceTextRange.Empty, ilOp)

and GenLoadFieldExpression cenv env ilTextRange (takeAddress: OlyILByRefKind voption) (field: IFieldSymbol) (ilArgExprs: OlyILExpression imarray) =
    match field.Constant with
    | ValueSome _ ->
        failwith "Did not expect a field constant."
    | _ ->
        ()

    let ilFieldRef = GenFieldAsILFieldReference cenv env field
    if field.IsInstance then
        let ilOp =
            match takeAddress with
            | ValueSome ilByRefKind ->
                OlyILOperation.LoadFieldAddress(ilFieldRef, ilArgExprs.[0], ilByRefKind)
            | _ ->
                OlyILOperation.LoadField(ilFieldRef, ilArgExprs.[0])
        OlyILExpression.Operation(ilTextRange, ilOp)
    else
        let ilValue =
            match takeAddress with
            | ValueSome ilByRefKind ->
                OlyILValue.StaticFieldAddress(ilFieldRef, ilByRefKind)
            | _ ->
                OlyILValue.StaticField ilFieldRef
        OlyILExpression.Value(ilTextRange, ilValue)

and GenValueExpression (cenv: cenv) env ilTextRange (takeAddress: OlyILByRefKind voption) (witnessArgs: WitnessSolution imarray) (value: IValueSymbol) (ilArgExprs: OlyILExpression imarray) : OlyILExpression =
    OlyAssert.False(value.IsFunction)

    if value.IsLocal && not(value.IsParameter) && (env.locals.Contains(value.Id) |> not) then
        failwith $"Local '{value.Name}' is missing or is out-of-scope."

    let env = { env with isReturnable = false }
    match cenv.funEnv.scopedLocals.TryGetValue value.Id with
    | true, local ->
        match takeAddress with
        | ValueSome ilByRefKind ->
            OlyILExpression.Value(ilTextRange, OlyILValue.LocalAddress(local.Index, ilByRefKind))
        | _ ->
            OlyILExpression.Value(ilTextRange, OlyILValue.Local local.Index)
    | _ ->
        match cenv.funEnv.arguments.TryGetValue value.Id with
        | true, n ->
            match takeAddress with
            | ValueSome ilByRefKind ->
                OlyILExpression.Value(ilTextRange, OlyILValue.ArgumentAddress(n, ilByRefKind))
            | _ ->
                OlyILExpression.Value(ilTextRange, OlyILValue.Argument n)
        | _ ->
            match value.Enclosing with
            | EnclosingSymbol.Local ->
                failwithf "Internal error: local '%s' does not exist" value.Name
            | _ ->
                match value with
                | :? IFunctionSymbol as func when not func.IsInstance ->
                    OlyAssert.Fail("Invalid use of static function.")
                | :? IFunctionSymbol as func when func.IsInstanceConstructor ->
                    let ilFuncInst = GenFunctionAsILFunctionInstance cenv env witnessArgs func
                    OlyILExpression.Operation(ilTextRange, OlyILOperation.New(ilFuncInst, ilArgExprs))
                | :? IFieldSymbol as field ->
                    GenLoadFieldExpression cenv env ilTextRange takeAddress field ilArgExprs
                | _ ->
                    failwithf "Internal error: local '%s' does not exist" value.Name

and GenCallArgumentExpressions (cenv: cenv) env (value: IValueSymbol) (argExprs: E imarray) =
    let valueTy = value.LogicalType
#if DEBUG
    match valueTy.TryFunction with
    | ValueSome _ -> ()
    | _ -> OlyAssert.Fail("Expected function type.")
#endif

    let expectedArgTys = valueTy.FunctionArgumentTypes

    (argExprs, expectedArgTys)
    ||> ImArray.map2 (fun argExpr expectedArgTy ->
        let argExprTy = argExpr.Type

        let isExpectingRealUnitType =
            match stripTypeEquations expectedArgTy with
            | TypeSymbol.Unit -> true
            | TypeSymbol.BaseObject ->
                match stripTypeEquations argExprTy with
                | TypeSymbol.Unit -> true
                | _ -> false
            | _ -> 
                false

        let env = 
            { env with 
                isReturnable = false
                isInArg = true }
        let ilArgExpr = GenExpression cenv env argExpr
        if isExpectingRealUnitType then
            OlyILExpression.Sequential(
                OlyILExpression.Operation(OlyILDebugSourceTextRange.Empty, OlyILOperation.Ignore(ilArgExpr)),
                OlyILExpression.Value(OlyILDebugSourceTextRange.Empty, OlyILValue.Unit)
            )
        else
            if expectedArgTy.IsFunction_t && argExprTy.IsClosure then
                failwith "Should be handled earlier in lowering."
            else
                ilArgExpr                
    )

and GenCallExpression (cenv: cenv) env (syntaxInfo: BoundSyntaxInfo) (receiverOpt: E option) (witnessArgs: WitnessSolution imarray) (argExprs: E imarray) (value: IValueSymbol) isVirtualCall =
    let syntaxNode = syntaxInfo.Syntax
    let ilTextRange = emitTextRange cenv syntaxNode

    if not value.TypeParameters.IsEmpty && value.Formal = value then
        OlyAssert.Fail "Unexpected formal value."

    match value with
    | :? IFunctionSymbol as func ->
        if func.LogicalParameterCount <> argExprs.Length then
            OlyAssert.Fail "Unable to code-gen partial call."
    | _ ->
        ()  

    let ilReceiverOpt =
        receiverOpt
        |> Option.map (fun receiver ->
            if value.IsInstance && value.Enclosing.IsAnyStruct && not receiver.Type.IsByRef_t then
                failwith "Expected a by-reference type for function call."
            GenExpression cenv { env with isReturnable = false } receiver
        )

    match value.TryWellKnownFunction with
    | ValueSome(WellKnownFunction.NewMutableArray) ->
        if argExprs.Length <> 1 || value.TypeParameters.Length <> 1 then
            failwith "Invalid NewMutableArray"

        match value with
        | :? IFunctionSymbol as func ->
            match stripTypeEquationsAndBuiltIn func.ReturnType with
            | TypeSymbol.Array(kind=kind) ->
                match kind with
                | ArrayKind.Immutable ->
                    failwith "Invalid NewMutableArray"
                | ArrayKind.Mutable ->
                    ()
            | _ ->
                failwith "Invalid NewMutableArray"
        | _ ->
            failwith "Invalid NewMutableArray"

        OlyILExpression.Operation(
            ilTextRange,
            OlyILOperation.NewMutableArray(emitILType cenv env value.TypeArguments[0], GenExpression cenv env argExprs[0])
        )

    | ValueSome(WellKnownFunction.GetTupleElement) ->
        if argExprs.Length <> 1 || ilReceiverOpt.IsSome || value.TypeParameters.Length <> 2 then
            failwith "Invalid GetTupleElement"

        match stripTypeEquationsAndBuiltIn value.TypeArguments[0] with
        | TypeSymbol.ConstantInt32(index) ->
            OlyILExpression.Operation(
                ilTextRange,
                OlyILOperation.LoadTupleElement(GenExpression cenv env argExprs[0], index)
            )
        | _ ->
            failwith "Invalid GetTupleElement"

    | ValueSome(WellKnownFunction.UnsafeAddressOf)
    | ValueSome(WellKnownFunction.AddressOf) when argExprs.Length = 1 && ilReceiverOpt.IsNone ->
        let argExpr = argExprs.[0]
        if argExpr.Type.IsByRef_t then GenExpression cenv { env with isReturnable = false; isInArg = true } argExpr
        else

        let rec emitAddressOf ilByRefKind argExpr =
            match argExpr with
            | E.Value(value=value) ->
                GenValueExpression cenv env ilTextRange (ValueSome ilByRefKind) witnessArgs value ImArray.empty
            | E.GetField(receiver=receiver;field=field) ->
                GenGetFieldExpression cenv env ilTextRange (ValueSome ilByRefKind) receiver field
            | E.Call(value=value) when value.TryWellKnownFunction = ValueSome WellKnownFunction.GetArrayElement ->
                match GenExpression cenv env argExpr with
                | OlyILExpression.Operation(ilTextRange, OlyILOperation.LoadArrayElement(ilReceiver, ilIndexArgs)) ->
                    OlyILExpression.Operation(ilTextRange, OlyILOperation.LoadArrayElementAddress(ilReceiver, ilIndexArgs, ilByRefKind))
                | _ ->
                    failwith "assert"
            | AutoDereferenced(expr) ->
                GenExpression cenv env expr
            | GetRefCellContents(expr) ->
                let ilElementTy = emitILType cenv env expr.Type.TryGetReferenceCellElement.Value
                let ilExpr = GenExpression cenv env expr
                let ilByRefKind =
                    if argExpr.Type.IsReadOnlyByRef then
                        OlyILByRefKind.Read
                    else
                        OlyILByRefKind.ReadWrite
                OlyILExpression.Operation(ilTextRange, OlyILOperation.LoadRefCellContentsAddress(ilElementTy, ilExpr, ilByRefKind))
            | _ ->
                failwith "Invalid use of intrinsic 'address_of'"

        let ilByRefKind =
            match value.TryWellKnownFunction with
            | ValueSome(WellKnownFunction.UnsafeAddressOf) ->
                OlyILByRefKind.ReadWrite
            | _ ->
                match value with
                | :? IFunctionSymbol as func ->
                    if func.ReturnType.IsReadOnlyByRef then
                        OlyILByRefKind.Read
                    else
                        OlyILByRefKind.ReadWrite
                | _ ->
                    OlyILByRefKind.ReadWrite
        emitAddressOf ilByRefKind argExpr

    | ValueSome(WellKnownFunction.FromAddress) when argExprs.Length = 1 && ilReceiverOpt.IsNone ->
        let ilArgExpr =
            match argExprs.[0] with
            | E.Value(value=value) ->
                GenValueExpression cenv env ilTextRange ValueNone witnessArgs value ImArray.empty
            | E.GetField(receiver=receiver;field=field) ->
                GenGetFieldExpression cenv env ilTextRange ValueNone receiver field
            | _ ->
                failwith "Invalid use of intrinsic 'from_address'"

        OlyILExpression.Operation(ilTextRange, OlyILOperation.LoadFromAddress(ilArgExpr))

    | ValueSome(WellKnownFunction.LoadFunction) when argExprs.Length = 2 ->
        let ilArgExpr =
            GenExpression cenv env argExprs[0]
        let ilFuncInst =               
            match argExprs[1] with
            | E.Value(_, value) when value.IsFunction && value.Enclosing.AsType.IsClosure && value.IsInstance && value.Name = "Invoke" ->
                GenFunctionAsILFunctionInstance cenv env witnessArgs (value :?> IFunctionSymbol)
            | _ ->
                failwith "Expected closure instance invoke."
        OlyILExpression.Operation(ilTextRange,
            OlyILOperation.LoadFunction(
                ilFuncInst,
                ilArgExpr
            )
        )

    | ValueSome(WellKnownFunction.LoadStaticFunction) when argExprs.Length = 1 ->
        let ilFuncInst =               
            match argExprs[0] with
            | E.Value(_, value) when value.IsFunction && not value.IsInstance ->
                GenFunctionAsILFunctionInstance cenv env witnessArgs (value :?> IFunctionSymbol)
            | _ ->
                failwith "Expected static function."
        OlyILExpression.Value(ilTextRange, OlyILValue.Function(ilFuncInst))

    | ValueSome(WellKnownFunction.LoadFunctionPtr) when argExprs.Length = 1 ->
        match argExprs[0] with
        | E.Value(syntaxInfo, value) when value.IsFunction ->
            let ilTextRange = emitTextRange cenv syntaxInfo.Syntax
            let ilFuncInst = GenFunctionAsILFunctionInstance cenv env witnessArgs (value :?> IFunctionSymbol)
            OlyILExpression.Value(ilTextRange, OlyILValue.FunctionPtr(ilFuncInst))
        | _ ->
            failwith "Invalid 'LoadFunctionPtr'."

    | ValueSome(WellKnownFunction.LoadNullPtr) when argExprs.Length = 0 ->
        let ilTextRange = emitTextRange cenv syntaxInfo.Syntax
        let func = value.AsFunction
        OlyILExpression.Value(ilTextRange, OlyILValue.Default(emitILType cenv env func.ReturnType))

    | wellKnownFuncKind -> 
        match wellKnownFuncKind with
        | ValueSome(WellKnownFunction.LoadFunction) 
        | ValueSome(WellKnownFunction.LoadStaticFunction)
        | ValueSome(WellKnownFunction.LoadFunctionPtr) -> 
            failwithf "'%A' is not handled." wellKnownFuncKind
        | _ -> ()

        let ilArgExprs = GenCallArgumentExpressions cenv env value argExprs

        let ilOp =
            match value with
            | :? IFunctionSymbol as func ->
                match func.TryWellKnownFunction with
                | ValueSome funcIntrin when 
                        not func.IsImported && 
                        (match funcIntrin with WellKnownFunction.Constant -> false | _ -> true) ->
                    match funcIntrin with
                    | WellKnownFunction.Print ->
                        OlyILOperation.Print(ilArgExprs.[0])
                    | WellKnownFunction.Throw ->
                        OlyILOperation.Throw(ilArgExprs.[0], emitILType cenv env func.ReturnType)
                    | WellKnownFunction.Cast 
                    | WellKnownFunction.UnsafeCast ->
                        OlyILOperation.Cast(ilArgExprs.[0], emitILType cenv env func.ReturnType)
                    | WellKnownFunction.Ignore ->
                        OlyILOperation.Ignore(ilArgExprs.[0])
                    | WellKnownFunction.Add ->
                        OlyILOperation.Add(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.Subtract ->
                        OlyILOperation.Subtract(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.Multiply ->
                        OlyILOperation.Multiply(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.Divide ->
                        OlyILOperation.Divide(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.Remainder ->
                        OlyILOperation.Remainder(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.Negate ->
                        OlyILOperation.Negate(ilArgExprs.[0])
                    | WellKnownFunction.Equal ->
                        OlyILOperation.Equal(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.NotEqual ->
                        OlyILOperation.NotEqual(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.GreaterThan ->
                        OlyILOperation.GreaterThan(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.GreaterThanOrEqual ->
                        OlyILOperation.GreaterThanOrEqual(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.LessThan ->
                        OlyILOperation.LessThan(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.LessThanOrEqual ->
                        OlyILOperation.LessThanOrEqual(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.Not ->
                        OlyILOperation.Not(ilArgExprs.[0])
                    | WellKnownFunction.And ->
                        failwith "Invald use of And"
                    | WellKnownFunction.Or ->
                        failwith "Invald use of Or"
                    | WellKnownFunction.BitwiseNot ->
                        OlyILOperation.BitwiseNot(ilArgExprs.[0])
                    | WellKnownFunction.BitwiseOr ->
                        OlyILOperation.BitwiseOr(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.BitwiseExclusiveOr ->
                        OlyILOperation.BitwiseExclusiveOr(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.BitwiseAnd ->
                        OlyILOperation.BitwiseAnd(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.BitwiseShiftLeft ->
                        OlyILOperation.BitwiseShiftLeft(ilArgExprs.[0], ilArgExprs.[1])
                    | WellKnownFunction.BitwiseShiftRight ->
                        OlyILOperation.BitwiseShiftRight(ilArgExprs.[0], ilArgExprs.[1])

                    | WellKnownFunction.GetArrayLength ->
                        OlyILOperation.LoadArrayLength(ilArgExprs.[0])
                    | WellKnownFunction.GetArrayElement ->
                        OlyILOperation.LoadArrayElement(ilArgExprs.[0], ilArgExprs.RemoveAt(0))
                    | WellKnownFunction.SetArrayElement ->
                        let ilReceiver = ilArgExprs[0]
                        let ilIndexArgExprs = ilArgExprs.RemoveAt(0).RemoveAt(ilArgExprs.Length - 2)
                        let ilArgExpr = ilArgExprs.[ilArgExprs.Length - 1]
                        OlyILOperation.StoreArrayElement(ilReceiver, ilIndexArgExprs, ilArgExpr)

                    | WellKnownFunction.LoadFunctionPtr ->
                        failwith "Not yet."

                    | _ ->
                        OlyAssert.Fail(sprintf "Invalid intrinsic function '%A'." funcIntrin)
                | _ ->

                    let ilArgExprs =
                        match ilReceiverOpt with
                        | Some ilReceiver ->
                            if not func.IsInstance then
                                failwith "Expected an instance member function with a receiver."
                            ImArray.createOne(ilReceiver).AddRange(ilArgExprs)
                        | _ ->
                            ilArgExprs

                    let ilFuncInst = GenFunctionAsILFunctionInstance cenv env witnessArgs func

                    if func.IsConstructor && receiverOpt.IsNone then
                        OlyAssert.False(func.IsStackEmplace)
                        OlyILOperation.New(ilFuncInst, ilArgExprs)
                    else
                        if func.Parameters.Length <> ilArgExprs.Length then
                            OlyAssert.Fail($"Parameters and arguments do not match - {func.Name}")
                        if isVirtualCall then
                            OlyAssert.False(func.IsStackEmplace)
                            OlyILOperation.CallVirtual(ilFuncInst, ilArgExprs)
                        else
                            OlyILOperation.Call(ilFuncInst, ilArgExprs)
            | _ ->
                let ilTyInst, ilWitnesses = GenValueTypeArgumentsAndWitnessArguments cenv env value witnessArgs

                let ilFunArgExpr =
                    match ilReceiverOpt with
                    | Some ilReceiver -> 
                        if value.IsInstance && value.IsField then
                            OlyILExpression.Operation(ilTextRange,
                                OlyILOperation.LoadField(
                                    GenFieldAsILFieldReference cenv env (value :?> IFieldSymbol), 
                                    ilReceiver
                                )
                            )
                        else
                            failwith "Invalid function argument for CallIndirect."

                    | _ ->
                        if value.IsField && not value.IsInstance then
                            OlyILExpression.Value(ilTextRange,
                                OlyILValue.StaticField(
                                    GenFieldAsILFieldReference cenv env (value :?> IFieldSymbol)
                                )
                            )
                        else
                            match cenv.funEnv.scopedLocals.TryGetValue value.Formal.Id with
                            | true, local ->
                                OlyILExpression.Value(ilTextRange, OlyILValue.Local local.Index)
                            | _ ->
                                match cenv.funEnv.arguments.TryGetValue value.Formal.Id with
                                | true, n ->
                                    OlyILExpression.Value(ilTextRange, OlyILValue.Argument n)
                                | _ ->
                                    failwith "Invalid function argument for CallIndirect."

                OlyAssert.Equal(0, ilTyInst.Length)
                OlyAssert.Equal(0, ilWitnesses.Length)

                OlyILOperation.CallIndirect(ilFunArgExpr, ilArgExprs)
        
        OlyILExpression.Operation(ilTextRange, ilOp)

and GenGetFieldExpression (cenv: cenv) env ilTextRange (takeAddress: OlyILByRefKind voption) receiver (field: IFieldSymbol) =
    let noReturnEnv = 
        if env.isReturnable then
            { env with isReturnable = false }
        else
            env

    let ilReceiver = GenExpression cenv noReturnEnv receiver
    GenLoadFieldExpression cenv env ilTextRange takeAddress field (ImArray.createOne ilReceiver)

and GenMemberDefinitionExpression cenv env (syntaxDebugNode: OlySyntaxNode) (bindingInfo: BindingInfoSymbol) (rhsExpr: E) (bodyExprOpt: E option) : OlyILExpression =
    match bindingInfo with
    | BindingProperty _ ->
        OlyAssert.True(bodyExprOpt.IsNone)
        GenExpression cenv env rhsExpr

    | BindingFunction(func)
    | BindingPattern(_, func) ->
        GenFunctionDefinitionExpression cenv env syntaxDebugNode func rhsExpr
        OlyAssert.False(func.IsLocal)
        OlyAssert.True(bodyExprOpt.IsNone)
        OlyILExpressionNone
      
    | BindingField _ ->
        OlyILExpressionNone

and GenLetExpressionAux cenv env (syntaxDebugNode: OlySyntaxNode) (bindingInfo: LocalBindingInfoSymbol) (rhsExpr: E) (bodyExprOpt: E option) (value: IValueSymbol) : OlyILExpression =
    // TODO: Unit logic is a bit complex, we need to simplify it.
        let mustBeRealUnit = 
            match stripTypeEquations value.Type with
            | TypeSymbol.Unit -> true
            | ty -> ty.IsRealUnit
        let ilRhsExpr = GenExpression cenv { env with isReturnable = false } rhsExpr
        let value = bindingInfo.Value
        let isMutable = value.IsMutable

        let ilTy = emitILType cenv env value.Type
        let name = value.Name
        let localId = value.Id

        // Optimization for single-use local
        match bodyExprOpt with

        // Handles indirect calls.
        | Some(E.Call(syntaxInfo=syntaxInfo;receiverOpt=None;witnessArgs=witnessArgs;args=argExprs;value=callValue)) 
                when callValue.Id = localId && value.IsSingleUse && (value.IsGenerated || not cenv.assembly.IsDebuggable) ->

            let syntaxNode = syntaxInfo.Syntax
            let ilTextRange = emitTextRange cenv syntaxNode
            let ilArgExprs = GenCallArgumentExpressions cenv env value argExprs
            let _ilTyInst, _ilWitnesses = GenValueTypeArgumentsAndWitnessArguments cenv env value witnessArgs // TODO: Do we want indirect calls to pass witnessess and type arguments?

            OlyILExpression.Operation(ilTextRange, OlyILOperation.CallIndirect(ilRhsExpr, ilArgExprs))
        | _ ->

        let ilLocal = 
            let flags = if isMutable then OlyILLocalFlags.Mutable else OlyILLocalFlags.None
            OlyILLocal(cenv.funEnv.ilLocals.Count, GenString cenv name, ilTy, flags)

        cenv.funEnv.scopedLocals.Add(localId, ilLocal) // this will protect against accidently adding the same local twice
        cenv.funEnv.ilLocals.Add(ilLocal)

        let ilBodyExpr =
            match bodyExprOpt with
            | Some bodyExpr ->
                GenExpression cenv { env with locals = env.locals.Add(value.Id) } bodyExpr
            | _ ->
                OlyILExpression.None(OlyILDebugSourceTextRange.Empty)

        if mustBeRealUnit then
            match stripTypeEquations rhsExpr.Type with
            | ty when ty.IsRealUnit ->
                OlyILExpression.Let(ilLocal.Index, ilRhsExpr, ilBodyExpr)
            | _ ->
                let ilNewRhsExpr =
                    OlyILExpression.Sequential(
                        ilRhsExpr,
                        OlyILExpression.Value(OlyILDebugSourceTextRange.Empty, OlyILValue.Unit)
                    )
                OlyILExpression.Let(ilLocal.Index, ilNewRhsExpr, ilBodyExpr)
        else
            OlyILExpression.Let(ilLocal.Index, ilRhsExpr, ilBodyExpr)

and GenLetExpression cenv env (syntaxDebugNode: OlySyntaxNode) (bindingInfo: LocalBindingInfoSymbol) (rhsExpr: E) (bodyExprOpt: E option) : OlyILExpression =
    match bindingInfo with
    | BindingLocalFunction(func) ->
        GenLetExpressionAux cenv env syntaxDebugNode bindingInfo rhsExpr bodyExprOpt func
      
    | BindingLocal(value) ->
        GenLetExpressionAux  cenv env syntaxDebugNode bindingInfo rhsExpr bodyExprOpt value

and GenFunctionDefinitionLambdaExpression (cenv: cenv) env (pars: ILocalParameterSymbol imarray) (body: E) =
    pars
    |> ImArray.iter (addLocalArgument cenv.funEnv)
    GenExpression cenv { env with isReturnable = true } body

and GenFunctionDefinitionExpression (cenv: cenv) env (syntaxDebugNode: OlySyntaxNode) (func: IFunctionSymbol) (rhsExpr: E) : unit =
#if DEBUG
    OlyAssert.True(func.IsFormal)
    OlyAssert.False(func.Enclosing.IsShape)
    let freeTyVars = rhsExpr.GetLogicalFreeTypeVariables()
    let freeLocals = rhsExpr.GetLogicalFreeAnyLocals()
    if (freeTyVars.Length > 0 || freeLocals.Length > 0) && func.IsLocal then
        failwith "Expected free local and free variables to be lifted from the expression in lowering."
    else
        let isValid =
            match func.Enclosing.TryEntity with
            | Some(ent) ->
                OlyAssert.True(env.context.RealEntity.IsFormal)
                OlyAssert.Equal(ent.Id, env.context.RealEntity.Id)
                ent.Functions
                |> ImArray.exists (fun x -> x.Id = func.Id)
            | _ ->
                OlyAssert.False(env.context.RealEntity.IsNamespace)
                func.IsStaticLocalFunction ||
                    (match rhsExpr with E.Lambda(flags=flags) -> flags.HasFlag(LambdaFlags.Static) | _ -> false)

        if not isValid && not (func.FunctionFlags.HasFlag(FunctionFlags.Extra)) then
            failwithf "Function '%s' does not exist on its enclosing type." func.Name
#endif
        let cenv = cenv.NewFunction()
        let env = 
            { env with 
                isInInstance = func.IsInstance
                isInConstructor = func.IsConstructor
                locals = Collections.Immutable.ImmutableHashSet<_>.Empty 
            }

        let ilFuncDefHandle = GenFunctionAsILFunctionDefinition cenv env func
        let ilFuncDef = cenv.assembly.GetFunctionDefinition(ilFuncDefHandle)
        let ilBodyExpr =
            match rhsExpr with
            | E.Lambda(_, _, _, pars, body, _, _, _) ->
                GenFunctionDefinitionLambdaExpression cenv env pars body.Expression
            | _ ->
                failwith "Expected lambda expression for function definition."
        let ilLocals = cenv.funEnv.ilLocals.ToImmutable() // We need to create the locals after we finished going through the right-hand expr.

        let ilBodyExpr =
            if cenv.assembly.IsDebuggable then
                OlyILExpression.Sequential(OlyILExpression.None(emitTextRange cenv syntaxDebugNode), ilBodyExpr)
            else
                ilBodyExpr

        ilFuncDef.BodyHandle.contents <- cenv.assembly.AddFunctionBody(OlyILFunctionBody(ilLocals, ilBodyExpr)) |> Some

[<Sealed>]
type OlyILAssemblyGenerator(asm: AssemblySymbol, isDebuggable) =
    
    let ilAsm = OlyILAssembly.Create(asm.Identity.Name, asm.Identity.Key, isDebuggable) // TODO: Not deterministic, but we will fix it later.
    let mutable cenvOpt = ValueNone

    member _.ILAssembly = ilAsm

    member this.Generate(boundTree: BoundTree) =
        let cenv = 
            match cenvOpt with
            | ValueSome cenv -> { cenv with syntaxTree = boundTree.SyntaxTree }
            | _ -> cenv.Create(boundTree.SyntaxTree, ilAsm)
        cenvOpt <- ValueSome cenv

        let env = env.Create()

        let env =
            match boundTree.Root with
            | BoundRoot.Namespace(_, _, namespac, _) ->
                { env with context = LocalContext.Namespace (namespac.FullNamespacePath) }
            | BoundRoot.Global(_, _, _) ->
                { env with context = LocalContext.Namespace Seq.empty }

        let bodyExpr =
            match boundTree.Root with
            | BoundRoot.Namespace(body=bodyExpr) 
            | BoundRoot.Global(body=bodyExpr) -> bodyExpr

        GenExpression cenv env bodyExpr
        |> ignore

        while cenv.delayedEntityGenQueue.Count > 0 do
            let f = cenv.delayedEntityGenQueue.Dequeue()
            f()
