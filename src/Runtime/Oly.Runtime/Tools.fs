module rec Oly.Runtime.Tools

open Oly.Core
open Oly.Metadata
open Oly.Runtime.CodeGen

[<Sealed;NoComparison;NoEquality>]
type DummyType(name: string) =

    member _.Name = name

    override _.ToString() = $"Type({name})"

[<Sealed;NoComparison;NoEquality>]
type DummyFunction(name: string) =

    member _.Name = name

    override _.ToString() = $"Function({name})"

[<Sealed;NoComparison;NoEquality>]
type DummyField(name: string) =

    member _.Name = name

    override _.ToString() = $"Field({name})"

[<Sealed;NoComparison;NoEquality>]
type DummyEmitter(onEmitBody) =
    interface IOlyRuntimeEmitter<DummyType, DummyFunction, DummyField> with

        member _.EmitExportedProperty(_, _, _, _, _, _) =
            raise(System.NotSupportedException())

        member this.EmitExternalType(externalPlatform, externalPath, externalName, enclosing, kind, flags, name, tyParCount) =
            DummyType(name)

        member this.EmitField(enclosingTy, flags, name, ty, attrs, constValueOpt) =
            DummyField(name)

        member this.EmitFunctionBody(body, _, func) =
            onEmitBody func body

        member this.EmitFunctionDefinition(externalInfoOpt, enclosingTy, flags, name, tyPars, pars, returnTy, overrides, sigKey, attrs) =
            DummyFunction(name)

        member this.EmitFunctionInstance(enclosingTy, formalFunc, tyArgs) =
            formalFunc

        member this.EmitFunctionReference(enclosingTy, formalFunc) =
            formalFunc

        member this.EmitTypeArray(elementTy, rank, kind) = DummyType("array")
        member this.EmitTypeBaseObject() = DummyType("base_object")
        member this.EmitTypeBool() = DummyType("bool")
        member this.EmitTypeByRef(arg1, arg2) = DummyType("by_ref")
        member this.EmitTypeChar16() = DummyType("char16")
        member this.EmitTypeDefinition(enclosing, kind, flags, name, tyParCount) = 
            DummyType(name)
        member this.EmitTypeDefinitionInfo(_, enclosing, kind, flags, name, tyPars, extends, implements, attrs, runtimeTyOpt) = 
            ()
        member this.EmitTypeFloat32() = DummyType("float32")
        member this.EmitTypeFloat64() = DummyType("float64")
        member this.EmitTypeFunction(inputTys, outputTy) = DummyType("func")
        member this.EmitTypeNativeFunctionPtr(_, parTys, returnTy) = DummyType("func_ptr")
        member this.EmitTypeGenericInstance(ty, tyArgs) = ty
        member this.EmitTypeHigherVariable(index, tyInst, enclosingTyParCount) = DummyType("variable_constructor")
        member this.EmitTypeInt16() = DummyType("int16")
        member this.EmitTypeInt32() = DummyType("int32")
        member this.EmitTypeInt64() = DummyType("int64")
        member this.EmitTypeInt8() = DummyType("int8")
        member this.EmitTypeConstantInt32(value) = DummyType("literal_int32")
        member this.EmitTypeNativeInt() = DummyType("native_int")
        member this.EmitTypeNativePtr(elementTy) = DummyType("native_ptr")
        member this.EmitTypeNativeUInt() = DummyType("native_uint")
        member this.EmitTypeRefCell(ty) = DummyType("ref_cell")
        member this.EmitTypeTuple(elementTys, names) = DummyType("tuple")
        member this.EmitTypeUInt16() = DummyType("uint16")
        member this.EmitTypeUInt32() = DummyType("uint32")
        member this.EmitTypeUInt64() = DummyType("uint64")
        member this.EmitTypeUInt8() = DummyType("uint8")
        member this.EmitTypeUnit() = DummyType("unit")
        member this.EmitTypeUtf16() = DummyType("utf16")
        member this.EmitTypeVariable(index, enclosingTyParCount) = DummyType("variable")
        member this.EmitTypeVoid() = DummyType("void")

    new() = 
        DummyEmitter(fun _ (lazyIrBody: Lazy<_>) -> ())

let addEntity (ilAsm: OlyILAssembly) (ilEntDefHandle: OlyILEntityDefinitionHandle) (ilKind: OlyILEntityKind) (name: string) (tyPars: OlyILTypeParameter imarray) (funcDefs: OlyILFunctionDefinitionHandle imarray) (fieldDefs: OlyILFieldDefinitionHandle imarray) =
    let ilEntDef =
        OlyILEntityDefinition(
            ilKind,
            OlyILEntityFlags.Public,
            ImArray.empty,
            OlyILEnclosing.Namespace(ImArray.empty, ilAsm.Identity),
            ilAsm.AddString(name),
            tyPars,
            funcDefs,
            fieldDefs,
            ImArray.empty,
            ImArray.empty,
            ImArray.empty,
            ImArray.empty,
            ImArray.empty,
            None
        )

    ilAsm.SetEntityDefinition(ilEntDefHandle, ilEntDef)

let addFunction 
        (ilAsm: OlyILAssembly)
        (ilEnclosingEntDefHandle: OlyILEntityDefinitionHandle)
        (name: string)
        (tyPars: OlyILTypeParameter imarray) 
        (pars: OlyILParameter imarray) 
        (ilFlags: OlyILFunctionFlags)
        (ilMemberFlags: OlyILMemberFlags)
        (ilLocals: OlyILLocal imarray, ilExpr: OlyILExpression, ilExprTy: OlyILType) =

    let ilFuncSpec =
        OlyILFunctionSpecification(false, OlyILCallingConvention.Default, ilAsm.AddString(name), tyPars, pars, ilExprTy)

    let ilFuncSpecHandle =
        ilAsm.AddFunctionSpecification(ilFuncSpec)

    let ilFuncBody =
        OlyILFunctionBody(ilLocals, ilExpr)

    let ilFuncBodyHandle =
        ilAsm.AddFunctionBody(ilFuncBody)

    let ilFuncDef =
        OlyILFunctionDefinition(
            ilFlags,
            ilMemberFlags,
            ImArray.empty,
            ilFuncSpecHandle,
            None,
            ref(Some ilFuncBodyHandle)
        )

    ilAsm.AddFunctionDefinition(ilEnclosingEntDefHandle, ilFuncDef), ilFuncSpecHandle

let createAssembly isDebuggable =
    OlyILAssembly.Create("__stub", "0", isDebuggable)

[<Sealed;NoComparison;NoEquality>]
type DummyLocalManager(ilAsm: OlyILAssembly) =

    let ilLocals = ResizeArray<OlyILLocal>()

    member this.CreateLocal(ilTy: OlyILType, ilFlags) =
        let ilLocal = OlyILLocal(ilLocals.Count, ilAsm.AddString("local"), ilTy, ilFlags)
        ilLocals.Add(ilLocal)
        ilLocal.Index

    member this.GetLocals() = ilLocals |> ImArray.ofSeq

let createEntityDefinitionHandle (ilAsm: OlyILAssembly) =
    ilAsm.NextEntityDefinition()

let createType ilAsm ilEntDefHandle ilKind name ilFuncDefHandles ilFieldDefHandles =
    addEntity ilAsm ilEntDefHandle ilKind name ImArray.empty ilFuncDefHandles ilFieldDefHandles
    OlyILTypeEntity(OlyILEntityConstructor(ilEntDefHandle))

let createFunctionDefinition ilAsm ilEnclosingEntDefHandle name ilTyPars ilPars ilFlags ilMemberFlags ilLocals ilExpr ilExprTy =
    addFunction ilAsm ilEnclosingEntDefHandle name ilTyPars ilPars ilFlags ilMemberFlags (ilLocals, ilExpr, ilExprTy)

[<Sealed;NoComparison;NoEquality>]
type DummyAssemblyBuilder(isDebuggable: bool) =

    let ilAsm = createAssembly isDebuggable

    let ilMainEntHandle =
        createEntityDefinitionHandle ilAsm

    let ilMainFuncDefHandle =
        createFunctionDefinition
            ilAsm
            ilMainEntHandle
            "main"
            ImArray.empty
            ImArray.empty
            OlyILFunctionFlags.None 
            OlyILMemberFlags.Static
            ImArray.empty
            (OlyILExpression.None(OlyILDebugSourceTextRange.Empty))
            OlyILTypeVoid
        |> fst

    let ilMainTy = 
        createType
            ilAsm
            ilMainEntHandle
            OlyILEntityKind.Module
            "MainModule" 
            (ImArray.createOne ilMainFuncDefHandle)
            ImArray.empty

    do
        ilAsm.EntryPoint <- Some(ilMainTy, ilMainFuncDefHandle)

    member _.MainFunctionDefinitionHandle = ilMainFuncDefHandle

    member this.CreateLocalManager() =
        DummyLocalManager(ilAsm)

    member this.CreateEntityDefinitionHandle() =
        createEntityDefinitionHandle ilAsm

    member this.CreateType(
                ilEntDefHandle: OlyILEntityDefinitionHandle, 
                ilKind: OlyILEntityKind, 
                name: string, 
                ilFuncDefHandles, 
                ilFieldDefHandles
            ): OlyILType =
        createType ilAsm ilEntDefHandle ilKind name ilFuncDefHandles ilFieldDefHandles

    member this.CreateFunctionDefinition(
                ilEnclosingEntDefHandle: OlyILEntityDefinitionHandle,
                name: string,
                ilTyPars: OlyILTypeParameter imarray,
                ilPars: OlyILParameter imarray,
                ilFlags: OlyILFunctionFlags,
                ilMemberFlags: OlyILMemberFlags,
                ilLocals: OlyILLocal imarray,
                ilExpr: OlyILExpression,
                ilExprTy: OlyILType
            ): OlyILFunctionDefinitionHandle * OlyILFunctionSpecificationHandle =
        createFunctionDefinition ilAsm ilEnclosingEntDefHandle name ilTyPars ilPars ilFlags ilMemberFlags ilLocals ilExpr ilExprTy

    member this.CreateFieldDefinition(
                name: string,
                ilTy: OlyILType,
                ilFlags: OlyILFieldFlags,
                ilMemberFlags: OlyILMemberFlags
            ): OlyILFieldDefinitionHandle =
        let ilFieldDef =
            OlyILFieldDefinition(ImArray.empty, ilAsm.AddString(name), ilTy, ilFlags, ilMemberFlags)

        ilAsm.AddFieldDefinition(ilFieldDef)

    member this.CreateFieldReference(ilEnclosingTy: OlyILType, ilFieldDefHandle: OlyILFieldDefinitionHandle) =
        match ilEnclosingTy with
        | OlyILTypeEntity(ilEntInst) ->
            let ilFieldDef = ilAsm.GetFieldDefinition(ilFieldDefHandle)
            OlyILFieldReference(OlyILEnclosing.Entity(ilEntInst), ilFieldDef.NameHandle, ilFieldDef.Type)
        | _ ->
            failwith "Expected entity type."

    member this.SetMainFunctionBody(ilEnclosingEntDefHandle, ilLocals: OlyILLocal imarray, ilExpr: OlyILExpression) =
        let ilFuncBody =
            OlyILFunctionBody(ilLocals, ilExpr)
        let ilFuncBodyHandle = ilAsm.AddFunctionBody(ilFuncBody)

        match ilAsm.GetFunctionDefinition(ilMainFuncDefHandle) with
        | OlyILFunctionDefinition(
            ilFlags, 
            ilMemberFlags, 
            ilAttrs,
            ilFuncSpecHandle,
            ilOverrides,
            _) ->
            ilAsm.SetFunctionDefinition(
                ilEnclosingEntDefHandle,
                ilMainFuncDefHandle, 
                OlyILFunctionDefinition(
                    ilFlags, 
                    ilMemberFlags, 
                    ilAttrs, 
                    ilFuncSpecHandle, 
                    ilOverrides, 
                    ref(Some(ilFuncBodyHandle))
                )
            )

    member this.TryGetIRFunctionBodyByJIT(ilFuncDefHandle: OlyILFunctionDefinitionHandle): OlyIRFunctionBody<DummyType, DummyFunction, DummyField> option =
        let ilFuncDef = ilAsm.GetFunctionDefinition(ilFuncDefHandle)
        let ilFuncSpec = ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)
        let funcName = ilAsm.GetStringOrEmpty(ilFuncSpec.NameHandle)

        let mutable result = None
        let onEmitBody = 
            fun (emittedFunc: DummyFunction) (irLazyFuncBody: Lazy<OlyIRFunctionBody<DummyType, DummyFunction, DummyField>>) ->
                let irFuncBody = irLazyFuncBody.Value
                if funcName = emittedFunc.Name then
                    match result with
                    | Some _ -> ()
                    | _ -> result <- Some irFuncBody

        let dummyRuntime =
            OlyRuntime(DummyEmitter(onEmitBody))

        dummyRuntime.ImportAssembly(ilAsm.ToReadOnly())
        dummyRuntime.EmitEntryPoint()

        result