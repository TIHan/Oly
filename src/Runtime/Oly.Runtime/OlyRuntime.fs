[<AutoOpen>]
module rec Oly.Runtime.Implementation

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns
open Oly.Runtime.CodeGen.Internal.Optimizer
open Oly.Metadata
open Oly.Core
open Oly.Core.TaskExtensions

let getAllILTypeParameters (ilAsm: OlyILReadOnlyAssembly) (ilEntDef: OlyILEntityDefinition) : OlyILTypeParameter imarray =
    let enclosingTyPars =
        match ilEntDef.Enclosing with
        | OlyILEnclosing.Entity(ilEntInst) ->
            getAllILTypeParameters ilAsm (ilAsm.GetEntityDefinition(ilEntInst.DefinitionOrReferenceHandle))
        | _ ->
            ImArray.empty
    enclosingTyPars.AddRange(ilEntDef.TypeParameters)

let setWitnessesToFunction (witnesses: RuntimeWitness imarray) genericContext (this: RuntimeFunction) =
    this.SetWitnesses(witnesses)

let createGenericContextFromFunction canErase (func: RuntimeFunction) =
    let isTyErased = func.EnclosingType.CanGenericsBeErased && not func.IsExternal
    let isFuncErased = canErase && func.CanGenericsBeErased

    let genericContext =
        if isTyErased then
            GenericContext.CreateErasing(func.Enclosing.TypeArguments)
        else
            GenericContext.Create(func.Enclosing.TypeArguments)
    if isFuncErased then
        genericContext.AddErasingFunctionTypeArguments(func.TypeArguments)
    else
        genericContext.AddFunctionTypeArguments(func.TypeArguments)

let private getEnclosingOfILEntityInstance (ilAsm: OlyILReadOnlyAssembly) (ilEntInst: OlyILEntityInstance) =
    match ilEntInst with
    | OlyILEntityInstance(defOrRefHandle=defOrRefHandle)
    | OlyILEntityConstructor(defOrRefHandle=defOrRefHandle) -> 
        if defOrRefHandle.Kind = OlyILTableKind.EntityDefinition then
            ilAsm.GetEntityDefinition(defOrRefHandle).Enclosing
        else
            ilAsm.GetEntityReference(defOrRefHandle).Enclosing

let areSimpleILExpressions depth (ilExprs: OlyILExpression imarray) =
    ilExprs
    |> ImArray.forall (isSimpleILExpression depth)
    
let isSimpleILExpression depth (ilExpr: OlyILExpression) =
    match ilExpr with
    | OlyILExpression.None _
    | OlyILExpression.Value _ -> true
    | OlyILExpression.Sequential(ilExpr1, ilExpr2) when depth <= 1 ->
        isSimpleILExpression (depth) ilExpr1 && isSimpleILExpression (depth) ilExpr2
    | OlyILExpression.IfElse(ilCondExpr, ilTrueTargetExpr, ilFalseTargetExpr) when depth <= 1 ->
        isSimpleILExpression (depth + 1) ilCondExpr &&
        isSimpleILExpression (depth + 1) ilTrueTargetExpr &&
        isSimpleILExpression (depth + 1) ilFalseTargetExpr
    | OlyILExpression.Operation(_, ilOp) when depth <= 1 ->
        match ilOp with
        | OlyILOperation.Add(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.Subtract(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.Multiply(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.Divide(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.Remainder(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.Equal(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.NotEqual(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.LessThan(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.LessThanOrEqual(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.GreaterThan(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.GreaterThanOrEqual(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.BitwiseAnd(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.BitwiseOr(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.BitwiseExclusiveOr(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.BitwiseShiftLeft(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.BitwiseShiftRight(ilArgExpr1, ilArgExpr2)
        | OlyILOperation.StoreField(_, ilArgExpr1, ilArgExpr2)
        | OlyILOperation.StoreRefCellContents(ilArgExpr1, ilArgExpr2) 
        | OlyILOperation.StoreToAddress(ilArgExpr1, ilArgExpr2) ->
            isSimpleILExpression (depth + 1) ilArgExpr1 && isSimpleILExpression (depth + 1) ilArgExpr2
        | OlyILOperation.LoadField(_, ilArgExpr)
        | OlyILOperation.LoadFieldAddress(_, ilArgExpr, _)
        | OlyILOperation.StoreStaticField(_, ilArgExpr) 
        | OlyILOperation.LoadFunction(_, ilArgExpr)
        | OlyILOperation.Store(_, ilArgExpr) 
        | OlyILOperation.StoreArgument(_, ilArgExpr) 
        | OlyILOperation.LoadRefCellContents(_, ilArgExpr)
        | OlyILOperation.LoadRefCellContentsAddress(_, ilArgExpr, _)
        | OlyILOperation.Negate(ilArgExpr) 
        | OlyILOperation.NewRefCell(_, ilArgExpr) 
        | OlyILOperation.Not(ilArgExpr)
        | OlyILOperation.BitwiseNot(ilArgExpr)
        | OlyILOperation.LoadFromAddress(ilArgExpr)
        | OlyILOperation.Witness(ilArgExpr, _, _) 
        | OlyILOperation.LoadTupleElement(ilArgExpr, _)
        | OlyILOperation.LoadArrayLength(ilArgExpr) 
        | OlyILOperation.NewMutableArray(_, ilArgExpr)
        | OlyILOperation.Print(ilArgExpr)
        | OlyILOperation.Throw(ilArgExpr, _)
        | OlyILOperation.Cast(ilArgExpr, _) ->
            isSimpleILExpression (depth + 1) ilArgExpr
        | OlyILOperation.Ignore(ilArgExpr) ->
            // We purposely do not increase the depth here.
            isSimpleILExpression depth ilArgExpr
        | OlyILOperation.Call(_, ilArgExprs)
        | OlyILOperation.CallVirtual(_, ilArgExprs) 
        | OlyILOperation.New(_, ilArgExprs) 
        | OlyILOperation.NewArray(_, _, ilArgExprs) 
        | OlyILOperation.NewTuple(_, ilArgExprs, _) ->
            areSimpleILExpressions (depth + 1) ilArgExprs
        | OlyILOperation.CallIndirect(ilArgExpr, ilArgExprs) 
        | OlyILOperation.LoadArrayElement(ilArgExpr, ilArgExprs) 
        | OlyILOperation.LoadArrayElementAddress(ilArgExpr, ilArgExprs, _) ->
            let depthPlusOne = depth + 1
            isSimpleILExpression depthPlusOne ilArgExpr &&
            areSimpleILExpressions depthPlusOne ilArgExprs
        | OlyILOperation.StoreArrayElement(ilArgExpr, ilArgExprs, ilRhsExpr) ->
            let depthPlusOne = depth + 1
            isSimpleILExpression depthPlusOne ilArgExpr &&
            isSimpleILExpression depthPlusOne ilRhsExpr &&
            areSimpleILExpressions depthPlusOne ilArgExprs

    // Special heuristic!
    // We really want to inline these functions as the optimizer can eliminate the tuple.
    // Handles pattern:
    //     pattern Window(node: UINode): (state: WindowModel, children: UINode[]) when (node.Tag == 0) =>
    //         let node = Unsafe.Cast<Window>(node)
    //         (node.State, node.Children)
    | OlyILExpression.Let(_, OlyILExpression.Operation(_, OlyILOperation.Cast(ilCastArgExpr, _)), OlyILExpression.Operation(_, OlyILOperation.NewTuple(_, ilNewTupleArgExprs, _))) ->
        // Do not increment 'depth' as we want to try to inline the function.
        isSimpleILExpression depth ilCastArgExpr &&
        areSimpleILExpressions depth ilNewTupleArgExprs

    | _ ->
        false

/// Does introspection, this is fine considering at the moment we already have the body
/// in-memory. When that changes and the information is say, only on-disk, what would we do?
/// Perhaps we only do introspection up to a point?
let checkFunctionInlineability (ilAsm: OlyILReadOnlyAssembly) (ilFuncDef: OlyILFunctionDefinition) =
    if ilFuncDef.IsConstructor || ilFuncDef.IsAbstract || (ilFuncDef.IsVirtual && not ilFuncDef.IsSealed) || ilFuncDef.IsExported || ilFuncDef.IsImported then
        false
    elif ilFuncDef.Flags &&& OlyILFunctionFlags.InlineMask = OlyILFunctionFlags.InlineNever then
        false
    elif ilFuncDef.Flags &&& OlyILFunctionFlags.InlineMask = OlyILFunctionFlags.InlineAlways then
        true
    elif ilAsm.IsDebuggable then
        false
    elif ilFuncDef.Flags &&& OlyILFunctionFlags.InlineMask = OlyILFunctionFlags.Inline then
        true
    else
        let ilFuncBody = ilAsm.GetFunctionBody(ilFuncDef.BodyHandle.contents.Value)
        isSimpleILExpression 0 ilFuncBody.BodyExpression

[<Sealed>]
type RuntimeTypeInstanceCache<'Type, 'Function, 'Field>(runtime: OlyRuntime<'Type, 'Function, 'Field>, ilAsm: OlyILReadOnlyAssembly) =

    let cache = Dictionary<OlyILEntityDefinitionHandle, RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, RuntimeType>>()

    member this.GetOrCreate(handle: OlyILEntityDefinitionHandle, fullTyArgs: RuntimeType imarray) =   
        let instances =
            match cache.TryGetValue(handle) with
            | true, instances -> instances
            | _ ->
                let instances = RuntimeTypeArgumentListTable()
                cache.[handle] <- instances
                instances

        match instances.TryGetValue(fullTyArgs) with
        | ValueSome res -> res
        | _ ->
            let formalTy = runtime.ResolveTypeDefinition(ilAsm, handle)
            let res = formalTy.Apply(fullTyArgs)
            instances.[fullTyArgs] <- res
            res            

[<NoEquality;NoComparison>]
type internal RuntimeAssembly<'Type, 'Function, 'Field> =
    private {
        ilAsm: OlyILReadOnlyAssembly
        EntityDefinitionCache: Dictionary<OlyILEntityDefinitionHandle, RuntimeType * RuntimeEntityDefinitionTypeArgumentWitnessListTable<'Type, 'Function, 'Field, 'Type>>
        EntityInstanceCache: Dictionary<OlyILEntityDefinitionHandle, RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Type>>
        entRefCache: Dictionary<OlyILEntityReferenceHandle, RuntimeType>

        FunctionDefinitionCache: Dictionary<OlyILFunctionDefinitionHandle, RuntimeFunction * RuntimeTypeArgumentWitnessListTable<'Type, 'Function, 'Field, 'Function>>
        FieldDefinitionCache: Dictionary<OlyILFieldDefinitionHandle, RuntimeField * RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Field>>

        FieldReferenceCache: Dictionary<OlyILFieldDefinitionHandle, RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Field>>

        FieldVariadicDefinitionCache: Dictionary<OlyILFieldDefinitionHandle, Dictionary<string, RuntimeField * RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Field>>>

        RuntimeTypeInstanceCache: RuntimeTypeInstanceCache<'Type, 'Function, 'Field>
        RuntimeFieldReferenceCache: RuntimeFieldReferenceCache<'Type, 'Function, 'Field>

        TypesThatInheritOrImplementType: Dictionary<OlyILEntityDefinitionHandle, ResizeArray<RuntimeType>>
    }

    member this.GetEmittedFunctionDefinition(func: RuntimeFunction) =
        match this.FunctionDefinitionCache.TryGetValue func.ILFunctionDefinitionHandle with
        | true, (_, emitted) -> emitted
        | _ -> failwithf "Function definition not cached: %A" func.Name

let createFunctionDefinition<'Type, 'Function, 'Field> (runtime: OlyRuntime<'Type, 'Function, 'Field>) (enclosingTy: RuntimeType) (ilFuncDefHandle: OlyILFunctionDefinitionHandle) =
    let asm = runtime.Assemblies[enclosingTy.AssemblyIdentity]
    let ilAsm = asm.ilAsm
    let ilFuncDef = ilAsm.GetFunctionDefinition(ilFuncDefHandle)
    let ilFuncSpec = ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)

    let name = ilAsm.GetStringOrEmpty ilFuncSpec.NameHandle

    if enclosingTy.IsBuiltIn then
        failwith "Expected non-built-in type."

    let genericContext = GenericContext.Default

    let enclosing = RuntimeEnclosing.Type(enclosingTy)

    let returnTy = runtime.ResolveType(ilAsm, ilFuncSpec.ReturnType, genericContext)

    let isExternal =
        ilFuncDef.Attributes
        |> ImArray.exists (function OlyILAttribute.Import _ -> true | _ -> false)
        
    let irFlags =
        if isExternal then
            RuntimeFunctionFlags.External
        else
            RuntimeFunctionFlags.None

    let irFlags =
        match ilAsm.EntryPoint with
        | Some(_, ilFuncDefHandle2) ->
            if ilFuncDefHandle = ilFuncDefHandle2 then
                irFlags ||| RuntimeFunctionFlags.EntryPoint
            else
                irFlags
        | _ ->
            irFlags

    let irFlags =
        if enclosing.IsExported then
            irFlags ||| RuntimeFunctionFlags.Exported
        else
            irFlags

    let flags = OlyIRFunctionFlags(ilFuncDef.Flags, ilFuncDef.MemberFlags, irFlags)

    let tyPars =
        ilFuncSpec.TypeParameters
        |> ImArray.mapi (fun i ilTyPar ->
            let constrSubtypes = 
                ilTyPar.Constraints
                |> ImArray.choose (fun ilConstr ->
                    match ilConstr with
                    | OlyILConstraint.SubtypeOf(ilTy) ->
                        runtime.ResolveType(ilAsm, ilTy, genericContext)
                        |> Some
                    | _ ->
                        None
                )

            let constrTraits = 
                ilTyPar.Constraints
                |> ImArray.choose (fun ilConstr ->
                    match ilConstr with
                    | OlyILConstraint.TraitType(ilTy) ->
                        runtime.ResolveType(ilAsm, ilTy, genericContext)
                        |> Some
                    | _ ->
                        None
                )
            { 
                Name = ilAsm.GetStringOrEmpty(ilTyPar.NameHandle)
                Arity = ilTyPar.Arity
                IsVariadic = ilTyPar.IsVariadic
                ILConstraints = ilTyPar.Constraints
                ConstraintSubtypes = Lazy<_>.CreateFromValue(constrSubtypes)
                ConstraintTraits = Lazy<_>.CreateFromValue(constrTraits)
            } : RuntimeTypeParameter
        )

    let tyArgs =
        tyPars
        |> ImArray.mapi (fun i _ ->
            RuntimeType.Variable(i, OlyILTypeVariableKind.Function)
        )

    let pars =
        ilFuncSpec.Parameters
        |> ImArray.map (fun ilPar ->
            { 
                Attributes = ilPar.Attributes |> ImArray.choose (fun x -> runtime.TryResolveConstructorAttribute(ilAsm, x, GenericContext.Default, ImArray.empty))
                Name = ilAsm.GetStringOrEmpty(ilPar.NameHandle)
                Type = runtime.ResolveType(ilAsm, ilPar.Type, genericContext)
            } : RuntimeParameter
        )

    let overrides =
        ilFuncDef.Overrides
        |> Option.map (fun x ->
            runtime.ResolveFunction(ilAsm, x, genericContext)
        )

    let attrs =
        ilFuncDef.Attributes
        |> ImArray.choose (fun x -> runtime.TryResolveConstructorAttribute(ilAsm, x, GenericContext.Default, ImArray.empty))

    let flags =
        if not enclosing.AsType.IsExported && not enclosing.AsType.IsExternal && checkFunctionInlineability ilAsm ilFuncDef then
            flags.SetInlineable()
        else
            flags

    let flags =
        let rec checkForNewtype (x: RuntimeType) =
            let x = x.StripAlias()
            if x.IsNewtype then true
            else
                if x.TypeArguments.IsEmpty then
                    false
                else
                    x.TypeArguments |> ImArray.exists checkForNewtype
                
        if checkForNewtype returnTy || (pars |> ImArray.exists (fun x -> checkForNewtype x.Type)) then
            flags.SetSignatureUsesNewType()
        else
            flags

    let funcState =
        {
            RuntimeFunctionState.Formal = Unchecked.defaultof<_>
            RuntimeFunctionState.Enclosing = enclosing
            RuntimeFunctionState.Name = name
                           
            RuntimeFunctionState.TypeArguments = tyArgs
            RuntimeFunctionState.TypeParameters = tyPars
                           
            RuntimeFunctionState.Parameters = pars
            RuntimeFunctionState.ReturnType = returnTy
            RuntimeFunctionState.Flags = flags
            RuntimeFunctionState.Attributes = attrs
                           
            RuntimeFunctionState.Overrides = overrides
            RuntimeFunctionState.Witnesses = ImArray.empty
            RuntimeFunctionState.Kind = RuntimeFunctionKind.Formal
                           
            RuntimeFunctionState.ILAssembly = ilAsm
            RuntimeFunctionState.ILFunctionDefinitionHandle = ilFuncDefHandle
        }
    let func = RuntimeFunction(funcState)
    funcState.Formal <- func
    func

/// *****************************************************************************************************
/// IMPORTER
/// *****************************************************************************************************

[<Sealed>]
type cenv<'Type, 'Function, 'Field>(localCount, argCount, vm: OlyRuntime<'Type, 'Function, 'Field>) =

    member inline _.GetILAssembly(asmIdent): OlyILReadOnlyAssembly =
        vm.Assemblies[asmIdent].ilAsm

    member inline _.ResolveType(ilAsm, ilTy, genericContext): RuntimeType =
        vm.ResolveType(ilAsm, ilTy, genericContext)

    member inline _.ResolveTypes(ilAsm: OlyILReadOnlyAssembly, ilTys: OlyILType imarray, genericContext: GenericContext) =
        vm.ResolveTypes(ilAsm, ilTys, genericContext)

    member inline _.ResolveFunctionDefinition(enclosingTy, ilFuncDefHandle): RuntimeFunction =
        vm.ResolveFunctionDefinition(enclosingTy, ilFuncDefHandle)

    member inline _.ResolveFunction(ilAsm, ilFuncRef, genericContext): RuntimeFunction = 
        vm.ResolveFunction(ilAsm, ilFuncRef, genericContext)

    member inline _.ResolveFunction(ilAsm: OlyILReadOnlyAssembly, ilFuncInst: OlyILFunctionInstance, genericContext: GenericContext, passedWitnesses: RuntimeWitness imarray) = 
        vm.ResolveFunction(ilAsm, ilFuncInst, genericContext, passedWitnesses)

    member inline _.ResolveField(ilAsm: OlyILReadOnlyAssembly, ilFieldRef: OlyILFieldReference, genericContext: GenericContext) =
        vm.ResolveField(ilAsm, ilFieldRef, genericContext)

    member inline _.ResolveField(enclosingTy: RuntimeType, ilAsm: OlyILReadOnlyAssembly, index, ilFieldDefHandle: OlyILFieldDefinitionHandle)=
        vm.ResolveField(enclosingTy, ilAsm, index, ilFieldDefHandle)

    member inline _.EmitILConstant(ilAsm: OlyILReadOnlyAssembly, ilConstant: OlyILConstant, genericContext: GenericContext): C<'Type, 'Function> * RuntimeType =
        vm.EmitILConstant(ilAsm, ilConstant, genericContext)

    member inline _.TryGetCallStaticConstructorExpression(field: RuntimeField): E<'Type, 'Function, 'Field> option =
        vm.TryGetCallStaticConstructorExpression(field)

    member inline _.TryGetCallStaticConstructorExpression(func: RuntimeFunction): E<'Type, 'Function, 'Field> option =
        vm.TryGetCallStaticConstructorExpression(func)

    member inline _.EmittedTypeVoid: 'Type = 
        vm.TypeVoid.Value

    member inline _.EmittedTypeUnit: 'Type =
        vm.TypeUnit.Value

    member inline _.EmittedTypeBool: 'Type =
        vm.TypeBool.Value

    member inline _.EmitType(ty): 'Type =
        vm.EmitType(ty)

    member inline _.EmitTypeArgument(ty): 'Type =
        vm.EmitTypeArgument(ty)

    member inline _.EmitField(field): 'Field =
        vm.EmitField(field)

    member inline _.EmitFunction(func): 'Function =
        vm.EmitFunction(func)

    member inline _.EmitFunctionNoErasure(func): 'Function =
        vm.EmitFunction(false, func)

    member inline _.EmitFunctionFromEnvironment(envFunc, func): 'Function =
        vm.EmitFunctionFromEnvironment(envFunc, func)

    member inline _.TryFindPossibleWitness(ty, abstractTy, witnesses): RuntimeWitness option =
        vm.TryFindPossibleWitness(ty, abstractTy, witnesses)

    member val LocalAddressExposed = Array.init localCount (fun _ -> false) with get
    member val LocalMutability = Array.init localCount (fun _ -> false) with get
    member val ArgumentAddressExposed = Array.init argCount (fun _ -> false) with get
    member val ArgumentMutability = Array.init argCount (fun _ -> false) with get

[<NoEquality;NoComparison>]
type env<'Type, 'Function, 'Field> =
    {
        GenericContext: GenericContext
        ILAssembly: OlyILReadOnlyAssembly
        EnclosingTypeParameterCount: int
        LocalTypes: RuntimeType imarray
        ArgumentTypes: RuntimeType imarray
        ILLocals: OlyILLocal imarray
        Function: RuntimeFunction
    }

    member this.PassedWitnesses = this.GenericContext.PassedWitnesses

    member env.HandleReceiver(cenv: cenv<'Type, 'Function, 'Field>, expectedArgTy: RuntimeType, irArg: E<'Type, 'Function, 'Field>, argTy: RuntimeType, isVirtual) : E<'Type, 'Function, 'Field> * RuntimeType =
        let expectedArgTy = expectedArgTy.StripAlias()
        let argTy = argTy.StripAlias()

#if DEBUG || CHECKED
        OlyAssert.False(expectedArgTy.IsTypeExtension)
        OlyAssert.False(expectedArgTy.IsModule)
        OlyAssert.False(argTy.IsTypeExtension)
        OlyAssert.False(argTy.IsModule)
#endif

        if not expectedArgTy.IsByRef_t && argTy.IsByRef_t && not argTy.IsByRefOfVariable then
            let argDerefTy = argTy.TypeArguments[0]
            let irArgDerefExpr = E.Operation(NoRange, O.LoadFromAddress(irArg, cenv.EmitType(argDerefTy)))
            irArgDerefExpr, argDerefTy
        else      
            irArg, argTy

let assertEnvironmentWitnesses env (func: RuntimeFunction) =
#if DEBUG || CHECKED
    if env.Function.EnclosingType.TypeParameters.IsEmpty then
        let checkWitnesses (witnesses: RuntimeWitness imarray) =
            witnesses
            |> ImArray.iter (fun w ->
                if w.TypeVariableKind = OlyILTypeVariableKind.Type then
                    failwith "Unexpected type variable from type"
            )
        let rec checkType (ty: RuntimeType) =
            checkWitnesses ty.Witnesses
            ty.TypeArguments
            |> ImArray.iter (checkType)
        checkWitnesses func.Witnesses
        func.Parameters
        |> ImArray.iter (fun par ->
            checkType par.Type
        )
#else
    ()
#endif

let createByReferenceRuntimeType irByRefKind elementTy =
    RuntimeType.ByRef(elementTy, irByRefKind)

let readTextRange (ilAsm: OlyILReadOnlyAssembly) (ilTextRange: OlyILDebugSourceTextRange) =
    let path =
        if ilTextRange.DebugSourceHandle.IsNil then
            OlyPath.Empty
        else
            let ilDbgSrc = ilAsm.GetDebugSource(ilTextRange.DebugSourceHandle)
            ilDbgSrc.Path
    OlyIRDebugSourceTextRange(path, ilTextRange.StartLine, ilTextRange.StartColumn, ilTextRange.EndLine, ilTextRange.EndColumn)

let canPossiblyEraseGenericFunction (envFunc: RuntimeFunction) (func: RuntimeFunction) =
    // TODO: This needs a re-work at some point.
    // If the current function body possibly overrides an external function, then we cannot erase the type parameters/arguments of the called function.
    func.CanGenericsBeErased &&
    (
        (
            envFunc.IsOverridesExternal && 
            envFunc.Witnesses.IsEmpty && 
            ((not envFunc.TypeParameters.IsEmpty) 
                || (not envFunc.EnclosingType.CanGenericsBeErased)) &&
            func.Formal.EnclosingType <> envFunc.Formal.EnclosingType
        )
        |> not
    )

let createDefaultExpression irTextRange (resultTy: RuntimeType, emittedTy: 'Type) =
    let asExpr irValue =
        E.Value(irTextRange, irValue)

    if resultTy.IsAnyStruct || resultTy.IsTypeVariable then
        match resultTy.StripAliasAndNewtypeAndEnum() with
        | RuntimeType.Int8 ->
            V.Constant(C.Int8(0y), emittedTy) |> asExpr
        | RuntimeType.Int16 ->
            V.Constant(C.Int16(0s), emittedTy) |> asExpr
        | RuntimeType.Int32 ->
            V.Constant(C.Int32(0), emittedTy) |> asExpr
        | RuntimeType.Int64 ->
            V.Constant(C.Int64(0L), emittedTy) |> asExpr
        | RuntimeType.UInt8 ->
            V.Constant(C.UInt8(0uy), emittedTy) |> asExpr
        | RuntimeType.UInt16 ->
            V.Constant(C.UInt16(0us), emittedTy) |> asExpr
        | RuntimeType.UInt32 ->
            V.Constant(C.UInt32(0u), emittedTy) |> asExpr
        | RuntimeType.UInt64 ->
            V.Constant(C.UInt64(0UL), emittedTy) |> asExpr
        | RuntimeType.Float32 ->
            V.Constant(C.Float32(0.0f), emittedTy) |> asExpr
        | RuntimeType.Float64 ->
            V.Constant(C.Float64(0.0), emittedTy) |> asExpr
        | RuntimeType.Char16 ->
            V.Constant(C.Char16(char 0), emittedTy) |> asExpr
        | _ ->
            V.Default(emittedTy) |> asExpr
    else
        V.Null(emittedTy) |> asExpr

let importCatchCase (cenv: cenv<'Type, 'Function, 'Field>) (env: env<'Type, 'Function, 'Field>) (expectedTy: RuntimeType) (ilCatchCase: OlyILCatchCase) =
    match ilCatchCase with
    | OlyILCatchCase.CatchCase(localIndex, ilBodyExpr) ->
        let ilLocal = env.ILLocals[localIndex]
        let localName = env.ILAssembly.GetStringOrEmpty(ilLocal.NameHandle)
        let irBodyExpr = importArgumentExpression cenv env expectedTy ilBodyExpr
        let catchTy = env.LocalTypes[localIndex]
        OlyIRCatchCase.CatchCase(localName, localIndex, irBodyExpr, cenv.EmitType(catchTy))

/// Uses CPS-style passing to help prevent stack overflows.
let importSequentialExpression (cenv: cenv<'Type, 'Function, 'Field>) (env: env<'Type, 'Function, 'Field>) (expectedTyOpt: RuntimeType option) (ilExpr: OlyILExpression) cont =
    match ilExpr with
    | OlyILExpression.Sequential(ilExpr1, ilExpr2) ->
        match ilExpr1 with
        | OlyILExpression.Sequential _ ->
            importSequentialExpression cenv env (Some RuntimeType.Void) ilExpr1 (fun (irExpr1, _) ->
                match ilExpr2 with
                | OlyILExpression.Let _
                | OlyILExpression.Sequential _ ->
                    importSequentialExpression cenv env expectedTyOpt ilExpr2 cont
                | _ ->
                    let irExpr2, resultTy = importExpression cenv env expectedTyOpt ilExpr2
                    cont(E.Sequential(irExpr1, irExpr2), resultTy)
            )
        | _ ->
            let irExpr1, _ = importExpression cenv env (Some RuntimeType.Void) ilExpr1
            match ilExpr2 with
            | OlyILExpression.Let _
            | OlyILExpression.Sequential _ ->
                importSequentialExpression cenv env expectedTyOpt ilExpr2 (fun (irExpr2, resultTy) ->
                    cont(E.Sequential(irExpr1, irExpr2), resultTy)
                )
            | _ ->
                let irExpr2, resultTy = importExpression cenv env expectedTyOpt ilExpr2
                cont(E.Sequential(irExpr1, irExpr2), resultTy)

    | OlyILExpression.Let(localIndex, ilRhsExpr, ilBodyExpr) ->
        let ilLocal = env.ILLocals.[localIndex]
        let localName = env.ILAssembly.GetStringOrEmpty(ilLocal.NameHandle)

        // TODO: We *could* expand the CPS to rhs-expr.
        let irRhsExpr = importArgumentExpression cenv env env.LocalTypes[localIndex] ilRhsExpr

        match ilBodyExpr with
        | OlyILExpression.Sequential _
        | OlyILExpression.Let _ ->
            importSequentialExpression cenv env expectedTyOpt ilBodyExpr (fun (bodyExpr, resultTy) ->
                cont(E.Let(localName, localIndex, irRhsExpr, bodyExpr), resultTy)
            )
        | _ ->
            let irBodyExpr, resultTy = importExpression cenv env expectedTyOpt ilBodyExpr
            cont(E.Let(localName, localIndex, irRhsExpr, irBodyExpr), resultTy)

    | _ ->
        OlyAssert.Fail("Invalid sequential expression.")

let importExpressionAux (cenv: cenv<'Type, 'Function, 'Field>) (env: env<'Type, 'Function, 'Field>) (expectedTyOpt: RuntimeType option) (ilExpr: OlyILExpression) : E<'Type, 'Function, 'Field> * RuntimeType =
    let resolveFunction (ilFuncInst: OlyILFunctionInstance) =
        cenv.ResolveFunction(env.ILAssembly, ilFuncInst, env.GenericContext, env.PassedWitnesses)

    match ilExpr with
    | OlyILExpression.None(ilTextRange) -> 
        E.None(readTextRange env.ILAssembly ilTextRange, cenv.EmittedTypeVoid), RuntimeType.Void

    | OlyILExpression.Try(ilBodyExpr, ilCatchCases, ilFinallyBodyExprOpt) ->
        let irBodyExpr, resultTy = importExpression cenv env expectedTyOpt ilBodyExpr
        let irCatchCases = ilCatchCases |> ImArray.map (importCatchCase cenv env resultTy)
        let irFinallyBodyExprOpt = ilFinallyBodyExprOpt |> Option.map (fst << importExpression cenv env (Some RuntimeType.Void))
        E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, cenv.EmitType(resultTy)), resultTy

    | OlyILExpression.While(ilConditionExpr, ilBodyExpr) ->
        // TODO: Fail if this is in a "non-imperative" context.
        let irConditionExpr = importArgumentExpression cenv env RuntimeType.Bool ilConditionExpr
        let irBodyExpr = importArgumentExpression cenv env RuntimeType.Void ilBodyExpr
        E.While(irConditionExpr, irBodyExpr, cenv.EmittedTypeVoid), RuntimeType.Void

    | OlyILExpression.Sequential _
    | OlyILExpression.Let _ ->
        importSequentialExpression cenv env expectedTyOpt ilExpr id

    | OlyILExpression.Value(ilTextRange, ilValue) ->
        let irTextRange = readTextRange env.ILAssembly ilTextRange

        let asExpr irValue =
            E.Value(irTextRange, irValue)

        match ilValue with
        | OlyILValue.Unit ->
            V.Unit(cenv.EmittedTypeUnit) |> asExpr, RuntimeType.Unit

        | OlyILValue.Constant(ilConstant) ->
            let irConstant, resultTy = cenv.EmitILConstant(env.ILAssembly, ilConstant, env.GenericContext)
            V.Constant(irConstant, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILValue.ConstantEnum(ilConstant, ilEnumTy) ->
            let irConstant, _ = cenv.EmitILConstant(env.ILAssembly, ilConstant, env.GenericContext)
            let resultTy = cenv.ResolveType(env.ILAssembly, ilEnumTy, env.GenericContext)

            if not resultTy.IsEnum then
                failwith "Expected enum type."

            V.Constant(irConstant, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILValue.Null(ilTy) ->
            let resultTy = cenv.ResolveType(env.ILAssembly, ilTy, env.GenericContext)
            V.Null(cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILValue.Default(ilTy) ->
            let resultTy = cenv.ResolveType(env.ILAssembly, ilTy, env.GenericContext)
            createDefaultExpression irTextRange (resultTy, cenv.EmitType(resultTy)), resultTy

        | OlyILValue.Argument(argIndex) ->
            let argTy = env.ArgumentTypes.[argIndex]
            V.Argument(argIndex, cenv.EmitType(argTy)) |> asExpr, argTy

        | OlyILValue.ArgumentAddress(argIndex, ilByRefKind) ->
            cenv.ArgumentAddressExposed[argIndex] <- true

            match ilByRefKind with
            | OlyILByRefKind.ReadWrite ->
                cenv.ArgumentMutability[argIndex] <- true
            | _ ->
                ()

            let argTy = env.ArgumentTypes.[argIndex]
            let irByRefKind = 
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.ReadOnly -> OlyIRByRefKind.ReadOnly
                | OlyILByRefKind.WriteOnly -> OlyIRByRefKind.WriteOnly
            let resultTy = createByReferenceRuntimeType irByRefKind argTy
            V.ArgumentAddress(argIndex, irByRefKind, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILValue.Local(localIndex) ->
            let localTy = cenv.ResolveType(env.ILAssembly, env.ILLocals.[localIndex].Type, env.GenericContext)
            V.Local(localIndex, cenv.EmitType(localTy)) |> asExpr, localTy

        | OlyILValue.LocalAddress(localIndex, ilByRefKind) ->
            cenv.LocalAddressExposed[localIndex] <- true
            
            match ilByRefKind with
            | OlyILByRefKind.ReadWrite ->
                cenv.LocalMutability[localIndex] <- true
            | _ ->
                ()

            let localTy = cenv.ResolveType(env.ILAssembly, env.ILLocals.[localIndex].Type, env.GenericContext).SetWitnesses(env.PassedWitnesses)

            let localTy =
                if localTy.IsNewtype then
                    localTy.RuntimeType.Value
                else
                    localTy

            let irByRefKind = 
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.ReadOnly -> OlyIRByRefKind.ReadOnly
                | OlyILByRefKind.WriteOnly -> OlyIRByRefKind.WriteOnly

            let _elementTy = cenv.EmitType(localTy)
            let resultTy = createByReferenceRuntimeType irByRefKind localTy
            V.LocalAddress(localIndex, irByRefKind, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILValue.StaticField(ilFieldRef) ->
            let field = cenv.ResolveField(env.ILAssembly, ilFieldRef, env.GenericContext)
            match field.ILConstant with
            | Some(ilConst) when not(env.Function.Flags.IsStatic && env.Function.Flags.IsConstructor)  ->
                let irConst, ty = cenv.EmitILConstant(env.ILAssembly, ilConst, env.GenericContext)
                E.Value(
                    irTextRange,
                    V.Constant(irConst, cenv.EmitType(ty))
                ), ty
            | _ ->
                let resultTy = field.Type

                let irField = OlyIRField(cenv.EmitField(field), field)
                let irValueExpr = V.StaticField(irField, cenv.EmitType(resultTy)) |> asExpr

                match cenv.TryGetCallStaticConstructorExpression(field) with
                | Some callStaticCtorExpr ->
                    E.Sequential(
                        callStaticCtorExpr,
                        irValueExpr
                    ), resultTy
                | _ ->
                    irValueExpr, resultTy

        | OlyILValue.StaticFieldAddress(ilFieldRef, ilByRefKind) ->
            let field = cenv.ResolveField(env.ILAssembly, ilFieldRef, env.GenericContext)
            let irByRefKind =
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.ReadOnly -> OlyIRByRefKind.ReadOnly
                | OlyILByRefKind.WriteOnly -> OlyIRByRefKind.WriteOnly
            let elementTy = cenv.EmitType(field.Type)
            let resultTy = createByReferenceRuntimeType irByRefKind field.Type

            let irField = OlyIRField(cenv.EmitField(field), field)
            let irValueExpr = V.StaticFieldAddress(irField, irByRefKind, cenv.EmitType(resultTy)) |> asExpr

            match cenv.TryGetCallStaticConstructorExpression(field) with
            | Some callStaticCtorExpr ->
                E.Sequential(
                    callStaticCtorExpr,
                    irValueExpr
                ), resultTy
            | _ ->
                irValueExpr, resultTy

        | OlyILValue.FunctionPtr(ilFuncInst) ->
            let func = resolveFunction ilFuncInst
            let resultTy = RuntimeType.NativeInt
            V.FunctionPtr(cenv.EmitFunction(func), cenv.EmitType(resultTy)) |> asExpr, RuntimeType.NativeInt

        | OlyILValue.Function(ilFuncInst) ->
            let func = resolveFunction ilFuncInst
            let argTys =
                func.Parameters
                |> ImArray.map (fun x -> x.Type)
            let returnTy = func.ReturnType

            let funcKind =
                if func.Flags.IsInstance && func.EnclosingType.IsScoped then
                    OlyIRFunctionKind.Scoped
                else
                    OlyIRFunctionKind.Normal

            let resultTy = RuntimeType.Function(argTys, returnTy, funcKind)

            V.Function(OlyIRFunction(cenv.EmitFunction(func), func), cenv.EmitType(resultTy)) |> asExpr, resultTy

    | OlyILExpression.IfElse(ilConditionExpr, ilTrueTargetExpr, ilFalseTargetExpr) ->
        let conditionExpr, _ = importExpression cenv env (Some RuntimeType.Bool) ilConditionExpr
        let trueTargetExpr, resultTy = importExpression cenv env expectedTyOpt ilTrueTargetExpr
        let falseTargetExpr, _ = importExpression cenv env expectedTyOpt ilFalseTargetExpr
        E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, trueTargetExpr.ResultType), resultTy

    | OlyILExpression.Operation(ilTextRange, OlyILOperation.Witness(ilBody, ilWitnessTy, ilReturnTy)) ->
        let irTextRange = readTextRange env.ILAssembly ilTextRange

        let witnessTy = cenv.ResolveType(env.ILAssembly, ilWitnessTy, env.GenericContext)
        let returnTy = cenv.ResolveType(env.ILAssembly, ilReturnTy, env.GenericContext)

        let irBody, _ = importExpression cenv env (Some witnessTy) ilBody

        if witnessTy.IsTypeExtension then
            E.Operation(irTextRange, O.Witness(irBody, cenv.EmitType(witnessTy), cenv.EmitType(returnTy))), returnTy
        else
            failwith "Invalid witness."

    | OlyILExpression.Operation(ilTextRange, ilOp) ->
        let irTextRange = readTextRange env.ILAssembly ilTextRange

        let inline asExpr irOp = E.Operation(irTextRange, irOp)

        let resolveFunctionArgs (func: RuntimeFunction) ilArgs isVirtual =
            let enclosingTy = func.EnclosingType.StripExtension()

            let parTys =
                func.Parameters
                |> ImArray.map (fun x -> x.Type)
            let parTys =
                if func.Flags.IsInstance then
                    let instanceTy =
                        if enclosingTy.IsAnyStruct then
                            let irByRefKind =
                                if func.IsMutable then
                                    OlyIRByRefKind.ReadWrite
                                else
                                    OlyIRByRefKind.ReadOnly
                            createByReferenceRuntimeType irByRefKind enclosingTy
                        else
                            enclosingTy
                    parTys
                    |> ImArray.prependOne instanceTy
                else
                    parTys
            let irArgs = 
                ilArgs 
                |> ImArray.mapi (fun i ilArg -> 
                    if func.Flags.IsInstance && i = 0 then
                        importReceiverExpression cenv env isVirtual parTys.[i] ilArg
                    else
                        importArgumentExpression cenv env parTys.[i] ilArg)

            irArgs
            
        let rec handleCall constrainedTy (func: RuntimeFunction) (irArgs: _ imarray) isVirtualCall =    
            assertEnvironmentWitnesses env func

            OlyAssert.False(func.EnclosingType.IsShape)

            // Verify use of 'base' calls.
            if not isVirtualCall && func.Flags.IsInstance && func.Flags.IsVirtual && not func.Flags.IsFinal && irArgs.Length > 0 && not(func.EnclosingType.IsAnyStruct) then
                match irArgs[0] with
                | E.Operation(op=O.Upcast(arg=E.Value(value=V.Argument(index=0)))) when func.EnclosingType <> env.ArgumentTypes[0] && subsumesType func.EnclosingType env.ArgumentTypes[0] ->
                    ()
                | _ ->
                    failwith $"Invalid base call. {func.EnclosingType.Name}.{func.Name}"

            let stripInlineFunctions = false
            if stripInlineFunctions then
                let dummyEmittedFunc = Unchecked.defaultof<_>
                let irFunc = OlyIRFunction(dummyEmittedFunc, func)
                let irExpr = O.Call(irFunc, irArgs, cenv.EmitType(func.ReturnType)) |> asExpr
                irExpr, func.ReturnType
            else
                let emittedFunc = cenv.EmitFunctionFromEnvironment(env.Function, func)
                let irFunc = OlyIRFunction(emittedFunc, func)

                let handle() =
//#if DEBUG || CHECKED
//                    Log(
//                        let witnesses = func.Witnesses
//                        let witnessText = 
//                            if witnesses.IsEmpty then
//                                ""
//                            else
//                                let text = witnesses |> ImArray.map (fun x -> x.TypeExtension.Name.ToString()) |> (String.concat "\n")
//                                $" - Witnesses: {text}"
//                        $"Devirtualized Function: {func.EnclosingType.Name}.{func.Name}{witnessText}"
//                    )
//#endif
#if DEBUG || CHECKED
                    if irFunc.RuntimeFunction.Flags.IsInstance then
                        OlyAssert.Equal(irFunc.RuntimeFunction.Parameters.Length + 1, irArgs.Length)
                    else
                        OlyAssert.Equal(irFunc.RuntimeFunction.Parameters.Length, irArgs.Length)
#endif
                    let irExpr = O.Call(irFunc, irArgs, cenv.EmitType(func.ReturnType)) |> asExpr
                    irExpr, func.ReturnType

                let callExpr, resultTy =
                    // Basic devirtualization.
                    if func.EnclosingType.IsAnyStruct || ((func.Flags.IsFinal || not func.Flags.IsVirtual) && isVirtualCall) then
                        handle()
                    elif isVirtualCall then
                        if func.Flags.IsStatic && func.Flags.IsAbstract then
                            // TODO: Review this to see if this is what we want to do. 
                            //       We might want to force the front-end compiler to emit it this way first.
                            O.CallConstrained(cenv.EmitType(constrainedTy), irFunc, irArgs, cenv.EmitType(func.ReturnType)) |> asExpr, func.ReturnType
                            //failwith $"Invalid 'CallVirtual({func.EnclosingType.Name}.{func.Name})'."
                        else
                            O.CallVirtual(irFunc, irArgs, cenv.EmitType(func.ReturnType)) |> asExpr, func.ReturnType
                    else
                        handle()

                callExpr, resultTy

        match ilOp with
        | OlyILOperation.LoadFunction(ilFuncInst, ilReceiverExpr) ->
            let func = cenv.ResolveFunction(env.ILAssembly, ilFuncInst, env.GenericContext, env.PassedWitnesses)

            assertEnvironmentWitnesses env func

            let argTys =
                func.Parameters
                |> ImArray.map (fun x -> x.Type)
            let returnTy = func.ReturnType

            let funcKind =
                if func.Flags.IsInstance && func.EnclosingType.IsScoped then
                    OlyIRFunctionKind.Scoped
                else
                    OlyIRFunctionKind.Normal

            let resultTy = RuntimeType.Function(argTys, returnTy, funcKind)

            let expectedArgTy =
                if func.EnclosingType.IsAnyStruct then                   
                    createByReferenceRuntimeType OlyIRByRefKind.ReadOnly func.EnclosingType
                else
                    func.EnclosingType

            let receiverExpr = importReceiverExpression cenv env false expectedArgTy ilReceiverExpr

            let emittedFunc = cenv.EmitFunction(func)

            let irFunc = OlyIRFunction(emittedFunc, func)
            E.Operation(
                irTextRange,
                O.LoadFunction(irFunc, receiverExpr, cenv.EmitType(resultTy))
            ),
            resultTy

        | OlyILOperation.LoadTupleElement(ilReceiver, index) ->
            let irReceiver, resultTy = importExpression cenv env None ilReceiver
            match resultTy with
            | RuntimeType.Tuple(itemTys, _) ->
                let resultTy = itemTys[index]
                O.LoadTupleElement(irReceiver, index, cenv.EmitType(resultTy)) |> asExpr, resultTy
            | _ ->
                failwith "assert"

        | OlyILOperation.LoadArrayElement(ilReceiver, ilIndexArgs) ->
            let irReceiver, resultTy = importExpression cenv env None ilReceiver
            let irIndexArgs = 
                ilIndexArgs
                |> ImArray.map (fun x -> 
                    let irExpr, resultTy = importExpression cenv env None x
                    if not resultTy.IsInteger then
                        invalidOp "Index must be an integer type."
                    irExpr
                )
            let resultTy =
                match resultTy with
                | RuntimeType.Array(elementTy, _, _) -> elementTy
                | _ -> failwith "Invalid type for LoadArrayElement."
            O.LoadArrayElement(irReceiver, irIndexArgs, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.LoadArrayElementAddress(ilReceiver, ilIndexArgs, ilByRefKind) ->
            let irByRefKind =
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.ReadOnly -> OlyIRByRefKind.ReadOnly
                | OlyILByRefKind.WriteOnly -> OlyIRByRefKind.WriteOnly
            let irReceiver, resultTy = importExpression cenv env None ilReceiver
            let irIndexArgs = 
                ilIndexArgs
                |> ImArray.map (fun x -> 
                    let irExpr, resultTy = importExpression cenv env None x
                    if not resultTy.IsInteger then
                        invalidOp "Index must be an integer type."
                    irExpr
                )
            let resultTy =
                match resultTy with
                | RuntimeType.Array(elementTy, _, _) -> 
                    createByReferenceRuntimeType irByRefKind elementTy
                | _ -> failwith "Invalid type for LoadArrayElementAddress."
            O.LoadArrayElementAddress(irReceiver, irIndexArgs, irByRefKind, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.StoreArrayElement(ilReceiver, ilIndexArgs, ilArg) ->
            // TODO: Add validation logic.
            let irReceiver, receiverTy = importExpression cenv env None ilReceiver
            let irIndexArgs = 
                ilIndexArgs
                |> ImArray.map (fun x -> 
                    let irExpr, resultTy = importExpression cenv env None x
                    if not resultTy.IsInteger then
                        invalidOp "Index must be an integer type."
                    irExpr
                )
            let irArg, _ = importExpression cenv env (Some receiverTy.StripAlias().TypeArguments[0]) ilArg
            let resultTy = RuntimeType.Void
            O.StoreArrayElement(irReceiver, irIndexArgs, irArg, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.LoadArrayLength(ilReceiver) ->
            // TODO: Add validation logic.
            let irReceiver, _ = importExpression cenv env None ilReceiver
            let resultTy = RuntimeType.Int32
            O.LoadArrayLength(irReceiver, 1, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.BitwiseNot(ilArg) ->
            // TODO: Add validation logic.
            let irArg, resultTy = importExpression cenv env None ilArg
            O.BitwiseNot(irArg, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.BitwiseOr(ilArg1, ilArg2) ->
            // TODO: Add validation logic.
            let irArg1, resultTy = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env None ilArg2
            O.BitwiseOr(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.BitwiseExclusiveOr(ilArg1, ilArg2) ->
            // TODO: Add validation logic.
            let irArg1, resultTy = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env None ilArg2
            O.BitwiseExclusiveOr(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.BitwiseAnd(ilArg1, ilArg2) ->
            // TODO: Add validation logic.
            let irArg1, resultTy = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env None ilArg2
            O.BitwiseAnd(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.BitwiseShiftLeft(ilArg1, ilArg2) ->
            // TODO: Add validation logic.
            let irArg1, resultTy = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env None ilArg2
            O.BitwiseShiftLeft(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.BitwiseShiftRight(ilArg1, ilArg2) ->
            // TODO: Add validation logic.
            let irArg1, resultTy = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env None ilArg2
            O.BitwiseShiftRight(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.Add(ilArg1, ilArg2) ->
            let irArg1, resultTy = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env None ilArg2
            O.Add(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy // TODO: Fix me, ResultType is ok assuming that both args are the same type.

        | OlyILOperation.Subtract(irArg1, irArg2) ->
            let irArg1, resultTy = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.Subtract(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy // TODO: Fix me, ResultType is ok assuming that both args are the same type.

        | OlyILOperation.Multiply(irArg1, irArg2) ->
            let irArg1, resultTy = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.Multiply(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy // TODO: Fix me, ResultType is ok assuming that both args are the same type.

        | OlyILOperation.Divide(irArg1, irArg2) ->
            let irArg1, resultTy = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.Divide(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy // TODO: Fix me, ResultType is ok assuming that both args are the same type.

        | OlyILOperation.Remainder(irArg1, irArg2) ->
            let irArg1, resultTy = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.Remainder(irArg1, irArg2, cenv.EmitType(resultTy)) |> asExpr, resultTy // TODO: Fix me, ResultType is ok assuming that both args are the same type.

        | OlyILOperation.Equal(irArg1, irArg2) ->
            let irArg1, resultTy1 = importExpression cenv env None irArg1
            let irArg2, resultTy2 = importExpression cenv env None irArg2

            if resultTy1.IsUtf16_t && resultTy2.IsUtf16_t then
                O.Utf16Equal(irArg1, irArg2, cenv.EmittedTypeBool) |> asExpr, RuntimeType.Bool
            else
                O.Equal(irArg1, irArg2, cenv.EmittedTypeBool) |> asExpr, RuntimeType.Bool

        | OlyILOperation.NotEqual(irArg1, irArg2) ->
            let irArg1, _ = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.NotEqual(irArg1, irArg2, cenv.EmittedTypeBool) |> asExpr, RuntimeType.Bool

        | OlyILOperation.GreaterThan(irArg1, irArg2) ->
            let irArg1, _ = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.GreaterThan(irArg1, irArg2, cenv.EmittedTypeBool) |> asExpr, RuntimeType.Bool

        | OlyILOperation.GreaterThanOrEqual(irArg1, irArg2) ->
            let irArg1, _ = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.GreaterThanOrEqual(irArg1, irArg2, cenv.EmittedTypeBool) |> asExpr, RuntimeType.Bool

        | OlyILOperation.LessThan(irArg1, irArg2) ->
            let irArg1, _ = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.LessThan(irArg1, irArg2, cenv.EmittedTypeBool) |> asExpr, RuntimeType.Bool

        | OlyILOperation.LessThanOrEqual(irArg1, irArg2) ->
            let irArg1, _ = importExpression cenv env None irArg1
            let irArg2, _ = importExpression cenv env None irArg2
            O.LessThanOrEqual(irArg1, irArg2, cenv.EmittedTypeBool) |> asExpr, RuntimeType.Bool

        | OlyILOperation.Not(irArg) ->
            let irArg, _ = importExpression cenv env None irArg
            O.Not(irArg, cenv.EmittedTypeBool) |> asExpr, RuntimeType.Bool

        | OlyILOperation.Negate(irArg) ->
            let irArg, resultTy = importExpression cenv env None irArg
            O.Negate(irArg, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.LoadRefCellContents(ilElementTy, ilArg) ->
            let elementTy = cenv.ResolveType( env.ILAssembly, ilElementTy, env.GenericContext)
            let irArg, _ = importExpression cenv env None ilArg
            O.LoadRefCellContents(irArg, cenv.EmitType(elementTy)) |> asExpr, elementTy

        | OlyILOperation.LoadRefCellContentsAddress(ilElementTy, ilArg, ilByRefKind) ->
            let elementTy = cenv.ResolveType(env.ILAssembly, ilElementTy, env.GenericContext)
            let irArg, _ = importExpression cenv env None ilArg

            let irByRefKind =
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.ReadOnly -> OlyIRByRefKind.ReadOnly
                | OlyILByRefKind.WriteOnly -> OlyIRByRefKind.WriteOnly

            let resultTy = createByReferenceRuntimeType irByRefKind elementTy

            O.LoadRefCellContentsAddress(irArg, irByRefKind, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.StoreRefCellContents(ilArg1, ilArg2) ->
            let irArg1, irArgTy1 = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env (Some irArgTy1.StripAlias().TypeArguments[0]) ilArg2
            O.StoreRefCellContents(irArg1, irArg2, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.Print(ilArg) ->
            let irArg = importArgumentExpression cenv env (cenv.ResolveType(env.ILAssembly, OlyILTypeBaseObject, env.GenericContext)) ilArg
            O.Print(irArg, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.Cast(ilArgExpr, ilResultTy) ->
            let irArgExpr, irArgExprTy = importExpression cenv env None ilArgExpr
            let resultTy = cenv.ResolveType(env.ILAssembly, ilResultTy, env.GenericContext)

            let defaultCase() =
                O.Cast(irArgExpr, cenv.EmitType(resultTy)) |> asExpr, resultTy

            if (resultTy.IsAnyStruct && not irArgExprTy.IsAnyStruct) then
                if irArgExprTy.IsByRef_t then
                    if resultTy.IsAnyPtr || resultTy.IsByRef_t then
                        defaultCase()
                    else
                        failwith "Invalid cast"
                else
                    O.Unbox(irArgExpr, cenv.EmitType(resultTy)) |> asExpr, resultTy
            else
                defaultCase()

        | OlyILOperation.Throw(ilArgExpr, ilResultTy) ->
            let irArgExpr, _ = importExpression cenv env None ilArgExpr
            let resultTy = cenv.ResolveType(env.ILAssembly, ilResultTy, env.GenericContext)
            O.Throw(irArgExpr, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.Ignore(ilArg) ->
            let irArg, resultTy = importExpression cenv env None ilArg
            // If the argument expression returns void, then we do not need to emit an Ignore op.
            if resultTy.IsVoid_t then
                irArg, RuntimeType.Void
            else
                O.Ignore(irArg, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.Store(localIndex, ilArg) ->
            cenv.LocalMutability[localIndex] <- true

            let irArg = importArgumentExpression cenv env env.LocalTypes.[localIndex] ilArg
            O.Store(localIndex, irArg, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.StoreArgument(argIndex, ilArg) ->
            cenv.ArgumentMutability[argIndex] <- true

            let irArg = importArgumentExpression cenv env env.ArgumentTypes.[argIndex] ilArg
            O.StoreArgument(argIndex, irArg, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.StoreToAddress(ilArg1, ilArg2) ->
            let irArg1, argTy1 = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env (Some argTy1.StripAlias().TypeArguments[0]) ilArg2
            if (not(argTy1.IsAnyPtr) && not(argTy1.IsByRef_t)) || argTy1.IsReadOnlyByRef then
                failwith "Invalid IL."
            O.StoreToAddress(irArg1, irArg2, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.StoreField(ilFieldRef, ilArg1, ilArg2) ->
            let field = cenv.ResolveField(env.ILAssembly, ilFieldRef, env.GenericContext)
            if field.ILConstant.IsSome then
                OlyAssert.Fail("Cannot modify a constant.")
            let expectedArgTy1 = 
                if field.EnclosingType.IsAnyStruct then
                    createByReferenceRuntimeType OlyIRByRefKind.ReadWrite field.EnclosingType
                else
                    field.EnclosingType
            let irArg1, argTy1 = importExpression cenv env (Some expectedArgTy1) ilArg1
            let irArg2 = importArgumentExpression cenv env field.Type ilArg2

            if field.EnclosingType.IsNewtype then
                if field.EnclosingType.IsAnyStruct then
                    if not argTy1.IsReadWriteByRef then
                        OlyAssert.Fail("Expected ReadWrite byref.")
                else
                    OlyAssert.Fail("Expected newtype to be a struct.")

                E.Operation(NoRange, O.StoreToAddress(irArg1, irArg2, cenv.EmittedTypeVoid)), RuntimeType.Void
            else

            let irArg1, _ = env.HandleReceiver(cenv, expectedArgTy1, irArg1, argTy1, false)

            let emittedField = cenv.EmitField(field)

            let irField = OlyIRField(emittedField, field)
            O.StoreField(irField, irArg1, irArg2, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.StoreStaticField(ilFieldRef, ilArg) ->
            let field = cenv.ResolveField(env.ILAssembly, ilFieldRef, env.GenericContext)
            
            // TODO: Add this check for non-constructors.
            //if not field.IsMutable then
            //    failwith "Invalid IL."

            let irArg = importArgumentExpression cenv env field.Type ilArg

            let irField = OlyIRField(cenv.EmitField(field), field)
            let irOpExpr = O.StoreStaticField(irField, irArg, cenv.EmittedTypeVoid) |> asExpr

            match cenv.TryGetCallStaticConstructorExpression(field) with
            | Some callStaticCtorExpr ->
                E.Sequential(
                    callStaticCtorExpr,
                    irOpExpr
                ), RuntimeType.Void
            | _ ->
                irOpExpr, RuntimeType.Void

        | OlyILOperation.LoadField(ilFieldRef, ilArg) ->
            let field = cenv.ResolveField(env.ILAssembly, ilFieldRef, env.GenericContext)
            OlyAssert.True(field.ILConstant.IsNone)

            let irArg, argTy = importExpression cenv env (Some field.EnclosingType) ilArg

            if field.EnclosingType.IsNewtype then
                let irArg, argTy =
                    if argTy.IsByRef_t then
                        let elementTy = argTy.TypeArguments[0]
                        E.Operation(
                            NoRange,
                            O.LoadFromAddress(irArg, cenv.EmitType(elementTy))
                        ), elementTy
                    else
                        irArg, argTy
                irArg, argTy
            else

            let expectedArgTy =
                if field.EnclosingType.IsAnyStruct then
                    createByReferenceRuntimeType OlyIRByRefKind.ReadOnly field.EnclosingType
                else
                    field.EnclosingType
            let irArg, _ = env.HandleReceiver(cenv, expectedArgTy, irArg, argTy, false)

            let emittedField = cenv.EmitField(field)

            let irField = OlyIRField(emittedField, field)
            O.LoadField(irField, irArg, cenv.EmitType(field.Type)) |> asExpr, field.Type

        | OlyILOperation.LoadFieldAddress(ilFieldRef, ilArg, ilByRefKind) ->
            let field = cenv.ResolveField(env.ILAssembly, ilFieldRef, env.GenericContext)

            if field.ILConstant.IsSome then
                failwith "Cannot take the address of a constant."

            let irByRefKind =
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.ReadOnly -> OlyIRByRefKind.ReadOnly
                | OlyILByRefKind.WriteOnly -> OlyIRByRefKind.WriteOnly
            let expectedArgTy =
                if field.EnclosingType.IsAnyStruct then
                    createByReferenceRuntimeType OlyIRByRefKind.ReadOnly field.EnclosingType
                else
                    field.EnclosingType

            let irArg, argTy = importExpression cenv env (Some expectedArgTy) ilArg

            if field.EnclosingType.IsNewtype then
                if argTy.IsByRef_t then
                    irArg, argTy
                else
                    OlyAssert.Fail("Expected a byref type")
            else

            let irArg, _ = env.HandleReceiver(cenv, expectedArgTy, irArg, argTy, false)

            let emittedField = cenv.EmitField(field)

            let irField = OlyIRField(emittedField, field)
            let resultTy = createByReferenceRuntimeType irByRefKind field.Type
            O.LoadFieldAddress(irField, irArg, irByRefKind, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.LoadFromAddress(ilArg) ->
            let irArg, resultTy = importExpression cenv env None ilArg
            let resultTy =
                if resultTy.IsByRef_t then
                    resultTy.TypeArguments[0]
                else
                    failwith "Expected ByRef type."
            O.LoadFromAddress(irArg, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.NewTuple(ilElementTys, ilArgs, ilNameHandles) ->
            let elementTys = cenv.ResolveTypes(env.ILAssembly, ilElementTys, env.GenericContext)
            let irArgs =
                if elementTys.Length = ilArgs.Length then
                    (elementTys, ilArgs)
                    ||> ImArray.map2 (fun elementTy ilArg -> importArgumentExpression cenv env elementTy ilArg)
                else
                    failwith "Invalid number of arguments for 'NewTuple'."

            let emittedElementTys =
                elementTys
                |> ImArray.map (fun x -> cenv.EmitType(x))

            let names =
                ilNameHandles
                |> ImArray.map (fun x -> env.ILAssembly.GetStringOrEmpty(x))

            let resultTy = RuntimeType.Tuple(elementTys, names)
            O.NewTuple(emittedElementTys, irArgs, cenv.EmitType(resultTy)) |> asExpr, resultTy

        // TODO: Get rid of 'NewMutableArray' in favor of 'NewArray'?
        | OlyILOperation.NewMutableArray(ilElementTy, ilSizeArgExpr) ->
            let elementTy = cenv.ResolveType(env.ILAssembly, ilElementTy, env.GenericContext)
            let irSizeArgExpr = importArgumentExpression cenv env RuntimeType.Int32 ilSizeArgExpr
                
            let emittedElementTy = cenv.EmitType(elementTy)
            let resultTy = RuntimeType.Array(elementTy, 1, true)

            O.NewMutableArray(emittedElementTy, irSizeArgExpr, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.NewArray(ilElementTy, ilKind, ilArgExprs) ->
            let elementTy = cenv.ResolveType(env.ILAssembly, ilElementTy, env.GenericContext)
            let irArgExprs =
                ilArgExprs
                |> ImArray.map (fun ilArgExpr -> importArgumentExpression cenv env elementTy ilArgExpr)

            let kind =
                match ilKind with
                | OlyILArrayKind.Immutable ->
                    OlyIRArrayKind.Immutable
                | OlyILArrayKind.Mutable ->
                    OlyIRArrayKind.Mutable

            let isMutable =
                // TODO: Get rid of 'isMutable' on RuntimeType.Array.
                match ilKind with
                | OlyILArrayKind.Mutable ->
                    true
                | _ ->
                    false
                
            let emittedElementTy = cenv.EmitType(elementTy)
            let resultTy = RuntimeType.Array(elementTy, 1, isMutable)
            O.NewArray(emittedElementTy, kind, irArgExprs, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.NewRefCell(ilElementTy, ilArg) ->
            let elementTy = cenv.ResolveType(env.ILAssembly, ilElementTy, env.GenericContext)
            let irArg = importArgumentExpression cenv env elementTy ilArg
                
            let emittedElementTy = cenv.EmitType(elementTy)
            let resultTy = RuntimeType.ReferenceCell(elementTy)
            O.NewRefCell(emittedElementTy, irArg, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.CallIndirect(ilFunArg, ilArgs) ->
            let irFunArg, funArgTy = importExpression cenv env None ilFunArg

            match funArgTy with
            | RuntimeType.Entity(ent) when ent.IsClosure ->
                let ilAsm = ent.ILAssembly
                let ilInvokeFuncDefHandle =
                    let ilEntDef = ilAsm.GetEntityDefinition(ent.ILEntityDefinitionHandle)
                    ilEntDef.FunctionHandles
                    |> Seq.filter (fun x ->
                        let ilFuncDef = ilAsm.GetFunctionDefinition(x)
                        not ilFuncDef.IsStatic && not ilFuncDef.IsConstructor
                    )
                    |> Seq.exactlyOne

                let invokeFunc = cenv.ResolveFunctionDefinition(funArgTy, ilInvokeFuncDefHandle)

                if invokeFunc.TypeParameters.IsEmpty |> not then
                    raise(System.NotImplementedException("CallIndirect on generic closure invoke function."))

                let emittedInvokeFunc = cenv.EmitFunction(invokeFunc)
                let emittedReturnTy = cenv.EmitType(invokeFunc.ReturnType)

                let irArgs = 
                    let argTys = invokeFunc.Parameters |> ImArray.map (fun x -> x.Type)
                    if argTys.Length = ilArgs.Length then
                        (argTys, ilArgs)
                        ||> ImArray.map2 (fun argTy ilArg -> importArgumentExpression cenv env argTy ilArg)
                    else
                        failwith "Invalid number of arguments for 'CallIndirect'."

                O.Call(OlyIRFunction(emittedInvokeFunc, invokeFunc), irArgs |> ImArray.prependOne irFunArg, emittedReturnTy) |> asExpr, invokeFunc.ReturnType
            | _ ->

            let argTys, returnTy =
                match funArgTy.StripAliasAndNewtype() with
                | RuntimeType.Function(argTys, returnTy, OlyIRFunctionKind.Normal)
                | RuntimeType.NativeFunctionPtr(_, argTys, returnTy) ->
                    argTys, returnTy
                | RuntimeType.ByRef(RuntimeType.Function(argTys, returnTy, OlyIRFunctionKind.Scoped), _) ->
                    argTys, returnTy
                | _ ->
                    failwith "Invalid indirect call."

            let irArgs = 
                if argTys.Length = ilArgs.Length then
                    (argTys, ilArgs)
                    ||> ImArray.map2 (fun argTy ilArg -> importArgumentExpression cenv env argTy ilArg)
                else
                    failwith "Invalid number of arguments for 'CallIndirect'."

            let emittedArgTys =
                argTys
                |> ImArray.map (fun x -> cenv.EmitType(x))
            let emittedReturnTy = cenv.EmitType(returnTy)

            O.CallIndirect(emittedArgTys, irFunArg, irArgs, emittedReturnTy) |> asExpr, returnTy

        | OlyILOperation.New(ilFuncInst, ilArgs) ->
            let func = cenv.ResolveFunction(env.ILAssembly, ilFuncInst, env.GenericContext, env.PassedWitnesses)

            OlyAssert.True(func.Flags.IsConstructor)
            OlyAssert.True(func.Flags.IsInstance)

            let parTys =
                func.Parameters
                |> ImArray.map (fun x -> x.Type)
            let irArgs = ilArgs |> ImArray.mapi (fun i ilArg -> importArgumentExpression cenv env parTys.[i] ilArg)

            let enclosingTy = func.EnclosingType

            if enclosingTy.IsNewtype then
                irArgs[0], enclosingTy.RuntimeType.Value
            else

            let emittedFunc = cenv.EmitFunction(func)
            let emittedEnclosingTy = cenv.EmitType(enclosingTy)

            let irFunc = OlyIRFunction(emittedFunc, func)
            let newExpr = O.New(irFunc, irArgs, emittedEnclosingTy) |> asExpr

            if func.EnclosingType.Formal <> env.Function.EnclosingType.Formal then
                match cenv.TryGetCallStaticConstructorExpression(func) with
                | Some callStaticCtorExpr ->
                    E.Sequential(
                        callStaticCtorExpr,
                        newExpr
                    ), enclosingTy
                | _ ->
                    newExpr, enclosingTy
            else
                newExpr, enclosingTy

        | OlyILOperation.Call(ilFuncInst, ilArgs) ->
            let constrainedTy =
                match ilFuncInst.Enclosing with
                | OlyILEnclosing.Entity entInst -> cenv.ResolveType(env.ILAssembly, entInst.AsType, env.GenericContext)
                | OlyILEnclosing.Witness(ty, _) -> cenv.ResolveType(env.ILAssembly, ty, env.GenericContext)
                | _ -> failwith "Invalid enclosing."
            let func = resolveFunction ilFuncInst
#if DEBUG || CHECKED
            Log(
                let witnesses = func.Witnesses
                let witnessText = 
                    if witnesses.IsEmpty then
                        ""
                    else
                        let text = witnesses |> ImArray.map (fun x -> x.TypeExtension.Name.ToString()) |> (String.concat "\n")
                        $" - Witnesses: {text}"
                $"Calling Function: {func.EnclosingType.Name}.{func.Name}{witnessText}"
            )
#endif
            assert(if func.Flags.IsStatic then ilArgs.Length = func.Parameters.Length else ilArgs.Length = func.Parameters.Length + 1)
            let irArgs = resolveFunctionArgs func ilArgs false
            handleCall constrainedTy func irArgs false

        | OlyILOperation.CallVirtual(ilFuncInst, ilArgs) ->
            let constrainedTy =
                match ilFuncInst.Enclosing with
                | OlyILEnclosing.Entity entInst -> cenv.ResolveType(env.ILAssembly, entInst.AsType, env.GenericContext)
                | OlyILEnclosing.Witness(ty, _) -> cenv.ResolveType(env.ILAssembly, ty, env.GenericContext)
                | _ -> failwith "Invalid enclosing."
            let func = resolveFunction ilFuncInst
#if DEBUG || CHECKED
            Log(
                let witnesses = func.Witnesses
                let witnessText = 
                    if witnesses.IsEmpty then
                        ""
                    else
                        let text = witnesses |> ImArray.map (fun x -> x.TypeExtension.Name.ToString()) |> (String.concat "\n")
                        $" - Witnesses: {text}"
                $"Calling Virtual Function: {func.EnclosingType.Name}.{func.Name}{witnessText}"
            )
#endif
            assert(if func.Flags.IsStatic then ilArgs.Length = func.Parameters.Length else ilArgs.Length = func.Parameters.Length + 1)
            let irArgs = resolveFunctionArgs func ilArgs true
            handleCall constrainedTy func irArgs true

        | OlyILOperation.Witness _ ->
            failwith "Invalid witness."

let importExpression (cenv: cenv<'Type, 'Function, 'Field>) (env: env<'Type, 'Function, 'Field>) (expectedTyOpt: RuntimeType option) (ilExpr: OlyILExpression) : E<'Type, 'Function, 'Field> * RuntimeType =
    let (irExpr, actualTy) as result = 
        StackGuard.Do(fun () ->
            importExpressionAux cenv env expectedTyOpt ilExpr
        )

    match expectedTyOpt with
    | None -> result
    | Some(expectedTy) ->

    let strippedExpectedTy = expectedTy.StripAliasAndNewtype()
    let strippedActualTy = actualTy.StripAliasAndNewtype()
        
    if strippedExpectedTy.IsVoid_t && strippedActualTy.IsUnit_t then
        E.Operation(NoRange, O.Ignore(irExpr, cenv.EmittedTypeVoid)), expectedTy
    else
        result

let importArgumentExpressionAux (cenv: cenv<'Type, 'Function, 'Field>) (env: env<'Type, 'Function, 'Field>) (isReceiver: bool) (expectedArgTy: RuntimeType) (ilArg: OlyILExpression) (isVirtual: bool) =
    let irArg, argTy = importExpression cenv env (Some expectedArgTy) ilArg

    let expectedArgTy = expectedArgTy.StripAliasAndNewtype()
    let argTy = argTy.StripAliasAndNewtype()

#if DEBUG || CHECKED
    OlyAssert.False(expectedArgTy.IsTypeExtension)
    OlyAssert.False(expectedArgTy.IsModule)
    OlyAssert.False(argTy.IsTypeExtension)
    OlyAssert.False(argTy.IsModule)
#endif

    let irArg, argTy =
        if isReceiver then
            env.HandleReceiver(cenv, expectedArgTy, irArg, argTy, isVirtual)
        else
            irArg, argTy

#if DEBUG || CHECKED
    OlyAssert.False(argTy.IsTypeExtension)
    OlyAssert.False(argTy.IsModule)
#endif

    if (not expectedArgTy.IsAnyPtr && not argTy.IsAnyPtr) then
        // TODO: Add extra checks? And/or add a new node that understands the relationship between the two types.
        if not expectedArgTy.IsAbstract && expectedArgTy.IsByRef_t <> argTy.IsByRef_t then
            failwith $"Runtime Error: Expected type '{expectedArgTy.DebugText}' but got '{argTy.DebugText}'."

    if argTy.Formal = expectedArgTy.Formal then
        if argTy.Witnesses.Length <> expectedArgTy.Witnesses.Length then
            failwith $"Runtime Error: Expected '{expectedArgTy.Witnesses.Length}' witnesses but got '{argTy.Witnesses.Length}'."
        if argTy <> expectedArgTy then
            failwith $"Runtime Error: Expected type '{expectedArgTy.DebugText}' but got '{argTy.DebugText}'."
        irArg
    else
        if expectedArgTy.IsVoid_t && argTy.IsUnit_t then
            E.Operation(NoRange, O.Ignore(irArg, cenv.EmittedTypeVoid))
        elif not expectedArgTy.IsAnyStruct && argTy.IsTypeVariable then
            E.Operation(NoRange, O.Box(irArg, cenv.EmitType(expectedArgTy)))
        
        elif subsumesType expectedArgTy argTy then
            if argTy.IsAnyStruct && not(expectedArgTy.IsAnyStruct) then
                E.Operation(NoRange, O.Box(irArg, cenv.EmitType(expectedArgTy)))
            else
                // No need to upcast for newtypes if its extending type is the same as the expected type.
                if argTy.IsNewtype && argTy.RuntimeType.Value.Formal = expectedArgTy.Formal then
                    irArg
                else
                    E.Operation(NoRange, O.Upcast(irArg, cenv.EmitType(expectedArgTy)))
        elif argTy.IsObjectType && not(expectedArgTy.IsObjectType) then // TODO: We shouldn't need to do this, because we will require a Cast node to handle it.
            E.Operation(NoRange, O.Unbox(irArg, cenv.EmitType(expectedArgTy)))
        else
            let ty =
                if isReceiver && argTy.IsByRef_t then
                    argTy.TypeArguments[0]
                else
                    argTy
            if ty = expectedArgTy then
                irArg
            else

            let possibleWitnessOpt =
                cenv.TryFindPossibleWitness(ty, expectedArgTy, env.PassedWitnesses)

            match possibleWitnessOpt with
            | Some(witness) ->
                let witnessUpcastedTy = cenv.EmitType(witness.TypeExtension)
                let resultTy = cenv.EmitType(expectedArgTy)
                E.Operation(NoRange, O.Witness(irArg, witnessUpcastedTy, resultTy))
            | _ ->
                // We could not prove the types match, but we expected a non-struct
                // and we have a struct, then always box.
                if argTy.IsAnyStruct && not expectedArgTy.IsAnyStruct then
                    E.Operation(NoRange, O.Box(irArg, cenv.EmitType(expectedArgTy)))
                else
                    match argTy, expectedArgTy with
                    | RuntimeType.ByRef(argTy, kind), RuntimeType.ByRef(expectedArgTy, expectedKind)
                    | RuntimeType.ByRef(argTy, kind), RuntimeType.ByRef(expectedArgTy, expectedKind) 
                    | RuntimeType.ByRef(argTy, kind), RuntimeType.ByRef(expectedArgTy, expectedKind) when argTy.StripAliasAndNewtype() = expectedArgTy.StripAliasAndNewtype() ->
                        match kind, expectedKind with
                        | OlyIRByRefKind.ReadWrite, OlyIRByRefKind.ReadWrite
                        | OlyIRByRefKind.ReadWrite, OlyIRByRefKind.ReadOnly
                        | OlyIRByRefKind.ReadWrite, OlyIRByRefKind.WriteOnly
                        | OlyIRByRefKind.ReadOnly, OlyIRByRefKind.ReadOnly
                        | OlyIRByRefKind.WriteOnly, OlyIRByRefKind.WriteOnly ->
                            irArg
                        | _ ->
                            let currentFunction = env.Function
                            let dumpExpr = Dump.DumpExpression irArg
                            failwith $"Expected {expectedKind} ByRef, but was given a {kind} ByRef. \n\n{currentFunction.EnclosingType.DebugText}.{currentFunction.Name}:\n{dumpExpr}"
                    | _ ->
                        // TODO: Add extra checks? And/or add a new node that understands the relationship between the two types.
                        if argTy.IsByRef_t && (argTy.TypeArguments[0].IsTypeVariable || argTy.TypeArguments[0].IsAnyStruct) then
                            if expectedArgTy.IsAbstract || expectedArgTy.IsAnyPtr || expectedArgTy.IsAnyNativeInt then
                                irArg
                            else
                                failwith $"Type {argTy.DebugText} is not a sub-type of {expectedArgTy.DebugText}."
                        elif (argTy.IsAnyPtr || argTy.IsAnyNativeInt) && (expectedArgTy.IsAnyPtr || expectedArgTy.IsAnyNativeInt) then
                            irArg
                        elif (argTy.IsEnumOrNewtype && argTy.RuntimeType.Value = expectedArgTy) || (expectedArgTy.IsEnumOrNewtype && expectedArgTy.RuntimeType.Value = argTy) then
                            irArg
                        else
                            failwith $"Type {argTy.DebugText} is not a sub-type of {expectedArgTy.DebugText}."

// TODO: We should try to replace 'importArgumentExpression' with just 'importExpression'.
let importArgumentExpression (cenv: cenv<'Type, 'Function, 'Field>) (env: env<'Type, 'Function, 'Field>) (expectedArgTy: RuntimeType) (ilArg: OlyILExpression) =
    importArgumentExpressionAux cenv env false expectedArgTy ilArg false

let importReceiverExpression (cenv: cenv<'Type, 'Function, 'Field>) (env: env<'Type, 'Function, 'Field>) (isVirtual: bool) (expectedArgTy: RuntimeType) (ilArg: OlyILExpression) =
    importArgumentExpressionAux cenv env true expectedArgTy ilArg isVirtual

let importFunctionBody 
        (vm: OlyRuntime<'Type, 'Function, 'Field>)
        (asmIdentity: OlyILAssemblyIdentity)
        (ilEntDefHandle: OlyILEntityDefinitionHandle)
        (ilFuncDefHandle: OlyILFunctionDefinitionHandle) 
        (genericContext: GenericContext) : OlyIRFunctionBody<'Type, 'Function, 'Field> =

    let asm = vm.Assemblies[asmIdentity]
    let enclosingTy = vm.ResolveTypeDefinition(asm.ilAsm, ilEntDefHandle)
    let func = vm.ResolveFunctionDefinition(enclosingTy, ilFuncDefHandle)

    OlyAssert.True(func.IsFormal)
    OlyAssert.True(enclosingTy.IsFormal)

    let ilAsm = asm.ilAsm
    let ilFuncDef = ilAsm.GetFunctionDefinition(ilFuncDefHandle)
    let ilFuncBody = ilAsm.GetFunctionBody(ilFuncDef.BodyHandle.contents.Value)

    let enclosingTy =
        let tyArgs =
            enclosingTy.TypeArguments
            |> ImArray.map (fun x -> 
                x.Substitute(genericContext)
            )
        enclosingTy.Apply(tyArgs).SetWitnesses(genericContext.PassedWitnesses)

    let funcTyArgs =
        func.TypeArguments
        |> ImArray.map (fun x -> 
            x.Substitute(genericContext)
        )

    let func = func.MakeInstance(enclosingTy, funcTyArgs) |> setWitnessesToFunction genericContext.PassedWitnesses genericContext
    let enclosingTy = enclosingTy.StripExtension()

    let instanceTy =
        if enclosingTy.IsAnyStruct then
            let irByRefKind =
                if func.IsMutable then
                    OlyIRByRefKind.ReadWrite
                else
                    OlyIRByRefKind.ReadOnly
            createByReferenceRuntimeType irByRefKind enclosingTy
        else
            enclosingTy

    let argTys =
        if func.Flags.IsInstance then
            func.Parameters
            |> ImArray.map (fun x -> x.Type)
            |> ImArray.prependOne instanceTy
        else
            func.Parameters
            |> ImArray.map (fun x -> x.Type)

  //  try
    vm.ImportFunctionBody(
        func,
        ilAsm, 
        ilFuncBody, 
        argTys, 
        func.ReturnType, 
        genericContext.SetPassedWitnesses(func.Witnesses)
    )
    //with
    //| ex ->
    //    let msg = $"Invalid Program in: {func.EnclosingType.Name}.{func.Name}\n{ex.StackTrace}\n\n"
    //    raise(AggregateException(msg, ex))

[<NoEquality;NoComparison>]
type TypeCache<'Type, 'Function, 'Field> =
    {
        Functions: RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Type>
        ScopedFunctions: RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Type>
    }

[<Sealed>]
type OlyRuntime<'Type, 'Function, 'Field>(emitter: IOlyRuntimeEmitter<'Type, 'Function, 'Field>) as this =

    let assemblies = Dictionary<OlyILAssemblyIdentity, RuntimeAssembly<'Type, 'Function, 'Field>>(OlyILAssemblyIdentity.Comparer)

    let mutable isEmittingTypeDefinition = false
    let delayed = Queue()

    let inlineFunctionBodyCache: LruCache<RuntimeFunction, Lazy<OlyIRFunctionBody<'Type, 'Function, 'Field>>> = LruCache(64)

    let primitiveTypes = Dictionary<RuntimeType, RuntimeType>()
    let addPrimitiveType primTy ty =
        if primitiveTypes.TryAdd(primTy, ty) |> not then
            failwith "Primitive type already exists."
        primitiveTypes[primTy] <- ty
    let tryGetPrimitiveType primTy =
        match primitiveTypes.TryGetValue primTy with
        | true, ty -> ValueSome ty
        | _ -> ValueNone

    let typeCache = 
        {
            Functions = RuntimeTypeArgumentListTable()
            ScopedFunctions = RuntimeTypeArgumentListTable()
        }

    let resolveFunctionDefinition (enclosingTy: RuntimeType) ilFuncDefHandle =
        OlyAssert.True(enclosingTy.IsFormal)
        let asm = assemblies[enclosingTy.AssemblyIdentity]

        match asm.FunctionDefinitionCache.TryGetValue ilFuncDefHandle with
        | true, (res, _) -> res
        | _ ->
            let res = createFunctionDefinition this enclosingTy ilFuncDefHandle
            if not res.IsFormal then
                failwith "Expected formal function."
            asm.FunctionDefinitionCache.[ilFuncDefHandle] <- (res, RuntimeTypeArgumentWitnessListTable())
            res

    let resolveWitness (ilAsm: OlyILReadOnlyAssembly) (ilWitness: OlyILWitness) (genericContext: GenericContext) =
        match ilWitness with
        | OlyILWitness.Implementation(index, ilKind, ilEntInst, ilSpecificAbstractFuncInstOpt) ->
            let implTy = this.ResolveType(ilAsm, ilEntInst.AsType, genericContext)
            if implTy.IsIntrinsic then
                OlyAssert.True(ilSpecificAbstractFuncInstOpt.IsNone)
                RuntimeWitness(index, ilKind, implTy, implTy, None)
            else
                let ty = genericContext.GetErasedTypeArgument(index, ilKind)
                let funcOpt =
                    ilSpecificAbstractFuncInstOpt
                    |> Option.map (fun x -> this.ResolveFunction(ilAsm, x, GenericContext.Default))
                RuntimeWitness(index, ilKind, ty, implTy, funcOpt)

    let tryResolveFunction ilAsm1 (ilFuncSpec1: OlyILFunctionSpecification) (funcTyArgs: _ imarray) (enclosingTy1: RuntimeType) (genericContext: GenericContext) =
        let enclosingTyParCount1 = enclosingTy1.TypeParameters.Length

        let tryResolve (enclosingTy1: RuntimeType) =
            if enclosingTy1.IsBuiltIn then ImArray.empty, funcTyArgs
            else

            let genericContext1 =
                if genericContext.IsErasing then
                    GenericContext.CreateErasing(enclosingTy1.TypeArguments.AddRange(funcTyArgs))
                else
                    GenericContext.Create(enclosingTy1.TypeArguments.AddRange(funcTyArgs))

            let asm = assemblies.[enclosingTy1.AssemblyIdentity]
            let ilEntDefHandle = enclosingTy1.ILEntityDefinitionHandle
            let ilEntDef = asm.ilAsm.GetEntityDefinition(ilEntDefHandle)
            let genericContext2 =
                if genericContext.IsErasing then
                    GenericContext.CreateErasing(enclosingTy1.TypeArguments.AddRange(funcTyArgs))
                else
                    GenericContext.Create(enclosingTy1.TypeArguments.AddRange(funcTyArgs))
            let enclosingTyParCount2 = ilEntDef.FullTypeParameterCount

            let find funcHandles =
                funcHandles
                |> ImArray.choose (fun ilFuncDefHandle2 ->
                    let ilFuncDef2 = asm.ilAsm.GetFunctionDefinition(ilFuncDefHandle2)
                    let ilFuncSpec2 = asm.ilAsm.GetFunctionSpecification(ilFuncDef2.SpecificationHandle)
                    if this.AreFunctionSpecificationsEqual(enclosingTyParCount1, ilAsm1, ilFuncSpec1, genericContext1, enclosingTyParCount2, asm.ilAsm, ilFuncSpec2, genericContext2) then
                        this.ResolveFunctionDefinition(enclosingTy1.Formal, ilFuncDefHandle2)
                        |> Some
                    else
                        None
                )

            let funcs =
                let funcName = ilAsm1.GetStringOrEmpty(ilFuncSpec1.NameHandle)
                if String.IsNullOrWhiteSpace(funcName) then
                    OlyAssert.Fail("Function has an invalid name.")
                asm.ilAsm.FindFunctionDefinitions(ilEntDefHandle, funcName)
                |> find
        
            funcs, funcTyArgs

        tryResolve enclosingTy1

    let getTotalTypeVariableUseCountFromType (ty: RuntimeType) =
        let rec loop count (ty: RuntimeType) =
            match ty.StripAlias() with
            | RuntimeType.Variable _ ->
                count + 1
            | RuntimeType.HigherVariable(tyArgs=tyArgs) ->
                let mutable count = count + 1
                tyArgs
                |> ImArray.iter (fun tyArg ->
                    count <- loop 0 tyArg
                )
                count
            | _ ->
                let mutable count = count
                ty.TypeArguments
                |> ImArray.iter (fun tyArg ->
                    count <- loop 0 tyArg
                )
                count
        loop 0 ty

    let getTotalTypeVariableUseCountFromFunction (func: RuntimeFunction) =
        let mutable count = 0
        func.Parameters
        |> ImArray.iter (fun par ->
            count <- count + getTotalTypeVariableUseCountFromType par.Type
        )
        count <- count + getTotalTypeVariableUseCountFromType func.ReturnType
        count

    let resolveFunction ilAsm1 (ilFuncSpec1: OlyILFunctionSpecification) (enclosing: RuntimeEnclosing) funcTyArgs genericContext =
        let funcs, funcTyArgs = 
            tryResolveFunction ilAsm1 (ilFuncSpec1: OlyILFunctionSpecification) funcTyArgs enclosing.AsType genericContext

        let funcs =
            if funcs.Length > 1 then
                // Handles use-case.
                (*
                    interface IA<T> =
                
                        Test(x: T): ()
                        Test(x: int32): ()
                
                    class Test =
                        implements IA<int32>
                
                        Test(x: int32): () =
                            print(x)
                
                    main(): () =
                        let t = Test()
                        let t = t: IA<int32>
                        t.Test(123)
                *)
                funcs
                |> ImArray.filter (fun func ->
                    OlyAssert.True(func.IsFormal)
                    let isNotSpecific =
                        funcs
                        |> ImArray.exists (fun func2 ->
                            if obj.ReferenceEquals(func, func2) then false
                            else
                                let funcCount = getTotalTypeVariableUseCountFromFunction func
                                let func2Count = getTotalTypeVariableUseCountFromFunction func2
                                funcCount > func2Count
                        )
                    not isNotSpecific
                )
            else
                funcs

        if funcs.IsEmpty then
            let name = ilAsm1.GetStringOrEmpty(ilFuncSpec1.NameHandle)
            failwith $"Unable to find function definition for '{name}' on '{enclosing.AsType.Name}'."
        elif funcs.Length > 1 then
            let name = ilAsm1.GetStringOrEmpty(ilFuncSpec1.NameHandle)
            failwith $"Too many function definitions found for '{name}'."
        else
            OlyAssert.True(funcs[0].IsFormal)

            this.Verify(funcs[0])

            let funcTyArgs =
                // We definitly need to do this.
                // Erase function type arguments via resolving the function.
                // Then substitute function type arguments.
                funcTyArgs
                |> ImArray.map (fun tyArg ->
                    tyArg.Substitute(genericContext)
                )

            funcs[0].MakeInstance(enclosing.AsType, funcTyArgs)

    let findImmediateFormalFunctionsByTypeAndFunctionSignature (targetTy: RuntimeType) (targetFunc: RuntimeFunction) =
        let ty =
            if targetTy.IsPrimitive then
                match tryGetPrimitiveType targetTy with
                | ValueSome ty -> ty
                | _ -> targetTy
            else
                targetTy

        if ty.IsBuiltIn then ImArray.empty
        else

        let ilAsm = assemblies.[ty.AssemblyIdentity].ilAsm

        let funcs =
            ilAsm.FindFunctionDefinitions(ty.ILEntityDefinitionHandle, targetFunc.Name)
            |> ImArray.choose (fun ilFuncDefHandle2 ->
                let ilFuncDef2 = ilAsm.GetFunctionDefinition(ilFuncDefHandle2)
                let ilFuncSpec2 = ilAsm.GetFunctionSpecification(ilFuncDef2.SpecificationHandle)
                if 
                    ilFuncSpec2.TypeParameters.Length = targetFunc.TypeParameters.Length &&
                    ilFuncSpec2.Parameters.Length = targetFunc.Parameters.Length &&
                    ilFuncSpec2.IsInstance = targetFunc.Flags.IsInstance then
                        let formalFunc = this.ResolveFunctionDefinition(ty.Formal, ilFuncDefHandle2)
                        let func = formalFunc.MakeInstance(ty, targetFunc.TypeArguments)

                        let areParameterTysEqual =
                            (targetFunc.Parameters, func.Parameters)
                            ||> ImArray.forall2 (fun par1 par2 -> par1.Type = par2.Type)
                        if areParameterTysEqual && targetFunc.ReturnType = func.ReturnType then
                            Some(formalFunc)
                        else
                            None
                else
                    None
            )
        funcs

    let findFunctionsByTypeAndFunctionSignature (targetTy: RuntimeType) (targetFunc: RuntimeFunction) =
        let targetFunc =
            match targetFunc.Overrides with
            | Some overridesFunc -> overridesFunc
            | _ -> targetFunc
        let funcs = findImmediateFormalFunctionsByTypeAndFunctionSignature targetTy targetFunc
        if not funcs.IsEmpty then
            funcs
            |> ImArray.map (fun func -> func.MakeInstance(targetTy, targetFunc.TypeArguments))
        else
            // REVIEW: This behavior is reliant on the order of types in the 'getAllDistinctInheritsAndImplements' list.
            //         Figure out rules for this.
            let tys = getAllDistinctInheritsAndImplements targetTy
            tys
            |> ImArray.tryPick (fun ty ->
                if subsumesType targetFunc.EnclosingType ty then
                    let funcs = findImmediateFormalFunctionsByTypeAndFunctionSignature ty targetFunc
                    if funcs.IsEmpty then
                        None
                    else
                        funcs
                        |> ImArray.map (fun func -> func.MakeInstance(ty, targetFunc.TypeArguments))
                        |> Some
                else
                    None
            )
            |> Option.defaultValue ImArray.empty

    let trySolveWitness enclosing (func: RuntimeFunction) (passedWitnesses: RuntimeWitness imarray) =
        match enclosing with
        | RuntimeEnclosing.Witness(_, abstractTy, Some witness) ->
            let tyParIndex = witness.TypeVariableIndex
            let enclosingTy = witness.Type.Formal
            let tyExt = witness.TypeExtension

            let witnessFormalFuncs =
                passedWitnesses
                |> ImArray.filter (fun witness -> witness.TypeVariableIndex = tyParIndex && witness.Type = enclosingTy)
                |> ImArray.map (fun witness ->
                    if subsumesType abstractTy witness.TypeExtension then
                        findFunctionsByTypeAndFunctionSignature tyExt func
                    else
                        ImArray.empty
                )
                |> Seq.concat
                |> ImArray.ofSeq

            if witnessFormalFuncs.IsEmpty then
                None
            elif witnessFormalFuncs.Length > 1 then
                failwith $"Multiple witness functions of '{func.Name}' are found."
            else
                witnessFormalFuncs[0]
                |> Some
        | _ ->
            None

    let rec emitConstant ilAsm (ilConstantValue: OlyILConstant) (genericContext: GenericContext) : C<'Type, 'Function> * RuntimeType =
        match ilConstantValue with
        | OlyILConstant.UInt8(value) -> C.UInt8(value), RuntimeType.UInt8
        | OlyILConstant.Int8(value) -> C.Int8(value), RuntimeType.Int8
        | OlyILConstant.UInt16(value) -> C.UInt16(value), RuntimeType.UInt16
        | OlyILConstant.Int16(value) -> C.Int16(value), RuntimeType.Int16
        | OlyILConstant.UInt32(value) -> C.UInt32(value), RuntimeType.UInt32
        | OlyILConstant.Int32(value) -> C.Int32(value), RuntimeType.Int32
        | OlyILConstant.UInt64(value) -> C.UInt64(value), RuntimeType.UInt64
        | OlyILConstant.Int64(value) -> C.Int64(value), RuntimeType.Int64
        | OlyILConstant.Float32(value) -> C.Float32(value), RuntimeType.Float32
        | OlyILConstant.Float64(value) -> C.Float64(value), RuntimeType.Float64
        | OlyILConstant.True -> C.True, RuntimeType.Bool
        | OlyILConstant.False -> C.False, RuntimeType.Bool
        | OlyILConstant.Char16(value) -> C.Char16(value), RuntimeType.Char16
        | OlyILConstant.Utf16(value) -> C.Utf16(value), RuntimeType.Utf16
        | OlyILConstant.Array(ilTy, elements) ->
            let elementTy = this.ResolveType(ilAsm, ilTy, GenericContext.Default)
            let emittedTy = this.EmitType(elementTy)
            C.Array(emittedTy, elements |> ImArray.map (fun x -> emitConstant ilAsm x genericContext |> fst)), RuntimeType.Array(elementTy, 1, false)
        | OlyILConstant.TypeVariable(index, ilKind) ->
            if genericContext.IsErasing then
                match genericContext.GetErasedTypeArgument(index, ilKind).Strip() with
                | RuntimeType.ConstantInt32(value) ->
                    C.Int32(value), RuntimeType.Int32
                | _ ->
                    raise(NotSupportedException("constant constraint as value"))
            else
                // TODO: This really does not happen since we do not have a platform that supports it.
                // REVIEW: Perhaps we should never allow this to happen and constant types must always be erased.
                raise(NotSupportedException("constant variable"))
        | OlyILConstant.External(ilFuncInst, ilTy) -> 
            let func = this.ResolveFunction(ilAsm, ilFuncInst, GenericContext.Default, ImArray.empty)
            C.External(this.EmitFunction(func)), func.ReturnType

    let rec emitField (field: RuntimeField) =
        let asm = assemblies.[field.AssemblyIdentity]

        match field.EnclosingType.TryGetVariadicTypeArgument() with
        | ValueSome(variadicTyArg) when field.Formal.Type = variadicTyArg ->
            let expansions =
                match asm.FieldVariadicDefinitionCache.TryGetValue field.ILFieldDefinitionHandle with
                | true, expansions -> expansions
                | _ ->
                    let expansions = Dictionary()
                    asm.FieldVariadicDefinitionCache[field.ILFieldDefinitionHandle] <- expansions
                    expansions

            let emitted =
                match expansions.TryGetValue field.Name with
                | true, (_, emitted) -> emitted
                | _ ->
                    let emitted = RuntimeTypeArgumentListTable()
                    expansions[field.Name] <- (field, emitted)
                    emitted

            // fast path
            match emitted.TryGetValue field.EnclosingType.TypeArguments with
            | ValueSome res -> res
            | _ -> emitFieldNoCache asm emitted field
        | _ ->
        
        if field.IsFormal || field.EnclosingType.CanGenericsBeErased then
            match asm.FieldDefinitionCache.TryGetValue field.ILFieldDefinitionHandle with
            | true, (_, emitted) ->
                // fast path
                match emitted.TryGetValue field.EnclosingType.TypeArguments with
                | ValueSome res -> res
                | _ -> emitFieldNoCache asm emitted field
            | _ ->
                failwithf "Field definition not cached: %A" field.Name
        else
            match asm.FieldReferenceCache.TryGetValue field.ILFieldDefinitionHandle with
            | true, emitted ->
                // fast path
                match emitted.TryGetValue field.EnclosingType.TypeArguments with
                | ValueSome res -> res
                | _ -> emitFieldNoCache asm emitted field
            | _ ->
                let emitted = RuntimeTypeArgumentListTable()
                asm.FieldReferenceCache[field.ILFieldDefinitionHandle] <- emitted
                // fast path
                match emitted.TryGetValue field.EnclosingType.TypeArguments with
                | ValueSome res -> res
                | _ -> emitFieldNoCache asm emitted field


    and emitFieldNoCache (asm: RuntimeAssembly<_, _, _>) (emitted: RuntimeTypeArgumentListTable<_, _, _, _>) (field: RuntimeField) =
        // It's very important we emit the enclosing and field type before
        // we cache the emitted field. Without this, we could emit duplicate fields.
        let enclosingTy = field.EnclosingType
        let emittedEnclosingTy = 
            if enclosingTy.IsNewtype then
                if field.IsStatic then
                    // We need to actually emit the newtype as a type definition here
                    // so the static field can be emitted correctly.
                    emitTypeDefinition enclosingTy
                else
                    OlyAssert.Fail($"Cannot emit instance field for newtype '{enclosingTy.Name}'.")
            else
                this.EmitType(enclosingTy)
        let fieldTy = this.EmitType(field.Type)
        match emitted.TryGetValue enclosingTy.TypeArguments with
        | ValueSome res -> res
        | _ ->
            let irAttrs = emitAttributes asm.ilAsm field.Attributes

            let constantOpt =
                field.ILConstantValueOption
                |> Option.map (fun x ->
                    this.EmitILConstant(asm.ilAsm, x, GenericContext.Default) |> fst
                )

            let res = 
                if not field.IsStatic && enclosingTy.IsAnyStruct && field.Formal.EnclosingType.Formal = field.Formal.Type.Formal then
                    // TODO: This only checks one layer. We need to check deeper.
                    OlyAssert.Fail($"Struct type '{enclosingTy.Name}' recursively contains itself.")

                if field.IsFormal || enclosingTy.CanGenericsBeErased then
                    this.Emitter.EmitFieldDefinition(
                        emittedEnclosingTy,
                        field.Flags, 
                        field.Name,
                        fieldTy,
                        field.Index,
                        irAttrs,
                        constantOpt
                    )
                else
                    let emittedField = emitField field.Formal
                    this.Emitter.EmitFieldReference(emittedEnclosingTy, emittedField)
            emitted.[enclosingTy.TypeArguments] <- res
            res

    and emitAttributes (ilAsm: OlyILReadOnlyAssembly) (attrs: RuntimeAttribute imarray) =
        attrs
        |> ImArray.map (fun attr ->
            let irCtor = this.EmitFunction(attr.Constructor)
            let irArgs =
                attr.Arguments
                |> ImArray.map (fun x -> emitConstant ilAsm x GenericContext.Default |> fst)
            let irNamedArgs =
                attr.NamedArguments
                |> ImArray.map (fun { Kind = ilKind; NameHandle = ilNameHandle; Constant = ilConstant } ->
                    let irKind =
                        match ilKind with
                        | OlyILAttributeNamedArgumentKind.Property -> OlyIRAttributeNamedArgumentKind.Property
                        | OlyILAttributeNamedArgumentKind.Field -> OlyIRAttributeNamedArgumentKind.Field
                    let name = ilAsm.GetStringOrEmpty(ilNameHandle)
                    let irValue = emitConstant ilAsm ilConstant GenericContext.Default |> fst
                    { Kind = irKind; Name = name; Constant = irValue }: OlyIRAttributeNamedArgument<'Type, 'Function>
                )
            OlyIRAttribute(irCtor, irArgs, irNamedArgs)                           
        )

    and emitConstraints (ilAsm: OlyILReadOnlyAssembly) (ilConstrs: OlyILConstraint imarray) genericContext =
        ilConstrs
        |> ImArray.map (fun ilConstr ->
            match ilConstr with
            | OlyILConstraint.Null -> OlyIRConstraint.Null
            | OlyILConstraint.Struct -> OlyIRConstraint.Struct
            | OlyILConstraint.NotStruct -> OlyIRConstraint.NotStruct
            | OlyILConstraint.Unmanaged -> OlyIRConstraint.Unmanaged
            | OlyILConstraint.Blittable -> OlyIRConstraint.Blittable
            | OlyILConstraint.Scoped -> OlyIRConstraint.Scoped
            | OlyILConstraint.SubtypeOf(ilTy) ->
                let ty = this.ResolveType(ilAsm, ilTy, genericContext)
                OlyIRConstraint.SubtypeOf(this.EmitType(ty))
            | OlyILConstraint.ConstantType(ilTy) ->
                let ty = this.ResolveType(ilAsm, ilTy, genericContext)
                OlyIRConstraint.ConstantType(this.EmitType(ty))
            | OlyILConstraint.TraitType(ilTy) ->
                let ty = this.ResolveType(ilAsm, ilTy, genericContext)
                OlyIRConstraint.TraitType(this.EmitType(ty))
        )     

    and emitTypeDefinition (tyDef: RuntimeType) =
        let asm = assemblies.[tyDef.AssemblyIdentity]
        let isGenericsErased = tyDef.CanGenericsBeErased

        if not isGenericsErased && not tyDef.IsFormal then
            failwith "Expected formal type."
        
        match asm.EntityDefinitionCache.TryGetValue(tyDef.ILEntityDefinitionHandle) with
        | true, (_, emitted) ->
            let key = struct(tyDef.TypeArguments, tyDef.Witnesses)
            match emitted.TryGetValue key with
            | ValueSome res -> res
            | _ ->
                let mustDelayFuncs = isEmittingTypeDefinition
                isEmittingTypeDefinition <- true

                if isGenericsErased then
                    tyDef.TypeArguments
                    |> ImArray.iter (function
                        | RuntimeType.Variable _
                        | RuntimeType.HigherVariable _ -> 
                            invalidOp $"Type variable cannot be erased for '{tyDef.Name}'."
                        | _ -> 
                            ()
                    )

                let ilAsm = asm.ilAsm
        
                let enclosingChoice =
                    match tyDef.Enclosing with
                    | RuntimeEnclosing.Namespace(path) -> Choice1Of2(path)
                    | _ -> 
                        Choice2Of2(emitTypeDefinition tyDef.Enclosing.AsType)
        
                let ilEntDef = ilAsm.GetEntityDefinition(tyDef.ILEntityDefinitionHandle)

                let tyFlags =
                    if isGenericsErased && not tyDef.TypeParameters.IsEmpty then
                        RuntimeTypeFlags.GenericsErased
                    else
                        RuntimeTypeFlags.None

                let tyFlags =
                    if tyDef.IsExported then
                        tyFlags ||| RuntimeTypeFlags.Exported
                    else
                        tyFlags

                match emitted.TryGetValue key with
                | ValueSome res -> res
                | _ ->

                let kind = ilEntDef.Kind

                let flags = OlyIRTypeFlags(ilEntDef.Flags, tyFlags)

                let tyParCount =
                    if isGenericsErased then
                        0
                    else
                        tyDef.TypeParameters.Length

                let res = this.Emitter.EmitTypeDefinition(enclosingChoice, kind, flags, tyDef.Name, tyParCount)
                emitted.[key] <- res

                let runtimeTyOpt = 
                    tyDef.RuntimeType 
                    |> Option.map (fun x ->
                        this.EmitType(x)
                    )

                let irAttrs = 
                    match tyDef with
                    | RuntimeType.Entity(ent) ->
                        emitAttributes ilAsm ent.Attributes
                    | _ ->
                        ImArray.empty

                let tyPars =
                    if isGenericsErased then
                        ImArray.empty
                    else
                        tyDef.TypeParameters
                        |> ImArray.map (fun tyPar -> 
                            // REVIEW: Should we be passing a default generic context here?
                            OlyIRTypeParameter(tyPar.Name, emitConstraints ilAsm tyPar.ILConstraints GenericContext.Default)
                        )

                let inheritTys =
                    tyDef.Extends
                    |> ImArray.map (fun x ->
                        // Emit the type first before subscribing!
                        let ty = this.EmitType(x)
                        this.SubscribeType(x, tyDef)
                        ty
                    )

                let implementTys = 
                    if tyDef.IsNewtype then
                        OlyAssert.Equal(0, tyDef.Implements.Length)
                        ImArray.empty
                    else
                        tyDef.Implements
                        |> ImArray.map (fun x ->
                            // Emit the type first before subscribing!
                            let ty = this.EmitType(x)
                            this.SubscribeType(x, tyDef)
                            ty
                        )

                // Verify that types cannot be marked as private in a namespace.
                match enclosingChoice with
                | Choice1Of2 _ when flags.IsPrivate ->
                    failwith $"'{tyDef.Name}' is marked as private in a namespace."
                | _ ->
                    ()

                this.Emitter.EmitTypeDefinitionInfo(res, enclosingChoice, kind, flags, tyDef.Name, tyPars, inheritTys, implementTys, irAttrs, runtimeTyOpt)

                if not mustDelayFuncs then
                    isEmittingTypeDefinition <- false

                let fields = tyDef.Fields
                let fields =
                    if tyDef.IsNewtype then
                        // Do not emit an instance field for newtypes.
                        fields |> ImArray.filter (fun x -> x.IsStatic)
                    else
                        fields
                fields
                |> ImArray.iter (fun field ->
                    if kind <> OlyILEntityKind.Class && 
                       kind <> OlyILEntityKind.Struct && 
                       kind <> OlyILEntityKind.Closure &&
                       kind <> OlyILEntityKind.Enum &&
                       kind <> OlyILEntityKind.Module &&
                       kind <> OlyILEntityKind.Newtype then
                        failwithf "Fields can only be declared on module, class, struct or closure types: '%s'." field.Name

                    if not field.EnclosingType.IsScoped && field.Type.IsScoped then
                        failwith $"Field '{field.Name}' cannot be of type '{field.Type.Name}'."

                    this.EmitField(field) |> ignore
                )

                let procFuncHandles f =
                    if mustDelayFuncs then
                        delayed.Enqueue(f)
                    else
                        f()

                if tyDef.IsExported then
                    procFuncHandles(fun () ->
                        // Eagerly emit functions if the type is exported.
                        // Generic functions will be eagerly emitted - requires the target runtime to support generics.
                        ilEntDef.FunctionHandles
                        |> ImArray.iter (fun ilFuncDefHandle ->
                            let ilFuncDef = ilAsm.GetFunctionDefinition(ilFuncDefHandle)

                            if not ilFuncDef.IsIntrinsic then
                                let func = this.ResolveFunctionDefinition(tyDef.Formal, ilFuncDefHandle)
                                this.EmitFunction(func) |> ignore
                        )
                    )
                else
                    procFuncHandles(fun () ->
                        ilEntDef.FunctionHandles
                        |> ImArray.iter (fun ilFuncDefHandle ->
                            let ilFuncDef = ilAsm.GetFunctionDefinition(ilFuncDefHandle)

                            if ilFuncDef.IsConstructor && ilFuncDef.IsStatic then
                                let func = this.ResolveFunctionDefinition(tyDef.Formal, ilFuncDefHandle)
                                let func =
                                    if isGenericsErased && not func.EnclosingType.TypeParameters.IsEmpty then
                                        func.MakeReference(tyDef)
                                    else
                                        func
                                this.EmitFunction(func) |> ignore
                            // Eagerly emit functions that override an external function.
                            // Generic functions that override external generic functions will be eagerly emitted - requires the target runtime to support generics.
                            elif 
                                (tyDef.TypeArguments.IsEmpty || isGenericsErased) &&
                                not ilFuncDef.IsIntrinsic && not ilFuncDef.IsImported then
                                    // If the overriden function is external, we must emit.
                                    match ilFuncDef.Overrides with
                                    | Some(overrides) ->
                                        let overridenFunc = this.ResolveFunction(ilAsm, overrides, GenericContext.Default)

                                        // TODO: We need to take into account of generics being erased to eagerly emit.
                                        let canEagerlyEmit = 
                                            overridenFunc.IsExternal ||
                                            (overridenFunc.EnclosingType.TypeParameters.IsEmpty && overridenFunc.TypeParameters.IsEmpty)
                                        if canEagerlyEmit then
                                            let func = this.ResolveFunctionDefinition(tyDef.Formal, ilFuncDefHandle)
                                            let func2 = func.MakeReference(tyDef)
                                            this.EmitFunction(func2) |> ignore
                                    | _ ->
                                        ()
                        )
                    )

                if not mustDelayFuncs then
                    let mutable f = Unchecked.defaultof<_>
                    while delayed.TryDequeue(&f) do
                        f()


                ilEntDef.PropertyDefinitionHandles
                |> ImArray.iter (fun ilPropDefHandle ->
                    let ilPropDef = ilAsm.GetPropertyDefinition(ilPropDefHandle)

                    // TODO: This could break if the getter or setter is an intrinsic function. We should handle it.

                    let getterOpt =
                        if ilPropDef.Getter.IsNil then
                            None
                        else
                            if isGenericsErased then                               
                                this.ResolveFunctionDefinition(tyDef.Formal, ilPropDef.Getter).MakeReference(tyDef)
                                |> this.EmitFunction
                                |> Some
                            else
                                this.ResolveFunctionDefinition(tyDef, ilPropDef.Getter)
                                |> this.EmitFunction
                                |> Some

                    let setterOpt =
                        if ilPropDef.Setter.IsNil then
                            None
                        else
                            if isGenericsErased then
                                this.ResolveFunctionDefinition(tyDef.Formal, ilPropDef.Setter).MakeReference(tyDef)
                                |> this.EmitFunction
                                |> Some
                            else
                                this.ResolveFunctionDefinition(tyDef, ilPropDef.Setter)
                                |> this.EmitFunction
                                |> Some

                    let irAttrs =
                        ilPropDef.Attributes
                        |> ImArray.choose (fun ilAttr ->
                            this.TryResolveConstructorAttribute(ilAsm, ilAttr, GenericContext.Default, ImArray.empty)
                        )
                        |> emitAttributes ilAsm

                    let genericContext =
                        if isGenericsErased then
                            GenericContext.CreateErasing(tyDef.TypeArguments).SetPassedWitnesses(tyDef.Witnesses)
                        else
                            GenericContext.Default

                    emitter.EmitProperty(
                        res,
                        ilAsm.GetStringOrEmpty(ilPropDef.NameHandle),
                        this.EmitType(this.ResolveType(ilAsm, ilPropDef.Type, genericContext)),
                        irAttrs,
                        getterOpt,
                        setterOpt                           
                    )
                )

                emitter.OnTypeDefinitionEmitted(res)
        
                res
        | _ ->
            failwithf "Entity definition not cached: %A" tyDef.Name

    and emitExternalType (ty: RuntimeType) externalPlatform externalPath externalName =
        let asm = assemblies.[ty.AssemblyIdentity]
        if not ty.IsExternal then
            failwith "Type is not external."
        
        match asm.EntityDefinitionCache.TryGetValue(ty.ILEntityDefinitionHandle) with
        | true, (_, emitted) ->
            let key = struct(ImArray.empty, ImArray.empty)
            match emitted.TryGetValue key with
            | ValueSome res -> res
            | _ ->
                let ilAsm = asm.ilAsm
        
                let enclosingChoice =
                    match ty.Enclosing with
                    | RuntimeEnclosing.Namespace(path) -> Choice1Of2(path)
                    | _ -> Choice2Of2(this.EmitType(ty.Enclosing.AsType))
        
                let ilEntDef = ilAsm.GetEntityDefinition(ty.ILEntityDefinitionHandle)
                let tyParCount = ty.TypeParameters.Length

                // TODO: Read-only?
                let tyFlags = RuntimeTypeFlags.None
                let flags = OlyIRTypeFlags(ilEntDef.Flags, tyFlags)

                // Verify that types cannot be marked as private in a namespace.
                match enclosingChoice with
                | Choice1Of2 _ when flags.IsPrivate ->
                    failwith $"'{ty.Name}' is marked as private in a namespace."
                | _ ->
                    ()

                let res = this.Emitter.EmitExternalType(externalPlatform, externalPath, externalName, enclosingChoice, ilEntDef.Kind, flags, ty.Name, tyParCount)
                emitted.[key] <- res

                res
        | _ ->
            failwithf "Entity definition not cached: %A" ty.Name

    and emitTypeGenericInstance (ty: RuntimeType) =
        let asm = assemblies.[ty.AssemblyIdentity]

        let emitted =
            match asm.EntityInstanceCache.TryGetValue(ty.ILEntityDefinitionHandle) with
            | true, emitted -> emitted
            | _ ->
                match asm.EntityDefinitionCache.TryGetValue(ty.ILEntityDefinitionHandle) with
                | true, _ ->
                    let emitted = RuntimeTypeArgumentListTable()
                    asm.EntityInstanceCache[ty.ILEntityDefinitionHandle] <- emitted
                    emitted
                | _ ->
                    failwithf "Entity definition not cached: %A" ty.Name

        let fullTyArgs = ty.TypeArguments
        match emitted.TryGetValue fullTyArgs with
        | ValueSome res -> res
        | _ ->
#if DEBUG || CHECKED
            (ty.TypeParameters, fullTyArgs)
            ||> ImArray.iter2 (fun tyPar tyArg ->
                if tyPar.Arity = 0 && tyArg.IsTypeConstructor then
                    failwith "Unexpected type constructor."
            )
#endif
            let formalTy = emitType false ty.Formal
            let emittedFullTyArgs = fullTyArgs |> ImArray.map (fun x -> this.EmitTypeArgument(x))
            let res = this.Emitter.EmitTypeGenericInstance(formalTy, emittedFullTyArgs)
            emitted.[fullTyArgs] <- res
            res

    and emitType (_isTopLevelTyArg: bool) (ty: RuntimeType) =
        if ty.IsBuiltIn then
            match ty with
            | RuntimeType.ForAll _ ->
                raise(NotSupportedException("Emitting 'ForAll' type."))

            | RuntimeType.ConstantInt32(value) ->
                this.Emitter.EmitTypeConstantInt32(value)
            | RuntimeType.UInt8 -> this.TypeUInt8.Value
            | RuntimeType.Int8 -> this.TypeInt8.Value
            | RuntimeType.UInt16 -> this.TypeUInt16.Value
            | RuntimeType.Int16 -> this.TypeInt16.Value
            | RuntimeType.UInt32 -> this.TypeUInt32.Value
            | RuntimeType.Int32 -> this.TypeInt32.Value
            | RuntimeType.UInt64 -> this.TypeUInt64.Value
            | RuntimeType.Int64 -> this.TypeInt64.Value
            | RuntimeType.Float32 -> this.TypeFloat32.Value
            | RuntimeType.Float64 -> this.TypeFloat64.Value
            | RuntimeType.Bool -> this.TypeBool.Value
            | RuntimeType.NativeInt -> this.TypeNativeInt.Value
            | RuntimeType.NativeUInt -> this.TypeNativeUInt.Value
            | RuntimeType.Char16 -> this.TypeChar.Value
            | RuntimeType.Utf16 -> this.TypeUtf16.Value
            | RuntimeType.Void -> this.TypeVoid.Value
            | RuntimeType.Unit -> this.TypeUnit.Value
            | RuntimeType.BaseObject -> this.TypeBaseObject.Value
            | RuntimeType.Tuple(tyArgs, names) ->
                this.Emitter.EmitTypeTuple(tyArgs |> ImArray.map (emitType false), names)
            | RuntimeType.NativePtr(elementTy) ->
                this.Emitter.EmitTypeNativePtr(emitType false elementTy)
            | RuntimeType.ReferenceCell(elementTy) ->
                this.Emitter.EmitTypeRefCell(emitType false elementTy)
            | RuntimeType.Array(elementTy, rank, isMutable) ->
                let kind =
                    if isMutable then
                        OlyIRArrayKind.Mutable
                    else
                        OlyIRArrayKind.Immutable
                this.Emitter.EmitTypeArray(emitType false elementTy, rank, kind)
            | RuntimeType.Function(argTys, returnTy, kind) ->
                let cache =
                    match kind with
                    | OlyIRFunctionKind.Normal -> typeCache.Functions
                    | OlyIRFunctionKind.Scoped -> typeCache.ScopedFunctions
                let key = argTys.Add(returnTy)
                match cache.TryGetValue key with
                | ValueSome result -> result
                | _ ->
                    let result = this.Emitter.EmitTypeFunction(argTys |> ImArray.map (emitType false), emitType false returnTy, kind)
                    cache[key] <- result
                    result

            | RuntimeType.NativeFunctionPtr(ilCc, argTys, returnTy) ->
                this.Emitter.EmitTypeNativeFunctionPtr(ilCc, argTys |> ImArray.map (emitType false), emitType false returnTy)
            | RuntimeType.ByRef(elementTy, kind) ->
                this.Emitter.EmitTypeByRef(emitType false elementTy, kind)
            | RuntimeType.Variable(index, ilKind) ->
                let irKind =
                    match ilKind with
                    | OlyILTypeVariableKind.Type -> OlyIRTypeVariableKind.Type
                    | OlyILTypeVariableKind.Function -> OlyIRTypeVariableKind.Function
                    | _ -> OlyAssert.Fail("Invalid type variable kind.")
                this.Emitter.EmitTypeVariable(index, irKind)
            | RuntimeType.HigherVariable(index, tyArgs, ilKind) ->
                let emittedTyArgs =
                    tyArgs
                    |> ImArray.map (fun x -> emitType false x)
                let irKind =
                    match ilKind with
                    | OlyILTypeVariableKind.Type -> OlyIRTypeVariableKind.Type
                    | OlyILTypeVariableKind.Function -> OlyIRTypeVariableKind.Function
                    | _ -> OlyAssert.Fail("Invalid type variable kind.")
                this.Emitter.EmitTypeHigherVariable(index, emittedTyArgs, irKind)

            | RuntimeType.Entity _ -> 
                failwith "Invalid type."

        elif ty.IsNewtype then
            emitType false (ty.RuntimeType.Value)

        elif ty.IsIntrinsic then
            emitType false (ty.Strip())

        elif ty.IsFormal then
            match ty.TryGetImportInfo() with
            | Some(externalPlatform, externalPath, externalName) ->
                emitExternalType ty externalPlatform externalPath externalName
            | _ ->
                emitTypeDefinition ty

        else
            if ty.IsExternal then
                emitTypeGenericInstance ty
            else
                if ty.CanGenericsBeErased || ty.IsFormal then
                    emitTypeDefinition ty
                else
                    emitTypeDefinition ty.Formal |> ignore
                    emitTypeGenericInstance ty

    let optimizeFunctionBody (func: RuntimeFunction) (funcBody: OlyIRFunctionBody<_, _, _>) (genericContext: GenericContext) =
        let irTier = this.GetFunctionTier(func)

        OptimizeFunctionBody
            (fun targetFunc -> 
                let canErase = canPossiblyEraseGenericFunction func targetFunc 
                let genericContext = createGenericContextFromFunction canErase targetFunc
                this.TryResolveFunctionBody(targetFunc, genericContext) |> Option.map (fun x -> x.Value)
            )
            (fun (envFunc, func) ->
                this.EmitFunctionFromEnvironment(envFunc, func)
            )
            this.EmitType
            func
            funcBody.ArgumentFlags
            funcBody.LocalFlags
            funcBody.Expression
            genericContext
            irTier

    let emitFunctionBody (func: RuntimeFunction) emittedFunc (genericContext: GenericContext) =
        let body = this.TryResolveFunctionBody(func, genericContext).Value
        
        // Optimize before emitting
        let body =
            lazy
                let funcBody = body.Value
                optimizeFunctionBody func funcBody genericContext

        let irTier = this.GetFunctionTier(func)

        this.Emitter.EmitFunctionBody(body, irTier, emittedFunc)


    let tryFindType(fullyQualifiedTypeName: string, tyParCount: int32) =
        // TODO: This should be optimized.
        let splitted = fullyQualifiedTypeName.Split(".") |> ImArray.ofSeq
        let enclosingTargetNames = splitted.RemoveAt(splitted.Length - 1)
        let targetName = splitted[splitted.Length - 1]
        let rec collect (enclosingTargetNames: string imarray) targetName =
            assemblies.Values
            |> Seq.map (fun asm ->
                asm.ilAsm.FindEntityDefinitions(targetName)
                |> ImArray.filter (fun ilEntDefHandle ->
                    let ilEntDef = asm.ilAsm.GetEntityDefinition(ilEntDefHandle)
                    if ilEntDef.FullTypeParameterCount = tyParCount then
                        match ilEntDef.Enclosing with
                        | OlyILEnclosing.Namespace(path, _) ->
                            if path.Length = enclosingTargetNames.Length then
                                (path, enclosingTargetNames)
                                ||> ImArray.forall2 (fun handle name ->
                                    asm.ilAsm.GetStringOrEmpty(handle) = name
                                )
                            else
                                false
                        | OlyILEnclosing.Entity _ ->
                            // TODO: Handle nested types.
                            false
                        | _ ->
                            false
                    else
                        false
                )
                |> ImArray.map (fun ilEntDefHandle ->
                    this.ResolveTypeDefinition(asm.ilAsm, ilEntDefHandle)
                )
            )
            |> Seq.concat
        collect enclosingTargetNames targetName
        |> Seq.tryExactlyOne

    member val TypeVoid: _ Lazy =       lazy emitter.EmitTypeVoid()
    member val TypeUnit: _ Lazy  =       lazy emitter.EmitTypeUnit()
    member val TypeUInt8: 'Type Lazy  =      lazy emitter.EmitTypeUInt8()
    member val TypeInt8: _ Lazy  =       lazy emitter.EmitTypeInt8()
    member val TypeUInt16: _ Lazy  =     lazy emitter.EmitTypeUInt16()
    member val TypeInt16: _ Lazy  =      lazy emitter.EmitTypeInt16()
    member val TypeUInt32: _ Lazy  =     lazy emitter.EmitTypeUInt32()
    member val TypeInt32: _ Lazy  =      lazy emitter.EmitTypeInt32()
    member val TypeUInt64: _ Lazy  =     lazy emitter.EmitTypeUInt64()
    member val TypeInt64: _ Lazy  =      lazy emitter.EmitTypeInt64()
    member val TypeFloat32: _ Lazy  =    lazy emitter.EmitTypeFloat32()
    member val TypeFloat64: _ Lazy =    lazy emitter.EmitTypeFloat64()
    member val TypeChar: _ Lazy  =       lazy emitter.EmitTypeChar16()
    member val TypeUtf16: _ Lazy  =      lazy emitter.EmitTypeUtf16()
    member val TypeBool: _ Lazy  =       lazy emitter.EmitTypeBool()
    member val TypeNativeInt: _ Lazy  =  lazy emitter.EmitTypeNativeInt()
    member val TypeNativeUInt: _ Lazy  = lazy emitter.EmitTypeNativeUInt()
    member val TypeBaseObject: _ Lazy  = lazy emitter.EmitTypeBaseObject()

    member internal this.Assemblies: Dictionary<OlyILAssemblyIdentity, RuntimeAssembly<'Type, 'Function, 'Field>> = assemblies

    member this.TryResolveConstructorAttribute(ilAsm: OlyILReadOnlyAssembly, ilAttr: OlyILAttribute, genericContext: GenericContext, passedWitnesses: RuntimeWitness imarray): RuntimeAttribute option =
        match ilAttr with
        | OlyILAttribute.Constructor(ilFuncInst, ilArgs, ilNamedArgs) ->
            let func = this.ResolveFunction(ilAsm, ilFuncInst, genericContext, passedWitnesses)
            if not func.Flags.IsConstructor && not func.Flags.IsInstance then
                failwith "Expected an instance constructor."
            {
                RuntimeAttribute.Constructor = func
                RuntimeAttribute.Arguments = ilArgs
                RuntimeAttribute.NamedArguments = ilNamedArgs
            }
            |> Some
        | _ ->
            None

    member this.InitializeEmitter() =
        emitter.Initialize(this)

    member this.SubscribeType(receiverTy: RuntimeType, ty: RuntimeType) =
        if not receiverTy.IsBuiltIn then
            let asm = assemblies.[receiverTy.AssemblyIdentity]
            let tys =
                match asm.TypesThatInheritOrImplementType.TryGetValue(receiverTy.ILEntityDefinitionHandle) with
                | true, tys -> tys
                | _ ->
                    let tys = ResizeArray()
                    asm.TypesThatInheritOrImplementType.[receiverTy.ILEntityDefinitionHandle] <- tys
                    tys
            tys.Add(ty)

    member this.Emitter: IOlyRuntimeEmitter<'Type, 'Function, 'Field> = emitter

    member this.ImportAssembly(ilAsm: OlyILReadOnlyAssembly) =
        if assemblies.ContainsKey(ilAsm.Identity) then
            // TODO: Should we error on this?
            ()
            //failwithf "Assembly %A already imported." ilAsm.Identity
        else

        assemblies[ilAsm.Identity] <- {
            ilAsm = ilAsm
            EntityDefinitionCache = Dictionary()
            EntityInstanceCache = Dictionary()
            entRefCache = Dictionary()
            FunctionDefinitionCache = Dictionary()
            FieldDefinitionCache = Dictionary()
            FieldReferenceCache = Dictionary()
            FieldVariadicDefinitionCache = Dictionary()
            RuntimeTypeInstanceCache = RuntimeTypeInstanceCache(this, ilAsm)
            RuntimeFieldReferenceCache = RuntimeFieldReferenceCache()
            TypesThatInheritOrImplementType = Dictionary()
        }

        ilAsm.ForEachPrimitiveType(fun (ilTy, ilEntDefHandle) ->
            match ilTy with
            | OlyILTypeEntity _
            | OlyILTypeVariable _
            | OlyILTypeHigherVariable _ ->
                failwith "Invalid primitive IL type."
            | _ ->
                let ty = this.ResolveTypeDefinition(ilAsm, ilEntDefHandle)
                addPrimitiveType (this.ResolveType(ilAsm, ilTy, GenericContext.Default)) ty
        )

    member this.FindEntryPoint() =
        let entryPointOpt =
            assemblies
            |> Seq.choose (fun pair ->
                let ilAsm = pair.Value.ilAsm
                match ilAsm.EntryPoint with
                | Some(ilEnclosingTy, ilFuncDefHandle) ->
                    let ty = this.ResolveType(ilAsm, ilEnclosingTy, GenericContext.Default)
                    Some(this.ResolveFunctionDefinition(ty.Formal, ilFuncDefHandle))
                | _ ->
                    None
            )
            |> Seq.tryExactlyOne
        match entryPointOpt with
        | Some entryPoint -> entryPoint
        | _ -> failwith "Unable to find entry point."

    member this.FindFunctionsByTypeAndFunctionSignature(ty: RuntimeType, func: RuntimeFunction) : RuntimeFunction imarray =
        findFunctionsByTypeAndFunctionSignature ty func

    member this.EmitEntryPoint() =
        let entryPoint = this.FindEntryPoint()
        this.EmitFunction(entryPoint) |> ignore

    member this.EmitAheadOfTime() =
        assemblies
        |> Seq.iter (fun pair ->
            let ilAsm = pair.Value.ilAsm
            ilAsm.EntityDefinitions
            |> Seq.iter (fun (ilEntDefHandle, ilEntDef) ->
                if not ilEntDef.IsExternal && ilEntDef.IsExported then
                    let ty = this.ResolveTypeDefinition(ilAsm, ilEntDefHandle)
                    this.EmitType(ty) |> ignore
            )
        )

    member internal this.ResolveField(enclosingTy: RuntimeType, ilAsm: OlyILReadOnlyAssembly, index: int, ilFieldDefHandle: OlyILFieldDefinitionHandle) : RuntimeField =
        let asm = assemblies.[ilAsm.Identity]

        match asm.FieldDefinitionCache.TryGetValue ilFieldDefHandle with
        | true, (res, _) -> res
        | _ ->
            let ilFieldDef = ilAsm.GetFieldDefinition(ilFieldDefHandle)

            if ilFieldDef.IsMutable && enclosingTy.IsClosure then
                failwith "Fields on a closure cannot be mutable."

            let attrs =
                ilFieldDef.Attributes
                |> ImArray.choose (fun x -> this.TryResolveConstructorAttribute(ilAsm, x, GenericContext.Default, ImArray.empty))
                
            let ilConstOpt =
                match ilFieldDef with
                | OlyILFieldConstant(constant=ilConst) -> Some ilConst
                | _ -> None

            let ilFieldFlags = ilFieldDef.Flags

            let res =
                {
                    RuntimeField.Formal = Unchecked.defaultof<_>
                    RuntimeField.Name = ilAsm.GetStringOrEmpty(ilFieldDef.NameHandle)
                    RuntimeField.Type = this.ResolveType(ilAsm, ilFieldDef.Type, GenericContext.Default)
                    RuntimeField.EnclosingType = enclosingTy
                    RuntimeField.Flags = OlyIRFieldFlags(ilFieldFlags, ilFieldDef.MemberFlags, enclosingTy.IsExported)
                    RuntimeField.Index = index
                    RuntimeField.Attributes = attrs
                    RuntimeField.ILAssembly = ilAsm
                    RuntimeField.ILFieldDefinitionHandle = ilFieldDefHandle
                    RuntimeField.ILConstant = ilConstOpt
                }
            res.Formal <- res
            asm.FieldDefinitionCache.[ilFieldDefHandle] <- (res, RuntimeTypeArgumentListTable())
            res

    member internal this.ResolveField(ilAsm: OlyILReadOnlyAssembly, ilFieldRef: OlyILFieldReference, genericContext: GenericContext) : RuntimeField =
        match ilFieldRef with
        | OlyILFieldReference(ilEnclosing, ilName, ilFieldTy) ->
            let enclosingTy = (this.ResolveEnclosing(ilAsm, ilEnclosing, genericContext, ImArray.empty)).AsType
            let name = ilAsm.GetStringOrEmpty(ilName)

            let fields =
                enclosingTy.Fields
                |> ImArray.filter (fun field ->
                    field.Name = name
                )

            if fields.IsEmpty then
                failwith $"Field '{name}' not found."
            elif fields.Length > 1 then
                failwith $"Multiple fields of '{name}' are found."
            else
                let asm = assemblies[ilAsm.Identity]
                let fieldTy = fields[0].Type
                { fields.[0] with EnclosingType = enclosingTy; Type = fieldTy.Substitute(genericContext) }
                |> asm.RuntimeFieldReferenceCache.Intern

    member this.ResolveFunctionDefinition(enclosingTy: RuntimeType, ilFuncDefHandle: OlyILFunctionDefinitionHandle) : RuntimeFunction =
        resolveFunctionDefinition enclosingTy ilFuncDefHandle

    member this.ResolveFunction(ilAsm: OlyILReadOnlyAssembly, ilFuncInst: OlyILFunctionInstance, genericContext: GenericContext, passedWitnesses: RuntimeWitness imarray) : RuntimeFunction =
        let vm = this
        match ilFuncInst with
        | OlyILFunctionInstance(ilEnclosing, ilFuncSpecHandle, ilFuncTyArgs, ilWitnesses) ->
            let enclosing = vm.ResolveEnclosing(ilAsm, ilEnclosing, genericContext, passedWitnesses)
            let ilFuncSpec = ilAsm.GetFunctionSpecification(ilFuncSpecHandle)

            let funcTyArgs =
                ilFuncTyArgs
                |> ImArray.map (fun x -> this.ResolveType(ilAsm, x, genericContext))

            let witnesses =
                let fixedGenericContext =
                    // TODO: This could add support for witness resolving for type parameters on types.
                    //let funcTyArgs =
                    //    if func.Flags.IsConstructor then
                    //        func.EnclosingType.TypeArguments
                    //    else 
                    //        funcTyArgs
                    if genericContext.IsErasingFunction then
                        GenericContext.Default.AddErasingFunctionTypeArguments(funcTyArgs)
                    else                     
                        GenericContext.Default.AddFunctionTypeArguments(funcTyArgs)
                ilWitnesses
                |> ImArray.map (fun x -> 
                    resolveWitness ilAsm x fixedGenericContext
                )

            let passedAndFilteredWitnesses =
                enclosing.AsType.Witnesses.AddRange(witnesses.AddRange(passedWitnesses))
                |> Seq.distinct
                |> ImArray.ofSeq

            let funcInst = vm.ResolveFunction(ilAsm, ilFuncSpec, enclosing, funcTyArgs, genericContext)

            let witnessFuncOpt =
                match ilEnclosing with
                | OlyILEnclosing.Witness(ilTy, _) ->
                    let possibleWitnesses = this.FilterWitnesses(ilTy, passedWitnesses)
                    vm.TryFindWitnessFunction(enclosing, funcInst, possibleWitnesses)
                | _ ->
                    None

            let findFuncs (ty: RuntimeType) =
                vm.FindFunctionsByTypeAndFunctionSignature(ty, funcInst)

            let witnessFuncOpt =
                let witnessFuncOpt2 =
                    match enclosing with
                    | RuntimeEnclosing.Witness(realTy, enclosingTy, None) ->
                        let possibleWitnesses =
                            match ilEnclosing with
                            | OlyILEnclosing.Witness(ilTy, _) ->
                                this.FilterWitnesses(ilTy, passedWitnesses)
                            | _ ->
                                ImArray.empty
                        vm.TryFindWitnessFunctionByAbstractFunction(realTy, enclosingTy, funcInst, possibleWitnesses)
                    | _ ->
                        None
                match witnessFuncOpt, witnessFuncOpt2 with
                | Some _, Some _ -> failwith "Ambiguous witness functions."
                | Some _, None -> witnessFuncOpt
                | None, Some _ -> witnessFuncOpt2
                | _ -> None

            let func =
                // Precedence
                // 1. Witness Type
                // 2. Concrete Type
                // 3. Abstract Type
                match witnessFuncOpt with
                | Some witnessFunc -> witnessFunc
                | _ ->
                    match enclosing with
                    | RuntimeEnclosing.Witness(realTy, enclosingTy, _) ->
                        let funcs = findFuncs realTy

                        if funcs.IsEmpty then
                            if enclosingTy.IsShape then
                                let rec find (ty: RuntimeType) =
                                    let funcs =
                                        ty.Extends
                                        |> ImArray.map (fun x ->
                                            findFuncs x
                                        )
                                        |> ImArray.concat
                                    if funcs.IsEmpty then
                                        ty.Extends
                                        |> ImArray.map (fun x -> find x)
                                        |> ImArray.concat
                                    else
                                        funcs
                                let funcs = find realTy
                                if funcs.IsEmpty then
                                    // Error since we did not find the function.
                                    // Below is just for printing, no actual necessary logic.
                                    let tyArgsText =
                                        if funcInst.TypeArguments.IsEmpty then
                                            ""
                                        elif funcInst.TypeArguments.Length = 1 then
                                            $"<{funcInst.TypeArguments[0].Name}>"
                                        else
                                            "<" + (funcInst.TypeArguments |> ImArray.map (fun x -> x.Name) |> ImArray.reduce (fun x y -> x + ", " + y)) + ">"
                                    let parsText =
                                        if funcInst.Parameters.IsEmpty then
                                            $"(): {funcInst.ReturnType.Name}"
                                        elif funcInst.Parameters.Length = 1 then
                                            $"({funcInst.Parameters[0].Type.Name}): {funcInst.ReturnType.Name}"
                                        else
                                            "(" + (funcInst.Parameters |> ImArray.map (fun x -> x.Type.Name) |> ImArray.reduce (fun x y -> x + ", " + y)) + $"): {funcInst.ReturnType.Name}"

                                    failwith $"Function not found: {funcInst.Name}{tyArgsText}{parsText}\nReal Type: {realTy.Name}\nEnclosing Type: {enclosingTy.Name}"                                  
                                if funcs.Length > 1 then
                                    failwith "Too many functions"
                                funcs.[0]
                            else
                                funcInst
                        else
                            if funcs.Length > 1 then
                                failwith "Too many functions"
                            funcs.[0]
                    | _ ->
                        funcInst

            func |> setWitnessesToFunction passedAndFilteredWitnesses genericContext

    member _.ResolveFunction(ilAsm, ilFuncSpec, enclosing, funcTyArgs, genericContext) =
        resolveFunction ilAsm ilFuncSpec enclosing funcTyArgs genericContext

    member this.ResolveFunction(ilAsm: OlyILReadOnlyAssembly, ilFuncRef: OlyILFunctionReference, genericContext: GenericContext) : RuntimeFunction =
        let enclosingTy = this.ResolveType(ilAsm, ilFuncRef.GetEnclosingType(), genericContext)
        let ilFuncSpecHandle = ilFuncRef.SpecificationHandle
        let enclosingTyParCount = enclosingTy.TypeArguments.Length

        let ilFuncSpec = ilAsm.GetFunctionSpecification(ilFuncSpecHandle)
        let name = ilAsm.GetStringOrEmpty(ilFuncSpec.NameHandle)

        let funcTyArgs =
            ilFuncSpec.TypeParameters
            |> ImArray.mapi (fun i _ ->
                RuntimeType.Variable(i, OlyILTypeVariableKind.Function)
            )
        let funcs = this.FindImmediatePossiblyOverridenFunctionDefinitions(enclosingTyParCount, ilAsm, ilFuncSpecHandle, enclosingTy, funcTyArgs, GenericContext.Default)

        if funcs.IsEmpty then
            failwith $"Function '{name}' not found."
        elif funcs.Length > 1 then
            failwith $"Multiple functions of '{name}' are found."
        else
            funcs.[0].MakeReference(enclosingTy)

    member private this.AreFunctionSpecificationParametersEqual(enclosingTyParCount1: int, ilAsm1: OlyILReadOnlyAssembly, ilFuncSpec1: OlyILFunctionSpecification, scopeTyArgs1: GenericContext, enclosingTyParCount2: int, ilAsm2: OlyILReadOnlyAssembly, ilFuncSpec2: OlyILFunctionSpecification, scopeTyArgs2: GenericContext) =
        let returnTy1 = this.ResolveType(ilAsm1, ilFuncSpec1.ReturnType, scopeTyArgs1)
        let returnTy2 = this.ResolveType(ilAsm2, ilFuncSpec2.ReturnType, scopeTyArgs2)
        if returnTy1 = returnTy2 then
            (ilFuncSpec1.Parameters, ilFuncSpec2.Parameters)
            ||> ImArray.forall2 (fun x1 x2 ->
                this.ResolveType(ilAsm1, x1.Type, scopeTyArgs1) = this.ResolveType(ilAsm2, x2.Type, scopeTyArgs2)
            )
        else
            false

    member private this.AreFunctionSpecificationsEqual(enclosingTyParCount1: int, ilAsm1: OlyILReadOnlyAssembly, ilFuncSpec1: OlyILFunctionSpecification, scopeTyArgs1: GenericContext, enclosingTyParCount2: int, ilAsm2: OlyILReadOnlyAssembly, ilFuncSpec2: OlyILFunctionSpecification, scopeTyArgs2: GenericContext) =
        if obj.ReferenceEquals(ilFuncSpec1, ilFuncSpec2) then
            if enclosingTyParCount1 = 0 && ilFuncSpec1.TypeParameters.IsEmpty then
                OlyAssert.Equal(0, enclosingTyParCount2)
                true
            else
                this.AreFunctionSpecificationParametersEqual(enclosingTyParCount1, ilAsm1, ilFuncSpec1, scopeTyArgs1, enclosingTyParCount2, ilAsm2, ilFuncSpec2, scopeTyArgs2)
        else
            ilFuncSpec1.IsInstance = ilFuncSpec2.IsInstance &&
            ilFuncSpec1.Parameters.Length = ilFuncSpec2.Parameters.Length &&
            ilFuncSpec1.TypeParameters.Length = ilFuncSpec2.TypeParameters.Length &&
            (
                let name1 = ilAsm1.GetStringOrEmpty(ilFuncSpec1.NameHandle)
                let name2 = ilAsm2.GetStringOrEmpty(ilFuncSpec2.NameHandle)
                if name1 = name2 then
                    this.AreFunctionSpecificationParametersEqual(enclosingTyParCount1, ilAsm1, ilFuncSpec1, scopeTyArgs1, enclosingTyParCount2, ilAsm2, ilFuncSpec2, scopeTyArgs2)
                else
                    false
            )

    member private this.AreILEnclosingsEqual(ilAsm1: OlyILReadOnlyAssembly, ilEnclosing1: OlyILEnclosing, ilAsm2: OlyILReadOnlyAssembly, ilEnclosing2: OlyILEnclosing) =
        match ilEnclosing1, ilEnclosing2 with
        | OlyILEnclosing.Witness(ilTy1, _), OlyILEnclosing.Witness(ilTy2, _) ->
            let ty1 = this.ResolveType(ilAsm1, ilTy1, GenericContext.Default)
            let ty2 = this.ResolveType(ilAsm2, ilTy2, GenericContext.Default)
            ty1 = ty2
        | OlyILEnclosing.Namespace(path1, _), OlyILEnclosing.Namespace(path2, _) ->
            if path1.Length = path2.Length then
                (path1, path2)
                ||> ImArray.forall2 (fun handle1 handle2 ->
                    (ilAsm1.GetStringOrEmpty(handle1)).Equals(ilAsm2.GetStringOrEmpty(handle2))
                )
            else
                false
        | OlyILEnclosing.Entity(ilEntInst1), OlyILEnclosing.Entity(ilEntInst2) ->
            match ilEntInst1, ilEntInst2 with
            | OlyILEntityInstance(handle1, ilTyArgs1), OlyILEntityInstance(handle2, ilTyArgs2) ->
                if ilTyArgs1.Length = ilTyArgs2.Length then
                    let name1 = 
                        let nameHandle =
                            if handle1.Kind = OlyILTableKind.EntityDefinition then
                                ilAsm1.GetEntityDefinition(handle1).NameHandle
                            else
                                ilAsm1.GetEntityReference(handle1).NameHandle
                        ilAsm1.GetStringOrEmpty(nameHandle)
                    let name2 = 
                        let nameHandle =
                            if handle2.Kind = OlyILTableKind.EntityDefinition then
                                ilAsm2.GetEntityDefinition(handle2).NameHandle
                            else
                                ilAsm2.GetEntityReference(handle2).NameHandle
                        ilAsm2.GetStringOrEmpty(nameHandle)
                    if name1 = name2 then
                        let ilEnclosing1 = getEnclosingOfILEntityInstance ilAsm1 ilEntInst1
                        let ilEnclosing2 = getEnclosingOfILEntityInstance ilAsm2 ilEntInst2
                        if this.AreILEnclosingsEqual(ilAsm1, ilEnclosing1, ilAsm2, ilEnclosing2) then
                            (ilTyArgs1, ilTyArgs2)
                            ||> ImArray.forall2 (fun ilTy1 ilTy2 ->
                                let ty1 = this.ResolveType(ilAsm1, ilTy1, GenericContext.Default)
                                let ty2 = this.ResolveType(ilAsm2, ilTy2, GenericContext.Default)
                                ty1 = ty2
                            )
                        else
                            false
                    else
                        false
                else
                    false

            | OlyILEntityConstructor(handle1), OlyILEntityConstructor(handle2) ->
                let name1 = 
                    let nameHandle =
                        if handle1.Kind = OlyILTableKind.EntityDefinition then
                            ilAsm1.GetEntityDefinition(handle1).NameHandle
                        else
                            ilAsm1.GetEntityReference(handle1).NameHandle
                    ilAsm1.GetStringOrEmpty(nameHandle)
                let name2 = 
                    let nameHandle =
                        if handle2.Kind = OlyILTableKind.EntityDefinition then
                            ilAsm2.GetEntityDefinition(handle2).NameHandle
                        else
                            ilAsm2.GetEntityReference(handle2).NameHandle
                    ilAsm2.GetStringOrEmpty(nameHandle)
                if name1 = name2 then
                    let ilEnclosing1 = getEnclosingOfILEntityInstance ilAsm1 ilEntInst1
                    let ilEnclosing2 = getEnclosingOfILEntityInstance ilAsm2 ilEntInst2
                    if this.AreILEnclosingsEqual(ilAsm1, ilEnclosing1, ilAsm2, ilEnclosing2) then
                        true
                    else
                        false
                else
                    false

            | _ ->
                false
                
        | _ ->
            false

    member this.TryFindPossibleWitness(ty: RuntimeType, abstractTy: RuntimeType, witnesses: RuntimeWitness imarray) =
        let possibleWitnesses =
            witnesses
            |> ImArray.choose (fun x -> 
                if x.TypeExtension.IsTypeExtension then
                    if x.AbstractFunction.IsSome then None
                    else

                    let extendsTy = x.TypeExtension.Extends[0]
                    if subsumesType extendsTy.Formal ty.Formal && subsumesType abstractTy x.TypeExtension then
                        // TODO: Checking the formals is a hack.
                        //    The reason why is because the 'ty' could be a type constructor where as the 'inherit ty' is never a type constructor.
                        if subsumesType extendsTy ty || (extendsTy.Formal = ty && ty.IsTypeConstructor) then
                            Some x
                        else
                            if ty.IsTypeConstructor then
                                failwith "Unexpected type constructor."
                            if not x.Type.IsTypeConstructor then
                                failwith "Expected type constructor."
                            if not x.TypeExtension.IsTypeConstructor then
                                failwith "Expected type constructor for type extension."

                            // The type extension itself is a type constructor and the
                            // concrete type is not - therefore, apply the type arguments
                            // to instantiate the proper witness.
                            RuntimeWitness(
                                x.TypeVariableIndex,
                                x.TypeVariableKind,
                                x.Type.Apply(ty.TypeArguments),
                                x.TypeExtension.Apply(ty.TypeArguments),
                                None
                            )
                            |> Some
                    else
                        None
                else
                    failwith "Expected type extension."
            )
        if possibleWitnesses.IsEmpty then
            None
        elif possibleWitnesses.Length > 1 then
            failwith "Ambiguous witnesses found."
        else
            Some possibleWitnesses[0]

    member this.TryFindWitnessFunction(enclosing: RuntimeEnclosing, func: RuntimeFunction, passedWitnesses: RuntimeWitness imarray) =
        trySolveWitness enclosing func passedWitnesses

    member this.TryFindWitnessFunctionByAbstractFunction(ty: RuntimeType, abstractTy: RuntimeType, abstractFunc: RuntimeFunction, passedWitnesses: RuntimeWitness imarray) =
        let foundFuncOpt =
            let possibleFuncs =
                passedWitnesses
                |> ImArray.choose (fun x ->
                    // If the enclosing type and abstract type are shapes, then we should not do equality on them. This allows cross-assembly shape definitions to work as
                    // the function signature is really the only one that matters here.
                    match x.AbstractFunction with
                    | Some possibleFunc when x.Type = ty && ((possibleFunc.EnclosingType = abstractTy) || (possibleFunc.EnclosingType.IsShape && abstractTy.IsShape)) ->
                        if (abstractFunc.Formal = possibleFunc.Formal) || (possibleFunc.EnclosingType.IsShape && abstractTy.IsShape && 
                                                                           possibleFunc.Name = abstractFunc.Name && 
                                                                           possibleFunc.TypeParameters.Length = abstractFunc.TypeParameters.Length) then
                            let formalFuncs = 
                                findFunctionsByTypeAndFunctionSignature x.TypeExtension abstractFunc 
                                |> ImArray.map (fun x -> x.Formal)
                            formalFuncs
                            |> ImArray.map (fun formalFunc ->
                                let enclosingTy = formalFunc.EnclosingType
                                formalFunc.MakeInstance(enclosingTy.Apply(x.TypeExtension.TypeArguments), abstractFunc.TypeArguments)
                            )
                            |> Some
                        else
                            None
                    | _ ->
                        None
                )
                |> ImArray.concat

            if possibleFuncs.IsEmpty then
                None
            elif possibleFuncs.Length = 1 then
                Some possibleFuncs[0]
            else
                failwith "Ambiguous specific abstract functions found."

        foundFuncOpt

    member this.FilterWitnesses(ilTy: OlyILType, witnesses: RuntimeWitness imarray) =
        witnesses
        |> ImArray.filter (fun witness ->
            match ilTy with
            | OlyILTypeVariable(tyVarIndex, tyVarKind)
            | OlyILTypeHigherVariable(tyVarIndex, _, tyVarKind) ->
                witness.TypeVariableIndex = tyVarIndex &&
                (
                    match witness.TypeVariableKind, tyVarKind with
                    | OlyILTypeVariableKind.Type, OlyILTypeVariableKind.Type
                    | OlyILTypeVariableKind.Function, OlyILTypeVariableKind.Function -> true
                    | _ -> false
                )
            | _ ->
                false
        )

    member this.ResolveEnclosing(ilAsm: OlyILReadOnlyAssembly, ilEnclosing: OlyILEnclosing, genericContext: GenericContext, witnesses: RuntimeWitness imarray) : RuntimeEnclosing =
        match ilEnclosing with
        | OlyILEnclosing.Entity(ilEntInst) ->
            let ty = this.ResolveType(ilAsm, ilEntInst.AsType, genericContext)
            RuntimeEnclosing.Type ty

        | OlyILEnclosing.Namespace(path, _) ->
            RuntimeEnclosing.Namespace(path |> ImArray.map ilAsm.GetStringOrEmpty)

        | OlyILEnclosing.Witness(ilTy, ilEntInst) ->
            let possibleWitnesses = this.FilterWitnesses(ilTy, witnesses)
            let ty = this.ResolveType(ilAsm, ilTy, genericContext)
            let abstractTy = this.ResolveType(ilAsm, ilEntInst.AsType, genericContext)
            let witnessOpt = this.TryFindPossibleWitness(ty, abstractTy, possibleWitnesses)
            RuntimeEnclosing.Witness(ty, abstractTy, witnessOpt)

    member this.ResolveTypeDefinition(ilAsm: OlyILReadOnlyAssembly, ilEntDefOrRefHandle: OlyILEntityDefinitionOrReferenceHandle) : RuntimeType =
        this.ResolveTypeDefinitionAux(ilAsm, ilEntDefOrRefHandle)  

    member this.ResolveTypeDefinitionAux(ilAsm: OlyILReadOnlyAssembly, ilEntDefOrRefHandle: OlyILEntityDefinitionOrReferenceHandle) : RuntimeType =
        let asm = assemblies.[ilAsm.Identity]

        let isAnonymousShape (ilEntDef: OlyILEntityDefinition) =
            ilEntDef.Kind = OlyILEntityKind.Shape && 
            ilEntDef.NameHandle.IsNil

        let verify (ilEntDef: OlyILEntityDefinition) =
            if isAnonymousShape ilEntDef then
                match ilEntDef.Enclosing with
                | OlyILEnclosing.Namespace(path, _) ->
                    if not path.IsEmpty then
                        OlyAssert.Fail("Invalid anonymous shape.")
                | _ ->
                    OlyAssert.Fail("Invalid anonymous shape.")

        if ilEntDefOrRefHandle.Kind = OlyILTableKind.EntityDefinition then
            match asm.EntityDefinitionCache.TryGetValue ilEntDefOrRefHandle with
            | true, (res, _) -> res
            | _ ->
                let ilEntDef = ilAsm.GetEntityDefinition(ilEntDefOrRefHandle)
                verify ilEntDef               

                let enclosing = this.ResolveEnclosing(ilAsm, ilEntDef.Enclosing, GenericContext.Default, ImArray.empty)

                let enclosingTyPars = enclosing.TypeParameters

                let tyPars =
                    ilEntDef.TypeParameters
                    |> ImArray.map (fun ilTyPar -> 
                        { 
                            Name = ilAsm.GetStringOrEmpty(ilTyPar.NameHandle)
                            Arity = ilTyPar.Arity
                            IsVariadic = ilTyPar.IsVariadic
                            ILConstraints = ilTyPar.Constraints
                            ConstraintSubtypes = Lazy<_>.CreateFromValue(ImArray.empty)
                            ConstraintTraits = Lazy<_>.CreateFromValue(ImArray.empty)
                        } : RuntimeTypeParameter
                    )

                let fullTyPars = enclosingTyPars.AddRange(tyPars)

                let fullTyArgs =
                    fullTyPars
                    |> ImArray.mapi (fun i _ -> 
                        RuntimeType.Variable(i, OlyILTypeVariableKind.Type)
                    )

                let ilPropDefLookup =
                    // TODO: Use a builder instead.
                    (ImmutableDictionary.Empty, ilEntDef.PropertyDefinitionHandles)
                    ||> ImArray.fold (fun ilPropDefLookup ilPropDefHandle ->
                        let ilPropDef = ilAsm.GetPropertyDefinition(ilPropDefHandle)
                        let ilPropDefLookup =
                            if ilPropDef.Getter.IsNil then
                                ilPropDefLookup
                            else
                                ilPropDefLookup.Add(ilPropDef.Getter, ilPropDefHandle)
                        if ilPropDef.Setter.IsNil then
                            ilPropDefLookup
                        else
                            ilPropDefLookup.Add(ilPropDef.Setter, ilPropDefHandle)
                    )
                    

                let ent =
                    {
                        RuntimeEntity.Enclosing = enclosing
                        RuntimeEntity.TypeParameters = fullTyPars
                        RuntimeEntity.TypeArguments = fullTyArgs
                        RuntimeEntity.Witnesses = ImArray.empty
                        RuntimeEntity.ExtendsLazy = Lazy<_>.CreateFromValue(ImArray.empty)
                        RuntimeEntity.ImplementsLazy = Lazy<_>.CreateFromValue(ImArray.empty)
                        RuntimeEntity.RuntimeTypeLazy = Lazy<_>.CreateFromValue(None)
                        RuntimeEntity.FieldsLazy = Lazy<_>.CreateFromValue(ImArray.empty)
                        RuntimeEntity.AsType = Unchecked.defaultof<_>

                        RuntimeEntity.Info =
                            {
                                RuntimeEntityInfo.Name = ilAsm.GetStringOrEmpty(ilEntDef.NameHandle)
                                RuntimeEntityInfo.ILAssembly = ilAsm
                                RuntimeEntityInfo.ILEntityDefinitionHandle = ilEntDefOrRefHandle
                                RuntimeEntityInfo.ILEntityKind = ilEntDef.Kind
                                RuntimeEntityInfo.ILEntityFlags = ilEntDef.Flags
                                RuntimeEntityInfo.ILPropertyDefinitionLookup = ilPropDefLookup

                                RuntimeEntityInfo.Flags = RuntimeEntityFlags.None
                                RuntimeEntityInfo.Formal = Unchecked.defaultof<_>
                                RuntimeEntityInfo.Attributes = ImArray.empty
                                RuntimeEntityInfo.StaticConstructor = None
                            }
                    }
                ent.Info.Formal <- ent
                let ty = RuntimeType.Entity(ent)
                ent.AsType <- ty
                asm.EntityDefinitionCache.[ilEntDefOrRefHandle] <- (ty, RuntimeEntityDefinitionTypeArgumentWitnessListTable())

                let runtimeTyOpt =
                    if ent.IsEnumOrNewtype then
                        let ilFieldDefHandles = ilEntDef.FieldDefinitionHandles
                        if ilFieldDefHandles.IsEmpty then
                            failwith "Enum is missing its principal field."
                        else
                            let ilFieldDef = ilAsm.GetFieldDefinition(ilFieldDefHandles[0])
                            if ilFieldDef.MemberFlags.HasFlag(OlyILMemberFlags.Static) then
                                failwith "Enum is missing its principal field."
                            Some(this.ResolveType(ilAsm, ilFieldDef.Type, GenericContext.Default))
                    else
                        None

                // Set RuntimeType first before Extends and Implements.
                ent.RuntimeTypeLazy <- Lazy<_>.CreateFromValue(runtimeTyOpt)

                let extends =
                    ilEntDef.Extends
                    |> ImArray.map (fun x -> this.ResolveType(ilAsm, x, GenericContext.Default))

                let extends =
                    if extends.IsEmpty then
                        if ent.IsClass then
                            ImArray.createOne RuntimeType.BaseObject
                        elif ent.IsEnum then
                            if ent.IsAnyStruct then
                                ImArray.empty
                            else
                                raise(NotSupportedException("Enum non-struct runtime type."))
                        elif ent.IsAnyStruct then
                            ImArray.empty
                        else
                            extends
                    else
                        extends

                let implements =
                    ilEntDef.Implements
                    |> ImArray.map (fun x -> this.ResolveType(ilAsm, x, GenericContext.Default))

                ent.ExtendsLazy <- Lazy<_>.CreateFromValue(extends)
                ent.ImplementsLazy <- Lazy<_>.CreateFromValue(implements)

                let fields =
                    ilEntDef.FieldDefinitionHandles
                    |> ImArray.mapi (fun i ilFieldDefHandle ->
                        let field = this.ResolveField(RuntimeType.Entity(ent), ilAsm, i, ilFieldDefHandle)
#if DEBUG || CHECKED
                        match field.Type with
                        | RuntimeType.Variable(_, OlyILTypeVariableKind.Function)
                        | RuntimeType.HigherVariable(_, _, OlyILTypeVariableKind.Function) ->
                            failwith "Not a valid field type as it contains function type variables."
                        | _ ->
                            ()
                        field.Type.TypeArguments
                        |> ImArray.iter (fun tyArg ->
                            match tyArg with
                            | RuntimeType.Variable(_, OlyILTypeVariableKind.Function)
                            | RuntimeType.HigherVariable(_, _, OlyILTypeVariableKind.Function) ->
                                failwith "Not a valid field type as it contains function type variables as one of its type arguments."
                            | _ ->
                                ()
                        )
#endif
                        field
                    )

                ent.FieldsLazy <- Lazy<_>.CreateFromValue(fields)

                let mutable entFlags = RuntimeEntityFlags.None

                let attrs =
                    ilEntDef.Attributes
                    |> ImArray.choose (fun x -> 
                        match x with
                        | OlyILAttribute.Intrinsic _ ->
                            entFlags <- entFlags ||| RuntimeEntityFlags.Intrinsic
                            None
                        | _ ->
                            this.TryResolveConstructorAttribute(ilAsm, x, GenericContext.Default, ImArray.empty)
                    )

                ent.Info.Flags <- entFlags
                ent.Info.Attributes <- attrs

                let staticCtorOpt =
                    ilEntDef.FunctionHandles
                    |> ImArray.tryPick (fun x ->
                        let ilFuncDef = ilAsm.GetFunctionDefinition(x)
                        if ilFuncDef.Flags.HasFlag(OlyILFunctionFlags.Constructor) && 
                           ilFuncDef.MemberFlags.HasFlag(OlyILMemberFlags.Static) then
                            let func = this.ResolveFunctionDefinition(ty.Formal, x)     

                            assert(func.TypeParameters.IsEmpty)
                            assert(func.Parameters.IsEmpty)
                            assert(func.ReturnType = RuntimeType.Void)

                            Some func
                        else
                            None
                    )

                ent.Info.StaticConstructor <- staticCtorOpt

                tyPars
                |> ImArray.iter (fun tyPar ->
                    let constrSubtypes = 
                        tyPar.ILConstraints
                        |> ImArray.choose (fun ilConstr ->
                            match ilConstr with
                            | OlyILConstraint.SubtypeOf(ilTy) ->
                                this.ResolveType(ilAsm, ilTy, GenericContext.Default)
                                |> Some
                            | _ ->
                                None
                        )
                    let constrTraits = 
                        tyPar.ILConstraints
                        |> ImArray.choose (fun ilConstr ->
                            match ilConstr with
                            | OlyILConstraint.TraitType(ilTy) ->
                                this.ResolveType(ilAsm, ilTy, GenericContext.Default)
                                |> Some
                            | _ ->
                                None
                        )
                    tyPar.ConstraintSubtypes <- Lazy<_>.CreateFromValue(constrSubtypes)
                    tyPar.ConstraintTraits <- Lazy<_>.CreateFromValue(constrTraits)
                )

                ty
        else
            match asm.entRefCache.TryGetValue(ilEntDefOrRefHandle) with
            | true, res -> res
            | _ ->
                let ilEntRef = ilAsm.GetEntityReference(ilEntDefOrRefHandle)
                let name = ilAsm.GetStringOrEmpty(ilEntRef.NameHandle)
                let arity = ilEntRef.Enclosing.TypeArgumentCount + ilEntRef.TypeParameterCount

                let identity = ilAsm.GetAssemblyIdentity(ilEntRef)

                let tys = this.FindTypes(ilAsm, ilEntRef.Enclosing, identity, name, arity)
 
                // TODO: Include enclosing names for the errors.
                if tys.Length = 0 then
                    failwithf "Unable to find type: %s - identity: %s" name (identity.Name)
                elif tys.Length > 1 then
                    failwithf "Multiple types loaded: %s" name
                else
                    let ty = tys.[0]
                    asm.entRefCache.[ilEntDefOrRefHandle] <- ty
                    ty

    member this.ResolveType(ilAsm: OlyILReadOnlyAssembly, ilTy: OlyILType, genericContext: GenericContext) : RuntimeType =
        match ilTy with
        | OlyILTypeVariable(index, ilKind) when genericContext.CanErase(index, ilKind) ->
            genericContext.GetErasedTypeArgument(index, ilKind)
        | OlyILTypeHigherVariable(index, ilTyArgs, ilKind) when genericContext.CanErase(index, ilKind) ->
            let tyArgs =
                ilTyArgs
                |> ImArray.map (fun x -> this.ResolveType(ilAsm, x, genericContext))

            let ty = genericContext.GetErasedTypeArgument(index, ilKind)
            if ty.IsTypeConstructor then
                let ty = ty.Apply(tyArgs)
                let asm = assemblies.[ty.AssemblyIdentity]
                asm.RuntimeTypeInstanceCache.GetOrCreate(ty.ILEntityDefinitionHandle, ty.TypeArguments)
            else
                failwith "Invalid type constructor."

        | OlyILTypeEntity(ilEntInst) ->
            match ilEntInst with
            | OlyILEntityInstance(ilEntDefOrRefHandle, ilTyArgs) ->
                let ty = this.ResolveTypeDefinition(ilAsm, ilEntDefOrRefHandle)
#if DEBUG || CHECKED
                OlyAssert.Equal(ty.TypeParameters.Length, ilTyArgs.Length)
#endif
                if ilTyArgs.IsEmpty then
                    ty
                else
                    let tyArgs = ilTyArgs |> ImArray.map (fun x -> this.ResolveType(ilAsm, x, genericContext))
                    let asm = assemblies.[ty.AssemblyIdentity]
                    asm.RuntimeTypeInstanceCache.GetOrCreate(ty.ILEntityDefinitionHandle, tyArgs).SetWitnesses(genericContext.PassedWitnesses)
            | OlyILEntityConstructor(ilEntDefOrRefHandle) ->
                this.ResolveTypeDefinition(ilAsm, ilEntDefOrRefHandle)

        | OlyILTypeForAll(ilTyPars, ilInnerTy) ->
            let tyPars =
                ilTyPars
                |> ImArray.map (fun ilTyPar ->
                    let constrSubtypes = 
                        ilTyPar.Constraints
                        |> ImArray.choose (fun ilConstr ->
                            match ilConstr with
                            | OlyILConstraint.SubtypeOf(ilTy) ->
                                this.ResolveType(ilAsm, ilTy, GenericContext.Default)
                                |> Some
                            | _ ->
                                None
                        )
                    let constrTraits = 
                        ilTyPar.Constraints
                        |> ImArray.choose (fun ilConstr ->
                            match ilConstr with
                            | OlyILConstraint.TraitType(ilTy) ->
                                this.ResolveType(ilAsm, ilTy, GenericContext.Default)
                                |> Some
                            | _ ->
                                None
                        )
                    { 
                        Name = ilAsm.GetStringOrEmpty(ilTyPar.NameHandle)
                        Arity = ilTyPar.Arity
                        IsVariadic = ilTyPar.IsVariadic
                        ILConstraints = ilTyPar.Constraints
                        ConstraintSubtypes = Lazy<_>.CreateFromValue(constrSubtypes)
                        ConstraintTraits = Lazy<_>.CreateFromValue(constrTraits)
                    } : RuntimeTypeParameter
                )
            let innerTy = this.ResolveType(ilAsm, ilInnerTy, GenericContext.Default)
            RuntimeType.ForAll(tyPars, innerTy)

        | _ ->
            match ilTy with
            | OlyILTypeDependentIndexer _ ->
                raise(System.NotImplementedException())

            | OlyILTypeInvalid _ ->
                OlyAssert.Fail("Invalid type.")

            | OlyILTypeModified _ ->
                OlyAssert.Fail("Invalid modified type.")

            | OlyILTypeConstantInt32(value) ->
                RuntimeType.ConstantInt32(value)

            | OlyILTypeBaseObject -> RuntimeType.BaseObject
            | OlyILTypeUInt8 -> RuntimeType.UInt8
            | OlyILTypeInt8 -> RuntimeType.Int8
            | OlyILTypeUInt16 -> RuntimeType.UInt16
            | OlyILTypeInt16 -> RuntimeType.Int16
            | OlyILTypeUInt32 -> RuntimeType.UInt32
            | OlyILTypeInt32 -> RuntimeType.Int32
            | OlyILTypeUInt64 -> RuntimeType.UInt64
            | OlyILTypeInt64 -> RuntimeType.Int64
            | OlyILTypeFloat32 -> RuntimeType.Float32
            | OlyILTypeFloat64 -> RuntimeType.Float64
            | OlyILTypeBool -> RuntimeType.Bool
            | OlyILTypeVoid -> RuntimeType.Void
            | OlyILTypeUnit -> RuntimeType.Unit
            | OlyILTypeChar16 -> RuntimeType.Char16
            | OlyILTypeUtf16 -> RuntimeType.Utf16
            | OlyILTypeNativeInt -> RuntimeType.NativeInt
            | OlyILTypeNativeUInt -> RuntimeType.NativeUInt
            | OlyILTypeNativePtr(ilElementTy) -> RuntimeType.NativePtr(this.ResolveType(ilAsm, ilElementTy, genericContext))
            | OlyILTypeNativeFunctionPtr(ilCc, ilArgTys, ilReturnTy) ->
                let argTys = this.ResolveTypes(ilAsm, ilArgTys, genericContext)
                let returnTy = this.ResolveType(ilAsm, ilReturnTy, genericContext)
                RuntimeType.NativeFunctionPtr(ilCc, argTys, returnTy)
            | OlyILTypeTuple(ilTyArgs, ilNameHandles) ->
                if ilTyArgs.Length < 2 then
                    OlyAssert.Fail("Invalid tuple type")
                let tyArgs =
                    ilTyArgs
                    |> ImArray.map (fun x -> this.ResolveType(ilAsm, x, genericContext))
                let names =
                    ilNameHandles
                    |> ImArray.map (fun x ->
                        ilAsm.GetStringOrEmpty(x)
                    )
                RuntimeType.Tuple(tyArgs, names)
            | OlyILTypeRefCell(ilElementTy) ->
                RuntimeType.ReferenceCell(this.ResolveType(ilAsm, ilElementTy, genericContext))
            | OlyILTypeArray(ilElementTy, rank, ilKind) ->
                let isMutable = ilKind = OlyILArrayKind.Mutable
                RuntimeType.Array(this.ResolveType(ilAsm, ilElementTy, genericContext), rank, isMutable)
            | OlyILTypeFunction(ilArgTys, ilReturnTy, ilKind) ->
                let argTys = this.ResolveTypes(ilAsm, ilArgTys, genericContext)
                let returnTy = this.ResolveType(ilAsm, ilReturnTy, genericContext)
                let kind =
                    match ilKind with
                    | OlyILFunctionKind.Normal ->
                        OlyIRFunctionKind.Normal
                    | OlyILFunctionKind.Scoped ->
                        OlyIRFunctionKind.Scoped
                RuntimeType.Function(argTys, returnTy, kind)

            | OlyILTypeVariable(index, ilKind) ->
                RuntimeType.Variable(index, ilKind)

            | OlyILTypeByRef(ilElementTy, ilKind) ->
                let kind =
                    match ilKind with
                    | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                    | OlyILByRefKind.ReadOnly -> OlyIRByRefKind.ReadOnly
                    | OlyILByRefKind.WriteOnly -> OlyIRByRefKind.WriteOnly
                let elementTy = this.ResolveType(ilAsm, ilElementTy, genericContext)
                if elementTy.IsByRef_t then
                    OlyAssert.Fail("Cannot have byref of a byref type.")
                RuntimeType.ByRef(elementTy, kind)

            | OlyILTypeHigherVariable(index, ilTyArgs, ilKind) ->
                let tyArgs =
                    ilTyArgs
                    |> ImArray.map (fun x -> this.ResolveType(ilAsm, x, genericContext))
                RuntimeType.HigherVariable(index, tyArgs, ilKind)

            | OlyILTypeEntity _
            | OlyILTypeForAll _ -> failwith "Invalid type."

    member this.ResolveTypes(ilAsm: OlyILReadOnlyAssembly, ilTys: OlyILType imarray, genericContext: GenericContext) : RuntimeType imarray =
        ilTys
        |> ImArray.map (fun ilTy -> this.ResolveType(ilAsm, ilTy, genericContext))

    member private this.FindTypes(ilAsmOrig: OlyILReadOnlyAssembly, ilEnclosing: OlyILEnclosing, ilAsmIdentity: OlyILAssemblyIdentity, name: string, arity: int) : RuntimeType imarray =
        let builder = ImArray.builder()

        let ilAsm = assemblies[ilAsmIdentity].ilAsm
        ilAsm.FindEntityDefinitions(name)
        |> ImArray.iter (fun handle ->
            let ilEntDef = ilAsm.GetEntityDefinition(handle)
            let name2 = ilAsm.GetStringOrEmpty(ilEntDef.NameHandle)
            if String.IsNullOrWhiteSpace name2 || name <> name2 then 
                ()
            else
                let tyParCount = ilEntDef.FullTypeParameterCount
                if tyParCount <> arity then 
                    ()
                elif this.AreILEnclosingsEqual(ilAsmOrig, ilEnclosing, ilAsm, ilEntDef.Enclosing) then
                    builder.Add(this.ResolveTypeDefinition(ilAsm, handle))
        )

        // If we did not find any type definitions,
        // then try to find a type reference.
        // This is effectively TYPE FORWARDING - bouncing from reference to reference in
        //     an effort to find a definition.
        if builder.Count = 0 then
            ilAsm.FindEntityReferences(name)
            |> ImArray.iter (fun handle ->
                let ilEntRef = ilAsm.GetEntityReference(handle)
                let name2 = ilAsm.GetStringOrEmpty(ilEntRef.NameHandle)
                if String.IsNullOrWhiteSpace name2 || name <> name2 then 
                    ()
                else
                    let tyParCount = ilEntRef.FullTypeParameterCount
                    if tyParCount <> arity then 
                        ()
                    elif this.AreILEnclosingsEqual(ilAsmOrig, ilEnclosing, ilAsm, ilEntRef.Enclosing) then
                        builder.Add(this.ResolveTypeDefinition(ilAsm, handle))
            )


        builder.ToImmutable()

    member private this.GetILFunctionDefinitionHandles(ty: RuntimeType) =
        let ty = ty.Formal
        let asm = assemblies.[ty.AssemblyIdentity]
        let ilEntDef = asm.ilAsm.GetEntityDefinition(ty.ILEntityDefinitionHandle)

        let enclosingTy = 
            let tyArgs = ImArray.init ilEntDef.FullTypeParameterCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))
            let ilEnclosing = OlyILEnclosing.Entity(OlyILEntityInstance(ty.ILEntityDefinitionHandle, tyArgs))
            (this.ResolveEnclosing(asm.ilAsm, ilEnclosing, GenericContext.Default, ImArray.empty)).AsType

        ilEntDef.FunctionHandles
        |> ImArray.map (fun x ->
            (enclosingTy, asm.ilAsm, x)
        )

    member private this.FindImmediateFunctionDefinitions(enclosingTyParCount1: int, ilAsm1: OlyILReadOnlyAssembly, ilFuncSpecHandle1: OlyILFunctionSpecificationHandle, enclosingTy: RuntimeType, funcTyArgs: RuntimeType imarray, genericContext: GenericContext) : RuntimeFunction imarray =
        let ilFuncSpec1 = ilAsm1.GetFunctionSpecification(ilFuncSpecHandle1)

        let ty = enclosingTy.Formal

        if ty.IsBuiltIn then ImArray.empty
        else

        let asm = assemblies.[ty.AssemblyIdentity]
        let ilEntDef = asm.ilAsm.GetEntityDefinition(ty.ILEntityDefinitionHandle)
        let enclosing = 
            let tyArgs = ImArray.init ilEntDef.FullTypeParameterCount (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))
            let ilEnclosing = OlyILEnclosing.Entity(OlyILEntityInstance(ty.ILEntityDefinitionHandle, tyArgs))
            this.ResolveEnclosing(asm.ilAsm, ilEnclosing, genericContext, ImArray.empty)

        let enclosingTy2 = enclosing.AsType

        let genericContext2 =
            if genericContext.IsErasing then
                GenericContext.CreateErasing(enclosingTy.TypeArguments.AddRange(funcTyArgs))
            else
                GenericContext.Create(enclosingTy.TypeArguments.AddRange(funcTyArgs))

        ilEntDef.FunctionHandles
        |> ImArray.choose (fun ilFuncDefHandle2 ->
            let ilFuncDef2 = asm.ilAsm.GetFunctionDefinition(ilFuncDefHandle2)
            let ilFuncSpec2 = asm.ilAsm.GetFunctionSpecification(ilFuncDef2.SpecificationHandle)
            let enclosingTyParCount2 = enclosingTy2.TypeArguments.Length
            if this.AreFunctionSpecificationsEqual(enclosingTyParCount1, ilAsm1, ilFuncSpec1, genericContext, enclosingTyParCount2, asm.ilAsm, ilFuncSpec2, genericContext2) then
                this.ResolveFunctionDefinition(enclosingTy.Formal, ilFuncDefHandle2)
                |> Some
            else
                None
        )

    member private this.FindImmediateOverridenFunctionDefinitions(enclosingTyParCount: int, ilAsm1: OlyILReadOnlyAssembly, ilFuncSpecHandle1: OlyILFunctionSpecificationHandle, enclosingTy: RuntimeType, funcTyArgs: RuntimeType imarray, genericContext: GenericContext) : RuntimeFunction imarray =
        let ilFuncDefHandles = this.GetILFunctionDefinitionHandles(enclosingTy)
        ilFuncDefHandles
        |> ImArray.choose (fun (enclosingTy, ilAsm2, ilFuncDefHandle2) ->
            let ilFuncDef = ilAsm2.GetFunctionDefinition(ilFuncDefHandle2)
            if ilFuncDef.MemberFlags &&& OlyILMemberFlags.Virtual = OlyILMemberFlags.Virtual then
                match ilFuncDef.Overrides with
                | Some(ilOverrides) ->
                    let overrides = this.ResolveFunction(ilAsm2, ilOverrides, genericContext)
                    let genericContext2 = genericContext.Set(overrides.EnclosingType.TypeArguments, funcTyArgs)
                    let ilFuncSpec2 = ilAsm2.GetFunctionSpecification(ilOverrides.SpecificationHandle)
                    if this.AreFunctionSpecificationsEqual(enclosingTyParCount, ilAsm1, ilAsm1.GetFunctionSpecification(ilFuncSpecHandle1), genericContext, enclosingTyParCount, ilAsm2, ilFuncSpec2, genericContext2) then
                        this.ResolveFunctionDefinition(enclosingTy.Formal, ilFuncDefHandle2)
                        |> Some
                    else
                        None
                | _ ->
                    // No explicit override found, so we must find it by exact signature.
                    let genericContext2 = genericContext.Set(enclosingTy.TypeArguments, funcTyArgs)
                    let ilFuncSpec2 = ilAsm2.GetFunctionSpecification(ilFuncDef.SpecificationHandle)
                    if this.AreFunctionSpecificationsEqual(enclosingTyParCount, ilAsm1, ilAsm1.GetFunctionSpecification(ilFuncSpecHandle1), genericContext, enclosingTyParCount, ilAsm2, ilFuncSpec2, genericContext2) then
                        this.ResolveFunctionDefinition(enclosingTy.Formal, ilFuncDefHandle2)
                        |> Some
                    else
                        None
            else
                None
        )

    member this.FindImmediatePossiblyOverridenFunctionDefinitions(enclosingTyParCount, ilAsm: OlyILReadOnlyAssembly, ilFuncSpecHandle, enclosingTy: RuntimeType, funcTyArgs, genericContext) : RuntimeFunction imarray =
        if enclosingTy.IsBuiltIn then ImArray.empty
        else
            // TODO: This is inefficient as we are going through function definitions twice.
            //       We should combine them, or at least just merge it all into 'FindImmediateFunctionDefinitions'.
            let funcs = this.FindImmediateFunctionDefinitions(enclosingTyParCount, ilAsm, ilFuncSpecHandle, enclosingTy, funcTyArgs, genericContext)
            if funcs.IsEmpty then
                // Do this as the possibility of an overriding function that were trying to find has a different name in
                // the concrete implementation.
                this.FindImmediateOverridenFunctionDefinitions(enclosingTyParCount, ilAsm, ilFuncSpecHandle, enclosingTy, funcTyArgs, genericContext)
            else
                funcs

    member this.FindMostSpecificFunctionsInHierarchy(enclosingTyParCount, ilAsm: OlyILReadOnlyAssembly, ilFuncSpecHandle, enclosingTy: RuntimeType, funcTyArgs, genericContext) : RuntimeFunction imarray =
        let findImmediate enclosingTy =            
            this.FindImmediatePossiblyOverridenFunctionDefinitions(enclosingTyParCount, ilAsm, ilFuncSpecHandle, enclosingTy, funcTyArgs, genericContext)

        let rec find (enclosingTy: RuntimeType) (mostSpecificFuncs: (_ * RuntimeFunction) imarray) =
            let enclosingTy =
                if enclosingTy.IsPrimitive then
                    match tryGetPrimitiveType enclosingTy with
                    | ValueSome ty -> ty
                    | _ -> enclosingTy
                else
                    enclosingTy

            if enclosingTy.IsPrimitive then mostSpecificFuncs
            else

            let funcs = 
                findImmediate enclosingTy
                |> ImArray.filter (fun func1 ->
                    let isOverriden =
                        mostSpecificFuncs
                        |> ImArray.exists (fun (_, func2) ->
                            match func2.Overrides with
                            | Some overrides -> 
                                overrides.Name = func1.Name &&
                                overrides.TypeParameters.Length = func1.TypeParameters.Length &&
                                overrides.Parameters.Length = func1.Parameters.Length &&
                                overrides.ReturnType = func1.ReturnType &&
                                (
                                    (overrides.Parameters, func1.Parameters)
                                    ||> ImArray.forall2 (fun par1 par2 ->
                                        par1.Type = par2.Type
                                    )
                                )
                            | _ ->
                                func2.Flags.IsVirtual &&
                                func2.Name = func1.Name &&
                                func2.TypeParameters.Length = func1.TypeParameters.Length &&
                                func2.Parameters.Length = func1.Parameters.Length &&
                                func2.ReturnType = func1.ReturnType &&
                                (
                                    (func2.Parameters, func1.Parameters)
                                    ||> ImArray.forall2 (fun par1 par2 ->
                                        par1.Type = par2.Type
                                    )
                                )
                        )
                    if isOverriden then
                        false
                    else
                        true
                )
                |> ImArray.map (fun func -> (enclosingTy, func))

            if enclosingTy.Extends.IsEmpty || 
               // This prevents an infinite loop with a non-alias intrinsic type.
               // The infinite loop occurs because when finding a function of a Built-In type,
               // it will find a non-alias intrinsic type who inherits the Built-In type.
               // Therefore, we need to stop searching the hierarchy.
               (not enclosingTy.IsAlias && enclosingTy.IsIntrinsic) then
                mostSpecificFuncs.AddRange(funcs)
            else
                find enclosingTy.Extends.[0] (mostSpecificFuncs.AddRange(funcs))
            
        let funcs = find enclosingTy ImArray.empty

        funcs
        |> ImArray.map (fun (_, func) -> func)

//****************************************************************************************************************************************************************************************************************

    member runtime.EmitField(field: RuntimeField) : 'Field =
        emitField field

    member runtime.EmitType(ty: RuntimeType) : 'Type =
        emitType false ty

    member runtime.EmitTypeArgument(ty: RuntimeType) : 'Type =
        emitType true ty

    member private vm.EmitFunctionDefinition(enclosingTy: RuntimeType, func: RuntimeFunction, genericContext: GenericContext) =
        let witnesses = func.Witnesses
        let funcTyArgs = func.TypeArguments
        let asm = assemblies.[func.AssemblyIdentity]

        if not genericContext.IsErasingType && not enclosingTy.IsFormal then
            failwith "Expected formal enclosing type."

        if func.IsExternal && genericContext.IsErasing then
            failwith "Expected zero erasing type arguments for external function."

        let isErasingFunc = genericContext.IsErasingFunction

        if isErasingFunc && (enclosingTy.TypeArguments.Length + func.TypeParameters.Length <> genericContext.Length) then
            failwith "Invalid number of type arguments for function."

        if not isErasingFunc && not witnesses.IsEmpty then
            failwith "Witnesses found for non-erased function."

        if not isErasingFunc && not func.IsFormal then
            failwith "Expected formal function."

        if isErasingFunc && func.IsFormal && ((not func.TypeArguments.IsEmpty) || (not func.TypeParameters.IsEmpty)) then
            failwith "Unexpected formal function."

        if not isErasingFunc && func.Kind = RuntimeFunctionKind.Instance then
            failwith "Unexpected function instance."

        match func.Overrides with
        | Some(overrides) when overrides.Flags.IsInstance <> func.Flags.IsInstance ->
            failwith $"Function '{func.Name}' has an invalid override."
        | _ ->
            ()

        if func.Flags.IsAbstract && func.HasILFunctionBody then
            failwith $"Function '{func.Name}' is abstract and cannot have an implementation."

        if enclosingTy.IsInterface && func.Flags.IsInstance && func.Overrides.IsSome && not func.Flags.IsAbstract && not func.Flags.IsFinal then
            failwith $"Function '{func.Name}' must be final."

        // TODO: Uncomment this when we start to figure out the rules.
        //if func.Flags.IsStatic && func.Overrides.IsSome then
        //    if func.Flags.IsVirtual then
        //        failwith $"Static function '{func.Name}' cannot be virtual."
        //    if func.Flags.IsFinal then
        //        failwith $"Static function '{func.Name}' cannot be final."

        let emitted = asm.GetEmittedFunctionDefinition(func)
        let key = struct(isErasingFunc, enclosingTy, funcTyArgs, witnesses, true)
        match emitted.TryGetValue(key) with
        | ValueSome(emittedFunc) -> 
            emittedFunc
        | _ ->
            if func.EnclosingType.IsEnum && func.Flags.IsInstance then
                failwith "Instance member functions on an 'enum' are not allowed."

            let enclosingTyParCount = enclosingTy.TypeArguments.Length

            let ilAsm = asm.ilAsm

            let tyPars = 
                if isErasingFunc then
                    ImArray.empty
                else
                    func.TypeParameters 
                    |> ImArray.map (fun tyPar -> 
                        OlyIRTypeParameter(tyPar.Name, emitConstraints ilAsm tyPar.ILConstraints genericContext)
                    )

            let pars = 
                func.Parameters 
                |> ImArray.map (fun par -> 
                    let attrs =
                        if par.Attributes.IsEmpty then
                            Lazy<_>.CreateFromValue(ImArray.empty)
                        else
                            lazy
                                emitAttributes ilAsm par.Attributes
                    OlyIRParameter(attrs, par.Name, this.EmitType(par.Type), true)
                )

            let overrides =
                func.Overrides 
                |> Option.map (fun x -> 
                    // We must pass witnesses to the overriden function.
                    if x.EnclosingType.CanGenericsBeErased && isErasingFunc then
                        let enclosingTy = 
                            x.EnclosingType.Substitute(genericContext)
                        let genericContext = GenericContext.Create(enclosingTy.TypeArguments, funcTyArgs)
                        let funcTyArgs =
                            x.TypeArguments
                            |> ImArray.map (fun x -> 
                                x.Substitute(genericContext)
                            )
                        this.EmitFunction(x.Formal.MakeInstance(enclosingTy, funcTyArgs) |> setWitnessesToFunction witnesses genericContext)
                    else
                        // We should not have witnesses to pass here.
                        if not witnesses.IsEmpty then
                            OlyAssert.Fail("Did not expected witnesses for overrides function.")
                        if x.EnclosingType.TypeParameters.IsEmpty then
                            if isErasingFunc then
                                this.EmitFunction(x.Formal)
                            else
                                this.EmitFunctionNoErasure(x.Formal)                                   
                        else
                            let enclosingTy = 
                                x.EnclosingType.Substitute(genericContext)
                            this.EmitFunction(x.Formal.MakeReference(enclosingTy))
                )

            let flags = func.Flags
            let flags =
                if isErasingFunc then
                    if func.TypeParameters.IsEmpty then
                        flags
                    else
                        flags.SetGenericsErased()
                else
                    flags

            let returnTy = this.EmitType(func.ReturnType)
            let emittedEnclosingTy = 
                if enclosingTy.IsNewtype then
                    // We need to actually emit the newtype as a type definition here
                    // so the function can be emitted correctly.
                    emitTypeDefinition enclosingTy
                else
                    this.EmitType(enclosingTy)

            match emitted.TryGetValue(key) with
            | ValueSome(emittedFunc) -> 
                emittedFunc
            | _ ->

            let sigKey = func.ComputeSignatureKey()

            let irAttrs = emitAttributes ilAsm func.Attributes

            pars
            |> ImArray.iter (fun par ->
                match par with
                | OlyIRParameter(attrs=attrs) -> attrs.Force() |> ignore
            )

            let externalInfoOpt =
                if func.IsExternal then
                    func.TryGetExternalInfo()
                else
                    None

            let flags, pars =
                if func.EnclosingType.IsNewtype then
                    OlyAssert.True(overrides.IsNone)
                    OlyAssert.False(func.Flags.IsConstructor && func.Flags.IsInstance)
                    OlyAssert.True(externalInfoOpt.IsNone)

                    let pars =
                        if func.Flags.IsInstance then
                            let fakeReceiverTy =
                                let extendsTy = func.EnclosingType.RuntimeType.Value
                                if extendsTy.IsAnyStruct then
                                    createByReferenceRuntimeType OlyIRByRefKind.ReadOnly extendsTy
                                else
                                    extendsTy
                            pars
                            |> ImArray.prependOne (OlyIRParameter(Lazy<_>.CreateFromValue(ImArray.empty), "", this.EmitType(fakeReceiverTy), false))
                        else
                            pars

                    flags.SetStatic(), pars
                else
                    flags, pars

            let emittedFunc = this.Emitter.EmitFunctionDefinition(externalInfoOpt, emittedEnclosingTy, flags, func.Name, tyPars, pars, returnTy, overrides, sigKey, irAttrs)
            emitted.[key] <- emittedFunc

#if DEBUG || CHECKED
            Log(
                let witnessText = 
                    if witnesses.IsEmpty then
                        ""
                    else
                        let text = witnesses |> ImArray.map (fun x -> x.TypeExtension.Name.ToString()) |> (String.concat "\n")
                        $" - Witnesses: {text}"
                $"Emitting Function: {func.EnclosingType.Name}.{func.Name}{witnessText}"
            )
#endif

            if func.HasILFunctionBody then
                emitFunctionBody func emittedFunc genericContext
            else
                if not func.Flags.IsAbstract && not func.Flags.IsExternal then
                    invalidOp $"Expected function body for: {func.EnclosingType.Name}::{func.Name}"

            let funcTyArgs = 
                if genericContext.IsErasing then
                    genericContext.TypeArguments
                    |> ImArray.skip enclosingTy.TypeArguments.Length
                    |> ImArray.ofSeq
                else
                    ImArray.empty

            let ilFuncSpecHandle = ilAsm.GetFunctionDefinition(func.ILFunctionDefinitionHandle).SpecificationHandle

            let tysThatInheritOrImplementTy =
                let asm = assemblies.[enclosingTy.AssemblyIdentity]
                match asm.TypesThatInheritOrImplementType.TryGetValue(enclosingTy.ILEntityDefinitionHandle) with
                | true, tys -> tys |> ImArray.ofSeq
                | _ -> ImArray.empty

            let isPrincipalFuncExternalOrExported = func.IsExternal || func.IsExported

            tysThatInheritOrImplementTy
            |> ImArray.iter (fun ty ->
                if subsumesType func.EnclosingType ty then
                    let funcs = this.FindImmediateOverridenFunctionDefinitions(enclosingTyParCount, ilAsm, ilFuncSpecHandle, ty, funcTyArgs, genericContext)
                    if funcs.IsEmpty then
                        if func.Flags.IsAbstract then
                            if not ty.IsAbstract then
                                let funcs =
                                    if funcs.IsEmpty then
                                        this.FindMostSpecificFunctionsInHierarchy(enclosingTyParCount, ilAsm, ilFuncSpecHandle, ty, funcTyArgs, genericContext)
                                    else
                                        funcs

                                let funcs =
                                    if funcs.Length > 1 then
                                        funcs
                                        |> ImArray.filter (fun x ->
                                            x.EnclosingType = ty
                                        )
                                    else
                                        funcs

                                if funcs.IsEmpty then
                                    failwithf "When emitting function definition, function not found: %A" func.Name
                                elif funcs.Length > 1 then
                                    failwithf "When emitting function definition, duplicate functions found: %A" func.Name
                                else
                                    let foundFunc = funcs |> ImArray.head
                                    if isPrincipalFuncExternalOrExported then
                                        this.EmitFunctionNoErasure(foundFunc)
                                        |> ignore
                                    else
                                        this.EmitFunction(foundFunc)
                                        |> ignore

                    elif funcs.Length > 1 then
                        failwithf "Duplicate functions found: %A" func.Name
                    else
                        let overridenFunc = funcs |> ImArray.head
                        if (overridenFunc.Enclosing.AsType.IsExternal) || (ty.TypeArguments.IsEmpty && funcTyArgs.IsEmpty) then
                            this.EmitFunctionNoErasure(overridenFunc) |> ignore
                        else
                            let funcInst =
                                if isErasingFunc then
                                    overridenFunc.MakeInstance(ty, funcTyArgs)
                                else
                                    overridenFunc.MakeReference(ty)
                            let funcInst = funcInst |> setWitnessesToFunction witnesses genericContext
                            if isErasingFunc then
                                this.EmitFunction(funcInst) |> ignore
                            else
                                this.EmitFunctionNoErasure(funcInst) |> ignore
                )

            emittedFunc

    member vm.EmitFunction(canErase, func: RuntimeFunction) =
        let genericContext = createGenericContextFromFunction canErase func

        if func.IsFormal then
            let enclosingTy =
                let enclosingTy = func.Enclosing.AsType
                if genericContext.IsErasingType then enclosingTy
                else enclosingTy.Formal
            vm.EmitFunctionDefinition(enclosingTy, func, genericContext)
        else
            if func.Enclosing.TypeArguments.IsEmpty && func.TypeArguments.IsEmpty then
                vm.EmitFunctionNoErasure(func.Formal)
            elif not genericContext.IsErasingFunction then
                
                if func.IsFormal then
                    failwith "Unexpected formal function."

                let asm = assemblies.[func.AssemblyIdentity]

                let emitted = asm.GetEmittedFunctionDefinition(func)
                let enclosingTy = func.EnclosingType
                let key = struct(false, enclosingTy, func.TypeArguments, func.Witnesses, false)
                match emitted.TryGetValue(key) with
                | ValueSome(emittedFunc) -> emittedFunc
                | _ ->
                    let emittedFuncDef = 
                        let enclosingTy = func.EnclosingType
                        if enclosingTy.IsExternal then
                            vm.EmitFunctionDefinition(func.EnclosingType.Formal, func.Formal, genericContext)
                        else
                            let enclosingTy =
                                let enclosingTy = func.Enclosing.AsType
                                if genericContext.IsErasingType then enclosingTy
                                else enclosingTy.Formal
                            let formalFunc =
                                if genericContext.IsErasingType && not enclosingTy.TypeParameters.IsEmpty then
                                    func.Formal.MakeReference(enclosingTy).MakeFormal()
                                else
                                    func.Formal
                            vm.EmitFunctionDefinition(enclosingTy, formalFunc, genericContext)

                    let emittedFunc = 
                        if genericContext.IsErasingType && func.TypeParameters.IsEmpty then
                            // This is an interesting case where the enclosing type is being erased and
                            // the function is a reference, in which case we do not need to emit a function reference.
                            emittedFuncDef
                        else
                            match func.Kind with
                            | RuntimeFunctionKind.Formal -> failwith "Unexpected formal function."
                            | RuntimeFunctionKind.Instance ->
                                let funcTyArgs = func.TypeArguments |> ImArray.map (fun x -> this.EmitTypeArgument(x))
                                this.Emitter.EmitFunctionInstance(this.EmitType(func.EnclosingType), emittedFuncDef, funcTyArgs)
                            | RuntimeFunctionKind.Reference ->
                                this.Emitter.EmitFunctionReference(this.EmitType(func.EnclosingType), emittedFuncDef)

                    emitted.[key] <- emittedFunc
                    emittedFunc
            else
                vm.EmitFunctionDefinition(func.Enclosing.AsType, func, genericContext)

    member runtime.EmitFunction(func: RuntimeFunction) : 'Function =
        runtime.EmitFunction(true, func)

    member runtime.EmitFunctionNoErasure(func: RuntimeFunction) : 'Function =
        // If the function has no type parameters, then emit it as erasing.
        // We do this because the function cache always stores 'isErasingFunc' as 'true' for these kinds of functions with no type parameters.
        if func.TypeParameters.IsEmpty then
            runtime.EmitFunction(func)
        else
            runtime.EmitFunction(false, func)

    member runtime.EmitILConstant(ilAsm, ilConstant: OlyILConstant, genericContext) =
        emitConstant ilAsm ilConstant genericContext

    member this.EmitFunctionFromEnvironment(envFunc: RuntimeFunction, func: RuntimeFunction) =
        if canPossiblyEraseGenericFunction envFunc func then
            this.EmitFunction(func)
        else
            // Forces no generic erasure.
            this.EmitFunctionNoErasure(func)

    member this.TryGetCallStaticConstructorExpression(enclosingTy: RuntimeType) =
        match enclosingTy.Formal.TryGetStaticConstructor() with
        | Some func ->
            let emittedStaticCtor = this.EmitFunction(func.MakeReference(enclosingTy))
            let irFunc =
                OlyIRFunction(emittedStaticCtor, func)

            E.Operation(
                NoRange, 
                O.CallStaticConstructor(irFunc, this.EmitType(RuntimeType.Void))
            )
            |> Some
        | _ ->
            None

    member this.TryGetCallStaticConstructorExpression(targetFunc: RuntimeFunction) =
        if (not targetFunc.Flags.IsStatic && targetFunc.Flags.IsConstructor) then
            this.TryGetCallStaticConstructorExpression(targetFunc.EnclosingType)
        else
            None

    member this.TryGetCallStaticConstructorExpression(targetField: RuntimeField) =
        if targetField.IsStatic then
            this.TryGetCallStaticConstructorExpression(targetField.EnclosingType)
        else
            None

    member this.TryResolveFunctionBody(func: RuntimeFunction, genericContext): Lazy<_> option =
        if func.HasILFunctionBody then
            let cachedBodyOpt =
                if func.Flags.IsInlineable then
                    match inlineFunctionBodyCache.TryGetValue(func) with
                    | ValueSome body -> Some body
                    | _ -> None
                else
                    None

            match cachedBodyOpt with
            | Some _ -> cachedBodyOpt
            | _ ->
                let body = 
                    lazy
                    importFunctionBody
                        this
                        func.Formal.EnclosingType.AssemblyIdentity
                        func.Formal.EnclosingType.ILEntityDefinitionHandle
                        func.Formal.ILFunctionDefinitionHandle
                        (genericContext.SetPassedWitnesses(func.EnclosingType.Witnesses.AddRange(func.Witnesses)))

                if func.Flags.IsInlineable then
                    inlineFunctionBodyCache.SetItem(func, body)

                Some body
        else
            None

    member this.OptimizeFunctionBody(func: RuntimeFunction, funcBody: OlyIRFunctionBody<_, _, _>, genericContext: GenericContext) =
        optimizeFunctionBody func funcBody genericContext

    member private this.Verify(func: RuntimeFunction) =

        // Closure Verification
        if func.EnclosingType.IsClosure && func.Flags.IsInstance && func.Flags.IsConstructor then
            let ilAsm = func.ILAssembly
            let ilFuncDef = ilAsm.GetFunctionDefinition(func.ILFunctionDefinitionHandle)
            let ilFuncBody = ilAsm.GetFunctionBody(ilFuncDef.BodyHandle.contents.Value)

            let fields = func.EnclosingType.Fields |> ImArray.filter (fun field -> field.Flags.IsInstance)
            let fieldSet = HashSet<RuntimeField>()
            let rec check hasCtor ilExpr =
                match ilExpr with
                | OlyILExpression.None _ -> hasCtor

                | OlyILExpression.Sequential(ilExpr1, ilExpr2) ->
                    let hasCtor = check hasCtor ilExpr1
                    check hasCtor ilExpr2

                | OlyILExpression.Operation(op=OlyILOperation.Call(ilFuncInst, argExprs)) when argExprs.Length = 1 && not hasCtor ->
                    let ctor = this.ResolveFunction(ilAsm, ilFuncInst, GenericContext.Default, ImArray.empty)
                    if ctor.Flags.IsInstance && ctor.Flags.IsConstructor && ctor.EnclosingType = func.EnclosingType.Extends[0] then
                        true
                    else
                        OlyAssert.Fail("Invalid closure constructor.")

                | OlyILExpression.Operation(op=OlyILOperation.StoreField(ilFieldRef, OlyILExpression.Value(value=OlyILValue.Argument(0)), OlyILExpression.Value(value=OlyILValue.Argument(argIndex)))) ->
                    let field = this.ResolveField(ilAsm, ilFieldRef, GenericContext.Default)
                    if field.Formal = fields[argIndex - 1].Formal && fieldSet.Add(field.Formal) then
                        true// do not allow base constructor call after fields are set
                    else
                        OlyAssert.Fail("Invalid closure constructor.")

                | _ ->
                    OlyAssert.Fail("Invalid closure constructor.")
            check false ilFuncBody.BodyExpression
            |> ignore
            if fieldSet.Count <> fields.Length then
                OlyAssert.Fail("Invalid closure constructor.")

    member this.ImportFunctionBody(
                bodyFunc: RuntimeFunction, 
                ilAsm: OlyILReadOnlyAssembly, 
                ilFuncBody: OlyILFunctionBody, 
                bodyArgTys: RuntimeType imarray, 
                returnTy: RuntimeType, 
                genericContext: GenericContext
            ) : OlyIRFunctionBody<'Type, 'Function, 'Field> =

        let setStaticFieldsToDefaultValueExprsOpt =
            if bodyFunc.Flags.IsStatic && bodyFunc.Flags.IsConstructor then
                let staticFields =
                    bodyFunc.EnclosingType.Fields
                    |> ImArray.filter (fun x -> x.IsStatic)

                let setStaticFieldsToDefaultValueExprs =
                    (E.None(NoRange, this.EmitType(RuntimeType.Void)), staticFields)
                    ||> ImArray.fold (fun irExpr field ->
                        let setfieldToDefaultValueExpr =
                            E.Operation(
                                NoRange, 
                                O.StoreStaticField(
                                    OlyIRField(this.EmitField(field), field),
                                    createDefaultExpression NoRange (field.Type, this.EmitType(field.Type)),
                                    this.EmitType(RuntimeType.Void)
                                )
                            )
                        match irExpr with
                        | E.None _ ->
                            setfieldToDefaultValueExpr
                        | _ ->
                            E.Sequential(
                                irExpr,
                                setfieldToDefaultValueExpr
                            )
                    )

                Some setStaticFieldsToDefaultValueExprs
            else
                None

        if genericContext.IsErasingFunction then
            genericContext.TypeArguments
            |> ImArray.iter (function
                | RuntimeType.Variable(index, ilKind)
                | RuntimeType.HigherVariable(index, _, ilKind) -> 
                    if genericContext.CanErase(index, ilKind) then
                        failwith "Type variable was not erased."
                | _ -> ()
            )

        let bodyEnclosingTy = bodyFunc.EnclosingType
        let enclosingTyParCount = bodyEnclosingTy.TypeArguments.Length

        let ilLocals, ilExpr =
            match ilFuncBody with
            | OlyILFunctionBody(ilLocals, ilExpr) -> ilLocals, ilExpr

        let localTys =
            ilLocals
            |> ImArray.map (fun x -> 
                this.ResolveType(ilAsm, x.Type, genericContext)
            )

        let localCount = ilLocals.Length

        let argCount =
            if bodyFunc.Flags.IsInstance then
                1 + bodyFunc.Parameters.Length
            else
                bodyFunc.Parameters.Length                

        let cenv = cenv(localCount, argCount, this)
        let env =
            {
                ArgumentTypes = bodyArgTys
                ILAssembly = ilAsm
                LocalTypes = localTys
                ILLocals = ilLocals
                EnclosingTypeParameterCount = enclosingTyParCount
                Function = bodyFunc
                GenericContext = genericContext
            }

        let irExpr = importArgumentExpression cenv env returnTy ilExpr

        let irFinalExpr =
            match setStaticFieldsToDefaultValueExprsOpt with
            | Some(setStaticFieldsToDefaultValueExprs) ->
                E.Sequential(
                    setStaticFieldsToDefaultValueExprs,
                    irExpr
                )
            | _ ->
                irExpr

        let irArgFlags =
            Array.init argCount (fun i ->
                let irArgFlags = OlyIRLocalFlags.None
                let irArgFlags =
                    if cenv.ArgumentMutability[i] then
                        irArgFlags ||| OlyIRLocalFlags.Mutable
                    else
                        irArgFlags
                let irArgFlags =
                    if bodyFunc.IsArgumentReadWriteByRefType(i) then
                        irArgFlags ||| OlyIRLocalFlags.ReadWriteByRefType
                    else
                        irArgFlags
                let irArgFlags =
                    if bodyFunc.IsArgumentReadOnlyByRefType(i) then
                        irArgFlags ||| OlyIRLocalFlags.ReadOnlyByRefType
                    else
                        irArgFlags
                let irArgFlags =
                    if bodyFunc.IsArgumentWriteOnlyByRefType(i) then
                        irArgFlags ||| OlyIRLocalFlags.WriteOnlyByRefType
                    else
                        irArgFlags
                let irArgFlags =
                    if cenv.ArgumentAddressExposed[i] then
                        irArgFlags ||| OlyIRLocalFlags.AddressExposed
                    else
                        irArgFlags
                irArgFlags                
            )

        let irLocalFlags =
            ilLocals
            |> Seq.mapi (fun i x ->
                let irLocalFlags = OlyIRLocalFlags.None
                let irLocalFlags =
                    if cenv.LocalMutability[i] then
                        irLocalFlags ||| OlyIRLocalFlags.Mutable
                    else
                        irLocalFlags
                let irLocalFlags =
                    if localTys[i].IsReadWriteByRef then
                        irLocalFlags ||| OlyIRLocalFlags.ReadWriteByRefType
                    else
                        irLocalFlags
                let irLocalFlags =
                    if localTys[i].IsReadOnlyByRef then
                        irLocalFlags ||| OlyIRLocalFlags.ReadOnlyByRefType
                    else
                        irLocalFlags
                let irLocalFlags =
                    if cenv.LocalAddressExposed[i] then
                        irLocalFlags ||| OlyIRLocalFlags.AddressExposed
                    else
                        irLocalFlags
                irLocalFlags
            )
            |> Array.ofSeq

        OlyIRFunctionBody(irFinalExpr, irArgFlags, irLocalFlags)

    member this.TryGetIROfFunction(fullyQualifiedTypeName: string, funcName: string): Result<OlyIRFunctionBody<'Type, 'Function, 'Field>, string> =
        let funcs =
            let nmspcAndTyName = fullyQualifiedTypeName.Split(".")
            let nmspc, tyName =
                if nmspcAndTyName.Length > 1 then
                    nmspcAndTyName[..nmspcAndTyName.Length - 2] |> ImArray.ofSeq, nmspcAndTyName[nmspcAndTyName.Length - 1]
                else
                    ImArray.empty, nmspcAndTyName[0]

            this.Assemblies.Values
            |> Seq.choose (fun asm ->
                let funcs =
                    asm.FunctionDefinitionCache.Values 
                    |> Seq.filter (fun (x, _) -> x.Name = funcName)
                    |> Seq.filter (fun (x, _) ->
                        if x.EnclosingType.Name = tyName then
                            match x.EnclosingType.Enclosing with
                            | RuntimeEnclosing.Namespace(nmspc2) when nmspc.Length = nmspc2.Length ->
                                (nmspc, nmspc2)
                                ||> ImArray.forall2 (fun x y -> x = y)
                            | _ ->
                                false
                        else
                            false
                    )
                    |> Seq.map (fun (x, _) -> x)
                    |> ImArray.ofSeq

                if funcs.IsEmpty then None
                else Some funcs
            )
            |> Seq.concat
            |> ImArray.ofSeq

        if funcs.IsEmpty then
            Error("No functions found.")
        elif funcs.Length > 1 then
            Error("Too many functons found.")
        else
            let func = funcs[0]
            match this.TryResolveFunctionBody(func, GenericContext.Default) with
            | Some(lazyIRBody) ->
                Ok(lazyIRBody.Value)
            | _ ->
                Error("Function body not found.")

    member this.GetFunctionTier(func: RuntimeFunction) =
        // TODO: At the moment, we do not use Tier1. We will have to do work if we want to make this
        //       a tiered JIT.
        if assemblies[func.EnclosingType.AssemblyIdentity].ilAsm.IsDebuggable then
            OlyIRFunctionTier.Tier0(true)
        else
            OlyIRFunctionTier.Tier2

    interface IOlyVirtualMachine<'Type, 'Function, 'Field> with

        member this.GetTypeVoid(): 'Type =
            this.EmitType(RuntimeType.Void)

        member this.GetTypeInt32(): 'Type =
            this.EmitType(RuntimeType.Int32)

        member this.GetTypeFloat32(): 'Type =
            this.EmitType(RuntimeType.Float32)

        /// Try to find a type based on its fully-qualified name.
        /// Note: Does not support nested types (yet).
        member this.TryFindType(fullyQualifiedTypeName: string, tyParCount: int32): 'Type option =
            tryFindType(fullyQualifiedTypeName, tyParCount)
            |> Option.map this.EmitType

        member this.TryFindType(fullyQualifiedTypeName): 'Type option =
            (this: IOlyVirtualMachine<'Type, 'Function, 'Field>).TryFindType(fullyQualifiedTypeName, 0)

        member this.TryFindField(fullyQualifiedTypeName, tyParCount, fieldName): 'Field option =
            match tryFindType(fullyQualifiedTypeName, tyParCount) with
            | Some(ty) -> ty.Fields |> ImArray.tryFind (fun x -> x.Name = fieldName) |> Option.map this.EmitField
            | _ -> None

        // TODO: This doesn't handle type variables well at all.
        member this.TryFindFunction(
                enclosingType: (string * int32),  
                name: string,
                typeParameterCount: int32,
                parameterCount: int32,
                kind: OlyFunctionKind): 'Function option =
            match tryFindType(fst enclosingType, snd enclosingType) with
            | Some(enclosingTy) ->
                let isInstance = kind = OlyFunctionKind.Instance
                let tyArgs =
                    ImArray.init typeParameterCount (fun i -> RuntimeType.Variable(i, OlyILTypeVariableKind.Function))

                let genericContext = GenericContext.Create(enclosingTy.TypeArguments, tyArgs)

                let ilAsm = assemblies[enclosingTy.AssemblyIdentity].ilAsm
                let ilFuncDefHandles = ilAsm.FindFunctionDefinitions(enclosingTy.ILEntityDefinitionHandle, name)
                let funcs =
                    ilFuncDefHandles
                    |> ImArray.choose (fun ilFuncDefHandle ->
                        let ilFuncDef = ilAsm.GetFunctionDefinition(ilFuncDefHandle)
                        let ilFuncSpec = ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)
                        if ilFuncSpec.Parameters.Length = parameterCount && ilFuncSpec.TypeParameters.Length = typeParameterCount then
                            let (funcs, _) = tryResolveFunction ilAsm ilFuncSpec tyArgs enclosingTy genericContext
                            if funcs.Length = 1 then
                                let func = funcs[0]
                                if func.Flags.IsInstance = isInstance then
                                    Some func
                                else
                                    None                                           
                            else
                                None
                        else
                            None
                    )

                if funcs.Length = 1 then
                    Some(this.EmitFunction(funcs[0]))
                else
                    None
            | _ ->
                None

        // TODO: This doesn't handle type variables well at all.
        member this.TryFindFunction(
                enclosingType: (string * int32),  
                name: string,
                typeParameterCount: int32,
                parameterTypes: (string * int32) imarray, 
                returnType: (string * int32),
                kind: OlyFunctionKind): 'Function option =
            match tryFindType(fst enclosingType, snd enclosingType) with
            | Some(enclosingTy) ->
                let targetParTys =
                    parameterTypes
                    |> ImArray.map (fun (tyName, tyParCount) -> tryFindType(tyName, tyParCount))

                let targetReturnTy = tryFindType(fst returnType, snd returnType)

                let foundTypes = (targetParTys |> ImArray.forall (fun x -> x.IsSome)) && targetReturnTy.IsSome
                if foundTypes then
                    let targetReturnTy = targetReturnTy.Value
                    let targetParTys =
                        targetParTys
                        |> ImArray.map (fun x -> x.Value)

                    let isInstance = kind = OlyFunctionKind.Instance
                    let tyArgs =
                        ImArray.init typeParameterCount (fun i -> RuntimeType.Variable(i, OlyILTypeVariableKind.Function))

                    let genericContext = GenericContext.Create(enclosingTy.TypeArguments, tyArgs)

                    let ilAsm = assemblies[enclosingTy.AssemblyIdentity].ilAsm
                    let ilFuncDefHandles = ilAsm.FindFunctionDefinitions(enclosingTy.ILEntityDefinitionHandle, name)
                    let funcs =
                        ilFuncDefHandles
                        |> ImArray.choose (fun ilFuncDefHandle ->
                            let ilFuncDef = ilAsm.GetFunctionDefinition(ilFuncDefHandle)
                            let ilFuncSpec = ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)
                            if ilFuncSpec.Parameters.Length = parameterTypes.Length && ilFuncSpec.TypeParameters.Length = typeParameterCount then
                                let (funcs, _) = tryResolveFunction ilAsm ilFuncSpec tyArgs enclosingTy genericContext
                                if funcs.Length = 1 then
                                    let func = funcs[0]
                                    if func.Flags.IsInstance = isInstance && func.Parameters.Length = parameterTypes.Length then
                                        let sigMatches =
                                            targetReturnTy = func.ReturnType &&
                                            (
                                                (targetParTys, func.Parameters |> ImArray.map (fun x -> x.Type))
                                                ||> ImArray.forall2 (=)
                                            )
                                        if sigMatches then
                                            Some func
                                        else
                                            None
                                    else
                                        None                                           
                                else
                                    None
                            else
                                None
                        )

                    if funcs.Length = 1 then
                        Some(this.EmitFunction(funcs[0]))
                    else
                        None
                else
                    None
            | _ ->
                None


        


        