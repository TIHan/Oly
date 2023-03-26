[<AutoOpen>]
module rec Oly.Runtime.Implementation

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable
open System.Diagnostics
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns
open Oly.Runtime.CodeGen.Internal.Optimizer
open Oly.Metadata
open Oly.Core
open Oly.Core.TaskExtensions

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
        isSimpleILExpression (depth + 1) ilExpr1 && isSimpleILExpression (depth + 1) ilExpr2
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
        | OlyILOperation.Cast(ilArgExpr, _)->
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
    | _ ->
        false

/// Does introspection, this is fine considering at the moment we already have the body
/// in-memory. When that changes and the information is say, only on-disk, what would we do?
/// Perhaps we only do introspection up to a point?
let checkFunctionInlineability (ilAsm: OlyILReadOnlyAssembly) (ilFuncDef: OlyILFunctionDefinition) =
    if ilFuncDef.IsConstructor || ilFuncDef.IsAbstract || (ilFuncDef.IsVirtual && not ilFuncDef.IsSealed) || ilFuncDef.IsExported || ilFuncDef.IsImported then
        false
    elif ilFuncDef.Flags.HasFlag(OlyILFunctionFlags.StackEmplace) then
        true
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

        // TODO: Add a flag to always inline the function, for stress testing purposes.

        // If there are locals, this isn't a candidate for inlining as we may not want the
        // extra locals.
        if ilFuncBody.Locals.IsEmpty then
            isSimpleILExpression 0 ilFuncBody.BodyExpression
        else
            false

[<Sealed>]
type RuntimeTypeInstanceCache<'Type, 'Function, 'Field>(runtime: OlyRuntime<'Type, 'Function, 'Field>, ilAsm: OlyILReadOnlyAssembly) =

    let cache = ConcurrentDictionary<OlyILEntityDefinitionHandle, RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, RuntimeType>>()

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
        EntityDefinitionCache: ConcurrentDictionary<OlyILEntityDefinitionHandle, RuntimeType * RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Type>>
        EntityInstanceCache: ConcurrentDictionary<OlyILEntityDefinitionHandle, RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Type>>
        entRefCache: ConcurrentDictionary<OlyILEntityReferenceHandle, RuntimeType>

        FunctionDefinitionCache: ConcurrentDictionary<OlyILFunctionDefinitionHandle, RuntimeFunction * RuntimeTypeArgumentWitnessListTable<'Type, 'Function, 'Field, 'Function>>
        FieldDefinitionCache: ConcurrentDictionary<OlyILFieldDefinitionHandle, RuntimeField * RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Field>>

        FieldVariadicDefinitionCache: ConcurrentDictionary<OlyILFieldDefinitionHandle, ConcurrentDictionary<string, RuntimeField * RuntimeTypeArgumentListTable<'Type, 'Function, 'Field, 'Field>>>

        RuntimeTypeInstanceCache: RuntimeTypeInstanceCache<'Type, 'Function, 'Field>
        RuntimeFieldReferenceCache: RuntimeFieldReferenceCache<'Type, 'Function, 'Field>

        TypesThatInheritOrImplementType: ConcurrentDictionary<OlyILEntityDefinitionHandle, ResizeArray<RuntimeType>>
    }  

let createFunctionDefinition<'Type, 'Function, 'Field> (runtime: OlyRuntime<'Type, 'Function, 'Field>) (enclosingTy: RuntimeType) (ilFuncDefHandle: OlyILFunctionDefinitionHandle) =
    let asm = runtime.Assemblies[enclosingTy.AssemblyIdentity]
    let ilAsm = asm.ilAsm
    let ilFuncDef = ilAsm.GetFunctionDefinition(ilFuncDefHandle)
    let ilFuncSpec = ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)

    let name = ilAsm.GetStringOrEmpty ilFuncSpec.NameHandle

    if enclosingTy.IsBuiltIn then
        failwith "Expected non-built-in type."

    let enclosing = RuntimeEnclosing.Type(enclosingTy)
    let enclosingTyParCount = enclosingTy.TypeParameters.Length

    let returnTy = runtime.ResolveType(enclosingTyParCount, ilAsm, ilFuncSpec.ReturnType, GenericContext.Default)

    let isExternal =
        ilFuncDef.Attributes
        |> ImArray.exists (function OlyILAttribute.Import _ -> true | _ -> false)
        
    let irFlags =
        if isExternal then
            RuntimeFunctionFlags.External
        else
            RuntimeFunctionFlags.None

    let irFlags =
        if ilFuncDef.Flags.HasFlag(OlyILFunctionFlags.StackEmplace) then
            irFlags ||| RuntimeFunctionFlags.StackEmplace
        else
            irFlags

    let irFlags =
        match ilAsm.EntryPoint with
        | Some(ilEnclosingTy, ilFuncDefHandle2) ->
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
            { 
                Name = ilAsm.GetStringOrEmpty(ilTyPar.NameHandle)
                Arity = ilTyPar.Arity
                IsVariadic = ilTyPar.IsVariadic
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
                Name = ilAsm.GetStringOrEmpty(ilPar.NameHandle)
                Type = runtime.ResolveType(enclosingTyParCount, ilAsm, ilPar.Type, GenericContext.Default)
                IsMutable = ilPar.IsMutable
                CanInlineClosure = ilPar.CanInlineClosure
            } : RuntimeParameter
        )

    let overrides =
        ilFuncDef.Overrides
        |> Option.map (fun x ->
            runtime.ResolveFunction(ilAsm, x, GenericContext.Default)
        )

    let attrs =
        ilFuncDef.Attributes
        |> ImArray.choose (fun x -> runtime.TryResolveAttribute(ilAsm, x, GenericContext.Default, ImArray.empty))

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

    member inline _.ResolveType(enclosingTyParCount, ilAsm, ilTy, genericContext): RuntimeType =
        vm.ResolveType(enclosingTyParCount, ilAsm, ilTy, genericContext)

    member inline _.ResolveTypes(enclosingTyParCount: int, ilAsm: OlyILReadOnlyAssembly, ilTys: OlyILType imarray, genericContext: GenericContext) =
        vm.ResolveTypes(enclosingTyParCount, ilAsm, ilTys, genericContext)

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

    member inline _.EmitFunctionFromEnvironment(envFunc, func): 'Function =
        vm.EmitFunctionFromEnvironment(envFunc, func)

    member inline _.TryFindPossibleWitness(ty, abstractTy, witnesses): RuntimeWitness option =
        vm.TryFindPossibleWitness(ty, abstractTy, witnesses)

    member val LocalAddressExposed = Array.init localCount (fun _ -> false) with get
    member val LocalMutability = Array.init localCount (fun _ -> false) with get
    member val ArgumentAddressExposed = Array.init argCount (fun _ -> false) with get
    member val ArgumentUsageCount = Array.init argCount (fun _ -> 0) with get

[<NoEquality;NoComparison>]
type env<'Type, 'Function, 'Field> =
    {
        PassedWitnesses: RuntimeWitness imarray
        GenericContext: GenericContext
        ILAssembly: OlyILReadOnlyAssembly
        EnclosingTypeParameterCount: int
        LocalTypes: RuntimeType imarray
        ArgumentTypes: RuntimeType imarray
        ILLocals: OlyILLocal imarray
        Function: RuntimeFunction
    }

    member env.HandleReceiver(cenv: cenv<'Type, 'Function, 'Field>, expectedArgTy: RuntimeType, irArg: E<'Type, 'Function, 'Field>, argTy: RuntimeType, isVirtual) : E<'Type, 'Function, 'Field> * RuntimeType =
        let expectedArgTy = expectedArgTy.StripAlias()
        let argTy = argTy.StripAlias()

#if DEBUG
        OlyAssert.False(expectedArgTy.IsTypeExtension)
        OlyAssert.False(expectedArgTy.IsModule)
        OlyAssert.False(argTy.IsTypeExtension)
        OlyAssert.False(argTy.IsModule)
#endif
        
        if argTy.IsByRef_t && (not argTy.IsByRefOfVariable || env.GenericContext.IsErasingFunction) && (not expectedArgTy.IsByRef_t) then
            match irArg with
            | E.Value(irTextRange, V.LocalAddress(n, _, _)) ->
                let ty = argTy.TypeArguments[0]
                E.Value(irTextRange, V.Local(n, cenv.EmitType(ty))), ty
        
            | E.Value(irTextRange, V.ArgumentAddress(n, _, _)) ->
                let ty = argTy.TypeArguments[0]
                E.Value(irTextRange, V.Argument(n, cenv.EmitType(ty))), ty
        
            | E.Operation(irTextRange, O.LoadFieldAddress(field, arg, _, _)) ->
                let elementTy = argTy.TypeArguments[0]
                E.Operation(irTextRange, O.LoadField(field, arg, cenv.EmitType(elementTy))), elementTy
            | _ ->
                irArg, argTy
        else
            irArg, argTy

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
            (not envFunc.TypeParameters.IsEmpty || not envFunc.EnclosingType.TypeParameters.IsEmpty) &&
            func.Formal.EnclosingType <> envFunc.Formal.EnclosingType
        )
        |> not
    )

let createDefaultExpression irTextRange (resultTy: RuntimeType, emittedTy: 'Type) =
    let asExpr irValue =
        E.Value(irTextRange, irValue)

    if resultTy.IsAnyStruct then
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
            V.Constant(C.Int8(0y), emittedTy) |> asExpr
        | RuntimeType.Char16 ->
            V.Constant(C.Char16(char 0), emittedTy) |> asExpr
        | _ ->
            V.DefaultStruct(emittedTy) |> asExpr
    else
        V.Null(emittedTy) |> asExpr

let incrementArgumentUsage (cenv: cenv<'Type, 'Function, 'Field>) argIndex =
    cenv.ArgumentUsageCount[argIndex] <- cenv.ArgumentUsageCount[argIndex] + 1

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
    // TODO: Expand this to Let, and potentially others.
    match ilExpr with
    | OlyILExpression.Sequential(ilExpr1, ilExpr2) ->
        match ilExpr1 with
        | OlyILExpression.Sequential _ ->
            importSequentialExpression cenv env (Some RuntimeType.Void) ilExpr1 (fun (irExpr1, _) ->
                match ilExpr2 with
                | OlyILExpression.Sequential _ ->
                    importSequentialExpression cenv env expectedTyOpt ilExpr2 cont
                | _ ->
                    let irExpr2, resultTy = importExpression cenv env expectedTyOpt ilExpr2
                    cont(E.Sequential(irExpr1, irExpr2), resultTy)
            )
        | _ ->
            let irExpr1, _ = importExpression cenv env (Some RuntimeType.Void) ilExpr1
            match ilExpr2 with
            | OlyILExpression.Sequential _ ->
                importSequentialExpression cenv env expectedTyOpt ilExpr2 (fun (irExpr2, resultTy) ->
                    cont(E.Sequential(irExpr1, irExpr2), resultTy)
                )
            | _ ->
                let irExpr2, resultTy = importExpression cenv env expectedTyOpt ilExpr2
                cont(E.Sequential(irExpr1, irExpr2), resultTy)
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

    | OlyILExpression.Sequential _ ->
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
            let resultTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilEnumTy, env.GenericContext)

            if not resultTy.IsEnum then
                failwith "Expected enum type."

            V.Constant(irConstant, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILValue.Null(ilTy) ->
            let resultTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilTy, env.GenericContext)
            V.Null(cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILValue.Default(ilTy) ->
            let resultTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilTy, env.GenericContext)
            createDefaultExpression irTextRange (resultTy, cenv.EmitType(resultTy)), resultTy

        | OlyILValue.Argument(argIndex) ->
            incrementArgumentUsage cenv argIndex

            let argTy = env.ArgumentTypes.[argIndex]
            V.Argument(argIndex, cenv.EmitType(argTy)) |> asExpr, argTy

        | OlyILValue.ArgumentAddress(argIndex, ilByRefKind) ->
            incrementArgumentUsage cenv argIndex
            cenv.ArgumentAddressExposed[argIndex] <- true

            let argTy = env.ArgumentTypes.[argIndex]
            let irByRefKind = 
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.Read -> OlyIRByRefKind.Read
            let resultTy = createByReferenceRuntimeType irByRefKind argTy
            V.ArgumentAddress(argIndex, irByRefKind, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILValue.Local(localIndex) ->
            let localTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, env.ILLocals.[localIndex].Type, env.GenericContext)

            let localTy =
                if localTy.IsNewtype then
                    localTy.Extends[0]
                else
                    localTy

            V.Local(localIndex, cenv.EmitType(localTy)) |> asExpr, localTy

        | OlyILValue.LocalAddress(localIndex, ilByRefKind) ->
            cenv.LocalAddressExposed[localIndex] <- true
            
            match ilByRefKind with
            | OlyILByRefKind.ReadWrite ->
                cenv.LocalMutability[localIndex] <- true
            | _ ->
                ()

            let localTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, env.ILLocals.[localIndex].Type, env.GenericContext)

            let localTy =
                if localTy.IsNewtype then
                    localTy.Extends[0]
                else
                    localTy

            let irByRefKind = 
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.Read -> OlyIRByRefKind.Read
            if not env.ILLocals.[localIndex].IsMutable && irByRefKind = OlyIRByRefKind.ReadWrite then
                failwith "Invalid IL."
            let elementTy = cenv.EmitType(localTy)
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
                | OlyILByRefKind.Read -> OlyIRByRefKind.Read
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

            let resultTy = RuntimeType.Function(argTys, returnTy)

            V.Function(cenv.EmitFunction(func), cenv.EmitType(resultTy)) |> asExpr, resultTy

    | OlyILExpression.Let(localIndex, ilRhsExpr, ilBodyExpr) ->
        let ilLocal = env.ILLocals.[localIndex]
        let localName = env.ILAssembly.GetStringOrEmpty(ilLocal.NameHandle)
        let irRhsExpr = importArgumentExpression cenv env env.LocalTypes[localIndex] ilRhsExpr
        let irBodyExpr, resultTy = importExpression cenv env expectedTyOpt ilBodyExpr
        E.Let(localName, localIndex, irRhsExpr, irBodyExpr), resultTy

    | OlyILExpression.IfElse(ilConditionExpr, ilTrueTargetExpr, ilFalseTargetExpr) ->
        let conditionExpr, _ = importExpression cenv env (Some RuntimeType.Bool) ilConditionExpr
        let trueTargetExpr, resultTy = importExpression cenv env expectedTyOpt ilTrueTargetExpr
        let falseTargetExpr, _ = importExpression cenv env expectedTyOpt ilFalseTargetExpr
        E.IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, trueTargetExpr.ResultType), resultTy

    | OlyILExpression.Operation(ilTextRange, OlyILOperation.Witness(ilBody, ilWitnessTy, ilReturnTy)) ->
        let irTextRange = readTextRange env.ILAssembly ilTextRange

        let witnessTy = cenv.ResolveType(ilWitnessTy.TypeArguments.Length, env.ILAssembly, ilWitnessTy, env.GenericContext)
        let returnTy = cenv.ResolveType(ilReturnTy.TypeArguments.Length, env.ILAssembly, ilReturnTy, env.GenericContext)

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
                                    OlyIRByRefKind.Read
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
            
        let rec handleCall (func: RuntimeFunction) (irArgs: _ imarray) isVirtualCall =           
            OlyAssert.False(func.EnclosingType.IsShape)

            if func.Flags.IsInlineable then

                (*
                    Stack Emplace:

                    This fixes a nasty and hidden bug in this example:

                    let mutable currentOffset = 0
                    match (abc)
                    | abc =>   // May or may not use a stack-emplace function for the target. Let's assume it does.
                        work()
                        work()
                        work()
                        currentOffset <- currentOffset + 5
                        print(currentOffset) // The bug here was that it prints "0" with optimizations on, due to copy-prop.
                    | _ =>
                        ()

                    Pattern matches could use a stack-emplace function for the target,
                    therefore, we need to check the parameters and the argument expressions of locals
                    and record the local if it is mutable or not because we will do a forced forward-sub
                    regardless of mutability for stack-emplaced functions.

                    This is also validation for stack-emplaced function calls.
                *)
                if func.Flags.IsStackEmplace then
                    (func.Parameters, irArgs)
                    ||> ImArray.iter2 (fun par irArg ->
                        match irArg with
                        | E.Value(value=V.Local(localIndex, _)) ->
                            if par.IsMutable <> env.ILLocals[localIndex].IsMutable then
                                OlyAssert.Fail("Invalid local mutability for stack-emplaced function.")

                            if par.IsMutable then
                                cenv.LocalMutability[localIndex] <- true

                        | E.Value(value=V.Argument(argIndex, _)) ->
                            if par.IsMutable <> env.Function.IsArgumentMutable(argIndex) then
                                OlyAssert.Fail("Invalid argument mutability for stack-emplaced function.")

                        | _ ->
                            OlyAssert.Fail("Invalid stack-emplaced function call.")
                    )

                let dummyEmittedFunc = Unchecked.defaultof<'Function>
                let irFunc = OlyIRFunction(dummyEmittedFunc, func)
                let irExpr = O.Call(irFunc, irArgs, cenv.EmitType(func.ReturnType)) |> asExpr
                irExpr, func.ReturnType
            else
                let emittedFunc = cenv.EmitFunctionFromEnvironment(env.Function, func)
                let irFunc = OlyIRFunction(emittedFunc, func)

                let handle() =
                    let irExpr = O.Call(irFunc, irArgs, cenv.EmitType(func.ReturnType)) |> asExpr
                    irExpr, func.ReturnType

                let callExpr, resultTy =
                    // Basic devirtualization.
                    if (func.Flags.IsFinal || not func.Flags.IsVirtual) && isVirtualCall then
                        handle()
                    elif isVirtualCall then
                        O.CallVirtual(irFunc, irArgs, cenv.EmitType(func.ReturnType)) |> asExpr, func.ReturnType
                    else
                        handle()

                callExpr, resultTy

        match ilOp with
        | OlyILOperation.LoadFunction(ilFuncInst, ilArgExpr) ->
            let func = cenv.ResolveFunction(env.ILAssembly, ilFuncInst, env.GenericContext, env.PassedWitnesses)
            let argTys =
                func.Parameters
                |> ImArray.map (fun x -> x.Type)
            let returnTy = func.ReturnType

            let resultTy = RuntimeType.Function(argTys, returnTy)

            let argExpr, _ = importExpression cenv env (Some resultTy) ilArgExpr

            let emittedFunc = cenv.EmitFunction(func)

            let irFunc = OlyIRFunction(emittedFunc, func)
            E.Operation(
                irTextRange,
                O.LoadFunction(irFunc, argExpr, cenv.EmitType(resultTy))
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
                |> ImArray.map (fun x -> importArgumentExpression cenv env RuntimeType.Int32 x)
            let resultTy =
                match resultTy with
                | RuntimeType.Array(elementTy, _, _) -> elementTy
                | _ -> failwith "Invalid type for LoadArrayElement."
            O.LoadArrayElement(irReceiver, irIndexArgs, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.LoadArrayElementAddress(ilReceiver, ilIndexArgs, ilByRefKind) ->
            let irByRefKind =
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.Read -> OlyIRByRefKind.Read
            let irReceiver, resultTy = importExpression cenv env None ilReceiver
            let irIndexArgs = 
                ilIndexArgs
                |> ImArray.map (fun x -> importArgumentExpression cenv env RuntimeType.Int32 x)
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
                |> ImArray.map (fun x -> importArgumentExpression cenv env RuntimeType.Int32 x)
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
            let elementTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilElementTy, env.GenericContext)
            let irArg, _ = importExpression cenv env None ilArg
            O.LoadRefCellContents(irArg, cenv.EmitType(elementTy)) |> asExpr, elementTy

        | OlyILOperation.LoadRefCellContentsAddress(ilElementTy, ilArg, ilByRefKind) ->
            let elementTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilElementTy, env.GenericContext)
            let irArg, _ = importExpression cenv env None ilArg

            let irByRefKind =
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.Read -> OlyIRByRefKind.Read

            let resultTy = createByReferenceRuntimeType irByRefKind elementTy

            O.LoadRefCellContentsAddress(irArg, irByRefKind, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.StoreRefCellContents(ilArg1, ilArg2) ->
            let irArg1, irArgTy1 = importExpression cenv env None ilArg1
            let irArg2, _ = importExpression cenv env (Some irArgTy1.StripAlias().TypeArguments[0]) ilArg2
            O.StoreRefCellContents(irArg1, irArg2, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.Print(ilArg) ->
            let irArg = importArgumentExpression cenv env (cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, OlyILTypeBaseObject, env.GenericContext)) ilArg
            O.Print(irArg, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.Cast(ilArgExpr, ilResultTy) ->
            let irArgExpr, _ = importExpression cenv env None ilArgExpr
            let resultTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilResultTy, env.GenericContext)
            O.Cast(irArgExpr, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.Throw(ilArgExpr, ilResultTy) ->
            let irArgExpr, _ = importExpression cenv env None ilArgExpr
            let resultTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilResultTy, env.GenericContext)
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

            if env.ILLocals[localIndex].IsMutable |> not then
                failwith $"Local '{localIndex}' is not mutable."
            let irArg = importArgumentExpression cenv env env.LocalTypes.[localIndex] ilArg
            O.Store(localIndex, irArg, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.StoreArgument(argIndex, ilArg) ->
            incrementArgumentUsage cenv argIndex

            if env.Function.Parameters[argIndex].IsMutable |> not then
                failwith $"Argument '{argIndex}' is not mutable."
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
                failwith "assert"
            let expectedArgTy1 = 
                if field.EnclosingType.IsAnyStruct then
                    createByReferenceRuntimeType OlyIRByRefKind.ReadWrite field.EnclosingType
                else
                    field.EnclosingType
            let irArg1, argTy1 = importExpression cenv env (Some expectedArgTy1) ilArg1
            let irArg1, _ = env.HandleReceiver(cenv, expectedArgTy1, irArg1, argTy1, false)
            let irArg2 = importArgumentExpression cenv env field.Type ilArg2
            let irField = OlyIRField(cenv.EmitField(field), field)
            O.StoreField(irField, irArg1, irArg2, cenv.EmittedTypeVoid) |> asExpr, RuntimeType.Void

        | OlyILOperation.StoreStaticField(ilFieldRef, ilArg) ->
            let field = cenv.ResolveField(env.ILAssembly, ilFieldRef, env.GenericContext)
            
            // TODO: Add this check for non-constructors.
            //if not field.IsMutable then
            //    failwith "Invalid IL."

            let irArg = importArgumentExpression cenv env field.Type ilArg

            let irField = OlyIRField(cenv.EmitField(field), field)
            let irOpExpr = O.StoreStaticField(irField, irArg, cenv.EmittedTypeVoid) |> asExpr

            if field.EnclosingType.Formal <> env.Function.EnclosingType.Formal then
                match cenv.TryGetCallStaticConstructorExpression(field) with
                | Some callStaticCtorExpr ->
                    E.Sequential(
                        callStaticCtorExpr,
                        irOpExpr
                    ), RuntimeType.Void
                | _ ->
                    irOpExpr, RuntimeType.Void
            else
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
                    createByReferenceRuntimeType OlyIRByRefKind.Read field.EnclosingType
                else
                    field.EnclosingType
            let irArg, _ = env.HandleReceiver(cenv, expectedArgTy, irArg, argTy, false)
            let irField = OlyIRField(cenv.EmitField(field), field)
            O.LoadField(irField, irArg, cenv.EmitType(field.Type)) |> asExpr, field.Type

        | OlyILOperation.LoadFieldAddress(ilFieldRef, ilArg, ilByRefKind) ->
            let field = cenv.ResolveField(env.ILAssembly, ilFieldRef, env.GenericContext)
            OlyAssert.True(field.ILConstant.IsNone)
            OlyAssert.False(field.EnclosingType.IsNewtype)

            let irByRefKind =
                match ilByRefKind with
                | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                | OlyILByRefKind.Read -> OlyIRByRefKind.Read
            let expectedArgTy =
                if field.EnclosingType.IsAnyStruct then
                    createByReferenceRuntimeType OlyIRByRefKind.Read field.EnclosingType
                else
                    field.EnclosingType

            let irArg, argTy = importExpression cenv env (Some expectedArgTy) ilArg
            let irArg, _ = env.HandleReceiver(cenv, expectedArgTy, irArg, argTy, false)
            let irField = OlyIRField(cenv.EmitField(field), field)
            let resultTy = createByReferenceRuntimeType irByRefKind field.Type
            O.LoadFieldAddress(irField, irArg, irByRefKind, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.LoadFromAddress(ilArg) ->
            let irArg, resultTy = importExpression cenv env None ilArg
            let resultTy =
                if resultTy.IsByRef_t then
                    resultTy.TypeArguments[0]
                else
                    failwith "Expected ByRef."
            O.LoadFromAddress(irArg, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.NewTuple(ilElementTys, ilArgs, ilNameHandles) ->
            let elementTys = cenv.ResolveTypes(env.EnclosingTypeParameterCount, env.ILAssembly, ilElementTys, env.GenericContext)
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

        | OlyILOperation.NewMutableArray(ilElementTy, ilSizeArgExpr) ->
            let elementTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilElementTy, env.GenericContext)
            let irSizeArgExpr = importArgumentExpression cenv env RuntimeType.Int32 ilSizeArgExpr
                
            let emittedElementTy = cenv.EmitType(elementTy)
            let resultTy = RuntimeType.Array(elementTy, 1, false)

            O.NewMutableArray(emittedElementTy, irSizeArgExpr, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.NewArray(ilElementTy, ilKind, ilArgExprs) ->
            let elementTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilElementTy, env.GenericContext)
            let irArgExprs =
                ilArgExprs
                |> ImArray.map (fun ilArgExpr -> importArgumentExpression cenv env elementTy ilArgExpr)
                
            let emittedElementTy = cenv.EmitType(elementTy)
            let resultTy = RuntimeType.Array(elementTy, 1, false)

            let kind =
                match ilKind with
                | OlyILArrayKind.Immutable ->
                    OlyIRArrayKind.Immutable
                | OlyILArrayKind.Mutable ->
                    OlyIRArrayKind.Mutable
            O.NewArray(emittedElementTy, kind, irArgExprs, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.NewRefCell(ilElementTy, ilArg) ->
            let elementTy = cenv.ResolveType(env.EnclosingTypeParameterCount, env.ILAssembly, ilElementTy, env.GenericContext)
            let irArg = importArgumentExpression cenv env elementTy ilArg
                
            let emittedElementTy = cenv.EmitType(elementTy)
            let resultTy = RuntimeType.ReferenceCell(elementTy)
            O.NewRefCell(emittedElementTy, irArg, cenv.EmitType(resultTy)) |> asExpr, resultTy

        | OlyILOperation.CallIndirect(ilFunArg, ilArgs) ->
            let irFunArg, funArgTy = importExpression cenv env None ilFunArg

            let argTys, returnTy =
                match funArgTy with
                | RuntimeType.Function(argTys, returnTy)
                | RuntimeType.NativeFunctionPtr(_, argTys, returnTy) ->
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
                irArgs[0], enclosingTy.Extends[0]
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
            let func = resolveFunction ilFuncInst
            assert(if func.Flags.IsStatic then ilArgs.Length = func.Parameters.Length else ilArgs.Length = func.Parameters.Length + 1)
            let irArgs = resolveFunctionArgs func ilArgs false
            handleCall func irArgs false

        | OlyILOperation.CallVirtual(ilFuncInst, ilArgs) ->
            let func = resolveFunction ilFuncInst
            assert(if func.Flags.IsStatic then ilArgs.Length = func.Parameters.Length else ilArgs.Length = func.Parameters.Length + 1)
            let irArgs = resolveFunctionArgs func ilArgs true
            handleCall func irArgs true

        | OlyILOperation.Witness _ ->
            failwith "Invalid witness."

let importExpression (cenv: cenv<'Type, 'Function, 'Field>) (env: env<'Type, 'Function, 'Field>) (expectedTyOpt: RuntimeType option) (ilExpr: OlyILExpression) : E<'Type, 'Function, 'Field> * RuntimeType =
    let (irExpr, actualTy) as result = 
#if DEBUG
        System.Threading.Tasks.Task.Run(fun () ->
            importExpressionAux cenv env expectedTyOpt ilExpr
        ).Result
#else
        importExpressionAux cenv env expectedTyOpt ilExpr
#endif

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

#if DEBUG
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

#if DEBUG
    OlyAssert.False(argTy.IsTypeExtension)
    OlyAssert.False(argTy.IsModule)
#endif

    if argTy.Formal = expectedArgTy.Formal then
        if argTy <> expectedArgTy then
            failwith $"Runtime Error: Expected type '{expectedArgTy.Name}' but got '{argTy.Name}'."
        irArg
    else
        if expectedArgTy.IsVoid_t && argTy.IsUnit_t then
            E.Operation(NoRange, O.Ignore(irArg, cenv.EmittedTypeVoid))
        elif expectedArgTy.IsObjectType && argTy.IsTypeVariable then
            E.Operation(NoRange, O.Box(irArg, cenv.EmitType(expectedArgTy)))
        
        elif subsumesType expectedArgTy argTy then
            if argTy.IsAnyStruct && not(expectedArgTy.IsAnyStruct) then
                E.Operation(NoRange, O.Box(irArg, cenv.EmitType(expectedArgTy)))
            else
                // No need to upcast for newtypes if its extending type is the same as the expected type.
                if argTy.IsNewtype && argTy.Extends[0].Formal = expectedArgTy.Formal then
                    irArg
                else
                    E.Operation(NoRange, O.Upcast(irArg, cenv.EmitType(expectedArgTy)))
        elif argTy.IsObjectType && not(expectedArgTy.IsObjectType) then
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
                irArg
                // TODO: Add this check.
                //failwith $"Type {argTy.Name} is not a sub-type of {expectedArgTy.Name}."

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
        (witnesses: RuntimeWitness imarray)
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
            |> ImArray.map (fun x -> x.Substitute(genericContext))
        enclosingTy.Apply(tyArgs)

    let funcTyArgs =
        func.TypeArguments
        |> ImArray.map (fun x -> x.Substitute(genericContext))

    let func = func.MakeInstance(enclosingTy, funcTyArgs).SetWitnesses(witnesses)
    let enclosingTy = enclosingTy.StripExtension()

    let instanceTy =
        if enclosingTy.IsAnyStruct then
            let irByRefKind =
                if func.IsMutable then
                    OlyIRByRefKind.ReadWrite
                else
                    OlyIRByRefKind.Read
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

    vm.ImportFunctionBody(
        func,
        ilAsm, 
        ilFuncBody, 
        argTys, 
        func.ReturnType, 
        genericContext, 
        func.Witnesses
    )

[<Sealed>]
type OlyRuntime<'Type, 'Function, 'Field>(emitter: IOlyRuntimeEmitter<'Type, 'Function, 'Field>) as this =

    let assemblies = ConcurrentDictionary<OlyILAssemblyIdentity, RuntimeAssembly<'Type, 'Function, 'Field>>(OlyILAssemblyIdentity.Comparer)

    let mutable isEmittingTypeDefinition = false
    let delayed = ConcurrentQueue()

    let inlineFunctionBodyCache: LruCache<RuntimeFunction, Lazy<OlyIRFunctionBody<'Type, 'Function, 'Field>>> = LruCache(64)

    let primitiveTypes = ConcurrentDictionary<RuntimeType, RuntimeType>()
    let addPrimitiveType primTy ty =
        if primitiveTypes.TryAdd(primTy, ty) |> not then
            failwith "Primitive type already exists."
        primitiveTypes[primTy] <- ty
    let tryGetPrimitiveType primTy =
        match primitiveTypes.TryGetValue primTy with
        | true, ty -> ValueSome ty
        | _ -> ValueNone

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
            let implTy = this.ResolveType(ilEntInst.TypeArguments.Length, ilAsm, ilEntInst.AsType, genericContext)
            if implTy.IsIntrinsic then
                OlyAssert.True(ilSpecificAbstractFuncInstOpt.IsNone)
                RuntimeWitness(index, implTy, implTy, None)
            else
                let ty = genericContext.GetErasedTypeArgument(index, ilKind)
                let funcOpt =
                    ilSpecificAbstractFuncInstOpt
                    |> Option.map (fun x -> this.ResolveFunction(ilAsm, x, GenericContext.Default))
                RuntimeWitness(index, ty, implTy, funcOpt)

    let trySolveWitness enclosingTyParCount ilAsm ilFuncSpecHandle enclosing name (funcTyArgs: RuntimeType imarray) (genericContext: GenericContext) (passedWitnesses: RuntimeWitness imarray) =
        match enclosing with
        | RuntimeEnclosing.Witness(_, _, Some witness) ->
            let tyParIndex = witness.TypeParameterIndex
            let enclosingTy = witness.Type
            let tyExt = witness.TypeExtension
            let fixedGenericContext = genericContext.Set(tyExt.TypeArguments, funcTyArgs)

            let witnessFormalFuncs =
                passedWitnesses
                |> ImArray.filter (fun witness -> witness.TypeParameterIndex = tyParIndex && witness.Type = enclosingTy)
                |> ImArray.map (fun witness ->
                    this.FindImmediateOverridenFunctionDefinitions(enclosingTyParCount, ilAsm, ilFuncSpecHandle, witness.TypeExtension, funcTyArgs, genericContext)
                    |> ImArray.map (fun func -> (witness.TypeExtension, func))
                )
                |> Seq.concat
                |> ImArray.ofSeq

            if witnessFormalFuncs.IsEmpty then
                None
            elif witnessFormalFuncs.Length > 1 then
                failwith $"Multiple witness functions of '{name}' are found."
            else
                let enclosingTy, witnessFormalFunc = witnessFormalFuncs.[0]
                if enclosingTy.TypeArguments.IsEmpty && witnessFormalFunc.TypeArguments.IsEmpty then
                    witnessFormalFunc |> Some
                else
                    let witnessFuncTyArgs =
                        witnessFormalFunc.TypeArguments
                        |> ImArray.map (fun x -> x.Substitute(fixedGenericContext))

                    witnessFormalFunc.MakeInstance(enclosingTy, witnessFuncTyArgs)
                    |> Some
        | _ ->
            None

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
            let ilEntDef = asm.ilAsm.GetEntityDefinition(enclosingTy1.ILEntityDefinitionHandle)
            let genericContext2 =
                if genericContext.IsErasing then
                    GenericContext.CreateErasing(enclosingTy1.TypeArguments.AddRange(funcTyArgs))
                else
                    GenericContext.Create(enclosingTy1.TypeArguments.AddRange(funcTyArgs))
            let enclosingTyParCount2 = ilEntDef.FullTypeParameterCount

            let funcs =
                ilEntDef.FunctionHandles
                |> ImArray.choose (fun ilFuncDefHandle2 ->
                    let ilFuncDef2 = asm.ilAsm.GetFunctionDefinition(ilFuncDefHandle2)
                    let ilFuncSpec2 = asm.ilAsm.GetFunctionSpecification(ilFuncDef2.SpecificationHandle)
                    if this.AreFunctionSpecificationsEqual(enclosingTyParCount1, ilAsm1, ilFuncSpec1, genericContext1, enclosingTyParCount2, asm.ilAsm, ilFuncSpec2, genericContext2) then
                        this.ResolveFunctionDefinition(enclosingTy1.Formal, ilFuncDefHandle2)
                        |> Some
                    else
                        None
                )
        
            funcs, funcTyArgs

        tryResolve enclosingTy1

    let resolveFunction ilAsm1 (ilFuncSpec1: OlyILFunctionSpecification) ilFuncTyArgs (enclosing: RuntimeEnclosing) genericContext =
        let enclosingTyParCount1 = enclosing.TypeParameters.Length
        let funcTyArgs =
            ilFuncTyArgs
            |> ImArray.map (fun x -> this.ResolveType(enclosingTyParCount1, ilAsm1, x, genericContext))
        let funcs, funcTyArgs = 
            tryResolveFunction ilAsm1 (ilFuncSpec1: OlyILFunctionSpecification) funcTyArgs enclosing.AsType genericContext
        if funcs.IsEmpty then
            let name = ilAsm1.GetStringOrEmpty(ilFuncSpec1.NameHandle)
            failwith $"Unable to find function definition for '{name}' on '{enclosing.AsType.Name}'."
        elif funcs.Length > 1 then
            let name = ilAsm1.GetStringOrEmpty(ilFuncSpec1.NameHandle)
            failwith $"Too many function definitions found for '{name}'."
        else
            let funcTyArgs =
                // We definitly need to do this.
                // Erase function type arguments via resolving the function.
                // Then substitute function type arguments.
                funcTyArgs
                |> ImArray.map (fun tyArg ->
                    tyArg.Substitute(genericContext)
                )

            funcs[0].MakeInstance(enclosing.AsType, funcTyArgs)

    let resolveWitnesses ilAsm ilWitnesses (funcTyArgs: _ imarray) (passedWitnesses: _ imarray) (genericContext: GenericContext) =
        let witnesses =
            let fixedGenericContext =
                // TODO: This could add support for witness resolving for type parameters on types.
                //let funcTyArgs =
                //    if func.Flags.IsConstructor then
                //        func.EnclosingType.TypeArguments
                //    else
                //        funcTyArgs
                if genericContext.IsErasingFunction || genericContext.FunctionTypeArguments.IsEmpty then
                    genericContext.AddErasingFunctionTypeArguments(funcTyArgs)
                else                     
                    genericContext.AddFunctionTypeArguments(funcTyArgs)
            ilWitnesses
            |> ImArray.map (fun x -> 
                resolveWitness ilAsm x fixedGenericContext
            )

        witnesses.AddRange(passedWitnesses)
        |> Seq.distinct
        |> ImArray.ofSeq

    let findFormalFunctionsByTypeAndFunctionSignature (targetTy: RuntimeType) (targetFunc: RuntimeFunction) =
        let ty =
            if targetTy.IsPrimitive then
                match tryGetPrimitiveType targetTy with
                | ValueSome ty -> ty
                | _ -> targetTy
            else
                targetTy
        if ty.IsBuiltIn then ImArray.empty
        else

        let asm = assemblies.[ty.AssemblyIdentity]
        let ilEntDef = asm.ilAsm.GetEntityDefinition(ty.ILEntityDefinitionHandle)

        let funcs =
            ilEntDef.FunctionHandles
            |> ImArray.choose (fun ilFuncDefHandle2 ->
                let ilFuncDef2 = asm.ilAsm.GetFunctionDefinition(ilFuncDefHandle2)
                let ilFuncSpec2 = asm.ilAsm.GetFunctionSpecification(ilFuncDef2.SpecificationHandle)
                let name = asm.ilAsm.GetStringOrEmpty(ilFuncSpec2.NameHandle)
                if 
                    name = targetFunc.Name &&
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
            let elementTy = this.ResolveType(0, ilAsm, ilTy, GenericContext.Default)
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

    let expandFields isGenericsErased (ty: RuntimeType) =
        let fields =
            // Variadic expansion of fields.
            match ty.TryGetVariadicTypeArgument() with
            | ValueSome(variadicTyArg) ->
                if not isGenericsErased then
                    failwith "Expected generic erasure for variadics."
                let fields = ty.Fields
                if fields.IsEmpty then
                    ImArray.empty
                else
                    let lastField = fields[fields.Length - 1]
                    if lastField.Type = variadicTyArg then
                        match lastField.Type with
                        | RuntimeType.Tuple(expandingFieldTys, _) ->
                            let headFields = fields.RemoveAt(fields.Length - 1)
                            let tailFields =
                                expandingFieldTys
                                |> ImArray.mapi (fun i fieldTy ->
                                    { lastField with
                                        Formal = lastField
                                        Name = lastField.Name + i.ToString()
                                        Type = fieldTy
                                    }

                                )
                            headFields.AddRange(tailFields)
                        | _ ->
                            fields
                    else
                        fields
            | _ ->
                ty.Fields
        fields

    let rec emitField (field: RuntimeField) =
        let asm = assemblies.[field.AssemblyIdentity]

        match field.EnclosingType.TryGetVariadicTypeArgument() with
        | ValueSome(variadicTyArg) when field.Formal.Type = variadicTyArg ->
            let expansions =
                match asm.FieldVariadicDefinitionCache.TryGetValue field.ILFieldDefinitionHandle with
                | true, expansions -> expansions
                | _ ->
                    let expansions = ConcurrentDictionary()
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

        match asm.FieldDefinitionCache.TryGetValue field.ILFieldDefinitionHandle with
        | true, (_, emitted) ->
            // fast path
            match emitted.TryGetValue field.EnclosingType.TypeArguments with
            | ValueSome res -> res
            | _ -> emitFieldNoCache asm emitted field
        | _ ->
            failwithf "Field definition not cached: %A" field.Name

    and emitFieldNoCache (asm: RuntimeAssembly<_, _, _>) (emitted: RuntimeTypeArgumentListTable<_, _, _, _>) (field: RuntimeField) =
        if field.EnclosingType.IsEnum && not(field.ILConstantValueOption.IsSome && field.IsStatic) then
            failwith "Member functions on an 'enum' is not allowed."

        // It's very important we emit the enclosing and field type before
        // we cache the emitted field. Without this, we could emit duplicate fields.
        let enclosingTy = this.EmitType(field.EnclosingType)
        let fieldTy = this.EmitType(field.Type)
        match emitted.TryGetValue field.EnclosingType.TypeArguments with
        | ValueSome res -> res
        | _ ->
            let irAttrs = emitAttributes asm.ilAsm field.Attributes

            let constantOpt =
                field.ILConstantValueOption
                |> Option.map (fun x ->
                    this.EmitILConstant(asm.ilAsm, x, GenericContext.Default) |> fst
                )

            let res = 
                this.Emitter.EmitField(
                    enclosingTy,
                    field.Flags, 
                    field.Name,
                    fieldTy,
                    irAttrs,
                    constantOpt
                )
            emitted.[field.EnclosingType.TypeArguments] <- res
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

    and emitTypeDefinition (tyDef: RuntimeType) =
        let asm = assemblies.[tyDef.AssemblyIdentity]
        let isGenericsErased = tyDef.CanGenericsBeErased

        if not isGenericsErased && not tyDef.IsFormal then
            failwith "Expected formal type."
        
        match asm.EntityDefinitionCache.TryGetValue(tyDef.ILEntityDefinitionHandle) with
        | true, (_, emitted) ->
            match emitted.TryGetValue tyDef.TypeArguments with
            | ValueSome res -> res
            | _ ->
                let mustDelayFuncs = isEmittingTypeDefinition
                isEmittingTypeDefinition <- true

                if isGenericsErased then
                    tyDef.TypeArguments
                    |> ImArray.iter (function
                        | RuntimeType.Variable _
                        | RuntimeType.HigherVariable _ -> failwith "Type variable cannot be erased."
                        | _ -> ()
                    )

                let ilAsm = asm.ilAsm
        
                let enclosingChoice =
                    match tyDef.Enclosing with
                    | RuntimeEnclosing.Namespace(path) -> Choice1Of2(path)
                    | _ -> Choice2Of2(this.EmitType(tyDef.Enclosing.AsType))
        
                let ilEntDef = ilAsm.GetEntityDefinition(tyDef.ILEntityDefinitionHandle)
                let tyPars =
                    if isGenericsErased then
                        ImArray.empty
                    else
                        tyDef.TypeParameters
                        |> ImArray.map (fun tyPar -> OlyIRTypeParameter(tyPar.Name))

                let inheritTys =
                    if tyDef.IsNewtype then
                        OlyAssert.Equal(1, tyDef.Extends.Length)
                        ImArray.empty
                    else
                        tyDef.Extends
                        |> ImArray.map (fun x ->
                            this.SubscribeType(x, tyDef)
                            this.EmitType(x)
                        )

                let implementTys = 
                    if tyDef.IsNewtype then
                        OlyAssert.Equal(0, tyDef.Implements.Length)
                        ImArray.empty
                    else
                        tyDef.Implements
                        |> ImArray.map (fun x ->
                            this.SubscribeType(x, tyDef)
                            this.EmitType(x)
                        )

                let flags =
                    if isGenericsErased && not tyDef.TypeParameters.IsEmpty then
                        OlyIRTypeFlags.GenericsErased
                    else
                        OlyIRTypeFlags.None

                let flags =
                    if tyDef.IsExported then
                        flags ||| OlyIRTypeFlags.Exported
                    else
                        flags

                match emitted.TryGetValue tyDef.TypeArguments with
                | ValueSome res -> res
                | _ ->

                let kind = ilEntDef.Kind

                let irAttrs = 
                    match tyDef with
                    | RuntimeType.Entity(ent) ->
                        emitAttributes ilAsm ent.Attributes
                    | _ ->
                        ImArray.empty

                let res = this.Emitter.EmitTypeDefinition(enclosingChoice, kind, flags, tyDef.Name, tyPars, inheritTys, implementTys, irAttrs)
                emitted.[tyDef.TypeArguments] <- res
                if not mustDelayFuncs then
                    isEmittingTypeDefinition <- false

                let fields = expandFields isGenericsErased tyDef
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
                                match tyDef with
                                | RuntimeType.Entity(ent) -> 
                                    ent.StaticConstructor <- Some func
                                | _ ->
                                    ()
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

                // Properties are only emitted if the runtime target supports it and the type is exported.
                if tyDef.IsExported then
                    ilEntDef.PropertyDefinitionHandles
                    |> ImArray.iter (fun ilPropDefHandle ->
                        let ilPropDef = ilAsm.GetPropertyDefinition(ilPropDefHandle)

                        let getterOpt =
                            if ilPropDef.Getter.IsNil then
                                None
                            else
                                this.ResolveFunctionDefinition(tyDef, ilPropDef.Getter)
                                |> this.EmitFunction
                                |> Some

                        let setterOpt =
                            if ilPropDef.Setter.IsNil then
                                None
                            else
                                this.ResolveFunctionDefinition(tyDef, ilPropDef.Setter)
                                |> this.EmitFunction
                                |> Some

                        let irAttrs =
                            ilPropDef.Attributes
                            |> ImArray.choose (fun ilAttr ->
                                this.TryResolveAttribute(ilAsm, ilAttr, GenericContext.Default, ImArray.empty)
                            )
                            |> emitAttributes ilAsm

                        emitter.EmitExportedProperty(
                            this.EmitType(tyDef),
                            ilAsm.GetStringOrEmpty(ilPropDef.NameHandle),
                            this.EmitType(this.ResolveType(0, ilAsm, ilPropDef.Type, GenericContext.Default)),
                            irAttrs,
                            getterOpt,
                            setterOpt                           
                        )
                    )
        
                res
        | _ ->
            failwithf "Entity definition not cached: %A" tyDef.Name

    and emitExternalType (ty: RuntimeType) externalPlatform externalPath externalName =
        let asm = assemblies.[ty.AssemblyIdentity]
        if not ty.IsExternal then
            failwith "Type is not external."
        
        match asm.EntityDefinitionCache.TryGetValue(ty.ILEntityDefinitionHandle) with
        | true, (_, emitted) ->
            match emitted.TryGetValue ImArray.empty with
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
                let flags = OlyIRTypeFlags.None

                let res = this.Emitter.EmitExternalType(externalPlatform, externalPath, externalName, enclosingChoice, ilEntDef.Kind, flags, ty.Name, tyParCount)
                emitted.[ImArray.empty] <- res

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
            let formalTy = emitType false ty.Formal
            let emittedFullTyArgs = fullTyArgs |> ImArray.map (fun x -> this.EmitTypeArgument(x))
            let res = this.Emitter.EmitTypeGenericInstance(formalTy, emittedFullTyArgs)
            emitted.[fullTyArgs] <- res
            res

    and emitType (isTopLevelTyArg: bool) (ty: RuntimeType) =
        if ty.IsBuiltIn then
            match ty with
            | RuntimeType.ForAll _ ->
                if isTopLevelTyArg then
                    raise(System.NotImplementedException("Top-level ForAll type."))
                else
                    failwith "Invalid use of ForAll type."

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
            | RuntimeType.BaseStruct -> this.TypeBaseStruct.Value
            | RuntimeType.BaseStructEnum -> this.TypeBaseStructEnum.Value
            | RuntimeType.BaseAttribute -> this.TypeBaseAttribute.Value
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
            | RuntimeType.Function(argTys, returnTy) ->
                this.Emitter.EmitTypeFunction(argTys |> ImArray.map (emitType false), emitType false returnTy)
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

            | RuntimeType.Entity _ -> failwith "Invalid type."

        elif ty.IsNewtype then
            emitType false (ty.Extends[0])

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
                let enclosingTyArgs =
                    targetFunc.EnclosingType.TypeArguments
                    |> ImArray.map (fun x -> x.Substitute(genericContext))
                let funcTyArgs =
                    targetFunc.TypeArguments
                    |> ImArray.map (fun x -> x.Substitute(genericContext))

                let canErase = canPossiblyEraseGenericFunction func targetFunc 
                let genericContext = createGenericContextFromFunction canErase targetFunc
                let genericContext = genericContext.Set(enclosingTyArgs, funcTyArgs)
                this.TryResolveFunctionBody(targetFunc, genericContext) |> Option.map (fun x -> x.Value)
            )
            (fun (envFunc, func) ->
                this.EmitFunctionFromEnvironment(envFunc, func)
            )
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

    let emitFunctionDefinition (enclosingTy: RuntimeType) (func: RuntimeFunction) (genericContext: GenericContext) =
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

        if isErasingFunc && func.IsFormal && (not func.TypeArguments.IsEmpty) then
            failwith "Unexpected formal function."

        if not isErasingFunc && func.Kind = RuntimeFunctionKind.Instance then
            failwith "Unexpected function instance."

        match asm.FunctionDefinitionCache.TryGetValue(func.ILFunctionDefinitionHandle) with
        | true, (_, emitted) ->
            let key = struct(isErasingFunc, enclosingTy, funcTyArgs, witnesses, true)
            match emitted.TryGetValue(key) with
            | ValueSome(emittedFunc) -> 
                emittedFunc
            | _ ->
                if func.EnclosingType.IsEnum then
                    failwith "Member functions on an 'enum' are not allowed."

                let enclosingTyParCount = enclosingTy.TypeArguments.Length

                let ilAsm = asm.ilAsm

                let tyPars = 
                    if isErasingFunc then
                        ImArray.empty
                    else
                        func.TypeParameters |> ImArray.map (fun tyPar -> OlyIRTypeParameter(tyPar.Name))

                let pars = 
                    func.Parameters 
                    |> ImArray.map (fun par -> 
                        OlyIRParameter(par.Name, this.EmitType(par.Type), true)
                    )

                let overrides =
                    func.Overrides 
                    |> Option.map (fun x -> 
                        // We must pass witnesses to the overriden function.
                        if x.EnclosingType.CanGenericsBeErased && isErasingFunc then
                            let enclosingTy = x.EnclosingType.Substitute(genericContext)
                            let genericContext = GenericContext.Create(enclosingTy.TypeArguments, funcTyArgs)
                            let funcTyArgs =
                                x.TypeArguments
                                |> ImArray.map (fun x -> x.Substitute(genericContext))
                            this.EmitFunction(x.Formal.MakeInstance(enclosingTy, funcTyArgs).SetWitnesses(witnesses))
                        else
                            if not witnesses.IsEmpty then
                                failwith "Function overrides with witnesses must be erased."
                            this.EmitFunction(x.Formal.MakeReference(x.EnclosingType))
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

                        let fakeReceiverTy =
                            let extendsTy = func.EnclosingType.Extends[0]
                            if extendsTy.IsAnyStruct then
                                createByReferenceRuntimeType OlyIRByRefKind.Read extendsTy
                            else
                                extendsTy

                        let pars =
                            pars
                            |> ImArray.prependOne (OlyIRParameter("", this.EmitType(fakeReceiverTy), false))

                        flags.SetStatic(), pars
                    else
                        flags, pars

                let emittedFunc = this.Emitter.EmitFunctionDefinition(externalInfoOpt, emittedEnclosingTy, flags, func.Name, tyPars, pars, returnTy, overrides, sigKey, irAttrs)
                emitted.[key] <- emittedFunc

                if func.HasILFunctionBody then
                    emitFunctionBody func emittedFunc genericContext
                else
                    if not func.Flags.IsAbstract && not func.Flags.IsExternal then
                        failwith "Expected function body."

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
                                        failwithf "Function not found: %A" func.Name
                                    elif funcs.Length > 1 then
                                        failwithf "Duplicate functions found: %A" func.Name
                                    else
                                        let foundFunc = funcs.[0]
                                        this.EmitFunction(foundFunc)
                                        |> ignore

                        elif funcs.Length > 1 then
                            failwithf "Duplicate functions found: %A" func.Name
                        else
                            let overridenFunc = funcs.[0]
                            if (overridenFunc.Enclosing.AsType.IsExternal) || (ty.TypeArguments.IsEmpty && funcTyArgs.IsEmpty) then
                                this.EmitFunction(overridenFunc) |> ignore
                            else
                                let funcInst =
                                    if isErasingFunc then
                                        overridenFunc.MakeInstance(ty, funcTyArgs)
                                    else
                                        overridenFunc.MakeReference(ty)
                                let funcInst = funcInst.SetWitnesses(witnesses)
                                this.EmitFunction(funcInst) |> ignore
                    )

                emittedFunc
        | _ ->
            failwithf "Function definition not cached: %A" func.Name

    let rec emitFunction canErase (func: RuntimeFunction) (irCustomBody: _ option) =
        let genericContext = createGenericContextFromFunction canErase func

        if func.IsFormal then
            let enclosingTy =
                let enclosingTy = func.Enclosing.AsType
                if genericContext.IsErasingType then enclosingTy
                else enclosingTy.Formal
            emitFunctionDefinition enclosingTy func genericContext
        else
            if irCustomBody.IsSome then
                failwith "Custom IR bodies are only allowed for formal function definitions."

            if func.Enclosing.TypeArguments.IsEmpty && func.TypeArguments.IsEmpty then
                emitFunction false func.Formal None
            elif not genericContext.IsErasingFunction then
                
                if func.IsFormal then
                    failwith "Unexpected formal function."

                let asm = assemblies.[func.AssemblyIdentity]

                match asm.FunctionDefinitionCache.TryGetValue func.ILFunctionDefinitionHandle with
                | true, (_, emitted) ->
                    let enclosingTy = func.EnclosingType
                    let key = struct(false, enclosingTy, func.TypeArguments, func.Witnesses, false)
                    match emitted.TryGetValue(key) with
                    | ValueSome(emittedFunc) -> emittedFunc
                    | _ ->
                        let emittedFuncDef = 
                            let enclosingTy = func.EnclosingType
                            if enclosingTy.IsExternal then
                                emitFunctionDefinition func.EnclosingType.Formal func.Formal genericContext
                            else
                                let enclosingTy =
                                    let enclosingTy = func.Enclosing.AsType
                                    if genericContext.IsErasingType then enclosingTy
                                    else enclosingTy.Formal
                                emitFunctionDefinition enclosingTy func.Formal genericContext

                        let emittedFunc = 
                            match func.Kind with
                            | RuntimeFunctionKind.Formal -> failwith "Unexpected formal function."
                            | RuntimeFunctionKind.Instance ->
                                let funcTyArgs = func.TypeArguments |> ImArray.map (fun x -> this.EmitTypeArgument(x))
                                this.Emitter.EmitFunctionInstance(this.EmitType(func.EnclosingType), emittedFuncDef, funcTyArgs)
                            | RuntimeFunctionKind.Reference ->
                                this.Emitter.EmitFunctionReference(this.EmitType(func.EnclosingType), emittedFuncDef)

                        emitted.[key] <- emittedFunc
                        emittedFunc
                | _ ->
                    failwithf "Function definition not cached: %A" func.Name
            else
                emitFunctionDefinition func.Enclosing.AsType func genericContext

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
    member val TypeBaseStruct: _ Lazy  = lazy emitter.EmitTypeBaseStruct()
    member val TypeBaseStructEnum: _ Lazy  = lazy emitter.EmitTypeBaseStructEnum()
    member val TypeBaseAttribute: _ Lazy  = lazy emitter.EmitTypeBaseAttribute()

    member internal this.Assemblies: ConcurrentDictionary<OlyILAssemblyIdentity, RuntimeAssembly<'Type, 'Function, 'Field>> = assemblies

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
            EntityDefinitionCache = ConcurrentDictionary()
            EntityInstanceCache = ConcurrentDictionary()
            entRefCache = ConcurrentDictionary()
            FunctionDefinitionCache = ConcurrentDictionary()
            FieldDefinitionCache = ConcurrentDictionary()
            FieldVariadicDefinitionCache = ConcurrentDictionary()
            RuntimeTypeInstanceCache = RuntimeTypeInstanceCache(this, ilAsm)
            RuntimeFieldReferenceCache = RuntimeFieldReferenceCache()
            TypesThatInheritOrImplementType = ConcurrentDictionary()
        }

        ilAsm.ForEachPrimitiveType(fun (ilTy, ilEntDefHandle) ->
            match ilTy with
            | OlyILTypeEntity _
            | OlyILTypeVariable _
            | OlyILTypeHigherVariable _ ->
                failwith "Invalid primitive IL type."
            | _ ->
                let ty = this.ResolveTypeDefinition(ilAsm, ilEntDefHandle)
                addPrimitiveType (this.ResolveType(0, ilAsm, ilTy, GenericContext.Default)) ty
        )

    member this.FindEntryPoint() =
        let entryPointOpt =
            assemblies
            |> Seq.choose (fun pair ->
                let ilAsm = pair.Value.ilAsm
                match ilAsm.EntryPoint with
                | Some(ilEnclosingTy, ilFuncDefHandle) ->
                    let ty = this.ResolveType(0, ilAsm, ilEnclosingTy, GenericContext.Default)
                    Some(this.ResolveFunctionDefinition(ty.Formal, ilFuncDefHandle))
                | _ ->
                    None
            )
            |> Seq.tryExactlyOne
        match entryPointOpt with
        | Some entryPoint -> entryPoint
        | _ -> failwith "Unable to find entry point."

    member this.FindFormalFunctionsByTypeAndFunctionSignature(ty: RuntimeType, func: RuntimeFunction) : RuntimeFunction imarray =
        findFormalFunctionsByTypeAndFunctionSignature ty func

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
            let attrs =
                ilFieldDef.Attributes
                |> ImArray.choose (fun x -> this.TryResolveAttribute(ilAsm, x, GenericContext.Default, ImArray.empty))
                
            let ilConstOpt =
                match ilFieldDef with
                | OlyILFieldConstant(constant=ilConst) -> Some ilConst
                | _ -> None

            let ilFieldFlags =
                if not(ilFieldDef.MemberFlags.HasFlag(OlyILMemberFlags.Static)) && enclosingTy.IsAnyStruct then
                    // Instance fields on structs are always considered mutable.
                    ilFieldDef.Flags ||| OlyILFieldFlags.Mutable
                else
                    ilFieldDef.Flags

            let res =
                {
                    RuntimeField.Formal = Unchecked.defaultof<_>
                    RuntimeField.Name = ilAsm.GetStringOrEmpty(ilFieldDef.NameHandle)
                    RuntimeField.Type = this.ResolveType(enclosingTy.TypeArguments.Length, ilAsm, ilFieldDef.Type, GenericContext.Default)
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
            let enclosingTy = (this.ResolveEnclosing(0, ilAsm, ilEnclosing, genericContext, ImArray.empty)).AsType
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
                { fields.[0] with EnclosingType = enclosingTy; Type = fields.[0].Type.Substitute(GenericContext.Create(enclosingTy.TypeArguments)) }
                |> asm.RuntimeFieldReferenceCache.Intern

    member this.ResolveFunctionDefinition(enclosingTy: RuntimeType, ilFuncDefHandle: OlyILFunctionDefinitionHandle) : RuntimeFunction =
        resolveFunctionDefinition enclosingTy ilFuncDefHandle

    member this.ResolveFunction(ilAsm: OlyILReadOnlyAssembly, ilFuncInst: OlyILFunctionInstance, genericContext: GenericContext, passedWitnesses: RuntimeWitness imarray) : RuntimeFunction =
        let vm = this
        match ilFuncInst with
        | OlyILFunctionInstance(ilEnclosing, ilFuncSpecHandle, ilFuncTyArgs, ilWitnesses) ->
            let enclosing = vm.ResolveEnclosing(0, ilAsm, ilEnclosing, genericContext, passedWitnesses)
            let ilFuncSpec = ilAsm.GetFunctionSpecification(ilFuncSpecHandle)
            let funcInst = vm.ResolveFunction(ilAsm, ilFuncSpec, ilFuncTyArgs, enclosing, genericContext)

            let witnessFuncOpt =
                vm.TryFindWitnessFunction(ilAsm, ilFuncSpecHandle, enclosing, funcInst, genericContext, passedWitnesses)

            let findFuncs (ty: RuntimeType) =
                vm.FindFormalFunctionsByTypeAndFunctionSignature(ty, funcInst)

            let witnessFuncOpt =
                let witnessFuncOpt2 =
                    match enclosing with
                    | RuntimeEnclosing.Witness(realTy, enclosingTy, None) ->
                        vm.TryFindWitnessFunctionByAbstractFunction(realTy, enclosingTy, funcInst, passedWitnesses)
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
                        let enclosingWithRealTy = RuntimeEnclosing.Type(realTy)
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
                                    failwithf "Function not found: %A" funcInst.Name
                                if funcs.Length > 1 then
                                    failwith "Too many functions"
                                funcs.[0]
                            else
                                funcInst
                        else
                            if funcs.Length > 1 then
                                failwith "Too many functions"
                            funcs.[0].MakeInstance(enclosingWithRealTy, funcInst.TypeArguments)
                    | _ ->
                        funcInst

            let passedAndFilteredWitnesses =
                vm.ResolveWitnesses(ilAsm, ilWitnesses, funcInst.TypeArguments, passedWitnesses, genericContext)

            if func.IsFormal then func
            else
                func.SetWitnesses(passedAndFilteredWitnesses)

    member _.ResolveFunction(ilAsm, ilFuncSpec, ilFuncTyArgs, enclosing, genericContext) =
        resolveFunction ilAsm ilFuncSpec ilFuncTyArgs enclosing genericContext

    member this.TryResolveAttribute(ilAsm: OlyILReadOnlyAssembly, ilAttr: OlyILAttribute, genericContext: GenericContext, passedWitnesses: RuntimeWitness imarray) =
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

    member this.ResolveFunction(ilAsm: OlyILReadOnlyAssembly, ilFuncRef: OlyILFunctionReference, genericContext: GenericContext) : RuntimeFunction =
        let enclosingTy = this.ResolveType(0, ilAsm, ilFuncRef.GetEnclosingType(), genericContext)
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

    member this.ResolveWitnesses(ilAsm, ilWitnesses, funcTyArgs, passedWitnesses, genericContext) =
        resolveWitnesses ilAsm ilWitnesses funcTyArgs passedWitnesses genericContext

    member private this.AreFunctionSpecificationsEqual(enclosingTyParCount1: int, ilAsm1: OlyILReadOnlyAssembly, ilFuncSpec1: OlyILFunctionSpecification, scopeTyArgs1: GenericContext, enclosingTyParCount2: int, ilAsm2: OlyILReadOnlyAssembly, ilFuncSpec2: OlyILFunctionSpecification, scopeTyArgs2: GenericContext) =
        ilFuncSpec1.IsInstance = ilFuncSpec2.IsInstance &&
        ilFuncSpec1.Parameters.Length = ilFuncSpec2.Parameters.Length &&
        ilFuncSpec1.TypeParameters.Length = ilFuncSpec2.TypeParameters.Length &&
        (
            let name1 = ilFuncSpec1.NameHandle |> ilAsm1.GetStringOrEmpty
            let name2 = ilFuncSpec2.NameHandle |> ilAsm2.GetStringOrEmpty
            if name1 = name2 then
                let returnTy1 = this.ResolveType(enclosingTyParCount1, ilAsm1, ilFuncSpec1.ReturnType, scopeTyArgs1)
                let returnTy2 = this.ResolveType(enclosingTyParCount2, ilAsm2, ilFuncSpec2.ReturnType, scopeTyArgs2)
                if returnTy1 = returnTy2 then
                    let parTys1 =
                        ilFuncSpec1.Parameters
                        |> ImArray.map (fun x -> this.ResolveType(enclosingTyParCount1, ilAsm1, x.Type, scopeTyArgs1))

                    let parTys2 =
                        ilFuncSpec2.Parameters
                        |> ImArray.map (fun x -> this.ResolveType(enclosingTyParCount2, ilAsm2, x.Type, scopeTyArgs2))

                    (parTys1, parTys2)
                    ||> ImArray.forall2 (=)
                else
                    false
            else
                false
        )

    member private this.AreILEnclosingsEqual(ilAsm1: OlyILReadOnlyAssembly, ilEnclosing1: OlyILEnclosing, ilAsm2: OlyILReadOnlyAssembly, ilEnclosing2: OlyILEnclosing) =
        match ilEnclosing1, ilEnclosing2 with
        | OlyILEnclosing.Witness(ilTy1, _), OlyILEnclosing.Witness(ilTy2, _) ->
            let ty1 = this.ResolveType(0, ilAsm1, ilTy1, GenericContext.Default)
            let ty2 = this.ResolveType(0, ilAsm2, ilTy2, GenericContext.Default)
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
                                let ty1 = this.ResolveType(0, ilAsm1, ilTy1, GenericContext.Default)
                                let ty2 = this.ResolveType(0, ilAsm2, ilTy2, GenericContext.Default)
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
                    if extendsTy.Formal = ty.Formal && subsumesType abstractTy x.TypeExtension then
                        // TODO: Checking the formals is a hack.
                        //    The reason why is because the 'ty' could be a type constructor where as the 'inherit ty' is never a type constructor.
                        if (extendsTy = ty) || (extendsTy.Formal = ty && ty.IsTypeConstructor) then
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
                            RuntimeWitness(x.TypeParameterIndex,
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

    member this.TryFindWitnessFunction(ilAsm, ilFuncSpecHandle, enclosing: RuntimeEnclosing, func: RuntimeFunction, genericContext: GenericContext, passedWitnesses: RuntimeWitness imarray) =
        match enclosing with
        | RuntimeEnclosing.Witness(_, abstractTy, _) ->
            let name = func.Name
            let genericContext =
                if genericContext.IsErasing then
                    GenericContext.CreateErasing(abstractTy.TypeArguments.AddRange(func.TypeArguments))
                else
                    GenericContext.Create(abstractTy.TypeArguments.AddRange(func.TypeArguments))

            match trySolveWitness abstractTy.TypeParameters.Length ilAsm ilFuncSpecHandle enclosing name func.TypeArguments genericContext passedWitnesses with
            | Some func -> Some func
            | _ -> None
        | _ ->
            None

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
                            findFormalFunctionsByTypeAndFunctionSignature x.TypeExtension abstractFunc
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
                let possibleFunc: RuntimeFunction = possibleFuncs[0]
                OlyAssert.True(possibleFunc.IsFormal)
                let enclosingTy = possibleFunc.EnclosingType.Apply(ty.TypeArguments)
                Some(possibleFunc.MakeInstance(enclosingTy, abstractFunc.TypeArguments))
            else
                failwith "Ambiguous specific abstract functions found."

        foundFuncOpt

    member this.ResolveEnclosing(enclosingTyParCount: int, ilAsm: OlyILReadOnlyAssembly, ilEnclosing: OlyILEnclosing, genericContext: GenericContext, witnesses) : RuntimeEnclosing =
        match ilEnclosing with
        | OlyILEnclosing.Entity(ilEntInst) ->
            this.ResolveType(ilEntInst.TypeArguments.Length, ilAsm, ilEntInst.AsType, genericContext)
            |> RuntimeEnclosing.Type
        | OlyILEnclosing.Namespace(path, _) ->
            RuntimeEnclosing.Namespace(path |> ImArray.map ilAsm.GetStringOrEmpty)
        | OlyILEnclosing.Witness(ilTy, ilEntInst) ->
            let ty = this.ResolveType(enclosingTyParCount, ilAsm, ilTy, genericContext)
            let abstractTy = this.ResolveType(ilEntInst.TypeArguments.Length, ilAsm, ilEntInst.AsType, genericContext)
            let witnessOpt = this.TryFindPossibleWitness(ty, abstractTy, witnesses)
            RuntimeEnclosing.Witness(ty, abstractTy, witnessOpt)

    member this.ResolveTypeDefinition(ilAsm: OlyILReadOnlyAssembly, ilEntDefOrRefHandle: OlyILEntityDefinitionOrReferenceHandle) : RuntimeType =
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

                let enclosing = this.ResolveEnclosing(0, ilAsm, ilEntDef.Enclosing, GenericContext.Default, ImArray.empty)

                let enclosingTyPars = enclosing.TypeParameters

                let tyPars =
                    ilEntDef.TypeParameters
                    |> ImArray.map (fun ilTyPar -> 
                        { 
                            Name = ilAsm.GetStringOrEmpty(ilTyPar.NameHandle)
                            Arity = ilTyPar.Arity
                            IsVariadic = ilTyPar.IsVariadic
                        } : RuntimeTypeParameter
                    )

                let fullTyPars = enclosingTyPars.AddRange(tyPars)

                let fullTyArgs =
                    fullTyPars
                    |> ImArray.mapi (fun i _ -> 
                        RuntimeType.Variable(i, OlyILTypeVariableKind.Type)
                    )

                let ent =
                    {
                        RuntimeEntity.Enclosing = enclosing
                        RuntimeEntity.Name = ilAsm.GetStringOrEmpty(ilEntDef.NameHandle)
                        RuntimeEntity.TypeParameters = fullTyPars
                        RuntimeEntity.TypeArguments = fullTyArgs
                        RuntimeEntity.Extends = ImArray.empty
                        RuntimeEntity.Implements = ImArray.empty
                        RuntimeEntity.Formal = Unchecked.defaultof<_>
                        RuntimeEntity.Fields = ImArray.empty
                        RuntimeEntity.Attributes = ImArray.empty

                        RuntimeEntity.StaticConstructor = None

                        RuntimeEntity.ILAssembly = ilAsm
                        RuntimeEntity.ILEntityDefinitionHandle = ilEntDefOrRefHandle
                        RuntimeEntity.ILEntityKind = ilEntDef.Kind
                        RuntimeEntity.ILEntityFlags = ilEntDef.Flags
                    }
                ent.Formal <- ent
                let ty = RuntimeType.Entity(ent)
                asm.EntityDefinitionCache.[ilEntDefOrRefHandle] <- (ty, RuntimeTypeArgumentListTable())

                let extends =
                    ilEntDef.Extends
                    |> ImArray.map (fun x -> this.ResolveType(0, ilAsm, x, GenericContext.CreateErasing(fullTyArgs)))

                let extends =
                    // TODO: Handle enums.
                    if extends.IsEmpty then
                        if ent.IsClass then
                            ImArray.createOne RuntimeType.BaseObject
                        elif ent.IsAttribute then
                            ImArray.createOne RuntimeType.BaseAttribute
                        elif ent.IsAnyStruct then
                            ImArray.createOne RuntimeType.BaseStruct
                        else
                            extends
                    else
                        extends

                let implements =
                    ilEntDef.Implements
                    |> ImArray.map (fun x -> this.ResolveType(0, ilAsm, x, GenericContext.CreateErasing(fullTyArgs)))

                ent.Extends <- extends
                ent.Implements <- implements

                let fields =
                    ilEntDef.FieldDefinitionHandles
                    |> ImArray.mapi (fun i ilFieldDefHandle ->
                        this.ResolveField(RuntimeType.Entity(ent), ilAsm, i, ilFieldDefHandle)
                    )

                ent.Fields <- fields

                let attrs =
                    ilEntDef.Attributes
                    |> ImArray.choose (fun x -> this.TryResolveAttribute(ilAsm, x, GenericContext.Default, ImArray.empty))

                ent.Attributes <- attrs

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

                ent.StaticConstructor <- staticCtorOpt

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

    member this.ResolveType(enclosingTyParCount, ilAsm: OlyILReadOnlyAssembly, ilTy: OlyILType, genericContext: GenericContext) : RuntimeType =
        match ilTy with
        | OlyILTypeVariable(index, ilKind) when genericContext.CanErase(index, ilKind) ->
            genericContext.GetErasedTypeArgument(index, ilKind)
        | OlyILTypeHigherVariable(index, ilTyArgs, ilKind) when genericContext.CanErase(index, ilKind) ->
            let tyArgs =
                ilTyArgs
                |> ImArray.map (fun x -> this.ResolveType(x.TypeArguments.Length, ilAsm, x, genericContext))

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
#if DEBUG
                OlyAssert.Equal(ty.TypeParameters.Length, ilTyArgs.Length)
#endif
                if ilTyArgs.IsEmpty then
                    ty
                else
                    let tyArgs = ilTyArgs |> ImArray.map (fun x -> this.ResolveType(enclosingTyParCount, ilAsm, x, genericContext))
                    let asm = assemblies.[ty.AssemblyIdentity]
                    asm.RuntimeTypeInstanceCache.GetOrCreate(ty.ILEntityDefinitionHandle, tyArgs)
            | OlyILEntityConstructor(ilEntDefOrRefHandle) ->
                this.ResolveTypeDefinition(ilAsm, ilEntDefOrRefHandle)

        | OlyILTypeForAll(ilTyPars, ilInnerTy) ->
            let tyPars =
                ilTyPars
                |> ImArray.map (fun ilTyPar ->
                    { 
                        Name = ilAsm.GetStringOrEmpty(ilTyPar.NameHandle)
                        Arity = ilTyPar.Arity
                        IsVariadic = ilTyPar.IsVariadic
                    } : RuntimeTypeParameter
                )
            let innerTy = this.ResolveType(enclosingTyParCount, ilAsm, ilInnerTy, GenericContext.Default)
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
            | OlyILTypeBaseStruct -> RuntimeType.BaseStruct
            | OlyILTypeBaseAttribute -> RuntimeType.BaseAttribute
            | OlyILTypeBaseStructEnum -> RuntimeType.BaseStructEnum
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
            | OlyILTypeNativePtr(ilElementTy) -> RuntimeType.NativePtr(this.ResolveType(enclosingTyParCount, ilAsm, ilElementTy, genericContext))
            | OlyILTypeNativeFunctionPtr(ilCc, ilArgTys, ilReturnTy) ->
                let argTys = this.ResolveTypes(enclosingTyParCount, ilAsm, ilArgTys, genericContext)
                let returnTy = this.ResolveType(enclosingTyParCount, ilAsm, ilReturnTy, genericContext)
                RuntimeType.NativeFunctionPtr(ilCc, argTys, returnTy)
            | OlyILTypeTuple(ilTyArgs, ilNameHandles) ->
                let tyArgs =
                    ilTyArgs
                    |> ImArray.map (fun x -> this.ResolveType(enclosingTyParCount, ilAsm, x, genericContext))
                let names =
                    ilNameHandles
                    |> ImArray.map (fun x ->
                        ilAsm.GetStringOrEmpty(x)
                    )
                RuntimeType.Tuple(tyArgs, names)
            | OlyILTypeRefCell(ilElementTy) ->
                RuntimeType.ReferenceCell(this.ResolveType(enclosingTyParCount, ilAsm, ilElementTy, genericContext))
            | OlyILTypeArray(ilElementTy, rank, ilKind) ->
                let isMutable = ilKind = OlyILArrayKind.Mutable
                RuntimeType.Array(this.ResolveType(enclosingTyParCount, ilAsm, ilElementTy, genericContext), rank, isMutable)
            | OlyILTypeFunction(ilArgTys, ilReturnTy) ->
                let argTys = this.ResolveTypes(enclosingTyParCount, ilAsm, ilArgTys, genericContext)
                let returnTy = this.ResolveType(enclosingTyParCount, ilAsm, ilReturnTy, genericContext)
                RuntimeType.Function(argTys, returnTy)

            | OlyILTypeVariable(index, ilKind) ->
                RuntimeType.Variable(index, ilKind)

            | OlyILTypeByRef(ilElementTy, ilKind) ->
                let kind =
                    match ilKind with
                    | OlyILByRefKind.ReadWrite -> OlyIRByRefKind.ReadWrite
                    | OlyILByRefKind.Read -> OlyIRByRefKind.Read
                RuntimeType.ByRef(this.ResolveType(enclosingTyParCount, ilAsm, ilElementTy, genericContext), kind)

            | OlyILTypeHigherVariable(index, ilTyArgs, ilKind) ->
                let tyArgs =
                    ilTyArgs
                    |> ImArray.map (fun x -> this.ResolveType(enclosingTyParCount, ilAsm, x, genericContext))
                RuntimeType.HigherVariable(index, tyArgs, ilKind)

            | OlyILTypeEntity _
            | OlyILTypeForAll _ -> failwith "Invalid type."

    member this.ResolveTypes(enclosingTyParCount, ilAsm: OlyILReadOnlyAssembly, ilTys: OlyILType imarray, genericContext: GenericContext) : RuntimeType imarray =
        ilTys
        |> ImArray.map (fun ilTy -> this.ResolveType(enclosingTyParCount, ilAsm, ilTy, genericContext))

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
            (this.ResolveEnclosing(tyArgs.Length, asm.ilAsm, ilEnclosing, GenericContext.Default, ImArray.empty)).AsType

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
            this.ResolveEnclosing(tyArgs.Length, asm.ilAsm, ilEnclosing, genericContext, ImArray.empty)

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

    member runtime.EmitFunction(func: RuntimeFunction) : 'Function =
        emitFunction true func None

    member runtime.EmitILConstant(ilAsm, ilConstant: OlyILConstant, genericContext) =
        emitConstant ilAsm ilConstant genericContext

    member this.EmitFunctionFromEnvironment(envFunc: RuntimeFunction, func: RuntimeFunction) =
        if canPossiblyEraseGenericFunction envFunc func then
            this.EmitFunction(func)
        else
            // Forces no generic erasure.
            emitFunction false func None

    member this.TryGetCallStaticConstructorExpression(enclosingTy: RuntimeType) =
        match enclosingTy.TryGetStaticConstructor() with
        | Some func ->
            let emittedStaticCtor = this.EmitFunction(func)
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
                        func.Witnesses
                        genericContext

                if func.Flags.IsInlineable then
                    inlineFunctionBodyCache.SetItem(func, body)

                Some body
        else
            None

    member this.OptimizeFunctionBody(func: RuntimeFunction, funcBody: OlyIRFunctionBody<_, _, _>, genericContext: GenericContext) =
        optimizeFunctionBody func funcBody genericContext

    member this.ImportFunctionBody(
                bodyFunc: RuntimeFunction, 
                ilAsm: OlyILReadOnlyAssembly, 
                ilFuncBody: OlyILFunctionBody, 
                bodyArgTys: RuntimeType imarray, 
                returnTy: RuntimeType, 
                genericContext: GenericContext, 
                passedWitnesses: RuntimeWitness imarray
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
                this.ResolveType(enclosingTyParCount, ilAsm, x.Type, genericContext)
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
                PassedWitnesses = passedWitnesses
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
            ImArray.init argCount (fun i ->
                let irArgFlags = OlyIRLocalFlags.None
                let irArgFlags =
                    if bodyFunc.IsArgumentMutable(i) then
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
                    if cenv.ArgumentAddressExposed[i] then
                        irArgFlags ||| OlyIRLocalFlags.AddressExposed
                    else
                        irArgFlags
                let irArgFlags =
                    if cenv.ArgumentUsageCount[i] = 1 then
                        irArgFlags ||| OlyIRLocalFlags.UsedOnlyOnce
                    else
                        irArgFlags
                irArgFlags                
            )

        let irLocalFlags =
            ilLocals
            |> ImArray.mapi (fun i x ->
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