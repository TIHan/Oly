[<RequireQualifiedAccess>]
module internal rec Oly.Compiler.Internal.Lowering.LambdaLifting

open System.Threading
open System.Collections.Generic
open System.Collections.ObjectModel

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeRewriter
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.WellKnownExpressions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal

let substituteConstant(tyParLookup: ReadOnlyDictionary<int64, TypeSymbol>, constant: ConstantSymbol) =
    match constant with
    | ConstantSymbol.Array(elementTy, elements) ->
        let newElementTy = elementTy.Substitute(tyParLookup)
        let newElements =
            elements
            |> ImArray.map (fun x -> substituteConstant(tyParLookup, constant))

        // TODO: Perf/Memory - Check to see if we really need to create a new constant symbol here.
        ConstantSymbol.Array(newElementTy, newElements)

    | ConstantSymbol.TypeVariable(tyPar) ->
        let newTyPar = tyPar.EmplaceSubstitute(tyParLookup)

        if newTyPar.Id = tyPar.Id then
            constant
        else
            ConstantSymbol.TypeVariable(newTyPar)

    | ConstantSymbol.External(func) ->
        let newFunc = func.Substitute(tyParLookup) :?> IFunctionSymbol

        if areLogicalFunctionSignaturesEqual newFunc func then
            constant
        else
            ConstantSymbol.External(newFunc)

    | _ ->
        constant

let substituteLiteral(tyParLookup: ReadOnlyDictionary<int64, TypeSymbol>, literal: BoundLiteral) =
    match literal with
    | BoundLiteral.Constant(ConstantSymbol.Array(elementTy, elements)) ->
        let newElementTy = elementTy.Substitute(tyParLookup)

        if areTypesEqual newElementTy elementTy then
            literal
        else
            BoundLiteral.Constant(ConstantSymbol.Array(newElementTy, elements))

    | BoundLiteral.DefaultInference(ty, isUnchecked) ->
        let newTy = ty.Substitute(tyParLookup)

        if areTypesEqual newTy ty then
            literal
        else
            BoundLiteral.DefaultInference(newTy, isUnchecked)

    | BoundLiteral.NullInference(ty) ->
        let newTy = ty.Substitute(tyParLookup)

        if areTypesEqual newTy ty then
            literal
        else
            BoundLiteral.NullInference(newTy)

    | BoundLiteral.ConstantEnum(constant, enumTy) ->
        let newConstant = substituteConstant(tyParLookup, constant)
        let newEnumTy = enumTy.Substitute(tyParLookup)

        // TODO: Perf/Memory - Check to see if we really need to create a new literal here.
        BoundLiteral.ConstantEnum(newConstant, newEnumTy)

    | BoundLiteral.NumberInference(lazyLiteral, ty) ->
        match lazyLiteral.Value with
        | Ok(literal) ->
            let newLiteral = substituteLiteral(tyParLookup, literal)
            let newTy = ty.Substitute(tyParLookup)

            if newLiteral = literal && areTypesEqual newTy ty then
                literal
            else
                BoundLiteral.NumberInference(Lazy.CreateFromValue(Ok(newLiteral)), newTy)
        | _ ->
            literal

    | _ ->
        literal          

/// Substitutes type variables with the new types and values with new values in the entire expression.
/// Some special rules:
///     - TODO: Document special rules here. Such as if the old value was a local and the new value is a instance field, it handles this by calling handleReceiverExpr callback.
let substitute
        (
            expr: E,
            tyParLookup: ReadOnlyDictionary<int64, TypeSymbol>, 
            valueLookup: Dictionary<int64, IValueSymbol>,
            handleReceiverExpr: IValueSymbol -> E
        ) =
    let newExpr =
        expr.Rewrite(
            (fun origExpr ->
                match origExpr with
                | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
                    let newBindingInfo =
                        match bindingInfo with
                        | BindingLocal(value) ->
                            OlyAssert.True(value.IsLocal)
                            let newValue = 
                                LocalSymbol(value.Name, value.Formal.Type.Substitute(tyParLookup), value.IsGenerated, value.IsMutable)

                            valueLookup[value.Id] <- newValue

                            BindingLocal(newValue)
                        | BindingLocalFunction(func) ->
                            // TODO: Handle type parameter constraints as they may have captured type variables we need to substitute.
                            OlyAssert.True(func.IsFormal)
                            OlyAssert.True(func.IsLocal)

                            let name = func.Name
                            let ty = func.Type.Substitute(tyParLookup)
                            let valueFlags = func.ValueFlags
                            let funcFlags = func.FunctionFlags

                            let attrs =
                                func.Attributes
                                |> ImArray.map (fun attr ->
                                    match attr with
                                    | AttributeSymbol.Constructor(ctor, args, namedArgs, flags) ->
                                        // TODO: Do we actually need to handle this?
                                        attr
                                    | _ ->
                                        attr
                                )

                            let tyPars = func.TypeParameters // TODO: Substitute constraints?
                            let tyArgs = func.TypeArguments
                            let pars = func.Parameters // TODO: We must subsitute parameter types?

                            let isMutable = valueFlags.HasFlag(ValueFlags.Mutable)

                            let newFunc =
                                FunctionSymbol(
                                    EnclosingSymbol.Local,
                                    attrs,
                                    name,
                                    ty,
                                    pars,
                                    tyPars,
                                    tyArgs,
                                    MemberFlags.None,
                                    funcFlags,
                                    FunctionSemantic.NormalFunction,
                                    WellKnownFunction.None,
                                    None,
                                    isMutable
                                )

                            valueLookup[func.Id] <- newFunc

                            BindingLocalFunction(newFunc)

                    BoundExpression.Let(syntaxInfo, newBindingInfo, rhsExpr, bodyExpr)
                | _ ->
                    origExpr
            ),
            (fun origExpr ->
                match origExpr with
                | BoundExpression.Literal(syntaxInfo, literal) ->
                    let newLiteral = substituteLiteral(tyParLookup, literal)

                    if newLiteral = literal then
                        origExpr
                    else
                        BoundExpression.Literal(syntaxInfo, newLiteral)

                | BoundExpression.Value(syntaxInfo, value) ->
                    match valueLookup.TryGetValue value.Formal.Id with
                    | true, newValue -> 
            
                        let tyPars =
                            // Special!
                            if value.IsLocal && not newValue.IsLocal then
                                newValue.TypeParameters
                            else
                                newValue.AllTypeParameters

                        let allTyArgs =
                            (tyPars, value.AllTypeArguments)
                            ||> ImArray.map2 (fun tyPar tyArg ->
                                mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                            )

                        let appliedNewValue = newValue.Formal.Substitute(allTyArgs)
                        let valueExpr =
                            // Special!
                            if value.IsLocal && newValue.IsInstance then
                                if newValue.IsFunction then
                                    OlyAssert.True(value.Type.IsFunction_t)
                                    OlyAssert.False(newValue.IsConstructor)
                                    OlyAssert.True(newValue.Enclosing.IsClosure)
                                    OlyAssert.True(value.Type.IsFunction_t)
                                    handleReceiverExpr newValue
                                else
                                    OlyAssert.True(newValue.IsField)
                                    let getFieldExpr =
                                        BoundExpression.GetField(
                                            syntaxInfo, 
                                            handleReceiverExpr newValue,
                                            appliedNewValue :?> IFieldSymbol
                                        )
                                    // TODO: Get rid of this commented code, or actually use it.
                                    //       Do we want to handle substitutions for ref cells here?
                                    //if not value.Type.IsRefCell_t && newValue.Type.IsRefCell_t then
                                    //    WellKnownExpressions.LoadRefCellContents getFieldExpr
                                    //else
                                    getFieldExpr                                      
                            else
                                let valueExpr = BoundExpression.Value(syntaxInfo, appliedNewValue)
                                // Special!
                                if newValue.Type.IsByRef_t && not value.Type.IsByRef_t then
                                    WellKnownExpressions.AutoDereferenceIfPossible valueExpr
                                else
                                    valueExpr

                        valueExpr
                    | _ ->
                        let allTyArgs =
                            (value.AllTypeParameters, value.AllTypeArguments) 
                            ||> ImArray.map2 (fun tyPar tyArg -> 
                                mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                            )

                        let appliedValue = value.Formal.Substitute(allTyArgs)
                        BoundExpression.Value(syntaxInfo, appliedValue)

                | BoundExpression.SetValue(syntaxInfo, value, rhsExpr) ->
                    match valueLookup.TryGetValue value.Formal.Id with
                    | true, newValue ->

                        let tyPars =
                            // Special!
                            if value.IsLocal && not newValue.IsLocal then
                                newValue.TypeParameters
                            else
                                newValue.AllTypeParameters

                        let allTyArgs =
                            (tyPars, value.AllTypeArguments)
                            ||> ImArray.map2 (fun tyPar tyArg ->
                                mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                            )

                        let appliedNewValue = newValue.Formal.Substitute(allTyArgs)

                        // Special!
                        if appliedNewValue.Type.IsByRef_t && not value.Type.IsByRef_t then
                            BoundExpression.SetContentsOfAddress(
                                syntaxInfo, 
                                BoundExpression.CreateValue(syntaxInfo.Syntax.Tree, appliedNewValue),
                                rhsExpr
                            )
                        else
                            BoundExpression.SetValue(syntaxInfo, appliedNewValue, rhsExpr)
                    | _ ->
                        let allTyArgs =
                            (value.AllTypeParameters, value.AllTypeArguments) 
                            ||> ImArray.map2 (fun tyPar tyArg -> 
                                mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                            )

                        let appliedValue = value.Formal.Substitute(allTyArgs)
                        BoundExpression.SetValue(syntaxInfo, appliedValue, rhsExpr)

                | BoundExpression.Call(syntaxInfo, receiverOpt, witnessArgs, argExprs, value, isVirtualCall) ->
                    let subbedWitnessArgs = 
                        WitnessSolution.EmplaceSubstitute(
                            witnessArgs,
                            tyParLookup
                        )

                    match valueLookup.TryGetValue value.Formal.Id with
                    | true, newValue ->

                        let closureInvokeOpt =
                            // Special!
                            if newValue.Type.IsClosure then
                                newValue.Type.GetClosureInvoke()
                                |> Some
                            else
                                None

                        let receiverOpt =
                            match receiverOpt with
                            // Special!
                            | None when newValue.IsInstanceNotConstructor ->
                                Some(handleReceiverExpr newValue)
                            | _ ->
                                if newValue.Type.IsClosure then
                                    BoundExpression.Value(syntaxInfo, newValue)
                                    |> Some
                                else
                                    receiverOpt

                        let tyPars =
                            // Special!
                            if value.IsLocal && not newValue.IsLocal then
                                newValue.TypeParameters
                            else
                                // Special!
                                match closureInvokeOpt with
                                | Some closureInvoke ->
                                    closureInvoke.TypeParameters
                                | _ ->
                                    newValue.AllTypeParameters

                        let allTyArgs =
                            (tyPars, value.AllTypeArguments)
                            ||> ImArray.map2 (fun tyPar tyArg ->
                                mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                            )

                        match closureInvokeOpt with
                        // Special!
                        | Some closureInvoke ->
                            let allTyArgs2 =
                                (closureInvoke.Formal.Enclosing.TypeParameters, newValue.Type.TypeArguments)
                                ||> ImArray.map2 (fun tyPar tyArg ->
                                    mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                                )
                            let allTyArgs = allTyArgs.AddRange(allTyArgs2)
                            let appliedClosureInvoke = closureInvoke.Formal.Substitute(allTyArgs)

                            let newArgExprs =
                                BoundExpression.TryImplicitCalls_LoadFunction(argExprs, appliedClosureInvoke)

                            BoundExpression.Call(
                                syntaxInfo,
                                receiverOpt,
                                subbedWitnessArgs,
                                newArgExprs,
                                appliedClosureInvoke,
                                isVirtualCall
                            )
                        | _ ->
                            let appliedNewValue = newValue.Formal.Substitute(allTyArgs)

                            let newArgExprs =
                                BoundExpression.TryImplicitCalls_LoadFunction(argExprs, appliedNewValue)

                            BoundExpression.Call(
                                syntaxInfo,
                                receiverOpt,
                                subbedWitnessArgs,
                                newArgExprs,
                                appliedNewValue,
                                isVirtualCall
                            )
                    | _ ->
                        let allTyArgs =
                            (value.AllTypeParameters, value.AllTypeArguments) 
                            ||> ImArray.map2 (fun tyPar tyArg -> 
                                mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                            )
                        
                        let appliedValue = 
                            let appliedValue = value.Formal.Substitute(allTyArgs)
                            match value.Enclosing with
                            | EnclosingSymbol.Witness(concreteTy, tyExt) ->
                                let concreteTy = concreteTy.Substitute(tyParLookup)
                                let tyExt = tyExt.Substitute(tyParLookup)
                                appliedValue.WithEnclosing(EnclosingSymbol.Witness(concreteTy, tyExt))
                            | _ ->
                                appliedValue

                        let newArgExprs =
                            BoundExpression.TryImplicitCalls_LoadFunction(argExprs, appliedValue)

                        BoundExpression.Call(
                            syntaxInfo,
                            receiverOpt,
                            subbedWitnessArgs,
                            newArgExprs,
                            appliedValue,
                            isVirtualCall
                        )

                | _ ->
                    origExpr
            ),
            fun _ -> true
        )

    newExpr

let substituteValues
    (expr, substitutions: (int64 * IValueSymbol) imarray)
    =
    let valueLookup = Dictionary()
    substitutions
    |> ImArray.iter (fun (id, value) ->
        valueLookup[id] <- value
    )
    substitute(expr, ReadOnlyDictionary(Dictionary()), valueLookup, fun _ -> OlyAssert.Fail("handleReceiverExpr"))

let canRewrite (expr: E) =
    match expr with
    | E.EntityDefinition _
    | E.MemberDefinition _ -> false
    | E.Let(bindingInfo=bindingInfo) -> not bindingInfo.Value.IsStaticLocalFunction
    | _ -> true

[<NoEquality;NoComparison>]
type cenv =
    {
        tree: BoundTree
        genNameNumber: int ref
        mutable enclosingTyPars: TypeParameterSymbol imarray
    }

    member this.GenerateClosureName() =
        let newId = this.genNameNumber.contents
        this.genNameNumber.contents <- this.genNameNumber.contents + 1
        "__oly_closure_" + string newId

let createClosureConstructor (freeLocals: IValueSymbol imarray) (tyParLookup: ReadOnlyDictionary<_, _>) (fields: IFieldSymbol imarray) (closure: EntitySymbol) =
    Assert.ThrowIfNot(freeLocals.Length = fields.Length)

    let thisCtorPar = createThisValue "" true true (closure.ToInstantiation())
    
    let ctorPars0 =
        freeLocals
        |> ImArray.map (fun x ->
            Assert.ThrowIf(x.IsMutable)
            createLocalParameterValue(ImArray.empty, "", x.Type.Substitute(tyParLookup), false)
        )
    let ctorPars =
        (ImArray.createOne thisCtorPar).AddRange(ctorPars0)

    let ctor = 
        createFunctionValue 
            closure.AsEnclosing
            ImArray.empty
            "__oly_ctor"
            ImArray.empty
            ctorPars
            closure.AsType
            (MemberFlags.Instance ||| MemberFlags.Public)
            FunctionFlags.Constructor
            WellKnownFunction.None
            None
            false

    assertNoForAllTypes ctor

    ctor

let createClosureInvoke (lambdaFlags: LambdaFlags) (tyParLookup: ReadOnlyDictionary<_, _>) attrs (pars: ILocalParameterSymbol imarray) invokeTyPars (funcTy: TypeSymbol) (closure: EntitySymbol) =
    let invokePars, invokeReturnTy =
        let invokePars =
            pars
            |> ImArray.map (fun par ->
                OlyAssert.True(par.Type.IsSolved)
                OlyAssert.False(par.Type.IsError_t)
                createLocalParameterValue(par.Attributes, par.Name, par.Type.Substitute(tyParLookup), par.IsMutable)
            )
        let returnTy =
            match funcTy.TryFunction with
            | ValueSome(_, returnTy) -> 
                OlyAssert.True(returnTy.IsSolved)
                OlyAssert.False(returnTy.IsError_t)
                returnTy.Substitute(tyParLookup)
            | _ -> 
                failwith "Expected function type."
        if lambdaFlags.HasFlag(LambdaFlags.Static) then
            invokePars, returnTy
        else
            let thisPar = createThisValue "" false true (closure.ToInstantiation())
            let thisPar = ImArray.createOne thisPar
            thisPar.AddRange(invokePars), returnTy

    let funcFlags =
        if lambdaFlags.HasFlag(LambdaFlags.Inline) then
            FunctionFlags.Inline
        else
            match tryAttributesInlineFlags attrs with
            | Some(inlineFlags) -> inlineFlags
            | _ -> FunctionFlags.None

    let memberFlags =
        let memberFlags = MemberFlags.Public
        if lambdaFlags.HasFlag(LambdaFlags.Static) then
            memberFlags
        else
            memberFlags ||| MemberFlags.Instance

    let name =
        if lambdaFlags.HasFlag(LambdaFlags.Static) then
            closure.Name
        else
            "Invoke"

    let enclosing =
        if lambdaFlags.HasFlag(LambdaFlags.Static) then
            EnclosingSymbol.Local
        else
            closure.AsEnclosing
    
    let invoke =
        createFunctionValue
            enclosing
            attrs
            name
            invokeTyPars
            invokePars
            invokeReturnTy
            memberFlags
            funcFlags
            WellKnownFunction.None
            None
            false

    assertNoForAllTypes invoke

    invoke   

let createClosureConstructorMemberDefinitionExpression (cenv: cenv) (ctor: FunctionSymbol) =
    let syntaxTree = cenv.tree.SyntaxTree
    let closure = ctor.Enclosing.TryEntity.Value

    let ctorLocalPars =
        ctor.LogicalParameters
        |> ROMem.toImArray
    
    let thisPar = createThisValue "" true true (closure.ToInstantiation())
    let thisExpr = E.CreateValue(syntaxTree, thisPar)
    
    let setFieldExprs =
        (closure.Fields, ctorLocalPars)
        ||> ImArray.map2 (fun field localPar ->
            E.SetField(
                BoundSyntaxInfo.Generated(syntaxTree),
                thisExpr,
                field,
                E.CreateValue(syntaxTree, localPar)
            )
        )
    
    let ctorRhs =
        E.CreateLambda(
            syntaxTree,
            LambdaFlags.None,
            ctor.TypeParameters,
            ((ImArray.createOne thisPar).AddRange(ctorLocalPars)),
            LazyExpression.CreateNonLazy(None, fun _ -> E.CreateSequential(syntaxTree, setFieldExprs))
        )
    
    let syntaxInfo = BoundSyntaxInfo.Generated(syntaxTree)
    E.MemberDefinition(
        syntaxInfo,
        BoundBinding.Implementation(
            syntaxInfo,
            BindingFunction(ctor),
            ctorRhs
        )
    )

let createClosureInvokeMemberDefinitionExpression (cenv: cenv) (bindingInfoOpt: LocalBindingInfoSymbol option) (freeLocals: IValueSymbol imarray) (pars: ILocalParameterSymbol imarray) (tyParLookup: ReadOnlyDictionary<_, _>) (invoke: FunctionSymbol) (bodyExpr: E) =
    let syntaxTree = cenv.tree.SyntaxTree
    let closure = invoke.Enclosing.TryEntity.Value

    let valueLookup = Dictionary<_, IValueSymbol>()
    (freeLocals, closure.Fields)
    ||> ImArray.iter2 (fun freeLocal field ->
        valueLookup[freeLocal.Id] <- field
    )

    match bindingInfoOpt with
    | Some(bindingInfo) ->
        valueLookup[bindingInfo.Value.Id] <- invoke
    | _ ->
        ()

    (pars.AsMemory(), invoke.LogicalParameters)
    ||> ROMem.iter2 (fun par invokePar ->
        valueLookup[par.Id] <- invokePar
    )

    let syntaxInfo = BoundSyntaxInfo.Generated(syntaxTree)

    let thisPar = invoke.Parameters[0]
    let thisExpr = E.CreateValue(syntaxTree, thisPar)

    let invokeRhs =
        let newBodyExpr =
            substitute(
                bodyExpr,
                tyParLookup, 
                valueLookup,
                fun _ -> thisExpr
            )
        E.CreateLambda(
            syntaxInfo,
            LambdaFlags.None,
            invoke.TypeParameters, 
            invoke.Parameters, 
            LazyExpression.CreateNonLazy(None, fun _ -> newBodyExpr)
        )
        
    E.MemberDefinition(
        syntaxInfo,
        BoundBinding.Implementation(
            syntaxInfo,
            BindingFunction(invoke),
            invokeRhs
        )
    )

let createClosureConstructorCallExpression (cenv: cenv) (freeLocals: IValueSymbol imarray) (freeTyVars: TypeParameterSymbol imarray) (ctor: IFunctionSymbol) =
    Assert.ThrowIfNot(ctor.Formal = ctor)
    Assert.ThrowIfNot(ctor.IsConstructor)

    let syntaxTree = cenv.tree.SyntaxTree

    let ctor = ctor.ApplyConstructor(freeTyVars |> ImArray.map (fun x -> x.AsType))
        
    let ctorArgExprs =
        freeLocals
        |> ImArray.map (fun x -> E.CreateValue(syntaxTree, x))

    E.Call(
        BoundSyntaxInfo.Generated(syntaxTree),
        None,
        ImArray.empty,
        ctorArgExprs,
        ctor,
        false
    )

[<NoEquality;NoComparison>]
type ClosureInfo =
    {
        Entity: EntityDefinitionSymbol
        Constructor: FunctionSymbol
        Invoke: FunctionSymbol
        FreeLocals: IValueSymbol imarray
        FreeTypeVariables: TypeParameterSymbol imarray
        OriginalParameters: ILocalParameterSymbol imarray
        TypeParameterLookup: ReadOnlyDictionary<int64, TypeSymbol>
        LambdaBodyExpression: E
        BindingInfo: (LocalBindingInfoSymbol) option
    }

let createClosure (cenv: cenv) (bindingInfoOpt: LocalBindingInfoSymbol option) origExpr =
    match origExpr with
    | E.Lambda(syntaxInfo, lambdaFlags, tyPars, pars, lazyBodyExpr, lambdaCachedTy, lambdaFreeLocals, lambdaFreeTyVars) ->

        // Find free type variables and free locals

        let freeTyVars = 
            origExpr.GetFreeTypeVariables().Values
            |> Seq.sortBy (fun x -> x.Index) 
            |> ImArray.ofSeq

        let freeTyVars = 
            let enclosingTyPars = 
                cenv.enclosingTyPars 
                |> ImArray.filter (fun x ->
                    freeTyVars
                    |> ImArray.exists (fun y -> x.Name = y.Name) // TODO: This is a hack, we should use 'x.Id = y.Id'.
                    |> not
                )
            enclosingTyPars.AddRange(freeTyVars)

        let freeLocals = 
            origExpr.GetFreeLocals()
            |> Seq.map (fun x -> x.Value |> snd)
            |> Seq.filter (fun x -> not x.IsMutable && not x.IsFunction)
            |> ImArray.ofSeq
            |> ImArray.filter (fun x ->
                match bindingInfoOpt with
                | Some(bindingInfo) ->
                    bindingInfo.Value.Id <> x.Formal.Id
                | _ ->
                    true
            )

        let name = 
            match bindingInfoOpt with
            | None ->
                cenv.GenerateClosureName()
            | Some(bindingInfo) ->
                bindingInfo.Value.Name + cenv.GenerateClosureName()
        
        let closureBuilder = 
            EntitySymbolBuilder.CreateClosure(
                Some cenv.tree.Assembly, 
                EnclosingSymbol.Local, 
                name
            )
        
        let tyParLookup = Dictionary()
        let closureTyParLookup = Dictionary()
        
        let closureTyPars = 
            freeTyVars
            |> ImArray.mapi (fun i x -> 
                // TODO: Handle constraints.
                let tyPar = TypeParameterSymbol(x.Name, i, x.Arity, x.IsVariadic, TypeParameterKind.Type, ref ImArray.empty)
                tyParLookup[x.Id] <- tyPar.AsType
                closureTyParLookup[x.Id] <- tyPar.AsType
                tyPar
            )
        
        let invokeTyPars =
            tyPars
            |> ImArray.mapi (fun i x ->
                let tyPar = TypeParameterSymbol(x.Name, closureTyPars.Length + i, x.Arity, x.IsVariadic, TypeParameterKind.Function i, ref ImArray.empty)
                tyParLookup[x.Id] <- tyPar.AsType
                tyPar
            )

        let attrs =
            match bindingInfoOpt with
            | Some(bindingInfo) ->
                match bindingInfo.Value with
                | :? IFieldSymbol as field -> field.Attributes
                | :? IFunctionSymbol as func -> func.Attributes
                | :? ILocalParameterSymbol as par -> par.Attributes
                | _ -> ImArray.empty
            | _ ->
                ImArray.empty

        let funcTy = origExpr.Type
        let bodyExpr = lazyBodyExpr.Expression
        
        let lambdaFlags = LambdaFlags.None
        let tyParLookup = tyParLookup |> ReadOnlyDictionary
        let closureTyParLookup = closureTyParLookup |> ReadOnlyDictionary

        (freeTyVars.AddRange(tyPars), (closureTyPars.AddRange(invokeTyPars)))
        ||> ImArray.iter2 (fun (oldTyPar: TypeParameterSymbol) (newTyPar: TypeParameterSymbol) ->
            let constrs =
                oldTyPar.Constraints
                |> ImArray.map (fun oldConstr ->
                    oldConstr.Substitute(tyParLookup)
                )
            newTyPar.SetConstraints(constrs)
        )

        // ------------------------------------------------------------------------
        
        let funcTy = funcTy.Substitute(closureTyParLookup)        
        closureBuilder.SetTypeParameters(Pass0, closureTyPars)

        let fields =
            freeLocals
            |> ImArray.mapi (fun i x ->
                let name =
                    if System.String.IsNullOrWhiteSpace(x.Name) then
                        ($"__oly_state{i}")
                    else
                        x.Name
                let field =
                    createFieldValue
                        closureBuilder.Entity.AsEnclosing
                        ImArray.empty
                        name
                        (x.Type.Substitute(closureTyParLookup))
                        (MemberFlags.Instance ||| MemberFlags.Public)
                        ValueFlags.None
                        (ref None)

                // Checks and balances.
                match stripTypeEquations field.Type with
                | TypeSymbol.ForAll _ -> OlyAssert.Fail("Invalid closure field.")
                | TypeSymbol.Entity(ent) ->
                    if not ent.TypeParameters.IsEmpty && ent.IsFormal then
                        OlyAssert.Fail("Invalid closure field.")
                | _ -> ()

                field
            )

        closureBuilder.SetFields(Pass2, fields)
        
        let ctor = createClosureConstructor freeLocals tyParLookup fields closureBuilder.Entity
        let invoke = createClosureInvoke lambdaFlags tyParLookup attrs pars invokeTyPars funcTy closureBuilder.Entity
        
        closureBuilder.SetFunctions(Pass2, [ctor;invoke] |> ImArray.ofSeq)

        let info =
            {
                Entity = closureBuilder.Entity
                Constructor = ctor
                Invoke = invoke
                FreeLocals = freeLocals
                FreeTypeVariables = freeTyVars
                OriginalParameters = pars
                TypeParameterLookup = tyParLookup
                LambdaBodyExpression = bodyExpr
                BindingInfo = bindingInfoOpt
            }

        toClosureExpression cenv info

    | _ ->
        failwith "Expected lambda."

let toClosureExpression cenv (info: ClosureInfo) =
    let closure = info.Entity
    let ctor = info.Constructor
    let invoke = info.Invoke
    let freeLocals = info.FreeLocals
    let freeTyVars = info.FreeTypeVariables
    let pars = info.OriginalParameters
    let tyParLookup = info.TypeParameterLookup
    let bodyExpr = info.LambdaBodyExpression
    let bindingInfoOpt = info.BindingInfo
        
    let ctorDefExpr = createClosureConstructorMemberDefinitionExpression cenv ctor
    let invokeDefExpr = createClosureInvokeMemberDefinitionExpression cenv bindingInfoOpt freeLocals pars tyParLookup invoke bodyExpr
        
    let ctorCallExpr = createClosureConstructorCallExpression cenv freeLocals freeTyVars ctor

    let syntaxTree = cenv.tree.SyntaxTree   
    E.CreateSequential(syntaxTree,
        [
            // Closure definition
            E.CreateEntityDefinition(
                BoundSyntaxInfo.Generated(syntaxTree),
                E.CreateSequential(ctorDefExpr, invokeDefExpr),
                closure
            )

            // Closure construction
            ctorCallExpr
        ]
    )

[<Sealed>]
type LambdaLiftingRewriterCore(cenv: cenv) =
    inherit BoundTreeRewriterCore()

    let makeLambdaBound expr =
        match expr with
        | E.Lambda(syntaxInfo, lambdaFlags, tyPars, pars, lazyLambdaBodyExpr, lazyLambdaTy, freeLocals, freeTyVars) ->
            E.Lambda(syntaxInfo, lambdaFlags ||| LambdaFlags.Bound, tyPars, pars, lazyLambdaBodyExpr, lazyLambdaTy, freeLocals, freeTyVars)
        | _ ->
            OlyAssert.Fail("Expected lambda expression")

    let mappings = Dictionary()

    override this.PreorderRewrite(origExpr) =
        match origExpr with
        | E.MemberDefinition(syntaxInfo, binding) ->
            match binding with
            | BoundBinding.Implementation(syntaxInfoImpl, bindingInfo, rhsExpr) when bindingInfo.Value.IsFunction ->
                match bindingInfo with
                | BindingFunction _ 
                | BindingPattern _ -> ()
                | _ -> OlyAssert.Fail("Invalid member binding")

                E.MemberDefinition(syntaxInfo, BoundBinding.Implementation(syntaxInfoImpl, bindingInfo, makeLambdaBound rhsExpr))
            | _ ->
                origExpr

        | E.Let(syntaxInfo, bindingInfo, (E.Lambda(flags=lambdaFlags) as rhsExpr), bodyExpr) when bindingInfo.Value.IsFunction ->
            match bindingInfo with
            | BindingLocal _ -> OlyAssert.Fail("Invalid local binding")
            | BindingLocalFunction _ -> ()

            if bindingInfo.Value.IsStaticLocalFunction then
                E.Let(syntaxInfo, bindingInfo, makeLambdaBound rhsExpr, bodyExpr)
            else
                if lambdaFlags.HasFlag(LambdaFlags.StackEmplace) then
                    let freeLocals = origExpr.GetFreeLocals()
                    let freeTyVars = origExpr.GetFreeTypeVariables()

                    match rhsExpr with
                    | E.Lambda(syntaxInfo=syntaxInfoLambda;tyPars=tyPars;pars=pars;body=lazyLambdaBodyExpr) ->
                        OlyAssert.Equal(0, tyPars.Length)

                        let tyParLookup = Dictionary()
                        let tyPars =
                            freeTyVars.Values
                            |> Seq.mapi (fun i tyPar ->
                                // TODO: Handle constraints
                                let newTyPar = TypeParameterSymbol(tyPar.Name, i, tyPar.Arity, tyPar.IsVariadic, TypeParameterKind.Function i, ref ImArray.empty)
                                tyParLookup[tyPar.Id] <- newTyPar.AsType
                                newTyPar
                            )
                            |> ImArray.ofSeq
                        let tyParLookup = ReadOnlyDictionary tyParLookup

                        let freeLocals = freeLocals.Values |> ImArray.ofSeq
                        let valueLookup = Dictionary()
                        let pars =
                            freeLocals
                            |> ImArray.map (fun (_, x) -> 
                                let par = createLocalParameterValue(ImArray.empty, x.Name, x.Type.Substitute(tyParLookup), x.IsMutable)
                                valueLookup[x.Id] <- par :> IValueSymbol
                                par
                            )
                            |> ImArray.append (
                                pars
                                |> ImArray.map (fun x ->
                                    // TODO: Do we need to replace type variables in the attributes?
                                    let par = createLocalParameterValue(x.Attributes, x.Name, x.Type.Substitute(tyParLookup), x.IsMutable)
                                    valueLookup[x.Id] <- par :> IValueSymbol
                                    par
                                )
                            )
                             
                        let func = bindingInfo.Value :?> IFunctionSymbol

                        let returnTy = func.ReturnType.Substitute(tyParLookup)

                        let newFunc =
                            createFunctionValue
                                EnclosingSymbol.Local
                                func.Attributes
                                func.Name
                                tyPars
                                pars
                                returnTy
                                MemberFlags.Private
                                (FunctionFlags.StackEmplace ||| FunctionFlags.StaticLocal)
                                WellKnownFunction.None
                                None
                                false

                        valueLookup[func.Id] <- newFunc

                        let newLambdaBodyExpr =
                            substitute(lazyLambdaBodyExpr.Expression, tyParLookup, valueLookup, fun _ -> failwith "unexpected receiver")

                        let newLazyLambdaBodyExpr =
                            LazyExpression.CreateNonLazy(lazyLambdaBodyExpr.TrySyntax, fun _ -> newLambdaBodyExpr)

#if DEBUG
                        let freeLocalsDebugOrig = lazyLambdaBodyExpr.Expression.GetFreeLocals()
                        let freeTyVarsDebugOrig = lazyLambdaBodyExpr.Expression.GetFreeTypeVariables()
                        let freeLocalsDebug = newLazyLambdaBodyExpr.Expression.GetFreeLocals()
                        let freeTyVarsDebug = newLazyLambdaBodyExpr.Expression.GetFreeTypeVariables()
                        OlyAssert.Equal(freeLocalsDebugOrig.Count, freeLocalsDebug.Count)
                        OlyAssert.Equal(freeTyVarsDebugOrig.Count, freeTyVarsDebug.Count)
#endif

                        let newRhsExpr =
                            E.CreateLambda(syntaxInfoLambda, (LambdaFlags.StackEmplace ||| LambdaFlags.Static ||| LambdaFlags.Bound), tyPars, pars, newLazyLambdaBodyExpr)

                        let newBodyExpr = 
                            let newArgExprs =
                                freeLocals
                                |> ImArray.map (fun (_, x) -> E.Value(BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree), x))
                            bodyExpr.Rewrite(fun expr ->
                                match expr with
                                | E.Call(syntaxInfo, None, witnessArgs, argExprs, value, false) when value.Formal.Id = func.Id ->
                                    let newFunc = newFunc.Apply(freeTyVars.Values |> Seq.map (fun x -> x.AsType) |> ImArray.ofSeq)
                                    let argExprs = argExprs.AddRange(newArgExprs)
                                    E.Call(syntaxInfo, None, witnessArgs, argExprs, newFunc, false)
                                | _ ->
                                    expr
                            )

                        E.Let(syntaxInfo, BindingLocalFunction(newFunc), newRhsExpr, newBodyExpr)
                    | _ ->
                        OlyAssert.Fail("Expected lambda expression")
                else
                    let value = bindingInfo.Value

                    let newRhsExpr = makeLambdaBound rhsExpr
                    let newRhsExpr = createClosure cenv (Some(bindingInfo)) newRhsExpr

                    let local =
                        createLocalValue
                            value.Name
                            newRhsExpr.Type

                    let newBindingInfo = BindingLocal(local)

                    let newRhsExpr = substituteValues(newRhsExpr, ImArray.createOne (value.Id, local))
                    let newBodyExpr = substituteValues(bodyExpr, ImArray.createOne (value.Id, local))

                    mappings.Add(newBindingInfo.Value.Id, bindingInfo)

                    E.Let(
                        syntaxInfo,
                        newBindingInfo,
                        newRhsExpr,
                        newBodyExpr
                    )

        | _ ->
            origExpr

    override this.Rewrite(origExpr) =
        match origExpr with
        | E.Lambda(syntaxInfo=syntaxInfo;flags=lambdaFlags;tyPars=tyPars;pars=pars) when not(lambdaFlags.HasFlag(LambdaFlags.Bound)) ->
            let newExpr = 
                if lambdaFlags.HasFlag(LambdaFlags.Static) then
                    let name = cenv.GenerateClosureName()
                    let local = createTemporaryValue origExpr.Type

                    let returnTy =
                        match local.Type.TryFunction with
                        | ValueSome(_, returnTy) -> returnTy
                        | _ -> OlyAssert.Fail("Expected function type")

                    let func =
                        createFunctionValue
                            EnclosingSymbol.Local
                            ImArray.empty
                            name
                            tyPars
                            pars
                            returnTy
                            MemberFlags.Private
                            FunctionFlags.StaticLocal
                            WellKnownFunction.None
                            None
                            false

                    E.Let(syntaxInfo, BindingLocalFunction(func), makeLambdaBound origExpr,
                        E.Value(syntaxInfo, func)
                    )
                else
                    createClosure cenv None origExpr
            E.TryImplicitCall_LoadFunction(newExpr, origExpr.Type)

        | _ ->
            origExpr

[<Sealed>]
type Rewriter(cenv: cenv, core) =
    inherit BoundTreeRewriter(core)

    override this.Rewrite(expr) =
        match expr with
        | E.EntityDefinition(ent=ent) ->
            let prevEnclosingTyPars = cenv.enclosingTyPars
            cenv.enclosingTyPars <- ent.TypeParameters
            let result = base.Rewrite(expr)
            cenv.enclosingTyPars <- prevEnclosingTyPars
            result

        | E.MemberDefinition(syntaxInfo, binding) ->
            match binding with
            | BoundBinding.Implementation(syntaxInfoBinding, bindingInfo, E.Lambda(syntaxInfoLambda, flags, tyPars, pars, lazyBodyExpr, cachedLambdaTy, freeLocals, freeVars)) ->
                match bindingInfo with
                | BindingFunction(func) when func.TypeParameters.Length > 0 ->
                    let bodyExpr = lazyBodyExpr.Expression

                    let prevEnclosingTyPars = cenv.enclosingTyPars
                    cenv.enclosingTyPars <- func.TypeParameters
                    let newBodyExpr = base.Rewrite(bodyExpr)
                    cenv.enclosingTyPars <- prevEnclosingTyPars
                    
                    if newBodyExpr = bodyExpr then
                        expr
                    else
                        E.MemberDefinition(syntaxInfo,
                            BoundBinding.Implementation(
                                syntaxInfoBinding,
                                bindingInfo,
                                E.Lambda(syntaxInfoLambda, flags, tyPars, pars, LazyExpression.CreateNonLazy(None, fun _ -> newBodyExpr), cachedLambdaTy, freeLocals, freeVars)
                            )
                        )

                | _ ->
                    base.Rewrite(expr)
            | _ ->
                base.Rewrite(expr)
            
        | _ ->
            base.Rewrite(expr)

let Lower (tree: BoundTree) =
    let cenv =
        {
            enclosingTyPars = ImArray.empty
            tree = tree
            genNameNumber = ref 0
        }

    let rewriter = Rewriter(cenv, LambdaLiftingRewriterCore(cenv))
    tree.UpdateRoot(rewriter.RewriteRoot(tree.Root))