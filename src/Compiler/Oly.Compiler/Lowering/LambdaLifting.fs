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
open Oly.Compiler.Internal.SymbolEnvironments

let substituteConstant(tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, constant: ConstantSymbol) =
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

let substituteLiteral(tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, literal: BoundLiteral) =
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
            tyParLookup: Dictionary<int64, TypeSymbol>, 
            valueLookup: Dictionary<int64, IValueSymbol>,
            handleReceiverExpr: IValueSymbol -> E
        ) =
    let newExpr =
        expr.Rewrite(
            (fun origExpr ->
                match origExpr with
                | E.Lambda(syntaxInfo, lambdaFlags, tyPars, pars, lazyBodyExpr, _lambdaCachedTy, _lambdaFreeLocals, _lambdaFreeTyVars) when not(lambdaFlags.HasFlag(LambdaFlags.Bound)) && not(lambdaFlags.HasFlag(LambdaFlags.Static)) ->
                    
                    let constrsList = ResizeArray()
                    let newTyPars =
                        tyPars
                        |> ImArray.map (fun tyPar ->
                            let constrs = ref tyPar.Constraints
                            let newTyPar = TypeParameterSymbol(tyPar.Name, tyPar.Index, tyPar.Arity, tyPar.Kind, constrs)
                            tyParLookup[tyPar.Id] <- newTyPar.AsType
                            constrsList.Add(constrs)
                            newTyPar
                        )

                    constrsList
                    |> Seq.iter (fun constrs ->
                        constrs.contents <-
                            constrs.contents
                            |> ImArray.map (fun constr ->
                                constr.Substitute(tyParLookup)
                            )
                    )

                    let newPars =
                        pars
                        |> ImArray.map (fun par ->
                            // TODO: Do we need to worry about substituting within the attribute?
                            let newPar = createLocalParameterValue(par.Attributes, par.Name, par.Type.Substitute(tyParLookup), par.IsMutable)
                            valueLookup[par.Id] <- newPar
                            newPar
                        )

                    E.CreateLambda(syntaxInfo, lambdaFlags, newTyPars, newPars, lazyBodyExpr)

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
                                            (
                                                (handleReceiverExpr newValue)
                                            ),
                                            appliedNewValue :?> IFieldSymbol
                                        )
                                    // Special!
                                    if newValue.Type.IsByRef_t && not value.Type.IsByRef_t then
                                        WellKnownExpressions.FromAddress getFieldExpr
                                    else
                                        getFieldExpr                                      
                            else
                                let valueExpr = BoundExpression.Value(syntaxInfo, appliedNewValue)
                                // Special!
                                if newValue.Type.IsByRef_t && not value.Type.IsByRef_t then
                                    WellKnownExpressions.FromAddress valueExpr
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
                            match appliedNewValue with
                            | :? IFieldSymbol as appliedNewField ->
                                BoundExpression.SetContentsOfAddress(
                                    syntaxInfo, 
                                    BoundExpression.GetField(syntaxInfo, handleReceiverExpr newValue, appliedNewField),
                                    rhsExpr
                                )
                            | _ ->
                                BoundExpression.SetContentsOfAddress(
                                    syntaxInfo, 
                                    BoundExpression.CreateValue(syntaxInfo.Syntax.Tree, appliedNewValue),
                                    rhsExpr
                                )
                        else
                            match appliedNewValue with
                            | :? IFieldSymbol as appliedNewField ->
                                BoundExpression.SetField(syntaxInfo, handleReceiverExpr newValue, appliedNewField, rhsExpr)
                            | _ ->
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
                                if appliedNewValue.Type.IsTypeVariable then
                                    argExprs
                                else
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
                            if appliedValue.Type.IsTypeVariable then
                                argExprs
                            else
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
            fun expr ->
                match expr with
                | E.EntityDefinition _ 
                | E.MemberDefinition _ -> false
                | _ -> true
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
    substitute(expr, Dictionary(), valueLookup, fun _ -> OlyAssert.Fail("handleReceiverExpr"))

let canRewrite (expr: E) =
    match expr with
    | E.EntityDefinition _
    | E.MemberDefinition _ -> false
    | E.Let(bindingInfo=bindingInfo) -> not bindingInfo.Value.IsStaticLocalFunction
    | _ -> true

[<NoEquality;NoComparison>]
type cenv =
    {
        g: g
        tree: BoundTree
        genNameNumber: int ref
        mutable enclosingTyPars: TypeParameterSymbol imarray
        mutable funcTyPars: TypeParameterSymbol imarray
    }

    member this.GenerateClosureName() =
        let newId = this.genNameNumber.contents
        this.genNameNumber.contents <- this.genNameNumber.contents + 1
        "__oly_closure_" + string newId

let createClosureConstructor (freeLocals: IValueSymbol imarray) (fields: IFieldSymbol imarray) (closure: EntitySymbol) =
    Assert.ThrowIfNot(freeLocals.Length = fields.Length)

    let thisCtorPar = createThisValue "" true true (closure.ToInstantiation())
    
    let ctorPars0 =
        fields
        |> ImArray.map (fun x ->
            createLocalParameterValue(ImArray.empty, "", x.Type, false)
        )
    let ctorPars =
        (ImArray.createOne thisCtorPar).AddRange(ctorPars0)

    let ctorFlags = FunctionFlags.Constructor

    let ctor = 
        createFunctionValue 
            closure.AsEnclosing
            ImArray.empty
            "__oly_ctor"
            ImArray.empty
            ctorPars
            (applyType closure.AsType closure.TypeArguments)
            (MemberFlags.Instance ||| MemberFlags.Public)
            ctorFlags
            WellKnownFunction.None
            None
            false

    assertNoForAllTypes ctor

    ctor

let createClosureInvoke name (lambdaFlags: LambdaFlags) (tyParLookup: Dictionary<_, _>) attrs (pars: ILocalParameterSymbol imarray) invokeTyPars (funcTy: TypeSymbol) (closure: EntitySymbol) =
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
        OlyAssert.False(lambdaFlags.HasFlag(LambdaFlags.Continuation))
        if lambdaFlags.HasFlag(LambdaFlags.InlineAlways) then
            FunctionFlags.InlineAlways
        elif lambdaFlags.HasFlag(LambdaFlags.Inline) then
            FunctionFlags.Inline
        elif lambdaFlags.HasFlag(LambdaFlags.InlineNever) then
            FunctionFlags.InlineNever
        else
            FunctionFlags.None

    let memberFlags =
        let memberFlags = MemberFlags.Public
        if lambdaFlags.HasFlag(LambdaFlags.Static) then
            memberFlags
        else
            memberFlags ||| MemberFlags.Instance

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

    let baseObjectCtorCall =
        match cenv.g.BaseObjectConstructor with
        | Some(baseCtor) when not closure.IsScoped ->
            E.Call(BoundSyntaxInfo.Generated(syntaxTree),
                Some thisExpr,
                ImArray.empty,
                ImArray.empty,
                baseCtor,
                false
            )
        | _ ->
            E.None(BoundSyntaxInfo.Generated(syntaxTree))

    let exprs =
        setFieldExprs
        |> ImArray.prependOne baseObjectCtorCall
    
    let ctorRhs =
        E.CreateLambda(
            syntaxTree,
            LambdaFlags.None,
            ctor.TypeParameters,
            ((ImArray.createOne thisPar).AddRange(ctorLocalPars)),
            LazyExpression.CreateNonLazy(None, fun _ -> E.CreateSequential(syntaxTree, exprs))
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

let createClosureInvokeMemberDefinitionExpression (cenv: cenv) (bindingInfoOpt: LocalBindingInfoSymbol option) (freeLocals: IValueSymbol imarray) (pars: ILocalParameterSymbol imarray) (tyParLookup: Dictionary<_, _>) (invoke: FunctionSymbol) (bodyExpr: E) =
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
                fun _ -> 
                    thisExpr
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

let createClosureConstructorCallExpression (cenv: cenv) (freeLocals: IValueSymbol imarray) (freeTyVars: TypeParameterSymbol imarray) (extraInst: TypeSymbol imarray) (ctor: IFunctionSymbol) (invoke: IFunctionSymbol) =
    Assert.ThrowIfNot(ctor.Formal = ctor)
    Assert.ThrowIfNot(ctor.IsConstructor)

    let syntaxTree = cenv.tree.SyntaxTree

    let ctor = ctor.ApplyConstructor((freeTyVars |> ImArray.map (fun x -> x.AsType)).AddRange(extraInst))
        
    let ctorArgExprs =
        freeLocals
        |> ImArray.map (fun x -> 
            if x.IsMutable then
                WellKnownExpressions.AddressOfMutable (E.CreateValue(syntaxTree, x))
            else
                E.CreateValue(syntaxTree, x)
        )

    let callCtorExpr =
        E.Call(
            BoundSyntaxInfo.Generated(syntaxTree),
            None,
            ImArray.empty,
            ctorArgExprs,
            ctor,
            false
        )

    callCtorExpr

[<NoEquality;NoComparison>]
type ClosureInfo =
    {
        Entity: EntityDefinitionSymbol
        Constructor: FunctionSymbol
        Invoke: FunctionSymbol
        FreeLocals: IValueSymbol imarray
        FreeTypeVariables: TypeParameterSymbol imarray
        OriginalParameters: ILocalParameterSymbol imarray
        TypeParameterLookup: Dictionary<int64, TypeSymbol>
        LambdaBodyExpression: E
        BindingInfo: (LocalBindingInfoSymbol) option
        ExtraTypeArguments: TypeSymbol imarray
    }

let createClosure (cenv: cenv) (bindingInfoOpt: LocalBindingInfoSymbol option) origExpr =
    match origExpr with
    | E.Lambda(_syntaxInfo, lambdaFlags, tyPars, pars, lazyBodyExpr, _lambdaCachedTy, _lambdaFreeLocals, _lambdaFreeTyVars) ->

        let enclosingTyPars = cenv.enclosingTyPars.AddRange(cenv.funcTyPars)
        // Find free type variables and free locals
        let freeTyVars = 
            origExpr.GetLogicalFreeTypeVariables()
            |> Seq.filter (fun x ->
                cenv.funcTyPars
                |> ImArray.exists (fun y -> y.Id = x.Id)
                |> not
            )
            |> Seq.sortBy (fun x -> x.Index) 
            |> ImArray.ofSeq

        let freeTyVars = enclosingTyPars.AddRange(freeTyVars)

        let freeLocals = 
            origExpr.GetFreeLocals()
            |> Seq.map (fun x -> x.Value |> snd)
            |> Seq.filter (fun x -> not x.IsFunction)
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

        let entFlags = 
            if lambdaFlags.HasFlag(LambdaFlags.Scoped) then
                EntityFlags.Final ||| EntityFlags.Scoped
            else
                EntityFlags.Final

        let extends =
            if lambdaFlags.HasFlag(LambdaFlags.Scoped) then
                match cenv.g.ImplicitExtendsForStruct with
                | Some ty -> ImArray.createOne ty
                | _ -> ImArray.empty
            else
                ImArray.createOne TypeSymbol.BaseObject
        
        let closureBuilder = 
            EntitySymbolBuilder.CreateClosure(
                Some cenv.tree.Assembly, 
                EnclosingSymbol.Local, 
                name,
                entFlags
            )

        closureBuilder.SetExtends(LambdaLifting, extends)
        
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
                // TODO: Handle constraints.
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

        // This will add extra type parameters with constraints to a type function.
        // We do this to potentially allow captured closures to inline themselves if they can.
        // The inline optimization is handled in the runtime and looks for a specific pattern with
        // type arguments of a type function which can be replaced by a closure type.
        let extraTyPars, extraTyParsLookup =
            let extraTyPars =
                let mutable index = closureTyPars.Length
                freeLocals
                |> ImArray.choosei (fun i x ->
                    if x.IsMutable then
                        None
                    else
                        match stripTypeEquations x.Type with
                        | TypeSymbol.Function(kind=FunctionKind.Normal) ->
                            let constr = ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(x.Type.Substitute(tyParLookup)))
                            let constrs = ImArray.createOne constr |> ref
                            let tyPar = TypeParameterSymbol("__oly_" + i.ToString(), index, 0, false, TypeParameterKind.Type, constrs)
                            index <- index + 1
                            Some(KeyValuePair(i, tyPar))
                        | _ ->
                            None
                )
            (
                (extraTyPars |> ImArray.map (fun x -> x.Value)),
                extraTyPars
                |> Dictionary
            )

        let closureTyPars = closureTyPars.AddRange(extraTyPars)
        
        let funcTy = funcTy.Substitute(closureTyParLookup)        
        closureBuilder.SetTypeParameters(Pass0, closureTyPars)

        let fields =
            let fieldNames = HashSet<string>() // TODO: Maybe use object pool for this?
            let rec checkFieldName name i =
                if fieldNames.Add(name) then
                    name
                else
                    checkFieldName (name + $"__oly_state{i}") i
                    
            freeLocals
            |> ImArray.mapi (fun i x ->
                let name =
                    if System.String.IsNullOrWhiteSpace(x.Name) then
                        $"__oly_state{i}"
                    else
                        x.Name

                let name = checkFieldName name i

                let field =
                    let fieldTy = 
                        match extraTyParsLookup.TryGetValue(i) with
                        | true, tyPar -> tyPar.AsType
                        | _ -> x.Type.Substitute(closureTyParLookup)
                    let fieldTy =
                        if x.IsMutable then
                            TypeSymbol.CreateByRef(fieldTy, ByRefKind.ReadWrite)
                        else
                            fieldTy

                    if not closureBuilder.Entity.IsScoped && fieldTy.IsScoped then
                        failwith "Cannot capture a value whose type is scoped in a closure."

                    createFieldValue
                        closureBuilder.Entity.AsEnclosing
                        ImArray.empty
                        name
                        fieldTy
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

        let invokeName =
            match bindingInfoOpt with
            | Some(bindingInfo) -> bindingInfo.Value.Name
            | _ -> "Invoke"
        
        let ctor = createClosureConstructor freeLocals fields closureBuilder.Entity
        let invoke = createClosureInvoke invokeName lambdaFlags tyParLookup attrs pars invokeTyPars funcTy closureBuilder.Entity
        
        closureBuilder.SetFunctions(Pass2, [ctor;invoke] |> ImArray.ofSeq)

        let extraTyArgs =
            extraTyParsLookup
            |> Seq.sortBy (fun x -> x.Key)
            |> Seq.map (fun x ->
                freeLocals[x.Key].Type
            )
            |> ImArray.ofSeq

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
                ExtraTypeArguments = extraTyArgs
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
        
    let ctorCallExpr = createClosureConstructorCallExpression cenv freeLocals freeTyVars info.ExtraTypeArguments ctor invoke

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

                    let funcFlags = FunctionFlags.StaticLocal

                    OlyAssert.False(lambdaFlags.HasFlag(LambdaFlags.Continuation))

                    let func =
                        createFunctionValue
                            EnclosingSymbol.Local
                            ImArray.empty
                            name
                            tyPars
                            pars
                            returnTy
                            MemberFlags.Private
                            funcFlags
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
                let freeTyVars = rhsExpr.GetLogicalFreeTypeVariables()
                let freeLocals = rhsExpr.GetLogicalFreeAnyLocals()

                match rhsExpr with
                | E.Lambda(syntaxInfo=syntaxInfoLambda;tyPars=tyPars;pars=pars;body=lazyLambdaBodyExpr) ->
                    let tyParLookup = Dictionary()
                    let tyPars =
                        tyPars.AddRange(freeTyVars)
                        //|> ImArray.mapi (fun i tyPar ->
                        //    // TODO: Handle constraints
                        //    let newTyPar = TypeParameterSymbol(tyPar.Name, i, tyPar.Arity, tyPar.IsVariadic, TypeParameterKind.Function i, ref ImArray.empty)
                        //    tyParLookup[tyPar.Id] <- newTyPar.AsType
                        //    newTyPar
                        //)

                    let valueLookup = Dictionary()
                    let pars =
                        freeLocals
                        |> ImArray.map (fun x -> 
                            let parTy =
                                let parTy = x.Type.Substitute(tyParLookup)
                                if x.IsMutable then
                                    OlyAssert.False(x.Type.IsByRef_t)
                                    TypeSymbol.ByRef(parTy, ByRefKind.ReadWrite)
                                else
                                    parTy
                            let par = createLocalParameterValue(ImArray.empty, x.Name, parTy, false)
                            valueLookup[x.Id] <- par :> IValueSymbol
                            par
                        )
                        |> ImArray.append (
                            pars
                            |> ImArray.map (fun x ->
                                let parTy =
                                    let parTy = x.Type.Substitute(tyParLookup)
                                    if x.IsMutable then
                                        OlyAssert.False(x.Type.IsByRef_t)
                                        TypeSymbol.ByRef(parTy, ByRefKind.ReadWrite)
                                    else
                                        parTy
                                // TODO: Do we need to replace type variables in the attributes?
                                let par = createLocalParameterValue(x.Attributes, x.Name, parTy, false)
                                valueLookup[x.Id] <- par :> IValueSymbol
                                par
                            )
                        )
                             
                    let func = bindingInfo.Value :?> IFunctionSymbol

                    let returnTy = func.ReturnType.Substitute(tyParLookup)

                    let newFunc =
                        let funcFlags =
                            if lambdaFlags.HasFlag(LambdaFlags.Continuation) then
                                FunctionFlags.InlineAlways
                            else
                                FunctionFlags.None
                        createFunctionValue
                            EnclosingSymbol.Local
                            func.Attributes
                            func.Name
                            tyPars
                            pars
                            returnTy
                            MemberFlags.Private
                            (funcFlags ||| FunctionFlags.StaticLocal)
                            WellKnownFunction.None
                            None
                            false

                    valueLookup[func.Id] <- newFunc

                    let newLambdaBodyExpr =
                        substitute(lazyLambdaBodyExpr.Expression, tyParLookup, valueLookup, fun _ -> failwith "unexpected receiver")

                    let newLazyLambdaBodyExpr =
                        LazyExpression.CreateNonLazy(lazyLambdaBodyExpr.TrySyntax, fun _ -> newLambdaBodyExpr)

#if DEBUG || CHECKED
                    let freeLocalsDebugOrig = lazyLambdaBodyExpr.Expression.GetFreeLocals()
                    let freeTyVarsDebugOrig = lazyLambdaBodyExpr.Expression.GetFreeTypeVariables()
                    let freeLocalsDebug = newLazyLambdaBodyExpr.Expression.GetFreeLocals()
                    let freeTyVarsDebug = newLazyLambdaBodyExpr.Expression.GetFreeTypeVariables()
                    OlyAssert.Equal(freeLocalsDebugOrig.Count, freeLocalsDebug.Count)
                    OlyAssert.Equal(freeTyVarsDebugOrig.Count, freeTyVarsDebug.Count)
#endif

                    let newRhsExpr =
                        let lambdaFlags =
                            if lambdaFlags.HasFlag(LambdaFlags.Continuation) then
                                LambdaFlags.InlineAlways
                            else
                                LambdaFlags.None
                        E.CreateLambda(syntaxInfoLambda, (lambdaFlags ||| LambdaFlags.Static ||| LambdaFlags.Bound), tyPars, pars, newLazyLambdaBodyExpr)

                    let newBodyExpr = 
                        let newArgExprs =
                            freeLocals
                            |> ImArray.map (fun x -> E.Value(BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree), x))
                        bodyExpr.Rewrite(fun expr ->
                            match expr with
                            | E.Value(syntaxInfo, value) when value.Formal.Id = func.Id ->
                                E.Value(syntaxInfo, newFunc)
                            | E.Call(syntaxInfo, None, witnessArgs, argExprs, value, false) when value.Formal.Id = func.Id ->
                                let newFunc = newFunc.Apply(value.TypeArguments.AddRange(freeTyVars |> Seq.map (fun x -> x.AsType)))
                                let argExprs = argExprs.AddRange(newArgExprs)
                                let argExprs =
                                    match newFunc.Type.TryGetFunctionWithParameters() with
                                    | ValueSome(parTys, _) ->
                                        (parTys, argExprs)
                                        ||> ImArray.map2 (fun parTy argExpr ->
                                            if parTy.IsReadWriteByRef && not argExpr.Type.IsByRef_t then
                                                AddressOfMutable argExpr
                                            else
                                                argExpr
                                        )
                                    | _ ->
                                        OlyAssert.Fail("should not happen")

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

                E.Let(
                    syntaxInfo,
                    newBindingInfo,
                    newRhsExpr,
                    newBodyExpr
                )

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

                    let prevFuncTyPars = cenv.funcTyPars
                    cenv.funcTyPars <- func.TypeParameters
                    let newBodyExpr = base.Rewrite(bodyExpr)
                    cenv.funcTyPars <- prevFuncTyPars
                    
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

let Lower (g: g) (tree: BoundTree) =
    let cenv =
        {
            g = g
            enclosingTyPars = ImArray.empty
            funcTyPars = ImArray.empty
            tree = tree
            genNameNumber = ref 0
        }

    let rewriter = Rewriter(cenv, LambdaLiftingRewriterCore(cenv))
    tree.UpdateRoot(rewriter.RewriteRoot(tree.Root))