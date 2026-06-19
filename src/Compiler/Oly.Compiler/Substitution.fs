module internal rec Oly.Compiler.Internal.Substitution

open Oly.Core
open System.Collections.Generic
open BoundTreePatterns
open BoundTreeExtensions
open WellKnownExpressions
open Symbols
open SymbolOperations
open SymbolQuery.Extensions
open BoundTree
open SymbolEnvironments

let private handleCallExpression syntaxInfo receiverOpt subbedWitnessArgs argExprs (appliedNewValue: IValueSymbol) isVirtualCall =
    let newArgExprs =
        if appliedNewValue.Type.IsAnyVariable_ste then
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
let substituteForClosure
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
                | BoundExpression.GetField(syntaxInfo, receiverExpr, field) ->
                    if areTypesEqual (stripByRef receiverExpr.Type) field.Enclosing.AsType then
                        origExpr
                    else
                        let newField = (stripByRef receiverExpr.Type).FindField(field.Name)
                        E.GetField(syntaxInfo, receiverExpr, newField)

                | BoundExpression.SetField(syntaxInfo, receiverExpr, field, rhsExpr, isCtorInit) ->
                    if areTypesEqual (stripByRef receiverExpr.Type) field.Enclosing.AsType then
                        origExpr
                    else
                        let newField = (stripByRef receiverExpr.Type).FindField(field.Name)
                        E.SetField(syntaxInfo, receiverExpr, newField, rhsExpr, isCtorInit)

                | BoundExpression.Literal(syntaxInfo, literal) ->
                    let newLiteral = substituteLiteral(tyParLookup, literal)

                    if newLiteral = literal then
                        origExpr
                    else
                        BoundExpression.Literal(syntaxInfo, newLiteral)

                | BoundExpression.Value(syntaxInfo, value) ->
                    match valueLookup.TryGetValue value.Formal.Id with
                    | true, newValue -> 

                        let appliedNewValue = 
                            if value.IsFunction then
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
                                newValue.Formal.Substitute(allTyArgs)
                            else
                                newValue

                        let valueExpr =
                            // Special!
                            if value.IsLocal && newValue.IsInstance then
                                if newValue.IsFunction then
                                    OlyAssert.True(value.Type.IsAnyFunction_ste)
                                    OlyAssert.False(newValue.IsConstructor)
                                    OlyAssert.True(newValue.Enclosing.IsClosure)
                                    OlyAssert.True(value.Type.IsAnyFunction_ste)
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
                                    if newValue.Type.IsAnyByRef_ste && not value.Type.IsAnyByRef_ste then
                                        WellKnownExpressions.FromAddress getFieldExpr
                                    else
                                        getFieldExpr                                      
                            else
                                let valueExpr = BoundExpression.Value(syntaxInfo, appliedNewValue)
                                // Special!
                                if newValue.Type.IsAnyByRef_ste && not value.Type.IsAnyByRef_ste then
                                    WellKnownExpressions.FromAddress valueExpr
                                else
                                    valueExpr

                        valueExpr
                    | _ ->
                        let appliedValue = 
                            if value.IsFunction then
                                let allTyArgs =
                                    (value.AllTypeParameters, value.AllTypeArguments) 
                                    ||> ImArray.map2 (fun tyPar tyArg -> 
                                        mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                                    )
                                value.Formal.Substitute(allTyArgs)
                            else
                                value
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
                        if appliedNewValue.Type.IsAnyByRef_ste && not value.Type.IsAnyByRef_ste then
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
                                    BoundExpression.CreateGeneratedValue(syntaxInfo.Syntax, appliedNewValue),
                                    rhsExpr
                                )
                        else
                            match appliedNewValue with
                            | :? IFieldSymbol as appliedNewField ->
                                BoundExpression.SetField(syntaxInfo, handleReceiverExpr newValue, appliedNewField, rhsExpr, isCtorInit = false)
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
                            if newValue.Type.IsClosure_ste then
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
                                if newValue.Type.IsClosure_ste then
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

                            if appliedNewValue.Type.IsAnyByRef_ste then
                                let rhsExpr =
                                    if appliedNewValue.IsField && receiverOpt.IsSome then
                                        OlyAssert.True(appliedNewValue.IsInstance)
                                        (FromAddress (E.GetField(syntaxInfo, receiverOpt.Value, appliedNewValue.AsField)))
                                    else
                                        OlyAssert.False(receiverOpt.IsSome)
                                        OlyAssert.False(appliedNewValue.IsInstance)
                                        (FromAddress (E.Value(syntaxInfo, appliedNewValue)))
                                createLocalDeclarationExpression 
                                    rhsExpr
                                    (fun syntaxInfo appliedNewValue ->
                                        handleCallExpression
                                            syntaxInfo
                                            None
                                            subbedWitnessArgs
                                            argExprs
                                            appliedNewValue
                                            isVirtualCall
                                    ) |> fst
                            else
                                handleCallExpression
                                    syntaxInfo
                                    receiverOpt
                                    subbedWitnessArgs
                                    argExprs
                                    appliedNewValue
                                    isVirtualCall
                    | _ ->
                        let allTyArgs =
                            (value.AllTypeParameters, value.AllTypeArguments) 
                            ||> ImArray.map2 (fun tyPar tyArg -> 
                                let newTyArg = tyArg.Substitute(tyParLookup)
                                let newTyArg =
                                    if newTyArg.IsUnit_ste then
                                        TypeSymbol.RealUnit
                                    else
                                        newTyArg
                                // REVIEW: This is a little curious, but perhaps it is ok.
                                if tyPar.HasArity then
                                    mkSolvedInferenceVariableType tyPar newTyArg.Formal
                                else
                                    mkSolvedInferenceVariableType tyPar newTyArg
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
                            if appliedValue.Type.IsAnyVariable_ste then
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
                | E.EntityDefinition(ent=ent) -> ent.IsLocal
                | E.MemberDefinition _ -> false
                | _ -> true
        )

    newExpr

let substituteValuesForClosure
    (expr, substitutions: (int64 * IValueSymbol) imarray)
    =
    let valueLookup = Dictionary()
    substitutions
    |> ImArray.iter (fun (id, value) ->
        valueLookup[id] <- value
    )
    substituteForClosure(expr, Dictionary(), valueLookup, fun _ -> OlyAssert.Fail("handleReceiverExpr"))

let substituteForAutoGeneralization
        (
            benv: BoundEnvironment,
            expr: BoundExpression, 
            localLookup: Dictionary<int64, IValueSymbol>,
            tyParReplace: Dictionary<int64, TypeSymbol>
        ) =
    let newExpr =
        expr.Rewrite(
            (fun origExpr ->
                match origExpr with
                | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) when bindingInfo.Value.IsLocal && not bindingInfo.IsFunction ->
                    let value = bindingInfo.Value
                    let rec handleTy ty =
                        match stripTypeEquationsExceptAlias ty with
                        | TypeSymbol.Variable(tyPar) ->
                            match tyParReplace.TryGetValue(tyPar.Id) with
                            | true, ty -> ty
                            | _ -> ty
                        | ty ->
                            if ty.TypeArguments.IsEmpty then
                                ty
                            else
                                let tyArgs = ty.TypeArguments |> ImArray.map handleTy
                                applyType ty.Formal tyArgs
                    let valueTy = value.Type
                    let newValueTy = handleTy valueTy
                    let newValue = createLocalValue value.Name newValueTy
                    localLookup[value.Id] <- newValue
                    BoundExpression.Let(syntaxInfo, LocalBindingInfoSymbol.BindingLocal(newValue), rhsExpr, bodyExpr)
                | _ ->
                    origExpr
            ),
            (fun origExpr ->
                match origExpr with
                | BoundExpression.Value(syntaxInfo, value) when value.IsLocal && not value.IsFunction ->
                    match localLookup.TryGetValue value.Formal.Id with
                    | true, newValue ->          
                        BoundExpression.Value(syntaxInfo, newValue)
                    | _ ->
                        origExpr

                | BoundExpression.Call(syntaxInfo, None, witnessArgs, argExprs, value, isVirtualCall) ->
                    let witnessArgs = 
                        WitnessSolution.EmplaceSubstitute(
                            witnessArgs,
                            tyParReplace
                        )

                    match localLookup.TryGetValue value.Formal.Id with
                    | true, newValue ->
                        let newValue =
                            if value.IsFormal then
                                if newValue.TypeParameters.IsEmpty then
                                    newValue
                                else
                                    OlyAssert.True(newValue.IsFormal)
                                    let newValue2 = freshenValue benv newValue
                                    UnifyTypes Flexible newValue2.Type value.Type
                                    |> ignore
                                    newValue2
                            else
                                let tyArgs = value.TypeArguments
                                OlyAssert.True(newValue.IsFormal)
                                actualValue newValue.Enclosing tyArgs newValue
                        BoundExpression.Call(
                            syntaxInfo,
                            None,
                            witnessArgs,
                            argExprs,
                            newValue,
                            isVirtualCall
                        )
                    | _ ->
                         if value.IsFunction && not value.TypeParameters.IsEmpty then
                             let rec handleTy ty =
                                 match stripTypeEquationsExceptAlias ty with
                                 | TypeSymbol.Variable(tyPar) ->
                                     match tyParReplace.TryGetValue(tyPar.Id) with
                                     | true, ty -> ty
                                     | _ -> ty
                                 | ty ->
                                     if ty.TypeArguments.IsEmpty then
                                         ty
                                     else
                                         let tyArgs = ty.TypeArguments |> ImArray.map handleTy
                                         applyType ty.Formal tyArgs
                             let tyArgs = value.TypeArguments |> ImArray.map handleTy
                             OlyAssert.False(value.IsFormal)
                             let tyPars = benv.EnclosingTypeParameters.AddRange(value.TypeParameters)
                             let tyArgs = (benv.EnclosingTypeParameters |> ImArray.map (fun tyPar -> tyPar.AsType)).AddRange(tyArgs)
                             let tyArgs =
                                (tyPars, tyArgs)
                                ||> ImArray.map2 (fun tyPar tyArg ->
                                    mkSolvedInferenceVariableType tyPar tyArg
                                )
                             let newValue = actualValue value.Enclosing tyArgs value.Formal
                             BoundExpression.Call(
                                 syntaxInfo,
                                 None,
                                 witnessArgs,
                                 argExprs,
                                 newValue,
                                 isVirtualCall
                             )
                         else
                            origExpr
                | _ ->
                    origExpr
            ),
            fun _ -> true
        )

    newExpr