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
            E.TryImplicitCalls_LoadFunction(argExprs, appliedNewValue)

    E.Call(
        syntaxInfo,
        receiverOpt,
        subbedWitnessArgs,
        newArgExprs,
        appliedNewValue,
        isVirtualCall
    )

let private substituteConstant(tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, constant: ConstantSymbol) =
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

let private substituteLiteral(tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, literal: BoundLiteral) =
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

let private substituteField(tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, field: IFieldSymbol) =
    let enclosingTy = field.Enclosing.AsType
    let newEnclosingTy = enclosingTy.Substitute(tyParLookup)
    if areTypesEqual enclosingTy newEnclosingTy then
        field
    else
        actualField (EnclosingSymbol.Entity(newEnclosingTy.AsEntity)) newEnclosingTy.TypeArguments field.Formal.AsField

/// Do not use for LambdaLifting.
let private substituteValue(benv: BoundEnvironment, valueLookup: Dictionary<int64, IValueSymbol>, tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, value: IValueSymbol) =
    match valueLookup.TryGetValue value.Formal.Id with
    | true, newValue ->
        if value.IsFormal then
            if newValue.TypeParameters.IsEmpty then
                newValue
            else
                OlyAssert.True(newValue.IsFormal)
                let newValue2 = freshenValue benv newValue
                let result = UnifyTypes Flexible newValue2.Type value.Type
                OlyAssert.True(result)
                newValue2
        else
            let tyArgs = value.TypeArguments
            OlyAssert.True(newValue.IsFormal)
            actualValue newValue.Enclosing tyArgs newValue
    | _ ->
        if value.HasLocalEnclosing then
            if value.TypeParameters.IsEmpty then
                value
            else
                OlyAssert.False(value.IsFormal)
                let tyArgs = 
                    value.TypeArguments
                    |> ImArray.map (fun tyArg -> tyArg.Substitute(tyParLookup))
                    |> ImArray.append (
                        benv.EnclosingTypeParameters 
                        |> ImArray.map (fun tyPar -> tyPar.AsType)
                    )
                let tyPars = benv.EnclosingTypeParameters.AddRange(value.TypeParameters)
                let tyArgs =
                    (tyPars, tyArgs)
                    ||> ImArray.map2 (fun tyPar tyArg ->
                        mkSolvedInferenceVariableType tyPar tyArg
                    )
                actualValue value.Enclosing tyArgs value.Formal
        else
            if value.AllTypeParameters.IsEmpty then
                value
            else
                // TODO:
                value
                //let enclosingTy = value.Enclosing.AsType
                //let newEnclosingTy = enclosingTy.Substitute(tyParLookup)
                //let tyArgs =
                //    newEnclosingTy.TypeArguments.AddRange(
                //        value.TypeArguments 
                //        |> ImArray.map (fun tyArg -> tyArg.Substitute(tyParLookup))
                //    )
                //actualValue (EnclosingSymbol.Entity(newEnclosingTy.AsEntity)) tyArgs value.Formal

let private substituteLocal(benv: BoundEnvironment, valueLookup: Dictionary<int64, IValueSymbol>, tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, local: ILocalSymbol) =
    OlyAssert.False(local.IsFunction)
    substituteValue(benv, valueLookup, tyParLookup, local).AsLocal

let private substitutePattern(benv: BoundEnvironment, valueLookup: Dictionary<int64, IValueSymbol>, tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, pat: IPatternSymbol) =
    substituteValue(benv, valueLookup, tyParLookup, pat).AsPattern

let private substituteLocalLetBinding(valueLookup: Dictionary<int64, IValueSymbol>, tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, local: ILocalSymbol) =
    let newLocal = 
        if local.IsMutable then
            createMutableLocalValue local.Name (local.Type.Substitute(tyParLookup))
        else
            createLocalValue local.Name (local.Type.Substitute(tyParLookup))
    valueLookup[local.Id] <- newLocal
    newLocal

let private substituteCasePattern(benv: BoundEnvironment, valueLookup: Dictionary<int64, IValueSymbol>, tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, casePat: BoundCasePattern) =
    match casePat with
    | BoundCasePattern.Discard _ -> casePat

    | BoundCasePattern.Tuple(syntaxInfo, casePats) ->
        let newCasePats =
            casePats
            |> ImArray.map (fun casePat -> substituteCasePattern(benv, valueLookup, tyParLookup, casePat))
        BoundCasePattern.Tuple(syntaxInfo, newCasePats)

    | BoundCasePattern.Literal(syntaxInfo, literal) ->
        let newLiteral = substituteLiteral(tyParLookup, literal)
        BoundCasePattern.Literal(syntaxInfo, newLiteral)

    | BoundCasePattern.FieldConstant(syntaxInfo, field) ->
        let newField = substituteField(tyParLookup, field)
        if obj.ReferenceEquals(newField, field) then
            casePat
        else
            BoundCasePattern.FieldConstant(syntaxInfo, newField)

    | BoundCasePattern.Local(syntaxInfo, local) ->
        let newLocal = substituteLocalLetBinding(valueLookup, tyParLookup, local)
        BoundCasePattern.Local(syntaxInfo, newLocal)

    | BoundCasePattern.Function(syntaxInfo, pat, witnessArgs, casePatArgs) ->
        let newWitnessArgs = 
            WitnessSolution.EmplaceSubstitute(
                witnessArgs,
                tyParLookup
            )
        let newPat = substitutePattern(benv, valueLookup, tyParLookup, pat)
        let newCasePatArgs =
            casePatArgs
            |> ImArray.map (fun casePat -> substituteCasePattern(benv, valueLookup, tyParLookup, casePat))
        BoundCasePattern.Function(syntaxInfo, newPat, newWitnessArgs, newCasePatArgs)

let private substituteMatchPattern(benv: BoundEnvironment, valueLookup: Dictionary<int64, IValueSymbol>, tyParLookup: IReadOnlyDictionary<int64, TypeSymbol>, matchPat: BoundMatchPattern) =
    match matchPat with
    | BoundMatchPattern.Cases(syntax, casePats) ->
        let newCasePats =
            casePats
            |> ImArray.map (fun casePat ->
                substituteCasePattern(benv, valueLookup, tyParLookup, casePat)
            )
        BoundMatchPattern.Cases(syntax, newCasePats)
    | BoundMatchPattern.Or(syntax, lhsMatchPat, rhsMatchPat) ->
        let newLhsMatchPat = substituteMatchPattern(benv, valueLookup, tyParLookup, lhsMatchPat)
        let newRhsMatchPat = substituteMatchPattern(benv, valueLookup, tyParLookup, rhsMatchPat)
        BoundMatchPattern.Or(syntax, newLhsMatchPat, newRhsMatchPat)

let substituteForAutoGeneralization
        (
            benv: BoundEnvironment,
            expr: E, 
            valueLookup: Dictionary<int64, IValueSymbol>,
            tyParLookup: Dictionary<int64, TypeSymbol>
        ) =
    expr.Rewrite(
        (fun origExpr ->
            match origExpr with
            | E.Let(syntaxInfo, BindingLocal(local), rhsExpr, bodyExpr) ->
                let newLocal = substituteLocalLetBinding(valueLookup, tyParLookup, local)
                E.Let(
                    syntaxInfo, 
                    LocalBindingInfoSymbol.BindingLocal(newLocal), 
                    rhsExpr, 
                    bodyExpr
                )

            | E.Value(syntaxInfo, value) when value.HasLocalEnclosing && not value.IsFunction ->
                let newLocal = substituteLocal(benv, valueLookup, tyParLookup, value.AsLocal).AsLocal
                if obj.ReferenceEquals(newLocal, value) then
                    origExpr
                else
                    E.Value(syntaxInfo, newLocal)

            | E.SetValue(syntaxInfo, value, rhsExpr) when value.HasLocalEnclosing && not value.IsFunction ->
                match valueLookup.TryGetValue value.Formal.Id with
                | true, newValue ->          
                    E.SetValue(syntaxInfo, newValue, rhsExpr)
                | _ ->
                    origExpr    
                    
            | E.GetField(syntaxInfo, receiverExprOpt, field) ->
                let newField = substituteField(tyParLookup, field)
                if obj.ReferenceEquals(newField, field) then
                    origExpr
                else
                    E.GetField(syntaxInfo, receiverExprOpt, newField)

            | E.SetField(syntaxInfo, receiverExprOpt, field, rhsExpr, isCtorInit) ->
                let newField = substituteField(tyParLookup, field)
                if obj.ReferenceEquals(newField, field) then
                    origExpr
                else
                    E.SetField(syntaxInfo, receiverExprOpt, newField, rhsExpr, isCtorInit)
                    
            | E.GetProperty(syntaxInfo, receiverExprOpt, prop, isVirtual) ->
                let enclosingTy = prop.Enclosing.AsType
                let newEnclosingTy = enclosingTy.Substitute(tyParLookup)
                if areTypesEqual enclosingTy newEnclosingTy then
                    origExpr
                else
                    let newProp = actualProperty (EnclosingSymbol.Entity(newEnclosingTy.AsEntity)) newEnclosingTy.TypeArguments prop.Formal.AsProperty
                    E.GetProperty(syntaxInfo, receiverExprOpt, newProp, isVirtual)

            | E.SetProperty(syntaxInfo, receiverExprOpt, prop, rhsExpr, isVirtual) ->
                let enclosingTy = prop.Enclosing.AsType
                let newEnclosingTy = enclosingTy.Substitute(tyParLookup)
                if areTypesEqual enclosingTy newEnclosingTy then
                    origExpr
                else
                    let newProp = actualProperty (EnclosingSymbol.Entity(newEnclosingTy.AsEntity)) newEnclosingTy.TypeArguments prop.Formal.AsProperty
                    E.SetProperty(syntaxInfo, receiverExprOpt, newProp, rhsExpr, isVirtual)

            | E.Match(syntax, benv, matchItemExprs, matchClauses, cachedExprTy) ->
                let newMatchClauses =
                    matchClauses
                    |> ImArray.map (fun matchClause ->
                        match matchClause with
                        | BoundMatchClause.MatchClause(syntax, matchPat, guardExprOpt, targetExpr) ->
                            let newMatchPat = substituteMatchPattern(benv, valueLookup, tyParLookup, matchPat)
                            BoundMatchClause.MatchClause(syntax, newMatchPat, guardExprOpt, targetExpr)
                    )
                E.Match(syntax, benv, matchItemExprs, newMatchClauses, cachedExprTy)

            | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, value, isVirtualCall) ->
                let witnessArgs = 
                    WitnessSolution.EmplaceSubstitute(
                        witnessArgs,
                        tyParLookup
                    )

                let newValue = substituteValue(benv, valueLookup, tyParLookup, value)
                E.Call(
                    syntaxInfo,
                    receiverExprOpt,
                    witnessArgs,
                    argExprs,
                    newValue,
                    isVirtualCall
                )
            | _ ->
                origExpr
        ),
        id,
        fun _ -> true
    )

/// Substitutes type variables with the new types and values with new values in the entire expression.
/// This is a little different than subsitution for auto-generalization when it comes to the "valueLookup".
/// Some special rules:
///     - TODO: Document special rules here. Such as if the old value was a local and the new value is a instance field, it handles this by calling handleReceiverExpr callback.
let substituteForLambdaLifting
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
                | E.GetProperty _
                | E.SetProperty _
                | E.Match _ -> failwith "expected expression to have been lowered"

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

                | E.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
                    let newBindingInfo =
                        match bindingInfo with
                        | BindingLocal(value) ->
                            OlyAssert.True(value.HasLocalEnclosing)
                            let newValue = 
                                LocalSymbol(value.Name, value.Formal.Type.Substitute(tyParLookup), value.IsGenerated, value.IsMutable)

                            valueLookup[value.Id] <- newValue

                            BindingLocal(newValue)
                        | BindingLocalFunction(func) ->
                            // TODO: Handle type parameter constraints as they may have captured type variables we need to substitute.
                            OlyAssert.True(func.IsFormal)
                            OlyAssert.True(func.HasLocalEnclosing)

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

                    E.Let(syntaxInfo, newBindingInfo, rhsExpr, bodyExpr)
                | _ ->
                    origExpr
            ),
            (fun origExpr ->
                match origExpr with
                | E.GetField(syntaxInfo, receiverExpr, field) ->
                    if areTypesEqual (stripByRef receiverExpr.Type) field.Enclosing.AsType then
                        origExpr
                    else
                        let newField = (stripByRef receiverExpr.Type).FindField(field.Name)
                        E.GetField(syntaxInfo, receiverExpr, newField)

                | E.SetField(syntaxInfo, receiverExpr, field, rhsExpr, isCtorInit) ->
                    if areTypesEqual (stripByRef receiverExpr.Type) field.Enclosing.AsType then
                        origExpr
                    else
                        let newField = (stripByRef receiverExpr.Type).FindField(field.Name)
                        E.SetField(syntaxInfo, receiverExpr, newField, rhsExpr, isCtorInit)

                | E.Literal(syntaxInfo, literal) ->
                    let newLiteral = substituteLiteral(tyParLookup, literal)

                    if newLiteral = literal then
                        origExpr
                    else
                        E.Literal(syntaxInfo, newLiteral)

                | E.Value(syntaxInfo, value) ->
                    match valueLookup.TryGetValue value.Formal.Id with
                    | true, newValue -> 

                        let appliedNewValue = 
                            if value.IsFunction then
                                let tyPars =
                                    // Special!
                                    if value.HasLocalEnclosing && not newValue.HasLocalEnclosing then
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
                            if value.HasLocalEnclosing && newValue.IsInstance then
                                if newValue.IsFunction then
                                    OlyAssert.True(value.Type.IsAnyFunction_ste)
                                    OlyAssert.False(newValue.IsConstructor)
                                    OlyAssert.True(newValue.Enclosing.IsClosure)
                                    OlyAssert.True(value.Type.IsAnyFunction_ste)
                                    handleReceiverExpr newValue
                                else
                                    OlyAssert.True(newValue.IsField)
                                    let getFieldExpr =
                                        E.GetField(
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
                                let valueExpr = E.Value(syntaxInfo, appliedNewValue)
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
                        E.Value(syntaxInfo, appliedValue)

                | E.SetValue(syntaxInfo, value, rhsExpr) ->
                    match valueLookup.TryGetValue value.Formal.Id with
                    | true, newValue ->

                        let tyPars =
                            // Special!
                            if value.HasLocalEnclosing && not newValue.HasLocalEnclosing then
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
                                E.SetContentsOfAddress(
                                    syntaxInfo, 
                                    E.GetField(syntaxInfo, handleReceiverExpr newValue, appliedNewField),
                                    rhsExpr
                                )
                            | _ ->
                                E.SetContentsOfAddress(
                                    syntaxInfo, 
                                    E.CreateGeneratedValue(syntaxInfo.Syntax, appliedNewValue),
                                    rhsExpr
                                )
                        else
                            match appliedNewValue with
                            | :? IFieldSymbol as appliedNewField ->
                                E.SetField(syntaxInfo, handleReceiverExpr newValue, appliedNewField, rhsExpr, isCtorInit = false)
                            | _ ->
                                E.SetValue(syntaxInfo, appliedNewValue, rhsExpr)
                    | _ ->
                        let allTyArgs =
                            (value.AllTypeParameters, value.AllTypeArguments) 
                            ||> ImArray.map2 (fun tyPar tyArg -> 
                                mkSolvedInferenceVariableType tyPar (tyArg.Substitute(tyParLookup))
                            )

                        let appliedValue = value.Formal.Substitute(allTyArgs)
                        E.SetValue(syntaxInfo, appliedValue, rhsExpr)

                | E.Call(syntaxInfo, receiverOpt, witnessArgs, argExprs, value, isVirtualCall) ->
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
                                    E.Value(syntaxInfo, newValue)
                                    |> Some
                                else
                                    receiverOpt

                        let tyPars =
                            // Special!
                            if value.HasLocalEnclosing && not newValue.HasLocalEnclosing then
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
                                E.TryImplicitCalls_LoadFunction(argExprs, appliedClosureInvoke)

                            E.Call(
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
                                E.TryImplicitCalls_LoadFunction(argExprs, appliedValue)

                        E.Call(
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

let substituteValuesForLambdaLifting
    (expr, substitutions: (int64 * IValueSymbol) imarray)
    =
    let valueLookup = Dictionary()
    substitutions
    |> ImArray.iter (fun (id, value) ->
        valueLookup[id] <- value
    )
    substituteForLambdaLifting(expr, Dictionary(), valueLookup, fun _ -> OlyAssert.Fail("handleReceiverExpr"))