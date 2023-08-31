module internal Oly.Compiler.Internal.Checker

open Oly.Core
open Oly.Compiler.Syntax
open System.Collections.Generic
open System.Collections.Immutable
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SemanticDiagnostics
open Oly.Compiler.Internal.SymbolEnvironments

let createGeneralizedFunctionTypeParameters (env: SolverEnvironment) (syntaxNode: OlySyntaxNode) (freeInputTyVars: ResizeArray<_>) (tyPars: ImmutableArray<TypeParameterSymbol>) =
    // TODO: Remove 'tyPars' as it is only used to check if it is empty or not.

    let generalizedTyPars = ResizeArray()
    let mutable tyParIndex = env.benv.EnclosingTypeParameters.Length
    let mutable nextTyParName = 'T'
    let rec computeNextTyParName () =
        if nextTyParName = 'z' then
            env.diagnostics.Error("The function definition was unable to generalize type parameters.", 10, syntaxNode)
        else
            let nextName =
                let name = 
                    if nextTyParName = 'S' then
                        'a'
                    else
                        nextTyParName + char 1
                if name > 'Z' then
                    'A'
                else
                    name

            match env.benv.TryFindTypeParameter (string nextName) with
            | Some _ ->
                computeNextTyParName()
            | _ ->
                nextTyParName <- nextName

    let addInferenceVariableTy ty =
        match stripTypeEquations ty with
        | TypeSymbol.InferenceVariable(tyParOpt, solution) 
        | TypeSymbol.HigherInferenceVariable(tyParOpt, _, _, solution) when not solution.HasSolution ->
            if tyPars.IsEmpty then
                let arity =
                    match tyParOpt with
                    | Some(tyPar) -> tyPar.Arity
                    | _ -> 0

                let newTyPar = TypeParameterSymbol(string nextTyParName, tyParIndex, arity, TypeParameterKind.Function tyParIndex, ref ImArray.empty)

                // TODO: Prevent duplicate constraints.
                // TODO: This may not handle the 'oldConstrEnt''s free type vars.
                let oldConstrs =
                    match tyParOpt with
                    | None -> solution.Constraints |> ImArray.ofSeq
                    | Some tyPar -> tyPar.Constraints

                let newConstrs =
                    oldConstrs
                    |> ImArray.map (fun constr ->
                        match constr with
                        | ConstraintSymbol.Null
                        | ConstraintSymbol.Struct
                        | ConstraintSymbol.NotStruct
                        | ConstraintSymbol.Unmanaged
                        | ConstraintSymbol.Scoped
                        | ConstraintSymbol.ConstantType _ ->
                            constr
                        | ConstraintSymbol.SubtypeOf(oldConstrTy) ->
                            let tyArgs = ImArray.createOne (mkSolvedInferenceVariableType newTyPar newTyPar.AsType)
                            ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(oldConstrTy.Value.Substitute(tyArgs)))
                    )

                newTyPar.SetConstraints(newConstrs)

                solution.Solution <- TypeSymbol.Variable(newTyPar)
                generalizedTyPars.Add(newTyPar)
                tyParIndex <- tyParIndex + 1
                computeNextTyParName()
            else
                env.diagnostics.Error(sprintf "Unable to infer the type. Be explicit i.e. 'x: int32'.", 6, syntaxNode)
        | TypeSymbol.Variable(tyPar) when tyPar.Arity > 0 ->
            env.diagnostics.Error(sprintf "Unable to infer the type variable with an arity greater than zero.", 6, syntaxNode)
            ()
        | _ ->
            ()

    freeInputTyVars
    |> Seq.iter (fun struct(_, ty) ->
        addInferenceVariableTy ty
    )

    generalizedTyPars
    |> ImmutableArray.CreateRange

let checkAmbiguousFunctionsFromEntity env (syntaxNode: OlySyntaxNode) (ent: EntitySymbol) =
    let ambiguousFunctions: (IFunctionSymbol * IFunctionSymbol) seq =
        ent.Formal.Functions
        |> Seq.map (fun func ->
            ent.AllLogicalInheritsAndImplements
            |> Seq.tryPick (fun ent2 ->
                ent2.Functions
                |> Seq.tryPick (fun func2 ->
                    if areLogicalFunctionSignaturesEqual func func2 then
                        Some func2
                    else
                        None
                )
            )
            |> Option.map (fun func2 ->
                (func, func2)
            )
        )
        |> Seq.choose id

    ambiguousFunctions
    |> Seq.iter (fun (func, func2) ->
        env.diagnostics.Error(sprintf "The function '%s' is ambiguous due to '%s' having the same signature." func.Name (printEnclosing env.benv func2.Enclosing), 10, syntaxNode)
    )

let rec checkTypeScope (env: SolverEnvironment) (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) =
    match ty.Enclosing with
    | EnclosingSymbol.Local ->
        match stripTypeEquations ty with
        | TypeSymbol.Entity(ent) ->        
            let isOutOfScope =
                ent.TypeArguments
                |> Seq.exists (fun ty -> checkTypeScope env syntaxNode ty |> not)
                ||
                let arity = ent.LogicalTypeParameterCount - env.benv.EnclosingTypeParameters.Length
                let tys = env.benv.GetUnqualifiedType(ent.Name, arity)
                if tys.IsEmpty then
                    false
                elif tys.Length = 1 then
                    ty.FormalId <> tys[0].FormalId
                else
                    false

            if isOutOfScope then
                env.diagnostics.Error(sprintf "The type '%s' at this point will be considered out of scope from where it was defined." ent.Name, 5, syntaxNode)

            not isOutOfScope
        | _ ->
            true
    | _ ->
        true

let checkFunctionType env (syntaxNode: OlySyntaxNode) (argExprs: BoundExpression imarray) (valueTy: TypeSymbol) =
    match valueTy.TryGetFunctionWithParameters() with
    | ValueSome(expectedArgTys, _) ->        
        let argTysWithSyntax =
            argExprs
            |> ImArray.map (fun x -> (x.Type, x.FirstReturnExpression.Syntax))
        solveFunctionInput env syntaxNode expectedArgTys argTysWithSyntax
    | _ ->
        if not valueTy.IsError_t then
            env.diagnostics.Error(sprintf "Not a function.", 3, syntaxNode)

// --------------------------------------------------------------------------------------------------

[<Literal>]
let private checkStructCycleDictionaryPoolMaxCount = 10
let private checkStructCycleDictionaryPool = 
    let stack = System.Collections.Concurrent.ConcurrentStack<Dictionary<EntitySymbol, bool>>()
    for _ = 1 to checkStructCycleDictionaryPoolMaxCount do
        stack.Push(Dictionary(EntitySymbolComparer()))
    stack

let private tryPopCheckStructCycleDictionary(result: byref<Dictionary<_, _>>) =
    checkStructCycleDictionaryPool.TryPop(&result)

let private pushCheckStructCycleDictionary(dict: Dictionary<_, _>) =
    dict.Clear()
    checkStructCycleDictionaryPool.Push(dict)

let rec private checkStructCycleInner (ent: EntitySymbol) (hash: Dictionary<_, _>) =
    match hash.TryGetValue ent with
    | true, result -> result
    | _ ->
        hash.[ent] <- false
        let mutable result =
            (ent.Fields)
            |> ImArray.forall (fun field ->
                if field.IsInstance && field.Type.IsAnyStruct then
                    match field.Type.TryEntity with
                    | ValueSome(ent) ->
                        checkStructCycleInner ent hash
                    | _ ->
                        true
                else
                    true
            )
        hash.[ent] <- result
        result

let checkStructCycle env syntaxNode (ent: EntitySymbol) =
    OlyAssert.True(ent.IsAnyStruct)

    let mutable hash = Unchecked.defaultof<_>
    let usedPool = tryPopCheckStructCycleDictionary(&hash)
    if not usedPool then
        hash <- Dictionary(EntitySymbolComparer())

    let result = checkStructCycleInner ent hash

    if usedPool then
        pushCheckStructCycleDictionary(hash)

    if not result then
        if ent.IsFormal || ent.TypeArguments.IsEmpty then
            env.diagnostics.Error(sprintf "'%s' has fields that cause a cycle." (printEntity env.benv ent), 10, syntaxNode)
        else
            env.diagnostics.Error(sprintf "'%s' is causing a cycle on a struct." (printEntity env.benv ent), 10, syntaxNode)

        false
    else
        true

let rec checkStructTypeCycle env syntaxNode (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) -> 
        if ent.IsAnyStruct then
            checkStructCycle env syntaxNode ent
        else
            true
    | TypeSymbol.HigherVariable(_, tyArgs) ->
        tyArgs
        |> ImArray.forall (fun ty -> checkStructTypeCycle env syntaxNode ty)
    | TypeSymbol.HigherInferenceVariable(_, tyArgs, _, _) ->
        tyArgs
        |> ImArray.forall (fun ty -> checkStructTypeCycle env syntaxNode ty)
    | TypeSymbol.Tuple(tyArgs, _) ->
        tyArgs
        |> ImArray.forall (fun ty -> checkStructTypeCycle env syntaxNode ty)
    | TypeSymbol.ForAll(_, innerTy) ->
        checkStructTypeCycle env syntaxNode innerTy
    | _ ->
        match ty.TryGetFunctionWithParameters() with
        | ValueSome(argTys, returnTy) ->
            if argTys |> ImArray.forall (fun ty -> checkStructTypeCycle env syntaxNode ty) then
                checkStructTypeCycle env syntaxNode returnTy
            else
                false
        | _ ->
            true

// --------------------------------------------------------------------------------------------------

let checkEntityConstructor env syntaxNode skipUnsolved (syntaxTys: OlySyntaxType imarray) (ent: EntitySymbol) =
    if ent.IsAnyStruct then
        checkStructCycle env syntaxNode ent
        |> ignore

    let tyPars = ent.TypeParameters
    let tyArgs = ent.TypeArguments

    let tyPars, tyArgs =
        let skipAmount = tyPars.Length - syntaxTys.Length
        tyPars |> ImArray.skip skipAmount, tyArgs |> ImArray.skip skipAmount

    (tyPars, tyArgs)
    ||> Seq.iteri2 (fun i tyPar tyArg ->
        if tyPar.Arity > 0 then
            match stripTypeEquations tyArg with
            | TypeSymbol.Error _ -> ()
            | _ ->
                if tyPar.Arity <> tyArg.Arity then
                    let syntaxTy = syntaxTys.[i]
                    env.diagnostics.Error(sprintf "Type instantiation '%s' has a type arity of %i, but expected %i." (printType env.benv tyArg) tyArg.Arity tyPar.Arity, 10, syntaxTy)
    )

    solveConstraints
        env
        skipUnsolved
        syntaxNode
        (if syntaxTys.IsEmpty then None else Some syntaxTys)
        tyArgs
        ImArray.empty (* type constructors do not support witnesses *)

let checkTypeConstructor env syntaxNode skipUnsolved (syntaxTys: OlySyntaxType imarray) ty =
    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) ->
        checkEntityConstructor env syntaxNode skipUnsolved syntaxTys ent
    | _ ->
        ()

/// Only used in pass3 to check types of value declarations
let rec checkTypeConstructorDepth env (syntaxNode: OlySyntaxNode) (syntaxTys: OlySyntaxType imarray) (ty: TypeSymbol) =
    let tyArgs = ty.TypeArguments
    let skipAmount = tyArgs.Length - syntaxTys.Length

    if skipAmount >= 0 then
        let tyArgs = tyArgs |> ImArray.skip skipAmount
        (syntaxTys, tyArgs)
        ||> ImArray.iter2 (fun syntaxTy tyArg ->
            match syntaxTy with
            | OlySyntaxType.Name(syntaxName) ->
                match syntaxName with
                | OlySyntaxName.Generic(_, syntaxTyArgsRoot) ->
                    checkTypeConstructorDepth env syntaxTyArgsRoot syntaxTyArgsRoot.Values tyArg
                | _ ->
                    checkTypeConstructorDepth env syntaxName ImArray.empty tyArg
            | _ ->
                ()
        )

    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) ->
        checkEntityConstructor env syntaxNode (* skipUnsolved *) false syntaxTys ent
    | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
        tyPar.Constraints
        |> ImArray.iter (fun constr ->
            match constr.TryGetSubtypeOf() with
            // If the constraint type has any type parameter constructors, then we skip this check
            // as it has already failed elsewhere. We do not support further "higher-rank" types.
            | ValueSome(constrTy) when constrTy.TypeParameters |> ImArray.forall (fun x -> x.HasArity |> not) ->
                if constrTy.IsTypeConstructor then
                    checkTypeConstructorDepth env syntaxNode syntaxTys (applyType constrTy tyArgs)
                else
                    checkTypeConstructorDepth env syntaxNode ImArray.empty (actualType tyArgs constrTy)
            | _ ->
                ()
        )
    | _ ->
        ()

and checkTypeConstructorDepthWithType env (syntaxTy: OlySyntaxType) ty =
    match syntaxTy with
    | OlySyntaxType.Name(syntaxName) ->
        match syntaxName with
        | OlySyntaxName.Generic(syntaxName, syntaxTyArgsRoot) ->
            checkTypeConstructorDepth env syntaxName syntaxTyArgsRoot.Values ty
        | _ ->
            checkTypeConstructorDepth env syntaxName ImArray.empty ty
    | _ ->
        ()

let rec checkConstraintClauses (env: SolverEnvironment) (syntaxConstrClauses: OlySyntaxConstraintClause imarray) (tyPars: TypeParameterSymbol imarray) =
    // Check the constraints' type constructors after we have binded everything.
    // This allows to write the constraints in any order.
    forEachConstraintBySyntaxConstraintClause syntaxConstrClauses tyPars (fun syntaxConstrClause _tyPar constrs ->
        match syntaxConstrClause.TryConstraints with
        | ValueSome syntaxConstrs ->
            (syntaxConstrs, constrs)
            ||> ImArray.tryIter2 (fun syntaxConstr constr ->
                match constr.TryGetSubtypeOf() with
                | ValueSome constrTy ->
                    match syntaxConstr with
                    | OlySyntaxConstraint.Type(syntaxTy) ->       
                        match syntaxTy with
                        | OlySyntaxType.Name(syntaxName) ->
                            match syntaxName with
                            | OlySyntaxName.Generic(_, syntaxTyArgsRoot) ->
                                checkTypeConstructor env syntaxTyArgsRoot (* skipUnsolved *) false syntaxTyArgsRoot.Values constrTy
                            | _ ->
                                ()
                        | _ ->
                            ()
                    | _ ->
                        ()
                | _ ->
                    ()
            )
        | _ ->
            ()
    )

and checkImplementation env (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) (super: TypeSymbol) =
    let funcsNotImplemented =
        super.FindIntrinsicFunctions(env.benv, QueryMemberFlags.Overridable, FunctionFlags.None)
        |> ImArray.filter (fun func -> func.IsAbstract)

    funcsNotImplemented
    |> ImArray.iter (fun func ->
        let queryMemberFlags =
            if func.IsInstance then
                QueryMemberFlags.InstanceFunctionOverrides
            else
                QueryMemberFlags.Static // TODO: StaticFunctionOverrides?
        
        let possibleFuncs = 
            ty.FindIntrinsicFunctions(env.benv, queryMemberFlags, FunctionFlags.None, func.Name)
            |> Seq.filter (fun x ->
                if x.Id <> func.Id then
                    if x.IsVirtual then
                        match x.FunctionOverrides with
                        | Some overridenFunc -> areLogicalFunctionSignaturesEqual overridenFunc func
                        | _ -> areLogicalFunctionSignaturesEqual x func
                    else
                        false
                else
                    false
            )
            |> ImArray.ofSeq

        let possibleFuncs =
            if possibleFuncs.IsEmpty then
                ty.FindIntrinsicFunctions(env.benv, queryMemberFlags, FunctionFlags.None)
                |> Seq.filter (fun x ->
                    if x.IsVirtual then
                        match x.FunctionOverrides with
                        | Some overridenFunc -> areLogicalFunctionSignaturesEqual overridenFunc func
                        | _ -> false
                    else
                        false
                )
                |> ImArray.ofSeq
            else
                possibleFuncs
        
        match possibleFuncs |> List.ofSeq with
        | [] ->
            env.diagnostics.Error(sprintf "The function '%s' is not implemented for '%s' on '%s'." (printValue env.benv func) (printEnclosing env.benv func.Enclosing) (PrettyPrint.printType env.benv ty), 0, syntaxNode)
        | [_] -> ()
        | _ ->
            env.diagnostics.Error(sprintf "The function '%s' is ambiguous." (printValue env.benv func), 0, syntaxNode)
    )

and checkInterfaceDefinition (env: SolverEnvironment) (syntaxNode: OlySyntaxNode) (ent: EntitySymbol) =

    ent.Extends
    |> ImArray.iter (fun ty ->
        if not ty.IsInterface then
            env.diagnostics.Error(sprintf "Cannot inherit the construct '%s'." (printType env.benv ty), 10, syntaxNode)
    )

    checkAmbiguousFunctionsFromEntity env syntaxNode ent

and checkLambdaExpression (env: SolverEnvironment) (pars: ImmutableArray<ILocalParameterSymbol>) (body: BoundExpression) (ty: TypeSymbol) =
    if ty.IsError_t then ()
    else
        match ty.TryGetFunctionWithParameters() with
        | ValueSome(argTys, returnTy) ->
            let syntaxBody = body.Syntax
            let argTysWithSyntax = pars |> ImArray.map (fun x -> (x.Type, syntaxBody))

            solveFunctionInput env syntaxBody argTys argTysWithSyntax

        | _ ->
            OlyAssert.Fail("Expected a function type.")

and private checkLambdaFunctionValueBindingAndAutoGeneralize env isStatic (syntax: OlySyntaxBinding) (binding: LocalBindingInfoSymbol) (rhsExpr: BoundExpression) (pars: ImmutableArray<ILocalParameterSymbol>) (body: BoundExpression) =
    let benv = env.benv
    let value = binding.Value

    let returnTy = body.Type

    // TODO: We do this even on non-local bindings because we may want to run the bodies of lambdas.
    //       Perhaps we could do this without having to go looking for inference variables.
    let freeInputTyVars = rhsExpr.GetFreeInferenceVariables()

    let funcFlags =
        if value.IsLocal && isStatic then
            FunctionFlags.StaticLocal
        else
            FunctionFlags.None
    
    if freeInputTyVars.Count > 0 && value.IsLocal then
        let generalizedTyPars =
            createGeneralizedFunctionTypeParameters
                env
                syntax
                freeInputTyVars
                ImmutableArray.Empty
        let func = createFunctionValue value.Enclosing ImmutableArray.Empty value.Name generalizedTyPars pars returnTy MemberFlags.Private funcFlags WellKnownFunction.None None false
        let bindingInfo = BindingLocalFunction(func)
        bindingInfo
    else
        let func = createFunctionValue value.Enclosing ImmutableArray.Empty value.Name ImmutableArray.Empty pars returnTy MemberFlags.Private funcFlags WellKnownFunction.None None false
        let bindingInfo = BindingLocalFunction(func)
        bindingInfo

and checkConstructorImplementation (env: SolverEnvironment) (thisValue: IValueSymbol) (enclosingTy: TypeSymbol) (expr: BoundExpression) =
    let rec loop (expr: BoundExpression) : BoundExpression =
        match expr with
        | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
            let newBodyExpr = loop bodyExpr
            if newBodyExpr = bodyExpr then
                expr
            else
                BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, newBodyExpr)

        | BoundExpression.Sequential(syntaxInfo, expr1, expr2, NormalSequential) ->
            let newExpr2 = loop expr2
            if newExpr2 = expr2 then
                expr
            else
                BoundExpression.Sequential(syntaxInfo, expr1, expr2, NormalSequential)

        | BoundExpression.Match(syntax, benv, matchExprs, matchClauses, cachedExprTy) ->
            let mutable hasNewMatchClauses = false
            let newMatchClauses =
                matchClauses
                |> ImArray.map (fun x ->
                    match x with
                    | BoundMatchClause.MatchClause(syntaxMatchClause, matchPat, guardExprOpt, targetExpr) ->
                        let newTargetExpr = loop targetExpr
                        if newTargetExpr = targetExpr then
                            x
                        else
                            hasNewMatchClauses <- true
                            BoundMatchClause.MatchClause(syntaxMatchClause, matchPat, guardExprOpt, newTargetExpr)
                )
            if hasNewMatchClauses then
                BoundExpression.Match(syntax, benv, matchExprs, newMatchClauses, cachedExprTy)
            else
                expr

        | BoundExpression.IfElse(syntaxInfo, conditionExpr, targetExpr1, targetExpr2, cachedExprTy) ->
            let newTargetExpr1 = loop targetExpr1
            let newTargetExpr2 = loop targetExpr2
            if newTargetExpr1 = targetExpr1 && newTargetExpr2 = targetExpr2 then
                expr
            else
                BoundExpression.IfElse(syntaxInfo, conditionExpr, newTargetExpr1, newTargetExpr2, cachedExprTy)

        | _ ->

        let canCheck =
            match expr with
            | BoundExpression.Call(value=value) when value.IsBase && value.IsFunction -> true
            | BoundExpression.SetField _
            | BoundExpression.Sequential _ -> true
            | _ -> false

        if canCheck then
            let expectedFields = enclosingTy.GetInstanceFields() |> ImArray.filter (fun x -> not x.IsFieldInit)
            let fields = expr.GetThisSetInstanceFields()
            let fieldNames = fields |> ImArray.map (fun x -> x.Name)
    
            let expectedFieldSet = HashSet(expectedFields |> Seq.filter (fun x -> not x.Type.IsError_t) |> Seq.map (fun x -> x.Name))
            expectedFieldSet.ExceptWith(fieldNames)
            expectedFieldSet
            |> Seq.sort
            |> Seq.iter (fun fieldName ->
                env.diagnostics.Error($"'{fieldName}' is not initialized.", 10, expr.Syntax)
            )

            expr
        else
            match expr with
            | BoundExpression.Call(value=value) when value.IsFunction && value.AsFunction.TryWellKnownFunction = ValueSome(WellKnownFunction.Throw) ->
                expr
            | BoundExpression.Call(syntaxInfo, None, witnessArgs, argExprs, value, isVirtual) 
                    when value.IsFunction && value.IsInstanceConstructor && areEnclosingsEqual value.Enclosing enclosingTy.AsEntity.AsEnclosing ->
                BoundExpression.Call(
                    syntaxInfo, 
                    Some(BoundExpression.Value(BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree), thisValue)),
                    witnessArgs,
                    argExprs,
                    value,
                    isVirtual
                )
            | _ ->
                env.diagnostics.Error("Invalid return expression for constructor.", 10, expr.Syntax)
                expr

    loop expr

and private checkValueBinding (env: SolverEnvironment) (rhsExpr: BoundExpression) (value: IValueSymbol) =

    let firstReturnExpression = rhsExpr.FirstReturnExpression

    // TODO: Should we actually Run?
    match firstReturnExpression with
    | BoundExpression.Lambda(body=lazyBody) ->
        if lazyBody.HasExpression |> not then
            lazyBody.Run()
    | _ ->
        ()

    let returnTy = 
        if firstReturnExpression.IsLambdaExpression then
            match firstReturnExpression.Type.TryFunction with
            | ValueSome(_, outputTy) -> outputTy
            | _ -> failwith "Expected a function type."
        else 
            firstReturnExpression.Type

    let syntax = firstReturnExpression.GetValidUserSyntax()

    if checkTypeScope env syntax returnTy then
        if value.IsInstanceConstructor then
            // Ignore the return type for constructors as we know it will be the enclosing.
            match value.Type.TryGetFunctionWithParameters(), firstReturnExpression.Type.TryGetFunctionWithParameters() with
            | ValueSome(argTys1, _), ValueSome(argTys2, _) ->
                let argTys2WithSyntax = argTys2 |> ImArray.map (fun x -> (x, syntax))
                solveFunctionInput env syntax argTys1 argTys2WithSyntax
            | _, _ ->
                solveTypes env syntax value.Type firstReturnExpression.Type

    syntax

and checkMemberBindingDeclaration (env: SolverEnvironment) (_syntaxBinding: OlySyntaxBinding) (binding: BindingInfoSymbol) (rhsExpr: BoundExpression) =
    checkValueBinding env rhsExpr binding.Value |> ignore
    binding, rhsExpr

and checkLetBindingDeclarationAndAutoGeneralize (env: SolverEnvironment) (syntaxBinding: OlySyntaxBinding) (binding: LocalBindingInfoSymbol) (rhsExpr: BoundExpression) =
    let syntax = checkValueBinding env rhsExpr binding.Value

    // Auto-generalization

    let bindingInfo2 =
        match binding with
        | BindingLocalFunction(func) when func.IsLocal ->
            let freeInputTyVars = rhsExpr.GetFreeInferenceVariables()

            if freeInputTyVars.Count > 0 && binding.Value.IsLocal then
                let generalizedTyPars = createGeneralizedFunctionTypeParameters env syntax freeInputTyVars func.TypeParameters
                let generalizedFunc = createFunctionWithTypeParametersOfFunction generalizedTyPars func
                BindingLocalFunction(generalizedFunc)
            else
                binding
        | BindingLocal(value) when not value.IsMutable ->
            match rhsExpr.Strip() with
            | BoundExpression.Lambda(_, lambdaFlags, _, parValues, body, _, _, _) ->
                checkLambdaFunctionValueBindingAndAutoGeneralize env (lambdaFlags.HasFlag(LambdaFlags.Static)) syntaxBinding binding rhsExpr parValues body.Expression
            | _ ->
                binding
        | _ ->
            match rhsExpr.Strip() with
            | BoundExpression.Lambda(body=bodyExpr) ->
                // TODO: We do this even on non-local bindings because we may want to run the bodies of lambdas.
                //       Perhaps we could do this without having to go looking for inference variables.
                let _freeInputTyVars = bodyExpr.Expression.GetFreeInferenceVariables()
                ()
            | _ ->
                ()
            binding

    let rhsExpr2 =
        match bindingInfo2 with
        | BindingLocalFunction(func=func) ->
            OlyAssert.True(func.IsLocal)
            if not func.TypeParameters.IsEmpty then
                // If the function has type parameters but the lambda expression does not, we probably generalized the function;
                //     therefore, we need to create a new lambda expression with those type parameters.
                match rhsExpr.Strip() with
                | BoundExpression.Lambda(syntaxInfo, lambdaFlags, tyPars, parValues, body, _, _, _) when tyPars.IsEmpty ->
                    BoundExpression.CreateLambda(syntaxInfo, lambdaFlags, func.TypeParameters, parValues, body)
                | _ ->
                    rhsExpr
            else
                rhsExpr
        | _ ->
            rhsExpr

    if bindingInfo2.Value.IsFunction then
        match bindingInfo2.Type.TryGetFunctionWithParameters(), rhsExpr2.Type.TryGetFunctionWithParameters() with
        | ValueSome(argTys1, _), ValueSome(argTys2, _) ->
            let argTys2WithSyntax = argTys2 |> ImArray.map (fun x -> (x, syntax))
            solveFunctionInput env syntax argTys1 argTys2WithSyntax
        | _ ->
            solveTypes env syntax bindingInfo2.Type rhsExpr2.Type

    bindingInfo2, rhsExpr2

and checkExpressionType (env: SolverEnvironment) (expectedTy: TypeSymbol) (boundExpr: BoundExpression) =
    let ty = boundExpr.Type

    if expectedTy.IsSolved && 
       ty.IsSolved && 
       not expectedTy.IsError_t && 
       not ty.IsError_t && 
       subsumesTypeInEnvironment env.benv expectedTy ty then
        let expectedTyArgs = expectedTy.TypeArguments
        let tyArgs =
            if areTypesEqual expectedTy ty then
                ty.TypeArguments
            else
                // This should always pass because it passed subsumption.
                match ty.AllLogicalInheritsAndImplements |> ImArray.tryFind (fun x -> areTypesEqual x expectedTy) with
                | Some(inheritOrImplementTy) ->
                    inheritOrImplementTy.TypeArguments
                | _ ->
                    ImArray.empty
        (expectedTyArgs, tyArgs)
        ||> ImArray.tryIter2 (fun expectedTyArg tyArg ->
            solveTypes env boundExpr.Syntax expectedTyArg tyArg
        )
    else
        solveTypes env boundExpr.Syntax expectedTy boundExpr.Type

and checkReceiverOfExpression (env: SolverEnvironment) (expr: BoundExpression) =
    let reportError name syntax =
        env.diagnostics.Error(sprintf "'%s' is not mutable." name, 10, syntax)
    
    let rec checkCall syntax (receiverOpt: BoundExpression option) (value: IValueSymbol) =
        match receiverOpt with
        | Some receiver when (value.Enclosing.IsAnyStruct || value.Enclosing.IsWitnessShape) ->
            if not value.IsReadOnly then
                if check value.Enclosing.IsWitnessShape receiver |> not then
                    env.diagnostics.Error(sprintf "Function call '%s' is not read-only and cannot be called on an immutable struct instance." value.Name, 10, syntax)
        | _ ->
            ()

    and checkAddressOf isWitnessShape (receiver: BoundExpression) =
        match receiver with
        | BoundExpression.Call(value=value;args=args) 
                when value.IsAddressOf ->
            check isWitnessShape args.[0]
        | _ ->
            true

    and check (isWitnessShape: bool) (receiver: BoundExpression) : bool =
        match receiver with
        | BoundExpression.Value(value=value) ->
            if (not value.IsMutable && (value.Type.IsAnyStruct || (isWitnessShape && not value.Type.IsReadWriteByRef))) || value.Type.IsReadOnlyByRef then
                reportError value.Name receiver.SyntaxNameOrDefault
                false
            else
                true
        | BoundExpression.GetField(receiver=receiver;field=field) ->
            if check false receiver then
                if field.Type.IsAnyStruct && not field.IsMutable then
                    reportError field.Name receiver.SyntaxNameOrDefault
                    false
                else
                    true
            else
                false

        | BoundExpression.GetProperty(syntaxInfo=syntaxInfo;receiverOpt=receiverOpt;prop=prop) ->
            match prop.Getter with
            | Some(getter) ->
                checkCall syntaxInfo.SyntaxNameOrDefault receiverOpt getter
            | _ ->
                ()
            true

        | BoundExpression.Sequential(expr2=expr2) ->
            check false expr2

        | _ ->
            checkAddressOf isWitnessShape receiver

    match expr with
    | BoundExpression.SetValue(value=value;rhs=rhs) ->
        checkExpressionType env value.Type rhs
        if not value.IsMutable then
            reportError value.Name expr.SyntaxNameOrDefault

    | BoundExpression.SetField(receiver=receiver;field=field;rhs=rhs) ->
        checkExpressionType env field.Type rhs
        if check false receiver then
            if not field.IsMutable then
                reportError field.Name expr.SyntaxNameOrDefault

    | BoundExpression.SetContentsOfAddress(lhs=lhsExpr) ->
        if not lhsExpr.Type.IsReadWriteByRef then
            env.diagnostics.Error("Cannot set contents of a read-only address.", 10, lhsExpr.Syntax)  

    | BoundExpression.SetProperty(syntaxInfo=syntaxInfo;receiverOpt=receiverOpt;prop=prop;rhs=rhs) ->
        match prop.Setter with
        | Some(setter) ->
            checkCall syntaxInfo.SyntaxNameOrDefault receiverOpt setter
        | _ ->
            ()

    | BoundExpression.GetProperty(syntaxInfo=syntaxInfo;receiverOpt=receiverOpt;prop=prop) ->
        match prop.Getter with
        | Some(getter) ->
            checkCall syntaxInfo.SyntaxNameOrDefault receiverOpt getter
        | _ ->
            ()

    | BoundExpression.Call(syntaxInfo=syntaxInfo;receiverOpt=receiverOpt;value=value) ->
        checkCall syntaxInfo.SyntaxNameOrDefault receiverOpt value

    | _ ->
        ()

and checkFunctionConstraints
        (env: SolverEnvironment) 
        skipUnsolved
        syntaxNode 
        (syntaxEnclosingTyArgsOpt: OlySyntaxType imarray option) 
        enclosingTyArgs
        syntaxFuncTyArgsOpt
        funcTyArgs
        (witnessArgs: WitnessSolution imarray) =
    solveFunctionConstraints env skipUnsolved syntaxNode syntaxEnclosingTyArgsOpt enclosingTyArgs syntaxFuncTyArgsOpt funcTyArgs witnessArgs

and checkConstraintsFromCallExpression diagnostics skipUnsolved (expr: BoundExpression) =
    match expr with
    | BoundExpression.Call(syntaxInfo, _, witnessArgs, _, value, _) ->
        // We cannot check constraints and witness for function groups, so skip it.
        if value.IsFunctionGroup then ()
        else

        match syntaxInfo.TryEnvironment with
        | Some benv ->

            checkStructTypeCycle 
                (SolverEnvironment.Create(diagnostics, benv))
                syntaxInfo.SyntaxNameOrDefault
                value.Type
            |> ignore

            let syntaxTyArgsOpt =
                let syntaxTyArgs =
                    match syntaxInfo.Syntax with
                    | :? OlySyntaxExpression as syntax ->
                        syntax.GetAllTypeArguments()
                    | _ ->
                        ImArray.empty
                if syntaxTyArgs.IsEmpty then
                    None
                else
                    if syntaxTyArgs.Length = value.AllTypeParameterCount then
                        Some syntaxTyArgs
                    else
                        None

            let syntaxNode: OlySyntaxNode =
                match syntaxInfo.TrySyntaxName with
                | Some(syntaxName) ->
                    match syntaxName.Parent with
                    | null -> syntaxName
                    | syntaxParent -> 
                        match syntaxParent with
                        | :? OlySyntaxName as syntaxParentName ->
                            syntaxParentName
                        | _ ->
                            syntaxName
                | _ ->
                    syntaxInfo.Syntax

            let enclosingTyArgs = value.Enclosing.TypeArguments
            let funcTyArgs = value.TypeArguments

            let syntaxEnclosingTyArgsOpt =
                syntaxTyArgsOpt 
                |> Option.bind (fun xs -> 
                    if xs.Length > enclosingTyArgs.Length then
                        None
                    else
                        Some(xs |> Seq.take enclosingTyArgs.Length |> ImArray.ofSeq)
                )

            let syntaxFuncTyArgsOpt =
                syntaxTyArgsOpt
                |> Option.bind (fun xs ->
                    if xs.Length > (enclosingTyArgs.Length + funcTyArgs.Length) then
                        None
                    else
                        Some(xs |> Seq.skip enclosingTyArgs.Length |> ImArray.ofSeq)
                )

            checkFunctionConstraints 
                (SolverEnvironment.Create(diagnostics, benv)) 
                skipUnsolved
                syntaxNode 
                syntaxEnclosingTyArgsOpt
                enclosingTyArgs
                syntaxFuncTyArgsOpt
                funcTyArgs
                witnessArgs
        | _ ->
            ()
    | _ ->
        OlyAssert.Fail("Expected 'Call' expression.")

and checkArgumentsFromCallExpression (env: SolverEnvironment) isReturnable (expr: BoundExpression) =
    match expr with
    | BoundExpression.Call(syntaxInfo, _, _, argExprs, value, _) ->
        OlyAssert.False(value.IsFunctionGroup)

        let syntaxNode =
            match syntaxInfo.Syntax with
            | :? OlySyntaxExpression as syntax ->
                match syntax with
                | OlySyntaxExpression.Call(syntax, _) -> syntax :> OlySyntaxNode
                | OlySyntaxExpression.InfixCall(_, syntax, _) -> syntax :> OlySyntaxNode
                | OlySyntaxExpression.PrefixCall(syntax, _) -> syntax :> OlySyntaxNode
                | _ -> syntax :> OlySyntaxNode
            | syntax ->
                syntax

        let valueTy = value.LogicalType

        (argExprs, valueTy.FunctionArgumentTypes)
        ||> ImArray.tryIter2 (fun argExpr expectedTy ->
            match argExpr with
            | BoundExpression.Lambda(body=body) ->
                OlyAssert.True(body.HasExpression)
                match expectedTy.TryFunction with
                | ValueSome(_, expectedTy) ->
                    body.Expression.ForEachReturningTargetExpression(fun expr ->
                        checkExpressionType env expectedTy expr
                    )
                | _ ->
                    ()
            | _ ->
                ()
        )

        if value.Enclosing.IsAbstract && value.IsConstructor && not value.IsBase then
            env.diagnostics.Error(sprintf "The constructor call is not allowed as the enclosing type '%s' is abstract." (printEnclosing env.benv value.Enclosing), 10, syntaxNode)

        if not isReturnable && value.IsInstanceConstructor && value.IsBase then
            env.diagnostics.Error("The base constructor call is only allowed as the last expression of a branch.", 10, syntaxNode)
    | _ ->
        OlyAssert.Fail("Expected 'Call' expression.")

and checkImmediateLambdaExpression env (expr: BoundExpression) =
    match expr with
    | BoundExpression.Lambda(_, _, _, parValues, lazyBodyExpr, lazyTy, _, _) ->
        if not lazyBodyExpr.HasExpression then
            lazyBodyExpr.Run()
            checkLambdaExpression env parValues lazyBodyExpr.Expression lazyTy.Type
    | _ ->
        OlyAssert.Fail("Expected 'Lambda' expression.")

/// This checks the expression to verify its correctness.
/// It does not check all expressions under the expression.
/// TODO: Remove this, we should do the specific checks in the binding functions as part of the binder...
and checkImmediateExpression (env: SolverEnvironment) isReturnable (expr: BoundExpression) =
    match expr with
    | BoundExpression.Call(value=value) when not value.IsFunctionGroup ->
        checkArgumentsFromCallExpression env isReturnable expr

    | BoundExpression.Sequential(_, expr1, _, _) ->
        match expr1 with
        | BoundExpression.Lambda _ ->
            checkImmediateLambdaExpression env expr1
        | _ ->
            ()
        solveTypes env (expr1.GetValidUserSyntax()) TypeSymbol.Unit expr1.Type

    | BoundExpression.GetProperty(syntaxInfo=syntaxInfo;prop=prop) ->
        // We can have a GetProperty expression even if the property does not have a getter.
        // The reason is because we initially bind to a GetProperty before potentially turning it into a SetProperty.
        if prop.Getter.IsSome then
            checkReceiverOfExpression env expr
        else
            env.diagnostics.Error($"Unable to get property value as '{prop.Name}' does not have a getter.", 10, syntaxInfo.Syntax)

    | BoundExpression.Lambda _ ->
        checkImmediateLambdaExpression env expr

    | _ ->
        ()

let checkStaticContextForFreeLocals env (expr: BoundExpression) (pars: ILocalParameterSymbol imarray) =
    let freeLocals = 
        match expr with
        | BoundExpression.EntityDefinition _ ->
            // This could be expensive when checking locally defined types.
            expr.GetFreeLocals()
        | _ ->
            expr.GetImmediateFreeLocals()
    freeLocals
    |> Seq.iter (fun pair ->
        let syntaxOpt, value = pair.Value
        if pars |> ImArray.exists (fun x -> value.Id = x.Id) |> not then
            match syntaxOpt with
            | Some syntax ->
                env.diagnostics.Error(sprintf "The free local value '%s' cannot be used in a static context." value.Name, 10, syntax)
            | _ ->
                env.diagnostics.Error(sprintf "A free local value cannot be used in a static context.", 10, expr.Syntax)
    )

let checkLocalLambdaKind env (bodyExpr: BoundExpression) (pars: ILocalParameterSymbol imarray) isStatic =
    if isStatic then
        checkStaticContextForFreeLocals env bodyExpr pars

let freshenAndCheckValue env (argExprs: BoundExpression imarray) (syntaxNode: OlySyntaxNode) (value: IValueSymbol) : IValueSymbol =
    let valueTy = value.LogicalType

    if not value.IsFunction && valueTy.IsQuantifiedFunction then 
        let tyPars = valueTy.TypeParameters
        if tyPars.IsEmpty then
            failwith "Expected type parameters for a quantified function type."

        let freshTy = freshenType env.benv tyPars ImmutableArray.Empty valueTy

        let value2 = 
            if value.IsMutable then
                createMutableLocalValue value.Name freshTy
            else
                createLocalValue value.Name freshTy
        checkFunctionType env syntaxNode argExprs value2.LogicalType
        value2 :> IValueSymbol
    else
        if value.Enclosing.TypeParameters.IsEmpty && value.TypeParameters.IsEmpty then
            checkFunctionType env syntaxNode argExprs valueTy
            value
        else
            let value2 = freshenValue env.benv value
            checkFunctionType env syntaxNode argExprs value2.LogicalType
            value2

let checkTypes (env: SolverEnvironment) syntaxNode (expectedTy: TypeSymbol) (ty: TypeSymbol) =
    solveTypes env syntaxNode expectedTy ty

let checkSubsumesType (env: SolverEnvironment) (syntaxNode: OlySyntaxNode) superTy ty =
    let res = subsumesTypeWith TypeVariableRigidity.Flexible superTy ty
    if not res then
        env.diagnostics.Error(sprintf "Expected type '%s' but is '%s'." (printType env.benv superTy) (printType env.benv ty), 0, syntaxNode.GetSuitableSyntaxForTypeError())

let checkParameter (env: SolverEnvironment) (syntaxNode: OlySyntaxNode) (func: IFunctionSymbol) (par: ILocalParameterSymbol) =
    par.Attributes
    |> ImArray.iter (fun attr ->
        match attr with
        | AttributeSymbol.Inline(inlineArg) ->
            if par.Type.IsFunction_t && not par.Type.IsNativeFunctionPtr_t then
                match inlineArg with
                | InlineArgumentSymbol.None ->
                    if not func.IsInline then
                        env.diagnostics.Error($"Parameter '{par.Name}' cannot be marked as 'inline' because the function '{func.Name}' is not.", 10, syntaxNode.GetChildNameIfPossible())

                | InlineArgumentSymbol.Always ->
                    if not func.IsInlineAlways then
                        env.diagnostics.Error($"Parameter '{par.Name}' cannot be marked as 'inline(always)' because the function '{func.Name}' is not.", 10, syntaxNode.GetChildNameIfPossible())

                | InlineArgumentSymbol.Never ->
                    if func.IsInlineNever then
                        env.diagnostics.Error($"Parameter '{par.Name}' cannot be marked as 'inline(never)' because the function '{func.Name}' already is.", 10, syntaxNode.GetChildNameIfPossible())

            else
                env.diagnostics.Error($"Parameter '{par.Name}' cannot be marked as 'inline' because the parameter's type is not a non-static function.", 10, syntaxNode.GetChildNameIfPossible())

        | _ ->
            ()

    )
