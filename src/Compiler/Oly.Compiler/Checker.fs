module internal Oly.Compiler.Internal.Checker

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open System.Collections.Generic
open System.Collections.Immutable
open Oly.Compiler.Syntax.Internal
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.SemanticErrors
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
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

                oldConstrs
                |> ImArray.iter (fun constr ->
                    let newConstr =
                        match constr with
                        | ConstraintSymbol.Null
                        | ConstraintSymbol.Struct
                        | ConstraintSymbol.NotStruct
                        | ConstraintSymbol.Unmanaged
                        | ConstraintSymbol.ConstantType _ ->
                            constr
                        | ConstraintSymbol.SubtypeOf(oldConstrTy) ->
                            let tyArgs = ImArray.createOne (mkSolvedInferenceVariableType newTyPar newTyPar.AsType)
                            ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(oldConstrTy.Value.Substitute(tyArgs)))
                    newTyPar.AddConstraint(newConstr)
                )

                solution.Solution <- Some(TypeSymbol.Variable(newTyPar))
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

let checkAmbiguousFunctionsFromEntity env (syntaxNode: OlySyntaxNode) (ent: IEntitySymbol) =
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
    match valueTy.TryFunction with
    | ValueSome(expectedArgTys, _) ->        
        let argTysWithSyntax =
            argExprs
            |> ImArray.map (fun x -> (x.Type, x.FirstReturnExpression.Syntax))
        solveFunctionInput env syntaxNode expectedArgTys argTysWithSyntax
    | _ ->
        if not valueTy.IsError_t then
            env.diagnostics.Error(sprintf "Not a function.", 3, syntaxNode)

let emptyStructCycleHash = System.Collections.ObjectModel.ReadOnlyDictionary(Dictionary()) 

let checkStructCycle (ent: IEntitySymbol) =
    if not ent.IsAnyStruct then
        true, emptyStructCycleHash
    else

    let hash = Dictionary()

    let rec check (ent: IEntitySymbol) =
        let formalId = ent.Formal.Id
        match hash.TryGetValue formalId with
        | true, result -> result
        | _ ->
            hash.[formalId] <- false
            let mutable result =
                ent.Fields
                |> Seq.forall (fun field ->
                    if field.IsInstance && field.Type.IsAnyStruct then
                        match field.Type.TryEntity with
                        | ValueSome(ent) ->
                            check ent
                        | _ ->
                            true
                    else
                        true
                )
            hash.[formalId] <- result
            result

    check ent, System.Collections.ObjectModel.ReadOnlyDictionary(hash)

let rec checkStructTypeCycle (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) -> checkStructCycle ent |> fst
    | TypeSymbol.HigherVariable(_, tyArgs) ->
        tyArgs
        |> Seq.forall (fun ty -> checkStructTypeCycle ty)
    | TypeSymbol.HigherInferenceVariable(_, tyArgs, _, _) ->
        tyArgs
        |> Seq.forall (fun ty -> checkStructTypeCycle ty)
    | TypeSymbol.Tuple(tyArgs, _) ->
        tyArgs
        |> Seq.forall (fun ty -> checkStructTypeCycle ty)
    | TypeSymbol.ForAll(_, innerTy) ->
        checkStructTypeCycle innerTy
    | _ ->
        match ty.TryFunction with
        | ValueSome(argTys, returnTy) ->
            seq { yield! argTys; yield returnTy }
            |> Seq.forall (fun ty -> checkStructTypeCycle ty)
        | _ ->
            true

let checkEntityConstructor env syntaxNode (syntaxTys: OlySyntaxType imarray) (ent: IEntitySymbol) =
    let result, _ = checkStructCycle ent

    if not result then
        if ent.IsFormal || ent.TypeArguments.IsEmpty then
            env.diagnostics.Error(sprintf "'%s' has fields that cause a cycle." (printEntity env.benv ent), 10, syntaxNode)
        else
            env.diagnostics.Error(sprintf "'%s' is causing a cycle on a struct." (printEntity env.benv ent), 10, syntaxNode)

    let tyArgs = ent.TypeArguments

    (ent.TypeParameters, tyArgs)
    ||> Seq.iteri2 (fun i tyPar tyArg ->
        if tyPar.Arity > 0 then
            match stripTypeEquations tyArg with
            | TypeSymbol.Error _ -> ()
            | _ ->
                if tyPar.Arity <> tyArg.Arity then
                    let syntaxTy = syntaxTys.[i]
                    env.diagnostics.Error(sprintf "Type instantiation '%s' has a type arity of %i, but expected %i." (printType env.benv tyArg) tyArg.Arity tyPar.Arity, 10, syntaxTy)

        // TODO: Should we do this here or somewhere else? Inside type parameter? We do something similar like this iin the solver in solveWitnesses.
        tyPar.Constraints
        |> Seq.iter (fun constr ->
            match constr with
            | ConstraintSymbol.SubtypeOf(constrTy) ->
                let constrTy = substituteType tyArgs constrTy.Value
                let exists =
                    if constrTy.IsShape then
                        subsumesShape env.benv constrTy tyArg
                    elif constrTy.IsTypeConstructor then
                        subsumesTypeConstructor constrTy tyArg
                    else
                        subsumesTypeWith Indexable constrTy tyArg
                if not exists && not tyArg.IsError_t then
                    let syntaxTy = syntaxTys.[i]
                    env.diagnostics.Error(sprintf "Type instantiation '%s' is missing the constraint '%s'." (printType env.benv tyArg) (printType env.benv constrTy), 10, syntaxTy)
            | ConstraintSymbol.ConstantType(constTy) ->
                let exists =
                    match stripTypeEquations constTy.Value, stripTypeEquations tyArg with
                    | TypeSymbol.Int32, TypeSymbol.ConstantInt32 _ -> true
                    | _ -> false
                if not exists && not tyArg.IsError_t then
                    let syntaxTy = syntaxTys.[i]
                    env.diagnostics.Error(sprintf "Type instantiation '%s' is missing the constraint '%s'." (printType env.benv tyArg) (printConstraint env.benv constr), 10, syntaxTy)
            | ConstraintSymbol.Unmanaged ->
                if not tyArg.IsUnmanaged && not tyArg.IsError_t then
                    let syntaxTy = syntaxTys.[i]
                    env.diagnostics.Error(sprintf "Type instantiation '%s' is missing the constraint '%s'." (printType env.benv tyArg) (printConstraint env.benv constr), 10, syntaxTy)
            | ConstraintSymbol.NotStruct ->
                if tyArg.IsAnyStruct && not tyArg.IsError_t then
                    let syntaxTy = syntaxTys.[i]
                    env.diagnostics.Error(sprintf "Type instantiation '%s' is missing the constraint '%s'." (printType env.benv tyArg) (printConstraint env.benv constr), 10, syntaxTy)
            | ConstraintSymbol.Struct ->
                if not tyArg.IsAnyStruct && not tyArg.IsError_t then
                    match tyArg.TryTypeParameter with
                    | ValueSome(tyPar) when tyPar.Constraints |> ImArray.exists (function ConstraintSymbol.Unmanaged -> true | _ -> false) ->
                        ()
                    | _ ->
                        let syntaxTy = syntaxTys.[i]
                        env.diagnostics.Error(sprintf "Type instantiation '%s' is missing the constraint '%s'." (printType env.benv tyArg) (printConstraint env.benv constr), 10, syntaxTy)
            | ConstraintSymbol.Null ->
                if not tyArg.IsNullable && not tyArg.IsError_t then
                    let syntaxTy = syntaxTys.[i]
                    env.diagnostics.Error(sprintf "Type instantiation '%s' is missing the constraint '%s'." (printType env.benv tyArg) (printConstraint env.benv constr), 10, syntaxTy)
        )
    )

let checkTypeConstructor env syntaxNode (syntaxTys: OlySyntaxType imarray) ty =
    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) ->
        checkEntityConstructor env syntaxNode syntaxTys ent
    | _ ->
        ()

/// Only used in pass3 to check types of value declarations
let rec checkTypeConstructorDepth env (syntaxNode: OlySyntaxNode) (syntaxTys: OlySyntaxType imarray) (ty: TypeSymbol) =
    (syntaxTys, ty.TypeArguments)
    ||> ImArray.tryIter2 (fun syntaxTy tyArg ->
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
        checkEntityConstructor env syntaxNode syntaxTys ent
    | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
        tyPar.Constraints
        |> ImArray.iter (fun constr ->
            match constr.TryGetSubtypeOf() with
            // If the constraint type has any type parameter constructors, then we skip this check
            // as it has already failed elsewhere. We do not support further "higher-rank" types.
            | ValueSome(constrTy) when constrTy.TypeParameters |> ImArray.forall (fun x -> x.HasArity |> not) ->
                let constrTy = actualType tyArgs constrTy
                checkTypeConstructorDepth env syntaxNode syntaxTys constrTy
            | _ ->
                ()
        )
    | _ ->
        ()

let checkTypeConstructorDepthWithType env (syntaxTy: OlySyntaxType) ty =
    match syntaxTy with
    | OlySyntaxType.Name(syntaxName) ->
        match syntaxName with
        | OlySyntaxName.Generic(_, syntaxTyArgsRoot) ->
            checkTypeConstructorDepth env syntaxTyArgsRoot syntaxTyArgsRoot.Values ty
        | _ ->
            checkTypeConstructorDepth env syntaxName ImArray.empty ty
    | _ ->
        ()

let checkConstraint env syntaxConstr (constr: ConstraintSymbol) =
    match constr.TryGetSubtypeOf() with
    | ValueSome constrTy ->
        match syntaxConstr with
        | OlySyntaxConstraint.Type(syntaxTy) ->       
            match syntaxTy with
            | OlySyntaxType.Name(syntaxName) ->
                match syntaxName with
                | OlySyntaxName.Generic(_, syntaxTyArgsRoot) ->
                    checkTypeConstructor env syntaxTyArgsRoot syntaxTyArgsRoot.Values constrTy
                | _ ->
                    ()
            | _ ->
                ()
        | _ ->
            ()
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
                checkConstraint env syntaxConstr constr
            )
        | _ ->
            ()
    )

and checkImplementation env (syntaxNode: OlySyntaxNode) (ty: TypeSymbol) (super: TypeSymbol) expr =
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
                if x.IsVirtual then
                    match x.FunctionOverrides with
                    | Some overridenFunc -> areLogicalFunctionSignaturesEqual overridenFunc func
                    | _ -> areLogicalFunctionSignaturesEqual x func
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

and checkInterfaceDefinition (env: SolverEnvironment) (syntaxNode: OlySyntaxNode) (ent: IEntitySymbol) =

    ent.Extends
    |> ImArray.iter (fun ty ->
        if not ty.IsInterface then
            env.diagnostics.Error(sprintf "Cannot inherit the construct '%s'." (printType env.benv ty), 10, syntaxNode)
    )

    checkAmbiguousFunctionsFromEntity env syntaxNode ent

and checkLambdaExpression (env: SolverEnvironment) (pars: ImmutableArray<ILocalParameterSymbol>) (body: BoundExpression) (ty: TypeSymbol) =
    if ty.IsError_t then ()
    else
        match ty.TryFunction with
        | ValueSome(argTys, returnTy) ->
            let syntaxBody = body.Syntax
            let argTysWithSyntax = pars |> ImArray.map (fun x -> (x.Type, syntaxBody))

            solveFunctionInput env syntaxBody argTys argTysWithSyntax
            solveTypes env body.FirstReturnExpression.Syntax returnTy body.Type

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

and checkConstructorFieldAssignments (env: SolverEnvironment) (enclosingTy: TypeSymbol) (setFieldsExpr: BoundExpression) =
    let canCheck =
        match setFieldsExpr with
        | BoundExpression.Call(value=value) when value.IsBase && value.IsFunction -> true
        | BoundExpression.SetField _
        | BoundExpression.Sequential _ -> true
        | _ -> false

    if canCheck then
        let expectedFields = enclosingTy.GetInstanceFields()
        let fields = setFieldsExpr.GetThisSetInstanceFields()
        let fieldNames = fields |> ImArray.map (fun x -> x.Name)
    
        let expectedFieldSet = HashSet(expectedFields |> Seq.filter (fun x -> not x.Type.IsError_t) |> Seq.map (fun x -> x.Name))
        expectedFieldSet.ExceptWith(fieldNames)
        expectedFieldSet
        |> Seq.sort
        |> Seq.iter (fun fieldName ->
            env.diagnostics.Error($"Field '{fieldName}' not assigned.", 10, setFieldsExpr.Syntax)
        )

and private checkValueBinding (env: SolverEnvironment) (rhsExpr: BoundExpression) (value: IValueSymbol) =

    let firstReturnExpression = rhsExpr.FirstReturnExpression

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
            match value.Type.TryFunction, firstReturnExpression.Type.TryFunction with
            | ValueSome(argTys1, _), ValueSome(argTys2, _) ->
                let argTys2WithSyntax = argTys2 |> ImArray.map (fun x -> (x, syntax))
                solveFunctionInput env syntax argTys1 argTys2WithSyntax
            | _, _ ->
                solveTypes env syntax value.Type firstReturnExpression.Type
        else
            match firstReturnExpression with
            | BoundExpression.Lambda(body=body) ->
                if not body.HasExpression then
                    body.Run()            
            | _ ->
                ()

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
        match bindingInfo2.Type.TryFunction, rhsExpr2.Type.TryFunction with
        | ValueSome(argTys1, _), ValueSome(argTys2, _) ->
            let argTys2WithSyntax = argTys2 |> ImArray.map (fun x -> (x, syntax))
            solveFunctionInput env syntax argTys1 argTys2WithSyntax
        | _ ->
            solveTypes env syntax bindingInfo2.Type rhsExpr2.Type

    bindingInfo2, rhsExpr2

and checkExpressionType (env: SolverEnvironment) (expectedTy: TypeSymbol) (boundExpr: BoundExpression) =
    let ty = boundExpr.Type
    if expectedTy.IsSolved && ty.IsSolved && not expectedTy.IsError_t && not ty.IsError_t && subsumesType expectedTy ty then
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
        | Some receiver when value.Enclosing.IsAnyStruct ->
            if not value.IsReadOnly then
                if check receiver |> not then
                    env.diagnostics.Error(sprintf "Function call '%s' is not read-only and cannot be called on an immutable struct instance." value.Name, 10, syntax)
        | _ ->
            ()

    and checkAddressOf (receiver: BoundExpression) =
        match receiver with
        | BoundExpression.Call(value=value;args=args) 
                when value.IsAddressOf ->
            check args.[0]
        | _ ->
            true

    and check (receiver: BoundExpression) =
        match receiver with
        | BoundExpression.Value(value=value) ->
            if (not value.IsMutable && value.Type.IsAnyStruct) || value.Type.IsReadOnlyByRef then
                reportError value.Name receiver.SyntaxNameOrDefault
                false
            else
                true
        | BoundExpression.GetField(receiver=receiver;field=field) ->
            if check receiver then
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
            check expr2
        | _ ->
            checkAddressOf receiver

    match expr with
    | BoundExpression.SetValue(value=value;rhs=rhs) ->
        checkExpressionType env value.Type rhs
        if not value.IsMutable then
            reportError value.Name expr.SyntaxNameOrDefault

    | BoundExpression.SetField(receiver=receiver;field=field;rhs=rhs) ->
        checkExpressionType env field.Type rhs
        if check receiver then
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

and checkConstraintsForSolving 
        (env: SolverEnvironment) 
        syntaxNode 
        (syntaxEnclosingTyArgsOpt: OlySyntaxType imarray option) 
        enclosingTyArgs
        syntaxFuncTyArgsOpt
        funcTyArgs
        (witnessArgs: WitnessSolution imarray) =
    solveConstraints env syntaxNode syntaxEnclosingTyArgsOpt enclosingTyArgs syntaxFuncTyArgsOpt funcTyArgs witnessArgs

and checkWitnessesFromCallExpression (expr: BoundExpression) =
    match expr with
    | BoundExpression.Call(syntaxInfo, _, witnessArgs, _, value, _) when not value.IsFunctionGroup ->
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

        witnessArgs.GetValue(syntaxTyArgsOpt, System.Threading.CancellationToken.None) |> ignore
    | _ ->
        OlyAssert.Fail("Expected 'Call' expression.")

and checkArgumentsFromCallExpression (env: SolverEnvironment) isReturnable (expr: BoundExpression) =
    match expr with
    | BoundExpression.Call(syntaxInfo, receiverOpt, _, argExprs, value, _) ->
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
                if not body.HasExpression then
                    body.Run()
                match expectedTy.TryFunction with
                | ValueSome(_, expectedTy) ->
                    body.Expression.GetReturningTargetExpressions()
                    |> ImArray.iter (fun expr ->
                        checkExpressionType env expectedTy expr
                    )
                | _ ->
                    ()
            | _ ->
                ()
        )

        if not (checkStructTypeCycle valueTy) then
            env.diagnostics.Error(sprintf "The call to '%s' will result in the type '%s' that causes a cycle on a struct." (printValueName value) (printType env.benv valueTy), 10, syntaxNode)

        value.AllTypeParameters
        |> Seq.iter (fun tyPar ->
            tyPar.Constraints
            |> Seq.iter (fun constr ->
                match constr.TryGetSubtypeOf() with
                | ValueSome constrTy ->
                    match constrTy.TryEntity with
                    | ValueSome ent ->
                        let result, _ = checkStructCycle ent
                        if not result then
                            env.diagnostics.Error(sprintf "The call to '%s' will result in a type parameter with the constraint '%s' that causes a cycle on a struct." value.Name (printType env.benv valueTy), 10, syntaxNode)
                    | _ ->
                        ()
                | _ ->
                    ()
            )
        )

        if value.Enclosing.IsAbstract && value.IsConstructor then
            env.diagnostics.Error(sprintf "The constructor call is not allowed as the enclosing type '%s' is abstract." (printEnclosing env.benv value.Enclosing), 10, syntaxNode)

        if not isReturnable && value.IsInstanceConstructor && value.IsBase then
            env.diagnostics.Error("The base constructor call is only allowed as the last expression of a branch.", 10, syntaxNode)

        if receiverOpt.IsSome then
            checkReceiverOfExpression env expr
    | _ ->
        OlyAssert.Fail("Expected 'Call' expression.")

/// This checks the expression to verify its correctness.
/// It does not check all expressions under the expression.
/// TODO: Remove this, we should do the specific checks in the binding functions as part of the binder...
and checkImmediateExpression (env: SolverEnvironment) isReturnable (expr: BoundExpression) =
    match expr with
    | BoundExpression.Call(value=value) when not value.IsFunctionGroup ->
        checkArgumentsFromCallExpression env isReturnable expr 
        checkWitnessesFromCallExpression expr  

    | BoundExpression.Sequential(_, expr1, _) ->
        solveTypes env (expr1.GetValidUserSyntax()) TypeSymbol.Unit expr1.Type

    | BoundExpression.GetField(receiver=receiver) ->
        checkImmediateExpression env false receiver

    | BoundExpression.GetProperty(syntaxInfo=syntaxInfo;receiverOpt=receiverOpt;prop=prop) ->
        receiverOpt
        |> Option.iter (checkImmediateExpression env false)
        // We can have a GetProperty expression even if the property does not have a getter.
        // The reason is because we initially bind to a GetProperty before potentially turning it into a SetProperty.
        if prop.Getter.IsSome then
            checkReceiverOfExpression env expr
        else
            env.diagnostics.Error($"Unable to get property value as '{prop.Name}' does not have a getter.", 10, syntaxInfo.Syntax)

    | BoundExpression.SetValue _
    | BoundExpression.SetContentsOfAddress _
    | BoundExpression.SetField _
    | BoundExpression.SetProperty _ ->
        checkReceiverOfExpression env expr

    | BoundExpression.Lambda(_, _, _, parValues, lazyBodyExpr, lazyTy, _, _) ->
        if not lazyBodyExpr.HasExpression then
            lazyBodyExpr.Run()
            checkLambdaExpression env parValues lazyBodyExpr.Expression lazyTy.Type

    | _ ->
        ()

let checkLocalLambdaKind env (bodyExpr: BoundExpression) (pars: ILocalParameterSymbol imarray) isStatic =
    if isStatic then
        let freeLocals = bodyExpr.GetImmediateFreeLocals()
        freeLocals
        |> Seq.iter (fun pair ->
            let syntaxOpt, value = pair.Value
            if pars |> ImArray.exists (fun x -> value.Id = x.Id) |> not then
                match syntaxOpt with
                | Some syntax ->
                    env.diagnostics.Error(sprintf "The free local value '%s' cannot be used in the context of a static local function." value.Name, 10, syntax)
                | _ ->
                    env.diagnostics.Error(sprintf "A free local value has been used in the context of a static local function.", 10, bodyExpr.Syntax)
        )

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
