module internal rec Oly.Compiler.Internal.FunctionOverloading

open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.ImplicitRules

let private filterFunctionsForOverloadingLeastGeneric (funcs: IFunctionSymbol imarray) =
    if funcs.Length <= 1 then funcs
    else

    let minTyParCount =
        funcs
        |> ImArray.map (fun x -> x.LogicalTypeParameterCount)
        |> Seq.min

    funcs
    |> ImArray.filter (fun x -> x.LogicalTypeParameterCount = minTyParCount)

let private filterFunctionsForOverloadingByLeastGenericReturnType (funcs: IFunctionSymbol imarray) =
    if funcs.Length <= 1 then funcs
    else

    funcs
    |> Seq.groupBy (fun func ->
        let funcTy = func.ReturnType
        let specificScore =
            let rec score currentScore (ty: TypeSymbol) =
                let ty = stripTypeEquations ty
                let tyArgs = ty.TypeArguments
                if not tyArgs.IsEmpty then
                    let currentScore = currentScore + 1
                    tyArgs
                    |> ImArray.map (fun x -> score currentScore x)
                    |> Seq.max
                else
                    currentScore
            score 0 funcTy
        specificScore
    )
    |> Seq.sortBy (fun x -> fst x)
    |> Seq.tryHead
    |> Option.map (fun x -> snd x |> ImArray.ofSeq)
    |> Option.defaultValue ImArray.empty

let private canScore (func: IFunctionSymbol) =
    let argTys = 
        match func.Type.TryGetFunctionWithParameters() with
        | ValueSome(argTys, _)-> argTys
        | _ -> OlyAssert.Fail("Expected function type") 

    if argTys.IsEmpty then
        None
    elif argTys.Length = 1 && argTys[0].IsFunction_t then
        Some(argTys)
    else
        let rec exists (tys: TypeSymbol imarray) =
            tys
            |> ImArray.exists (fun x ->
                match stripTypeEquations x with
                | TypeSymbol.Function _ -> true
                | _ ->
                    exists x.TypeArguments
            )
        if exists argTys then
            Some(argTys)
        else
            None

let private filterFunctionsForOverloadingByMostGenericArgumentTypes (funcs: IFunctionSymbol imarray) =
    if funcs.Length <= 1 then funcs
    else

    // TODO: This is probably wrong and it should be by least generic, but need to take into account the passing
    //       arguments.
    funcs
    |> Seq.groupBy (fun func ->
        match canScore func with
        | None -> System.Int32.MinValue
        | Some argTys ->
            let specificScore =
                let rec score currentScore (ty: TypeSymbol) =
                    let ty = stripTypeEquations ty
                    let tyArgs = ty.TypeArguments
                    if not tyArgs.IsEmpty then
                        let currentScore = currentScore + 1
                        tyArgs
                        |> ImArray.map (fun x -> score currentScore x)
                        |> Seq.max
                    else
                        currentScore
                // TODO: Kind of a hack using TypeSymbol.Tuple.
                let inputTy = 
                    if argTys.IsEmpty then
                        TypeSymbol.Unit
                    elif argTys.Length = 1 then
                        argTys[0]
                    else
                        TypeSymbol.Tuple(argTys, ImArray.empty)
                score 0 inputTy
            specificScore
    )
    |> Seq.sortByDescending (fun x -> fst x)
    |> Seq.tryHead
    |> Option.map (fun x -> snd x |> ImArray.ofSeq)
    |> Option.defaultValue ImArray.empty

let private filterFunctionsForOverloadingByWeight skipEager resArgs (returnTyOpt: TypeSymbol option) (funcs: IFunctionSymbol imarray) =
    if funcs.Length <= 1 then funcs
    else

    let rec computeWeight (currentRigidWeight, currentWeight) (expectedTy: TypeSymbol) (ty: TypeSymbol) : struct(int * int) =
        let expectedTy = stripTypeEquationsAndBuiltIn expectedTy
        let ty = stripTypeEquationsAndBuiltIn ty
        if ((ty.IsSolved || (ty.IsEagerInferenceVariable_t && not skipEager)) && UnifyTypes Generalizable expectedTy ty) then
            let currentWeight = currentWeight + 1
            let currentRigidWeight =
                if UnifyTypes Rigid expectedTy.Formal ty.Formal then
                    currentRigidWeight + 1
                else
                    currentRigidWeight
            if expectedTy.LogicalTypeParameterCount = ty.LogicalTypeParameterCount then
                let expectedTyArgs = expectedTy.TypeArguments |> Seq.skip (expectedTy.TypeParameters.Length - expectedTy.LogicalTypeParameterCount) |> ImArray.ofSeq
                let tyArgs = ty.TypeArguments |> Seq.skip (ty.TypeParameters.Length - ty.LogicalTypeParameterCount) |> ImArray.ofSeq

                OlyAssert.Equal(expectedTyArgs.Length, tyArgs.Length)

                (struct(currentRigidWeight, currentWeight), expectedTyArgs, tyArgs)
                |||> ImArray.fold2 (fun (struct(currentRigidWeight, currentWeight)) expectedTy ty ->
                    computeWeight (currentRigidWeight, currentWeight) expectedTy ty
                )
            else
                (currentRigidWeight, currentWeight + expectedTy.LogicalTypeParameterCount)
        else
            (0, 0)
    
    let mapArgTys (func: IFunctionSymbol) (argTys: TypeSymbol imarray) =
        let mutable currentRigidWeight = 0
        let mutable currentWeight = 0

        if argTys.IsEmpty then ()
        else
            (func.LogicalParameters, argTys.AsMemory())
            ||> ROMem.iter2 (fun par argTy ->
                let struct(rigidWeight, weight) = computeWeight (0, 0) par.Type argTy
                currentRigidWeight <- currentRigidWeight + rigidWeight
                currentWeight <- currentWeight + weight
            )
            
        match returnTyOpt with
        | Some returnTy when returnTy.IsSolved ->
            let struct(rigidWeight, weight) = computeWeight (0, 0) func.ReturnType returnTy
            currentRigidWeight <- currentRigidWeight + rigidWeight
            currentWeight <- currentWeight + weight
        | _ ->
            ()
    
        func, struct(currentRigidWeight, currentWeight)
    
    match resArgs with
    | ResolutionArguments.ByType(argTys) ->
        let result =
            funcs
            |> ImArray.map (fun func ->
                mapArgTys func argTys
            )
    
        let _, maxWeight =
            result
            |> Seq.maxBy (fun (_, x) -> x)
    
        result
        |> ImArray.choose (fun (func, weight) ->
            if weight = maxWeight then
                Some func
            else
                None  
        )
    | _ ->
        funcs

let private filterFunctionsForOverloadingFinalPhase (funcs: IFunctionSymbol imarray) =
    if funcs.Length <= 1 then funcs
    else

    let scoredResult =
        if funcs.IsEmpty || funcs.Length = 1 then funcs
        else
            filterFunctionsForOverloadingByMostGenericArgumentTypes funcs

    if scoredResult.Length > 1 then
        let filteredFuncs = 
            filterMostSpecificFunctionsByEnclosing scoredResult
            |> filterFunctionsForOverloadingByLeastGenericReturnType
            |> filterFunctionsForOverloadingLeastGeneric

        if filteredFuncs.IsEmpty || filteredFuncs.Length = 1 then filteredFuncs
        else filteredFuncs
    else
        scoredResult
    
let private filterFunctionsForOverloadingPhase4 resArgs (returnTyOpt: TypeSymbol option) (funcs: IFunctionSymbol imarray) =
    if funcs.Length <= 1 then funcs
    else

    let checkArgTys (func: IFunctionSymbol) (argTys: TypeSymbol imarray) =
        func.LogicalParameterCount = argTys.Length &&
        (
            if argTys.IsEmpty then true
            else
                (func.LogicalParameters, argTys.AsMemory())
                ||> ROMem.forall2 (fun par argTy ->
                    match argTy.TryGetFunctionWithParameters() with
                    | ValueSome(argTys, _) when par.Type.IsFunction_t -> 
                        match par.Type.TryGetFunctionWithParameters() with
                        | ValueSome(parTys, _) when parTys.Length = 1 && parTys[0].IsVariadicTypeVariable ->
                            // Variadic variables will always return true.
                            true
                        | _ ->
                            // This handles overloads that take a function type to only
                            // accept the same number of arguments for the function type.
                            let argCount1 = par.Type.FunctionParameterCount
                            let argCount2 = argTys.Length
                            argCount1 = argCount2 &&
                            subsumesTypeWith Generalizable par.Type argTy
                    | _ ->
                        subsumesTypeWith Generalizable par.Type argTy
                )
        )

    // We didn't find a specific function, let's find ones with sub types.
    let funcs2 =
        match resArgs with
        | ResolutionArguments.NotAFunctionCall -> ImArray.empty
        | ResolutionArguments.Any ->
            match returnTyOpt with
            | Some returnTy when returnTy.IsSolved ->
                funcs
                |> ImArray.filter (fun func ->
                    UnifyTypes Generalizable returnTy func.ReturnType
                )
            | _ ->
                funcs
        | ResolutionArguments.ByType(argTys) ->
            funcs
            |> ImArray.filter (fun func ->
                match returnTyOpt with
                | Some returnTy when returnTy.IsSolved ->
                    UnifyTypes Generalizable returnTy func.ReturnType &&
                    checkArgTys func argTys
                | _ ->
                    checkArgTys func argTys
            )
        | ResolutionArguments.ByFunctionType(funcTy) ->
            funcs
            |> ImArray.filter (fun func ->
                UnifyTypes Generalizable funcTy func.LogicalType
            )

    let funcs2 =
        if funcs2.IsEmpty then
            funcs
        else
            funcs2

    if funcs2.Length = 1 then funcs2
    else
        funcs2

let private filterFunctionsForOverloadingPhase3 (resArgs: ResolutionArguments) (returnTyOpt: TypeSymbol option) (funcs: IFunctionSymbol imarray): _ imarray =
    if funcs.Length <= 1 then funcs
    else

    let checkArgTys rigidity (func: IFunctionSymbol) (argTys: TypeSymbol imarray) =
        let parCount = func.LogicalParameterCount
        let argCount = argTys.Length
        parCount = argCount &&
        (
            if argTys.IsEmpty then true
            else
                (func.LogicalParameters, argTys.AsMemory())
                ||> ROMem.forall2 (fun par argTy -> 
                    UnifyTypes rigidity argTy par.Type
                )
        )

    let findSpecificFuncs rigidity =
        match resArgs with
        | ResolutionArguments.NotAFunctionCall -> ImArray.empty
        | ResolutionArguments.Any -> 
            match returnTyOpt with
            | Some returnTy when returnTy.IsSolved ->
                funcs
                |> ImArray.filter (fun func ->
                    UnifyTypes rigidity returnTy func.ReturnType
                )
            | _ ->
                ImArray.empty
        | ResolutionArguments.ByType argTys ->
            let funcs, argTys = ImplicitPassingArgumentsForOverloading funcs argTys
            funcs
            |> ImArray.filter (fun func ->
                checkArgTys rigidity func argTys
            )
        | ResolutionArguments.ByFunctionType(funcTy) ->
            funcs
            |> ImArray.filter (fun func ->
                UnifyTypes rigidity funcTy func.LogicalType
            )

    let specificFuncs =
        let specificFuncs2 = findSpecificFuncs IntegerGeneralizable
        if specificFuncs2.IsEmpty then       
            findSpecificFuncs NumberGeneralizable
        else
            specificFuncs2

    let funcs =
        if specificFuncs.IsEmpty then
            funcs
        else
            specificFuncs

    filterFunctionsForOverloadingPhase4 resArgs returnTyOpt funcs

/// Overloading Phase 2:
///     Optionally filters candidates by return type.
///     This is a fast operation.
let private filterFunctionsForOverloadingPhase2 (returnTyOpt: TypeSymbol option) (candidates: IFunctionSymbol imarray): _ imarray =
    if candidates.Length <= 1 then candidates
    else

    match returnTyOpt with
    | Some returnTy when returnTy.IsSolved ->
        let results =
            candidates
            |> ImArray.filter (fun func ->
                subsumesTypeWith Generalizable returnTy func.ReturnType ||
                UnifyTypes Generalizable returnTy func.ReturnType
            )
        if results.IsEmpty then
            candidates
        else
            results
    | _ ->
        candidates

/// Overloading Phase 1:
///     Filters candidates by resolution type arity, and argument count.
///     This is a fast operation and does not check types.
let private filterFunctionsForOverloadingPhase1 (benv: BoundEnvironment) resTyArity (argCountOpt: int voption) (candidates: IFunctionSymbol imarray): _ imarray =
    if candidates.IsEmpty then ImArray.empty
    else

    candidates
    |> ImArray.filter (fun func ->
        (
            let tyParCount = 
                if func.IsConstructor then
                    match func.Enclosing with
                    | EnclosingSymbol.Entity(ent) ->
                        func.Enclosing.TypeParameters.Length - benv.GetEnclosingTypeArguments(ent.Id).Length
                    | _ ->
                        func.Enclosing.TypeParameters.Length
                else
                    func.TypeParameters.Length
            match resTyArity with
            | ResolutionTypeArity.Any -> true
            | ResolutionTypeArity.FirstOrder n -> tyParCount = n
            | ResolutionTypeArity.SecondOrder n -> tyParCount = n // REVIEW: Is this right? We should find out and/or explain why....
        ) &&
        (
            match argCountOpt with
            | ValueSome resArgCount ->
                resArgCount = func.LogicalParameterCount
            | _ ->
                true
        )
    )

/// Overloading Part 1:
///     This is used in name resolution upon finding a list of function candidates.
///     Does not check against types.
let filterFunctionsForOverloadingPart1 (benv: BoundEnvironment) resTyArity (argCountOpt: int voption) (candidates: IFunctionSymbol imarray): _ imarray =
    filterFunctionsForOverloadingPhase1 benv resTyArity argCountOpt candidates

/// Overloading Part 2:
///     Proper function overloading that checks types.
///     This is used in call expression checking.
let filterFunctionsForOverloadingPart2 (resArgs: ResolutionArguments) (returnTyOpt: TypeSymbol option) (candidates: IFunctionSymbol imarray): _ imarray =
    let phase2Results = filterFunctionsForOverloadingPhase2 returnTyOpt candidates
    filterFunctionsForOverloadingPhase3 resArgs returnTyOpt phase2Results

/// Overloading Part 3:
///     Handles ambiguity for generics by using scores.
let filterFunctionsForOverloadingPart3 skipEager (resArgs: ResolutionArguments) (returnTyOpt: TypeSymbol option) (candidates: IFunctionSymbol imarray) =
    let funcs3 = filterFunctionsForOverloadingByWeight skipEager resArgs None candidates

    if returnTyOpt.IsSome then
        filterFunctionsForOverloadingByWeight skipEager resArgs returnTyOpt funcs3
        |> filterFunctionsForOverloadingFinalPhase
    else
        funcs3
