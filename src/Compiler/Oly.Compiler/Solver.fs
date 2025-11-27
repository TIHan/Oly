module internal rec Oly.Compiler.Internal.Solver

open Oly.Core
open System.Collections.Immutable
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.SemanticDiagnostics
open Oly.Compiler.Internal.SymbolQuery

[<NoEquality;NoComparison;Struct>]
type SolverEnvironment =
    {
        diagnostics: OlyDiagnosticLogger
        benv: BoundEnvironment
        pass: CompilerPass
        reportTypeErrors: bool
    }

    static member Create(diagnostics, benv, pass) =
        {
            diagnostics = diagnostics
            benv = benv
            pass = pass
            reportTypeErrors = true
        }

    static member CreateNoTypeErrors(diagnostics, benv, pass) =
        {
            diagnostics = diagnostics
            benv = benv
            pass = pass
            reportTypeErrors = false
        }

[<RequireQualifiedAccess>]
type WitnessSolver =
    | Subtype
    | Trait
    
type ConstraintSolverError =
    | GeneralFailure
    | ShapeMembers_AmbiguousFunctions of abstractFunc: IFunctionSymbol

let private solveTypesNoError expectedTy (ty: TypeSymbol) =
    UnifyTypes Flexible expectedTy ty

let private solveTypesWithSubsumptionNoErrorWith (env: SolverEnvironment) rigidity expectedTy (ty: TypeSymbol) =
    // REVIEW: We ignore solving if the type is an error.
    //         Typically, this is due to a Call expression returning an error type because it's
    //         currently a function group; which could be resolved later from function overloading.
    //         Not the greatest thing to do, but it works in this scenario.
    if ty.IsError_t then true
    else
    // Rules:
    //     1. Perform inference type unification when checking the argument type.
    //     2. If '1.' fails, then see if the expected type subsumes the given type.
    //     3. If '2.' fails, then check for shape subsumption if the expected type is a shape.
    if not (UnifyTypes rigidity expectedTy ty || subsumesTypeWith rigidity expectedTy ty || (if expectedTy.IsShape then subsumesShapeWith env.benv rigidity expectedTy ty else false)) then
        false
    else
        true

let solveTypes (env: SolverEnvironment) (syntaxNode: OlySyntaxNode) expectedTy (ty: TypeSymbol) =
    if not (solveTypesNoError expectedTy ty) && env.reportTypeErrors then
        env.diagnostics.Error(sprintf "Expected type '%s' but is '%s'." (printType env.benv expectedTy) (printType env.benv ty), 0, syntaxNode)

let solveTypesWithSubsumptionWith (env: SolverEnvironment) rigidity syntaxNode expectedTy (ty: TypeSymbol) =
    if not (solveTypesWithSubsumptionNoErrorWith env rigidity expectedTy ty) && env.reportTypeErrors then
        env.diagnostics.Error(sprintf "Expected type '%s' but is '%s'." (printType env.benv expectedTy) (printType env.benv ty), 0, syntaxNode)

let solveTypesWithSubsumption (env: SolverEnvironment) syntaxNode expectedTy (ty: TypeSymbol) =
    solveTypesWithSubsumptionWith env Flexible syntaxNode expectedTy ty

let solveFunctionAmbiguities env syntaxNode (funcs: IFunctionSymbol seq) (argTys: TypeSymbol imarray) =
    if Seq.isEmpty funcs then
        env.diagnostics.Error("No functions are present for this construct.", 0, syntaxNode)
        invalidFunction ()
    else
        let funcs =
            funcs
            |> Seq.filter (fun x -> 
                let ty = x.Type
                match ty.TryGetFunctionWithParameters() with
                | ValueSome(argTys2, _) when argTys.Length = argTys2.Length ->
                    (argTys, argTys2)
                    ||> ImArray.forall2 (UnifyTypes Rigid)
                | _ ->
                    false)
    
        let length = Seq.length funcs

        if length > 1 then
            let funcNames =
                funcs
                |> Seq.map (fun func -> func.Name)
                |> List.ofSeq
            env.diagnostics.Error(sprintf "Ambiguous functions found: %A. Consider being explicit." funcNames, 0, syntaxNode)

        if length = 0 then
            // TODO: This is a little weird, it's just to print "(.., ..)".
            let inputTy = TypeSymbol.Tuple(argTys, ImArray.empty)
            env.diagnostics.Error(sprintf "No functions matched the given input type '%s'." (printType env.benv inputTy), 0, syntaxNode)
            invalidFunction ()
        else
            funcs |> Seq.head

let private tryFindMostSpecificTypeForExtension benv (tyExt: EntitySymbol) targetTy =            
    tyExt.Implements
    |> filterMostSpecificTypes
    |> ImArray.filter (fun x ->
        subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv Generalizable targetTy x
    )
    |> Seq.tryExactlyOne

let solveShape env syntaxNode (tyArgs: TypeArgumentSymbol imarray) (witnessArgs: WitnessSolution imarray) (targetShape: TypeSymbol) (principalTyPar: TypeParameterSymbol) principalTyArg (isAttempt: bool) : Result<unit, ConstraintSolverError> =
    OlyAssert.True(targetShape.IsShape)

    let filteredWitnessArgs =
        witnessArgs
        |> ImArray.filter (fun x -> x.TypeParameter.Id = principalTyPar.Id && areTypesEqual x.Type targetShape)

    let shapeMembers = 
        subsumesShapeMembersWith env.benv TypeVariableRigidity.Generalizable QueryFunction.IntrinsicAndExtrinsic targetShape principalTyArg

    let isValid =
        if shapeMembers.IsEmpty then
            false
        else
            shapeMembers
            |> ImArray.forall (fun (_, xs) -> xs.Length = 1)

    if isValid then
        shapeMembers
        |> ImArray.iter (fun (abstractFunc, funcs) ->
            let func = funcs[0]

            if not(areFunctionTypeParameterConstraintsEqualWith Indexable abstractFunc func) then
                env.diagnostics.Error($"Shape member '{printValue env.benv abstractFunc}' has different constraints compared to '{printValue env.benv func}'.", 10, syntaxNode)
            else

            let witnessArgOpt =
                filteredWitnessArgs
                |> ImArray.tryFind (fun witnessArg ->
                    if not witnessArg.HasSolution then
                        match witnessArg.Function with
                        | Some func2 ->
                            areValueSignaturesEqual func2.Formal abstractFunc.Formal
                        | _ ->
                            false
                    else
                        false
                )

            match witnessArgOpt with
            | None -> ()// REVIEW: Should we report an error? If this happens, then there is probably an error reported elsewhere.
            | Some witnessArg ->
                let freshAbstractFunc = freshenValue env.benv abstractFunc :?> IFunctionSymbol

                match freshAbstractFunc.Type.TryGetFunctionWithParameters(), func.Type.TryGetFunctionWithParameters() with
                | ValueSome(argTys1, returnTy1), ValueSome(argTys2, returnTy2) ->
                    let isInstance = freshAbstractFunc.IsInstance
                    (argTys1, argTys2)
                    ||> ImArray.iteri2 (fun i ty1 ty2 ->
                        if isInstance && i = 0 then ()
                        else
                            solveTypes env syntaxNode ty1 ty2
                    )

                    solveTypes env syntaxNode returnTy1 returnTy2

                | _ ->
                    ()

                if func.Enclosing.IsTypeExtension then
                    witnessArg.Solution <- WitnessSymbol.TypeExtension(func.Enclosing.TryEntity.Value, Some freshAbstractFunc) |> Some
                else
                    witnessArg.Solution <- WitnessSymbol.Type(principalTyArg) |> Some
        )
        Ok()
    else
        if shapeMembers.IsEmpty then Ok()
        else
            let mutable err = None
            shapeMembers
            |> ImArray.iter (fun (abstractFunc, funcs) ->
                if funcs.IsEmpty then
                    let formalAbstractFunc = abstractFunc.Formal
                    // If the struct doesn't have an instance constructor, we will still allow it for the shape member '{ new() }' since it can technically be constructed.
                    if formalAbstractFunc.IsInstanceConstructor && formalAbstractFunc.AsFunction.LogicalParameterCount = 0 && principalTyArg.IsAnyStruct && not principalTyArg.IsTypeVariable then
                        ()
                    else
                        if not isAttempt then
                            env.diagnostics.Error($"Shape member '{printValue env.benv formalAbstractFunc}' does not exist on '{printType env.benv principalTyArg}'.", 10, syntaxNode)
                elif funcs.Length > 1 then
                    if err.IsNone then
                        err <- Some(ShapeMembers_AmbiguousFunctions(abstractFunc))                
            )
            match err with
            | Some(err) ->
                Error(err)
            | _ ->
                Ok()

let solveWitnessesByTypeParameter env (syntaxNode: OlySyntaxNode) (solver: WitnessSolver) (actualTarget: TypeSymbol) (tyPar: TypeParameterSymbol) (tyParTyArgs: TypeArgumentSymbol imarray) (witnesses: WitnessSolution seq) : bool =
    let possibleConstrs =
        tyPar.Constraints
        |> Seq.choose (fun constr ->
            match constr.TryGetAnySubtypeOf() with
            | ValueSome constrTy when (solver = WitnessSolver.Trait) || not constr.IsTrait ->
                if actualTarget.IsShape then
                    let constrTy =
                        if constrTy.IsTypeConstructor && constrTy.Arity = tyParTyArgs.Length then
                            applyType constrTy tyParTyArgs
                        else
                            constrTy
                    if subsumesShape env.benv actualTarget constrTy then
                        Some(constr, constrTy)
                    else
                        None
                // TODO: Do we need to take into account tyParTyArgs?
                elif subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv Generalizable actualTarget constrTy then
                    Some(constr, constrTy)
                else
                    None
            | _ ->
                None
        )
        |> List.ofSeq

    match possibleConstrs with
    | [] -> false
    | [(constr, constrTy)] ->
        if subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv Flexible actualTarget constrTy then
            witnesses
            |> Seq.iter (fun witness ->
                if not witness.HasSolution then
                    if subsumesType actualTarget witness.Type then
                        witness.Solution <- Some(WitnessSymbol.TypeParameter(tyPar))
            )
            true
        else
            false
    | _ ->
        let names =
            possibleConstrs
            |> Seq.map (fun (_, constrTy) -> printType env.benv constrTy)
            |> String.concat "\n"
        env.diagnostics.Error(sprintf "Unable to solve '%s' due to the possible implementations:\n    %s\nUse explicit type annotations." (printType env.benv (tyPar.AsType)) names, 10, syntaxNode)
        true // Return true for recovery

let rec solveWitnessesByType env (syntaxNode: OlySyntaxNode) (solver: WitnessSolver) (tyArgs: TypeArgumentSymbol imarray) (witnessArgs: WitnessSolution imarray) (target: TypeSymbol) (tyPar: TypeParameterSymbol) (ty: TypeSymbol) (isAttempt: bool) : Result<unit, ConstraintSolverError> =

    let ty =
        // Built-in types themselves are not entities, but an entity can be equivelant to a built-in type.
        // These entities might inherit or implement types, or contain functions that will make solving
        // the constraint pass.
        if ty.IsBuiltIn then
            match env.benv.TryFindEntityByIntrinsicType ty with
            | ValueSome ent -> ent.AsType
            | _ -> ty
        else
            ty

    let solveSubsumption () =
        if target.IsShape then
            solveShape env syntaxNode tyArgs witnessArgs target tyPar ty isAttempt
        else
            if subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv TypeVariableRigidity.Generalizable target ty then
                witnessArgs
                |> ImArray.iter (fun witness ->
                    if witness.HasSolution then ()
                    else
                        if subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv TypeVariableRigidity.Generalizable target witness.Type then
                            witness.Solution <- Some(WitnessSymbol.Type(ty))
                )
                Ok()
            else
                Error(GeneralFailure)

    // If we can solve the witness by subsumption, then we are done.
    // This allows the concrete implementation to take precedence over extensions.
    match solveSubsumption() with
    | Ok _ as res -> res
    | Error(ShapeMembers_AmbiguousFunctions _) as res -> res
    | _ ->

    // Type parameters from types do not support witnesses.
    // This isn't so much a limitation, but a design decision; augmenting a type with witnesses have usability issues for the end-user that isn't worth it.
    // However, the Oly Runtime has limited support for it but is impossible to describe it in OlyIL.
    //     The limited support is for implementation details when a type parameter is captured.
    // Current design is a work-in-progress.
    if tyPar.Kind = TypeParameterKind.Type then
        // TODO: Instead of returning a 'bool', we should return a structure that represents why a constraint failed.
        Error(GeneralFailure)
    else

    if solver <> WitnessSolver.Trait then
        Error(GeneralFailure)
    else

    match tryFindTypeExtensionsWithTargetType env.benv target ty with
    | ValueNone -> Error(GeneralFailure)
    | ValueSome(tyExts) ->
        OlyAssert.False(tyExts.IsEmpty)

        let tyExts =
            tyExts
            |> ImArray.filter (fun tyExt ->
                if tyPar.HasArity then
                    tyExt.IsTypeConstructor
                else
                    true
            )

        if tyExts.IsEmpty then
            env.diagnostics.Error("Unable to solve. TODO better msg.", 10, syntaxNode)
            Ok() // Return true for recovery
        elif tyExts.Length = 1 then
            let tyExt = tyExts[0]
            let mostSpecificTy = (tryFindMostSpecificTypeForExtension env.benv tyExt target).Value

            let witnessCandidates =
                witnessArgs
                |> ImArray.choose (fun witness ->      
                    if witness.HasSolution then None
                    else
                        if areTypeParametersEqual tyPar witness.TypeParameter && subsumesType target witness.Type && 
                           subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv Generalizable target mostSpecificTy then
                           Some witness
                        else
                            None
                )

            if witnessCandidates.Length = 1 then
                let witness = witnessCandidates[0]
                subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv Flexible target mostSpecificTy
                |> ignore
                let appliedTyExt = 
                    // Note: This is necessary to do!
                    if not tyExt.TypeParameters.IsEmpty && tyExt.IsFormal && not ty.IsFormal then
                        applyEntity ty.TypeArguments tyExt
                    else
                        tyExt
                witness.Solution <- Some(WitnessSymbol.TypeExtension(appliedTyExt, None))
            elif witnessCandidates.Length > 1 then
                // TODO: Provide a test case that hits this diagnostic, is it possible?
                env.diagnostics.Error($"Solving witnesses is too complex.", 10, syntaxNode)

            Ok()
        else
            let names =
                tyExts
                |> Seq.map (fun x -> printEntity env.benv x)
                |> String.concat "\n    "
            env.diagnostics.Error(sprintf "Unable to solve due to ambiguity of the possibly resolved constraints:\n    %s\n\nUse explicit type annotations to disambiguate." names, 10, syntaxNode)
            Ok() // Return true for recovery

and solveWitnesses env (syntaxNode: OlySyntaxNode) (solver: WitnessSolver) (tyArgs: TypeArgumentSymbol imarray) (witnessArgs: WitnessSolution imarray) (target: TypeSymbol) (tyPar: TypeParameterSymbol) (tyArg: TypeArgumentSymbol) (isAttempt: bool): Result<unit, ConstraintSolverError> =
    OlyAssert.True(tyArg.IsSolved)

    let ty = stripTypeEquations tyArg
    match ty with
    | TypeSymbol.HigherVariable(tyPar2, tyParTyArgs) ->
        if solveWitnessesByTypeParameter env syntaxNode solver target tyPar2 tyParTyArgs witnessArgs then
            Ok()
        else
            Error(GeneralFailure)
    | TypeSymbol.Variable(tyPar2) ->
        if solveWitnessesByTypeParameter env syntaxNode solver target tyPar2 ImArray.empty witnessArgs then
            Ok()
        else
            Error(GeneralFailure)
    | _ ->
        solveWitnessesByType env syntaxNode solver tyArgs witnessArgs target tyPar ty isAttempt

and solveConstraintNull env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    tyArg.IsNullable

and solveConstraintStruct env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.Struct -> true | _ -> false)

    | tyArg ->
        tyArg.IsAnyStruct

and solveConstraintNotStruct env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.NotStruct -> true | _ -> false)

    | tyArg ->
        not tyArg.IsAnyStruct

and solveConstraintUnmanaged env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    tyArg.IsUnmanaged(env.pass)

and solveConstraintBlittable env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    tyArg.IsBlittable(env.pass)

and solveConstraintScoped env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.Scoped -> true | _ -> false)

    | _ ->
        true // any type can work

and solveConstraintConstantType env (syntaxNode: OlySyntaxNode) (constTy: TypeSymbol) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.Variable(tyPar) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.ConstantType(constTy2) -> areTypesEqual constTy constTy2.Value | _ -> false)

    | tyArg ->
        match stripTypeEquationsAndBuiltIn constTy, tyArg with
        | TypeSymbol.Int32, TypeSymbol.ConstantInt32 _ -> true
        | _ -> false

and solveConstraint env (syntaxNode: OlySyntaxNode) (tyArgs: TypeArgumentSymbol imarray) (witnessArgs: WitnessSolution imarray) (constr: ConstraintSymbol) tyPar (tyArg: TypeArgumentSymbol) (isAttempt: bool) : Result<unit, ConstraintSolverError> =
    OlyAssert.True(tyArg.IsSolved)

    if tyArg.IsError_t then
        // Error recovery: always assume the constraint is solved for an error type
        Ok()
    else
        match constr with
        | ConstraintSymbol.Null ->
            if solveConstraintNull env syntaxNode tyArg then
                Ok()
            else
                Error(GeneralFailure)
        | ConstraintSymbol.Struct ->
            if solveConstraintStruct env syntaxNode tyArg then
                Ok()
            else
                Error(GeneralFailure)
        | ConstraintSymbol.NotStruct ->
            if solveConstraintNotStruct env syntaxNode tyArg then
                Ok()
            else
                Error(GeneralFailure)
        | ConstraintSymbol.Unmanaged ->
            if solveConstraintUnmanaged env syntaxNode tyArg then
                Ok()
            else
                Error(GeneralFailure)
        | ConstraintSymbol.Blittable ->
            if solveConstraintBlittable env syntaxNode tyArg then
                Ok()
            else
                Error(GeneralFailure)
        | ConstraintSymbol.Scoped ->
            if solveConstraintScoped env syntaxNode tyArg then
                Ok()
            else
                Error(GeneralFailure)
        | ConstraintSymbol.SubtypeOf(target) ->
            solveWitnesses env syntaxNode WitnessSolver.Subtype tyArgs witnessArgs target.Value tyPar tyArg isAttempt
        | ConstraintSymbol.ConstantType(constTy) ->
            if solveConstraintConstantType env syntaxNode constTy.Value tyArg then
                Ok()
            else
                Error(GeneralFailure)
        | ConstraintSymbol.TraitType(target) ->
            solveWitnesses env syntaxNode WitnessSolver.Trait tyArgs witnessArgs target.Value tyPar tyArg isAttempt
 
and solveConstraints
        (env: SolverEnvironment) 
        (skipUnsolved: bool)
        (syntaxNode: OlySyntaxNode) 
        (syntaxTyArgsOpt: OlySyntaxType imarray option) 
        (tyPars: TypeParameterSymbol imarray)
        (tyArgs: TypeArgumentSymbol imarray)
        (isAttempt: bool)
        (witnessArgs: WitnessSolution imarray) =

#if DEBUG || CHECKED
    OlyAssert.Equal(tyPars.Length, tyArgs.Length)

    match env.pass with
    | Pass3
    | Pass4 
    | PostInferenceAnalysis -> ()
    | _ -> failwith "Unexpected compiler pass"
#endif

    tyArgs
    |> ImArray.iteri (fun i tyArg ->
        if tyArg.IsSolved then
            match tyArg.TryImmedateTypeParameter with
            | ValueSome tyParToCheck ->
                let tyPar = tyPars[i]
                OlyAssert.Equal(tyPar.Id, tyParToCheck.Id)
                let syntaxNode: OlySyntaxNode =
                    match syntaxTyArgsOpt with
                    | Some syntaxTyArgs when i < syntaxTyArgs.Length ->
                        syntaxTyArgs[i]
                    | _ ->
                        syntaxNode

                let witnessArgs =
                    witnessArgs
                    |> ImArray.filter (fun x -> x.TypeParameter.Id = tyPar.Id && not x.HasSolution)

                let subTys =
                    tyPar.Constraints
                    |> ImArray.choose (fun constr ->
                        match constr.TryGetAnySubtypeOf() with
                        | ValueSome(ty) -> Some ty
                        | _ -> None
                    )

                // This allows to infer types if the there is only one subtype constraint.
                // TODO: We do something similar when solving the witness, could we combine them somehow?
                if subTys.Length = 1 then
                    let subTy = subTys[0]
                    if subsumesTypeInEnvironmentWith env.benv Flexible subTy tyArg then
                        ()
                    else
                        match tryFindTypeExtensionsWithTargetType env.benv subTy tyArg with
                        | ValueNone -> ()
                        | ValueSome(tyExts) ->
                            OlyAssert.False(tyExts.IsEmpty)
                            if tyExts.Length = 1 then
                                let tyExt = tyExts[0]
                                let appliedTyExt = 
                                    // Note: This is necessary to do!
                                    if not tyExt.TypeParameters.IsEmpty && tyExt.IsFormal && not tyArg.IsFormal then
                                        applyEntity tyArg.TypeArguments tyExt
                                    else
                                        tyExt
                                let mostSpecificTy = (tryFindMostSpecificTypeForExtension env.benv appliedTyExt subTy).Value
                                if areGeneralizedTypesEqual mostSpecificTy subTy then
                                    UnifyTypes Flexible mostSpecificTy subTy |> ignore
                    
                tyPar.Constraints
                |> ImArray.iter (fun constr ->
                    let constr = constr.Substitute(tyArgs)
                    let solved = solveConstraint env syntaxNode tyArgs witnessArgs constr tyPar tyArg isAttempt

                    match solved with
                    | Error(GeneralFailure) ->
                        if not isAttempt then
                            Error_MissingConstraint(env.benv, syntaxNode, tyArg, constr)
                            |> env.diagnostics.Report
                    | Error(ShapeMembers_AmbiguousFunctions(abstractFunc)) ->
                        if not isAttempt then
                            // TODO: Better error
                            env.diagnostics.Error($"'{printValueName abstractFunc}' has ambiguous functions.", 10, syntaxNode)
                    | _ ->
                        ()
                )

                witnessArgs
                |> ImArray.filter (fun x -> not x.HasSolution)
                |> ImArray.iter (fun x ->
                    x.Solution <- Some(WitnessSymbol.Type(TypeSymbol.Error(None, None)))
                )
            | _ ->
                OlyAssert.Fail("Expected type parameter associated with inference variable.")
        else
            if not skipUnsolved then
                env.diagnostics.Error($"Type parameter '{(printType env.benv tyArg)}' was unable to be inferred.", 10, syntaxNode)
                match tyArg.TryImmedateTypeParameter with
                | ValueSome tyParToCheck ->
                    let tyPar = tyPars[i]
                    OlyAssert.Equal(tyPar.Id, tyParToCheck.Id)
                    witnessArgs
                    |> ImArray.filter (fun x -> x.TypeParameter.Id = tyPar.Id && not x.HasSolution)
                    |> ImArray.iter (fun x ->
                        x.Solution <- Some(WitnessSymbol.Type(TypeSymbol.Error(Some tyPar, None)))
                    )
                    UnifyTypes Flexible tyArg (TypeSymbol.Error(Some tyPar, None)) |> ignore
                | _ ->
                    OlyAssert.Fail("Expected type parameter associated with inference variable.")
    )

and solveFunctionConstraints 
        env 
        skipUnsolved
        (syntaxNode: OlySyntaxNode) 
        (syntaxEnclosingTyArgsOpt: OlySyntaxType imarray option)
        (enclosingTyPars: TypeParameterSymbol imarray)
        (enclosingTyArgs: TypeArgumentSymbol imarray)
        (syntaxFuncTyArgsOpt: OlySyntaxType imarray option)
        (funcTyPars: TypeParameterSymbol imarray)
        (funcTyArgs: TypeArgumentSymbol imarray) 
        (isAttempt: bool)
        (witnessArgs: WitnessSolution imarray) =
    solveConstraints env skipUnsolved syntaxNode syntaxEnclosingTyArgsOpt enclosingTyPars enclosingTyArgs isAttempt witnessArgs
    solveConstraints env skipUnsolved syntaxNode syntaxFuncTyArgsOpt funcTyPars funcTyArgs isAttempt witnessArgs
