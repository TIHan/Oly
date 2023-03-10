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

[<NoEquality;NoComparison;Struct>]
type SolverEnvironment =
    {
        diagnostics: OlyDiagnosticLogger
        benv: BoundEnvironment
    }

    static member Create(diagnostics, benv) =
        {
            diagnostics = diagnostics
            benv = benv
        }

let rec solveTypes (env: SolverEnvironment) (syntaxNode: OlySyntaxNode) expectedTy (ty: TypeSymbol) =
    let res = UnifyTypes Flexible expectedTy ty
    if not res then
        env.diagnostics.Error(sprintf "Expected type '%s' but is '%s'." (printType env.benv expectedTy) (printType env.benv ty), 0, syntaxNode)

let rec solveTypesWithSubsumption (env: SolverEnvironment) syntaxNode expectedTy (ty: TypeSymbol) =
    // REVIEW: We ignore solving if the type is an error.
    //         Typically, this is due to a Call expression returning an error type because it's
    //         currently a function group; which could be resolved later from function overloading.
    //         Not the greatest thing to do, but it works in this scenario.
    if ty.IsError_t then ()
    else
    // Rules:
    //     1. Perform inference type unification when checking the argument type.
    //     2. If '1.' fails, then see if the expected type subsumes the given type.
    //     3. If '2.' fails, then check for shape subsumption if the expected type is a shape.
    if not (UnifyTypes Flexible expectedTy ty || subsumesType expectedTy ty || (if expectedTy.IsShape then subsumesShape env.benv expectedTy ty else false)) then 
        env.diagnostics.Error(sprintf "Expected type '%s' but is '%s'." (printType env.benv expectedTy) (printType env.benv ty), 0, syntaxNode)

and solveFunctionInput env (syntaxNode: OlySyntaxNode) (expectedArgTys: TypeSymbol imarray) (argTysWithSyntax: (TypeSymbol * OlySyntaxNode) imarray) =
    let argTys =
        argTysWithSyntax
        |> ImArray.map fst

    if expectedArgTys.Length <> argTys.Length then
        env.diagnostics.Error(sprintf "Expected %i argument(s) but only given %i." expectedArgTys.Length argTys.Length, 0, syntaxNode)
    else
        (expectedArgTys, argTys)
        ||> Seq.iteri2 (fun i tyx tyy ->
            let syntax = (argTysWithSyntax.[i] |> snd)
            let syntax =
                if syntax.IsDummy then
                    syntaxNode
                else
                    syntax
            solveTypesWithSubsumption env syntax tyx tyy
        )

and solveFunctionAmbiguities env syntaxNode (funcs: IFunctionSymbol seq) (argTys: TypeSymbol imarray) =
    if Seq.isEmpty funcs then
        env.diagnostics.Error("No functions are present for this construct.", 0, syntaxNode)
        invalidFunction ()
    else
        let funcs =
            funcs
            |> Seq.filter (fun x -> 
                let ty = x.Type
                match ty.TryFunction with
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

let solveShape env syntaxNode (tyArgs: TypeArgumentSymbol imarray) (witnessArgs: WitnessSolution imarray) (targetShape: TypeSymbol) (principalTyPar: TypeParameterSymbol) principalTyArg =
    OlyAssert.True(targetShape.IsShape)

    let filteredWitnessArgs =
        witnessArgs
        |> ImArray.filter (fun x -> x.TypeParameter.Id = principalTyPar.Id && areEntitiesEqual x.Entity targetShape.AsEntity)

    let shapeMembers = 
        subsumesShapeMembersWith env.benv TypeVariableRigidity.Generalizable QueryFunction.IntrinsicAndExtrinsic targetShape principalTyArg
        |> ImArray.ofSeq

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
            | None -> () // REVIEW: Should we report an error? If this happens, then there is probably an error reported elsewhere.
            | Some witnessArg ->
                let freshAbstractFunc = freshenValue env.benv abstractFunc :?> IFunctionSymbol

                match freshAbstractFunc.Type.TryFunction, func.Type.TryFunction with
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
    else
        if shapeMembers.IsEmpty then ()
        else
            shapeMembers
            |> ImArray.iter (fun (abstractFunc, funcs) ->
                if funcs.IsEmpty then
                    env.diagnostics.Error($"Shape member '{printValue env.benv abstractFunc.Formal}' does not exist on '{printType env.benv principalTyArg}'.", 10, syntaxNode)
                elif funcs.Length > 1 then
                    env.diagnostics.Error($"'{printValueName abstractFunc}' has ambiguous functions.", 10, syntaxNode)                     
            )

let solveWitnessesByTypeParameter env (syntaxNode: OlySyntaxNode) (actualTarget: TypeSymbol) (tyPar: TypeParameterSymbol) (tyParTyArgs: TypeArgumentSymbol imarray) (witnesses: WitnessSolution seq) : bool =
    let possibleConstrs =
        tyPar.Constraints
        |> Seq.choose (fun constr ->
            match constr.TryGetSubtypeOf() with
            | ValueSome constrTy ->
                if actualTarget.IsShape then
                    let constrTy =
                        if constrTy.IsTypeConstructor && constrTy.Arity = tyParTyArgs.Length then
                            constrTy.Apply(tyParTyArgs)
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
                    if subsumesType actualTarget witness.Entity.AsType then
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

let rec solveWitnessesByType env (syntaxNode: OlySyntaxNode) (tyArgs: TypeArgumentSymbol imarray) (witnessArgs: WitnessSolution imarray) (target: TypeSymbol) (tyPar: TypeParameterSymbol) (ty: TypeSymbol) =

    let ty =
        if target.IsShape then
            // Built-in types can never fit shapes,
            // so try to use an entity that is type equivelant.
            if ty.IsBuiltIn then
                match env.benv.TryFindEntityByIntrinsicType ty with
                | ValueSome ent -> ent.AsType
                | _ -> ty
            else
                ty
        else
            ty

    let solveSubsumption () =
        if target.IsShape then
            solveShape env syntaxNode tyArgs witnessArgs target tyPar ty
            true // Error recovery // TODO: We should make better error messages for constraints
        else
            if subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv TypeVariableRigidity.Generalizable target ty then
                subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv TypeVariableRigidity.FlexibleAndGeneralizable target ty |> ignore
                witnessArgs
                |> ImArray.iter (fun witness ->
                    if not witness.HasSolution then
                        // TODO: This shouldn't be generalizable. Hidden bug here...
                        if subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv TypeVariableRigidity.Generalizable target witness.Entity.AsType then
                            witness.Solution <- Some(WitnessSymbol.Type(ty))
                )
                true
            else
                false

    // Type parameters from types cannot solve for type extensions.
    if tyPar.Kind = TypeParameterKind.Type then
        solveSubsumption()
    else

    match env.benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty) with
    | ValueSome tyExts ->

        let possibleTyExts =
            tyExts.GetSimilar(target)
            |> List.ofSeq

        match possibleTyExts with
        | [] -> solveSubsumption()
        | [tyExt] ->
            let mostSpecificTy =              
                tyExt.Implements
                |> filterMostSpecificTypes
                |> ImArray.filter (fun x ->
                    subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv Generalizable target x
                )
                |> Seq.tryExactlyOne

            let mostSpecificTy =
                match mostSpecificTy with
                | Some x -> x
                | _ -> failwith "Should have found the most specific type."

            witnessArgs
            |> Seq.iter (fun witness ->              
                if areTypeParametersEqual tyPar witness.TypeParameter && subsumesType target witness.Entity.AsType && subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith env.benv FlexibleAndGeneralizable target mostSpecificTy then
                    witness.Solution <- Some(WitnessSymbol.TypeExtension(tyExt, None))
                    solveConstraintsAux env syntaxNode None tyExt.TypeArguments witnessArgs
            )
            true

        | _ ->
            let names =
                possibleTyExts
                |> Seq.map (fun x -> printEntity env.benv x)
                |> String.concat "\n    "
            env.diagnostics.Error(sprintf "Unable to solve due to ambiguity of the possibly resolved constraints:\n    %s\n\nUse explicit type annotations to disambiguate." names, 10, syntaxNode)
            true // Return true for recovery
    | _ ->
        solveSubsumption()

and solveWitnessesByInferenceVariable env (syntaxNode: OlySyntaxNode) (witnessArgs: WitnessSolution imarray) ty =
    match stripTypeEquations ty with
    | TypeSymbol.InferenceVariable(varTyParOpt, varSolution) ->

        match varTyParOpt with
        | None ->
            witnessArgs
            |> ImArray.iter (fun witnessArg ->
                let constr = ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(witnessArg.Entity.AsType))
                if varSolution.Constraints |> Seq.exists (fun constr2 -> areConstraintsEqual constr constr2) |> not then
                    varSolution.AddConstraint(constr)
                witnessArg.Solution <- WitnessSymbol.Type(ty) |> Some
            )
            true
        | Some tyPar ->
            witnessArgs
            |> ImArray.forall (fun witnessArg ->
                let witnessTy = witnessArg.Entity.AsType

                let satisfiesConstr =
                    tyPar.Constraints
                    |> ImArray.exists (fun constr ->
                        match constr with
                        | ConstraintSymbol.Null
                        | ConstraintSymbol.Struct
                        | ConstraintSymbol.NotStruct 
                        | ConstraintSymbol.Unmanaged 
                        | ConstraintSymbol.ConstantType _ -> false
                        | ConstraintSymbol.SubtypeOf(ty) ->
                            if areTypesEqual ty.Value witnessTy then
                                true
                            else
                                match witnessTy.TryImmedateTypeParameter with
                                | ValueSome(tyPar) ->
                                    tyPar.Constraints
                                    |> ImArray.exists (fun constr2 ->
                                        areConstraintsEqual constr constr2
                                    )
                                | _ ->
                                    false
                    )
                if satisfiesConstr then
                    witnessArg.Solution <- WitnessSymbol.TypeParameter(tyPar) |> Some
                satisfiesConstr
            )

    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        env.diagnostics.Error("Higher inference variable not supported for inferring constraints.", 10, syntaxNode)
        false

    | _ ->
        false

and solveWitnesses env (syntaxNode: OlySyntaxNode) (tyArgs: TypeArgumentSymbol imarray) (witnessArgs: WitnessSolution imarray) (target: TypeSymbol) (tyPar: TypeParameterSymbol) (tyArg: TypeArgumentSymbol) =
    let ty = stripTypeEquations tyArg
    match ty with
    | TypeSymbol.InferenceVariable _
    | TypeSymbol.HigherInferenceVariable _ ->
        solveWitnessesByInferenceVariable env syntaxNode witnessArgs ty
    | TypeSymbol.HigherVariable(tyPar2, tyParTyArgs) ->
        solveWitnessesByTypeParameter env syntaxNode target tyPar2 tyParTyArgs witnessArgs
    | TypeSymbol.Variable(tyPar2) ->
        solveWitnessesByTypeParameter env syntaxNode target tyPar2 ImArray.empty witnessArgs
    | _ ->
        solveWitnessesByType env syntaxNode tyArgs witnessArgs target tyPar ty

and solveConstraintNull env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.InferenceVariable(varTyParOpt, varSolution) ->
        match varTyParOpt with
        | None ->
            if varSolution.Constraints |> Seq.exists (function ConstraintSymbol.Null -> true | _ -> false) |> not then
                varSolution.AddConstraint(ConstraintSymbol.Null)
            true
        | Some tyPar ->
            tyPar.Constraints 
            |> ImArray.exists (function ConstraintSymbol.Null -> true | _ -> false)

    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        env.diagnostics.Error("Higher inference variable not supported for inferring constraints.", 10, syntaxNode)
        false

    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.Null -> true | _ -> false)

    | tyArg ->
        tyArg.IsNullable

and solveConstraintStruct env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.InferenceVariable(varTyParOpt, varSolution) ->
        match varTyParOpt with
        | None ->
            if varSolution.Constraints |> Seq.exists (function ConstraintSymbol.Struct -> true | _ -> false) |> not then
                varSolution.AddConstraint(ConstraintSymbol.Struct)
            true
        | Some tyPar ->
            tyPar.Constraints 
            |> ImArray.exists (function ConstraintSymbol.Struct -> true | _ -> false)

    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        env.diagnostics.Error("Higher inference variable not supported for inferring constraints.", 10, syntaxNode)
        false

    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.Struct -> true | _ -> false)

    | tyArg ->
        tyArg.IsAnyStruct

and solveConstraintNotStruct env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.InferenceVariable(varTyParOpt, varSolution) ->
        match varTyParOpt with
        | None ->
            if varSolution.Constraints |> Seq.exists (function ConstraintSymbol.NotStruct -> true | _ -> false) |> not then
                varSolution.AddConstraint(ConstraintSymbol.NotStruct)
            true
        | Some tyPar ->
            tyPar.Constraints 
            |> ImArray.exists (function ConstraintSymbol.NotStruct -> true | _ -> false)

    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        env.diagnostics.Error("Higher inference variable not supported for inferring constraints.", 10, syntaxNode)
        false

    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.NotStruct -> true | _ -> false)

    | tyArg ->
        not tyArg.IsAnyStruct

and solveConstraintUnmanaged env (syntaxNode: OlySyntaxNode) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.InferenceVariable(varTyParOpt, varSolution) ->
        match varTyParOpt with
        | None ->
            if varSolution.Constraints |> Seq.exists (function ConstraintSymbol.Unmanaged -> true | _ -> false) |> not then
                varSolution.AddConstraint(ConstraintSymbol.Unmanaged)
            true
        | Some tyPar ->
            tyPar.Constraints 
            |> ImArray.exists (function ConstraintSymbol.Unmanaged -> true | _ -> false)

    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        env.diagnostics.Error("Higher inference variable not supported for inferring constraints.", 10, syntaxNode)
        false

    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.Unmanaged -> true | _ -> false)

    | tyArg ->
        // TODO: This doesn't actually check for unmanaged, but good enough for now. We will need to fix it.
        tyArg.IsUnmanaged

and solveConstraintConstantType env (syntaxNode: OlySyntaxNode) (constTy: TypeSymbol) (tyArg: TypeArgumentSymbol) =
    match stripTypeEquations tyArg with
    | TypeSymbol.InferenceVariable(varTyParOpt, varSolution) ->
        match varTyParOpt with
        | None ->
            if varSolution.Constraints |> Seq.exists (function ConstraintSymbol.ConstantType(constTy2) -> areTypesEqual constTy constTy2.Value | _ -> false) |> not then
                varSolution.AddConstraint(ConstraintSymbol.ConstantType(Lazy<_>.CreateFromValue(constTy)))
            true
        | Some tyPar ->
            tyPar.Constraints 
            |> ImArray.exists (function ConstraintSymbol.ConstantType(constTy2) -> areTypesEqual constTy constTy2.Value | _ -> false)

    | TypeSymbol.HigherInferenceVariable(_, _, _, _) ->
        env.diagnostics.Error("Higher inference variable not supported for inferring constraints.", 10, syntaxNode)
        false

    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _) ->
        tyPar.Constraints 
        |> ImArray.exists (function ConstraintSymbol.ConstantType(constTy2) -> areTypesEqual constTy constTy2.Value | _ -> false)

    | tyArg ->
        match stripTypeEquationsAndBuiltIn constTy, tyArg with
        | TypeSymbol.Int32, TypeSymbol.ConstantInt32 _ -> true
        | _ -> false

and solveConstraint env (syntaxNode: OlySyntaxNode) (tyArgs: TypeArgumentSymbol imarray) (witnessArgs: WitnessSolution imarray) (constr: ConstraintSymbol) tyPar (tyArg: TypeArgumentSymbol) =
    match constr with
    | ConstraintSymbol.Null ->
        solveConstraintNull env syntaxNode tyArg
    | ConstraintSymbol.Struct ->
        solveConstraintStruct env syntaxNode tyArg
    | ConstraintSymbol.NotStruct ->
        solveConstraintNotStruct env syntaxNode tyArg
    | ConstraintSymbol.Unmanaged ->
        solveConstraintUnmanaged env syntaxNode tyArg
    | ConstraintSymbol.ConstantType(constTy) ->
        solveConstraintConstantType env syntaxNode constTy.Value tyArg
    | ConstraintSymbol.SubtypeOf(target) ->
        solveWitnesses env syntaxNode tyArgs witnessArgs target.Value tyPar tyArg
 
and private solveConstraintsAux
        env 
        (syntaxNode: OlySyntaxNode) 
        (syntaxTyArgsOpt: OlySyntaxType imarray option) 
        (tyArgs: TypeArgumentSymbol imarray) 
        (witnessArgs: WitnessSolution imarray) =
    tyArgs
    |> ImArray.iteri (fun i tyArg ->
        match tyArg.TryImmedateTypeParameter with
        | ValueSome tyPar ->
            let syntaxNode: OlySyntaxNode =
                match syntaxTyArgsOpt with
                | Some syntaxTyArgs ->
                    syntaxTyArgs[i]
                | _ ->
                    syntaxNode
                    
            tyPar.Constraints
            |> ImArray.iter (fun constr ->
                let constr = constr.Substitute(tyArgs)
                let solved = solveConstraint env syntaxNode tyArgs witnessArgs constr tyPar tyArg

                if not solved then
                    env.diagnostics.Error(sprintf "Type instantiation '%s' is missing the constraint '%s'." (printType env.benv tyArg) (printConstraint env.benv constr), 3, syntaxNode)
            )
        | _ ->
            ()
    )

and solveConstraints 
        env 
        (syntaxNode: OlySyntaxNode) 
        (syntaxEnclosingTyArgsOpt: OlySyntaxType imarray option)
        (enclosingTyArgs: TypeArgumentSymbol imarray)
        (syntaxFuncTyArgsOpt: OlySyntaxType imarray option)
        (funcTyArgs: TypeArgumentSymbol imarray) 
        (witnessArgs: WitnessSolution imarray) =
    solveConstraintsAux env syntaxNode syntaxEnclosingTyArgsOpt enclosingTyArgs witnessArgs
    solveConstraintsAux env syntaxNode syntaxFuncTyArgsOpt funcTyArgs witnessArgs
