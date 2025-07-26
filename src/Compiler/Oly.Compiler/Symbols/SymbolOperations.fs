module internal rec Oly.Compiler.Internal.SymbolOperations

open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler.Internal.Symbols

type IFunctionSymbol with

    member this.IsOverriding(func: IFunctionSymbol) =
        if this.Id <> func.Id then
            match this.FunctionOverrides with
            | Some(overrides) -> overrides.Id = func.Id
            | _ -> 
                if not this.IsVirtual && func.IsVirtual && this.IsInstance = func.IsInstance then
                    let mutable superEnt = Unchecked.defaultof<_>
                    let mutable ent = Unchecked.defaultof<_>
                    if func.Enclosing.TryGetEntity(&superEnt) &&
                       this.Enclosing.TryGetEntity(&ent) &&
                       subsumesEntity superEnt ent then
                        areLogicalFunctionSignaturesParameterOnlyEqual func this &&
                        areTypesEqual func.ReturnType this.ReturnType
                        // Covariant return types
                     //   subsumesType func.ReturnType this.ReturnType
                    else 
                        false
                else
                    false
        else
            false

type IPropertySymbol with

    member this.IsOverriding(prop: IPropertySymbol) =
        let isGetterOverriding =
            match this.Getter, prop.Getter with
            | Some thisGetter, Some propGetter ->
                thisGetter.IsOverriding(propGetter)
            | _ ->
                false

        let isSetterOverriding =
            match this.Setter, prop.Setter with
            | Some thisSetter, Some propSetter ->
                thisSetter.IsOverriding(propSetter)
            | _ ->
                false

        isGetterOverriding || isSetterOverriding

[<AutoOpen>]
module SymbolComparers =

    let private areEntitiesSimilar (ent1: EntitySymbol) (ent2: EntitySymbol) =
        if ent1.Name = ent2.Name && ent1.TypeParameters.Length = ent2.TypeParameters.Length then
            areEnclosingsSimilar ent1.Enclosing ent2.Enclosing
        else
            false

    let private areEnclosingsSimilar (enclosing1: EnclosingSymbol) (enclosing2: EnclosingSymbol) =
        match enclosing1, enclosing2 with
        | EnclosingSymbol.Local, EnclosingSymbol.Local -> true
        | EnclosingSymbol.Witness _, EnclosingSymbol.Witness _ -> false // we don't really check for witnesses here, so it's fine
        | EnclosingSymbol.RootNamespace, EnclosingSymbol.RootNamespace -> true
        | EnclosingSymbol.Entity(ent1), EnclosingSymbol.Entity(ent2) ->
            areEntitiesSimilar ent1.Formal ent2.Formal
        | _ ->
            false

    let MultiStringComparer() =
        { new EqualityComparer<string imarray>() with
            member _.GetHashCode(strs) = strs.Length
            member _.Equals(strs1, strs2) =
                (strs1, strs2)
                ||> ImArray.forall2 (fun str1 str2 -> str1 = str2)
        }

    let TypeSymbolGeneralizedComparer() =
        { new EqualityComparer<TypeSymbol>() with
            member _.GetHashCode(ty) = int ty.FormalId
            member _.Equals(ty1, ty2) = UnifyTypes Generalizable ty1 ty2
        }

    let TypeSymbolComparer() =
        { new EqualityComparer<TypeSymbol>() with
            member _.GetHashCode(ty) = int ty.FormalId
            member _.Equals(ty1, ty2) = areTypesEqual ty1 ty2
        }

    let ValueSymbolComparer() =
        { new EqualityComparer<IValueSymbol>() with
            member _.GetHashCode(value) = int value.Id
            member _.Equals(value1, value2) = value1.Id = value2.Id
        }

    let FunctionSignatureSymbolComparer() =
        { new EqualityComparer<IFunctionSymbol>() with
            member _.GetHashCode(func) = func.Name.GetHashCode()
            member _.Equals(func1, func2) = areLogicalFunctionSignaturesEqual func1 func2
        }

    let EntitySymbolGeneralizedComparer() =
        { new EqualityComparer<EntitySymbol>() with
            member _.GetHashCode(ent) = int ent.FormalId
            member _.Equals(ent1, ent2) = areGeneralizedEntitiesEqual ent1 ent2
        }
    
    let EntitySymbolComparer() =
        { new EqualityComparer<EntitySymbol>() with
            member _.GetHashCode(ent) = int ent.FormalId
            member _.Equals(ent1, ent2) = areEntitiesEqual ent1 ent2
        }

    let SimilarEntitySymbolComparer() =
        { new EqualityComparer<EntitySymbol>() with
            member _.GetHashCode(ent) = ent.Name.GetHashCode()
            member _.Equals(ent1, ent2) = areEntitiesSimilar ent1 ent2
        }

    let SimilarValueSymbolComparer() =
        { new EqualityComparer<IValueSymbol>() with
            member _.GetHashCode(value) = value.Name.GetHashCode()
            member _.Equals(value1, value2) = areValueSignaturesEqual value1 value2 && (areEnclosingsEqual value1.Formal.Enclosing value2.Formal.Enclosing)
        }

    let TypeParameterSymbolComparer() =
        { new EqualityComparer<TypeParameterSymbol>() with
            member _.GetHashCode(tyPar) = int tyPar.Id
            member _.Equals(tyPar1, tyPar2) = tyPar1.Id = tyPar2.Id
        }

type TypeVariableRigidity =
    /// No inference. Types must exactly match.
    | Rigid

    /// No inference. Similar to 'Rigid', but type variables can be equal if the indices are equal.
    | Indexable

    /// Can solve inference variables. Type variables can be equal if the indices are equal.
    | Flexible

    /// No inference, but is relaxed on type variables equality.
    | Generalizable

    | IntegerGeneralizable

    | NumberGeneralizable

// --------------------------------------------------------------------------------------------------------------------

let private solveHigherInferenceVariable (rigidity: TypeVariableRigidity) (tyArgs: TypeSymbol imarray) (externalSolution: VariableSolutionSymbol) (solution: VariableSolutionSymbol) (ty2: TypeSymbol) =
    let result0 =
        if externalSolution.HasSolution then
            let ty1 = externalSolution.Solution
            ty1.FormalId = ty2.FormalId
        else
            true

    if result0 then
        OlyAssert.Equal(tyArgs.Length, ty2.LogicalTypeArguments.Length)

        let result =
            (tyArgs, ty2.LogicalTypeArguments)
            ||> ImArray.forall2 (fun ty1 ty2 ->
                UnifyTypes rigidity ty1 ty2
            )

        if result then
            let ty2 =
                if ty2.TypeArguments.Length <> tyArgs.Length then
                    // REVIEW: This is a tad inefficient, but not very common to hit among user code.
                    let constrsList = ResizeArray()
                    let formalTy2 = ty2.Formal
                    let forallTyPars = 
                        formalTy2.TypeParameters 
                        |> ImArray.skip (ty2.TypeArguments.Length - tyArgs.Length)
                        |> ImArray.mapi (fun i tyPar ->
                            let constrs = ref tyPar.Constraints
                            constrsList.Add(constrs)
                            TypeParameterSymbol(tyPar.Name, i, tyPar.Arity, tyPar.IsVariadic, TypeParameterKind.Type, constrs)
                        )
                    let forallTyArgs = ty2.TypeArguments |> Seq.take (ty2.TypeArguments.Length - tyArgs.Length) |> ImArray.ofSeq
                    let forallTyArgs = forallTyArgs.AddRange(forallTyPars |> ImArray.map (fun x -> x.AsType))
                    constrsList
                    |> Seq.iter (fun constrs ->
                        constrs.contents <-
                            constrs.contents
                            |> ImArray.map (fun constr ->
                                constr.Substitute(forallTyArgs)
                            )
                    )
                    TypeSymbol.ForAll(forallTyPars, applyType formalTy2 forallTyArgs)
                else
                    ty2

            if externalSolution.HasSolution then
                if UnifyTypes rigidity externalSolution.Solution ty2 then
                    // TODO: This isn't properly tested. Should figure out what test could hit this path.
                    solution.Solution <- applyType ty2 tyArgs
                    true
                else
                    false
            else
                solution.Solution <- applyType ty2.Formal tyArgs
                match ty2.Formal with
                | TypeSymbol.InferenceVariable(_, solution) ->
                    if obj.ReferenceEquals(externalSolution, solution) then
                        // Prevent circular reference.
                        ()
                    else
                        externalSolution.Solution <- ty2.Formal
                | _ ->
                    externalSolution.Solution <- ty2.Formal
                true
            
        else
            false
    else
        false

let private unifyVariadicInferenceVariable rigidity (variadicTyVar: TypeSymbol) (tyArgs: TypeSymbol imarray) : bool =
    // This handles the actual expansion of the variadic type, which is stored as a tuple type.
    if variadicTyVar.IsVariadicInferenceVariable then
        // TODO: Kind of a hack using TypeSymbol.Tuple.
        let inputTy = 
            if tyArgs.IsEmpty then
                TypeSymbol.Unit
            elif tyArgs.Length = 1 then
                tyArgs[0]
            else
                TypeSymbol.Tuple(tyArgs, ImArray.empty)

        UnifyTypes rigidity variadicTyVar inputTy
    else
        false

let private unifyVariadicTypes rigidity (tyArgs1: TypeSymbol imarray) (tyArgs2: TypeSymbol imarray) : bool =
    // This handles the actual expansion of the variadic type, which is stored as a tuple type.
    if tyArgs1.Length = 1 then
        if tyArgs1[0].IsVariadicInferenceVariable then
            unifyVariadicInferenceVariable rigidity tyArgs1[0] tyArgs2
        else
            match tyArgs1[0] with
            | TypeSymbol.HigherInferenceVariable(_, tyArgs, externalSolution, solution) ->
                // SPECIAL CASE: Handle variadic type variables with tuple solutions for higher kinds.
                // REVIEW: This is rather complicated, is there a way to simplify this?
                if externalSolution.HasSolution && 
                   solution.HasSolution &&  
                   tyArgs.Length = 1 then
                    match stripTypeEquations externalSolution.Solution with
                    | TypeSymbol.InferenceVariable(_, externalSolution) when not externalSolution.HasSolution ->
                        match stripTypeEquations solution.Solution with
                        | TypeSymbol.HigherInferenceVariable(_, tyArgs, externalSolution, solution) 
                                when not externalSolution.HasSolution && not solution.HasSolution ->
                            match tyArgs[0] with
                            | TypeSymbol.InferenceVariable(Some tyPar, innerSolution) 
                                    when tyPar.IsVariadic && innerSolution.HasSolution && innerSolution.Solution.IsAnyTuple ->
                                let isValid =
                                    if tyArgs.Length = 1 && tyArgs2.Length = 1 && 
                                       tyArgs[0].IsAnyTuple && tyArgs2[0].IsAnyTuple then
                                        (tyArgs, tyArgs2)
                                        ||> ImArray.forall2 (UnifyTypes rigidity)
                                    else
                                        false
                                if isValid then
                                    externalSolution.Solution <- Types.Tuple
                                    solution.Solution <- tyArgs2[0]
                                    true
                                else
                                    false
                            | _ ->
                                false
                        | _ ->
                            false
                    | _ ->
                        false
                else
                    false
            | _ ->
                false
    else
        false

let private unifyReturnType rigidity (returnTy1: TypeSymbol) (returnTy2: TypeSymbol) : bool =
    UnifyTypes rigidity returnTy1 returnTy2 && returnTy1.IsRealUnit = returnTy2.IsRealUnit

let UnifyTypes (rigidity: TypeVariableRigidity) (origTy1: TypeSymbol) (origTy2: TypeSymbol) : bool =
    if obj.ReferenceEquals(origTy1, origTy2) then true
    else

    let ty1 = stripTypeEquationsAndBuiltIn origTy1
    let ty2 = stripTypeEquationsAndBuiltIn origTy2

    if obj.ReferenceEquals(ty1, ty2) then true
    else

    let res =
        match ty1, ty2 with
        | TypeSymbol.BaseObject, TypeSymbol.BaseObject
        | TypeSymbol.Unit, TypeSymbol.Unit
        | TypeSymbol.Void, TypeSymbol.Void
        | TypeSymbol.Int8, TypeSymbol.Int8
        | TypeSymbol.UInt8, TypeSymbol.UInt8
        | TypeSymbol.Int16, TypeSymbol.Int16
        | TypeSymbol.UInt16, TypeSymbol.UInt16
        | TypeSymbol.Int32, TypeSymbol.Int32
        | TypeSymbol.UInt32, TypeSymbol.UInt32
        | TypeSymbol.Int64, TypeSymbol.Int64
        | TypeSymbol.UInt64, TypeSymbol.UInt64
        | TypeSymbol.Float32, TypeSymbol.Float32
        | TypeSymbol.Float64, TypeSymbol.Float64
        | TypeSymbol.Bool, TypeSymbol.Bool
        | TypeSymbol.Char16, TypeSymbol.Char16
        | TypeSymbol.Utf16, TypeSymbol.Utf16 
        | TypeSymbol.NativeInt, TypeSymbol.NativeInt
        | TypeSymbol.NativeUInt, TypeSymbol.NativeUInt -> true

        | TypeSymbol.DependentIndexer(inputValueTy1, innerTy1), TypeSymbol.DependentIndexer(inputValueTy2, innerTy2) ->
            let pass1 = UnifyTypes rigidity inputValueTy1 inputValueTy2
            let pass2 = UnifyTypes rigidity innerTy1 innerTy2
            pass1 && pass2

        | TypeSymbol.NativePtr(elementTy1), TypeSymbol.NativePtr(elementTy2) ->
            UnifyTypes rigidity elementTy1 elementTy2

        | TypeSymbol.EagerInferenceVariable(varSolution1, eagerTy1), TypeSymbol.EagerInferenceVariable(varSolution2, eagerTy2) ->
            if UnifyTypes rigidity eagerTy1 eagerTy2 then
                if (rigidity = Flexible) then
                    varSolution1.Solution <- eagerTy1
                    varSolution2.Solution <- eagerTy2
                true
            else
                false

        | TypeSymbol.EagerInferenceVariable(varSolution, eagerTy), ty 
        | ty, TypeSymbol.EagerInferenceVariable(varSolution, eagerTy) when (rigidity = Flexible) ->
            OlyAssert.True(eagerTy.IsSolved)
            OlyAssert.False(eagerTy.IsTypeVariable)
            match ty with
            | TypeSymbol.InferenceVariable(_, tySolution) ->
                tySolution.Solution <- eagerTy
                varSolution.Solution <- ty
                UnifyTypes rigidity ty1 ty2
            | _ ->
                if ty.IsAnyStruct then
                    match ty with
                    | TypeSymbol.UInt8
                    | TypeSymbol.Int8
                    | TypeSymbol.UInt16
                    | TypeSymbol.Int16
                    | TypeSymbol.UInt32
                    | TypeSymbol.Int32
                    | TypeSymbol.UInt64
                    | TypeSymbol.Int64 when eagerTy.IsFixedInteger ->
                        varSolution.Solution <- ty
                        UnifyTypes rigidity ty1 ty2
                    | TypeSymbol.Float32
                    | TypeSymbol.Float64 when eagerTy.IsReal || eagerTy.IsFixedInteger ->
                        varSolution.Solution <- ty
                        UnifyTypes rigidity ty1 ty2
                    | _ ->
                        varSolution.Solution <- eagerTy
                        UnifyTypes rigidity ty1 ty2
                else
                    if subsumesTypeWith rigidity eagerTy ty then
                        varSolution.Solution <- ty
                    else
                        varSolution.Solution <- eagerTy
                    UnifyTypes rigidity ty1 ty2

        | TypeSymbol.EagerInferenceVariable(_, eagerTy), targetTy
        | targetTy, TypeSymbol.EagerInferenceVariable(_, eagerTy) ->
            OlyAssert.True(eagerTy.IsSolved)
            OlyAssert.False(eagerTy.IsTypeVariable)
            if targetTy.IsAnyStruct && not targetTy.IsTypeVariable then
                match targetTy with
                | TypeSymbol.UInt8
                | TypeSymbol.Int8
                | TypeSymbol.UInt16
                | TypeSymbol.Int16
                | TypeSymbol.UInt32
                | TypeSymbol.Int32
                | TypeSymbol.UInt64
                | TypeSymbol.Int64 when eagerTy.IsFixedInteger ->
                    if rigidity = Rigid then
                        UnifyTypes rigidity targetTy eagerTy 
                    else
                        rigidity = IntegerGeneralizable || rigidity = NumberGeneralizable || rigidity = Generalizable
                | TypeSymbol.Float32
                | TypeSymbol.Float64 when eagerTy.IsReal || eagerTy.IsFixedInteger ->
                    if rigidity = Rigid then
                        UnifyTypes rigidity targetTy eagerTy 
                    else
                        rigidity = NumberGeneralizable || rigidity = Generalizable
                | _ ->
                    false
            elif targetTy.IsTypeVariable then
                rigidity = IntegerGeneralizable || rigidity = NumberGeneralizable || rigidity = Generalizable
            else
                subsumesTypeWith rigidity eagerTy targetTy

        | TypeSymbol.ByRef(ty1, kind1), TypeSymbol.ByRef(ty2, kind2) ->
            UnifyTypes rigidity ty1 ty2 && kind1 = kind2

        | TypeSymbol.RefCell ty1, TypeSymbol.RefCell ty2 ->
            UnifyTypes rigidity ty1 ty2

        | TypeSymbol.Variable _, TypeSymbol.InferenceVariable _
        | TypeSymbol.Variable _, TypeSymbol.HigherInferenceVariable _
        | TypeSymbol.HigherVariable _, TypeSymbol.InferenceVariable _
        | TypeSymbol.HigherVariable _, TypeSymbol.HigherInferenceVariable _
        | TypeSymbol.InferenceVariable _, TypeSymbol.Variable _
        | TypeSymbol.InferenceVariable _, TypeSymbol.HigherVariable _
        | TypeSymbol.HigherInferenceVariable _, TypeSymbol.Variable _
        | TypeSymbol.HigherInferenceVariable _, TypeSymbol.HigherVariable _ when (rigidity = Generalizable) -> true

        | TypeSymbol.Variable(tyPar1), TypeSymbol.Variable(tyPar2) when (rigidity <> Generalizable) -> 
            match rigidity with
            | Indexable
            | Flexible ->
                TypeParameterSymbol.ReasonablyEquals(tyPar1, tyPar2)
            | _ ->
                tyPar1.Id = tyPar2.Id

        | TypeSymbol.HigherVariable(tyPar1, tyArgs1), TypeSymbol.HigherVariable(tyPar2, tyArgs2) when (rigidity <> Generalizable) && tyArgs1.Length = tyArgs2.Length ->
            (
                match rigidity with
                | Indexable
                | Flexible ->
                    TypeParameterSymbol.ReasonablyEquals(tyPar1, tyPar2)
                | _ ->
                    tyPar1.Id = tyPar2.Id
            ) &&
            (
                (tyArgs1, tyArgs2)
                ||> ImArray.forall2 (fun ty1 ty2 -> UnifyTypes rigidity ty1 ty2)
            )

        | TypeSymbol.ConstantInt32 n1, TypeSymbol.ConstantInt32 n2 -> n1 = n2

        | TypeSymbol.Function(inputTy=inputTy1; returnTy=returnTy1; kind=kind1), TypeSymbol.Function(inputTy=inputTy2; returnTy=returnTy2; kind=kind2)
        | TypeSymbol.Function(inputTy=inputTy1; returnTy=returnTy1; kind=kind1), TypeSymbol.ForAll(_, TypeSymbol.Function(inputTy=inputTy2; returnTy=returnTy2; kind=kind2))
        | TypeSymbol.ForAll(_, TypeSymbol.Function(inputTy=inputTy1; returnTy=returnTy1; kind=kind1)), TypeSymbol.Function(inputTy=inputTy2; returnTy=returnTy2; kind=kind2) ->
            if kind1 = kind2 then
                let result =
                    (
                        match stripTypeEquations inputTy1, stripTypeEquations inputTy2 with
                        | TypeSymbol.Tuple(elementTys1, _), TypeSymbol.Tuple(elementTys2, _) when elementTys1.Length = elementTys2.Length ->
                            UnifyTypes rigidity inputTy1 inputTy2

                        | (TypeSymbol.Tuple(elementTys, _) as tupTy), ty
                        | ty, (TypeSymbol.Tuple(elementTys, _) as tupTy) when elementTys.Length = 1 && not ty.IsUnit_t ->
                            let elementTy = elementTys[0]
                            if (not elementTy.IsAnyTuple || elementTy.IsOneTuple) && not elementTy.IsUnit_t then
                                UnifyTypes rigidity elementTy ty
                            else
                                UnifyTypes rigidity tupTy ty

                        | TypeSymbol.Tuple _, ty
                        | ty, TypeSymbol.Tuple _ when not ty.IsSolved && not ty.IsVariadicInferenceVariable ->
                            false

                        | _ ->
                            UnifyTypes rigidity inputTy1 inputTy2
                    )
                unifyReturnType rigidity returnTy1 returnTy2 && result
            else
                false

        | TypeSymbol.NativeFunctionPtr(ilCallConv1, inputTy1, returnTy1), TypeSymbol.NativeFunctionPtr(ilCallConv2, inputTy2, returnTy2) ->
            let result =
                (
                    match stripTypeEquations inputTy1, stripTypeEquations inputTy2 with
                    | TypeSymbol.Tuple(elementTys1, _), TypeSymbol.Tuple(elementTys2, _) when elementTys1.Length = elementTys2.Length ->
                        UnifyTypes rigidity inputTy1 inputTy2

                    | (TypeSymbol.Tuple(elementTys, _) as tupTy), ty
                    | ty, (TypeSymbol.Tuple(elementTys, _) as tupTy) when elementTys.Length = 1 && not ty.IsUnit_t ->
                        let elementTy = elementTys[0]
                        if (not elementTy.IsAnyTuple || elementTy.IsOneTuple) && not elementTy.IsUnit_t then
                            UnifyTypes rigidity elementTy ty
                        else
                            UnifyTypes rigidity tupTy ty

                    | TypeSymbol.Tuple _, ty
                    | ty, TypeSymbol.Tuple _ when not ty.IsSolved && not ty.IsVariadicInferenceVariable ->
                        false

                    | _ ->
                        UnifyTypes rigidity inputTy1 inputTy2
                )
            unifyReturnType rigidity returnTy1 returnTy2 &&
            ilCallConv1 = ilCallConv2 && result

        | TypeSymbol.Tuple(tyArgs1, _), TypeSymbol.Tuple(tyArgs2, _) ->
            // This handles the actual expansion of the variadic type, which is stored as a tuple type.
            if unifyVariadicTypes rigidity tyArgs1 tyArgs2 then
                true
            elif unifyVariadicTypes rigidity tyArgs2 tyArgs1 then
                true
            elif tyArgs1.Length <> tyArgs2.Length then
                false
            else
                (tyArgs1, tyArgs2)
                ||> ImArray.forall2 (fun ty1 ty2 -> UnifyTypes rigidity ty1 ty2)

        | TypeSymbol.ForAll(tyPars=tyPars1; innerTy=innerTy1), TypeSymbol.ForAll(tyPars=tyPars2; innerTy=innerTy2) ->
            if tyPars1.Length = tyPars2.Length then
                (
                    (tyPars1, tyPars2)
                    ||> ImArray.forall2 (fun tyPar1 tyPar2 -> areTypeParameterSignaturesAndConstraintsEqual tyPar1 tyPar2)
                ) && UnifyTypes rigidity innerTy1 innerTy2
            else
                false

        | TypeSymbol.Array(elementTy1, rank1, kind1), TypeSymbol.Array(elementTy2, rank2, kind2) ->
            UnifyTypes rigidity elementTy1 elementTy2 &&
            rank1 = rank2 &&
            kind1 = kind2

        | TypeSymbol.FixedArray(elementTy1, lengthTy1, kind1), TypeSymbol.FixedArray(elementTy2, lengthTy2, kind2) ->
            UnifyTypes rigidity lengthTy1 lengthTy2 &&
            UnifyTypes rigidity elementTy1 elementTy2 &&
            kind1 = kind2

        | TypeSymbol.Entity(ent1), TypeSymbol.Entity(ent2) ->
            ent1.FormalId = ent2.FormalId && ent1.TypeArguments.Length = ent2.TypeArguments.Length &&
            (ent1.TypeArguments, ent2.TypeArguments) 
            ||> ImArray.forall2 (fun ty1 ty2 -> 
                UnifyTypes rigidity ty1 ty2)

        | TypeSymbol.Entity(ent), _ when ent.IsClosure ->
            UnifyTypes rigidity ent.TryClosureInvoke.Value.Type ty2
        | _, TypeSymbol.Entity(ent) when ent.IsClosure ->
            UnifyTypes rigidity ty1 ent.TryClosureInvoke.Value.Type

        | TypeSymbol.InferenceVariable(tyParOpt, solution), _ when (rigidity = Flexible) && not solution.HasSolution ->
            match tyParOpt with
            | Some(tyPar) when tyPar.Arity > 0 ->
                solution.Solution <- ty2.Formal
            | _ ->
                solution.Solution <- ty2
            true

        | _, TypeSymbol.InferenceVariable(tyParOpt, solution) when (rigidity = Flexible) && not solution.HasSolution ->
            match tyParOpt with
            | Some(tyPar) when tyPar.Arity > 0 ->
                solution.Solution <- ty1.Formal
            | _ ->
                solution.Solution <- ty1
            true

        | TypeSymbol.HigherInferenceVariable(_, tyArgs, externalSolution, solution), ty
        | ty, TypeSymbol.HigherInferenceVariable(_, tyArgs, externalSolution, solution) when (rigidity = Flexible) && (not solution.HasSolution) && tyArgs.Length = ty.LogicalArity ->
            solveHigherInferenceVariable rigidity tyArgs externalSolution solution ty

        | TypeSymbol.Variable(tyPar1), TypeSymbol.HigherVariable(tyPar2, _) when (rigidity <> Generalizable) ->
            tyPar1.Index = tyPar2.Index && tyPar1.Arity = tyPar2.Arity
        | TypeSymbol.HigherVariable(tyPar1, _), TypeSymbol.Variable(tyPar2) when (rigidity <> Generalizable) ->
            tyPar1.Index = tyPar2.Index && tyPar1.Arity = tyPar2.Arity

        // Error types will always succeed.
        | TypeSymbol.Error _, _ 
        | _, TypeSymbol.Error _ -> true

        | TypeSymbol.Variable _, _
        | TypeSymbol.HigherVariable _, _
        | _, TypeSymbol.Variable _
        | _, TypeSymbol.HigherVariable _ ->
            (rigidity = Generalizable)

        | TypeSymbol.InferenceVariable(Some tyPar1, varSolution1), TypeSymbol.InferenceVariable(Some tyPar2, varSolution2) when (rigidity = Rigid) && not varSolution1.HasSolution && not varSolution2.HasSolution ->
            tyPar1.Index = tyPar2.Index

        | TypeSymbol.HigherInferenceVariable(Some tyPar1, tyArgs1, _, varSolution1), TypeSymbol.HigherInferenceVariable(Some tyPar2, tyArgs2, _, varSolution2) when (rigidity = Rigid) && not varSolution1.HasSolution && not varSolution2.HasSolution ->
            tyPar1.Index = tyPar2.Index &&
            tyArgs1.Length = tyArgs2.Length &&
            (
                (tyArgs1, tyArgs2)
                ||> ImArray.forall2 areTypesEqual
            )

        | TypeSymbol.InferenceVariable(solution=solution), _
        | _, TypeSymbol.InferenceVariable(solution=solution) ->
            rigidity = Generalizable && not solution.HasSolution

        | TypeSymbol.HigherInferenceVariable(solution=solution), _
        | _, TypeSymbol.HigherInferenceVariable(solution=solution) ->
            rigidity = Generalizable && not solution.HasSolution

        | _ ->
            if origTy1.IsRealUnit then
                origTy2.IsRealUnit
            else
                false
    res

// **********************************************************************

type VariableSolutionSymbol with

    member this.AddConstraint (constr: ConstraintSymbol) =
        if not this.HasSolution && this.Constraints |> Seq.exists (areConstraintsEqual constr) |> not then
            this.ForceAddConstraint constr

let areEnclosingsEqual (enclosing1: EnclosingSymbol) (enclosing2: EnclosingSymbol) =
    if obj.ReferenceEquals(enclosing1, enclosing2) then true
    else
        match enclosing1, enclosing2 with
        | EnclosingSymbol.RootNamespace, EnclosingSymbol.RootNamespace
        | EnclosingSymbol.Local, EnclosingSymbol.Local -> true
        | EnclosingSymbol.Entity(ent1), EnclosingSymbol.Entity(ent2) -> areEntitiesEqual ent1 ent2
        | _ -> false

let areConstraintsEqualWith rigidity (constr1: ConstraintSymbol) (constr2: ConstraintSymbol) : bool =
    if obj.ReferenceEquals(constr1, constr2) then true
    else
        match constr1, constr2 with
        | ConstraintSymbol.NotStruct, ConstraintSymbol.NotStruct -> true
        | ConstraintSymbol.SubtypeOf(ty1), ConstraintSymbol.SubtypeOf(ty2)
        | ConstraintSymbol.TraitType(ty1), ConstraintSymbol.TraitType(ty2) -> 
            let ty1 = ty1.Value
            let ty2 = ty2.Value
            if ty1.IsShape && ty2.IsShape then
                areShapesEqualWith rigidity ty1 ty2
            elif ty1.IsShape || ty2.IsShape then
                false
            else
                areTypesEqualWithRigidity rigidity ty1 ty2
        | _ -> false

let areConstraintsEqual (constr1: ConstraintSymbol) (constr2: ConstraintSymbol) : bool =
    areConstraintsEqualWith Rigid constr1 constr2

let areConstantsEqual (cns1: ConstantSymbol) (cns2: ConstantSymbol) : bool =
    if obj.ReferenceEquals(cns1, cns2) then true
    else
        match cns1, cns2 with
        | ConstantSymbol.UInt8(value1), 
          ConstantSymbol.UInt8(value2) -> value1 = value2
        | ConstantSymbol.UInt16(value1), 
          ConstantSymbol.UInt16(value2) -> value1 = value2
        | ConstantSymbol.UInt32(value1), 
          ConstantSymbol.UInt32(value2) -> value1 = value2
        | ConstantSymbol.UInt64(value1), 
          ConstantSymbol.UInt64(value2) -> value1 = value2
        | ConstantSymbol.Int8(value1), 
          ConstantSymbol.Int8(value2) -> value1 = value2
        | ConstantSymbol.Int16(value1), 
          ConstantSymbol.Int16(value2) -> value1 = value2
        | ConstantSymbol.Int32(value1), 
          ConstantSymbol.Int32(value2) -> value1 = value2
        | ConstantSymbol.Int64(value1), 
          ConstantSymbol.Int64(value2) -> value1 = value2
        | ConstantSymbol.Float32(value1), 
          ConstantSymbol.Float32(value2) -> value1 = value2
        | ConstantSymbol.Float64(value1), 
          ConstantSymbol.Float64(value2) -> value1 = value2
        | ConstantSymbol.Char16(value1), 
          ConstantSymbol.Char16(value2) -> value1 = value2
        | ConstantSymbol.Utf16(value1), 
          ConstantSymbol.Utf16(value2) -> value1 = value2
        | ConstantSymbol.True, 
          ConstantSymbol.True -> true
        | ConstantSymbol.False, 
          ConstantSymbol.False -> true
        | ConstantSymbol.TypeVariable(tyPar1), 
          ConstantSymbol.TypeVariable(tyPar2) -> areTypeParameterSignaturesAndConstraintsEqual tyPar1 tyPar2
        | ConstantSymbol.External(func1),
          ConstantSymbol.External(func2) -> areValueSignaturesEqual func1 func2
        | ConstantSymbol.Array(elementTy1, elements1),
          ConstantSymbol.Array(elementTy2, elements2) when elements1.Length = elements2.Length ->
          areTypesEqual elementTy1 elementTy2 &&
          (
            (elements1, elements2)
            ||> ImArray.forall2 areConstantsEqual
          )
        | _ ->
            false

let areManyConstraintsEqual (constrs1: ConstraintSymbol seq) (constrs2: ConstraintSymbol seq) =
    if obj.ReferenceEquals(constrs1, constrs2) then true
    else
        // TODO: This is dependent on the order of the constraints which may not be what we want.
        if Seq.length constrs1 = Seq.length constrs2 then
            (constrs1, constrs2)
            ||> Seq.forall2 areConstraintsEqual
        else
            false

let areFunctionTypeParameterConstraintsEqualWith rigidity (func1: IFunctionSymbol) (func2: IFunctionSymbol) =
    (func1.TypeParameters, func2.TypeParameters)
    ||> ImArray.forall2 (fun tyPar1 tyPar2 ->
        if tyPar1.Constraints.Length = tyPar2.Constraints.Length then
            (tyPar1.Constraints, tyPar2.Constraints)
            ||> ImArray.forall2 (fun constr1 constr2 ->
                areConstraintsEqualWith rigidity constr1 constr2
            )
        else
            false
    )

let areTypeParametersEqual (tyPar1: TypeParameterSymbol) (tyPar2: TypeParameterSymbol) =
    if obj.ReferenceEquals(tyPar1, tyPar2) then true
    else
        tyPar1.Index = tyPar2.Index &&
        tyPar1.Name = tyPar2.Name &&
        tyPar1.Arity = tyPar2.Arity &&
        tyPar1.IsVariadic = tyPar2.IsVariadic &&
        areManyConstraintsEqual tyPar1.Constraints tyPar2.Constraints

let areTypeParameterSignaturesAndConstraintsEqual (tyPar1: TypeParameterSymbol) (tyPar2: TypeParameterSymbol) =
    if obj.ReferenceEquals(tyPar1, tyPar2) then true
    else
        tyPar1.Arity = tyPar2.Arity &&
        areManyConstraintsEqual tyPar1.Constraints tyPar2.Constraints

let areManyTypeParameterSignaturesAndConstraintsEqual (tyPars1: ImmutableArray<TypeParameterSymbol>) (tyPars2: ImmutableArray<TypeParameterSymbol>) =
    if obj.ReferenceEquals(tyPars1, tyPars2) then true
    else
        tyPars1.Length = tyPars2.Length &&
        (
            (tyPars1, tyPars2)
            ||> Seq.forall2 (fun tyPar1 tyPar2 ->
                areTypeParameterSignaturesAndConstraintsEqual tyPar1 tyPar2
            )
        )

let areWitnessesEqual (witness1: WitnessSymbol) (witness2: WitnessSymbol) =
    match witness1, witness2 with
    | WitnessSymbol.TypeParameter(tyPar1), WitnessSymbol.TypeParameter(tyPar2) ->
        areTypeParametersEqual tyPar1 tyPar2
    | WitnessSymbol.TypeExtension(tyExt1, funcOpt1), WitnessSymbol.TypeExtension(tyExt2, funcOpt2) ->
        areEntitiesEqual tyExt1 tyExt2 &&
        (
            match funcOpt1, funcOpt2 with
            | Some func1, Some func2 -> areLogicalFunctionSignaturesEqual func1 func2
            | None, None -> true
            | _ -> false
        )
    | WitnessSymbol.Type(ty1), WitnessSymbol.Type(ty2) ->
        areTypesEqual ty1 ty2
    | _ ->
        false

let areEntitiesEqual (ent1: EntitySymbol) (ent2: EntitySymbol) =
    if obj.ReferenceEquals(ent1, ent2) then true
    else
        ent1.Kind = ent2.Kind &&
        ent1.FormalId = ent2.FormalId &&
        areEnclosingsEqual ent1.Enclosing ent2.Enclosing &&
        ent1.TypeParameters.Length = ent2.TypeParameters.Length &&
        (
            (ent1.TypeParameters, ent2.TypeParameters)
            ||> Seq.forall2 (fun tyPar1 tyPar2 ->
                areTypeParametersEqual tyPar1 tyPar2
            )
        ) &&
        (
            if ent1.TypeArguments.Length = ent2.TypeArguments.Length then
                (ent1.TypeArguments, ent2.TypeArguments)
                ||> Seq.forall2 (fun ty1 ty2 ->
                    areTypesEqual ty1 ty2
                )
            else
                false
        )

let areNamespacesEqual (nmspace1: INamespaceSymbol) (nmspace2: INamespaceSymbol) =
    OlyAssert.True(nmspace1.IsNamespace)
    OlyAssert.True(nmspace2.IsNamespace)

    if nmspace1.Name = nmspace2.Name then
        match nmspace1.Enclosing, nmspace2.Enclosing with
        | EnclosingSymbol.RootNamespace, EnclosingSymbol.RootNamespace ->
            true
        | EnclosingSymbol.Entity(ent1), EnclosingSymbol.Entity(ent2) ->
            areNamespacesEqual ent1 ent2
        | _ ->
            false
    else
        false

let areGeneralizedEntitiesEqual (ent1: EntitySymbol) (ent2: EntitySymbol) =
    if obj.ReferenceEquals(ent1, ent2) then true
    else
        ent1.Kind = ent2.Kind &&
        ent1.FormalId = ent2.FormalId &&
        areEnclosingsEqual ent1.Enclosing ent2.Enclosing &&
        ent1.TypeParameters.Length = ent2.TypeParameters.Length &&
        (
            (ent1.TypeParameters, ent2.TypeParameters)
            ||> Seq.forall2 (fun tyPar1 tyPar2 ->
                areTypeParametersEqual tyPar1 tyPar2
            )
        ) &&
        (
            if ent1.TypeArguments.Length = ent2.TypeArguments.Length then
                (ent1.TypeArguments, ent2.TypeArguments)
                ||> Seq.forall2 (fun ty1 ty2 ->
                    areGeneralizedTypesEqual ty1 ty2
                )
            else
                false
        )

let areTypesEqualWithRigidity rigidity ty1 ty2 =
    UnifyTypes rigidity ty1 ty2

let areTypesEqual (ty1: TypeSymbol) (ty2: TypeSymbol) =
    areTypesEqualWithRigidity Rigid ty1 ty2

let areGeneralizedTypesEqual (ty1: TypeSymbol) (ty2: TypeSymbol) =
    areTypesEqualWithRigidity Generalizable ty1 ty2

/// Type variable checks are indexable
let areLogicalConstructorSignaturesEqual (func1: IFunctionSymbol) (func2: IFunctionSymbol) =
    // IMPORTANT: If either function is a group, then return false. No such thing as equality for function groups.
    if func1.IsFunctionGroup || func2.IsFunctionGroup then
        false
    elif not func1.IsConstructor || not func2.IsConstructor then false
    elif obj.ReferenceEquals(func1, func2) then true
    else
        func1.LogicalParameterCount = func2.LogicalParameterCount &&
        func1.TypeParameters.Length = func2.TypeParameters.Length &&
        (
            (func1.TypeArguments, func2.TypeArguments)
            ||> ImArray.forall2 (UnifyTypes TypeVariableRigidity.Indexable)
        ) &&
        (
            (func1.LogicalParameters, func2.LogicalParameters)
            ||> ROMem.forall2 (fun par1 par2 ->
                UnifyTypes TypeVariableRigidity.Indexable par1.Type par2.Type
            )
        ) && not func1.IsFunctionGroup && not func2.IsFunctionGroup

/// Type variable checks are indexable
let inline areLogicalFunctionSignaturesParameterOnlyEqualAux (func1: IFunctionSymbol) (func2: IFunctionSymbol) ([<InlineIfLambda>] f) : bool =
    // IMPORTANT: If either function is a group, then return false. No such thing as equality for function groups.
    if func1.IsFunctionGroup || func2.IsFunctionGroup then
        false
    elif obj.ReferenceEquals(func1, func2) then true
    elif func1.IsConstructor || func2.IsConstructor then
        areLogicalConstructorSignaturesEqual func1 func2
    else
        func1.Name = func2.Name &&
        func1.LogicalParameterCount = func2.LogicalParameterCount &&
        func1.TypeParameters.Length = func2.TypeParameters.Length &&
        (
            (func1.TypeArguments, func2.TypeArguments)
            ||> ImArray.forall2 (UnifyTypes TypeVariableRigidity.Indexable)
        ) &&
        (
            (func1.LogicalParameters, func2.LogicalParameters)
            ||> ROMem.forall2 (fun par1 par2 ->
                UnifyTypes TypeVariableRigidity.Indexable par1.Type par2.Type
            )
        ) && f func1 func2

let areLogicalFunctionSignaturesParameterOnlyEqual (func1: IFunctionSymbol) (func2: IFunctionSymbol) : bool =
    areLogicalFunctionSignaturesParameterOnlyEqualAux func1 func2 (fun _ _ -> true)

/// Type variable checks are indexable
let areLogicalFunctionSignaturesEqual (func1: IFunctionSymbol) (func2: IFunctionSymbol) =
    areLogicalFunctionSignaturesParameterOnlyEqualAux func1 func2
        (fun func1 func2 ->
            UnifyTypes TypeVariableRigidity.Indexable func1.ReturnType func2.ReturnType
        )

let areFieldSignaturesEqual (field1: IFieldSymbol) (field2: IFieldSymbol) =
    if obj.ReferenceEquals(field1, field2) then true
    else
        field1.Name = field2.Name && areTypesEqual field1.Type field2.Type

let areFieldsEqual (field1: IFieldSymbol) (field2: IFieldSymbol) =
    areFieldSignaturesEqual field1 field2 &&
    field1.MemberFlags = field2.MemberFlags &&
    field1.ValueFlags = field2.ValueFlags &&
    areEnclosingsEqual field1.Enclosing field2.Enclosing

let areValueSignaturesEqual (value1: IValueSymbol) (value2: IValueSymbol) =
    // IMPORTANT: If either value is a function group, then return false. No such thing as equality for function groups.
    if value1.IsFunctionGroup || value2.IsFunctionGroup then
        false
    elif obj.ReferenceEquals(value1, value2) then true
    else
        match value1, value2 with
        | (:? IFunctionSymbol as func1), (:? IFunctionSymbol as func2) ->
            value1.Name = value2.Name && areLogicalFunctionSignaturesEqual func1 func2

        | (:? IFieldSymbol as field1), (:? IFieldSymbol as field2) ->
            value1.Name = value2.Name && areFieldSignaturesEqual field1 field2

        | _ ->
            false

// TODO: Practically identical to 'subsumesShapeMembersWith'. There a way to re-use logic between them?
let areShapesEqualWith rigidity (ty1: TypeSymbol) (ty2: TypeSymbol) =
    OlyAssert.True(ty1.IsShape)
    OlyAssert.True(ty2.IsShape)

    let funcs1: IFunctionSymbol imarray = ty1.Functions
    let funcs2: IFunctionSymbol imarray = ty2.Functions

    if funcs1.Length = funcs2.Length then
        funcs1
        |> ImArray.forall (fun superFunc ->
            funcs2
            |> ImArray.exists (fun func ->
                if func.IsInstance = superFunc.IsInstance && func.Name = superFunc.Name && func.TypeArguments.Length = superFunc.TypeArguments.Length && func.Parameters.Length = superFunc.Parameters.Length then
                        // TODO: This really isn't right.
                        let isInstance = func.IsInstance
                        if not isInstance || not ty2.IsAnyStruct || (if superFunc.IsImmutable then func.IsImmutable else true) then
                            let result =
                                (superFunc.Parameters, func.Parameters)
                                ||> ImArray.foralli2 (fun i par1 par2 ->
                                    if i = 0 && isInstance then
                                        // We return true here because the first parameter of an instance member function is the 'shape' entity,
                                        // which cannot be unified.
                                        true
                                    else
                                        UnifyTypes rigidity par1.Type par2.Type
                                ) &&
                                UnifyTypes rigidity superFunc.ReturnType func.ReturnType
                            if not result then
                                areLogicalFunctionSignaturesEqual superFunc func &&
                                areFunctionTypeParameterConstraintsEqualWith Indexable superFunc.Formal.AsFunction func.Formal.AsFunction
                            else
                                areFunctionTypeParameterConstraintsEqualWith Indexable superFunc.Formal.AsFunction func.Formal.AsFunction
                        else
                            false
                    else
                        false
            )
        )
    else
        false

[<Struct;NoComparison;NoEquality>]
type TypeSymbolGeneralizedMap<'T> private (map: ImmutableDictionary<TypeSymbol, 'T>) =

    member this.SetItem(key: TypeSymbol, value) =
        if key.IsError_t then
            this
        else
            TypeSymbolGeneralizedMap(map.SetItem(key, value))

    member _.TryFind(key) =
        match map.TryGetValue key with
        | true, value -> ValueSome value
        | _ -> ValueNone

    member _.Values = map.Values
   
    static member Create() =     
        let map = ImmutableDictionary.Create<TypeSymbol, 'T>(TypeSymbolGeneralizedComparer())
        TypeSymbolGeneralizedMap(map)

[<Struct;NoComparison;NoEquality>]
type TypeSymbolMap<'T> private (map: ImmutableDictionary<TypeSymbol, 'T>) =

    member this.SetItem(key: TypeSymbol, value) =
        if key.IsError_t then
            this
        else
            TypeSymbolMap(map.SetItem(key, value))

    member _.TryFind(key) =
        match map.TryGetValue key with
        | true, value -> ValueSome value
        | _ -> ValueNone

    member _.Values = map.Values
   
    static member Create() =     
        let map = ImmutableDictionary.Create<TypeSymbol, 'T>(TypeSymbolComparer())
        TypeSymbolMap(map)

[<Struct;NoComparison;NoEquality>]
type EntitySymbolGeneralizedMap<'T> private (map: ImmutableDictionary<EntitySymbol, 'T>) =

    member _.Add(key, value) =
        EntitySymbolGeneralizedMap(map.Add(key, value))

    member _.SetItem(key, value) =
        EntitySymbolGeneralizedMap(map.SetItem(key, value))

    member _.TryFind(key) =
        match map.TryGetValue key with
        | true, value -> ValueSome value
        | _ -> ValueNone

    member _.ContainsKey(key) =
        map.ContainsKey(key)

    member _.GetSimilar(tr: EntitySymbol) =
        map
        |> Seq.choose (fun pair ->
            if areGeneralizedEntitiesEqual tr pair.Key then
                Some pair.Value
            else
                None
        )

    member _.GetSimilar(ty: TypeSymbol) =
        map
        |> Seq.choose (fun pair ->
            if areGeneralizedTypesEqual ty pair.Key.AsType then
                Some pair.Value
            else
                None
        )

    member _.Values = map.Values
   
    static member Create() =     
        let map = ImmutableDictionary.Create<EntitySymbol, 'T>(EntitySymbolGeneralizedComparer())
        EntitySymbolGeneralizedMap(map)

[<Struct;NoComparison;NoEquality>]
type EntitySymbolGeneralizedMapEntitySet private (map: ImmutableDictionary<EntitySymbol, EntitySymbolSet>) =

    member _.SetItem(key, value) =
        let entSet =
            match map.TryGetValue key with
            | true, entSet -> entSet
            | _ -> EntitySymbolSet.Create(Seq.empty)
        let entSet = entSet.Add(value)
        EntitySymbolGeneralizedMapEntitySet(map.SetItem(key, entSet))

    member _.TryFind(key) =
        match map.TryGetValue key with
        | true, value -> ValueSome value
        | _ -> ValueNone

    member _.ContainsKey(key) =
        map.ContainsKey(key)

    member _.GetSimilar(tr: EntitySymbol) =
        map
        |> Seq.choose (fun pair ->
            if areGeneralizedEntitiesEqual tr pair.Key then
                Some pair.Value
            else
                None
        )

    member _.GetSimilar(ty: TypeSymbol) =
        map
        |> Seq.choose (fun pair ->
            if areGeneralizedTypesEqual ty pair.Key.AsType then
                Some pair.Value
            else
                None
        )

    member _.Values = map.Values
   
    static member Create() =     
        let map = ImmutableDictionary.Create<EntitySymbol, EntitySymbolSet>(EntitySymbolGeneralizedComparer())
        EntitySymbolGeneralizedMapEntitySet(map)

[<Struct;NoComparison;NoEquality>]
type EntitySymbolSet private (set: ImmutableHashSet<EntitySymbol>) =

    member _.Add(key) =
        EntitySymbolSet(set.Add(key))

    member _.TryFind(key) =
        match set.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    member _.Values = set :> _ seq

    member _.Count = set.Count
   
    static member Create(trs: EntitySymbol seq) =     
        let set = ImmutableHashSet.CreateRange<EntitySymbol>(EntitySymbolComparer(), trs)
        EntitySymbolSet(set)

[<Struct;NoComparison;NoEquality>]
type TypeSymbolGeneralizedSet private (set: ImmutableHashSet<TypeSymbol>) =

    member _.Add(key) =
        TypeSymbolGeneralizedSet(set.Add(key))

    member _.TryFind(key) =
        match set.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    member _.Values = set :> _ seq
   
    static member Create(tys) =     
        let set = ImmutableHashSet.CreateRange<TypeSymbol>(TypeSymbolGeneralizedComparer(), tys)
        TypeSymbolGeneralizedSet(set)

[<Struct;NoComparison;NoEquality>]
type ValueSymbolMap<'T> private (map: ImmutableDictionary<IValueSymbol, 'T>) =

    member _.Add(key, value) =
        ValueSymbolMap(map.Add(key, value))

    member _.SetItem(key, value) =
        ValueSymbolMap(map.SetItem(key, value))

    member _.TryFind(key) =
        match map.TryGetValue key with
        | true, value -> Some value
        | _ -> None
   
    static member Create() =     
        let map = ImmutableDictionary.Create<IValueSymbol, 'T>(ValueSymbolComparer())
        ValueSymbolMap(map)

[<Struct;NoComparison;NoEquality>]
type TypeSymbolMutableSet private (set: HashSet<TypeSymbol>) =

    member _.Add(key) =
        set.Add(key)

    member _.TryGet(key) =
        match set.TryGetValue key with
        | true, value -> ValueSome value
        | _ -> ValueNone

    member _.Contains(key) =
        set.Contains(key)

    static member Create() =
        let set = HashSet<TypeSymbol>(TypeSymbolComparer())
        TypeSymbolMutableSet(set)

[<Struct;NoComparison;NoEquality>]
type TypeSymbolGeneralizedMutableSet private (set: HashSet<TypeSymbol>) =

    member _.Add(key) =
        set.Add(key)

    member _.TryGet(key) =
        match set.TryGetValue key with
        | true, value -> ValueSome value
        | _ -> ValueNone

    member _.Contains(key) =
        set.Contains(key)

    static member Create() =
        let set = HashSet<TypeSymbol>(TypeSymbolGeneralizedComparer())
        TypeSymbolGeneralizedMutableSet(set)

[<Struct;NoComparison;NoEquality>]
type FunctionSignatureMutableSet private (set: HashSet<IFunctionSymbol>) =

    member _.Add(key) =
        set.Add(key)

    member _.TryGet(key) =
        match set.TryGetValue key with
        | true, value -> ValueSome value
        | _ -> ValueNone

    member _.Contains(key) =
        set.Contains(key)
   
    static member Create(collection: IEnumerable<_>) =     
        let set = HashSet<IFunctionSymbol>(collection, FunctionSignatureSymbolComparer())
        FunctionSignatureMutableSet(set)

[<Struct;NoComparison;NoEquality>]
type TypeParameterSymbolMap<'T> private (map: ImmutableDictionary<TypeParameterSymbol, 'T>) =

    member _.Add(key, value) =
        TypeParameterSymbolMap(map.Add(key, value))

    member _.SetItem(key, value) =
        TypeParameterSymbolMap(map.SetItem(key, value))

    member _.TryFind(key) =
        match map.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    member this.Find(key) =
        match this.TryFind(key) with
        | Some value -> value
        | _ -> failwith "Internal error: Unable to find type parameter symbol."
   
    static member Create() =     
        let map = ImmutableDictionary.Create<TypeParameterSymbol, 'T>(TypeParameterSymbolComparer())
        TypeParameterSymbolMap(map)

    static member Create(map: ImmutableDictionary<TypeParameterSymbol, 'T>) =
        TypeParameterSymbolMap(map)

[<Struct;NoComparison;NoEquality>]
type ExtensionMemberSymbolOrderedSet private (tyExts: ExtensionMemberSymbol imarray) =

    member _.Values = tyExts

    member _.Add(value: ExtensionMemberSymbol) =
        let tyExts2 = tyExts.RemoveAll(Predicate(fun value2 -> value.FormalId = value2.FormalId)).Add(value)
        ExtensionMemberSymbolOrderedSet(tyExts2)

    member _.AddRange(values: ExtensionMemberSymbol imarray) =
        let tyExts2 = tyExts.RemoveAll(Predicate(fun value2 -> values |> ImArray.exists (fun value1 -> value1.FormalId = value2.FormalId))).AddRange(values)
        ExtensionMemberSymbolOrderedSet(tyExts2)

    static member Create() =
        ExtensionMemberSymbolOrderedSet(ImArray.empty)

// **********************************************************************

type NameMap<'T> = System.Collections.Immutable.ImmutableDictionary<string, 'T>

module NameMap =

    let empty<'T> = NameMap<'T>.Empty

    let ofSeq<'T> values : NameMap<'T> = System.Collections.Immutable.ImmutableDictionary.CreateRange(values)

type MultiNameMap<'T> = System.Collections.Immutable.ImmutableDictionary<System.Collections.Immutable.ImmutableArray<string>, 'T>

module MultiNameMap =

    let emptyWithComparer<'T> : MultiNameMap<'T> = System.Collections.Immutable.ImmutableDictionary.Create(SymbolComparers.MultiStringComparer()) 

    let empty<'T> = MultiNameMap<'T>.Empty

    let ofSeq<'T> values : MultiNameMap<'T> = System.Collections.Immutable.ImmutableDictionary.CreateRange(values)

type IntMap<'T> = System.Collections.Immutable.ImmutableDictionary<int, 'T>

module IntMap =

    let empty<'T> = IntMap<'T>.Empty

    let ofSeq<'T> values : IntMap<'T> = System.Collections.Immutable.ImmutableDictionary.CreateRange(values)

type IntIntMap<'T> = System.Collections.Immutable.ImmutableDictionary<int * int, 'T>

module IntIntMap =

    let empty<'T> = IntIntMap<'T>.Empty

    let ofSeq<'T> values : IntIntMap<'T> = System.Collections.Immutable.ImmutableDictionary.CreateRange(values)

type IdMap<'T> = System.Collections.Immutable.ImmutableDictionary<int64, 'T>

module IdMap =

    let empty<'T> = IdMap<'T>.Empty

    let ofSeq<'T> values : IdMap<'T> = System.Collections.Immutable.ImmutableDictionary.CreateRange(values)

// TODO: This is a little weird, why even have UnqualifiedSymbol.FunctionGroup when IValueSymbol can be a FunctionGroupSymbol?
[<RequireQualifiedAccess;ReferenceEquality;NoComparison>]
type UnqualifiedSymbol =
    | FunctionGroup of FunctionGroupSymbol
    | Function of IFunctionSymbol
    | Local of ILocalSymbol
    | Field of IFieldSymbol
    | Property of IPropertySymbol
    | AmbiguousValues of IValueSymbol imarray

type IValueSymbol with

    member this.ToUnqualified() =
        match this with
        | :? FunctionGroupSymbol as x -> UnqualifiedSymbol.FunctionGroup(x)
        | :? IFunctionSymbol as x -> UnqualifiedSymbol.Function(x)
        | :? ILocalSymbol as x -> UnqualifiedSymbol.Local(x)
        | :? IFieldSymbol as x -> UnqualifiedSymbol.Field(x)
        | :? IPropertySymbol as x -> UnqualifiedSymbol.Property(x)
        | _ ->
            failwith "Invalid value symbol."

type TypeSymbol with

        static member Distinct(tys: TypeSymbol seq) =
            HashSet(tys, TypeSymbolComparer()) :> _ seq

        static member DistinctByGeneralization(tys: TypeSymbol seq) =
            HashSet(tys, TypeSymbolGeneralizedComparer()) :> _ seq

        member this.LogicalTypeArguments : TypeSymbol imarray =
            match this with
            | TypeSymbol.Entity(ent) -> ent.LogicalTypeArguments
            | _ -> this.TypeArguments

        /// Returns the type parameters of the entity but excludes its enclosing's type parameters.
        /// Useful when dealing with nested types.
        member this.LogicalTypeParameters =
            let enclosingTyPars = this.Enclosing.TypeParameters
            if enclosingTyPars.IsEmpty then
                this.TypeParameters
            else
                this.TypeParameters
                |> Seq.skip enclosingTyPars.Length
                |> ImArray.ofSeq

        member this.LogicalTypeParameterCount =
            let enclosingTyPars = this.Enclosing.TypeParameters
            if enclosingTyPars.IsEmpty then
                this.TypeParameters.Length
            else
                this.TypeParameters.Length - enclosingTyPars.Length

        member this.IsBaseObject_t =
            match stripTypeEquationsAndBuiltIn this with
            | TypeSymbol.BaseObject -> true
            | _ -> false

        member this.Hierarchy(f: TypeSymbol -> bool, set: HashSet<TypeSymbol>): unit =
            let ty = stripTypeEquationsAndBuiltIn this
            match ty.TryEntity with
            | ValueSome ent -> ent.Hierarchy(f, set)
            | _ ->
                match ty.TryTypeParameter with
                | ValueSome tyPar ->
                    let mutable cont = true

                    let mutable i = 0
                    while (cont && i < tyPar.Constraints.Length) do
                        match tyPar.Constraints[i] with
                        | ConstraintSymbol.SubtypeOf(ty)
                        | ConstraintSymbol.TraitType(ty) ->
                            let ty = ty.Value
                            if set.Add(ty) then
                                cont <- f ty
                        | _ ->
                            ()
                        i <- i + 1
                | _ ->
                    ()

        member this.Hierarchy(f: TypeSymbol -> bool): unit =
            let set = HashSet(SymbolComparers.TypeSymbolComparer())
            this.Hierarchy(f, set)

        member this.HierarchyExists(f: TypeSymbol -> bool): bool =
            let ty = stripTypeEquationsAndBuiltIn this
            match ty.TryEntity with
            | ValueSome ent -> ent.HierarchyExists(f)
            | _ ->
                let mutable exists = false
                this.Hierarchy(fun x ->
                    exists <- f x
                    not exists
                )
                exists

        member this.FlattenHierarchy(): TypeSymbol imarray =
            let builder = ImArray.builder()
            this.Hierarchy(fun x -> builder.Add(x); true)
            builder.ToImmutable()

        member this.FlattenHierarchyIncluding(): TypeSymbol imarray =
            let builder = ImArray.builder()
            builder.Add(this)
            this.Hierarchy(fun x -> builder.Add(x); true)
            builder.ToImmutable()

        member this.HierarchyIncluding(f: TypeSymbol -> bool): unit =
            if f(this) then
                let set = HashSet(SymbolComparers.TypeSymbolComparer())
                this.Hierarchy(f, set)

        // TODO: Remove this.
        member this.AllLogicalInheritsAndImplements: _ imarray =
            this.FlattenHierarchy()

        member this.AllLogicalImplements =
            match this.TryEntity with
            | ValueSome ent -> ent.AllLogicalImplements
            // TODO: type parameters?
            | _ -> ImArray.empty

        member this.AllLogicalInherits =
            match this.TryEntity with
            | ValueSome ent -> ent.AllLogicalInherits
            // TODO: type parameters?
            | _ -> ImArray.empty

        member this.AllInherits =
            match this.TryEntity with
            | ValueSome ent -> ent.AllInherits
            // TODO: type parameters?
            | _ -> ImArray.empty

        member this.AllImplements =
            match this.TryEntity with
            | ValueSome ent -> ent.AllImplements
            // TODO: type parameters?
            | _ -> ImArray.empty

        /// Immediate functions directly on type. Does not include inherited/implemeted types' functions.
        member this.Functions: _ imarray =
            match stripTypeEquations this with
            | TypeSymbol.Entity(ent) -> ent.Functions
            | _ -> ImArray.empty

        /// Immediate fields. Does not include fields from inherited/implemented types.
        member this.Fields =
            match stripTypeEquations this with
            | TypeSymbol.Entity(ent) -> ent.Fields
            // TODO: Type parameter?
            | _ -> ImArray.empty

        /// Immediate properties. Does not include properties from inherited/implemented types.
        member this.Properties =
            match stripTypeEquations this with
            | TypeSymbol.Entity(ent) -> ent.Properties
            // TODO: Type parameter?
            | _ -> ImArray.empty

        member this.IsImported =
            match this with
            | TypeSymbol.Entity(ent) -> ent.IsImported
            | _ -> false

        member this.IsExported =
            match this with
            | TypeSymbol.Entity(ent) -> ent.IsExported
            | _ -> false

        member this.IsUnmanaged(pass) =
#if DEBUG || CHECKED
            match pass with
            | Pass3
            | Pass4 
            | PostInferenceAnalysis -> ()
            | _ -> OlyAssert.Fail("Invalid pass")
#endif
            match stripTypeEquations this with
            | TypeSymbol.Int8
            | TypeSymbol.UInt8
            | TypeSymbol.Int16
            | TypeSymbol.UInt16
            | TypeSymbol.Int32
            | TypeSymbol.UInt32 
            | TypeSymbol.Int64
            | TypeSymbol.UInt64
            | TypeSymbol.Float32 
            | TypeSymbol.Float64 
            | TypeSymbol.Bool
            | TypeSymbol.Char16
            | TypeSymbol.NativeInt
            | TypeSymbol.NativeUInt
            | TypeSymbol.NativePtr _
            | TypeSymbol.NativeFunctionPtr _ -> true
            | TypeSymbol.Entity(ent) -> ent.IsUnmanaged(pass)
            | TypeSymbol.Tuple(itemTys, _) ->
                itemTys
                |> ImArray.forall (fun x -> x.IsUnmanaged(pass))
            | TypeSymbol.Variable(tyPar) ->
                tyPar.Constraints 
                |> ImArray.exists (function ConstraintSymbol.Unmanaged | ConstraintSymbol.Blittable -> true | _ -> false)
            | _ -> 
                false

        member this.IsBlittable(pass) =
#if DEBUG || CHECKED
            match pass with
            | Pass3
            | Pass4 
            | PostInferenceAnalysis -> ()
            | _ -> OlyAssert.Fail("Invalid pass")
#endif
            match stripTypeEquations this with
            | TypeSymbol.Int8
            | TypeSymbol.UInt8
            | TypeSymbol.Int16
            | TypeSymbol.UInt16
            | TypeSymbol.Int32
            | TypeSymbol.UInt32 
            | TypeSymbol.Int64
            | TypeSymbol.UInt64
            | TypeSymbol.Float32 
            | TypeSymbol.Float64
            | TypeSymbol.NativeInt
            | TypeSymbol.NativeUInt -> true
            | TypeSymbol.NativePtr(elementTy) -> elementTy.IsBlittable(pass)
            | TypeSymbol.NativeFunctionPtr(ilCallConv, _, _) ->
                match ilCallConv with
                | Oly.Metadata.OlyILCallingConvention.Blittable ->
                    true
                | _ ->
                    false
            | TypeSymbol.Entity(ent) -> ent.IsBlittable(pass)
            | TypeSymbol.Tuple(itemTys, _) ->
                itemTys
                |> ImArray.forall (fun x -> x.IsBlittable(pass))
            | TypeSymbol.Variable(tyPar) ->
                tyPar.Constraints 
                |> ImArray.exists (function ConstraintSymbol.Blittable -> true | _ -> false)
            | _ -> 
                false

type FunctionGroupSymbol with

    static member CreateWithDefaultEnclosing(name: string, funcs: IFunctionSymbol imarray, fakeParCount, isPattern) =
        FunctionGroupSymbol(EnclosingSymbol.RootNamespace, name, funcs, fakeParCount, isPattern)

    static member Create(name: string, funcs: IFunctionSymbol imarray, fakeParCount, isPattern) =
        OlyAssert.False(funcs.IsEmpty)
        let principalFunc = funcs[0]

        // TODO: Check if this actually has performance implications.
        let areEnclosingsSame = funcs |> ImArray.forall (fun x -> areEnclosingsEqual x.Enclosing principalFunc.Enclosing)
        let enclosing =
            // We do this to make scoping in function-groups more sane as we can do it in bulk.
            // Scoping looks at a value's enclosing - so this is why we generally want to have one, even for function groups.
            if areEnclosingsSame then
                principalFunc.Enclosing
            else
                EnclosingSymbol.RootNamespace
        FunctionGroupSymbol(enclosing, name, funcs, fakeParCount, isPattern)

    static member Create(funcs: IFunctionSymbol imarray) =
        let principalFunc = funcs[0]
        FunctionGroupSymbol.Create(principalFunc.Name, funcs, principalFunc.Parameters.Length, false)

    static member CreateIfPossible(funcs: IFunctionSymbol imarray) =
        Assert.ThrowIfNot(not funcs.IsEmpty)
        if funcs.Length = 1 then
            funcs[0]
        else
            FunctionGroupSymbol.Create(funcs) :> IFunctionSymbol

type EntitySymbol with

    static member Distinct(ents: EntitySymbol seq) =
        HashSet(ents, EntitySymbolComparer()) :> _ seq

    member this.IsAggregatedNamespace =
        this.IsNamespace &&
        match this with
        | :? AggregatedNamespaceSymbol -> true
        | _ -> false

    member this.Apply(tyArgs: TypeSymbol imarray) =
        applyEntity tyArgs this

    member this.ToInstantiation() =
        if this.TypeParameters.IsEmpty then
            this
        else
            OlyAssert.True(this.IsTypeConstructor)
            this.TypeParameters 
            |> ImArray.map (fun x -> x.AsType)
            |> this.Apply

    member this.IsImported =
        attributesContainImport this.Attributes

    member this.IsExported =
        attributesContainExport this.Attributes

    member this.IsIntrinsic =
        this.Flags &&& EntityFlags.Intrinsic = EntityFlags.Intrinsic

    member this.IsInvalid =
        this.Flags &&& EntityFlags.Invalid = EntityFlags.Invalid

    member this.IsScoped =
        this.Flags &&& EntityFlags.Scoped = EntityFlags.Scoped

    member this.TryImportedInfo =
        this.Attributes
        |> Seq.tryPick (fun x ->
            match x with
            | AttributeSymbol.Import(platform, path, name) -> Some(platform, path, name)
            | _ -> None
        )

    member this.ImplementsInterfaces =
        this.Implements
        |> filterTypesAsInterfaces

    member this.AllLogicalImplements: TypeSymbol imarray =
        let results =
            if this.IsTypeExtension then
                seq {
                    for implementTy in this.Implements do
                        yield implementTy
                        yield! implementTy.AllLogicalImplements
                }
            elif this.IsInterface then
                Seq.empty
            else
                seq {
                    for implementTy in this.Implements do
                        yield implementTy
                        yield! implementTy.AllLogicalImplements
                }

        TypeSymbol.Distinct(results) |> ImArray.ofSeq

    /// Includes ImplicitBaseTypes
    member this.AllLogicalInherits: TypeSymbol imarray =
        let results =
            if this.IsTypeExtension then
                Seq.empty
            elif this.IsInterface then
                seq {
                    for inheritTy in this.Extends do
                        yield inheritTy
                        yield! inheritTy.AllLogicalInherits
                }
            else
                seq {
                    for inheritTy in this.Extends do
                        yield inheritTy
                        yield! inheritTy.AllLogicalInherits
                }

        TypeSymbol.Distinct(results) |> ImArray.ofSeq
    
    member this.AllInherits: TypeSymbol imarray =
        let results =
            seq {
                for inheritTy in this.Extends do
                    yield inheritTy
                    yield! inheritTy.AllInherits
            }

        TypeSymbol.Distinct(results) |> ImArray.ofSeq

    member this.AllImplements: TypeSymbol imarray =
        let results =
            seq {
                for implementTy in this.Implements do
                    yield implementTy
                    yield! implementTy.AllImplements
            }

        TypeSymbol.Distinct(results) |> ImArray.ofSeq

    member this.Hierarchy(f: TypeSymbol -> bool, set: HashSet<TypeSymbol>): unit =
        let mutable cont = true
        if this.IsTypeExtension then
            let implements = this.Implements
            let mutable i = 0
            while (cont && i < implements.Length) do
                let ty = implements[i]
                if set.Add(ty) then
                    cont <- f ty
                    if cont then
                        ty.Hierarchy(f, set)
                i <- i + 1

        elif this.IsInterface then
            let extends = this.Extends
            let mutable i = 0
            while (cont && i < extends.Length) do
                let ty = extends[i]
                if set.Add(ty) then
                    cont <- f ty
                    if cont then
                        ty.Hierarchy(f, set)
                i <- i + 1

        else
            let extends = this.Extends
            let mutable i = 0
            while (cont && i < extends.Length) do
                let ty = extends[i]
                if set.Add(ty) then
                    cont <- f ty
                    if cont then
                        ty.Hierarchy(f, set)
                i <- i + 1

            let implements = this.Implements
            let mutable i = 0
            while (cont && i < implements.Length) do
                let ty = implements[i]
                if set.Add(ty) then
                    cont <- f ty
                    if cont then
                        ty.Hierarchy(f, set)
                i <- i + 1

    member this.Hierarchy(f: TypeSymbol -> bool): unit =
        let set = HashSet(SymbolComparers.TypeSymbolComparer())
        this.Hierarchy(f, set)

    member inline this.HierarchyForEach([<InlineIfLambda>] f: TypeSymbol -> unit): unit =
        this.Hierarchy(fun x ->
            f x
            true
        )

    member this.HierarchyExists(f: TypeSymbol -> bool): bool =
        let mutable exists = false
        this.Hierarchy(fun x ->
            exists <- f x
            not exists
        )
        exists

    member this.FlattenHierarchy(): TypeSymbol imarray =
        let builder = ImArray.builder()
        this.Hierarchy(fun x -> builder.Add(x); true)
        builder.ToImmutable()

    /// 'Logical' means that certain results may not be included based on language rules.
    /// Includes implicit base types.
    /// For example: 
    ///
    ///    Extension types have to have an inherited type,
    ///        but when logically looking for members, 
    ///        we do not want to include the inherited type's members.
    ///        Therefore, the inherited type is excluded from the result of this call.
    ///
    ///    Where as non-'Logical' calls get access to the exact information for the type/entity.
    ///    For example: 'x.Inherits' where 'x' is the extension type;
    ///                 this will return the single inherited type which the extension type is extending.
    // TODO: Remove this.
    member this.AllLogicalInheritsAndImplements: TypeSymbol imarray =
        this.FlattenHierarchy()

    static member private CheckUnmanaged(pass, hash: HashSet<EntitySymbol>, ty: TypeSymbol) =
#if DEBUG || CHECKED
        match pass with
        | Pass3
        | Pass4 
        | PostInferenceAnalysis -> ()
        | _ -> OlyAssert.Fail("Invalid pass")
#endif
        match ty.TryEntity with
        | ValueSome(ent) ->
            ent.CheckUnmanaged(pass, hash)
        | _ ->
            ty.IsUnmanaged(pass)

    member private this.CheckUnmanaged(pass, hash: HashSet<EntitySymbol>) =
#if DEBUG || CHECKED
        match pass with
        | Pass3
        | Pass4 
        | PostInferenceAnalysis -> ()
        | _ -> OlyAssert.Fail("Invalid pass")
#endif
        if this.IsAnyStruct then
            if hash.Add(this) then
                if this.IsAlias && this.Extends.Length > 0 then
                    EntitySymbol.CheckUnmanaged(pass, hash, this.Extends[0])
                else
                    this.Fields 
                    |> ImArray.filter (fun x -> x.IsInstance)
                    |> ImArray.forall (fun x -> EntitySymbol.CheckUnmanaged(pass, hash, x.Type))
            else
                true
        else
            false

    static member private CheckBlittable(pass, hash: HashSet<EntitySymbol>, ty: TypeSymbol) =
#if DEBUG || CHECKED
        match pass with
        | Pass3
        | Pass4 
        | PostInferenceAnalysis -> ()
        | _ -> OlyAssert.Fail("Invalid pass")
#endif
        match ty.TryEntity with
        | ValueSome(ent) ->
            ent.CheckBlittable(pass, hash)
        | _ ->
            ty.IsBlittable(pass)

    member private this.CheckBlittable(pass, hash: HashSet<EntitySymbol>) =
#if DEBUG || CHECKED
        match pass with
        | Pass3
        | Pass4 
        | PostInferenceAnalysis -> ()
        | _ -> OlyAssert.Fail("Invalid pass")
#endif
        if this.IsAnyStruct then
            if hash.Add(this) then
                if this.IsAlias && this.Extends.Length > 0 then
                    EntitySymbol.CheckBlittable(pass, hash, this.Extends[0])
                else
                    this.Fields 
                    |> ImArray.filter (fun x -> x.IsInstance)
                    |> ImArray.forall (fun x -> EntitySymbol.CheckBlittable(pass, hash, x.Type))
            else
                true
        else
            false

    member this.IsUnmanaged(pass: CompilerPass) =
#if DEBUG || CHECKED
        match pass with
        | Pass3
        | Pass4 
        | PostInferenceAnalysis -> ()
        | _ -> OlyAssert.Fail("Invalid pass")
#endif
        let flags = this.__CachedFlags
        if flags.HasFlag(EntityCachedFlags.NotUnmanaged) then
            false
        else if flags.HasFlag(EntityCachedFlags.Unmanaged) then
            true
        else
            if this.IsAnyStruct then
                // TODO: Allocation, maybe an object pool would be better?
                //       We only try to do this once so maybe it is ok.
                let hash = HashSet<EntitySymbol>(EntitySymbolComparer())
                let result = this.CheckUnmanaged(pass, hash)
                if result then
                    this.__CachedFlags <- flags ||| EntityCachedFlags.Unmanaged
                else
                    this.__CachedFlags <- flags ||| EntityCachedFlags.NotUnmanaged
                result
            else
                this.__CachedFlags <- flags ||| EntityCachedFlags.NotUnmanaged
                false

    member this.IsBlittable(pass: CompilerPass) =
#if DEBUG || CHECKED
        match pass with
        | Pass3
        | Pass4 
        | PostInferenceAnalysis -> ()
        | _ -> OlyAssert.Fail("Invalid pass")
#endif
        let flags = this.__CachedFlags
        if flags.HasFlag(EntityCachedFlags.NotBlittable) then
            false
        else if flags.HasFlag(EntityCachedFlags.Blittable) then
            true
        else
            if this.IsAnyStruct then
                // TODO: Allocation, maybe an object pool would be better?
                //       We only try to do this once so maybe it is ok.
                let hash = HashSet<EntitySymbol>(EntitySymbolComparer())
                let result = this.CheckBlittable(pass, hash)
                if result then
                    this.__CachedFlags <- flags ||| EntityCachedFlags.Blittable
                else
                    this.__CachedFlags <- flags ||| EntityCachedFlags.NotBlittable
                result
            else
                this.__CachedFlags <- flags ||| EntityCachedFlags.NotBlittable
                false


let subsumesEntityWith rigidity (super: EntitySymbol) (ent: EntitySymbol) =
    if ent.FormalId = super.FormalId then
        if ent.TypeArguments.Length = super.TypeArguments.Length then
            (ent.TypeArguments, super.TypeArguments)
            ||> ImArray.forall2 (fun ty superTy ->
                if superTy.IsTypeConstructor then
                    match stripTypeEquations superTy with
                    | TypeSymbol.Variable(tyPar) ->
                        tyPar.Constraints
                        |> Seq.exists (function
                            | ConstraintSymbol.Null
                            | ConstraintSymbol.Struct
                            | ConstraintSymbol.NotStruct 
                            | ConstraintSymbol.Unmanaged
                            | ConstraintSymbol.Blittable
                            | ConstraintSymbol.Scoped
                            | ConstraintSymbol.ConstantType _ -> false
                            | ConstraintSymbol.SubtypeOf(superTy)
                            | ConstraintSymbol.TraitType(superTy) ->
                                subsumesTypeConstructorWith rigidity superTy.Value ty
                        )
                    | _ ->
                        subsumesTypeConstructorWith rigidity superTy ty
                else
                    UnifyTypes rigidity ty superTy
            )
        else
            false
    else
        let superTy = super.AsType
        ent.HierarchyExists (fun x ->
            subsumesTypeWith rigidity superTy x
        )

let subsumesEntity (super: EntitySymbol) (ent: EntitySymbol) =
    subsumesEntityWith Rigid super ent

let subsumesTypeWith rigidity (superTy: TypeSymbol) (ty: TypeSymbol) =
    if UnifyTypes rigidity ty superTy then
        if ty.IsTypeVariable && not superTy.IsTypeVariable then
            false
        else
            true
    else
        match stripTypeEquationsAndBuiltIn ty, stripTypeEquationsAndBuiltIn superTy with
        | _, TypeSymbol.BaseObject -> true
        | TypeSymbol.Variable(tyPar), superTy
        | TypeSymbol.HigherVariable(tyPar, _), superTy ->
            (false, tyPar.Constraints)
            ||> Seq.fold (fun exists constr ->
                match constr with
                | ConstraintSymbol.Null
                | ConstraintSymbol.Struct
                | ConstraintSymbol.NotStruct 
                | ConstraintSymbol.Unmanaged
                | ConstraintSymbol.Blittable
                | ConstraintSymbol.Scoped -> exists
                | ConstraintSymbol.SubtypeOf(ty)
                | ConstraintSymbol.TraitType(ty) ->
                    subsumesTypeWith rigidity superTy ty.Value
                | ConstraintSymbol.ConstantType(ty) ->
                    areTypesEqualWithRigidity rigidity superTy ty.Value
            )
        | TypeSymbol.ByRef(ty, ByRefKind.ReadWrite), TypeSymbol.ByRef(superTy, ByRefKind.ReadOnly) ->
            subsumesTypeWith rigidity superTy ty

        | TypeSymbol.ByRef(ty, ByRefKind.ReadWrite), TypeSymbol.ByRef(superTy, ByRefKind.WriteOnly) ->
            subsumesTypeWith rigidity superTy ty

        | _ -> 
            match ty.TryEntity, superTy.TryEntity with
            | ValueSome(ent), ValueSome(super) ->
                subsumesEntityWith rigidity super ent
            | _ ->
                false


let subsumesType superTy ty =
    subsumesTypeWith Rigid superTy ty

let subsumesTypeConstructorWith rigidity (superTy: TypeSymbol) (ty: TypeSymbol) =
    if not superTy.IsTypeConstructor || not ty.IsTypeConstructor then
        false
    else
        if UnifyTypes rigidity superTy ty then true
        else
            if superTy.Arity = ty.Arity then    
                ty.HierarchyExists(fun ty -> areTypesEqual ty.Formal superTy)
            else
                false

let subsumesTypeConstructor superTy ty =
    subsumesTypeConstructorWith Rigid superTy ty

let filterTypesAsInterfaces (tys: TypeSymbol imarray) =
    tys
    |> ImArray.filter (fun x -> x.IsInterface)

let filterTypesAsAbstract (tys: TypeSymbol imarray) =
    tys
    |> ImArray.filter (fun x -> x.IsAbstract)

let filterMostSpecificTypes (tys: TypeSymbol imarray) =
    tys
    |> ImArray.filter (fun ty1 ->
        tys
        |> ImArray.forall (fun ty2 ->
            if subsumesType ty1 ty2 || subsumesType ty2 ty1 then
                subsumesType ty2 ty1
            else
                true
        )
    )

let filterMostSpecificExtensions (tyExts: EntitySymbol imarray) =       
    tyExts
    |> ImArray.filter (fun ty1 ->
        OlyAssert.True(ty1.IsTypeExtension)
        let ty1 = ty1.Extends[0]
        tyExts
        |> ImArray.forall (fun ty2 ->
            OlyAssert.True(ty2.IsTypeExtension)
            let ty2 = ty2.Extends[0]
            if subsumesType ty1 ty2 || subsumesType ty2 ty1 then
                subsumesType ty2 ty1
            else
                true
        )
    )

let private getTotalTypeVariableUseCountFromType (ty: TypeSymbol) =
    let mutable count = 0

    let rec implType ty =
        match stripTypeEquations ty with
        | TypeSymbol.Variable(_) ->
            count <- count + 1
        | TypeSymbol.HigherVariable(_, tyArgs) ->
            count <- count + 1
            for i = 0 to tyArgs.Length - 1 do
                implType tyArgs.[i]
        | TypeSymbol.NativeFunctionPtr(_, inputTy, returnTy)
        | TypeSymbol.Function(inputTy, returnTy, _) ->
            implType inputTy
            implType returnTy
        | TypeSymbol.ForAll(_, innerTy) ->
            implType innerTy
        | TypeSymbol.Tuple(tyArgs, _) ->
            tyArgs |> ImArray.iter implType
        | TypeSymbol.Entity(ent) ->
            for i = 0 to ent.TypeArguments.Length - 1 do
                implType ent.TypeArguments.[i]
        | _ ->
            let tyTyArgs = ty.TypeArguments
            for i = 0 to tyTyArgs.Length - 1 do
                implType tyTyArgs[i]

    implType ty
    count

let filterMostSpecificFunctionsByEnclosing (funcs: IFunctionSymbol imarray) =
    let filteredFuncs =
        funcs
        |> ImArray.choose (fun func ->
            let exists =
                funcs
                |> ImArray.exists (fun func2 ->
                    if func.Id = func2.Id then
                        false
                    else
                        if areEnclosingsEqual func.Enclosing func2.Enclosing then
                            false
                        else
                            match func.Enclosing.TryEntity, func2.Enclosing.TryEntity with
                            | Some(ent), Some(super) ->
                                subsumesEntity super ent
                            | _ ->
                                false
                )
            if exists then
                Some func
            else
                None
        )
    if filteredFuncs.IsEmpty then
        funcs
    else
        filteredFuncs

let private isPossiblyNotSpecific (func: IFunctionSymbol) (targetFunc: IFunctionSymbol) =
    if targetFunc.IsVirtual then (areLogicalFunctionSignaturesEqual func targetFunc)
    else

    if targetFunc.IsStatic && targetFunc.FunctionOverrides.IsSome then (areLogicalFunctionSignaturesEqual func targetFunc)
    else

    if func.Enclosing.IsTypeExtension && targetFunc.Enclosing.IsTypeExtension then (areLogicalFunctionSignaturesEqual func targetFunc)
    else

    func.IsConstructor && targetFunc.IsConstructor && areLogicalConstructorSignaturesEqual func targetFunc

let filterMostSpecificFunctions (funcs: IFunctionSymbol imarray) =
    funcs
    |> ImArray.filter (fun func1 ->
        let isNotSpecific =
            funcs
            |> ImArray.exists (fun func2 ->
                if func1.Id = func2.Id then false
                else
                    if isPossiblyNotSpecific func1 func2 then
                        match func1.Enclosing.TryEntity, func2.Enclosing.TryEntity with
                        | Some ent1, Some ent2 -> 
                            if ent1.IsTypeExtension && ent2.IsTypeExtension then
                                if areTypesEqual ent1.Extends[0] ent2.Extends[0] then
                                    false
                                else
                                    subsumesType ent1.Extends[0] ent2.Extends[0]
                            else
                                if areEntitiesEqual ent1 ent2 then
                                    // Handles use-case.
                                    (*
                                        interface IA<T> =
                                        
                                            Test(x: T): ()
                                            Test(x: int32): ()
                                        
                                        class Test =
                                            implements IA<int32>
                                        
                                            Test(x: int32): () =
                                                print(x)
                                        
                                        main(): () =
                                            let t = Test()
                                            let t = t: IA<int32>
                                            t.Test(123)
                                    *)
                                    let xCount = getTotalTypeVariableUseCountFromType func1.Formal.AsFunction.Type
                                    let yCount = getTotalTypeVariableUseCountFromType func2.Formal.AsFunction.Type
                                    if xCount > yCount then
                                        true
                                    else
                                        false
                                else
                                    subsumesEntity ent1 ent2
                        | _ -> 
                            false
                    else
                        false
            )
        if isNotSpecific then false
        else true
    )

let filterMostSpecificProperties (props: IPropertySymbol seq) =
    props
    |> Seq.filter (fun prop1 ->
        let isNotSpecific =
            props
            |> Seq.exists (fun prop2 ->
                if prop1.Id = prop2.Id then false
                else
                    if ((prop1.Enclosing.IsTypeExtension && prop2.Enclosing.IsTypeExtension)) && ((prop1.Name = prop2.Name && areTypesEqual prop1.Type prop2.Type)) then
                        match prop1.Enclosing.TryEntity, prop2.Enclosing.TryEntity with
                        | Some ent1, Some ent2 -> 
                            if ent1.IsTypeExtension && ent2.IsTypeExtension then
                                if areTypesEqual ent1.Extends[0] ent2.Extends[0] then
                                    false
                                else
                                    subsumesType ent1.Extends[0] ent2.Extends[0]
                            else
                                subsumesEntity ent1 ent2
                        | _ -> 
                            false
                    else
                        false
            )
        if isNotSpecific then false
        else true
    )

let distinctFunctions (funcs: IFunctionSymbol imarray) =
    let funcSetComparer =
        { new IEqualityComparer<IFunctionSymbol> with
            member _.GetHashCode _ = 0
            member _.Equals(func1, func2) =
                areLogicalFunctionSignaturesEqual func1 func2 &&
                areEnclosingsEqual func1.Enclosing func2.Enclosing
        }
    let funcSet = HashSet(funcSetComparer)

    funcs
    |> ImArray.iter (fun x ->
        funcSet.Add(x) |> ignore
    )

    funcSet
    |> ImArray.ofSeq

let distinctProperties (props: IPropertySymbol imarray) =
    let propSetComparer =
        { new IEqualityComparer<IPropertySymbol> with
            member _.GetHashCode _ = 0
            member _.Equals(prop1, prop2) =
                prop1.Name = prop2.Name && prop1.Type = prop2.Type &&
                areEnclosingsEqual prop1.Enclosing prop2.Enclosing
        }
    let propSet = HashSet(propSetComparer)

    props
    |> ImArray.iter (fun x ->
        propSet.Add(x) |> ignore
    )

    propSet
    |> ImArray.ofSeq

let rec typeExistsInType (target: TypeSymbol) (ty: TypeSymbol) =
    areTypesEqual target ty ||
    (target.TypeArguments |> ImArray.exists (fun x -> typeExistsInType x ty))

let typeExistsInConstraint (constr: ConstraintSymbol) (ty: TypeSymbol) =
    match constr.TryGetAnySubtypeOf() with
    | ValueSome constrTy ->
        let isEqual = areTypesEqual constrTy ty
        isEqual || (constrTy.TypeArguments |> ImArray.exists (fun x -> typeExistsInType x ty))
    | _ ->
        false

// TODO: Handle full paths with namespaces, etc.
let createNameOfTypeExtensionWithType (tyToExtend: TypeSymbol) (withTy: TypeSymbol) =
    let tyToExtendTyArgNames =
        if tyToExtend.TypeArguments.IsEmpty then ""
        else
            let ls = 
                tyToExtend.TypeArguments
                |> Seq.map (fun x -> x.Name)
                |> String.concat ", "
            "<" + ls + ">"

    let withTyTyArgNames =
        if withTy.TypeArguments.IsEmpty then ""
        else
            let ls = 
                withTy.TypeArguments
                |> Seq.map (fun x -> x.Name)
                |> String.concat ", "
            "<" + ls + ">"

    let name =
        "__oly_type_extension_" + tyToExtend.Name + tyToExtendTyArgNames + "__oly_with_type_" + withTy.Name + withTyTyArgNames
    name

// TODO: Handle full paths with namespaces, etc.
let createNameOfTypeExtension (tyToExtend: TypeSymbol) =
    "__oly_type_extension_" + tyToExtend.Name

[<RequireQualifiedAccess>]
module private MemberPrefixNames =

    [<Literal>]
    let PropertyGet = "get_"

    [<Literal>]
    let PropertySet = "set_"

    [<Literal>]
    let Pattern = "pattern_"

    [<Literal>]
    let PatternGuard = "pattern_guard_"


let createFunctionValueSemantic (enclosing: EnclosingSymbol) attrs name (tyPars: TypeParameterSymbol imarray) (pars: ILocalParameterSymbol imarray) returnTy (memberFlags: MemberFlags) funcFlags funcSemantic funcIntrin (overrides: IFunctionSymbol option) isMutable =
    if (funcFlags &&& FunctionFlags.Constructor = FunctionFlags.Constructor) && not tyPars.IsEmpty then
        failwith "Constructors cannot contain type parameters."

    if isMutable && not enclosing.IsAnyStruct && not enclosing.IsShape then
        failwith "Function marked with 'mutable' must have an enclosing struct or shape type."

    if isMutable && not (memberFlags.HasFlag(MemberFlags.Instance)) then
        failwith "Function marked with 'mutable' must be an instance member."

    if isMutable && funcFlags.HasFlag(FunctionFlags.Constructor) then
        failwith "Function marked with 'mutable' must not be a constructor."

    if (funcFlags &&& FunctionFlags.Constructor = FunctionFlags.Constructor) && (memberFlags &&& MemberFlags.Instance = MemberFlags.Instance) then
        if pars.IsEmpty then
            failwith "Constructors must have at least a single parameter whose type is the enclosing."
        else
            let parTy = pars.[0].Type
            match enclosing.TryType with
            | Some(enclosingTy) ->
                if not (enclosingTy.IsAnyStruct && (areTypesEqual (TypeSymbol.CreateByRef(enclosingTy.ToInstantiation(), ByRefKind.ReadWrite)) parTy)) &&
                   not (areTypesEqual enclosingTy parTy) then
                    failwith "First parameter of an instance constructor is not the same as the enclosing."
            | _ ->
                failwith "Expected a type."

    if (funcFlags &&& FunctionFlags.Constructor = FunctionFlags.Constructor) && overrides.IsSome then
        failwith "Constructors cannot override."

    if (funcFlags &&& FunctionFlags.ImplicitDefaultConstructor = FunctionFlags.ImplicitDefaultConstructor) && 
        (
            (
                (memberFlags &&& MemberFlags.Instance = MemberFlags.Instance && pars.Length > 1) ||
                (memberFlags &&& MemberFlags.Instance <> MemberFlags.Instance && not pars.IsEmpty)
            ) &&
            not (enclosing.IsNewtype && pars.Length = 2) // Newtypes must have a single parameter
        ) then
        failwith "Default constructors cannot contain parameters."

 //   if funcFlags.HasFlag(FunctionFlags.StaticLocal) && (not (memberFlags.HasFlag(MemberFlags.Private)) || not enclosing.IsLocalEnclosing) then
   //     failwith "Invalid static local function."

    let funcTy = 
        TypeSymbol.CreateFunction(tyPars, pars |> ImArray.map (fun x -> x.Type), returnTy, FunctionKind.Normal)

    let tyArgs = tyPars |> Seq.map (fun x -> x.AsType) |> ImmutableArray.CreateRange

    // TODO: Clean this up. We do not need to iterate over all the attributes multiple times to determine the flags.
    //       We only need to do it once.

    let funcFlags =
        let inlineFlagsOpt = tryAttributesInlineFlags attrs
        match inlineFlagsOpt with
        | Some(inlineFlags) ->
            funcFlags ||| inlineFlags
        | _ ->
            funcFlags

    let funcFlags =
        let unmanagedFlagsOpt = tryAttributesUnmanagedFlags attrs
        match unmanagedFlagsOpt with
        | Some(unmanagedFlags) ->
            funcFlags ||| unmanagedFlags
        | _ ->
            funcFlags

    let funcFlags =
        if attributesContainPure attrs then
            funcFlags ||| FunctionFlags.Pure
        else
            funcFlags

    let funcFlags =
        if attributesContainBlittable attrs then
            funcFlags ||| FunctionFlags.Blittable
        else
            funcFlags

    let name =
        match funcSemantic with
        | PatternFunction ->
            name
        | PatternGuardFunction ->
            MemberPrefixNames.PatternGuard + name
        | GetterFunction ->
            MemberPrefixNames.PropertyGet + name
        | SetterFunction ->
            MemberPrefixNames.PropertySet + name
        | _ ->
            name

    FunctionSymbol(enclosing, attrs, name, funcTy, pars, tyPars, tyArgs, memberFlags, funcFlags, funcSemantic, funcIntrin, overrides, isMutable)

let createFunctionValue (enclosing: EnclosingSymbol) attrs name (tyPars: TypeParameterSymbol imarray) (pars: ILocalParameterSymbol imarray) returnTy memberFlags funcFlags funcIntrin overrides isMutable =
    createFunctionValueSemantic enclosing attrs name tyPars pars returnTy memberFlags funcFlags NormalFunction funcIntrin overrides isMutable

let createLocalValue name valueTy =
    LocalSymbol(name, valueTy, false, false)

let createMutableLocalValue name valueTy =
    LocalSymbol(name, valueTy, false, true)

let createLocalGeneratedValue name valueTy =
    LocalSymbol(name, valueTy, true, false)

let createTemporaryValue valueTy =
    createLocalGeneratedValue "tmp" valueTy

let createMutableLocalGeneratedValue name valueTy =
    LocalSymbol(name, valueTy, true, true)

let createLocalParameterValue (attrs, name, ty: TypeSymbol, isMutable) =
    LocalParameterSymbol(attrs, name, ty, false, false, isMutable) :> ILocalParameterSymbol

let private createLocalParameterThisValue (name, ty, isMutable) =
    LocalParameterSymbol(ImArray.empty, name, ty, true, false, isMutable) :> ILocalParameterSymbol

let private createLocalParameterBaseValue (name, ty) =
    LocalParameterSymbol(ImArray.empty, name, ty, true, true, false) :> ILocalParameterSymbol

let createLocalBridgeValue valueTy =
    createLocalGeneratedValue LocalBridgeName valueTy

let createThisValue name isCtor mightBeReadOnly (ent: EntitySymbol) =
    OlyAssert.False(ent.IsTypeConstructor)

    let ty =
        let ty =
            if ent.IsTypeExtension then
                if ent.Extends.Length = 1 then
                    ent.Extends.[0]
                else
                    TypeSymbolError
            else
                ent.AsType
        if ty.IsAnyStruct then
            let kind =
                if isCtor then ByRefKind.ReadWrite
                else 
                    if mightBeReadOnly then
                        ByRefKind.ReadOnly
                    else
                        ByRefKind.ReadWrite
            TypeSymbol.CreateByRef(ty, kind)
        else
            ty
    createLocalParameterThisValue(name, ty, false)

let createBaseValue name isCtor mightBeReadOnly (ent: EntitySymbol) =
    let ty =
        let ty =
            if ent.IsTypeExtension then
                if ent.Extends.Length = 1 then
                    ent.Extends.[0]
                else
                    TypeSymbolError
            else
                ent.AsType
        if ty.IsAnyStruct then
            let kind =
                if isCtor then ByRefKind.ReadWrite
                else 
                    if mightBeReadOnly then
                        ByRefKind.ReadOnly
                    else
                        ByRefKind.ReadWrite
            TypeSymbol.CreateByRef(ty, kind)
        else
            ty
    createLocalParameterBaseValue(name, ty)

let createBaseInstanceConstructors (ent: EntitySymbol) =
    ent.Functions
    |> ImArray.filter (fun x -> x.IsInstance && x.IsConstructor)
    |> ImArray.map (fun x ->
        let id = newId()
        // TODO: This isn't efficient.
        let notFormal = x // TODO: This is gross.
        let x = x.Formal.AsFunction
        { new IFunctionSymbol with
            member _.Name = x.Name
            member _.Enclosing = x.Enclosing
            member _.TypeParameters = x.TypeParameters
            member _.TypeArguments = x.TypeArguments
            member _.Parameters = x.Parameters
            member _.ReturnType = x.ReturnType
            member _.ValueFlags = x.ValueFlags
            member _.MemberFlags = x.MemberFlags &&& ~~~MemberFlags.Virtual
            member _.FunctionFlags = x.FunctionFlags
            member _.Attributes = x.Attributes
            member this.Formal = this
            member _.IsFunction = x.IsFunction
            member _.IsFunctionGroup = x.IsFunctionGroup
            member _.IsField = x.IsField
            member _.IsThis = true
            member _.IsBase = true
            member _.Id = id
            member _.FunctionOverrides = x.FunctionOverrides
            member _.IsProperty = false
            member _.IsPattern = false
            member _.Type = x.Type
            member _.Semantic = x.Semantic
            member _.WellKnownFunction = WellKnownFunction.None
            member _.AssociatedFormalPattern = None
        }.WithEnclosing(notFormal.Enclosing).AsFunction
    )

let createThisInstanceConstructors name (ent: EntitySymbol) =
    ent.Functions
    |> ImArray.filter (fun x -> x.IsInstance && x.IsConstructor)
    |> ImArray.map (fun x ->
        let id = newId()
        { new IFunctionSymbol with
            member _.Name = name
            member _.Enclosing = x.Enclosing
            member _.TypeParameters = x.TypeParameters
            member _.TypeArguments = x.TypeArguments
            member _.Parameters = x.Parameters
            member _.ReturnType = x.ReturnType
            member _.ValueFlags = x.ValueFlags
            member _.MemberFlags = x.MemberFlags &&& ~~~MemberFlags.Virtual
            member _.FunctionFlags = x.FunctionFlags
            member _.Attributes = x.Attributes
            member _.Formal = x.Formal
            member _.IsFunction = x.IsFunction
            member _.IsFunctionGroup = x.IsFunctionGroup
            member _.IsField = x.IsField
            member _.IsThis = true
            member _.IsBase = false
            member _.Id = id
            member _.FunctionOverrides = x.FunctionOverrides
            member _.IsProperty = false
            member _.IsPattern = false
            member _.Type = x.Type
            member _.Semantic = x.Semantic
            member _.WellKnownFunction = WellKnownFunction.None
            member _.AssociatedFormalPattern = None
        }
    )

let createFieldValue (enclosing: EnclosingSymbol) attrs name fieldTy memberFlags valueFlags associatedFormalPropId =
    FieldSymbol(attrs, enclosing, memberFlags, name, fieldTy, valueFlags, associatedFormalPropId) :> IFieldSymbol

let createPropertyValue (enclosing: EnclosingSymbol) attrs name propTy memberFlags getterOpt setterOpt backingFieldOpt =
    PropertySymbol(enclosing, attrs, name, ValueFlags.None, memberFlags, propTy, getterOpt, setterOpt, backingFieldOpt)

let createFieldConstant (enclosing: EnclosingSymbol) attrs name fieldTy memberFlags constantSymbol =

    if (memberFlags &&& ~~~MemberFlags.AccessorMask) <> MemberFlags.None then
        failwith "Invalid member flags for field constant."

    let id = newId()
    let constantSymbolOpt = ValueSome constantSymbol
    { new IFieldSymbol with
        member _.Id = id
        member _.Name = name
        member _.Enclosing = enclosing
        member _.Attributes = attrs
        member _.Type = fieldTy
        member _.IsField = true
        member _.IsFunction = false
        member _.IsFunctionGroup = false
        member _.IsProperty = false
        member _.IsPattern = false
        member _.FunctionFlags = FunctionFlags.None
        member _.ValueFlags = ValueFlags.None
        member _.IsBase = false
        member _.IsThis = false
        member this.Formal = this :> IValueSymbol
        member _.FunctionOverrides = None
        member _.TypeArguments = ImArray.empty
        member _.TypeParameters = ImArray.empty
        member _.MemberFlags = memberFlags
        member _.Constant = constantSymbolOpt    
        member _.AssociatedFormalPropertyId = None
    }

// Invalid Symbols ----------------------------------------------------

let invalidModule () =
    let id = newId()
    { new IModuleSymbol() with
        member _.FormalId = newId()
        member _.Entities = ImArray.empty
        member _.Name = ""
        member _.Functions = ImArray.empty
        member _.InstanceConstructors = ImArray.empty
        member _.Fields = ImArray.empty
        member _.Properties = ImArray.empty
        member _.Patterns = ImArray.empty
        member _.TypeParameters = ImArray.empty
        member _.TypeArguments = ImArray.empty
        member _.ContainingAssembly = None
        member this.Formal = this
        member _.Implements = ImArray.empty
        member _.Extends = ImArray.empty
        member _.Enclosing = EnclosingSymbol.RootNamespace
        member _.Kind = EntityKind.Module
        member _.Attributes = ImArray.empty
        member _.Flags = EntityFlags.None
        member _.Documentation = String.Empty
    }

let invalidFunction () =
    InvalidFunctionSymbol(EnclosingSymbol.RootNamespace, "") :> IFunctionSymbol

let invalidCustomFunction enclosing name tyPars parCount =
    let pars = 
        ImArray.init parCount (fun _ -> createLocalParameterValue(ImArray.empty, "", TypeSymbolError, false))

    createFunctionValue 
        enclosing
        ImmutableArray.Empty
        name
        tyPars
        pars
        TypeSymbolError
        MemberFlags.None
        FunctionFlags.None

let invalidValue (enclosingTyOpt: TypeSymbol option) =
    let id = newId()
    let enclosing =
        match enclosingTyOpt with
        | Some enclosingTy ->
            match enclosingTy.TryEntity with
            | ValueSome ent ->
                EnclosingSymbol.Entity ent
            | _ ->
                EnclosingSymbol.RootNamespace
        | _ ->
            EnclosingSymbol.RootNamespace
    let fieldTy = TypeSymbolError
    { new IValueSymbol with

        member _.Id = id

        member _.Enclosing = enclosing

        member _.Name = ""

        member _.Type = fieldTy

        member _.MemberFlags = MemberFlags.None

        member this.Formal = this

        member _.IsField = false

        member _.IsFunction = false
        member _.IsFunctionGroup = false
        member _.FunctionOverrides = None
        member _.IsProperty = false
        member _.IsPattern = false
        member _.TypeParameters = ImmutableArray.Empty
        member _.TypeArguments = ImmutableArray.Empty
        member _.FunctionFlags = FunctionFlags.None
        member _.ValueFlags = ValueFlags.Invalid
        member _.IsThis = false
        member _.IsBase = false
    }

let invalidLocal () =
    let id = newId()
    let ty = TypeSymbolError
    { new ILocalSymbol with

        member _.Id = id

        member _.Enclosing = EnclosingSymbol.Local

        member _.Name = ""

        member _.Type = ty

        member _.MemberFlags = MemberFlags.None

        member this.Formal = this

        member _.IsField = false

        member _.IsFunction = false
        member _.IsFunctionGroup = false
        member _.FunctionOverrides = None
        member _.IsProperty = false
        member _.IsPattern = false
        member _.TypeParameters = ImmutableArray.Empty
        member _.TypeArguments = ImmutableArray.Empty
        member _.FunctionFlags = FunctionFlags.None
        member _.ValueFlags = ValueFlags.Invalid
        member _.IsThis = false
        member _.IsBase = false
    }

let invalidField name (enclosingTyOpt: TypeSymbol option) =
    let enclosing =
        match enclosingTyOpt with
        | Some enclosingTy ->
            match enclosingTy.TryEntity with
            | ValueSome ent ->
                EnclosingSymbol.Entity ent
            | _ ->
                EnclosingSymbol.RootNamespace
        | _ ->
            EnclosingSymbol.RootNamespace
    let fieldTy = TypeSymbolError
    FieldSymbol(ImArray.empty, enclosing, MemberFlags.None, name, fieldTy, ValueFlags.Invalid, ref None)

let invalidType () =
    TypeSymbolError

let invalidTypeParameter tyParKind =
    TypeParameterSymbol("", 0, 0, tyParKind, ref ImArray.empty)

let invalidConstraint () =
    ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(invalidType()))

let invalidEntityWithEnclosing enclosing =
    { new EntitySymbol() with
        member _.FormalId = newId()
        member _.Enclosing = enclosing
        member _.Name = ""
        member _.ContainingAssembly = None
        member _.TypeParameters = enclosing.TypeParameters
        member _.Extends = ImArray.empty
        member _.InstanceConstructors = ImArray.empty
        member _.Functions = ImArray.empty
        member _.Properties = ImArray.empty
        member _.Patterns = ImArray.empty
        member _.Fields = ImArray.empty
        member _.Implements = ImArray.empty
        member _.Entities = ImArray.empty
        member _.TypeArguments = enclosing.TypeArguments
        member _.Kind = EntityKind.Class
        member this.Formal = this
        member _.Attributes = ImArray.empty
        member _.Flags = EntityFlags.Invalid
        member _.Documentation = String.Empty
    }

let invalidNamespaceWithEnclosing (enclosing: EnclosingSymbol) =
    OlyAssert.True(enclosing.IsNamespace)
    { new INamespaceSymbol() with
        member _.FormalId = newId()
        member _.Enclosing = enclosing
        member _.Name = ""
        member _.ContainingAssembly = None
        member _.TypeParameters = ImArray.empty
        member _.Extends = ImArray.empty
        member _.Functions = ImArray.empty
        member _.InstanceConstructors = ImArray.empty
        member _.Properties = ImArray.empty
        member _.Patterns = ImArray.empty
        member _.Fields = ImArray.empty
        member _.Implements = ImArray.empty
        member _.Entities = ImArray.empty
        member _.TypeArguments = ImArray.empty
        member _.Kind = EntityKind.Namespace
        member this.Formal = this
        member _.Attributes = ImArray.empty
        member _.Flags = EntityFlags.Invalid
        member _.Documentation = String.Empty
    }

let invalidateEntity (ent: EntitySymbol) =
    let id = newId()
    { new EntitySymbol() with
        member _.FormalId = newId()
        member _.Enclosing = ent.Enclosing
        member _.Name = ent.Name
        member _.ContainingAssembly = ent.ContainingAssembly
        member _.TypeParameters = ent.TypeParameters
        member _.Extends = ent.Extends
        member _.Functions = ent.Functions
        member _.InstanceConstructors = ent.InstanceConstructors
        member _.Properties = ent.Properties
        member _.Patterns = ent.Patterns
        member _.Fields = ent.Fields
        member _.Implements = ent.Implements
        member _.Entities = ent.Entities
        member _.TypeArguments = ent.TypeArguments
        member _.Kind = ent.Kind
        member this.Formal = this
        member _.Attributes = ent.Attributes
        member _.Flags = ent.Flags ||| EntityFlags.Invalid
        member _.Documentation = String.Empty
    }

let invalidEntity = invalidEntityWithEnclosing EnclosingSymbol.RootNamespace

let invalidNamespace = invalidNamespaceWithEnclosing EnclosingSymbol.RootNamespace

let invalidParameter () =
    createLocalParameterValue(ImArray.empty, "", invalidType(), false)

let invalidTypeArguments () : TypeSymbol list =
    []

// Freshen

let private unifyHiddenLinks (enclosingTyInst: IdMap<TypeSymbol imarray>) (tyPar: TypeParameterSymbol) inferenceTy =
    let linkedTyOpt = 
        match tyPar.HiddenLink with
        | Some tyPar ->
            Some tyPar.AsType
        | _ ->
            None
    match linkedTyOpt with
    | Some linkedTy -> UnifyTypes Flexible inferenceTy linkedTy |> ignore
    | _ -> ()

let freshenTypeAux (tyParExists: TypeSymbol -> bool) (enclosingTyInst: IdMap<TypeSymbol imarray>) isStrict (tyPars: ImmutableArray<TypeParameterSymbol>) (explicitTyArgs: TypeArgumentSymbol imarray) ty (cache: System.Collections.Generic.Dictionary<TypeParameterSymbol, TypeSymbol>) : TypeSymbol =
    let tyArgOffset = tyPars.Length - explicitTyArgs.Length
    if tyArgOffset < 0 then
        failwith "Internal error: Invalid tyArgOffset, must be greater than or equal to zero."

    let rec freshen (tys: System.Collections.Generic.Dictionary<TypeParameterSymbol, TypeSymbol>) (explicitTyArgs: TypeArgumentSymbol imarray) ty =

        match tyParExists ty with
        | true -> ty
        | _ ->

        match stripTypeEquations ty with
        | TypeSymbol.Function(inputTy, returnTy, kind) ->
            TypeSymbol.Function(
                freshen tys explicitTyArgs inputTy,
                freshen tys explicitTyArgs returnTy,
                kind
            )

        | TypeSymbol.ForAll(tyPars, innerTy) ->
            tyPars
            |> ImArray.iter (fun tyPar ->
                tys[tyPar] <- tyPar.AsType
            )
            TypeSymbol.ForAll(
                tyPars,
                freshen tys explicitTyArgs innerTy
            )

        | TypeSymbol.Variable(tyPar) ->   
            match tys.TryGetValue tyPar with
            | true, inferenceTy -> inferenceTy
            | _ ->
                let ty = 
                    match explicitTyArgs |> Seq.tryItem (tyPar.Index - tyArgOffset) with
                    | Some ty -> ty
                    | _ -> 
                        let inferenceTy =
                            if isStrict then
                                mkStrictInferenceVariableType (Some tyPar)
                            else
                                mkInferenceVariableType (Some tyPar)
                        unifyHiddenLinks enclosingTyInst tyPar inferenceTy
                        inferenceTy
                tys.Add(tyPar, ty)
                ty

        | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
            let inferenceTy =
                match tys.TryGetValue tyPar with
                | true, inferenceTy -> inferenceTy
                | _ ->
                    let ty = 
                        match explicitTyArgs |> Seq.tryItem (tyPar.Index - tyArgOffset) with
                        | Some ty -> ty
                        | _ -> 
                            let inferenceTy =
                                if isStrict then
                                    mkStrictInferenceVariableType (Some tyPar)
                                else
                                    mkInferenceVariableType (Some tyPar)
                            unifyHiddenLinks enclosingTyInst tyPar inferenceTy
                            inferenceTy
                    tys.Add(tyPar, ty)
                    ty
            applyType inferenceTy (tyArgs |> ImArray.map (freshen tys explicitTyArgs))

        | TypeSymbol.Tuple(tyArgs, names) ->
            TypeSymbol.Tuple(tyArgs |> ImArray.map (fun x -> freshen tys explicitTyArgs x), names)

        | TypeSymbol.Array(elementTy, rank, kind) ->
            TypeSymbol.Array(freshen tys explicitTyArgs elementTy, rank, kind)

        | TypeSymbol.FixedArray(elementTy, lengthTy, kind) ->
            TypeSymbol.FixedArray(freshen tys explicitTyArgs elementTy, freshen tys explicitTyArgs lengthTy, kind)

        | TypeSymbol.Entity(ent) when not (ent.IsTypeConstructor) ->
            let enclosingTyInst =
                match enclosingTyInst.TryGetValue ent.FormalId with
                | true, enclosingTyInst -> enclosingTyInst
                | _ -> ImArray.empty
            let tyArgs =
                ent.TypeArguments
                |> Seq.skip enclosingTyInst.Length
                |> Seq.map (freshen tys explicitTyArgs)
            TypeSymbol.Entity(applyEntity (enclosingTyInst.AddRange(tyArgs)) ent.Formal)

        | TypeSymbol.ByRef(innerTy, kind) ->
            TypeSymbol.CreateByRef(freshen tys explicitTyArgs innerTy, kind)

        | _ ->
            ty
        
    // We do this specifically for inference variables as we want to maintain the type parameter.
    match ty with
    | TypeSymbol.InferenceVariable(Some tyPar, varSolution) when varSolution.HasSolution ->
        let ty = (freshen cache explicitTyArgs varSolution.Solution)
        if ty.HasImmediateStrictInferenceVariableTypeParameter then
            mkSolvedStrictInferenceVariableType tyPar ty
        else
            mkSolvedInferenceVariableType tyPar ty
    | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, _, varSolution) when varSolution.HasSolution ->
        let newTyArgs = tyArgs |> ImArray.map (fun tyArg -> freshen cache explicitTyArgs tyArg)
        mkSolvedHigherInferenceVariableType tyPar newTyArgs (freshen cache explicitTyArgs varSolution.Solution)
    | _ ->
        freshen cache explicitTyArgs ty

let freshenValueAux tyParExists enclosingTyInst (value: IValueSymbol) =
    if value.Enclosing.TypeParameters.IsEmpty && value.TypeParameters.IsEmpty then value
    else

    match value with
    | :? LocalSymbol
    | :? IFieldSymbol -> value
    | :? IPropertySymbol
    | :? IFunctionSymbol ->
        let isStrict = value.HasStrictInference
        let cache = Dictionary<TypeParameterSymbol, TypeSymbol>(TypeParameterSymbolComparer())
        let tyArgs =
            let tyPars =
                if value.IsLocal then
                    value.TypeParameters
                else
                    value.AllTypeParameters
            let tyArgs = 
                if value.IsLocal then
                    value.TypeArguments
                else
                    value.AllTypeArguments

            tyArgs
            |> ImArray.map (fun ty ->
                freshenTypeAux tyParExists enclosingTyInst isStrict tyPars ImArray.empty ty cache
            )

        let enclosing = 
            let tyArgsForEnclosing =
                tyArgs 
                |> Seq.take value.Enclosing.TypeParameters.Length 
                |> ImmutableArray.CreateRange
            applyEnclosing tyArgsForEnclosing value.Enclosing.Formal

        if value.IsLocal then
            value.Formal.Substitute(tyArgs)
        else
            value.Formal.GetActual(enclosing, tyArgs)
    | _ ->
        if not value.IsInvalid then
            failwith "Invalid value symbol"
        value