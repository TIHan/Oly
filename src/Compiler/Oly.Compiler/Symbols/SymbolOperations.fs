module internal rec Oly.Compiler.Internal.SymbolOperations

open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler.Internal.Symbols

[<AutoOpen>]
module SymbolComparers =

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
        { new EqualityComparer<IEntitySymbol>() with
            member _.GetHashCode(ent) = int ent.Formal.Id
            member _.Equals(ent1, ent2) = areGeneralizedEntitiesEqual ent1 ent2
        }
    
    let EntitySymbolComparer() =
        { new EqualityComparer<IEntitySymbol>() with
            member _.GetHashCode(ent) = int ent.Id
            member _.Equals(ent1, ent2) = ent1.Id = ent2.Id
        }

    let SimilarEntitySymbolComparer() =
        { new EqualityComparer<IEntitySymbol>() with
            member _.GetHashCode(ent) = ent.Name.GetHashCode()
            member _.Equals(ent1, ent2) = areEntitiesEqual ent1 ent2
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

[<Obsolete("remove this")>]
let isVariableTy (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.Variable _ -> true
    | _ -> false

type TypeVariableRigidity =
    /// Can solve inference variables.
    | Flexible

    /// No inference. Types must exactly match.
    | Rigid

    /// No inference. Type variables can be equal if the indices are equal.
    | Indexable

    /// No inference, but is relaxed on type variables equality.
    | Generalizable

    | IntegerGeneralizable

    | NumberGeneralizable

    /// TODO: Get rid of this, only used twice.
    | FlexibleAndGeneralizable

let UnifyTypes (rigidity: TypeVariableRigidity) (ty1: TypeSymbol) (ty2: TypeSymbol) : bool =
    if obj.ReferenceEquals(ty1, ty2) then true
    else

    let ty1 = stripTypeEquationsAndBuiltIn ty1
    let ty2 = stripTypeEquationsAndBuiltIn ty2

    if obj.ReferenceEquals(ty1, ty2) then true
    else

    let res =
        match ty1, ty2 with
        | TypeSymbol.BaseObject, TypeSymbol.BaseObject
        | TypeSymbol.BaseStruct, TypeSymbol.BaseStruct
        | TypeSymbol.BaseStructEnum, TypeSymbol.BaseStructEnum
        | TypeSymbol.BaseAttribute, TypeSymbol.BaseAttribute
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

        | TypeSymbol.ObjectInferenceVariable _, TypeSymbol.ObjectInferenceVariable _ -> true

        | TypeSymbol.ObjectInferenceVariable(varSolution), ty 
        | ty, TypeSymbol.ObjectInferenceVariable(varSolution) when (rigidity = Flexible || rigidity = FlexibleAndGeneralizable) ->
            match ty with
            | TypeSymbol.InferenceVariable(_, tySolution) ->
                tySolution.Solution <- Some(TypeSymbol.BaseObject)
                varSolution.Solution <- Some(ty)
                true
            | TypeSymbol.HigherInferenceVariable _ ->
                varSolution.Solution <- Some(TypeSymbol.BaseObject)
                false
            | _ ->
                if ty.IsAnyStruct || ty.IsTypeVariable (* TODO: type variable check will need to be modified when we have a null constraint *) then
                    false
                else
                    varSolution.Solution <- Some(ty)
                    true

        | TypeSymbol.ObjectInferenceVariable(_), targetTy
        | targetTy, TypeSymbol.ObjectInferenceVariable(_) ->
            if targetTy.IsAnyStruct then
                false
            elif targetTy.IsTypeVariable then
                if rigidity = Rigid then
                    false
                else
                    true
            else
                if rigidity = Rigid then
                    UnifyTypes rigidity targetTy TypeSymbol.BaseObject
                else
                    true

        | TypeSymbol.NumberInferenceVariable(varSolution1, defaultTy1, _), TypeSymbol.NumberInferenceVariable(varSolution2, defaultTy2, _) ->
            if UnifyTypes rigidity defaultTy1 defaultTy2 then
                if (rigidity = Flexible || rigidity = FlexibleAndGeneralizable) then
                    varSolution1.Solution <- Some(defaultTy1)
                    varSolution2.Solution <- Some(defaultTy2)
                true
            else
                false

        | TypeSymbol.NumberInferenceVariable(varSolution, defaultTy, _), ty 
        | ty, TypeSymbol.NumberInferenceVariable(varSolution, defaultTy, _) when (rigidity = Flexible || rigidity = FlexibleAndGeneralizable) ->
            match ty with
            | TypeSymbol.InferenceVariable(_, tySolution) ->
                tySolution.Solution <- Some(defaultTy)
                varSolution.Solution <- Some(ty)
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
                    | TypeSymbol.Int64 when defaultTy.IsFixedInteger ->
                        varSolution.Solution <- Some(ty)
                        UnifyTypes rigidity ty1 ty2
                    | TypeSymbol.Float32
                    | TypeSymbol.Float64 when defaultTy.IsReal || defaultTy.IsFixedInteger ->
                        varSolution.Solution <- Some(ty)
                        UnifyTypes rigidity ty1 ty2
                    | _ ->
                        varSolution.Solution <- Some(defaultTy)
                        UnifyTypes rigidity ty1 ty2
                else
                    varSolution.Solution <- Some(defaultTy)
                    UnifyTypes rigidity ty1 ty2

        | TypeSymbol.NumberInferenceVariable(_, defaultTy, _), targetTy
        | targetTy, TypeSymbol.NumberInferenceVariable(_, defaultTy, _) ->
            if targetTy.IsAnyStruct then
                match targetTy with
                | TypeSymbol.UInt8
                | TypeSymbol.Int8
                | TypeSymbol.UInt16
                | TypeSymbol.Int16
                | TypeSymbol.UInt32
                | TypeSymbol.Int32
                | TypeSymbol.UInt64
                | TypeSymbol.Int64 when defaultTy.IsFixedInteger ->
                    if rigidity = Rigid then
                        UnifyTypes rigidity targetTy defaultTy 
                    else
                        rigidity = IntegerGeneralizable || rigidity = NumberGeneralizable || rigidity = Generalizable || rigidity = FlexibleAndGeneralizable
                | TypeSymbol.Float32
                | TypeSymbol.Float64 when defaultTy.IsReal || defaultTy.IsFixedInteger ->
                    if rigidity = Rigid then
                        UnifyTypes rigidity targetTy defaultTy 
                    else
                        rigidity = NumberGeneralizable || rigidity = Generalizable || rigidity = FlexibleAndGeneralizable
                | _ ->
                    false
            elif targetTy.IsTypeVariable then
                rigidity = IntegerGeneralizable || rigidity = NumberGeneralizable || rigidity = Generalizable || rigidity = FlexibleAndGeneralizable
            else
                false

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
        | TypeSymbol.HigherInferenceVariable _, TypeSymbol.HigherVariable _ when (rigidity = Generalizable || rigidity = FlexibleAndGeneralizable) -> true

        | TypeSymbol.Variable(tyPar1), TypeSymbol.Variable(tyPar2) when (rigidity <> Generalizable && rigidity <> FlexibleAndGeneralizable) -> 
            match rigidity with
            | Indexable ->
                TypeParameterSymbol.ReasonablyEquals(tyPar1, tyPar2)
            | _ ->
                tyPar1.Id = tyPar2.Id

        | TypeSymbol.HigherVariable(tyPar1, tyArgs1), TypeSymbol.HigherVariable(tyPar2, tyArgs2) when (rigidity <> Generalizable && rigidity <> FlexibleAndGeneralizable) && tyArgs1.Length = tyArgs2.Length ->
            (
                match rigidity with
                | Indexable ->
                    TypeParameterSymbol.ReasonablyEquals(tyPar1, tyPar2)
                | _ ->
                    tyPar1.Id = tyPar2.Id
            ) &&
            (
                (tyArgs1, tyArgs2)
                ||> Seq.forall2 (fun ty1 ty2 -> UnifyTypes rigidity ty1 ty2)
            )

        | TypeSymbol.ConstantInt32 n1, TypeSymbol.ConstantInt32 n2 -> n1 = n2

        | TypeSymbol.Function(argTys=argTys1; returnTy=returnTy1), TypeSymbol.Function(argTys=argTys2; returnTy=returnTy2)
        | TypeSymbol.Function(argTys=argTys1; returnTy=returnTy1), TypeSymbol.ForAll(_, TypeSymbol.Function(argTys=argTys2; returnTy=returnTy2))
        | TypeSymbol.ForAll(_, TypeSymbol.Function(argTys=argTys1; returnTy=returnTy1)), TypeSymbol.Function(argTys=argTys2; returnTy=returnTy2) ->

            // This handles the actual expansion of the variadic type, which is stored as a tuple type.
            if argTys1.Length = 1 && argTys1[0].IsVariadicInferenceVariable then
                // TODO: Kind of a hack using TypeSymbol.Tuple.
                let inputTy = 
                    if argTys2.IsEmpty then
                        TypeSymbol.Unit
                    elif argTys2.Length = 1 then
                        argTys2[0]
                    else
                        TypeSymbol.Tuple(argTys2, ImArray.empty)

                UnifyTypes rigidity argTys1[0] inputTy

            elif argTys2.Length = 1 && argTys2[0].IsVariadicInferenceVariable then
                // TODO: Kind of a hack using TypeSymbol.Tuple.
                let inputTy = 
                    if argTys1.IsEmpty then
                        TypeSymbol.Unit
                    elif argTys1.Length = 1 then
                        argTys1[0]
                    else
                        TypeSymbol.Tuple(argTys1, ImArray.empty)

                UnifyTypes rigidity inputTy argTys2[0]

            elif argTys1.Length = argTys2.Length then
                (argTys1, argTys2)
                ||> ImArray.forall2 (UnifyTypes rigidity)
                &&
                UnifyTypes rigidity returnTy1 returnTy2
            else
                false

        | TypeSymbol.NativeFunctionPtr(ilCallConv1, argTys1, returnTy1), TypeSymbol.NativeFunctionPtr(ilCallConv2, argTys2, returnTy2) ->
            // This handles the actual expansion of the variadic type, which is stored as a tuple type.
            if argTys1.Length = 1 && argTys1[0].IsVariadicInferenceVariable then
                // TODO: Kind of a hack using TypeSymbol.Tuple.
                let inputTy = 
                    if argTys2.IsEmpty then
                        TypeSymbol.Unit
                    elif argTys2.Length = 1 then
                        argTys2[0]
                    else
                        TypeSymbol.Tuple(argTys2, ImArray.empty)

                UnifyTypes rigidity argTys1[0] inputTy

            elif argTys2.Length = 1 && argTys2[0].IsVariadicInferenceVariable then
                // TODO: Kind of a hack using TypeSymbol.Tuple.
                let inputTy = 
                    if argTys1.IsEmpty then
                        TypeSymbol.Unit
                    elif argTys1.Length = 1 then
                        argTys1[0]
                    else
                        TypeSymbol.Tuple(argTys1, ImArray.empty)

                UnifyTypes rigidity inputTy argTys2[0]

            elif argTys1.Length = argTys2.Length then
                ilCallConv1 = ilCallConv2 &&
                (argTys1, argTys2)
                ||> ImArray.forall2 (UnifyTypes rigidity)
                &&
                UnifyTypes rigidity returnTy1 returnTy2
            else
                false

        | TypeSymbol.Tuple(tyArgs1, _), TypeSymbol.Tuple(tyArgs2, _) ->
            // This handles the actual expansion of the variadic type, which is stored as a tuple type.
            if tyArgs1.Length = 1 && tyArgs1[0].IsVariadicInferenceVariable then
                // TODO: Kind of a hack using TypeSymbol.Tuple.
                let inputTy = 
                    if tyArgs2.IsEmpty then
                        TypeSymbol.Unit
                    elif tyArgs2.Length = 1 then
                        tyArgs2[0]
                    else
                        TypeSymbol.Tuple(tyArgs2, ImArray.empty)

                UnifyTypes rigidity tyArgs1[0] inputTy

            elif tyArgs2.Length = 1 && tyArgs2[0].IsVariadicInferenceVariable then
                // TODO: Kind of a hack using TypeSymbol.Tuple.
                let inputTy = 
                    if tyArgs1.IsEmpty then
                        TypeSymbol.Unit
                    elif tyArgs1.Length = 1 then
                        tyArgs1[0]
                    else
                        TypeSymbol.Tuple(tyArgs1, ImArray.empty)

                UnifyTypes rigidity inputTy tyArgs2[0]

            elif tyArgs1.Length <> tyArgs2.Length then
                false
            else
                (tyArgs1, tyArgs2)
                ||> Seq.forall2 (fun ty1 ty2 -> UnifyTypes rigidity ty1 ty2)

        | TypeSymbol.ForAll(tyPars=tyPars1; innerTy=innerTy1), TypeSymbol.ForAll(tyPars=tyPars2; innerTy=innerTy2) ->
            if tyPars1.Length = tyPars2.Length then
                (
                    (tyPars1, tyPars2)
                    ||> Seq.forall2 (fun tyPar1 tyPar2 -> areTypeParameterSignaturesAndConstraintsEqual tyPar1 tyPar2)
                ) && UnifyTypes rigidity innerTy1 innerTy2
            else
                false

        | TypeSymbol.Array(elementTy1, rank1, kind1), TypeSymbol.Array(elementTy2, rank2, kind2) ->
            UnifyTypes rigidity elementTy1 elementTy2 &&
            rank1 = rank2 &&
            kind1 = kind2

        | TypeSymbol.Entity(ent1), TypeSymbol.Entity(ent2) ->
            ent1.Formal.Id = ent2.Formal.Id && ent1.TypeArguments.Length = ent2.TypeArguments.Length &&
            (ent1.TypeArguments, ent2.TypeArguments) 
            ||> Seq.forall2 (fun ty1 ty2 -> 
                UnifyTypes rigidity ty1 ty2)

        | TypeSymbol.Entity(ent), _ when ent.IsClosure ->
            UnifyTypes rigidity ent.TryClosureInvoke.Value.Type ty2
        | _, TypeSymbol.Entity(ent) when ent.IsClosure ->
            UnifyTypes rigidity ty1 ent.TryClosureInvoke.Value.Type

        | TypeSymbol.InferenceVariable(tyParOpt, solution), _ when (rigidity = Flexible || rigidity = FlexibleAndGeneralizable) && not solution.HasSolution ->
            match tyParOpt with
            | Some(tyPar) when tyPar.Arity > 0 ->
                solution.Solution <- Some (ty2.Formal)
            | _ ->
                solution.Solution <- Some (ty2)
            true

        | _, TypeSymbol.InferenceVariable(tyParOpt, solution) when (rigidity = Flexible || rigidity = FlexibleAndGeneralizable) && not solution.HasSolution ->
            match tyParOpt with
            | Some(tyPar) when tyPar.Arity > 0 ->
                solution.Solution <- Some (ty1.Formal)
            | _ ->
                solution.Solution <- Some (ty1)
            true

        | TypeSymbol.HigherInferenceVariable(_, tyArgs, externalSolution, solution), _ when (rigidity = Flexible || rigidity = FlexibleAndGeneralizable) && (not solution.HasSolution) && tyArgs.Length = ty2.Arity ->
            let result0 =
                match externalSolution.Solution with
                | None ->
                    true
                | Some(ty1) ->
                    ty1.FormalId = ty2.FormalId

            if result0 then
                let result =
                    (tyArgs, ty2.TypeArguments)
                    ||> Seq.forall2 (fun ty1 ty2 ->
                        UnifyTypes rigidity ty1 ty2
                    )

                if result then
                    let appliedTy = applyType ty2 tyArgs
                    let res = Some (appliedTy)
                    solution.Solution <- res
                    if not externalSolution.HasSolution then
                        // TODO: We should generalize the type.
                        externalSolution.Solution <- Some(ty2)
                    true
                else
                    false
            else
                false

        | _, TypeSymbol.HigherInferenceVariable(_, tyArgs, externalSolution, solution) when (rigidity = Flexible || rigidity = FlexibleAndGeneralizable) && (not solution.HasSolution) && tyArgs.Length = ty1.Arity ->
            let result0 =
                match externalSolution.Solution with
                | None ->
                    true
                | Some(ty2) ->
                    ty1.FormalId = ty2.FormalId

            if result0 then
                let result =
                    (ty1.TypeArguments, tyArgs)
                    ||> Seq.forall2 (fun ty1 ty2 ->
                        UnifyTypes rigidity ty1 ty2
                    )

                if result then
                    let appliedTy = applyType ty1 tyArgs
                    let res = Some (appliedTy)
                    solution.Solution <- res
                    if not externalSolution.HasSolution then
                        // TODO: We should generalize the type.
                        externalSolution.Solution <- Some(ty1)
                    true
                else
                    false
            else
                false

        | TypeSymbol.Variable(tyPar1), TypeSymbol.HigherVariable(tyPar2, _) when (rigidity <> Generalizable && rigidity <> FlexibleAndGeneralizable) ->
            tyPar1.Index = tyPar2.Index && tyPar1.Arity = tyPar2.Arity
        | TypeSymbol.HigherVariable(tyPar1, _), TypeSymbol.Variable(tyPar2) when (rigidity <> Generalizable && rigidity <> FlexibleAndGeneralizable) ->
            tyPar1.Index = tyPar2.Index && tyPar1.Arity = tyPar2.Arity

        // Error types will always succeed.
        | TypeSymbol.Error _, _ 
        | _, TypeSymbol.Error _ -> true

        | TypeSymbol.Variable _, _
        | TypeSymbol.HigherVariable _, _
        | _, TypeSymbol.Variable _
        | _, TypeSymbol.HigherVariable _ ->
            (rigidity = Generalizable || rigidity = FlexibleAndGeneralizable)

        | TypeSymbol.InferenceVariable(Some tyPar1, varSolution1), TypeSymbol.InferenceVariable(Some tyPar2, varSolution2) when (rigidity = Rigid) && not varSolution1.HasSolution && not varSolution2.HasSolution ->
            tyPar1.Index = tyPar2.Index

        | TypeSymbol.HigherInferenceVariable(Some tyPar1, tyArgs1, _, varSolution1), TypeSymbol.HigherInferenceVariable(Some tyPar2, tyArgs2, _, varSolution2) when (rigidity = Rigid) && not varSolution1.HasSolution && not varSolution2.HasSolution ->
            tyPar1.Index = tyPar2.Index &&
            tyArgs1.Length = tyArgs2.Length &&
            (
                (tyArgs1, tyArgs2)
                ||> Seq.forall2 areTypesEqual
            )

        | TypeSymbol.InferenceVariable(solution=solution), _
        | _, TypeSymbol.InferenceVariable(solution=solution) ->
            rigidity = Generalizable && not solution.HasSolution

        | TypeSymbol.HigherInferenceVariable(solution=solution), _
        | _, TypeSymbol.HigherInferenceVariable(solution=solution) ->
            rigidity = Generalizable && not solution.HasSolution

        | _ ->
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
        | ConstraintSymbol.SubtypeOf(ty1), ConstraintSymbol.SubtypeOf(ty2) -> 
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

let areEntitiesEqual (ent1: IEntitySymbol) (ent2: IEntitySymbol) =
    if obj.ReferenceEquals(ent1, ent2) then true
    else
        ent1.Kind = ent2.Kind &&
        ent1.Formal.Id = ent2.Formal.Id &&
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

let areGeneralizedEntitiesEqual (ent1: IEntitySymbol) (ent2: IEntitySymbol) =
    if obj.ReferenceEquals(ent1, ent2) then true
    else
        ent1.Kind = ent2.Kind &&
        ent1.Formal.Id = ent2.Formal.Id &&
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
let areLogicalFunctionSignaturesEqual (func1: IFunctionSymbol) (func2: IFunctionSymbol) =
    if obj.ReferenceEquals(func1, func2) then true
    else
        func1.Name = func2.Name &&
        func1.LogicalParameterCount = func2.LogicalParameterCount &&
        func1.TypeParameters.Length = func2.TypeParameters.Length &&
        (
            (func1.TypeParameters, func2.TypeParameters)
            ||> ImArray.forall2 (fun tyPar1 tyPar2 -> 
                TypeParameterSymbol.ReasonablyEquals(tyPar1, tyPar2)
            )
        ) &&
        (
            (func1.LogicalParameters, func2.LogicalParameters)
            ||> ROMem.forall2 (fun par1 par2 ->
                UnifyTypes TypeVariableRigidity.Indexable par1.Type par2.Type
            )
        ) &&
        UnifyTypes TypeVariableRigidity.Indexable func1.ReturnType func2.ReturnType

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
    if obj.ReferenceEquals(value1, value2) then true
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
                        if not isInstance || not ty2.IsAnyStruct || (if superFunc.IsReadOnly then func.IsReadOnly else true) then
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

    member _.SetItem(key, value) =
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

    member _.SetItem(key, value) =
        TypeSymbolMap(map.SetItem(key, value))

    member _.TryFind(key) =
        match map.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    member _.Values = map.Values
   
    static member Create() =     
        let map = ImmutableDictionary.Create<TypeSymbol, 'T>(TypeSymbolComparer())
        TypeSymbolMap(map)

[<Struct;NoComparison;NoEquality>]
type EntitySymbolGeneralizedMap<'T> private (map: ImmutableDictionary<IEntitySymbol, 'T>) =

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

    member _.GetSimilar(tr: IEntitySymbol) =
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
        let map = ImmutableDictionary.Create<IEntitySymbol, 'T>(EntitySymbolGeneralizedComparer())
        EntitySymbolGeneralizedMap(map)

[<Struct;NoComparison;NoEquality>]
type EntitySymbolSet private (set: ImmutableHashSet<IEntitySymbol>) =

    member _.Add(key) =
        EntitySymbolSet(set.Add(key))

    member _.TryFind(key) =
        match set.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    member _.Values = set :> _ seq
   
    static member Create(trs) =     
        let set = ImmutableHashSet.CreateRange<IEntitySymbol>(EntitySymbolComparer(), trs)
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

        member this.Apply(tyArgs: TypeSymbol imarray) =
            applyType this tyArgs

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

        member this.IsBaseStruct_t =
            match stripTypeEquationsAndBuiltIn this with
            | TypeSymbol.BaseStruct -> true
            | _ -> false

        member this.IsBaseStructEnum_t =
            match stripTypeEquationsAndBuiltIn this with
            | TypeSymbol.BaseStructEnum -> true
            | _ -> false

        member this.IsBaseAttribute_t =
            match stripTypeEquationsAndBuiltIn this with
            | TypeSymbol.BaseAttribute -> true
            | _ -> false

        member this.IsImplicitBaseType =
            match stripTypeEquationsAndBuiltIn this with
            | TypeSymbol.BaseObject
            | TypeSymbol.BaseStruct
            | TypeSymbol.BaseStructEnum
            | TypeSymbol.BaseAttribute -> true
            | _ -> false

        member this.ImplicitBaseTypes =
            let ty = stripTypeEquationsAndBuiltIn this
            if ty.IsAlias || ty.IsTypeExtension || ty.IsShape || ty.IsBaseObject_t then
                Seq.empty
            else
                seq {
                    // TODO: This is inefficient but works.
                    if ty.IsAnyStruct && not(this.IsBaseStruct_t) then
                        yield TypeSymbol.BaseStruct
                    if ty.IsEnum && this.IsAnyStruct && not(this.IsBaseStructEnum_t) then
                        yield TypeSymbol.BaseStructEnum
                    if ty.IsAttribute && not(ty.IsBaseAttribute_t) then
                        yield TypeSymbol.BaseAttribute
                    yield TypeSymbol.BaseObject
                }

        member this.AllLogicalInheritsAndImplements: _ imarray =
            let ty = stripTypeEquationsAndBuiltIn this
            match ty.TryEntity with
            | ValueSome ent -> ent.AllLogicalInheritsAndImplements
            | _ ->
                let result =
                    ty.ImplicitBaseTypes
                    |> ImArray.ofSeq
                match ty.TryTypeParameter with
                | ValueSome tyPar ->
                    tyPar.Constraints
                    |> Seq.choose (function
                        | ConstraintSymbol.SubtypeOf(ty) ->
                            Some ty.Value
                        | _ ->
                            None
                    )
                    |> Seq.append result
                    |> TypeSymbol.Distinct
                    |> ImArray.ofSeq
                | _ ->
                    result

        member this.AllLogicalImplements =
            match this.TryEntity with
            | ValueSome ent -> ent.AllLogicalImplements
            // TODO: type parameters?
            | _ -> ImArray.empty

        member this.AllLogicalInherits =
            match this.TryEntity with
            | ValueSome ent -> ent.AllLogicalInherits
            // TODO: type parameters?
            | _ -> this.ImplicitBaseTypes
                   |> ImArray.ofSeq

        member this.AllInherits =
            match this.TryEntity with
            | ValueSome ent -> ent.AllInherits
            // TODO: type parameters?
            | _ -> ImArray.empty

        /// Immediate functions directly on type. Does not include inherited/implemeted types' functions.
        member this.Functions: _ imarray =
            match stripTypeEquations this with
            | TypeSymbol.Entity(ent) -> ent.Functions
            | _ -> ImArray.empty

        /// All logical functions from the type and logical inherited/implemented types
        member this.AllLogicalFunctions =
            match stripTypeEquations this with
            | TypeSymbol.Entity(ent) -> ent.AllLogicalFunctions
            | TypeSymbol.Variable(tyPar)
            | TypeSymbol.InferenceVariable(Some tyPar, _) ->
                findMostSpecificIntrinsicFunctionsOfTypeParameter false tyPar
            | TypeSymbol.HigherVariable(tyPar, tyArgs)
            | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, _, _) -> 
                findMostSpecificIntrinsicFunctionsOfTypeParameter true tyPar
                |> Seq.map (fun (func: IFunctionSymbol) ->
                    let enclosing = 
                        func.Enclosing
                        |> actualEnclosing tyArgs 
                    actualFunction enclosing (enclosing.TypeArguments.AddRange(func.TypeArguments)) func
                )
            | _ ->
                Seq.empty

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

type IEntitySymbol with

    member this.IsAggregatedNamespace =
        this.IsNamespace &&
        match this with
        | :? AggregatedNamespaceSymbol -> true
        | _ -> false

    member this.TryFindDefaultInstanceConstructor() =
        this.Functions
        |> ImArray.tryFind (fun x -> x.IsInstanceConstructor && x.LogicalParameterCount = 0)

    member this.HasDefaultInstanceConstructor =
        this.Functions
        |> ImArray.exists (fun x -> x.IsInstanceConstructor && x.LogicalParameterCount = 0)

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

    member this.ImplicitBaseTypes =
        if this.IsNamespace then Seq.empty
        else this.AsType.ImplicitBaseTypes

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

        let results = Seq.append results this.ImplicitBaseTypes

        TypeSymbol.Distinct(results) |> ImArray.ofSeq
    
    member this.AllInherits: TypeSymbol imarray =
        let results =
            seq {
                for inheritTy in this.Extends do
                    yield inheritTy
                    yield! inheritTy.AllInherits
            }

        TypeSymbol.Distinct(results) |> ImArray.ofSeq

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
    member this.AllLogicalInheritsAndImplements: TypeSymbol imarray =
        let builder = ResizeArray()
        if this.IsTypeExtension then
            for implementTy in this.Implements do
                builder.Add(implementTy)
                builder.AddRange(implementTy.AllLogicalInheritsAndImplements)
        elif this.IsInterface then
            for inheritTy in this.Extends do
                builder.Add(inheritTy)
                builder.AddRange(inheritTy.AllLogicalInheritsAndImplements)
        else
            for inheritTy in this.Extends do
                builder.Add(inheritTy)
                builder.AddRange(inheritTy.AllLogicalInheritsAndImplements)

            for implementTy in this.Implements do
                builder.Add(implementTy)
                builder.AddRange(implementTy.AllLogicalInheritsAndImplements)

        builder.AddRange(this.ImplicitBaseTypes)

        TypeSymbol.Distinct(builder) |> ImArray.ofSeq

    member this.AllLogicallyInheritedAndImplementedFunctions =
        this.AllLogicalInheritsAndImplements
        |> Seq.collect (fun x -> x.Functions)

    member this.AllLogicalFunctions: _ seq =
        this.AllLogicallyInheritedAndImplementedFunctions
        |> Seq.append (this.Functions)

let subsumesEntityWith rigidity (super: IEntitySymbol) (ent: IEntitySymbol) =
    if ent.Formal.Id = super.Formal.Id then
        if ent.TypeArguments.Length = super.TypeArguments.Length then
            (ent.TypeArguments, super.TypeArguments)
            ||> Seq.forall2 (fun ty superTy ->
                if superTy.IsTypeConstructor then
                    match stripTypeEquations superTy with
                    | TypeSymbol.Variable(tyPar) ->
                        tyPar.Constraints
                        |> Seq.exists (function
                            | ConstraintSymbol.Null
                            | ConstraintSymbol.Struct
                            | ConstraintSymbol.NotStruct 
                            | ConstraintSymbol.Unmanaged 
                            | ConstraintSymbol.ConstantType _ -> false
                            | ConstraintSymbol.SubtypeOf(superTy) ->
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
        ent.AllLogicalInheritsAndImplements
        |> Seq.exists (fun x ->
            match x.TryEntity with
            | ValueSome x ->
                subsumesEntityWith rigidity super x
            | _ ->
                false
        )

let subsumesEntity (super: IEntitySymbol) (ent: IEntitySymbol) =
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
        | ty, TypeSymbol.BaseStruct -> ty.IsAnyStruct
        | ty, TypeSymbol.BaseAttribute -> ty.IsAttribute
        | ty, TypeSymbol.BaseStructEnum -> ty.IsAnyStruct && ty.IsEnum
        | TypeSymbol.Variable(tyPar), superTy
        | TypeSymbol.HigherVariable(tyPar, _), superTy ->
            (false, tyPar.Constraints)
            ||> Seq.fold (fun exists constr ->
                match constr with
                | ConstraintSymbol.Null
                | ConstraintSymbol.Struct
                | ConstraintSymbol.NotStruct 
                | ConstraintSymbol.Unmanaged -> exists
                | ConstraintSymbol.ConstantType(ty) ->
                    areTypesEqualWithRigidity rigidity superTy ty.Value
                | ConstraintSymbol.SubtypeOf(ty) ->
                    subsumesTypeWith rigidity superTy ty.Value
            )
        | TypeSymbol.ByRef(ty, ByRefKind.ReadWrite), TypeSymbol.ByRef(superTy, ByRefKind.Read) ->
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
                ty.AllLogicalInheritsAndImplements
                |> Seq.exists (fun (ty: TypeSymbol) ->
                    areTypesEqual ty.Formal superTy
                )
            else
                false

let subsumesTypeConstructor superTy ty =
    subsumesTypeConstructorWith Rigid superTy ty

let filterTypesAsInterfaces (tys: TypeSymbol imarray) =
    tys
    |> ImArray.filter (fun x -> x.IsInterface)

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

let filterMostSpecificFunctions (funcs: IFunctionSymbol imarray) =
    funcs
    |> ImArray.filter (fun x ->
        if (x.IsConstructor || x.IsFinal || not x.IsVirtual) && not x.Enclosing.IsTypeExtension then true
        else
            let isOverriden =
                funcs
                |> ImArray.exists (fun y ->
                    if x.Id = y.Id then false
                    else
                        if (y.IsVirtual || (x.Enclosing.IsTypeExtension && y.Enclosing.IsTypeExtension)) && areLogicalFunctionSignaturesEqual x y then
                            match x.Enclosing.TryEntity, y.Enclosing.TryEntity with
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
            if isOverriden then false
            else true
    )

let filterMostSpecificProperties (props: IPropertySymbol imarray) =
    props
    |> ImArray.filter (fun x ->
        if (x.IsConstructor || x.IsFinal || not x.IsVirtual) && not x.Enclosing.IsTypeExtension then true
        else
            let isOverriden =
                props
                |> ImArray.exists (fun y ->
                    if x.Id = y.Id then false
                    else
                        if (y.IsVirtual || (x.Enclosing.IsTypeExtension && y.Enclosing.IsTypeExtension)) && x.Name = y.Name && areTypesEqual x.Type y.Type then
                            match x.Enclosing.TryEntity, y.Enclosing.TryEntity with
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
            if isOverriden then false
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

let findMostSpecificIntrinsicFunctionsOfTypeParameter isTyCtor (tyPar: TypeParameterSymbol): _ imarray =
    let tys =
        tyPar.Constraints
        |> Seq.choose (fun x ->
            match x with
            | ConstraintSymbol.Null
            | ConstraintSymbol.Struct
            | ConstraintSymbol.NotStruct 
            | ConstraintSymbol.Unmanaged -> None
            | ConstraintSymbol.SubtypeOf(ty) when not ty.Value.IsTypeConstructor || ty.Value.IsTypeConstructor = isTyCtor ->
                ty.Value
                |> Some
            | _ ->
                None
        )

    let tys =
        tys
        |> Seq.collect (fun ty ->
            let isMostSpecific =
                tys
                |> Seq.exists (fun ty2 ->
                    if areTypesEqual ty ty2 then false
                    else
                        subsumesType ty ty2
                )
                |> not
            if isMostSpecific then
                ty.AllLogicalInherits.Add(ty)
            else
                ImArray.empty
        )
        |> TypeSymbol.Distinct

    tys
    |> Seq.collect (fun ty -> 
        ty.Functions
    )
    |> ImArray.ofSeq
    |> filterMostSpecificFunctions

let rec typeExistsInType (target: TypeSymbol) (ty: TypeSymbol) =
    areTypesEqual target ty ||
    (target.TypeArguments |> ImArray.exists (fun x -> typeExistsInType x ty))

let typeExistsInConstraint (constr: ConstraintSymbol) (ty: TypeSymbol) =
    match constr.TryGetSubtypeOf() with
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
                if not (enclosingTy.IsAnyStruct && (areTypesEqual (TypeSymbol.CreateByRef(enclosingTy, ByRefKind.ReadWrite)) parTy)) &&
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

    if funcFlags.HasFlag(FunctionFlags.StaticLocal) && (not (memberFlags.HasFlag(MemberFlags.Private)) || not enclosing.IsLocalEnclosing) then
        failwith "Invalid static local function."

    let funcTy = 
        TypeSymbol.CreateFunction(tyPars, pars |> ImArray.map (fun x -> x.Type), returnTy)

    let tyArgs = tyPars |> Seq.map (fun x -> x.AsType) |> ImmutableArray.CreateRange

    let funcFlags =
        if attributesContainInline attrs then
            funcFlags ||| FunctionFlags.Inline
        elif attributesContainNotInline attrs then
            funcFlags ||| FunctionFlags.InlineNever
        else
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

let createLocalParameterValue (attrs, name, ty, isMutable) =
    LocalParameterSymbol(attrs, name, ty, false, false, isMutable) :> ILocalParameterSymbol

let private createLocalParameterThisValue (name, ty, isMutable) =
    LocalParameterSymbol(ImArray.empty, name, ty, true, false, isMutable) :> ILocalParameterSymbol

let private createLocalParameterBaseValue (name, ty) =
    LocalParameterSymbol(ImArray.empty, name, ty, true, true, false) :> ILocalParameterSymbol

let createLocalBridgeValue valueTy =
    createLocalGeneratedValue LocalBridgeName valueTy

let createThisValue name isCtor mightBeReadOnly (ent: IEntitySymbol) =
    OlyAssert.False(ent.IsTypeConstructor)

    let ty =
        let ty =
            if ent.IsTypeExtension then
                if ent.Extends.Length = 1 then
                    ent.Extends.[0]
                else
                    TypeSymbol.Error(None)
            else
                ent.AsType
        if ty.IsAnyStruct then
            let kind =
                if isCtor then ByRefKind.ReadWrite
                else 
                    if mightBeReadOnly then
                        ByRefKind.Read
                    else
                        ByRefKind.ReadWrite
            TypeSymbol.CreateByRef(ty, kind)
        else
            ty
    createLocalParameterThisValue(name, ty, false)

let createBaseValue name isCtor mightBeReadOnly (ent: IEntitySymbol) =
    let ty =
        let ty =
            if ent.IsTypeExtension then
                if ent.Extends.Length = 1 then
                    ent.Extends.[0]
                else
                    TypeSymbol.Error(None)
            else
                ent.AsType
        if ty.IsAnyStruct then
            let kind =
                if isCtor then ByRefKind.ReadWrite
                else 
                    if mightBeReadOnly then
                        ByRefKind.Read
                    else
                        ByRefKind.ReadWrite
            TypeSymbol.CreateByRef(ty, kind)
        else
            ty
    createLocalParameterBaseValue(name, ty)

let createBaseInstanceConstructors name (ent: IEntitySymbol) =
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
        }
    )

let createFieldValue (enclosing: EnclosingSymbol) attrs name fieldTy memberFlags valueFlags associatedFormalPropId =
    FieldSymbol(attrs, enclosing, memberFlags, name, fieldTy, valueFlags, associatedFormalPropId) :> IFieldSymbol

let createPropertyValue (enclosing: EnclosingSymbol) attrs name propTy memberFlags getterOpt setterOpt backingFieldOpt =
    PropertySymbol(enclosing, attrs, name, ValueFlags.None, memberFlags, propTy, getterOpt, setterOpt, backingFieldOpt)

let createPatternValue (enclosing: EnclosingSymbol) attrs name func =
    PatternSymbol(enclosing, attrs, name, func) :> IPatternSymbol

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
    { new IModuleSymbol with
        member _.Entities = ImArray.empty
        member _.Id = id
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
    }

let invalidFunction () =
    InvalidFunctionSymbol(EnclosingSymbol.RootNamespace, "") :> IFunctionSymbol

let invalidCustomFunction enclosing name tyPars parCount =
    let pars = 
        ImArray.init parCount (fun _ -> createLocalParameterValue(ImArray.empty, "", TypeSymbol.Error None, false))

    createFunctionValue 
        enclosing
        ImmutableArray.Empty
        name
        tyPars
        pars
        (TypeSymbol.Error(None))
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
    let fieldTy = TypeSymbol.Error None
    { new IValueSymbol with

        member _.Id = id

        member _.Enclosing = enclosing

        member _.Name = ""

        member _.Type = fieldTy

        member _.MemberFlags = MemberFlags.None

        member this.Formal = this

        member _.IsField = false

        member _.IsFunction = false
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
    let ty = TypeSymbol.Error None
    { new ILocalSymbol with

        member _.Id = id

        member _.Enclosing = EnclosingSymbol.Local

        member _.Name = ""

        member _.Type = ty

        member _.MemberFlags = MemberFlags.None

        member this.Formal = this

        member _.IsField = false

        member _.IsFunction = false
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
    let fieldTy = TypeSymbol.Error None
    FieldSymbol(ImArray.empty, enclosing, MemberFlags.None, name, fieldTy, ValueFlags.Invalid, ref None)

let invalidType () =
    TypeSymbol.Error None

let invalidTypeParameter tyParKind =
    TypeParameterSymbol("", 0, 0, tyParKind, ref ImArray.empty)

let invalidConstraint () =
    ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(invalidType()))

let invalidEntityWithEnclosing enclosing =
    { new IEntitySymbol with
        member _.Enclosing = enclosing
        member _.Id = -1L
        member _.Name = ""
        member _.ContainingAssembly = None
        member _.TypeParameters = ImArray.empty
        member _.Extends = ImArray.empty
        member _.InstanceConstructors = ImArray.empty
        member _.Functions = ImArray.empty
        member _.Properties = ImArray.empty
        member _.Patterns = ImArray.empty
        member _.Fields = ImArray.empty
        member _.Implements = ImArray.empty
        member _.Entities = ImArray.empty
        member _.TypeArguments = ImArray.empty
        member _.Kind = EntityKind.Class
        member this.Formal = this
        member _.Attributes = ImArray.empty
        member _.Flags = EntityFlags.None ||| EntityFlags.Invalid
    }

let invalidNamespaceWithEnclosing enclosing =
    { new INamespaceSymbol with
        member _.Enclosing = enclosing
        member _.Id = -1L
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
    }

let invalidEntity = invalidEntityWithEnclosing EnclosingSymbol.RootNamespace

let invalidNamespace = invalidNamespaceWithEnclosing EnclosingSymbol.RootNamespace

let invalidParameter () =
    createLocalParameterValue(ImArray.empty, "", invalidType(), false)

let invalidTypeArguments () : TypeSymbol list =
    []

// --------------------------------------------------------------------

let freshWitnesses (tyPar: TypeParameterSymbol) =
    tyPar.Constraints
    |> ImArray.choose (fun x -> 
        match x.TryGetSubtypeOf() with
        | ValueSome constrTy ->
            match constrTy.TryEntity with
            | ValueSome ent when ent.IsInterface ->
                WitnessSolution(tyPar, ent, None)
                |> Some
            | _ ->
                None
        | _ ->
            None
    ) 

let freshWitnessesWithTypeArguments asm (tyArgs: TypeArgumentSymbol imarray) (tyPar: TypeParameterSymbol) =
    tyPar.Constraints
    |> ImArray.choose (fun constr -> 
        match constr.TryGetSubtypeOf() with
        | ValueSome constrTy ->
            match constrTy.TryEntity with
            | ValueSome ent when ent.IsInterface || ent.IsShape ->
                let ent = ent.Substitute(tyArgs)
                if ent.IsShape then
                    ent.Functions
                    |> ImArray.map (fun func ->
                        WitnessSolution(tyPar, ent, Some func)
                    )
                    |> Some
                else
                    WitnessSolution(tyPar, ent, None)
                    |> ImArray.createOne
                    |> Some
            | _ ->
                None 
        | _ ->
            None
    )
    |> ImArray.concat
