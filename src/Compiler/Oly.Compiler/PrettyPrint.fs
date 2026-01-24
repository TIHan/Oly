module internal rec Oly.Compiler.Internal.PrettyPrint

open System
open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.SymbolEnvironments

let printFunctionName name =
    if OlySyntaxFacts.IsOperator name then
        "(" + name + ")"
    else
        name

let printTypeName isDefinition name =
    if isDefinition && OlySyntaxFacts.IsOperator name then
        "(" + name + ")"
    else
        name

let rec private printTypeAux (benv: BoundEnvironment) isDefinition isTyCtor (ty: TypeSymbol) =
    match ty with
    | TypeSymbol.Error _ -> "?"
    | TypeSymbol.Unit -> "()"
    | TypeSymbol.EagerInferenceVariable(solution, _) ->
        if solution.HasSolution then           
            printTypeAux benv isDefinition isTyCtor solution.Solution
        else
            "?"
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
    | TypeSymbol.BaseObject
    | TypeSymbol.Utf16
    | TypeSymbol.Void
    | TypeSymbol.NativeInt
    | TypeSymbol.NativeUInt
    | TypeSymbol.NativePtr _
    | TypeSymbol.ByRef _ -> printIntrinsicType benv isDefinition isTyCtor ty
    | TypeSymbol.ConstantInt32 n -> n.ToString()
    | TypeSymbol.RefCell ty -> "refCell<" + printTypeAux benv isDefinition true ty + ">"
    | TypeSymbol.Entity(ent) -> 
        if isTyCtor && ent.IsTypeConstructor && not isDefinition then
            printEntityConstructorAux benv ent
        else
            printEntityAux benv isDefinition ent   

    | TypeSymbol.Tuple(tyArgs, names) -> 
        if ty.IsRealUnit_ste then
            "(())"
        elif Seq.isEmpty tyArgs then
            "()"
        else
            if names.Length = tyArgs.Length then
                "(" + 
                    (
                        (tyArgs, names) 
                        ||> Seq.map2 (fun tyArg name ->
                            if String.IsNullOrWhiteSpace name then
                                printTypeAux benv isDefinition false tyArg
                            else
                                name + ": " + printTypeAux benv isDefinition false tyArg
                        ) 
                        |> Seq.reduce (fun x y -> x + ", " + y)
                    ) 
                 + ")"
            else
                "(" + (tyArgs |> Seq.map (printTypeAux benv isDefinition false) |> Seq.reduce (fun x y -> x + ", " + y)) + ")"

    | TypeSymbol.Array(elementTy, rank, kind) -> 
        if rank <= 0 then
            failwith "Expected rank to be greater than zero."

        let elementText = printTypeAux benv isDefinition false elementTy
        let elementText =
            if (stripTypeEquationsExceptAlias elementTy).IsAnyFunction_ste then
                "(" + elementText + ")"
            else
                elementText

        match rank with
        | 1 ->
            match kind with
            | ArrayKind.Immutable ->
                elementText + "[]"
            | ArrayKind.Mutable ->
                "mutable " + elementText + "[]"
        | _ ->
            let commas = Array.init (rank - 1) (fun _ -> ",") |> String.concat ""
            match kind with
            | ArrayKind.Immutable ->
                elementText + $"[{commas}]"
            | ArrayKind.Mutable ->
                "mutable " + elementText + $"[{commas}]"

    | TypeSymbol.FixedArray(elementTy, lengthTy, kind) -> 
        let elementText = printTypeAux benv isDefinition false elementTy
        let elementText =
            if (stripTypeEquationsExceptAlias elementTy).IsAnyFunction_ste then
                "(" + elementText + ")"
            else
                elementText

        match kind with
        | ArrayKind.Immutable ->
            elementText + $"[{printType benv lengthTy}]"
        | ArrayKind.Mutable ->
            "mutable " + elementText + $"[{printType benv lengthTy}]"

    | TypeSymbol.Variable(tyPar) ->
        let name =
            if tyPar.IsVariadic then
                tyPar.Name + "..."
            else
                tyPar.Name
        name + 
        (if tyPar.Arity = 0 || (isTyCtor && not isDefinition) then "" 
         else "<" + (Seq.init tyPar.Arity (fun _ -> "_") |> String.concat ", ") + ">"
        )

    | TypeSymbol.HigherVariable(tyPar, tyArgs) -> 
        let name =
            if tyPar.IsVariadic then
                tyPar.Name + "..."
            else
                tyPar.Name
        name + "<" + (tyArgs |> Seq.map (printTypeAux benv isDefinition true) |> String.concat ", ") + ">"

    | TypeSymbol.Function(inputTy, returnTy, kind) -> 
        match kind with
        | FunctionKind.Normal ->
            printTypeAux benv isDefinition false inputTy + " -> " + printTypeAux benv isDefinition false returnTy
        | FunctionKind.Scoped ->
            "scoped " + printTypeAux benv isDefinition false inputTy + " -> " + printTypeAux benv isDefinition false returnTy

    | TypeSymbol.NativeFunctionPtr(ilCallConv, inputTy, returnTy) ->
        if ilCallConv.HasFlag(Oly.Metadata.OlyILCallingConvention.Blittable) then
            "static blittable " +
            printTypeAux benv isDefinition false inputTy + " -> " + printTypeAux benv isDefinition false returnTy
        elif ilCallConv = Oly.Metadata.OlyILCallingConvention.Default then
            "static " +
            printTypeAux benv isDefinition false inputTy + " -> " + printTypeAux benv isDefinition false returnTy
        else
            // TODO: This really isn't what we want, but it is ok for now.
            "static [" + (ilCallConv.ToString()) + "] " +
            printTypeAux benv isDefinition false inputTy + " -> " + printTypeAux benv isDefinition false returnTy

    | TypeSymbol.ForAll(tyPars, innerTy) -> 
        "<" + (tyPars |> Seq.map (fun x -> x.Name) |> String.concat ", ") + ">" + " " + printTypeAux benv isDefinition false innerTy

    | TypeSymbol.InferenceVariable(tyPar, solution) ->
        if solution.HasSolution && solution.Solution.IsSolved then
            match tyPar with
            | Some tyPar when not tyPar.IsVariadic && solution.Solution.IsUnit_ste ->
                "(())"
            | _ ->
                printTypeAux benv isDefinition isTyCtor solution.Solution
        else
            match tyPar with
            | Some tyPar -> "?" + tyPar.DisplayName
            | _ -> "?"

    | TypeSymbol.HigherInferenceVariable(tyPar, tyArgs, _, solution) ->
        if solution.HasSolution && solution.Solution.IsSolved then
            printTypeAux benv isDefinition isTyCtor solution.Solution
        else
            // TODO: Do we need to do anything else here? What happens if we don't have a solution with tyArgs?
            match tyPar with
            | Some tyPar -> "?" + tyPar.Name + "<" + (tyArgs |> Seq.map (fun x -> x.Name) |> String.concat ", ") + ">"
            | _ -> "?" + "<" + (tyArgs |> Seq.map (printTypeAux benv isDefinition true) |> String.concat ", ") + ">"

    | TypeSymbol.DependentIndexer _ ->
        match stripTypeEquations ty with
        | TypeSymbol.DependentIndexer _ ->
            "?dependent?"
        | ty ->
            printTypeAux benv isDefinition isTyCtor ty

and private printIntrinsicType benv isDefinition isInner (ty: TypeSymbol) =
    match benv.TryFindAliasTypeByIntrinsicType(ty.Formal) with
    | ValueSome aliasTy -> printTypeAux benv isDefinition isInner (applyType aliasTy ty.TypeArguments)
    | _ -> 
        match ty with
        | TypeSymbol.ByRef(innerTy, _) ->
            ty.Name + "<" + printTypeAux benv isDefinition true innerTy + ">"
        | TypeSymbol.NativePtr(elementTy) ->
            ty.Name + "<" + printTypeAux benv isDefinition true elementTy + ">"
        | _ ->
            ty.Name

and printType benv ty =
    printTypeAux benv false false ty

and printConstraint benv constr =
    match constr with
    | ConstraintSymbol.Null ->
        "null"
    | ConstraintSymbol.Struct ->
        "struct"
    | ConstraintSymbol.NotStruct ->
        "not struct"
    | ConstraintSymbol.Unmanaged ->
        "unmanaged"
    | ConstraintSymbol.Blittable ->
        "blittable"
    | ConstraintSymbol.Scoped ->
        "scoped"
    | ConstraintSymbol.SubtypeOf(lazyTy) ->
        printType benv lazyTy.Value
    | ConstraintSymbol.ConstantType(constTy) ->
        $"constant {printType benv constTy.Value}"
    | ConstraintSymbol.TraitType(lazyTy) ->
        $"trait {printType benv lazyTy.Value}"

and printTypeDefinition benv ty =
    printTypeAux benv true false ty

and printTypeHideHigherOrder benv ty =
    printTypeAux benv true ty

and private printEntityAux (benv: BoundEnvironment) isDefinition (ent: EntitySymbol) =
    if ent.IsShape && ent.IsAnonymous then
        let props =
            ent.Properties 
            |> ImArray.map (fun x -> x :> IValueSymbol)
        let propFuncsLookup = 
            ent.Properties
            |> Seq.map (fun x ->
                [|x.Getter;x.Setter|]
            )
            |> Seq.concat
            |> Seq.choose id
            |> Seq.map (fun x -> System.Collections.Generic.KeyValuePair(x.Id, x))
            |> System.Collections.Generic.Dictionary
        let values =
            ent.Functions
            |> Seq.filter (fun x -> propFuncsLookup.ContainsKey x.Id |> not)
            |> Seq.map (fun x -> x :> IValueSymbol)
            |> Seq.append props
            |> Seq.append
                (ent.Fields |> Seq.map (fun x -> x :> IValueSymbol))
            |> ImArray.ofSeq

        if values.IsEmpty then
            "{}"
        else
            "{ " + 
            (
                values
                |> Seq.map (printValue benv)
                |> String.concat "; "
            )
            + " }"

    elif ent.LogicalTypeParameterCount > 0 then
        let tyArgs = ent.LogicalTypeArguments
        if not isDefinition && OlySyntaxFacts.IsOperator(ent.Name) && tyArgs.Length = 1 then
            sprintf "%s%s"
                (
                    let ty = tyArgs[0]
                    if ty.IsAnyFunction_ste then
                        "(" + printTypeAux benv isDefinition true ty + ")"
                    else
                        printTypeAux benv isDefinition true ty
                )
                (printTypeName isDefinition ent.Name) 
        else
            sprintf "%s<%s>" 
                (printTypeName isDefinition ent.Name) 
                (
                    ent.LogicalTypeArguments
                    |> ImArray.map (fun ty -> printTypeAux benv isDefinition true ty) |> String.concat ", "
                )
    else
        ent.Name

and printEntity benv ent =
    printEntityAux benv false ent

and printEntityDefinition benv ent =
    printEntityAux benv true ent

and private printEntityConstructorAux (benv: BoundEnvironment) (ent: EntitySymbol) =
    ent.Name

and printValueName (value: IValueSymbol) =
    if value.IsConstructor then
        value.Enclosing.AsEntity.Name
    else
        value.Name

and printEntityConstructor benv ent =
    printEntityConstructorAux benv ent

let printParameter (benv: BoundEnvironment) (par: ILocalParameterSymbol) =
    if String.IsNullOrEmpty(par.Name) then
        printTypeAux benv false true par.Type
    else
        par.Name + ": " + printTypeAux benv false true par.Type

let printParameters (benv: BoundEnvironment) (pars: ILocalParameterSymbol romem) =
    if pars.Length = 1 then
        let par = pars[0]
        if String.IsNullOrEmpty(par.Name) then
            printParameter benv par
        else
            "(" + printParameter benv par + ")"
    else
        "(" + (pars |> ROMem.mapAsImArray (printParameter benv) |> String.concat ", ") + ")"

let printLogicalParameters (benv: BoundEnvironment) (func: IFunctionSymbol) =
    let pars = func.LogicalParameters
    if pars.IsEmpty then "()"
    else
        let printedInput = printParameters benv pars
        if pars.Length = 1 && pars.[0].Name = "" then
            "(" + printedInput + ")"
        else
            printedInput

let printTypeParameters (benv: BoundEnvironment) (tyPars: TypeParameterSymbol seq) : string * string =
    let tyPars = tyPars |> ImArray.ofSeq

    let constrs =
        tyPars
        |> ImArray.choose (fun tyPar ->
            let constrs = tyPar.Constraints |> Seq.cache
            if Seq.isEmpty constrs then
                None
            else
                let tyParText = 
                    let isTyCtor = tyPar.HasArity
                    printTypeAux benv false isTyCtor tyPar.AsType
                let constrs =
                    constrs |> Seq.map (fun constr ->
                        match constr with
                        | ConstraintSymbol.Null ->
                            "null"
                        | ConstraintSymbol.Struct ->
                            "struct"
                        | ConstraintSymbol.NotStruct ->
                            "struct"
                        | ConstraintSymbol.Unmanaged ->
                            "unmanaged"
                        | ConstraintSymbol.Blittable ->
                            "blittable"
                        | ConstraintSymbol.Scoped ->
                            "scoped"
                        | ConstraintSymbol.SubtypeOf(ty) ->
                            let isTyCtor = ty.Value.IsTypeConstructor_steea
                            printTypeAux benv false isTyCtor ty.Value
                        | ConstraintSymbol.ConstantType(ty) ->
                            "constant " + printTypeAux benv (* isDefinition *) false (* isTyCtor *) false ty.Value
                        | ConstraintSymbol.TraitType(ty) ->
                            let isTyCtor = ty.Value.IsTypeConstructor_steea
                            "trait " + printTypeAux benv false isTyCtor ty.Value
                    )
                    |> String.concat ", "
                Some(tyParText + ": " + constrs)
        )

    let constrs =
        let constrs = constrs |> Seq.cache
        if Seq.isEmpty constrs then
            String.Empty
        else
            " where " + (constrs |> String.concat "")

    if tyPars.IsEmpty then
        String.Empty, String.Empty
    else
        ("<" + (tyPars |> ImArray.map (fun tyPar -> printTypeAux benv false false tyPar.AsType) |> String.concat ", ") + ">"), constrs

let private printConstant (benv: BoundEnvironment) (constant: ConstantSymbol) =
    match constant with
    | ConstantSymbol.Error -> "??"
    | ConstantSymbol.UInt8(value) -> value.ToString()
    | ConstantSymbol.Int8(value) -> value.ToString()
    | ConstantSymbol.UInt16(value) -> value.ToString()
    | ConstantSymbol.Int16(value) -> value.ToString()
    | ConstantSymbol.UInt32(value) -> value.ToString()
    | ConstantSymbol.Int32(value) -> value.ToString()
    | ConstantSymbol.UInt64(value) -> value.ToString()
    | ConstantSymbol.Int64(value) -> value.ToString()
    | ConstantSymbol.Float32(value) -> value.ToString()
    | ConstantSymbol.Float64(value) -> value.ToString()
    | ConstantSymbol.True -> "true"
    | ConstantSymbol.False -> "false"
    | ConstantSymbol.Char16(value) -> value.ToString()
    | ConstantSymbol.Utf16(value) -> value
    | ConstantSymbol.TypeVariable(tyPar) -> tyPar.Name
    | ConstantSymbol.Array(_, elements) ->
        let printedConstants =
            elements
            |> ImArray.map (printConstant benv)
            |> String.concat "; "
        $"[{printedConstants}]"
    | ConstantSymbol.External(func) ->
        printValue benv func

let private printField (benv: BoundEnvironment) (field: IFieldSymbol) =
    if field.IsFieldConstant then
        $"{field.Name}: {printType benv field.Type} = {printConstant benv field.Constant.Value}"
    else
        if field.IsMutable then
            $"field mutable {field.Name}: {printType benv field.Type}"
        else
            $"field {field.Name}: {printType benv field.Type}"

let private printValueAux (benv: BoundEnvironment) noConstrs (value: IValueSymbol) =
    let prefixText =
        if value.IsInstance || value.IsLocal || OlySyntaxFacts.IsOperator(value.Name) then String.Empty
        elif value.IsFunction && (value :?> IFunctionSymbol).IsPatternFunction then "pattern "
        elif value.IsFieldConstant then "constant "
        else "static "

    let left, printedConstrs = 
        match value with
        | :? IFunctionSymbol as func ->
            let printedTyPars, printedConstrs = 
                if func.Formal.Id = func.Id then
                    let tyPars =
                        if func.IsConstructor then
                            func.Enclosing.LogicalTypeParameters
                        else
                            func.TypeParameters
                    let printedTyPars, printedConstrs = printTypeParameters benv tyPars
                    if noConstrs then
                        printedTyPars, String.Empty
                    else
                        printedTyPars, printedConstrs
                else
                    if func.TypeArguments.IsEmpty then String.Empty, String.Empty
                    else
                        "<" + (func.TypeArguments |> Seq.map (fun x -> printTypeAux benv false false x) |> String.concat ", ") + ">" , String.Empty
            let name =
                if func.IsConstructor then
                    "new", printedConstrs
                else
                    printFunctionName func.Name + printedTyPars, printedConstrs
            name

        | :? IFieldSymbol as field ->
            printField benv field, String.Empty

        | :? IPropertySymbol as prop ->
            if prop.Getter.IsSome && prop.Setter.IsSome then
                prop.Name + $": {printTypeAux benv false false value.Type} get, set", String.Empty
            elif prop.Getter.IsSome then
                prop.Name + $": {printTypeAux benv false false value.Type} get", String.Empty
            elif prop.Setter.IsSome then
                prop.Name + $": {printTypeAux benv false false value.Type} set", String.Empty
            else
                prop.Name + $": {printTypeAux benv false false value.Type} (invalid)", String.Empty
        | _ ->
            value.Name + ": ", String.Empty
    let right =
        match value with
        | :? IFunctionSymbol as func ->
            if func.IsFunctionGroup then
                func.Name
            else
                let printedOutput = 
                    let returnTy = func.ReturnType
                    if returnTy.IsShape_ste then // handles anonymous shapes with ctors, ex: '{ new() }'
                        String.Empty
                    else
                        printTypeAux benv false false func.ReturnType
                let printedInput = 
                    if func.IsParameterLessFunction then
                        String.Empty
                    else
                        printLogicalParameters benv func
                if String.IsNullOrWhiteSpace(printedOutput) then
                    printedInput
                else
                    printedInput + ": " + printedOutput

        | _ ->
            if value.IsField || value.IsProperty then String.Empty
            else
                printTypeAux benv false false value.Type

    prefixText + left + right + printedConstrs

let printValue (benv: BoundEnvironment) (value: IValueSymbol) =
    printValueAux benv false value

let printValueNoConstraints (benv: BoundEnvironment) (value: IValueSymbol) =
    printValueAux benv true value

let printModule (benv: BoundEnvironment) (modul: IModuleSymbol) =
    modul.Name

let printEnclosing (benv: BoundEnvironment) (enclosing: EnclosingSymbol) =
    match enclosing with
    | EnclosingSymbol.RootNamespace -> ""
    | EnclosingSymbol.Local -> ""
    | EnclosingSymbol.Witness(_, tr) -> printEntity benv tr
    | EnclosingSymbol.Entity(ent) -> printEntity benv ent

let printEnclosingDefinition (benv: BoundEnvironment) (enclosing: EnclosingSymbol) =
    match enclosing with
    | EnclosingSymbol.RootNamespace -> ""
    | EnclosingSymbol.Local -> ""
    | EnclosingSymbol.Witness(_, tr) -> printEntityDefinition benv tr
    | EnclosingSymbol.Entity(ent) -> printEntityDefinition benv ent

/// Similar to 'printValue', but if the value is associated with a property or pattern,
/// it will print out either of those instead of the value.
/// REVIEW: This is kinda expensive, but it's really only called during errors, so it very likely does not matter.
let printMember (benv: BoundEnvironment) (value: IValueSymbol) =
    OlyAssert.True(value.Enclosing.IsEntity)
    if value.IsFunction && value.Enclosing.IsEntity then
        let enclosingEnt = value.Enclosing.AsEntity
        let props = 
            enclosingEnt.Properties
            |> ImArray.filter (fun prop ->
                let exists =
                    match prop.Getter with
                    | Some getter -> getter.Formal.Id = value.Formal.Id
                    | _ -> false
                if exists then
                    true
                else
                    match prop.Setter with
                    | Some setter -> setter.Formal.Id = value.Formal.Id
                    | _ -> false
            )
        let pats =
            enclosingEnt.Patterns
            |> ImArray.filter (fun pat ->
                let exists = pat.PatternFunction.Formal.Id = value.Formal.Id
                if exists then
                    true
                else
                    match pat.PatternGuardFunction with
                    | Some guard -> guard.Formal.Id = value.Formal.Id
                    | _ -> false
            )
        if not props.IsEmpty then
            printValue benv props[0]
        elif not pats.IsEmpty then
            printValue benv pats[0]
        else
            printValue benv value
    else
        printValue benv value
