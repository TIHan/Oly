module internal rec Oly.Compiler.Internal.BoundTreeExtensions

open System.Collections.Generic
open System.Collections.ObjectModel

open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeVisitor
open Oly.Compiler.Internal.BoundTreeRewriter
open Oly.Compiler.Internal.BoundTreePatterns
open System.Threading

// TODO: Rename 'intrinsic' uses to something else as we use 'intrinsic' to mean compiler 'intrinsic'.
// TODO: Rename 'extrinsic' to something else.

[<AutoOpen>]
module private Helpers =

    let findImmediateFieldsOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: IEntitySymbol) =
        filterFields queryMemberFlags valueFlags nameOpt ent.Fields
        |> filterValuesByAccessibility benv queryMemberFlags
    
    let findIntrinsicFieldsOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: IEntitySymbol) =
        let fields = findImmediateFieldsOfEntity benv queryMemberFlags valueFlags nameOpt ent
    
        let inheritedFields =
            ent.Extends
            |> Seq.map (fun x ->
                match x.TryEntity with
                | ValueSome x ->
                    findIntrinsicFieldsOfEntity benv queryMemberFlags valueFlags nameOpt x
                | _ ->
                    Seq.empty
            )
            |> Seq.concat
            |> filterValuesByAccessibility benv queryMemberFlags
    
        Seq.append inheritedFields fields
    
    let findImmediatePropertiesOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: IEntitySymbol) =
        filterProperties queryMemberFlags valueFlags nameOpt ent.Properties
        |> filterValuesByAccessibility benv queryMemberFlags
    
    let findIntrinsicPropertiesOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: IEntitySymbol) =
        let fields = findImmediatePropertiesOfEntity benv queryMemberFlags valueFlags nameOpt ent
    
        let inheritedProperties =
            ent.Extends
            |> Seq.map (fun x ->
                match x.TryEntity with
                | ValueSome x ->
                    findIntrinsicPropertiesOfEntity benv queryMemberFlags valueFlags nameOpt x
                | _ ->
                    Seq.empty
            )
            |> Seq.concat
            |> filterValuesByAccessibility benv queryMemberFlags
    
        Seq.append inheritedProperties fields
    
    let findFieldsOfType (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) queryField (ty: TypeSymbol) =
        let intrinsicFields =
            match stripTypeEquations ty with
            | TypeSymbol.Variable(tyPar) ->
                tyPar.Constraints
                |> Seq.choose (function
                    | ConstraintSymbol.Null
                    | ConstraintSymbol.Struct
                    | ConstraintSymbol.NotStruct 
                    | ConstraintSymbol.Unmanaged 
                    | ConstraintSymbol.ConstantType _ -> None
                    | ConstraintSymbol.SubtypeOf(ty) -> Some ty.Value
                )
                |> Seq.collect(fun ent ->
                    filterFields queryMemberFlags valueFlags nameOpt ent.Fields
                )
            | TypeSymbol.Entity(ent) ->
                findIntrinsicFieldsOfEntity benv queryMemberFlags valueFlags nameOpt ent
            | _ ->
                Seq.empty
    
        let extrinsicFields =
            match queryField with
            | QueryField.IntrinsicAndExtrinsic ->
                match benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty) with
                | ValueSome (traitImpls) ->
                    traitImpls.Values
                    |> Seq.collect (fun trImpl ->
                        filterFields queryMemberFlags valueFlags nameOpt trImpl.Fields
                    )
                | _ ->
                    Seq.empty
            | _ ->
                Seq.empty
    
        Seq.append intrinsicFields extrinsicFields

    let findPropertiesOfType (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) queryField (ty: TypeSymbol) =
        let intrinsicProps =
            match stripTypeEquations ty with
            | TypeSymbol.Variable(tyPar) 
            | TypeSymbol.HigherVariable(tyPar, _) ->
                tyPar.Constraints
                |> Seq.choose (function
                    | ConstraintSymbol.Null
                    | ConstraintSymbol.Struct
                    | ConstraintSymbol.NotStruct 
                    | ConstraintSymbol.Unmanaged 
                    | ConstraintSymbol.ConstantType _ -> None
                    | ConstraintSymbol.SubtypeOf(ty) -> Some ty.Value
                )
                |> Seq.collect(fun ent ->
                    filterProperties queryMemberFlags valueFlags nameOpt ent.Properties
                )
            | TypeSymbol.Entity(ent) ->
                findIntrinsicPropertiesOfEntity benv queryMemberFlags valueFlags nameOpt ent
            | _ ->
                Seq.empty
    
        let extrinsicProps =
            match queryField with
            | QueryField.IntrinsicAndExtrinsic ->
                let results1 =
                    match benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty) with
                    | ValueSome (traitImpls) ->
                        traitImpls.Values
                        |> Seq.collect (fun trImpl ->
                            filterProperties queryMemberFlags valueFlags nameOpt trImpl.Properties
                        )
                    | _ ->
                        Seq.empty

                let results2 =
                    match benv.senv.typeExtensionMembers.TryFind(stripTypeEquationsAndBuiltIn ty) with
                    | ValueSome extMembers ->
                        extMembers.Values
                        |> Seq.choose (function
                            | ExtensionMemberSymbol.Property prop -> 
                                // REVIEW: We do a similar thing when looking for extension member functions in 'findExtensionMembersOfType',
                                //         is there a way to combine these together so we do not have to repeat this logic?
                                let tyArgs = ty.TypeArguments
                                let enclosing = applyEnclosing tyArgs prop.Enclosing
                                let prop = actualProperty enclosing tyArgs prop
                                Some prop
                            | _ -> 
                                None
                        )
                        |> filterProperties queryMemberFlags valueFlags nameOpt
                    | _ ->
                        Seq.empty

                Seq.append results1 results2
            | _ ->
                Seq.empty
    
        Seq.append intrinsicProps extrinsicProps
        |> filterValuesByAccessibility benv queryMemberFlags

    type Locals = System.Collections.Generic.HashSet<int64>

    type Iterator(predicate, canCache, checkInnerLambdas, freeLocals: FreeLocals, locals: Locals) =
        inherit BoundTreeVisitor(BoundTreeVisitorCore())

        let checkValue (value: IValueSymbol) =
            value.IsLocal && not value.IsStaticLocalFunction && predicate value && not (locals.Contains(value.Id))

        static member HandlePossibleLambda(predicate: IValueSymbol -> bool, canCache, checkInnerLambdas, expr, freeLocals: FreeLocals, locals: Locals) =
            match expr with
            | BoundExpression.Lambda(pars=pars;freeLocals=freeLocalsRef;body=bodyExpr) ->
                for i = 0 to pars.Length - 1 do
                    let par = pars.[i]
                    if predicate par then
                        if not (locals.Add(par.Id)) then 
                            failwith "Local already added"
                    
                let iterator = Iterator(predicate, canCache, checkInnerLambdas, freeLocals, locals)
                if not bodyExpr.HasExpression then
                    bodyExpr.Run()
                iterator.VisitExpression(bodyExpr.Expression) |> ignore

            | _ ->
                let iterator = Iterator(predicate, canCache, checkInnerLambdas, freeLocals, locals)
                iterator.VisitExpression(expr) |> ignore

        override _.VisitLocalBindingInfo(bindingInfo) =
            let value = bindingInfo.Value
            //OlyAssert.Equal(value.Formal, value)
            if not value.IsStaticLocalFunction && not value.IsTargetJump && predicate value && not (locals.Add(value.Id)) then 
                failwithf "Local already added - name: %s id: %i" value.Name value.Id
            true            

        override this.VisitExpression(expr) =
            match expr with
            | BoundExpression.Lambda(flags=flags) ->
                if checkInnerLambdas then
                    Iterator.HandlePossibleLambda((fun x -> not(locals.Contains(x.Formal.Id)) && predicate x), canCache, true, expr, freeLocals, locals)
                    

                false

            | BoundExpression.Value(syntaxInfo=syntaxInfo;value=value) ->
                if checkValue value.Formal then
                    freeLocals.[value.Formal.Id] <- (syntaxInfo.Syntax.TryName, value.Formal)
                base.VisitExpression(expr)

            // TODO: We shouldn't use the Call's syntax, we need the value syntax
            | BoundExpression.Call(syntaxInfo=syntaxInfo;syntaxValueNameOpt=syntaxNameOpt;value=value) ->
                let syntax = syntaxInfo.Syntax
                if checkValue value.Formal then
                    let syntaxOpt =
                        match syntaxNameOpt with
                        | Some syntaxName -> Some syntaxName
                        | _ ->
                            match syntax with
                            | :? OlySyntaxExpression as syntax ->
                                match syntax with
                                | OlySyntaxExpression.Name(syntaxName) -> Some syntaxName
                                | OlySyntaxExpression.Call(OlySyntaxExpression.Name(syntaxName), _) -> Some syntaxName
                                | _ -> None
                            | _ -> None
                    freeLocals.[value.Formal.Id] <- (syntaxOpt, value.Formal)
                base.VisitExpression(expr)

            | BoundExpression.SetValue(value=value) ->
                if checkValue value.Formal then
                    freeLocals.[value.Formal.Id] <- (None, value.Formal)
                base.VisitExpression(expr)
                    
            | _ ->
                base.VisitExpression(expr)

    let computeFreeImmutableLocals (expr: BoundExpression) =
        let freeLocals = FreeLocals()
        let locals = Locals()

        Iterator.HandlePossibleLambda((fun x -> not x.IsMutable && (not x.IsFunction)), false, true, expr, freeLocals, locals)

        ReadOnlyFreeLocals(freeLocals)

    let computeFreeMutableLocals (expr: BoundExpression) =
        let freeLocals = FreeLocals()
        let locals = Locals()

        Iterator.HandlePossibleLambda((fun x -> x.IsMutable && (not x.IsFunction)), false, true, expr, freeLocals, locals)

        ReadOnlyFreeLocals(freeLocals)

    let computeFreeLocals (expr: BoundExpression) =
        let freeLocals = FreeLocals()
        let locals = Locals()

        Iterator.HandlePossibleLambda((fun x -> not x.IsFunction), false, true, expr, freeLocals, locals)

        ReadOnlyFreeLocals(freeLocals)

    let computeImmediateFreeLocals (expr: BoundExpression) =
        let freeLocals = FreeLocals()
        let locals = Locals()

        Iterator.HandlePossibleLambda((fun _ -> true), false, false, expr, freeLocals, locals)

        ReadOnlyFreeLocals(freeLocals)
    
    let getFreeInferenceVariablesFromType add (ty: TypeSymbol) =
        let rec implType ty =
            match stripTypeEquations ty with
            | TypeSymbol.InferenceVariable(_, solution) ->
                add solution.Id ty
            | TypeSymbol.HigherInferenceVariable(_, tyArgs, _, solution) ->
                for i = 0 to tyArgs.Length - 1 do
                    implType tyArgs.[i]
                add solution.Id ty
            | TypeSymbol.HigherVariable(_, tyArgs) ->
                for i = 0 to tyArgs.Length - 1 do
                    implType tyArgs.[i]
            | TypeSymbol.NativeFunctionPtr(_, argTys, returnTy)
            | TypeSymbol.Function(argTys, returnTy) ->
                argTys
                |> ImArray.iter implType
                implType returnTy
            | TypeSymbol.ForAll(tyPars, innerTy) ->
                // TODO: What to do with the type parameters?
                implType innerTy
            | TypeSymbol.Tuple(tyArgs, _) ->
                tyArgs |> Seq.iter implType
            | TypeSymbol.Entity(ent) ->
                for i = 0 to ent.TypeArguments.Length - 1 do
                    implType ent.TypeArguments.[i]
            | _ ->
                ()
    
        implType ty
    
    let getFreeInferenceVariables (expr: BoundExpression) =
        let inputs = ResizeArray<struct(int64 * TypeSymbol)>()
    
        let addInput id item =
            let exists =
                inputs |> Seq.exists (fun struct(id2, _) -> id = id2)
            if not exists then
                inputs.Add(struct(id, item))
    
        let implType ty = getFreeInferenceVariablesFromType addInput ty

        let handleLiteral (literal: BoundLiteral) =
            // TODO:
            ()

        let rec handlePattern (pat: BoundCasePattern) =
            match pat with
            | BoundCasePattern.Discard _ -> ()
            | BoundCasePattern.Literal(_, _, literal) -> handleLiteral literal
            | BoundCasePattern.Tuple(_, pats) ->
                pats |> ImArray.iter handlePattern
            | BoundCasePattern.Local(_, _, value) ->
                implType value.Type
            | BoundCasePattern.Function(_, _, func, _, pats) ->
                implType func.Type
                pats |> ImArray.iter handlePattern
            | BoundCasePattern.FieldConstant(_, _, field) ->
                implType field.Type

        let rec handleMatchPattern (matchPat: BoundMatchPattern) =
            match matchPat with
            | BoundMatchPattern.Cases(_, pats) ->
                pats |> ImArray.iter handlePattern
            | BoundMatchPattern.Or(_, lhsPat, rhsPat) ->
                handleMatchPattern lhsPat
                handleMatchPattern rhsPat
    
        let rec handleExpression expr =
            match expr with
            | BoundExpression.Let(rhsExpr=rhsExpr;bodyExpr=bodyExpr) ->
                handleExpression rhsExpr
                handleExpression bodyExpr

            | BoundExpression.Sequential(_, left, right) ->
                handleExpression left
                handleExpression right

            | BoundExpression.IfElse(_, conditionExpr, trueTargetExpr, falseTargetExpr, _) ->
                handleExpression conditionExpr
                handleExpression trueTargetExpr
                handleExpression falseTargetExpr

            | BoundExpression.Match(_, _, matchExprs, matchClauses, _) ->
                matchExprs
                |> ImArray.iter handleExpression

                matchClauses
                |> ImArray.iter (function
                    | BoundMatchClause.MatchClause(_, matchPat, conditionExprOpt, targetExpr) ->
                        handleMatchPattern matchPat
                        conditionExprOpt |> Option.iter handleExpression
                        handleExpression targetExpr
                )

            | BoundExpression.Typed(_, bodyExpr, ty) ->
                handleExpression bodyExpr
                implType ty

            | BoundExpression.Value(value=value) ->
                if value.IsLocal then
                    implType value.Type

            | BoundExpression.Literal(_, literal) ->
                handleLiteral literal

            | BoundExpression.Call(receiverOpt=receiverOpt;args=args;value=value) ->
                args |> Seq.iter (fun arg -> handleExpression arg)
                receiverOpt
                |> Option.iter (fun receiver -> handleExpression receiver)
                implType value.Type

            | BoundExpression.Lambda(body=bodyExpr;cachedLambdaTy=cachedLambdaTy) ->
                if not bodyExpr.HasExpression then bodyExpr.Run()
                handleExpression bodyExpr.Expression
                implType cachedLambdaTy.Type

            | _ ->
                ()
    
        handleExpression expr
        inputs

    let getFreeTypeParametersFromType (tySet: TypeSymbolMutableSet) (existing: HashSet<int64>) add (ty: TypeSymbol) =
        ty.TypeParameters
        |> ImArray.iter (fun x ->
            existing.Add(x.Id) |> ignore
        )

        let rec implType ty =
            if tySet.Add(ty) then
                match stripTypeEquations ty with
                | TypeSymbol.Variable(tyPar) ->
                    if existing.Contains(tyPar.Id) |> not then
                        add tyPar
                | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
                    if existing.Contains(tyPar.Id) |> not then
                        add tyPar
                    for i = 0 to tyArgs.Length - 1 do
                        implType tyArgs.[i]
                | TypeSymbol.NativeFunctionPtr(_, argTys, returnTy)
                | TypeSymbol.Function(argTys, returnTy) ->
                    argTys
                    |> ImArray.iter implType
                    implType returnTy
                | TypeSymbol.ForAll(tyPars, innerTy) ->
                    tyPars
                    |> ImArray.iter (fun x ->
                        existing.Add(x.Id) |> ignore
                    )
                    implType innerTy
                | TypeSymbol.Tuple(tyArgs, _) ->
                    tyArgs |> Seq.iter implType
                | TypeSymbol.Entity(ent) ->
                    for i = 0 to ent.TypeArguments.Length - 1 do
                        implType ent.TypeArguments.[i]
                | _ ->
                    let tyTyArgs = ty.TypeArguments
                    for i = 0 to tyTyArgs.Length - 1 do
                        implType tyTyArgs[i]

        ty.Fields
        |> ImArray.iter (fun x -> implType x.Type)

        ty.Functions
        |> ImArray.iter (fun x -> implType x.Type)
    
        implType ty

[<AutoOpen>]
module private FreeVariablesHelper =

    type Variables = System.Collections.Generic.HashSet<int64>

    type Iterator(freeVars: FreeVariables, vars: Variables) =
        inherit BoundTreeVisitor(BoundTreeVisitorCore())

        let ignoredValues = Variables()

        let addTyPars (tyPars: TypeParameterSymbol imarray) =
            tyPars
            |> ImArray.iter (fun tyPar ->
                vars.Add(tyPar.Id) |> ignore
            )

        let rec visitType (ty: TypeSymbol) =
            match ty.TryTypeParameter with
            | ValueSome tyPar ->
                if not (vars.Contains tyPar.Id) then
                    freeVars.[tyPar.Id] <- tyPar
            | _ -> ()

            match stripTypeEquations ty with
            | TypeSymbol.NativeFunctionPtr(_, argTys, returnTy)
            | TypeSymbol.Function(argTys, returnTy) ->
                argTys
                |> ImArray.iter visitType
                visitType returnTy
            | TypeSymbol.ForAll(tyPars, innerTy) ->
                addTyPars tyPars
                visitType innerTy
            | _ ->
                if not ty.IsTypeConstructor then
                    ty.TypeArguments
                    |> ImArray.iter (fun ty -> visitType ty)

        override this.CanVisit(expr) =
            match expr with
            | BoundExpression.EntityDefinition _ 
            | BoundExpression.MemberDefinition _ -> false
            | _ -> true

        override this.VisitExpression(expr) =
            match expr with
            | BoundExpression.EntityDefinition _ 
            | BoundExpression.MemberDefinition _ -> false
            | BoundExpression.Lambda(tyPars=tyPars;pars=pars;freeVars=freeVarsRef) ->
                addTyPars tyPars

                pars
                |> ImArray.iter (fun par ->
                    visitType par.Type
                )
                base.VisitExpression(expr)

            | BoundExpression.Value(value=value) ->
                if not (ignoredValues.Contains(value.Id)) then
                    visitType value.Type
                base.VisitExpression(expr)

            | BoundExpression.Let(bindingInfo=bindingInfo) ->
                bindingInfo.Value.TypeParameters
                |> ImArray.iter (fun tyPar -> vars.Add(tyPar.Id) |> ignore)
                base.VisitExpression(expr)

            | BoundExpression.Call(value=value) ->
                value.TypeArguments
                |> ImArray.iter (fun tyArg ->
                    visitType tyArg
                )

                // We do not want to check the formal value if it has type parameters.
                if not value.TypeParameters.IsEmpty then
                    ignoredValues.Add(value.Formal.Id) |> ignore
                    visitType value.Type

                base.VisitExpression(expr)
                    
            | _ ->
                base.VisitExpression(expr)

    let computeFreeTypeVariables (expr: BoundExpression) =
        let find expr =
            let freeVars = FreeVariables()
            let vars = Variables()

            let iterator = Iterator(freeVars, vars)
            iterator.VisitExpression(expr) |> ignore

            ReadOnlyFreeTypeVariables(freeVars)

        match expr with
        | BoundExpression.Lambda(freeVars=freeVarsRef) ->
            match !freeVarsRef with
            | ValueSome(freeVars) -> freeVars
            | _ ->
                let freeVars = find expr
                freeVarsRef := ValueSome(freeVars)
                freeVars
        | _ ->
            find expr

type BoundExpression with

    /// Gets all the free locals within the body of the expression.
    /// Does not include mutable or function values.
    member this.GetFreeImmutableLocals() =
        computeFreeImmutableLocals this

    /// Gets all the free mutable locals within the body of the expression.
    member this.GetFreeMutableLocals() =
        computeFreeMutableLocals this

    member this.GetFreeLocals() =
        computeFreeLocals this

    /// Gets all the free locals within the body of the expression; will not traverse inner lambda expressions hence "Immediate".
    /// Used for checking individual lambda expressions to see if they contain any locals.
    member this.GetImmediateFreeLocals() =
        computeImmediateFreeLocals this

    member this.GetFreeInferenceVariables() =
        getFreeInferenceVariables this

    member this.GetFreeTypeVariables() =
        computeFreeTypeVariables this

    /// Does not include type variables from types themselves; only function type parameters.
    /// Sorted by index.
    member this.GetLogicalFreeTypeVariables() =
        this.GetFreeTypeVariables().Values
        |> Seq.filter (fun x ->
            x.Kind <> TypeParameterKind.Type
        )
        |> Seq.sortBy (fun x -> x.Index) 
        |> ImArray.ofSeq

    member this.GetLogicalFreeAnyLocals() =
        this.GetFreeLocals()
        |> Seq.map (fun x -> x.Value |> snd)
        |> ImArray.ofSeq

type IEntitySymbol with

    member this.GetInstanceFields() =
        this.Fields
        |> ImArray.filter (fun x -> x.IsInstance)

    member this.FindMostSpecificIntrinsicFunctions(benv: BoundEnvironment, queryMemberFlags, funcFlags) =
        findMostSpecificIntrinsicFunctionsOfEntity benv queryMemberFlags funcFlags None this

    member this.FindMostSpecificIntrinsicFunctions(benv: BoundEnvironment, queryMemberFlags, funcFlags, name) =
        findMostSpecificIntrinsicFunctionsOfEntity benv queryMemberFlags funcFlags (Some name) this

    member this.FindIntrinsicFields(benv, queryMemberFlags) =
        findIntrinsicFieldsOfEntity benv queryMemberFlags ValueFlags.None None this

    member this.FindIntrinsicFields(benv, queryMemberFlags, name) =
        findIntrinsicFieldsOfEntity benv queryMemberFlags ValueFlags.None (Some name) this

    member this.FindIntrinsicProperties(benv, queryMemberFlags) =
        findIntrinsicPropertiesOfEntity benv queryMemberFlags ValueFlags.None None this

    member this.FindNestedEntities(benv: BoundEnvironment) =
        this.Entities
        // TODO: Handle accessibility

    member this.TryFindNestedEntity(benv, name, tyArity: ResolutionTypeArity) =
        let nestedEntOpt =
            this.FindNestedEntities(benv)
            |> ImArray.tryFind (fun x -> 
                match tyArity.TryArity with
                | ValueSome n -> x.LogicalTypeParameterCount = n
                | _ -> true
                && x.Name = name
            )
        match nestedEntOpt with
        | Some nestedEnt ->         
            Some nestedEnt
        | _ ->
            None

type TypeSymbol with

    member this.ReplaceInferenceVariablesWithError() =
        match stripTypeEquations this with
        | TypeSymbol.InferenceVariable(tyParOpt, _)
        | TypeSymbol.HigherInferenceVariable(tyParOpt=tyParOpt) -> TypeSymbol.Error(tyParOpt)
        | _ ->
            let tyArgs =
                this.TypeArguments
                |> ImArray.map (fun tyArg ->
                    tyArg.ReplaceInferenceVariablesWithError()
                )
            if tyArgs.IsEmpty then
                this
            else
                applyType this.Formal tyArgs

    member this.GetFreeTypeParameters() : TypeParameterSymbol imarray =
        let builder = ImArray.builder()
        getFreeTypeParametersFromType (TypeSymbolMutableSet.Create()) (HashSet()) builder.Add this
        builder.ToImmutable()

    member this.GetInstanceFields() =
        this.Fields
        |> ImArray.filter (fun x -> x.IsInstance)

    member this.FindIntrinsicFunctions(benv, queryMemberFlags, funcFlags) =
        findMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags None this

    member this.FindIntrinsicFunctions(benv, queryMemberFlags, funcFlags, name) =
        findMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags (Some name) this

    member this.FindIntrinsicFields(benv, queryMemberFlags) =
        match this.TryEntity with
        | ValueSome(ent) ->
            ent.FindIntrinsicFields(benv, queryMemberFlags)
        | _ ->
            Seq.empty

    member this.FindIntrinsicFields(benv, queryMemberFlags, name) =
        match this.TryEntity with
        | ValueSome(ent) ->
            ent.FindIntrinsicFields(benv, queryMemberFlags, name)
        | _ ->
            Seq.empty

    member this.FindFields(benv, queryMemberFlags, queryField) =
        findFieldsOfType benv queryMemberFlags ValueFlags.None None queryField this

    member this.FindFields(benv, queryMemberFlags, queryField, name) =
        findFieldsOfType benv queryMemberFlags ValueFlags.None (Some name) queryField this

    member this.FindProperties(benv, queryMemberFlags, queryField) =
        findPropertiesOfType benv queryMemberFlags ValueFlags.None None queryField this

    member this.FindProperties(benv, queryMemberFlags, queryField, name) =
        findPropertiesOfType benv queryMemberFlags ValueFlags.None (Some name) queryField this

    member this.FindFunctions(benv, queryMemberFlags, funcFlags, queryFunc) =
        findMostSpecificFunctionsOfType benv queryMemberFlags funcFlags None queryFunc this

    member this.FindFunctions(benv, queryMemberFlags, funcFlags, queryFunc, name) =
        findMostSpecificFunctionsOfType benv queryMemberFlags funcFlags (Some name) queryFunc this

    member this.TryFindNestedEntity(benv: BoundEnvironment, name, tyArity) =
        match benv.TryGetEntity this with
        | ValueSome ent -> ent.TryFindNestedEntity(benv, name, tyArity)
        | _ -> None
        
type IValueSymbol with

    member this.IsFunctionGroup =
        match this with
        | :? FunctionGroupSymbol -> true
        | _ -> false

type BoundTree with

    member this.ForEachForTooling(filter: IBoundNode -> bool, f: IBoundNode -> unit) =
        let mutable iterator = Unchecked.defaultof<BoundTreeVisitor>
        let visitor =
            { new BoundTreeVisitorCore() with

                override _.VisitPattern(pattern) =
                    if filter pattern then
                        f pattern
                        true
                    else
                        false

                override _.VisitBinding(binding) =
                    if filter binding then
                        f binding
                        true
                    elif binding.IsGenerated then
                        match binding with
                        | BoundBinding.Implementation(_, bindingInfo, rhsExpr) ->
                            iterator.VisitExpression(rhsExpr) |> ignore
                            iterator.VisitBindingInfo(bindingInfo) |> ignore
                        | BoundBinding.Signature(_, bindingInfo) ->
                            iterator.VisitBindingInfo(bindingInfo) |> ignore
                        false
                    else
                        false

                override _.VisitExpression(expr) =
                    if expr.IsGenerated then
                        match expr with
                        | BoundExpression.Call(receiverOpt=receiverOpt;args=args) ->
                            for i = 0 to args.Length - 1 do
                                iterator.VisitExpression(args.[i]) |> ignore
                            receiverOpt
                            |> Option.iter (fun receiver -> iterator.VisitExpression(receiver) |> ignore)
                        
                        | BoundExpression.Lambda(body=body) ->
                            Assert.ThrowIfNot(body.HasExpression)
                            iterator.VisitExpression(body.Expression) |> ignore
                        
                        | BoundExpression.SetField(_, receiver, _, _, rhs) ->
                            iterator.VisitExpression(receiver) |> ignore
                            iterator.VisitExpression(rhs) |> ignore

                        | BoundExpression.EntityDefinition(body=body) ->
                            iterator.VisitExpression(body) |> ignore

                        | BoundExpression.Sequential(_, expr1, expr2) ->
                            iterator.VisitExpression(expr1) |> ignore
                            iterator.VisitExpression(expr2) |> ignore

                        | BoundExpression.MemberDefinition(_, binding) ->
                            iterator.VisitBinding(binding) |> ignore

                        | BoundExpression.Let(bindingInfo=bindingInfo;rhsExpr=rhsExpr;bodyExpr=bodyExpr) ->
                            iterator.VisitExpression(rhsExpr) |> ignore
                            iterator.VisitLocalBindingInfo(bindingInfo) |> ignore
                            iterator.VisitExpression(bodyExpr) |> ignore
                        
                        | _ ->
                            ()
                        false
                    elif filter expr then
                        f expr
                        true
                    else
                        false

                override _.VisitRoot(root) =
                    if filter root then
                        f root
                        true
                    else
                        false
            }

        iterator <- BoundTreeVisitor(visitor)
        
        iterator.VisitRoot(this.Root) |> ignore

    member this.ForEachExpression(f: BoundExpression -> unit) =
        let mutable iterator = Unchecked.defaultof<BoundTreeVisitor>
        let visitor =
            { new BoundTreeVisitorCore() with

                override _.VisitPattern(pattern) = false

                override _.VisitBinding(binding) =
                    match binding with
                    | BoundBinding.Implementation(_, bindingInfo, rhsExpr) ->
                        iterator.VisitExpression(rhsExpr) |> ignore
                    | _ ->
                        ()
                    false

                override _.VisitExpression(expr) =
                    f expr
                    true

                override _.VisitRoot _ = true
            }

        iterator <- BoundTreeVisitor(visitor)
        
        iterator.VisitRoot(this.Root) |> ignore

    member this.ForEachForTooling(f) =
        this.ForEachForTooling((fun _ -> true), f)

    member this.RewriteExpression(rewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.Rewrite(expr) =
                    rewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        this.UpdateRoot(rewriter.RewriteRoot(this.Root))

    member this.RewriteExpressionPreorder(rewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.PreorderRewrite(expr) =
                    rewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        this.UpdateRoot(rewriter.RewriteRoot(this.Root))

    member this.RewriteExpression(preorderRewrite, postorderRewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.PreorderRewrite(expr) =
                    preorderRewrite expr

                override _.Rewrite(expr) =
                    postorderRewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        this.UpdateRoot(rewriter.RewriteRoot(this.Root))

type BoundExpression with

    member this.Rewrite(rewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.Rewrite(expr) =
                    rewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    member this.Rewrite(rewritePreorder, rewritePostorder, canRewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.Rewrite(expr) =
                    rewritePostorder expr

                override _.PreorderRewrite(expr) =
                    rewritePreorder expr

                override _.CanRewrite(expr) =
                    canRewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    member this.PreorderRewrite(preorderRewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.PreorderRewrite(expr) =
                    preorderRewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    member this.PreorderRewrite(canRewrite, preorderRewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.CanRewrite(expr) =
                    canRewrite expr

                override _.PreorderRewrite(expr) =
                    preorderRewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    member this.Rewrite(canRewrite, rewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.CanRewrite(boundNode) =
                    canRewrite boundNode

                override _.Rewrite(expr) =
                    rewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    static member TryImplicitCall_LoadFunction(argExpr: BoundExpression, argTy: TypeSymbol) =
        if argTy.IsFunction_t then
            let argExprTy = argExpr.Type
            if argExprTy.IsClosure then
                let cloInvoke = argExprTy.GetClosureInvoke()
                let funcExpr =
                    BoundExpression.CreateValue(
                        argExpr.Syntax.Tree,
                        cloInvoke
                    )
                let resultExpr =
                    WellKnownExpressions.LoadFunction
                        argExpr
                        funcExpr
                        cloInvoke.LogicalType
                resultExpr
            else
                match argExpr with
                | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, (BoundExpression.Value(_, value) as bodyExpr)) when bindingInfo.Value.Id = value.Id && value.IsStaticLocalFunction ->
                    let resultExpr =
                        WellKnownExpressions.LoadStaticFunction
                            bodyExpr
                            argTy
                    BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, resultExpr)
                | _ ->
                    argExpr
        else
            argExpr

    static member TryImplicitCalls_LoadFunction(argExprs: BoundExpression imarray, value: IValueSymbol) =
        let logicalTy = value.LogicalType
        let argTys = logicalTy.FunctionArgumentTypes
        (argExprs, argTys)
        ||> ImArray.map2 (fun argExpr argTy ->                                   
            BoundExpression.TryImplicitCall_LoadFunction(argExpr, argTy)
        )

type IFunctionSymbol with

    member this.IsInline =
        this.FunctionFlags &&& FunctionFlags.InlineMask = FunctionFlags.Inline

    member this.IsNotInline =
        this.FunctionFlags &&& FunctionFlags.InlineMask = FunctionFlags.NotInline

// ************************************************************************

/// Associates constraints with their corresponding syntax, because
/// it is by-design that internal symbols do not reference syntax nodes directly.
/// This relies on the order in which the constraints are added to the type parameter,
/// which is not unsual for associations of other syntax and symbols.
/// There have been other approaches to this problem such as storing the associations
/// as part of bound definitions themselves, but that introduces complexity and makes
/// the tree be more error-prone when the tree is transformed in lowering.
let forEachConstraintBySyntaxConstraintClause (syntaxConstrClauses: OlySyntaxConstraintClause imarray) (tyPars: TypeParameterSymbol imarray) f =
    syntaxConstrClauses
    |> ImArray.iter (fun syntaxConstrClause ->
        match syntaxConstrClause with
        | OlySyntaxConstraintClause.ConstraintClause(_, syntaxTy, _, _) ->
            match syntaxTy with
            | OlySyntaxType.Name(syntaxName) ->
                match syntaxName with
                | OlySyntaxName.Identifier(syntaxIdent) ->
                    let ident = syntaxIdent.ValueText
                    match tyPars |> ImArray.tryFind (fun x -> x.Name = ident) with
                    | Some tyPar ->
                        let constrs = 
                            // non-second-order generic constraints
                            tyPar.Constraints 
                            |> ImArray.filter (function ConstraintSymbol.SubtypeOf(lazyTy) when lazyTy.Value.IsTypeConstructor -> false | _ -> true)
                        f syntaxConstrClause tyPar constrs
                    | _ ->
                        ()
                | OlySyntaxName.Generic(OlySyntaxName.Identifier(syntaxIdent), _) ->
                    let ident = syntaxIdent.ValueText
                    match tyPars |> ImArray.tryFind (fun x -> x.Name = ident) with
                    | Some tyPar ->
                        let constrs = 
                            // second-order generic constraints
                            tyPar.Constraints 
                            |> ImArray.filter (function ConstraintSymbol.SubtypeOf(lazyTy) when lazyTy.Value.IsTypeConstructor -> true | _ -> false)
                        f syntaxConstrClause tyPar constrs
                    | _ ->
                        ()
                | _ ->
                    ()
            | _ ->
                ()
        | _ ->
            ()
    )

// ************************************************************************

let subsumesShapeMembersWith benv rigidity queryFunc (superShapeTy: TypeSymbol) (ty: TypeSymbol) =
    OlyAssert.True(superShapeTy.IsShape)

    if ty.IsTypeConstructor then
        Seq.empty
    else

    let funcs = ty.FindFunctions(benv, QueryMemberFlags.StaticOrInstance, FunctionFlags.None, queryFunc) |> ImArray.ofSeq
    superShapeTy.FindIntrinsicFunctions(benv, QueryMemberFlags.StaticOrInstance, FunctionFlags.None)
    |> Seq.map (fun superFunc ->
        let results =
            funcs
            |> ImArray.filter (fun func ->
                if func.IsInstance = superFunc.IsInstance && func.Name = superFunc.Name && func.TypeArguments.Length = superFunc.TypeArguments.Length && func.Parameters.Length = superFunc.Parameters.Length then
                    // TODO: This really isn't right.
                    let isInstance = func.IsInstance
                    if not isInstance || not ty.IsAnyStruct || (if superFunc.IsReadOnly then func.IsReadOnly else true) then
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
                            areLogicalFunctionSignaturesEqual superFunc func
                        else
                            true
                    else
                        false
                else
                    false
            )
        (superFunc, results)
    )

let subsumesTypeOrShapeWith benv rigidity (superTy: TypeSymbol) (ty: TypeSymbol) =
    if superTy.IsShape then
        subsumesShapeWith benv rigidity superTy ty
    else
        subsumesTypeWith rigidity superTy ty

let subsumesTypeOrShape benv superTy ty =
    subsumesTypeOrShapeWith benv Rigid superTy ty

let subsumesShapeWith benv rigidity (superShapeTy: TypeSymbol) (ty: TypeSymbol) =
    OlyAssert.True(superShapeTy.IsShape)
    
    if ty.IsTypeConstructor then
        false
    else
        let areFuncsValid =
            subsumesShapeMembersWith benv rigidity QueryFunction.Intrinsic superShapeTy ty
            |> Seq.forall (fun (_, xs) -> 
                xs.Length = 1
            )

        areFuncsValid

let subsumesShape benv (superShapeTy: TypeSymbol) (ty: TypeSymbol) =
    subsumesShapeWith benv TypeVariableRigidity.Rigid superShapeTy ty

// Similar to subsumesTypeAndUnifyTypes - but includes the bound environment which will take into account type extensions.
let subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity (superTy: TypeSymbol) (ty: TypeSymbol) =
    if subsumesTypeOrShapeWith benv rigidity superTy ty then true
    else
        if superTy.IsTypeConstructor then
            subsumesTypeConstructorWith rigidity superTy ty
        else
            match stripTypeEquations ty, stripTypeEquations superTy with
            | TypeSymbol.ForAll(_, innerTy), superTy when not superTy.IsTypeConstructor ->
                subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity superTy innerTy
            | TypeSymbol.Variable(tyPar), superTy ->
                tyPar.Constraints
                |> Seq.exists (function
                    | ConstraintSymbol.Null
                    | ConstraintSymbol.Struct
                    | ConstraintSymbol.NotStruct 
                    | ConstraintSymbol.Unmanaged -> true
                    | ConstraintSymbol.ConstantType(ty) ->
                        subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity superTy ty.Value
                    | ConstraintSymbol.SubtypeOf(ty) ->
                        subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity superTy ty.Value
                )
            | _ -> 
                false

let stripLiteral (literal: BoundLiteral) =
    match literal with
    | BoundLiteral.NumberInference(lazyLiteral, _) ->
        match lazyLiteral.Value with
        | Ok(literal) -> stripLiteral literal
        | _ -> literal
    | _ ->
        literal

let areLiteralsEqual (literal1: BoundLiteral) (literal2: BoundLiteral) =
    match stripLiteral literal1, stripLiteral literal2 with
    | BoundLiteral.Constant(cns1),
      BoundLiteral.Constant(cns2) -> areConstantsEqual cns1 cns2
    | BoundLiteral.ConstantEnum(cns1, ty1),
      BoundLiteral.ConstantEnum(cns2, ty2) -> areTypesEqual ty1 ty2 && areConstantsEqual cns1 cns2
    | BoundLiteral.NullInference(ty1),
      BoundLiteral.NullInference(ty2) -> areTypesEqual ty1 ty2
    | _ -> false

let areTargetExpressionsEqual (expr1: E) (expr2: E) =
    match expr1, expr2 with
    | E.Call(_, None, witnessArgs1, argExprs1, _, value1, false),
        E.Call(_, None, witnessArgs2, argExprs2, _, value2, false) ->
        value1.IsFunction &&
        areValueSignaturesEqual value1 value2 && 
        argExprs1.Length = argExprs2.Length &&
        (witnessArgs1.GetValue(None, CancellationToken.None).IsEmpty) &&
        (witnessArgs2.GetValue(None, CancellationToken.None).IsEmpty) &&
        (argExprs1, argExprs2)
        ||> ImArray.forall2 (fun expr1 expr2 ->
            match expr1, expr2 with
            | E.Value(value=value1), E.Value(value=value2) when value1.IsLocal && not value1.IsMutable ->
                areValueSignaturesEqual value1 value2
            | E.Literal(_, literal1), E.Literal(_, literal2) ->
                areLiteralsEqual literal1 literal2
            | _ ->
                false
        )
    | E.Value(value=value1), E.Value(value=value2) when value1.IsLocal && not value1.IsMutable ->
        areValueSignaturesEqual value1 value2
    | E.Literal(_, literal1), E.Literal(_, literal2) ->
        areLiteralsEqual literal1 literal2
    | _ ->
        false