[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Pass3

open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.SymbolQuery
open Oly.Compiler.Internal.SymbolQuery.Extensions

// Pass 3 check for duplicates
let bindTypeDeclarationPass3 (cenv: cenv) (env: BinderEnvironment) (entities: EntitySymbolBuilder imarray) syntaxAttrs syntaxIdent syntaxConstrClauses syntaxTyDefBody =
    let envBody = unsetSkipCheckTypeConstructor env

    let entBuilder = entities.[cenv.entityDefIndex]
    cenv.entityDefIndex <- 0

    let ent = entBuilder.Entity

    checkConstraintClauses (SolverEnvironment.Create(cenv.diagnostics, envBody.benv, cenv.pass)) syntaxConstrClauses ent.TypeParameters

    let attrs = bindAttributes cenv envBody true syntaxAttrs
    entBuilder.SetAttributes(cenv.pass, attrs)

    if not ent.Extends.IsEmpty then
        let superTy = ent.Extends.[0]
        match superTy.TryEntity, ent.TryFindDefaultInstanceConstructor() with
        | ValueSome(superEnt), Some(ctor) when ctor.FunctionFlags &&& FunctionFlags.ImplicitDefaultConstructor = FunctionFlags.ImplicitDefaultConstructor ->
            if not superEnt.HasDefaultInstanceConstructor then
                cenv.diagnostics.Error($"The type '{printEntity envBody.benv ent}' cannot implicitly create a default constructor as its base type '{printEntity envBody.benv superEnt}' does not have a default constructor.", 10, syntaxIdent)
        | _ ->
            ()

    let _env: BinderEnvironment = bindTypeDeclarationBodyPass3 cenv envBody entBuilder.NestedEntityBuilders entBuilder false syntaxTyDefBody

    if ent.IsNewtype then
        if ent.GetInstanceFields().Length <> 1 then
            cenv.diagnostics.Error($"Newtype '{ent.Name}' must have only a single field.", 10, syntaxIdent)

    env

let bindTypeDeclarationBodyPass3 (cenv: cenv) (env: BinderEnvironment) entities (entBuilder: EntitySymbolBuilder) isRoot syntaxTyDeclBody =
    let env = env.SetResolutionMustSolveTypes()

    let ent = entBuilder.Entity

    let env = unsetSkipCheckTypeConstructor env
    let env = env.SetAccessorContext(ent)
    let env = env.SetEnclosing(EnclosingSymbol.Entity(ent))
    let env = openContentsOfEntityAndOverride env OpenContent.All ent

    let funcs = 
        ent.FindMostSpecificIntrinsicFunctions(env.benv, QueryMemberFlags.StaticOrInstance, FunctionFlags.None)
        |> ImArray.filter (fun x ->
            if x.IsConstructor || (x.IsStatic && not x.IsNewSlot) then
                false
            else
                match x.Enclosing.TryEntity with
                | Some ent2 -> 
                    if ent2.Id = ent.Id then
                        false
                    else
                        if ent.IsTypeExtension && not ent.Extends.IsEmpty then
                            subsumesType ent2.AsType ent.Extends.[0]
                            |> not
                        else
                            true
                | _ -> true
        )
        |> ImArray.filter (fun x ->
            if x.IsVirtual then
                // Only include sealed functions as we may override non-sealed virtual functions.
                x.IsFinal
            else
                true
        )

    let fieldOrPropSet =
        ent.FindIntrinsicFields(env.benv, QueryMemberFlags.StaticOrInstance)
        |> Seq.filter (fun x ->
            match x.Enclosing.TryEntity with
            | Some ent -> ent.Id <> entBuilder.Entity.Id
            | _ -> true
        )
        |> Seq.map (fun x ->
            x.Name
        )
        |> HashSet

    ent.FindIntrinsicProperties(env.benv, QueryMemberFlags.StaticOrInstance)
    |> Seq.filter (fun x ->
        match x.Enclosing.TryEntity with
        | Some ent -> ent.Id <> entBuilder.Entity.Id
        | _ -> true
    )
    |> Seq.iter (fun x ->
        fieldOrPropSet.Add(x.Name) |> ignore
    )

    let inheritedFuncSet = FunctionSignatureMutableSet.Create(funcs)
    let funcSet = FunctionSignatureMutableSet.Create([])

    let implicitDefaultCtors =
        entBuilder.Entity.Functions
        |> ImArray.filter (fun x -> x.FunctionFlags &&& FunctionFlags.ImplicitDefaultConstructor = FunctionFlags.ImplicitDefaultConstructor)

    let hasImplicitInstanceDefaultCtor =
        implicitDefaultCtors
        |> ImArray.exists (fun x -> x.IsInstanceConstructor)

    let hasImplicitStaticDefaultCtor =
        implicitDefaultCtors
        |> ImArray.exists (fun x -> x.IsStaticConstructor)

    let doesFuncAlreadyExist (func: IFunctionSymbol) =
        if (funcSet.Add(func) |> not) then
            true
        else
            match inheritedFuncSet.TryGet func with
            | ValueSome inheritedFunc ->
                if func.IsNewSlot then
                    false
                else
                    true
            | _ ->
                false

    let env =
        match syntaxTyDeclBody with
        | OlySyntaxTypeDeclarationBody.Body(syntaxExtends, syntaxImplements, _, syntaxBodyExpr) ->

            // Check type constructors of extends.
            match syntaxExtends with
            | OlySyntaxExtends.Inherits(_, syntaxTyList) ->
                (syntaxTyList.ChildrenOfType, ent.Extends)
                ||> ImArray.tryIter2 (fun syntaxTy ty ->
                    checkTypeConstructorDepthWithType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxTy ty
                )
            | _ ->
                ()

            // Check type constructors of implements.
            match syntaxImplements with
            | OlySyntaxImplements.Implements(_, syntaxTyList) ->
                (syntaxTyList.ChildrenOfType, ent.Implements)
                ||> ImArray.tryIter2 (fun syntaxTy ty ->
                    checkTypeConstructorDepthWithType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxTy ty
                )
            | _ ->
                ()

            bindTopLevelExpressionPass3 cenv env isRoot entities syntaxBodyExpr
            |> fst
        | _ ->
            env

    let syntaxMemberDecls = syntaxTyDeclBody.GetMemberDeclarations()
    (syntaxMemberDecls, entBuilder.Bindings)
    ||> ImArray.iter2 (fun (syntaxAttrs, syntax) (binding, isImpl) ->
        let attrs = bindAttributes cenv env true syntaxAttrs
        let attrs = addImportAttributeIfNecessary binding.Value.Enclosing binding.Value.Name attrs

        match binding.Value with
        | :? FunctionSymbol as func -> 
            func.SetAttributes_Pass3_NonConcurrent(attrs)
            match syntax with
            | OlySyntaxBindingDeclaration.Function(_, _, syntaxPars, _, _) ->
                let syntaxPars = syntaxPars.Values
                let logicalPars = func.LogicalParameters
                // REVIEW: We are having to iterate through all the parameters
                //         to bind the attributes. The performance is probably ok.
                //         But, if we do not want to iterate the parameters again,
                //         we could have a list of syntax attributes along with the parameter index per parameter
                //         as part of the 'entBuilder.Bindings'.
                if syntaxPars.Length = logicalPars.Length then
                    for i = 0 to syntaxPars.Length - 1 do
                        let syntaxPar = syntaxPars[i]
                        let logicalPar = logicalPars[i]
                        match syntaxPar with
                        | OlySyntaxParameter.Pattern(syntaxAttrs, _, _, _, _)
                        | OlySyntaxParameter.Type(syntaxAttrs, _) ->
                            match syntaxAttrs with
                            | OlySyntaxAttributes.Empty _ -> ()
                            | _ ->
                                match logicalPar with
                                | :? LocalParameterSymbol as par ->
                                    par.SetAttributes_Pass3_NonConcurrent(bindAttributes cenv env true syntaxAttrs)
                                | _ ->
                                    ()
                        | _ ->
                            ()
            | _ ->
                ()
                
        | :? FieldSymbol as field -> 
            field.SetAttributes_Pass3_NonConcurrent(attrs)
        | _ -> ()

        let duplicateError (value: IValueSymbol) syntaxNode =
            if not value.IsInvalid then
                cenv.diagnostics.Error(sprintf "'%s' has duplicate member definitions." (printValue env.benv value), 10, syntaxNode)

        let tryOverride (func: FunctionSymbol) =
            let mostSpecificFuncs =
                let tys = ent.ExtendsAndImplementsForMemberOverriding
                tys
                |> ImArray.map (fun ty ->
                    ty.FindIntrinsicFunctions(env.benv, QueryMemberFlags.StaticOrInstance, FunctionFlags.None, func.Name)
                )
                |> Seq.concat
                |> ImArray.ofSeq
                |> filterMostSpecificFunctions
                |> ImArray.filter (fun x -> 
                    x.IsVirtual && 
                    not x.IsFinal && 
                    func.IsProtected = x.IsProtected &&
                    (
                        if func.IsNewSlot then
                            x.IsAbstract
                        else
                            true
                    )
                )
                |> distinctFunctions
                |> ImArray.filter (fun overridenFunc ->
                    areLogicalFunctionSignaturesEqual func overridenFunc
                )
            
            if mostSpecificFuncs.Length > 1 then
                cenv.diagnostics.Error($"The member '{func.Name}' is ambiguous to override.", 10, syntax.Identifier)
                true
            elif mostSpecificFuncs.Length = 1 then
                let overridenFunc = mostSpecificFuncs.[0]

                OlyAssert.Equal(func.IsInstance, overridenFunc.IsInstance)
                
                if func.IsInstance then
                    if overridenFunc.Enclosing.IsInterface then
                        if func.IsVirtual then
                            if func.Enclosing.IsInterface then
                                // This forces the function be final.
                                func.SetFinal_Pass3()
                        else
                            if not func.Enclosing.IsInterface then
                                // This forces the function to be a virtual/final/new-slot if it could override an interface function.
                                func.SetVirtualFinalNewSlot_Pass3()

                if not func.IsVirtual then
                    cenv.diagnostics.Error($"The member '{func.Name}' will hide over its base.", 10, syntax.Identifier)
                    false
                else
                    if areLogicalFunctionSignaturesEqual func overridenFunc then
                        func.SetOverrides_Pass3_NonConcurrent(overridenFunc)
                        let func = func :> IFunctionSymbol

                        (func.TypeParameters, overridenFunc.TypeParameters)
                        ||> ImArray.tryIter2 (fun tyPar1 tyPar2 ->
                            if tyPar1.Constraints.Length = tyPar2.Constraints.Length then
                                (tyPar1.Constraints, tyPar2.Constraints)
                                ||> ImArray.iter2 (fun constr1 constr2 ->
                                    if not(areConstraintsEqualWith Indexable constr1 constr2) then
                                        cenv.diagnostics.Error($"'{printConstraint env.benv constr1}' constraint does not exist on the overriden function's type parameter '{printType env.benv tyPar2.AsType}'.", 10, syntax.Identifier)
                                )
                            else
                                cenv.diagnostics.Error($"'{func.Name}' type parameter constraints do not match its overriden function.", 10, syntax.Identifier)
                        )

                        true
                    else
                        false
            else
                false

        let checkFunc (func: IFunctionSymbol) =
            match func.FunctionOverrides with
            | Some overridenFunc when overridenFunc.Enclosing.IsInterface -> ()
            | _ ->
                duplicateError func syntax.Identifier

        let rec handleFunc (func: FunctionSymbol) =
            if func.IsPatternFunction then
                match func.AssociatedFormalPattern with
                | Some pat ->
                    match pat.PatternGuardFunction with
                    | Some guardFunc ->
                        handleFunc (guardFunc :?> FunctionSymbol)
                    | _ -> ()
                | _ -> ()

            if tryOverride func then
                if not func.IsExplicitOverrides then
                    checkFunc func
            else
                if func.IsExplicitOverrides then
                    cenv.diagnostics.Error($"The function '{printValue env.benv func}' cannot find a function to override.", 10, syntax.Identifier)
                else
                    if doesFuncAlreadyExist func then
                        duplicateError func syntax.Identifier

        if binding.Value.IsProperty then           
            if not ent.IsInterface then
                match binding.Value with
                | :? IPropertySymbol as prop ->

                    let mustCheckExistsGetter =
                        match prop.Getter with
                        | Some getter ->
                            let getter = getter :?> FunctionSymbol
                            if tryOverride getter then
                                if not getter.IsExplicitOverrides then
                                    checkFunc getter
                                false
                            else
                                if getter.IsExplicitOverrides then
                                    cenv.diagnostics.Error($"The property '{printValue env.benv prop}' cannot find a 'get' to override.", 10, syntax.Identifier)
                                    false
                                else
                                    true
                        | _ ->
                            false

                    let mustCheckExistsSetter =
                        match prop.Setter with
                        | Some setter ->
                            let setter = setter :?> FunctionSymbol
                            if tryOverride setter then
                                if not setter.IsExplicitOverrides then
                                    checkFunc setter
                                false
                            else
                                if setter.IsExplicitOverrides then
                                    cenv.diagnostics.Error($"The property '{printValue env.benv prop}' cannot find a 'set' to override.", 10, syntax.Identifier)
                                    false
                                else
                                    prop.Getter.IsNone
                        | _ ->
                            false

                    if mustCheckExistsGetter || mustCheckExistsSetter then
                        if fieldOrPropSet.Add(binding.Value.Name) |> not then
                            duplicateError binding.Value syntax.Identifier
                | _ ->
                    ()

        elif binding.Value.IsField then
            if fieldOrPropSet.Add(binding.Value.Name) then
                if not ent.IsNewtype && not isImpl && ((binding.Value.IsInstance && hasImplicitInstanceDefaultCtor) || (not binding.Value.IsInstance && hasImplicitStaticDefaultCtor)) then
                    cenv.diagnostics.Error($"The field '{binding.Value.Name}' must be given a default value.", 10, syntax.Identifier)

                if not ent.IsModule && isImpl && not hasImplicitInstanceDefaultCtor && binding.Value.IsInstance then
                    cenv.diagnostics.Error($"The field '{binding.Value.Name}' must not be given a default value.", 10, syntax.Identifier)

                if ent.IsNewtype then
                    if binding.Value.IsMutable then
                        cenv.diagnostics.Error($"The field '{binding.Value.Name}' cannot be mutable on newtypes.", 10, syntax.Identifier)
            else
                duplicateError binding.Value syntax.Identifier

        elif binding.Value.IsFunction then
            handleFunc (binding.Value :?> FunctionSymbol)

        // Check constraints.
        match binding with
        | BindingFunction(func)
        | BindingPattern(_, func) when not func.TypeParameters.IsEmpty ->
            match syntax with
            | OlySyntaxBindingDeclaration.Function(_, _, _, _, syntaxConstrClauseList) ->
                checkConstraintClauses (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxConstrClauseList.ChildrenOfType func.TypeParameters
            | _ ->
                ()
        | _ ->
            ()
            
        // Check type constructors.
        match binding with
        | BindingField(field=field) ->
            match syntax with
            | OlySyntaxBindingDeclaration.Value(_, syntaxReturnTyAnnot) ->
                match syntaxReturnTyAnnot with
                | OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxReturnTy) ->
                    checkTypeConstructorDepthWithType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxReturnTy field.Type
                | _ ->
                    ()
            | _ ->
                ()
        | BindingProperty(prop=prop) ->
            match syntax with
            // TODO:
            | _ ->
                ()
        | BindingFunction(func)
        | BindingPattern(_, func) ->
            match syntax with
            | OlySyntaxBindingDeclaration.Function(_, _, syntaxPars, syntaxReturnTyAnnot, _) ->
                let syntaxParTys =
                    match syntaxPars with
                    | OlySyntaxParameters.Parameters(_, syntaxParList, _) ->
                        syntaxParList.ChildrenOfType
                        |> ImArray.choose (fun x ->
                            match x with
                            | OlySyntaxParameter.Pattern(_, _, _, _, syntaxTy) ->
                                syntaxTy
                                |> Some
                            | OlySyntaxParameter.Type(_, syntaxTy) ->
                                syntaxTy
                                |> Some
                            | _ ->
                                None
                        )
                    | _ ->
                        ImArray.empty

                (syntaxParTys.AsMemory(), func.LogicalParameters)
                ||> ROMem.tryIter2 (fun syntaxParTy par ->
                    checkTypeConstructorDepthWithType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxParTy par.Type
                )

                match syntaxReturnTyAnnot with
                | OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxReturnTy) ->
                    checkTypeConstructorDepthWithType (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxReturnTy func.ReturnType
                | _ ->
                    ()
            | _ ->
                ()
    )

    env

let private bindTopLevelExpressionPass3 (cenv: cenv) (env: BinderEnvironment) (canOpen: bool) (entities: EntitySymbolBuilder imarray) (syntaxExpr: OlySyntaxExpression) : BinderEnvironment * bool =
    cenv.ct.ThrowIfCancellationRequested()

    match syntaxExpr with
    | OlySyntaxExpression.OpenDeclaration _ 
    | OlySyntaxExpression.OpenStaticDeclaration _
    | OlySyntaxExpression.OpenExtensionDeclaration _ ->
        // We re-bind to check constraints and to open contents again.
        let enclosing = currentEnclosing env
        if canOpen && enclosing.IsNamespaceOrModule then
            bindOpenDeclaration cenv env canOpen OpenContent.Values syntaxExpr, canOpen
        else
            env, canOpen

    | OlySyntaxExpression.Sequential(syntaxExpr1, syntaxExpr2) ->
        let env1, canOpen = bindTopLevelExpressionPass3 cenv env canOpen entities syntaxExpr1
        bindTopLevelExpressionPass3 cenv env1 canOpen entities syntaxExpr2

    | OlySyntaxExpression.TypeDeclaration(syntaxAttrs, _, _, syntaxTyDefName, _, syntaxConstrClauseList, _, syntaxTyDefBody) ->
        let prevEntityDefIndex = cenv.entityDefIndex
        let env = bindTypeDeclarationPass3 cenv env entities syntaxAttrs syntaxTyDefName.Identifier syntaxConstrClauseList.ChildrenOfType syntaxTyDefBody
        cenv.entityDefIndex <- prevEntityDefIndex + 1
        env, false

    | _ ->
        env, false

