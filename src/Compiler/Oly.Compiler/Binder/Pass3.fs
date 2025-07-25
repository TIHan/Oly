﻿[<RequireQualifiedAccess>]
module internal rec Oly.Compiler.Internal.Binder.Pass3

open System.Diagnostics
open System.Collections.Generic

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.SymbolQuery
open Oly.Compiler.Internal.SymbolQuery.Extensions
open Oly.Compiler.Internal.Binder.OpenDeclarations
open Oly.Compiler.Internal.Binder.Attributes

type private FakeUnit = FakeUnit

let ForEachBinding projection (syntaxTyDeclBody: OlySyntaxTypeDeclarationBody, bindings: (BindingInfoSymbol * bool) imarray) =
    let mutable bindingIndex = 0
    let rec f (expr: OlySyntaxExpression) cont : FakeUnit =
        match expr with
        | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, _, _, _, syntaxBinding) ->
            let binding = bindings[bindingIndex]
            bindingIndex <- bindingIndex + 1
            let rec addBinding syntaxAttrs syntaxBinding binding =
                match syntaxBinding with
                | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _)
                | OlySyntaxBinding.Signature(syntaxBindingDecl) ->
                    projection syntaxAttrs syntaxBindingDecl binding
                | OlySyntaxBinding.Property(syntaxBindingDecl, syntaxPropBindingList)
                | OlySyntaxBinding.PropertyWithDefault(syntaxBindingDecl, syntaxPropBindingList, _, _) ->
                    match (fst binding) with
                    | BindingInfoSymbol.BindingProperty(innerBindings, _) ->
                        syntaxPropBindingList.ChildrenOfType
                        |> ImArray.iteri (fun i syntaxPropBinding ->
                            match syntaxPropBinding with
                            | OlySyntaxPropertyBinding.Binding(syntaxAttrs, _, _, _, _, syntaxBinding) ->
                                let isImpl =
                                    match syntaxBinding with
                                    | OlySyntaxBinding.Signature _ -> false
                                    | _ -> true
                                projection syntaxAttrs syntaxBinding.Declaration (innerBindings[i],isImpl)
                            | _ ->
                                unreached()
                        )
                    | _ ->
                        failwith "Expected a property binding."  
                    projection syntaxAttrs syntaxBindingDecl binding

                | OlySyntaxBinding.PatternWithGuard(syntaxBindingDecl, _) ->
                    // TODO: What about the guard?
                    projection syntaxAttrs syntaxBindingDecl binding
                | _ ->
                    ()
            addBinding syntaxAttrs syntaxBinding binding
            cont(FakeUnit)
        | OlySyntaxExpression.Sequential(expr1, expr2) ->
            f expr1 (fun FakeUnit ->
                f expr2 cont
            )
        | _ ->
            cont(FakeUnit)

    syntaxTyDeclBody.Children
    |> ImArray.iter (function
        | :? OlySyntaxExpression as expr ->
            f expr id |> ignore
        | _ ->
            ()
    )

(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)

// Pass 3 check for duplicates
let bindTypeDeclaration (cenv: cenv) (env: BinderEnvironment) (entities: EntitySymbolBuilder imarray) syntaxAttrs syntaxIdent syntaxConstrClauses syntaxTyDefBody =
    let envBody = unsetSkipCheckTypeConstructor env

    let entBuilder = entities.[cenv.entityDefIndex]
    cenv.entityDefIndex <- 0

    let ent = entBuilder.Entity

    checkConstraintClauses (SolverEnvironment.Create(cenv.diagnostics, envBody.benv, cenv.pass)) syntaxConstrClauses ent.TypeParameters

    let attrs = bindAttributes cenv envBody syntaxAttrs

    // IMPORTANT: Be careful when trying to look at a type's attributes when it may not have been fully populated.
    //            In this case, it is OK because we always populate the attributes for the parent first before the children.
    let attrs = Pass2.addExportAttributeIfNecessary cenv syntaxIdent ent.Enclosing attrs
    entBuilder.SetAttributes(cenv.pass, attrs)

    if not ent.Extends.IsEmpty then
        let superTy = ent.Extends.[0]
        match superTy.TryEntity, ent.TryFindDefaultInstanceConstructor() with
        | ValueSome(superEnt), Some(ctor) when ctor.FunctionFlags &&& FunctionFlags.ImplicitDefaultConstructor = FunctionFlags.ImplicitDefaultConstructor ->
            if not superEnt.HasDefaultInstanceConstructor then
                cenv.diagnostics.Error($"The type '{printEntity envBody.benv ent}' cannot implicitly create a default constructor as its base type '{printEntity envBody.benv superEnt}' does not have a default constructor.", 10, syntaxIdent)
        | _ ->
            ()

    let _env: BinderEnvironment = bindTypeDeclarationBody cenv envBody entBuilder.NestedEntityBuilders entBuilder false syntaxTyDefBody

    if ent.IsNewtype then
        if ent.GetInstanceFields().Length <> 1 then
            cenv.diagnostics.Error($"Newtype '{ent.Name}' must have only a single field.", 10, syntaxIdent)

    env

let bindTypeDeclarationBody (cenv: cenv) (env: BinderEnvironment) entities (entBuilder: EntitySymbolBuilder) isRoot syntaxTyDeclBody =
    let env = env.SetResolutionMustSolveTypes()

    let ent = entBuilder.Entity

    let env = unsetSkipCheckTypeConstructor env
    let env = env.SetAccessorContext(ent)
    let env = env.SetEnclosing(EnclosingSymbol.Entity(ent))
    let env = openContentsOfEntityAndOverride cenv.declTable.contents env OpenContent.All ent

    let inheritedFuncs = 
        ent.FindMostSpecificIntrinsicFunctions(env.benv, QueryMemberFlags.StaticOrInstance, FunctionFlags.None)
        |> ImArray.filter (fun inheritedFunc ->
            if inheritedFunc.IsConstructor || (inheritedFunc.IsStatic && not inheritedFunc.IsNewSlot) then
                false
            else
                match inheritedFunc.Enclosing.TryEntity with
                | Some ent2 -> 
                    if ent2.FormalId = ent.FormalId then
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
        ent.FindIntrinsicFields(env.benv.ac, QueryMemberFlags.StaticOrInstance)
        |> Seq.filter (fun x ->
            match x.Enclosing.TryEntity with
            | Some ent -> ent.FormalId <> entBuilder.Entity.FormalId
            | _ -> true
        )
        |> Seq.map (fun x ->
            x.Name
        )
        |> HashSet

    ent.FindIntrinsicProperties(env.benv, QueryMemberFlags.StaticOrInstance)
    |> Seq.filter (fun x ->
        match x.Enclosing.TryEntity with
        | Some ent -> ent.FormalId <> entBuilder.Entity.FormalId
        | _ -> true
    )
    |> Seq.iter (fun x ->
        fieldOrPropSet.Add(x.Name) |> ignore
    )

    let inheritedFuncSet = FunctionSignatureMutableSet.Create(inheritedFuncs)
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
            inheritedFuncSet.Contains(func)

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

            bindTopLevelExpression cenv env isRoot entities syntaxBodyExpr
            |> fst
        | _ ->
            env

    let rec processMember (syntaxAttrs, syntax) (binding: BindingInfoSymbol, isImpl) =
        let attrs = bindAttributes cenv env syntaxAttrs
        let attrs = Pass2.addImportAttributeIfNecessary binding.Value.Enclosing binding.Value.Name attrs
        let attrs = Pass2.addExportAttributeIfNecessary cenv syntax binding.Value.Enclosing attrs

        // IPatternSymbol should not show up here, only the function of it.
        OlyAssert.False(binding.Value.IsPattern)

        match binding.Value with
        | :? FunctionSymbol as func -> 
            func.SetAttributes_Pass3_NonConcurrent(cenv.pass, attrs)
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
                                    par.SetAttributes_Pass3_NonConcurrent(bindAttributes cenv env syntaxAttrs)
                                | _ ->
                                    ()
                        | _ ->
                            ()
            | _ ->
                ()
                
        | :? FieldSymbol as field -> 
            field.SetAttributes_Pass3_NonConcurrent(attrs)

        | :? PropertySymbol as prop ->
            prop.SetAttributes_Pass3_NonConcurrent(cenv.pass, attrs)

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

            let mostSpecificFuncs =
                if mostSpecificFuncs.Length > 1 then
                    let nonInterfaceMostSpecificFuncs =
                        // If we have ambiguities 
                        mostSpecificFuncs
                        |> ImArray.filter (fun x -> not x.Enclosing.IsInterface)
                    if nonInterfaceMostSpecificFuncs.Length > 0 then
                        nonInterfaceMostSpecificFuncs
                    else
                        mostSpecificFuncs
                else
                    mostSpecificFuncs
            
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
                        func.SetOverrides_Pass3_NonConcurrent(cenv.pass, overridenFunc)
                        let func = func :> IFunctionSymbol

                        (func.TypeParameters, overridenFunc.TypeParameters)
                        ||> ImArray.tryIter2 (fun tyPar1 tyPar2 ->
                            let mutable constraintsMatch = true

                            if tyPar1.Constraints.Length = tyPar2.Constraints.Length then
                                (tyPar1.Constraints, tyPar2.Constraints)
                                ||> ImArray.iter2 (fun constr1 constr2 ->
                                    if not(areConstraintsEqualWith Indexable constr1 constr2) then
                                        constraintsMatch <- false
                                )
                            else
                                constraintsMatch <- false

                            if not constraintsMatch then
                                cenv.diagnostics.Error($"'{func.Name}' type parameter constraints do not match its overriden function.\nExpected: {printValue env.benv overridenFunc}\nActual: {printValue env.benv func}", 10, syntax.Identifier)
                        )

                        let isExported = func.IsExported
                        let isOverridenExportedOrImported = overridenFunc.IsExported || overridenFunc.IsImported

                        match isExported, isOverridenExportedOrImported with
                        | true, true
                        | false, false -> ()
                        | true, false ->
                            cenv.diagnostics.Error($"'{func.Name}' cannot be exported because the function its overriding is neither imported or exported.", 10, syntax.Identifier)
                        | false, true ->
                            if func.TypeParameters.IsEmpty |> not then
                                cenv.diagnostics.Error($"'{func.Name}' has type parameters and must be exported with the attribute '#[export]' because the function its overriding is imported or exported.", 10, syntax.Identifier)

                        true
                    else
                        false
            else
                false

        let checkFuncFromNewtype (func: IFunctionSymbol) =
            // Newtypes cannot override anything.
            if func.Enclosing.IsNewtype then
                cenv.diagnostics.Error($"'{func.Name}' cannot be overriden in a newtype declaration.", 10, syntax.Identifier)

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
                if doesFuncAlreadyExist func then
                    duplicateError func syntax.Identifier
                if not func.IsExplicitOverrides then
                    checkFunc func
                else
                    checkFuncFromNewtype func
            else
                if func.IsExplicitOverrides then
                    cenv.diagnostics.Error($"The function '{printValue env.benv func}' cannot find a function to override.", 10, syntax.Identifier)
                else
                    if doesFuncAlreadyExist func then
                        duplicateError func syntax.Identifier

        if binding.Value.IsField then
            if fieldOrPropSet.Add(binding.Value.Name) then
                if not ent.IsNewtype && not isImpl && ((binding.Value.IsInstance && hasImplicitInstanceDefaultCtor) || (not binding.Value.IsInstance && hasImplicitStaticDefaultCtor)) then
                    cenv.diagnostics.Error($"The field '{binding.Value.Name}' must be given a default value.", 10, syntax.Identifier)

                if not ent.IsModule && isImpl && not hasImplicitInstanceDefaultCtor && binding.Value.IsInstance then
                    cenv.diagnostics.Error($"The field '{binding.Value.Name}' must not be given a default value.", 10, syntax.Identifier)
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
        | BindingProperty(binding, prop) ->
            if prop.IsAutoProperty then
                let isImported =
                    match prop.Getter with
                    | Some getter -> getter.IsImported
                    | _ -> false
                let isImported =
                    if isImported then
                        true
                    else
                        match prop.Setter with
                        | Some setter -> setter.IsImported
                        | _ -> false
                if isImported then
                    // REVIEW: This is sort of a hack.
                    //         We have to do this because "AttributeImporter" attributes
                    //         are not resolved until Pass3 and property creation with the backing field
                    //         occurs in Pass2.
                    //         A way to possibly get rid of this hack, is by creating the backing field in Pass3
                    //         instead of Pass2.
                    let backingField = prop.BackingField.Value
                    entBuilder.RemoveField(cenv.pass, backingField)
                    prop.RemoveBackingField_Pass3_NonConcurrent()
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

    (syntaxTyDeclBody, entBuilder.Bindings)
    |> ForEachBinding (fun syntaxAttrs syntaxBindingDecl binding ->
        processMember (syntaxAttrs, syntaxBindingDecl) binding
    )

    env

let private bindTopLevelExpression (cenv: cenv) (env: BinderEnvironment) (canOpen: bool) (entities: EntitySymbolBuilder imarray) (syntaxExpr: OlySyntaxExpression) : BinderEnvironment * bool =
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
        let env1, canOpen = bindTopLevelExpression cenv env canOpen entities syntaxExpr1
        bindTopLevelExpression cenv env1 canOpen entities syntaxExpr2

    | OlySyntaxExpression.TypeDeclaration(syntaxAttrs, _, _, syntaxTyDefName, _, syntaxConstrClauseList, _, syntaxTyDefBody) ->
        let prevEntityDefIndex = cenv.entityDefIndex
        let env = bindTypeDeclaration cenv env entities syntaxAttrs syntaxTyDefName.Identifier syntaxConstrClauseList.ChildrenOfType syntaxTyDefBody
        cenv.entityDefIndex <- prevEntityDefIndex + 1
        env, false

    | _ ->
        env, false

