﻿[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Environment

open System.Threading
open System.Collections.Immutable
open System.Collections.Generic
open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments

[<NoEquality;NoComparison>]
type cenv =
    {
        mutable bindAnonymousShapeTypeHole: cenv -> BinderEnvironment -> TypeParameterSymbol imarray -> OlySyntaxSeparatorList<OlySyntaxExpression> -> TypeSymbol
        declTable: BoundDeclarationTable ref
        asm: AssemblySymbol
        syntaxTree: OlySyntaxTree
        diagnostics: OlyDiagnosticLogger
        ct: CancellationToken
        pass: CompilerPass
        mutable entryPoint: IFunctionSymbol option
        mutable entityDefIndex: int
        mutable memberDefIndex: int
    }

let recordValueDeclaration cenv value (syntaxNode: OlySyntaxNode) =
    cenv.declTable.contents <- cenv.declTable.contents.SetValueDeclaration(value, syntaxNode.GetLocation())

let recordEntityDeclaration cenv ent (syntaxNode: OlySyntaxNode) =
    cenv.declTable.contents <- cenv.declTable.contents.SetEntityDeclaration(ent, syntaxNode.GetLocation())
    
let recordTypeParameterDeclaration cenv tyPar (syntaxNode: OlySyntaxNode) =
    cenv.declTable.contents <- cenv.declTable.contents.SetTypeParameterDeclaration(tyPar, syntaxNode.GetLocation())

let rec private getTopLevelEnclosingType (enclosing: EnclosingSymbol) =
    OlyAssert.True(enclosing.IsType)
    match enclosing with
    | EnclosingSymbol.Entity(ent) when ent.Enclosing.IsType ->
        getTopLevelEnclosingType ent.Enclosing
    | _ ->
        enclosing

type BinderEnvironment =
    {
       benv: BoundEnvironment

       // Context info
       // TODO: Put context info into a separate data type
       isIntrinsic: bool
       isInInstanceConstructorType: TypeSymbol option
       isInEntityDefinitionTypeParameters: bool
       isInFunctionDefinitionTypeParameters: bool
       isInConstraint: bool
       isInOpenDeclaration: bool
       isInLocalLambda: bool
       resolutionMustSolveTypes: bool
       skipCheckTypeConstructor: bool
       skipTypeExtensionBinding: bool

       contextTypeOrTypeConstructor: TypeSymbol option

       isReturnable: bool
       isPassedAsArgument: bool

       // TODO: Get rid of this...
       implicitThisOpt: ILocalParameterSymbol option

       isExecutable: bool
    }

    member this.SetIsInInstanceConstructorType(ent: EntitySymbol) =
        { this with isInInstanceConstructorType = Some(TypeSymbol.Entity(ent)) }

    member this.UnsetIsInInstanceConstructorType() =
        if this.isInInstanceConstructorType.IsSome then
            { this with isInInstanceConstructorType = None }
        else
            this

    member this.ClearLocals_unused() =
        let benv = this.benv.ClearLocals_unused()

        // If unchanged, do not construct a new one.
        if obj.ReferenceEquals(this.benv, benv) then
            this
        else
            { this with benv = benv }

    /// Context type is not the enclosing type, but rather using certain expressions like "let!"
    /// will be understood by the given type.
    /// The context type will be set with the formal type of the given type.
    member this.SetContextType(ty: TypeSymbol) =
        { this with contextTypeOrTypeConstructor = Some ty.Formal }

    /// If set to 'false', will also set 'PassedAsArgument' to 'false'.
    member this.SetReturnable(isReturnable: bool) =
        if this.isReturnable = isReturnable then 
            if isReturnable then
                this
            elif this.isPassedAsArgument then
                { this with isPassedAsArgument = false }
            else
                this
        else
            if isReturnable then
                { this with isReturnable = true }
            else
                { this with isReturnable = false; isPassedAsArgument = false }

    member this.SetPassedAsArgument(isPassedAsArgument: bool) =
        if this.isPassedAsArgument = isPassedAsArgument then this
        else
            { this with isPassedAsArgument = isPassedAsArgument }

    member this.EnclosingTypeParameters =
        this.benv.EnclosingTypeParameters

    member this.GetEnclosingTypeParametersAsTypes() =
        this.benv.EnclosingTypeParameters
        |> ImArray.map (fun x -> x.AsType)

    member this.SetIntrinsicType(ty: TypeSymbol, ent: EntitySymbol) =
        if ent.IsAlias then
            let env = 
                { this with
                    benv = 
                        { this.benv with
                            senv = 
                                { this.benv.senv with
                                    intrinsicTypesByAliasTypes = this.benv.senv.intrinsicTypesByAliasTypes.SetItem(ent.AsType, ty)
                                    aliasTypesByIntrinsicTypes = this.benv.senv.aliasTypesByIntrinsicTypes.SetItem(ty, ent.AsType)
                                }
                        }
                }
            env
        else
            let env = 
                { this with
                    benv = 
                        { this.benv with
                            senv = 
                                { this.benv.senv with
                                    entitiesByIntrinsicTypes = this.benv.senv.entitiesByIntrinsicTypes.SetItem(ty, ent)
                                }
                        }
                }
            env

    member this.TryFindConcreteEntityByType(ty: TypeSymbol) =
        match ty.TryEntity with
        | ValueSome ent -> ValueSome ent
        | _ -> this.TryFindIntrinsicType ty

    member this.TryFindIntrinsicType(ty) =
        this.benv.TryFindEntityByIntrinsicType(ty)

    member this.TryFindAliasedIntrinsicType(ty) =
        this.benv.TryFindEntityByIntrinsicType(ty)

    member this.TryGetEntity(ty) =
        this.benv.TryGetEntity(ty)

    member this.AddNamespace(ent: EntitySymbol) =
        if not ent.IsNamespace then
            failwith "Expected namespace."

        let group =
            match this.benv.senv.namespaces.TryGetValue(ent.FullNamespacePath) with
            | true, group -> group
            | _ -> AggregatedNamespaceSymbol(ent.Name, ent.Enclosing, ImArray.empty)

#if DEBUG || CHECKED
        let exists = group.Namespaces |> ImArray.exists (fun x -> obj.ReferenceEquals(x, ent))
        if exists then failwith "assert"
#endif

        let namespaces = this.benv.senv.namespaces.SetItem(ent.FullNamespacePath, group.AddNamespace(ent))
        { this with
            benv = 
                { this.benv with
                    senv = 
                        { this.benv.senv with
                            namespaces = namespaces                        
                        }
                }
        }

    member this.AddUnqualifiedType(name, arity, ty: TypeSymbol) =
        if arity < 0 then
            invalidArg (nameof(arity)) "Less than zero."

        match ty.TryEntity with
        | ValueSome ent when ent.IsNamespace -> failwith "Cannot add a namespace as a type."
        | _  ->

#if DEBUG || CHECKED
        if ty.IsTypeConstructor then
            OlyAssert.True(ty.Arity >= arity)
#endif

        let arityGroup =
            match this.benv.senv.unqualifiedTypes.TryGetValue arity with
            | true, arityGroup -> arityGroup
            | _ -> NameMap.empty
        let arityGroup = 
            // Type variables and locals will override what's in scope.
            if ty.IsTypeVariable || ty.Enclosing.IsLocalEnclosing then
                arityGroup.SetItem(name, ImArray.createOne ty)
            else
                match arityGroup.TryGetValue(name) with
                | true, tys ->
#if DEBUG || CHECKED
                    let exists = 
                        tys 
                        |> ImArray.exists (fun x -> 
                            (
                                if x.IsAlias then
                                    match ty.TryEntity with
                                    | ValueSome _ ->
                                        areEntitiesEqual x.AsEntity ty.AsEntity
                                    | _ ->
                                        false
                                else
                                    true
                            ) &&
                            areTypesEqual x ty
                        )
                    OlyAssert.False(exists)
#endif                  
                    arityGroup.SetItem(name, tys.Add(ty))
                | _ ->
                    arityGroup.SetItem(name, ImArray.createOne ty)

        { this with
            benv = 
                { this.benv with
                    senv = 
                        { this.benv.senv with
                            unqualifiedTypes =
                                this.benv.senv.unqualifiedTypes.SetItem(arity, arityGroup)
                        }
                }
        }

    /// Will override what's in scope.
    member this.SetUnqualifiedType(name, arity, ty: TypeSymbol) =
        if arity < 0 then
            invalidArg (nameof(arity)) "Less than zero."

        match ty.TryEntity with
        | ValueSome ent when ent.IsNamespace && not(ty.IsAlias) -> 
            OlyAssert.Fail("Cannot add a namespace as a type.")
        | _ ->

#if DEBUG || CHECKED
        if ty.IsTypeConstructor then
            OlyAssert.True(ty.Arity >= arity)
#endif

        let arityGroup =
            match this.benv.senv.unqualifiedTypes.TryGetValue arity with
            | true, arityGroup -> arityGroup
            | _ -> NameMap.empty
        let arityGroup = arityGroup.SetItem(name, ImArray.createOne ty)

#if DEBUG || CHECKED
        arityGroup.Values
        |> Seq.iter (fun tys ->
            tys
            |> ImArray.iter (fun ty ->
                OlyAssert.True(ty.IsSolved)
                if ty.IsTypeExtension then
                    if ty.Inherits.Length > 0 then
                        OlyAssert.True(ty.Inherits[0].IsSolved)
                        OlyAssert.Equal(1, ty.Inherits.Length)
            )
        )
#endif

        { this with
            benv = 
                { this.benv with
                    senv = 
                        { this.benv.senv with
                            unqualifiedTypes =
                                this.benv.senv.unqualifiedTypes.SetItem(arity, arityGroup)
                        }
                }
        }

    member this.AddParameter(diagnostics: OlyDiagnosticLogger, parameter: ILocalParameterSymbol, syntaxNode: OlySyntaxNode) =
        { this with
            benv = 
                { this.benv with
                    senv = 
                        { this.benv.senv with
                            parameters = 
                                // TODO: Should we do this check elsewhere? Passing syntax nodes as part of an "Add" on the environment feels like it syntax node gets added along with.
                                if (this.benv.TryFindParameter parameter.Name).IsSome then
                                    diagnostics.Error(sprintf "Parameter '%s' already exists." parameter.Name, 0, syntaxNode)
                                    this.benv.senv.parameters
                                else
                                    this.benv.senv.parameters.Add(parameter)
                        }
                }
        }

    member this.SetParameters(pars: ILocalParameterSymbol seq) =
        { this with
            benv = 
                { this.benv with
                    senv = 
                        { this.benv.senv with
                            parameters = ImmutableArray.CreateRange(pars)
                        }
                }
        }

    static member private SetUnqualifiedValue(unqualifiedSymbols: NameMap<UnqualifiedSymbol>, unqualified, value: IValueSymbol) =
        let unqualifiedSymbols =
            if value.IsLocal then
                unqualifiedSymbols.SetItem(value.Name, unqualified)
            else
                match unqualifiedSymbols.TryGetValue(value.Name) with
                | true, current ->
                    match current with
                    | UnqualifiedSymbol.Local _ ->
                        unqualifiedSymbols.SetItem(value.Name, unqualified)
                    | UnqualifiedSymbol.FunctionGroup(funcGroup) ->
                        unqualifiedSymbols.SetItem(value.Name, UnqualifiedSymbol.AmbiguousValues(ImArray.createTwo funcGroup value))
                    | UnqualifiedSymbol.Function(func) ->
                        unqualifiedSymbols.SetItem(value.Name, UnqualifiedSymbol.AmbiguousValues(ImArray.createTwo func value))
                    | UnqualifiedSymbol.Field(field) ->
                        unqualifiedSymbols.SetItem(value.Name, UnqualifiedSymbol.AmbiguousValues(ImArray.createTwo field value))
                    | UnqualifiedSymbol.Property(prop) ->
                        unqualifiedSymbols.SetItem(value.Name, UnqualifiedSymbol.AmbiguousValues(ImArray.createTwo prop value))
                    | UnqualifiedSymbol.AmbiguousValues(values) ->
                        unqualifiedSymbols.SetItem(value.Name, UnqualifiedSymbol.AmbiguousValues(values.Add(value)))
                | _ ->
                    unqualifiedSymbols.SetItem(value.Name, unqualified)
        unqualifiedSymbols

    member this.AddUnqualifiedValue(value: IValueSymbol) =
        this.SetUnqualifiedValueAux(value, true)

    member this.TryAddUnqualifiedValue(value: IValueSymbol) =
        this.SetUnqualifiedValueAux(value, false)

    member this.SetUnqualifiedValueAux(valueToSet: IValueSymbol, canReplace: bool) =
        let isPatFunc = valueToSet.IsFunction && valueToSet.AsFunction.IsPatternFunction
        let unqualifiedSymbols = 
            if isPatFunc then
                this.benv.senv.unqualifiedPatterns
            else
                this.benv.senv.unqualifiedSymbols
        let unqualifiedToSet = valueToSet.ToUnqualified()
        if valueToSet.IsFunction then
            let funcsToSet =
                match unqualifiedToSet with
                | UnqualifiedSymbol.FunctionGroup(funcGroup) -> funcGroup.Functions
                | UnqualifiedSymbol.Function(func) -> ImArray.createOne func
                | _ -> ImArray.empty

#if DEBUG || CHECKED
            funcsToSet
            |> ImArray.iter (fun func ->
                OlyAssert.False(func.IsFunctionGroup)
            )
#endif

            if funcsToSet.IsEmpty then this
            else
                let name = 
                    if valueToSet.IsConstructor then
                        valueToSet.Enclosing.AsEntity.Name
                    else
                        valueToSet.Name
                let create (funcs: IFunctionSymbol imarray) =
                    if funcs.Length = 1 then
                        funcs[0]
                        |> UnqualifiedSymbol.Function
                    else
                        FunctionGroupSymbol.CreateWithDefaultEnclosing(name, funcs, funcs[0].Parameters.Length, isPatFunc)
                        |> UnqualifiedSymbol.FunctionGroup
                let unqualifiedSymbols =
                    let defaultCase() =
                        let unqualified = create funcsToSet
                        unqualifiedSymbols.SetItem(name, unqualified)

                    if (funcsToSet.Length = 1 && funcsToSet[0].Enclosing.IsLocalEnclosing) then
                        defaultCase()
                    else

                    match unqualifiedSymbols.TryGetValue name with
                    | true, currentQualified ->
                        let currentFuncs =
                            match currentQualified with
                            // Merge functions.
                            | UnqualifiedSymbol.FunctionGroup(currentFuncGroup) -> currentFuncGroup.Functions
                            | UnqualifiedSymbol.Function(currentFunc) -> ImArray.createOne currentFunc
                            | _ -> ImArray.empty

                        if currentFuncs.IsEmpty then
                            defaultCase()
                        else

#if DEBUG || CHECKED
                            // Check the enclosings to make sure we are not adding values who have the same enclosing multiple times.
                            // We prefer to add values in bulk for an individual enclosing.
                            let exists =
                                currentFuncs
                                |> ImArray.exists (fun x ->
                                    funcsToSet
                                    |> ImArray.exists (fun y ->
                                        areEnclosingsEqual x.Enclosing y.Enclosing

                                        // It is possible to add the same constructor more than once because of aliases. So we ignore it for this assertion.
                                        && not x.IsInstanceConstructor && not y.IsInstanceNotConstructor 
                                    )
                                )
                            OlyAssert.False(exists)
#endif
                            let envEnclosing = this.benv.senv.enclosing
                            if canReplace then
                                let funcs =
                                    currentFuncs
                                    |> ImArray.filter (fun x ->
                                        let exists =
                                            // TODO: this uses indexable rigidity, should we do generalizable instead?
                                            // TODO: PERFORMANCE ISSUES - quadratic, this can be expensive if there are a lot of functions.
                                            //       Consider creating a Map based on number of parameters to improve perf.
                                            funcsToSet
                                            |> ImArray.exists (fun y -> 
                                                if areEnclosingsEqual envEnclosing y.Enclosing then
                                                    areLogicalFunctionSignaturesEqual x y
                                                else
                                                    false
                                            )
                                        not exists
                                    )
                                unqualifiedSymbols.SetItem(name, create (funcs.AddRange(funcsToSet)))
                            else
                                // These enclosings are used to support the check if a function is defined within a current module before the 'open' declarations are processed.
                                // Example:
                                //     module Test.A
                                //
                                //     open Test.B // hypothetically could have its own `TestM(): ()`
                                //      
                                //     #[open]
                                //     module NestedA =
                                //         let TestM(): () = ()
                                // -- 
                                // We need to make sure 'NestedA.TestM' stays in scope and will not be replaced with another function with the same signature from another module.
                                // TODO: Instead of doing it this way, perhaps we could "mark" these unqualified functions i.e. (NestedA.TestM) as not able to be merged with others.
                                //       The marking information would likely have to be stored in the 'UnqualifiedSymbol'.
                                //       But, we should only do it if this would likely be a performance boost.
                                let currentFuncEnclosings =
                                    currentFuncs
                                    |> ImArray.map (fun func ->
                                        getTopLevelEnclosingType func.Enclosing
                                    )
                                let funcsToSet2 =
                                    funcsToSet
                                    |> ImArray.filter (fun x ->
                                        let exists =
                                            // TODO: this uses indexable rigidity, should we do generalizable instead?
                                            // TODO: PERFORMANCE ISSUES - quadratic, this can be expensive if there are a lot of functions.
                                            //       Consider creating a Map based on number of parameters to improve perf.
                                            currentFuncs
                                            |> ImArray.existsi (fun i y ->
                                                if areEnclosingsEqual envEnclosing currentFuncEnclosings[i] then
                                                    areLogicalFunctionSignaturesEqual x y
                                                else
                                                    false
                                            )
                                        not exists
                                    )
                                if funcsToSet2.IsEmpty then
                                    unqualifiedSymbols
                                else
                                    unqualifiedSymbols.SetItem(name, create (currentFuncs.AddRange(funcsToSet2)))
                    | _ ->
                        defaultCase()

                if isPatFunc then
                    { this with
                        benv = 
                            { this.benv with
                                senv = 
                                    { this.benv.senv with
                                        unqualifiedPatterns = unqualifiedSymbols  
                                    }
                            }
                    }
                else
                    { this with
                        benv = 
                            { this.benv with
                                senv = 
                                    { this.benv.senv with
                                        unqualifiedSymbols = unqualifiedSymbols  
                                    }
                            }
                    }
        else
            OlyAssert.False(isPatFunc)
            if valueToSet.IsFieldConstant then
                { this with
                    benv = 
                        { this.benv with
                            senv = 
                                { this.benv.senv with
                                    unqualifiedSymbols = BinderEnvironment.SetUnqualifiedValue(unqualifiedSymbols, unqualifiedToSet, valueToSet)
                                    unqualifiedPatterns = BinderEnvironment.SetUnqualifiedValue(this.benv.senv.unqualifiedPatterns, unqualifiedToSet, valueToSet)
                                }
                        }
                }
            else
                { this with
                    benv = 
                        { this.benv with
                            senv = 
                                { this.benv.senv with
                                    unqualifiedSymbols = BinderEnvironment.SetUnqualifiedValue(unqualifiedSymbols, unqualifiedToSet, valueToSet)
                                }
                        }
                }
                

    member this.SetEnclosing(enclosingSymbol: EnclosingSymbol) =
        match enclosingSymbol, this.benv.senv.enclosing with
        | EnclosingSymbol.Local, EnclosingSymbol.Local -> this
        | _ ->
            { this with
                benv =
                    { this.benv with
                        senv =
                            { this.benv.senv with
                                enclosing = enclosingSymbol
                            }
                    }
            }

    member this.SetAccessorContext(ent: EntitySymbol) =
        if ent.IsNamespace then
            this
        else
            { this with benv = { this.benv with ac = { this.benv.ac with Entity = Some ent }} }

    member this.SetEnclosingTypeParameters(tyPars: TypeParameterSymbol imarray) =
        if tyPars.IsEmpty && this.benv.senv.typeParameters.IsEmpty then this
        else
            { this with
                benv =
                    { this.benv with
                        senv =
                            { this.benv.senv with
                                typeParameters = tyPars
                            }
                    }
            }

    member this.AddTypeExtension(tyExt: EntitySymbol) =
        if not tyExt.IsTypeExtension then
            failwith "Expected a type extension."

        if tyExt.Extends.Length <> 1 then
            this
        else

        let extendsTy =
            match tyExt.Extends.Length = 1 with
            | true -> tyExt.Extends.[0]
            | _ -> failwith "Expecting a type extension that inherits a type."

        let implementsTys = 
            tyExt.AllLogicalImplements
            |> Seq.collect (fun implementsTy -> 
                implementsTy.AllLogicalInheritsAndImplements.Add(implementsTy)
            )
            |> TypeSymbol.Distinct
            |> ImArray.ofSeq

        if implementsTys.IsEmpty then
            // Normal type extension

            let extendsTy = (stripTypeEquationsAndBuiltIn extendsTy).Formal

            let typeExtensionMembers = this.benv.senv.typeExtensionMembers
            let typeExtensionMembers2 =
                let tyExts =
                    match typeExtensionMembers.TryFind(extendsTy) with
                    | ValueSome tyExts -> tyExts
                    | _ -> ExtensionMemberSymbolOrderedSet.Create()

                let funcs =
                    tyExt.Functions
                    |> ImArray.map (fun func -> ExtensionMemberSymbol.Function(func))

                let props =
                    tyExt.Properties
                    |> ImArray.map (fun prop -> ExtensionMemberSymbol.Property(prop))

                let tyExts2 = tyExts.AddRange(funcs.AddRange(props))

                if extendsTy.IsError_t then
                    typeExtensionMembers
                else
                    OlyAssert.True(extendsTy.IsSolved)
                    typeExtensionMembers.SetItem(extendsTy, tyExts2)
            { this with
                benv =
                    { this.benv with
                        senv =
                            { this.benv.senv with
                                typeExtensionMembers = typeExtensionMembers2
                            }
                    }
            }
        else
            // Type extension that implements interfaces

            let typeExtensionsWithImplements = this.benv.senv.typeExtensionsWithImplements
            let typeExtensionsWithImplements2 =
                let tyExts =
                    match typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn extendsTy) with
                    | ValueSome tyExts -> tyExts
                    | _ -> EntitySymbolGeneralizedMapEntitySet.Create()

                let tyExts2 = 
                    (tyExts, implementsTys)
                    ||> Seq.fold (fun tyExts ty ->
                        match ty.TryEntity with
                        | ValueSome withEnt when withEnt.IsInterface ->
                            tyExts.SetItem(withEnt, tyExt)
                        | _ ->
                            tyExts
                    )

                typeExtensionsWithImplements.SetItem(stripTypeEquationsAndBuiltIn extendsTy, tyExts2)
            { this with
                benv =
                    { this.benv with
                        senv =
                            { this.benv.senv with
                                typeExtensionsWithImplements = typeExtensionsWithImplements2
                            }
                    }
            }

    member this.SetEnclosingTypeArguments(id: int64, tyArgs: ImmutableArray<TypeSymbol>) =
        if tyArgs.IsEmpty then this
        else
            { this with
                benv =
                    { this.benv with
                        senv =
                            { this.benv.senv with
                                enclosingTyInst = this.benv.senv.enclosingTyInst.SetItem(id, tyArgs)
                            }
                    }
            }

    member this.SetIsInOpenDeclaration() =
        if this.isInOpenDeclaration then this
        else { this with isInOpenDeclaration = true }

    member this.UnsetIsInOpenDeclaration() =
        if this.isInOpenDeclaration then { this with isInOpenDeclaration = false }
        else this

    member this.SetResolutionMustSolveTypes() =
        if this.resolutionMustSolveTypes then this
        else { this with resolutionMustSolveTypes = true }

    member this.UnsetResolutionMustSolveTypes() =
        if this.resolutionMustSolveTypes then { this with resolutionMustSolveTypes = false }
        else this

    member this.UnqualifiedPatternExists(ident: string) =
        match this.benv.senv.unqualifiedPatterns.TryGetValue ident with
        | true, UnqualifiedSymbol.FunctionGroup funcGroup ->
            funcGroup.Functions
            |> ImArray.exists (fun func ->
                func.IsPatternFunction && func.ReturnType.IsUnit_t
            )
        | true, UnqualifiedSymbol.Function(func) ->
            func.IsPatternFunction && func.ReturnType.IsUnit_t

        | true, UnqualifiedSymbol.Field(field) when field.Constant.IsSome ->
            true

        | _ ->
            false

    member this.HasOpenedEntity(ent: EntitySymbol) =
        this.benv.openedEnts.Contains(ent)

    member this.AddOpenedEntity(ent: EntitySymbol) =
        { this with
            benv = 
                { this.benv with
                    openedEnts = this.benv.openedEnts.Add(ent)
                    partialAutoOpenedRootEnts = this.benv.partialAutoOpenedRootEnts.Remove(ent)
                }
        }

    member this.AddPartialOpenedRootEntity(ent: EntitySymbol) =
        OlyAssert.True(ent.IsNonNamespaceRootInScope(this.benv.ac.AssemblyIdentity))
        { this with
            benv =
                { this.benv with
                    partialAutoOpenedRootEnts = this.benv.partialAutoOpenedRootEnts.Add(ent)
                }
            
        }

let checkSyntaxHigherTypeArguments cenv (syntaxTyArgs: OlySyntaxType imarray) =
    syntaxTyArgs
    |> ImArray.iter (fun syntaxTy ->
        match syntaxTy with
        | OlySyntaxType.WildCard _ -> ()
        | _ -> cenv.diagnostics.Error("Only underscores, '_', are allowed to specify second-order generics.", 10, syntaxTy)
    )

let currentEnclosing env =
    env.benv.senv.enclosing

let canReportMissingIdentifier (cenv: cenv) (identifier: string) =
    if cenv.syntaxTree.HasErrors && identifier = "" then
        false
    else
        true

let typeResolutionArityOfName syntaxName =
    match syntaxName with
    | OlySyntaxName.Generic(_, syntaxTyArgs) ->
        match syntaxTyArgs with
        | OlySyntaxTypeArguments.TypeArguments(_, syntaxTyArgList, _) ->
            let count = syntaxTyArgList.ChildrenOfType.Length
            if count <= 0 then ResolutionTypeArity.Any
            else ResolutionTypeArity.Create(count)
        | _ ->
            ResolutionTypeArity.Any
    | _ ->
        ResolutionTypeArity.Any

let tryEvaluateLazyLiteral (diagnostics: OlyDiagnosticLogger) (lazyLiteral: Lazy<Result<BoundLiteral, OlyDiagnostic>>) : BoundLiteral voption =
    match lazyLiteral.Value with
    | Ok literal ->
        ValueSome literal
    | Error(diag) ->
        diagnostics.AddDiagnostic(diag)
        ValueNone

let setSkipCheckTypeConstructor (env: BinderEnvironment) =
    if env.skipCheckTypeConstructor then env
    else { env with skipCheckTypeConstructor = true }

let unsetSkipCheckTypeConstructor (env: BinderEnvironment) =
    if env.skipCheckTypeConstructor then { env with skipCheckTypeConstructor = false }
    else env
