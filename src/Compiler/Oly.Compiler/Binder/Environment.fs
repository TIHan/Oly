[<AutoOpen>]
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

       // TODO: Get rid of this...
       implicitThisOpt: ILocalParameterSymbol option

       isExecutable: bool
    }

    /// Context type is not the enclosing type, but rather using certain expressions like "let!"
    /// will be understood by the given type.
    /// The context type will be set with the formal type of the given type.
    member this.SetContextType(ty: TypeSymbol) =
        { this with contextTypeOrTypeConstructor = Some ty.Formal }

    member this.SetReturnable(isReturnable: bool) =
        { this with isReturnable = isReturnable }

    member this.EnclosingTypeParameters =
        this.benv.EnclosingTypeParameters

    member this.GetEnclosingTypeParametersAsTypes() =
        this.benv.EnclosingTypeParameters
        |> ImArray.map (fun x -> x.AsType)

    member this.SetIntrinsicType(ty: TypeSymbol, ent: IEntitySymbol) =
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

    member this.TryFindIntrinsicType(ty) =
        this.benv.TryFindEntityByIntrinsicType(ty)

    member this.TryFindAliasedIntrinsicType(ty) =
        this.benv.TryFindEntityByIntrinsicType(ty)

    member this.TryGetEntity(ty) =
        this.benv.TryGetEntity(ty)

    member this.AddNamespace(ent: IEntitySymbol) =
        if not ent.IsNamespace then
            failwith "Expected namespace."

        let group =
            match this.benv.senv.namespaces.TryGetValue(ent.FullNamespacePath) with
            | true, group -> group
            | _ -> AggregatedNamespaceSymbol(ent.Name, ent.Enclosing, ImArray.empty)

#if DEBUG
        let exists = group.Namespaces |> ImArray.exists (fun x -> x.Id = ent.Id)
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
#if DEBUG
                    let exists = tys |> ImArray.exists (fun x -> areTypesEqual x ty && x.FormalId = ty.FormalId)
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

        let arityGroup =
            match this.benv.senv.unqualifiedTypes.TryGetValue arity with
            | true, arityGroup -> arityGroup
            | _ -> NameMap.empty
        let arityGroup = arityGroup.SetItem(name, ImArray.createOne ty)

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

    member this.SetUnqualifiedValue(value: IValueSymbol) =
        let unqualifiedSymbols = this.benv.senv.unqualifiedSymbols
        let unqualified = value.ToUnqualified()
        if value.IsFunction then
            let funcs =
                match unqualified with
                | UnqualifiedSymbol.FunctionGroup(funcGroup) -> funcGroup.Functions
                | UnqualifiedSymbol.Function(func) -> ImArray.createOne func
                | _ -> ImArray.empty

            if funcs.IsEmpty then this
            else
                let name = 
                    if value.IsConstructor then
                        value.Enclosing.AsEntity.Name
                    else
                        value.Name
                let unqualifiedSymbols =
                    let defaultCase() =
                        let unqualified =
                            if funcs.Length = 1 then
                                UnqualifiedSymbol.Function(funcs[0])
                            else
                                let funcGroup = FunctionGroupSymbol(name, funcs, funcs[0].Parameters.Length)
                                UnqualifiedSymbol.FunctionGroup(funcGroup)
                        unqualifiedSymbols.SetItem(name, unqualified)

                    match unqualifiedSymbols.TryGetValue name with
                    | true, result ->
                        match result with
                        // Merge functions.
                        | UnqualifiedSymbol.FunctionGroup(funcGroup) ->
#if DEBUG
                            let exists =
                                funcGroup.Functions
                                |> ImArray.exists (fun x ->
                                    areEnclosingsEqual x.Enclosing value.Enclosing
                                )
                            OlyAssert.False(exists)
#endif

                            let funcGroup = FunctionGroupSymbol(funcGroup.Name, funcGroup.Functions.AddRange(funcs), funcGroup.Functions[0].Parameters.Length)
                            unqualifiedSymbols.SetItem(name, UnqualifiedSymbol.FunctionGroup funcGroup)
                        | UnqualifiedSymbol.Function(func) ->
                            if func.Enclosing.IsLocalEnclosing then
                                unqualifiedSymbols.SetItem(name, UnqualifiedSymbol.Function func)
                            else
#if DEBUG
                            OlyAssert.False(areEnclosingsEqual func.Enclosing value.Enclosing)
#endif

                            let funcGroup = FunctionGroupSymbol(func.Name, funcs |> ImArray.prependOne func, func.Parameters.Length)
                            unqualifiedSymbols.SetItem(name, UnqualifiedSymbol.FunctionGroup funcGroup)
                        | _ ->
                            defaultCase()
                    | _ ->
                        defaultCase()

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
            { this with
                benv = 
                    { this.benv with
                        senv = 
                            { this.benv.senv with
                                unqualifiedSymbols =
                                    unqualifiedSymbols.SetItem(value.Name, unqualified)
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

    member this.SetAccessorContext(ent: IEntitySymbol) =
        { this with benv = { this.benv with ac = { Entity = Some ent }} }

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

    member this.AddTypeExtension(tyExt: IEntitySymbol) =
        if not tyExt.IsTypeExtension then
            failwith "Expected a type extension."

        if tyExt.Extends.Length <> 1 then
            this
        else

        let inheritsTy =
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

            let typeExtensionMembers = this.benv.senv.typeExtensionMembers
            let typeExtensionMembers2 =
                let tyExts =
                    match typeExtensionMembers.TryFind(stripTypeEquationsAndBuiltIn inheritsTy) with
                    | ValueSome tyExts -> tyExts
                    | _ -> ExtensionMemberSymbolOrderedSet.Create()

                let funcs =
                    tyExt.Functions
                    |> ImArray.map (fun func -> ExtensionMemberSymbol.Function(func))

                let props =
                    tyExt.Properties
                    |> ImArray.map (fun prop -> ExtensionMemberSymbol.Property(prop))

                let tyExts2 = tyExts.AddRange(funcs.AddRange(props))

                if inheritsTy.IsError_t then
                    typeExtensionMembers
                else
                    OlyAssert.True(inheritsTy.IsSolved)
                    typeExtensionMembers.SetItem(stripTypeEquationsAndBuiltIn inheritsTy, tyExts2)
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
                    match typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn inheritsTy) with
                    | ValueSome tyExts -> tyExts
                    | _ -> EntitySymbolGeneralizedMap.Create()

                let tyExts2 = 
                    (tyExts, implementsTys)
                    ||> Seq.fold (fun tyExts ty ->
                        match ty.TryEntity with
                        | ValueSome withEnt when withEnt.IsInterface ->
                            tyExts.SetItem(withEnt, tyExt)
                        | _ ->
                            tyExts
                    )

                typeExtensionsWithImplements.SetItem(stripTypeEquationsAndBuiltIn inheritsTy, tyExts2)
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

    member this.SetResolutionMustSolveTypes() =
        if this.resolutionMustSolveTypes then this
        else { this with resolutionMustSolveTypes = true }

    member this.UnqualifiedPatternExists(ident: string) =
        match this.benv.senv.unqualifiedSymbols.TryGetValue ident with
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