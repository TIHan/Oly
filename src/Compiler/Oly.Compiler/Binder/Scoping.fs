[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Scoping

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.SymbolQuery
open Oly.Compiler.Internal.SymbolQuery.Extensions

[<RequireQualifiedAccess>]
type OpenContent =
    | None
    | All
    | Entities
    | Values

/// 'canReplace' means it will replace values with the same signature.
let private scopeInValue canReplace (env: BinderEnvironment) (value: IValueSymbol) =
    if canReplace then
        env.AddUnqualifiedValue(value)
    else
        env.TryAddUnqualifiedValue(value)

let private getAccessibleInstanceConstructors onlyPrivateOrProtected env (ent: EntitySymbol) =
    ent.InstanceConstructors
    |> ImArray.filter (fun x -> 
        if onlyPrivateOrProtected then
            if x.IsPrivate || x.IsProtected then
                x.IsAccessible(env.benv.ac)
            else
                false
        else
            x.IsAccessible(env.benv.ac)
    )

/// 'canReplace' means it will replace constructors with the same signature.
let scopeInInstanceConstructors canReplace onlyPrivateOrProtected (env: BinderEnvironment) (ent: EntitySymbol) =
    let instanceCtors = 
        if ent.IsAlias then
            match (stripTypeEquations ent.AsType).TryEntity with
            | ValueSome(ent) -> 
                getAccessibleInstanceConstructors
                    onlyPrivateOrProtected
                    env
                    ent
            | _ -> 
                ImArray.empty
        else
            getAccessibleInstanceConstructors
                onlyPrivateOrProtected
                env
                ent
    if instanceCtors.IsEmpty then
        env
    else
        scopeInValue canReplace env (FunctionGroupSymbol.Create(ent.Name, instanceCtors, instanceCtors[0].Parameters.Length, false))

/// 'canReplace' means it will replace entities with the same name and arity.
let private scopeInEntityAux canReplace (env: BinderEnvironment) (ent: EntitySymbol) =
    if ent.IsNamespace then
        // REVIEW: This will not partially open namespaces. We should consider doing this as a feature.
        env
    elif ent.IsShape || ent.IsTypeExtension then
        let arity =
            if ent.Enclosing.IsLocalEnclosing then
                ent.LogicalTypeParameterCount - env.EnclosingTypeParameters.Length
            else
                ent.LogicalTypeParameterCount
        if canReplace then
            env.SetUnqualifiedType(ent.Name, arity, ent.AsType)
        else
            env.AddUnqualifiedType(ent.Name, arity, ent.AsType)
    else
        let env =
            match ent.TryGetIntrinsicType() with
            | true, ty ->
                env.SetIntrinsicType(ty, ent)
            | _ ->
                env

        let arity =
            if ent.Enclosing.IsLocalEnclosing then
                ent.LogicalTypeParameterCount - env.EnclosingTypeParameters.Length
            else
                ent.LogicalTypeParameterCount
        if canReplace then
            env.SetUnqualifiedType(ent.Name, arity, ent.AsType)
        else
            env.AddUnqualifiedType(ent.Name, arity, ent.AsType)

let scopeInEntity env ent =
    scopeInEntityAux false env ent

let scopeInEntityAndOverride env ent =
    scopeInEntityAux true env ent

let scopeInTypeParameter (env: BinderEnvironment) (tyPar: TypeParameterSymbol) =
    env.AddUnqualifiedType(tyPar.Name, tyPar.Arity, tyPar.AsType)

let openContentsOfEntityAux (declTable: BoundDeclarationTable) canOverride canOpenNamespace (env: BinderEnvironment) openContent (ent: EntitySymbol) =
    if openContent = OpenContent.None then env
    else

    if ent.IsAggregatedNamespace then
        match ent with
        | :? AggregatedNamespaceSymbol as aggr ->
            (env, aggr.Namespaces)
            ||> ImArray.fold (fun env nmsp ->
                openContentsOfEntityAux declTable canOverride true env openContent nmsp
            )
        | _ ->
            failwith "not possible"        
    elif ent.IsNamespace && not canOpenNamespace then
        match env.benv.senv.namespaces.TryGetValue ent.FullNamespacePath with
        | true, ent ->
            openContentsOfEntityAux declTable canOverride false env openContent ent
        | _ ->
            env
    else

    if env.HasOpenedEntity(ent) then
        env
    else

    let env =
        if (openContent = OpenContent.Values || openContent = OpenContent.All) then
            env.AddOpenedEntity(ent)
        elif ent.IsNonNamespaceRootInScope(env.benv.ac.AssemblyIdentity) && 
             (match env.benv.ac.Entity with Some scopeEnt -> not(areEntitiesEqual scopeEnt.Formal ent.Formal) | _ -> true) then
            env.AddPartialOpenedRootEntity(ent)
        else
            env

    if ent.IsTypeExtension then
        match openContent with
        | OpenContent.All 
        | OpenContent.Values ->
            env.AddTypeExtension(ent)
        | _ ->
            env
    elif ent.IsShape then
        env
    else
        let env1 =
            (env, ent.GetAccessibleNestedEntities(env.benv.ac) |> ImArray.ofSeq)
            ||> ImArray.fold (fun env ent ->

                // Special-case for private types in namespaces. Only allow them in the current compilation unit which is dictated by the declaration table.
                if ent.IsPrivate && ent.Enclosing.IsNamespace && not (declTable.EntityDeclarations.ContainsKey(ent.Formal)) then
                    env
                else

                let env =
                    match openContent with
                    | OpenContent.All
                    | OpenContent.Entities ->
                        scopeInEntityAux canOverride env ent
                    | _ ->
                        env

                let env =
                    if ent.IsAutoOpenable then
                        openContentsOfEntityAux declTable canOverride false env openContent ent
                    else
                        env       
                            
                // REVIEW: Should we scope in the instance constructors after we potentially
                //         auto-opened?
                match openContent with
                | OpenContent.All
                | OpenContent.Values ->
                    scopeInInstanceConstructors canOverride false env ent  
                | _ ->
                    env
            )

        let env2 =
            match openContent with
            | OpenContent.All
            | OpenContent.Values ->
                let env2 =
                    let funcGroups = 
                        ent.GetImmediateAccessibleStaticFunctions(env.benv.ac)
                        |> Seq.filter (fun func -> not func.IsConstructor)
                        |> Seq.groupBy (fun func -> (func.Name, func.IsPatternFunction))
                        |> Seq.map (fun ((name, isPattern), funcs) ->
                            let funcs = funcs |> ImArray.ofSeq
                            if funcs.Length = 1 then
                                funcs[0]
                            else
                                FunctionGroupSymbol.Create(name, funcs, funcs[0].Parameters.Length, isPattern) :> IFunctionSymbol
                                
                        )
                        |> ImArray.ofSeq
                    (env1, funcGroups)
                    ||> ImArray.fold (fun env func ->
                        scopeInValue canOverride env func
                    )

                let env3 =
                    (env2, ent.GetImmediateAccessibleStaticProperties(env.benv.ac) |> ImArray.ofSeq)
                    ||> ImArray.fold (fun env prop ->
                        scopeInValue canOverride env prop
                    )

                (env3, ent.GetImmediateAccessibleStaticFields(env.benv.ac) |> ImArray.ofSeq)
                ||> ImArray.fold (fun env field ->
                    scopeInValue canOverride env field
                )
            | _ ->
                env1
        
        env2

let openContentsOfEntity declTable env openContent ent =
    openContentsOfEntityAux declTable false false env openContent ent

let openContentsOfEntityAndOverride declTable env openContent ent =
    openContentsOfEntityAux declTable true false env openContent ent

let addTypeParameter (cenv: cenv) (env: BinderEnvironment) (syntaxNode: OlySyntaxNode) (tyPar: TypeParameterSymbol) =
    let tys = env.benv.GetUnqualifiedType(tyPar.Name, tyPar.Arity)
    if tys |> ImArray.exists (fun x -> x.IsTypeVariable && not env.isInEntityDefinitionTypeParameters) then
        cenv.diagnostics.Error(sprintf "Type parameter '%s' has already been declared." tyPar.Name, 10, syntaxNode)
        env, tyPar
    else
        scopeInTypeParameter env tyPar, tyPar

let addTypeParametersFromEntity (cenv: cenv) (env: BinderEnvironment) (syntaxTyPars: OlySyntaxType imarray) (ent: EntitySymbol) =
    if syntaxTyPars.IsEmpty then
        env
    else
        let env = { env with isInEntityDefinitionTypeParameters = true }

        let tyPars = 
            ent.TypeParameters.RemoveRange(0, ent.TypeParameters.Length - syntaxTyPars.Length)
            |> ImArray.ofSeq

        let tyParsWithSyntax =
            (syntaxTyPars, tyPars)
            ||> ImArray.map2 (fun syntaxTyPar tyPars -> (syntaxTyPar, tyPars))

        let env =
            (env, tyParsWithSyntax)
            ||> ImArray.fold (fun env (syntaxTyPar, tyPar) ->
                addTypeParameter cenv env syntaxTyPar tyPar |> fst
            )
        { env with isInEntityDefinitionTypeParameters = false }