[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Scoping

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTree

[<RequireQualifiedAccess>]
type OpenContent =
    | None
    | All
    | Entities
    | Values

let scopeInInstanceConstructors (env: BinderEnvironment) (ent: EntitySymbol) =
    let instanceCtors = ent.InstanceConstructors
    if instanceCtors.IsEmpty then
        env
    elif instanceCtors.Length = 1 then
        env.SetUnqualifiedValue(instanceCtors[0])
    else
        env.SetUnqualifiedValue(FunctionGroupSymbol(ent.Name, instanceCtors, instanceCtors[0].Parameters.Length, false)) 

let private scopeInEntityAux canOverride (env: BinderEnvironment) (ent: EntitySymbol) =
    if ent.IsNamespace then
        // REVIEW: This will not partially open namespaces. We should consider doing this as a feature.
        env
    elif ent.IsShape || ent.IsTypeExtension then
        let arity =
            if ent.Enclosing.IsLocalEnclosing then
                ent.LogicalTypeParameterCount - env.EnclosingTypeParameters.Length
            else
                ent.LogicalTypeParameterCount
        if canOverride then
            env.SetUnqualifiedType(ent.Name, arity, ent.AsType)
        else
            env.AddUnqualifiedType(ent.Name, arity, ent.AsType)
    else
        let env =
            match ent.TryIntrinsicType with
            | Some ty ->
                env.SetIntrinsicType(ty, ent)
            | _ ->
                env

        let arity =
            if ent.Enclosing.IsLocalEnclosing then
                ent.LogicalTypeParameterCount - env.EnclosingTypeParameters.Length
            else
                ent.LogicalTypeParameterCount
        if canOverride then
            env.SetUnqualifiedType(ent.Name, arity, ent.AsType)
        else
            env.AddUnqualifiedType(ent.Name, arity, ent.AsType)

let scopeInEntity env ent =
    scopeInEntityAux false env ent

let scopeInEntityAndOverride env ent =
    scopeInEntityAux true env ent

let scopeInTypeParameter (env: BinderEnvironment) (tyPar: TypeParameterSymbol) =
    env.AddUnqualifiedType(tyPar.Name, tyPar.Arity, tyPar.AsType)

let openContentsOfEntityAux canOverride canOpenNamespace (env: BinderEnvironment) openContent (ent: EntitySymbol) =
    if openContent = OpenContent.None then env
    else

    if ent.IsAggregatedNamespace then
        match ent with
        | :? AggregatedNamespaceSymbol as aggr ->
            (env, aggr.Namespaces)
            ||> ImArray.fold (fun env nmsp ->
                openContentsOfEntityAux canOverride true env openContent nmsp
            )
        | _ ->
            failwith "not possible"        
    elif ent.IsNamespace && not canOpenNamespace then
        match env.benv.senv.namespaces.TryGetValue ent.FullNamespacePath with
        | true, ent ->
            openContentsOfEntityAux canOverride false env openContent ent
        | _ ->
            env
    else

    if env.HasOpenedEntity(ent) then
        env
    else

    let env =
        if (openContent = OpenContent.Values || openContent = OpenContent.All) then
            env.AddOpenedEntity(ent)
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
            (env, ent.Entities |> filterEntitiesByAccessibility env.benv.ac |> ImArray.ofSeq)
            ||> ImArray.fold (fun env ent ->
                let env =
                    match openContent with
                    | OpenContent.All
                    | OpenContent.Entities ->
                        scopeInEntityAux canOverride env ent
                    | _ ->
                        env

                let env =
                    if ent.IsAutoOpenable then
                        openContentsOfEntityAux canOverride false env openContent ent
                    else
                        env       
                            
                // REVIEW: Should we scope in the instance constructors after we potentially
                //         auto-opened?
                match openContent with
                | OpenContent.All
                | OpenContent.Values ->
                    scopeInInstanceConstructors env ent  
                | _ ->
                    env
            )

        let env2 =
            match openContent with
            | OpenContent.All
            | OpenContent.Values ->
                let env2 =
                    let funcGroups = 
                        ent.Functions
                        |> filterValuesByAccessibility env.benv.ac QueryMemberFlags.Static
                        |> Seq.filter (fun func -> not func.IsConstructor)
                        |> Seq.groupBy (fun func -> (func.Name, func.IsPatternFunction))
                        |> Seq.map (fun ((name, isPattern), funcs) ->
                            let funcs = funcs |> ImArray.ofSeq
                            if funcs.Length = 1 then
                                funcs[0]
                            else
                                FunctionGroupSymbol(name, funcs, funcs[0].Parameters.Length, isPattern) :> IFunctionSymbol
                                
                        )
                        |> ImArray.ofSeq
                    (env1, funcGroups)
                    ||> ImArray.fold (fun env func ->
                        env.SetUnqualifiedValue(func)
                    )

                let env3 =
                    (env2, ent.Properties |> filterValuesByAccessibility env.benv.ac QueryMemberFlags.Static |> ImArray.ofSeq)
                    ||> ImArray.fold (fun env prop ->
                        env.SetUnqualifiedValue(prop)
                    )

                (env3, ent.Fields |> filterValuesByAccessibility env.benv.ac QueryMemberFlags.Static |> ImArray.ofSeq)
                ||> ImArray.fold (fun env field ->
                    env.SetUnqualifiedValue(field)
                )
            | _ ->
                env1
        
        env2

let openContentsOfEntity env openContent ent =
    openContentsOfEntityAux false false env openContent ent

let openContentsOfEntityAndOverride env openContent ent =
    openContentsOfEntityAux true false env openContent ent

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