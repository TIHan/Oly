﻿module internal rec Oly.Compiler.Internal.SymbolEnvironments

open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations

let ResolutionTypeArityZero = ResolutionTypeArity.FirstOrder 0
let (|ResolutionTypeArityZero|_|) (resTyArity: ResolutionTypeArity) =
    match resTyArity with
    | ResolutionTypeArity.FirstOrder 0 -> Some()
    | _ -> None

[<RequireQualifiedAccess;NoEquality;NoComparison>]
type ResolutionTypeArity =
    | Any
    | FirstOrder of int32
    | SecondOrder of int32

    member this.TryArity =
        match this with
        | Any -> ValueNone
        | FirstOrder n -> ValueSome n
        | SecondOrder n -> ValueSome n

    member this.IsZero =
        match this with
        | FirstOrder 0 -> true
        | _ -> false

    member this.IsAny_t =
        match this with
        | Any _ -> true
        | _ -> false

    member this.IsSecondOrder_t =
        match this with
        | SecondOrder _ -> true
        | _ -> false

    static member Create(arity: int32) =
        if arity < 0 then
            invalidArg (nameof(arity)) "Arity is less than zero."

        match arity with
        | 0 -> ResolutionTypeArityZero
        | _ -> ResolutionTypeArity.FirstOrder arity

[<RequireQualifiedAccess;NoEquality;NoComparison>]
type ResolutionArguments =
    | NotAFunctionCall
    | Any
    | ByType of TypeSymbol imarray
    | ByFunctionType of TypeSymbol

    member this.IsEmpty =
        match this with
        | NotAFunctionCall
        | Any -> true
        | ByType(tys) -> tys.IsEmpty
        | ByFunctionType _ -> true

    member this.IsExplicit =
        match this with
        | ByType _ -> true
        | ByFunctionType _ -> false
        | _ -> false

    member this.IsAny_t =
        match this with
        | Any -> true
        | _ -> false

    member this.TryGetCount() =
        match this with
        | ByType tys -> tys.Length |> ValueSome
        | ByFunctionType(ty) ->
            ty.FunctionParameterCount
            |> ValueSome
        | _ -> 
            ValueNone

[<NoEquality;NoComparison>]
type ScopeEnvironment =
    {
        entitiesByIntrinsicTypes: TypeSymbolGeneralizedMap<IEntitySymbol>
        intrinsicTypesByAliasTypes: TypeSymbolGeneralizedMap<TypeSymbol>
        aliasTypesByIntrinsicTypes: TypeSymbolGeneralizedMap<TypeSymbol>

        namespaces: MultiNameMap<AggregatedNamespaceSymbol>

        // Unqualified
        unqualifiedTypes: IntMap<NameMap<TypeSymbol imarray>>
        unqualifiedSymbols: NameMap<UnqualifiedSymbol>

        enclosingTyInst: IdMap<TypeSymbol imarray>

        parameters: ILocalParameterSymbol imarray
        typeParameters: TypeParameterSymbol imarray
        enclosing: EnclosingSymbol

        // Current info
        typeExtensionsWithImplements: TypeSymbolGeneralizedMap<EntitySymbolGeneralizedMap<IEntitySymbol>>
        typeExtensionMembers: TypeSymbolGeneralizedMap<ExtensionMemberSymbolOrderedSet>
    }

[<NoEquality;NoComparison>]
type AccessorContext =
    {
        AssemblyIdentity: Oly.Metadata.OlyILAssemblyIdentity
        Entity: IEntitySymbol option
    }

[<NoEquality;NoComparison>]
type BoundEnvironment =
    {
        senv: ScopeEnvironment
        openedEnts: ImmutableHashSet<IEntitySymbol>
        openDecls: IEntitySymbol imarray
        ac: AccessorContext
        implicitExtendsForStruct: TypeSymbol option
        implicitExtendsForEnum: TypeSymbol option
    }

    static member Empty =
        let senv = 
            {
                entitiesByIntrinsicTypes = TypeSymbolGeneralizedMap.Create()
                intrinsicTypesByAliasTypes = TypeSymbolGeneralizedMap.Create()
                aliasTypesByIntrinsicTypes = TypeSymbolGeneralizedMap.Create()

                namespaces = MultiNameMap.emptyWithComparer

                unqualifiedTypes = IntMap.Empty

                unqualifiedSymbols = NameMap.Empty

                parameters = ImArray.empty

                typeParameters = ImArray.empty
                enclosing = EnclosingSymbol.Local
                enclosingTyInst = IdMap.Empty

                typeExtensionsWithImplements = TypeSymbolGeneralizedMap.Create()
                typeExtensionMembers = TypeSymbolGeneralizedMap.Create()
            }

        {
            senv = senv
            openedEnts = ImmutableHashSet.Empty
            openDecls = ImArray.empty
            ac = { Entity = None; AssemblyIdentity = Unchecked.defaultof<Oly.Metadata.OlyILAssemblyIdentity> }
            implicitExtendsForStruct = None
            implicitExtendsForEnum = None
        }

    member this.AddOpenDeclaration(ent: IEntitySymbol) =
        { this with
            openDecls = this.openDecls.Add(ent)
        }

    member this.TryGetOpenDeclaration(index) =
        if index < 0 || index >= this.openDecls.Length then
            ValueNone
        else
            ValueSome(this.openDecls[index])

    member this.TryGetNamespace(name: string) =
        // TODO: We create an array everytime we query this, not great. We should fix it.
        match this.senv.namespaces.TryGetValue (ImArray.createOne name) with
        | true, ent -> Some ent
        | _ -> None

    member this.TryGetNamespace(fullName: string imarray) =
        match this.senv.namespaces.TryGetValue fullName with
        | true, ent -> Some ent
        | _ -> None


    /// TODO: We should try to get rid of this and senv.enclosingTyInst as well.
    member this.GetEnclosingTypeArguments(id: int64) =
        match this.senv.enclosingTyInst.TryGetValue id with
        | true, tyArgs -> tyArgs
        | _ -> ImmutableArray.Empty

    member this.EnclosingTypeParameters =
        this.senv.typeParameters

    member this.TryFindEntityByIntrinsicType(ty: TypeSymbol) =
        if ty.IsBuiltIn then
            this.senv.entitiesByIntrinsicTypes.TryFind(ty)
        else
            ValueNone

    member this.TryFindIntrinsicTypeByAliasType(aliasTy: TypeSymbol) =
        if aliasTy.IsAlias then
            this.senv.intrinsicTypesByAliasTypes.TryFind(aliasTy)
        else
            ValueNone

    member this.TryFindAliasTypeByIntrinsicType(intrinsicTy) =
        this.senv.aliasTypesByIntrinsicTypes.TryFind(intrinsicTy)

    member this.TryGetEntity(ty: TypeSymbol) =
        match ty.TryEntity with
        | ValueSome ent -> ValueSome ent
        | _ -> ValueNone

    member this.GetScopedTypeParameters(value: IValueSymbol) =
        if value.IsConstructor then
            value.Enclosing.TypeParameters
        else
            match value.Enclosing with
            | EnclosingSymbol.Local ->
                this.EnclosingTypeParameters.AddRange(value.TypeParameters)
            | _ ->
                value.Enclosing.TypeParameters.AddRange(value.TypeParameters)

    member this.GetScopedTypeArguments(value: IValueSymbol) =
        if value.IsConstructor then
            value.Enclosing.TypeArguments
        else
            match value.Enclosing with
            | EnclosingSymbol.Local ->
                let enclosingTyInst =
                    this.EnclosingTypeParameters
                    |> ImArray.map (fun x -> x.AsType)
                enclosingTyInst.AddRange(value.TypeArguments)
            | _ ->
                value.Enclosing.TypeArguments.AddRange(value.TypeArguments)

    /// Get an unqualified type within the current scope.
    /// An empty array means a type is not found in the current scope.
    /// More that 1 type returned means the type has ambiguities.
    member this.GetUnqualifiedType(name): TypeSymbol imarray =
        let tys = this.GetUnqualifiedType(name, 0)
        if not tys.IsEmpty then
            tys
        else
            this.senv.unqualifiedTypes
            |> Seq.choose (fun pair ->
                if pair.Key <> 0 then
                    match pair.Value.TryGetValue name with
                    | true, tys -> Some tys
                    | _ -> None
                else
                    None
            )
            |> Seq.concat
            |> ImArray.ofSeq

    member this.GetUnqualifiedType(name, arity): TypeSymbol imarray =
        match this.senv.unqualifiedTypes.TryGetValue arity with
        | true, arityGroup ->
            match arityGroup.TryGetValue name with
            | true, tys -> tys
            | _ -> ImArray.empty
        | _ ->
            ImArray.empty

    member this.GetUnqualifiedType(name, tyArity: ResolutionTypeArity, predicate) =
        let arity =
            match tyArity.TryArity with
            | ValueSome arity -> arity
            | _ -> 0
        this.GetUnqualifiedType(name, arity)
        |> ImArray.filter predicate

    member this.TryGetUnqualifiedFunction(name: string, arity) =
        match this.senv.unqualifiedSymbols.TryGetValue(name) with
        | true, s ->
            match s with
            | UnqualifiedSymbol.FunctionGroup(funcGroup) when funcGroup.LogicalTypeParameterCount = arity ->
                let funcs =
                    funcGroup.Functions
                    |> ImArray.filter (fun x -> x.LogicalTypeParameterCount = arity)
                if funcs.IsEmpty then
                    None
                else
                    FunctionGroupSymbol(funcGroup.Name, funcs, funcs[0].Parameters.Length) :> IFunctionSymbol
                    |> Some
            | UnqualifiedSymbol.Function(func) when func.LogicalTypeParameterCount = arity ->
                Some func
            | _ ->
                None
        | _ ->
            None

    member this.TryGetUnqualifiedValue(name: string) =
        match this.senv.unqualifiedSymbols.TryGetValue(name) with
        | true, s ->
            Some s
        | _ ->
            None

    member this.TryFindParameter name =
        this.senv.parameters |> Seq.tryFind (fun x -> x.Name = name) 

    member this.TryFindTypeParameter name =
        let resultOpt = this.senv.typeParameters |> Seq.tryFind (fun x -> x.Name = name)

        // TODO: We should get rid of this check, we only need it to look up the type parameter in the constraint.
        if resultOpt.IsNone then
            let tys = this.GetUnqualifiedType(name)
            if tys.Length = 1 then
                match tys[0] with
                | TypeSymbol.Variable(tyPar) -> Some tyPar
                | _ -> None
            else
                None
        else
            resultOpt

    member this.TypeParameterExists(ty: TypeSymbol) =
        match stripTypeEquations ty with
        | TypeSymbol.Variable(tyPar)
        | TypeSymbol.HigherVariable(tyPar, _) ->
            match this.TryFindTypeParameter tyPar.Name with
            | Some tyPar2 when tyPar2.Id = tyPar.Id -> true
            | _ -> false
        | _ ->
            false

let tryFindTypeHasTypeExtensionImplementedType benv (targetTy: TypeSymbol) ty =
    match targetTy.TryEntity with
    | ValueSome(ent) ->
        benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty)
        |> ValueOption.bind (fun x -> x.TryFind(ent))
    | _ ->
        ValueNone

let typeHasTypeExtensionImplementedType benv (targetTy: TypeSymbol) (ty: TypeSymbol) =
    match targetTy.TryEntity with
    | ValueSome(ent) ->
        benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty)
        |> ValueOption.map (fun x -> x.ContainsKey(ent))
        |> ValueOption.defaultValue false
    | _ ->
        false

let subsumesTypeInEnvironment (benv: BoundEnvironment) (superTy: TypeSymbol) (ty: TypeSymbol) =
    if subsumesType superTy ty then
        true
    elif ty.IsBuiltIn then
        match benv.TryFindEntityByIntrinsicType(ty) with
        | ValueSome(ent) ->
            subsumesType superTy ent.AsType
        | _ ->
            false
    else
        false
            