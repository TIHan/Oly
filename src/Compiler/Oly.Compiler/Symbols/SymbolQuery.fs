module internal rec Oly.Compiler.Internal.SymbolQuery

open System
open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments

[<RequireQualifiedAccess>]
type QueryMemberFlags =
    | StaticOrInstance =          0x00000
    | Static =                    0x00001
    | Instance =                  0x00010
    | Overridable =               0x00100

    /// This will by-pass any accessor logic.
    | InstanceFunctionOverrides = 0x01010
    // TODO: Add StaticInstanceFunctionOverrides

    | PatternFunction =           0x10001

[<RequireQualifiedAccess>]
type QueryProperty =
    | Intrinsic
    | IntrinsicAndExtrinsic

let findIntrinsicTypeIfPossible (benv: BoundEnvironment) (ty: TypeSymbol) =
    match benv.TryFindIntrinsicTypeByAliasType(ty) with
    | ValueSome intrinsicTy ->
        match benv.TryFindEntityByIntrinsicType(intrinsicTy) with
        | ValueSome ent -> ent.AsType
        | _ -> ty
    | _ ->
        match benv.TryFindEntityByIntrinsicType(ty) with
        | ValueSome ent -> ent.AsType
        | _ -> ty

let canAccessValue (ac: AccessorContext) (value: IValueSymbol) =
    if value.IsPublic then true
    elif value.IsInternal then
        // TODO: There a way to make this a faster check?
        match ac.Entity, value.Enclosing.TryEntity with
        | Some ent1, Some ent2 when not ent2.IsNamespace ->
            match ent1.ContainingAssembly, ent2.ContainingAssembly with
            | Some asm1, Some asm2 ->
                (asm1.Identity :> IEquatable<Oly.Metadata.OlyILAssemblyIdentity>).Equals(asm2.Identity)
            | _ ->
                false
        | _, Some ent1 ->
            match ent1.ContainingAssembly with
            | Some asm1 ->
                (asm1.Identity :> IEquatable<Oly.Metadata.OlyILAssemblyIdentity>).Equals(ac.AssemblyIdentity)
            | _ ->
                false
        | _ -> 
            false
    elif value.IsProtected then
        match ac.Entity, value.Enclosing.TryEntity with
        | Some ent1, Some ent2 -> subsumesType ent1.AsType ent2.AsType || subsumesType ent2.AsType ent1.AsType
        | _ -> false
    else
        match ac.Entity, value.Enclosing.TryEntity with
        | Some ent1, Some ent2 -> areEntitiesEqual ent1 ent2
        | _ -> false

let canAccessEntity (ac: AccessorContext) (ent: EntitySymbol) =
    if ent.IsPublic then true
    elif ent.IsInternal then
        // TODO: There a way to make this a faster check?
        match ac.Entity with
        | Some ent1 ->
            match ent1.ContainingAssembly, ent.ContainingAssembly with
            | Some asm1, Some asm2 ->
                (asm1.Identity :> IEquatable<Oly.Metadata.OlyILAssemblyIdentity>).Equals(asm2.Identity)
            | _ ->
                false
        | _ -> 
            true
    else
        match ac.Entity, ent.Enclosing.TryEntity with
        | Some ent1, Some ent2 -> 
            areEntitiesEqual ent1 ent2
        | _ -> 
            false

let filterFields (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) (fields: IFieldSymbol seq) =
    let isInstance = queryMemberFlags &&& QueryMemberFlags.Instance = QueryMemberFlags.Instance
    let isStatic = queryMemberFlags &&& QueryMemberFlags.Static = QueryMemberFlags.Static
    let isOverridable = queryMemberFlags &&& QueryMemberFlags.Overridable = QueryMemberFlags.Overridable
    fields
    |> Seq.filter (fun field -> 
        (if isStatic = isInstance then true else field.IsInstance = isInstance) &&
        (if isOverridable then field.IsOverridable = true else true) &&
        (field.ValueFlags &&& valueFlags = valueFlags) &&
        (
            match nameOpt with
            | None -> true
            | Some(name) -> name = field.Name
        ))

let filterProperties (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) (props: IPropertySymbol seq) =
    let isInstance = queryMemberFlags &&& QueryMemberFlags.Instance = QueryMemberFlags.Instance
    let isStatic = queryMemberFlags &&& QueryMemberFlags.Static = QueryMemberFlags.Static
    let isOverridable = queryMemberFlags &&& QueryMemberFlags.Overridable = QueryMemberFlags.Overridable
    props
    |> Seq.filter (fun prop -> 
        (if isStatic = isInstance then true else prop.IsInstance = isInstance) &&
        (if isOverridable then prop.IsOverridable = true else true) &&
        (prop.ValueFlags &&& valueFlags = valueFlags) &&
        (
            match nameOpt with
            | None -> true
            | Some(name) -> name = prop.Name
        ))

let filterFunctions (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (funcs: IFunctionSymbol seq) =
    let isInstance = queryMemberFlags &&& QueryMemberFlags.Instance = QueryMemberFlags.Instance
    let isStatic = queryMemberFlags &&& QueryMemberFlags.Static = QueryMemberFlags.Static
    let isOverridable = queryMemberFlags &&& QueryMemberFlags.Overridable = QueryMemberFlags.Overridable
    let canCheckOverrides = queryMemberFlags &&& QueryMemberFlags.InstanceFunctionOverrides = QueryMemberFlags.InstanceFunctionOverrides
    funcs
    |> Seq.filter (fun func ->
        let func =
            if canCheckOverrides then
                match func.FunctionOverrides with
                | Some func -> func
                | _ -> func
            else
                func
            
        (if isStatic = isInstance then true else func.IsInstance = isInstance) &&
        (if isOverridable then func.IsOverridable = true else true) &&
        (func.FunctionFlags &&& funcFlags = funcFlags) &&
        (
            match nameOpt with
            | None -> true
            | Some(name) -> 
                if func.IsConstructor then
                    name = func.Enclosing.AsEntity.Name
                else
                    name = func.Name
        ))

let filterValuesByAccessibility<'T when 'T :> IValueSymbol> ac (queryMemberFlags: QueryMemberFlags) (values: 'T seq) =
    let isInstance = queryMemberFlags &&& QueryMemberFlags.Instance = QueryMemberFlags.Instance
    let isStatic = queryMemberFlags &&& QueryMemberFlags.Static = QueryMemberFlags.Static
    let isOverridable = queryMemberFlags &&& QueryMemberFlags.Overridable = QueryMemberFlags.Overridable
    let canCheckOverrides = queryMemberFlags &&& QueryMemberFlags.InstanceFunctionOverrides = QueryMemberFlags.InstanceFunctionOverrides

    let values =
        values
        |> Seq.filter (fun value ->
            (if isStatic = isInstance then true else value.IsInstance = isInstance) &&
            (if isOverridable then value.IsOverridable = true else true)
        )

    // We are querying for functions that override, we must include private functions in this case.
    if canCheckOverrides then 
        values
    else
        values
        |> Seq.filter (canAccessValue ac)

let filterEntitiesByAccessibility ac (ents: EntitySymbol seq) =
    ents
    |> Seq.filter (canAccessEntity ac)

let queryImmediatePropertiesOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: EntitySymbol) =
    filterProperties queryMemberFlags valueFlags nameOpt ent.Properties
    |> filterValuesByAccessibility benv.ac queryMemberFlags
    
let queryIntrinsicPropertiesOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: EntitySymbol) =
    let props = queryImmediatePropertiesOfEntity benv queryMemberFlags valueFlags nameOpt ent
        
    let inheritedProps =
        ent.Extends
        |> Seq.map (fun x ->
            match x.TryEntity with
            | ValueSome x ->
                queryIntrinsicPropertiesOfEntity benv queryMemberFlags valueFlags nameOpt x
            | _ ->
                Seq.empty
        )
        |> Seq.concat
        |> filterValuesByAccessibility benv.ac queryMemberFlags
    
    Seq.append props inheritedProps

let queryHierarchicalTypesOfTypeParameter (tyPar: TypeParameterSymbol) =
    seq {
        for x in tyPar.Constraints do
            match x with
            | ConstraintSymbol.SubtypeOf(ty) 
            | ConstraintSymbol.TraitType(ty) ->
                yield! ty.Value.FlattenHierarchyIncluding()
            | _ ->
                ()
    }
    |> TypeSymbol.Distinct

let queryMostSpecificIntrinsicFunctionsOfTypeParameter (tyPar: TypeParameterSymbol): _ imarray =
    queryHierarchicalTypesOfTypeParameter tyPar
    |> Seq.collect (fun ty -> 
        ty.Functions
    )
    |> Seq.filter (fun func -> 
        (not func.IsFormal) ||
        (not tyPar.HasArity) || 
        (func.Formal.Enclosing.TypeParameterCount = 0) || 
        (func.Formal.Enclosing.TypeParameterCount = tyPar.Arity)
    )
    |> ImArray.ofSeq
    |> filterMostSpecificFunctions

let queryMostSpecificPropertiesOfTypeParameter benv (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) queryProp isTyCtor (tyPar: TypeParameterSymbol) =
    queryHierarchicalTypesOfTypeParameter tyPar
    |> Seq.collect (fun ty -> 
        queryPropertiesOfType benv queryMemberFlags valueFlags nameOpt queryProp ty
    )
    |> Seq.filter (fun (prop: IPropertySymbol) -> 
        (not prop.IsFormal) ||
        (not tyPar.HasArity) || 
        (prop.Formal.Enclosing.TypeParameterCount = 0) || 
        (prop.Formal.Enclosing.TypeParameterCount = tyPar.Arity)
    )
    |> filterMostSpecificProperties

let queryPropertiesOfType (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) queryProp (ty: TypeSymbol) =
    let ty = findIntrinsicTypeIfPossible benv ty
    let intrinsicProps =
        match stripTypeEquations ty with
        | TypeSymbol.Variable(tyPar) 
        | TypeSymbol.HigherVariable(tyPar, _) ->
            queryMostSpecificPropertiesOfTypeParameter benv queryMemberFlags valueFlags nameOpt queryProp false tyPar
        | TypeSymbol.Entity(ent) ->
            queryIntrinsicPropertiesOfEntity benv queryMemberFlags valueFlags nameOpt ent
        | _ ->
            Seq.empty
    
    let extrinsicProps =
        match queryProp with
        | QueryProperty.IntrinsicAndExtrinsic ->
            let results1 =
                match benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty) with
                | ValueSome (traitImpls) ->
                    traitImpls.Values
                    |> Seq.collect (fun trImpl ->
                        trImpl.Values
                        |> Seq.collect (fun trImpl ->
                            filterProperties queryMemberFlags valueFlags nameOpt trImpl.Properties
                        )
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
    |> filterValuesByAccessibility benv.ac queryMemberFlags

type EntitySymbol with

    member this.TryFindDefaultInstanceConstructor() =
        this.Functions
        |> ImArray.tryFind (fun x -> x.IsInstanceConstructor && x.LogicalParameterCount = 0)

    member this.HasDefaultInstanceConstructor =
        this.Functions
        |> ImArray.exists (fun x -> x.IsInstanceConstructor && x.LogicalParameterCount = 0)

    member this.AllLogicallyInheritedAndImplementedFunctions =
        let builder = ImArray.builder()
        this.HierarchyForEach(fun x ->
            builder.AddRange(x.Functions)
        )
        builder.ToImmutable()

    member this.AllLogicalFunctions: _ seq =
        this.AllLogicallyInheritedAndImplementedFunctions
        |> Seq.append (this.Functions)

type TypeSymbol with

    /// All logical functions from the type and logical inherited/implemented types
    member this.AllLogicalFunctions =
        match stripTypeEquations this with
        | TypeSymbol.Entity(ent) -> ent.AllLogicalFunctions
        | TypeSymbol.Variable(tyPar)
        | TypeSymbol.InferenceVariable(Some tyPar, _) ->
            queryMostSpecificIntrinsicFunctionsOfTypeParameter tyPar
        | TypeSymbol.HigherVariable(tyPar, tyArgs)
        | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, _, _) -> 
            queryMostSpecificIntrinsicFunctionsOfTypeParameter tyPar
            |> Seq.map (fun (func: IFunctionSymbol) ->
                let enclosing = 
                    func.Formal.Enclosing
                    |> applyEnclosing tyArgs 
                actualFunction enclosing (enclosing.TypeArguments.AddRange(func.TypeArguments)) func
            )
        | _ ->
            Seq.empty

