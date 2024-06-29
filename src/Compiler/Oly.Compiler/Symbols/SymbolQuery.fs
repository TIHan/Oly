module internal rec Oly.Compiler.Internal.SymbolQuery

open System
open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments

let private combineConcreteAndExtensionMembers (concreteMembers: #IValueSymbol seq) (extMembers: #IValueSymbol seq) : #IValueSymbol imarray =
    let filteredExtMembers =
        // Concrete members take precedent.
        extMembers
        |> Seq.filter (fun extMember ->
#if DEBUG || CHECKED
            OlyAssert.True(extMember.Enclosing.IsTypeExtension)
#endif
            let concreteMemberExists =
                concreteMembers
                |> Seq.exists (fun concreteMember ->
#if DEBUG || CHECKED
                    OlyAssert.False(concreteMember.Enclosing.IsTypeExtension)
#endif
                    areValueSignaturesEqual concreteMember extMember
                )
            not concreteMemberExists
        )

    Seq.append concreteMembers filteredExtMembers
    |> ImArray.ofSeq

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
type QueryFunction =
    /// Query for members that are only directly on the type.
    | Intrinsic
    /// Query for members that are directly on the type and its extension members that are in scope.
    | IntrinsicAndExtrinsic

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

let queryIntrinsicAndExtrinsicInheritsAndImplementsOfType (benv: BoundEnvironment) (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _)
    | TypeSymbol.InferenceVariable(Some tyPar, _)
    | TypeSymbol.HigherInferenceVariable(Some tyPar, _, _, _) ->
        tyPar.Constraints
        |> ImArray.choose (fun x -> 
            match x.TryGetAnySubtypeOf() with
            | ValueSome constrTy -> Some constrTy
            | _ -> None
        )
    | ty ->
        let intrinsic = ty.AllLogicalInheritsAndImplements
        match benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty) with
        | ValueSome (tyExts) ->
            let extrinsic =
                tyExts.Values
                |> Seq.collect (fun x ->
                    x.Values
                    |> Seq.collect (fun x ->
                        if x.IsTypeExtension then
                            x.Implements
                        else
                            ImArray.empty
                    )
                )

            Seq.append intrinsic extrinsic
            |> ImArray.ofSeq
        | _ ->
            intrinsic

let queryImmediateFunctionsOfEntity (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (ent: EntitySymbol) =
    let isPatternFunction = queryMemberFlags &&& QueryMemberFlags.PatternFunction = QueryMemberFlags.PatternFunction
    let funcs =
        if isPatternFunction then
            ent.Patterns
            |> ImArray.map (fun x -> x.PatternFunction)     
        else
            ent.Functions
    filterFunctions queryMemberFlags funcFlags nameOpt funcs
    |> filterValuesByAccessibility benv.ac queryMemberFlags

// Finds the most specific functions of an entity
let rec queryMostSpecificIntrinsicFunctionsOfEntity (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (ent: EntitySymbol) : IFunctionSymbol imarray =
    let funcs = queryImmediateFunctionsOfEntity benv queryMemberFlags funcFlags nameOpt ent |> ImArray.ofSeq

    let overridenFuncs =
        funcs
        |> ImArray.filter (fun x -> x.FunctionOverrides.IsSome)

    let inheritedFuncs =
        // TODO: If we make newtypes not extend anything, then this should not be needed.
        if ent.IsNewtype then Seq.empty
        else
            let inheritedFuncs = ImArray.builder()

            ent.Extends
            |> ImArray.iter (fun x ->
                inheritedFuncs.AddRange(queryMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags nameOpt x)
            )

            inheritedFuncs.ToImmutable()
            |> ImArray.filter (fun (x: IFunctionSymbol) -> 
                not x.IsConstructor &&
                let isOverriden =
                    overridenFuncs
                    |> ImArray.exists (fun y ->
                        x.IsVirtual && areLogicalFunctionSignaturesEqual x y.FunctionOverrides.Value
                    )
                not isOverriden
            )
            |> filterValuesByAccessibility benv.ac queryMemberFlags

    let nestedCtors =
        if (queryMemberFlags &&& QueryMemberFlags.Instance <> QueryMemberFlags.Instance) then
            ent.Entities
            |> Seq.map (fun ent -> 
                queryMostSpecificIntrinsicFunctionsOfEntity benv (queryMemberFlags ||| QueryMemberFlags.Instance) (funcFlags ||| FunctionFlags.Constructor) nameOpt ent)
            |> Seq.concat
            |> filterValuesByAccessibility benv.ac queryMemberFlags
        else
            Seq.empty

    let funcs = Seq.append funcs inheritedFuncs
    let funcs = Seq.append funcs nestedCtors |> ImArray.ofSeq

    // Most specific functions
    funcs
    |> filterMostSpecificFunctions

and queryMostSpecificIntrinsicFunctionsOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (ty: TypeSymbol) : _ imarray =
    let ty = findIntrinsicTypeIfPossible benv ty
    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) ->
        queryMostSpecificIntrinsicFunctionsOfEntity benv queryMemberFlags funcFlags nameOpt ent

    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.InferenceVariable(Some tyPar, _) ->
        OlyAssert.False(tyPar.HasArity)

        queryMostSpecificIntrinsicFunctionsOfTypeParameter tyPar
        |> filterFunctions queryMemberFlags funcFlags nameOpt
        |> filterValuesByAccessibility benv.ac queryMemberFlags
        |> ImArray.ofSeq

    | TypeSymbol.HigherVariable(tyPar, tyArgs)
    | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, _, _) ->
        OlyAssert.True(tyPar.HasArity)

        queryMostSpecificIntrinsicFunctionsOfTypeParameter tyPar
        |> filterFunctions queryMemberFlags funcFlags nameOpt
        |> filterValuesByAccessibility benv.ac queryMemberFlags
        |> Seq.map (fun func ->
            if func.IsFormal then
                if func.Enclosing.TypeParameterCount = 0 then
                    func
                else
                    let enclosing =
                        func.Enclosing
                        |> applyEnclosing tyArgs
                    actualFunction enclosing (enclosing.TypeArguments.AddRange(func.TypeArguments)) func
            else
                func
        )
        |> ImArray.ofSeq

    | _ ->
        ImArray.empty

let queryExtensionMembersOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (ty: TypeSymbol) =
    let find ty =
        match benv.senv.typeExtensionMembers.TryFind(stripTypeEquationsAndBuiltIn ty) with
        | ValueSome(exts) ->
            exts.Values
            |> Seq.choose (fun extMember ->
                match extMember with
                | ExtensionMemberSymbol.Function(func) -> 
                    OlyAssert.False(func.Enclosing.AsType.Inherits[0].IsAliasAndNotCompilerIntrinsic)    
                    func.NewSubstituteExtension(ty.TypeArguments)
                    |> Some
                | ExtensionMemberSymbol.Property _ ->
                    // TODO: Handle properties.
                    None
            )
            |> filterFunctions queryMemberFlags funcFlags nameOpt
            |> filterValuesByAccessibility benv.ac queryMemberFlags
        | _ ->
            Seq.empty

    let results = find ty
    if Seq.isEmpty results then
        // Filter most specific extended types
        ty.AllLogicalInheritsAndImplements
        |> Seq.map find
        |> Seq.concat
    else
        results

    |> ImArray.ofSeq
    |> filterMostSpecificFunctions

let queryMostSpecificInterfaceExtensionMembersOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) ty =
    match tryFindTypeExtensions benv ty with
    | ValueSome(tyExts) ->
        tyExts
        |> ImArray.map (fun tyExt ->
            if tyExt.IsFormal && not ty.IsFormal then
                tyExt.SubstituteExtension(ty.TypeArguments)
            else
                tyExt
        )
        |> Seq.collect (fun tyExt ->
            tyExt.Functions
            |> filterFunctions queryMemberFlags funcFlags nameOpt
            |> filterValuesByAccessibility benv.ac queryMemberFlags
        )
        |> ImArray.ofSeq
        |> filterMostSpecificFunctions
    | _ ->
        ImArray.empty

let queryAllExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty =
    let extMembers = queryExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
    let extInterfaceMembers = 
        queryMostSpecificInterfaceExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
    ImArray.append extMembers extInterfaceMembers
    |> filterMostSpecificFunctions

let queryMostSpecificFunctionsOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (queryFunc: QueryFunction) (ty: TypeSymbol) =
    let intrinsicFuncs = queryMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags nameOpt ty

    let extrinsicFuncs =
        match queryFunc with
        | QueryFunction.IntrinsicAndExtrinsic ->
            queryAllExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
        | _ ->
            ImArray.empty

    combineConcreteAndExtensionMembers intrinsicFuncs extrinsicFuncs

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

