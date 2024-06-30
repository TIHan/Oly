module internal rec Oly.Compiler.Internal.SymbolQuery

open System
open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments

let private findIntrinsicTypeIfPossible (benv: BoundEnvironment) (ty: TypeSymbol) =
    match benv.TryFindEntityByIntrinsicType(stripTypeEquations ty) with
    | ValueSome ent -> ent.AsType
    | _ -> ty

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

let private canAccessValue (ac: AccessorContext) (value: IValueSymbol) =
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

let private canAccessEntity (ac: AccessorContext) (ent: EntitySymbol) =
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

let private filterFields (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) (fields: IFieldSymbol seq) =
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

let private filterProperties (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) (props: IPropertySymbol seq) =
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

let private filterFunctions (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (funcs: IFunctionSymbol seq) =
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

let private filterValuesByAccessibility<'T when 'T :> IValueSymbol> ac (queryMemberFlags: QueryMemberFlags) (values: 'T seq) =
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

let private filterEntitiesByAccessibility ac (ents: EntitySymbol seq) =
    ents
    |> Seq.filter (canAccessEntity ac)

let private queryHierarchicalTypesOfTypeParameter (tyPar: TypeParameterSymbol) =
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

let private queryHierarchicalValuesOfEntity (queryImmediateValues: BoundEnvironment -> QueryMemberFlags -> FunctionFlags -> string option -> TypeSymbol -> #IValueSymbol seq) benv queryMemberFlags funcFlags nameOpt (ent: EntitySymbol) =
    let values = ImArray.builder()
    let isNotTypeExtensionOrInterface = not(ent.IsTypeExtension || ent.IsInterface)
    if ent.IsTypeExtension then
        ent.FlattenHierarchy() |> ImArray.append ent.Extends
    else
        ent.FlattenHierarchy()
    |> ImArray.iter (fun x ->
        let queryMemberFlags =
            if x.IsInterface && isNotTypeExtensionOrInterface then
                QueryMemberFlags.Static
            else
                queryMemberFlags
        values.AddRange(queryImmediateValues benv queryMemberFlags funcFlags nameOpt x : _ seq)
    )
    values.ToImmutable()

let private queryHierarchicalTypes (ty: TypeSymbol) =
    match stripTypeEquations ty with
        | TypeSymbol.Entity(ent) ->
            // TODO: Add 'ent.FlattenHierarchyIncluding' to avoid 'AsType'.
            ent.AsType.FlattenHierarchyIncluding()
            |> TypeSymbol.Distinct

        | TypeSymbol.Variable(tyPar)
        | TypeSymbol.InferenceVariable(Some tyPar, _) ->
            queryHierarchicalTypesOfTypeParameter tyPar

        | TypeSymbol.HigherVariable(tyPar, tyArgs)
        | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, _, _) ->
            OlyAssert.True(tyPar.HasArity)

            queryHierarchicalTypesOfTypeParameter tyPar
            |> Seq.map (fun ty ->
                actualType tyArgs ty
            )

        | _ ->
            ImArray.empty

let private queryImmediateFunctionsOfEntity (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (ent: EntitySymbol) =
    let isPatternFunction = queryMemberFlags &&& QueryMemberFlags.PatternFunction = QueryMemberFlags.PatternFunction
    let funcs =
        if isPatternFunction then
            ent.Patterns
            |> ImArray.map (fun x -> x.PatternFunction)     
        else
            ent.Functions
    filterFunctions queryMemberFlags funcFlags nameOpt funcs
    |> filterValuesByAccessibility benv.ac queryMemberFlags

let private queryImmediateFunctionsOfType (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (ty: TypeSymbol) =
    let ty = findIntrinsicTypeIfPossible benv ty
    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) ->
        queryImmediateFunctionsOfEntity benv queryMemberFlags funcFlags nameOpt ent
    | _ ->
        // TODO: Handle others to consolidate.
        Seq.empty

// Finds the most specific functions of an entity
let private queryMostSpecificIntrinsicFunctionsOfEntity (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (ent: EntitySymbol) : IFunctionSymbol imarray =
    let funcs =
        queryImmediateFunctionsOfEntity benv queryMemberFlags funcFlags nameOpt ent |> ImArray.ofSeq

    let overridenFuncs =
        funcs
        |> ImArray.filter (fun x -> x.FunctionOverrides.IsSome)

    let inheritedFuncs =
        // TODO: If we make newtypes not extend anything, then this should not be needed.
        if ent.IsNewtype || ent.IsShape then Seq.empty
        else
            let inheritedFuncs =
                queryHierarchicalValuesOfEntity
                    queryImmediateFunctionsOfType
                    benv
                    queryMemberFlags
                    funcFlags
                    nameOpt
                    ent

            inheritedFuncs
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

let private queryMostSpecificIntrinsicFunctionsOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (ty: TypeSymbol) : _ imarray =
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

let private queryExtensionMembersOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (ty: TypeSymbol) =
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

let private queryMostSpecificInterfaceExtensionMembersOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) ty =
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

let private queryAllExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty =
    let extMembers = queryExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
    let extInterfaceMembers = 
        queryMostSpecificInterfaceExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
    ImArray.append extMembers extInterfaceMembers
    |> filterMostSpecificFunctions

let private queryMostSpecificFunctionsOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (queryFunc: QueryFunction) (ty: TypeSymbol) =
    let intrinsicFuncs = queryMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags nameOpt ty

    let extrinsicFuncs =
        match queryFunc with
        | QueryFunction.IntrinsicAndExtrinsic ->
            queryAllExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
        | _ ->
            ImArray.empty

    combineConcreteAndExtensionMembers intrinsicFuncs extrinsicFuncs

let private queryImmediatePropertiesOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: EntitySymbol) =
    filterProperties queryMemberFlags valueFlags nameOpt ent.Properties
    |> filterValuesByAccessibility benv.ac queryMemberFlags
    
let private queryIntrinsicPropertiesOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: EntitySymbol) =
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

let private queryMostSpecificIntrinsicFunctionsOfTypeParameter (tyPar: TypeParameterSymbol): _ imarray =
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

let private queryImmediateFieldsOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: EntitySymbol) =
    filterFields queryMemberFlags valueFlags nameOpt ent.Fields
    |> filterValuesByAccessibility benv.ac queryMemberFlags
    
let private queryIntrinsicFieldsOfEntity (benv: BoundEnvironment) queryMemberFlags valueFlags (nameOpt: string option) (ent: EntitySymbol) =
    let fields = queryImmediateFieldsOfEntity benv queryMemberFlags valueFlags nameOpt ent
    
    let inheritedFields =
        // TODO: If we make newtypes not extend anything, then this should not be needed.
        if ent.IsNewtype then
            Seq.empty
        else
            ent.Extends
            |> Seq.map (fun x ->
                match x.TryEntity with
                | ValueSome x ->
                    queryIntrinsicFieldsOfEntity benv queryMemberFlags valueFlags nameOpt x
                | _ ->
                    Seq.empty
            )
            |> Seq.concat
            |> filterValuesByAccessibility benv.ac queryMemberFlags
    
    Seq.append inheritedFields fields
    
let private queryFieldsOfType (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) (ty: TypeSymbol) =
    let ty = findIntrinsicTypeIfPossible benv ty
    match stripTypeEquations ty with
    | TypeSymbol.Variable(tyPar) ->
        tyPar.Constraints
        |> Seq.choose (function
            | ConstraintSymbol.Null
            | ConstraintSymbol.Struct
            | ConstraintSymbol.NotStruct 
            | ConstraintSymbol.Unmanaged
            | ConstraintSymbol.Blittable
            | ConstraintSymbol.Scoped
            | ConstraintSymbol.ConstantType _ -> None
            | ConstraintSymbol.SubtypeOf(ty) 
            | ConstraintSymbol.TraitType(ty) -> Some ty.Value
        )
        |> Seq.collect(fun ent ->
            filterFields queryMemberFlags valueFlags nameOpt ent.Fields
        )
    | TypeSymbol.Entity(ent) ->
        queryIntrinsicFieldsOfEntity benv queryMemberFlags valueFlags nameOpt ent
    | _ ->
        Seq.empty

let private queryMostSpecificPropertiesOfTypeParameter benv (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) queryProp isTyCtor (tyPar: TypeParameterSymbol) =
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

let private queryPropertiesOfType (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) queryProp (ty: TypeSymbol) =
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

module Extensions =

    type IValueSymbol with

        member this.IsAccessible(ac) =
            canAccessValue ac this

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

        member this.GetInstanceFields() =
            this.Fields
            |> ImArray.filter (fun x -> x.IsInstance)

        member this.GetImmediateAccessibleStaticFunctions(ac) =
            this.Functions
            |> filterValuesByAccessibility ac QueryMemberFlags.Static

        member this.GetImmediateAccessibleStaticFields(ac) =
            this.Fields
            |> filterValuesByAccessibility ac QueryMemberFlags.Static

        member this.GetImmediateAccessibleStaticProperties(ac) =
            this.Properties 
            |> filterValuesByAccessibility ac QueryMemberFlags.Static

        member this.GetAccessibleNestedEntities(ac) =
            this.Entities
            |> filterEntitiesByAccessibility ac

        member this.FindMostSpecificIntrinsicFunctions(benv: BoundEnvironment, queryMemberFlags, funcFlags) =
            queryMostSpecificIntrinsicFunctionsOfEntity benv queryMemberFlags funcFlags None this

        member this.FindMostSpecificIntrinsicFunctions(benv: BoundEnvironment, queryMemberFlags, funcFlags, name) =
            queryMostSpecificIntrinsicFunctionsOfEntity benv queryMemberFlags funcFlags (Some name) this

        member this.FindIntrinsicFields(benv, queryMemberFlags) =
            queryIntrinsicFieldsOfEntity benv queryMemberFlags ValueFlags.None None this

        member this.FindIntrinsicFields(benv, queryMemberFlags, name) =
            queryIntrinsicFieldsOfEntity benv queryMemberFlags ValueFlags.None (Some name) this

        member this.FindIntrinsicProperties(benv, queryMemberFlags) =
            queryIntrinsicPropertiesOfEntity benv queryMemberFlags ValueFlags.None None this

        member this.FindNestedEntities(benv: BoundEnvironment, nameOpt: string option, tyArity: ResolutionTypeArity) =
            this.Entities
            |> filterEntitiesByAccessibility benv.ac
            |> Seq.filter (fun x ->
                match tyArity.TryArity with
                | ValueSome n -> x.LogicalTypeParameterCount = n
                | _ -> true
                &&
                (
                    match nameOpt with
                    | Some name -> x.Name = name
                    | _ -> true
                )
            )
            |> ImArray.ofSeq

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

        member this.GetInstanceFields() =
            this.Fields
            |> ImArray.filter (fun x -> x.IsInstance)

        member this.FindIntrinsicFunctions(benv, queryMemberFlags, funcFlags) =
            queryMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags None this

        member this.FindIntrinsicFunctions(benv, queryMemberFlags, funcFlags, name) =
            queryMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags (Some name) this

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

        member this.FindField(name: string) =
            this.Fields
            |> ImArray.find (fun x -> x.Name = name)

        member this.FindFields(benv, queryMemberFlags) =
            queryFieldsOfType benv queryMemberFlags ValueFlags.None None this

        member this.FindFields(benv, queryMemberFlags, name) =
            queryFieldsOfType benv queryMemberFlags ValueFlags.None (Some name) this

        member this.FindProperties(benv, queryMemberFlags, queryField) =
            queryPropertiesOfType benv queryMemberFlags ValueFlags.None None queryField this

        member this.FindProperties(benv, queryMemberFlags, queryField, name) =
            queryPropertiesOfType benv queryMemberFlags ValueFlags.None (Some name) queryField this

        member this.FindFunctions(benv, queryMemberFlags, funcFlags, queryFunc) =
            queryMostSpecificFunctionsOfType benv queryMemberFlags funcFlags None queryFunc this

        member this.FindFunctions(benv, queryMemberFlags, funcFlags, queryFunc, name) =
            queryMostSpecificFunctionsOfType benv queryMemberFlags funcFlags (Some name) queryFunc this

        member this.FindNestedEntities(benv, nameOpt, resTyArity) =
            let ty = findIntrinsicTypeIfPossible benv this
            match stripTypeEquations ty with
            | TypeSymbol.Entity(ent) ->
                ent.FindNestedEntities(benv, nameOpt, resTyArity)
            | _ ->
                ImArray.empty

