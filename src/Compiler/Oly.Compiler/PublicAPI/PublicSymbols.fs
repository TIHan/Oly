[<AutoOpen>]
module rec Oly.Compiler.Symbols

open System
open System.Threading
open System.Diagnostics
open System.Collections.Immutable
open Oly.Core
open Oly.Metadata
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.CompilerImports
open Oly.Compiler.Internal.SymbolQuery
open Oly.Compiler.Internal.SymbolQuery.Extensions

type ISymbolCollector =

    abstract CompareSyntax : syntaxNode1: OlySyntaxNode * syntaxNode2: OlySyntaxNode -> bool

    abstract FilterSyntax : syntaxNode: OlySyntaxNode -> bool

    abstract CollectSymbol : symbolUseInfo: OlySymbolUseInfo -> unit

let private stripRetargetedEntitySymbol (symbol: EntitySymbol) : EntitySymbol =
    match symbol with
    | :? RetargetedEntitySymbol as symbol ->
        symbol.Original
    | _ ->
        symbol

let private stripRetargetedValueSymbol (symbol: IValueSymbol) : IValueSymbol =
    match symbol with
    | :? RetargetedFieldSymbol as symbol ->
        symbol.Original
    | :? RetargetedFunctionSymbol as symbol ->
        symbol.Original
    | :? RetargetedPatternSymbol as symbol ->
        symbol.Original
    | :? RetargetedPropertySymbol as symbol ->
        symbol.Original
    | _ ->
        symbol

[<AbstractClass>]
type OlySymbol internal () =

    abstract Name : string

    abstract TryGetDefinitionLocation: OlyBoundModel * ct: CancellationToken -> OlySourceLocation option

    abstract IsSimilarTo: OlySymbol -> bool

    abstract IsEqualTo: OlySymbol -> bool

    abstract IsFormalEqualTo: OlySymbol -> bool

    abstract IsExported: bool

    abstract IsImported: bool

    abstract IsInLocalScope: bool

    member this.IsType =
        match this with
        | :? OlyTypeSymbol -> true
        | _ -> false

    member this.IsFunction =
        match this with
        | :? OlyValueSymbol as symbol -> symbol.IsFunction
        | _ -> false

    member this.IsConstructor =
        match this with
        | :? OlyValueSymbol as symbol -> symbol.IsConstructor
        | _ -> false

    member this.IsProperty =
        match this with
        | :? OlyValueSymbol as symbol -> symbol.IsProperty
        | _ -> false

    member this.IsField =
        match this with
        | :? OlyValueSymbol as symbol -> symbol.IsField
        | _ -> false

    member this.IsFieldOrAutoProperty =
        match this with
        | :? OlyValueSymbol as symbol -> symbol.IsField || symbol.Internal.IsAutoProperty
        | _ -> false

    member this.IsFunctionGroup =
        match this with
        | :? OlyFunctionGroupSymbol -> true
        | _ -> false

    member this.AsFunctionGroup =
        this :?> OlyFunctionGroupSymbol

    member this.AsValue =
        this :?> OlyValueSymbol

    member this.AsType =
        this :?> OlyTypeSymbol

    member this.AsConstant =
        this :?> OlyConstantSymbol

[<Sealed>][<DebuggerDisplay("{SignatureText}")>] 
type OlyNamespaceSymbol internal (ent: EntitySymbol) =
    inherit OlySymbol()

    let mutable tys = ValueNone
    let mutable namespaces = ValueNone

    member internal _.Internal = ent

    override _.Name = ent.Name

    override _.TryGetDefinitionLocation(_, ct) = 
        ct.ThrowIfCancellationRequested()
        None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyNamespaceSymbol as symbol ->
            areEntitiesEqual this.Internal.Formal symbol.Internal.Formal
        | _ ->
            false

    override this.IsEqualTo(symbol) =
        this.IsSimilarTo(symbol)

    override this.IsFormalEqualTo(symbol) =
        this.IsEqualTo(symbol)

    override this.IsExported = false

    override this.IsImported = false

    override this.IsInLocalScope = false

    member this.FullyQualifiedName =
        ent.FullNamespacePath
        |> String.concat "."

    member _.Types =
        match tys with
        | ValueNone ->
            tys <-
                ent.Entities
                |> ImArray.filter (fun x -> not x.IsNamespace)
                |> ImArray.map (fun x -> OlyTypeSymbol(x.AsType))
                |> ValueSome
        | _ ->
            ()
        tys.Value

    member _.Namespaces =
        match namespaces with
        | ValueNone ->
            namespaces <-
                ent.Entities
                |> ImArray.filter (fun x -> x.IsNamespace)
                |> ImArray.map (fun x -> OlyNamespaceSymbol(x))
                |> ValueSome
        | _ ->
            ()
        namespaces.Value

[<Sealed>][<DebuggerDisplay("{SignatureText}")>] 
type OlyTypeSymbol internal (ty: TypeSymbol) =
    inherit OlySymbol()

    member internal _.Internal = ty

    member _.IsTypeParameter =
        match ty with
        | TypeSymbol.Variable _ 
        | TypeSymbol.HigherVariable _ -> true
        | _ -> false

    override _.Name = ty.Name

    member _.FullyQualifiedName =
        match ty with
        | TypeSymbol.Entity(ent) ->
            // TODO: Fix this. This doesn't give correct syntax.
            ent.QualifiedName
        | _ ->
            ty.Name

    override _.TryGetDefinitionLocation(boundModel, ct) =
        match stripTypeEquationsExceptAlias ty with
        | TypeSymbol.Entity(ent) ->
            match boundModel.TryFindDefinition(ent, ct) with
            | res when res.IsSome -> res
            | _ ->
                match ent.ContainingAssembly with
                | Some asm ->
                    boundModel.TryFindExternalDefinition(asm.Identity, ent, ct)
                | _ ->
                    None
        | TypeSymbol.Variable(tyPar)
        | TypeSymbol.HigherVariable(tyPar, _) ->
            boundModel.TryFindDefinition(tyPar, ct)
        | _ ->
            None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyTypeSymbol as symbol ->
            match this.Internal, symbol.Internal with
            | TypeSymbol.Entity(ent1), TypeSymbol.Entity(ent2) ->
                let formal1 = stripRetargetedEntitySymbol ent1.Formal
                let formal2 = stripRetargetedEntitySymbol ent2.Formal
                areEntitiesEqual formal1 formal2
            | _ ->
                areTypesEqual this.Internal symbol.Internal
        | _ ->
            false

    static member private IsEqualToInternal(ty1: TypeSymbol, ty2: TypeSymbol) =
        match ty1, ty2 with
        | TypeSymbol.Entity(ent1), TypeSymbol.Entity(ent2) ->
            if ent1.TypeArguments.Length = ent2.TypeArguments.Length then
                let formal1 = stripRetargetedEntitySymbol ent1.Formal
                let formal2 = stripRetargetedEntitySymbol ent2.Formal

                areEntitiesEqual formal1 formal2 &&
                (
                    (ent1.TypeArguments, ent2.TypeArguments)
                    ||> ImArray.forall2 (fun x y -> 
                        OlyTypeSymbol.IsEqualToInternal(x, y)
                    )
                )
            else
                false
        | _ ->
            areTypesEqual ty1 ty2

    override this.IsEqualTo(symbol) =
        match symbol with
        | :? OlyTypeSymbol as symbol ->
            OlyTypeSymbol.IsEqualToInternal(this.Internal, symbol.Internal)
        | _ ->
            false

    override this.IsFormalEqualTo(symbol) =
        match symbol with
        | :? OlyTypeSymbol as symbol ->
            OlyTypeSymbol.IsEqualToInternal(this.Internal.Formal, symbol.Internal.Formal)
        | _ ->
            false

    override this.IsExported = ty.IsExported

    override this.IsImported = ty.IsImported

    override this.IsInLocalScope =
        match ty with
        | TypeSymbol.Entity(ent) -> ent.IsLocal
        | _ -> true

    member _.Fields =
        ty.FindFields(BoundEnvironment.Empty, QueryMemberFlags.StaticOrInstance)
        |> Seq.map (fun x -> 
            OlyValueSymbol(x)
        )

    member _.Functions =
        ty.FindMostSpecificFunctions(BoundEnvironment.Empty, QueryMemberFlags.StaticOrInstance, FunctionFlags.None, QueryFunction.Intrinsic)
        |> ImArray.map (fun x ->
            OlyValueSymbol(x)
        )

    member _.ImmediateFunctions =
        ty.Functions
        |> ImArray.map (fun x ->
            OlyValueSymbol(x)
        )

    member _.Properties =
        ty.FindMostSpecificProperties(BoundEnvironment.Empty, QueryMemberFlags.StaticOrInstance, QueryProperty.Intrinsic)
        |> Seq.map (fun x ->
            OlyValueSymbol(x)
        )

    member _.Types =
        ty.FindNestedEntities(BoundEnvironment.Empty, None, ResolutionTypeArity.Any)
        |> ImArray.map (fun x -> OlyTypeSymbol(x.AsType))

    member _.IsInterface = ty.IsInterface_ste

    member _.IsTypeExtension = ty.IsTypeExtension_ste

    member _.IsEnum = ty.IsEnum_ste

    /// Is type symbol a reference type?
    member _.IsReference = ty.IsReference_ste

    member _.IsShape = ty.IsShape_ste

    member _.IsTuple = ty.IsAnyTuple

    /// Is type symbol a reference type?
    member _.IsValue = ty.IsAnyStruct_ste

    member _.IsBuiltIn = ty.IsBuiltIn_ste

    member _.IsModule = ty.IsModule_ste

    member _.IsAlias = ty.IsAlias_steea

    member _.IsUnit = ty.IsUnit_ste

    member _.IsAnyArray = ty.IsAnyArray_ste

    /// Is it an immutable array?
    member _.IsArray = ty.IsArray

    /// Is it a mutable array?
    member _.IsMutableArray = ty.IsMutableArray_t

    member _.IsTypeAnyByRef = ty.IsAnyByRef_ste

    member _.GetTupleItemSignatureTexts() =
        match stripTypeEquations ty with
        | TypeSymbol.Tuple(elementTys, elementNames) ->
            if elementTys.Length = elementNames.Length then
                (elementNames, elementTys)
                ||> ImArray.map2 (fun name ty -> $"{name}: {printTypeDefinition BoundEnvironment.Empty ty}")
            else
                elementTys
                |> ImArray.map (fun ty -> printTypeDefinition BoundEnvironment.Empty ty)
        | _ ->
            ImArray.empty       

    member _.LogicalTypeParameterCount = 
        // REVIEW: This is kind of weird, why doesn't `ty.LogicalTypeParameterCount` return the same for tuples?
        match stripTypeEquations ty with
        | TypeSymbol.Tuple(elementTys=itemTys) -> itemTys.Length
        | _ ->
            ty.LogicalTypeParameterCount

    member this.StripByRef() =
        match ty.TryByReferenceElementType with
        | ValueSome ty -> OlyTypeSymbol(ty)
        | _ -> this

    member _.Extends =
        ty.Inherits
        |> ImArray.map (fun x -> OlyTypeSymbol(x))

    member _.Implements =
        ty.Implements
        |> ImArray.map (fun x -> OlyTypeSymbol(x))

    member _.Enclosing: OlyEnclosingSymbol = OlyEnclosingSymbol(ty.Enclosing)

    member this.IsSubTypeOf(superTy: OlyTypeSymbol) =
        match superTy.Internal, this.Internal with
        | TypeSymbol.Entity(superEnt), TypeSymbol.Entity(ent) ->
            let superFormal = stripRetargetedEntitySymbol superEnt.Formal
            let formal = stripRetargetedEntitySymbol ent.Formal
            subsumesEntity superFormal formal
        | _ ->
            subsumesType superTy.Internal this.Internal
            //subsumesTypeInEnvironment benv superTy.Internal this.Internal

    member this.Documentation =
        match ty with
        | TypeSymbol.Entity(ent) -> ent.Documentation.Trim()
        | _ -> String.Empty

    member this.TypeArguments =
        ty.TypeArguments
        |> ImArray.map (fun x -> OlyTypeSymbol(x))

    /// This only returns if all the types are structs with primitives.
    member this.TryGetPackedSizeInBytes() =
        let visited = System.Collections.Generic.Dictionary(TypeSymbolComparer());
        let rec tryGetSizeInBytes ty =
            let ty = stripTypeEquationsAndBuiltIn ty
            match visited.TryGetValue(ty) with
            | true, v -> v
            | _ ->
                let v =
                    match ty with
                    | TypeSymbol.UInt8
                    | TypeSymbol.Int8 -> 1
                    | TypeSymbol.UInt16
                    | TypeSymbol.Int16 
                    | TypeSymbol.Char16 -> 2
                    | TypeSymbol.ConstantInt32 _ 
                    | TypeSymbol.UInt32
                    | TypeSymbol.Int32 
                    | TypeSymbol.Float32 -> 4
                    | TypeSymbol.UInt64
                    | TypeSymbol.Int64 
                    | TypeSymbol.Float64 -> 8
                    | ty ->
                        if ty.IsAnyStruct_ste then
                            let mutable isValid = true
                            let fieldBytes =
                                ty.GetInstanceFields()
                                |> ImArray.map (fun x -> 
                                    match tryGetSizeInBytes x.Type with
                                    | -1 ->
                                        isValid <- false
                                        -1
                                    | v ->
                                        v)
                            if fieldBytes.IsEmpty || not isValid then
                                -1
                            elif fieldBytes.Length > 1 then
                                fieldBytes
                                |> ImArray.reduce (+)
                            else
                                fieldBytes[0]
                        else
                            -1
                visited[ty] <- v
                v
        match tryGetSizeInBytes ty with
        | -1 -> ValueNone
        | v -> ValueSome v

and [<NoComparison;NoEquality;RequireQualifiedAccess>] OlyConstant =
    | UInt8 of value: uint8
    | Int8 of value: int8
    | UInt16 of value: uint16
    | Int16 of value: int16
    | UInt32 of value: uint32
    | Int32 of value: int32
    | UInt64 of value: uint64
    | Int64 of value: int64
    | Float32 of value: float32
    | Float64 of value: float
    | Char16 of value: char
    | Utf16 of value: string
    | Array of elementTy: OlyTypeSymbol * values: OlyConstant imarray
    | Variable of OlyTypeSymbol
    | External of OlyValueSymbol
    | True
    | False
    | Null
    | Default
    | Error

    member this.AsInt32 =
        match this with
        | Int32 value -> value
        | _ -> failwith "Expected Int32"

[<Sealed>] 
type OlyConstantSymbol internal (internalLiteral: BoundLiteral) =
    inherit OlySymbol()

    let rec constantToConstant (constant: ConstantSymbol) =
        match constant with
        | ConstantSymbol.UInt8(value) -> OlyConstant.UInt8(value)
        | ConstantSymbol.Int8(value) -> OlyConstant.Int8(value)
        | ConstantSymbol.UInt16(value) -> OlyConstant.UInt16(value)
        | ConstantSymbol.Int16(value) -> OlyConstant.Int16(value)
        | ConstantSymbol.UInt32(value) -> OlyConstant.UInt32(value)
        | ConstantSymbol.Int32(value) -> OlyConstant.Int32(value)
        | ConstantSymbol.UInt64(value) -> OlyConstant.UInt64(value)
        | ConstantSymbol.Int64(value) -> OlyConstant.Int64(value)
        | ConstantSymbol.Float32(value) -> OlyConstant.Float32(value)
        | ConstantSymbol.Float64(value) -> OlyConstant.Float64(value)
        | ConstantSymbol.True -> OlyConstant.True
        | ConstantSymbol.False -> OlyConstant.False
        | ConstantSymbol.Char16(value) -> OlyConstant.Char16(value)
        | ConstantSymbol.Utf16(value) -> OlyConstant.Utf16(value)
        | ConstantSymbol.Array(elementTy, values) ->
            OlyConstant.Array(
                OlyTypeSymbol(elementTy), 
                (
                    values
                    |> ImArray.map (fun x ->
                        constantToConstant x
                    )
                )
            )
        | ConstantSymbol.TypeVariable(tyPar) ->
            OlyConstant.Variable(OlyTypeSymbol(tyPar.AsType))
        | ConstantSymbol.External(func) ->
            OlyConstant.External(
                OlyValueSymbol(func)
            )
        | ConstantSymbol.Error -> OlyConstant.Error

    member internal _.Internal = internalLiteral

    member _.Value =
        let rec f internalLiteral =
            match internalLiteral with
            | BoundLiteral.Constant(cns) -> constantToConstant cns
            | BoundLiteral.NullInference _ -> OlyConstant.Null
            | BoundLiteral.NumberInference(lazyInternalLiteral, _) when lazyInternalLiteral.IsValueCreated -> 
                match lazyInternalLiteral.Value with
                | Ok(literal) ->
                    OlyConstantSymbol(literal).Value
                | _ ->
                    OlyConstant.Error
            | BoundLiteral.DefaultInference _ -> OlyConstant.Default
            | BoundLiteral.ConstantEnum(constant, _) ->
                constantToConstant constant
            | _ -> OlyConstant.Error
        f internalLiteral

    member this.Type =
        OlyTypeSymbol(internalLiteral.Type)

    override _.Name = ""

    override _.TryGetDefinitionLocation(_, ct) = 
        ct.ThrowIfCancellationRequested()
        None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyConstantSymbol as symbol ->
            areLiteralsEqual this.Internal symbol.Internal
        | _ ->
            false

    override this.IsEqualTo(symbol) =
        this.IsSimilarTo(symbol)

    override this.IsFormalEqualTo(symbol) =
        this.IsEqualTo(symbol)

    override this.IsExported =
        match this.Value with
        | OlyConstant.Variable(ty) -> ty.IsExported
        | OlyConstant.External(func) -> func.IsExported
        | _ -> false

    override this.IsImported =
        match this.Value with
        | OlyConstant.Variable(ty) -> ty.IsImported
        | OlyConstant.External(func) -> func.IsImported
        | _ -> false

    override this.IsInLocalScope = false

[<Sealed>][<DebuggerDisplay("{SignatureText}")>] 
type OlyFunctionGroupSymbol internal (funcGroup: FunctionGroupSymbol) =
    inherit OlyValueSymbol(funcGroup)

    member internal _.Internal: FunctionGroupSymbol = funcGroup

    override _.TryGetDefinitionLocation(_, ct) = 
        ct.ThrowIfCancellationRequested()
        None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyValueSymbol as symbol ->
            funcGroup.Functions
            |> ImArray.exists (fun x -> areValueSignaturesEqual x.Formal symbol.Internal.Formal)
        | :? OlyTypeSymbol when this.IsConstructor ->
            match this.Enclosing.TryType with
            | Some tySymbol ->
                tySymbol.IsSimilarTo(symbol)
            | _ ->
                false
        | _ ->
            false

    override this.IsEqualTo _ =
        false

    override this.IsExported = false

    override this.IsImported = false

    override this.IsInLocalScope =
        funcGroup.IsLocal

    member _.Functions =
        funcGroup.Functions
        |> Seq.map (fun x -> OlyValueSymbol(x))
        |> ImmutableArray.CreateRange

    // TODO: Consider creating an OlyFunctionSymbol
[<DebuggerDisplay("{SignatureText}")>] 
type OlyValueSymbol internal (value: IValueSymbol) =
    inherit OlySymbol()

    member internal _.Internal: IValueSymbol = value

    override _.Name =
        if value.IsConstructor then
            value.Enclosing.AsType.Name
        else
            value.Name

    override _.TryGetDefinitionLocation(boundModel, ct) =
        if value.IsLocal then
            boundModel.TryFindDefinition(value, ct)
        else
            match boundModel.TryFindDefinition(value, ct) with
            | res when res.IsSome -> res
            | _ ->
                match value.Enclosing.TryEntity |> Option.bind (fun x -> x.ContainingAssembly) with
                | Some asm ->
                    boundModel.TryFindExternalDefinition(asm.Identity, value, ct)
                | _ ->
                    None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyValueSymbol as symbol ->
            match this.IsInLocalScope, symbol.IsInLocalScope with
            | true, true ->
                this.Internal.Formal.Id = symbol.Internal.Formal.Id
            | false, false ->
                let formal1 = stripRetargetedValueSymbol this.Internal.Formal
                let formal2 = stripRetargetedValueSymbol symbol.Internal.Formal
                if areValueSignaturesEqual formal1 formal2 then
                    true
                else
                    if formal1.IsProperty && formal2.IsField then
                        match formal2 with
                        | :? IFieldSymbol as field ->
                            match field.AssociatedFormalPropertyId with
                            | Some(associatedFormalPropId) ->
                                associatedFormalPropId = formal1.Id
                            | _ ->
                                false
                        | _ ->
                            false
                    elif formal1.IsField && formal2.IsProperty then
                        match formal1 with
                        | :? IFieldSymbol as field ->
                            match field.AssociatedFormalPropertyId with
                            | Some(associatedFormalPropId) ->
                                associatedFormalPropId = formal2.Id
                            | _ ->
                                false
                        | _ ->
                            false
                    else
                        false
            | _ ->
                false
        | :? OlyTypeSymbol when this.IsConstructor ->
            match this.Enclosing.TryType with
            | Some tySymbol ->
                tySymbol.IsSimilarTo(symbol)
            | _ ->
                false
        | _ ->
            false

    override this.IsEqualTo symbol =
        match symbol with
        | :? OlyValueSymbol as symbol -> 
            this.Internal.Name = symbol.Internal.Name &&
            areValueSignaturesEqual this.Internal symbol.Internal &&
            areEnclosingsEqual this.Internal.Enclosing symbol.Internal.Enclosing
        | _ -> 
            false   

    override this.IsFormalEqualTo symbol =
        match symbol with
        | :? OlyValueSymbol as symbol -> 
            this.Internal.Formal.Name = symbol.Internal.Formal.Name &&
            areValueSignaturesEqual this.Internal.Formal symbol.Internal.Formal &&
            areEnclosingsEqual this.Internal.Formal.Enclosing symbol.Internal.Formal.Enclosing
        | _ -> 
            false        

    override this.IsExported = value.IsExported

    override this.IsImported = value.IsImported

    override _.IsInLocalScope = value.IsLocal

    member _.ReturnType = 
        match value with
        | :? IFunctionSymbol as func ->
            OlyTypeSymbol(func.ReturnType)
            |> Some
        | _ ->
            None

    member _.Type = OlyTypeSymbol(value.Type)

    member _.Enclosing: OlyEnclosingSymbol = OlyEnclosingSymbol(value.Enclosing)

    member this.IsExternal =
        match value.Enclosing with
        | EnclosingSymbol.Entity(ent) -> ent.IsImported
        | _ -> false

    member this.IsFunction = value.IsFunction

    member this.IsFunctionGroup = value.IsFunctionGroup

    member this.IsConstructor = value.IsConstructor

    member this.IsOperator = value.IsFunction && OlySyntaxFacts.IsOperator value.Name

    member this.IsField = value.IsField

    member this.IsBackingFieldForProperty = value.IsField && (value :?> IFieldSymbol).AssociatedFormalPropertyId.IsSome

    member this.IsFieldConstant =
        match value with
        | :? IFieldSymbol as field -> field.Constant.IsSome
        | _ -> false

    member this.TryFieldConstant =
        match value with
        | :? IFieldSymbol as field ->
            match field.Constant with
            | ValueSome(constant) ->
                OlyConstantSymbol(constant.ToLiteral())
                |> ValueSome
            | _ ->
                ValueNone
        | _ ->
            ValueNone

    member this.IsProperty = value.IsProperty

    member this.IsMutable = value.IsMutable

    member this.IsStatic = not value.IsInstance && not value.IsLocal

    member this.IsAbstract = value.IsAbstract

    member this.IsOverridable = value.IsOverridable

    member this.IsParameter = this.IsInLocalScope && (value.ValueFlags &&& ValueFlags.Parameter = ValueFlags.Parameter)

    member this.IsEntryPoint =
        match value with
        | :? IFunctionSymbol as func -> func.IsEntryPoint
        | _ -> false

    member this.Parameters =
        match value with
        | :? IFunctionSymbol as func ->
            func.Parameters
            |> ImArray.map (fun x -> OlyValueSymbol(x))
        | _ ->
            ImArray.empty

    member this.LogicalParameters =
        match value with
        | :? IFunctionSymbol as func ->
            func.LogicalParameters
            |> ROMem.mapAsImArray (fun x -> OlyValueSymbol(x))
        | _ ->
            ImArray.empty

    member this.LogicalParameterCount =
        match value with
        | :? IFunctionSymbol as func ->
            func.LogicalParameterCount
        | _ ->
            0

    member this.TypeParameterCount =
        match value with
        | :? IFunctionSymbol as func ->
            func.TypeParameters.Length
        | _ ->
            0

    member this.IsPure =
        value.FunctionFlags.HasFlag(FunctionFlags.Pure)

    member this.TryPropertyGetterSetter =
        match value with
        | :? IPropertySymbol as prop ->
            let getterOpt =
                prop.Getter
                |> Option.map (fun x ->
                    OlyValueSymbol(x)
                )
            let setterOpt =
                prop.Setter
                |> Option.map (fun x ->
                    OlyValueSymbol(x)
                )
            Some(getterOpt, setterOpt)
        | _ ->
            None

    member this.IsGetterFunction = 
        match value with
        | :? IFunctionSymbol as func ->
            func.Semantic = FunctionSemantic.GetterFunction
        | _ ->
            false

    member this.IsSetterFunction = 
        match value with
        | :? IFunctionSymbol as func ->
            func.Semantic = FunctionSemantic.SetterFunction
        | _ ->
            false

    member this.IsPatternFunction = 
        match value with
        | :? IFunctionSymbol as func ->
            func.Semantic = FunctionSemantic.PatternFunction
        | _ ->
            false

    member this.IsPatternGuardFunction =
        match value with
        | :? IFunctionSymbol as func ->
            func.Semantic = FunctionSemantic.PatternGuardFunction
        | _ ->
            false

    member this.IsNormalFunction =
        match value with
        | :? IFunctionSymbol as func ->
            func.Semantic = FunctionSemantic.NormalFunction
        | _ ->
            false

    member this.ForEachAttribute f =
        let attrs =
            match value with
            | :? IFunctionSymbol as func -> func.Attributes
            | :? IPatternSymbol as pat -> pat.Attributes
            | :? IFieldSymbol as field -> field.Attributes
            | :? ILocalParameterSymbol as par -> par.Attributes
            | _ -> ImArray.empty
        attrs
        |> ImArray.iter (fun attr -> f(OlyAttributeSymbol(attr)))

    override this.GetHashCode() = value.Formal.Id.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? OlyValueSymbol as valueSymbol ->
            areValueSignaturesEqual value valueSymbol.Internal
        | _ ->
            false

[<Sealed>] 
type OlyAttributeSymbol internal (attr: AttributeSymbol) =
    inherit OlySymbol()

    member internal _.Internal = attr

    override _.Name =
        match attr with
        | AttributeSymbol.Open -> "open"
        | AttributeSymbol.Null -> "null"
        | AttributeSymbol.Import _ -> "import"
        | AttributeSymbol.Export -> "export"
        | AttributeSymbol.Intrinsic _ -> "intrinsic"
        | AttributeSymbol.Inline _ -> "inline"
        | AttributeSymbol.Unmanaged _ -> "unmanaged"
        | AttributeSymbol.Blittable -> "blittable"
        | AttributeSymbol.Pure -> "pure"
        | AttributeSymbol.Constructor(ctor, _, _, _) -> ctor.Enclosing.AsType.Name

    override this.IsExported =
        match attr with
        | AttributeSymbol.Constructor(ctor=ctor) -> ctor.IsExported
        | _ -> false

    override this.IsImported =
        match attr with
        | AttributeSymbol.Constructor(ctor=ctor) -> ctor.IsImported
        | _ -> false

    override _.IsInLocalScope = false

    override this.TryGetDefinitionLocation(boundModel, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        match attr with
        | AttributeSymbol.Constructor(ctor, _, _, _) -> OlyValueSymbol(ctor).TryGetDefinitionLocation(boundModel, ct)
        | _ -> None

    override this.IsSimilarTo(symbol) =
        match attr with
        | AttributeSymbol.Constructor(ctor, _, _, _) ->
            OlyValueSymbol(ctor).IsSimilarTo(symbol)
        | _ ->
            match symbol with
            | :? OlyAttributeSymbol as symbol ->
                match attr, symbol.Internal with
                | AttributeSymbol.Import(platform1, path1, name1), AttributeSymbol.Import(platform2, path2, name2) ->
                    platform1 = platform2 && path1 = path2 && name1 = name2
                | AttributeSymbol.Export, AttributeSymbol.Export ->
                    true
                | AttributeSymbol.Inline _, AttributeSymbol.Inline _ -> 
                    true
                | _ -> 
                    false
            | _ ->
                false

    override this.IsEqualTo(symbol) =
        // TODO: Implement
        false

    override this.IsFormalEqualTo(symbol) =
        // TODO: Implement
        false

[<Sealed>] 
type OlyEnclosingSymbol internal (enclosing: EnclosingSymbol) =

    member _.TryType: OlyTypeSymbol option =
        match enclosing with
        | EnclosingSymbol.Entity(ent) when not ent.IsNamespace -> OlyTypeSymbol(ent.AsType) |> Some
        | EnclosingSymbol.Witness(concreteTy, _) -> OlyTypeSymbol(concreteTy) |> Some
        | _ -> None

    member _.TryNamespace =
        match enclosing with
        | EnclosingSymbol.Entity(ent) when ent.IsNamespace -> OlyNamespaceSymbol(ent) |> Some
        | _ -> None

    member this.IsStruct =
        match enclosing with
        | EnclosingSymbol.Entity(ent) -> ent.IsAnyStruct
        | _ -> false

    member this.IsTypeExtension =
        match enclosing with
        | EnclosingSymbol.Entity(ent) -> ent.IsTypeExtension
        | _ -> false

[<Sealed>] 
type OlyDirectiveSymbol internal (name: string, value: string) =
    inherit OlySymbol()

    override _.Name = name

    override _.TryGetDefinitionLocation(_, _) = None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyDirectiveSymbol as symbol ->
            name = symbol.Name && symbol.Value = value
        | _ ->
            false

    override this.IsEqualTo(symbol) =
        this.IsSimilarTo(symbol)

    override this.IsFormalEqualTo(symbol) =
        this.IsEqualTo(symbol)

    override this.IsExported = false

    override this.IsImported = false

    override _.IsInLocalScope = false

    member _.Value = value

[<Sealed>] 
type OlyConditionalDirectiveSymbol internal () =
    inherit OlySymbol()

    override _.Name = String.Empty

    override _.TryGetDefinitionLocation(_, _) = None

    override this.IsSimilarTo(_) = false

    override this.IsEqualTo(_) = false

    override this.IsFormalEqualTo(symbol) =
        this.IsEqualTo(symbol)

    override this.IsExported = false

    override this.IsImported = false

    override _.IsInLocalScope = false

[<Sealed>]
type OlySymbolUseInfo internal (symbol: OlySymbol, subModel: OlyBoundSubModel) =

    member internal _.InternalEnvironment: BoundEnvironment = subModel.InternalEnvironment

    member _.SubModel: OlyBoundSubModel = subModel

    member _.Symbol: OlySymbol = symbol

    member _.Syntax: OlySyntaxNode = subModel.SyntaxNode

    member this.Container: Choice<OlyEnclosingSymbol, OlyValueSymbol> =
        if symbol.IsInLocalScope then
            match this.InternalEnvironment.senv.enclosingValue with
            | Some(value) -> Choice2Of2(OlyValueSymbol(value))
            | _ -> Choice1Of2(OlyEnclosingSymbol(this.InternalEnvironment.senv.enclosing))
        else
            Choice1Of2(OlyEnclosingSymbol(this.InternalEnvironment.senv.enclosing))

    member _.IsCallee =
        if obj.ReferenceEquals(subModel.Node, null) then
            false
        else
            let mutable node: IBoundNode = subModel.Node
            if node.IsExpression then
                match System.Runtime.CompilerServices.Unsafe.As<_, BoundExpression>(&node) with
                | BoundExpression.Call(value=value) ->
                    match symbol with
                    | :? OlyValueSymbol as valueToCheck ->
                        obj.ReferenceEquals(valueToCheck.Internal, value)
                    | _ ->
                        false
                | _ ->
                    false
            else
                false

    member this.TryGetAliasedType() =
        let benv = this.InternalEnvironment
        if symbol.IsType then
            let ty = symbol.AsType.Internal
            if ty.IsAlias_steea then
                match ty with
                | TypeSymbol.Entity(ent) ->
                    if ent.Extends.IsEmpty then
                        None
                    else
                        let ty = ent.Extends[0]
                        match benv.TryFindEntityByIntrinsicType(ty) with
                        | ValueSome(ent) ->
                            Some(OlyTypeSymbol(ent.AsType))
                        | _ ->
                            Some(OlyTypeSymbol(ty))
                | _ ->
                    match ty.TryGetIntrinsicType() with
                    | true, ty ->
                        match benv.TryFindEntityByIntrinsicType(ty) with
                        | ValueSome(ent) ->
                            Some(OlyTypeSymbol(ent.AsType))
                        | _ ->
                            Some(OlyTypeSymbol(ty))
                    | _ ->
                        None
            else
                None
        else
            None

    member this.SignatureText =
        let benv = this.InternalEnvironment
        let syntax = this.Syntax
        match symbol with
        | :? OlyTypeSymbol as tySymbol ->
            if syntax.IsDefinition then
                printTypeDefinition benv tySymbol.Internal
            else
                printType benv tySymbol.Internal
        | :? OlyFunctionGroupSymbol as funcGroupSymbol ->
            funcGroupSymbol.Name
        | :? OlyValueSymbol as valueSymbol ->
            printValue benv valueSymbol.Internal
        | :? OlyConstantSymbol as constantSymbol ->
            let valueText =
                let rec f (c: OlyConstant) =
                    match c with
                    | OlyConstant.UInt8(value) -> string value
                    | OlyConstant.Int8(value) -> string value
                    | OlyConstant.UInt16(value) -> string value
                    | OlyConstant.Int16(value) -> string value
                    | OlyConstant.UInt32(value) -> string value
                    | OlyConstant.Int32(value) -> string value
                    | OlyConstant.UInt64(value) -> string value
                    | OlyConstant.Int64(value) -> string value
                    | OlyConstant.Float32(value) -> string value
                    | OlyConstant.Float64(value) -> string value
                    | OlyConstant.True -> "true"
                    | OlyConstant.False -> "false"
                    | OlyConstant.Char16(value) -> $"'{value}'"
                    | OlyConstant.Utf16(value) -> $"\"{value}\""
                    | OlyConstant.Null -> "null"
                    | OlyConstant.Default -> "default"
                    | OlyConstant.Array(_, values) ->
                        let innerText =
                            values
                            |> ImArray.map f
                            |> String.concat ";"
                        $"[{innerText}]"
                    | OlyConstant.Variable(ty) -> OlySymbolUseInfo(ty, OlyBoundSubModel(subModel.Root, syntax, subModel.Node)).SignatureText
                    | OlyConstant.External(func) -> OlySymbolUseInfo(func, OlyBoundSubModel(subModel.Root, syntax, subModel.Node)).SignatureText
                    | OlyConstant.Error -> "?"
                f constantSymbol.Value
            $"{valueText}: {OlySymbolUseInfo(constantSymbol.Type, OlyBoundSubModel(subModel.Root, syntax, subModel.Node)).SignatureText}"


        | :? OlyNamespaceSymbol as symbol ->
            let names = ResizeArray()
            let rec loop (enclosing: EnclosingSymbol) =
                match enclosing with
                | EnclosingSymbol.Entity(ent) ->
                    names.Add(ent.Name)
                    loop enclosing.Enclosing
                | _ ->
                    ()
            loop symbol.Internal.AsEnclosing
            names |> Seq.rev |> String.concat "."

        | :? OlyAttributeSymbol as symbol ->
                match symbol.Internal with
                | AttributeSymbol.Open -> "open"
                | AttributeSymbol.Null -> "null"
                | AttributeSymbol.Blittable -> "blittable"
                | AttributeSymbol.Pure -> "pure"
                | AttributeSymbol.Import(platform, path, name) -> 
                    $"import(\"{platform}\", \"{path}\", \"{name}\")"
                | AttributeSymbol.Export -> 
                    "export"
                | AttributeSymbol.Intrinsic name -> 
                    $"intrinsic(\"{name}\")"
                | AttributeSymbol.Inline(inlineArg) -> 
                    match inlineArg with
                    | InlineArgumentSymbol.None -> "inline"
                    | InlineArgumentSymbol.Never -> "inline(never)"
                    | InlineArgumentSymbol.Always -> "inline(always)"
                | AttributeSymbol.Unmanaged(unmanagedArg) -> 
                    match unmanagedArg with
                    | UnmanagedArgumentSymbol.AllocationOnly -> "unmanaged(allocation_only)"
                | AttributeSymbol.Constructor(ctor, _, _, _) -> 
                    OlySymbolUseInfo(OlyValueSymbol(ctor), OlyBoundSubModel(subModel.Root, syntax, subModel.Node)).SignatureText

        | :? OlyDirectiveSymbol as symbol ->
            $"#{symbol.Name} \"{symbol.Value}\""

        | _ ->
            String.Empty

[<Sealed>]
type OlySymbolUseInfo<'T when 'T :> OlySymbol> internal (symbol: 'T, subModel: OlyBoundSubModel) =
    let untypedInfo = OlySymbolUseInfo(symbol, subModel)

    member _.Symbol = symbol

    member _.UntypedInfo = untypedInfo   

// TODO: Weird name.
[<Sealed>] 
type OlyBoundSubModel internal (boundModel: OlyBoundModel, syntax: OlySyntaxNode, boundNode: IBoundNode) =

    member _.Root: OlyBoundModel = boundModel

    member internal _.Node = boundNode

    member internal _.InternalEnvironment =
        if obj.ReferenceEquals(boundNode, null) then
            boundModel.GetBoundTree(CancellationToken.None).RootEnvironment
        else
            match boundNode.TryGetEnvironment() with
            | Some(benv) -> benv
            | _ -> boundModel.GetBoundTree(CancellationToken.None).RootEnvironment

    member this.GetSignatureText(symbol: OlySymbol) =
        let symbolInfo = OlySymbolUseInfo(symbol, OlyBoundSubModel(boundModel, syntax, boundNode))
        symbolInfo.SignatureText

    member this.TryGetAliasType(symbol: OlySymbol) =
        let symbolInfo = OlySymbolUseInfo(symbol, OlyBoundSubModel(boundModel, syntax, boundNode))
        symbolInfo.TryGetAliasedType()

    member this.GetFunctions(symbol: OlyTypeSymbol) =
        symbol.Internal.FindMostSpecificFunctions(this.InternalEnvironment, QueryMemberFlags.StaticOrInstance, FunctionFlags.None, QueryFunction.IntrinsicAndExtrinsic)
        |> ImArray.map (fun x ->
            OlyValueSymbol(x)
        )

    member this.GetProperties(symbol: OlyTypeSymbol) =
        symbol.Internal.FindMostSpecificProperties(this.InternalEnvironment, QueryMemberFlags.StaticOrInstance, QueryProperty.IntrinsicAndExtrinsic)
        |> Seq.map (fun x ->
            OlyValueSymbol(x)
        )

    member this.IsUnqualified(symbol: OlySymbol) =
        match symbol with
        | :? OlyValueSymbol as valueSymbol ->
            let value = valueSymbol.Internal
            if valueSymbol.IsPatternFunction then
                match this.InternalEnvironment.senv.unqualifiedPatterns.TryGetValue(value.Name) with
                | true, unqualifiedSymbol ->
                    match unqualifiedSymbol with
                    | UnqualifiedSymbol.Local(x) -> x.Formal.Id = value.Formal.Id
                    | UnqualifiedSymbol.AmbiguousValues(xs) -> xs |> ImArray.exists (fun x -> x.Formal.Id = value.Formal.Id)
                    | UnqualifiedSymbol.Field(x) -> x.Formal.Id = value.Formal.Id
                    | UnqualifiedSymbol.Function(x) -> x.Formal.Id = value.Formal.Id
                    | UnqualifiedSymbol.FunctionGroup(x) -> (x.Functions |> ImArray.exists (fun x -> x.Formal.Id = value.Formal.Id))
                    | UnqualifiedSymbol.Property(x) -> x.Formal.Id = value.Formal.Id
                | _ ->
                    false
            else
                match this.InternalEnvironment.senv.unqualifiedSymbols.TryGetValue(value.Name) with
                | true, unqualifiedSymbol ->
                    match unqualifiedSymbol with
                    | UnqualifiedSymbol.Local(x) -> x.Formal.Id = value.Formal.Id
                    | UnqualifiedSymbol.AmbiguousValues(xs) -> xs |> ImArray.exists (fun x -> x.Formal.Id = value.Formal.Id)
                    | UnqualifiedSymbol.Field(x) -> x.Formal.Id = value.Formal.Id
                    | UnqualifiedSymbol.Function(x) -> x.Formal.Id = value.Formal.Id
                    | UnqualifiedSymbol.FunctionGroup(x) -> (x.Functions |> ImArray.exists (fun x -> x.Formal.Id = value.Formal.Id))
                    | UnqualifiedSymbol.Property(x) -> x.Formal.Id = value.Formal.Id
                | _ ->
                    false
        | _ ->
            // TODO:
            false

    member this.GetUnqualifiedValueSymbols() =
        this.InternalEnvironment.senv.unqualifiedSymbols.Values
        |> Seq.map (fun x -> 
            match x with
            | UnqualifiedSymbol.Local value ->
                seq { OlyValueSymbol(value) }
            | UnqualifiedSymbol.Field value ->
                seq { OlyValueSymbol(value) }
            | UnqualifiedSymbol.Property value ->
                seq { OlyValueSymbol(value) }
            | UnqualifiedSymbol.Function value ->
                seq { OlyValueSymbol(value) }
            | UnqualifiedSymbol.FunctionGroup(funcGroup) ->
                seq { OlyFunctionGroupSymbol(funcGroup) :> OlyValueSymbol }
            | UnqualifiedSymbol.AmbiguousValues(values) when not values.IsEmpty ->
                seq { OlyValueSymbol(values[0]) }
            | _ ->
                Seq.empty
        )
        |> Seq.concat

    /// Gets unqualified pattern functions.
    /// Gets qualified pattern functions by one level.
    member this.GetPatternFunctionSymbols() =
        let symbols1 =
            this.InternalEnvironment.senv.unqualifiedPatterns.Values
            |> Seq.map (fun x -> 
                match x with
                | UnqualifiedSymbol.Function value when value.IsPatternFunction ->
                    seq { OlyValueSymbol(value) }
                | UnqualifiedSymbol.FunctionGroup(funcGroup) when funcGroup.IsPatternFunction ->
                    seq { OlyFunctionGroupSymbol(funcGroup) :> OlyValueSymbol }
                | _ ->
                    Seq.empty
            )
            |> Seq.concat

        let symbols2 =
            this.InternalEnvironment.senv.unqualifiedTypes.Values
            |> Seq.collect (fun tys ->
                tys.Values
                |> Seq.collect (fun tys ->
                    tys
                    |> ImArray.collect (fun ty ->
                        ty.FindMostSpecificFunctions(this.InternalEnvironment, QueryMemberFlags.PatternFunction, FunctionFlags.None, QueryFunction.IntrinsicAndExtrinsic)
                        |> ImArray.filter (fun x -> x.IsPatternFunction)
                        |> ImArray.map (fun x ->
                            OlyValueSymbol(x)
                        )
                    )
                )
            )

        Seq.append symbols1 symbols2

    /// Tries to return the type that a pattern expects to be matching on.
    member this.TryGetMatchType(syntaxNode: OlySyntaxNode, ct: CancellationToken): OlySymbolUseInfo<OlyTypeSymbol> option =

        let matchIndexOpt =
            let position = syntaxNode.TextSpan.End
            let rec loop (node: OlySyntaxNode) =
                match node with
                | :? OlySyntaxPattern as node ->
                    match node with
                    | OlySyntaxPattern.Function(_, _, argList, _)
                    | OlySyntaxPattern.Parenthesis(_, argList, _) ->
                        (node :> OlySyntaxNode, argList.TryFindIndexByPosition(position))
                        |> Some

                    | _ ->
                        Some(node, 0)

                | :? OlySyntaxMatchClause ->
                    Some(node, 0)

                | _ ->
                    if node.HasParent then
                        loop node.Parent
                    else
                        None
            loop syntaxNode

        match matchIndexOpt with
        | None -> None
        | Some(syntaxNode, matchIndex) ->

        let syntaxNodeOpt =
            let rec loop finish (syntaxNode: OlySyntaxNode) =
                match syntaxNode with
                | :? OlySyntaxExpression as syntaxExpr ->
                    match syntaxExpr with
                    | OlySyntaxExpression.Match _ -> Some(syntaxNode)
                    | _ -> None
                | :? OlySyntaxPattern as syntaxPat ->
                    match syntaxPat with
                    | OlySyntaxPattern.Parenthesis _ ->
                        if finish then
                            Some(syntaxNode)
                        else
                            if syntaxNode.HasParent then
                                loop true syntaxNode.Parent
                            else
                                None
                    | _ ->
                        Some(syntaxNode)
                | _ ->
                    if syntaxNode.HasParent then
                        loop finish syntaxNode.Parent
                    else
                        None
            loop false syntaxNode

        match syntaxNodeOpt with
        | None -> None
        | Some syntaxNode ->
            let tree = boundModel.GetBoundTree(ct)
            let mutable matchTyOpt = None

            tree.ForEachForTooling((fun boundNode -> ct.ThrowIfCancellationRequested(); matchTyOpt.IsNone && nodeContains boundNode syntaxNode),
                fun boundNode ->
                    match boundNode with
                    | :? BoundExpression as expr ->
                        match expr with
                        | BoundExpression.Match(syntax, _, matchExprs, _, _) when obj.ReferenceEquals(syntax, syntaxNode) ->
                            if matchIndex < matchExprs.Length then
                                let matchExpr = matchExprs[matchIndex]
                                match matchExpr.TryEnvironment with
                                | Some benv ->
                                    let ty = matchExpr.Type 
                                    if ty.IsSolved then
                                        matchTyOpt <- Some(matchExpr.Syntax, ty, boundNode)
                                | _ ->
                                    ()
                        | _ ->
                            ()

                    | :? BoundCasePattern as casePat ->
                        match casePat with
                        | BoundCasePattern.Function(syntaxInfo, pat, _, _) when obj.ReferenceEquals(syntaxInfo.Syntax, syntaxNode) ->
                            let returnTy = pat.PatternFunction.ReturnType
                            match stripTypeEquations returnTy with
                            | TypeSymbol.Tuple(itemTys, _) when matchIndex < itemTys.Length ->
                                let ty = itemTys[matchIndex]
                                if ty.IsSolved then
                                    matchTyOpt <- Some(syntaxInfo.Syntax, ty, boundNode)
                            | ty when ty.IsSolved ->
                                matchTyOpt <- Some(syntaxInfo.Syntax, ty, boundNode)
                            | _ ->
                                ()
                        | _ ->
                            ()
                           
                    | _ ->
                        ()
            )

            matchTyOpt
            |> Option.map (fun (syntax, ty, boundNode) ->
                OlySymbolUseInfo<OlyTypeSymbol>(
                    OlyTypeSymbol(ty),
                    OlyBoundSubModel(boundModel, syntax, boundNode)
                )
            )

    member this.GetUnqualifiedValueSymbols(containsText: string) =
        if String.IsNullOrWhiteSpace containsText then
            this.GetUnqualifiedValueSymbols()
        else
            this.InternalEnvironment.senv.unqualifiedSymbols
            |> Seq.filter (fun x -> x.Key.Contains(containsText))
            |> Seq.map (fun x -> 
                match x.Value with
                | UnqualifiedSymbol.Local value ->
                    seq { OlyValueSymbol(value) }
                | UnqualifiedSymbol.Field value ->
                    seq { OlyValueSymbol(value) }
                | UnqualifiedSymbol.Property value ->
                    seq { OlyValueSymbol(value) }
                | UnqualifiedSymbol.Function value ->
                    seq { OlyValueSymbol(value) }
                | UnqualifiedSymbol.FunctionGroup(funcGroup) ->
                    seq { OlyFunctionGroupSymbol(funcGroup) :> OlyValueSymbol }
                | UnqualifiedSymbol.AmbiguousValues(values) when not values.IsEmpty ->
                    seq { OlyValueSymbol(values[0]) }
                | _ ->
                    Seq.empty
            )
            |> Seq.concat

    member this.GetUnqualifiedTypeSymbols() =
        this.InternalEnvironment.senv.unqualifiedTypes.Values
        |> Seq.concat
        |> Seq.map (fun pair -> pair.Value)
        |> Seq.choose (fun tys ->
            if tys.IsEmpty then
                None
            elif tys.Length = 1 then
                OlyTypeSymbol(tys[0])
                |> Some
            else
                OlyTypeSymbol(TypeSymbolError)
                |> Some
        )

    member this.GetUnqualifiedTypeSymbols(containsText: string) =
        if String.IsNullOrWhiteSpace containsText then
            this.GetUnqualifiedTypeSymbols()
        else
            this.InternalEnvironment.senv.unqualifiedTypes
            |> Seq.map (fun x -> x.Value |> Seq.filter (fun x -> x.Key.Contains(containsText)))
            |> Seq.concat
            |> Seq.map (fun pair -> pair.Value)
            |> Seq.choose (fun tys ->
                if tys.IsEmpty then
                    None
                elif tys.Length = 1 then
                    OlyTypeSymbol(tys[0])
                    |> Some
                else
                    OlyTypeSymbol(TypeSymbolError)
                    |> Some
            )

    member this.GetUnqualifiedNamespaceSymbols() =
        this.InternalEnvironment.senv.namespaces.Values
        |> Seq.choose (fun group ->
            match (group :> INamespaceSymbol).Enclosing with
            | EnclosingSymbol.RootNamespace ->
                OlyNamespaceSymbol(group)
                |> Some
            | _ ->
                None
        )

    member this.GetUnqualifiedNamespaceSymbols(containsText: string) =
        if String.IsNullOrWhiteSpace containsText then
            this.GetUnqualifiedNamespaceSymbols()
        else
            this.InternalEnvironment.senv.namespaces.Values
            |> Seq.choose (fun group ->
                match (group :> INamespaceSymbol).Enclosing with
                | EnclosingSymbol.RootNamespace when (group :> EntitySymbol).Name.Contains(containsText) ->
                    OlyNamespaceSymbol(group)
                    |> Some
                | _ ->
                    None
            )

    member this.GetUnqualifiedSymbols() =
        this.GetUnqualifiedValueSymbols()
        |> Seq.map (fun x -> x :> OlySymbol)
        |> Seq.append (this.GetUnqualifiedTypeSymbols() |> Seq.map (fun x -> x :> OlySymbol))
        |> Seq.append (this.GetUnqualifiedNamespaceSymbols() |> Seq.map (fun x -> x :> OlySymbol))

    member this.GetUnqualifiedSymbols(containsText: string) =
        if String.IsNullOrWhiteSpace containsText then
            this.GetUnqualifiedSymbols()
        else
            this.GetUnqualifiedValueSymbols(containsText)
            |> Seq.map (fun x -> x :> OlySymbol)
            |> Seq.append (this.GetUnqualifiedTypeSymbols(containsText) |> Seq.map (fun x -> x :> OlySymbol))
            |> Seq.append (this.GetUnqualifiedNamespaceSymbols(containsText) |> Seq.map (fun x -> x :> OlySymbol))

    member _.SyntaxNode = syntax

//*****************************************************************************************************************************************************
//*****************************************************************************************************************************************************
//****** Symbol functions that go through the bound tree ******
//****** TODO: The inner functions in OlyBoundModel should be lifted out and placed here.
//*****************************************************************************************************************************************************
//*****************************************************************************************************************************************************

let private getTypeSymbolByIdentifier (bm: OlyBoundModel) (collector: ISymbolCollector) (syntaxIdent: OlySyntaxToken) (boundNode: IBoundNode) (ty: TypeSymbol) =
    if collector.FilterSyntax syntaxIdent then
        match ty.TryEntityNoAlias with
        | ValueSome(ent) when ent.IsNamespace ->
            collector.CollectSymbol(OlySymbolUseInfo(OlyNamespaceSymbol(ent), OlyBoundSubModel(bm, syntaxIdent, boundNode)))
        | _ ->
            collector.CollectSymbol(OlySymbolUseInfo(OlyTypeSymbol(ty), OlyBoundSubModel(bm, syntaxIdent, boundNode)))

let private getTypeSymbolByName bm (collector: ISymbolCollector) (syntaxName: OlySyntaxName) (boundNode: IBoundNode) (ty: TypeSymbol) =
    match syntaxName with
    | OlySyntaxName.Identifier(syntaxIdent) ->
        getTypeSymbolByIdentifier bm collector syntaxIdent boundNode ty

    | OlySyntaxName.Parenthesis(_, syntaxIdentOrOperator, _) ->
        getTypeSymbolByIdentifier bm collector syntaxIdentOrOperator boundNode ty

    | OlySyntaxName.Generic(syntaxName, syntaxTyArgs) ->
        getTypeSymbolByName bm collector syntaxName boundNode ty
        getTypeArgumentSymbolsWithTypes bm collector syntaxTyArgs boundNode ty.TypeArguments

    | OlySyntaxName.Qualified _ ->

        match ty.TryEntityNoAlias with
        | ValueSome(ent) ->
            (ent.AsEnclosing, syntaxName.AllNames |> List.rev)
            ||> List.fold (fun enclosing syntaxName ->
                match enclosing.TryEntity with
                | Some ent ->
                    if ent.IsNamespace then 
                        getTypeSymbolByName bm collector syntaxName boundNode ent.AsNamespaceType
                        ent.Enclosing
                    else
                        getTypeSymbolByName bm collector syntaxName boundNode ent.AsType
                        ent.Enclosing
                | _ ->
                    enclosing
            )
            |> ignore
        | _ ->
            ()

    | _ ->
        raise(InternalCompilerException())

let private getTypeArgumentSymbolsWithTypes bm (collector: ISymbolCollector) (syntaxTyArgs: OlySyntaxTypeArguments) (boundNode: IBoundNode) (tys: ImmutableArray<TypeSymbol>) =
    let values = syntaxTyArgs.Values
    (values, tys)
    ||> Seq.iter2 (fun syntaxTyArg ty ->
        getTypeSymbol bm collector syntaxTyArg boundNode ty
    )

let private getTypeSymbolByExpression bm (collector: ISymbolCollector) (syntaxExpr: OlySyntaxExpression) (boundNode: IBoundNode) (ty: TypeSymbol) : unit =
    match syntaxExpr with
    | OlySyntaxExpression.Literal(syntaxLiteral) ->
        match syntaxLiteral with
        | OlySyntaxLiteral.Default(syntaxToken)
        | OlySyntaxLiteral.UInt8(syntaxToken)
        | OlySyntaxLiteral.Int8(syntaxToken)
        | OlySyntaxLiteral.UInt16(syntaxToken)
        | OlySyntaxLiteral.Int16(syntaxToken)
        | OlySyntaxLiteral.UInt32(syntaxToken)
        | OlySyntaxLiteral.Int32(syntaxToken)
        | OlySyntaxLiteral.UInt64(syntaxToken)
        | OlySyntaxLiteral.Int64(syntaxToken)
        | OlySyntaxLiteral.Float32(syntaxToken)
        | OlySyntaxLiteral.Float64(syntaxToken)
        | OlySyntaxLiteral.Bool(syntaxToken)
        | OlySyntaxLiteral.Null(syntaxToken)
        | OlySyntaxLiteral.Integer(syntaxToken)
        | OlySyntaxLiteral.Real(syntaxToken)
        | OlySyntaxLiteral.Char16(syntaxToken)
        | OlySyntaxLiteral.Utf16(syntaxToken) ->
            if collector.FilterSyntax syntaxToken then
                collector.CollectSymbol(OlySymbolUseInfo(OlyTypeSymbol(ty), OlyBoundSubModel(bm, syntaxToken, boundNode)))
        | _ ->
            ()

    | OlySyntaxExpression.Name(syntaxName) ->
        getTypeSymbolByName bm collector syntaxName boundNode ty

    | _ ->
        ()

let private getTypeSymbol (bm: OlyBoundModel) (collector: ISymbolCollector) (syntaxTy: OlySyntaxType) (boundNode: IBoundNode) (ty: TypeSymbol) =

    let handleTupleElementList (syntaxElementList: OlySyntaxSeparatorList<OlySyntaxTupleElement>) =
        let tyArgs = ty.TypeArguments
        let syntaxElements = syntaxElementList.ChildrenOfType
        if tyArgs.IsEmpty && syntaxElements.Length = 1 then
            match syntaxElements[0] with
            | OlySyntaxTupleElement.Type(syntaxTy) ->
                getTypeSymbol bm collector syntaxTy boundNode ty
            | _ ->
                ()
        else
            (syntaxElements, ty.TypeArguments)
            ||> ImArray.tryIter2 (fun syntaxElement ty ->
                match syntaxElement with
                | OlySyntaxTupleElement.IdentifierWithTypeAnnotation(syntaxIdent, _, syntaxTy) ->
                    if collector.FilterSyntax syntaxIdent then
                        () // TODO: Capture ident for tuple element name
                    getTypeSymbol bm collector syntaxTy boundNode ty
                | OlySyntaxTupleElement.Type(syntaxTy) ->
                    getTypeSymbol bm collector syntaxTy boundNode ty
                | _ ->
                    ()
            )

    match syntaxTy with
    | OlySyntaxType.Function(syntaxInputTy, _, syntaxOutputTy)
    | OlySyntaxType.ScopedFunction(_, syntaxInputTy, _, syntaxOutputTy) ->
        match ty.TryGetFunctionWithParameters() with
        | ValueSome(argTys, returnTy) ->
            // TODO: Kind of a hack using TypeSymbol.Tuple.
            let inputTy = 
                if argTys.IsEmpty then
                    TypeSymbol.Unit
                elif argTys.Length = 1 then
                    argTys[0]
                else
                    TypeSymbol.Tuple(argTys, ImArray.empty)

            getTypeSymbol bm collector syntaxInputTy boundNode inputTy
            getTypeSymbol bm collector syntaxOutputTy boundNode returnTy
        | _ ->
            ()

    | OlySyntaxType.FunctionPtr(_, _, syntaxInputTy, _, syntaxOutputTy) ->
        match ty.TryGetFunctionWithParameters() with
        | ValueSome(argTys, returnTy) ->
            // TODO: Kind of a hack using TypeSymbol.Tuple.
            let inputTy = 
                if argTys.IsEmpty then
                    TypeSymbol.Unit
                elif argTys.Length = 1 then
                    argTys[0]
                else
                    TypeSymbol.Tuple(argTys, ImArray.empty)

            getTypeSymbol bm collector syntaxInputTy boundNode inputTy
            getTypeSymbol bm collector syntaxOutputTy boundNode returnTy
        | _ ->
            ()

    | OlySyntaxType.Name(syntaxName) ->
        getTypeSymbolByName bm collector syntaxName boundNode ty

    | OlySyntaxType.Tuple(_, syntaxTupleElementList, _) ->
        handleTupleElementList syntaxTupleElementList

    | OlySyntaxType.Variadic(syntaxIdent, _) ->
        getTypeSymbolByIdentifier bm collector syntaxIdent boundNode ty

    | OlySyntaxType.VariadicIndexer(syntaxIdent, _, _, syntaxConstExpr, _) ->
        getTypeSymbolByIdentifier bm collector syntaxIdent boundNode ty
        match stripTypeEquationsExceptAlias ty, syntaxConstExpr with
        | TypeSymbol.DependentIndexer(inputValueTy, _), OlySyntaxExpression.Name(OlySyntaxName.Identifier(syntaxIdentForConst)) ->
            getTypeSymbolByIdentifier bm collector syntaxIdentForConst boundNode inputValueTy
        | _ ->
            ()             

    | OlySyntaxType.Array(syntaxElementTy, _) ->
        if ty.TypeArguments.Length > 0 then
            getTypeSymbol bm collector syntaxElementTy boundNode ty.TypeArguments[0]

    | OlySyntaxType.MutableArray(_, syntaxElementTy, _) ->
        if ty.TypeArguments.Length > 0 then
            getTypeSymbol bm collector syntaxElementTy boundNode ty.TypeArguments[0]

    | OlySyntaxType.FixedArray(syntaxElementTy, syntaxRankBrackets) ->
        if ty.TypeArguments.Length >= 3 then
            getTypeSymbol bm collector syntaxElementTy boundNode ty.TypeArguments[0]
            match syntaxRankBrackets.Element with
            | OlySyntaxFixedArrayLength.Expression(syntaxExpr) ->
                getTypeSymbolByExpression bm collector syntaxExpr boundNode ty.TypeArguments[1]
            | _ ->
                ()

    | OlySyntaxType.MutableFixedArray(_, syntaxElementTy, syntaxRankBrackets) ->
        if ty.TypeArguments.Length >= 3 then
            getTypeSymbol bm collector syntaxElementTy boundNode ty.TypeArguments[0]
            match syntaxRankBrackets.Element with
            | OlySyntaxFixedArrayLength.Expression(syntaxExpr) ->
                getTypeSymbolByExpression bm collector syntaxExpr boundNode ty.TypeArguments[1]
            | _ ->
                ()

    | OlySyntaxType.Shape(syntaxCurlyBrackets) ->
        // TODO: Finish the rest of this for fields and properties.

        let syntaxExprs = syntaxCurlyBrackets.Element.ChildrenOfType
        let syntaxExprs =
            syntaxExprs
            |> ImArray.map (fun x -> x.FlattenSequentials())
            |> ImArray.concat

        let syntaxBindingDecls =
            syntaxExprs
            |> ImArray.choose (function
                | OlySyntaxExpression.ValueDeclaration(_, _, _, _, _, 
                        OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Function _ as syntaxBindingDecl)) ->
                    Some syntaxBindingDecl
                | _ ->
                    None
            )

        (syntaxBindingDecls, ty.Functions)
        ||> ImArray.tryIter2 (fun syntaxBindingDecl func ->
            match syntaxBindingDecl with
            | OlySyntaxBindingDeclaration.Function(syntaxFuncName, _, _, _, _) ->
                match syntaxFuncName with
                | OlySyntaxFunctionName.Identifier(syntaxToken) 
                | OlySyntaxFunctionName.Parenthesis(_, syntaxToken, _)->
                    getValueSymbolByIdentifier bm collector syntaxToken boundNode func
                | _ ->
                    ()
            | _ ->
                ()
        )
        ()

    | OlySyntaxType.Literal _ ->
        () // TODO:

    | OlySyntaxType.Postfix(syntaxElementTy, syntaxIdent) ->
        if ty.TypeArguments.Length >= 1 then
            getTypeSymbol bm collector syntaxElementTy boundNode ty.TypeArguments.[0]
        getTypeSymbolByIdentifier bm collector syntaxIdent boundNode ty

    | OlySyntaxType.WildCard _
    | OlySyntaxType.Error _ -> ()

    | _ ->
        raise(InternalCompilerUnreachedException())

let private getValueSymbolByIdentifier bm (collector: ISymbolCollector) (syntaxIdent: OlySyntaxToken) (boundNode: IBoundNode) (value: IValueSymbol) =
    if collector.FilterSyntax syntaxIdent then
        match value with
        | :? FunctionGroupSymbol as funcGroup ->
            collector.CollectSymbol(OlySymbolUseInfo(OlyFunctionGroupSymbol(funcGroup), OlyBoundSubModel(bm, syntaxIdent, boundNode)))
        | _ ->
            collector.CollectSymbol(OlySymbolUseInfo(OlyValueSymbol(value), OlyBoundSubModel(bm, syntaxIdent, boundNode)))

let private nodeContains (boundNode: IBoundNode) (syntaxTarget: OlySyntaxNode) =
    let span = boundNode.Syntax.FullTextSpan
    let targetSpan = syntaxTarget.TextSpan
    span.Contains(targetSpan)

let private nodeHas (syntax: OlySyntaxNode) (syntaxTarget: OlySyntaxNode) =
    let span = syntax.TextSpan
    let targetSpan = syntaxTarget.TextSpan
    span.Contains(targetSpan)

let private nodeIntersects (syntax: OlySyntaxNode) (syntaxTarget: OlySyntaxNode) =
    let span = syntax.TextSpan
    let targetSpan = syntaxTarget.TextSpan
    span.IntersectsWith(targetSpan)

let private nodeEquals (syntax: OlySyntaxNode) (syntaxTarget: OlySyntaxNode) =
    let span = syntax.TextSpan
    let targetSpan = syntaxTarget.TextSpan
    span.Start = targetSpan.Start && span.End = targetSpan.End

[<Sealed>] 
type OlyBoundModel internal (
        asm: AssemblySymbol, 
        syntaxTree: OlySyntaxTree, 
        tryGetLocation: (OlyILAssemblyIdentity * ISymbol * CancellationToken -> OlySourceLocation option), 
        getPartialDeclTable: (CancellationToken -> BoundDeclarationTable), 
        getBoundTree: (CancellationToken -> BoundTree)) as this =

    let rec getParameterSymbols (collector: ISymbolCollector) (syntaxPars: OlySyntaxParameters) (boundNode: IBoundNode) (logicalPars: ILocalParameterSymbol romem) =
        (syntaxPars.Values.AsMemory(), logicalPars)
        ||> ROMem.tryIter2 (fun syntaxPar par ->
            getParameterSymbol collector syntaxPar boundNode par
        )

    and getParameterSymbol (collector: ISymbolCollector) (syntaxPar: OlySyntaxParameter) (boundNode: IBoundNode) (par: ILocalParameterSymbol) =
        match syntaxPar with
        | OlySyntaxParameter.Pattern(syntaxAttrs, _, OlySyntaxPattern.Name(OlySyntaxName.Identifier(syntaxIdent)), _, syntaxTy) ->
            getAttributeSymbols collector syntaxAttrs.Values boundNode par.Attributes
            if collector.FilterSyntax syntaxIdent then
                collector.CollectSymbol(OlySymbolUseInfo(OlyValueSymbol(par), OlyBoundSubModel(this, syntaxIdent, boundNode)))
            getTypeSymbol this collector syntaxTy boundNode par.Type

        | OlySyntaxParameter.Type(syntaxAttrs, syntaxTy) ->
            getAttributeSymbols collector syntaxAttrs.Values boundNode par.Attributes
            getTypeSymbol this collector syntaxTy boundNode par.Type

        | _ ->
            ()

    and getParameterSymbolsByValues (collector: ISymbolCollector) (syntaxPars: OlySyntaxParameters) (boundNode: IBoundNode) (pars: ImmutableArray<ILocalParameterSymbol>) =
        (syntaxPars.Values, pars)
        ||> Seq.iter2 (fun syntaxPar par ->
            getParameterSymbolByValue collector syntaxPar boundNode par
        )

    and getParameterSymbolByValue (collector: ISymbolCollector) (syntaxPar: OlySyntaxParameter) (boundNode: IBoundNode) (par: ILocalParameterSymbol) =
        match syntaxPar with
        | OlySyntaxParameter.Pattern(syntaxAttrs, _, OlySyntaxPattern.Name(OlySyntaxName.Identifier(syntaxIdent)), _, syntaxTy) ->
            getAttributeSymbols collector syntaxAttrs.Values boundNode par.Attributes
            if collector.FilterSyntax syntaxIdent then
                collector.CollectSymbol(OlySymbolUseInfo(OlyValueSymbol(par), OlyBoundSubModel(this, syntaxIdent, boundNode)))
            getTypeSymbol this collector syntaxTy boundNode par.Type

        | OlySyntaxParameter.Type(syntaxAttrs, syntaxTy) ->
            getAttributeSymbols collector syntaxAttrs.Values boundNode par.Attributes
            getTypeSymbol this collector syntaxTy boundNode par.Type

        | _ ->
            ()

    and getTypeParameterSymbols (collector: ISymbolCollector) (syntaxTyPars: OlySyntaxTypeParameters) (boundNode: IBoundNode) (tyPars: TypeParameterSymbol imarray) =
        let syntaxTyPars = syntaxTyPars.Values
        let skipAmount = tyPars.Length - syntaxTyPars.Length

        // Defensive.
        if skipAmount >= 0 then
            (syntaxTyPars.AsMemory(), tyPars.AsMemory().Slice(skipAmount))
            ||> ROMem.iter2 (fun syntaxTyPar tyPar -> 
                getTypeSymbol this collector syntaxTyPar boundNode tyPar.AsType
            )

    and getTypeSymbolsByTypes (collector: ISymbolCollector) (syntaxTys: OlySyntaxType seq) (boundNode: IBoundNode) (tys: ImmutableArray<TypeSymbol>) =
        (syntaxTys, tys)
        ||> Seq.iter2 (fun syntaxTy ty ->
            getTypeSymbol this collector syntaxTy boundNode ty
        )

    and getTypeSymbolByExpression bm (collector: ISymbolCollector) (syntaxExpr: OlySyntaxExpression) (boundNode: IBoundNode) (ty: TypeSymbol) : unit =
        match syntaxExpr with
        | OlySyntaxExpression.Literal(syntaxLiteral) ->
            match syntaxLiteral with
            | OlySyntaxLiteral.Default(syntaxToken)
            | OlySyntaxLiteral.UInt8(syntaxToken)
            | OlySyntaxLiteral.Int8(syntaxToken)
            | OlySyntaxLiteral.UInt16(syntaxToken)
            | OlySyntaxLiteral.Int16(syntaxToken)
            | OlySyntaxLiteral.UInt32(syntaxToken)
            | OlySyntaxLiteral.Int32(syntaxToken)
            | OlySyntaxLiteral.UInt64(syntaxToken)
            | OlySyntaxLiteral.Int64(syntaxToken)
            | OlySyntaxLiteral.Float32(syntaxToken)
            | OlySyntaxLiteral.Float64(syntaxToken)
            | OlySyntaxLiteral.Bool(syntaxToken)
            | OlySyntaxLiteral.Null(syntaxToken)
            | OlySyntaxLiteral.Integer(syntaxToken)
            | OlySyntaxLiteral.Real(syntaxToken)
            | OlySyntaxLiteral.Char16(syntaxToken)
            | OlySyntaxLiteral.Utf16(syntaxToken) ->
                if collector.FilterSyntax syntaxToken then
                    collector.CollectSymbol(OlySymbolUseInfo(OlyTypeSymbol(ty), OlyBoundSubModel(this, syntaxToken, boundNode)))
            | _ ->
                ()

        | OlySyntaxExpression.Name(syntaxName) ->
            getTypeSymbolByName bm collector syntaxName boundNode ty

        | _ ->
            ()

    and getAttributeSymbol (collector: ISymbolCollector) (syntaxAttr: OlySyntaxAttribute) (boundNode: IBoundNode) (attr: AttributeSymbol) : unit =
        match syntaxAttr with
        | OlySyntaxAttribute.Expression(syntaxExpr) ->
            match syntaxExpr, attr with
            | OlySyntaxExpression.Name(syntaxName), AttributeSymbol.Constructor(ctor, _, _, _) ->
                getSymbolsByNameAndValue collector syntaxName boundNode ctor None
            // TODO: namedArgs
            | OlySyntaxExpression.Call(syntaxReceiverExpr, syntaxArgs), AttributeSymbol.Constructor(ctor, args, namedArgs, _) ->
                match syntaxArgs with
                | OlySyntaxArguments.Arguments(_, syntaxArgList, syntaxNamedArgList, _) ->
                    // TODO: syntaxNamedArgList
                    let syntaxArgs = syntaxArgList.ChildrenOfType
                    (syntaxArgs, args)
                    ||> ImArray.tryIter2 (fun syntaxArg arg ->
                        match syntaxArg with
                        | OlySyntaxExpression.Literal(syntaxLiteral) ->
                            match syntaxLiteral with
                            | OlySyntaxLiteral.Default(syntaxToken)
                            | OlySyntaxLiteral.UInt8(syntaxToken)
                            | OlySyntaxLiteral.Int8(syntaxToken)
                            | OlySyntaxLiteral.UInt16(syntaxToken)
                            | OlySyntaxLiteral.Int16(syntaxToken)
                            | OlySyntaxLiteral.UInt32(syntaxToken)
                            | OlySyntaxLiteral.Int32(syntaxToken)
                            | OlySyntaxLiteral.UInt64(syntaxToken)
                            | OlySyntaxLiteral.Int64(syntaxToken)
                            | OlySyntaxLiteral.Float32(syntaxToken)
                            | OlySyntaxLiteral.Float64(syntaxToken)
                            | OlySyntaxLiteral.Bool(syntaxToken)
                            | OlySyntaxLiteral.Null(syntaxToken)
                            | OlySyntaxLiteral.Integer(syntaxToken)
                            | OlySyntaxLiteral.Real(syntaxToken)
                            | OlySyntaxLiteral.Char16(syntaxToken)
                            | OlySyntaxLiteral.Utf16(syntaxToken) ->
                                if collector.FilterSyntax syntaxToken then
                                    collector.CollectSymbol(OlySymbolUseInfo(OlyConstantSymbol(arg.ToLiteral()), OlyBoundSubModel(this, syntaxToken, boundNode)))
                            | _ ->
                                ()
                        | OlySyntaxExpression.Call(syntaxExpr, _) ->
                            match syntaxExpr with
                            | OlySyntaxExpression.Name(syntaxName) ->
                                match arg with
                                | ConstantSymbol.External(func) ->
                                    getSymbolsByNameAndValue collector syntaxName boundNode func None
                                | _ ->
                                    ()
                            | _ ->
                                ()
                        | _ ->
                            ()
                    )
                | _ ->
                    ()

                match syntaxReceiverExpr with
                | OlySyntaxExpression.Name(syntaxName) ->
                    getSymbolsByNameAndValue collector syntaxName boundNode ctor None
                | _ ->
                    ()
            | _ ->
                ()
        | _ ->
            ()

    and getAttributeSymbols (collector: ISymbolCollector) (syntaxAttrs: OlySyntaxAttribute imarray) (boundNode: IBoundNode) (attrs: AttributeSymbol imarray) =
        (syntaxAttrs, attrs)
        ||> ImArray.tryIter2 (fun syntaxAttr attr ->
            getAttributeSymbol collector syntaxAttr boundNode attr
        )

    and getSymbolsByConstraint (collector: ISymbolCollector) (syntaxConstr: OlySyntaxConstraint) (boundNode: IBoundNode) (constr: ConstraintSymbol) =
        match syntaxConstr with
        | OlySyntaxConstraint.Type(syntaxTy) ->
            match constr.TryGetAnySubtypeOf() with
            | ValueSome constrTy ->
                getTypeSymbol this collector syntaxTy boundNode constrTy
            | _ ->
                ()
        | OlySyntaxConstraint.ConstantType(_, syntaxTy) ->
            match constr with
            | ConstraintSymbol.ConstantType(ty) ->
                getTypeSymbol this collector syntaxTy boundNode ty.Value
            | _ ->
                ()
        | OlySyntaxConstraint.TraitType(_, syntaxTy) ->
            match constr with
            | ConstraintSymbol.TraitType(ty) ->
                getTypeSymbol this collector syntaxTy boundNode ty.Value
            | _ ->
                ()
        | _ ->
            ()

    and getSymbolsByConstraintClauseList (collector: ISymbolCollector) (syntaxConstrClauseList: OlySyntaxSeparatorList<OlySyntaxConstraintClause>) (boundNode: IBoundNode) (tyPars: TypeParameterSymbol imarray) =
        forEachConstraintBySyntaxConstraintClause syntaxConstrClauseList.ChildrenOfType tyPars (fun syntaxConstrClause tyPar constrs ->
            match syntaxConstrClause with
            | OlySyntaxConstraintClause.ConstraintClause(_, syntaxTy, _, syntaxConstrList) ->
                getTypeSymbol this collector syntaxTy boundNode tyPar.AsType
                (syntaxConstrList.ChildrenOfType, constrs)
                ||> ImArray.tryIter2 (fun syntaxConstr constr ->
                    getSymbolsByConstraint collector syntaxConstr boundNode constr
                )
            | _ ->
                ()
        )

    and getTypeSymbolByNameAndEnclosing (collector: ISymbolCollector) (syntaxName: OlySyntaxName) (boundNode: IBoundNode) (enclosing: EnclosingSymbol) =
        match enclosing with
        | EnclosingSymbol.Entity(ent) ->
            if ent.IsNamespace then
                getTypeSymbolByName this collector syntaxName boundNode ent.AsNamespaceType
            else
                getTypeSymbolByName this collector syntaxName boundNode ent.AsType
        | EnclosingSymbol.Witness(concreteTy, _) ->
            getTypeSymbolByName this collector syntaxName boundNode concreteTy
        | _ ->
            ()

    and getSymbolsByNameAndValue (collector: ISymbolCollector) (syntaxName: OlySyntaxName) (boundNode: IBoundNode) (value: IValueSymbol) (enclosingTyOpt: TypeSymbol option) : unit =
        // TODO: What is 'value.IsBridge' doing again?
        if value.IsSingleUse then ()
        else

        let syntaxNames = 
            if value.IsInstanceNotConstructor then
                ImArray.empty
            else
                syntaxName.GetTopMostName().EnclosingNames
                |> Seq.takeWhile ((<>)syntaxName)
                |> Seq.rev
                |> ImArray.ofSeq

        let mutable enclosing = 
            match enclosingTyOpt with
            | Some(TypeSymbol.Entity(ent)) when not value.IsInstanceNotConstructor && ent.IsAlias ->
                ent.AsEnclosing
            | _ ->
                if value.IsConstructor then
                    value.Enclosing.Enclosing
                else
                    value.Enclosing
        syntaxNames
        |> ImArray.iter (fun syntaxName ->
            getTypeSymbolByNameAndEnclosing collector syntaxName boundNode enclosing
            enclosing <- enclosing.Enclosing
        )

        if syntaxNames.IsEmpty && value.IsInvalid && not value.IsFunctionGroup then
            getTypeSymbolByNameAndEnclosing collector syntaxName boundNode enclosing
        else
            let syntaxName = syntaxName.LastName
            match syntaxName with
            | OlySyntaxName.Identifier(syntaxIdent) ->
                getValueSymbolByIdentifier this collector syntaxIdent boundNode value.Formal

                let tyArgs = value.AllTypeArguments
                if not tyArgs.IsEmpty then
                    match syntaxName.Parent with
                    | :? OlySyntaxName as syntaxName ->
                        match syntaxName with
                        | OlySyntaxName.Generic(_, syntaxTyArgs) ->
                            let syntaxTyArgs = syntaxTyArgs.Values
                            let skipAmount = max 0 (tyArgs.Length - syntaxTyArgs.Length)
                            getTypeArgumentSymbols collector syntaxTyArgs boundNode (tyArgs |> Seq.skip skipAmount |> ImArray.ofSeq)
                        | _ ->
                            ()
                    | _ ->
                        ()
            | _ ->
                ()

    and getTypeArgumentSymbols (collector: ISymbolCollector) (syntaxTyArgs: OlySyntaxType imarray) (boundNode: IBoundNode) (tyArgs: ImmutableArray<TypeArgumentSymbol>) =
        let tyArgs =
            // This handles variadic type arguments.
            if syntaxTyArgs.Length > tyArgs.Length && not tyArgs.IsEmpty then
                match stripTypeEquations tyArgs[tyArgs.Length - 1] with
                | TypeSymbol.Tuple(expandedTyArgs, _) ->
                    tyArgs.RemoveAt(tyArgs.Length - 1).AddRange(expandedTyArgs)
                | _ ->
                    tyArgs
            else
                tyArgs
        (syntaxTyArgs, tyArgs)
        ||> ImArray.tryIter2 (fun syntaxTyArg tyArg ->
            getTypeSymbol this collector syntaxTyArg boundNode tyArg
        )

    and getTypeSymbolFromTypeAnnotation (collector: ISymbolCollector) (syntaxReturnTyAnnot: OlySyntaxReturnTypeAnnotation) (boundNode: IBoundNode) (ty: TypeSymbol) =
        match syntaxReturnTyAnnot with
        | OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy) ->
            getTypeSymbol this collector syntaxTy boundNode ty
        | _ ->
            ()

    let collectSymbols (syntaxNode: OlySyntaxNode) (boundNode: IBoundNode) (collector: ISymbolCollector) (ct: CancellationToken) =

        let boundTree = this.GetBoundTree(ct)

        // TODO: There must be a better way to handle this.
        //       It's hacky because we assume the directives are always at the top, but it wont be the case for #if, #when, #else.
        let directiveSymbols = 
            let chooser (x: OlyToken) =
                if x.IsAnyDirective && collector.FilterSyntax x.Node then
                    match x.TryDirectiveText with
                    | ValueSome(name, value) ->
                        OlySymbolUseInfo(
                            OlyDirectiveSymbol(name, value),
                            OlyBoundSubModel(this, x.Node, boundNode)
                        )
                        |> Some
                    | _ ->
                        match x.TryConditionalDirectiveText with
                        | ValueSome(_hashIfText, _bodyText, _hashEndText) ->
                            OlySymbolUseInfo(
                                OlyConditionalDirectiveSymbol(),
                                OlyBoundSubModel(this, x.Node, boundNode)
                            )
                            |> Some
                        | _ ->
                            None
                else
                    None
            match boundTree.SyntaxTree.GetRoot(ct).TryGetFirstToken(true) with
            | Some(firstNonTriviaToken) when not firstNonTriviaToken.IsEndOfSource ->                
                if collector.CompareSyntax(syntaxNode, firstNonTriviaToken.Node) || (syntaxNode.TextSpan.End < firstNonTriviaToken.TextSpan.Start) then
                    firstNonTriviaToken.GetLeadingTrivia()
                    |> ImArray.choose chooser
                else
                    ImArray.empty
            | _ ->
                boundTree.SyntaxTree.GetRoot(ct).FindTokens(syntaxNode.TextSpan.Start, skipTrivia = false, ct = ct)
                |> ImArray.choose chooser
        directiveSymbols
        |> ImArray.iter collector.CollectSymbol

        let openDecls = boundTree.RootEnvironment.openDecls

        (openDecls, boundTree.SyntaxTree.GetOpenDeclarationNames(ct))
        ||> ImArray.tryIter2 (fun ent syntaxName ->
            ct.ThrowIfCancellationRequested()
            if collector.CompareSyntax(syntaxNode, syntaxName) then
                if ent.IsNamespace then
                    getTypeSymbolByName this collector syntaxName boundNode ent.AsNamespaceType
                else
                    getTypeSymbolByName this collector syntaxName boundNode ent.AsType
        )

        boundTree.ForEachForTooling((fun boundNode -> ct.ThrowIfCancellationRequested(); collector.CompareSyntax(syntaxNode, boundNode.Syntax)), 
            fun boundNode ->
                ct.ThrowIfCancellationRequested()
                this.GetImmediateSymbols(boundNode, collector)
        )

    let getLocalBindingSymbols collector (syntax: OlySyntaxNode) (boundNode: IBoundNode) (bindingInfo: LocalBindingInfoSymbol) =
        match bindingInfo with
        | BindingLocalFunction(func) ->
            match syntax with
            | :? OlySyntaxBindingDeclaration as syntax ->
                match syntax with
                | OlySyntaxBindingDeclaration.Function(syntaxFuncName, syntaxTyPars, syntaxPars, syntaxReturnTyAnnot, syntaxConstrClauseList) ->
                    getValueSymbolByIdentifier this collector syntaxFuncName.Identifier boundNode func
                    getTypeParameterSymbols collector syntaxTyPars boundNode func.TypeParameters
                    getParameterSymbols collector syntaxPars boundNode func.LogicalParameters
                    getTypeSymbolFromTypeAnnotation collector syntaxReturnTyAnnot boundNode func.ReturnType
                    getSymbolsByConstraintClauseList collector syntaxConstrClauseList boundNode func.TypeParameters

                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this collector syntaxIdent boundNode func
                    getTypeSymbolFromTypeAnnotation collector syntaxTyAnnot boundNode func.Type

                | _ ->
                    ()
            | _ ->
                ()

        | BindingLocal(value) ->
            match syntax with
            | :? OlySyntaxBindingDeclaration as syntax ->
                match syntax with
                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this collector syntaxIdent boundNode value
                    getTypeSymbolFromTypeAnnotation collector syntaxTyAnnot boundNode value.Type                
                | _ ->
                    ()
            | _ ->
                ()

    let rec getBindingSymbols collector (syntaxNode: OlySyntaxNode) (boundNode: IBoundNode) (bindingInfo: BindingInfoSymbol) =

        match bindingInfo with
        | BindingProperty(_, prop) ->
            match syntaxNode.TryGetBindingDeclaration() with
            | ValueSome(syntax) ->
                match syntax with
                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this collector syntaxIdent boundNode prop
                    getTypeSymbolFromTypeAnnotation collector syntaxTyAnnot boundNode prop.Type
                | _ ->
                    ()
            | _ ->
                ()

        | BindingFunction(func)
        | BindingPattern(_, func) ->

            match syntaxNode with
            | :? OlySyntaxPropertyBinding as syntaxPropBinding ->
                match syntaxPropBinding with
                | OlySyntaxPropertyBinding.Binding(syntaxAttrs, _, _, _, _, _) ->
                    getAttributeSymbols collector syntaxAttrs.Values boundNode func.Attributes
                | _ ->
                    unreached()
            | _ ->
                ()

            match syntaxNode.TryGetBindingDeclaration() with
            | ValueSome(syntax) ->
                match syntax with
                | OlySyntaxBindingDeclaration.Function(syntaxFuncName, syntaxTyPars, syntaxPars, syntaxReturnTyAnnot, syntaxConstrClauseList) ->
                    getValueSymbolByIdentifier this collector syntaxFuncName.Identifier boundNode func
                    getTypeParameterSymbols collector syntaxTyPars boundNode func.TypeParameters
                    getParameterSymbols collector syntaxPars boundNode func.LogicalParameters
                    getTypeSymbolFromTypeAnnotation collector syntaxReturnTyAnnot boundNode func.ReturnType
                    getSymbolsByConstraintClauseList collector syntaxConstrClauseList boundNode func.TypeParameters

                | OlySyntaxBindingDeclaration.New(syntaxNewToken, syntaxPars) ->
                    getValueSymbolByIdentifier this collector syntaxNewToken boundNode func
                    getParameterSymbols collector syntaxPars boundNode func.LogicalParameters

                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this collector syntaxIdent boundNode func
                    getTypeSymbolFromTypeAnnotation collector syntaxTyAnnot boundNode func.Type

                | OlySyntaxBindingDeclaration.Get(syntaxGetToken) ->
                    getValueSymbolByIdentifier this collector syntaxGetToken boundNode func

                | OlySyntaxBindingDeclaration.Set(syntaxSetToken) ->
                    getValueSymbolByIdentifier this collector syntaxSetToken boundNode func

                | OlySyntaxBindingDeclaration.Getter(syntaxGetToken, syntaxPars) ->
                    getValueSymbolByIdentifier this collector syntaxGetToken boundNode func
                    getParameterSymbols collector syntaxPars boundNode func.LogicalParameters

                | OlySyntaxBindingDeclaration.Setter(syntaxSetToken, syntaxPars) ->
                    getValueSymbolByIdentifier this collector syntaxSetToken boundNode func
                    getParameterSymbols collector syntaxPars boundNode func.LogicalParameters

                | _ ->
                    ()

            | _ ->
                ()

        | BindingField(field) ->
            match syntaxNode with
            | :? OlySyntaxExpression as syntaxExpr ->
                match syntaxExpr with
                | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, _, _, _, _) ->
                    getAttributeSymbols collector syntaxAttrs.Values boundNode (field: IFieldSymbol).Attributes
                | _ ->
                    ()
            | _ ->
                ()

            match syntaxNode.TryGetBindingDeclaration() with
            | ValueSome(syntax) ->
                match syntax with
                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this collector syntaxIdent boundNode field
                    getTypeSymbolFromTypeAnnotation collector syntaxTyAnnot boundNode field.Type                
                | _ ->
                    ()
            | _ ->
                ()

    let getSymbolsByLiteral (collector: ISymbolCollector) (syntax: OlySyntaxNode) (boundNode: IBoundNode) (literal: BoundLiteral) =
        if collector.FilterSyntax syntax then
            collector.CollectSymbol(OlySymbolUseInfo(OlyConstantSymbol(literal), OlyBoundSubModel(this, syntax, boundNode)))

    member internal _.GetImmediateSymbols(boundNode: IBoundNode, collector) =
        match boundNode with
        | :? BoundRoot as root ->
            match root with
            | BoundRoot.Namespace(syntax, benv, namespac, _) ->
                match syntax with
                | OlySyntaxCompilationUnit.Namespace(_, syntaxName, _, _) ->
                    getTypeSymbolByName this collector syntaxName boundNode namespac.AsNamespaceType
                | _ ->
                    ()

            | _ ->
                ()

        | :? BoundExpression as boundNode ->
            match boundNode with
            | BoundExpression.Call(syntaxInfo, _, _witnessArgs (* will need this for explicit witnesses *), _, value, _) ->
                match syntaxInfo.TrySyntaxNameAndEnvironment with
                | Some(syntaxName, benv) ->
                    getSymbolsByNameAndValue collector syntaxName boundNode value syntaxInfo.TryType
                | _ ->
                    ()

            | BoundExpression.Value(syntaxInfo, value) ->
                match syntaxInfo.TrySyntaxNameAndEnvironment with
                | Some(syntaxName, benv) ->
                    getSymbolsByNameAndValue collector syntaxName.LastName boundNode value syntaxInfo.TryType
                | _ ->
                    ()

            | BoundExpression.Literal(syntaxInfo, literal) ->
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    getSymbolsByLiteral collector syntaxInfo.Syntax boundNode literal
                | _ ->
                    ()

            | BoundExpression.GetField(syntaxInfo, _, field)
            | BoundExpression.SetField(syntaxInfo, _, field, _, _) ->
                match syntaxInfo.TrySyntaxNameAndEnvironment with
                | Some(syntaxName, benv) ->
                    getValueSymbolByIdentifier this collector syntaxName.LastIdentifier boundNode field
                | _ ->
                    ()

            | BoundExpression.SetValue(syntaxInfo, value, _) ->
                match syntaxInfo.TrySyntaxNameAndEnvironment with
                | Some(syntaxName, benv) ->
                    getSymbolsByNameAndValue collector syntaxName boundNode value syntaxInfo.TryType
                | _ ->
                    ()

            | BoundExpression.GetProperty(syntaxInfo, _, prop, _) 
            | BoundExpression.SetProperty(syntaxInfo, _, prop, _, _) ->
                match syntaxInfo.TrySyntaxNameAndEnvironment with
                | Some(syntaxName, benv) ->
                    getSymbolsByNameAndValue collector syntaxName boundNode prop syntaxInfo.TryType
                | _ ->
                    ()

            | BoundExpression.Let(syntaxInfo, bindingInfo, _, _) when not syntaxInfo.IsGenerated ->
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    let syntax =
                        match syntaxInfo.Syntax.TryGetBindingDeclaration() with
                        | ValueSome syntax -> syntax :> OlySyntaxNode
                        | _ -> syntaxInfo.Syntax
                    getLocalBindingSymbols collector syntax boundNode bindingInfo
                | _ ->
                    ()

            | BoundExpression.MemberDefinition(syntaxInfo, binding) when not syntaxInfo.IsGenerated ->
                let attrs =
                    match binding.Info.Value with
                    | :? IFunctionSymbol as func -> func.Attributes
                    | :? IFieldSymbol as field -> field.Attributes
                    | :? IPropertySymbol as prop -> prop.Attributes
                    | _ -> ImArray.empty

                if not attrs.IsEmpty then
                    match syntaxInfo.TryEnvironment with
                    | Some benv ->
                        match syntaxInfo.Syntax with
                        | :? OlySyntaxExpression as syntaxExpr ->
                            match syntaxExpr with
                            | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, _, _, _, syntaxBinding) ->
                                getAttributeSymbols collector syntaxAttrs.Values boundNode attrs
                            | _ ->
                                ()
                        | _ ->
                            ()
                    | _ ->
                        ()

                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    getBindingSymbols collector syntaxInfo.Syntax boundNode binding.Info
                | _ ->
                    ()

            | BoundExpression.EntityDefinition(syntaxInfo, _, ent) ->
                match syntaxInfo.TryEnvironment, syntaxInfo.Syntax with
                | Some benv, (:? OlySyntaxCompilationUnit as syntax) ->
                    match syntax with
                    | OlySyntaxCompilationUnit.Module(syntaxAttrs, syntaxAccessor, _, syntaxName, syntaxConstrClauseList, _, _) ->
                        let syntaxAttrs = syntaxAttrs.Values
                        getAttributeSymbols collector syntaxAttrs boundNode ent.Attributes
                        getTypeSymbolByName this collector syntaxName boundNode ent.AsType
                        getSymbolsByConstraintClauseList collector syntaxConstrClauseList boundNode ent.TypeParameters
                    | _ ->
                        ()

                | Some benv, (:? OlySyntaxExpression as syntax) ->
                    match syntax with
                    | OlySyntaxExpression.TypeDeclaration(syntaxAttrs, _, _, syntaxTyDefName, syntaxTyPars, syntaxConstrClauseList, _, syntaxBodyExpr) ->
                        let syntaxAttrs = syntaxAttrs.Values
                        getAttributeSymbols collector syntaxAttrs boundNode ent.Attributes
                        getTypeSymbolByIdentifier this collector syntaxTyDefName.Identifier boundNode (TypeSymbol.Entity(ent))
                        getTypeParameterSymbols collector syntaxTyPars boundNode ent.LogicalTypeParameters
                        getSymbolsByConstraintClauseList collector syntaxConstrClauseList boundNode ent.TypeParameters

                        match syntaxBodyExpr with
                        | OlySyntaxTypeDeclarationBody.Body(syntaxExtends, syntaxImplements, syntaxCaseList, _) ->  
                            if ent.IsEnum then
                                let syntaxTyDeclCases = syntaxCaseList.ChildrenOfType
                                let ty = ent.AsType
                                let mutable i = 0
                                ent.Fields
                                |> ImArray.iter (fun x -> 
                                    if x.IsFieldConstant && areTypesEqual x.Type ty then
                                        if i < syntaxTyDeclCases.Length then
                                            match syntaxTyDeclCases[i] with
                                            | OlySyntaxTypeDeclarationCase.Case(_, syntaxIdent)
                                            | OlySyntaxTypeDeclarationCase.EnumCase(_, syntaxIdent, _, _) ->
                                                getValueSymbolByIdentifier this collector syntaxIdent boundNode x
                                            | _ ->
                                                ()
                                            i <- i + 1
                                )
                            let syntaxExtends =
                                match syntaxExtends with
                                | OlySyntaxExtends.Inherits(_, syntaxTys) ->
                                    syntaxTys.ChildrenOfType
                                | OlySyntaxExtends.Type(syntaxTy) ->
                                    ImArray.createOne syntaxTy
                                | _ ->
                                    ImArray.empty

                            let syntaxImplements =
                                match syntaxImplements with
                                | OlySyntaxImplements.Implements(_, syntaxTys) ->
                                    syntaxTys.ChildrenOfType
                                | _ ->
                                    ImArray.empty

                            getTypeSymbolsByTypes collector syntaxExtends boundNode ent.Extends
                            getTypeSymbolsByTypes collector syntaxImplements boundNode ent.Implements
                        | _ ->
                            ()
                    | _ ->
                        ()
                | _ ->
                    ()

            | BoundExpression.Typed(syntaxInfo, _, ty) ->
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    match syntaxInfo.Syntax with
                    | :? OlySyntaxExpression as syntax ->
                        match syntax with
                        | OlySyntaxExpression.Typed(_, _, syntaxTy) ->
                            getTypeSymbol this collector syntaxTy boundNode ty
                        | _ ->
                            ()
                    | _ ->
                        ()
                | _ ->
                    ()

            | BoundExpression.Lambda(syntaxInfo, _, _, parValues, _, _, _, _) ->
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    match syntaxInfo.Syntax with
                    | :? OlySyntaxExpression as syntax ->
                        match syntax with
                        | OlySyntaxExpression.Lambda(_, syntaxPars, _, _) ->
                            getParameterSymbolsByValues collector syntaxPars boundNode parValues
                        | _ ->
                            ()
                    | _ ->
                        ()
                | _ ->
                    ()

            | BoundExpression.Try(catchCases=catchCases) ->
                // TODO: We iterate over catch cases already, but we do it again here.
                //       This may have perf issues, but likely not as there tends not to be very many
                //       catch cases.
                catchCases
                |> ImArray.iter (fun catchCase ->
                    match catchCase with
                    | BoundCatchCase.CatchCase(syntaxInfo, par, _) ->
                        match syntaxInfo.TryEnvironment with
                        | Some benv ->
                            match syntaxInfo.Syntax with
                            | :? OlySyntaxCatchOrFinallyExpression as syntax ->
                                match syntax with
                                | OlySyntaxCatchOrFinallyExpression.Catch(_, _, syntaxPar, _, _, _, _) ->
                                    getParameterSymbol collector syntaxPar boundNode par
                                | _ ->
                                    ()
                            | _ ->
                                ()
                        | _ ->
                            ()
                )

            | BoundExpression.Witness(syntaxInfo, _, castFunc, _, _, _) ->
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    match syntaxInfo.Syntax with
                    | :? OlySyntaxExpression as syntax ->
                        match syntax with
                        | OlySyntaxExpression.Call(syntaxReceiverExpr, _) ->
                            match syntaxReceiverExpr with
                            | OlySyntaxExpression.Name(syntaxName) ->
                                getSymbolsByNameAndValue collector syntaxName boundNode castFunc None
                            | _ ->
                                ()
                        | _ ->
                            ()
                    | _ ->
                        ()
                | _ ->
                    ()
                    
            | BoundExpression.ErrorWithNamespace(syntaxName, benv, namespaceEnt) ->
                let ent = 
                    if namespaceEnt.IsNamespace then
                        namespaceEnt.AsNamespaceType
                    else
                        namespaceEnt.AsType
                getTypeSymbolByName this collector syntaxName boundNode ent

            | BoundExpression.ErrorWithType(syntaxName, benv, ty) ->
                getTypeSymbolByName this collector syntaxName boundNode ty

            | _ ->
                ()
                    
        | :? BoundCasePattern as pattern ->
            match pattern with
            | BoundCasePattern.FieldConstant(syntaxInfo, field) ->
                match syntaxInfo.TrySyntaxNameAndEnvironment with
                | Some(syntaxName, benv) ->
                    getSymbolsByNameAndValue collector syntaxName boundNode field syntaxInfo.TryType
                | _ ->
                    ()

            | BoundCasePattern.Literal(syntaxInfo, literal) ->
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntaxNode, benv) ->
                    getSymbolsByLiteral collector syntaxNode boundNode literal
                | _ ->
                    ()
                
            | BoundCasePattern.Local(syntaxInfo, value) ->
                match syntaxInfo.TrySyntaxNameAndEnvironment with
                | Some(syntaxName, benv) ->
                    getSymbolsByNameAndValue collector syntaxName boundNode value syntaxInfo.TryType
                | _ ->
                    ()

            | BoundCasePattern.Function(syntaxInfo, pat, _, _) ->
                match syntaxInfo.TrySyntaxNameAndEnvironment with
                | Some(syntaxName, benv) ->
                    getSymbolsByNameAndValue collector syntaxName boundNode pat.PatternFunction syntaxInfo.TryType
                | _ ->
                    ()

            | _ ->
                ()


        | _ ->
            ()

    member this.TryGetSubModel(syntaxToken: OlyToken, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let data = ResizeArray<IBoundNode>()
        
        let syntaxNode = syntaxToken.Node

        let boundTree = this.GetBoundTree(ct)

        boundTree.ForEachForTooling((fun boundNode -> ct.ThrowIfCancellationRequested(); nodeContains boundNode syntaxNode),
            (fun boundNode ->
                ct.ThrowIfCancellationRequested()
                match boundNode with
                | :? BoundExpression as expr ->
                    match expr with
                    | BoundExpression.Sequential _ -> ()
                    | _ ->
                        data.Add(boundNode)
                | _ ->
                    ()
            )
        )

        data
        |> Seq.sortBy (fun boundNode ->
            boundNode.Syntax.TextSpan.Width
        )
        |> Seq.filter (fun boundNode -> boundNode.TryGetEnvironment().IsSome)
        |> Seq.tryHead
        |> Option.map (fun boundNode -> OlyBoundSubModel(this, boundNode.Syntax, boundNode))

    member this.TryGetWhitespaceSubModel(offside: int, syntaxToken: OlyToken, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        if not syntaxToken.IsWhitespaceTrivia then None
        else

        match syntaxToken.TryPreviousToken((fun _ -> true), ct = ct) with
        | ValueNone -> None
        | ValueSome syntaxToken ->

        let data = ResizeArray<IBoundNode>()
        
        let syntaxNode = syntaxToken.Node

        let boundTree = this.GetBoundTree(ct)

        boundTree.ForEachForTooling((fun boundNode -> ct.ThrowIfCancellationRequested(); nodeContains boundNode syntaxNode),
            (fun boundNode ->
                ct.ThrowIfCancellationRequested()
                match boundNode with
                | :? BoundExpression as expr ->
                    match expr with
                    | BoundExpression.Let(syntaxInfo, _, _, ((BoundExpression.None _) as rhsExpr)) when not syntaxInfo.IsGenerated ->
                        let column = expr.Syntax.GetTextRange(ct).Start.Column
                        if offside >= column then
                            data.Add(rhsExpr)
                    | _ ->
                        ()
                | _ ->
                    ()
            )
        )

        data
        |> Seq.sortBy (fun boundNode ->
            boundNode.Syntax.TextSpan.Width
        )
        |> Seq.filter (fun boundNode -> boundNode.TryGetEnvironment().IsSome)
        |> Seq.tryHead
        |> Option.map (fun boundNode -> OlyBoundSubModel(this, boundNode.Syntax, boundNode))

    member internal this.GetBoundTree(ct: CancellationToken): BoundTree =
        getBoundTree ct

    member this.GetSymbols(node: OlySyntaxNode, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let symbols = ImmutableArray.CreateBuilder<OlySymbolUseInfo>()

        let collector =
            { new ISymbolCollector with
                member _.CompareSyntax(syntaxNode1, syntaxNode2) = nodeIntersects syntaxNode1 syntaxNode2
                member _.FilterSyntax(syntaxNode) = nodeHas node syntaxNode
                member _.CollectSymbol(symbolUseInfo) = symbols.Add(symbolUseInfo)
            }

        collectSymbols node Unchecked.defaultof<_> collector ct
        symbols.ToImmutable()

    member this.ForEachSymbol(node: OlySyntaxNode, action, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let collector =
            { new ISymbolCollector with
                member _.CompareSyntax(syntaxNode1, syntaxNode2) = nodeIntersects syntaxNode1 syntaxNode2
                member _.FilterSyntax(syntaxNode) = nodeHas node syntaxNode
                member _.CollectSymbol(symbolUseInfo) = action symbolUseInfo
            }

        collectSymbols node Unchecked.defaultof<_> collector ct

    member this.TryGetAnonymousModuleSymbol(ct: CancellationToken) : OlySymbolUseInfo<OlyTypeSymbol> option =
        ct.ThrowIfCancellationRequested()
        let syntax = this.SyntaxTree.GetRoot(ct) :?> OlySyntaxCompilationUnit
        match syntax with
        | OlySyntaxCompilationUnit.AnonymousModule _ ->
            let boundTree = this.GetBoundTree(ct)
            let rootSymbol = boundTree.RootSymbol
            OlySymbolUseInfo<OlyTypeSymbol>(OlyTypeSymbol(rootSymbol.AsType), OlyBoundSubModel(this, syntax, Unchecked.defaultof<_>))
            |> Some
        | _ ->
            None

    member this.GetSymbolsByPossibleName(node: OlySyntaxNode, possibleName: string, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let symbols = ImmutableArray.CreateBuilder<OlySymbolUseInfo>()

        let collector =
            { new ISymbolCollector with
                member _.CompareSyntax(syntaxNode1, syntaxNode2) = nodeIntersects syntaxNode1 syntaxNode2
                member _.FilterSyntax(syntaxNode) = nodeHas node syntaxNode
                member _.CollectSymbol(symbolUseInfo) = 
                    // REVIEW: There a way to do this without having to allocate a public symbol?
                    //         We could simply add a new interface method to check the internal symbol's name.
                    if symbolUseInfo.Symbol.Name.Contains(possibleName) then
                        symbols.Add(symbolUseInfo)
            }

        collectSymbols node Unchecked.defaultof<_> collector ct
        symbols.ToImmutableArray()

    member this.TryFindSymbol(token: OlyToken, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let symbols = ImmutableArray.CreateBuilder<OlySymbolUseInfo>()

        let collector =
            { new ISymbolCollector with
                member _.CompareSyntax(syntaxNode1, syntaxNode2) = nodeHas syntaxNode2 syntaxNode1
                member _.FilterSyntax(syntaxNode) = nodeEquals token.Node syntaxNode
                member _.CollectSymbol(symbolUseInfo) = symbols.Add(symbolUseInfo)
            }

        collectSymbols token.Node Unchecked.defaultof<_> collector ct

        symbols
        |> Seq.tryExactlyOne

    /// Gets the semantic diagnostics.
    member this.GetDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        this.GetBoundTree(ct).Diagnostics

    member this.HasErrors(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        this.GetDiagnostics(ct)
        |> ImArray.exists (fun x -> x.IsError)

    member _.SyntaxTree: OlySyntaxTree = syntaxTree

    member _.AssemblyIdentity = asm.Identity

    member internal _.TryFindExternalDefinition(identity: OlyILAssemblyIdentity, s: ISymbol, ct) : OlySourceLocation option = tryGetLocation(identity, s, ct)

    member internal _.TryFindDefinition(value: IValueSymbol, ct) =
        let declTable = getPartialDeclTable ct
        match declTable.ValueDeclarations.TryGetValue value with
        | true, location ->
            location
            |> Some
        | _ ->
            let boundTree = this.GetBoundTree(ct)
            let declTable = boundTree.DeclarationTable
            match declTable.ValueDeclarations.TryGetValue value with
            | true, location ->
                location
                |> Some
            | _ ->
                // If we cannot find the constructor, go to the type definition.
                // This can happen for implicit default ctors.
                if value.IsInstanceConstructor then
                    match value.Enclosing with
                    | EnclosingSymbol.Entity(ent) ->
                        this.TryFindDefinition(ent, ct)
                    | _ ->
                        None
                else
                    None

    member internal _.TryFindDefinition(ent: EntitySymbol, ct) : OlySourceLocation option =
        let declTable = getPartialDeclTable ct
        match declTable.EntityDeclarations.TryGetValue ent with
        | true, location ->
            location
            |> Some
        | _ ->
            let boundTree = this.GetBoundTree(ct)
            let declTable = boundTree.DeclarationTable
            match declTable.EntityDeclarations.TryGetValue ent with
            | true, location ->
                location
                |> Some
            | _ ->
                None

    member internal _.TryFindDefinition(tyPar: TypeParameterSymbol, ct) : OlySourceLocation option =
        let declTable = getPartialDeclTable ct
        match declTable.TypeParameterDeclarations.TryGetValue tyPar with
        | true, location ->
            location
            |> Some
        | _ ->
            let boundTree = this.GetBoundTree(ct)
            let declTable = boundTree.DeclarationTable
            match declTable.TypeParameterDeclarations.TryGetValue tyPar with
            | true, location ->
                location
                |> Some
            | _ ->
                None

    member internal this.TryFindDefinition(s: ISymbol, ct) : OlySourceLocation option =
        match s with
        | :? EntitySymbol as ent ->
            match ent.Formal with
            | :? Internal.CompilerImports.RetargetedEntitySymbol as ent ->
                this.TryFindDefinition(ent.Original, ct)
            | ent ->
                this.TryFindDefinition(ent, ct)
        | :? TypeParameterSymbol as tyPar ->
            this.TryFindDefinition(tyPar, ct)
        | :? IValueSymbol as value ->
            match value.Formal with
            | :? Internal.CompilerImports.RetargetedFunctionSymbol as func ->
                this.TryFindDefinition(func.Original, ct)
            | :? Internal.CompilerImports.RetargetedPropertySymbol as prop ->
                this.TryFindDefinition(prop.Original, ct)
            | :? Internal.CompilerImports.RetargetedFieldSymbol as field ->
                this.TryFindDefinition(field.Original, ct)
            | :? Internal.CompilerImports.RetargetedPatternSymbol as pat ->
                this.TryFindDefinition(pat.Original, ct)
            | value ->
                this.TryFindDefinition(value, ct)
        | _ ->
            None
