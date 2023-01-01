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

[<NoEquality;NoComparison>]
type internal FullyBoundResult =
    {
        BoundTree: BoundTree
        SemanticDiagnostics: OlyDiagnostic imarray
    }

[<AbstractClass>]
type OlySymbol internal (syntax: OlySyntaxNode) =

    abstract SignatureText : string

    abstract Name : string

    abstract TryGetDefinitionLocation: ct: CancellationToken -> OlySourceLocation option

    abstract IsSimilarTo: OlySymbol -> bool

    abstract IsLocal: bool

    member _.UseSyntax = syntax

    member this.UseSyntaxTree = this.UseSyntax.Tree

    member this.IsDefinition(ct: CancellationToken) =
        match this.TryGetDefinitionLocation(ct) with
        | Some location ->
            location.TextSpan.IsEqualTo(syntax.TextSpan)
        | _ ->
            false

    member this.AsValue =
        this :?> OlyValueSymbol

    member this.AsConstant =
        this :?> OlyConstantSymbol

[<Sealed>][<DebuggerDisplay("{SignatureText}")>] 
type OlyNamespaceSymbol internal (boundModel, benv, location, ent: IEntitySymbol) =
    inherit OlySymbol(location)

    let mutable tys = ValueNone
    let mutable namespaces = ValueNone

    member internal _.Internal = ent

    override _.SignatureText =
        let names = ResizeArray()
        let rec loop (enclosing: EnclosingSymbol) =
            match enclosing with
            | EnclosingSymbol.Entity(ent) ->
                names.Add(ent.Name)
                loop enclosing.Enclosing
            | _ ->
                ()
        loop ent.AsEnclosing
        names |> Seq.rev |> String.concat "."

    override _.Name = ent.Name

    override _.TryGetDefinitionLocation(ct) = 
        ct.ThrowIfCancellationRequested()
        None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyNamespaceSymbol as symbol ->
            areEntitiesEqual this.Internal.Formal symbol.Internal.Formal
        | _ ->
            false

    override this.IsLocal = false

    member _.Types =
        match tys with
        | ValueNone ->
            tys <-
                ent.Entities
                |> ImArray.filter (fun x -> not x.IsNamespace)
                |> ImArray.map (fun x -> OlyTypeSymbol(boundModel, benv, location, x.AsType))
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
                |> ImArray.map (fun x -> OlyNamespaceSymbol(boundModel, benv, location, x))
                |> ValueSome
        | _ ->
            ()
        namespaces.Value

[<Sealed>][<DebuggerDisplay("{SignatureText}")>] 
type OlyTypeSymbol internal (boundModel: OlyBoundModel, benv: BoundEnvironment, location: OlySyntaxNode, ty: TypeSymbol) =
    inherit OlySymbol(location)

    let stripTypeEquations ty =
        match stripTypeEquations ty with
        | TypeSymbol.ByRef(ty, _) ->
            stripTypeEquations ty
        | ty ->
            ty

    member internal _.Internal = ty

    member _.IsTypeParameter =
        match stripTypeEquations ty with
        | TypeSymbol.Variable _ 
        | TypeSymbol.HigherVariable _ -> true
        | _ -> false

    override _.Name = ty.Name

    override _.SignatureText = printTypeDefinition benv ty

    override _.TryGetDefinitionLocation(ct) =
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
                areEntitiesEqual ent1.Formal ent2.Formal
            | _ ->
                areTypesEqual this.Internal symbol.Internal
        | :? OlyValueSymbol as symbol when symbol.IsConstructor ->
            match symbol.Enclosing.TryType with
            | Some tySymbol ->
                this.IsSimilarTo(tySymbol)
            | _ ->
                false
        | _ ->
            false

    override this.IsLocal =
        match ty with
        | TypeSymbol.Entity(ent) -> ent.IsLocal
        | _ -> true

    member _.Fields =
        match stripTypeEquations ty with
        | TypeSymbol.Entity(ent) -> 
            ent.FindIntrinsicFields(benv, QueryMemberFlags.StaticOrInstance) 
            |> Seq.map (fun x -> 
                OlyValueSymbol(boundModel, benv, location, x)
            )
        | _ -> Seq.empty

    member _.Functions =
        let ty = stripTypeEquations ty
        ty.FindFunctions(benv, QueryMemberFlags.StaticOrInstance, FunctionFlags.None, QueryFunction.IntrinsicAndExtrinsic)
        |> ImArray.map (fun x ->
            OlyValueSymbol(boundModel, benv, location, x)
        )

    member _.Properties =
        let ty = stripTypeEquations ty
        ty.FindProperties(benv, QueryMemberFlags.StaticOrInstance, QueryField.IntrinsicAndExtrinsic)
        |> Seq.map (fun x ->
            OlyValueSymbol(boundModel, benv, location, x)
        )

    member _.Types =
        match stripTypeEquations ty with
        | TypeSymbol.Entity(ent) ->
            ent.FindNestedEntities(benv)
            |> ImArray.map (fun x -> OlyTypeSymbol(boundModel, benv, location, x.AsType))
        | _ ->
            ImArray.empty

    member _.IsInterface = ty.IsInterface

    member _.IsTypeExtension = ty.IsTypeExtension

    member _.IsEnum = ty.IsEnum

    member _.IsClass = ty.IsClass

    member _.IsShape = ty.IsShape

    member _.IsStruct = ty.IsAnyStruct

    member _.IsModule = ty.IsModule

    member _.IsAlias =
        match ty with
        | TypeSymbol.Entity(ent) -> ent.IsAlias
        | _ -> false

    member _.Enclosing: OlyEnclosingSymbol = OlyEnclosingSymbol(boundModel, benv, location, ty.Enclosing)

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
type OlyConstantSymbol internal (boundModel, benv, location, internalLiteral: BoundLiteral) =
    inherit OlySymbol(location)

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
                OlyTypeSymbol(boundModel, benv, location, elementTy), 
                (
                    values
                    |> ImArray.map (fun x ->
                        constantToConstant x
                    )
                )
            )
        | ConstantSymbol.TypeVariable(tyPar) ->
            OlyConstant.Variable(OlyTypeSymbol(boundModel, benv, location, tyPar.AsType))
        | ConstantSymbol.External(func) ->
            OlyConstant.External(
                OlyValueSymbol(boundModel, benv, location, func)
            )
        | ConstantSymbol.Error -> OlyConstant.Error

    member internal _.Internal = internalLiteral

    member _.Value =
        let rec f internalLiteral =
            match internalLiteral with
            | BoundLiteral.Constant(cns) -> constantToConstant cns
            | BoundLiteral.NullInference _ -> OlyConstant.Null
            | BoundLiteral.NumberInference(lazyInternalLiteral, _) when lazyInternalLiteral.IsValueCreated -> OlyConstantSymbol(boundModel, benv, location, lazyInternalLiteral.Value).Value
            | BoundLiteral.DefaultInference _ -> OlyConstant.Default
            | BoundLiteral.ConstantEnum(constant, _) ->
                constantToConstant constant
            | _ -> OlyConstant.Error
        f internalLiteral

    member this.Type =
        OlyTypeSymbol(boundModel, benv, location, internalLiteral.Type)

    override _.Name = ""

    override _.SignatureText = ""

    override _.TryGetDefinitionLocation(ct) = 
        ct.ThrowIfCancellationRequested()
        None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyConstantSymbol as symbol ->
            areLiteralsEqual this.Internal symbol.Internal
        | _ ->
            false

    override this.IsLocal = false

[<Sealed>][<DebuggerDisplay("{SignatureText}")>] 
type OlyFunctionGroupSymbol internal (boundModel: OlyBoundModel, benv: BoundEnvironment, location, funcGroup: FunctionGroupSymbol) =
    inherit OlyValueSymbol(boundModel, benv, location, funcGroup)

    member internal _.Internal: FunctionGroupSymbol = funcGroup

    override _.SignatureText = funcGroup.Name

    override _.TryGetDefinitionLocation(ct) = 
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

    override this.IsLocal =
        funcGroup.IsLocal

    member _.Functions =
        funcGroup.Functions
        |> Seq.map (fun x -> OlyValueSymbol(boundModel, benv, location, x))
        |> ImmutableArray.CreateRange

    // TODO: Consider creating an OlyFunctionSymbol
[<DebuggerDisplay("{SignatureText}")>] 
type OlyValueSymbol internal (boundModel: OlyBoundModel, benv: BoundEnvironment, location, value: IValueSymbol) =
    inherit OlySymbol(location)

    member internal _.Internal: IValueSymbol = value

    override _.Name = value.Name

    override _.SignatureText = printValue benv value

    override _.TryGetDefinitionLocation(ct) =
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
            match this.IsLocal, symbol.IsLocal with
            | true, true ->
                this.Internal.Formal.Id = symbol.Internal.Formal.Id
            | false, false ->
                if areValueSignaturesEqual this.Internal.Formal symbol.Internal.Formal then
                    true
                else
                    if this.IsProperty && symbol.IsField then
                        match symbol.Internal with
                        | :? IFieldSymbol as field ->
                            match field.AssociatedFormalPropertyId with
                            | Some(associatedFormalPropId) ->
                                associatedFormalPropId = this.Internal.Formal.Id
                            | _ ->
                                false
                        | _ ->
                            false
                    elif this.IsField && symbol.IsProperty then
                        match this.Internal with
                        | :? IFieldSymbol as field ->
                            match field.AssociatedFormalPropertyId with
                            | Some(associatedFormalPropId) ->
                                associatedFormalPropId = symbol.Internal.Formal.Id
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

    override _.IsLocal = value.IsLocal

    member _.ReturnType = 
        match value with
        | :? IFunctionSymbol as func ->
            OlyTypeSymbol(boundModel, benv, location, func.ReturnType)
            |> Some
        | _ ->
            None

    member _.Type = OlyTypeSymbol(boundModel, benv, location, value.Type)

    member _.Enclosing: OlyEnclosingSymbol = OlyEnclosingSymbol(boundModel, benv, location, value.Enclosing)

    member this.IsExternal =
        match value.Enclosing with
        | EnclosingSymbol.Entity(ent) -> ent.IsImported
        | _ -> false

    member this.IsFunction = value.IsFunction

    member this.IsConstructor = value.IsConstructor

    member this.IsOperator = value.IsFunction && OlySyntaxFacts.IsOperator value.Name

    member this.IsField = value.IsField

    member this.IsFieldConstant =
        match value with
        | :? IFieldSymbol as field -> field.Constant.IsSome
        | _ -> false

    member this.TryFieldConstant =
        match value with
        | :? IFieldSymbol as field ->
            match field.Constant with
            | ValueSome(constant) ->
                OlyConstantSymbol(boundModel, benv, location, constant.ToLiteral())
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

    member this.IsParameter = this.IsLocal && (value.ValueFlags &&& ValueFlags.Parameter = ValueFlags.Parameter)

    member this.IsEntryPoint =
        match value with
        | :? IFunctionSymbol as func -> func.IsEntryPoint
        | _ -> false

    member this.Parameters =
        match value with
        | :? IFunctionSymbol as func ->
            func.Parameters
            |> ImArray.map (fun x -> OlyValueSymbol(boundModel, benv, location, x))
        | _ ->
            ImArray.empty

    member this.IsPure =
        value.FunctionFlags.HasFlag(FunctionFlags.Pure)

    member this.TryPropertyGetterSetter =
        match value with
        | :? IPropertySymbol as prop ->
            let getterOpt =
                prop.Getter
                |> Option.map (fun x ->
                    OlyValueSymbol(boundModel, benv, location, x)
                )
            let setterOpt =
                prop.Setter
                |> Option.map (fun x ->
                    OlyValueSymbol(boundModel, benv, location, x)
                )
            Some(getterOpt, setterOpt)
        | _ ->
            None

    override this.GetHashCode() = value.Formal.Id.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? OlyValueSymbol as valueSymbol ->
            areValueSignaturesEqual value valueSymbol.Internal
        | _ ->
            false

[<Sealed>] 
type OlyAttributeSymbol internal (boundModel: OlyBoundModel, benv: BoundEnvironment, location, attr: AttributeSymbol) =
    inherit OlySymbol(location)

    member private _.Internal = attr

    override _.Name =
        match attr with
        | AttributeSymbol.Open -> "open"
        | AttributeSymbol.Null -> "null"
        | AttributeSymbol.Import _ -> "import"
        | AttributeSymbol.Export _ -> "export"
        | AttributeSymbol.Intrinsic _ -> "intrinsic"
        | AttributeSymbol.Inline -> "inline"
        | AttributeSymbol.NotInline -> "not inline"
        | AttributeSymbol.Blittable -> "blittable"
        | AttributeSymbol.Pure -> "pure"
        | AttributeSymbol.Constructor(ctor, _, _, _) -> ctor.Name

    override _.SignatureText =
        match attr with
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
        | AttributeSymbol.Inline -> "inline"
        | AttributeSymbol.NotInline -> "not inline"
        | AttributeSymbol.Constructor(ctor, _, _, _) -> OlyValueSymbol(boundModel, benv, location, ctor).SignatureText

    override _.IsLocal = false

    override this.TryGetDefinitionLocation(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        match attr with
        | AttributeSymbol.Constructor(ctor, _, _, _) -> OlyValueSymbol(boundModel, benv, location, ctor).TryGetDefinitionLocation(ct)
        | _ -> None

    override this.IsSimilarTo(symbol) =
        match attr with
        | AttributeSymbol.Constructor(ctor, _, _, _) ->
            OlyValueSymbol(boundModel, benv, location, ctor).IsSimilarTo(symbol)
        | _ ->
            match symbol with
            | :? OlyAttributeSymbol as symbol ->
                match attr, symbol.Internal with
                | AttributeSymbol.Import(platform1, path1, name1), AttributeSymbol.Import(platform2, path2, name2) ->
                    platform1 = platform2 && path1 = path2 && name1 = name2
                | AttributeSymbol.Export, AttributeSymbol.Export ->
                    true
                | AttributeSymbol.Inline, AttributeSymbol.Inline -> 
                    true
                | _ -> 
                    false
            | _ ->
                false

[<Sealed>] 
type OlyEnclosingSymbol internal (boundModel: OlyBoundModel, benv: BoundEnvironment, syntax, enclosing: EnclosingSymbol) =

    member _.TryType: OlyTypeSymbol option =
        match enclosing with
        | EnclosingSymbol.Entity(ent) when not ent.IsNamespace -> OlyTypeSymbol(boundModel, benv, syntax, ent.AsType) |> Some
        | EnclosingSymbol.Witness(concreteTy, _) -> OlyTypeSymbol(boundModel, benv, syntax, concreteTy) |> Some
        | _ -> None

    member _.TryNamespace =
        match enclosing with
        | EnclosingSymbol.Entity(ent) when ent.IsNamespace -> OlyNamespaceSymbol(boundModel, benv, syntax, ent) |> Some
        | _ -> None

    member this.IsStruct =
        match enclosing with
        | EnclosingSymbol.Entity(ent) -> ent.IsAnyStruct
        | _ -> false

[<Sealed>] 
type OlyDirectiveSymbol internal (syntaxNode: OlySyntaxNode, name: string, value: string) =
    inherit OlySymbol(syntaxNode)

    override _.SignatureText =
        $"#{name} \"{value}\""

    override _.Name = name

    override _.TryGetDefinitionLocation(_) = None

    override this.IsSimilarTo(symbol) =
        match symbol with
        | :? OlyDirectiveSymbol as symbol ->
            name = symbol.Name && symbol.Value = value
        | _ ->
            false

    override _.IsLocal = false

    member _.Value = value

// TODO: Weird name.
[<Sealed>] 
type OlyBoundSubModel internal (boundModel: OlyBoundModel, boundNode: IBoundNode, benv: BoundEnvironment) =

    let syntax = boundModel.SyntaxTree.DummyNode

    member _.Root: OlyBoundModel = boundModel

    member internal _.InternalBoundNode = boundNode

    member internal _.InternalEnvironment = benv

    member _.GetUnqualifiedValueSymbols() =
        benv.senv.unqualifiedSymbols.Values
        |> Seq.map (fun x -> 
            match x with
            | UnqualifiedSymbol.Local value ->
                seq { OlyValueSymbol(boundModel, benv, syntax, value) }
            | UnqualifiedSymbol.Field value ->
                seq { OlyValueSymbol(boundModel, benv, syntax, value) }
            | UnqualifiedSymbol.Property value ->
                seq { OlyValueSymbol(boundModel, benv, syntax, value) }
            | UnqualifiedSymbol.Function value ->
                seq { OlyValueSymbol(boundModel, benv, syntax, value) }
            | UnqualifiedSymbol.FunctionGroup(funcGroup) ->
                seq { OlyFunctionGroupSymbol(boundModel, benv, syntax, funcGroup) :> OlyValueSymbol }
            | UnqualifiedSymbol.AmbiguousValues(values) when not values.IsEmpty ->
                seq { OlyValueSymbol(boundModel, benv, syntax, values[0]) }
            | _ ->
                Seq.empty
        )
        |> Seq.concat

    member this.GetUnqualifiedValueSymbols(containsText: string) =
        if String.IsNullOrWhiteSpace containsText then
            this.GetUnqualifiedValueSymbols()
        else
            benv.senv.unqualifiedSymbols
            |> Seq.filter (fun x -> x.Key.Contains(containsText))
            |> Seq.map (fun x -> 
                match x.Value with
                | UnqualifiedSymbol.Local value ->
                    seq { OlyValueSymbol(boundModel, benv, syntax, value) }
                | UnqualifiedSymbol.Field value ->
                    seq { OlyValueSymbol(boundModel, benv, syntax, value) }
                | UnqualifiedSymbol.Property value ->
                    seq { OlyValueSymbol(boundModel, benv, syntax, value) }
                | UnqualifiedSymbol.Function value ->
                    seq { OlyValueSymbol(boundModel, benv, syntax, value) }
                | UnqualifiedSymbol.FunctionGroup(funcGroup) ->
                    seq { OlyFunctionGroupSymbol(boundModel, benv, syntax, funcGroup) :> OlyValueSymbol }
                | UnqualifiedSymbol.AmbiguousValues(values) when not values.IsEmpty ->
                    seq { OlyValueSymbol(boundModel, benv, syntax, values[0]) }
                | _ ->
                    Seq.empty
            )
            |> Seq.concat

    member _.GetUnqualifiedTypeSymbols() =
        benv.senv.unqualifiedTypes.Values
        |> Seq.concat
        |> Seq.map (fun pair -> pair.Value)
        |> Seq.choose (fun tys ->
            if tys.IsEmpty then
                None
            elif tys.Length = 1 then
                OlyTypeSymbol(boundModel, benv, syntax, tys[0])
                |> Some
            else
                OlyTypeSymbol(boundModel, benv, syntax, TypeSymbol.Error(None))
                |> Some
        )

    member this.GetUnqualifiedTypeSymbols(containsText: string) =
        if String.IsNullOrWhiteSpace containsText then
            this.GetUnqualifiedTypeSymbols()
        else
            benv.senv.unqualifiedTypes
            |> Seq.map (fun x -> x.Value |> Seq.filter (fun x -> x.Key.Contains(containsText)))
            |> Seq.concat
            |> Seq.map (fun pair -> pair.Value)
            |> Seq.choose (fun tys ->
                if tys.IsEmpty then
                    None
                elif tys.Length = 1 then
                    OlyTypeSymbol(boundModel, benv, syntax, tys[0])
                    |> Some
                else
                    OlyTypeSymbol(boundModel, benv, syntax, TypeSymbol.Error(None))
                    |> Some
            )

    member _.GetUnqualifiedNamespaceSymbols() =
        benv.senv.namespaces.Values
        |> Seq.choose (fun group ->
            match (group :> INamespaceSymbol).Enclosing with
            | EnclosingSymbol.RootNamespace ->
                OlyNamespaceSymbol(boundModel, benv, syntax, group)
                |> Some
            | _ ->
                None
        )

    member this.GetUnqualifiedNamespaceSymbols(containsText: string) =
        if String.IsNullOrWhiteSpace containsText then
            this.GetUnqualifiedNamespaceSymbols(containsText)
        else
            benv.senv.namespaces.Values
            |> Seq.choose (fun group ->
                match (group :> INamespaceSymbol).Enclosing with
                | EnclosingSymbol.RootNamespace when (group :> IEntitySymbol).Name.Contains(containsText) ->
                    OlyNamespaceSymbol(boundModel, benv, syntax, group)
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

    member _.SyntaxNode = boundNode.Syntax

//*****************************************************************************************************************************************************
//*****************************************************************************************************************************************************
//****** Symbol functions that go through the bound tree ******
//****** TODO: The inner functions in OlyBoundModel should be lifted out and placed here.
//*****************************************************************************************************************************************************
//*****************************************************************************************************************************************************

let private getTypeSymbolByIdentifier (bm: OlyBoundModel) (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxIdent: OlySyntaxToken) (ty: TypeSymbol) =
    if predicate syntaxIdent then
        match ty.TryEntity with
        | ValueSome(ent) when ent.IsNamespace ->
            addSymbol(OlyNamespaceSymbol(bm, benv, syntaxIdent, ent))
        | _ ->
            addSymbol(OlyTypeSymbol(bm, benv, syntaxIdent, ty))

let private getTypeSymbolByName bm (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxName: OlySyntaxName) (ty: TypeSymbol) =
    match syntaxName with
    | OlySyntaxName.Identifier(syntaxIdent) ->
        getTypeSymbolByIdentifier bm addSymbol benv predicate syntaxIdent ty

    | OlySyntaxName.Parenthesis(_, syntaxIdentOrOperator, _) ->
        getTypeSymbolByIdentifier bm addSymbol benv predicate syntaxIdentOrOperator ty

    | OlySyntaxName.Generic(syntaxName, syntaxTyArgs) ->
        getTypeSymbolByName bm addSymbol benv predicate syntaxName ty
        getTypeArgumentSymbolsWithTypes bm addSymbol benv predicate syntaxTyArgs ty.TypeArguments

    | OlySyntaxName.Qualified _ ->

        match ty.TryEntity with
        | ValueSome(ent) ->
            (ent.AsEnclosing, syntaxName.AllNames |> List.rev)
            ||> List.fold (fun enclosing syntaxName ->
                match enclosing.TryEntity with
                | Some ent ->
                    if ent.IsNamespace then 
                        getTypeSymbolByName bm addSymbol benv predicate syntaxName ent.AsNamespaceType
                        ent.Enclosing
                    else
                        getTypeSymbolByName bm addSymbol benv predicate syntaxName ent.AsType
                        ent.Enclosing
                | _ ->
                    enclosing
            )
            |> ignore
        | _ ->
            ()

    | _ ->
        raise(InternalCompilerException())

let private getTypeArgumentSymbolsWithTypes bm (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxTyArgs: OlySyntaxTypeArguments) (tys: ImmutableArray<TypeSymbol>) =
    let values = syntaxTyArgs.Values
    (values, tys)
    ||> Seq.iter2 (fun syntaxTyArg ty ->
        getTypeSymbol bm addSymbol benv predicate syntaxTyArg ty
    )

let private getTypeSymbol (bm: OlyBoundModel) (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxTy: OlySyntaxType) (ty: TypeSymbol) =

    let handleTupleElementList (syntaxElementList: OlySyntaxSeparatorList<OlySyntaxTupleElement>) =
        let tyArgs = ty.TypeArguments
        let syntaxElements = syntaxElementList.ChildrenOfType
        if tyArgs.IsEmpty && syntaxElements.Length = 1 then
            match syntaxElements[0] with
            | OlySyntaxTupleElement.Type(syntaxTy) ->
                getTypeSymbol bm addSymbol benv predicate syntaxTy ty
            | _ ->
                ()
        else
            (syntaxElements, ty.TypeArguments)
            ||> ImArray.tryIter2 (fun syntaxElement ty ->
                match syntaxElement with
                | OlySyntaxTupleElement.IdentifierWithTypeAnnotation(syntaxIdent, _, syntaxTy) ->
                    if predicate syntaxIdent then
                        () // TODO: Capture ident for tuple element name
                    getTypeSymbol bm addSymbol benv predicate syntaxTy ty
                | OlySyntaxTupleElement.Type(syntaxTy) ->
                    getTypeSymbol bm addSymbol benv predicate syntaxTy ty
                | _ ->
                    ()
            )

    match syntaxTy with
    | OlySyntaxType.Function(syntaxInputTy, _, syntaxOutputTy) ->
        match ty.TryFunction with
        | ValueSome(argTys, returnTy) ->
            // TODO: Kind of a hack using TypeSymbol.Tuple.
            let inputTy = 
                if argTys.IsEmpty then
                    TypeSymbol.Unit
                elif argTys.Length = 1 then
                    argTys[0]
                else
                    TypeSymbol.Tuple(argTys, ImArray.empty)

            getTypeSymbol bm addSymbol benv predicate syntaxInputTy inputTy
            getTypeSymbol bm addSymbol benv predicate syntaxOutputTy returnTy
        | _ ->
            ()

    | OlySyntaxType.FunctionPtr(_, _, syntaxInputTy, _, syntaxOutputTy) ->
        match ty.TryFunction with
        | ValueSome(argTys, returnTy) ->
            // TODO: Kind of a hack using TypeSymbol.Tuple.
            let inputTy = 
                if argTys.IsEmpty then
                    TypeSymbol.Unit
                elif argTys.Length = 1 then
                    argTys[0]
                else
                    TypeSymbol.Tuple(argTys, ImArray.empty)

            getTypeSymbol bm addSymbol benv predicate syntaxInputTy inputTy
            getTypeSymbol bm addSymbol benv predicate syntaxOutputTy returnTy
        | _ ->
            ()

    | OlySyntaxType.Name(syntaxName) ->
        getTypeSymbolByName bm addSymbol benv predicate syntaxName ty

    | OlySyntaxType.Tuple(_, syntaxTupleElementList, _) ->
        handleTupleElementList syntaxTupleElementList

    | OlySyntaxType.Variadic(syntaxIdent, _) ->
        getTypeSymbolByIdentifier bm addSymbol benv predicate syntaxIdent ty

    | OlySyntaxType.VariadicIndexer(syntaxIdent, _, _, syntaxConstExpr, _) ->
        getTypeSymbolByIdentifier bm addSymbol benv predicate syntaxIdent ty
        match stripTypeEquationsExceptAlias ty, syntaxConstExpr with
        | TypeSymbol.DependentIndexer(inputValueTy, _), OlySyntaxExpression.Name(OlySyntaxName.Identifier(syntaxIdentForConst)) ->
            getTypeSymbolByIdentifier bm addSymbol benv predicate syntaxIdentForConst inputValueTy
        | _ ->
            ()             

    | OlySyntaxType.Array(syntaxElementTy, _) ->
        if ty.TypeArguments.Length > 0 then
            getTypeSymbol bm addSymbol benv predicate syntaxElementTy ty.TypeArguments.[0]

    | OlySyntaxType.MutableArray(syntaxElementTy, _) ->
        if ty.TypeArguments.Length > 0 then
            getTypeSymbol bm addSymbol benv predicate syntaxElementTy ty.TypeArguments.[0]

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
                    getValueSymbolByIdentifier bm addSymbol benv predicate syntaxToken func
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
            getTypeSymbol bm addSymbol benv predicate syntaxElementTy ty.TypeArguments.[0]
        getTypeSymbolByIdentifier bm addSymbol benv predicate syntaxIdent ty

    | OlySyntaxType.WildCard _
    | OlySyntaxType.Error _ -> ()

    | _ ->
        raise(InternalCompilerUnreachedException())

let private getValueSymbolByIdentifier bm (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxIdent: OlySyntaxToken) (value: IValueSymbol) =
    if predicate syntaxIdent then
        match value with
        | :? FunctionGroupSymbol as funcGroup ->
            addSymbol(OlyFunctionGroupSymbol(bm, benv, syntaxIdent, funcGroup))
        | _ ->
            addSymbol(OlyValueSymbol(bm, benv, syntaxIdent, value))

[<Sealed>] 
type OlyBoundModel internal (
        asm: AssemblySymbol, 
        syntaxTree: OlySyntaxTree, 
        tryGetLocation: (OlyILAssemblyIdentity * ISymbol * CancellationToken -> OlySourceLocation option), 
        getPartialDeclTable: (CancellationToken -> BoundDeclarationTable), 
        getResult: (CancellationToken -> FullyBoundResult)) as this =

    let rec nodeContains (boundNode: IBoundNode) (syntaxTarget: OlySyntaxNode) =
        let span = boundNode.Syntax.FullTextSpan
        let targetSpan = syntaxTarget.TextSpan
        span.Contains(targetSpan)

    let nodeHas (syntax: OlySyntaxNode) (syntaxTarget: OlySyntaxNode) =
        let span = syntax.TextSpan
        let targetSpan = syntaxTarget.TextSpan
        span.Contains(targetSpan)

    let nodeEquals (syntax: OlySyntaxNode) (syntaxTarget: OlySyntaxNode) =
        let span = syntax.TextSpan
        let targetSpan = syntaxTarget.TextSpan
        span.Start = targetSpan.Start && span.End = targetSpan.End

    let rec getParameterSymbols (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxPars: OlySyntaxParameters) (logicalPars: ILocalParameterSymbol romem) =
        (syntaxPars.Values.AsMemory(), logicalPars)
        ||> ROMem.iter2 (fun syntaxPar par ->
            getParameterSymbol addSymbol benv predicate syntaxPar par
        )

    and getParameterSymbol (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxPar: OlySyntaxParameter) (par: ILocalParameterSymbol) =
        match syntaxPar with
        | OlySyntaxParameter.IdentifierWithTypeAnnotation(syntaxAttrs, _, syntaxIdent, _, syntaxTy) ->
            getAttributeSymbols addSymbol benv predicate syntaxAttrs.Values par.Attributes
            if predicate syntaxIdent then
                addSymbol(OlyValueSymbol(this, benv, syntaxIdent, par))
            getTypeSymbol this addSymbol benv predicate syntaxTy par.Type

        | OlySyntaxParameter.Identifier(syntaxAttrs, _, syntaxIdent) ->
            getAttributeSymbols addSymbol benv predicate syntaxAttrs.Values par.Attributes
            if predicate syntaxIdent then
                if String.IsNullOrWhiteSpace par.Name then
                    getTypeSymbolByIdentifier this addSymbol benv predicate syntaxIdent par.Type
                else
                    addSymbol(OlyValueSymbol(this, benv, syntaxIdent, par))

        | OlySyntaxParameter.Type(syntaxAttrs, syntaxTy) ->
            getAttributeSymbols addSymbol benv predicate syntaxAttrs.Values par.Attributes
            getTypeSymbol this addSymbol benv predicate syntaxTy par.Type

        | OlySyntaxParameter.Error _ ->
            ()

        | _ ->
            raise(InternalCompilerException())

    and getParameterSymbolsByValues (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxPars: OlySyntaxParameters) (pars: ImmutableArray<ILocalParameterSymbol>) =
        (syntaxPars.Values, pars)
        ||> Seq.iter2 (fun syntaxPar par ->
            getParameterSymbolByValue addSymbol benv predicate syntaxPar par
        )

    and getParameterSymbolByValue (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxPar: OlySyntaxParameter) (par: ILocalParameterSymbol) =
        match syntaxPar with
        | OlySyntaxParameter.IdentifierWithTypeAnnotation(syntaxAttrs, _, syntaxIdent, _, syntaxTy) ->
            getAttributeSymbols addSymbol benv predicate syntaxAttrs.Values par.Attributes
            if predicate syntaxIdent then
                addSymbol(OlyValueSymbol(this, benv, syntaxIdent, par))
            getTypeSymbol this addSymbol benv predicate syntaxTy par.Type

        | OlySyntaxParameter.Identifier(syntaxAttrs, _, syntaxIdent) ->
            getAttributeSymbols addSymbol benv predicate syntaxAttrs.Values par.Attributes
            if predicate syntaxIdent then
                addSymbol(OlyValueSymbol(this, benv, syntaxIdent, par))

        | OlySyntaxParameter.Type(syntaxAttrs, syntaxTy) ->
            getAttributeSymbols addSymbol benv predicate syntaxAttrs.Values par.Attributes
            getTypeSymbol this addSymbol benv predicate syntaxTy par.Type

        | OlySyntaxParameter.Error _ ->
            ()

        | _ ->
            raise(InternalCompilerException())

    and getTypeParameterSymbols (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxTyPars: OlySyntaxTypeParameters) (tyPars: TypeParameterSymbol imarray) =
        let syntaxTyPars = syntaxTyPars.Values
        let skipAmount = tyPars.Length - syntaxTyPars.Length

        // Defensive.
        if skipAmount >= 0 then
            (syntaxTyPars.AsMemory(), tyPars.AsMemory().Slice(skipAmount))
            ||> ROMem.iter2 (fun syntaxTyPar tyPar -> 
                getTypeSymbol this addSymbol benv predicate syntaxTyPar tyPar.AsType
            )

    and getTypeSymbolsByTypes (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxTys: OlySyntaxType seq) (tys: ImmutableArray<TypeSymbol>) =
        (syntaxTys, tys)
        ||> Seq.iter2 (fun syntaxTy ty ->
            getTypeSymbol this addSymbol benv predicate syntaxTy ty
        )

    and getAttributeSymbol (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxAttr: OlySyntaxAttribute) (attr: AttributeSymbol) : unit =
        match syntaxAttr with
        | OlySyntaxAttribute.Expression(syntaxExpr) ->
            match syntaxExpr, attr with
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
                                if predicate syntaxToken then
                                    addSymbol(OlyConstantSymbol(this, benv, syntaxToken, arg.ToLiteral()))
                            | _ ->
                                ()
                        | OlySyntaxExpression.Call(syntaxExpr, syntaxArgs) ->
                            match syntaxExpr with
                            | OlySyntaxExpression.Name(syntaxName) ->
                                match arg with
                                | ConstantSymbol.External(func) ->
                                    getSymbolsByNameAndValue addSymbol benv predicate syntaxName func
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
                    getSymbolsByNameAndValue addSymbol benv predicate syntaxName ctor
                | _ ->
                    ()
            | _ ->
                ()
        | _ ->
            ()

    and getAttributeSymbols (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxAttrs: OlySyntaxAttribute imarray) (attrs: AttributeSymbol imarray) =
        (syntaxAttrs, attrs)
        ||> ImArray.tryIter2 (fun syntaxAttr attr ->
            getAttributeSymbol addSymbol benv predicate syntaxAttr attr
        )

    and getSymbolsByConstraint (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxConstr: OlySyntaxConstraint) (constr: ConstraintSymbol) =
        match syntaxConstr with
        | OlySyntaxConstraint.Type(syntaxTy) ->
            match constr.TryGetSubtypeOf() with
            | ValueSome constrTy ->
                getTypeSymbol this addSymbol benv predicate syntaxTy constrTy
            | _ ->
                ()
        | OlySyntaxConstraint.ConstantType(_, syntaxTy) ->
            match constr with
            | ConstraintSymbol.ConstantType(ty) ->
                getTypeSymbol this addSymbol benv predicate syntaxTy ty.Value
            | _ ->
                ()
        | _ ->
            ()

    and getSymbolsByConstraintClauseList (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxConstrClauseList: OlySyntaxSeparatorList<OlySyntaxConstraintClause>) (tyPars: TypeParameterSymbol imarray) =
        forEachConstraintBySyntaxConstraintClause syntaxConstrClauseList.ChildrenOfType tyPars (fun syntaxConstrClause tyPar constrs ->
            match syntaxConstrClause with
            | OlySyntaxConstraintClause.ConstraintClause(_, syntaxTy, _, syntaxConstrList) ->
                getTypeSymbol this addSymbol benv predicate syntaxTy tyPar.AsType
                (syntaxConstrList.ChildrenOfType, constrs)
                ||> ImArray.tryIter2 (fun syntaxConstr constr ->
                    getSymbolsByConstraint addSymbol benv predicate syntaxConstr constr
                )
            | _ ->
                ()
        )

    and getTypeSymbolByNameAndEnclosing (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxName: OlySyntaxName) (enclosing: EnclosingSymbol) =
        match enclosing with
        | EnclosingSymbol.Entity(ent) ->
            if ent.IsNamespace then
                getTypeSymbolByName this addSymbol benv predicate syntaxName ent.AsNamespaceType
            else
                getTypeSymbolByName this addSymbol benv predicate syntaxName ent.AsType
        | EnclosingSymbol.Witness(concreteTy, _) ->
            getTypeSymbolByName this addSymbol benv predicate syntaxName concreteTy
        | _ ->
            ()

    and getSymbolsByNameAndValue (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxName: OlySyntaxName) (value: IValueSymbol) : unit =
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
            if value.IsConstructor then
                value.Enclosing.Enclosing
            else
                value.Enclosing
        syntaxNames
        |> ImArray.iter (fun syntaxName ->
            getTypeSymbolByNameAndEnclosing addSymbol benv predicate syntaxName enclosing
            enclosing <- enclosing.Enclosing
        )

        if syntaxNames.IsEmpty && value.IsInvalid && not value.IsFunctionGroup then
            getTypeSymbolByNameAndEnclosing addSymbol benv predicate syntaxName enclosing
        else
            let syntaxName = syntaxName.LastName
            match syntaxName with
            | OlySyntaxName.Identifier(syntaxIdent) ->
                getValueSymbolByIdentifier this addSymbol benv predicate syntaxIdent value.Formal

                let tyArgs = value.AllTypeArguments
                if not tyArgs.IsEmpty then
                    match syntaxName.Parent with
                    | :? OlySyntaxName as syntaxName ->
                        match syntaxName with
                        | OlySyntaxName.Generic(_, syntaxTyArgs) ->
                            let syntaxTyArgs = syntaxTyArgs.Values
                            let skipAmount = max 0 (tyArgs.Length - syntaxTyArgs.Length)
                            getTypeArgumentSymbols addSymbol benv predicate syntaxTyArgs (tyArgs |> Seq.skip skipAmount |> ImArray.ofSeq)
                        | _ ->
                            ()
                    | _ ->
                        ()
            | _ ->
                ()

    and getTypeArgumentSymbols (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxTyArgs: OlySyntaxType imarray) (tyArgs: ImmutableArray<TypeArgumentSymbol>) =
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
            getTypeSymbol this addSymbol benv predicate syntaxTyArg tyArg
        )

    and getTypeSymbolFromTypeAnnotation (addSymbol: OlySymbol -> unit) benv (predicate: OlySyntaxToken -> bool) (syntaxReturnTyAnnot: OlySyntaxReturnTypeAnnotation) (ty: TypeSymbol) =
        match syntaxReturnTyAnnot with
        | OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy) ->
            getTypeSymbol this addSymbol benv predicate syntaxTy ty
        | _ ->
            ()

    let getSymbols (syntaxNode: OlySyntaxNode) (filterSymbol: OlySymbol -> bool) (compare: OlySyntaxNode -> OlySyntaxNode -> bool) canFindMultipleSymbols (ct: CancellationToken) =

        let predicate = fun node -> if canFindMultipleSymbols then true else nodeEquals syntaxNode node

        let symbols = ImmutableArray.CreateBuilder<OlySymbol>()
        let addSymbol = 
            fun symbol -> 
                if filterSymbol symbol then
                    symbols.Add(symbol)

        let boundTree = this.GetBoundTree(ct)

        // TODO: There must be a better way to handle this.
        //       It's hacky because we assume the directives are always at the top, but it wont be the case for #if, #when, #else.
        let directiveSymbols = 
            let chooser (x: OlyToken) =
                if x.IsDirective && predicate x.Node then
                    match x.TryDirectiveText with
                    | ValueSome(name, value) ->
                        OlyDirectiveSymbol(x.Node, name, value) |> Some
                    | _ ->
                        None
                else
                    None
            match boundTree.SyntaxTree.GetRoot(ct).TryGetFirstToken(true) with
            | Some(firstNonTriviaToken) when not firstNonTriviaToken.IsEndOfSource ->                
                if compare syntaxNode firstNonTriviaToken.Node || syntaxNode.TextSpan.End < firstNonTriviaToken.TextSpan.Start then
                    firstNonTriviaToken.GetLeadingTrivia()
                    |> ImArray.choose chooser
                else
                    ImArray.empty
            | _ ->
                boundTree.SyntaxTree.GetRoot(ct).FindTokens(syntaxNode.TextSpan.Start, skipTrivia = false, ct = ct)
                |> ImArray.choose chooser
        directiveSymbols
        |> ImArray.iter addSymbol

        let openDecls = boundTree.RootEnvironment.openDecls

        (openDecls, boundTree.SyntaxTree.GetOpenDeclarationNames(ct))
        ||> ImArray.tryIter2 (fun ent syntaxName ->
            ct.ThrowIfCancellationRequested()
            if compare syntaxNode syntaxName then
                if ent.IsNamespace then
                    getTypeSymbolByName this addSymbol boundTree.RootEnvironment predicate syntaxName ent.AsNamespaceType
                else
                    getTypeSymbolByName this addSymbol boundTree.RootEnvironment predicate syntaxName ent.AsType
        )

        boundTree.ForEachForTooling((fun boundNode -> ct.ThrowIfCancellationRequested(); compare syntaxNode boundNode.Syntax), 
            fun boundNode ->
                ct.ThrowIfCancellationRequested()
                this.GetImmediateSymbols(boundNode, addSymbol, predicate)
        )

        symbols.ToImmutable()

    let getLocalBindingSymbols addSymbol benv predicate (syntax: OlySyntaxNode) (bindingInfo: LocalBindingInfoSymbol) =
        match bindingInfo with
        | BindingLocalFunction(func) ->
            match syntax with
            | :? OlySyntaxBindingDeclaration as syntax ->
                match syntax with
                | OlySyntaxBindingDeclaration.Function(syntaxFuncName, syntaxTyPars, syntaxPars, syntaxReturnTyAnnot, syntaxConstrClauseList) ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxFuncName.Identifier func
                    getTypeParameterSymbols addSymbol benv predicate syntaxTyPars func.TypeParameters
                    getParameterSymbols addSymbol benv predicate syntaxPars func.LogicalParameters
                    getTypeSymbolFromTypeAnnotation addSymbol benv predicate syntaxReturnTyAnnot func.ReturnType
                    getSymbolsByConstraintClauseList addSymbol benv predicate syntaxConstrClauseList func.TypeParameters

                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxIdent func
                    getTypeSymbolFromTypeAnnotation addSymbol benv predicate syntaxTyAnnot func.Type

                | _ ->
                    ()
            | _ ->
                ()

        | BindingLocal(value) ->
            match syntax with
            | :? OlySyntaxBindingDeclaration as syntax ->
                match syntax with
                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxIdent value
                    getTypeSymbolFromTypeAnnotation addSymbol benv predicate syntaxTyAnnot value.Type                
                | _ ->
                    ()
            | _ ->
                ()

    let getBindingSymbols addSymbol benv predicate (syntax: OlySyntaxNode) (bindingInfo: BindingInfoSymbol) =
        match bindingInfo with
        | BindingProperty(_, prop) ->
            match syntax with
            | :? OlySyntaxBindingDeclaration as syntax ->
                match syntax with
                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxIdent prop
                    getTypeSymbolFromTypeAnnotation addSymbol benv predicate syntaxTyAnnot prop.Type
                | _ ->
                    ()
            | _ ->
                ()

        | BindingFunction(func)
        | BindingPattern(_, func) ->
            match syntax with
            | :? OlySyntaxBindingDeclaration as syntax ->
                match syntax with
                | OlySyntaxBindingDeclaration.Function(syntaxFuncName, syntaxTyPars, syntaxPars, syntaxReturnTyAnnot, syntaxConstrClauseList) ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxFuncName.Identifier func
                    getTypeParameterSymbols addSymbol benv predicate syntaxTyPars func.TypeParameters
                    getParameterSymbols addSymbol benv predicate syntaxPars func.LogicalParameters
                    getTypeSymbolFromTypeAnnotation addSymbol benv predicate syntaxReturnTyAnnot func.ReturnType
                    getSymbolsByConstraintClauseList addSymbol benv predicate syntaxConstrClauseList func.TypeParameters

                | OlySyntaxBindingDeclaration.New(syntaxNewToken, syntaxPars) ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxNewToken func
                    getParameterSymbols addSymbol benv predicate syntaxPars func.LogicalParameters

                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxIdent func
                    getTypeSymbolFromTypeAnnotation addSymbol benv predicate syntaxTyAnnot func.Type

                | OlySyntaxBindingDeclaration.Get _
                | OlySyntaxBindingDeclaration.Set _ ->
                    // TODO:
                    ()

                | _ ->
                    ()

            | _ ->
                ()

        | BindingField(field) ->
            match syntax with
            | :? OlySyntaxBindingDeclaration as syntax ->
                match syntax with
                | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxIdent field
                    getTypeSymbolFromTypeAnnotation addSymbol benv predicate syntaxTyAnnot field.Type                
                | _ ->
                    ()
            | _ ->
                ()

    let getSymbolsByLiteral addSymbol benv predicate (syntax: OlySyntaxNode) (literal: BoundLiteral) =
        if predicate syntax then
            addSymbol(OlyConstantSymbol(this, benv, syntax, literal))

    member internal _.GetImmediateSymbols(boundNode: IBoundNode, addSymbol: OlySymbol -> unit, predicate: OlySyntaxNode -> bool) =
        match boundNode with
        | :? BoundRoot as root ->
            match root with
            | BoundRoot.Namespace(syntax, benv, namespac, _) ->
                match syntax with
                | OlySyntaxCompilationUnit.Namespace(_, syntaxName, _, _) ->
                    getTypeSymbolByName this addSymbol benv predicate syntaxName namespac.AsNamespaceType
                | _ ->
                    ()

            | _ ->
                ()

        | :? BoundExpression as boundNode ->
            match boundNode with
            | BoundExpression.Call(BoundSyntaxInfo.User(syntax, benv), _, witnessArgs (* will need this for explicit witnesses *), _, Some syntaxValueName, value, _) ->
                getSymbolsByNameAndValue addSymbol benv predicate syntaxValueName value

            | BoundExpression.Value(syntaxInfo, value) ->
                match syntaxInfo.Syntax.TryName, syntaxInfo.TryEnvironment with
                | Some syntaxName, Some benv ->
                    getSymbolsByNameAndValue addSymbol benv predicate syntaxName.LastName value
                | _ ->
                    ()

            | BoundExpression.Literal(syntaxInfo, literal) ->
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    getSymbolsByLiteral addSymbol benv predicate syntaxInfo.Syntax literal
                | _ ->
                    ()

            | BoundExpression.GetField(syntaxInfo, _, syntaxNameOpt, field) ->
                match syntaxInfo.TryEnvironment, syntaxNameOpt with
                | Some benv, Some syntaxName ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxName.LastIdentifier field
                | _ ->
                    ()

            | BoundExpression.SetField(syntaxInfo, _, syntaxNameOpt, field, _) ->
                match syntaxInfo.TryEnvironment, syntaxNameOpt with
                | Some benv, Some syntaxName ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxName.LastIdentifier field
                | _ ->
                    ()

            | BoundExpression.SetValue(syntaxInfo, syntaxValueNameOpt, value, _) ->
                match syntaxInfo.TryEnvironment, syntaxValueNameOpt with
                | Some benv, Some syntaxValueName ->
                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxValueName.LastIdentifier value
                | _ ->
                    ()

            | BoundExpression.GetProperty(syntaxInfo, _, syntaxNameOpt, prop) 
            | BoundExpression.SetProperty(syntaxInfo, _, syntaxNameOpt, prop, _) ->
                match syntaxInfo.TryEnvironment, syntaxNameOpt with
                | Some benv, Some syntaxName ->
                    getSymbolsByNameAndValue addSymbol benv predicate syntaxName prop
                | _ ->
                    ()

            | BoundExpression.Let(syntaxInfo, bindingInfo, _, _) when not syntaxInfo.IsGeneratedKind ->
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    let syntax =
                        match syntaxInfo.Syntax.TryGetBindingDeclaration() with
                        | ValueSome syntax -> syntax :> OlySyntaxNode
                        | _ -> syntaxInfo.Syntax
                    getLocalBindingSymbols addSymbol benv predicate syntax bindingInfo
                | _ ->
                    ()

            | BoundExpression.MemberDefinition(syntaxInfo, binding) when not syntaxInfo.IsGeneratedKind ->
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
                                getAttributeSymbols addSymbol benv predicate syntaxAttrs.Values attrs
                            | _ ->
                                ()
                        | _ ->
                            ()
                    | _ ->
                        ()

                let syntaxInfo = binding.SyntaxInfo
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    let syntax =
                        match syntaxInfo.Syntax.TryGetBindingDeclaration() with
                        | ValueSome syntax -> syntax :> OlySyntaxNode
                        | _ -> syntaxInfo.Syntax
                    getBindingSymbols addSymbol benv predicate syntax binding.Info
                | _ ->
                    ()

            | BoundExpression.EntityDefinition(syntaxInfo, _, ent) ->
                match syntaxInfo.TryEnvironment, syntaxInfo.Syntax with
                | Some benv, (:? OlySyntaxCompilationUnit as syntax) ->
                    match syntax with
                    | OlySyntaxCompilationUnit.Module(syntaxAttrs, syntaxAccessor, _, syntaxName, syntaxConstrClauseList, _, _) ->
                        let syntaxAttrs = syntaxAttrs.Values
                        getAttributeSymbols addSymbol benv predicate syntaxAttrs ent.Attributes
                        getTypeSymbolByName this addSymbol benv predicate syntaxName ent.AsType
                        getSymbolsByConstraintClauseList addSymbol benv predicate syntaxConstrClauseList ent.TypeParameters
                    | _ ->
                        ()

                | Some benv, (:? OlySyntaxExpression as syntax) ->
                    match syntax with
                    | OlySyntaxExpression.TypeDeclaration(syntaxAttrs, _, _, syntaxTyDefName, syntaxTyPars, syntaxConstrClauseList, _, syntaxBodyExpr) ->
                        let syntaxAttrs = syntaxAttrs.Values
                        getAttributeSymbols addSymbol benv predicate syntaxAttrs ent.Attributes
                        getTypeSymbolByIdentifier this addSymbol benv predicate syntaxTyDefName.Identifier (TypeSymbol.Entity(ent))
                        getTypeParameterSymbols addSymbol benv predicate syntaxTyPars ent.LogicalTypeParameters
                        getSymbolsByConstraintClauseList addSymbol benv predicate syntaxConstrClauseList ent.TypeParameters

                        match syntaxBodyExpr with
                        | OlySyntaxTypeDeclarationBody.Body(syntaxExtends, syntaxImplements, syntaxCaseList, _) ->  
                            if ent.IsEnum then
                                if ent.Extends.Length = 1 then
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
                                                    getValueSymbolByIdentifier this addSymbol benv predicate syntaxIdent x
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

                            getTypeSymbolsByTypes addSymbol benv predicate syntaxExtends ent.Extends
                            getTypeSymbolsByTypes addSymbol benv predicate syntaxImplements ent.Implements
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
                            getTypeSymbol this addSymbol benv predicate syntaxTy ty
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
                            getParameterSymbolsByValues addSymbol benv predicate syntaxPars parValues
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
                getTypeSymbolByName this addSymbol benv predicate syntaxName ent

            | BoundExpression.ErrorWithType(syntaxName, benv, ty) ->
                getTypeSymbolByName this addSymbol benv predicate syntaxName ty

            | _ ->
                ()
                    
        | :? BoundCasePattern as pattern ->
            match pattern with
            | BoundCasePattern.FieldConstant(syntaxPattern, benv, field) ->
                match syntaxPattern with
                | OlySyntaxPattern.Name(syntaxName)
                | OlySyntaxPattern.Function(syntaxName, _, _, _) ->
                    getSymbolsByNameAndValue addSymbol benv predicate syntaxName field
                | _ ->
                    ()

            | BoundCasePattern.Literal(syntaxPattern, benv, literal) ->
                getSymbolsByLiteral addSymbol benv predicate syntaxPattern literal
                
            | BoundCasePattern.Local(syntaxPattern, benv, value) ->
                match syntaxPattern with
                | OlySyntaxPattern.Name(syntaxName) ->
                    getSymbolsByNameAndValue addSymbol benv predicate syntaxName value
                | _ ->
                    ()

            | BoundCasePattern.Function(syntaxPattern, benv, pat, _, _) ->
                match syntaxPattern with
                | OlySyntaxPattern.Name(syntaxName)
                | OlySyntaxPattern.Function(syntaxName, _, _, _) ->
                    getSymbolsByNameAndValue addSymbol benv predicate syntaxName pat.PatternFunction
                | _ ->
                    ()

            | _ ->
                ()


        | _ ->
            ()

    member this.TryGetSubModel(syntaxToken: OlyToken, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        let data = ResizeArray<IBoundNode * BoundEnvironment>()
        
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
                        match expr.TryEnvironment with
                        | Some benv ->
                            data.Add(boundNode, benv)
                        | _ ->
                            ()
                | _ ->
                    ()
            )
        )

        data
        |> Seq.sortBy (fun (boundNode, _) ->
            boundNode.Syntax.TextSpan.Width
        )
        |> Seq.tryHead
        |> Option.map (fun (boundNode, benv) -> OlyBoundSubModel(this, boundNode, benv))

    member this.TryGetWhitespaceSubModel(offside: int, syntaxToken: OlyToken, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        if not syntaxToken.IsWhitespaceTrivia then None
        else

        match syntaxToken.TryPreviousToken((fun _ -> true), ct = ct) with
        | ValueNone -> None
        | ValueSome syntaxToken ->

        let data = ResizeArray<IBoundNode * BoundEnvironment>()
        
        let syntaxNode = syntaxToken.Node

        let boundTree = this.GetBoundTree(ct)

        boundTree.ForEachForTooling((fun boundNode -> ct.ThrowIfCancellationRequested(); nodeContains boundNode syntaxNode),
            (fun boundNode ->
                ct.ThrowIfCancellationRequested()
                match boundNode with
                | :? BoundExpression as expr ->
                    match expr with
                    | BoundExpression.Let(BoundSyntaxInfo.User _, _, _, ((BoundExpression.None _) as noneExpr)) ->
                        let column = expr.Syntax.GetTextRange(ct).Start.Column
                        if offside >= column then
                            match noneExpr.TryEnvironment with
                            | Some benv ->
                                data.Add(boundNode, benv)
                            | _ ->
                                ()
                    | _ ->
                        ()
                | _ ->
                    ()
            )
        )

        data
        |> Seq.sortBy (fun (boundNode, _) ->
            boundNode.Syntax.TextSpan.Width
        )
        |> Seq.tryHead
        |> Option.map (fun (boundNode, benv) -> OlyBoundSubModel(this, boundNode, benv))

    member internal this.GetBoundTree(ct: CancellationToken): BoundTree =
        let result = getResult ct
        result.BoundTree

    member this.GetSymbols(node: OlySyntaxNode, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        getSymbols node (fun _ -> true) nodeHas true ct

    member this.GetSymbolsByPossibleName(node: OlySyntaxNode, possibleName: string, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        getSymbols node (fun x -> x.Name.Contains(possibleName)) nodeHas true ct

    member this.TryFindSymbol(token: OlyToken, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        let symbols = getSymbols token.Node (fun _ -> true) (fun x y -> nodeHas y x) false ct
        symbols
        |> Seq.tryExactlyOne

    /// Gets the semantic diagnostics.
    member this.GetDiagnostics(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        let result = getResult ct
        result.SemanticDiagnostics

    member _.SyntaxTree: OlySyntaxTree = syntaxTree

    member _.AssemblyIdentity = asm.Identity

    member internal _.TryFindExternalDefinition(identity: OlyILAssemblyIdentity, s: ISymbol, ct) : OlySourceLocation option = tryGetLocation(identity, s, ct)

    member internal _.TryFindDefinition(value: IValueSymbol, ct) =
        let declTable = getPartialDeclTable ct
        match declTable.ValueDeclarations.TryGetValue value.Formal with
        | true, location ->
            location
            |> Some
        | _ ->
            let boundTree = this.GetBoundTree(ct)
            let declTable = boundTree.DeclarationTable
            match declTable.ValueDeclarations.TryGetValue value.Formal with
            | true, location ->
                location
                |> Some
            | _ ->
                None

    member internal _.TryFindDefinition(ent: IEntitySymbol, ct) : OlySourceLocation option =
        let declTable = getPartialDeclTable ct
        match declTable.EntityDeclarations.TryGetValue ent.Formal with
        | true, location ->
            location
            |> Some
        | _ ->
            let boundTree = this.GetBoundTree(ct)
            let declTable = boundTree.DeclarationTable
            match declTable.EntityDeclarations.TryGetValue ent.Formal with
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
        | :? IEntitySymbol as ent ->           
            this.TryFindDefinition(ent, ct)
        | :? TypeParameterSymbol as tyPar ->
            this.TryFindDefinition(tyPar, ct)
        | :? IValueSymbol as value ->
            this.TryFindDefinition(value, ct)
        | _ ->
            None
