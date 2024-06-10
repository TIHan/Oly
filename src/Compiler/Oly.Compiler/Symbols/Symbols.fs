module internal rec Oly.Compiler.Internal.Symbols

open System
open System.Diagnostics
open Oly.Core
open System.Collections.Immutable
open System.Collections.Generic
open System.Collections.ObjectModel
open Oly.Metadata

let newId =
    let i = ref 64L // We do not start with 0 because we leave 0-63 for built-in types.
    fun () -> System.Threading.Interlocked.Increment i

type ISymbol = interface end

[<Literal>]
let AnonymousEntityName = ""

[<Literal>]
let EntryPointName = "main"

let mkVariableSolution() = VariableSolutionSymbol(false, false)
let mkVariableType name = TypeSymbol.Variable(name)
let mkInferenceVariableType tyParOpt = 
    match tyParOpt with
    | Some (tyPar: TypeParameterSymbol) when tyPar.HasArity ->
        TypeSymbol.CreateInferenceVariable(tyParOpt, VariableSolutionSymbol(false, true))
    | _ ->
        TypeSymbol.CreateInferenceVariable(tyParOpt, mkVariableSolution())
let mkHigherInferenceVariableType tyParOpt (tyArgs: TypeSymbol imarray) = 
    TypeSymbol.CreateHigherInferenceVariable(tyParOpt, tyArgs, VariableSolutionSymbol(false, true), mkVariableSolution())
let mkSolvedInferenceVariableType (tyPar: TypeParameterSymbol) (ty: TypeSymbol) =
#if DEBUG || CHECKED
    if not ty.IsError_t then
        if not tyPar.HasArity && ty.IsTypeConstructor then
            failwith "Unexpected type constructor."
        if tyPar.HasArity && not ty.IsTypeConstructor then
            failwith "Expected type constructor."
#endif
    let varSolution = VariableSolutionSymbol(false, tyPar.HasArity)
    varSolution.Solution <- ty
    TypeSymbol.CreateInferenceVariable(Some tyPar, varSolution)

let mkSolvedHigherInferenceVariableType tyPar tyArgs ty = 
    let varSolution = mkVariableSolution()
    varSolution.Solution <- ty
    TypeSymbol.CreateHigherInferenceVariable(Some tyPar, tyArgs, varSolution, varSolution)

let mkInferenceVariableTypeOfParameter () = TypeSymbol.CreateInferenceVariable(None, VariableSolutionSymbol(true, false))

[<System.Flags>]
type AttributeFlags =
    | AllowOnFunction =     0x00001
    | AllowOnType =         0x00010
    | AllowOnConstructor =  0x00100
    | AllowOnAll =          0x00111

// TODO: Introduce an AttributeExpression instead of just using ConstantSymbol
[<RequireQualifiedAccess>]
type AttributeNamedArgumentSymbol =
    | Property of IPropertySymbol * value: ConstantSymbol
    | Field of IFieldSymbol * value: ConstantSymbol

[<RequireQualifiedAccess>]
type InlineArgumentSymbol =
    | None
    | Never
    | Always

    member this.ToFunctionFlags() =
        match this with
        | InlineArgumentSymbol.None ->
            FunctionFlags.Inline
        | InlineArgumentSymbol.Never ->
            FunctionFlags.InlineNever
        | InlineArgumentSymbol.Always ->
            FunctionFlags.InlineAlways

[<RequireQualifiedAccess>]
type UnmanagedArgumentSymbol =
    | AllocationOnly

    member this.ToFunctionFlags() =
        match this with
        | UnmanagedArgumentSymbol.AllocationOnly ->
            FunctionFlags.UnmanagedAllocationOnly

[<RequireQualifiedAccess>]
type AttributeSymbol =
    | Open
    | Null
    | Pure
    | Import of platform: string * path: ImmutableArray<string> * name: string
    | Export
    | Blittable
    | Intrinsic of name: string
    | Inline of InlineArgumentSymbol
    | Unmanaged of UnmanagedArgumentSymbol
    | Constructor of ctor: IFunctionSymbol * args: ConstantSymbol imarray * namedArgs: AttributeNamedArgumentSymbol imarray * flags: AttributeFlags

    member this.Flags =
        match this with
        | Open
        | Null ->
            AttributeFlags.AllowOnType
        | Import _ 
        | Export
        | Intrinsic _ 
        | Blittable 
        | Pure ->
            AttributeFlags.AllowOnFunction ||| AttributeFlags.AllowOnType
        | Inline _
        | Unmanaged _ ->
            AttributeFlags.AllowOnFunction
        | Constructor(_, _, _, flags) ->
            flags

let attributesContainImport (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.exists (function AttributeSymbol.Import _ -> true | _ -> false)

let attributesContainExport (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.exists (function AttributeSymbol.Export -> true | _ -> false)

let attributesContainIntrinsic (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.exists (function AttributeSymbol.Intrinsic _ -> true | _ -> false)

let attributesContainPure (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.exists (function AttributeSymbol.Pure -> true | _ -> false)

let attributesContainOpen (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.exists (function AttributeSymbol.Open -> true | _ -> false)

let attributesContainBlittable (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.exists (function AttributeSymbol.Blittable -> true | _ -> false)

let attributesContainNull (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.exists (function AttributeSymbol.Null -> true | _ -> false)

let tryAttributesInlineArgument (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.tryPick (fun attr ->
        match attr with
        | AttributeSymbol.Inline(inlineArg) ->
            Some(inlineArg)
        | _ ->
            None
    )

let tryAttributesInlineFlags (attrs: AttributeSymbol imarray) =
    tryAttributesInlineArgument attrs
    |> Option.map (fun x -> x.ToFunctionFlags())

let tryAttributesUnmanagedArgument (attrs: AttributeSymbol imarray) =
    attrs
    |> ImArray.tryPick (fun attr ->
        match attr with
        | AttributeSymbol.Unmanaged(unmanagedArg) ->
            Some(unmanagedArg)
        | _ ->
            None
    )

let tryAttributesUnmanagedFlags (attrs: AttributeSymbol imarray) =
    tryAttributesUnmanagedArgument attrs
    |> Option.map (fun x -> x.ToFunctionFlags())

[<Flags>]
type EntityFlags =
    | None              = 0x000000000L

    | Public            = 0x000000000L
    | Internal          = 0x000000001L
    | Private           = 0x000000002L
    | AccessorMask      = 0x000000007L

    | ReadOnly          = 0x000000010L
    | Abstract          = 0x000000100L
    | Final             = 0x000001000L
    | Intrinsic         = 0x000010000L
    | AutoOpen          = 0x000100000L
    | Nullable          = 0x001000000L

    | Scoped            = 0x010000000L

    | Invalid           = 0x100000000L

/// These flags are not persisted in IL metadata.
/// The only use is to cache information so that the computation for that information is done once (usually).
/// (usually) means that it technically could happen multiple times on different threads since there will be no locks.
[<Flags>]
type EntityCachedFlags =
    | Empty         = 0b00000

    // If it is not unmanaged, then it is not blittable.
    | NotUnmanaged  = 0b00011
    | NotBlittable  = 0b00010

    // If it is blittable, then it is unmanaged.
    | Unmanaged     = 0b01000
    | Blittable     = 0b11000

[<Struct;RequireQualifiedAccess>]
type AssemblySymbol = 
    | IL of OlyILAssemblyIdentity with

    member this.Name =
        match this with
        | IL(identity) -> identity.Name

    member this.Identity =
        match this with
        | IL(identity) -> identity

/// An entity is effectively equivalent to a named type.
/// A namespace is also represented as an EntitySymbol and can be promoted to a type in certain cases.
[<AbstractClass>]
[<DebuggerDisplay("{Name}")>]
type EntitySymbol() =

    let mutable qualifiedName: string = null

    member val Id: int64 = newId()

    abstract Kind : EntityKind

    abstract Enclosing : EnclosingSymbol

    abstract ContainingAssembly : AssemblySymbol option
    
    abstract Name : string

    abstract TypeParameters : ImmutableArray<TypeParameterSymbol>

    abstract TypeArguments : ImmutableArray<TypeSymbol>

    abstract Entities : EntitySymbol imarray

    abstract Fields : IFieldSymbol imarray

    abstract Functions : IFunctionSymbol imarray

    abstract InstanceConstructors : IFunctionSymbol imarray

    abstract Properties : IPropertySymbol imarray

    abstract Patterns : IPatternSymbol imarray

    abstract Extends : TypeSymbol imarray

    abstract Implements : TypeSymbol imarray

    abstract Formal : EntitySymbol

    abstract Attributes : AttributeSymbol imarray

    abstract Flags : EntityFlags

    member this.TryEnumUnderlyingType =
        if (this.IsEnum) then
            if this.Fields.Length > 0 then
                let field = this.Fields[0]
                if field.IsInstance then
                    Some field.Type
                else
                    None
            else
                None
        else
            None

    /// Mutability
    /// Do not use directly! Use the extension member 'IsUnmanaged' and `IsBlittable`.
    /// This is just meant for storage.
    member val __CachedFlags : EntityCachedFlags = EntityCachedFlags.Empty with get, set 
    
    // TODO/REVIEW: This is weird, but it is trying to create a unique qualified name.
    //              It would be preferrable to have a qualified name that is consistent with the syntax.
    member this.QualifiedName =
        if isNull qualifiedName then
            qualifiedName <-
                let rec loop enclosing : string imarray =
                    match enclosing with
                    | EnclosingSymbol.Entity(ent) when ent.IsNamespace ->
                        (loop ent.Enclosing).Add(ent.Name)
                    | EnclosingSymbol.Entity(ent) ->
                        let ilTyParCount = ent.TypeParameters.Length - ent.Enclosing.TypeParameters.Length
                        let name =
                            if ilTyParCount = 0 then
                                ent.Name
                            else
                                ent.Name + "````" + ilTyParCount.ToString()                               
                        (loop ent.Enclosing).Add(name).Add("::")
                    | _ ->
                        ImArray.empty

                if this.IsNamespace then
                    (loop this.Enclosing).Add(this.Name)
                    |> String.concat "."
                else
                    let ilTyParCount = this.TypeParameters.Length - this.Enclosing.TypeParameters.Length
                    let name =
                        if ilTyParCount = 0 then
                            this.Name
                        else
                            this.Name + "````" + ilTyParCount.ToString()  
                    (loop this.Enclosing).Add(name)
                    |> String.concat "."
        qualifiedName

    override this.ToString() = this.Name

    interface ISymbol

[<Sealed;DebuggerDisplay("{DebugName}")>]
type EntityDefinitionSymbol(containingAsmOpt, enclosing, attrs: _ imarray ref, name, flags, kind, tyPars: _ imarray ref, funcs: FunctionSymbol imarray ref, fields: _ imarray ref, props: PropertySymbol imarray ref, pats: PatternSymbol imarray ref, extends: _ imarray ref, implements: _ imarray ref, entsHole: ResizeArray<EntitySymbol>) =
    inherit EntitySymbol()

    // REVIEW: The first time this gets evaluated, we must already have the fields set.
    //         Could we make this less error prone? We haven't had a problem with it yet but it can be surprising.
    let lazyFlags =
        lazy
            let fields = fields.contents
            if ImArray.isEmpty fields then flags
            else
                let isReadOnly =
                    fields
                    |> Seq.forall (fun (x: IFieldSymbol) -> not x.IsMutable)
                let computedFlags =
                    if isReadOnly then
                        EntityFlags.ReadOnly
                    else
                        EntityFlags.None
                computedFlags ||| flags

    let mutable ents = ImArray.empty

    member _.DebugName: string = name

    // Mutability
    member _.ClearEntities(pass: CompilerPass) =
        if pass <> CompilerPass.Pass0 then
            OlyAssert.Fail($"ClearEntities - Invalid Pass {pass}")
        ents <- ImArray.empty

    override _.Entities = 
        if ents.Length <> entsHole.Count then
            ents <- entsHole |> ImArray.ofSeq
        ents

    member _.FunctionDefinitions = funcs.contents

    override _.Functions = funcs.contents |> ImArray.map (fun x -> x :> IFunctionSymbol)
    override _.InstanceConstructors =
        funcs.contents
        |> ImArray.filter (fun func -> func.IsInstance && func.IsConstructor)
        |> ImArray.map (fun x -> x :> IFunctionSymbol)
    override _.Enclosing = enclosing
    override _.Name = name 
    override _.Fields = fields.contents
    override _.Properties = props.contents |> ImArray.map (fun x -> x :> IPropertySymbol)
    override _.Patterns = pats.contents |> ImArray.map (fun x -> x :> IPatternSymbol)
    override _.TypeArguments = tyPars.contents |> ImArray.map (fun (x: TypeParameterSymbol) -> x.AsType)
    override _.Implements = implements.contents
    override _.Extends = extends.contents
    override _.TypeParameters = tyPars.contents
    override _.Kind = kind
    override _.ContainingAssembly = containingAsmOpt
    override this.Formal = this
    override _.Attributes = attrs.contents
    override _.Flags = lazyFlags.Value

let applyType (ty: TypeSymbol) (tyArgs: ImmutableArray<TypeSymbol>) =
    if ty.IsError_t then ty
    else

    OlyAssert.True(ty.IsFormal)
    OlyAssert.Equal(ty.Arity, tyArgs.Length)

    if tyArgs.IsEmpty then ty
    else

    match ty with
    | TypeSymbol.ForAll(tyPars, innerTy) -> 
        OlyAssert.False(innerTy.IsFormal)

        let tyArgs =
            (tyArgs, tyPars)
            ||> ImArray.map2 (fun tyArg tyPar ->
                mkSolvedInferenceVariableType tyPar tyArg
            )
        substituteType tyArgs innerTy

    | TypeSymbol.Entity(ent) -> 
        OlyAssert.True(ent.IsTypeConstructor)
        (applyEntity tyArgs ent).AsType

    | TypeSymbol.Variable(tyPar) when tyPar.Arity > 0 ->
        TypeSymbol.HigherVariable(tyPar, tyArgs)

    | TypeSymbol.InferenceVariable(tyParOpt, solution) ->
        TypeSymbol.CreateHigherInferenceVariable(tyParOpt, tyArgs, solution, VariableSolutionSymbol(false, false))

    | TypeSymbol.ByRef(_, kind) ->
        if tyArgs.Length <> 1 then
            failwith "Expected only one type instantiation."
        TypeSymbol.CreateByRef(tyArgs.[0], kind)

    | TypeSymbol.Function(kind=kind) ->
        OlyAssert.Equal(2, tyArgs.Length)
        TypeSymbol.Function(VariadicTypeArguments(tyArgs[0].AsParameters(), ImArray.empty), tyArgs[1], kind)

    | TypeSymbol.NativeFunctionPtr(ilCallConv, _, _) ->
        OlyAssert.Equal(2, tyArgs.Length)
        TypeSymbol.NativeFunctionPtr(ilCallConv, VariadicTypeArguments(tyArgs[0].AsParameters(), ImArray.empty), tyArgs[1])

    | TypeSymbol.NativePtr _ ->
        TypeSymbol.NativePtr(tyArgs[0])

    | TypeSymbol.Tuple _ ->
        OlyAssert.True(tyArgs.Length > 0)
        TypeSymbol.CreateTuple(tyArgs)

    | TypeSymbol.Array(_, rank, kind) ->
        TypeSymbol.Array(tyArgs[0], rank, kind)
        
    | _ ->
        failwith "Unexpected type application"

let actualType (tyArgs: TypeArgumentSymbol imarray) (ty: TypeSymbol) =
    // TODO: This could be a little more memory efficient by not constructing new type symbols if they didn't change.
    let rec instTy ty =
        match stripTypeEquations ty with
        | TypeSymbol.Variable(tyPar) -> 
            tyArgs.[tyPar.Index]

        | TypeSymbol.HigherVariable(tyPar, tyArgs2) ->
            let tyArgs3 = tyArgs2 |> ImArray.map instTy
            applyType tyArgs.[tyPar.Index].Formal tyArgs3

        | TypeSymbol.Function(inputTys, returnTy, kind) ->
            let inputTys =
                match inputTys with
                | VariadicTypeArguments(tys, _) ->
                    VariadicTypeArguments(tys |> ImArray.map instTy, ImArray.empty)
            TypeSymbol.Function(inputTys, instTy returnTy, kind)

        | TypeSymbol.NativeFunctionPtr(ilCallConv, inputTys, returnTy) ->
            let inputTys =
                match inputTys with
                | VariadicTypeArguments(tys, _) ->
                    VariadicTypeArguments(tys |> ImArray.map instTy, ImArray.empty)
            TypeSymbol.NativeFunctionPtr(ilCallConv, inputTys, instTy returnTy)

        | TypeSymbol.ForAll(tyPars, innerTy) ->
            tyPars
            |> ImArray.iter (fun tyPar ->
                let exists =
                    tyArgs 
                    |> ImArray.exists (fun x -> 
                        match (x.TryImmedateTypeParameter: TypeParameterSymbol voption) with 
                        | ValueSome x -> x.Id = tyPar.Id 
                        | _ -> false
                    )
                if exists then
                    failwith "ForAll type must have all of its type parameters substituted."
            )
            instTy innerTy

        | TypeSymbol.Tuple(elementTys, names) ->
            TypeSymbol.Tuple(elementTys |> ImArray.map instTy, names)

        | TypeSymbol.Entity(ent) ->
            let tyArgs = 
                (ent.TypeParameters, ent.TypeArguments)
                ||> ImArray.map2 (fun tyPar tyArg ->
                    if tyPar.HasArity && not tyArg.IsTypeVariable then
                        tyArg
                    else
                        instTy tyArg
                )
            TypeSymbol.Entity(applyEntity tyArgs ent.Formal)

        | TypeSymbol.RefCell(innerTy) ->
            TypeSymbol.RefCell(instTy innerTy)

        | TypeSymbol.Array(elementTy, rank, kind) ->
            TypeSymbol.Array(instTy elementTy, rank, kind)

        | TypeSymbol.ByRef(innerTy, kind) ->
            TypeSymbol.CreateByRef(instTy innerTy, kind)

        | TypeSymbol.NativePtr(elementTy) ->
            TypeSymbol.NativePtr(instTy elementTy)

        | TypeSymbol.DependentIndexer(inputValueTy, innerTy) ->
            TypeSymbol.DependentIndexer(instTy inputValueTy, instTy innerTy)

        | _ ->
            ty
    instTy ty

let actualTypes (tyArgs: TypeArgumentSymbol imarray) (tys: TypeSymbol seq) =
    tys
    |> Seq.map (actualType tyArgs)
    |> ImmutableArray.CreateRange

let applyEnclosing (tyArgs: TypeArgumentSymbol imarray) (enclosing: EnclosingSymbol) =
    match enclosing with
    | EnclosingSymbol.Local
    | EnclosingSymbol.RootNamespace -> enclosing
    | EnclosingSymbol.Entity(ent) ->
        EnclosingSymbol.Entity(applyEntity tyArgs ent)
    | EnclosingSymbol.Witness(n, tr) ->
        EnclosingSymbol.Witness(n, applyEntity tyArgs tr)

let actualEntities (tyArgs: TypeArgumentSymbol imarray) (ents: EntitySymbol imarray) =
    ents
    |> ImArray.map (fun x ->
        let tyArgs2 = actualTypes tyArgs x.TypeArguments
        (applyEntity tyArgs2 x)
    )

let private filterForTypeParameterVariableSolution (tyPar: TypeParameterSymbol) (varSolution: VariableSolutionSymbol) =
    (not varSolution.HasSolution) || (not varSolution.Solution.IsTypeConstructor) || (varSolution.Solution.Arity = tyPar.Arity)

let filterForTypeParameters (tys: TypeSymbol seq) =
    tys
    |> Seq.choose (fun x ->
        match x with
        | TypeSymbol.Variable(tyPar) -> KeyValuePair(tyPar.Id, x) |> Some
        | TypeSymbol.HigherVariable(tyPar, tyArgs) when tyPar.Arity = tyArgs.Length -> KeyValuePair(tyPar.Id, x) |> Some

        | TypeSymbol.InferenceVariable(Some tyPar, varSolution) -> 
            if filterForTypeParameterVariableSolution tyPar varSolution then
                KeyValuePair(tyPar.Id, x) |> Some
            else
                None
        | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, varExternalSolution, varSolution) when tyPar.Arity = tyArgs.Length -> 
            if filterForTypeParameterVariableSolution tyPar varExternalSolution && filterForTypeParameterVariableSolution tyPar varSolution then
                KeyValuePair(tyPar.Id, x) |> Some
            else
                None
        | _ -> 
            None
    )
    |> Seq.distinctBy (fun pair -> pair.Key)

let substituteEntity (tyArgs: TypeArgumentSymbol imarray) (ent: EntitySymbol) =
    let tys =
        tyArgs
        |> filterForTypeParameters
        |> Dictionary
        |> ReadOnlyDictionary
    tryActualEntity tys ent

let substituteType (tyArgs: TypeArgumentSymbol imarray) (ty: TypeSymbol) =
    let tys =
        tyArgs
        |> filterForTypeParameters
        |> Dictionary
        |> ReadOnlyDictionary
    tryActualType tys ty

let substituteTypes (tyArgs: TypeArgumentSymbol imarray) (tys: TypeSymbol imarray) =
    tys
    |> ImArray.map (substituteType tyArgs)

[<Sealed;DebuggerDisplay("Applied({DebugName})")>]
type AppliedEntitySymbol(tyArgs: TypeArgumentSymbol imarray, ent: EntitySymbol) =
    inherit EntitySymbol()

    do
        OlyAssert.True(ent.IsFormal)
        OlyAssert.True(ent.IsTypeConstructor)
        OlyAssert.False(ent.TypeParameters.IsEmpty)
        OlyAssert.False(tyArgs.IsEmpty)
        OlyAssert.Equal(ent.TypeParameters.Length, tyArgs.Length)

    let tyArgs =
        // We have to do this to associate the entity's type parameter with passed type argument to apply.
        (ent.TypeParameters, tyArgs)
        ||> ImArray.map2 (fun tyPar tyArg ->
            mkSolvedInferenceVariableType tyPar tyArg
        )

    [<VolatileField>]
    let mutable appliedFuncs = ValueNone
    [<VolatileField>]
    let mutable appliedInstanceCtors = ValueNone
    [<VolatileField>]
    let mutable appliedFields = ValueNone
    [<VolatileField>]
    let mutable appliedImplements = ValueNone
    [<VolatileField>]
    let mutable appliedExtends = ValueNone
    [<VolatileField>]
    let mutable appliedEntities = ValueNone
    [<VolatileField>]
    let mutable appliedProps = ValueNone
    [<VolatileField>]
    let mutable appliedPats = ValueNone

    // TODO: Fix me now!
    let enclosing = applyEnclosing (tyArgs |> ImArray.take ent.Enclosing.TypeParameterCount) ent.Enclosing

    member _.DebugName = ent.Name

    override this.Entities =
        match appliedEntities with
        | ValueNone ->
            if ent.Entities.IsEmpty then ImArray.empty
            else
                let tyArgs = tyArgs
                let ents =
                    ent.Entities
                    |> ImArray.map (fun x -> substituteEntity tyArgs x)
                appliedEntities <- ValueSome ents
                ents
        | ValueSome ents ->
            ents
    override _.Enclosing = enclosing
    override _.TypeParameters = ent.TypeParameters
    override _.ContainingAssembly = ent.ContainingAssembly

    override this.Fields =
        match appliedFields with
        | ValueNone ->
            if ent.Fields.IsEmpty then ImArray.empty
            else
                let fields = 
                    let enclosing = this.AsEnclosing
                    ent.Fields 
                    |> ImArray.map (fun x -> actualField enclosing tyArgs x)
                appliedFields <- ValueSome fields
                fields
        | ValueSome fields ->
            fields

    override this.Properties =
        match appliedProps with
        | ValueNone ->
            if ent.Properties.IsEmpty then ImArray.empty
            else
                let props = 
                    let enclosing = this.AsEnclosing
                    ent.Properties
                    |> ImArray.map (fun x -> actualProperty enclosing tyArgs x)
                appliedProps <- ValueSome props
                props
        | ValueSome props ->
            props

    override this.Patterns =
        match appliedPats with
        | ValueNone ->
            if ent.Properties.IsEmpty then ImArray.empty
            else
                let pats = 
                    let enclosing = this.AsEnclosing
                    ent.Patterns
                    |> ImArray.map (fun x -> actualPattern enclosing tyArgs x)
                appliedPats <- ValueSome pats
                pats
        | ValueSome pats ->
            pats

    override this.Functions =
        match appliedFuncs with
        | ValueNone ->
            if ent.Functions.IsEmpty then ImArray.empty
            else
                let funcs = 
                    let enclosing = this.AsEnclosing: EnclosingSymbol
                    ent.Functions
                    |> ImArray.map (fun x -> 
                        let tyArgs2 = 
                            if x.IsConstructor then
                                enclosing.TypeArguments
                            else
                                enclosing.TypeArguments.AddRange(x.TypeArguments)
                        actualFunction enclosing tyArgs2 x)
                appliedFuncs <- ValueSome funcs
                funcs
        | ValueSome(funcs) ->
            funcs

    override this.InstanceConstructors =
        match appliedInstanceCtors with
        | ValueNone ->
            let funcs = (this :> EntitySymbol).Functions
            if funcs.IsEmpty then ImArray.empty
            else
                let instanceCtors =
                    funcs
                    |> ImArray.filter (fun func -> func.IsInstance && func.IsConstructor)
                appliedInstanceCtors <- ValueSome instanceCtors
                instanceCtors
        | ValueSome(instanceCtors) ->
            instanceCtors

    override _.Implements = 
        match appliedImplements with
        | ValueNone ->
            if ent.Implements.IsEmpty then ImArray.empty
            else
                let implements = actualTypes tyArgs ent.Implements
                appliedImplements <- ValueSome implements
                implements
        | ValueSome(implements) ->
            implements

    override _.Extends =
        match appliedExtends with
        | ValueNone ->
            if ent.Extends.IsEmpty then ImArray.empty
            else
                let inherits = actualTypes tyArgs ent.Extends
                appliedExtends <- ValueSome inherits
                inherits
        | ValueSome(inherits) ->
            inherits

    override _.Name = ent.Name
    override _.TypeArguments = tyArgs
    override _.Kind = ent.Kind
    override _.Formal = ent.Formal
    override _.Attributes = ent.Attributes
    override _.Flags = ent.Flags

let applyEntity (tyArgs: TypeArgumentSymbol imarray) (ent: EntitySymbol) : EntitySymbol =
    OlyAssert.True(ent.IsFormal)
    OlyAssert.Equal(ent.TypeParameters.Length, tyArgs.Length)

    if tyArgs.IsEmpty then 
        ent
    else 
        OlyAssert.True(ent.IsTypeConstructor)
        AppliedEntitySymbol(tyArgs, ent)

let actualEntity (tyArgs: TypeArgumentSymbol imarray) (ent: EntitySymbol) =
    OlyAssert.True(ent.IsFormal)

    let tyArgs2 = 
        (ent.TypeParameters, ent.TypeArguments)
        ||> ImArray.map2 (fun tyPar tyArg -> 
            if tyPar.HasArity then
                if tyArg.IsTypeVariable then
                    tyArg
                else
                    let tyArg2 = tyArgs[tyPar.Index]
                    if tyArg2.IsTypeConstructor then
                        tyArg2
                    else
                        tyArg

            else
                actualType tyArgs tyArg
        )
    applyEntity tyArgs2 ent

let actualParameter (tyArgs: TypeArgumentSymbol imarray) (par: ILocalParameterSymbol) =
    actualValue par.Enclosing tyArgs par :?> ILocalParameterSymbol

let actualParameters (tyArgs: TypeArgumentSymbol imarray) (pars: ILocalParameterSymbol imarray) =
    pars
    |> Seq.map (fun par -> actualParameter tyArgs par)
    |> ImmutableArray.CreateRange

[<Sealed;DebuggerDisplay("Actual({DebugName})")>]
type ActualFunctionSymbol(enclosing: EnclosingSymbol, tyArgs: TypeArgumentSymbol imarray, func: IFunctionSymbol) =
    let id = newId ()
    
    let pars =
        lazy 
            let pars = actualParameters tyArgs (func.Formal :?> IFunctionSymbol).Parameters
#if DEBUG || CHECKED
            pars
            |> ImArray.iter (fun par ->
                if not par.Type.IsError_t && par.Type.IsTypeConstructor then
                    failwith "Unexpected type constructor"
            )
#endif
            pars
    
    let returnTy = actualType tyArgs (func.Formal :?> IFunctionSymbol).ReturnType

#if DEBUG || CHECKED
    do
        if not returnTy.IsError_t && returnTy.IsTypeConstructor then
            failwith "Unexpected type constructor"
#endif
    
    let ty =
        lazy
            let argTys = pars.Value |> ImArray.map (fun x -> x.Type)
            TypeSymbol.CreateFunction(ImArray.empty (* TODO: This may not be right, we might want to pass typars. *), argTys, returnTy, FunctionKind.Normal)
    
    let funcTyArgs = actualTypes tyArgs func.Formal.TypeArguments

    member _.DebugName = func.Name

    interface IFunctionSymbol with
        
        member _.Id = id
        
        member _.Enclosing = enclosing
        
        member _.Name = func.Name
        
        member _.TypeParameters = func.TypeParameters          
        
        member _.TypeArguments = funcTyArgs
        
        member _.Parameters = pars.Value
        
        member _.ReturnType = returnTy
        
        member _.Type = ty.Value
        
        member _.FunctionFlags = func.FunctionFlags
        
        member _.FunctionOverrides = func.FunctionOverrides
        
        member _.IsProperty = false

        member _.IsPattern = false
        
        member _.IsFunction = true
        
        member _.IsField = false
        
        member _.Formal = func.Formal
        
        member _.Attributes = func.Attributes
        
        member _.ValueFlags = func.ValueFlags
        
        member _.MemberFlags = func.MemberFlags
        
        member _.Semantic = func.Semantic
        
        member _.IsThis = func.IsThis
        
        member _.IsBase = func.IsBase

        member _.WellKnownFunction = func.WellKnownFunction

        member _.AssociatedFormalPattern = func.AssociatedFormalPattern
        

let actualFunction (enclosing: EnclosingSymbol) (tyArgs: TypeArgumentSymbol imarray) (func: IFunctionSymbol) =
    ActualFunctionSymbol(enclosing, tyArgs, func) :> IFunctionSymbol

let actualField enclosing (tyArgs: TypeArgumentSymbol imarray) (field: IFieldSymbol) =
    let ty = actualType tyArgs field.Type
    let id = newId ()
    { new IFieldSymbol with
        member _.Id = id
        member _.Name = field.Name
        member _.Type = ty
        member _.Enclosing = enclosing
        member _.Formal = field.Formal
        member _.IsField = field.IsField

        member _.FunctionFlags = field.FunctionFlags
        member _.FunctionOverrides = field.FunctionOverrides
        member _.IsProperty = false
        member _.IsPattern = false
        member _.IsFunction = false
        member _.TypeParameters = field.TypeParameters
        member _.TypeArguments = field.TypeArguments
        member _.ValueFlags = field.ValueFlags
        member _.Attributes = field.Attributes
        member _.MemberFlags = field.MemberFlags
        member _.IsThis = field.IsThis
        member _.IsBase = field.IsBase
        member _.Constant = field.Constant
        member _.AssociatedFormalPropertyId = field.AssociatedFormalPropertyId
    }

let tryActualProperty enclosing (tys: IReadOnlyDictionary<int64, TypeSymbol>) (prop: IPropertySymbol) : IPropertySymbol =
    let ty = tryActualType tys prop.Type
    let id = newId ()

    let mutable actualGetter = ValueNone
    let mutable actualSetter = ValueNone
    let mutable fieldCache = ValueNone
    let mutable enclosingCache = ValueNone

    { new IPropertySymbol with
        member _.Id = id
        member _.Name = prop.Name
        member _.Type = ty
        member _.Enclosing = enclosing
        member _.Formal = prop.Formal
        member _.IsField = prop.IsField

        member _.FunctionFlags = prop.FunctionFlags
        member _.FunctionOverrides = prop.FunctionOverrides
        member _.IsProperty = prop.IsProperty
        member _.IsPattern = false
        member _.IsFunction = prop.IsFunction
        member _.TypeParameters = prop.TypeParameters
        member _.TypeArguments = prop.TypeArguments
        member _.ValueFlags = prop.ValueFlags
        member _.Attributes = prop.Attributes
        member _.MemberFlags = prop.MemberFlags
        member _.IsThis = prop.IsThis
        member _.IsBase = prop.IsBase

        member _.BackingField =
            match fieldCache with
            | ValueSome result -> result
            | _ ->
                match prop.BackingField with
                | Some(backingField) ->
                    let newBackingField = tryActualField enclosing tys backingField |> Some
                    fieldCache <- ValueSome newBackingField
                    newBackingField
                | _ ->
                    fieldCache <- ValueSome None
                    None

        member _.Getter =
            match actualGetter with
            | ValueSome result -> result
            | _ ->
                match prop.Getter with
                | Some(getter) ->
                    let newGetter = tryActualFunction enclosing tys getter |> Some
                    actualGetter <- ValueSome newGetter
                    newGetter
                | _ ->
                    actualGetter <- ValueSome None
                    None

        member _.Setter =
            match actualSetter with
            | ValueSome result -> result
            | _ ->
                match prop.Setter with
                | Some(setter) ->
                    let newSetter = tryActualFunction enclosing tys setter |> Some
                    actualSetter <- ValueSome newSetter
                    newSetter
                | _ ->
                    actualSetter <- ValueSome None
                    None
    }

let actualProperty enclosing (tyArgs: TypeArgumentSymbol imarray) (prop: IPropertySymbol) : IPropertySymbol =
    let ty = actualType tyArgs prop.Type
    let id = newId ()

    let mutable actualGetter = ValueNone
    let mutable actualSetter = ValueNone
    let mutable fieldCache = ValueNone

    { new IPropertySymbol with
        member _.Id = id
        member _.Name = prop.Name
        member _.Type = ty
        member _.Enclosing = enclosing
        member _.Formal = prop.Formal
        member _.IsField = prop.IsField

        member _.FunctionFlags = prop.FunctionFlags
        member _.FunctionOverrides = prop.FunctionOverrides
        member _.IsProperty = prop.IsProperty
        member _.IsFunction = prop.IsFunction
        member _.IsPattern = false
        member _.TypeParameters = prop.TypeParameters
        member _.TypeArguments = prop.TypeArguments
        member _.ValueFlags = prop.ValueFlags
        member _.Attributes = prop.Attributes
        member _.MemberFlags = prop.MemberFlags
        member _.IsThis = prop.IsThis
        member _.IsBase = prop.IsBase

        member _.BackingField =
            match fieldCache with
            | ValueSome result -> result
            | _ ->
                match prop.BackingField with
                | Some(backingField) ->
                    let newBackingField = actualField enclosing tyArgs backingField |> Some
                    fieldCache <- ValueSome newBackingField
                    newBackingField
                | _ ->
                    fieldCache <- ValueSome None
                    None

        member _.Getter =
            match actualGetter with
            | ValueSome result -> result
            | _ ->
                match prop.Getter with
                | Some(getter) ->
                    let newGetter = actualFunction enclosing tyArgs getter |> Some
                    actualGetter <- ValueSome newGetter
                    newGetter
                | _ ->
                    actualGetter <- ValueSome None
                    None

        member _.Setter =
            match actualSetter with
            | ValueSome result -> result
            | _ ->
                match prop.Setter with
                | Some(setter) ->
                    let newSetter = actualFunction enclosing tyArgs setter |> Some
                    actualSetter <- ValueSome newSetter
                    newSetter
                | _ ->
                    actualSetter <- ValueSome None
                    None
    }

let actualPattern enclosing (tyArgs: TypeArgumentSymbol imarray) (pat: IPatternSymbol) : IPatternSymbol =
    let func = actualFunction enclosing tyArgs pat.PatternFunction
    let guardOpt = pat.PatternGuardFunction |> Option.map (fun x -> actualFunction enclosing tyArgs x)

    let id = newId ()

    { new IPatternSymbol with
        member _.Id = id
        member _.Name = pat.Name
        member _.Type = func.Type
        member _.Enclosing = enclosing
        member _.Formal = pat.Formal
        member _.IsField = pat.IsField

        member _.FunctionFlags = pat.FunctionFlags
        member _.FunctionOverrides = pat.FunctionOverrides
        member _.IsProperty = pat.IsProperty
        member _.IsFunction = pat.IsFunction
        member _.IsPattern = true
        member _.TypeParameters = pat.TypeParameters
        member _.TypeArguments = pat.TypeArguments
        member _.ValueFlags = pat.ValueFlags
        member _.Attributes = pat.Attributes
        member _.MemberFlags = pat.MemberFlags
        member _.IsThis = pat.IsThis
        member _.IsBase = pat.IsBase

        member _.PatternFunction = func
        member _.PatternGuardFunction = guardOpt
    }

let actualValue (enclosing: EnclosingSymbol) (tyArgs: TypeArgumentSymbol imarray) (value: IValueSymbol) : IValueSymbol =
    if tyArgs.IsEmpty && value.TypeParameters.IsEmpty then
        value
    else

    match value with
    | :? LocalSymbol ->
        LocalSymbol(value.Name, actualType tyArgs value.Type, value.IsGenerated, value.IsMutable) :> IValueSymbol
    | :? LocalParameterSymbol as par ->
        LocalParameterSymbol(par.Attributes, value.Name, actualType tyArgs value.Type, par.IsThis, par.IsBase, par.IsMutable) :> IValueSymbol
    | :? IFunctionSymbol as func ->
        actualFunction enclosing tyArgs func :> IValueSymbol
    | :? IFieldSymbol as field ->
        actualField enclosing tyArgs field :> IValueSymbol
    | :? IPropertySymbol as prop ->
        actualProperty enclosing tyArgs prop :> IValueSymbol
    | _ ->
        if value.ValueFlags &&& ValueFlags.Invalid = ValueFlags.Invalid then
            value
        else
            failwith "Invalid value symbol."

let tryActualType (tys: IReadOnlyDictionary<int64, TypeSymbol>) (ty: TypeSymbol) =
    if tys.Count = 0 then ty
    else

    // TODO: This could be a little more memory efficient by not constructing new type symbols if they didn't change.
    let rec instTy ty =
        match stripTypeEquations ty with
        | TypeSymbol.Variable(tyPar) -> 
            match tys.TryGetValue(tyPar.Id) with
            | true, ty -> mkSolvedInferenceVariableType tyPar ty
            | _ -> ty

        | TypeSymbol.HigherVariable(tyPar, tyArgs2) ->
            let tyArgs3 = tyArgs2 |> ImArray.map instTy
            match tys.TryGetValue(tyPar.Id) with
            | true, ty -> applyType (mkSolvedInferenceVariableType tyPar ty) tyArgs3
            | _ -> TypeSymbol.HigherVariable(tyPar, tyArgs3)

        | TypeSymbol.Function(inputTys, returnTy, kind) ->
            let inputTys =
                match inputTys with
                | VariadicTypeArguments(tys, _) ->
                    VariadicTypeArguments(tys |> ImArray.map instTy, ImArray.empty)
            TypeSymbol.Function(inputTys, instTy returnTy, kind)

        | TypeSymbol.NativeFunctionPtr(ilCallConv, inputTys, returnTy) ->
            let inputTys =
                match inputTys with
                | VariadicTypeArguments(tys, _) ->
                    VariadicTypeArguments(tys |> ImArray.map instTy, ImArray.empty)
            TypeSymbol.NativeFunctionPtr(ilCallConv, inputTys, instTy returnTy)

        | TypeSymbol.ForAll(tyPars, innerTy) ->
            tyPars
            |> ImArray.iter (fun tyPar ->
                if tys.ContainsKey(tyPar.Id) |> not then
                    failwith "ForAll type must have all of its type parameters substituted."
            )
            instTy innerTy

        | TypeSymbol.Tuple(tyArgs, names) ->
            TypeSymbol.Tuple(tyArgs |> ImArray.map instTy, names)

        | TypeSymbol.Entity(ent) ->
            let tyArgs = 
                (ent.TypeParameters, ent.TypeArguments)
                ||> ImArray.map2 (fun tyPar tyArg ->
                    if tyPar.HasArity && not tyArg.IsTypeVariable then
                        tyArg
                    else
                        instTy tyArg
                )
            TypeSymbol.Entity(applyEntity tyArgs ent.Formal)

        | TypeSymbol.RefCell(innerTy) ->
            TypeSymbol.RefCell(instTy innerTy)

        | TypeSymbol.Array(elementTy, rank, kind) ->
            TypeSymbol.Array(instTy elementTy, rank, kind)

        | TypeSymbol.ByRef(innerTy, kind) ->
            TypeSymbol.CreateByRef(instTy innerTy, kind)

        | TypeSymbol.NativePtr(elementTy) ->
            TypeSymbol.NativePtr(instTy elementTy)

        | TypeSymbol.DependentIndexer(inputValueTy, innerTy) ->
            let inputValueTy = instTy inputValueTy
            let innerTy = instTy innerTy
            match tryComputeDependentType inputValueTy innerTy with
            | ValueSome(ty) -> ty
            | _ -> TypeSymbol.DependentIndexer(inputValueTy, innerTy)
        | _ -> ty
    instTy ty

let tryActualTypes tys (tys2: TypeSymbol seq) =
    tys2
    |> Seq.map (tryActualType tys)
    |> ImArray.ofSeq

let tryActualParameter tys (par: ILocalParameterSymbol) : ILocalParameterSymbol =
    tryActualValue par.Enclosing tys par :?> ILocalParameterSymbol

let tryActualParameters tys (pars: ImmutableArray<ILocalParameterSymbol>) =
    pars
    |> Seq.map (fun par -> tryActualParameter tys par)
    |> ImArray.ofSeq

let tryActualFunction (enclosing: EnclosingSymbol) tys (func: IFunctionSymbol) =
    let id = newId ()

    let pars =
        lazy tryActualParameters tys func.Parameters

    let returnTy = tryActualType tys func.ReturnType

    let funcTyArgs = tryActualTypes tys func.TypeArguments

    let funcTy =
        lazy
            let argTys = pars.Value |> ImArray.map (fun x -> x.Type)
            TypeSymbol.CreateFunction(ImArray.empty (* TODO: This may not be right, we might want to pass typars. *), argTys, returnTy, FunctionKind.Normal)

    { new IFunctionSymbol with

        member _.Id = id

        member _.Enclosing = enclosing

        member _.Name = func.Name

        member _.TypeParameters = func.TypeParameters // TODO: At some point, we need to apply trait type args here           

        member _.TypeArguments = funcTyArgs

        member _.Parameters = pars.Value

        member _.ReturnType = returnTy

        member _.Type = funcTy.Value

        member _.FunctionFlags = func.FunctionFlags // TODO: Do we need to change flags?

        member _.FunctionOverrides = func.FunctionOverrides

        member _.IsProperty = false

        member _.IsPattern = false

        member _.IsFunction = true

        member _.IsField = false

        member _.Formal = func.Formal

        member _.Attributes = func.Attributes

        member _.ValueFlags = func.ValueFlags

        member _.MemberFlags = func.MemberFlags

        member _.Semantic = func.Semantic

        member _.IsThis = func.IsThis

        member _.IsBase = func.IsBase

        member _.WellKnownFunction = func.WellKnownFunction

        member _.AssociatedFormalPattern = func.AssociatedFormalPattern
    }

let tryActualField enclosing (tys: IReadOnlyDictionary<int64, TypeSymbol>) (field: IFieldSymbol) =
    let origFieldTy = field.Type
    let fieldTy = tryActualType tys origFieldTy

    match stripTypeEquations origFieldTy with
    | TypeSymbol.ForAll(tyPars, _) ->
        let tyArgs =
            tyPars
            |> ImArray.map (fun tyPar ->
                match tys.TryGetValue tyPar.Id with
                | true, ty -> ty
                | _ -> failwith "ForAll type parameters must be substituted."
            )
        PolymorphicFieldSymbol(enclosing, field, fieldTy, tyArgs, field.IsGenerated) :> IFieldSymbol
    | _ ->

    let id = newId ()
    { new IFieldSymbol with
        member _.Id = id
        member _.Name = field.Name
        member _.Type = fieldTy
        member _.Enclosing = enclosing
        member _.Formal = field.Formal
        member _.IsField = field.IsField

        member _.FunctionFlags = field.FunctionFlags
        member _.FunctionOverrides = field.FunctionOverrides
        member _.IsProperty = field.IsProperty
        member _.IsPattern = false
        member _.IsFunction = field.IsFunction
        member _.TypeParameters = field.TypeParameters
        member _.TypeArguments = field.TypeArguments
        member _.ValueFlags = field.ValueFlags
        member _.Attributes = field.Attributes
        member _.MemberFlags = field.MemberFlags
        member _.IsThis = field.IsThis
        member _.IsBase = field.IsBase
        member _.Constant = field.Constant
        member _.AssociatedFormalPropertyId = field.AssociatedFormalPropertyId
    }

let tryActualValue (enclosing: EnclosingSymbol) (tys: IReadOnlyDictionary<int64, _>) (value: IValueSymbol) : IValueSymbol =
    match value with
    | :? LocalSymbol as value ->
        match stripTypeEquations value.Type with
        | TypeSymbol.ForAll(tyPars, innerTy) ->
            let tyArgs =
                tyPars
                |> ImArray.map (fun tyPar ->
                    match tys.TryGetValue tyPar.Id with
                    | true, ty -> ty
                    | _ -> tyPar.AsType
                )
            PolymorphicLocalSymbol(value, tryActualType tys value.Type, tyArgs, value.IsGenerated) :> IValueSymbol
        | _ ->
            LocalSymbol(value.Name, tryActualType tys value.Type, value.IsGenerated, value.IsMutable) :> IValueSymbol
    | :? LocalParameterSymbol as par ->
        match stripTypeEquations value.Type with
        | TypeSymbol.ForAll(tyPars, innerTy) ->
            let tyArgs =
                tyPars
                |> ImArray.map (fun tyPar ->
                    match tys.TryGetValue tyPar.Id with
                    | true, ty -> ty
                    | _ -> failwith "ForAll type parameters must be substituted."
                )
            PolymorphicLocalSymbol(par, tryActualType tys value.Type, tyArgs, value.IsGenerated) :> IValueSymbol
        | _ ->
            LocalParameterSymbol(par.Attributes, value.Name, tryActualType tys value.Type, par.IsThis, par.IsBase, par.IsMutable) :> IValueSymbol
    | :? FunctionGroupSymbol as funcGroup ->
        // Do not substitute anything for a function group.
        // If we have a function group here, it most likely means there was a bind error.
        funcGroup :> IValueSymbol
    | :? IFunctionSymbol as func ->
        tryActualFunction enclosing tys func :> IValueSymbol
    | :? IFieldSymbol as field ->
        tryActualField enclosing tys field :> IValueSymbol
    | :? IPropertySymbol as prop ->
        tryActualProperty enclosing tys prop :> IValueSymbol
    | _ ->
        if value.ValueFlags &&& ValueFlags.Invalid = ValueFlags.Invalid then
            value
        else
            failwith "Invalid value symbol."

let tryActualEntity (tys: IReadOnlyDictionary<_, _>) (ent: EntitySymbol) =
    let tyArgs2 = 
        (ent.TypeParameters, ent.TypeArguments)
        ||> ImArray.map2 (fun tyPar tyArg -> 
            if tyPar.HasArity && not tyArg.IsTypeVariable then
                tyArg
            else
                tryActualType tys tyArg
        )
    applyEntity tyArgs2 ent.Formal

let tryActualEnclosing tys (enclosing: EnclosingSymbol) =
    match enclosing with
    | EnclosingSymbol.Local
    | EnclosingSymbol.RootNamespace -> enclosing
    | EnclosingSymbol.Entity(ent) ->
        EnclosingSymbol.Entity(tryActualEntity tys ent)
    | EnclosingSymbol.Witness(n, ent) ->
        EnclosingSymbol.Witness(n, tryActualEntity tys ent)

let tryComputeDependentType (inputValueTy: TypeSymbol) (innerTy: TypeSymbol) =
    match stripTypeEquationsAux false true inputValueTy with
    | TypeSymbol.ConstantInt32(value) ->
        // This is where the computation of the dependent type will be evaluated.
        match stripTypeEquationsAux false true innerTy with
        | TypeSymbol.Tuple(tyArgs, _) when value < tyArgs.Length && value >= 0 ->
            tyArgs[value]
            |> ValueSome
        | ty when ty.IsSolved && value = 0 ->
            ty
            |> ValueSome
        | _ ->
            ValueNone
    | _ ->
        ValueNone

let private stripTypeEquations_InferenceVariable skipAlias skipModifiers (ty: TypeSymbol) =
    match ty with
    | TypeSymbol.InferenceVariable(tyParOpt, solution) ->
        if solution.HasSolution then
            let ty2 = solution.Solution
            let strippedTy2 =
                match tyParOpt with
                | Some(tyPar) when tyPar.Arity > 0 ->
                    if ty2.IsTypeConstructor then
                        stripTypeEquationsAux skipAlias skipModifiers ty2
                    else
                        // Because we have a second-order generic and our solution isn't a type constructor,
                        //     we must extract the type constructor out of it and reset the solution using it.
                        // TODO: We should make this path illegal if we have a non-error type.
                        match stripTypeEquationsAux skipAlias skipModifiers ty2 with
                        | TypeSymbol.HigherVariable(tyPar2, _) ->
                            solution.Solution <- TypeSymbol.Variable(tyPar2)
                            stripTypeEquationsAux skipAlias skipModifiers ty

                        | TypeSymbol.HigherInferenceVariable(_, _, externalSolution, _) ->
                            OlyAssert.True(externalSolution.IsTypeConstructor)

                            solution.Solution <- TypeSymbol.CreateInferenceVariable(tyParOpt, externalSolution)
                            stripTypeEquationsAux skipAlias skipModifiers ty

                        | TypeSymbol.Entity(ent) when not ent.IsNamespace ->
                            solution.Solution <- ent.Formal.AsType
                            stripTypeEquationsAux skipAlias skipModifiers ty

                        | _ ->
                            TypeSymbol.Error(Some tyPar, Some "Internal Error: Expected type constructor.")
                | _ ->
                    stripTypeEquationsAux skipAlias skipModifiers ty2
            solution.Solution <- (* preserve alias *) stripTypeEquationsExceptAlias ty2 // cache solution
            strippedTy2
        else
            ty
    | _ ->
        failwith "Expected inference variable type."

let private stripTypeEquations_HigherInferenceVariable_Solution skipAlias skipModifiers (solution: VariableSolutionSymbol) =
    OlyAssert.True(solution.HasSolution)

    let strippedTy = stripTypeEquationsAux skipAlias skipModifiers solution.Solution
    solution.Solution <- (* preserve alias *) stripTypeEquationsExceptAlias solution.Solution // cache solution
    strippedTy

let private stripTypeEquations_HigherInferenceVariable_ExternalSolution skipAlias skipModifiers tyArgs (externalSolution: VariableSolutionSymbol) (solution: VariableSolutionSymbol) =
    OlyAssert.True(externalSolution.HasSolution)
    OlyAssert.True(externalSolution.IsTypeConstructor)
    OlyAssert.False(solution.HasSolution)
    OlyAssert.False(solution.IsTypeConstructor)

    let appliedTy = applyType externalSolution.Solution tyArgs
    solution.Solution <- appliedTy
    stripTypeEquationsAux skipAlias skipModifiers appliedTy

let private stripTypeEquations_HigherInferenceVariable skipAlias skipModifiers (ty: TypeSymbol) =
    match ty with
    | TypeSymbol.HigherInferenceVariable(_, tyArgs, externalSolution, solution) ->
        OlyAssert.True(externalSolution.IsTypeConstructor)

        if solution.HasSolution then
            stripTypeEquations_HigherInferenceVariable_Solution skipAlias skipModifiers solution
        else
            if externalSolution.HasSolution then
                stripTypeEquations_HigherInferenceVariable_ExternalSolution skipAlias skipModifiers tyArgs externalSolution solution
            else
                ty
    | _ ->
        failwith "Expected higher inference variable type." 

let private stripTypeEquations_EagerInferenceVariable skipAlias skipModifiers (ty: TypeSymbol) =
    match ty with
    | TypeSymbol.EagerInferenceVariable(solution, _) ->
        if solution.HasSolution then
            let strippedTy = stripTypeEquationsAux skipAlias skipModifiers solution.Solution
            solution.Solution <- (* preserve alias *) stripTypeEquationsExceptAlias solution.Solution // cache solution
            strippedTy
        else
            ty
    | _ ->
        failwith "Expected eager inference variable type."

let private stripTypeEquations_Entity skipAlias skipModifiers (ty: TypeSymbol) =
    match ty with
    | TypeSymbol.Entity(ent) ->
        if skipAlias then
            ty
        elif ent.Kind = EntityKind.Alias then
            if ent.Extends.Length = 1 then
                stripTypeEquationsAux skipAlias skipModifiers ent.Extends[0]
            else
                // Alias is expected to have an extends.
                // REVIEW: This does not guarantee that there will be an error in diagnostics.
                // TODO: An entity-kind can only be an alias if there is an extends, so this is technically not valid.
                //       We should assert there is a single extends.
                TypeSymbolError
        else
            ty
    | _ ->
        failwith "Expected entity type."

let private stripTypeEquations_Function skipAlias skipModifiers (ty: TypeSymbol) =
    match ty with
    // 'Function' is a built-in type whose formal definition has a variadic type parameter as a single argument.
    // This handles the actual expansion of the variadic type, which is stored as a tuple type.
    | TypeSymbol.Function(inputTys, returnTy, kind) ->
        match inputTys with
        | VariadicTypeArguments(tys, _) when tys.Length = 1 && tys[0].IsSolved ->
            let inputTy = tys[0]
            match inputTy with
            | TypeSymbol.InferenceVariable(Some tyPar, _) when tyPar.IsVariadic ->
                match stripTypeEquationsAux skipAlias skipModifiers inputTy with
                | TypeSymbol.Tuple(itemTys, _) -> TypeSymbol.CreateFunction(itemTys, returnTy, kind)
                | TypeSymbol.Unit -> TypeSymbol.CreateFunction(ImArray.empty, returnTy, kind)
                | _ -> ty
            | _ ->
                ty
        | _ ->
            ty
    | _ ->
        failwith "Expected function type."

let private stripTypeEquations_Tuple skipAlias skipModifiers (ty: TypeSymbol) =
    match ty with
    // 'Tuple' is a built-in type whose formal definition has a variadic type parameter as a single argument.
    // This handles the actual expansion of the variadic type, which is stored as a tuple type.
    | TypeSymbol.Tuple(argTys, _) ->
        if argTys.Length = 1 && argTys[0].IsSolved then
            match argTys[0] with
            | TypeSymbol.InferenceVariable(Some tyPar, _) when tyPar.IsVariadic ->
                match stripTypeEquationsAux skipAlias skipModifiers argTys[0] with
                | TypeSymbol.Tuple _ as ty -> ty
                | _ -> ty
            | _ ->
                ty
        else
            ty
    | _ ->
        failwith "Expected tuple type."

let private stripTypeEquations_DependentIndexer skipAlias skipModifiers (ty: TypeSymbol) =
    match ty with
    | TypeSymbol.DependentIndexer(inputValueTy, innerTy) ->
        match tryComputeDependentType inputValueTy innerTy with
        | ValueSome(ty) -> stripTypeEquationsAux skipAlias skipModifiers ty
        | _ -> ty
    | _ ->
        failwith "Expected dependent indexer type."

let stripTypeEquationsAux skipAlias skipModifiers (ty: TypeSymbol) =
    match ty with
    | TypeSymbol.InferenceVariable _ ->
        stripTypeEquations_InferenceVariable skipAlias skipModifiers ty

    | TypeSymbol.HigherInferenceVariable _ ->
        stripTypeEquations_HigherInferenceVariable skipAlias skipModifiers ty

    | TypeSymbol.EagerInferenceVariable _ ->
        stripTypeEquations_EagerInferenceVariable skipAlias skipModifiers ty

    | TypeSymbol.Entity _ ->
        stripTypeEquations_Entity skipAlias skipModifiers ty

    | TypeSymbol.Function _ ->
        stripTypeEquations_Function skipAlias skipModifiers ty

    | TypeSymbol.Tuple _ ->
        stripTypeEquations_Tuple skipAlias skipModifiers ty

    | TypeSymbol.DependentIndexer _ ->
        stripTypeEquations_DependentIndexer skipAlias skipModifiers ty

    | _ ->
        ty

let stripTypeEquations (ty: TypeSymbol) =
    stripTypeEquationsAux false false ty

let stripTypeEquationsAndBuiltIn (ty: TypeSymbol) =
    let ty = stripTypeEquationsAux false true ty
    match ty.TryIntrinsicType with
    | Some(intrinTy: TypeSymbol) -> 
        OlyAssert.False(intrinTy.IsAlias)
        intrinTy
    | _ -> 
        OlyAssert.False(ty.IsAlias)
        ty

let stripTypeEquationsExceptAlias (ty: TypeSymbol) =
    stripTypeEquationsAux true false ty

let stripByRef (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.ByRef(innerTy, _) -> innerTy
    | _ -> ty

let actualConstraint (tyArgs: TypeArgumentSymbol imarray) (constr: ConstraintSymbol) =
    match constr with
    | ConstraintSymbol.Null
    | ConstraintSymbol.Struct
    | ConstraintSymbol.NotStruct 
    | ConstraintSymbol.Unmanaged
    | ConstraintSymbol.Blittable
    | ConstraintSymbol.Scoped -> constr
    | ConstraintSymbol.ConstantType(ty) ->
        ConstraintSymbol.ConstantType(Lazy<_>.CreateFromValue(actualType tyArgs ty.Value))
    | ConstraintSymbol.SubtypeOf(ty) ->
        ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(actualType tyArgs ty.Value))

type INamespaceSymbol = EntitySymbol

[<Sealed>]
type AggregatedNamespaceSymbol(name, enclosing: EnclosingSymbol, ents: INamespaceSymbol imarray) as this =
    inherit EntitySymbol()

    let nestedEnts =
        lazy
#if DEBUG || CHECKED
            let path = enclosing.FullNamespacePath.Add(name)
            ents
            |> ImArray.iter (fun x ->
                OlyAssert.EqualArray((path : string imarray), (x.FullNamespacePath : string imarray))
            )
#endif
            // TODO: We could optimize this a bit without as many allocations and iterations.
            let nestedEnts =
                ents
                |> ImArray.map (fun x -> x.Entities)
                |> ImArray.concat
            if nestedEnts.IsEmpty || nestedEnts.Length = 1 then
                nestedEnts
            else
                let nmspaces = Dictionary<string, AggregatedNamespaceSymbol>()
                nestedEnts
                |> ImArray.filter (fun x -> 
                    if x.IsNamespace then
                        let aggrNmspace =
                            match nmspaces.TryGetValue x.Name with
                            | true, aggrNmspace -> aggrNmspace
                            | _ ->
                                AggregatedNamespaceSymbol(x.Name, EnclosingSymbol.Entity(this), ImArray.empty)
                        nmspaces[x.Name] <- aggrNmspace.AddNamespace(x)
                        false
                    else
                        true
                )
                |> ImArray.append (nmspaces.Values |> Seq.map (fun x -> x :> EntitySymbol) |> ImArray.ofSeq)

    member _.Namespaces = ents

    member _.AddNamespace(ent: INamespaceSymbol) =
        if not ent.IsNamespace then
            failwith "Expected namespace."

        OlyAssert.Equal(name, ent.Name)
        AggregatedNamespaceSymbol(name, enclosing, ents.Add(ent))

    override this.Attributes = ImArray.empty
    override this.ContainingAssembly = None
    override this.Enclosing = enclosing
    override this.Entities = nestedEnts.Value
    override this.Extends = ImArray.empty
    override this.Fields = ImArray.empty
    override this.Flags = EntityFlags.None
    override this.Formal = this
    override this.Functions = ImArray.empty
    override this.Implements = ImArray.empty
    override this.Patterns = ImArray.empty
    override this.InstanceConstructors = ImArray.empty
    override this.Kind = EntityKind.Namespace
    override this.Name = name
    override this.Properties = ImArray.empty
    override this.TypeArguments = ImArray.empty
    override this.TypeParameters = ImArray.empty

[<RequireQualifiedAccess>]
type EnclosingSymbol =
    | Entity of EntitySymbol
    | Witness of concreteTy: TypeSymbol * ent: EntitySymbol
    | Local
    | RootNamespace

    interface ISymbol

    member this.Formal =
        match this with
        | Entity(ent) -> EnclosingSymbol.Entity(ent.Formal)
        | Witness(concreteTy, ent) -> EnclosingSymbol.Witness(concreteTy, ent.Formal)
        | Local
        | RootNamespace -> this

    member this.IsImported =
        match this with
        | Entity(ent) ->
            ent.Attributes
            |> ImArray.exists (function AttributeSymbol.Import _ -> true | _ -> false)
        | _ ->
            false

    member this.IsExported =
        match this with
        | Entity(ent) ->
            ent.Attributes
            |> ImArray.exists (function AttributeSymbol.Export -> true | _ -> false)
        | _ ->
            false

    member this.IsRootNamespaceEnclosing =
        match this with
        | RootNamespace -> true
        | _ -> false

    member this.IsAnonymousModule =
        match this with
        | Entity(ent) ->
            ent.IsAnonymousModule
        | _ ->
            false

    member this.IsLocalEnclosing =
        match this with
        | Local -> true
        | _ -> false

    member this.IsNamespace =
        match this with
        | EnclosingSymbol.Entity(ent) -> ent.IsNamespace
        | EnclosingSymbol.RootNamespace -> true
        | _ -> false

    member this.IsNamespaceOrModule =
        match this with
        | EnclosingSymbol.Entity(ent) -> ent.IsNamespaceOrModule
        | EnclosingSymbol.RootNamespace -> true
        | _ -> false

    member this.Enclosing =
        match this with
        | Entity(ent) ->
            ent.Enclosing
        | _ ->
            RootNamespace

    member this.TypeArguments : TypeArgumentSymbol imarray =
        match this with
        | Entity(ent) -> ent.TypeArguments
        | Witness(_, ent) -> ent.TypeArguments
        | _ -> ImArray.empty

    member this.TypeParameters: TypeParameterSymbol imarray =
        match this with
        | Entity(ent) -> ent.TypeParameters
        | Witness(_, ent) -> ent.TypeParameters
        | _ -> ImmutableArray.Empty

    member this.TypeParameterCount =
        match this with
        | Entity(ent) -> ent.TypeParameters.Length
        | Witness(_, ent) -> ent.TypeParameters.Length
        | _ -> 0

    // Gets the type parameter count that does not include its enclosing's type parameter count.
    member this.LogicalTypeParameterCount =
        match this.TryEntity with
        | Some ent -> ent.LogicalTypeParameterCount
        | _ -> 0

    // Gets the type parameter count that does not include its enclosing's type parameters.
    member this.LogicalTypeParameters =
        match this.TryEntity with
        | Some ent -> ent.LogicalTypeParameters
        | _ -> ImArray.empty

    member this.IsModule =
        match this with
        | Entity(ent) -> ent.IsModule
        | _ -> false

    member this.IsClosure =
        match this with
        | Entity(ent) -> ent.IsClosure
        | _ -> false

    member this.IsEnum =
        match this with
        | Entity(ent) -> ent.IsEnum
        | _ -> false

    member this.IsTypeExtension =
        match this with
        | Entity(ent) when ent.Kind = EntityKind.TypeExtension -> true
        | _ -> false

    member this.IsTypeConstructor =
        match this with
        | Entity(ent) -> ent.IsTypeConstructor
        | Witness(_, ent) -> ent.IsTypeConstructor
        | _ -> false

    member this.IsInterface =
        match this with
        | Entity(ent) -> ent.IsInterface
        | _ -> false

    member this.IsSealed =
        match this with
        | Entity(ent) -> ent.IsSealed
        | _ -> false

    member this.IsClass =
        match this with
        | Entity(ent) -> ent.IsClass
        | _ -> false

    member this.IsNewtype =
        match this with
        | Entity(ent) -> ent.IsNewtype
        | _ -> false

    member this.IsClassOrStructOrModuleOrNewtype =
        match this with
        | Entity(ent) -> ent.IsClassOrStructOrModuleOrNewtype
        | _ -> false

    member this.IsAbstract =
        match this with
        | Entity(ent) -> ent.IsAbstract
        | _ -> false

    member this.IsAnyStruct =
        match this with
        | Entity(ent) -> ent.IsAnyStruct
        | _ -> false

    member this.IsReadOnly =
        match this with
        | Entity(ent) -> ent.IsReadOnly
        | _ -> false

    member this.IsShape =
        match this with
        | Entity(ent) -> ent.IsShape
        | _ -> false

    member this.IsWitnessShape =
        match this with
        | Witness(_, ent) -> ent.IsShape
        | _ -> false

    member this.IsFinal =
        match this with
        | Entity(ent) -> ent.IsSealed
        | _ -> true

    member this.TryEntity : EntitySymbol option =
        match this with
        | Entity(ent) -> ent |> Some
        | _ -> None

    member this.TryType : TypeSymbol option =
        match this with
        | Entity(ent) when not ent.IsNamespace -> ent.AsType |> Some
        | Witness(witnessTy, _) -> witnessTy |> Some
        | _ -> None

    member this.TryNamespace : INamespaceSymbol option =    
        match this with
        | Entity(ent) when ent.IsNamespace -> ent |> Some
        | _ -> None

    member this.Implements =
        match this with
        | Entity(ent) -> ent.Implements
        | _ -> ImArray.empty

    member this.IsType =
        this.TryType.IsSome

    member this.AsType =
        match this.TryType with
        | Some ty -> ty
        | _ -> failwith "Enclosing is not a type."

    member this.AsEntity =
        match this.TryEntity with
        | Some ent -> ent
        | _ -> failwith "Enclosing is not an entity."

    member this.FullNamespacePath: string imarray =
        match this with
        | Entity(ent) -> ent.FullNamespacePath
        | _ -> ImArray.empty

/// Member flags.
[<System.Flags>]
type MemberFlags =
    | None =                0x00000000
    
    | Public =              0x00000000
    | Internal =            0x00000001
    | Protected =           0x00000002
    | Private =             0x00000003   
    | AccessorMask =        0x00000007

    /// A member that can be overrided.
    | Virtual =             0x00000010

    /// A member that does not have an implementation and can be overrided.
    | Abstract =            0x00000110

    /// An instance member that will not override a base member.
    | NewSlot =             0x00001010

    /// A member that cannot be overriden.
    /// TODO: Rename to 'Final'.
    | Sealed =              0x00010000

    /// Instance (not static) member
    | Instance =            0x00100000

    | ExplicitOverrides =   0x01000000

[<System.Flags>]
type FunctionFlags =
    | None                          = 0x0000000000000L

    /// Constructor member.
    | Constructor                   = 0x0000000000001L

    /// Static local (not a member).
    | StaticLocal                   = 0x0000000000010L

    /// Function is marked 'inline' and will be inlined by the runtime.
    | Inline                        = 0x0000000000100L

    /// Function is marked 'not inline' and will never be inlined by the runtime.
    | InlineNever                   = 0x0000000000200L

    | InlineAlways                  = 0x0000000000300L

    | InlineMask                    = 0x0000000000700L

    | Pure                          = 0x0000000001000L

    | ImplicitDefaultConstructor    = 0x0000000010001L

    | Extra                         = 0x0000000100000L

    | EntryPoint                    = 0x0000001000000L

    | RequiresExplicitTypeArguments = 0x0001000000000L

    | ParameterLess                 = 0x0010000000000L

    | Blittable                     = 0x0100000000000L

    | UnmanagedAllocationOnly       = 0x1000000000000L

[<System.Flags>]
type ValueFlags =
    | None =      0x0000000
    | Imported =  0x0000001
    | Exported =  0x0000010
    /// Marks a function, local, or field as 'mutable'.
    /// If a function is marked 'mutable', meaning that the function is an instance member on a (struct or shape) and does mutate the receiver.
    | Mutable =   0x0000100
    | Parameter = 0x0001000
    | Generated = 0x0010000
    | FieldInit = 0x0100000
    | Invalid =   0x1000000

[<RequireQualifiedAccess>]
type EntityKind =
    | Namespace
    | Module
    | TypeExtension
    | Alias
    | Class
    | Interface
    | Shape
    | Struct
    | Enum
    | Closure
    | Newtype

type FunctionSemantic =
    | NormalFunction
    | GetterFunction
    | SetterFunction
    | PatternFunction
    | PatternGuardFunction

[<RequireQualifiedAccess>]
type WellKnownFunction =
    | None
    | Add
    | Subtract
    | Multiply
    | Divide
    | Remainder
    | And
    | Or
    | Not
    | Negate
    | Equal
    | NotEqual
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    | AddressOf
    | UnsafeAddressOf
    | FromAddress
    | NewRefCell
    | LoadRefCellContents
    | StoreRefCellContents
    | Print
    | Throw
    | Cast
    | UnsafeCast
    | Ignore

    | BitwiseAnd
    | BitwiseOr
    | BitwiseExclusiveOr
    | BitwiseNot
    | BitwiseShiftLeft
    | BitwiseShiftRight

    | NewMutableArray
    | GetArrayLength
    | GetArrayElement
    | SetArrayElement

    | LoadFunction
    | LoadStaticFunction
    | LoadFunctionPtr
    | LoadNullPtr

    | GetTupleElement

    | Import // TODO: this is a little weird as its not really a function or op...
    | Constant

    static member TryFromName(name: string) =
        match name with
        | "cast" -> Cast |> Some
        | "unsafe_cast" -> UnsafeCast |> Some
        | "add" -> Add |> Some
        | "subtract" -> Subtract |> Some
        | "multiply" -> Multiply |> Some
        | "divide" -> Divide |> Some
        | "remainder" -> Remainder |> Some
        | "and" -> And |> Some
        | "or" -> Or |> Some
        | "not" -> Not |> Some
        | "negate" -> Negate |> Some
        | "equal" -> Equal |> Some
        | "not_equal" -> NotEqual |> Some
        | "greater_than" -> GreaterThan |> Some
        | "greater_than_or_equal" -> GreaterThanOrEqual |> Some
        | "less_than" -> LessThan |> Some
        | "less_than_or_equal" -> LessThanOrEqual |> Some
        | "address_of" -> AddressOf |> Some
        | "unsafe_address_of" -> UnsafeAddressOf |> Some
        | "from_address" -> FromAddress |> Some
        | "new_ref_cell" -> NewRefCell |> Some
        | "load_ref_cell_contents" -> LoadRefCellContents |> Some
        | "store_ref_cell_contents" -> StoreRefCellContents |> Some
        | "print" -> Print |> Some
        | "throw" -> Throw |> Some
        | "bitwise_not" -> BitwiseNot |> Some
        | "bitwise_and" -> BitwiseAnd |> Some
        | "bitwise_or" -> BitwiseOr |> Some
        | "bitwise_exclusive_or" -> BitwiseExclusiveOr |> Some
        | "bitwise_shift_left" -> BitwiseShiftLeft |> Some
        | "bitwise_shift_right" -> BitwiseShiftRight |> Some
        | "get_length" -> GetArrayLength |> Some
        | "get_element" -> GetArrayElement |> Some
        | "set_element" -> SetArrayElement |> Some
        | "load_function" -> LoadFunction |> Some
        | "load_static_function" -> LoadStaticFunction |> Some
        | "load_function_ptr" -> LoadFunctionPtr |> Some
        | "import" -> Import |> Some
        | "constant" -> Constant |> Some
        | "get_tuple_element" -> GetTupleElement |> Some
        | "new_array" -> NewMutableArray |> Some
        | "load_null_ptr" -> LoadNullPtr |> Some
        | _ -> option.None

    static member TryFromAttribute(attr: AttributeSymbol) =
        match attr with
        | AttributeSymbol.Intrinsic(name) ->
            WellKnownFunction.TryFromName(name)
        | _ ->
            option.None

    static member TryFromAttributes(attrs: AttributeSymbol imarray) =
        attrs
        |> ImArray.tryPick (fun attr -> 
            match attr with
            | AttributeSymbol.Intrinsic(name) -> Some name
            | _ -> option.None
        )
        |> Option.bind (fun name ->
            WellKnownFunction.TryFromName(name)
        )

[<DebuggerDisplay("{Name}")>]
type IFunctionSymbol =
    inherit IValueSymbol

    abstract Attributes: AttributeSymbol imarray

    abstract Parameters: ILocalParameterSymbol imarray

    abstract ReturnType: TypeSymbol
    
    abstract Semantic: FunctionSemantic

    abstract WellKnownFunction: WellKnownFunction

    abstract AssociatedFormalPattern: IPatternSymbol option

[<Sealed;DebuggerDisplay("{Name}")>]
type FunctionSymbol(enclosing, attrs, name, funcTy: TypeSymbol, pars: ILocalParameterSymbol imarray, tyPars, tyArgs, memberFlags, funcFlags, funcSemantic, wellKnownFunc, overrides: IFunctionSymbol option, isMutable) =

#if DEBUG || CHECKED
    do
        match funcTy.TryFunction with
        | ValueSome(_, outputTy: TypeSymbol) when not outputTy.IsError_t && outputTy.IsTypeConstructor ->
            failwith "Unexpected type constructor."
        | _ ->
            ()

        pars
        |> ImArray.iter (fun par ->
            if not par.Type.IsError_t && par.Type.IsTypeConstructor then
                failwith "Unexpected type constructor."
        )
#endif

    let id = newId ()

    let valueFlags =
        if isMutable then
            ValueFlags.Mutable
        else
            ValueFlags.None

    let valueFlags =
        if attributesContainImport attrs then
            valueFlags ||| ValueFlags.Imported
        else
            valueFlags

    let valueFlags =
        if attributesContainExport attrs then
            valueFlags ||| ValueFlags.Exported
        else
            valueFlags

    let mutable overrides = overrides
    let mutable attrs = attrs
    let mutable patOpt = None
    let mutable funcFlags = funcFlags
    let mutable memberFlags = memberFlags
    let mutable wellKnownFunc = wellKnownFunc

    member _.Enclosing = enclosing
    member _.Attributes = attrs
    member _.Name = name
    member _.Id = id
    member _.Parameters = pars
    member _.TypeParameters = tyPars
    member _.TypeArguments = tyArgs
    member _.Type = funcTy
    member _.FunctionFlags = funcFlags
    member _.ValueFlags = valueFlags
    member _.MemberFlags = memberFlags
    member _.Semantic = funcSemantic
    member _.WellKnownFunction = wellKnownFunc
    member _.FunctionOverrides = overrides
    member this.ReturnType = (this :> IFunctionSymbol).ReturnType

    member _.AssociatedFormalPattern = patOpt

    /// Mutability
    member this.SetWellKnownFunction(wkf) =
        wellKnownFunc <- wkf

    /// Mutability
    member this.SetOverrides_Pass3_NonConcurrent(overridesToSet: IFunctionSymbol) =
        if overridesToSet.IsFinal then
            failwith "Cannot set overrides with a sealed function."
        if not overridesToSet.IsVirtual then
            failwith "Cannot set overrides with a non-virtual function."

        overrides <- Some overridesToSet

    /// Mutability - is this the only good way to handle this?
    member this.SetAttributes_Pass3_NonConcurrent(newAttrs: AttributeSymbol imarray) = 
        attrs <- newAttrs

    /// Mutability
    member this.SetAssociatedFormalPattern_Pass2_NonConcurrent(pat: IPatternSymbol) =
        OlyAssert.True(pat.IsFormal)
        OlyAssert.True(this.IsPatternFunction)
        OlyAssert.False(this.IsLocal)
        patOpt <- Some pat

    /// Mutability - only used in LambdaLifting
    member this.SetStaticLocal() =
        funcFlags <- funcFlags ||| FunctionFlags.StaticLocal

    /// Mutability
    member this.SetVirtualFinalNewSlot_Pass3() =
#if DEBUG || CHECKED
        OlyAssert.False(this.IsVirtual)
        OlyAssert.False(this.IsFinal)
        OlyAssert.False(this.IsNewSlot)
#endif
        memberFlags <- memberFlags ||| MemberFlags.Virtual ||| MemberFlags.Sealed ||| MemberFlags.NewSlot

    /// Mutability
    member this.SetFinal_Pass3() =
#if DEBUG || CHECKED
        OlyAssert.True(this.IsVirtual)
#endif
        memberFlags <- memberFlags ||| MemberFlags.Sealed

    interface IFunctionSymbol with
        member _.Enclosing = enclosing
        member _.Attributes = attrs
        member _.Name = name
        member _.Type = funcTy
        member _.Parameters = pars
        member _.ReturnType =
            match funcTy.TryFunction with
            | ValueSome(_, outputTy) -> outputTy
            | _ -> funcTy

        member _.TypeParameters = tyPars
        member _.TypeArguments = tyArgs
        member _.Id = id
        member _.FunctionFlags = funcFlags
        member _.FunctionOverrides = overrides
        member _.IsProperty = false
        member _.IsPattern = false

        member _.IsFunction = true
        member _.IsField = false
        member this.Formal = this :> IValueSymbol
        member _.ValueFlags = valueFlags
        member _.MemberFlags = memberFlags
        member _.IsThis = false
        member _.IsBase = false
        member _.Semantic = funcSemantic
        member _.WellKnownFunction = wellKnownFunc
        member _.AssociatedFormalPattern = patOpt

[<Sealed;DebuggerDisplay("{Name}")>]
type InvalidFunctionSymbol(enclosing, name) =
    
    let func =
        FunctionSymbol(
            enclosing, 
            ImArray.empty, 
            name, 
            TypeSymbolError, 
            ImArray.empty, 
            ImArray.empty, 
            ImArray.empty, 
            MemberFlags.None, 
            FunctionFlags.None,
            NormalFunction,
            WellKnownFunction.None,
            None,
            false
        ) :> IFunctionSymbol

    member _.Name = func.Name

    interface IFunctionSymbol with
        member _.Enclosing = func.Enclosing
        member _.Attributes = func.Attributes
        member _.Name = func.Name
        member _.Type = func.Type
        member _.Parameters = func.Parameters
        member _.ReturnType = func.ReturnType
        member _.TypeParameters = func.TypeParameters
        member _.TypeArguments = func.TypeArguments
        member _.Id = func.Id
        member _.FunctionFlags = func.FunctionFlags
        member _.FunctionOverrides = func.FunctionOverrides
        member _.IsProperty = false
        member _.IsPattern = false
        member _.IsFunction = true
        member _.IsField = false
        member this.Formal = this :> IValueSymbol
        member _.ValueFlags = func.ValueFlags
        member _.MemberFlags = func.MemberFlags
        member _.IsThis = false
        member _.IsBase = false
        member _.Semantic = func.Semantic
        member _.WellKnownFunction = func.WellKnownFunction
        member _.AssociatedFormalPattern = None

/// Technically an invalid value, but is useful for diagnostics/tooling purposes.
[<Sealed;DebuggerDisplay("{Name}")>]
type FunctionGroupSymbol(enclosing: EnclosingSymbol, name: string, funcs: IFunctionSymbol imarray, fakeParCount, isPattern) =

    let id = newId()
    let errorTy = TypeSymbolError     

    let principalFunc =
        if funcs.IsEmpty then
            invalidArg (nameof(funcs)) "Empty."

        funcs.[0]

    let pars =
        ImArray.init fakeParCount (fun i -> 
            let id = newId()
            let isThis = principalFunc.IsInstance && i = 0
            { new ILocalParameterSymbol with
                member this.IsThis = isThis
                member this.IsBase = false
                member this.Enclosing = EnclosingSymbol.Local
                member this.Formal = this :> IValueSymbol
                member this.FunctionFlags = FunctionFlags.None
                member this.FunctionOverrides = None
                member this.IsProperty = false
                member this.Id = id
                member this.IsField = false
                member this.IsFunction = false
                member this.IsPattern = false
                member this.MemberFlags = MemberFlags.None
                member this.Name = ""
                member this.Type = errorTy
                member this.TypeArguments = ImArray.empty
                member this.TypeParameters = ImArray.empty
                member this.ValueFlags = ValueFlags.Parameter ||| ValueFlags.Invalid
                member this.Attributes = ImArray.empty
            }
        )

    member this.Name = name

    member this.Functions: IFunctionSymbol imarray = funcs

    interface IFunctionSymbol with
        
        member this.Attributes = ImArray.empty

        member this.Parameters = pars

        member this.ReturnType = errorTy

        member this.Enclosing: EnclosingSymbol = enclosing

        member this.MemberFlags = if principalFunc.IsInstance && pars.Length >= 1 then MemberFlags.Instance else MemberFlags.None

        member this.Formal: IValueSymbol = this :> IValueSymbol

        member this.FunctionFlags: FunctionFlags = FunctionFlags.None

        member this.FunctionOverrides: IFunctionSymbol option = None

        member this.IsProperty = false

        member _.IsPattern = false

        member this.Id: int64 = id

        member this.IsField: bool = false

        member this.IsFunction: bool = true

        member this.Name: string = name

        member this.Type: TypeSymbol = errorTy

        member this.TypeArguments: ImmutableArray<TypeSymbol> = ImArray.empty

        member this.TypeParameters: ImmutableArray<TypeParameterSymbol> = ImArray.empty

        member this.ValueFlags: ValueFlags = ValueFlags.Invalid

        member this.IsThis = false

        member this.IsBase = false

        member this.Semantic = 
            if isPattern then
                PatternFunction
            else
                NormalFunction

        member this.WellKnownFunction = WellKnownFunction.None

        member this.AssociatedFormalPattern = None

    new(name: string, funcs: IFunctionSymbol imarray, fakeParCount, isPattern) =
        FunctionGroupSymbol(EnclosingSymbol.RootNamespace, name, funcs, fakeParCount, isPattern)

    static member Create(funcs: IFunctionSymbol imarray) =
        if funcs.Length <= 0 then
            failwith "assert"
        let principalFunc = funcs[0]
        FunctionGroupSymbol(principalFunc.Name, funcs, principalFunc.Parameters.Length, false)

    static member CreateIfPossible(funcs: IFunctionSymbol imarray) =
        Assert.ThrowIfNot(not funcs.IsEmpty)
        if funcs.Length = 1 then
            funcs[0]
        else
            FunctionGroupSymbol.Create(funcs) :> IFunctionSymbol

[<DebuggerDisplay("{Name}")>]
type IFieldSymbol =
    inherit IValueSymbol

    abstract Attributes: AttributeSymbol imarray

    abstract Constant: ConstantSymbol voption

    /// Meant for tooling, ties a field to a property whose backing field is this one.
    abstract AssociatedFormalPropertyId: int64 option

[<Sealed;DebuggerDisplay("{Name}")>]
type FieldSymbol(attrs, enclosing, memberFlags, name, ty, valueFlags, associatedFormalPropId: int64 option ref) =
    let id = newId()

    let mutable attrs = attrs
    let mutable constant = ValueNone
    let mutable valueFlags = valueFlags

    /// Mutability - is this the only good way to handle this?
    member _.SetAttributes_Pass3_NonConcurrent(newAttrs) = attrs <- newAttrs

    /// Mutability
    member _.SetConstant_Pass4_NonConcurrent(newConstant) = constant <- newConstant

    member _.Name = name
    member _.Type = ty

    interface IFieldSymbol with
        member this.Attributes: imarray<AttributeSymbol> = attrs
        member this.Constant: ConstantSymbol voption = constant
        member this.Enclosing: EnclosingSymbol = enclosing
        member this.IsProperty = false
        member _.IsPattern = false
        member this.Formal: IValueSymbol = this :> IValueSymbol
        member this.FunctionFlags: FunctionFlags = FunctionFlags.None
        member this.FunctionOverrides: IFunctionSymbol option = None
        member this.Id: int64 = id
        member this.IsBase: bool = false
        member this.IsField: bool = true
        member this.IsFunction: bool = false
        member this.IsThis: bool = false
        member this.MemberFlags: MemberFlags = memberFlags
        member this.Name: string = name
        member this.Type: TypeSymbol = ty
        member this.ValueFlags: ValueFlags = valueFlags  
        member this.AssociatedFormalPropertyId = associatedFormalPropId.contents

        member _.TypeParameters =
            match stripTypeEquations ty with
            | TypeSymbol.ForAll(tyPars, _) -> tyPars
            | _ -> ImArray.empty

        member _.TypeArguments =
            match stripTypeEquations ty with
            | TypeSymbol.ForAll(tyPars, _) ->
                tyPars
                |> ImArray.map (fun tyPar -> tyPar.AsType)
            | _ -> 
                ImArray.empty

[<Sealed>]
type PolymorphicFieldSymbol(enclosing, field: IFieldSymbol, ty: TypeSymbol, tyArgs: TypeSymbol imarray, isGenerated) =
    
    let id = newId ()
    let flags =
        if isGenerated then
            field.ValueFlags ||| ValueFlags.Generated
        else
            field.ValueFlags

    interface IFieldSymbol with

        member this.AssociatedFormalPropertyId: int64 option = 
            field.AssociatedFormalPropertyId

        member this.Attributes: AttributeSymbol imarray = 
            field.Attributes

        member this.Constant: ConstantSymbol voption = 
            field.Constant

        member _.Id = id

        member _.Enclosing = enclosing

        member _.Type = ty

        member _.Name = field.Name

        member _.TypeParameters = field.TypeParameters

        member _.TypeArguments = tyArgs

        member _.IsField = true

        member _.IsFunction = 
            OlyAssert.Equal(false, field.IsFunction)
            false

        member _.MemberFlags = field.MemberFlags

        member _.FunctionFlags = 
            OlyAssert.Equal(FunctionFlags.None, field.FunctionFlags)
            FunctionFlags.None

        member _.FunctionOverrides = 
            OlyAssert.Equal(None, field.FunctionOverrides)
            None

        member _.IsProperty = false

        member _.IsPattern = false

        member this.Formal = field.Formal

        member _.ValueFlags = flags

        member _.IsThis = false

        member _.IsBase = false

[<DebuggerDisplay("{Name}")>]
type IPropertySymbol =
    inherit IValueSymbol

    abstract Attributes: AttributeSymbol imarray

    abstract Getter : IFunctionSymbol option

    abstract Setter : IFunctionSymbol option

    abstract BackingField : IFieldSymbol option

[<Sealed>]
type PropertySymbol(enclosing, attrs, name, valueFlags, memberFlags, propTy, getterOpt: FunctionSymbol option, setterOpt: FunctionSymbol option, backingFieldOpt) =
    
    let id = newId()
    do
        if valueFlags &&& ValueFlags.Mutable = ValueFlags.Mutable then
            failwith "Properties cannot be marked 'mutable'"

    member _.Id = id
    member _.Type = propTy
    member _.Getter = getterOpt
    member _.Setter = setterOpt
    member _.BackingField = backingFieldOpt

    interface IPropertySymbol with
        member this.Attributes: imarray<AttributeSymbol> = attrs
        member this.Enclosing: EnclosingSymbol = enclosing
        member this.Formal: IValueSymbol = this :> IValueSymbol

        member this.FunctionFlags: FunctionFlags = FunctionFlags.None
        member this.FunctionOverrides: IFunctionSymbol option = None
        member _.IsProperty = true
        member _.IsPattern = false
        member this.Getter: IFunctionSymbol option = getterOpt |> Option.map (fun x -> x :> IFunctionSymbol)
        member this.Id: int64 = id
        member this.IsBase: bool = false
        member this.IsField: bool = false
        member this.IsFunction: bool = false
        member this.IsThis: bool = false
        member this.MemberFlags: MemberFlags = memberFlags
        member this.Name: string = name
        member this.Setter: IFunctionSymbol option = setterOpt |> Option.map (fun x -> x :> IFunctionSymbol)
        member this.Type: TypeSymbol = propTy
        member this.TypeArguments: imarray<TypeArgumentSymbol> = ImArray.empty
        member this.TypeParameters: imarray<TypeParameterSymbol> = ImArray.empty
        member this.ValueFlags: ValueFlags = valueFlags
        member this.BackingField = backingFieldOpt

[<DebuggerDisplay("{Name}")>]
type IPatternSymbol =
    inherit IValueSymbol

    abstract Attributes: AttributeSymbol imarray

    abstract PatternFunction: IFunctionSymbol

    abstract PatternGuardFunction: IFunctionSymbol option

[<Sealed>]
type PatternSymbol(enclosing, attrs, name, func: IFunctionSymbol) =
    
    let id = newId()
    let mutable guardOpt = None

    member _.Name = name
    member _.Type = func.Type

    member _.PatternFunction = func
    member _.PatternGuardFunction = guardOpt

    /// Mutability
    member _.SetPatternGuardFunction_Pass2_NonConcurrent(guardFunc: FunctionSymbol) =
        guardOpt <- Some (guardFunc :> IFunctionSymbol)

    interface IPatternSymbol with
        member this.Attributes: imarray<AttributeSymbol> = attrs
        member this.Enclosing: EnclosingSymbol = enclosing
        member this.Formal: IValueSymbol = this :> IValueSymbol

        member this.FunctionFlags: FunctionFlags = FunctionFlags.None
        member this.FunctionOverrides: IFunctionSymbol option = None
        member _.IsProperty = false
        member _.IsPattern = true
        member this.PatternFunction = func
        member this.PatternGuardFunction = guardOpt
        member this.Id: int64 = id
        member this.IsBase: bool = false
        member this.IsField: bool = false
        member this.IsFunction: bool = false
        member this.IsThis: bool = false
        member this.MemberFlags: MemberFlags = func.MemberFlags
        member this.Name: string = name
        member this.Type: TypeSymbol = func.Type
        member this.TypeArguments: imarray<TypeArgumentSymbol> = ImArray.empty
        member this.TypeParameters: imarray<TypeParameterSymbol> = ImArray.empty
        member this.ValueFlags: ValueFlags = func.ValueFlags

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type ConstantSymbol =
    | Int8 of value: int8
    | UInt8 of value: uint8
    | Int16 of value: int16
    | UInt16 of value: uint16
    | Int32 of value: int32
    | UInt32 of value: uint32
    | Int64 of value: int64
    | UInt64 of value: uint64
    | Float32 of value: float32
    | Float64 of value: float
    | True
    | False
    | Array of elementTy: TypeSymbol * elements: ConstantSymbol imarray
    | Char16 of  value: char
    | Utf16 of value: string
    | TypeVariable of TypeParameterSymbol
    | External of IFunctionSymbol
    | Error

    member this.Type =
        match this with
        | Int8 _ -> TypeSymbol.Int8
        | UInt8 _ -> TypeSymbol.UInt8
        | Int16 _ -> TypeSymbol.Int16
        | UInt16 _ -> TypeSymbol.UInt16
        | Int32 _ -> TypeSymbol.Int32
        | UInt32 _ -> TypeSymbol.UInt32
        | Int64 _ -> TypeSymbol.Int64
        | UInt64 _ -> TypeSymbol.UInt64
        | Float32 _ -> TypeSymbol.Float32
        | Float64 _ -> TypeSymbol.Float64
        | True
        | False -> TypeSymbol.Bool
        | Char16 _ -> TypeSymbol.Char16
        | Utf16 _ -> TypeSymbol.Utf16
        | Array(elementTy, _) -> TypeSymbol.CreateArray(elementTy)
        | External(func) -> func.ReturnType
        | Error -> TypeSymbolError
        | TypeVariable(tyPar) ->
            let tyOpt =
                tyPar.Constraints
                |> ImArray.tryPick (function ConstraintSymbol.ConstantType(constTy) -> Some constTy.Value | _ -> None)
            match tyOpt with
            | Some ty -> ty
            | _ -> TypeSymbol.Error(Some tyPar, None)

    interface ISymbol

[<NoComparison;NoEquality;DebuggerDisplay("{Name}")>]
type ExtensionMemberSymbol =
    | Function of IFunctionSymbol
    | Property of IPropertySymbol

    member this.Enclosing =
        match this with
        | Function(func) -> func.Enclosing
        | Property(prop) -> prop.Enclosing

    member this.Id =
        match this with
        | Function(func) -> func.Id
        | Property(prop) -> prop.Id

    member this.FormalId =
        match this with
        | Function(func) -> func.Formal.Id
        | Property(prop) -> prop.Formal.Id

    member this.Name =
        match this with
        | Function(func) -> func.Name
        | Property(prop) -> prop.Name

    member this.Type =
        match this with
        | Function(func) -> func.Type
        | Property(prop) -> prop.Type

/// A value is a local, function, or field.
[<DebuggerDisplay("{Name}")>]
type IValueSymbol =
    inherit ISymbol

    abstract Enclosing : EnclosingSymbol

    abstract Name : string

    abstract Type : TypeSymbol

    abstract Id : int64

    abstract TypeParameters : TypeParameterSymbol imarray

    abstract TypeArguments : TypeArgumentSymbol imarray

    abstract IsFunction : bool

    abstract IsField : bool

    abstract IsProperty : bool

    abstract IsPattern : bool

    abstract FunctionFlags : FunctionFlags

    abstract MemberFlags : MemberFlags

    abstract ValueFlags: ValueFlags 

    abstract FunctionOverrides : IFunctionSymbol option

    abstract IsThis : bool

    abstract IsBase : bool

    abstract Formal : IValueSymbol

type ILocalSymbol =
    inherit IValueSymbol

[<Sealed>]
[<DebuggerDisplay("{Name}")>]
type LocalSymbol(name: string, ty: TypeSymbol, isGenerated, isMutable) =
    // REVIEW: I wonder how this will come back to haunt us....
    //         A local symbol whose is a type constructor is illegal, even for error recovery.
    //         We assume there is already an error reported if we encounter a type constructor here.
    // TODO: Remove this as we should never have to report this error.
    let ty =
        if ty.IsTypeConstructor then
            TypeSymbol.Error(None, Some "Internal Error: Type constructor is used illegally.")
        else
            ty
#if DEBUG || CHECKED
    do
        if not ty.IsError_t && ty.IsTypeConstructor then
            failwith "Unexpected type constructor."
#endif
    
    let id = newId ()
    let flags =
        if isGenerated then
            ValueFlags.Generated
        else
            ValueFlags.None
    let flags =
        if isMutable then
            flags ||| ValueFlags.Mutable
        else
            flags

    let mutable ty = ty

    member _.Type = ty
    member _.Name = name

    /// Mutability
    member _.SetType(newTy) =
        ty <- newTy

    interface ILocalSymbol with

        member _.Id = id

        member _.Enclosing = EnclosingSymbol.Local

        member _.Type = ty

        member _.Name = name

        member _.TypeParameters =
            match stripTypeEquations ty with
            | TypeSymbol.ForAll(tyPars, _) -> tyPars
            | _ -> ImArray.empty

        member _.TypeArguments =
            match stripTypeEquations ty with
            | TypeSymbol.ForAll(tyPars, _) ->
                tyPars
                |> ImArray.map (fun tyPar -> tyPar.AsType)
            | _ -> 
                ImArray.empty

        member _.IsField = false

        member _.IsFunction = false

        member _.MemberFlags = MemberFlags.None

        member _.FunctionFlags = FunctionFlags.None

        member _.FunctionOverrides = None

        member _.IsProperty = false

        member _.IsPattern = false

        member this.Formal = this :> IValueSymbol

        member _.ValueFlags = flags

        member _.IsThis = false

        member _.IsBase = false

[<Sealed>]
type LocalParameterSymbol(attrs, name: string, ty: TypeSymbol, isThis: bool, isBase: bool, isMutable) =
    // REVIEW: I wonder how this will come back to haunt us....
    //         A local symbol whose is a type constructor is illegal, even for error recovery.
    //         We assume there is already an error reported if we encounter a type constructor here.
    // TODO: Remove this as we should never have to report this error.
    let ty =
        if ty.IsTypeConstructor then
            TypeSymbol.Error(None, Some "Internal Error: Type constructor is used illegally.")
        else
            ty
#if DEBUG || CHECKED
    do
        if not ty.IsError_t && ty.IsTypeConstructor then
            failwith "Unexpected type constructor."
#endif
    let id = newId()

    let valueFlags =
        if isMutable then
            ValueFlags.Mutable ||| ValueFlags.Parameter
        else
            ValueFlags.Parameter
            
    let mutable attrs = attrs

    member _.IsThis = isThis

    member _.IsBase = isBase

    member _.Attributes = attrs

    /// Mutability - is this the only good way to handle this?
    member _.SetAttributes_Pass3_NonConcurrent(newAttrs) = attrs <- newAttrs

    interface ILocalParameterSymbol with

        member _.Attributes = attrs

        member _.IsThis = isThis

        member _.IsBase = isBase

        member _.Id = id

        member _.Enclosing = EnclosingSymbol.Local

        member _.Type = ty

        member _.Name = name

        member _.TypeParameters =
            match stripTypeEquations ty with
            | TypeSymbol.ForAll(tyPars, _) -> tyPars
            | _ -> ImArray.empty

        member _.TypeArguments =
            match stripTypeEquations ty with
            | TypeSymbol.ForAll(tyPars, _) ->
                tyPars
                |> ImArray.map (fun tyPar -> tyPar.AsType)
            | _ -> 
                ImArray.empty

        member _.IsField = false

        member _.IsFunction = false

        member _.MemberFlags = MemberFlags.None

        member _.FunctionFlags = FunctionFlags.None

        member _.FunctionOverrides = None

        member _.IsProperty = false

        member _.IsPattern = false

        member this.Formal = this :> IValueSymbol

        member _.ValueFlags = valueFlags

[<Sealed>]
type PolymorphicLocalSymbol(value: ILocalSymbol, ty: TypeSymbol, tyArgs: TypeSymbol imarray, isGenerated) =
    // REVIEW: I wonder how this will come back to haunt us....
    //         A local symbol whose is a type constructor is illegal, even for error recovery.
    //         We assume there is already an error reported if we encounter a type constructor here.
    // TODO: Remove this as we should never have to report this error.
    let ty =
        if ty.IsTypeConstructor then
            TypeSymbol.Error(None, Some "Internal Warning: Type constructor is used illegally.")
        else
            ty
#if DEBUG || CHECKED
    do
        if not ty.IsError_t && ty.IsTypeConstructor then
            failwith "Unexpected type constructor."
#endif
    
    let id = newId ()
    let flags =
        if isGenerated then
            ValueFlags.Generated
        else
            ValueFlags.None
    let flags =
        if value.IsMutable then
            flags ||| ValueFlags.Mutable
        else
            flags

    do
        OlyAssert.True(value.IsLocal)

    interface ILocalSymbol with

        member _.Id = id

        member _.Enclosing = EnclosingSymbol.Local

        member _.Type = ty

        member _.Name = value.Name

        member _.TypeParameters = value.TypeParameters

        member _.TypeArguments = tyArgs

        member _.IsField = false

        member _.IsFunction = false

        member _.MemberFlags = MemberFlags.None

        member _.FunctionFlags = FunctionFlags.None

        member _.FunctionOverrides = None

        member _.IsProperty = false

        member _.IsPattern = false

        member this.Formal = value

        member _.ValueFlags = flags

        member _.IsThis = false

        member _.IsBase = false

type ILocalParameterSymbol =
    inherit ILocalSymbol

    abstract Attributes: AttributeSymbol imarray

[<RequireQualifiedAccess>]
type ConstraintSymbol =
    | Null
    | Struct
    | NotStruct
    | Unmanaged
    | Blittable
    | Scoped
    | ConstantType of Lazy<TypeSymbol>
    | SubtypeOf of Lazy<TypeSymbol>

    member this.TryGetSubtypeOf() =
        match this with
        | ConstantType(lazyTy)
        | SubtypeOf(lazyTy) ->
            ValueSome lazyTy.Value
        | _ ->
            ValueNone

    interface ISymbol

[<NoComparison;RequireQualifiedAccess>]
type TypeParameterKind =
    | Type
    | Function of index: int

[<DebuggerDisplay("{DebugName}")>]
[<Sealed>]
type TypeParameterSymbol private (id: int64, name: string, index: int, arity: int, isVariadic: bool, kind: TypeParameterKind, constrs: ConstraintSymbol imarray ref, hiddenLinkOpt: TypeParameterSymbol option) as this =

    let ty = TypeSymbol.Variable(this)
    let mutable constrs = constrs

    let higherTyPars =
        if arity > 0 then
            ImArray.init arity (fun i -> TypeParameterSymbol("_", i, 0, TypeParameterKind.Type, ref ImArray.empty))
        else
            ImArray.empty

    do
        if arity < 0 then
            failwith "Internal error: Arity must not be below zero."

    member _.Id = id

    member _.Name = name

    member _.DisplayName = 
        name + 
        (if arity = 0 then "" 
         else "<" + (Seq.init arity (fun _ -> "_") |> String.concat ", ") + ">"
        )

    member this.DebugName = 
        this.DisplayName + " - " + id.ToString()

    member _.Constraints = constrs.contents

    member _.Arity = arity

    member _.HasArity = arity > 0

    member _.IsVariadic = isVariadic

    member _.Kind = kind

    member this.AsType = ty

    member _.Index = index

    member _.IsHidden = hiddenLinkOpt.IsSome

    member _.HiddenLink = hiddenLinkOpt

    member _.HigherTypeParameters = higherTyPars

    /// Mutability
    member this.SetConstraints(newConstrs) =
        constrs.contents <- newConstrs

    member this.CreateHiddenLink(name, index, arity, isVariadic, kind) =
        // This does not handle constraints very well.
        // The type parameter and its link share the exact same constraints which means
        // that the types of constraints are not reflected properly in the link.
        // We share this because inference can pick up more constraints.
        // TODO/REVIEW: What should we do here? Not allow local types in a generic-inference-only context? Probably.
        //              If we do that, then we can create a new version of the constraints here since it will be
        //              fully realized.
        TypeParameterSymbol(newId(), name, index, arity, isVariadic, kind, constrs, Some this)

    static member ReasonablyEquals(tyPar1: TypeParameterSymbol, tyPar2: TypeParameterSymbol) =
        let isSameIndex =
            match tyPar1.Kind, tyPar2.Kind with
            | TypeParameterKind.Type, TypeParameterKind.Type -> tyPar1.Index = tyPar2.Index
            | TypeParameterKind.Function index1, TypeParameterKind.Function index2 -> index1 = index2
            | _ -> false
        if isSameIndex then
            tyPar1.Arity = tyPar2.Arity && tyPar1.IsVariadic = tyPar2.IsVariadic
        else
            false

    interface ISymbol

    new(name: string, index: int, arity: int, isVariadic: bool, kind: TypeParameterKind, constrs: ConstraintSymbol imarray ref) =
        TypeParameterSymbol(newId(), name, index, arity, isVariadic, kind, constrs, None)

    new(name: string, index: int, arity: int, kind: TypeParameterKind, constrs: ConstraintSymbol imarray ref) =
        TypeParameterSymbol(name, index, arity, false, kind, constrs)

type TypeArgumentSymbol = TypeSymbol

[<ReferenceEquality;NoComparison>]
type WitnessSymbol =
    | TypeExtension of tyExt: EntitySymbol * specificAbstractFunc: IFunctionSymbol option
    | TypeParameter of tyPar: TypeParameterSymbol
    | Type of TypeSymbol

[<Sealed>]
type WitnessSolution (tyPar: TypeParameterSymbol, ent: EntitySymbol, funcOpt: IFunctionSymbol option) =

#if DEBUG || CHECKED
    do
        match funcOpt with
        | Some func ->
            OlyAssert.Equal(ent.Formal.Id, func.Enclosing.TryEntity.Value.Formal.Id)
        | _ ->
            ()
#endif

    member _.TypeParameter = tyPar
    
    member _.Entity = ent

    member _.Function = funcOpt // For shape members

    member val Solution : WitnessSymbol option = None with get, set

    member this.HasSolution = this.Solution.IsSome

    interface ISymbol

[<Sealed>]
type VariableSolutionSymbol (isTyOfParameter: bool, isTyCtor: bool) =

    let id = newId ()

    let constrs = ResizeArray<ConstraintSymbol>()
    let mutable solutionState: TypeSymbol = Unchecked.defaultof<_> // We are not using option for perf reasons.

    member this.Id = id

    member this.Solution

        with get(): TypeSymbol = 
            solutionState
        and set (value: TypeSymbol) = 
#if DEBUG || CHECKED
            if not value.IsError_t then
                if isTyCtor then
                    if not value.IsTypeConstructor then
                        failwith "Expected type constructor."
                    if value.TypeParameters |> ImArray.exists (fun x -> x.HasArity) then
                        failwith "Unexpected type parameter with arity."

            // Sanity check to make sure we do not create a circular reference. Not bullet-proof though.
            match value with
            | TypeSymbol.InferenceVariable(_, solution) ->
                OlyAssert.False(obj.ReferenceEquals(this, solution))
            | TypeSymbol.HigherInferenceVariable(_, _, externalSolution, solution) ->
                OlyAssert.False(obj.ReferenceEquals(this, externalSolution))
                OlyAssert.False(obj.ReferenceEquals(this, solution))
            | _ ->
                ()
#endif
            solutionState <- value

    member this.HasSolution = 
        obj.ReferenceEquals(solutionState, null)
        |> not

    member this.Constraints = 
        constrs :> _ seq

    // Mutability
    member this.ForceAddConstraint constr =
        constrs.Add(constr)

    member this.IsTypeOfParameter = isTyOfParameter

    member this.IsTypeConstructor = isTyCtor

    interface ISymbol

[<RequireQualifiedAccess>]
type ByRefKind =
    | ReadWrite
    | Read

let private FormalFunctionTypeParameters =
    let returnTy = TypeParameterSymbol("TReturn", 0, 0, TypeParameterKind.Type, ref ImArray.empty)
    let argsTy = TypeParameterSymbol("TArguments", 1, 0, true, TypeParameterKind.Type, ref ImArray.empty)
    ImArray.createTwo returnTy argsTy

let private FormalNormalFunctionType =
    let tyPars = FormalFunctionTypeParameters
    TypeSymbol.Function(VariadicTypeArguments(ImArray.createOne tyPars[0].AsType, ImArray.empty), tyPars[1].AsType, FunctionKind.Normal)

let private FormalScopedFunctionType =
    let tyPars = FormalFunctionTypeParameters
    TypeSymbol.Function(VariadicTypeArguments(ImArray.createOne tyPars[0].AsType, ImArray.empty), tyPars[1].AsType, FunctionKind.Scoped)

let private FormalRefCellTypeParameters =
    TypeParameterSymbol("T", 0, 0, TypeParameterKind.Type, ref ImArray.empty)
    |> ImArray.createOne

let private FormalRefCellType =
    TypeSymbol.RefCell(FormalRefCellTypeParameters[0].AsType)

let private FormalNativePtrTypeParameters =
    TypeParameterSymbol("T", 0, 0, TypeParameterKind.Type, ref ImArray.empty)
    |> ImArray.createOne

let private FormalNativePtrType =
    TypeSymbol.NativePtr(FormalNativePtrTypeParameters[0].AsType)

[<RequireQualifiedAccess>]
module private FormalArray =

    let TypeParameters =
        TypeParameterSymbol("T", 0, 0, TypeParameterKind.Type, ref ImArray.empty)
        |> ImArray.createOne

    let private comparer =
        { new IEqualityComparer<struct(int32 * ArrayKind)> with

            member this.GetHashCode((rank, _)) = 
                rank

            member this.Equals((rank1, kind1), (rank2, kind2)) =
                rank1 = rank2 && kind1 = kind2
        }

    let private lockObj = obj()
    let private cache = System.Collections.Concurrent.ConcurrentDictionary(comparer)

    let Get(rank: int32, kind: ArrayKind) : TypeSymbol =
        let key = struct(rank, kind)
        match cache.TryGetValue key with
        | true, formalTy -> formalTy
        | _ ->
            lock lockObj (fun () ->
                match cache.TryGetValue key with
                | true, formalTy -> formalTy
                | _ ->
                    let formalTy = TypeSymbol.Array(TypeParameters[0].AsType, rank, kind)
                    cache[key] <- formalTy
                    formalTy
            )

[<RequireQualifiedAccess>]
module private FormalNativeFunctionPtr =

    let TypeParameters =
        let returnTy = TypeParameterSymbol("TReturn", 0, 0, TypeParameterKind.Type, ref ImArray.empty)
        let argsTy = TypeParameterSymbol("TArguments", 1, 0, true, TypeParameterKind.Type, ref ImArray.empty)
        ImArray.createTwo returnTy argsTy

    let private lockObj = obj()
    let private cache = System.Collections.Concurrent.ConcurrentDictionary()

    let Get(ilCallConv) : TypeSymbol =
        let key = ilCallConv
        match cache.TryGetValue key with
        | true, formalTy -> formalTy
        | _ ->
            lock lockObj (fun () ->
                match cache.TryGetValue key with
                | true, formalTy -> formalTy
                | _ ->
                    let formalTy = TypeSymbol.NativeFunctionPtr(ilCallConv, VariadicTypeArguments(ImArray.createOne TypeParameters[0].AsType, ImArray.empty), TypeParameters[1].AsType)
                    cache[key] <- formalTy
                    formalTy
            )

let private FormalTupleTypeParameters: TypeParameterSymbol imarray =
    let tyPar = TypeParameterSymbol("TElements", 0, 0, true, TypeParameterKind.Type, ref ImArray.empty)
    ImArray.createOne tyPar

let private FormalTupleType =
    TypeSymbol.Tuple(ImArray.createOne FormalTupleTypeParameters[0].AsType, ImArray.empty)

let private FormalDependentIndexerTypeParameters =
    let inputValueTyPar = TypeParameterSymbol("T", 0, 0, TypeParameterKind.Type, ref ImArray.empty)
    let innerTyPar = TypeParameterSymbol("N", 1, 0, TypeParameterKind.Type, ref ImArray.empty)
    ImArray.createTwo inputValueTyPar innerTyPar

let private FormalDependentIndexerType =
    TypeSymbol.DependentIndexer(FormalDependentIndexerTypeParameters[0].AsType, FormalDependentIndexerTypeParameters[1].AsType)

let private ByReferenceTypeParameters = TypeParameterSymbol("T", 0, 0, TypeParameterKind.Type, ref ImArray.empty) |> ImArray.createOne
let private FormalReadWriteByRef = TypeSymbol.ByRef(ByReferenceTypeParameters.[0].AsType, ByRefKind.ReadWrite)
let private FormalReadByRef = TypeSymbol.ByRef(ByReferenceTypeParameters.[0].AsType, ByRefKind.Read)

let TypeSymbolError =
    TypeSymbol.Error(None, None)

[<RequireQualifiedAccess>]
type ArrayKind =
    | Immutable
    | Mutable

[<RequireQualifiedAccess>]
type TypeModifier =
    | Int32 of name: string * value: int32

[<RequireQualifiedAccess>]
type FunctionKind =
    | Normal
    | Scoped

type VariadicTypeArguments =
    | VariadicTypeArguments of parTys: TypeSymbol imarray * parNames: string imarray

    member this.Types =
        match this with
        | VariadicTypeArguments(tys, _) -> tys

    member this.Length = this.Types.Length

[<RequireQualifiedAccess;DebuggerDisplay("{DebugName}")>]
type TypeSymbol =
    | BaseObject

    | Void
    | Unit
    | Int8
    | UInt8
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    | Float32
    | Float64
    | Bool
    | Char16
    | Utf16
    | ConstantInt32 of int32
    | ByRef of TypeSymbol * ByRefKind
    | NativeInt
    | NativeUInt
    | NativePtr of elementTy: TypeSymbol
    | NativeFunctionPtr of OlyILCallingConvention * inputTys: VariadicTypeArguments * returnTy: TypeSymbol
    | Array of elementTy: TypeSymbol * rank: int * kind: ArrayKind
    | Entity of ent: EntitySymbol
    | Tuple of elementTys: TypeArgumentSymbol imarray * elementNames: string imarray
    | RefCell of contentTy: TypeSymbol
    | Function of inputTys: VariadicTypeArguments * returnTy: TypeSymbol * kind: FunctionKind
    | ForAll of tyPars: ImmutableArray<TypeParameterSymbol> * innerTy: TypeSymbol
    | Variable of TypeParameterSymbol
    | HigherVariable of TypeParameterSymbol * tyArgs: TypeArgumentSymbol imarray
    | InferenceVariable of tyParOpt: TypeParameterSymbol option * solution: VariableSolutionSymbol
    | HigherInferenceVariable of tyParOpt: TypeParameterSymbol option * tyArgs: TypeArgumentSymbol imarray * externalSolution: VariableSolutionSymbol * solution: VariableSolutionSymbol

    /// An eager inference variable type is a type that will solve itself with the specified 'eagerTy' if it was not able be solved in flexible unificiation the first time.
    | EagerInferenceVariable of solution: VariableSolutionSymbol * eagerTy: TypeSymbol

    | DependentIndexer of inputValueTy: TypeSymbol * innerTy: TypeSymbol

    | Error of tyParOpt: TypeParameterSymbol option * msgOpt: string option // 'msgOpt' will display an error message in post inference checks

    static member GetFormalByRef(kind) =
        match kind with
        | ByRefKind.ReadWrite ->
            FormalReadWriteByRef
        | ByRefKind.Read ->
            FormalReadByRef

    static member CreateByRef(elementTy, kind) =
        ByRef(mkSolvedInferenceVariableType ByReferenceTypeParameters.[0] elementTy, kind)

    static member CreateInferenceVariable(tyParOpt: TypeParameterSymbol option, solution: VariableSolutionSymbol) =
#if DEBUG || CHECKED
        if solution.IsTypeConstructor then
            match tyParOpt with
            | Some tyPar -> OlyAssert.True(tyPar.HasArity)
            | _ -> OlyAssert.Fail("Expected type parameter with arity.")
        else
            match tyParOpt with
            | Some tyPar -> OlyAssert.False(tyPar.HasArity)
            | _ -> ()
#endif
        TypeSymbol.InferenceVariable(tyParOpt, solution)

    static member CreateHigherInferenceVariable(tyParOpt, tyArgs, externalSolution, solution) =
#if DEBUG || CHECKED
        match tyParOpt with
        | Some (tyPar: TypeParameterSymbol) ->
            OlyAssert.True(tyPar.HasArity)
        | _ ->
            ()

        tyArgs
        |> ImArray.iter (fun tyArg ->
            OlyAssert.False(tyArg.IsTypeConstructor)
        )
#endif
        OlyAssert.True(externalSolution.IsTypeConstructor)
        TypeSymbol.HigherInferenceVariable(tyParOpt, tyArgs, externalSolution, solution)

    member this.ToInstantiation() =
        applyType this this.TypeArguments

    member this.IsBuiltIn =
        match stripTypeEquations this with
        | BaseObject
        | Void
        | Unit
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32
        | Int64
        | UInt64
        | Float32
        | Float64
        | Bool
        | Char16
        | Utf16
        | ConstantInt32 _
        | ByRef _
        | NativeInt
        | NativeUInt
        | NativePtr _
        | NativeFunctionPtr _
        | Array _
        | Tuple _
        | RefCell _
        | Function _
        | DependentIndexer _ -> true
        | _ -> false

    member this.IsEagerInferenceVariable_t =
        match stripTypeEquations this with
        | EagerInferenceVariable _ -> true
        | _ -> false

    member this.IsOrWasInferenceVariable =
        match this with
        | EagerInferenceVariable _ 
        | InferenceVariable _
        | HigherInferenceVariable _ -> true
        | _ -> false

    member this.IsRealUnit =
        match stripTypeEquations this with
        | TypeSymbol.Tuple(tyArgs, _) when tyArgs.Length = 1 ->
            (match stripTypeEquations tyArgs.[0] with TypeSymbol.Unit -> true | _ -> false)
        | _ ->
            match this.TryImmedateTypeParameter with
            | ValueSome _ when this.IsUnit_t -> true
            | _ -> false

    member this.Formal =
        match stripTypeEquationsExceptAlias this with
        | TypeSymbol.Entity(ent) -> TypeSymbol.Entity(ent.Formal)
        | TypeSymbol.ByRef(_, kind) -> TypeSymbol.GetFormalByRef(kind)
        | TypeSymbol.Function(kind=FunctionKind.Normal) -> FormalNormalFunctionType
        | TypeSymbol.Function(kind=FunctionKind.Scoped) -> FormalScopedFunctionType
        | TypeSymbol.NativeFunctionPtr(ilCallConv, _, _) -> FormalNativeFunctionPtr.Get(ilCallConv)
        | TypeSymbol.RefCell _ -> FormalRefCellType
        | TypeSymbol.NativePtr _ -> FormalNativePtrType
        | TypeSymbol.Array(_, rank, kind) -> FormalArray.Get(rank, kind)
        | TypeSymbol.Tuple _ -> FormalTupleType
        | TypeSymbol.DependentIndexer _ -> FormalDependentIndexerType
        | TypeSymbol.HigherVariable(tyPar, _) ->
            tyPar.AsType
        | TypeSymbol.HigherInferenceVariable(tyParOpt=tyParOpt;externalSolution=externalSolution) ->
            // TODO: There a way to do this without allocating? May not actually be a problem,
            //       but generally we try to avoid allocations for properties that are commonly hit.
            TypeSymbol.CreateInferenceVariable(tyParOpt, externalSolution)
        | _ -> 
            this // TODO: What other cases we need to handle?

    member this.Name =
        match this with
        | Unit -> "__oly_unit"
        | Void -> "__oly_void"
        | Int8 -> "__oly_int8"
        | UInt8 -> "__oly_uint8"
        | Int16 -> "__oly_int16"
        | UInt16 -> "__oly_uint16"
        | Int32 -> "__oly_int32"
        | UInt32 -> "__oly_uint32"
        | Int64 -> "__oly_int64"
        | UInt64 -> "__oly_uint64"
        | Float32 -> "__oly_float32"
        | Float64 -> "__oly_float64"
        | Bool -> "__oly_bool"
        | Char16 -> "__oly_char16"
        | Utf16 -> "__oly_utf16"
        | Entity(ent) -> ent.Name
        | Tuple _ -> "__oly_tuple"
        | RefCell _ -> "__oly_ref_cell"
        | Function(kind=kind) -> 
            match kind with
            | FunctionKind.Scoped ->
                "__oly_scoped_function"
            | _ ->
                "__oly_function"
        | ByRef(_, kind) ->
            match kind with
            | ByRefKind.ReadWrite -> "__oly_read_write_by_ref"
            | ByRefKind.Read -> "__oly_read_by_ref"
        | ForAll(_, innerTy) -> innerTy.Name
        | Variable(tyPar) -> tyPar.Name
        | HigherVariable(tyPar, _) -> tyPar.Name
        | InferenceVariable(solution=solution) ->
            if solution.HasSolution then
                solution.Solution.Name
            else
                "?"
        | HigherInferenceVariable(externalSolution=externalSolution;solution=solution) ->
            if solution.HasSolution then
                solution.Solution.Name
            else
                if externalSolution.HasSolution then
                    externalSolution.Solution.Name
                else
                    "?"
        | ConstantInt32 n -> n.ToString()
        | BaseObject -> "__oly_object"
        | NativeInt -> "__oly_native_int"
        | NativeUInt -> "__oly_native_uint"
        | NativePtr _ -> "__oly_native_ptr"
        | NativeFunctionPtr _ -> "__oly_native_function_ptr"
        | Array _ -> "__oly_array"
        | EagerInferenceVariable(solution, eagerTy) ->
            if solution.HasSolution then
                solution.Solution.Name
            else
                "?"
        | DependentIndexer(_, formalTy) -> "!!dependent!!" + formalTy.Name // TODO:
        | Error _ -> 
            "?"

    /// Returns false for NativeInt or NativeUInt
    member this.IsFixedInteger =
        match stripTypeEquations this with
        | TypeSymbol.UInt8
        | TypeSymbol.Int8
        | TypeSymbol.UInt16
        | TypeSymbol.Int16
        | TypeSymbol.UInt32
        | TypeSymbol.Int32
        | TypeSymbol.UInt64
        | TypeSymbol.Int64 -> true
        | _ -> false

    member this.IsInteger =
        match stripTypeEquations this with
        | TypeSymbol.UInt8
        | TypeSymbol.Int8
        | TypeSymbol.UInt16
        | TypeSymbol.Int16
        | TypeSymbol.UInt32
        | TypeSymbol.Int32
        | TypeSymbol.UInt64
        | TypeSymbol.Int64 
        | TypeSymbol.NativeInt
        | TypeSymbol.NativeUInt -> true
        | _ -> false

    /// Is the type a float32 or float64?
    member this.IsReal =
        match stripTypeEquations this with
        | TypeSymbol.Float32
        | TypeSymbol.Float64 -> true
        | _ -> false

    member this.DebugName = 
        if this.TypeArguments.IsEmpty && not(this.IsTypeConstructor) then
            this.Name
        else
            let prefix =
                if this.IsTypeConstructor then
                    "(type constructor) "
                else
                    String.Empty    
            prefix + this.Name + "<" + (this.TypeArguments |> Seq.map (fun x -> if this.IsTypeConstructor then "_" else x.DebugName) |> String.concat ", ") + ">"

    member this.Arity =
        match stripTypeEquationsExceptAlias this with
        | Variable(tyPar)
        | HigherVariable(tyPar, _)
        | InferenceVariable(Some tyPar, _)
        | HigherInferenceVariable(Some tyPar, _, _, _)
        | Error(tyParOpt = Some tyPar) -> tyPar.Arity
        | Entity(ent) -> ent.TypeParameters.Length
        | Tuple(tyArgs, _) -> tyArgs.Length
        | ForAll(tyPars, _) -> 
#if DEBUG || CHECKED
            OlyAssert.False(tyPars.IsEmpty)
#endif
            tyPars.Length
        | Function _
        | NativeFunctionPtr _ -> 2
        | ByRef _ 
        | NativePtr _ -> 1
        | Array _ -> 1
        | _ -> 0

    member this.LogicalArity =
        match stripTypeEquationsExceptAlias this with
        | Variable(tyPar)
        | HigherVariable(tyPar, _)
        | InferenceVariable(Some tyPar, _)
        | HigherInferenceVariable(Some tyPar, _, _, _)
        | Error(tyParOpt = Some tyPar) -> tyPar.Arity
        | Entity(ent) -> ent.LogicalTypeParameterCount
        | Tuple(tyArgs, _) -> tyArgs.Length
        | ForAll(tyPars, _) -> 
#if DEBUG || CHECKED
            OlyAssert.False(tyPars.IsEmpty)
#endif
            tyPars.Length
        | Function _
        | NativeFunctionPtr _ -> 2
        | ByRef _ 
        | NativePtr _ -> 1
        | Array _ -> 1
        | _ -> 0

    member this.TypeParameters: TypeParameterSymbol imarray =
        match stripTypeEquationsExceptAlias this with
        | Unit
        | Void
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32
        | Int64
        | UInt64
        | Float32
        | Float64
        | Bool
        | Char16
        | Utf16 
        | ConstantInt32 _
        | Error _ 
        | BaseObject
        | NativeInt
        | NativeUInt
        | EagerInferenceVariable _ -> ImArray.empty
        | RefCell _ -> FormalRefCellTypeParameters
        | Function _ -> FormalFunctionTypeParameters
        | ForAll(tyPars, _) -> tyPars
        | ByRef _ -> ByReferenceTypeParameters
        | Entity(ent) -> ent.TypeParameters
        | Array _ -> FormalArray.TypeParameters
        | Tuple _ -> FormalTupleTypeParameters
        | NativePtr _ -> FormalNativePtrTypeParameters
        | NativeFunctionPtr _ -> FormalFunctionTypeParameters
        | DependentIndexer _ -> FormalDependentIndexerTypeParameters

        | Variable(tyPar)
        | HigherVariable(tyPar, _)
        | InferenceVariable(Some tyPar, _) 
        | HigherInferenceVariable(Some tyPar, _, _, _) ->
            tyPar.HigherTypeParameters
        | InferenceVariable _ ->
            ImArray.empty
        | HigherInferenceVariable(_, tyArgs, _, _) ->
            // REVIEW: Perhaps higher inference variables ALWAYS need a type parameter associated with.
            tyArgs |> ImArray.mapi (fun i _ -> TypeParameterSymbol("_", i, 0, TypeParameterKind.Type, ref ImArray.empty))

    member this.TypeArguments : TypeArgumentSymbol imarray =
        match stripTypeEquationsExceptAlias this with
        | Unit
        | Void
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32
        | Int64
        | UInt64
        | Float32
        | Float64
        | Bool
        | Char16
        | Utf16 
        | ConstantInt32 _ 
        | Error _ 
        | BaseObject
        | NativeInt
        | NativeUInt
        | EagerInferenceVariable _ -> ImArray.empty
        | RefCell(elementTy) ->
            ImArray.createOne elementTy

        | Function(inputTys, returnTy, _)
        | NativeFunctionPtr(_, inputTys, returnTy) ->
            ImArray.appendOne returnTy inputTys.Types
                
        | HigherInferenceVariable(_, tyArgs, _, _)
        | HigherVariable(_, tyArgs) -> tyArgs
        | ForAll(tyPars, _) ->
            tyPars
            |> Seq.map (fun tyPar -> tyPar.AsType)
            |> ImArray.ofSeq
        | ByRef(ty, _) -> ImArray.createOne ty
        | Entity(ent) -> ent.TypeArguments
        | Tuple(tyArgs, _) -> tyArgs
        | Array(elementTy, _, _) -> ImArray.createOne elementTy
        | NativePtr(elementTy) -> ImArray.createOne elementTy
        | DependentIndexer(inputValueTy, innerTy) -> ImArray.createTwo inputValueTy innerTy

        | Variable(tyPar)
        | InferenceVariable(Some tyPar, _) ->
            tyPar.HigherTypeParameters
            |> ImArray.map (fun x -> x.AsType)
        | InferenceVariable _ ->
            ImArray.empty

    member this.FormalId =
        match stripTypeEquationsExceptAlias this with
        | Error _ -> 0L
        | Unit -> 1
        | Void -> 2
        | Int8 -> 3
        | UInt8 -> 4
        | Int16 -> 5
        | UInt16 -> 6
        | Int32 -> 7
        | UInt32 -> 8
        | Int64 -> 9
        | UInt64 -> 10
        | Float32 -> 11
        | Float64 -> 12
        | Bool -> 13
        | Char16 -> 14
        | Utf16 -> 15
        | Function _ -> 16
        | ForAll _ -> 17
        | Variable _ -> 18
        | HigherVariable _ -> 19
        | InferenceVariable(tyParOpt, solution)
        // TODO/REVIEW: Maybe we want to do something special for inference variable with kind?
        | HigherInferenceVariable(tyParOpt, _, solution, _) ->
            if solution.HasSolution then
                solution.Solution.FormalId
            else
                match tyParOpt with
                | Some tyPar -> tyPar.AsType.FormalId
                | _ -> 20
        | Tuple _ -> 21
        | RefCell _ -> 22
        | ConstantInt32 _ -> 23
        | BaseObject -> 24
        | ByRef _ -> 25
        | NativeInt -> 26
        | NativeUInt -> 27
        | NativePtr _ -> 28
        | Array _ -> 29
        | EagerInferenceVariable(_, eagerTy) -> eagerTy.FormalId
        | NativeFunctionPtr _ -> 39
        | DependentIndexer _ -> 40
        | Entity(ent) -> ent.Formal.Id

    member this.Enclosing =
        match this with
        | Unit
        | Void
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32 
        | Int64
        | UInt64
        | Float32 
        | Float64 
        | Bool
        | Char16 
        | Utf16 
        | Function _ 
        | ForAll _
        | Variable _ 
        | HigherVariable _
        | InferenceVariable _
        | HigherInferenceVariable _
        | Tuple _ 
        | RefCell _
        | ConstantInt32 _ 
        | BaseObject
        | ByRef _
        | NativeInt
        | NativeUInt
        | NativePtr _
        | NativeFunctionPtr _
        | Array _
        | EagerInferenceVariable _
        | DependentIndexer _
        | Error _ -> EnclosingSymbol.RootNamespace
        | Entity(ent) -> ent.Enclosing

    /// TODO: Rename to 'IsAnyTypeVariable'
    member this.IsTypeVariable =
        match stripTypeEquations this with
        | Variable _
        | HigherVariable _ -> true
        | _ -> false

    member this.IsAnyTypeVariableWithNotStructConstraint =
        match stripTypeEquations this with
        | Variable(tyPar)
        | HigherVariable(tyPar, _) ->
            tyPar.Constraints
            |> ImArray.exists (function
                | ConstraintSymbol.NotStruct -> true
                | _ -> false
            )
        | _ -> false

    member this.IsAnyTypeVariableWithoutStructOrUnmanagedOrNotStructConstraint =
        match stripTypeEquations this with
        | Variable(tyPar)
        | HigherVariable(tyPar, _) ->
            tyPar.Constraints
            |> ImArray.exists (function
                | ConstraintSymbol.Struct
                | ConstraintSymbol.Unmanaged
                | ConstraintSymbol.Blittable
                | ConstraintSymbol.NotStruct -> true
                | _ -> false
            )
            |> not
        | _ -> false

    member this.IsAnyNonStruct =
        (not this.IsAnyStruct) || this.IsAnyTypeVariableWithNotStructConstraint

    member this.IsTypeConstructor =
        this.Arity > 0 && this.IsFormal

    member this.TryEnumUnderlyingType =
        match stripTypeEquations this with
        | Entity(ent) -> ent.TryEnumUnderlyingType
        | _ -> None

    member this.Inherits =
        match stripTypeEquations this with
        | Entity(ent) -> ent.Extends
        | _ -> ImArray.empty

    member this.Implements =
        match stripTypeEquations this with
        | Entity(ent) -> ent.Implements
        | Variable(tyPar)
        | HigherVariable(tyPar, _)
        | InferenceVariable(Some tyPar, _)
        | HigherInferenceVariable(Some tyPar, _, _, _) -> 
            tyPar.Constraints
            |> ImArray.choose (function
                | ConstraintSymbol.Null
                | ConstraintSymbol.Struct
                | ConstraintSymbol.NotStruct 
                | ConstraintSymbol.Unmanaged
                | ConstraintSymbol.Blittable
                | ConstraintSymbol.Scoped
                | ConstraintSymbol.ConstantType _ -> None
                | ConstraintSymbol.SubtypeOf(ty) -> Some ty.Value
            )
        | _ -> ImArray.empty

    // TODO: Rename this to TryEntityNoAlias.
    member this.TryEntity: EntitySymbol voption =
        match stripTypeEquations this with
        | Entity(ent) -> ValueSome(ent)
        | _ -> ValueNone

    member this.AsEntity: EntitySymbol =
        match stripTypeEquationsExceptAlias this with
        | Entity(ent) -> ent
        | _ -> OlyAssert.Fail("Expected type to be an entity.")

    member this.AsEntityNoAlias: EntitySymbol =
        match stripTypeEquations this with
        | Entity(ent) -> ent
        | _ -> OlyAssert.Fail("Expected type to be an entity.")

    member this.TryTypeParameter =
        match stripTypeEquations this with
        | Variable(tyPar)
        | HigherVariable(tyPar, _)
        | InferenceVariable(Some tyPar, _)
        | HigherInferenceVariable(Some tyPar, _, _, _)
        | Error(Some tyPar, _) -> ValueSome tyPar
        | _ -> ValueNone

    // TODO: Rename to 'TryImmediateTypeParameter'.
    /// Try to get a type parameter without stripping the type.
    member this.TryImmedateTypeParameter =
        match this with
        | Variable(tyPar)
        | HigherVariable(tyPar, _)
        | InferenceVariable(Some tyPar, _)
        | HigherInferenceVariable(Some tyPar, _, _, _)
        | Error(Some tyPar, _) -> ValueSome tyPar
        | _ -> ValueNone

    /// Try to get a type parameter from an inference variable without stripping the type.
    member this.TryImmediateInferenceVariableTypeParameter =
        match this with
        | InferenceVariable(Some tyPar, _)
        | HigherInferenceVariable(Some tyPar, _, _, _) -> ValueSome tyPar
        | _ -> ValueNone

    member this.HasTypeParameter =
        this.TryTypeParameter.IsSome

    /// TODO: Rename to "IsAnyFunction".
    member this.IsFunction_t =
        match stripTypeEquations this with
        | Function _ -> true
        | NativeFunctionPtr _ -> true
        | ForAll(_, TypeSymbol.Function _) -> true
        | _ -> false

    member this.IsScopedFunction =
        match stripTypeEquations this with
        | Function(kind=FunctionKind.Scoped)
        | ForAll(_, TypeSymbol.Function(kind=FunctionKind.Scoped)) -> true
        | _ -> false

    member this.IsScoped =
        match stripTypeEquations this with
        | ByRef _ -> true
        | Entity(ent) -> ent.Flags.HasFlag(EntityFlags.Scoped)
        | Variable(tyPar)
        | HigherVariable(tyPar, _) ->
            tyPar.Constraints
            |> ImArray.exists (function
                | ConstraintSymbol.Scoped -> true
                | _ -> false
            )
        | this -> this.IsScopedFunction

    member this.IsNativeFunctionPtr_t =
        match stripTypeEquations this with
        | NativeFunctionPtr _ -> true
        | _ -> false

    member this.IsUnit_t =
        match stripTypeEquations this with
        | TypeSymbol.Unit -> true
        | _ -> false

    member this.IsVoid_t =
        match stripTypeEquations this with
        | TypeSymbol.Void -> true
        | _ -> false

    member this.IsError_t =
        match stripTypeEquations this with
        | Error _ -> true
        | _ -> false

    member this.HasTypeVariableArity =
        match stripTypeEquations this with
        | Variable(tyPar) -> tyPar.HasArity
        | HigherVariable _ -> true
        | _ -> false

    member this.IsQuantifiedFunction =
        match stripTypeEquations this with
        | ForAll(_, ty) -> ty.IsFunction_t
        | _ -> false

    member this.IsTypeExtension =
        match this.TryEntity with
        | ValueSome ent -> ent.IsTypeExtension
        | _ -> false

    member this.IsEnum =
        match this.TryEntity with
        | ValueSome ent -> ent.IsEnum
        | _ -> false

    member this.IsShape =
        match this.TryEntity with
        | ValueSome(ent) -> ent.IsShape
        | _ -> false

    member this.IsAnonymous =
        match this.TryEntity with
        | ValueSome(ent) -> ent.IsAnonymous
        | _ -> false

    member this.IsAnonymousShape =
        match this.TryEntity with
        | ValueSome(ent) -> ent.IsAnonymousShape
        | _ -> false

    member this.IsInterface =
        match this.TryEntity with
        | ValueSome ent -> ent.IsInterface
        | _ -> false

    member this.IsAbstract =
        match this.TryEntity with
        | ValueSome ent -> ent.IsAbstract
        | _ -> false

    /// Returns true if the type is a class type.
    member this.IsClass =
        match stripTypeEquations this with
        | Entity(ent) -> ent.IsClass
        | BaseObject 
        | Utf16
        | RefCell _
        | Tuple _ 
        | Array _ 
        | Function _ -> true
        | _ -> false

    member this.IsAlias: bool =
        match stripTypeEquationsExceptAlias this with
        | Entity(ent) -> ent.IsAlias
        | _ -> false

    member this.IsAliasAndNotCompilerIntrinsic =
        this.IsAlias && (not this.AsEntity.IsCompilerIntrinsic)

    member this.IsClosure =
        match stripTypeEquations this with
        | Entity(ent) -> ent.IsClosure
        | _ -> false

    member this.IsModule =
        match this.TryEntity with
        | ValueSome ent -> ent.IsModule
        | _ -> false

    member this.IsByRef_t =
        match stripTypeEquations this with
        | TypeSymbol.ByRef _ -> true
        | _ -> false

    member this.IsNativePtr_t =
        match stripTypeEquations this with
        | TypeSymbol.NativePtr _ -> true
        | _ -> false

    member this.IsAnyArray =
        match stripTypeEquations this with
        | TypeSymbol.Array _ -> true
        | _ -> false

    member this.IsMutableArray_t =
        match stripTypeEquations this with
        | TypeSymbol.Array(_, _, ArrayKind.Mutable) -> true
        | _ -> false

    member this.IsAnyTuple =
        match stripTypeEquations this with
        | TypeSymbol.Tuple _ -> true
        | _ -> false

    member this.IsAnyPtr =
        match stripTypeEquations this with
        | TypeSymbol.NativePtr _
        | TypeSymbol.NativeFunctionPtr _ -> true
        | _ -> false

    member this.IsRefCell_t =
        match stripTypeEquations this with
        | TypeSymbol.RefCell _ -> true
        | _ -> false

    member this.IsReadOnlyByRef =
        match stripTypeEquations this with
        | TypeSymbol.ByRef(_, ByRefKind.Read) -> true
        | _ -> false

    member this.IsReadWriteByRef =
        match stripTypeEquations this with
        | TypeSymbol.ByRef(_, ByRefKind.ReadWrite) -> true
        | _ -> false

    member this.TryByReferenceElementType =
        match stripTypeEquations this with
        | TypeSymbol.ByRef(elementTy, _) -> elementTy |> ValueSome
        | _ -> ValueNone

    member this.GetByReferenceElementType() =
        match stripTypeEquations this with
        | TypeSymbol.ByRef(elementTy, _) -> elementTy
        | _ -> OlyAssert.Fail("Expected ByRef type.")

    member this.TryGetArrayElementType() =
        match stripTypeEquations this with
        | TypeSymbol.Array(elementTy, _, _) -> elementTy |> ValueSome
        | _ -> ValueNone

    member this.TryGetTupleItemTypes() =
        match stripTypeEquations this with
        | TypeSymbol.Tuple(itemTys, _) -> itemTys |> ValueSome
        | _ -> ValueNone

    member this.TryGetReferenceCellElement =
        match stripTypeEquations this with
        | TypeSymbol.RefCell(elementTy) -> elementTy |> ValueSome
        | _ -> ValueNone

    member this.IsVariadicInferenceVariable =
        match stripTypeEquations this with
        | TypeSymbol.InferenceVariable(Some tyPar, _) -> tyPar.IsVariadic
        | _ -> false

    member this.IsVariadicTypeVariable =
        match stripTypeEquations this with
        | TypeSymbol.Variable(tyPar) -> tyPar.IsVariadic
        | _ -> false

    member this.IsStruct =
        match stripTypeEquations this with
        | Entity(ent) -> ent.IsStruct
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32 
        | Int64
        | UInt64
        | Float32 
        | Float64 
        | Bool
        | Char16
        | NativeInt
        | NativeUInt
        | NativePtr _
        | NativeFunctionPtr _ -> true
        | _ -> false

    member this.IsAnyStruct =
        match stripTypeEquations this with
        | Entity(ent) -> ent.IsAnyStruct
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32 
        | Int64
        | UInt64
        | Float32 
        | Float64 
        | Bool
        | Char16
        | NativeInt
        | NativeUInt
        | NativePtr _ 
        | NativeFunctionPtr _
        | Tuple _ -> true
        // Scoped function types are structs.
        | Function(kind=FunctionKind.Scoped) -> true
        | Variable(tyPar)
        | HigherVariable(tyPar, _) ->
            tyPar.Constraints
            |> ImArray.exists (function
                | ConstraintSymbol.Struct
                | ConstraintSymbol.Unmanaged -> true
                | _ -> false
            )
        | _ -> false

    member this.IsTypeExtendingAStruct =
        match stripTypeEquations this with
        | Entity(ent) when ent.IsTypeExtension ->
            if ent.Extends.IsEmpty then
                false
            else
                ent.IsAnyStruct
        | _ ->
            false

    member this.IsSealed =
        match stripTypeEquations this with
        | Entity(ent) -> ent.IsSealed
        | _ -> true

    member this.IsReadOnly =
        match stripTypeEquations this with
        | Entity ent -> ent.IsReadOnly
        | Unit
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32 
        | Int64
        | UInt64
        | Float32 
        | Float64 
        | Bool
        | Char16 
        | Utf16
        | Tuple _
        | Function _ 
        | ConstantInt32 _ -> true
        | ByRef(_, ByRefKind.Read) 
        | Array(_, _, ArrayKind.Immutable) -> true
        | _ -> false
        
    member this.IsSolved =
        match stripTypeEquations this with
        | InferenceVariable _
        | HigherInferenceVariable _
        | EagerInferenceVariable _ -> false
        | _ -> true

    member this.IsFormal =
        match stripTypeEquationsExceptAlias this with
        | Entity(ent) -> ent.Id = ent.Formal.Id
        | ForAll _ -> true
        | (Tuple _ as ty) -> obj.ReferenceEquals(ty, Types.Tuple)
        | (NativePtr _ as ty) -> obj.ReferenceEquals(ty, Types.NativePtr)
        | Variable _
        | Void
        | NativeInt
        | NativeUInt
        | BaseObject
        | Unit
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32 
        | Int64
        | UInt64
        | Float32 
        | Float64 
        | Bool
        | Char16 
        | Utf16 -> true
        | ByRef(elementTy, _) ->
            match elementTy with
            | TypeSymbol.Variable(tyPar) ->
                obj.ReferenceEquals(tyPar, ByReferenceTypeParameters[0])
            | _ ->
                false
        | Function(kind=FunctionKind.Normal) as ty ->
            obj.ReferenceEquals(FormalNormalFunctionType, ty)
        | Function(kind=FunctionKind.Scoped) as ty ->
            obj.ReferenceEquals(FormalScopedFunctionType, ty)
        | NativeFunctionPtr(ilCallConv, _, _) as ty ->
            obj.ReferenceEquals(FormalNativeFunctionPtr.Get(ilCallConv), ty)
        | RefCell _ as ty ->
            obj.ReferenceEquals(FormalRefCellType, ty)
        | Array(_, rank, kind) as ty ->
            obj.ReferenceEquals(FormalArray.Get(rank, kind), ty)
        | DependentIndexer _ as ty ->
            obj.ReferenceEquals(FormalDependentIndexerType, ty)

        | InferenceVariable(Some tyPar, _) -> tyPar.Arity > 0

        | _ -> 
            false

    /// TODO: Rename to "TryAnyFunction".
    member this.TryFunction =
        match stripTypeEquations this with
        | TypeSymbol.Function(inputTy, outputTy, _)
        | TypeSymbol.NativeFunctionPtr(_, inputTy, outputTy)
        | TypeSymbol.ForAll(_, TypeSymbol.Function(inputTy, outputTy, _)) -> 
            ValueSome(inputTy, outputTy)
        | TypeSymbol.Variable(tyPar) when not tyPar.Constraints.IsEmpty ->
            let funcOpt =
                tyPar.Constraints
                |> ImArray.tryPick (fun x ->
                    match x with
                    | ConstraintSymbol.SubtypeOf(ty) ->
                        match stripTypeEquations ty.Value with
                        | TypeSymbol.Function _ ->
                            match ty.Value.TryFunction with
                            | ValueSome x -> Some x
                            | _ -> None
                        | _ ->
                            None
                    | _ ->
                        None
                )
            match funcOpt with
            | Some func ->
                ValueSome func
            | _ ->
                ValueNone
        | _ -> 
            ValueNone

    member this.FunctionParameterCount =
        match this.TryFunction with
        | ValueSome(inputTys, _) -> inputTys.Length
        | _ -> 0

    member this.FirstTypeArgument =
        this.TypeArguments[0]

    member this.AsParameters(): TypeSymbol imarray =
        match this with
        | TypeSymbol.Unit -> ImArray.empty
        | TypeSymbol.Tuple(argTys, _) -> argTys
        | _ -> ImArray.createOne this

    member inline this.ForEachParameter ([<InlineIfLambda>] f) =
        match this with
        | TypeSymbol.Unit -> ()
        | TypeSymbol.Tuple(argTys, _) -> argTys |> ImArray.iter f
        | _ -> f this

    member this.TryGetFunctionWithParameters() =
        match stripTypeEquations this with
        | TypeSymbol.Function(inputTys, outputTy, _)
        | TypeSymbol.NativeFunctionPtr(_, inputTys, outputTy)
        | TypeSymbol.ForAll(_, TypeSymbol.Function(inputTys, outputTy, _)) -> 
            ValueSome(inputTys.Types, outputTy)
        | _ -> 
            ValueNone

    member this.FunctionArgumentTypes: TypeSymbol imarray =
        match this.TryFunction with
        | ValueSome(inputTys, _) ->
            inputTys.Types
        | _ -> 
            ImArray.empty

    static member CreateTupleOrOneOrUnit(tys: ImmutableArray<TypeSymbol>) =
        if tys.Length >= 2 then
            TypeSymbol.CreateTuple(tys)
        elif tys.Length = 1 then
            tys[0]
        else
            TypeSymbol.Unit

    static member CreateTuple(tys: ImmutableArray<TypeSymbol>) =
        if tys.Length < 2 then
            failwith "Creating a tuple type requires two or more element types."
        else
            TypeSymbol.Tuple(tys, ImArray.empty)

    static member CreateArray(elementTy: TypeSymbol, rank) =
        if rank < 1 then failwith "Invalid rank."
        TypeSymbol.Array(elementTy, rank, ArrayKind.Immutable)

    static member CreateMutableArray(elementTy: TypeSymbol, rank) =
        if rank < 1 then failwith "Invalid rank."
        TypeSymbol.Array(elementTy, rank, ArrayKind.Mutable)

    static member CreateArray(elementTy: TypeSymbol) =
        TypeSymbol.CreateArray(elementTy, 1)

    static member CreateMutableArray(elementTy: TypeSymbol) =
        TypeSymbol.CreateMutableArray(elementTy, 1)

    static member CreateFunction(inputTys: TypeSymbol imarray, outputTy: TypeSymbol, kind) =
        let inputTys = VariadicTypeArguments(inputTys, ImArray.empty)
        TypeSymbol.Function(inputTys, outputTy, kind)

    static member CreateFunction(tyPars: ImmutableArray<TypeParameterSymbol>, inputTys: TypeSymbol imarray, returnTy: TypeSymbol, kind) =
        let inputTys = VariadicTypeArguments(inputTys, ImArray.empty)
        if tyPars.IsEmpty then
            TypeSymbol.Function(inputTys, returnTy, kind)
        else
            tyPars
            |> ImArray.iter (fun tyPar ->
                if tyPar.Kind = TypeParameterKind.Type then 
                    failwith $"Expected type parameter kind 'Function': '{tyPar.Name}'"
            )
            TypeSymbol.ForAll(tyPars, TypeSymbol.Function(inputTys, returnTy, kind))

    static member CreateFunctionPtr(ilCallConv, inputTys: TypeSymbol imarray, outputTy: TypeSymbol) =
        let inputTys = VariadicTypeArguments(inputTys, ImArray.empty)
        TypeSymbol.NativeFunctionPtr(ilCallConv, inputTys, outputTy)

    interface ISymbol

type IModuleSymbol = EntitySymbol
type INamespaceOrModuleSymbol = EntitySymbol

[<DebuggerDisplay("{Name}")>]
type IAssembly =
    inherit ISymbol

    abstract UniqueId : string

    abstract Name : string

    abstract Modules : IModuleSymbol seq


type ISymbolImporter =

    abstract TryImportModule : path: string seq * name: string -> IModuleSymbol option


[<AutoOpen>]
module SymbolExtensions =

    [<AutoOpen>]
    module FunctionSymbolExtensions =
    
        type IFunctionSymbol with

            member this.IsPure =
                this.FunctionFlags.HasFlag(FunctionFlags.Pure)

            member this.IsBlittable =
                this.FunctionFlags.HasFlag(FunctionFlags.Blittable)

            member this.IsPatternFunction =
                match this.Semantic with
                | FunctionSemantic.PatternFunction -> true
                | _ -> false

            member this.IsPatternGuardFunction =
                match this.Semantic with
                | FunctionSemantic.PatternGuardFunction -> true
                | _ -> false
    
            member this.LogicalParameterCount =
                if this.IsInstance then this.Parameters.Length - 1
                else this.Parameters.Length
    
            member this.LogicalParameters: _ romem =
                if this.IsInstance then this.Parameters.AsMemory().Slice(1)
                else this.Parameters.AsMemory()

            member this.LogicalTypeParameters =
                if this.IsConstructor then
                    this.Enclosing.LogicalTypeParameters
                else
                    this.TypeParameters

            member this.LogicalTypeParameterCount =
                this.LogicalTypeParameters.Length
                
    [<AutoOpen>]
    module ValueSymbolExtensions =

        [<Literal>]
        let LocalBridgeName = "__oly_bridge"
    
        type IValueSymbol with

            member this.IsFormal =
                this.Formal = this

            member this.IsUnmanagedAllocationOnly =
                this.FunctionFlags.HasFlag(FunctionFlags.UnmanagedAllocationOnly)

            member this.IsPublic =
                this.MemberFlags &&& MemberFlags.AccessorMask = MemberFlags.Public

            member this.IsPrivate =
                this.MemberFlags &&& MemberFlags.AccessorMask = MemberFlags.Private

            member this.IsInternal =
                this.MemberFlags &&& MemberFlags.AccessorMask = MemberFlags.Internal

            member this.IsProtected =
                this.MemberFlags &&& MemberFlags.AccessorMask = MemberFlags.Protected

            member this.IsGenerated: bool =
                this.ValueFlags &&& ValueFlags.Generated = ValueFlags.Generated

            member this.IsReadOnly =
                not this.IsMutable
    
            member this.IsSingleUse =
                this.IsLocal && this.Name = LocalBridgeName && not this.IsMutable

            member this.IsFieldConstant =
                this.IsField && (this :?> IFieldSymbol).Constant.IsSome

            member this.IsFieldInit =
                this.IsField && (this.ValueFlags &&& ValueFlags.FieldInit = ValueFlags.FieldInit)
    
            /// Returns the type of the value except it excludes the instance argument type and quantified type parameters.
            /// Useful for checking implementations for abstract functions.
            /// TODO: Move this to IValueSymbol so we do not have to recompute this everytime we call this.
            member this.LogicalType =
                if this.IsInstance && this.IsFunction then
                    match this.Type.TryFunction with
                    | ValueSome(inputTys, outputTy) ->
                        TypeSymbol.CreateFunction(inputTys.Types.RemoveAt(0), outputTy, FunctionKind.Normal)
                    | _ ->
                        this.Type
                else
                    this.Type
    
            member this.IsLocal =
                match this.Enclosing with
                | EnclosingSymbol.Local -> true
                | _ -> false

            /// Returns the type parameters of the value, 
            ///     or the enclosing type parameters if the value is a constructor.
            member this.TypeParametersOrConstructorEnclosingTypeParameters =
                if this.IsConstructor then
                    let enclosingEnclosingTyPars = this.Enclosing.Enclosing.TypeParameters
                    if enclosingEnclosingTyPars.IsEmpty then
                        this.Enclosing.TypeParameters
                    else
                        this.Enclosing.TypeParameters 
                        |> Seq.skip (enclosingEnclosingTyPars.Length) 
                        |> ImArray.ofSeq
                else
                    this.TypeParameters

            /// Returns the type arguments of the value, 
            ///     or the enclosing type arguments if the value is a constructor.
            member this.TypeArgumentsOrConstructorEnclosingTypeArguments =
                if this.IsConstructor then
                    let enclosingEnclosingTyPars = this.Enclosing.Enclosing.TypeParameters
                    if enclosingEnclosingTyPars.IsEmpty then
                        this.Enclosing.TypeArguments
                    else
                        this.Enclosing.TypeArguments 
                        |> Seq.skip (enclosingEnclosingTyPars.Length) 
                        |> ImArray.ofSeq
                else
                    this.TypeArguments

            /// Includes enclosing type parameters and the value's type parameters. 
            member this.AllTypeParameters =
                if this.IsConstructor then
                    this.Enclosing.TypeParameters
                else
                    if this.TypeParameters.IsEmpty then
                        this.Enclosing.TypeParameters
                    else
                        this.Enclosing.TypeParameters.AddRange(this.TypeParameters)

            /// Includes enclosing type parameters and the value's type parameters. 
            member this.AllTypeParameterCount =
                if this.IsConstructor then
                    this.Enclosing.TypeParameterCount
                else
                    if this.TypeParameters.IsEmpty then
                        this.Enclosing.TypeParameterCount
                    else
                        this.Enclosing.TypeParameterCount + this.TypeParameters.Length
    
            /// Includes enclosing type arguments and the value's type arguments. 
            member this.AllTypeArguments =
                if this.IsConstructor then
                    this.Enclosing.TypeArguments
                else
                    if this.TypeParameters.IsEmpty then
                        this.Enclosing.TypeArguments
                    else
                        this.Enclosing.TypeArguments.AddRange(this.TypeArguments)
    
            member this.IsMutable =
                this.ValueFlags &&& ValueFlags.Mutable = ValueFlags.Mutable  
    
            member this.TryWellKnownFunction =
                match this with
                | :? FunctionGroupSymbol as funcGroup ->
                    // Address-of is special as its use impacts what return type
                    // an expression will have. This is due to automatic dereferencing.
                    if 
                        funcGroup.Functions
                        |> ImArray.forall (fun x -> x.IsAddressOf) then
                            ValueSome WellKnownFunction.AddressOf
                    else
                            ValueNone
                | :? IFunctionSymbol as func ->
                    if func.WellKnownFunction <> WellKnownFunction.None then
                        ValueSome func.WellKnownFunction
                    else
                        ValueNone
                | _ ->
                    ValueNone

            member this.IsAddressOf =
                match this.TryWellKnownFunction with
                | ValueSome(WellKnownFunction.AddressOf) -> true
                | _ -> false

            member this.IsFromAddress =
                match this.TryWellKnownFunction with
                | ValueSome(WellKnownFunction.FromAddress) -> true
                | _ -> false

            member this.IsLoadFunctionPtr =
                match this.TryWellKnownFunction with
                | ValueSome(WellKnownFunction.LoadFunctionPtr) -> true
                | _ -> false

            member this.IsExported =
                this.ValueFlags &&& ValueFlags.Exported = ValueFlags.Exported
    
            member this.IsImported =
                this.ValueFlags &&& ValueFlags.Imported = ValueFlags.Imported
    
            member this.IsConstructor =
                this.FunctionFlags &&& FunctionFlags.Constructor = FunctionFlags.Constructor

            member this.IsStaticConstructor =
                not this.IsInstance && this.IsConstructor
    
            member this.IsInstanceConstructor =
                this.IsInstance && this.IsConstructor
    
            member this.IsInstanceNotConstructor =
                this.IsInstance && not this.IsConstructor
    
            member this.IsAutoProperty =
                match this with
                | :? IPropertySymbol as prop -> prop.BackingField.IsSome
                | _ -> false
    
            member this.IsInstance =
                this.MemberFlags &&& MemberFlags.Instance = MemberFlags.Instance

            member this.IsStatic =
                this.MemberFlags &&& MemberFlags.Instance <> MemberFlags.Instance
    
            member this.IsOverridable =
                this.IsVirtual && not this.IsFinal
    
            member this.IsAbstract =
                this.MemberFlags &&& MemberFlags.Abstract = MemberFlags.Abstract

            member this.IsNewSlot =
                this.MemberFlags &&& MemberFlags.NewSlot = MemberFlags.NewSlot

            member this.IsFinal =
                this.MemberFlags &&& MemberFlags.Sealed = MemberFlags.Sealed

            member this.IsVirtual =
                this.MemberFlags &&& MemberFlags.Virtual = MemberFlags.Virtual

            member this.IsExplicitOverrides =
                this.MemberFlags &&& MemberFlags.ExplicitOverrides = MemberFlags.ExplicitOverrides

            member this.IsConcrete =
                if this.Enclosing.IsInterface then false
                elif not this.IsVirtual then true
                else this.IsVirtual && (this.IsFinal || this.Enclosing.IsSealed)
    
            member this.IsStaticLocalFunction =
                (this.FunctionFlags &&& FunctionFlags.StaticLocal = FunctionFlags.StaticLocal) &&
                this.IsLocal

            member this.RequiresExplicitTypeArguments =
                this.FunctionFlags &&& FunctionFlags.RequiresExplicitTypeArguments = FunctionFlags.RequiresExplicitTypeArguments

            member this.IsParameterLessFunction =
                this.FunctionFlags &&& FunctionFlags.ParameterLess = FunctionFlags.ParameterLess
    
            member this.RequiresWitness =
                match this.Enclosing with
                | EnclosingSymbol.Witness _ -> true
                | _ -> false
    
            member this.IsParameter =
                this.ValueFlags &&& ValueFlags.Parameter = ValueFlags.Parameter

            member this.AsFunction = this :?> IFunctionSymbol

            member this.AsProperty = this :?> IPropertySymbol

            member this.MorphShapeConstructor(concreteTy, shapeTy: TypeSymbol) =
                OlyAssert.True(shapeTy.IsShape)
                OlyAssert.True(this.IsInstanceConstructor)
                match this with
                | :? IFunctionSymbol as func when func.IsInstanceConstructor ->
                    let id = newId()
                    let enclosing = EnclosingSymbol.Witness(concreteTy, shapeTy.AsEntity)
                    let returnTy = concreteTy
                    let parTys =
                        func.Parameters 
                        |> ImArray.mapi (fun i x -> 
                            if i = 0 then
                                concreteTy
                            else
                                x.Type
                        )
                    let funcTy = TypeSymbol.CreateFunction(func.TypeParameters, parTys, returnTy, FunctionKind.Normal)
                    { new IFunctionSymbol with
    
                        member _.Enclosing = enclosing
    
                        member _.Name = func.Name
    
                        member _.Type = funcTy
    
                        member _.Id = id
    
                        member _.TypeParameters = func.TypeParameters
    
                        member _.TypeArguments = func.TypeArguments
    
                        member _.IsFunction = func.IsFunction
    
                        member _.IsField = func.IsField
    
                        member _.MemberFlags = func.MemberFlags
    
                        member _.FunctionFlags = func.FunctionFlags
    
                        member _.FunctionOverrides = func.FunctionOverrides

                        member _.IsProperty = func.IsProperty

                        member _.IsPattern = false
    
                        member this.Formal = func
    
                        member _.Parameters = func.Parameters
    
                        member _.ReturnType = returnTy
    
                        member _.Attributes = func.Attributes
    
                        member _.ValueFlags = func.ValueFlags

                        member _.IsThis = func.IsThis
                        
                        member _.IsBase = func.IsBase

                        member _.Semantic = func.Semantic

                        member _.WellKnownFunction = func.WellKnownFunction

                        member _.AssociatedFormalPattern = func.AssociatedFormalPattern

                    } :> IValueSymbol
                | _ ->
                    OlyAssert.Fail("Invalid constructor.")
    
            member this.WithEnclosing(enclosing) =
                let id = newId()
                match this with
                | :? FunctionGroupSymbol as funcGroup ->
                    FunctionGroupSymbol(enclosing, funcGroup.Name, funcGroup.Functions, funcGroup.Functions[0].Parameters.Length, (funcGroup: IValueSymbol).IsPattern) :> IValueSymbol
                | :? IFunctionSymbol as func ->
                    { new IFunctionSymbol with
    
                        member _.Enclosing = enclosing
    
                        member _.Name = func.Name
    
                        member _.Type = func.Type
    
                        member _.Id = id
    
                        member _.TypeParameters = func.TypeParameters
    
                        member _.TypeArguments = func.TypeArguments
    
                        member _.IsFunction = func.IsFunction
    
                        member _.IsField = func.IsField
    
                        member _.MemberFlags = func.MemberFlags
    
                        member _.FunctionFlags = func.FunctionFlags
    
                        member _.FunctionOverrides = func.FunctionOverrides

                        member _.IsProperty = func.IsProperty

                        member _.IsPattern = false
    
                        member _.Formal = func.Formal
    
                        member _.Parameters = func.Parameters
    
                        member _.ReturnType = func.ReturnType
    
                        member _.Attributes = func.Attributes
    
                        member _.ValueFlags = func.ValueFlags

                        member _.IsThis = func.IsThis
                        
                        member _.IsBase = func.IsBase

                        member _.Semantic = func.Semantic

                        member _.WellKnownFunction = func.WellKnownFunction

                        member _.AssociatedFormalPattern = func.AssociatedFormalPattern

                    } :> IValueSymbol
                | :? IFieldSymbol as field ->
                    { new IFieldSymbol with
    
                        member _.Enclosing = enclosing
    
                        member _.Name = field.Name
    
                        member _.Type = field.Type
    
                        member _.Id = id
    
                        member _.TypeParameters = field.TypeParameters
    
                        member _.TypeArguments = field.TypeArguments
    
                        member _.IsFunction = field.IsFunction
    
                        member _.IsField = field.IsField
    
                        member _.MemberFlags = field.MemberFlags
    
                        member _.FunctionFlags = field.FunctionFlags
    
                        member _.FunctionOverrides = field.FunctionOverrides

                        member _.IsProperty = field.IsProperty

                        member _.IsPattern = false
    
                        member _.Formal = field.Formal
    
                        member _.ValueFlags = field.ValueFlags
    
                        member _.Attributes = field.Attributes

                        member _.IsThis = field.IsThis

                        member _.IsBase = field.IsBase

                        member _.Constant = field.Constant

                        member _.AssociatedFormalPropertyId = field.AssociatedFormalPropertyId
                    } :> IValueSymbol
                | :? IPropertySymbol as prop ->
                    let getterOpt = prop.Getter |> Option.map (fun x -> x.WithEnclosing(enclosing) :?> IFunctionSymbol)
                    let setterOpt = prop.Setter |> Option.map (fun x -> x.WithEnclosing(enclosing) :?> IFunctionSymbol)
                    let backingFieldOpt = prop.BackingField |> Option.map (fun x -> x.WithEnclosing(enclosing) :?> IFieldSymbol)
                    { new IPropertySymbol with
    
                        member _.Enclosing = enclosing
    
                        member _.Name = prop.Name
    
                        member _.Type = prop.Type
    
                        member _.Id = id
    
                        member _.TypeParameters = prop.TypeParameters
    
                        member _.TypeArguments = prop.TypeArguments
    
                        member _.IsFunction = prop.IsFunction
    
                        member _.IsField = prop.IsField
    
                        member _.MemberFlags = prop.MemberFlags
    
                        member _.FunctionFlags = prop.FunctionFlags
    
                        member _.FunctionOverrides = prop.FunctionOverrides

                        member _.IsProperty = prop.IsProperty

                        member _.IsPattern = false
    
                        member _.Formal = prop.Formal
    
                        member _.ValueFlags = prop.ValueFlags
    
                        member _.Attributes = prop.Attributes

                        member _.IsThis = prop.IsThis

                        member _.IsBase = prop.IsBase

                        member _.Getter = getterOpt

                        member _.Setter = setterOpt

                        member _.BackingField = backingFieldOpt
                    } :> IValueSymbol
                |  :? IPatternSymbol as pat ->
                    let func = pat.PatternFunction.WithEnclosing(enclosing) :?> IFunctionSymbol
                    let guardOpt = pat.PatternGuardFunction |> Option.map (fun x -> x.WithEnclosing(enclosing) :?> IFunctionSymbol)
                    { new IPatternSymbol with
    
                        member _.Enclosing = enclosing
    
                        member _.Name = pat.Name
    
                        member _.Type = pat.Type
    
                        member _.Id = id
    
                        member _.TypeParameters = pat.TypeParameters
    
                        member _.TypeArguments = pat.TypeArguments
    
                        member _.IsFunction = pat.IsFunction
    
                        member _.IsField = pat.IsField
    
                        member _.MemberFlags = pat.MemberFlags
    
                        member _.FunctionFlags = pat.FunctionFlags
    
                        member _.FunctionOverrides = pat.FunctionOverrides

                        member _.IsProperty = pat.IsProperty

                        member _.IsPattern = true
    
                        member _.Formal = pat.Formal
    
                        member _.ValueFlags = pat.ValueFlags
    
                        member _.Attributes = pat.Attributes

                        member _.IsThis = pat.IsThis

                        member _.IsBase = pat.IsBase

                        member _.PatternFunction = func

                        member _.PatternGuardFunction = guardOpt

                    } :> IValueSymbol
                | _ ->
                    let value = this
                    { new IValueSymbol with
    
                        member _.Enclosing = enclosing
    
                        member _.Name = value.Name
    
                        member _.Type = value.Type
    
                        member _.Id = id
    
                        member _.TypeParameters = value.TypeParameters
    
                        member _.TypeArguments = value.TypeArguments
    
                        member _.IsFunction = value.IsFunction
    
                        member _.IsField = value.IsField
    
                        member _.MemberFlags = value.MemberFlags
    
                        member _.FunctionFlags = value.FunctionFlags
    
                        member _.FunctionOverrides = value.FunctionOverrides

                        member _.IsProperty = false

                        member _.IsPattern = false
    
                        member _.Formal = value.Formal
    
                        member _.ValueFlags = value.ValueFlags
    
                        member _.IsThis = value.IsThis

                        member _.IsBase = value.IsBase
                    }

    type QualifiedName = string

    [<AutoOpen>]
    module EntitySymbolExtensions =
    
        type EntitySymbol with

            member this.IsPublic =
                this.Flags &&& EntityFlags.AccessorMask = EntityFlags.Public

            member this.IsPrivate =
                this.Flags &&& EntityFlags.AccessorMask = EntityFlags.Private

            member this.IsInternal =
                this.Flags &&& EntityFlags.AccessorMask = EntityFlags.Internal

            member this.IsNullable =
                if this.IsAnyStruct || this.IsNamespaceOrModule then false
                else
                    this.Flags.HasFlag(EntityFlags.Nullable) ||
                    (this.Flags.HasFlag(EntityFlags.Abstract) && not(this.Flags.HasFlag(EntityFlags.Final)))

            member this.IsAutoOpenable =
                this.Flags.HasFlag(EntityFlags.AutoOpen)

            // Gets the type parameter count that does not include its enclosing's type parameter count.
            member this.LogicalTypeParameterCount =
                let n = this.TypeParameters.Length - this.Enclosing.TypeParameters.Length
                if n < 0 then
                    failwith "Should not happen: logical type parameter count is less than zero."
                n

            // Gets the type parameters that does not include its enclosing's type parameters.
            member this.LogicalTypeParameters =
                let enclosingTyPars = this.Enclosing.TypeParameters
                if enclosingTyPars.IsEmpty then
                    this.TypeParameters
                else
                    this.TypeParameters.RemoveRange(0, enclosingTyPars.Length)

            // Gets the type arguments that does not include its enclosing's type arguments.
            member this.LogicalTypeArguments =
                let enclosingTyArgs = this.Enclosing.TypeArguments
                if enclosingTyArgs.IsEmpty then
                    this.TypeArguments
                else
                    this.TypeArguments.RemoveRange(0, enclosingTyArgs.Length)

            member this.IsNested =
                match this.Enclosing with
                | EnclosingSymbol.Entity _ -> true
                | _ -> false

            member this.IsTypeConstructor =
                not this.TypeParameters.IsEmpty && this.IsFormal         
                    
            member this.FullNamespacePath =
                if not this.IsNamespace then
                    failwith "Expected a namespace."

                let rec loop enclosing : string imarray =
                    match enclosing with
                    | EnclosingSymbol.Entity(ent) when ent.IsNamespace ->
                        (loop ent.Enclosing).Add(ent.Name)
                    | _ ->
                        ImArray.empty
                (loop this.Enclosing).Add(this.Name)

            member this.TopMostEnclosingNonNamespaceEntity =
                match this.Enclosing with
                | EnclosingSymbol.Entity(ent) when not ent.IsNamespace -> ent.TopMostEnclosingNonNamespaceEntity
                | _ -> this
    
            member this.TryClosureInvoke =
                if this.IsClosure then
                    this.Functions 
                    |> Seq.filter (fun x -> not x.IsConstructor) 
                    |> Seq.exactlyOne
                    |> ValueSome
                else
                    ValueNone
    
            member this.IsClosure =
                this.Kind = EntityKind.Closure

            member this.IsEnum =
                this.Kind = EntityKind.Enum

            member this.IsNewtype =
                this.Kind = EntityKind.Newtype

            member this.IsNamespace =
                this.Kind = EntityKind.Namespace

            member this.IsNamespaceOrModule =
                this.IsNamespace || this.IsModule

            member this.TryTypeExtension =
                if this.IsTypeExtension then
                    let inherits = this.Extends
                    if inherits.Length = 1 then
                        ValueSome(inherits.[0], this.Implements)
                    else
                        ValueNone
                else
                    ValueNone
    
            member this.AsType = 
                if this.IsNamespace && not(this.Flags.HasFlag(EntityFlags.Invalid)) then
                    OlyAssert.Fail("Namespace cannot turn into a type.")
                TypeSymbol.Entity(this)

            member this.AsNamespaceType =
                if not this.IsNamespace && not(this.Flags.HasFlag(EntityFlags.Invalid)) then
                    OlyAssert.Fail("Entity must be a namespace in order to turn into a pseudo-type.")
                TypeSymbol.Entity(this)
    
            member this.AsEnclosing = 
                EnclosingSymbol.Entity(this)
    
            member this.IsFormal =
                this.Id = this.Formal.Id
    
            member this.IsAnonymous =
                this.Name = AnonymousEntityName
    
            member this.IsShape =
                this.Kind = EntityKind.Shape

            member this.IsAnonymousShape =
                this.IsShape && this.IsAnonymous

            member this.IsAnonymousModule =
                this.IsPrivate && this.IsModule && this.IsAnonymous

            member this.IsStruct =
                match this.Kind with
                | EntityKind.Struct -> true
                | EntityKind.Alias ->
                    match this.Extends |> Seq.tryExactlyOne with
                    | Some realTy -> realTy.IsStruct
                    | _ -> false
                | _ ->
                    false
    
            /// Returns true if the entity is a struct, an enum struct, or a newtype struct.
            member this.IsAnyStruct =
                match this.Kind with
                | EntityKind.Struct -> true
                | EntityKind.Alias ->
                    match this.Extends |> Seq.tryExactlyOne with
                    | Some realTy -> realTy.IsAnyStruct
                    | _ -> false
                | EntityKind.Newtype when this.Extends.Length = 1 -> this.Extends.[0].IsAnyStruct
                | EntityKind.Enum when this.TryEnumUnderlyingType.IsSome -> this.TryEnumUnderlyingType.Value.IsAnyStruct
                | EntityKind.TypeExtension when this.Extends.Length = 1 -> this.Extends[0].IsAnyStruct
                | EntityKind.Closure -> this.Flags.HasFlag(EntityFlags.Scoped)
                | _ ->
                    false
    
            member this.IsInterface =
                this.Kind = EntityKind.Interface
    
            member this.IsTypeExtension =
                this.Kind = EntityKind.TypeExtension
    
            member this.IsClass =
                this.Kind = EntityKind.Class

            /// TODO: Rename to IsFinal.
            member this.IsSealed =
                this.Flags &&& EntityFlags.Final = EntityFlags.Final
                
            member this.IsAbstract =
                this.Flags &&& EntityFlags.Abstract = EntityFlags.Abstract
    
            member this.IsAlias =
                match this.Kind with
                | EntityKind.Alias -> true
                | _ -> false
    
            member this.IsModule =
                this.Kind = EntityKind.Module
    
            member this.IsInheritable =
                match this.Kind with
                | EntityKind.Interface
                | EntityKind.Class -> not this.IsSealed
                | _ -> false

            member this.IsClassOrStructOrModuleOrNewtype =
                this.IsClass || this.IsStruct || this.IsModule || this.IsNewtype
    
            member this.IsReadOnly =
                this.Flags &&& EntityFlags.ReadOnly = EntityFlags.ReadOnly
    
            member this.TryCompilerIntrinsic =
                this.Attributes
                |> Seq.tryPick (fun attr -> 
                    match attr with
                    | AttributeSymbol.Intrinsic(name) -> Some name
                    | _ -> None
                )
    
            member this.IsCompilerIntrinsic = 
                this.TryCompilerIntrinsic.IsSome

            member this.CanDeclareConstructor = 
                (this.IsClass || this.IsAnyStruct || this.IsShape) && not this.IsAlias

        type EnclosingSymbol with

            member this.CanDeclareConstructor =
                match this with
                | EnclosingSymbol.Entity(ent) -> ent.CanDeclareConstructor
                | _ -> false

[<NoEquality;NoComparison>]
type BindingInfoSymbol =
    | BindingFunction of func: FunctionSymbol
    | BindingField of field: FieldSymbol
    | BindingPattern of pat: PatternSymbol * func: FunctionSymbol
    | BindingProperty of getterAndSetterBindings: BindingInfoSymbol imarray * prop: PropertySymbol

    /// For patterns, this returns the pattern function, not the pattern itself.
    member this.Value =
        match this with
        | BindingFunction(func=func) -> func :> IValueSymbol
        | BindingField(field=field) -> field :> IValueSymbol
        | BindingPattern(func=func) -> func :> IValueSymbol
        | BindingProperty(prop=prop) -> prop :> IValueSymbol

    member this.Type =
        match this with
        | BindingFunction(func=func) ->
            func.Type
        | BindingField(field=field) ->
            field.Type
        | BindingPattern(pat=pat) ->
            pat.Type
        | BindingProperty(prop=prop) ->
            prop.Type

[<NoEquality;NoComparison>]
type LocalBindingInfoSymbol =
    | BindingLocalFunction of func: FunctionSymbol
    | BindingLocal of value: ILocalSymbol

    member this.IsFunction =
        match this with
        | BindingLocalFunction _ -> true
        | _ -> false

    member this.Value =
        match this with
        | BindingLocalFunction(func=func) -> func :> IValueSymbol
        | BindingLocal(value=value) -> value

    member this.Type =
        match this with
        | BindingLocalFunction(func=func) ->
            func.Type
        | BindingLocal(value=value) ->
            value.Type

    member this.TypeParameters =
        match this with
        | BindingLocal _ -> ImArray.empty
        | BindingLocalFunction(func=func) -> func.TypeParameters

[<RequireQualifiedAccess>]
module Types =
    let ByRef = TypeSymbol.GetFormalByRef(ByRefKind.ReadWrite)

    let InRef = TypeSymbol.GetFormalByRef(ByRefKind.Read)

    let Tuple = FormalTupleType

    let NativePtr = FormalNativePtrType

[<AutoOpen>]
module OtherExtensions =

    type AttributeSymbol with

        member this.TryIntrinsicType =
            match this with
            | AttributeSymbol.Intrinsic("void") -> TypeSymbol.Void |> ValueSome
            | AttributeSymbol.Intrinsic("uint8") -> TypeSymbol.UInt8 |> ValueSome
            | AttributeSymbol.Intrinsic("int8") -> TypeSymbol.Int8 |> ValueSome
            | AttributeSymbol.Intrinsic("uint16") -> TypeSymbol.UInt16 |> ValueSome
            | AttributeSymbol.Intrinsic("int16") -> TypeSymbol.Int16 |> ValueSome
            | AttributeSymbol.Intrinsic("uint32") -> TypeSymbol.UInt32 |> ValueSome
            | AttributeSymbol.Intrinsic("int32") -> TypeSymbol.Int32 |> ValueSome
            | AttributeSymbol.Intrinsic("uint64") -> TypeSymbol.UInt64 |> ValueSome
            | AttributeSymbol.Intrinsic("int64") -> TypeSymbol.Int64 |> ValueSome
            | AttributeSymbol.Intrinsic("float32") -> TypeSymbol.Float32 |> ValueSome
            | AttributeSymbol.Intrinsic("float64") -> TypeSymbol.Float64 |> ValueSome
            | AttributeSymbol.Intrinsic("bool") -> TypeSymbol.Bool |> ValueSome
            | AttributeSymbol.Intrinsic("char16") -> TypeSymbol.Char16 |> ValueSome
            | AttributeSymbol.Intrinsic("utf16") -> TypeSymbol.Utf16 |> ValueSome
            | AttributeSymbol.Intrinsic("native_int") -> TypeSymbol.NativeInt |> ValueSome
            | AttributeSymbol.Intrinsic("native_uint") -> TypeSymbol.NativeUInt |> ValueSome
            | AttributeSymbol.Intrinsic("native_ptr") -> Types.NativePtr |> ValueSome
            | AttributeSymbol.Intrinsic("by_ref_read_write") -> Types.ByRef |> ValueSome
            | AttributeSymbol.Intrinsic("by_ref_read") -> Types.InRef |> ValueSome
            | AttributeSymbol.Intrinsic("base_object") -> TypeSymbol.BaseObject |> ValueSome
            | _ -> ValueNone

    type EntitySymbol with

        member this.TryIntrinsicType =
            // TODO: Uncomment this.
            //   OlyAssert.True(this.IsFormal)
            if this.Flags &&& EntityFlags.Intrinsic = EntityFlags.Intrinsic then
                this.Attributes
                |> ImArray.tryPick (fun x ->
                    match x.TryIntrinsicType with
                    | ValueSome x -> 
                        Some x
                    | _ -> 
                        None
                )
            else
                None

    type TypeSymbol with

        member this.IsNewtype =
            match stripTypeEquations this with
            | TypeSymbol.Entity(ent) -> ent.IsNewtype
            | _ -> false

        member this.IsNullable =
            match stripTypeEquations this with
            | TypeSymbol.Entity(ent) -> ent.IsNullable
            | TypeSymbol.Variable(tyPar) 
            | TypeSymbol.HigherVariable(tyPar, _) ->
                tyPar.Constraints
                |> ImArray.exists (function
                    | ConstraintSymbol.Null -> true
                    | _ -> false
                )
            // Object is always nullable.
            | TypeSymbol.BaseObject -> true
            | _ -> false

        member this.TryIntrinsicType =
            // TODO: Uncomment this.
            //OlyAssert.True(this.IsFormal)
            match stripTypeEquationsExceptAlias this with
            | TypeSymbol.Entity(ent) -> 
                if ent.IsFormal then
                    ent.TryIntrinsicType
                else
                    match ent.Formal.TryIntrinsicType with
                    | Some formalIntrinTy ->
                        Some(applyType formalIntrinTy ent.TypeArguments)
                    | _ ->
                        None
            | _ -> 
                if this.IsBuiltIn then
                    Some(this)
                else
                    None

type CompilerPass =
    | Pass0
    | Pass1
    | Pass2
    | Pass3
    | Pass4
    | PostInferenceAnalysis
    | LambdaLifting

// -----------------------------------------------
// - Substitution and application helpers
// -----------------------------------------------

[<AutoOpen>]
module SymbolHelpers =

    type IFunctionSymbol with

        member this.NewApply(enclosingTyArgs: TypeArgumentSymbol imarray, tyArgs: TypeArgumentSymbol imarray) =
            OlyAssert.True(this.IsFormal)
            if enclosingTyArgs.IsEmpty && tyArgs.IsEmpty then
                OlyAssert.True(this.Enclosing.TypeParameters.IsEmpty)
                OlyAssert.True(this.TypeParameters.IsEmpty)
                this
            else
                let enclosingTyArgs =
                    (this.Enclosing.TypeParameters, enclosingTyArgs)
                    ||> ImArray.map2 (fun tyPar tyArg ->
                        mkSolvedInferenceVariableType tyPar tyArg
                    )

                let tyArgs =
                    (this.TypeParameters, tyArgs)
                    ||> ImArray.map2 (fun tyPar tyArg ->
                        mkSolvedInferenceVariableType tyPar tyArg
                    )

                let enclosing = applyEnclosing enclosingTyArgs this.Enclosing
                actualFunction enclosing (enclosing.TypeArguments.AddRange(tyArgs)) this

        member this.NewSubstitute(enclosingTyArgs: TypeArgumentSymbol imarray, tyArgs: TypeArgumentSymbol imarray) =
            if enclosingTyArgs.IsEmpty && tyArgs.IsEmpty then
                this
            else
                let allTyArgs = enclosingTyArgs.AddRange(tyArgs)

                let enclosingTyArgs =
                    (this.Enclosing.TypeParameters, this.Enclosing.TypeArguments)
                    ||> ImArray.map2 (fun tyPar tyArg ->
                        mkSolvedInferenceVariableType tyPar (tyArg.Substitute(enclosingTyArgs))
                    )

                let tyArgs =
                    (this.TypeParameters, this.TypeArguments)
                    ||> ImArray.map2 (fun tyPar tyArg ->
                        mkSolvedInferenceVariableType tyPar (tyArg.Substitute(allTyArgs))
                    )

                this.Formal.AsFunction.NewApply(enclosingTyArgs, tyArgs)

        member this.NewSubstituteExtension(enclosingTyArgs: TypeArgumentSymbol imarray) =
            OlyAssert.True(this.Enclosing.IsTypeExtension)
            let ent = this.Enclosing.AsEntity
            let enclosing = EnclosingSymbol.Entity(ent.SubstituteExtension(enclosingTyArgs))
            actualFunction enclosing (enclosing.TypeArguments.AddRange(this.TypeArguments)) this.Formal.AsFunction

    type IValueSymbol with

        /// Gets the actual value via substitution by the provided type instantiations.
        /// Must provide all type instantiations within scope.
        /// A different enclosing can be provided as well.
        member this.GetActual(enclosing, tyArgs) =
            actualValue enclosing tyArgs this

        member this.Substitute(tyParLookup: IReadOnlyDictionary<_, _>): IValueSymbol =
            if tyParLookup.Count = 0 then
                this
            else
                let enclosing = tryActualEnclosing tyParLookup this.Enclosing
                tryActualValue enclosing tyParLookup this

        /// Similar to GetActual, but will only substitute type parameters associated with the given type arguments by the exact type parameter id.
        member this.Substitute(tyArgs: TypeArgumentSymbol imarray) =
            if tyArgs.IsEmpty then
                this
            else

            let tyParLookup =
                tyArgs
                |> filterForTypeParameters
                |> Dictionary
                |> ReadOnlyDictionary

            this.Substitute(tyParLookup)

        member this.ApplyConstructor(tyArgs: TypeArgumentSymbol imarray) =
            Assert.ThrowIfNot(this.Formal = this)
            Assert.ThrowIfNot(this.IsConstructor)

            let ent = this.Enclosing.TryEntity.Value
            let tyPars = ent.TypeParameters

            if tyArgs.IsEmpty && tyPars.IsEmpty then
                this
            else
                let tyArgs =
                    (tyPars, tyArgs)
                    ||> ImArray.map2 (fun closureTyPar tyArg ->
                        mkSolvedInferenceVariableType closureTyPar tyArg
                    )
                this.Substitute(tyArgs)

        member this.Apply(tyArgs: TypeArgumentSymbol imarray) =
            Assert.ThrowIfNot(this.Formal = this)
            Assert.ThrowIf(this.IsConstructor)
            Assert.ThrowIf(this.IsField)

            let tyPars = this.TypeParameters

            if tyArgs.IsEmpty && tyPars.IsEmpty then
                this
            else
                let tyArgs =
                    (tyPars, tyArgs)
                    ||> ImArray.map2 (fun closureTyPar tyArg ->
                        mkSolvedInferenceVariableType closureTyPar tyArg
                    )
                this.Substitute(tyArgs)

        member this.IsInvalid =
            this.ValueFlags &&& ValueFlags.Invalid = ValueFlags.Invalid

    type IFunctionSymbol with

        member this.IsEntryPoint =
            this.FunctionFlags &&& FunctionFlags.EntryPoint = FunctionFlags.EntryPoint

        member this.GetFormal() =
            this.Formal :?> IFunctionSymbol

        /// Gets the actual function via substitution by the provided type instantiations.
        /// Must provide all type instantiations within scope.
        /// A different enclosing can be provided as well.
        member this.GetActual(enclosing, tyArgs) =
            actualFunction enclosing tyArgs this

    type EntitySymbol with

        member this.Substitute(tyParLookup: IReadOnlyDictionary<_, _>) =
            tryActualEntity tyParLookup this

        member this.Substitute(tyArgs: TypeArgumentSymbol imarray) =
            substituteEntity tyArgs this

        member this.SubstituteExtension(tyArgs: TypeArgumentSymbol imarray) =
            OlyAssert.True(this.IsTypeExtension)
            let extendTy = this.Extends[0]
            let tyArgs =
                (extendTy.TypeArguments, tyArgs)
                ||> ImArray.choose2 (fun ty tyArg ->
                    if ty.IsTypeVariable then
                        mkSolvedInferenceVariableType ty.TryTypeParameter.Value tyArg
                        |> Some
                    else
                        None
                )
            this.Substitute(tyArgs)

        member this.Contains(ent: EntitySymbol) =
            this.Entities
            |> ImArray.exists (fun x -> x.Id = ent.Id)

        member this.IsLocal =
            match this.Enclosing with
            | EnclosingSymbol.Local -> true
            | EnclosingSymbol.Entity(ent) -> ent.IsLocal
            | _ -> false

    type TypeParameterSymbol with

        /// Emplace in this context means that if the lookup was successful, the result itself
        /// MUST be a type parameter.
        member this.EmplaceSubstitute(tyParLookup: IReadOnlyDictionary<_, TypeSymbol>) =
            if tyParLookup.Count = 0 then
                this
            else
                match tyParLookup.TryGetValue this.Id with
                | true, ty -> ty.TryImmedateTypeParameter.Value
                | _ -> this

    type WitnessSolution with

        /// Emplace in this context means that if a type parameter lookup was successful, the result itself
        /// MUST be a type parameter.
        static member EmplaceSubstitute(witnessArgs: WitnessSolution imarray, tyParLookup: IReadOnlyDictionary<_, TypeSymbol>) =
            let result =
                witnessArgs
                |> ImArray.map (fun (witnessArg: WitnessSolution) ->
                    let tyPar = witnessArg.TypeParameter
                    let ent = witnessArg.Entity.Substitute(tyParLookup)
                    let funcOpt =
                        witnessArg.Function
                        |> Option.map (fun func -> func.Substitute(tyParLookup) :?> IFunctionSymbol)
                    let witness =
                        let witness = witnessArg.Solution.Value
                        // Important: If the witness type is a type constructor, do not substitute its values.
                        //            Otherwise it would create a type that is not a type constructor and
                        //            make the constraint solutions invalid.
                        match witness with
                        | WitnessSymbol.Type(ty) when not ty.IsTypeConstructor ->
                            WitnessSymbol.Type(ty.Substitute(tyParLookup))
                        | WitnessSymbol.TypeExtension(tyExt, funcOpt) when not tyExt.IsTypeConstructor ->
                            let funcOpt =
                                funcOpt
                                |> Option.map (fun func -> func.Substitute(tyParLookup) :?> IFunctionSymbol)
                            WitnessSymbol.TypeExtension(tyExt.Substitute(tyParLookup), funcOpt)
                        | WitnessSymbol.TypeParameter(tyPar) ->
                            WitnessSymbol.TypeParameter(tyPar.EmplaceSubstitute(tyParLookup))
                        | _ ->
                            witness

                    let subbedWitnessArg = WitnessSolution(tyPar, ent, funcOpt)
                    subbedWitnessArg.Solution <- Some witness
                    subbedWitnessArg
                )
            result

    type TypeSymbol with

        member this.Substitute(tyParLookup: IReadOnlyDictionary<_, _>) =
            match stripTypeEquations this with
            | TypeSymbol.ForAll(tyPars, innerTy) ->
                TypeSymbol.ForAll(tyPars, tryActualType tyParLookup innerTy)
            | ty ->
                tryActualType tyParLookup ty

        member this.Substitute(tyArgs: TypeArgumentSymbol imarray) =
            match stripTypeEquations this with
            | TypeSymbol.ForAll(tyPars, innerTy) ->
                TypeSymbol.ForAll(tyPars, substituteType tyArgs innerTy)
            | ty ->
                substituteType tyArgs ty

        member this.GetClosureInvoke() =
            OlyAssert.Equal(true, this.IsClosure)
            match stripTypeEquations this with
            | TypeSymbol.Entity(ent) ->
                ent.Functions |> ImArray.filter (fun x -> not x.IsConstructor) |> Seq.head
            | _ ->
                OlyAssert.Fail("Expected closure invoke.")

    type ConstraintSymbol with

        member this.Substitute(tyArgs: TypeArgumentSymbol imarray) =
            match this with
            | ConstraintSymbol.Null
            | ConstraintSymbol.Struct
            | ConstraintSymbol.NotStruct 
            | ConstraintSymbol.Unmanaged
            | ConstraintSymbol.Blittable
            | ConstraintSymbol.Scoped -> this
            | ConstraintSymbol.ConstantType(ty) ->
                ConstraintSymbol.ConstantType(Lazy<_>.CreateFromValue(ty.Value.Substitute(tyArgs)))
            | ConstraintSymbol.SubtypeOf(ty) ->
                ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(ty.Value.Substitute(tyArgs)))

        member this.Substitute(tyParLookup: IReadOnlyDictionary<_, _>) =
            match this with
            | ConstraintSymbol.Null
            | ConstraintSymbol.Struct
            | ConstraintSymbol.NotStruct 
            | ConstraintSymbol.Unmanaged
            | ConstraintSymbol.Blittable
            | ConstraintSymbol.Scoped -> this
            | ConstraintSymbol.ConstantType(ty) ->
                ConstraintSymbol.ConstantType(Lazy<_>.CreateFromValue(ty.Value.Substitute(tyParLookup)))
            | ConstraintSymbol.SubtypeOf(ty) ->
                ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(ty.Value.Substitute(tyParLookup)))

// -----------------------------------------------
// - Custom asserts
// -----------------------------------------------

let assertNoForAllTypes (func: IFunctionSymbol) =
#if DEBUG || CHECKED
    match func.Enclosing with
    | EnclosingSymbol.Local -> ()
    | _ ->

    let enclosingTy = func.Enclosing.AsType
    func.Parameters
    |> ImArray.iter (fun par ->
        match stripTypeEquations par.Type with
        | TypeSymbol.ForAll _ -> OlyAssert.Fail("Parameters cannot be typed with 'ForAll'.")
        | _ -> ()

        if par.Type.IsTypeVariable then ()
        elif par.Type.IsTypeConstructor && enclosingTy.FormalId <> par.Type.FormalId then 
            
            // REVIEW/TODO: This is a little strange, but two shapes can be considered equivelant across assembly boundaries.
            //              Maybe the "FormalId" check above is just bad as we can't rely on it.
            if not enclosingTy.IsAnonymousShape && not par.Type.IsAnonymousShape then
                OlyAssert.Fail("Parameters cannot be typed with a non-variable type constructor.")
    )

    match stripTypeEquations func.ReturnType with
    | TypeSymbol.ForAll _ -> OlyAssert.Fail("Return types cannot be 'ForAll'.")
    | _ -> ()

    if func.ReturnType.IsTypeVariable then ()
    elif func.ReturnType.IsTypeConstructor && enclosingTy.FormalId <> func.ReturnType.FormalId then OlyAssert.Fail("Return types cannot be a non-variable type constructor.")
#endif
    ()