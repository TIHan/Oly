module internal rec Oly.Compiler.Internal.CompilerImports

open System
open System.Threading
open System.Collections.Generic
open System.Collections.Concurrent
open System.Diagnostics

open Oly.Core
open Oly.Metadata
open Oly.Compiler.Syntax // Only used for creating OlyDiagnostic without a syntax tree and/or text span.
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolBuilders

[<Sealed>]
type private RetargetedFunctionSymbol(currentAsmIdent: OlyILAssemblyIdentity, importer: Importer, func: IFunctionSymbol) =

    let id = newId()

    let lazyEnclosing: Lazy<EnclosingSymbol> =
        lazy
            retargetEnclosing currentAsmIdent importer func.Enclosing

    let lazyTyPars =
        lazy
            let enclosingTyPars = lazyEnclosing.Value.TypeParameters
            let funcTyPars: TypeParameterSymbol imarray = 
                func.TypeParameters
                |> ImArray.map (retargetTypeParameter currentAsmIdent importer)

            let tyPars = enclosingTyPars.AddRange(funcTyPars)

            (func.TypeParameters, funcTyPars)
            ||> ImArray.iter2 (fun oldTyPar tyPar ->
                if not oldTyPar.Constraints.IsEmpty then 
                    tyPar.SetConstraints(oldTyPar.Constraints |> ImArray.map (retargetConstraint currentAsmIdent importer tyPars))
            )

            funcTyPars

    let lazyTyArgs =
        lazy
            lazyTyPars.Value
            |> ImArray.map (fun (tyPar: TypeParameterSymbol) -> tyPar.AsType)

    let lazyPars =
        lazy
            let enclosingTyPars = lazyEnclosing.Value.TypeParameters
            let funcTyPars = lazyTyPars.Value
            let tyPars = enclosingTyPars.AddRange(funcTyPars)
            func.Parameters
            |> ImArray.map (retargetParameter currentAsmIdent importer tyPars)

    let lazyReturnTy =
        lazy
            let enclosingTyPars = lazyEnclosing.Value.TypeParameters
            let funcTyPars = lazyTyPars.Value
            let tyPars = enclosingTyPars.AddRange(funcTyPars)
            retargetType currentAsmIdent importer tyPars func.ReturnType

    let lazyTy =
        lazy
            let argTys = lazyPars.Value |> ImArray.map (fun (x: ILocalParameterSymbol) -> x.Type)
            TypeSymbol.CreateFunction(ImArray.empty (* TODO: This may not be right, we might want to pass typars. *), argTys, lazyReturnTy.Value)

    let lazyOverrides =
        lazy
            func.FunctionOverrides
            |> Option.map (retargetFunction currentAsmIdent importer)

    do
        OlyAssert.True(func.IsFormal)
        OlyAssert.False(func.IsBase)
        OlyAssert.False(func.IsField)
        OlyAssert.False(func.IsPattern)
        OlyAssert.False(func.IsProperty)
        OlyAssert.False(func.IsThis)
        OlyAssert.True(func.IsFunction)
    
    interface IFunctionSymbol with
        member this.AssociatedFormalPattern = func.AssociatedFormalPattern
        member this.Attributes = func.Attributes
        member this.Enclosing = lazyEnclosing.Value
        member this.Formal = this
        member this.FunctionFlags = func.FunctionFlags
        member this.FunctionOverrides = lazyOverrides.Value
        member this.Id = id
        member this.IsBase = false
        member this.IsField = false
        member this.IsFunction = true
        member this.IsPattern = false
        member this.IsProperty = false
        member this.IsThis = false
        member this.MemberFlags = func.MemberFlags
        member this.Name = func.Name
        member this.Parameters = lazyPars.Value
        member this.ReturnType = lazyReturnTy.Value
        member this.Semantic = func.Semantic
        member this.Type = lazyTy.Value
        member this.TypeArguments = lazyTyArgs.Value
        member this.TypeParameters = lazyTyPars.Value
        member this.ValueFlags = func.ValueFlags
        member this.WellKnownFunction = func.WellKnownFunction

[<Sealed>]
type private RetargetedEntitySymbol(currentAsmIdent: OlyILAssemblyIdentity, importer: Importer, ent: IEntitySymbol) =
    
    let id = newId()

    let lazyEnclosing =
        lazy
            retargetEnclosing currentAsmIdent importer ent.Enclosing

    let lazyTyPars =
        lazy
            let tyPars: TypeParameterSymbol imarray = 
                ent.TypeParameters
                |> ImArray.map (retargetTypeParameter currentAsmIdent importer)

            (ent.TypeParameters, tyPars)
            ||> ImArray.iter2 (fun oldTyPar tyPar ->
                if not oldTyPar.Constraints.IsEmpty then 
                    tyPar.SetConstraints(oldTyPar.Constraints |> ImArray.map (retargetConstraint currentAsmIdent importer tyPars))
            )

            tyPars

    let lazyTyArgs =
        lazy
            lazyTyPars.Value
            |> ImArray.map (fun (tyPar: TypeParameterSymbol) -> tyPar.AsType)

    let lazyEntities =
        lazy
            ent.Entities
            |> ImArray.map (retargetEntity currentAsmIdent importer)

    let lazyFunctions =
        lazy
            ent.Functions
            |> ImArray.map (retargetFunction currentAsmIdent importer)

    let lazyExtends =
        lazy
            let tyPars = lazyTyPars.Value
            ent.Extends
            |> ImArray.map (retargetType currentAsmIdent importer tyPars)

    let lazyImplements =
        lazy
            let tyPars = lazyTyPars.Value
            ent.Implements
            |> ImArray.map (retargetType currentAsmIdent importer tyPars)

    do
        OlyAssert.True(ent.IsFormal || ent.IsNamespace)

    interface IEntitySymbol with
        member this.Attributes = ent.Attributes
        member this.ContainingAssembly = ent.ContainingAssembly
        member this.Enclosing = lazyEnclosing.Value
        member this.Entities = lazyEntities.Value
        member this.Extends = lazyExtends.Value
        member this.Fields = ent.Fields
        member this.Flags = ent.Flags
        member this.Formal = this
        member this.Functions = lazyFunctions.Value
        member this.Id = id
        member this.Implements = lazyImplements.Value
        member this.InstanceConstructors = ent.InstanceConstructors
        member this.Kind = ent.Kind
        member this.Name = ent.Name
        member this.Patterns = ent.Patterns
        member this.Properties = ent.Properties
        member this.TypeArguments = lazyTyArgs.Value
        member this.TypeParameters = lazyTyPars.Value

let private retargetConstraint currentAsmIdent importer (tyPars: TypeParameterSymbol imarray) (constr: ConstraintSymbol) =
    match constr with
    | ConstraintSymbol.NotStruct
    | ConstraintSymbol.Struct
    | ConstraintSymbol.Null
    | ConstraintSymbol.Unmanaged -> constr
    | ConstraintSymbol.ConstantType(lazyTy) ->
        let ty = lazyTy.Value
        let rty = retargetType currentAsmIdent importer tyPars ty
        if obj.ReferenceEquals(rty, ty) then
            constr
        else
            ConstraintSymbol.ConstantType(Lazy<_>.CreateFromValue(rty))
    | ConstraintSymbol.SubtypeOf(lazyTy) ->
        let ty = lazyTy.Value
        let rty = retargetType currentAsmIdent importer tyPars ty
        if obj.ReferenceEquals(rty, ty) then
            constr
        else
            ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(rty))

let private retargetTypeParameter currentAsmIdent importer (tyPar: TypeParameterSymbol) =
    if tyPar.Constraints.IsEmpty then
        tyPar
    else
        TypeParameterSymbol(tyPar.Name, tyPar.Index, tyPar.Arity, tyPar.IsVariadic, tyPar.Kind, ref ImArray.empty)

let private retargetParameter currentAsmIdent importer (tyPars: TypeParameterSymbol imarray) (par: ILocalParameterSymbol) =
    match par with
    | :? LocalParameterSymbol ->
        LocalParameterSymbol(par.Attributes, par.Name, retargetType currentAsmIdent importer tyPars par.Type, par.IsThis, par.IsBase, par.IsMutable)
    | _ ->
        OlyAssert.Fail("Invalid parameter symbol")

let private retargetFunction currentAsmIdent importer ent =
    RetargetedFunctionSymbol(currentAsmIdent, importer, ent) :> IFunctionSymbol

let private retargetEntity currentAsmIdent (importer: Importer) (ent: IEntitySymbol) =
    if ent.IsAnonymous then
        RetargetedEntitySymbol(currentAsmIdent, importer, ent) :> IEntitySymbol
    else
       
        let qualName = ent.QualifiedName
        match importer.TryGetEntity(qualName) with
        | true, ent -> 
            OlyAssert.False(ent.IsAnonymous)
            ent
        | _ ->
            let ent = RetargetedEntitySymbol(currentAsmIdent, importer, ent) :> IEntitySymbol
            importer.AddEntity(qualName, ent)
            ent

let private retargetEnclosing currentAsmIdent importer enclosing =
    match enclosing with
    | EnclosingSymbol.Local
    | EnclosingSymbol.RootNamespace -> enclosing
    | EnclosingSymbol.Witness _ -> OlyAssert.Fail("Invalid enclosing symbol")
    | EnclosingSymbol.Entity(ent) ->
        let rent = retargetEntity currentAsmIdent importer ent
        if obj.ReferenceEquals(ent, rent) then
            enclosing
        else
            EnclosingSymbol.Entity(rent)

let private retargetType currentAsmIdent (importer: Importer) (tyPars: TypeParameterSymbol imarray) (ty: TypeSymbol) =
    match ty with
    | TypeSymbol.Variable(tyPar) ->
        let newTyPar = tyPars[tyPar.Index]
        OlyAssert.Equal(tyPar.Name, newTyPar.Name)
        TypeSymbol.Variable(newTyPar)

    | TypeSymbol.InferenceVariable(_, solution) when solution.HasSolution ->
        retargetType currentAsmIdent importer tyPars solution.Solution.Value

    | TypeSymbol.Entity(ent) ->
        if ent.IsFormal then
            let rent = retargetEntity currentAsmIdent importer ent
            if obj.ReferenceEquals(rent, ent) then
                ty
            else
                TypeSymbol.Entity(rent)
        else
            let formalEnt = ent.Formal
            let formalREnt = retargetEntity currentAsmIdent importer formalEnt
            if obj.ReferenceEquals(formalREnt, formalEnt) then
                ty
            else
                let tyArgs =
                    ent.TypeArguments |> ImArray.map (retargetType currentAsmIdent importer tyPars)
                TypeSymbol.Entity(formalREnt.Apply(tyArgs))

    | _ ->
        // TODO:
        ty

/// L2 cache that is to the current compilation.
[<NoEquality;NoComparison>]
type SharedImportCache =
    private {
        importedAsms: ConcurrentDictionary<string, OlyILAssembly>
        entFromName: ConcurrentDictionary<string, ConcurrentDictionary<QualifiedName, IEntitySymbol>>
        gate: obj
    }

    member this.AddEntity(ent: IEntitySymbol) =
        match ent.ContainingAssembly with
        | Some(asm) ->
            let identity = asm.Identity
            let ents =
                match this.entFromName.TryGetValue identity.Name with
                | true, ents -> ents
                | _ ->
                    lock this.gate (fun () ->
                        match this.entFromName.TryGetValue identity.Name with
                        | true, ents -> ents
                        | _ ->
                            let ents = ConcurrentDictionary()
                            this.entFromName.TryAdd(identity.Name, ents) |> ignore
                            ents
                    )
                
            ents.TryAdd(ent.QualifiedName, ent) |> ignore
        | _ ->
            ()

    member this.TryGetEntity(ilAsmIdentity: OlyILAssemblyIdentity, qualName: QualifiedName) =
        match this.entFromName.TryGetValue ilAsmIdentity.Name with
        | true, ents ->
            match ents.TryGetValue(qualName) with 
            | true, ent -> ValueSome ent
            | _ -> ValueNone
        | _ ->
            ValueNone

    member this.ContainsAssembly(ilAsmIdentity: OlyILAssemblyIdentity) =
        this.importedAsms.ContainsKey(ilAsmIdentity.Name)

    member this.AddAssembly(ilAsm: OlyILAssembly) =
        this.importedAsms.[ilAsm.Identity.Name] <- ilAsm

    static member Create() =
        let importedAsms = ConcurrentDictionary()
        let entFromName = ConcurrentDictionary()
        {
            importedAsms = importedAsms
            entFromName = entFromName
            gate = obj()
        }

let private StringImmutableArrayComparer() =
    { new EqualityComparer<string imarray>() with
        member _.GetHashCode(strs) = strs.Length
        member _.Equals(strs1, strs2) =
            (strs1, strs2)
            ||> ImArray.forall2 (fun str1 str2 ->
                str1.Equals(str2)
            )
    }

[<Sealed>]
type NamespaceEnvironment private (state: Dictionary<string imarray, NamespaceBuilder>) =

    member this.ForEach(f) =
        state.Values
        |> Seq.iter (fun x -> f x.Entity)

    member this.GetOrCreate(namespacePath: string imarray): NamespaceBuilder =
        if namespacePath.IsEmpty then
            invalidArg "namespacePath" "Path must not be empty."

        match state.TryGetValue(namespacePath) with
        | true, entBuilder -> entBuilder
        | _ ->
            let enclosing, enclosingNamespaceBuilderOpt =
                if namespacePath.Length > 1 then
                    let enclosingNamespaceBuilder = this.GetOrCreate(namespacePath.RemoveAt(namespacePath.Length - 1))
                    EnclosingSymbol.Entity(enclosingNamespaceBuilder.Entity), Some enclosingNamespaceBuilder
                else
                    EnclosingSymbol.RootNamespace, None
            let name = namespacePath.[namespacePath.Length - 1]
            let builder = NamespaceBuilder.Create(enclosing, name)

            match enclosingNamespaceBuilderOpt with
            | Some enclosingNamespaceBuilder -> enclosingNamespaceBuilder.AddEntity(builder.Entity, builder.Entity.LogicalTypeParameterCount)
            | _ -> ()

            state.[namespacePath] <- builder
            builder

    static member Create() =
        NamespaceEnvironment(Dictionary(StringImmutableArrayComparer()))

/// L1 cache that is local to the current reading assembly the handles are located in.
[<NoEquality;NoComparison>]
type internal LocalCache =
    private {
        identity: OlyILAssemblyIdentity
        tyFromEntDef: ConcurrentDictionary<OlyILEntityDefinitionHandle, TypeSymbol>
        tyFromEntRef: ConcurrentDictionary<OlyILEntityReferenceHandle, TypeSymbol>
        entFromEntDef: ConcurrentDictionary<OlyILEntityDefinitionHandle, IEntitySymbol>
        entFromEntRef: ConcurrentDictionary<OlyILEntityReferenceHandle, IEntitySymbol>
        funcFromFuncDef: ConcurrentDictionary<OlyILFunctionDefinitionHandle, IFunctionSymbol>
    }

    static member Create(identity) =
        {
            identity = identity
            tyFromEntDef = ConcurrentDictionary()
            tyFromEntRef = ConcurrentDictionary()
            entFromEntDef = ConcurrentDictionary()
            entFromEntRef = ConcurrentDictionary()
            funcFromFuncDef = ConcurrentDictionary()
        }

[<NoEquality;NoComparison>]
type Imports =
    internal {
        diagnostics: ResizeArray<OlyDiagnostic>
        namespaceEnv: NamespaceEnvironment
        localCaches: ConcurrentDictionary<string, LocalCache>
        localCacheOpt: LocalCache option
        sharedCache: SharedImportCache
    }

    member internal this.GetLocalCache(ilAsm: OlyILAssembly) =
        match this.localCacheOpt with
        | Some localCache when localCache.identity.Name = ilAsm.Identity.Name ->
            localCache
        | _ ->
            let localCache =
                match this.localCaches.TryGetValue(ilAsm.Identity.Name) with
                | true, localCache -> localCache
                | _ ->
                    let localCache = LocalCache.Create(ilAsm.Identity)
                    this.localCaches.[ilAsm.Identity.Name] <- localCache
                    localCache
            localCache

    member this.GetOrCreateLocalEntity(ilAsm: OlyILAssembly, ilEntDefHandle: OlyILEntityDefinitionHandle) =
        let localCache = this.GetLocalCache(ilAsm)
        match localCache.entFromEntDef.TryGetValue ilEntDefHandle with
        | true, ent -> 
            OlyAssert.False(ent.IsAnonymous)
            ent
        | _ ->
            let ent: IEntitySymbol = ImportedEntityDefinitionSymbol.Create(ilAsm, this, ilEntDefHandle)
            let asm =
                match ent.ContainingAssembly with
                | Some asm -> asm
                | _ -> failwith "Imported entity must have a containing assembly."

            let ent =
                let ilEntDef = ilAsm.GetEntityDefinition(ilEntDefHandle)

                // Anonymous
                if ilEntDef.NameHandle.IsNil then
                   ent
                else
                    match this.sharedCache.entFromName.TryGetValue(asm.Identity.Name) with
                    | true, ents ->
                        match ents.TryGetValue(ent.QualifiedName) with
                        | true, ent -> ent
                        | _ -> 
                            this.sharedCache.AddEntity(ent)
                            let funcs = ent.Functions
                            funcs
                            |> ImArray.iter (fun x ->
                                System.Diagnostics.Debug.WriteLine(x.Name)
                            )
                            ent
                    | _ ->
                        this.sharedCache.AddEntity(ent)
                        ent
            localCache.entFromEntDef.TryAdd(ilEntDefHandle, ent) |> ignore
            ent

    static member Create(diagnostics, namespaceEnv, sharedCache: SharedImportCache) =
        {
            diagnostics = diagnostics
            namespaceEnv = namespaceEnv
            sharedCache = sharedCache
            localCacheOpt = None
            localCaches = ConcurrentDictionary()
        }
        

[<NoEquality;NoComparison>]
type private cenv =
    {
        ilAsm: OlyILAssembly
        imports: Imports
        namespaceEnv: NamespaceEnvironment
    }

    member this.WithAssembly(ilNewAsm: OlyILAssembly) =
        let localCache = this.imports.GetLocalCache(ilNewAsm)
        { this with ilAsm = ilNewAsm; imports = { this.imports with localCacheOpt = Some localCache } }

let private importEntityKind (ilEntKind: OlyILEntityKind) =
    match ilEntKind with
    | OlyILEntityKind.Alias -> EntityKind.Alias
    | OlyILEntityKind.Class -> EntityKind.Class
    | OlyILEntityKind.Closure -> EntityKind.Closure
    | OlyILEntityKind.Interface -> EntityKind.Interface
    | OlyILEntityKind.Module -> EntityKind.Module
    | OlyILEntityKind.Shape -> EntityKind.Shape
    | OlyILEntityKind.Struct -> EntityKind.Struct
    | OlyILEntityKind.Enum -> EntityKind.Enum
    | OlyILEntityKind.TypeExtension -> EntityKind.TypeExtension
    | OlyILEntityKind.Attribute -> EntityKind.Attribute
    | OlyILEntityKind.Newtype -> EntityKind.Newtype
        
let private importTypeParameterSymbol (asm: OlyILAssembly) tyParIndex tyParKind (ilTyPar: OlyILTypeParameter) =
    TypeParameterSymbol(asm.GetStringOrEmpty(ilTyPar.NameHandle), tyParIndex, ilTyPar.Arity, tyParKind, ref ImArray.empty)

let private importTypeParameterSymbols cenv (enclosingTyPars: TypeParameterSymbol imarray) isFunc (ilTyPars: OlyILTypeParameter imarray) =
    let offset = enclosingTyPars.Length
    let tyPars =
        ilTyPars
        |> ImArray.mapi (fun i ilTyPar ->
            let tyParKind =
                if isFunc then
                    TypeParameterKind.Function(i)
                else
                    TypeParameterKind.Type
            importTypeParameterSymbol cenv.ilAsm (offset + i) tyParKind ilTyPar
        )

    let enclosingTyPars = 
        if isFunc then enclosingTyPars
        else enclosingTyPars.AddRange(tyPars)
    let funcTyPars =
        if isFunc then tyPars
        else ImArray.empty
            
    tyPars
    |> ImArray.iteri (fun i tyPar ->
        let constrs =
            ilTyPars.[i].Constraints
            |> ImArray.map (fun ilConstr ->
                match ilConstr with
                | OlyILConstraint.Null ->
                    ConstraintSymbol.Null
                | OlyILConstraint.Struct ->
                    ConstraintSymbol.Struct
                | OlyILConstraint.NotStruct ->
                    ConstraintSymbol.NotStruct
                | OlyILConstraint.Unmanaged ->
                    ConstraintSymbol.Unmanaged
                | OlyILConstraint.ConstantType(ilTy) ->
                    ConstraintSymbol.ConstantType(lazy importTypeSymbol cenv enclosingTyPars funcTyPars ilTy)
                | OlyILConstraint.SubtypeOf(ilTy) ->
                    ConstraintSymbol.SubtypeOf(lazy importTypeSymbol cenv enclosingTyPars funcTyPars ilTy)
            )
        tyPar.SetConstraints(constrs)
    )
    tyPars

let private importEntityFlags (ilEntFlags: OlyILEntityFlags) =
    let flags = EntityFlags.None

    let flags =
        if ilEntFlags.HasFlag(OlyILEntityFlags.Final) then
            flags ||| EntityFlags.Final
        else
            flags

    let flags =
        if ilEntFlags.HasFlag(OlyILEntityFlags.Abstract) then
            flags ||| EntityFlags.Abstract
        else
            flags

    let flags =
        if ilEntFlags.HasFlag(OlyILEntityFlags.AutoOpen) then
            flags ||| EntityFlags.AutoOpen
        else
            flags

    let flags =
        if ilEntFlags.HasFlag(OlyILEntityFlags.Nullable) then
            flags ||| EntityFlags.Nullable
        else
            flags

    flags

let private importFunctionTypeInfo (cenv: cenv) (enclosingTyPars: TypeParameterSymbol imarray) (funcTyPars: TypeParameterSymbol imarray) (ilArgTys: OlyILType imarray) (ilReturnTy: OlyILType) =
    let argTys =
        ilArgTys |> ImArray.map (fun x -> importTypeSymbol cenv enclosingTyPars funcTyPars x)

    let returnTy = importTypeSymbol cenv enclosingTyPars funcTyPars ilReturnTy

    argTys, returnTy

let private importTypeSymbol (cenv: cenv) (enclosingTyPars: TypeParameterSymbol imarray) (funcTyPars: TypeParameterSymbol imarray) (ilTy: OlyILType) : TypeSymbol =
    match ilTy with
    | OlyILType.OlyILTypeForAll(ilTyPars, ilInnerTy) ->
        TypeSymbol.ForAll(
            importTypeParameterSymbols cenv enclosingTyPars false ilTyPars,
            importTypeSymbol cenv enclosingTyPars funcTyPars ilInnerTy
        )
    | OlyILType.OlyILTypeDependentIndexer(ilInputValueTy, ilInnerTy) ->
        TypeSymbol.DependentIndexer(importTypeSymbol cenv enclosingTyPars funcTyPars ilInputValueTy, importTypeSymbol cenv enclosingTyPars funcTyPars ilInnerTy)
    | OlyILType.OlyILTypeNativeInt -> TypeSymbol.NativeInt
    | OlyILType.OlyILTypeNativeUInt -> TypeSymbol.NativeUInt
    | OlyILType.OlyILTypeNativePtr(ilElementTy) -> 
        match ilElementTy with
        | OlyILTypeVoid ->
            TypeSymbol.NativePtr(TypeSymbol.Void)
        | _ ->
            TypeSymbol.NativePtr(importTypeSymbol cenv enclosingTyPars funcTyPars ilElementTy)
    | OlyILType.OlyILTypeArray(ilElementTy, rank, ilKind) -> 
        let kind =
            match ilKind with
            | OlyILArrayKind.Immutable -> ArrayKind.Immutable
            | OlyILArrayKind.Mutable -> ArrayKind.Mutable
        TypeSymbol.Array(importTypeSymbol cenv enclosingTyPars funcTyPars ilElementTy, rank, kind)
    | OlyILType.OlyILTypeInt8 -> TypeSymbol.Int8
    | OlyILType.OlyILTypeUInt8 -> TypeSymbol.UInt8
    | OlyILType.OlyILTypeInt16 -> TypeSymbol.Int16
    | OlyILType.OlyILTypeUInt16 -> TypeSymbol.UInt16
    | OlyILType.OlyILTypeInt32 -> TypeSymbol.Int32
    | OlyILType.OlyILTypeUInt32 -> TypeSymbol.UInt32
    | OlyILType.OlyILTypeInt64 -> TypeSymbol.Int64
    | OlyILType.OlyILTypeUInt64 -> TypeSymbol.UInt64
    | OlyILType.OlyILTypeUtf16 -> TypeSymbol.Utf16
    | OlyILType.OlyILTypeBool -> TypeSymbol.Bool
    | OlyILType.OlyILTypeChar16 -> TypeSymbol.Char16
    | OlyILType.OlyILTypeFloat32 -> TypeSymbol.Float32
    | OlyILType.OlyILTypeFloat64 -> TypeSymbol.Float64
    | OlyILType.OlyILTypeUnit -> TypeSymbol.Tuple(ImArray.createOne TypeSymbol.Unit, ImArray.empty)
    | OlyILType.OlyILTypeVoid -> TypeSymbol.Unit
    | OlyILType.OlyILTypeBaseObject -> TypeSymbol.BaseObject
    | OlyILType.OlyILTypeBaseStruct -> TypeSymbol.BaseStruct
    | OlyILType.OlyILTypeBaseStructEnum -> TypeSymbol.BaseStructEnum
    | OlyILType.OlyILTypeBaseAttribute -> TypeSymbol.BaseAttribute
    | OlyILType.OlyILTypeByRef(ilElementTy, OlyILByRefKind.ReadWrite) -> TypeSymbol.ByRef(importTypeSymbol cenv enclosingTyPars funcTyPars ilElementTy, ByRefKind.ReadWrite)
    | OlyILType.OlyILTypeByRef(ilElementTy, OlyILByRefKind.Read) -> TypeSymbol.ByRef(importTypeSymbol cenv enclosingTyPars funcTyPars ilElementTy, ByRefKind.Read)
    | OlyILType.OlyILTypeRefCell(ilElementTy) -> TypeSymbol.RefCell(importTypeSymbol cenv enclosingTyPars funcTyPars ilElementTy)
    | OlyILType.OlyILTypeConstantInt32(n) -> TypeSymbol.ConstantInt32(n)

    | OlyILType.OlyILTypeFunction(ilArgTys, ilReturnTy) -> 
        let argTys, returnTy = importFunctionTypeInfo cenv enclosingTyPars funcTyPars ilArgTys ilReturnTy
        TypeSymbol.Function(argTys, returnTy)

    | OlyILType.OlyILTypeNativeFunctionPtr(ilCc, ilArgTys, ilReturnTy) ->
        let argTys, returnTy = importFunctionTypeInfo cenv enclosingTyPars funcTyPars ilArgTys ilReturnTy
        TypeSymbol.NativeFunctionPtr(ilCc, argTys, returnTy)

    | OlyILType.OlyILTypeTuple(ilElementTys, ilNameHandles) ->
        let names =
            ilNameHandles
            |> ImArray.map (fun x -> cenv.ilAsm.GetStringOrEmpty(x))
        TypeSymbol.Tuple(ilElementTys |> ImArray.map (importTypeSymbol cenv enclosingTyPars funcTyPars), names)
    | OlyILType.OlyILTypeVariable(index, ilKind) ->
        let tyPar =
            if ilKind = OlyILTypeVariableKind.Type then
                enclosingTyPars[index]
            else
                funcTyPars[index]
        TypeSymbol.Variable(tyPar)

    | OlyILType.OlyILTypeHigherVariable(index, ilTyInst, ilKind) ->
        let tyPar =
            if ilKind = OlyILTypeVariableKind.Type then
                enclosingTyPars[index]
            else
                funcTyPars[index]

        if tyPar.Arity = 0 || tyPar.Arity <> ilTyInst.Length then
            failwith "Invalid second-order type variable."

        TypeSymbol.HigherVariable(tyPar, ilTyInst |> ImArray.map (importTypeSymbol cenv enclosingTyPars funcTyPars))

    | OlyILType.OlyILTypeEntity(ilEntRef) ->
        TypeSymbol.Entity(importEntitySymbol cenv enclosingTyPars funcTyPars ilEntRef)

let private importEntitySymbolFromDefinition (cenv: cenv) (ilEntDefHandle: OlyILEntityDefinitionHandle) =
    cenv.imports.GetOrCreateLocalEntity(cenv.ilAsm, ilEntDefHandle)

let private getEnclosingOfILEntityInstance (ilAsm: OlyILAssembly) (ilEntInst: OlyILEntityInstance) =
    match ilEntInst with
    | OlyILEntityInstance(defOrRefHandle=defOrRefHandle)
    | OlyILEntityConstructor(defOrRefHandle=defOrRefHandle) -> 
        if defOrRefHandle.Kind = OlyILTableKind.EntityDefinition then
            ilAsm.GetEntityDefinition(defOrRefHandle).Enclosing
        else
            ilAsm.GetEntityReference(defOrRefHandle).Enclosing

let private getNameOfILEntityDefinition (ilAsm: OlyILAssembly) (ilEntDef: OlyILEntityDefinition) =
    let name = ilAsm.GetStringOrEmpty(ilEntDef.NameHandle)
    if ilEntDef.TypeParameters.IsEmpty then
        name
    else
        name + "````" + ilEntDef.TypeParameters.Length.ToString()

let private getQualifiedNameOfILEntityDefinition (ilAsm: OlyILAssembly) (ilEntDef: OlyILEntityDefinition) =
    let name = getNameOfILEntityDefinition ilAsm ilEntDef
    match ilEntDef with
    | OlyILEntityDefinition(enclosing=enclosing) ->
        let rec loop enclosing =
            match enclosing with
            | OlyILEnclosing.Namespace(path, _) ->
                (path |> ImArray.map ilAsm.GetStringOrEmpty)
    
            | OlyILEnclosing.Entity(ilEntInst) ->
                let enclosingName =
                    match ilEntInst with
                    | OlyILEntityInstance(ilDefOrRefHandle, _)
                    | OlyILEntityConstructor(ilDefOrRefHandle) ->
                        if ilDefOrRefHandle.Kind = OlyILTableKind.EntityDefinition then
                            ilAsm.GetEntityDefinition(ilDefOrRefHandle).NameHandle
                        else
                            ilAsm.GetEntityReference(ilDefOrRefHandle).NameHandle
                        |> ilAsm.GetStringOrEmpty
                (loop (getEnclosingOfILEntityInstance ilAsm ilEntInst)).Add(enclosingName).Add("::")
            | _ ->
                ImArray.empty
        (loop enclosing).Add(name)
        |> String.concat "."

let private tryFindEntityDefinition (qualName: QualifiedName) (ilAsm: OlyILAssembly) =
    ilAsm.EntityDefinitions
    |> Seq.tryFind (fun (_, ilEntDef) ->
        qualName = (getQualifiedNameOfILEntityDefinition ilAsm ilEntDef)
    )

let private getNameOfILEntityReference (ilAsm: OlyILAssembly) (ilEntRef: OlyILEntityReference) =
    let name = ilAsm.GetStringOrEmpty(ilEntRef.NameHandle)
    if ilEntRef.TypeParameterCount = 0 then
        name
    else
        name + "````" + ilEntRef.TypeParameterCount.ToString()

let private getQualifiedNameOfILEntityReference (ilAsm: OlyILAssembly) (ilEntRef: OlyILEntityReference) =
    let name = getNameOfILEntityReference ilAsm ilEntRef
    let rec loop enclosing =
        match enclosing with
        | OlyILEnclosing.Namespace(path, _) ->
            (path |> ImArray.map ilAsm.GetStringOrEmpty)
        | OlyILEnclosing.Entity(ilEntInst) ->
            let enclosingName =
                match ilEntInst with
                | OlyILEntityInstance(ilDefOrRefHandle, _)
                | OlyILEntityConstructor(ilDefOrRefHandle) ->
                    if ilDefOrRefHandle.Kind = OlyILTableKind.EntityDefinition then
                        ilAsm.GetEntityDefinition(ilDefOrRefHandle).NameHandle
                    else
                        ilAsm.GetEntityReference(ilDefOrRefHandle).NameHandle
                    |> ilAsm.GetStringOrEmpty
            (loop (getEnclosingOfILEntityInstance ilAsm ilEntInst)).Add(enclosingName).Add("::")
        | _ ->
            ImArray.empty
    (loop ilEntRef.Enclosing).Add(name)
    |> String.concat "."

let private tryFindEntityReference (qualName: QualifiedName) (ilAsm: OlyILAssembly) =
    ilAsm.EntityReferences
    |> Seq.tryFind (fun (_, ilEntRef) ->
        qualName = getQualifiedNameOfILEntityReference ilAsm ilEntRef
    )

let private findEntityDefinition cenv (qualName: QualifiedName) (ilEntRef: OlyILEntityReference) =
    let asmIdentity = cenv.ilAsm.GetAssemblyIdentity(ilEntRef)
    match cenv.imports.sharedCache.entFromName.TryGetValue(asmIdentity.Name) with
    | true, sharedEntCache when sharedEntCache.ContainsKey(qualName) ->
        match sharedEntCache.TryGetValue(qualName) with
        | true, sharedEnt ->
            sharedEnt
        | _ ->
            failwith "Should not happen"
    | _ ->
        match cenv.imports.sharedCache.importedAsms.TryGetValue(asmIdentity.Name) with
        | true, ilOtherAsm ->
            let foundOpt = tryFindEntityDefinition qualName ilOtherAsm

            match foundOpt with
            | Some(ilEntDefHandle, _ilEntDef) ->
                importEntitySymbolFromDefinition (cenv.WithAssembly(ilOtherAsm)) ilEntDefHandle
            | _ ->
                let found2Opt = tryFindEntityReference qualName ilOtherAsm
                match found2Opt with
                | Some(ilEntRefHandle2, ilEntRef2) when obj.ReferenceEquals(ilEntRef, ilEntRef2) |> not ->
                    findEntityDefinition cenv qualName ilEntRef2
                | _ ->
                    cenv.imports.diagnostics.Add(OlyDiagnostic.CreateError(sprintf "Unable to find '%s'." qualName))
                    invalidEntity
        | _ ->
            cenv.imports.diagnostics.Add(OlyDiagnostic.CreateError(sprintf "Unable to find assembly: %s::%s." asmIdentity.Name asmIdentity.Key))
            invalidEntity

let private importEntitySymbolFromReference (cenv: cenv) (ilEntRefHandle: OlyILEntityReferenceHandle) =
    let localCache = cenv.imports.GetLocalCache(cenv.ilAsm)
    match localCache.entFromEntRef.TryGetValue ilEntRefHandle with
    | true, x -> x
    | _ ->
        let ilEntRef = cenv.ilAsm.GetEntityReference(ilEntRefHandle)

        let qualName = getQualifiedNameOfILEntityReference cenv.ilAsm ilEntRef

        let ent = findEntityDefinition cenv qualName ilEntRef

        if obj.ReferenceEquals(invalidEntity, ent) |> not then
            localCache.entFromEntRef.[ilEntRefHandle] <- ent
            cenv.imports.sharedCache.AddEntity(ent)
        ent

let private importEntitySymbol (cenv: cenv) (enclosingTyPars: TypeParameterSymbol imarray) (funcTyPars: TypeParameterSymbol imarray) (ilEntRef: OlyILEntityInstance) =
    match ilEntRef with
    | OlyILEntityInstance.OlyILEntityInstance(ilEntDefOrSpecHandle, ilTyInst) ->
        let ent =
            if ilEntDefOrSpecHandle.Kind = OlyILTableKind.EntityDefinition then
                importEntitySymbolFromDefinition cenv ilEntDefOrSpecHandle
            else
                importEntitySymbolFromReference cenv ilEntDefOrSpecHandle

        let tyArgs =
            ilTyInst
            |> ImArray.map (importTypeSymbol cenv enclosingTyPars funcTyPars)

        actualEntity tyArgs ent

    | OlyILEntityInstance.OlyILEntityConstructor(ilEntDefOrSpecHandle) ->
        if ilEntDefOrSpecHandle.Kind = OlyILTableKind.EntityDefinition then
            importEntitySymbolFromDefinition cenv ilEntDefOrSpecHandle
        else
            importEntitySymbolFromReference cenv ilEntDefOrSpecHandle

let private importNamespace (namespaceEnv: NamespaceEnvironment) (path: string imarray) =
    if path.IsEmpty then failwith "Path cannot be empty."
    namespaceEnv.GetOrCreate(path)

let private importEnclosing (cenv: cenv) (entToAdd: IEntitySymbol) tyParCount (ilEnclosing: OlyILEnclosing) =
    match ilEnclosing with
    | OlyILEnclosing.Witness _ ->
        failwith "Witnesses are not allowed to be imported."
    | OlyILEnclosing.Entity(ilEntInst) ->
        let enclosingEnt = importEntitySymbolFromDefinition cenv ilEntInst.DefinitionOrReferenceHandle
        enclosingEnt.AsEnclosing
    | OlyILEnclosing.Namespace(path, _) ->
        if path.IsEmpty then
            EnclosingSymbol.RootNamespace
        else
            let path =
                path
                |> ImArray.map (fun x -> cenv.ilAsm.GetStringOrEmpty(x))
            let namespaceBuilder = importNamespace cenv.namespaceEnv path
            namespaceBuilder.AddEntity(entToAdd, tyParCount)
            EnclosingSymbol.Entity(namespaceBuilder.Entity)

let private importFunctionFromDefinition (cenv: cenv) (enclosingEnt: IEntitySymbol) (ilEnclosingEntDefHandle: OlyILEntityDefinitionHandle) (ilFuncDefHandle: OlyILFunctionDefinitionHandle) =
    let localCache = cenv.imports.GetLocalCache(cenv.ilAsm)
    match localCache.funcFromFuncDef.TryGetValue ilFuncDefHandle with
    | true, res -> res
    | _ ->
        let res = ImportedFunctionDefinitionSymbol(cenv.ilAsm, cenv.imports, enclosingEnt, ilEnclosingEntDefHandle, ilFuncDefHandle) :> IFunctionSymbol
        localCache.funcFromFuncDef.[ilFuncDefHandle] <- res
        res

let private importFunctionOverridesFromReference (cenv: cenv) (tyPars: TypeParameterSymbol imarray) (currentFunc: IFunctionSymbol) (ilFuncRef: OlyILFunctionReference) =
    match ilFuncRef with
    | OlyILFunctionReference(ilEnclosing, ilFuncSpecHandle) ->
        let ent =
            match ilEnclosing with
            | OlyILEnclosing.Entity(ilEntInst) ->
                importEntitySymbol cenv tyPars ImArray.empty ilEntInst
            | _ ->
                failwith "Expected entity."
        let ilFuncSpec = cenv.ilAsm.GetFunctionSpecification(ilFuncSpecHandle)
        
        match ilFuncSpec with
        | OlyILFunctionSpecification(isInstance, ilCallConv, ilName, ilTyPars, ilPars, ilReturnTy) ->
            let name = cenv.ilAsm.GetStringOrEmpty(ilName)
            if String.IsNullOrWhiteSpace name then
                failwith "Invalid name"

            let enclosingTyPars = ent.TypeParameters
            let funcTyPars = importTypeParameterSymbols cenv ent.TypeParameters true ilTyPars
            let pars = ilPars |> ImArray.map (fun ilPar -> importParameter cenv enclosingTyPars funcTyPars ilPar)
            let returnTy = importTypeSymbol cenv enclosingTyPars funcTyPars ilReturnTy

            let possibleFuncs = ent.AllLogicalFunctions
            let funcOpt = 
                possibleFuncs
                |> Seq.tryFind (fun func ->
                    (areEnclosingsEqual func.Enclosing currentFunc.Enclosing |> not) &&
                  //  func.FunctionOverrides.IsSome &&
                    // TODO: This isn't complete.
                    // TODO:
                    // TODO:
                    func.Name = name //&&
                    //func.TypeParameters.Length = ilTyPars.Length &&
                    //func.Parameters.Length = pars.Length &&
                    //(
                    //    (func.Parameters, pars)
                    //    ||> ImArray.forall2 areParameterSignaturesEqual
                    //) &&
                    //areTypesEqual func.ReturnType returnTy
                )

            match funcOpt with
            | Some func -> func
            | _ -> failwith $"Unable to import function overrides {name}."

let private importParameter (cenv: cenv) (enclosingTyPars: TypeParameterSymbol imarray) (funcTyPars: TypeParameterSymbol imarray) (ilPar: OlyILParameter) =
    let name = cenv.ilAsm.GetStringOrEmpty(ilPar.NameHandle)
    let ty = importTypeSymbol cenv enclosingTyPars funcTyPars ilPar.Type
    let isThis = false // TODO:
    let attrs = ImArray.empty // TODO:
    LocalParameterSymbol(attrs, name, ty, isThis, false, ilPar.IsMutable) :> ILocalParameterSymbol

let private importMemberFlags (ilMemberFlags: OlyILMemberFlags) =
    let flags =
        match ilMemberFlags &&& OlyILMemberFlags.AccessorMask with
        | OlyILMemberFlags.Public ->
            MemberFlags.Public
        | OlyILMemberFlags.Internal ->
            MemberFlags.Internal
        | OlyILMemberFlags.Protected ->
            MemberFlags.Protected
        | _ ->
            MemberFlags.Private

    let flags =
        if ilMemberFlags &&& OlyILMemberFlags.Static = OlyILMemberFlags.Static then
            flags
        else
            flags ||| MemberFlags.Instance

    let flags =
        if ilMemberFlags &&& OlyILMemberFlags.Abstract = OlyILMemberFlags.Abstract then
            flags ||| MemberFlags.Abstract
        else
            flags

    let flags =
        if ilMemberFlags &&& OlyILMemberFlags.Final = OlyILMemberFlags.Final then
            flags ||| MemberFlags.Sealed
        else
            flags

    let flags =
        if ilMemberFlags &&& OlyILMemberFlags.Virtual = OlyILMemberFlags.Virtual then
            flags ||| MemberFlags.Virtual
        else
            flags

    let flags =
        if ilMemberFlags &&& OlyILMemberFlags.NewSlot = OlyILMemberFlags.NewSlot then
            flags ||| MemberFlags.NewSlot
        else
            flags

    flags

let private importFunctionFlags (ilFuncFlags: OlyILFunctionFlags) =
    let funcFlags =
        if ilFuncFlags &&& OlyILFunctionFlags.Constructor = OlyILFunctionFlags.Constructor then
            FunctionFlags.Constructor
        else
            FunctionFlags.None

    let funcFlags =
        if ilFuncFlags &&& OlyILFunctionFlags.InlineMask = OlyILFunctionFlags.Inline then
            funcFlags ||| FunctionFlags.Inline
        elif ilFuncFlags &&& OlyILFunctionFlags.InlineMask = OlyILFunctionFlags.NotInline then
            funcFlags ||| FunctionFlags.NotInline
        else
            funcFlags

    let funcFlags =
        if ilFuncFlags.HasFlag(OlyILFunctionFlags.Pure) then
            funcFlags ||| FunctionFlags.Pure
        else
            funcFlags

    let funcFlags =
        if ilFuncFlags.HasFlag(OlyILFunctionFlags.ParameterLess) then
            funcFlags ||| FunctionFlags.ParameterLess
        else
            funcFlags

    funcFlags

let private importFieldFlags (ilFieldFlags: OlyILFieldFlags) =
    if ilFieldFlags &&& OlyILFieldFlags.Mutable = OlyILFieldFlags.Mutable then
        ValueFlags.Mutable
    else
        ValueFlags.None

let private importAttribute cenv (ilAttr: OlyILAttribute) =
    match ilAttr with
    | OlyILAttribute.Import(platform, path, name) ->
        let platform = cenv.ilAsm.GetStringOrEmpty(platform)
        let path = path |> ImArray.map cenv.ilAsm.GetStringOrEmpty
        let name = cenv.ilAsm.GetStringOrEmpty(name)
        AttributeSymbol.Import(platform, path, name)
    | OlyILAttribute.Export ->
        AttributeSymbol.Export
    | OlyILAttribute.Intrinsic(name) ->
        let name = cenv.ilAsm.GetStringOrEmpty(name)
        AttributeSymbol.Intrinsic(name)
    | OlyILAttribute.Constructor(funcInst, args, namedArgs) ->
        // TODO: 
        match funcInst with
        | OlyILFunctionInstance(enclosing, specHandle, tyArgs, witnesses) ->
            match enclosing with
            | OlyILEnclosing.Entity(entInst) ->   
                failwith "Importing attribute constructor not implemented yet."
             //   let funcRef = OlyILFunctionReference(entInst.AsType, specHandle)
             //   let olyFunc = importFunctionFromReference cenv ImArray.empty funcRef
             //   AttributeSymbol.Constructor(olyFunc, ImArray.empty, AttributeFlags.AllowOnAll)
            | _ ->
                failwith "Expected entity."

[<Sealed>]
[<DebuggerDisplay("{DebugName}")>]
type ImportedFunctionDefinitionSymbol(ilAsm: OlyILAssembly, imports: Imports, enclosingEnt: IEntitySymbol, ilEnclosingEntDefHandle: OlyILEntityDefinitionHandle, ilFuncDefHandle: OlyILFunctionDefinitionHandle) as this =
    
    let cenv = { ilAsm = ilAsm; imports = imports; namespaceEnv = imports.namespaceEnv }

    let id = newId()
    let mutable patOpt = None

    let ilFuncDef = cenv.ilAsm.GetFunctionDefinition(ilFuncDefHandle)
    let ilFuncSpec = cenv.ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)
    let funcFlags = importFunctionFlags ilFuncDef.Flags
    let memberFlags = importMemberFlags ilFuncDef.MemberFlags

    let enclosing = enclosingEnt.AsEnclosing

    let isInstance = memberFlags &&& MemberFlags.Instance = MemberFlags.Instance
    let isConstructor = funcFlags &&& FunctionFlags.Constructor = FunctionFlags.Constructor

    let ilCallConv = ilFuncSpec.CallingConvention

    let funcFlags =
        if ilCallConv.HasFlag(OlyILCallingConvention.Blittable) then
            funcFlags ||| FunctionFlags.Blittable
        else
            funcFlags

    let lazyValueFlags =
        lazy
            // Clean up value flags.
            if 
                    not ilFuncDef.IsStatic && 
                    (ilFuncDef.Flags.HasFlag(OlyILFunctionFlags.Mutable)) && 
                    not (ilFuncDef.Flags.HasFlag(OlyILFunctionFlags.Constructor)) && 
                    (enclosing.IsAnyStruct || enclosing.IsShape) then
                ValueFlags.Mutable
            else
                ValueFlags.None

    let lazyName =
        lazy
            let ilFuncDef = cenv.ilAsm.GetFunctionDefinition(ilFuncDefHandle)
            let ilFuncSpec = cenv.ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)
            cenv.ilAsm.GetStringOrEmpty(ilFuncSpec.NameHandle)

    let lazyTyPars =
        lazy
            importTypeParameterSymbols cenv enclosingEnt.TypeParameters true ilFuncSpec.TypeParameters

    let lazyTyArgs =
        lazy
            lazyTyPars.Value
            |> ImArray.map (fun tyPar -> tyPar.AsType)

    let lazyPars =
        lazy
            let ilFuncDef = cenv.ilAsm.GetFunctionDefinition(ilFuncDefHandle)
            let ilFuncSpec = cenv.ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)
            let enclosingTyPars = enclosingEnt.TypeParameters
            let funcTyPars = lazyTyPars.Value
            let pars =
                let ilPars = ilFuncSpec.Parameters
                let ilPars =
                    if this.IsInstance then
                        let ilTyArgs = ImArray.init enclosingEnt.TypeParameters.Length (fun i -> OlyILTypeVariable(i, OlyILTypeVariableKind.Type))
                        let ilEnclosingTy = OlyILTypeEntity(OlyILEntityInstance(ilEnclosingEntDefHandle, ilTyArgs))
                        let ilEnclosingTy =
                            if enclosingEnt.IsAnyStruct then
                                if enclosingEnt.IsReadOnly then
                                    OlyILTypeByRef(ilEnclosingTy, OlyILByRefKind.Read)
                                else
                                    OlyILTypeByRef(ilEnclosingTy, OlyILByRefKind.ReadWrite)
                            else
                                ilEnclosingTy
                        ImArray.createOne(OlyILParameter(OlyILTableIndex.CreateString(-1), ilEnclosingTy, false, false)).AddRange(ilPars)
                    else
                        ilPars

                ilPars
                |> ImArray.map (importParameter cenv enclosingTyPars funcTyPars)
            pars

    let lazyAttrs =
        lazy
            let attrs =
                ilFuncDef.Attributes
                |> ImArray.map (importAttribute cenv)
            if ilCallConv.HasFlag(OlyILCallingConvention.Blittable) then
                attrs.Add(AttributeSymbol.Blittable)
            else
                attrs

    let lazyReturnTy =
        lazy
            let ilFuncDef = cenv.ilAsm.GetFunctionDefinition(ilFuncDefHandle)
            let ilFuncSpec = cenv.ilAsm.GetFunctionSpecification(ilFuncDef.SpecificationHandle)

            let returnTy = 
                if isConstructor then
                    if isInstance then
                        enclosingEnt.AsType
                    else
                        TypeSymbol.Unit
                else
                    let tyPars = lazyTyPars.Value
                    importTypeSymbol cenv enclosingEnt.TypeParameters tyPars ilFuncSpec.ReturnType
            returnTy

    let lazyTy =
        lazy
            let pars = (this :> IFunctionSymbol).Parameters
            let tyPars = (this :> IFunctionSymbol).TypeParameters
            let returnTy = (this :> IFunctionSymbol).ReturnType
            let ty =
                if this.IsInstanceNotConstructor && pars.IsEmpty then
                    failwith "Expected full parameters."
                TypeSymbol.CreateFunction(tyPars, pars |> ImArray.map (fun x -> x.Type), returnTy)
            ty
        
    let lazyFuncOverrides =
        lazy
            ilFuncDef.Overrides
            |> Option.map (importFunctionOverridesFromReference cenv enclosingEnt.TypeParameters this)

    let lazyWellKnownFunc =
        lazy
            lazyAttrs.Value
            |> WellKnownFunction.TryFromAttributes
            |> Option.defaultValue WellKnownFunction.None

    member this.DebugName = lazyName.Value

    /// Mutability.
    member this.SetAssociatedFormalPattern(pat: IPatternSymbol) =
        patOpt <- Some pat

    interface IFunctionSymbol with

        member _.Enclosing = enclosing

        member _.Id = id

        member _.Name = lazyName.Value

        member this.Formal = this :> IValueSymbol

        member _.TypeParameters = lazyTyPars.Value

        member this.TypeArguments = lazyTyArgs.Value

        member _.Attributes = lazyAttrs.Value

        member this.Parameters = lazyPars.Value

        member this.ReturnType = lazyReturnTy.Value
        member _.IsField = false
        member _.FunctionFlags = funcFlags
        member _.MemberFlags = memberFlags
        member _.IsFunction = true
        member _.ValueFlags = lazyValueFlags.Value

        member this.Type = lazyTy.Value

        member this.FunctionOverrides =
            lazyFuncOverrides.Value

        member _.IsProperty = false
        member _.IsPattern = false

        member _.IsThis = false
        member _.IsBase = false

        member _.Semantic = NormalFunction

        member _.WellKnownFunction = lazyWellKnownFunc.Value
        member _.AssociatedFormalPattern = patOpt

[<Sealed>]
[<DebuggerDisplay("{DebugName}")>]
type ImportedFieldDefinitionSymbol (enclosing: EnclosingSymbol, ilAsm: OlyILAssembly, imports: Imports, ilFieldDefHandle: OlyILFieldDefinitionHandle) =
    
    let cenv = { ilAsm = ilAsm; imports = imports; namespaceEnv = imports.namespaceEnv }

    let id = newId()
    let ilFieldDef = cenv.ilAsm.GetFieldDefinition(ilFieldDefHandle)
    let valueFlags = importFieldFlags ilFieldDef.Flags
    let memberFlags = importMemberFlags ilFieldDef.MemberFlags

    let lazyName =
        lazy
            cenv.ilAsm.GetStringOrEmpty(ilFieldDef.NameHandle)

    let lazyTy =
        lazy
            let fieldTy = importTypeSymbol cenv enclosing.TypeParameters ImArray.empty ilFieldDef.Type
            if ilFieldDef.IsConstant && enclosing.IsEnum then
                match enclosing.TryType with
                | Some(enclosingTy) when not enclosingTy.Inherits.IsEmpty && areTypesEqual enclosingTy.Inherits.[0] fieldTy ->
                    enclosingTy
                | _ ->
                    fieldTy
            else
                fieldTy

    let lazyConstant =
        lazy
            match ilFieldDef with
            | OlyILFieldConstant(_, ilNamedConst, _) ->
                match ilNamedConst with
                | OlyILConstant.Int32(value) -> ConstantSymbol.Int32(value) |> ValueSome
                | OlyILConstant.Float32(value) -> ConstantSymbol.Float32(value) |> ValueSome
                | OlyILConstant.Float64(value) -> ConstantSymbol.Float64(value) |> ValueSome
                | _ -> ValueNone
            | _ ->
                ValueNone

    member _.DebugName = lazyName.Value

    interface IFieldSymbol with

        member this.Enclosing: EnclosingSymbol = enclosing

        member this.Formal: IValueSymbol = this :> IValueSymbol

        member this.FunctionFlags: FunctionFlags = FunctionFlags.None

        member this.FunctionOverrides: IFunctionSymbol option = None

        member _.IsProperty = false

        member _.IsPattern = false

        member this.Id: int64 = id

        member this.IsField: bool = true

        member this.IsFunction: bool = false

        member this.IsThis: bool = false

        member this.IsBase: bool = false

        member this.MemberFlags: MemberFlags = memberFlags

        member this.Name: string = lazyName.Value

        member this.Type: TypeSymbol = lazyTy.Value

        member this.TypeArguments: imarray<TypeArgumentSymbol> = ImArray.empty

        member this.TypeParameters: imarray<TypeParameterSymbol> = ImArray.empty

        member this.ValueFlags: ValueFlags = valueFlags

        member _.Attributes = ImArray.empty // TODO:

        member _.Constant = lazyConstant.Value

        member _.AssociatedFormalPropertyId = None

[<Sealed>]
[<DebuggerDisplay("{DebugName}")>]
type ImportedEntityDefinitionSymbol private (ilAsm: OlyILAssembly, imports: Imports, ilEntDefHandle: OlyILEntityDefinitionHandle) as this =

    let cenv = { ilAsm = ilAsm; imports = imports; namespaceEnv = imports.namespaceEnv }

    let id = newId()
    let ilEntDef = cenv.ilAsm.GetEntityDefinition(ilEntDefHandle)
    let name = cenv.ilAsm.GetStringOrEmpty(ilEntDef.NameHandle)
    let entFlags = importEntityFlags ilEntDef.Flags
    let entFlags =
        if ilEntDef.Attributes |> ImArray.exists (function OlyILAttribute.Intrinsic _ -> true | _ -> false) then
            entFlags ||| EntityFlags.Intrinsic
        else
            entFlags

    let lazyEnclosing =
        lazy
            importEnclosing cenv this ilEntDef.TypeParameters.Length ilEntDef.Enclosing

    let lazyTyPars =
        lazy
            let enclosingTyPars = lazyEnclosing.Value.TypeParameters
            enclosingTyPars.AddRange(importTypeParameterSymbols cenv enclosingTyPars false ilEntDef.TypeParameters)

    let lazyTyArgs =
        lazy
            lazyTyPars.Value
            |> ImArray.map (fun tyPar -> tyPar.AsType)

    let lazyEnts =
        lazy
            ilEntDef.EntityDefinitionHandles
            |> ImArray.map (fun ilEntDefHandle ->
                importEntitySymbolFromDefinition cenv ilEntDefHandle
            )

    let lazyInherits =
        lazy
            ilEntDef.Extends
            |> ImArray.map (fun ilTy ->
                match ilTy with
                | OlyILTypeVoid -> TypeSymbol.Void
                | _ ->
                    importTypeSymbol cenv lazyTyPars.Value ImArray.empty ilTy
            )

    let lazyImplements =
        lazy
            ilEntDef.Implements
            |> ImArray.map (importTypeSymbol cenv lazyTyPars.Value ImArray.empty)

    let lazyFuncs =
        lazy
            ilEntDef.FunctionHandles
            |> ImArray.map (importFunctionFromDefinition cenv this ilEntDefHandle)

    let lazyInstanceCtors =
        lazy
            lazyFuncs.Value
            |> ImArray.filter (fun func -> func.IsInstance && func.IsConstructor)

    let lazyFields =
        lazy
            let asEnclosing = this.AsEnclosing
            ilEntDef.FieldDefinitionHandles
            |> ImArray.map (fun ilFieldDefHandle ->
                ImportedFieldDefinitionSymbol(asEnclosing, ilAsm, imports, ilFieldDefHandle) :> IFieldSymbol
            )

    let lazyProps =
        lazy
            ilEntDef.PropertyDefinitionHandles
            |> ImArray.choose (fun ilPropDefHandle ->
                let ilPropDef = ilAsm.GetPropertyDefinition(ilPropDefHandle)
                let name = ilPropDef.NameHandle |> ilAsm.GetStringOrEmpty
                let attrs = ImArray.empty // TODO:
                let propTy = importTypeSymbol cenv lazyTyPars.Value ImArray.empty ilPropDef.Type

                let valueFlags = ValueFlags.None

                let getterOpt =
                    if ilPropDef.Getter.IsNil then
                        None
                    else
                        importFunctionFromDefinition cenv this ilEntDefHandle ilPropDef.Getter
                        |> Some

                let setterOpt =
                    if ilPropDef.Setter.IsNil then
                        None
                    else
                        importFunctionFromDefinition cenv this ilEntDefHandle ilPropDef.Setter
                        |> Some

                let isValid, memberFlags =
                    match getterOpt, setterOpt with
                    | Some(getter), Some(setter) ->
                        if getter.IsInstance = setter.IsInstance then
                            let getterAccessor = getter.MemberFlags &&& MemberFlags.AccessorMask
                            let setterAccessor = setter.MemberFlags &&& MemberFlags.AccessorMask

                            let memberFlags =
                                if getterAccessor < setterAccessor then
                                    getter.MemberFlags
                                elif setterAccessor < getterAccessor then
                                    setter.MemberFlags
                                else
                                    getter.MemberFlags
                            true, memberFlags
                        else
                            false, MemberFlags.None
                    | Some(getter), _ ->
                        true, getter.MemberFlags
                    | _, Some(setter) ->
                        true, setter.MemberFlags
                    | _ ->
                        false, MemberFlags.None                               

                if not isValid then
                    None
                else
                    let memberFlags = memberFlags &&& (MemberFlags.AccessorMask ||| MemberFlags.Instance)

                    // TODO: We should make a ImportedPropertyDefinitionSymbol to do this.
                    let id = newId()
                    let enclosing = EnclosingSymbol.Entity(this)
                    { new IPropertySymbol with
                          member this.Attributes = attrs
                          member this.BackingField = None
                          member this.Enclosing = enclosing
                          member this.Formal = this :> IValueSymbol
                          member this.FunctionFlags = FunctionFlags.None
                          member this.FunctionOverrides = None
                          member this.Getter = getterOpt
                          member this.Id = id
                          member this.IsBase = false
                          member this.IsField = false
                          member this.IsFunction = false
                          member this.IsPattern = false
                          member this.IsProperty = true
                          member this.IsThis = false
                          member this.MemberFlags = memberFlags
                          member this.Name = name
                          member this.Setter = setterOpt
                          member this.Type = propTy
                          member this.TypeArguments = ImArray.empty
                          member this.TypeParameters = ImArray.empty
                          member this.ValueFlags = valueFlags
                        
                    }
                    |> Some
            )

    let lazyAttrs =
        lazy
            ilEntDef.Attributes
            |> ImArray.map (importAttribute cenv)

    let lazyPats =
        lazy
            ImArray.empty // TODO:

    let kind = importEntityKind ilEntDef.Kind

    let containingAsmOpt = AssemblySymbol.IL(cenv.ilAsm.Identity) |> Some

    member _.DebugName = name

    interface IEntitySymbol with

        member _.ContainingAssembly = containingAsmOpt

        member _.Enclosing: EnclosingSymbol = lazyEnclosing.Value

        member _.Entities: IEntitySymbol imarray = lazyEnts.Value

        member _.Fields: IFieldSymbol imarray = lazyFields.Value

        member _.Properties: IPropertySymbol imarray = lazyProps.Value

        member _.Patterns: IPatternSymbol imarray = lazyPats.Value

        member _.Flags: EntityFlags = entFlags

        member this.Formal: IEntitySymbol = this :> IEntitySymbol

        member _.Functions: IFunctionSymbol imarray = lazyFuncs.Value

        member _.InstanceConstructors = lazyInstanceCtors.Value

        member _.Id: int64 = id

        member _.Implements: TypeSymbol imarray = lazyImplements.Value

        member this.Extends: TypeSymbol imarray = 
            lazyInherits.Value

        member _.Kind: EntityKind = kind

        member _.Name: string = name

        member _.TypeArguments: TypeSymbol imarray = lazyTyArgs.Value

        member _.TypeParameters: TypeParameterSymbol imarray = lazyTyPars.Value

        member _.Attributes = lazyAttrs.Value

    static member Create(asm: OlyILAssembly, imports: Imports, ilEntDefHandle: OlyILEntityDefinitionHandle) =
        ImportedEntityDefinitionSymbol(asm, imports, ilEntDefHandle) :> IEntitySymbol

/// Not thread safe.
[<Sealed>]
type Importer(namespaceEnv: NamespaceEnvironment, sharedCache: SharedImportCache) =

    let entities: ConcurrentDictionary<QualifiedName, IEntitySymbol> = ConcurrentDictionary()

    let currentAssemblies = ConcurrentDictionary()
    let ents = ResizeArray()

    let handleNamespace (ent: INamespaceSymbol) =
        let namespaceBuilder = importNamespace namespaceEnv ent.FullNamespacePath
        ent.Entities
        |> Seq.iter (fun ent ->
            if not ent.IsNamespace then
                namespaceBuilder.AddEntity(ent, ent.LogicalTypeParameterCount)
        )

    let handleEntity (ent: IEntitySymbol) =
        if ent.IsNamespace then
            handleNamespace ent
        else
            let rec loop (enclosing: EnclosingSymbol) =
                match enclosing with
                | EnclosingSymbol.Entity(ent) when ent.IsNamespace ->
                    handleNamespace ent
                | EnclosingSymbol.RootNamespace ->
                    ()
                | _ ->
                    loop enclosing.Enclosing
            ents.Add(ent)
            loop ent.Enclosing

    member this.ImportAssembly(ilAsm: OlyILAssembly) =
        sharedCache.AddAssembly(ilAsm)
        currentAssemblies.[ilAsm.Identity] <- ()

    member this.ImportEntity(ent: IEntitySymbol) =
        if ent.IsNamespace || ent.IsAnonymous then
            handleEntity ent
        else
            let qualName = ent.QualifiedName
            match entities.TryGetValue(qualName) with
            | false, _ ->
                entities[qualName] <- ent
                handleEntity ent
            | _ ->
                ()

    member this.TryGetEntity(qualName, rent: outref<IEntitySymbol>): bool =
        entities.TryGetValue(qualName, &rent)

    member this.AddEntity(qualName, rent: IEntitySymbol) =
        OlyAssert.False(rent.IsAnonymous)
        entities[qualName] <- rent

    member this.ImportAndRetargetEntity(currentAsmIdent: OlyILAssemblyIdentity, ent: IEntitySymbol) =
        let ent = retargetEntity currentAsmIdent this ent
        handleEntity ent

    member this.ForEachEntity(diagnostics, ct: CancellationToken, f, forEachPrimTy) =
        ct.ThrowIfCancellationRequested()

        sharedCache.importedAsms.Values
        |> Seq.iter (fun ilAsm ->
            ct.ThrowIfCancellationRequested()

            if currentAssemblies.ContainsKey(ilAsm.Identity) then
                let cenv =
                    {
                        namespaceEnv = namespaceEnv
                        ilAsm = ilAsm
                        imports = Imports.Create(diagnostics, namespaceEnv, sharedCache)
                    }
                ilAsm.EntityDefinitions
                |> Seq.iter (fun (ilEntDefHandle, _) ->
                    ct.ThrowIfCancellationRequested()
                    let ent = importEntitySymbolFromDefinition cenv ilEntDefHandle
                    ct.ThrowIfCancellationRequested()
                    let qualName = ent.QualifiedName
                    this.AddEntity(qualName, ent)
                    f ent
                )

                ilAsm.ForEachPrimitiveType(fun (ilTy, ilEntDefHandle) ->
                    let ty = importTypeSymbol cenv ImArray.empty ImArray.empty ilTy
                    ct.ThrowIfCancellationRequested()
                    let ent = importEntitySymbolFromDefinition cenv ilEntDefHandle
                    ct.ThrowIfCancellationRequested()
                    let qualName = ent.QualifiedName
                    this.AddEntity(qualName, ent)
                    forEachPrimTy ty ent
                )
        )

        for i = 0 to ents.Count - 1 do
            let ent = ents[i]
            f ent

        // Add namespaces last as it should be populated after we tried to import other entities.
        namespaceEnv.ForEach(f)

[<Sealed>]
type CompilerImports private (namespaceEnv, importer) =

    member _.NamespaceEnvironment = namespaceEnv

    member _.Importer = importer

    new(sharedCache) =
        let namespaceEnv = NamespaceEnvironment.Create()
        let importer = Importer(namespaceEnv, sharedCache)
        CompilerImports(namespaceEnv, importer)
