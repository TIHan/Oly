[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Pass2

open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.PrettyPrint

let private bindTopLevelBindingDeclaration cenv env syntaxBinding (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing syntaxBindingDecl =
    let binding = bindMemberBindingDeclaration cenv env attrs false memberFlags valueExplicitness propInfoOpt syntaxBindingDecl
    checkEnumForInvalidFieldOrFunction cenv syntaxBindingDecl binding

    checkSyntaxDeclarationBinding cenv enclosing memberFlags valueExplicitness syntaxBinding
    checkSyntaxBindingDeclaration cenv valueExplicitness syntaxBindingDecl

    match tryFindIntrinsicAttribute syntaxAttrs attrs with
    | ValueSome _ ->
        cenv.diagnostics.Error("Value has an 'intrinsic' attribute and must not be given an implementation.", 10, syntaxBindingDecl.Identifier)
    | _ ->
        match enclosing with
        | EnclosingSymbol.Entity(ent) when ent.TryCompilerIntrinsic.IsSome ->
            cenv.diagnostics.Error("Values has an 'intrinsic' attribute and must not be given an implementation.", 10, syntaxBindingDecl.Identifier)
        | _ ->
            ()

    binding

let private bindTopLevelBindingDeclarationSignature cenv env (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing (syntaxBindingDecl: OlySyntaxBindingDeclaration) =
    let attrs = 
        match enclosing with
        | EnclosingSymbol.Entity(ent) ->
            match ent.TryImportedInfo with
            | Some(platform, path, name) ->
                if attributesContainImport attrs then attrs
                else
                    attrs.Add(AttributeSymbol.Import(platform, path.Add(name), syntaxBindingDecl.Identifier.ValueText))
            | _ ->
                attrs
        | _ ->
            attrs

    let binding = bindMemberBindingDeclaration cenv env attrs true memberFlags valueExplicitness propInfoOpt syntaxBindingDecl
    checkEnumForInvalidFieldOrFunction cenv syntaxBindingDecl binding

    if checkBindingSignature cenv attrs enclosing binding memberFlags valueExplicitness syntaxBindingDecl then
        checkSyntaxBindingDeclaration cenv valueExplicitness syntaxBindingDecl

    match binding.Value with
    | :? IFunctionSymbol as func ->
        AttributeBinder.bindIntrinsicPrimitivesForFunction cenv env syntaxAttrs func
    | _ ->
        ()

    binding

let private bindTopLevelBinding cenv env (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing syntaxBinding =
    match syntaxBinding with
    | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _) ->
        bindTopLevelBindingDeclaration cenv env syntaxBinding (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing syntaxBindingDecl

    | OlySyntaxBinding.Signature(syntaxBindingDecl) ->
        bindTopLevelBindingDeclarationSignature cenv env (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing syntaxBindingDecl

    | OlySyntaxBinding.PatternWithGuard(syntaxBindingDecl, syntaxGuardBinding) ->
        let bindingInfo = bindTopLevelBindingDeclaration cenv env syntaxBinding (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing syntaxBindingDecl

        match bindingInfo with
        | BindingPattern(pat, _) ->
            match syntaxGuardBinding with
            | OlySyntaxGuardBinding.Implementation(_, syntaxIdentOptional, _, _, _, _, _)
            | OlySyntaxGuardBinding.Signature(_, syntaxIdentOptional) ->
                let tyPars = bindingInfo.Value.TypeParameters

                let pars =
                    match bindingInfo.Value with
                    | :? IFunctionSymbol as func when func.Parameters.Length >= 1 ->
                        func.Parameters
                    | _ ->
                        ImArray.createOne(createLocalParameterValue(ImArray.empty, "", TypeSymbol.Error(None), false))

                let returnTy = TypeSymbol.Bool
                let guardFuncName = 
                    "guard_" + 
                        if syntaxIdentOptional.IsDummy then
                            bindingInfo.Value.Name
                        else
                            syntaxIdentOptional.ValueText
                let guardFunc = 
                    createFunctionValueSemantic
                        enclosing
                        ImArray.empty
                        guardFuncName
                        tyPars
                        pars
                        returnTy
                        memberFlags
                        bindingInfo.Value.FunctionFlags
                        FunctionSemantic.PatternGuardFunction
                        WellKnownFunction.None
                        None
                        false

                pat.SetPatternGuardFunction_Pass2_NonConcurrent(guardFunc)

            | _ ->
                raise(InternalCompilerException())

            bindingInfo
        | _ ->
            cenv.diagnostics.Error("Expected 'pattern' with 'when'.", 10, syntaxBinding.Declaration.Identifier)
            bindingInfo

    | OlySyntaxBinding.Property(syntaxBindingDecl, syntaxBindingList) ->
        match propInfoOpt with
        | Some _ ->
            cenv.diagnostics.Error("Invalid property declaration.", 10, syntaxBindingDecl)
            invalidBinding syntaxBindingDecl.Identifier.ValueText
        | _ ->

        let propName, propTy =
            match syntaxBindingDecl with
            | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxReturnTyAnnot) ->
                syntaxIdent.ValueText, bindReturnTypeAnnotation cenv env syntaxReturnTyAnnot
            | _ ->
                cenv.diagnostics.Error("Invalid property declaration.", 10, syntaxBindingDecl)
                "", invalidType()

        let syntaxBindings = syntaxBindingList.ChildrenOfType
        if syntaxBindings.IsEmpty then
            raise(InternalCompilerException())

        if syntaxBindings.Length > 2 then
            cenv.diagnostics.Error($"Too many bindings for property '{propName}'.", 10, syntaxBindingDecl)

        let getOrSet1 =
            match syntaxBindings[0] with
            | OlySyntaxPropertyBinding.Binding(syntaxAttrs, syntaxAccessor, syntaxValueDeclPremodifiers, syntaxValueDeclKind, syntaxBinding) ->
                match syntaxValueDeclKind with
                | OlySyntaxValueDeclarationKind.None _
                | OlySyntaxValueDeclarationKind.Error _ -> ()
                | _ ->
                    cenv.diagnostics.Error("Invalid value declaration.", 10, syntaxBinding)
                bindTopLevelValueDeclaration cenv env (Some(propName, propTy, valueExplicitness)) syntaxAttrs syntaxAccessor syntaxValueDeclPremodifiers.ChildrenOfType syntaxValueDeclKind ImArray.empty syntaxBinding
            | _ ->
                raise(InternalCompilerUnreachedException())

        let getOrSetOpt2 =
            if syntaxBindings.Length = 2 then
                match syntaxBindings[1] with
                | OlySyntaxPropertyBinding.Binding(syntaxAttrs, syntaxAccessor, syntaxValueDeclPremodifiers, syntaxValueDeclKind, syntaxBinding) ->
                    match syntaxValueDeclKind with
                    | OlySyntaxValueDeclarationKind.None _
                    | OlySyntaxValueDeclarationKind.Error _ -> ()
                    | _ ->
                        cenv.diagnostics.Error("Invalid value declaration.", 10, syntaxBinding)
                    let bindingInfo = bindTopLevelValueDeclaration cenv env (Some(propName, propTy, valueExplicitness)) syntaxAttrs syntaxAccessor syntaxValueDeclPremodifiers.ChildrenOfType syntaxValueDeclKind ImArray.empty syntaxBinding
                    Some bindingInfo
                | _ ->
                    raise(InternalCompilerUnreachedException())
            else
                None

        let getterOpt, setterOpt =
            match getOrSet1 with
            | BindingFunction(func) ->
                match func.Semantic with
                | GetterFunction ->
                    match getOrSetOpt2 with
                    | None ->
                        Some func, None
                    | Some(bindingInfo) ->
                        match bindingInfo with
                        | BindingFunction(func2) ->
                            match func2.Semantic with
                            | SetterFunction ->
                                Some func, Some func2
                            | GetterFunction ->
                                cenv.diagnostics.Error("More than one 'get' binding declaration.", 10, syntaxBindingDecl)
                                Some func, None
                            | _ ->
                                Some func, None
                        | _ ->
                            Some func, None

                | SetterFunction ->
                    match getOrSetOpt2 with
                    | None ->
                        None, Some func
                    | Some(bindingInfo) ->
                        match bindingInfo with
                        | BindingFunction(func2) ->
                            match func2.Semantic with
                            | GetterFunction ->
                                Some func2, Some func
                            | SetterFunction ->
                                cenv.diagnostics.Error("More than one 'set' binding declaration.", 10, syntaxBindingDecl)
                                None, Some func
                            | _ ->
                                None, Some func
                        | _ ->
                            None, Some func

                | _ ->
                    None, None
                    
            | _ ->
                None, None

        let prop = createPropertyValue enclosing attrs propName propTy memberFlags getterOpt setterOpt None
        let getterOrSetterBindings =
            seq {
                getOrSet1
                match getOrSetOpt2 with
                | Some(bindingInfo) ->
                    bindingInfo
                | _ ->
                    ()
            }
            |> ImArray.ofSeq

        BindingProperty(getterOrSetterBindings, prop)

    | _ ->
        raise(InternalCompilerException())

let private bindTopLevelValueDeclaration 
        cenv 
        env 
        propInfoOpt 
        syntaxAttrs 
        (syntaxAccessor: OlySyntaxAccessor) 
        syntaxValueDeclPremodifiers 
        syntaxValueDeclKind 
        syntaxValueDeclPostmodifiers 
        syntaxBinding =
    let enclosing = currentEnclosing env

    let isStaticProp =
        match propInfoOpt with
        | Some(_, _, propExplicitness) -> propExplicitness.IsExplicitStatic
        | _ -> false

    let memberFlags, valueExplicitness = 
        bindValueModifiersAndKindAsMemberFlags 
            cenv 
            env 
            isStaticProp 
            syntaxValueDeclPremodifiers 
            syntaxValueDeclKind 
            syntaxValueDeclPostmodifiers

    let valueExplicitness =
        match syntaxBinding with
        | OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Get _, _, _)
        | OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Get _) ->
            { valueExplicitness with IsExplicitGet = true }
        | OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Set _, _, _) 
        | OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Set _) ->
            { valueExplicitness with IsExplicitSet = true }
        | _ ->
            valueExplicitness

    let memberFlags =
        match syntaxAccessor with
        | OlySyntaxAccessor.Private _ -> memberFlags ||| MemberFlags.Private
        | OlySyntaxAccessor.Internal _ -> memberFlags ||| MemberFlags.Internal
        | OlySyntaxAccessor.Protected _ -> memberFlags ||| MemberFlags.Protected
        | _ -> memberFlags ||| MemberFlags.Public

    let attrs = bindAttributes cenv env false syntaxAttrs

    bindTopLevelBinding cenv env (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing syntaxBinding

let private canAddImplicitDefaultConstructor (ent: IEntitySymbol) =
    not ent.IsCompilerIntrinsic && 
    not ent.IsImported && 
    ((ent.IsClass || ent.IsStruct || ent.IsNewtype || ent.IsModule) && not ent.IsAlias)

let private addImplicitDefaultConstructor (cenv: cenv) (entBuilder: EntitySymbolBuilder) =
    let ent = entBuilder.Entity
    if not ent.IsModule then
        // Instance
        let hasInstanceCtor =
            ent.Functions
            |> ImArray.exists (fun x -> x.IsInstanceConstructor)

        if not hasInstanceCtor then
            let enclosing = ent.AsEnclosing
            let enclosingTy = ent.AsType
            // TODO: We do something similar in Logic.fs, we should re-use some of it.
            let parsWithInstance =
                let tryAddrTy (ty: TypeSymbol) =
                    if ty.IsAnyStruct then
                        TypeSymbol.CreateByRef(ty, ByRefKind.ReadWrite)
                    else
                        ty
                ImArray.createOne (createLocalParameterValue(ImArray.empty, "", tryAddrTy enclosingTy, false))

            let parsWithInstance =
                if ent.IsNewtype then
                    let parTy =
                        if ent.Extends.Length <> 1 then
                            TypeSymbol.Error(None)
                        else
                            ent.Extends[0]
                    parsWithInstance.Add(createLocalParameterValue(ImArray.empty, "", parTy, false))
                else
                    parsWithInstance

            let ctor = 
                createFunctionValue 
                    enclosing
                    ImArray.empty 
                    "__oly_static_ctor"
                    ImArray.empty
                    parsWithInstance
                    enclosingTy
                    (MemberFlags.Instance ||| MemberFlags.Public)
                    FunctionFlags.ImplicitDefaultConstructor
                    WellKnownFunction.None
                    None
                    false
            entBuilder.SetFunctions(cenv.pass, ent.Functions.Add(ctor))

    // Static
    let hasStaticCtor =
        ent.Functions
        |> ImArray.exists (fun x -> x.IsStaticConstructor)

    let hasStaticFields =
        ent.Fields
        |> ImArray.exists (fun x -> not x.IsInstance)
        
    if not ent.IsEnum && not hasStaticCtor && hasStaticFields then
        let enclosing = ent.AsEnclosing
        let enclosingTy = ent.AsType

        let ctor = 
            createFunctionValue 
                enclosing
                ImArray.empty 
                "__oly_static_ctor"
                ImArray.empty
                ImArray.empty
                enclosingTy
                MemberFlags.Public
                FunctionFlags.ImplicitDefaultConstructor
                WellKnownFunction.None
                None
                false
        entBuilder.SetFunctions(cenv.pass, ent.Functions.Add(ctor))

let private bindTypeDeclarationCases (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) (syntaxCases: OlySyntaxTypeDeclarationCase imarray) =
    let ent = entBuilder.Entity
    if ent.IsEnum then
        let enclosing = ent.AsEnclosing
        let ty = ent.AsType

        let mutable hasExplicitNonEnumCase = false
        let mutable extendsTyOpt = (Some ent.Extends[0])
        let mutable requireAllCasesAConstant = false
        let mutable autoIncrement = 0
        let fieldConstants =
            syntaxCases
            |> ImArray.choose (fun syntaxCase ->
                match syntaxCase with
                | OlySyntaxTypeDeclarationCase.EnumCase(_, syntaxIdent, _, syntaxExpr) ->
                    requireAllCasesAConstant <- true
                    let constantExpr = bindConstantExpression cenv env extendsTyOpt syntaxExpr
                    match tryEvaluateFixedIntegralConstantExpression cenv env extendsTyOpt constantExpr with
                    | ValueSome(_value, constantSymbol) ->
                        let field = createFieldConstant enclosing ImArray.empty syntaxIdent.ValueText ty MemberFlags.Public constantSymbol
                        recordValueDeclaration cenv field syntaxIdent
                        Some field
                    | _ ->
                        cenv.diagnostics.Error("Invalid constant for enum case.", 10, syntaxCase)
                        None
                | OlySyntaxTypeDeclarationCase.Case(_, syntaxIdent) ->
                    hasExplicitNonEnumCase <- true
                    if requireAllCasesAConstant then
                        None
                    else
                        let constantSymbol = ConstantSymbol.Int32(int32 autoIncrement)
                        autoIncrement <- autoIncrement + 1
                        let field = createFieldConstant enclosing ImArray.empty syntaxIdent.ValueText ty MemberFlags.Public constantSymbol
                        recordValueDeclaration cenv field syntaxIdent
                        Some field
                | _ ->
                    raise(InternalCompilerUnreachedException())
            )

        if requireAllCasesAConstant && hasExplicitNonEnumCase then
            syntaxCases
            |> ImArray.iter (fun syntaxCase ->
                match syntaxCase with
                | OlySyntaxTypeDeclarationCase.Case(_, syntaxIdent) ->
                    cenv.diagnostics.Error("Enum case requires a constant.", 10, syntaxIdent)
                | _ ->
                    ()
            )

        entBuilder.SetFields(cenv.pass, fieldConstants)

/// Pass 2 - Type declaration.
let bindTypeDeclarationPass2 (cenv: cenv) (env: BinderEnvironment) (entities: EntitySymbolBuilder imarray) syntaxIdent (syntaxTyPars: OlySyntaxTypeParameters) syntaxTyDefBody =
    let envBody = setSkipCheckTypeConstructor env

    let entBuilder = entities.[cenv.entityDefIndex]
    cenv.entityDefIndex <- 0

    let envBody =
        if entBuilder.Entity.IsModule then
            if entBuilder.Entity.IsAutoOpenable then
                envBody
            else
                openContentsOfEntityAndOverride envBody OpenContent.Entities entBuilder.Entity
        else
            envBody

    let envBody = addTypeParametersFromEntity cenv envBody syntaxTyPars.Values entBuilder.Entity

    bindTypeDeclarationBodyPass2 cenv envBody entBuilder.NestedEntityBuilders entBuilder syntaxTyDefBody

    // TODO: We need to do this in the very top-level ModuleDefinition.
    checkEntityExport cenv env syntaxIdent entBuilder.Entity

let addBindingDeclarationsToEntityPass2 cenv env (bindings: (BindingInfoSymbol * bool) imarray) (entBuilder: EntitySymbolBuilder) =
    let funcs = ImArray.builder()
    let fields = ImArray.builder()
    let props = ImArray.builder()
    let pats = ImArray.builder()
    bindings
    |> ImArray.iter (fun (bindingInfo, _) ->
        match bindingInfo with
        | BindingPattern(pat, _) ->
            pats.Add(pat)
        | _ ->
            ()
        // TODO: Just use pattern matching on bindingInfo instead of value.
        match bindingInfo.Value with
        | :? IFieldSymbol as field ->
            fields.Add field

        | :? FunctionSymbol as func ->
            funcs.Add func

        | :? PropertySymbol as prop ->
            match prop.Getter, prop.Setter with
            | None, None -> ()
            | getterOpt, setterOpt ->
                props.Add prop
                getterOpt |> Option.iter funcs.Add
                setterOpt |> Option.iter funcs.Add
                prop.BackingField |> Option.iter fields.Add

        | _ ->
            ()

        match bindingInfo with
        | BindingPattern(pat, _) ->
            pat.PatternGuardFunction
            |> Option.iter (fun func ->
                match func with
                | :? FunctionSymbol as func ->
                    funcs.Add func
                | _ ->
                    ()
            )
        | _ ->
            ()
    )

    entBuilder.SetBindings(cenv.pass, bindings)
    if entBuilder.Entity.IsEnum then ()
    else
        entBuilder.SetFields(cenv.pass, fields.ToImmutable())
        entBuilder.SetProperties(cenv.pass, props.ToImmutable())
        entBuilder.SetFunctions(cenv.pass, funcs.ToImmutable())
        entBuilder.SetPatterns(cenv.pass, pats.ToImmutable())

let private bindBodyExpressionPass2 cenv env supers entities (entBuilder: EntitySymbolBuilder) syntaxBodyExpr =
    let fieldsAndFuncs, _ = bindTopLevelExpressionPass2 cenv env supers entities ImmutableArray.Empty syntaxBodyExpr

    addBindingDeclarationsToEntityPass2 cenv env fieldsAndFuncs entBuilder

/// Pass 2 - Gather all entity definitions.
let bindTypeDeclarationBodyPass2 (cenv: cenv) (env: BinderEnvironment) entities (entBuilder: EntitySymbolBuilder) (syntaxEntDefBody: OlySyntaxTypeDeclarationBody) =
    let env = setSkipCheckTypeConstructor env

    let envWithEnclosing = 
        env.SetEnclosing(EnclosingSymbol.Entity(entBuilder.Entity)).SetEnclosingTypeParameters(entBuilder.Entity.TypeParameters)

    match syntaxEntDefBody with
    | OlySyntaxTypeDeclarationBody.None _ ->
        ()

    | OlySyntaxTypeDeclarationBody.Body(syntaxExtends, syntaxImplements, syntaxCaseList, syntaxExpr) ->
        let supers =
            filterTypesAsInterfaces (entBuilder.Entity.AllLogicalInheritsAndImplements)
            |> ImmutableArray.CreateRange

        bindTypeDeclarationCases cenv env entBuilder syntaxCaseList.ChildrenOfType
        bindBodyExpressionPass2 cenv envWithEnclosing supers entities entBuilder syntaxExpr

        // Re-bind 'extends' and 'implements' to perform checks.
        let _extends = bindExtends cenv envWithEnclosing syntaxExtends // Rebind to check constraints
        let _implements = bindImplements cenv envWithEnclosing syntaxImplements // Rebind to check constraints
        ()

    | _ ->
        raise(InternalCompilerException())

    // Implicit default constructor logic
    if canAddImplicitDefaultConstructor entBuilder.Entity then
        addImplicitDefaultConstructor cenv entBuilder

/// Pass 2 - Gather all value definitions.
let bindTopLevelExpressionPass2 (cenv: cenv) (env: BinderEnvironment) (supers: ImmutableArray<TypeSymbol>) (entities: EntitySymbolBuilder imarray) (bindings: (BindingInfoSymbol * bool) imarray) (syntaxExpr: OlySyntaxExpression) : (BindingInfoSymbol * bool) imarray * BinderEnvironment =
    cenv.ct.ThrowIfCancellationRequested()

    match syntaxExpr with
    | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, syntaxAccessor, syntaxValueDeclPremodifierList, syntaxValueDeclKind, syntaxValueDeclPostmodifierList, syntaxBinding) ->
        let bindingInfo = bindTopLevelValueDeclaration cenv env None syntaxAttrs syntaxAccessor syntaxValueDeclPremodifierList.ChildrenOfType syntaxValueDeclKind syntaxValueDeclPostmodifierList.ChildrenOfType syntaxBinding
        let isImpl =
            match syntaxBinding with
            | OlySyntaxBinding.Implementation _ -> true
            | _ -> false
        let bindings = bindings.Add(bindingInfo, isImpl)
        bindings, env

    | OlySyntaxExpression.Sequential(syntaxExpr1, syntaxExpr2) ->
        let bindings, env1 = bindTopLevelExpressionPass2 cenv env supers entities bindings syntaxExpr1
        bindTopLevelExpressionPass2 cenv env1 supers entities bindings syntaxExpr2

    | OlySyntaxExpression.TypeDeclaration(syntaxAttrs, _, _, syntaxTyDefName, syntaxTyPars, _, _, syntaxTyDefBody) ->
        let prevEntityDefIndex = cenv.entityDefIndex
        bindTypeDeclarationPass2 cenv env entities syntaxTyDefName.Identifier syntaxTyPars syntaxTyDefBody
        cenv.entityDefIndex <- prevEntityDefIndex + 1
        bindings, env

    | _ ->
        bindings, env