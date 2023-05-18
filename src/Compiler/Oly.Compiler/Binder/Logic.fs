[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Logic

open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.PrettyPrint

let private createInstancePars cenv syntaxNode valueExplicitness (enclosing: EnclosingSymbol) checkMutable name pars =
    let tryAddrTy (ty: TypeSymbol) =
        if checkMutable && valueExplicitness.IsExplicitMutable then
            if not ty.IsAnyStruct && not ty.IsShape then
                cenv.diagnostics.Error(sprintf "The function '%s' marked with 'mutable' must have its enclosing type be a struct." name, 10, syntaxNode)
            if ty.IsAnyStruct && enclosing.IsReadOnly then
                cenv.diagnostics.Error(sprintf "The function '%s' marked with 'mutable' must have its enclosing type be read-only." name, 10, syntaxNode)

        if ty.IsAnyStruct then
            let kind = 
                if valueExplicitness.IsExplicitMutable && not enclosing.IsReadOnly then                              
                    ByRefKind.ReadWrite
                else
                    ByRefKind.Read
            TypeSymbol.CreateByRef(ty, kind)
        else
            ty
    match enclosing with
    | EnclosingSymbol.Entity(ent) ->
        let ty =
            if ent.IsTypeExtension then
                if ent.Extends.Length = 1 then
                    ent.Extends.[0]
                else
                    TypeSymbolError
            else
                ent.AsType
        let attrs = ImArray.empty
        ImArray.prependOne (createLocalParameterValue(attrs, "", tryAddrTy ty, false)) pars
    | EnclosingSymbol.Witness(witnessTy, _) ->
        let attrs = ImArray.empty
        ImArray.prependOne (createLocalParameterValue(attrs, "", tryAddrTy witnessTy, false)) pars
    | _ ->
        pars

let createAutoPropertyValue (enclosing: EnclosingSymbol) attrs propName propTy (memberFlags: MemberFlags) hasAutoPropSet getterOpt setterOpt =
    let enclosingEnt = enclosing.AsEntity

    let associatedFormalPropId = ref None
    let backingFieldOpt =
        if enclosingEnt.IsClass || enclosingEnt.IsStruct || enclosingEnt.IsModule then
            let backingFieldName = propName // Same name as the property, but it's private.
            let backingFieldMemberFlags =
                if memberFlags.HasFlag(MemberFlags.Instance) then
                    MemberFlags.Instance ||| MemberFlags.Private
                else
                    MemberFlags.None ||| MemberFlags.Private
            let backingFieldValueFlags =
                if hasAutoPropSet then
                    ValueFlags.Mutable
                else
                    ValueFlags.None
            createFieldValue
                enclosing
                ImArray.empty
                backingFieldName
                propTy
                backingFieldMemberFlags
                backingFieldValueFlags
                associatedFormalPropId
            |> Some
        else
            None

    let prop =
        createPropertyValue
            enclosing
            attrs
            propName
            propTy
            (memberFlags &&& ~~~(MemberFlags.Abstract ||| MemberFlags.Virtual ||| MemberFlags.Sealed))
            getterOpt
            setterOpt
            backingFieldOpt

    associatedFormalPropId.contents <- Some prop.Id
    prop

let createAutoPropertyGetterFunction cenv syntaxNode valueExplicitness enclosing name propTy memberFlags isMutable =
    let isInstance = memberFlags &&& MemberFlags.Instance = MemberFlags.Instance
    let getterName = "get_" + name
    let getterPars =
        if isInstance then
            createInstancePars cenv syntaxNode valueExplicitness enclosing false getterName ImArray.empty
        else
            ImArray.empty
    createFunctionValueSemantic enclosing ImArray.empty getterName ImArray.empty getterPars propTy memberFlags FunctionFlags.None FunctionSemantic.GetterFunction WellKnownFunction.None None isMutable

/// A function is an entry point if the following is true:
///     1. Function's name is "main" with exact case.
///     2. Function is static.
///     3. Function's enclosing type is a module.
///     4. The compilation unit is marked as an executable.
///     5. The compilation unit allows anonymous module definitions.
let private isValidEntryPoint (cenv: cenv) env (enclosing: EnclosingSymbol) (funcName: string) (isInstance: bool) =
    if funcName = EntryPointName && env.isExecutable && not isInstance then
        match enclosing with
        | EnclosingSymbol.Entity(ent) when ent.IsModule && cenv.syntaxTree.ParsingOptions.AnonymousModuleDefinitionAllowed ->
            true
        | _ ->
            false
    else
        false

let private bindBindingDeclarationAux (cenv: cenv) env (attrs: AttributeSymbol imarray) onlyBindAsType (memberFlags: MemberFlags) (valueExplicitness: ValueExplicitness) (propInfoOpt: (string * TypeSymbol * ValueExplicitness) option) (syntaxBindingDecl: OlySyntaxBindingDeclaration) : Choice<BindingInfoSymbol, LocalBindingInfoSymbol> option =
    let enclosing = currentEnclosing env

    let isCtor = syntaxBindingDecl.Identifier.IsNew

    if isCtor && not enclosing.CanDeclareConstructor then
        cenv.diagnostics.Error(sprintf "'%s' can only be declared on a class or struct." syntaxBindingDecl.Identifier.ValueText, 10, syntaxBindingDecl.Identifier)
        None
    elif enclosing.IsNamespace then
        cenv.diagnostics.Error("Values cannot be declared on a namespace.", 10, syntaxBindingDecl.Identifier)
        None
    else

    let isExplicitGet = syntaxBindingDecl.IsExplicitGet
    let isExplicitSet = syntaxBindingDecl.IsExplicitSet

    let funcFlags =
        match enclosing with
        | EnclosingSymbol.Local ->
            if valueExplicitness.IsExplicitStatic then FunctionFlags.StaticLocal
            else FunctionFlags.None
        | _ ->
            FunctionFlags.None

    let isInstance = memberFlags &&& MemberFlags.Instance = MemberFlags.Instance
    let isMutable =
        if (enclosing.IsAnyStruct || enclosing.IsShape) && isInstance then
            if valueExplicitness.IsExplicitMutable then
                true
            else
                false
        else
            false

    let bindFunction env1 funcName (hasRequire, tyPars: TypeParameterSymbol imarray) (returnTy: TypeSymbol) syntaxNode (syntaxPars: OlySyntaxParameters) =
        let funcSemantic =
            if isExplicitSet then SetterFunction
            elif isExplicitGet then GetterFunction
            elif valueExplicitness.IsExplicitPattern then PatternFunction
            else NormalFunction

        let pars = 
            match funcSemantic with
            | GetterFunction ->
                let syntaxPars = syntaxPars.Values
                if not syntaxPars.IsEmpty then
                    cenv.diagnostics.Error("'get' functions must have no parameters.", 10, syntaxNode)
                ImArray.empty
            | SetterFunction ->
                let syntaxPars = syntaxPars.Values
                if syntaxPars.IsEmpty || syntaxPars.Length > 1 then
                    cenv.diagnostics.Error("'set' functions must have one parameter.", 10, syntaxNode)
                    ImArray.empty
                else
                    let syntaxPar = syntaxPars[0]
                    let setterInfoOpt =
                        match propInfoOpt with
                        | Some(_, propTy, _) -> Some propTy
                        | _ -> None
                    ImArray.createOne (bindParameter cenv env setterInfoOpt false syntaxPar |> snd)
            | NormalFunction
            | PatternFunction
            | PatternGuardFunction ->
                bindParameters cenv env1 onlyBindAsType syntaxPars
                |> snd

        let funcFlags =
            if isValidEntryPoint cenv env enclosing funcName isInstance then
                funcFlags ||| FunctionFlags.EntryPoint
            else
                funcFlags

        let funcFlags =
            if tyPars.IsEmpty then
                funcFlags
            else
                // Example:
                //     test<T>: T = ...
                match syntaxPars with
                | OlySyntaxParameters.Empty ->
                    funcFlags ||| FunctionFlags.ParameterLess
                | _ ->
                    funcFlags

        let funcFlags =
            if hasRequire then
                OlyAssert.False(tyPars.IsEmpty)
                funcFlags ||| FunctionFlags.RequiresExplicitTypeArguments
            else
                funcFlags

        let parsWithInstance =
            if isInstance then
                createInstancePars cenv syntaxNode valueExplicitness enclosing true funcName pars
            else
                if valueExplicitness.IsExplicitMutable then
                    cenv.diagnostics.Error("Static functions cannot be marked with mutable.", 10, syntaxNode)
                pars

        let funcIntrin =
            attrs
            |> WellKnownFunction.TryFromAttributes
            |> Option.defaultValue WellKnownFunction.None

        match funcSemantic with
        | PatternFunction ->
            if pars.Length <> 1 then
                cenv.diagnostics.Error("Patterns can only have a single immutable parameter.", 10, syntaxNode)
            
            let hasMutablePar =
                pars |> ImArray.exists (fun x -> x.IsMutable)
            if hasMutablePar then
                cenv.diagnostics.Error("Patterns can only have a single immutable parameter.", 10, syntaxNode)
        | _ ->
            ()

        let func = 
            createFunctionValueSemantic
                enclosing
                attrs
                funcName
                tyPars
                parsWithInstance
                returnTy
                memberFlags
                funcFlags
                funcSemantic
                funcIntrin
                None
                isMutable

        if func.IsEntryPoint then
            match cenv.entryPoint with
            | None ->
                cenv.entryPoint <- Some func
            | _ ->
                cenv.diagnostics.Error("Duplicate entry points are defined for this compilation.", 10, syntaxNode)

        func

    let bindPropertyGetterSetter (cenv: cenv) env syntaxNode (syntaxParsOpt: OlySyntaxParameters option) propName propTy =
        match syntaxParsOpt with
        | Some syntaxPars ->
            let funcName =
                if isExplicitSet then "set_" + propName
                else "get_" + propName
            let returnTy =
                if isExplicitSet then TypeSymbol.Unit
                else propTy
    
            let func = bindFunction env funcName (false, ImArray.empty) returnTy syntaxNode syntaxPars
    
            let logicalPars = func.LogicalParameters
            if isExplicitSet && (logicalPars.IsEmpty || not (areTypesEqual logicalPars.[0].Type propTy)) then
                cenv.diagnostics.Error($"Expected first parameter of 'set' binding to be of type '{printType env.benv propTy}'.", 10, syntaxNode)
    
            if func.IsLocal then
                BindingLocalFunction(func)
                |> Choice2Of2
                |> Some
            else
                BindingFunction(func)
                |> Choice1Of2
                |> Some
        | _ ->
            match enclosing with
            | EnclosingSymbol.Entity(enclosingEnt) ->
                let name = propName 
                // If we have a 'get or 'set' - this is an auto property for a class, struct, or module.
                // Or it can be for the signature of standard properties with basic getters and setters on shapes and interfaces.
                if valueExplicitness.IsExplicitGet || valueExplicitness.IsExplicitSet then
                    let getterOpt = 
                        if valueExplicitness.IsExplicitGet then
                            createAutoPropertyGetterFunction cenv syntaxNode valueExplicitness enclosing name propTy memberFlags isMutable
                            |> Some
                        else
                            None
                    let setterOpt =
                        let isInstance = memberFlags &&& MemberFlags.Instance = MemberFlags.Instance
                        if valueExplicitness.IsExplicitSet then
                            let setterName = "set_" + name
                            let parValue = createLocalParameterValue(ImArray.empty, "", propTy, false)
                            let pars = ImArray.createOne parValue
                            let setterPars =
                                if isInstance then
                                    createInstancePars cenv syntaxNode valueExplicitness enclosing false setterName pars
                                else
                                    pars
                            let isMutable = 
                                if (enclosing.IsAnyStruct || enclosing.IsShape) && isInstance then
                                    true // Setter functions are always considered mutable.
                                else
                                    false
                            createFunctionValueSemantic enclosing ImArray.empty setterName ImArray.empty setterPars TypeSymbol.Unit memberFlags FunctionFlags.None FunctionSemantic.SetterFunction WellKnownFunction.None None isMutable
                            |> Some
                        else
                            None

                    if enclosingEnt.IsTypeExtension then
                        cenv.diagnostics.Error("Type extensions cannot have auto properties. Use explicitly implemented properties instead.", 10, syntaxNode)
                    elif enclosingEnt.IsEnum then
                        cenv.diagnostics.Error("Enums cannot have properties.", 10, syntaxNode)

                    if getterOpt.IsSome then
                        OlyAssert.False(setterOpt.IsSome)
                        BindingFunction(getterOpt.Value) 
                        |> Choice1Of2
                        |> Some
                    elif setterOpt.IsSome then
                        BindingFunction(setterOpt.Value) 
                        |> Choice1Of2
                        |> Some
                    else
                        None
                else
                    None
            | _ ->
                None

    let bindValue env (syntaxIdent: OlySyntaxToken) syntaxReturnTyAnnot =
        let ty = bindReturnTypeAnnotation cenv env syntaxReturnTyAnnot
        let value = 
            match enclosing with
            | EnclosingSymbol.Entity(enclosingEnt) ->
                let name = syntaxIdent.ValueText      
                
                if valueExplicitness.IsExplicitField then
                    let valueFlags =
                        if valueExplicitness.IsExplicitMutable then
                            ValueFlags.Mutable
                        else
                            ValueFlags.None

                    if not enclosingEnt.IsClass && not enclosingEnt.IsStruct && not enclosingEnt.IsModule && not enclosingEnt.IsNewtype then
                        cenv.diagnostics.Error("Fields are not allowed here.", 10, syntaxIdent)
                        
                    createFieldValue enclosing attrs name ty memberFlags valueFlags (ref None) :> IValueSymbol
                else
                    cenv.diagnostics.Error("Short-hand property getters not implemented (yet).", 10, syntaxIdent)
                    if valueExplicitness.IsExplicitMutable then
                        cenv.diagnostics.Error("'mutable' is not allowed short-hand property declarations.", 10, syntaxIdent)
                    let getterOpt = createAutoPropertyGetterFunction cenv syntaxIdent valueExplicitness enclosing name ty memberFlags false |> Some
                    createAutoPropertyValue enclosing attrs name ty memberFlags false getterOpt None
            | _ ->
                if valueExplicitness.IsExplicitMutable then
                    createMutableLocalValue syntaxIdent.ValueText ty
                else
                    createLocalValue syntaxIdent.ValueText ty

        if value.IsProperty then
            BindingProperty(ImArray.empty, value :?> PropertySymbol)
            |> Choice1Of2
            |> Some
        else
            if value.IsField then
                BindingField(value :?> FieldSymbol)
                |> Choice1Of2
                |> Some
            else
                BindingLocal(value :?> ILocalSymbol)
                |> Choice2Of2
                |> Some

    match propInfoOpt with
    | Some _ when not isExplicitGet && not isExplicitSet ->
        cenv.diagnostics.Error("Invalid declaration. Expected 'get' or 'set' declaration.", 10, syntaxBindingDecl)
        None
    | _ ->
        
    match syntaxBindingDecl with
    | OlySyntaxBindingDeclaration.Function(syntaxFuncName, syntaxTyPars, syntaxPars, syntaxReturnTyAnnot, syntaxConstrClauseList) ->
        let syntaxIdent = syntaxFuncName.Identifier

        let env1, tyPars = bindTypeParameters cenv env true syntaxTyPars.Values
        bindConstraintClauseList cenv env1 syntaxConstrClauseList
        let returnTy = bindReturnTypeAnnotation cenv env1 syntaxReturnTyAnnot

        let func = bindFunction env1 syntaxIdent.ValueText (syntaxTyPars.HasRequire, tyPars) returnTy syntaxIdent syntaxPars
        if func.IsLocal then
            BindingLocalFunction(func)
            |> Choice2Of2
            |> Some
        else
            if func.IsPatternFunction then
                let pat = PatternSymbol(func.Enclosing, ImArray.empty, func.Name, func)
                func.SetAssociatedFormalPattern_Pass2_NonConcurrent(pat)
                BindingPattern(pat, func)
                |> Choice1Of2
                |> Some
            else
                BindingFunction(func)
                |> Choice1Of2
                |> Some

    | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxTyAnnot) ->   
        bindValue env syntaxIdent syntaxTyAnnot

    | OlySyntaxBindingDeclaration.New(syntaxNewToken, syntaxPars) ->

        let memberFlags =
            memberFlags &&& ~~~MemberFlags.Virtual
        let memberFlags =
            memberFlags &&& ~~~MemberFlags.NewSlot
        let memberFlags =
            memberFlags &&& ~~~MemberFlags.Abstract
        let memberFlags =
            memberFlags &&& ~~~MemberFlags.Sealed
        
        if valueExplicitness.IsExplicitMutable then
            cenv.diagnostics.Error("Constructors cannot be marked as mutable.", 10, syntaxNewToken)

        match enclosing with
        | EnclosingSymbol.Entity(ent) when not ent.IsInterface && not ent.IsModule && not ent.IsTypeExtension && not ent.IsShape && not ent.IsAlias ->
            let returnTy = TypeSymbol.Entity(ent)
            let env1, pars = bindParameters cenv env onlyBindAsType syntaxPars

            let funcFlags = funcFlags ||| FunctionFlags.Constructor

            let parsWithInstance =
                if memberFlags &&& MemberFlags.Instance = MemberFlags.Instance then
                    let tryAddrTy (ty: TypeSymbol) =
                        if ty.IsAnyStruct then
                            TypeSymbol.CreateByRef(ty, ByRefKind.ReadWrite)
                        else
                            ty
                    match enclosing with
                    | EnclosingSymbol.Entity(ent) ->
                        ImArray.prependOne (createLocalParameterValue(ImArray.empty, "", tryAddrTy ent.AsType, false)) pars
                    | EnclosingSymbol.Witness(concreteTy, _) ->
                        ImArray.prependOne (createLocalParameterValue(ImArray.empty, "", tryAddrTy concreteTy, false)) pars
                    | _ ->
                        pars
                else
                    pars

            let name =
                if memberFlags.HasFlag(MemberFlags.Instance) then
                    "__oly_ctor"
                else
                    "__oly_static_ctor"                  

            let func = 
                createFunctionValue
                    enclosing
                    attrs
                    name
                    ImArray.empty
                    parsWithInstance
                    returnTy
                    memberFlags
                    funcFlags
                    WellKnownFunction.None
                    None
                    false

            if func.IsLocal then
                BindingLocalFunction(func)
                |> Choice2Of2
                |> Some
            else
                BindingFunction(func)
                |> Choice1Of2
                |> Some
        | _ ->
            cenv.diagnostics.Error("Constructors can only be declared within a type definition.", 10, syntaxNewToken)
            None

    | OlySyntaxBindingDeclaration.Getter(syntaxGetOrSetToken, syntaxPars)
    | OlySyntaxBindingDeclaration.Setter(syntaxGetOrSetToken, syntaxPars) ->
        match propInfoOpt with
        | None ->
            cenv.diagnostics.Error("Invalid declaration. Unexpected 'get' or 'set' binding declaration.", 10, syntaxBindingDecl)
            None

        | Some(propName, propTy, _) ->
            bindPropertyGetterSetter cenv env syntaxGetOrSetToken (Some syntaxPars) propName propTy

    | OlySyntaxBindingDeclaration.Get(syntaxGetOrSetToken)
    | OlySyntaxBindingDeclaration.Set(syntaxGetOrSetToken) ->
        match propInfoOpt with
        | None ->
            cenv.diagnostics.Error("Invalid declaration. Unexpected 'get' or 'set' binding declaration.", 10, syntaxBindingDecl)
            None

        | Some(propName, propTy, _) ->
            bindPropertyGetterSetter cenv env syntaxGetOrSetToken None propName propTy

    | OlySyntaxBindingDeclaration.Error(_) ->
        None

    | _ ->
        raise(InternalCompilerException())

let bindMemberBindingDeclaration (cenv: cenv) env (attrs: AttributeSymbol imarray) onlyBindAsType (memberFlags: MemberFlags) valueExplicitness (propInfoOpt: (string * TypeSymbol * ValueExplicitness) option) (syntaxBindingDecl: OlySyntaxBindingDeclaration) =
    match bindBindingDeclarationAux cenv env attrs onlyBindAsType memberFlags valueExplicitness propInfoOpt syntaxBindingDecl with
    | Some(Choice1Of2(bindingInfo)) ->
        recordValueDeclaration cenv bindingInfo.Value syntaxBindingDecl.Identifier
        checkValueExport cenv syntaxBindingDecl.Identifier bindingInfo.Value
        bindingInfo
    | Some(Choice2Of2 _) ->
        OlyAssert.Fail("Bad member binding.")
    | _ ->
        invalidBinding syntaxBindingDecl.Identifier.ValueText

let bindLetBindingDeclaration (cenv: cenv) env (attrs: AttributeSymbol imarray) onlyBindAsType (memberFlags: MemberFlags) valueExplicitness (syntaxBindingDecl: OlySyntaxBindingDeclaration) =  
    match bindBindingDeclarationAux cenv env attrs onlyBindAsType memberFlags valueExplicitness None syntaxBindingDecl with
    | Some(Choice2Of2(bindingInfo)) ->
        checkValueExport cenv syntaxBindingDecl.Identifier bindingInfo.Value
        bindingInfo
    | Some(Choice1Of2 _) ->
        OlyAssert.Fail("Bad let binding.")
    | _ ->
        invalidLocalBinding syntaxBindingDecl.Identifier.ValueText

let addImportAttributeIfNecessary (enclosing: EnclosingSymbol) importName attrs =
    match enclosing with
    | EnclosingSymbol.Entity(ent) ->
        match ent.TryImportedInfo with
        | Some(platform, path, name) ->
            if attributesContainImport attrs then attrs
            else
                attrs.Add(AttributeSymbol.Import(platform, path.Add(name), importName))
        | _ ->
            attrs
    | _ ->
        attrs    

let bindOpenDeclaration (cenv: cenv) (env: BinderEnvironment) canOpen openContent syntaxExpr =
    match syntaxExpr with
    | OlySyntaxExpression.OpenDeclaration(_, syntaxName) ->
        let env1 =
            if canOpen then
                let namespaceEnt = bindNameAsNamespace cenv (env.SetIsInOpenDeclaration()) syntaxName
                let env = 
                    if openContent = OpenContent.Entities then
                        { env with benv = env.benv.AddOpenDeclaration(namespaceEnt) }
                    else
                        env
                openContentsOfEntity env openContent namespaceEnt
            else
                env
        env1

    | OlySyntaxExpression.OpenStaticDeclaration(_, _, syntaxName) ->
        let env1 =
            if canOpen then
                let ty = bindNameAsType cenv (env.SetIsInOpenDeclaration()) None ResolutionTypeArityZero syntaxName
                if ty.IsError_t then
                    env
                else
                    match ty.TryEntity with
                    | ValueSome ent ->
                        let env = 
                            if openContent = OpenContent.Entities then
                                { env with benv = env.benv.AddOpenDeclaration(ent) }
                            else
                                env
                        if ent.IsTypeExtension then
                            cenv.diagnostics.Error($"'{printEntity env.benv ent}' is an extension and must be opened using 'open extension'.", 10, syntaxName)
                            env
                        elif ent.IsShape then
                            cenv.diagnostics.Error($"'{printEntity env.benv ent}' is a shape and cannot be opened.", 10, syntaxName)
                            env
                        else
                            openContentsOfEntity env openContent ent
                    | _ ->
                        cenv.diagnostics.Error($"Not a named type.", 10, syntaxName)
                        env
            else
                env
        env1

    | OlySyntaxExpression.OpenExtensionDeclaration(_, _, syntaxName) ->
        let env1 =
            if canOpen then
                let ty = bindNameAsType cenv (env.SetIsInOpenDeclaration()) None ResolutionTypeArityZero syntaxName
                if ty.IsError_t then
                    env
                else
                    match ty.TryEntity with
                    | ValueSome ent ->
                        let env = 
                            if openContent = OpenContent.Entities then
                                { env with benv = env.benv.AddOpenDeclaration(ent) }
                            else
                                env
                        if ent.IsTypeExtension then
                            openContentsOfEntity env openContent ent
                        else
                            cenv.diagnostics.Error($"'{printType env.benv ty}' is not an extension.", 10, syntaxName)
                            env
                    | _ ->
                        cenv.diagnostics.Error($"Not a named type.", 10, syntaxName)
                        env
            else
                env
        env1

    | _ ->
        env

let tryBindOpenDeclaration (cenv: cenv) (env: BinderEnvironment) canOpen openContent syntaxExpr =
    let diagLogger = OlyDiagnosticLogger.Create()
    let cenv = { cenv with diagnostics = diagLogger }
    bindOpenDeclaration cenv env canOpen openContent syntaxExpr

let bindParameter (cenv: cenv) env implicitTyOpt onlyBindAsType syntaxPar : BinderEnvironment * ILocalParameterSymbol =
    match syntaxPar with
    | OlySyntaxParameter.Identifier(syntaxAttrs, _, syntaxIdent) ->
        let parName, ty =
            match currentEnclosing env with
            | EnclosingSymbol.Local ->
                if onlyBindAsType then
                    cenv.diagnostics.Error("Parameter name expected, but got a type.", 10, syntaxIdent)
                    "", bindIdentifierAsType cenv env syntaxIdent ResolutionTypeArityZero syntaxIdent.ValueText
                else
                    syntaxIdent.ValueText, mkInferenceVariableTypeOfParameter()
            | _ ->
                if onlyBindAsType then
                    "", bindIdentifierAsType cenv env syntaxIdent ResolutionTypeArityZero syntaxIdent.ValueText
                else
                    match implicitTyOpt with
                    | Some(ty) ->
                        syntaxIdent.ValueText, ty
                    | _ ->
                        cenv.diagnostics.Error("Parameter must have an explicit type annotation as it is part of a top-level function.", 10, syntaxIdent)
                        syntaxIdent.ValueText, TypeSymbolError
        
        let attrs = bindAttributes cenv env false syntaxAttrs
        let par = createLocalParameterValue(attrs, parName, ty, syntaxPar.IsMutable)
        recordValueDeclaration cenv par syntaxIdent
        env, par

    | OlySyntaxParameter.IdentifierWithTypeAnnotation(syntaxAttrs, _, syntaxIdent, _, syntaxTy) ->
        let ty = bindType cenv env None ResolutionTypeArityZero syntaxTy
        let attrs = bindAttributes cenv env false syntaxAttrs
        let par = createLocalParameterValue(attrs, syntaxIdent.ValueText, ty, syntaxPar.IsMutable)
        recordValueDeclaration cenv par syntaxIdent
        env.AddParameter(cenv.diagnostics, par, syntaxIdent), par

    | OlySyntaxParameter.Type(syntaxAttrs, syntaxType) ->
        let ty = bindType cenv env None ResolutionTypeArityZero syntaxType
        let attrs = bindAttributes cenv env false syntaxAttrs
        let par = createLocalParameterValue(attrs, "", ty, false)

        match currentEnclosing env with
        | EnclosingSymbol.Local -> 
            if not onlyBindAsType then
                cenv.diagnostics.Error("Parameter name expected, but got a type.", 10, syntaxType)
        | _ -> 
            ()

        env, par

    | OlySyntaxParameter.Error _ ->
        env, invalidParameter()

    | _ ->
        raise(InternalCompilerException())

let bindParameterList (cenv: cenv) env onlyBindAsType pars (syntaxParList: OlySyntaxSeparatorList<OlySyntaxParameter>) : BinderEnvironment * _ list =
    ((env, pars), syntaxParList.ChildrenOfType)
    ||> ImArray.fold (fun (env, pars) syntaxPar ->
        let env, par = bindParameter cenv env None onlyBindAsType syntaxPar
        env, pars @ [par]
    )

let bindParameters (cenv: cenv) env onlyBindAsType syntaxPars : BinderEnvironment * ILocalParameterSymbol imarray =
    match syntaxPars with
    | OlySyntaxParameters.Parameters(_, syntaxParList, _) ->
        let env1, pars = bindParameterList cenv env onlyBindAsType [] syntaxParList
        env1, ImmutableArray.CreateRange(pars)
    | OlySyntaxParameters.Empty _ ->
        env, ImArray.empty

    | _ ->
        raise(InternalCompilerException())
