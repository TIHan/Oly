[<RequireQualifiedAccess>]
module internal rec Oly.Compiler.Internal.Binder.Pass2

open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Binder.EarlyAttributes

let addImportAttributeIfNecessary (enclosing: EnclosingSymbol) importName attrs =
    let mutable hasImport = false
    let newAttrs =
        match enclosing with
        | EnclosingSymbol.Entity(ent) ->
            match ent.TryImportedInfo with
            | Some(platform, path, name) ->
                hasImport <- true
                if attributesContainImport attrs then attrs
                else
                    attrs.Add(AttributeSymbol.Import(platform, path.Add(name), importName))
            | _ ->
                attrs
        | _ ->
            attrs

    // An explicit 'import' will always take precedence over an attribute importer.
    if hasImport then
        newAttrs
    else
        let mutable hasImporter = false
        newAttrs
        |> ImArray.iter (fun attr ->
            match attr with
            | AttributeSymbol.Constructor(ctor=ctor) when ctor.Enclosing.AsEntity.IsAttributeImporter ->
                hasImporter <- true
            | _ ->
                ()
        )
        if hasImporter then
            newAttrs.Add(AttributeSymbol.Import(System.String.Empty, ImArray.empty, importName))
        else
            newAttrs

let addExportAttributeIfNecessary (cenv: cenv) (env: BinderEnvironment) syntaxNode attrs =
    if env.isInExport then
        if attributesContainExport attrs then 
            cenv.diagnostics.Error("The 'export' attribute is redundant since the enclosing type is marked 'export'.", 10, syntaxNode)
            attrs
        else
            attrs.Add(AttributeSymbol.Export)
    else
        attrs

[<Sealed>]
type private PropertyInfo(name: string, ty: TypeSymbol, explicitness: ValueExplicitness, memberFlags: MemberFlags) =
    member _.Name = name
    member _.Type = ty
    member _.Explicitness = explicitness
    member _.MemberFlags = memberFlags 

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

let private createInstanceFunctionParameters (cenv: cenv) syntaxNode valueExplicitness (enclosing: EnclosingSymbol) checkMutable name pars =
    let tryAddrTy (ty: TypeSymbol) =
        if checkMutable && valueExplicitness.IsExplicitMutable then
            if not ty.IsStruct_ste then
                cenv.diagnostics.Error(sprintf "The function '%s' marked with 'mutable' must have its enclosing type be a struct." name, 10, syntaxNode)
            if ty.IsStruct_ste && enclosing.IsReadOnly then
                cenv.diagnostics.Error(sprintf "The function '%s' marked with 'mutable' must have its enclosing type be read-only." name, 10, syntaxNode)

        if ty.IsStruct_ste || ty.IsShape_ste then
            let kind = 
                if valueExplicitness.IsExplicitMutable && not enclosing.IsReadOnly then                              
                    ByRefKind.ReadWrite
                else
                    ByRefKind.ReadOnly
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
        let parInstanceTy = applyType ty.Formal ty.TypeArguments
        ImArray.prependOne (createLocalParameterValue(attrs, "", tryAddrTy parInstanceTy, false)) pars
    | EnclosingSymbol.Witness(witnessTy, _) ->
        let attrs = ImArray.empty
        ImArray.prependOne (createLocalParameterValue(attrs, "", tryAddrTy witnessTy, false)) pars
    | _ ->
        pars

let private createAutoPropertyValue (enclosing: EnclosingSymbol) attrs propName propTy (memberFlags: MemberFlags) hasAutoPropSet getterOpt setterOpt =
    let enclosingEnt = enclosing.AsEntity

    let associatedFormalPropId = ref None
    let backingFieldOpt =
        if enclosingEnt.IsClassOrStructOrModuleOrNewtype then
            let backingFieldName = "@" + propName // Same name as the property, but it's private.
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

let private bindParameterIdentifier (cenv: cenv) env isMutable implicitTyOpt onlyBindAsType syntaxAttrs (syntaxIdent: OlySyntaxToken) =
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
        
    let attrs = bindEarlyAttributes cenv env syntaxAttrs
    let par = createLocalParameterValue(attrs, parName, ty, isMutable)
    recordValueDeclaration cenv par syntaxIdent
    env, par

let private bindParameterTypeName cenv env onlyBindAsType syntaxAttrs syntaxName =
    let ty = bindNameAsType cenv env None ResolutionTypeArityZero syntaxName
    let attrs = bindEarlyAttributes cenv env syntaxAttrs
    let par = createLocalParameterValue(attrs, "", ty, false)

    match currentEnclosing env with
    | EnclosingSymbol.Local -> 
        if not onlyBindAsType then
            cenv.diagnostics.Error("Parameter name expected, but got a type.", 10, syntaxName)
    | _ -> 
        ()

    env, par

let private bindParameterType cenv env onlyBindAsType syntaxAttrs syntaxTy =
    let ty = bindType cenv env None ResolutionTypeArityZero syntaxTy
    let attrs = bindEarlyAttributes cenv env syntaxAttrs
    let par = createLocalParameterValue(attrs, "", ty, false)

    match currentEnclosing env with
    | EnclosingSymbol.Local -> 
        if not onlyBindAsType then
            cenv.diagnostics.Error("Parameter name expected, but got a type.", 10, syntaxTy)
    | _ -> 
        ()

    env, par

let private bindParameterPattern cenv env syntaxAttrs syntaxPat (syntaxColonToken: OlySyntaxToken) syntaxTy =
    let ty =
        match syntaxTy with
        // TODO: Clean this up by introducing a SyntaxOptionalTypeAnnotation node.
        | OlySyntaxType.Error(syntaxToken) when syntaxToken.IsDummy && syntaxColonToken.IsDummy ->
            (mkInferenceVariableType None)
        | _ ->
            bindType cenv env None ResolutionTypeArityZero syntaxTy

    match syntaxPat with
    | OlySyntaxPattern.Parenthesis(_, syntaxPatList, _) ->
        let syntaxPats = syntaxPatList.ChildrenOfType

        if syntaxPats.Length = 1 then
            bindParameterPattern cenv env syntaxAttrs syntaxPats[0] syntaxColonToken syntaxTy
        else
            let matchTy =
                if syntaxPats.IsEmpty then
                    mkInferenceVariableType None
                else
                    TypeSymbol.CreateTuple(
                        syntaxPats
                        |> ImArray.map (fun _ -> mkInferenceVariableType None)
                    )
            let attrs = bindEarlyAttributes cenv env syntaxAttrs
            let par = createLocalParameterValue(attrs, "", matchTy, false)

            checkTypes (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxPat ty matchTy

            (env, par)
    | _ ->
        let attrs = bindEarlyAttributes cenv env syntaxAttrs
        let par = createLocalParameterValue(attrs, "", ty, false)
        (env, par)

let bindParameter (cenv: cenv) env implicitTyOpt onlyBindAsType syntaxPar : (BinderEnvironment * ILocalParameterSymbol) =
    match syntaxPar with
    | OlySyntaxParameter.Pattern(syntaxAttrs, _, syntaxPat, syntaxColonToken, syntaxTy) ->
        match syntaxPat with
        | OlySyntaxPattern.Name(OlySyntaxName.Identifier(syntaxIdent)) ->
            match syntaxTy with
            // TODO: Clean this up by introducing a SyntaxOptionalTypeAnnotation node.
            | OlySyntaxType.Error(syntaxToken) when syntaxToken.IsDummy && syntaxColonToken.IsDummy ->
                bindParameterIdentifier cenv env syntaxPar.IsMutable implicitTyOpt onlyBindAsType syntaxAttrs syntaxIdent
            | _ ->
                let ty = bindType cenv env None ResolutionTypeArityZero syntaxTy
                let attrs = bindEarlyAttributes cenv env syntaxAttrs
                let par = createLocalParameterValue(attrs, syntaxIdent.ValueText, ty, syntaxPar.IsMutable)
                recordValueDeclaration cenv par syntaxIdent
                (env.AddParameter(cenv.diagnostics, par, syntaxIdent), par)

        | OlySyntaxPattern.Name(syntaxName) ->
            bindParameterTypeName cenv env onlyBindAsType syntaxAttrs syntaxName

        | _ ->
            if onlyBindAsType then
                match syntaxPat with
                | OlySyntaxPattern.Discard _ ->
                    bindParameterType cenv env onlyBindAsType syntaxAttrs syntaxTy
                | _ ->
                    cenv.diagnostics.Error("Patterns are not allowed for signatures.", 10, syntaxPat)
                    (env, invalidParameter())
            else
                bindParameterPattern cenv env syntaxAttrs syntaxPat syntaxColonToken syntaxTy

    | OlySyntaxParameter.Type(syntaxAttrs, syntaxTy) ->
        bindParameterType cenv env onlyBindAsType syntaxAttrs syntaxTy

    | OlySyntaxParameter.Error _ ->
        (env, invalidParameter())

    | _ ->
        raise(InternalCompilerException())

let bindParameters (cenv: cenv) env onlyBindAsType syntaxPars : ILocalParameterSymbol imarray =
    match syntaxPars with
    | OlySyntaxParameters.Parameters(_, syntaxParList, _) ->
        let _, pars =
            ((env, []), syntaxParList.ChildrenOfType)
            ||> ImArray.fold (fun (env, pars) syntaxPar ->
                let (env, par) = bindParameter cenv env None onlyBindAsType syntaxPar
                env, pars @ [par]
            )
        ImmutableArray.CreateRange(pars)

    | OlySyntaxParameters.Empty _ ->
        ImArray.empty

    | _ ->
        raise(InternalCompilerException())

let bindBindingDeclarationCore (cenv: cenv) env (syntaxAttrs: OlySyntaxAttributes, attrs: AttributeSymbol imarray) onlyBindAsType (memberFlags: MemberFlags) (valueExplicitness: ValueExplicitness) (syntaxBindingDecl: OlySyntaxBindingDeclaration) : Choice<BindingInfoSymbol, LocalBindingInfoSymbol> option =
    bindBindingDeclarationCoreAux cenv env (syntaxAttrs, attrs) onlyBindAsType memberFlags valueExplicitness None syntaxBindingDecl

let private bindBindingDeclarationCoreAux (cenv: cenv) env (syntaxAttrs: OlySyntaxAttributes, attrs: AttributeSymbol imarray) onlyBindAsType (memberFlags: MemberFlags) (valueExplicitness: ValueExplicitness) (propInfoOpt: PropertyInfo option) (syntaxBindingDecl: OlySyntaxBindingDeclaration) : Choice<BindingInfoSymbol, LocalBindingInfoSymbol> option =
    let enclosing = currentEnclosing env

    let isCtor = syntaxBindingDecl.Identifier.IsNew

    if isCtor && not enclosing.CanDeclareConstructor then
        cenv.diagnostics.Error(sprintf "'%s' can only be declared on a class, struct or shape." syntaxBindingDecl.Identifier.ValueText, 10, syntaxBindingDecl.Identifier)
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
        if (enclosing.IsTypeExtensionExtendingStruct || enclosing.IsStruct || enclosing.IsShape) && isInstance then
            if valueExplicitness.IsExplicitMutable then
                true
            else
                false
        else
            false

    let bindFunction env1 funcName (hasRequire, tyPars: TypeParameterSymbol imarray) (returnTy: TypeSymbol) syntaxNode (syntaxPars: OlySyntaxParameters) memberFlags =
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
                        | Some propInfo -> Some propInfo.Type
                        | _ -> None
                    let (_, par) = bindParameter cenv env setterInfoOpt false syntaxPar
                    ImArray.createOne par
            | NormalFunction
            | PatternFunction
            | PatternGuardFunction ->
                bindParameters cenv env1 onlyBindAsType syntaxPars

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
                createInstanceFunctionParameters cenv syntaxNode valueExplicitness enclosing true funcName pars
            else
                if valueExplicitness.IsExplicitMutable then
                    cenv.diagnostics.Error("Static functions cannot be marked with mutable.", 10, syntaxNode)
                pars

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
                WellKnownFunction.None
                None
                isMutable

        // Begin - intrinsic functions
        let wkf =
            attrs
            |> ImArray.tryPicki (fun i attr ->
                match attr with
                | AttributeSymbol.Intrinsic(intrinsicName) ->
                    let error () =
                        cenv.diagnostics.Error("Invalid intrinsic for this construct.", 10, syntaxAttrs.Values[i])
                        None

                    match intrinsicName with
                    | "constant" ->
                        if attrs |> ImArray.exists (function AttributeSymbol.Import _ -> true | _ -> false) then
                            Some(WellKnownFunction.Constant)
                        else
                            error()

                    | "importer" ->
                        None // This is handled elsewhere

                    | _ ->
                        match WellKnownFunction.TryFromName intrinsicName with
                        | Some(wkf) ->
                            if Oly.Compiler.Internal.WellKnownFunctions.Validate(wkf, func) then
                                Some wkf
                            else
                                error()
                        | _ ->
                            error()
                | _ ->
                    None
            )
            |> Option.defaultValue WellKnownFunction.None
        func.SetWellKnownFunction(wkf)
        // End - intrinsic functions

        if func.IsEntryPoint then
            match cenv.entryPoint with
            | None ->
                cenv.entryPoint <- Some func
            | _ ->
                cenv.diagnostics.Error("Duplicate entry points are defined for this compilation.", 10, syntaxNode)

        // We use 'tryIter2' to be resilient against any possible partially complete syntax.
        (syntaxPars.Values, pars)
        ||> ImArray.tryIter2 (fun syntaxPar par ->
            checkParameter (SolverEnvironment.Create(cenv.diagnostics, env.benv, cenv.pass)) syntaxPar func par
        )

        func

    let bindPropertyGetterSetter (cenv: cenv) env syntaxNode (syntaxParsOpt: OlySyntaxParameters option) propName propTy (propMemberFlags: MemberFlags) =
        let memberFlags = 
            if (propMemberFlags &&& MemberFlags.AccessorMask) > (memberFlags &&& MemberFlags.AccessorMask) then
                (memberFlags &&& ~~~MemberFlags.AccessorMask) ||| (propMemberFlags &&& MemberFlags.AccessorMask)
            else
                memberFlags
        match syntaxParsOpt with
        | Some syntaxPars ->
            let returnTy =
                if isExplicitSet then TypeSymbol.Unit
                else propTy
    
            let func = bindFunction env propName (false, ImArray.empty) returnTy syntaxNode syntaxPars memberFlags
    
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
                            createAutoPropertyGetterFunction cenv syntaxNode valueExplicitness enclosing attrs name propTy memberFlags isMutable
                            |> Some
                        else
                            None
                    let setterOpt =
                        let isInstance = memberFlags &&& MemberFlags.Instance = MemberFlags.Instance
                        if valueExplicitness.IsExplicitSet then
                            let parValue = createLocalParameterValue(ImArray.empty, "", propTy, false)
                            let pars = ImArray.createOne parValue
                            let setterPars =
                                if isInstance then
                                    createInstanceFunctionParameters cenv syntaxNode valueExplicitness enclosing false name pars
                                else
                                    pars
                            let isMutable = 
                                if (enclosing.IsStruct || enclosing.IsShape) && isInstance then
                                    true // Setter functions are always considered mutable.
                                else
                                    false
                            createFunctionValueSemantic enclosing attrs name ImArray.empty setterPars TypeSymbol.Unit memberFlags FunctionFlags.None FunctionSemantic.SetterFunction WellKnownFunction.None None isMutable
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
                    let getterOpt = createAutoPropertyGetterFunction cenv syntaxIdent valueExplicitness enclosing ImArray.empty name ty memberFlags false |> Some
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
        bindConstraintClauseList cenv env1 syntaxConstrClauseList.ChildrenOfType
        let returnTy = bindReturnTypeAnnotation cenv env1 syntaxReturnTyAnnot

        let func = bindFunction env1 syntaxIdent.ValueText (syntaxTyPars.HasRequire, tyPars) returnTy syntaxIdent syntaxPars memberFlags
        if func.IsLocal then
            BindingLocalFunction(func)
            |> Choice2Of2
            |> Some
        else
            if func.IsPatternFunction then
                let pat = PatternSymbol(func.Enclosing, ImArray.empty, func.Name, func)
                func.SetAssociatedFormalPattern_Pass2_NonConcurrent(cenv.pass, pat)
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
        | EnclosingSymbol.Entity(ent) when not ent.IsInterface && not ent.IsModule && not ent.IsTypeExtension && not ent.IsAlias ->
            let returnTy = applyType (TypeSymbol.Entity(ent)) ent.TypeArguments
            let pars = bindParameters cenv env onlyBindAsType syntaxPars

            let funcFlags = funcFlags ||| FunctionFlags.Constructor

            let parsWithInstance =
                if memberFlags &&& MemberFlags.Instance = MemberFlags.Instance then
                    let tryAddrTy (ty: TypeSymbol) =
                        if ty.IsStruct_ste then
                            TypeSymbol.CreateByRef(ty, ByRefKind.ReadWrite)
                        else
                            ty
                    match enclosing with
                    | EnclosingSymbol.Entity(ent) ->
                        ImArray.prependOne (createLocalParameterValue(ImArray.empty, "", tryAddrTy (applyType ent.AsType ent.TypeArguments), false)) pars
                    | EnclosingSymbol.Witness(concreteTy, _) ->
                        failwith "Unexpected enclosing witness"
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

        | Some propInfo ->
            bindPropertyGetterSetter cenv env syntaxGetOrSetToken (Some syntaxPars) propInfo.Name propInfo.Type propInfo.MemberFlags

    | OlySyntaxBindingDeclaration.Get(syntaxGetOrSetToken)
    | OlySyntaxBindingDeclaration.Set(syntaxGetOrSetToken) ->
        match propInfoOpt with
        | None ->
            cenv.diagnostics.Error("Invalid declaration. Unexpected 'get' or 'set' binding declaration.", 10, syntaxBindingDecl)
            None
        | Some propInfo ->
            bindPropertyGetterSetter cenv env syntaxGetOrSetToken None propInfo.Name propInfo.Type propInfo.MemberFlags

    | OlySyntaxBindingDeclaration.Error(_) ->
        None

    | _ ->
        raise(InternalCompilerException())

let private bindMemberBindingDeclaration (cenv: cenv) env (syntaxAttrs, attrs: AttributeSymbol imarray) onlyBindAsType (memberFlags: MemberFlags) valueExplicitness (propInfoOpt: PropertyInfo option) (syntaxBindingDecl: OlySyntaxBindingDeclaration) =
    match bindBindingDeclarationCoreAux cenv env (syntaxAttrs, attrs) onlyBindAsType memberFlags valueExplicitness propInfoOpt syntaxBindingDecl with
    | Some(Choice1Of2(bindingInfo)) ->
        recordValueDeclaration cenv bindingInfo.Value syntaxBindingDecl.Identifier
        checkValueExport cenv syntaxBindingDecl.Identifier bindingInfo.Value
        bindingInfo
    | Some(Choice2Of2 _) ->
        OlyAssert.Fail("Bad member binding.")
    | _ ->
        invalidBinding syntaxBindingDecl.Identifier.ValueText

let private createAutoPropertyGetterFunction cenv syntaxNode valueExplicitness enclosing attrs name propTy memberFlags isMutable =
    let isInstance = memberFlags &&& MemberFlags.Instance = MemberFlags.Instance
    let getterPars =
        if isInstance then
            createInstanceFunctionParameters cenv syntaxNode valueExplicitness enclosing false name ImArray.empty
        else
            ImArray.empty
    createFunctionValueSemantic enclosing attrs name ImArray.empty getterPars propTy memberFlags FunctionFlags.None FunctionSemantic.GetterFunction WellKnownFunction.None None isMutable

let private addBindingDeclarationsToEntity (cenv: cenv) env (bindings: (BindingInfoSymbol * bool) imarray) (entBuilder: EntitySymbolBuilder) =
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
            // REVIEW: This is a weird check. We basically do not want to set instance fields on a Newtype, otherwise we will have problems...
            if not (field.IsInstance && entBuilder.Entity.IsNewtype) then
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
    if entBuilder.Entity.IsEnum || entBuilder.Entity.IsNewtype then
        OlyAssert.False(entBuilder.Entity.Fields.IsEmpty)
        if entBuilder.Entity.IsNewtype then
            entBuilder.SetFields(cenv.pass, fields.ToImmutable()) // TODO: This actually appends for newtypes. We should add a new function to 'EntityBuilder' - 'AddFieldsForNewtype' to be specific.
        entBuilder.SetProperties(cenv.pass, props.ToImmutable())
        entBuilder.SetFunctions(cenv.pass, funcs.ToImmutable())
        entBuilder.SetPatterns(cenv.pass, pats.ToImmutable())
    else
        entBuilder.SetFields(cenv.pass, fields.ToImmutable())
        entBuilder.SetProperties(cenv.pass, props.ToImmutable())
        entBuilder.SetFunctions(cenv.pass, funcs.ToImmutable())
        entBuilder.SetPatterns(cenv.pass, pats.ToImmutable())

let bindAnonymousShapeType (cenv: cenv) (env: BinderEnvironment) (tyPars: TypeParameterSymbol imarray) (syntaxExprList: OlySyntaxSeparatorList<OlySyntaxExpression>) =
    match cenv.pass with
    | Pass1 // We do partially allow Pass1 to access Pass2's 'bindAnonymousShapeType'; we have to gather the members. TODO: Is there a better way to do this?
    | Pass2
    | Pass3
    | Pass4 -> ()
    | _ -> failwith "Invalid pass."

    let env =
        if tyPars.IsEmpty then env
        else
            (env, tyPars)
            ||> ImArray.fold (fun env tyPar ->
                scopeInTypeParameter env tyPar
            )
    let entBuilder = EntitySymbolBuilder.CreateAnonymousShape(cenv.asm)
    let env = env.SetEnclosing(entBuilder.Entity.AsEnclosing).SetEnclosingTypeParameters(tyPars)

    entBuilder.SetTypeParameters(Pass0, tyPars)

    let syntaxExprs = syntaxExprList.ChildrenOfType

    let env, entities =
        let cenv = { cenv with pass = Pass0 }
        ((env, ImArray.empty), syntaxExprs)
        ||> ImArray.fold (fun (env, entities) syntaxExpr ->
            Pass0.bindTopLevelExpression cenv env entities syntaxExpr
        )
    
    let env =
        let cenv = { cenv with pass = Pass1 }
        (env, syntaxExprs)
        ||> ImArray.fold (fun env syntaxExpr ->
            let env, _ = Pass1.bindTopLevelExpression cenv env false entBuilder entities syntaxExpr
            env
        )

    let bindingInfos, env =
        let cenv = { cenv with pass = Pass2 }
        ((ImArray.empty, env), syntaxExprs)
        ||> ImArray.fold (fun (bindingInfos, env) syntaxExpr ->
            let bindingInfos, env = bindTopLevelExpression cenv env ImArray.empty entities bindingInfos syntaxExpr
            bindingInfos, env
        )

    addBindingDeclarationsToEntity { cenv with pass = Pass2 } env bindingInfos entBuilder

    entBuilder.Entity.AsType

(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)

let private reportIntrinsicAttributeImplementationError (cenv: cenv) syntaxNode =
    cenv.diagnostics.Error("Value has an 'intrinsic' attribute and must not be given an implementation.", 10, syntaxNode)

let private reportImportAttributeImplementationError (cenv: cenv) syntaxNode =
    cenv.diagnostics.Error("Value has an 'import' attribute and must not be given an implementation.", 10, syntaxNode)

let private bindTopLevelBindingDeclaration cenv env syntaxBinding (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing syntaxBindingDecl =
    let binding = bindMemberBindingDeclaration cenv env (syntaxAttrs, attrs) false memberFlags valueExplicitness propInfoOpt syntaxBindingDecl
    checkEnumForInvalidFieldOrFunction cenv syntaxBindingDecl binding

    checkSyntaxDeclarationBinding cenv enclosing memberFlags valueExplicitness syntaxBinding
    checkSyntaxBindingDeclaration cenv valueExplicitness syntaxBindingDecl

    match tryFindIntrinsicAttribute syntaxAttrs attrs with
    | ValueSome _ ->
        reportIntrinsicAttributeImplementationError cenv syntaxBindingDecl.Identifier
    | _ ->
        match enclosing with
        | EnclosingSymbol.Entity(ent) when ent.TryCompilerIntrinsic.IsSome ->
            reportIntrinsicAttributeImplementationError cenv syntaxBindingDecl.Identifier
        | _ ->
            ()

    match tryFindImportAttribute syntaxAttrs attrs with
    | ValueSome _ ->
        reportImportAttributeImplementationError cenv syntaxBindingDecl.Identifier
    | _ ->
        match enclosing with
        | EnclosingSymbol.Entity(ent) when ent.IsImported ->
            reportImportAttributeImplementationError cenv syntaxBindingDecl.Identifier
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

    let binding = bindMemberBindingDeclaration cenv env (syntaxAttrs, attrs) true memberFlags valueExplicitness propInfoOpt syntaxBindingDecl
    checkEnumForInvalidFieldOrFunction cenv syntaxBindingDecl binding

    if checkBindingSignature cenv attrs enclosing binding memberFlags valueExplicitness propInfoOpt.IsNone syntaxBindingDecl then
        checkSyntaxBindingDeclaration cenv valueExplicitness syntaxBindingDecl

    binding

let private bindTopLevelPropertyBinding cenv env (enclosing: EnclosingSymbol) attrs memberFlags valueExplicitness (syntaxBindingDecl: OlySyntaxBindingDeclaration, syntaxPropBindings: OlySyntaxPropertyBinding imarray) =
    let propName, propTy =
        match syntaxBindingDecl with
        | OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxReturnTyAnnot) ->
            syntaxIdent.ValueText, bindReturnTypeAnnotation cenv env syntaxReturnTyAnnot
        | _ ->
            cenv.diagnostics.Error("Invalid property declaration.", 10, syntaxBindingDecl)
            "", invalidType()

    OlyAssert.False(syntaxPropBindings.IsEmpty)

    if syntaxPropBindings.Length > 2 then
        cenv.diagnostics.Error($"Too many bindings for property '{propName}'.", 10, syntaxBindingDecl)

    let getOrSet1 =
        match syntaxPropBindings[0] with
        | OlySyntaxPropertyBinding.Binding(syntaxAttrs, syntaxAccessor, syntaxValueDeclPremodifiers, syntaxValueDeclKind, syntaxValueDeclPostmodifiers, syntaxBinding) ->
            match syntaxValueDeclKind with
            | OlySyntaxValueDeclarationKind.None _
            | OlySyntaxValueDeclarationKind.Error _ -> ()
            | _ ->
                cenv.diagnostics.Error("Invalid value declaration.", 10, syntaxBinding)
            bindTopLevelValueDeclaration cenv env (Some(PropertyInfo(propName, propTy, valueExplicitness, memberFlags))) syntaxAttrs syntaxAccessor syntaxValueDeclPremodifiers.ChildrenOfType syntaxValueDeclKind syntaxValueDeclPostmodifiers.ChildrenOfType syntaxBinding
        | _ ->
            raise(InternalCompilerUnreachedException())

    let getOrSetOpt2 =
        if syntaxPropBindings.Length = 2 then
            match syntaxPropBindings[1] with
            | OlySyntaxPropertyBinding.Binding(syntaxAttrs, syntaxAccessor, syntaxValueDeclPremodifiers, syntaxValueDeclKind, syntaxValueDeclPostmodifiers, syntaxBinding) ->
                match syntaxValueDeclKind with
                | OlySyntaxValueDeclarationKind.None _
                | OlySyntaxValueDeclarationKind.Error _ -> ()
                | _ ->
                    cenv.diagnostics.Error("Invalid value declaration.", 10, syntaxBinding)
                let bindingInfo = bindTopLevelValueDeclaration cenv env (Some(PropertyInfo(propName, propTy, valueExplicitness, memberFlags))) syntaxAttrs syntaxAccessor syntaxValueDeclPremodifiers.ChildrenOfType syntaxValueDeclKind syntaxValueDeclPostmodifiers.ChildrenOfType syntaxBinding
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

    let isAutoProp =
        syntaxPropBindings
        |> ImArray.forall (function
            | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Get _))
            | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Set _))
            | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Get _, _, _))
            | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Set _, _, _)) ->
                true
            | _ ->
                false
        )

    let isAutoProp =
        // If either getter or setter is imported, then do not create a backing field.
        if getOrSet1.Value.IsImported then
            false
        else
            match getOrSetOpt2 with
            | Some getOrSet2 when getOrSet2.Value.IsImported ->
                true
            | _ ->
                isAutoProp

    let hasAutoPropSet =
        if isAutoProp then
            syntaxPropBindings
            |> ImArray.exists (function
                | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Set _))
                | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Set _, _, _)) ->
                    true
                | _ ->
                    false
            )
        else
            false

    let memberAccessorFlags =
        let principalFlags = memberFlags &&& MemberFlags.AccessorMask
        let flags1 = getOrSet1.Value.MemberFlags &&& MemberFlags.AccessorMask
        let flags2 =
            match getOrSetOpt2 with
            | Some getOrSet2 ->
                getOrSet2.Value.MemberFlags &&& MemberFlags.AccessorMask
            | _ ->
                flags1

        System.Math.Min(     
            System.Math.Max(LanguagePrimitives.EnumToValue(flags1), LanguagePrimitives.EnumToValue(principalFlags)),
            System.Math.Max(LanguagePrimitives.EnumToValue(flags2), LanguagePrimitives.EnumToValue(principalFlags))
        )
        |> LanguagePrimitives.EnumOfValue

    let memberFlags = (memberFlags &&& ~~~MemberFlags.AccessorMask) ||| memberAccessorFlags

    let prop =
        if isAutoProp then
            createAutoPropertyValue enclosing attrs propName propTy memberFlags hasAutoPropSet getterOpt setterOpt
        else
            createPropertyValue enclosing attrs propName propTy memberFlags getterOpt setterOpt None

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

    recordValueDeclaration cenv prop syntaxBindingDecl.Identifier
    let binding = BindingProperty(getterOrSetterBindings, prop)
    if checkBindingSignature cenv attrs enclosing binding memberFlags valueExplicitness false syntaxBindingDecl then
        checkSyntaxBindingDeclaration cenv valueExplicitness syntaxBindingDecl
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
                        ImArray.createOne(createLocalParameterValue(ImArray.empty, "", TypeSymbolError, false))

                let returnTy = TypeSymbol.Bool
                let guardFuncName = 
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

    | OlySyntaxBinding.Property(syntaxBindingDecl, syntaxPropBindingList)
    | OlySyntaxBinding.PropertyWithDefault(syntaxBindingDecl, syntaxPropBindingList, _, _) ->
        match propInfoOpt with
        | Some _ ->
            cenv.diagnostics.Error("Invalid property declaration.", 10, syntaxBindingDecl)
            invalidBinding syntaxBindingDecl.Identifier.ValueText
        | _ ->
            bindTopLevelPropertyBinding cenv env enclosing attrs memberFlags valueExplicitness (syntaxBindingDecl, syntaxPropBindingList.ChildrenOfType)

    | _ ->
        raise(InternalCompilerException())

let private bindTopLevelValueDeclaration 
        cenv 
        env 
        (propInfoOpt: PropertyInfo option)
        syntaxAttrs 
        (syntaxAccessor: OlySyntaxAccessor) 
        syntaxValueDeclPremodifiers 
        syntaxValueDeclKind 
        syntaxValueDeclPostmodifiers 
        syntaxBinding =
    let enclosing = currentEnclosing env

    let memberFlags, valueExplicitness = 
        bindValueModifiersAndKindAsMemberFlags 
            cenv 
            env 
            (propInfoOpt |> Option.map (fun x -> x.Explicitness))
            syntaxValueDeclPremodifiers 
            syntaxValueDeclKind 
            syntaxValueDeclPostmodifiers
    OlyAssert.Equal(MemberFlags.None, memberFlags &&& MemberFlags.AccessorMask)

    let valueExplicitness =
        match syntaxBinding with
        | OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Getter _, _, _)
        | OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Get _, _, _)
        | OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Get _) ->
            { valueExplicitness with IsExplicitGet = true }
        | OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Setter _, _, _)
        | OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Set _, _, _)
        | OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Set _) ->
            { valueExplicitness with IsExplicitSet = true }
        | _ ->
            valueExplicitness

    let defaultAccessorFlags =
        if valueExplicitness.IsExplicitField then 
            MemberFlags.Private 
        else 
            MemberFlags.Public

    let defaultAccessorFlags = 
        propInfoOpt
        |> Option.map (fun x -> x.MemberFlags &&& MemberFlags.AccessorMask)
        |> Option.defaultValue defaultAccessorFlags

    let memberFlags = 
        memberFlags ||| 
            (Pass1.bindAccessorAsMemberFlags defaultAccessorFlags syntaxAccessor)

    let attrs = bindEarlyAttributes cenv env syntaxAttrs
    let attrs = addImportAttributeIfNecessary enclosing syntaxBinding.Declaration.Identifier.ValueText attrs
    let attrs = addExportAttributeIfNecessary cenv env syntaxBinding attrs

    bindTopLevelBinding cenv env (syntaxAttrs, attrs) memberFlags valueExplicitness propInfoOpt enclosing syntaxBinding

let private canAddImplicitDefaultConstructor (ent: EntitySymbol) =
    not ent.IsCompilerIntrinsic && 
    not ent.IsImported && 
    not ent.IsAlias &&
    not ent.IsEnum &&
    not ent.IsTypeExtension &&
    (ent.IsClass || ent.IsStruct || ent.IsNewtype || ent.IsModule)

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
                    if ty.IsStruct_ste then
                        TypeSymbol.CreateByRef(ty, ByRefKind.ReadWrite)
                    else
                        ty
                ImArray.createOne (createLocalParameterValue(ImArray.empty, "", tryAddrTy (applyType enclosingTy enclosingTy.TypeArguments), false))

            let parsWithInstance =
                if ent.IsNewtype then
                    let parTy = ent.UnderlyingTypeOfEnumOrNewtype
                    parsWithInstance.Add(createLocalParameterValue(ImArray.empty, "", parTy, false))
                else
                    parsWithInstance

            let ctor = 
                createFunctionValue 
                    enclosing
                    ImArray.empty 
                    "__oly_ctor"
                    ImArray.empty
                    parsWithInstance
                    (applyType enclosingTy enclosingTy.TypeArguments)
                    (MemberFlags.Instance ||| MemberFlags.Public)
                    FunctionFlags.ImplicitDefaultConstructor
                    WellKnownFunction.None
                    None
                    false
            entBuilder.SetFunctions(cenv.pass, ent.FunctionDefinitions.Add(ctor))

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
                (applyType enclosingTy enclosingTy.TypeArguments) // TODO: Can we just make this void?
                MemberFlags.Public
                FunctionFlags.ImplicitDefaultConstructor
                WellKnownFunction.None
                None
                false
        entBuilder.SetFunctions(cenv.pass, ent.FunctionDefinitions.Add(ctor))

let private bindTypeDeclarationCases (cenv: cenv) (env: BinderEnvironment) (entBuilder: EntitySymbolBuilder) (syntaxCases: OlySyntaxTypeDeclarationCase imarray) =
    let ent = entBuilder.Entity
    if ent.IsEnum then
        let enclosing = ent.AsEnclosing
        let ty = ent.AsType

        let mutable hasExplicitNonEnumCase = false
        let mutable runtimeTyOpt = Some(ent.UnderlyingTypeOfEnumOrNewtype)
        let mutable requireAllCasesAConstant = false
        let mutable autoIncrement = 0
        let fieldConstants =
            syntaxCases
            |> ImArray.choose (fun syntaxCase ->
                match syntaxCase with
                | OlySyntaxTypeDeclarationCase.EnumCase(_, syntaxIdent, _, syntaxExpr) ->
                    requireAllCasesAConstant <- true
                    let constantExpr = bindConstantExpression cenv env runtimeTyOpt syntaxExpr
                    match tryEvaluateFixedIntegralConstantExpression cenv env constantExpr with
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
let bindTypeDeclaration (cenv: cenv) (env: BinderEnvironment) (entities: EntitySymbolBuilder imarray) syntaxAttrs syntaxIdent (syntaxTyPars: OlySyntaxTypeParameters) syntaxTyDefBody =
    let entBuilder = entities.[cenv.entityDefIndex]
    cenv.entityDefIndex <- 0

    let attrs = bindEarlyAttributes cenv env syntaxAttrs

    // IMPORTANT: Be careful when trying to look at a type's attributes when it may not have been fully populated.
    //            In this case, it is OK because we always populate the attributes for the parent first before the children.
    let attrs = Pass2.addExportAttributeIfNecessary cenv env syntaxIdent attrs
    // IMPORTANT: These attributes are temporarily set, they get re-set in Pass3.
    entBuilder.SetAttributes(cenv.pass, attrs)

    let env =
        if entBuilder.Entity.IsExported && not env.isInExport then
            { env with isInExport = true }
        else
            env

    bindTypeDeclarationBody cenv env entBuilder.NestedEntityBuilders entBuilder syntaxTyPars.Values syntaxTyDefBody

    // TODO: We need to do this in the very top-level ModuleDefinition.
    checkEntityExport cenv syntaxIdent entBuilder.Entity

let private bindBodyExpression cenv env supers entities (entBuilder: EntitySymbolBuilder) syntaxBodyExpr =
    let fieldsAndFuncs, _ = bindTopLevelExpression cenv env supers entities ImmutableArray.Empty syntaxBodyExpr

    addBindingDeclarationsToEntity cenv env fieldsAndFuncs entBuilder

/// Pass 2 - Gather all entity definitions.
let bindTypeDeclarationBody (cenv: cenv) (env: BinderEnvironment) entities (entBuilder: EntitySymbolBuilder) (syntaxTyPars: OlySyntaxType imarray) (syntaxEntDefBody: OlySyntaxTypeDeclarationBody) =
    let env = env.SetResolutionMustSolveTypes()

    let ent = entBuilder.Entity

    let env = env.SetAccessorContext(ent)
    let env = env.SetEnclosing(EnclosingSymbol.Entity(ent))
    let env = openContentsOfEntityAndOverride cenv.declTable.contents env OpenContent.Entities ent
    let env = addTypeParametersFromEntity cenv env syntaxTyPars ent
    let env = env.SetEnclosingTypeParameters(ent.TypeParameters)

    match syntaxEntDefBody with
    | OlySyntaxTypeDeclarationBody.Body(syntaxExtends, syntaxImplements, syntaxCaseList, syntaxExpr) ->
        let supers =
            filterTypesAsInterfaces (ent.AllLogicalInheritsAndImplements)
            |> ImmutableArray.CreateRange

        bindTypeDeclarationCases cenv env entBuilder syntaxCaseList.ChildrenOfType
        bindBodyExpression cenv env supers entities entBuilder syntaxExpr

        // Re-bind 'extends' and 'implements' to perform checks.
        let _extends = bindExtends cenv env syntaxExtends // Rebind to check constraints
        let _implements = bindImplements cenv env syntaxImplements // Rebind to check constraints
        ()

    | _ ->
        raise(InternalCompilerException())

    // Implicit default constructor logic
    if canAddImplicitDefaultConstructor entBuilder.Entity then
        addImplicitDefaultConstructor cenv entBuilder

/// Pass 2 - Gather all value definitions.
let bindTopLevelExpression (cenv: cenv) (env: BinderEnvironment) (supers: ImmutableArray<TypeSymbol>) (entities: EntitySymbolBuilder imarray) (bindings: (BindingInfoSymbol * bool) imarray) (syntaxExpr: OlySyntaxExpression) : (BindingInfoSymbol * bool) imarray * BinderEnvironment =
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
        let bindings, env1 = bindTopLevelExpression cenv env supers entities bindings syntaxExpr1
        bindTopLevelExpression cenv env1 supers entities bindings syntaxExpr2

    | OlySyntaxExpression.TypeDeclaration(syntaxAttrs, _, _, syntaxTyDefName, syntaxTyPars, _, _, syntaxTyDefBody) ->
        let prevEntityDefIndex = cenv.entityDefIndex
        bindTypeDeclaration cenv env entities syntaxAttrs syntaxTyDefName.Identifier syntaxTyPars syntaxTyDefBody
        cenv.entityDefIndex <- prevEntityDefIndex + 1
        bindings, env

    | _ ->
        bindings, env