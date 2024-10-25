[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.Pass1

open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.SymbolOperations

let private stripNewtype (ty: TypeSymbol) =
    OlyAssert.True(ty.IsNewtype)
    let underlyingTy = ty.AsEntity.UnderlyingTypeOfNewtype
    if underlyingTy.IsNewtype then
        stripNewtype underlyingTy
    else
        underlyingTy

/// Pass 1 - Get type definition inherits and implements.
let bindTypeDeclarationPass1 (cenv: cenv) (env: BinderEnvironment) (entities: EntitySymbolBuilder imarray) (syntaxIdent: OlySyntaxToken) (syntaxTyPars: OlySyntaxTypeParameters) (syntaxConstrClauses: OlySyntaxConstraintClause imarray) syntaxTyDefBody =
    let entBuilder = entities.[cenv.entityDefIndex]
    cenv.entityDefIndex <- 0

    bindTypeDeclarationBodyPass1 cenv env syntaxIdent false entBuilder entBuilder.NestedEntityBuilders syntaxTyPars.Values syntaxConstrClauses syntaxTyDefBody |> ignore

    scopeInEntityAndOverride env entBuilder.Entity

/// Pass 1 - Get type inherits and implements.
let bindTypeDeclarationBodyPass1 (cenv: cenv) (env: BinderEnvironment) (syntaxNode: OlySyntaxNode) canOpen (entBuilder: EntitySymbolBuilder) entities (syntaxTyPars: OlySyntaxType imarray) syntaxConstrClauses (syntaxEntDefBody: OlySyntaxTypeDeclarationBody) =
    let env = env.SetResolutionMustSolveTypes()

    let ent = entBuilder.Entity

    let env = env.SetAccessorContext(ent)
    let env = openContentsOfEntityAndOverride cenv.declTable.contents env OpenContent.Entities ent
    let env = addTypeParametersFromEntity cenv env syntaxTyPars ent
    bindConstraintClauseList cenv env syntaxConstrClauses
    let env = env.SetEnclosingTypeParameters(ent.TypeParameters)

    (* CHECK FOR DUPLICATE NESTED ENTITIES *)
    let duplicateEnts = HashSet()
    let syntaxIdents = syntaxEntDefBody.GetNestedTypeDeclarationIdentifiers()
    (entBuilder.NestedEntityBuilders, syntaxIdents)
    ||> ImArray.iter2 (fun nestedEntBuilder syntaxIdent ->
        let nestedEnt = nestedEntBuilder.Entity
        let key = (nestedEnt.Name, nestedEnt.TypeParameters.Length)
        if duplicateEnts.Add(key) |> not then
            cenv.diagnostics.Error(sprintf "'%s' has already been declared." (printEntity env.benv nestedEnt), 10, syntaxIdent)
    )
    (**)

    let getDefaultExtendType (ty: TypeSymbol) =
        if ty.IsEnum then
            // DEFAULT
            match env.benv.implicitExtendsForEnum with
            | Some(extendsTy) ->
                extendsTy
            | _ ->
                TypeSymbol.BaseObject
        elif ty.IsAnyStruct then
            // DEFAULT
            match env.benv.implicitExtendsForStruct with
            | Some(extendsTy) ->
                extendsTy
            | _ ->
                TypeSymbol.BaseObject
        else
            // DEFAULT
            TypeSymbol.BaseObject

    let defaultExtends (extends: TypeSymbol imarray) =
        if ent.IsAlias then
            extends
        elif ent.IsClass && extends.IsEmpty then
            // DEFAULT
            ImArray.createOne TypeSymbol.BaseObject
        elif ent.IsEnum && extends.IsEmpty then
            // DEFAULT
            match env.benv.implicitExtendsForEnum with
            | Some(extendsTy) ->
                ImArray.createOne extendsTy
            | _ ->
                ImArray.createOne TypeSymbol.BaseObject
        elif ent.IsAnyStruct && extends.IsEmpty then
            // DEFAULT
            match env.benv.implicitExtendsForStruct with
            | Some(extendsTy) ->
                ImArray.createOne extendsTy
            | _ ->
                ImArray.createOne TypeSymbol.BaseObject
        else
            extends

    let handleEnumExtends syntaxNode (extends: TypeSymbol imarray) =
        if ent.IsEnum then
            if extends.IsEmpty then
                // Int32 is default for enum declarations.
                entBuilder.SetRuntimeType(cenv.pass, TypeSymbol.Int32, MemberFlags.Private, "value", ValueFlags.Generated)
                extends 
            else
                let runtimeTy = extends[0]
                if not runtimeTy.IsInteger then
                    cenv.diagnostics.Error($"'{printEntity env.benv ent}' can only extend integers.", 10, syntaxNode)
                entBuilder.SetRuntimeType(cenv.pass, runtimeTy, MemberFlags.Private, "value", ValueFlags.Generated)
                ImArray.empty
        else
            extends        

    match syntaxEntDefBody with
    | OlySyntaxTypeDeclarationBody.Body(syntaxExtends, syntaxImplements, _, syntaxExpr) ->
        let extends = bindExtends cenv env syntaxExtends |> handleEnumExtends syntaxExtends
        let implements = bindImplements cenv env syntaxImplements

        if ent.IsAlias then
            if implements.IsEmpty |> not then
                cenv.diagnostics.Error(sprintf "Aliases cannot implement interfaces.", 10, syntaxNode)
        elif not ent.IsEnum && not ent.IsClass && not ent.IsNewtype && not ent.IsTypeExtension then // TODO: This logic is a bit weird, we should look at this closely.
            ent.Extends
            |> ImArray.iter (fun ty ->
                if ty.IsSealed then
                    cenv.diagnostics.Error(sprintf "'%s' is not inheritable." ent.Name, 10, syntaxNode)
            )

        let inheritCount = extends.Length
        if ent.IsTypeExtension || ent.IsAlias then 
            if inheritCount <> 1 && not ent.IsCompilerIntrinsic then
                cenv.diagnostics.Error(sprintf "Aliases, newtypes, and type extensions must inherit from a single type that will be extended.", 10, syntaxNode)
            else
                ent.Extends
                |> Seq.iter (fun ty ->
                    if ty.IsShape then
                        cenv.diagnostics.Error(sprintf "'%s' is not extendable through a type extension." (printType env.benv ty), 10, syntaxNode)
                )
        elif not ent.IsInterface && not ent.IsNewtype then
            if not ent.IsClass && inheritCount > 0 then
                cenv.diagnostics.Error(sprintf "Only classes and interfaces can inherit and be inherited. Consider using 'class %s' or 'interface %s'." ent.Name ent.Name, 10, syntaxNode)
            elif inheritCount > 1 then
                cenv.diagnostics.Error("Multiple inheritance is not enabled.", 10, syntaxNode)
            elif inheritCount = 1 && extends[0].IsSealed then
                cenv.diagnostics.Error($"'{printType env.benv extends[0]}' is sealed and cannot be inherited.", 10, syntaxNode)
        elif ent.IsInterface then
            if implements.IsEmpty |> not then
                cenv.diagnostics.Error(sprintf "Interfaces cannot implement interfaces.", 10, syntaxNode)

        implements
        |> ImArray.iter (fun implementsTy ->
            if not implementsTy.IsInterface then
                cenv.diagnostics.Error(sprintf "Cannot implement non-interfaces.", 10, syntaxNode)
        )

        (* BEGIN NEWTYPE LOGIC *)
        let extends, implements =
            if ent.IsNewtype then
                OlyAssert.True(extends.IsEmpty)
                match syntaxExpr with
                | OlySyntaxExpression.Sequential(OlySyntaxExpression.ValueDeclaration(_, syntaxAccessor, _, _, _, OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxReturnTyAnnot))), _) 
                | OlySyntaxExpression.ValueDeclaration(_, syntaxAccessor, _, _, _, OlySyntaxBinding.Signature(OlySyntaxBindingDeclaration.Value(syntaxIdent, syntaxReturnTyAnnot))) ->
                    let extendTy = bindReturnTypeAnnotation cenv env syntaxReturnTyAnnot
                    let memberAccessFlags = bindAccessorAsMemberFlags true syntaxAccessor
                    entBuilder.SetRuntimeType(cenv.pass, extendTy, memberAccessFlags, syntaxIdent.ValueText, ValueFlags.None)
                | _ ->
                    entBuilder.SetRuntimeType(cenv.pass, TypeSymbolError, MemberFlags.Private, "value", ValueFlags.Invalid)
                    cenv.diagnostics.Error($"Expected field definition signature for newtype '{ent.Name}' as the first expression.", 10, syntaxNode)

            if ent.IsNewtype then
                if not extends.IsEmpty then
                    cenv.diagnostics.Error($"'{ent.Name}' is a newtype which cannot be extended.", 10, syntaxExtends)
                if not implements.IsEmpty then
                    cenv.diagnostics.Error($"'{ent.Name}' is a newtype which cannot implement interfaces directly.", 10, syntaxImplements)
                ImArray.createOne(getDefaultExtendType (stripNewtype ent.AsType)), ImArray.empty
            elif ent.IsTypeExtension then
                // Type extensions that extend alias types are not supported, with the exception of intrinsic alias types.
                // The reason being is that phantom alias types' type parameters are not sound:
                //     alias ExampleAlias<T1> = __oly_int32
                // Lack of 'T1' uses makes 'ExampleAlias' a phantom type.
                // If this were supported for extensions, because this is an alias, '__oly_int32' does not know what 'T1' will ever be.
                // However, intrinsic alias types can never be phantom types.
                if not extends.IsEmpty && extends[0].IsAliasAndNotCompilerIntrinsic then
                    cenv.diagnostics.Error($"'{printType env.benv extends[0]}' is an alias and cannot be used with a type extension.", 10, syntaxExtends.Children[1])
                    ImArray.createOne(TypeSymbolError), implements
                else
                    extends, implements
            else
                let extends = defaultExtends extends
                extends, implements
        (* END NEWTYPE LOGIC *)

        // Check recursive inheritance.
        let extends =
            extends
            |> ImArray.filter (fun x -> 
                match x.TryEntity with
                | ValueSome(x) ->
                    let notValid =
                        x.Formal.AllInherits.Add(x.Formal.AsType)
                        |> TypeSymbol.Distinct
                        |> Seq.exists (fun x ->
                            match x.TryEntity with
                            | ValueSome(x) -> x.FormalId = ent.FormalId
                            | _ -> false
                        )
                    if notValid then
                        cenv.diagnostics.Error($"'{printEntity env.benv ent}' is recursively extending itself.", 10, syntaxExtends)
                        false
                    else
                        true
                | _ ->
                    true
            )

        // Check recursive implements.
        let implements =
            implements
            |> ImArray.filter (fun x -> 
                match x.TryEntity with
                | ValueSome(x) ->
                    let notValid =
                        x.Formal.AllImplements.Add(x.Formal.AsType)
                        |> TypeSymbol.Distinct
                        |> Seq.exists (fun x ->
                            match x.TryEntity with
                            | ValueSome(x) -> x.FormalId = ent.FormalId
                            | _ -> false
                        )
                    if notValid then
                        cenv.diagnostics.Error($"'{printEntity env.benv ent}' is recursively implementing itself.", 10, syntaxImplements)
                        false
                    else
                        true
                | _ ->
                    true
            )

        let extends =
            if ent.IsInterface then
                extends
            elif extends.Length > 1 then
                // Multiple inheritance not allowed here.
                // So just truncate the rest.
                ImArray.createOne extends[0]
            else
                extends

        if not ent.IsCompilerIntrinsic then
            entBuilder.SetExtends(cenv.pass, extends)
            entBuilder.SetImplements(cenv.pass, implements)

        let env, _ = bindTopLevelExpressionPass1 cenv env canOpen entBuilder entities syntaxExpr
        env

    | _ ->
        raise(InternalCompilerException())

/// Pass 1 - Get type definition inherits and implements.
///          Checking constraints on constructors for inherits and implements are ignored.
let bindTopLevelExpressionPass1 (cenv: cenv) (env: BinderEnvironment) (canOpen: bool) (entBuilder: EntitySymbolBuilder) (entities: EntitySymbolBuilder imarray) (syntaxExpr: OlySyntaxExpression) : _ * bool =
    cenv.ct.ThrowIfCancellationRequested()

    match syntaxExpr with
    | OlySyntaxExpression.OpenDeclaration _ 
    | OlySyntaxExpression.OpenStaticDeclaration _
    | OlySyntaxExpression.OpenExtensionDeclaration _ ->
        let enclosing = currentEnclosing env
        if canOpen && enclosing.IsNamespaceOrModule then
            bindOpenDeclaration cenv env canOpen OpenContent.Entities syntaxExpr, canOpen
        else
            cenv.diagnostics.Error("Top-level open declarations can only be declared in modules or namespaces.", 10, syntaxExpr)
            env, canOpen

    | OlySyntaxExpression.Sequential(syntaxExpr1, syntaxExpr2) ->
        let env1, canOpen = bindTopLevelExpressionPass1 cenv env canOpen entBuilder entities syntaxExpr1
        bindTopLevelExpressionPass1 cenv env1 canOpen entBuilder entities syntaxExpr2

    | OlySyntaxExpression.TypeDeclaration(_, _, _, syntaxTyDefName, syntaxTyPars, syntaxConstrClauseList, _, syntaxTyDefBody) ->
        let prevEntityDefIndex = cenv.entityDefIndex
        let env = bindTypeDeclarationPass1 cenv env entities syntaxTyDefName.Identifier syntaxTyPars syntaxConstrClauseList.ChildrenOfType syntaxTyDefBody
        cenv.entityDefIndex <- prevEntityDefIndex + 1
        env, false

    | OlySyntaxExpression.ValueDeclaration _ ->
        env, false

    | OlySyntaxExpression.None _ ->
        env, false

    | _ ->
        if not cenv.syntaxTree.HasErrors then
            cenv.diagnostics.Error("Invalid expression in top-level context.", 10, syntaxExpr)
        env, false