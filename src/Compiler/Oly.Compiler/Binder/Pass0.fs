[<RequireQualifiedAccess>]
module internal rec Oly.Compiler.Internal.Binder.Pass0

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolBuilders
open Oly.Compiler.Internal.Binder.EarlyAttributes

let private bindAccessorAsEntityFlags (cenv: cenv) (env: BinderEnvironment) (syntaxAccessor: OlySyntaxAccessor) =
    match syntaxAccessor with
    | OlySyntaxAccessor.Internal _ -> EntityFlags.Internal
    | OlySyntaxAccessor.Public _ -> EntityFlags.Public
    | OlySyntaxAccessor.Private _ -> EntityFlags.Private
    | OlySyntaxAccessor.Protected _ -> 
        cenv.diagnostics.Error("Types cannot be declared as 'protected'.", 10, syntaxAccessor)
        EntityFlags.None
    | _ ->
        // Default is public.
        EntityFlags.Public

let processAttributesForEntityFlags flags (attrs: AttributeSymbol imarray) =
    (flags, attrs)
    ||> ImArray.fold (fun flags attr ->
        match attr with
        | AttributeSymbol.Open ->
            flags ||| EntityFlags.AutoOpen
        | AttributeSymbol.Null ->
            flags ||| EntityFlags.Nullable
        | AttributeSymbol.Export ->
            flags ||| EntityFlags.Exported
        | AttributeSymbol.Import _ ->
            flags ||| EntityFlags.Imported
        | AttributeSymbol.Intrinsic("importer") ->
            flags ||| EntityFlags.AttributeImporter
        | _ ->
            flags
    )

(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)
(********************************************************************************************************************************************************************************************)

/// Pass 0 - Type definition with type parameters.
let bindTypeDeclaration (cenv: cenv) (env: BinderEnvironment) (syntaxAttrs: OlySyntaxAttributes) (syntaxAccessor: OlySyntaxAccessor) syntaxTyKind (syntaxIdent: OlySyntaxToken) (syntaxTyPars: OlySyntaxTypeParameters) syntaxTyDefBody (entities: EntitySymbolBuilder imarray) docText =
    // We only early bind built-in attributes (import, export, intrinsic) in pass(0).
    let attrs = bindEarlyAttributes cenv env syntaxAttrs

    let flags, kind =
        match syntaxTyKind with
        | OlySyntaxTypeDeclarationKind.Alias _ -> EntityFlags.None, EntityKind.Alias
        | OlySyntaxTypeDeclarationKind.Class _ -> EntityFlags.Final, EntityKind.Class
        | OlySyntaxTypeDeclarationKind.Interface _ -> EntityFlags.Abstract, EntityKind.Interface
        | OlySyntaxTypeDeclarationKind.Module _ -> EntityFlags.Abstract ||| EntityFlags.Final, EntityKind.Module
        | OlySyntaxTypeDeclarationKind.Shape _ -> EntityFlags.Abstract, EntityKind.Shape
        | OlySyntaxTypeDeclarationKind.Struct _ -> EntityFlags.Final, EntityKind.Struct
        | OlySyntaxTypeDeclarationKind.Extension _ -> EntityFlags.Final, EntityKind.TypeExtension
        | OlySyntaxTypeDeclarationKind.AbstractClass _ -> EntityFlags.Abstract, EntityKind.Class
        | OlySyntaxTypeDeclarationKind.AbstractDefaultClass _ -> EntityFlags.None, EntityKind.Class
        | OlySyntaxTypeDeclarationKind.SealedInterface _ -> EntityFlags.Abstract ||| EntityFlags.Final, EntityKind.Interface
        | OlySyntaxTypeDeclarationKind.Enum _ -> EntityFlags.Final, EntityKind.Enum
        | OlySyntaxTypeDeclarationKind.Newtype _ -> EntityFlags.Final, EntityKind.Newtype
        | _ ->
            raise(InternalCompilerException())

    let flags = flags ||| (bindAccessorAsEntityFlags cenv env syntaxAccessor)

    let intrinsicTyOpt =
        tryAddIntrinsicPrimitivesForEntity cenv env kind syntaxTyPars.Count syntaxAttrs attrs

    let flags =
        if intrinsicTyOpt.IsSome then
            flags ||| EntityFlags.Intrinsic
        else
            flags

    let flags = processAttributesForEntityFlags flags attrs

    let enclosing = currentEnclosing env

    let entBuilder = EntitySymbolBuilder.Create(Some cenv.asm, enclosing, syntaxIdent.ValueText, flags, kind, docText)
    let ent = entBuilder.Entity

    OlyAssert.True(ent.TypeParameters.IsEmpty)
    let _, tyPars = bindTypeParameters cenv env false syntaxTyPars.Values

    let tyPars =
        if ent.Enclosing.IsLocalEnclosing then
            tyPars
            |> ImArray.map (fun tyPar ->
                match tyPar.Kind with
                | TypeParameterKind.Function _ ->
                    // Handles generic local type definitions.
                    tyPar.CreateHiddenLink(tyPar.Name, tyPar.Index, tyPar.Arity, tyPar.IsVariadic, TypeParameterKind.Type)
                | _ ->
                    tyPar
            )
        else
            tyPars

    if OlySyntaxFacts.IsOperator syntaxIdent.ValueText && tyPars.Length <> 1 then
        cenv.diagnostics.Error("Postfix type operators must only have a single type parameter.", 10, syntaxIdent)

    // If the entity has free type parameters/variables, then we add them to the definition.
    entBuilder.SetTypeParameters(cenv.pass, tyPars)
    entBuilder.SetAttributes(cenv.pass, attrs)

    let env1, envWithEnclosing =
        match intrinsicTyOpt with
        | Some intrinsicTy when kind = EntityKind.Alias ->
            entBuilder.SetExtends(cenv.pass, ImArray.createOne intrinsicTy)
        | _ ->
            ()
            
        env, env.SetEnclosing(ent.AsEnclosing).SetEnclosingTypeParameters(tyPars)    

    let _, (nestedEnts: EntitySymbolBuilder imarray) = bindTypeDeclarationBody cenv envWithEnclosing syntaxIdent entBuilder ImArray.empty syntaxTyDefBody

    entBuilder.SetEntities(cenv.pass, nestedEnts)

    recordEntityDeclaration cenv ent syntaxIdent

    env1, entities.Add(entBuilder), entBuilder

/// Pass 0 - Gather all type definitions.
let bindTypeDeclarationBody (cenv: cenv) (env: BinderEnvironment) (syntaxNode: OlySyntaxNode) (entBuilder: EntitySymbolBuilder) (entities: EntitySymbolBuilder imarray) (syntaxEntDefBody: OlySyntaxTypeDeclarationBody) =
    let env = env.SetResolutionMustSolveTypes()

    let ent = entBuilder.Entity

    let env = env.SetAccessorContext(ent)  

    match syntaxEntDefBody with
    | OlySyntaxTypeDeclarationBody.Body(syntaxExtends, syntaxImplements, syntaxCaseList, syntaxExpr) ->
        let syntaxCases = syntaxCaseList.ChildrenOfType

        if not syntaxCases.IsEmpty && not ent.IsEnum then
            cenv.diagnostics.Error("Only 'enum' types can define cases.", 10, syntaxNode)
        elif ent.IsEnum then
            if syntaxCases.IsEmpty then
                cenv.diagnostics.Error("Enum declaration must specify one or more cases.", 10, syntaxNode)

        if ent.IsAlias then
            match syntaxExtends with
            | OlySyntaxExtends.Inherits _ ->
                cenv.diagnostics.Error("Alias declarations do not support 'inherits'.", 10, syntaxExtends)
            | _ ->
                ()

            match syntaxImplements with
            | OlySyntaxImplements.Empty() -> ()
            | _ -> 
                cenv.diagnostics.Error("Alias declarations do not support 'implements'.", 10, syntaxImplements)

            match syntaxExpr with
            | OlySyntaxExpression.None() -> ()
            | _ -> 
                cenv.diagnostics.Error("Alias declarations do not support member declarations.", 10, syntaxNode)
          
        bindTopLevelExpression cenv env entities syntaxExpr

    | _ ->
        raise(InternalCompilerUnreachedException())

/// Pass 0 - Gather all type definitions.
let bindTopLevelExpression (cenv: cenv) (env: BinderEnvironment) (entities: EntitySymbolBuilder imarray) (syntaxExpr: OlySyntaxExpression) =
    cenv.ct.ThrowIfCancellationRequested()

    match syntaxExpr with
    | OlySyntaxExpression.Sequential(syntaxExpr1, syntaxExpr2) ->
        let env1, entities = bindTopLevelExpression cenv env entities syntaxExpr1
        bindTopLevelExpression cenv env1 entities syntaxExpr2

    | OlySyntaxExpression.TypeDeclaration(syntaxAttrs, syntaxAccessor, syntaxTyKind, syntaxTyDefName, syntaxTyPars, _, _, syntaxTyDefBody) ->
        let env1, entities, _ = bindTypeDeclaration cenv env syntaxAttrs syntaxAccessor syntaxTyKind syntaxTyDefName.Identifier syntaxTyPars syntaxTyDefBody entities (syntaxExpr.GetLeadingCommentText())
        env1, entities

    | OlySyntaxExpression.OpenDeclaration _ 
    | OlySyntaxExpression.OpenStaticDeclaration _
    | OlySyntaxExpression.OpenExtensionDeclaration _
    | OlySyntaxExpression.ValueDeclaration _
    | OlySyntaxExpression.None ->
        env, entities

    | _ ->
        if not cenv.syntaxTree.HasErrors then
            cenv.diagnostics.Error("Invalid expression in top-level context.", 10, syntaxExpr)
        env, entities