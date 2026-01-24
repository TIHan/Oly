module internal rec Oly.Compiler.Internal.Binder.OpenDeclarations

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.PrettyPrint

let bindOpenDeclaration (cenv: cenv) (env: BinderEnvironment) canOpen openContent syntaxExpr =
    match syntaxExpr with
    | OlySyntaxExpression.OpenDeclaration(_, syntaxName) ->
        let env1 =
            if canOpen then
                let namespaceEnt = bindNameAsNamespace cenv (env.SetIsInOpenDeclaration()) syntaxName

                match env.benv.senv.enclosing with
                | EnclosingSymbol.Entity(enclosingEnt) 
                        // Do not re-open the same namespace.
                        when enclosingEnt.IsNamespace && areNamespacesEqual enclosingEnt namespaceEnt ->
                    env
                | _ ->
                    let env = 
                        if openContent = OpenContent.Entities then
                            { env with benv = env.benv.AddOpenDeclaration(namespaceEnt) }
                        else
                            env
                    openContentsOfEntity cenv.declTable.contents env openContent namespaceEnt
            else
                env
        env1

    | OlySyntaxExpression.OpenStaticDeclaration(_, _, syntaxName) ->
        let env1 =
            if canOpen then
                let ty = bindNameAsType cenv (env.SetIsInOpenDeclaration()) None ResolutionTypeArityZero syntaxName
                if ty.IsError_ste then
                    env
                else
                    match ty.TryEntityNoAlias with
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
                            match env.benv.senv.enclosing with
                            | EnclosingSymbol.Entity(enclosingEnt) when areEntitiesEqual enclosingEnt ent ->
                                OlyAssert.True(enclosingEnt.IsFormal)
                                env
                            | _ ->
                                openContentsOfEntity cenv.declTable.contents env openContent ent
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
                if ty.IsError_ste then
                    env
                else
                    match ty.TryEntityNoAlias with
                    | ValueSome ent ->
                        let env = 
                            if openContent = OpenContent.Entities then
                                { env with benv = env.benv.AddOpenDeclaration(ent) }
                            else
                                env
                        if ent.IsTypeExtension then
                            openContentsOfEntity cenv.declTable.contents env openContent ent
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
