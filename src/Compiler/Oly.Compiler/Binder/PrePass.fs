[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.PrePass

open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Binder

/// PrePass - Open declarations only.
let bindTopLevelExpressionPrePass (cenv: cenv) (env: BinderEnvironment) (canOpen: bool) (syntaxExpr: OlySyntaxExpression) =
    cenv.ct.ThrowIfCancellationRequested()

    if canOpen then
        match syntaxExpr with
        | OlySyntaxExpression.Sequential(syntaxExpr1, syntaxExpr2) ->
            let env1, canOpen = bindTopLevelExpressionPrePass cenv env canOpen syntaxExpr1
            bindTopLevelExpressionPrePass cenv env1 canOpen syntaxExpr2

        | OlySyntaxExpression.OpenDeclaration _ 
        | OlySyntaxExpression.OpenStaticDeclaration _
        | OlySyntaxExpression.OpenExtensionDeclaration _ ->
            let env = tryBindOpenDeclaration cenv env canOpen OpenContent.All syntaxExpr
            env, canOpen

        | _ ->
            env, false
    else
        env, false