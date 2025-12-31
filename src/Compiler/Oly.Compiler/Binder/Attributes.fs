module internal rec Oly.Compiler.Internal.Binder.Attributes

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.SymbolQuery
open Oly.Compiler.Internal.SymbolQuery.Extensions
open Oly.Compiler.Internal.Binder.EarlyAttributes

let private tryAttributeConstant cenv syntaxNode =
    function
    | BoundExpression.Literal(_, literal) ->
        let rec toConstant (literal) =
            match literal with
            | BoundLiteral.Constant(cns) -> Some cns
            | BoundLiteral.NumberInference(lazyLiteral, _) -> 
                match tryEvaluateLazyLiteral cenv.diagnostics lazyLiteral with
                | ValueSome literal -> toConstant literal
                | _ -> None
            | BoundLiteral.DefaultInference _ ->
                cenv.diagnostics.Error("'default' not supported yet for attributes.", 10, syntaxNode)
                None
            | BoundLiteral.NullInference _ ->
                cenv.diagnostics.Error("'null' not supported yet for attributes.", 10, syntaxNode)
                None
            | BoundLiteral.ConstantEnum(constant, _) ->
                constant
                |> Some
            | BoundLiteral.Error ->
                None
        toConstant literal
    | BoundExpression.Call(value = (:? IFunctionSymbol as func)) when func.WellKnownFunction = WellKnownFunction.Constant ->
        ConstantSymbol.External(func) |> Some
    | BoundExpression.Value(value=value) ->
        match value with
        | :? IFieldSymbol as field when field.Constant.IsSome ->
            Some field.Constant.Value
        | _ ->
            cenv.diagnostics.Error("Invalid expression for an attribute.", 10, syntaxNode)
            None
    | _ ->
        cenv.diagnostics.Error("Invalid expression for an attribute.", 10, syntaxNode)
        None

let private bindAttributeFieldOrPropertyExpression cenv env (value: IValueSymbol) (syntaxArgs: (OlySyntaxToken * OlySyntaxExpression) imarray) =
    syntaxArgs
    |> ImArray.choose (fun (syntaxIdent, syntaxExpr) ->
        let ident = syntaxIdent.ValueText
        match value.Enclosing with
        | EnclosingSymbol.Entity(ent) ->
            let ty = ent.AsType
            match ty.FindFields(env.benv, QueryMemberFlags.Instance) |> Seq.tryFind (fun x -> x.Name = ident) with
            | Some(field) ->
                let expr = bindAttributeExpression cenv env field.Type true syntaxExpr
                match tryAttributeConstant cenv syntaxExpr expr with
                | Some(value) ->
                    AttributeNamedArgumentSymbol.Field(field, value) |> Some
                | _ ->
                    None
            | _ ->
                // REVIEW: Only intrinsic properties will work, which is fine. However, from a design standpoint, are there useful use cases for extrinsic properties?
                match ty.FindMostSpecificProperties(env.benv, QueryMemberFlags.Instance, QueryProperty.Intrinsic, ident) |> Seq.tryFind (fun x -> x.Name = ident) with
                | Some(prop) ->
                    let expr = bindAttributeExpression cenv env prop.Type true syntaxExpr
                    match tryAttributeConstant cenv syntaxExpr expr with
                    | Some(value) ->
                        AttributeNamedArgumentSymbol.Property(prop, value) |> Some
                    | _ ->
                        None
                | _ ->
                    cenv.diagnostics.Error($"Unable to find field or property with the name '{ident}'.", 10, syntaxIdent)
                    None
        | _ ->
            raise(InternalCompilerUnreachedException())
    )

let private isValidAttributeArguments (cenv: cenv) (env: BinderEnvironment) argExprs =
    argExprs 
    |> ImArray.forall (function 
        | BoundExpression.Literal _ -> true
        | BoundExpression.Call(value=value) ->
            match value with
            | :? IFunctionSymbol as func when not func.IsInstance ->
                if func.Parameters.Length = 0 && func.WellKnownFunction = WellKnownFunction.Constant then
                    true
                else
                    false
            | _ ->
                false
        | BoundExpression.Value(value=value) ->
            match value with
            | :? IFieldSymbol as field when field.Constant.IsSome ->
                true
            | _ ->
                false
        | _ -> 
            false
    )

let private bindAttributeExpression (cenv: cenv) (env: BinderEnvironment) (expectedTy: TypeSymbol) isArg syntaxExpr =
    match syntaxExpr with
    | OlySyntaxExpression.Initialize(syntaxExpr, _) ->
        match bindAttributeExpressionAux cenv env expectedTy isArg syntaxExpr with
        | BoundExpression.Call _ as expr ->
            if isArg then
                cenv.diagnostics.Error("Not a valid attribute expression.", 10, syntaxExpr)  
                BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))
            else
                expr
        | expr ->
            expr
    | _ ->
        bindAttributeExpressionAux cenv env expectedTy isArg syntaxExpr

let private bindAttributeExpressionAux (cenv: cenv) (env: BinderEnvironment) (expectedTy: TypeSymbol) isArg syntaxExpr =

    let errorAttribute cenv env syntaxExpr =
        cenv.diagnostics.Error("Not a valid attribute expression.", 10, syntaxExpr)  

    match syntaxExpr with
    | OlySyntaxExpression.Call(OlySyntaxExpression.Name(syntaxName), syntaxArgs) ->
        // TODO: Handle generic attributes
        let argExprs = bindAttributeArguments cenv env syntaxArgs
        let resContext =
            if isArg then
                ResolutionContext.ValueOnly
            else
                ResolutionContext.ValueOnlyAttribute
        let resInfo = ResolutionInfo.Create(ValueSome argExprs, ResolutionTypeArity.Any, resContext)
        let item = bindNameAsItem cenv env (Some syntaxExpr) None resInfo syntaxName
        match item with
        | ResolutionItem.Error _ ->
            BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))
        | ResolutionItem.Expression(expr) ->
            let expr = checkExpression cenv env (Some expectedTy) expr
            
            if isArg then expr
            else
                match expr with
                | BoundExpression.Call(value=value;args=argExprs) when value.IsInstanceConstructor && isValidAttributeArguments cenv env argExprs ->
                    expr
                | _ ->
                    if not expr.Type.IsError_t then
                        errorAttribute cenv env syntaxExpr
                    expr
        | _ ->
            errorAttribute cenv env syntaxExpr
            BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))

    | OlySyntaxExpression.Array(_, syntaxExprList, _) ->
        let elementTy =
            match stripTypeEquations expectedTy with
            | TypeSymbol.Array(elementTy, _, _) -> elementTy
            | _ -> mkInferenceVariableType(None)
        let syntaxExprs = syntaxExprList.ChildrenOfType
        let elements = 
            syntaxExprs 
            |> ImArray.map (bindAttributeExpression cenv env elementTy true) 
            |> ImArray.choose (function
                | BoundExpression.Call(value=(:? IFunctionSymbol as func);args=argExprs) when func.WellKnownFunction = WellKnownFunction.Constant && argExprs.IsEmpty ->
                    ConstantSymbol.External(func)
                    |> Some
                | BoundExpression.Literal(syntaxInfo, _) as expr -> 
                    tryAttributeConstant cenv syntaxInfo.Syntax expr
                | _ -> 
                    None
            )

        BoundExpression.Literal(BoundSyntaxInfo.User(syntaxExpr, env.benv), BoundLiteral.Constant(ConstantSymbol.Array(elementTy, elements)))

    | OlySyntaxExpression.Name(syntaxName) ->
        let resInfo =
            if isArg then
                ResolutionInfo.Create(ValueNone, ResolutionTypeArity.Any, ResolutionContext.ValueOnly)
            else
                ResolutionInfo.Create(ValueSome ImArray.empty, ResolutionTypeArity.Any, ResolutionContext.ValueOnlyAttribute)
        let item = bindNameAsItem cenv env (Some syntaxExpr) None resInfo syntaxName
        match item with
        | ResolutionItem.Error _ ->
            BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))
        | ResolutionItem.Expression(expr) ->
            checkExpression cenv env (Some expectedTy) expr
        | _ ->
            errorAttribute cenv env syntaxExpr
            BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))

    | _ ->
        bindEarlyAttributeExpression cenv env syntaxExpr

let private bindAttributeArguments (cenv: cenv) (env: BinderEnvironment) syntaxArgs =
    match syntaxArgs with
    | OlySyntaxArguments.Arguments(_, syntaxArgList, syntaxNamedArgList, _) ->
        if not syntaxNamedArgList.Children.IsEmpty then
            cenv.diagnostics.Error("Named arguments not allowed in attribute.", 10, syntaxArgs)
            
        syntaxArgList.ChildrenOfType
        |> ImArray.map (fun x -> bindAttributeExpression cenv env (mkInferenceVariableType None) true x)

    | OlySyntaxArguments.Empty _ ->
        ImArray.empty

    | _ ->
        raise(InternalCompilerUnreachedException())

let bindAttributes (cenv: cenv) (env: BinderEnvironment) syntaxAttrs =
    match cenv.pass with
    | Pass3
    | Pass4 -> ()
    | _ -> failwith "Invalid pass."

    match syntaxAttrs with
    | OlySyntaxAttributes.Attributes _ ->
        syntaxAttrs.Values
        |> ImArray.choose (fun syntaxAttr ->
            match syntaxAttr with
            | OlySyntaxAttribute.Expression(syntaxExpr) ->
                let expr = bindAttributeExpression cenv env (mkInferenceVariableType None) false syntaxExpr
                match expr with
                | BoundExpression.Call(value=value;args=argExprs) ->
                    if value.IsInstanceConstructor then
                        let attrArgs =
                            argExprs
                            |> ImArray.choose (fun x -> tryAttributeConstant cenv syntaxAttr x)

                        let attrNamedArgs =
                            match syntaxExpr with
                            | OlySyntaxExpression.Initialize(_, syntaxInitializer) ->
                                match syntaxInitializer with
                                | OlySyntaxInitializer.Initializer(_, syntaxFieldPats, _) ->
                                    let syntaxArgs =
                                        syntaxFieldPats.ChildrenOfType
                                        |> ImArray.choose (function
                                            | OlySyntaxFieldPattern.FieldPattern(syntaxName, _, syntaxExpr) ->
                                                match syntaxName with
                                                | OlySyntaxName.Identifier(syntaxIdent) ->
                                                    (syntaxIdent, syntaxExpr)
                                                    |> Some
                                                | _ ->
                                                    cenv.diagnostics.Error("Not a valid field or property.", 10, syntaxName)
                                                    None
                                            | OlySyntaxFieldPattern.Error _ ->
                                                None
                                            | _ ->
                                                unreached()
                                        )
                                    bindAttributeFieldOrPropertyExpression cenv env value syntaxArgs
                                | _ ->
                                    unreached()
                            | _ ->
                                ImArray.empty

                        AttributeSymbol.Constructor(value :?> IFunctionSymbol, attrArgs, attrNamedArgs, AttributeFlags.AllowOnAll)
                        |> Some
                    else
                        cenv.diagnostics.Error($"Invalid attribute '{value.Name}'.", 10, syntaxExpr)
                        None
                | _ ->
                    cenv.diagnostics.Error($"Invalid attribute.", 10, syntaxExpr)
                    None

            | _ ->
                bindEarlyAttribute cenv env syntaxAttr
        )
        |> ImArray.ofSeq

    | OlySyntaxAttributes.Empty _ ->
        ImArray.empty

    | _ ->
        unreached()
