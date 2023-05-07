[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.AttributeBinder

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.Binder

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
            | BoundLiteral.Error _ ->
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

let private bindAttributeNamedArguments cenv env (value: IValueSymbol) syntaxArgs =
    // TODO: We should do this logic else-where for named arguments. - probably in bindNameAsItem?
    match syntaxArgs with
    | OlySyntaxArguments.Arguments(_, _, syntaxNamedArgList, _) ->
        let namedArgs =
            syntaxNamedArgList.ChildrenOfType
            |> ImArray.choose (function
                | OlySyntaxNamedArgument.NamedArgument(syntaxIdent, _, syntaxExpr) ->
                    let ident = syntaxIdent.ValueText
                    match value.Enclosing with
                    | EnclosingSymbol.Entity(ent) ->
                        let ty = ent.AsType
                        match ty.FindFields(env.benv, QueryMemberFlags.Instance, QueryField.Intrinsic) |> Seq.tryFind (fun x -> x.Name = ident) with
                        | Some(field) ->
                            let expr = bindAttributeExpression cenv env field.Type true syntaxExpr
                            match tryAttributeConstant cenv syntaxExpr expr with
                            | Some(value) ->
                                AttributeNamedArgumentSymbol.Field(field, value) |> Some
                            | _ ->
                                None
                        | _ ->
                            match ty.FindProperties(env.benv, QueryMemberFlags.Instance, QueryField.Intrinsic, ident) |> Seq.tryFind (fun x -> x.Name = ident) with
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
                | _ ->
                    raise(InternalCompilerUnreachedException())
            )
        namedArgs
    | _ ->
        ImArray.empty

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

let bindAttributeExpression (cenv: cenv) (env: BinderEnvironment) (expectedTy: TypeSymbol) isArg syntaxExpr =

    let errorAttribute cenv env syntaxExpr =
        cenv.diagnostics.Error("Not a valid attribute expression.", 10, syntaxExpr)  

    match syntaxExpr with
    | OlySyntaxExpression.Literal(syntaxLiteral) ->
        BoundExpression.Literal(BoundSyntaxInfo.User(syntaxLiteral, env.benv), bindLiteral cenv env (Some expectedTy) syntaxLiteral)
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
        let resInfo = ResolutionInfo.Default.UpdateArguments(ResolutionArguments.Any)
        let item = bindNameAsItem cenv env (Some syntaxExpr) None resInfo syntaxName
        match item with
        | ResolutionItem.Error _ ->
            BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))
        | ResolutionItem.Expression(expr) ->
            expr
        | _ ->
            errorAttribute cenv env syntaxExpr
            BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))

    | _ ->
        errorAttribute cenv env syntaxExpr
        BoundExpression.None(BoundSyntaxInfo.User(syntaxExpr, env.benv))

let bindAttributeArguments (cenv: cenv) (env: BinderEnvironment) syntaxArgs =
    match syntaxArgs with
    | OlySyntaxArguments.Arguments(_, syntaxArgList, _, _) ->
        syntaxArgList.ChildrenOfType
        |> ImArray.map (fun x -> bindAttributeExpression cenv env (mkInferenceVariableType None) true x)
    | OlySyntaxArguments.Empty _ ->
        ImArray.empty

    | _ ->
        raise(InternalCompilerUnreachedException())

let bindAttributes (cenv: cenv) (env: BinderEnvironment) isLate syntaxAttrs =
    match syntaxAttrs with
    | OlySyntaxAttributes.Attributes _ ->
        syntaxAttrs.Values
        |> ImArray.choose (fun syntaxAttr ->
            match syntaxAttr with
            | OlySyntaxAttribute.AutoOpen _ ->
                AttributeSymbol.Open
                |> Some

            | OlySyntaxAttribute.Nullable _ ->
                AttributeSymbol.Null
                |> Some

            | OlySyntaxAttribute.Intrinsic(_, _, strLit, _) ->
                AttributeSymbol.Intrinsic(strLit.ValueText)
                |> Some

            | OlySyntaxAttribute.Blittable _ ->
                AttributeSymbol.Blittable
                |> Some

            | OlySyntaxAttribute.Inline(_, _, syntaxIdent, _) ->
                let inlineArg =
                    match syntaxIdent.ValueText with
                    | "never" -> InlineArgumentSymbol.Never
                    | "always" -> InlineArgumentSymbol.Always
                    | _ -> InlineArgumentSymbol.None
                AttributeSymbol.Inline(inlineArg)
                |> Some

            | OlySyntaxAttribute.Export _ ->
                AttributeSymbol.Export
                |> Some

            | OlySyntaxAttribute.Pure _ ->
                AttributeSymbol.Pure
                |> Some

            | OlySyntaxAttribute.Import(syntaxName, syntaxArgs) ->
                let argExprs = bindAttributeArguments cenv env syntaxArgs
                match syntaxArgs with
                | OlySyntaxArguments.Arguments(_, _, syntaxNamedArgList, _) ->
                    if not syntaxNamedArgList.Children.IsEmpty then
                        cenv.diagnostics.Error("Named arguments do not exist on 'import'.", 10, syntaxNamedArgList)
                | _ ->
                    ()

                // TODO: This is kind of weird. What if we create an attribute named "import" or we import an attribute named "import", what happens?
                let resInfo = ResolutionInfo.Create(ValueSome argExprs, ResolutionTypeArityZero, ResolutionContext.ValueOnly)
                let resFormalItem = bindNameAsFormalItem cenv env None None resInfo syntaxName
                match resFormalItem with
                | ResolutionFormalItem.Value(_, value) when value.IsFunction ->
                    let func = value :?> IFunctionSymbol
                    if func.Parameters.Length = 3 && argExprs.Length = 3 then
                        let strArgs =
                            argExprs
                            |> ImArray.mapi (fun i x ->
                                match x with
                                | BoundExpression.Literal(syntaxInfo, BoundLiteral.Constant(ConstantSymbol.Utf16(value))) -> 
                                    if i = 0 && System.String.IsNullOrWhiteSpace value then
                                        cenv.diagnostics.Error("Import platform cannot be a whitespace-only or empty string.", 10, syntaxInfo.Syntax)
                                    if i = 2 && System.String.IsNullOrWhiteSpace value then
                                        cenv.diagnostics.Error("Import name cannot be a whitespace-only or empty string.", 10, syntaxInfo.Syntax)
                                    value
                                | _ -> 
                                    cenv.diagnostics.Error("Invalid argument for import attribute.", 10, x.Syntax)
                                    ""
                            )

                        let platform = strArgs[0]
                        let path = strArgs[1]
                        let name = strArgs[2]

                        AttributeSymbol.Import(platform, ImArray.createOne path, name)
                        |> Some
                    else
                        cenv.diagnostics.Error("Invalid import attribute.", 10, syntaxAttr)
                        AttributeSymbol.Import("", ImArray.empty, "")
                        |> Some
                | _ ->
                    cenv.diagnostics.Error("Invalid import attribute.", 10, syntaxAttr)
                    AttributeSymbol.Import("", ImArray.empty, "")
                    |> Some

            | OlySyntaxAttribute.Expression(syntaxExpr) ->
                if isLate then
                    let expr = bindAttributeExpression cenv env (mkInferenceVariableType None) false syntaxExpr
                    match expr with
                    | BoundExpression.Call(value=value;args=argExprs) ->
                        let attrArgs =
                            argExprs
                            |> ImArray.choose (fun x -> tryAttributeConstant cenv syntaxAttr x)

                        let attrNamedArgs =
                            match syntaxExpr with
                            | OlySyntaxExpression.Call(_, syntaxArgs) ->
                                bindAttributeNamedArguments cenv env value syntaxArgs
                            | _ ->
                                ImArray.empty

                        AttributeSymbol.Constructor(value :?> IFunctionSymbol, attrArgs, attrNamedArgs, AttributeFlags.AllowOnAll)
                        |> Some
                    | _ ->
                        None
                else
                    None

            | OlySyntaxAttribute.Error _ ->
                None

            | _ ->
                raise(InternalCompilerException())
        )
        |> ImArray.ofSeq
    | OlySyntaxAttributes.Empty _ ->
        ImArray.empty

    | _ ->
        raise(InternalCompilerException())

let tryAddIntrinsicPrimitivesForEntity cenv (env: BinderEnvironment) (kind: EntityKind) tyParCount (syntaxAttrs: OlySyntaxAttributes) (attrs: AttributeSymbol imarray) =
    match attrs |> ImArray.tryFindIndex (function AttributeSymbol.Intrinsic _ -> true | _ -> false) with
    | Some i ->
        let attr = attrs.[i]
        let syntaxAttr = syntaxAttrs.Values.[i]
        match attr with
        | AttributeSymbol.Intrinsic(intrinsicName) ->
            let error () =
                cenv.diagnostics.Error("Invalid intrinsic for this construct.", 10, syntaxAttr)

            if kind <> EntityKind.Alias then
                if not (attributesContainImport attrs) then
                    cenv.diagnostics.Error("'intrinsic' attribute can only be used on alias types or types that are imported.", 10, syntaxAttr)

            if tyParCount = 0 then
                match intrinsicName with
                | "int8" ->
                    Some TypeSymbol.Int8
                | "int16" ->
                    Some TypeSymbol.Int16
                | "int32" ->
                    Some TypeSymbol.Int32
                | "int64" ->
                    Some TypeSymbol.Int64
                | "uint8" ->
                    Some TypeSymbol.UInt8
                | "uint16" ->
                    Some TypeSymbol.UInt16
                | "uint32" ->
                    Some TypeSymbol.UInt32
                | "uint64" ->
                    Some TypeSymbol.UInt64
                | "float32" ->
                    Some TypeSymbol.Float32
                | "float64" ->
                    Some TypeSymbol.Float64
                | "char16" ->
                    Some TypeSymbol.Char16
                | "bool" ->
                    Some TypeSymbol.Bool
                | "native_int" ->
                    Some TypeSymbol.NativeInt
                | "native_uint" ->
                    Some TypeSymbol.NativeUInt
                | "utf16" ->
                    Some TypeSymbol.Utf16
                | "base_object" ->
                    Some TypeSymbol.BaseObject
                | "void" ->
                    Some TypeSymbol.Void
                | _ ->
                    error()
                    None

            elif tyParCount = 1 then
                match intrinsicName with
                | "by_ref_read_write" ->
                    Some(Types.ByRef)
                | "by_ref_read" ->
                    Some(Types.InRef)
                | "native_ptr" ->
                    Some(Types.NativePtr)
                | _ ->
                    error()
                    None
            else
                error()
                None
        | _ ->
            None
    | _ ->
        None

let tryFindIntrinsicAttribute (syntaxAttrs: OlySyntaxAttributes) (attrs: AttributeSymbol imarray) =
    match attrs |> ImArray.tryFindIndex (function AttributeSymbol.Intrinsic _ -> true | _ -> false) with
    | Some i ->
        let attr = attrs.[i]
        let syntaxAttr = syntaxAttrs.Values.[i]
        ValueSome(syntaxAttr, attr)
    | _ ->
        ValueNone

let bindIntrinsicPrimitivesForFunction cenv (env: BinderEnvironment) (syntaxAttrs: OlySyntaxAttributes) (func: IFunctionSymbol) =
    let attrs = func.Attributes
    let tyParCount = func.TypeParameters.Length
    let parCount = func.Parameters.Length
    match tryFindIntrinsicAttribute syntaxAttrs attrs with
    | ValueSome(syntaxAttr, attr) ->
        match attr with
        | AttributeSymbol.Intrinsic(intrinsicName) ->
            let error () =
                cenv.diagnostics.Error("Invalid intrinsic for this construct.", 10, syntaxAttr)

            match intrinsicName with
            | "constant" ->
                if attrs |> ImArray.exists (function AttributeSymbol.Import _ -> true | _ -> false) then
                    ()
                else
                    error()

            | _ ->
                match WellKnownFunction.TryFromName intrinsicName with
                | Some(wkf) ->
                    if Oly.Compiler.Internal.WellKnownFunctions.Validate(wkf, func) then
                        ()
                    else
                        error()
                | _ ->
                    error()
        | _ ->
            ()
    | _ ->
        ()