module internal rec Oly.Compiler.Internal.Binder.EarlyAttributes

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.Binder

// TODO: We could do the verification for the import arguments much earlier in parsing, ensuring we only allow strings.

let private bindImportAttributeArguments (cenv: cenv) (env: BinderEnvironment) syntaxArgs =
    match syntaxArgs with
    | OlySyntaxArguments.Arguments(_, syntaxArgList, _, _) ->
        syntaxArgList.ChildrenOfType
        |> ImArray.map (fun x -> bindEarlyAttributeExpression cenv env x)

    | OlySyntaxArguments.Empty _ ->
        ImArray.empty

    | _ ->
        raise(InternalCompilerUnreachedException())

let bindEarlyAttributeExpression (cenv: cenv) (env: BinderEnvironment) syntaxExpr =
    let errorAttribute cenv env syntaxExpr =
        cenv.diagnostics.Error("Not a valid attribute expression.", 10, syntaxExpr)  

    match syntaxExpr with
    | OlySyntaxExpression.Literal(syntaxLiteral) ->
        BoundExpression.Literal(BoundSyntaxInfo.User(syntaxLiteral, env.benv), bindLiteral cenv syntaxLiteral)

    | _ ->
        errorAttribute cenv env syntaxExpr
        BoundExpression.None(BoundSyntaxInfo.User(syntaxExpr, env.benv))

let bindEarlyAttribute (cenv: cenv) (env: BinderEnvironment) syntaxAttr =
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

    | OlySyntaxAttribute.Unmanaged(_, _, syntaxIdent, _) ->
        match syntaxIdent.ValueText with
        | "allocation_only" -> 
            AttributeSymbol.Unmanaged(UnmanagedArgumentSymbol.AllocationOnly)
            |> Some
        | _ ->
            None

    | OlySyntaxAttribute.Export _ ->
        AttributeSymbol.Export
        |> Some

    | OlySyntaxAttribute.Pure _ ->
        AttributeSymbol.Pure
        |> Some

    | OlySyntaxAttribute.Import(syntaxName, syntaxArgs) ->
        let argExprs = bindImportAttributeArguments cenv env syntaxArgs
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

    | OlySyntaxAttribute.Expression _
    | OlySyntaxAttribute.Error _ ->
        None

    | _ ->
        raise(InternalCompilerException())

let bindEarlyAttributes (cenv: cenv) (env: BinderEnvironment) syntaxAttrs =
    match syntaxAttrs with
    | OlySyntaxAttributes.Attributes _ ->
        syntaxAttrs.Values
        |> ImArray.choose (fun syntaxAttr ->
            bindEarlyAttribute cenv env syntaxAttr
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
                if intrinsicName <> "importer" then
                    cenv.diagnostics.Error("Invalid intrinsic for this construct.", 10, syntaxAttr)

            if kind <> EntityKind.Alias then
                if not (attributesContainImport attrs) then
                    cenv.diagnostics.Error("'intrinsic' attribute can only be used on alias types or types that are imported.", 10, syntaxAttr)

            match env.benv.ac.Entity with
            | Some(ent) when not ent.TypeParameters.IsEmpty ->
                error()
                None
            | _ ->

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
                | "by_ref" ->
                    Some(Types.ByRef)
                | "by_ref_read_only" ->
                    Some(Types.InRef)
                | "by_ref_write_only" ->
                    Some(Types.OutRef)
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
