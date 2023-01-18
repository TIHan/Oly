[<AutoOpen>]
module rec Oly.Compiler.Workspace.Extensions.WorkspaceExtensions

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic
open System.Collections.Concurrent
open Oly.Compiler.Extensions
open Oly.Compiler.Workspace
open Oly.Compiler.Syntax

[<Struct;RequireQualifiedAccess>]
type OlyClassificationKind =
    | None
    | Type
    | Class
    | Interface
    | Struct
    | Shape
    | Enum
    | EnumStruct
    | Parameter
    | TypeParameter
    | LocalValue
    | Constructor
    | ConstructorStruct
    | Function
    | Field
    | FieldConstant
    | Property
    | StaticFunction
    | StaticField
    | StaticProperty
    | MutableParameter
    | MutableLocalValue
    | MutableField
    | MutableStaticField
    | AbstractFunction
    | StaticAbstractFunction
    | AbstractProperty
    | StaticAbstractProperty
    | Operator
    | Namespace
    | Module
    | Keyword
    | KeywordControl
    | ConstantNumber
    | ConstantBool
    | ConstantChar
    | ConstantUtf
    | ConstantNull
    | ConstantDefault
    | Directive
    | ConditionalDirective

type OlyClassificationModifierFlags =
    | None       = 0x00000
    | Definition = 0x00001

[<Struct;DebuggerDisplay("{Kind}")>]
type OlyClassificationItem(kind: OlyClassificationKind, flags: OlyClassificationModifierFlags, start: OlyTextPosition, width: int) =

    member _.Kind = kind

    member _.Flags = flags

    member _.Start = start

    member _.Width = width

let private classifyValueKind (valueSymbol: OlyValueSymbol) =
    if valueSymbol.IsParameter then
        OlyClassificationKind.Parameter
    elif valueSymbol.IsFunction then
        if valueSymbol.IsConstructor then
            if valueSymbol.Enclosing.IsStruct then
                OlyClassificationKind.ConstructorStruct
            else
                OlyClassificationKind.Constructor
        else
            if valueSymbol.IsStatic then
                if valueSymbol.IsAbstract then
                    OlyClassificationKind.StaticAbstractFunction
                else
                    OlyClassificationKind.StaticFunction
            else
                if valueSymbol.IsAbstract then
                    OlyClassificationKind.AbstractFunction
                else
                    OlyClassificationKind.Function
    elif valueSymbol.IsOperator then
        OlyClassificationKind.Operator
    elif valueSymbol.IsField then
        if valueSymbol.IsFieldConstant then
            OlyClassificationKind.FieldConstant
        else
            if valueSymbol.IsStatic then
                if valueSymbol.IsMutable then
                    OlyClassificationKind.MutableStaticField
                else
                    OlyClassificationKind.StaticField
            else
                if valueSymbol.IsMutable then
                    OlyClassificationKind.MutableField
                else
                    OlyClassificationKind.Field
    elif valueSymbol.IsProperty then
        if valueSymbol.IsStatic then
            if valueSymbol.IsAbstract then
                OlyClassificationKind.StaticAbstractProperty
            else
                OlyClassificationKind.StaticProperty
        else
            if valueSymbol.IsAbstract then
                OlyClassificationKind.AbstractProperty
            else
                OlyClassificationKind.Property
    else
        OlyClassificationKind.LocalValue

let private classifyTypeKind (tySymbol: OlyTypeSymbol) =
    if tySymbol.IsTypeParameter then
        OlyClassificationKind.TypeParameter
    elif tySymbol.IsEnum then
        if tySymbol.IsStruct then
            OlyClassificationKind.EnumStruct
        else
            OlyClassificationKind.Enum
    elif tySymbol.IsStruct then
        OlyClassificationKind.Struct
    elif tySymbol.IsInterface then
        OlyClassificationKind.Interface
    elif tySymbol.IsClass then
        OlyClassificationKind.Class
    elif tySymbol.IsShape then
        OlyClassificationKind.Shape
    else
        OlyClassificationKind.Type

let private classifyFunctionGroupKind (funcGroupSymbol: OlyFunctionGroupSymbol) =
    if funcGroupSymbol.IsOperator then
        OlyClassificationKind.Operator
    else
        if funcGroupSymbol.IsStatic then
            OlyClassificationKind.StaticFunction
        else
            OlyClassificationKind.Function

let private classifyNamespaceKind (namespaceSymbol: OlyNamespaceSymbol) =
    OlyClassificationKind.Namespace

let private classifyConstantKind (constantSymbol: OlyConstantSymbol) =
    match constantSymbol.Value with
    | OlyConstant.UInt8 _
    | OlyConstant.Int8 _
    | OlyConstant.UInt16 _
    | OlyConstant.Int16 _
    | OlyConstant.UInt32 _
    | OlyConstant.Int32 _
    | OlyConstant.UInt64 _
    | OlyConstant.Int64 _
    | OlyConstant.Float32 _
    | OlyConstant.Float64 _ -> OlyClassificationKind.ConstantNumber
    | OlyConstant.Char16 _ -> OlyClassificationKind.ConstantChar
    | OlyConstant.Utf16 _ -> OlyClassificationKind.ConstantUtf
    | OlyConstant.True
    | OlyConstant.False _ -> OlyClassificationKind.ConstantBool
    | OlyConstant.Default -> OlyClassificationKind.ConstantDefault
    | OlyConstant.Null -> OlyClassificationKind.ConstantNull
    | OlyConstant.Array _ -> OlyClassificationKind.None // TODO: Maybe OlyClassificationKind.ConstantArray?
    | OlyConstant.External value -> classifyValueKind value 
    | OlyConstant.Variable _ -> OlyClassificationKind.TypeParameter
    | OlyConstant.Error -> OlyClassificationKind.None

let private classify (r: OlyTextRange) (span: OlyTextSpan) kind flags =
    OlyClassificationItem(kind, flags, r.Start, span.Width)

let private classifyType r span (tySymbol: OlyTypeSymbol) =
    let kind = classifyTypeKind tySymbol
    classify r span kind

let private classifyValue r span (valueSymbol: OlyValueSymbol) =
    let kind = classifyValueKind valueSymbol
    classify r span kind

let private classifyFunctionGroup r span (funcGroupSymbol: OlyFunctionGroupSymbol) =
    let kind = classifyFunctionGroupKind funcGroupSymbol
    classify r span kind

let private classifyNamespace r span (namespaceSymbol: OlyNamespaceSymbol) =
    let kind = classifyNamespaceKind namespaceSymbol
    classify r span kind

let private classifyConstant r span (constantSymbol: OlyConstantSymbol) =
    let kind = classifyConstantKind constantSymbol
    classify r span kind

type OlySymbol with

    member this.ClassificationKind =
        match this with
        | :? OlyNamespaceSymbol as symbol ->
            classifyNamespaceKind symbol
        | :? OlyFunctionGroupSymbol as symbol ->
            classifyFunctionGroupKind symbol
        | :? OlyValueSymbol as symbol ->
            classifyValueKind symbol
        | :? OlyTypeSymbol as symbol ->
            classifyTypeKind symbol
        | :? OlyConstantSymbol as symbol ->
            classifyConstantKind symbol
        | :? OlyDirectiveSymbol ->
            OlyClassificationKind.Directive
        | :? OlyConditionalDirectiveSymbol ->
            OlyClassificationKind.ConditionalDirective
        | _ ->
            OlyClassificationKind.None

type OlyDocument with

    member this.GetSemanticClassifications(range: OlyTextRange, ct: CancellationToken) : OlyClassificationItem imarray =
        ct.ThrowIfCancellationRequested()

        let boundModel = this.BoundModel

        match boundModel.SyntaxTree.TryFindNode(range, ct) with
        | Some syntaxNode ->
            let symbols = boundModel.GetSymbols(syntaxNode, ct)

            symbols
            |> ImArray.choose (fun symbol ->
                ct.ThrowIfCancellationRequested()
                match symbol.UseSyntax.TryFindFirstIdentifierOrLiteral() with
                | Some(identToken) ->
                    let r = identToken.GetTextRange(ct)
                    let span = identToken.TextSpan
                    let flags = OlyClassificationModifierFlags.None
                    match symbol with
                    | :? OlyTypeSymbol as tySymbol ->
                        Some(classifyType r span tySymbol flags)
                    | :? OlyFunctionGroupSymbol as funcGroupSymbol ->
                        Some(classifyFunctionGroup r span funcGroupSymbol flags)
                    | :? OlyValueSymbol as valueSymbol ->
                        Some(classifyValue r span valueSymbol flags)
                    | :? OlyNamespaceSymbol as namespaceSymbol ->
                        Some(classifyNamespace r span namespaceSymbol flags)
                    | :? OlyConstantSymbol as constantSymbol ->
                        Some(classifyConstant r span constantSymbol flags)
                    | _ ->
                        None
                | _ ->
                    None
            )
        | _ ->
            ImArray.empty

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type OlyCompletionContext =
    | None
    | Unqualified of OlyBoundSubModel
    | UnqualifiedType of OlyBoundSubModel
    | OpenDeclaration of OlyBoundSubModel
    | Symbol of OlySymbol * inStaticContext: bool

[<Struct;DebuggerDisplay("{Label}")>]
type OlyCompletionItem(label: string, classificationKind: OlyClassificationKind, detail: string, insertText: string) =

    member _.Label = label

    member _.ClassificationKind = classificationKind

    member _.Detail = detail

    member _.InsertText = insertText

    member internal this.WithLabelAndInsertText(label: string, insertText: string) =
        OlyCompletionItem(label, classificationKind, detail, insertText)

    new(label, kind, detail) =
        OlyCompletionItem(label, kind, detail, "")

let private allGreekSymbols =
    let greekSymbols =
        [
            ('Α', "ALPHA")
            ('Β', "BETA")
            ('Γ', "GAMMA")
            ('Ε', "EPSILON")
            ('Ζ', "ZETA")
            ('Η', "ETA")
            ('Θ', "THETA")
            ('Ι', "IOTA")
            ('Κ', "KAPPA")
            ('Λ', "LAMBDA")
            ('Μ', "MU")
            ('Ν', "NU")
            ('Ξ', "XI")
            ('Ο', "OMICRON")
            ('Π', "PI")
            ('Ρ', "RHO")
            ('Σ', "SIGMA")
            ('Τ', "TAU")
            ('Υ', "UPSILON")
            ('Φ', "PHI")
            ('Χ', "CHI")
            ('Ψ', "PSI")
            ('Ω', "OMEGA")
        ]
    
    let greekSymbolsLowercase =
        greekSymbols
        |> List.map (fun (x, y) ->
            (Char.ToLowerInvariant(x), y.ToLowerInvariant())
        )

    greekSymbols @ greekSymbolsLowercase @ [('ς', "final sigma")]
    |> Seq.map (fun (x, y) -> KeyValuePair(x, y))
    |> ConcurrentDictionary

let private controlKeywords =
    [
        "match"
        "open"
        "when"
        "and"
        "or"
        "is"
        "if"
        "else"
        "while"
        "import"
        "export"
        "where"
        "for"
        "do"
        "in"
        "out"
        "try"
        "finally"
        "as"
        "to"
        "of"
        "throw"
    ]
    |> ImArray.ofSeq

let private keywords =
    [
        "let"
        "get"
        "set"
        "type"
        "module"
        "with"
        "mutable"
        "inline"
        "trait"
        "static"
        "pure"
        "component"
        "public"
        "private"
        "protected"
        "internal"
        "inherits"
        "implements"
        "interface"
        "refine"
        "class"
        "shape"
        "constant"
        "abstract"
        "struct"
        "default"
        "attribute"
        "intrinsic"
        "extension"
        "implicit"
        "explicit"
        "overrides"
        "modifier"
        "sealed"
        "data"
        "base"
        "pattern"
        "constraint"
        "true"
        "false"
        "null"
        "enum"
    ]
    |> ImArray.ofSeq

type OlyDocument with

    member this.TryFindSymbol(line, column, ct) =
        let boundModel = this.BoundModel
        let syntaxTree = this.SyntaxTree

        match syntaxTree.GetSourceText(ct).TryGetPosition(OlyTextPosition(line, column)) with
        | Some position ->
            match syntaxTree.GetRoot(ct).TryFindToken(position, skipTrivia = false, ct = ct) with
            | Some token ->
                boundModel.TryFindSymbol(token, ct) 
            | _ ->
                None
        | _ ->
            None

    member this.TryFindFunctionCallSymbol(line, column, ct) =
        let boundModel = this.BoundModel
        let syntaxTree = this.SyntaxTree

        match syntaxTree.GetSourceText(ct).TryGetPosition(OlyTextPosition(line, column)) with
        | Some position ->
            match syntaxTree.TryFindFunctionCallToken(position) with
            | Some token ->
                boundModel.TryFindSymbol(token, ct)
            | _ ->
                None
        | _ ->
            None

    member this.FindSimilarSymbols(symbol: OlySymbol, ct) =
        let boundModel = this.BoundModel       
        let symbols = boundModel.GetSymbolsByPossibleName(this.SyntaxTree.GetRoot(ct), symbol.Name, ct)    
        symbols |> ImArray.filter (fun x -> x.IsSimilarTo(symbol))

    member this.GetAllSymbols(ct) : _ imarray =
        let boundModel = this.BoundModel
        boundModel.GetSymbols(this.SyntaxTree.GetRoot(ct), ct)

    member this.GetAllSymbolsByPossibleName(possibleName: string, ct) : _ imarray =
        let boundModel = this.BoundModel
        boundModel.GetSymbolsByPossibleName(this.SyntaxTree.GetRoot(ct), possibleName, ct)

    member this.GetCompletions(line, column, ct) =
        let syntaxTree = this.SyntaxTree
        match syntaxTree.GetSourceText(ct).TryGetPosition(OlyTextPosition(line, column)) with
        | Some position -> this.GetCompletions(position, ct)
        | _ -> Seq.empty

    member this.GetCompletions(position: int, ct) =
        let syntaxTree = this.SyntaxTree
        let boundModel = this.BoundModel

        let completions = ResizeArray<OlyCompletionItem>()

        let context =
            let tokenOpt = syntaxTree.GetRoot(ct).TryFindToken(position, ct=ct, skipTrivia = false)

            let hasDotOnLeft, tokenOpt =
                match tokenOpt with
                | Some token -> 
                    if token.IsDot then
                        true, tokenOpt
                    else
                        match token.TryPreviousToken((fun _ -> true), ct = ct) with
                        | ValueSome token2 ->
                            let r1 = token.GetTextRange(ct)
                            let r2 = token2.GetTextRange(ct)
                            if token2.IsDot then
                                true, Some token2
                            elif r1.Start.Line = r2.Start.Line then
                                false, Some token2
                            else
                                false, Some token
                        | _ -> 
                            false, tokenOpt
                | _ ->
                    false, tokenOpt

            let isPossiblyInStaticContext =
                // If the dot is after a parenthesis expression, we might
                // not be in a static context.
                tokenOpt
                |> Option.map (fun token ->
                    if token.IsDot then
                        match token.Node.Parent with
                        | :? OlySyntaxExpression as parentExpr ->
                            match parentExpr with
                            | OlySyntaxExpression.MemberAccess(OlySyntaxExpression.Parenthesis _, _, _) ->
                                false
                            | _ ->
                                true
                        | _ ->
                            true
                    else
                        true
                )
                |> Option.defaultValue true

            let tokenOpt =
                tokenOpt
                |> Option.map (fun token ->
                    if token.IsWhitespaceTrivia || token.IsDot then
                        match token.TryPreviousToken((fun t -> t.IsIdentifier || t.IsBase || t.IsThis), ct = ct) with
                        | ValueSome token2 -> 
                            let r1 = token.GetTextRange(ct)
                            let r2 = token2.GetTextRange(ct)
                            if r1.Start.Line = r2.Start.Line then
                                token2
                            else
                                token
                        | _ -> 
                            token
                    else
                        token
                )

            let tokenOpt =
                tokenOpt
                |> Option.map (fun token ->
                    if token.IsTrivia || token.IsEndOfSource then
                        match token.TryPreviousToken((fun _ -> true), skipTrivia = true, ct = ct) with
                        | ValueSome token2 ->
                            let r1 = token.GetTextRange(ct)
                            let r2 = token2.GetTextRange(ct)
                            if r1.Start.Line = r2.Start.Line then
                                token2
                            else
                                token
                        | _ -> 
                            token
                    else
                        token
                )

            match tokenOpt with
            | Some token ->

                let rec isInOpenDeclaration(parentOpt: OlySyntaxNode) =
                    match parentOpt with
                    | null ->
                        false
                    | parent ->
                        match parent with
                        | :? OlySyntaxExpression as parent ->
                            match parent with
                            | OlySyntaxExpression.OpenDeclaration _ -> true
                            | _ -> false
                        | :? OlySyntaxName as parent ->
                            isInOpenDeclaration parent.Parent
                        | _ ->
                            false

                let context =
                    if token.IsTrivia && not token.IsWhitespaceTrivia then
                        OlyCompletionContext.None
                    elif hasDotOnLeft then
                        match boundModel.TryFindSymbol(token, ct) with
                        | Some symbol ->
                            match symbol with
                            | :? OlyValueSymbol as symbol ->
                                match symbol.ReturnType with
                                | Some(returnTySymbol) ->
                                    OlyCompletionContext.Symbol(returnTySymbol, false)
                                | _ ->
                                    OlyCompletionContext.Symbol(symbol, false)
                            | _ -> 
                                OlyCompletionContext.Symbol(symbol, isPossiblyInStaticContext)
                        | _ ->
                            OlyCompletionContext.None
                    else
                        match boundModel.TryGetSubModel(token, ct) with
                        | Some subModel ->
                            let isInOpenDecl = isInOpenDeclaration token.Node.Parent

                            if isInOpenDecl then
                                OlyCompletionContext.OpenDeclaration subModel
                            // TODO: This only checks return type annotations, we need to look at others.
                            elif subModel.SyntaxNode.IsInReturnTypeAnnotation || subModel.SyntaxNode.IsParameterMissingTypeAnnotation || subModel.SyntaxNode.IsType then
                                OlyCompletionContext.UnqualifiedType subModel
                            else
                                if token.IsWhitespaceTrivia then
                                    match boundModel.TryGetWhitespaceSubModel(token.Text.Length, token, ct) with
                                    | Some subModel ->
                                        OlyCompletionContext.Unqualified subModel
                                    | _ ->
                                        OlyCompletionContext.Unqualified subModel
                                else
                                    OlyCompletionContext.Unqualified subModel
                        | _ ->
                            OlyCompletionContext.None

                ct.ThrowIfCancellationRequested()
                let containsText =
                    if token.IsIdentifierOrOperatorOrKeyword then
                        token.Text
                    else
                        ""
                match context with
                | OlyCompletionContext.None ->
                    ()
                | OlyCompletionContext.OpenDeclaration subModel ->
                    subModel.GetUnqualifiedNamespaceSymbols(containsText)
                    |> Seq.iter (fun namespaceSymbol ->
                        ct.ThrowIfCancellationRequested()
                        let kind = classifyNamespaceKind namespaceSymbol
                        completions.Add(OlyCompletionItem(namespaceSymbol.Name, kind, namespaceSymbol.SignatureText))
                    )
                    subModel.GetUnqualifiedTypeSymbols(containsText)
                    |> Seq.iter (fun ty ->
                        ct.ThrowIfCancellationRequested()
                        let kind = classifyTypeKind ty
                        completions.Add(OlyCompletionItem(ty.Name, kind, ty.SignatureText))
                    )
                | OlyCompletionContext.UnqualifiedType subModel ->
                    subModel.GetUnqualifiedTypeSymbols(containsText)
                    |> Seq.iter (fun ty ->
                        ct.ThrowIfCancellationRequested()
                        let kind = classifyTypeKind ty
                        completions.Add(OlyCompletionItem(ty.Name, kind, ty.SignatureText))
                    )
                | OlyCompletionContext.Unqualified subModel ->                   
                    subModel.GetUnqualifiedSymbols(containsText)
                    |> Seq.iter (fun x ->
                        ct.ThrowIfCancellationRequested()
                        match x with
                        | :? OlyFunctionGroupSymbol as funcGroup ->
                            let kind = classifyFunctionGroupKind funcGroup
                            completions.Add(OlyCompletionItem(funcGroup.Name, kind, funcGroup.SignatureText))
                        | :? OlyValueSymbol as value ->
                            let kind = classifyValueKind value
                            completions.Add(OlyCompletionItem(value.Name, kind, value.SignatureText))
                        | :? OlyTypeSymbol as ty ->
                            let kind = classifyTypeKind ty
                            completions.Add(OlyCompletionItem(ty.Name, kind, ty.SignatureText))
                        | :? OlyNamespaceSymbol as namespaceSymbol ->
                            let kind = classifyNamespaceKind namespaceSymbol
                            completions.Add(OlyCompletionItem(namespaceSymbol.Name, kind, namespaceSymbol.SignatureText))
                        | _ ->
                            ()
                    )

                | OlyCompletionContext.Symbol(symbol, inStaticContext) ->
                    match symbol with
                    | :? OlyNamespaceSymbol as symbol ->
                        if inStaticContext then
                            symbol.Types
                            |> ImArray.iter (fun ty -> 
                                ct.ThrowIfCancellationRequested()
                                let kind = classifyTypeKind ty
                                completions.Add(OlyCompletionItem(ty.Name, kind, ty.SignatureText))
                            )

                            symbol.Namespaces
                            |> ImArray.iter (fun nmspace ->
                                ct.ThrowIfCancellationRequested()
                                completions.Add(OlyCompletionItem(nmspace.Name, OlyClassificationKind.Namespace, nmspace.SignatureText))
                            )
                    | :? OlyTypeSymbol as symbol ->
                        if inStaticContext then
                            symbol.Types
                            |> ImArray.iter (fun ty -> 
                                ct.ThrowIfCancellationRequested()
                                let kind = classifyTypeKind ty
                                completions.Add(OlyCompletionItem(ty.Name, kind, ty.SignatureText))
                            )

                        let propFuncs = HashSet({ new IEqualityComparer<OlyValueSymbol> with
                                                        member _.GetHashCode(value) = value.GetHashCode()
                                                        member _.Equals(value1, value2) = value1 = value2 })

                        symbol.Fields
                        |> Seq.iter (fun field ->
                            ct.ThrowIfCancellationRequested()
                            if field.IsStatic = inStaticContext then
                                let kind = classifyValueKind field
                                completions.Add(OlyCompletionItem(field.Name, kind, field.SignatureText))
                        )

                        symbol.Properties
                        |> Seq.iter (fun prop ->
                            ct.ThrowIfCancellationRequested()
                            if prop.IsStatic = inStaticContext then
                                let kind = classifyValueKind prop
                                completions.Add(OlyCompletionItem(prop.Name, kind, prop.SignatureText))

                                match prop.TryPropertyGetterSetter with
                                | Some(Some(getter), _) ->
                                    propFuncs.Add(getter) |> ignore
                                | _ ->
                                    ()

                                match prop.TryPropertyGetterSetter with
                                | Some(_, Some(setter)) ->
                                    propFuncs.Add(setter) |> ignore
                                | _ ->
                                    ()
                        )

                        symbol.Functions
                        |> ImArray.iter (fun func ->
                            ct.ThrowIfCancellationRequested()
                            if propFuncs.Contains(func) |> not then
                                if func.IsStatic = inStaticContext && not func.IsConstructor then
                                    let kind = classifyValueKind func
                                    let label =
                                        if func.IsOperator then 
                                            "(" + func.Name + ")"
                                        else
                                            func.Name
                                    completions.Add(OlyCompletionItem(label, kind, func.SignatureText))
                        )

                    | :? OlyValueSymbol as symbol ->
                        let ty = symbol.Type

                        let propFuncs = HashSet({ new IEqualityComparer<OlyValueSymbol> with
                                                        member _.GetHashCode(value) = value.GetHashCode()
                                                        member _.Equals(value1, value2) = value1 = value2 })

                            
                        ty.Fields
                        |> Seq.iter (fun field ->
                            ct.ThrowIfCancellationRequested()
                            if field.IsStatic = inStaticContext then
                                let kind = classifyValueKind field
                                completions.Add(OlyCompletionItem(field.Name, kind, field.SignatureText))
                        )

                        ty.Properties
                        |> Seq.iter (fun prop ->
                            ct.ThrowIfCancellationRequested()
                            if prop.IsStatic = inStaticContext then
                                let kind = classifyValueKind prop
                                completions.Add(OlyCompletionItem(prop.Name, kind, prop.SignatureText))

                                match prop.TryPropertyGetterSetter with
                                | Some(Some(getter), _) ->
                                    propFuncs.Add(getter) |> ignore
                                | _ ->
                                    ()

                                match prop.TryPropertyGetterSetter with
                                | Some(_, Some(setter)) ->
                                    propFuncs.Add(setter) |> ignore
                                | _ ->
                                    ()
                        )

                        ty.Functions
                        |> ImArray.iter (fun func ->
                            ct.ThrowIfCancellationRequested()
                            if propFuncs.Contains(func) |> not then
                                if func.IsStatic = inStaticContext && not func.IsConstructor then
                                    let kind = classifyValueKind func
                                    let label =
                                        if func.IsOperator then 
                                            "(" + func.Name + ")"
                                        else
                                            func.Name
                                    completions.Add(OlyCompletionItem(label, kind, func.SignatureText))
                        )

                    | _ ->
                        ()

                context
            | _ ->
                OlyCompletionContext.None

        let completions =
            completions
            |> Seq.filter (fun x -> 
                ct.ThrowIfCancellationRequested()
                not (x.Label.StartsWith("__oly_"))
            )
            |> Seq.map (fun x ->
                ct.ThrowIfCancellationRequested()
                if x.Label.Length = 1 then
                    let label = x.Label.[0]
                    match allGreekSymbols.TryGetValue label with
                    | true, text ->
                        x.WithLabelAndInsertText(sprintf "%s (%s)" x.Label text, x.Label)
                    | _ ->
                        x
                else
                    x
            )
            |> Array.ofSeq

        match context with
        | OlyCompletionContext.None ->
            Seq.empty
        | OlyCompletionContext.Unqualified _ ->
            let controlKeywordCompletions =
                controlKeywords
                |> Seq.map (fun x ->
                    ct.ThrowIfCancellationRequested()
                    OlyCompletionItem(x, OlyClassificationKind.KeywordControl, String.Empty)
                )
        
            let keywordCompletions =
                keywords
                |> Seq.map (fun x ->
                    ct.ThrowIfCancellationRequested()
                    OlyCompletionItem(x, OlyClassificationKind.Keyword, String.Empty)
                )
            
            seq { yield! completions; yield! controlKeywordCompletions; yield! keywordCompletions }
        | _ ->
            completions :> _ seq
        |> Seq.sortBy(fun x ->
            ct.ThrowIfCancellationRequested()
            x.Label
        )

    // TODO: Better API name.
    member this.IsTarget(value, ?ct) =
        let ct = defaultArg ct CancellationToken.None
        match this.SyntaxTree.GetCompilationUnitConfiguration(ct).Target with
        | Some(_, value2) ->
            value = value2
        | _ ->
            false