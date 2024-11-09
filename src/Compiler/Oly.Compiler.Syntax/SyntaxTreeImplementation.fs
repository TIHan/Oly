namespace rec Oly.Compiler.Syntax

open System
open System.Text
open System.Threading
open System.Runtime.CompilerServices
open System.Diagnostics
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Syntax.Internal
open Oly.Compiler.Syntax.Internal.Lexer
open Oly.Compiler.Syntax.Internal.Parser
open Oly.Compiler.Syntax.Internal.SyntaxErrors

[<Struct;NoEquality;NoComparison;DebuggerDisplay("{Text}")>]
type OlyToken =
    
    val private node: OlySyntaxToken

    new(node) =
        {
            node = node
        }

    member this.GetLeadingTrivia() =
        this.node.GetLeadingTrivia()
        |> ImArray.map (fun (x: OlySyntaxNode) -> OlyToken(x :?> OlySyntaxToken))

    member this.Tree = this.node.Tree

    member this.Node = this.node        

    member this.TextSpan = this.node.TextSpan

    member this.FullTextSpan = this.node.FullTextSpan
                
    member this.GetTextRange(ct) =
        this.Tree.GetSourceText(ct).GetTextRange(this.TextSpan)

    member this.GetFullTextRange(ct) =
        this.Tree.GetSourceText(ct).GetTextRange(this.FullTextSpan)

    member this.Text = this.node.Internal.Text

    member this.IsAnyDirective =
        match this.node.Internal.RawToken with
        | Directive _
        | ConditionalDirective _ -> 
            true
        | _ ->
            false

    member this.TryDirectiveText =
        match this.node.Internal.RawToken with
        | Directive(_, identToken, _, valueToken) -> 
            ValueSome(identToken.ValueText, valueToken.ValueText)
        | _ ->
            ValueNone

    member this.TryConditionalDirectiveText =
        match this.node.Internal.RawToken with
        | ConditionalDirective(prevtoken, bodyText, token) -> 
            ValueSome(prevtoken.ValueText, bodyText, token.ValueText)
        | _ ->
            ValueNone

    member this.TryStringLiteralText = this.node.Internal.RawToken.TryStringLiteralText

    member this.IsKeyword = this.node.Internal.RawToken.IsKeyword

    member this.IsIdentifier = this.node.Internal.RawToken.IsIdentifierToken

    member this.IsOperator = this.node.Internal.RawToken.IsOperator

    member this.IsBase = this.node.Internal.RawToken = Base

    member this.IsThis = this.node.Internal.RawToken = This

    member this.IsStringLiteral =
        match this.node.Internal.RawToken with
        | StringLiteral _ -> true
        | _ -> false

    member this.IsNumericLiteral =
        match this.node.Internal.RawToken with
        | Int8Literal _
        | UInt8Literal _
        | Int16Literal _
        | UInt16Literal _ 
        | Int32Literal _
        | UInt32Literal _
        | Int64Literal _
        | UInt64Literal _
        | Float32Literal _
        | Float64Literal _ -> true
        | _ -> false

    member this.IsBoolLiteral =
        match this.node.Internal.RawToken with
        | True
        | False -> true
        | _ -> false

    member this.IsCharLiteral =
        match this.node.Internal.RawToken with
        | CharLiteral _ -> true
        | _ -> false

    member this.IsLiteral =
        this.node.Internal.RawToken.IsLiteral

    member this.IsTrivia = this.node.Internal.RawToken.IsTrivia

    member this.IsWhitespaceTrivia =
        match this.node.Internal.RawToken with
        | Token.Space _ -> true
        | _ -> false

    member this.IsSingleLineCommentStartTrivia =
        match this.node.Internal.RawToken with
        | Token.SingleLineCommentStart -> true
        | _ -> false

    member this.IsSingleLineCommentTrivia =
        match this.node.Internal.RawToken with
        | Token.SingleLineComment _ -> true
        | _ -> false

    member this.IsMultiLineCommentStartTrivia =
        match this.node.Internal.RawToken with
        | Token.MultiLineCommentStart -> true
        | _ -> false

    member this.IsMultiLineCommentTrivia =
        match this.node.Internal.RawToken with
        | Token.MultiLineComment _ -> true
        | _ -> false

    member this.IsMultiLineCommentEndTrivia =
        match this.node.Internal.RawToken with
        | Token.MultiLineCommentEnd -> true
        | _ -> false

    member this.IsNewLineTrivia =
        match this.node.Internal.RawToken with
        | Token.NewLine -> true
        | _ -> false

    member this.IsCarriageReturnTrivia =
        match this.node.Internal.RawToken with
        | Token.CarriageReturn -> true
        | _ -> false

    member this.IsCarriageReturnNewLineTrivia =
        match this.node.Internal.RawToken with
        | Token.CarriageReturnNewLine -> true
        | _ -> false

    member this.IsDot = this.node.Internal.RawToken = Token.Dot

    member this.IsComma = this.node.Internal.RawToken = Token.Comma

    member this.IsRightArrow = this.node.Internal.RawToken = Token.RightArrow

    member this.IsLeftArrow = this.node.Internal.RawToken = Token.LeftArrow

    member this.IsMinus = this.node.Internal.RawToken = Token.Minus

    member this.IsSemiColon = this.node.Internal.RawToken = Token.SemiColon

    member this.IsEqual = this.node.Internal.RawToken = Token.Equal

    member this.IsColon = this.node.Internal.RawToken = Token.Colon

    member this.IsEndOfSource = this.node.Internal.RawToken = Token.EndOfSource

    member this.ReplaceWith(text: string, ct) =
        let textSpan = this.TextSpan
        let textChange = OlyTextChange(textSpan, text)

        let sourceText = this.Node.Tree.GetSourceText(ct)
        let newSourceText = sourceText.ApplyTextChanges([|textChange|])

        OlySyntaxTree.Parse(this.Tree.Path, newSourceText, parsingOptions = this.Tree.ParsingOptions)

    member private this.Change(text: string, pos: int, ct) =
        let textChange = OlyTextChange(OlyTextSpan.Create(pos, 0), text)

        let sourceText = this.Node.Tree.GetSourceText(ct)
        let newSourceText = sourceText.ApplyTextChanges([|textChange|])

        let newTree = OlySyntaxTree.Parse(this.Tree.Path, newSourceText, parsingOptions = this.Tree.ParsingOptions)
        newTree.GetRoot(ct).FindTokens(pos + text.Length, ct = ct)
        |> Seq.exactlyOne

    member this.Prepend(text: string, ct) =
        this.Change(text, this.TextSpan.Start, ct)

    member this.Append(text: string, ct) =
        this.Change(text, this.TextSpan.End, ct)

    member this.HasParentTypeParametersOrTypeArguments =
        match this.Node.Parent with
        | :? OlySyntaxTypeParameters
        | :? OlySyntaxTypeArguments ->
            true
        | _ ->
            false

    member this.IsIdentifierOrOperatorOrKeyword =
        this.node.Internal.RawToken.IsIdentifierOrOperatorOrKeyword

    member this.TryPreviousToken(?predicate: OlyToken -> bool, ?skipTrivia: bool, ?ct: CancellationToken) : OlyToken voption =
        let predicate = defaultArg predicate (fun _ -> true)
        let skipTrivia = defaultArg skipTrivia true
        let ct = defaultArg ct CancellationToken.None
        let origNode = this.Node
        let isTrivia = this.IsTrivia

        let rec loop (node: OlySyntaxNode) =
            match node.Parent with
            | null -> ValueNone
            | parentNode ->
                let tokens = parentNode.GetDescendantTokens(skipTrivia = skipTrivia && not isTrivia, ct = ct)
                let indexOpt = tokens |> Seq.tryFindIndex (fun x -> obj.ReferenceEquals(x.Node, origNode))
                match indexOpt with
                | Some index ->
                    match index with
                    | 0 ->
                        loop parentNode
                    | _ ->
                        let tokenOpt = 
                            tokens 
                            |> Seq.take (index) 
                            |> Seq.filter (fun x -> if skipTrivia && x.IsTrivia then false else predicate x)
                            |> Seq.tryLast
                        match tokenOpt with
                        | Some token -> ValueSome token
                        | _ -> loop parentNode
                | _ ->
                    ValueNone
        loop origNode

[<Sealed;DebuggerDisplay("{Path}")>]
type internal OlySyntaxTreeImplementation(path, getSourceText: CancellationToken -> IOlySourceText, version, parsingOptions: OlyParsingOptions) as this =
    inherit OlySyntaxTree(path, getSourceText, version, parsingOptions)

    let weakTable = ConditionalWeakTable<OlySyntaxNode, SyntaxTree>()

    let getRoot =
        WeakCacheValue(fun ct ->
            let internalTree = this.GetInternal(ct)
            let root = OlySyntaxCompilationUnit(this, 0, null, internalTree.Root) :> OlySyntaxNode
            weakTable.Add(root, internalTree)
            root
        )

    override _.WithPath(path) = 
        OlySyntaxTreeImplementation(path, getSourceText, version + 1UL, parsingOptions) :> OlySyntaxTree

    override this.ApplySourceText(sourceText: IOlySourceText) =
        OlySyntaxTreeImplementation(path, 
            (fun ct ->
                ct.ThrowIfCancellationRequested()
                sourceText
            ),
            version + 1UL,
            parsingOptions
        ) :> OlySyntaxTree

    override this.GetRoot(ct: CancellationToken) : OlySyntaxNode = 
        ct.ThrowIfCancellationRequested()
        getRoot.GetValue(ct)

[<RequireQualifiedAccess>]
module OlySyntaxFacts =

    let private SyntaxOperators =
        seq {
            EqualEqual
            EqualEqualEqual
            Exclamation
            ExclamationEqual
            ExclamationEqualEqual
            Dollar
            ForwardSlash
            Percent
            Caret
            Ampersand
            AmpersandAmpersand
            Star
            StarStar
            Minus
            MinusMinus
            Plus
            PlusPlus
            Pipe
            PipePipe
            DotDot
            DotDotDot
            Tilde
            LessThan
            LessThanEqual
            LessThanEqualLessThan
            GreaterThan
            GreaterThanEqual
            GreaterThanEqualGreaterThan
            LessThanStarGreaterThan
            GreaterThanGreaterThanEqual
            PipeGreaterThan
            PipeGreaterThanGreaterThan
            LessThanColon
            ColonGreaterThan 
            GreaterThanGreaterThan 
            LessThanLessThan
            Not
            And
            Or
            (Identifier("[]"))
            (Identifier("[,]"))
        }
        |> ImArray.ofSeq

    let IsOperator name =
        SyntaxOperators
        |> ImArray.exists (fun op -> op.Text = name)

[<AutoOpen>]
module OlySyntaxTreeExtensions =

    type OlySyntaxTree with
    
        // TODO: Make this public but we need a new struct: OlyToken
        static member internal ParseSourceTextInternalTokens(sourceText: IOlySourceText, ct) =
            let lexer = Lexer.create ImArray.empty sourceText
            let tokens = ResizeArray()
            let mutable tokenToAdd = Lexer.scanToken lexer ct
            while tokenToAdd <> EndOfSource do
                tokens.Add(tokenToAdd)
                tokenToAdd <- Lexer.scanToken lexer ct
            tokens :> _ seq
    
        // TODO: Make this public but we need a new struct: OlyToken
        static member internal ParseInternalTokens(text: string, ct) =
            OlySyntaxTree.ParseSourceTextInternalTokens(OlySourceText.Create(text), ct)
    
        static member Parse(path: OlyPath, sourceText: IOlySourceText, ?parsingOptions: OlyParsingOptions) : OlySyntaxTree =
            let parsingOptions = defaultArg parsingOptions OlyParsingOptions.Default
            OlySyntaxTreeImplementation(path, (fun ct -> ct.ThrowIfCancellationRequested(); sourceText), 0UL, parsingOptions) :> OlySyntaxTree
    
        static member Parse(path: OlyPath, getSourceText: CancellationToken -> IOlySourceText, ?parsingOptions) =
            let parsingOptions = defaultArg parsingOptions OlyParsingOptions.Default
            OlySyntaxTreeImplementation(path, getSourceText, 0UL, parsingOptions) :> OlySyntaxTree
    
        static member Parse(path: OlyPath, text: string, ?parsingOptions) =
            let parsingOptions = defaultArg parsingOptions OlyParsingOptions.Default
            OlySyntaxTree.Parse(path, OlySourceText.Create(text), parsingOptions = parsingOptions)

    type OlySyntaxBindingDeclaration with
   
        member this.Identifier =
            match this with
            | OlySyntaxBindingDeclaration.Value(ident, _) ->
                ident
            | OlySyntaxBindingDeclaration.Function(funcName, _, _, _, _) ->
                funcName.Identifier
            | OlySyntaxBindingDeclaration.New(newToken, _) ->
                newToken
            | OlySyntaxBindingDeclaration.Getter(getToken, _)
            | OlySyntaxBindingDeclaration.Get(getToken) ->
                getToken
            | OlySyntaxBindingDeclaration.Setter(setToken, _)
            | OlySyntaxBindingDeclaration.Set(setToken) ->
                setToken
            | OlySyntaxBindingDeclaration.Error(errorToken) ->
                errorToken
            | _ ->
                failwith "Invalid syntax binding declaration."

        member this.HasReturnTypeAnnotation = this.Internal.HasReturnTypeAnnotation

        member this.IsExplicitNew = this.Internal.IsExplicitNew

        member this.IsExplicitGet = this.Internal.IsExplicitGet

        member this.IsExplicitSet = this.Internal.IsExplicitSet

        member this.IsExplicitFunction = this.Internal.IsExplicitFunction

    type OlySyntaxAttributes with

        member this.Values =
            match this with
            | OlySyntaxAttributes.Attributes(hashAttrList) ->
                hashAttrList.ChildrenOfType
                |> ImArray.choose (fun x ->
                    match x with
                    | OlySyntaxHashAttribute.HashAttribute(_, brackets) ->
                        // TODO: This is a little weird. We should add a OlySyntaxBrackets active pattern.
                        if brackets.Children.Length <> 3 then
                            None
                        else
                            match brackets.Children.[1] with
                            | :? OlySyntaxAttribute as attr -> Some attr
                            | _ -> None
                    | _ ->
                        None
                )
            | _ ->
                ImArray.empty

    type OlySyntaxConstraintClause with

        member this.TryConstraints =
            match this with
            | OlySyntaxConstraintClause.ConstraintClause(_, _, _, constrList) -> 
                constrList.ChildrenOfType
                |> ValueSome
            | _ -> 
                ValueNone

        member this.TryType =
            match this with
            | OlySyntaxConstraintClause.ConstraintClause(_, ty, _, _) -> ty |> ValueSome
            | _ -> ValueNone

    type OlySyntaxType with

        member this.ExplicitTypeArgumentCount =
            this.Internal.ExplicitTypeArgumentCount

    type OlySyntaxParameter with

        member this.IsMutable =
            this.Internal.IsMutable

    type OlySyntaxParameters with

        member this.Values =
            match this with
            | OlySyntaxParameters.Parameters(_, parList, _) ->
                parList.ChildrenOfType
            | _ ->
                ImArray.empty

    type OlySyntaxTypeArguments with

        member this.Values =
            match this with
            | OlySyntaxTypeArguments.TypeArguments(_, parList, _) ->
                parList.ChildrenOfType
            | _ ->
                ImArray.empty

        member this.Count =
            match this with
            | OlySyntaxTypeArguments.TypeArguments(_, tyParList, _) -> tyParList.ChildrenOfType.Length
            | _ -> 0

    type OlySyntaxBinding with

        member this.Declaration =
            match this with
            | OlySyntaxBinding.Implementation(bindingDecl, _, _)
            | OlySyntaxBinding.Signature(bindingDecl)
            | OlySyntaxBinding.Property(bindingDecl, _)
            | OlySyntaxBinding.PropertyWithDefault(bindingDecl, _, _, _)
            | OlySyntaxBinding.PatternWithGuard(bindingDecl, _) -> bindingDecl
            | _ -> 
                failwith "Invalid syntax binding."

    type OlySyntaxSeparatorList<'T when 'T :> OlySyntaxNode and 'T : not struct> with

        member this.TryFindIndexByPosition(position) =
            match this.InternalNode with
            | :? ISyntaxSeparatorList as syntax ->
                syntax.TryFindIndexByRelativePosition(position - this.FullTextSpan.Start)
            | _ ->
                -1

    type OlySyntaxNode with

        member this.TryName =
            match this with
            | :? OlySyntaxName as name -> Some name
            | :? OlySyntaxExpression as expr ->
                match expr with
                | OlySyntaxExpression.Name(name) -> Some name
                | OlySyntaxExpression.Call(expr, _) -> expr.TryName
                | _ -> None
            | :? OlySyntaxPattern as pat ->
                match pat with
                | OlySyntaxPattern.Name(name)
                | OlySyntaxPattern.Function(name, _, _, _) ->
                    Some name
                | _ ->
                    None
            | _ ->
                None

        member this.IsNewToken =
            match this.InternalNode with
            | :? SyntaxToken as token -> token.RawToken = Token.New
            | _ -> false

        member this.IsDummy: bool =
            if obj.ReferenceEquals(this.Tree.DummyNode, this) then true
            else
                match this.InternalNode with
                | :? SyntaxToken as token -> token.IsDummy
                | _ -> false

        member this.IsParenthesisExpression =
            match this.InternalNode with
            | :? SyntaxExpression as node ->
                match node with
                | SyntaxExpression.Parenthesis _ -> true
                | _ -> false
            | _ ->
                false

        member this.IsNameExpression =
            match this.InternalNode with
            | :? SyntaxExpression as node ->
                match node with
                | SyntaxExpression.Name _ -> true
                | _ -> false
            | _ ->
                false

        member this.IsName =
            match this.InternalNode with
            | :? SyntaxName -> true
            | _ -> false

        member this.IsParameter =
            match this.InternalNode with
            | :? SyntaxParameter -> true
            | _ -> false

        member this.IsParameters =
            match this.InternalNode with
            | :? SyntaxParameters -> true
            | _ -> false

        member this.IsTypeParameters =
            match this.InternalNode with
            | :? SyntaxTypeParameters -> true
            | _ -> false

        member this.IsArguments =
            match this.InternalNode with
            | :? SyntaxArguments -> true
            | _ -> false

        member this.IsTypeArguments =
            match this.InternalNode with
            | :? SyntaxTypeArguments -> true
            | _ -> false

        member this.IsConstraints =
            match this.InternalNode with
            | :? SyntaxConstraintClause -> true
            | _ -> false

        member this.IsAttributes =
            match this.InternalNode with
            | :? SyntaxAttributes -> true
            | _ -> false

        member this.IsAttribute =
            match this.InternalNode with
            | :? SyntaxAttribute -> true
            | _ -> false

        member this.IsCompilationUnit =
            match this.InternalNode with
            | :? SyntaxCompilationUnit -> true
            | _ -> false

        member this.IsTypeDeclarationKind =
            match this.InternalNode with
            | :? SyntaxTypeDeclarationKind -> true
            | _ -> false

        member this.IsLambdaKind =
            match this.InternalNode with
            | :? SyntaxLambdaKind -> true
            | _ -> false

        member this.IsLambdaExpression =
            match this.InternalNode with
            | :? SyntaxExpression as node ->
                match node with
                | SyntaxExpression.Lambda _ -> true
                | _ -> false
            | _ ->
                false

        member this.IsValueKind =
            match this.InternalNode with
            | :? SyntaxValueDeclarationKind -> true
            | _ -> false

        member this.IsSeparatorList =
            match this.InternalNode with
            | :? ISyntaxSeparatorList -> true
            | _ -> false

        member this.IsTypeDeclarationExpression =
            match this.InternalNode with
            | :? SyntaxExpression as node ->
                match node with
                | SyntaxExpression.TypeDeclaration _ -> true
                | _ -> false
            | _ ->
                false

        member this.IsValueDeclarationExpression =
            match this.InternalNode with
            | :? SyntaxExpression as node ->
                match node with
                | SyntaxExpression.ValueDeclaration _ -> true
                | _ -> false
            | _ ->
                false

        member this.IsBindingDeclaration =
            match this.InternalNode with
            | :? SyntaxBindingDeclaration -> true
            | _ -> false

        member this.IsError =
            this.InternalNode.IsError

        member this.IsFunctionBindingDeclaration =
            match this.InternalNode with
            | :? SyntaxBindingDeclaration as node ->
                match node with
                | SyntaxBindingDeclaration.Function _ -> true
                | _ -> false
            | _ -> 
                false

        member this.IsValueBindingDeclaration =
            match this.InternalNode with
            | :? SyntaxBindingDeclaration as node ->
                match node with
                | SyntaxBindingDeclaration.Value _ -> true
                | _ -> false
            | _ -> 
                false

        member this.IsType =
            match this.InternalNode with
            | :? SyntaxType -> true
            | _ -> false

        member this.IsTypeParameter =
            match this.InternalNode with
            | :? SyntaxType ->
                match this.Parent with
                | :? OlySyntaxParameters -> true
                | _ -> false
            | _ -> 
                false

        member this.IsTypeArgument =
            match this.InternalNode with
            | :? SyntaxType ->
                match this.Parent with
                | :? OlySyntaxArguments -> true
                | _ -> false
            | _ -> 
                false

        member this.IsLiteral =
            match this.InternalNode with
            | :? SyntaxLiteral -> true
            | _ -> false

        member this.IsSequentialExpression =
            match this.InternalNode with
            | :? SyntaxExpression as node ->
                match node with
                | SyntaxExpression.Sequential _ -> true
                | _ -> false
            | _ ->
                false

        member this.IsExpression =
            match this.InternalNode with
            | :? SyntaxExpression -> true
            | _ -> false

        /// If possible, returns the root name if there is any.
        /// Otherwise, returns the same given node.
        member this.GetRootNameIfPossible() =
            match this.Parent with
            | :? OlySyntaxName as name ->
                name.GetRootNameIfPossible()
            | _ ->
                this

        /// If possible, returns a plausible OlySyntaxName if it exists as one of the immediate children of the given node.
        /// Otherwise, returns the same given node.
        /// TODO: We could expand this a bit more.
        member this.GetChildNameIfPossible() : OlySyntaxNode =
            match this with
            | :? OlySyntaxParameter as par ->
                match par with
                | OlySyntaxParameter.Pattern(_, _, pat, _, _) -> 
                    match pat.TryName with
                    | Some name -> name
                    | _ -> this
                | _ -> 
                    this
            | _ ->
                this

        member this.IsDefinition =
            match this with
            | :? OlySyntaxToken as token when token.Internal.IsIdentifierOrOperator ->
                match this.Parent with
                | :? OlySyntaxTypeDeclarationName
                | :? OlySyntaxFunctionName
                | :? OlySyntaxBindingDeclaration -> true
                | _ -> false
            | _ ->
                false

        member this.TryGetParentExpression(ignoreSequentialExpr: bool, ct: CancellationToken) =
            ct.ThrowIfCancellationRequested()
            match this.Parent with
            | null -> None
            | parentNode ->
                match parentNode.InternalNode with
                | :? SyntaxExpression as x ->
                    match x with
                    | SyntaxExpression.Sequential _ when ignoreSequentialExpr ->
                        parentNode.TryGetParentExpression(ignoreSequentialExpr, ct)
                    | _ ->
                        Some(parentNode)
                | _ ->
                    parentNode.TryGetParentExpression(ignoreSequentialExpr, ct)

        member this.GetDescendantTokens(?skipTrivia: bool, ?ct: CancellationToken) : OlyToken imarray =
            let skipTrivia = defaultArg skipTrivia true
            let ct = defaultArg ct CancellationToken.None

            let res = ImArray.builder()
            let rec loop (node: OlySyntaxNode) =
                ct.ThrowIfCancellationRequested()
                match node with
                | :? OlySyntaxToken as node when not node.Internal.IsDummy ->
                    if not skipTrivia then
                        node.Children
                        |> ImArray.iter loop

                    res.Add(OlyToken(node))
                | _ ->
                    node.Children
                    |> ImArray.iter loop

            loop this
            res.ToImmutable()

        member this.GetDescendants(?predicate: OlySyntaxNode -> bool) =
            let predicate = defaultArg predicate (fun _ -> true)

            let res = ImArray.builder()
            let rec loop (node: OlySyntaxNode) =
                if node.IsToken then ()
                else
                    if predicate node then
                        res.Add(node)

                    node.Children
                    |> ImArray.iter loop

            this.Children
            |> ImArray.iter loop
            res.ToImmutable()

        member this.ChooseDescendants<'T>(chooser: OlySyntaxNode -> 'T option) =
            let res = ImArray.builder()
            let rec loop (node: OlySyntaxNode) =
                if node.IsToken then ()
                else
                    match chooser node with
                    | Some node ->
                        res.Add(node)
                    | _ ->
                        ()

                    node.Children
                    |> ImArray.iter loop

            this.Children
            |> ImArray.iter loop
            res.ToImmutable()

        member this.TryGetFirstToken(?skipTrivia: bool) =
            let skipTrivia = defaultArg skipTrivia false
            let rec loop (node: OlySyntaxNode) =
                match node with
                | :? OlySyntaxToken as node ->
                    if not node.Children.IsEmpty && not skipTrivia then
                        OlyToken(node.Children.[0] :?> OlySyntaxToken)
                        |> Some
                    else
                        OlyToken(node)
                        |> Some
                | _ ->
                    node.Children
                    |> ImArray.tryPick loop

            loop this

        member this.TryGetLastToken(?skipTrivia: bool) =
            let skipTrivia = defaultArg skipTrivia false
            let rec loop (node: OlySyntaxNode) =
                match node with
                | :? OlySyntaxToken as node ->
                    OlyToken(node)
                    |> Some
                | _ ->
                    // TODO: Optimize this.
                    node.Children
                    |> ImArray.rev
                    |> ImArray.tryPick loop

            loop this

        member this.KindName =
            // TODO: cheap but works for now, we should create a SyntaxKind public enum instead and use the names of that for this
            let reflectionTy = this.InternalNode.GetType()
            if reflectionTy.DeclaringType <> null && not (FSharp.Reflection.FSharpType.IsModule reflectionTy.DeclaringType) then
                reflectionTy.DeclaringType.Name.Replace("`1", "")
            else
                reflectionTy.Name.Replace("`1", "")

        member this.DebugName = 
            // TODO: cheap but works for now, we should create a SyntaxKind public enum instead and use the names of that for this
            this.InternalNode.GetType().Name.Replace("Syntax", "").Replace("`1", "")

        member this.IsToken = this.InternalNode.IsToken

        member this.IsEndOfSourceToken = 
            if this.InternalNode.IsToken then
                match this.InternalNode with
                | :? SyntaxToken as token ->
                    match token.RawToken with
                    | EndOfSource -> true
                    | _ -> false
                | _ ->
                    false
            else
                false

        member this.IsTrivia = 
            if this.InternalNode.IsToken then
                match this.InternalNode with
                | :? SyntaxToken as token -> token.RawToken.IsTrivia
                | _ -> false
            else
                false

        member this.TryGetToken() =
            match this with
            | :? OlySyntaxToken as node -> Some(OlyToken(node))
            | _ -> None

        member this.IsReturnTypeAnnotation =    
            match this.InternalNode with
            | :? SyntaxReturnTypeAnnotation -> true
            | _ -> false

        member this.IsParameterMissingTypeAnnotation =
            match this.InternalNode with
            | :? SyntaxParameter as node ->
                match node with
                | SyntaxParameter.Pattern(_, _, _, _, ty, _) ->
                    match ty with
                    | SyntaxType.Error _ -> true
                    | _ -> false
                | _ ->
                    true
            | _ ->
                false

        member this.IsInReturnTypeAnnotation =
            if this.IsReturnTypeAnnotation then true
            else
                match this.Parent with
                | null -> false
                | parent -> parent.IsInReturnTypeAnnotation

        member this.IsInPattern =
            match this.InternalNode with
            | :? SyntaxPattern -> true
            | _ ->
                match this.Parent with
                | null -> false
                | parent -> parent.IsInPattern

        member this.IsInMatchClause =
            match this.InternalNode with
            | :? SyntaxMatchClause -> true
            | :? SyntaxMatchGuard -> false           
            | _ ->
                match this.Parent with
                | null -> false
                | parent -> 
                    match parent.InternalNode with
                    | :? SyntaxMatchClause as matchClause ->
                        // If 'this' is the targetExpr, then we consider the expression not in the match clause.
                        match matchClause with
                        | SyntaxMatchClause.MatchClause(targetExpr=targetExpr) when obj.ReferenceEquals(this.InternalNode, targetExpr) ->
                            false
                        | _ ->
                            true
                    | _ ->
                        parent.IsInMatchClause

        member this.GetLocation() =
            OlySourceLocation(this)
                
        member this.GetTextRange(ct: CancellationToken) =
            this.Tree.GetSourceText(ct).GetTextRange(this.TextSpan)

        member this.GetFullTextRange(ct) =
            this.Tree.GetSourceText(ct).GetTextRange(this.FullTextSpan)

        member private this.GetCommentText(childIndex: int) =
            let childNode = this.Children[childIndex]
            match childNode.InternalNode with
            | :? SyntaxToken as childToken ->
                match childToken with
                | SyntaxToken.TokenWithTrivia(nextTrivia, triviaToken, _) when triviaToken.IsPossibleNewLine && nextTrivia.IsComment ->
                    nextTrivia.ValueText
                | _ ->
                    String.Empty
            | _ ->
                String.Empty

        member this.GetLeadingCommentText() =
            match this with
            | :? OlySyntaxToken as node ->
                if node.Children.IsEmpty then
                    String.Empty
                else
                    node.GetCommentText(node.Children.Length - 1)
            | _ ->
                if this.Children.IsEmpty then
                    String.Empty
                else
                    // TODO: Can we do this without allocating an option?
                    match this.Children |> ImArray.tryFind (fun x -> not x.Children.IsEmpty) with
                    | Some nextNode -> nextNode.GetLeadingCommentText()
                    | _ -> String.Empty

        /// TODO: This is a weird and doesn't match how we do GetLeadingCommentText
        member this.GetLeadingTrivia() =
            match this with
            | :? OlySyntaxToken as node ->
                if node.Children.IsEmpty then
                    ImArray.empty
                else
                    let childNode = node.Children[0]
                    let childNodes = childNode.GetLeadingTrivia()
                    childNodes.Add(childNode)
            | _ ->
                if this.Children.Length > 0 then
                    this.Children.[0].GetLeadingTrivia()
                else
                    ImArray.empty

        member this.BuildSource(?ct: CancellationToken) =
            let ct = defaultArg ct CancellationToken.None

            let builder = StringBuilder()

            let tokens = this.GetDescendantTokens(skipTrivia = false)
            tokens
            |> ImArray.iter (fun x ->
                ct.ThrowIfCancellationRequested()
                builder.Append(x.Text) |> ignore
            )
            
            builder.ToString()

        member this.TryFindFirstIdentifierOrLiteral() =
            let rec loop (node: OlySyntaxNode) =
                match node with
                | :? OlySyntaxToken as node ->
                    if node.Internal.IsIdentifierOrOperator || node.Internal.IsLiteral then
                        Some(OlyToken(node))
                    else
                        None
                | _ ->
                    node.Children
                    |> ImArray.tryPick loop

            loop this

        member this.FindTokens(position: int, ?skipTrivia: bool, ?ct: CancellationToken) : OlyToken imarray =
            let skipTrivia = defaultArg skipTrivia true
            let ct = defaultArg ct CancellationToken.None

            let possibleTokens = imarray.CreateBuilder()

            let rec loop (node: OlySyntaxNode) =
                ct.ThrowIfCancellationRequested()
                if node.FullTextSpan.IntersectsWith(position)then
                    match node with
                    | :? OlySyntaxToken as node ->
                        if not skipTrivia then
                            node.Children
                            |> ImArray.iter loop

                        if node.TextSpan.IntersectsWith(position) then
                            possibleTokens.Add(OlyToken(node))
                    | _ ->
                        node.Children
                        |> ImArray.iter loop

            loop this
            possibleTokens.ToImmutable()

        member this.FindTokens(textSpan: OlyTextSpan, ?skipTrivia: bool, ?ct: CancellationToken) : OlyToken imarray =
            let skipTrivia = defaultArg skipTrivia true
            let ct = defaultArg ct CancellationToken.None

            let possibleTokens = imarray.CreateBuilder()

            let rec loop (node: OlySyntaxNode) =
                ct.ThrowIfCancellationRequested()
                let canVisit =
                    if not skipTrivia && node.IsTrivia then
                        node.TextSpan.Contains(textSpan)
                    else
                        node.FullTextSpan.Contains(textSpan)

                if canVisit then
                    match node with
                    | :? OlySyntaxToken as node ->
                        if not skipTrivia && node.IsTrivia then
                            possibleTokens.Add(OlyToken(node))
                        else
                            if not skipTrivia then
                                node.Children
                                |> ImArray.iter loop

                            if node.TextSpan.Contains(textSpan) then
                                possibleTokens.Add(OlyToken(node))
                    | _ ->
                        node.Children
                        |> ImArray.iter loop

            loop this
            possibleTokens.ToImmutable()

        member this.FindTokens(textPosition: OlyTextPosition, ?skipTrivia: bool, ?ct: CancellationToken) : OlyToken imarray =
            let skipTrivia = defaultArg skipTrivia true
            let ct = defaultArg ct CancellationToken.None
            let sourceText = this.Tree.GetSourceText(ct)
            match sourceText.TryGetPosition(textPosition) with
            | None -> ImArray.empty
            | Some position -> this.FindTokens(position, skipTrivia, ct)

        member this.FindTokens(textRange: OlyTextRange, ?skipTrivia: bool, ?ct: CancellationToken) : OlyToken imarray =
            let skipTrivia = defaultArg skipTrivia true
            let ct = defaultArg ct CancellationToken.None
            let sourceText = this.Tree.GetSourceText(ct)
            match sourceText.TryGetTextSpan(textRange) with
            | None -> ImArray.empty
            | Some textSpan -> this.FindTokens(textSpan, skipTrivia, ct)

        member this.BestSyntaxForReporting =
            let rec find (syntax: OlySyntaxNode) : OlySyntaxNode =
                // TODO: Add more cases.
                match syntax with
                | :? OlySyntaxExpression as syntaxExpr ->
                    match syntaxExpr with
                    | OlySyntaxExpression.TypeDeclaration(_, _, _, syntaxTyDeclName, _, _, _, _) ->
                        syntaxTyDeclName
                    | OlySyntaxExpression.ValueDeclaration(_, _, _, _, _, syntaxBinding) ->
                        match syntaxBinding.TryGetBindingDeclaration() with
                        | ValueSome(syntaxBindingDecl) ->
                            match syntaxBindingDecl with
                            | OlySyntaxBindingDeclaration.Function(syntaxFuncName, _, _, _, _) ->
                                syntaxFuncName
                            | _ ->
                                // TODO: Add more cases.
                                syntaxBindingDecl
                        | _ ->
                            syntaxBinding
                    | OlySyntaxExpression.Sequential(syntaxExpr1, _) ->
                        find syntaxExpr1
                    | _ ->
                        syntaxExpr
                | syntaxNode ->
                    syntaxNode
            find this

    type OlySyntaxToken with

        member this.ValueText =
            this.Internal.ValueText

        member this.IsNew =
            this.Internal.RawToken = Token.New

        member this.IsIs =
            this.Internal.RawToken = Token.Is

        member this.IsTrueToken =
            this.Internal.RawToken = Token.True

        member this.IsFalseToken =
            this.Internal.RawToken = Token.False

        member this.IsKeyword =
            this.Internal.RawToken.IsKeyword

    type OlySyntaxName with

        member this.EnclosingPath = this.Internal.Path

        member this.NameText = this.Internal.NameText

        member this.LastName =
            match this with
            | OlySyntaxName.Identifier _
            | OlySyntaxName.Parenthesis _ -> this
            | OlySyntaxName.Generic(name, _) -> name.LastName
            | OlySyntaxName.Qualified(_, _, tail) -> tail.LastName

            | _ -> 
                failwith "Invalid syntax name."

        member this.LastIdentifier =    
            match this.LastName with
            | OlySyntaxName.Identifier(token)
            | OlySyntaxName.Parenthesis(_, token, _) -> token
            | _ -> failwith "should not happen"

        member this.AllNames =
            match this with
            | OlySyntaxName.Qualified(head, _, tail) ->
                head.AllNames @ tail.AllNames
            | OlySyntaxName.Identifier _ 
            | OlySyntaxName.Generic _
            | OlySyntaxName.Parenthesis _ ->
                [this]

            | _ ->
                failwith "Invalid syntax name."

        member this.EnclosingNames =
            match this with
            | OlySyntaxName.Qualified(head, _, tail) ->
                head.AllNames @ tail.EnclosingNames
            | OlySyntaxName.Identifier _ 
            | OlySyntaxName.Generic _
            | OlySyntaxName.Parenthesis _ ->
                []

            | _ ->
                failwith "Invalid syntax name."

        member this.GetTopMostName() =
            match this.Parent with
            | null -> this
            | parent ->
                match parent with
                | :? OlySyntaxName as parentName -> 
                    parentName.GetTopMostName()
                | _ ->
                    this

    type OlySyntaxTypeParameters with

        member this.Count =
            match this with
            | OlySyntaxTypeParameters.TypeParameters(_, tyParList, _) 
            | OlySyntaxTypeParameters.RequireTypeParameters(_, _, tyParList, _) -> tyParList.ChildrenOfType.Length
            | _ -> 0

        member this.Values =
            match this with
            | OlySyntaxTypeParameters.TypeParameters(_, tyParList, _)
            | OlySyntaxTypeParameters.RequireTypeParameters(_, _, tyParList, _) ->
                tyParList.ChildrenOfType
            | _ ->
                ImArray.empty

        member this.HasRequire =
            match this with
            | OlySyntaxTypeParameters.RequireTypeParameters _ -> true
            | _ -> false

    type OlySyntaxFunctionName with

        member this.Identifier =
            match this with
            | OlySyntaxFunctionName.Identifier(ident) -> ident
            | OlySyntaxFunctionName.Parenthesis(_, operator, _)-> operator
            | _ -> failwith "Invalid identifier"

    type private FakeUnit = FakeUnit

    type OlySyntaxTypeDeclarationBody with

        member this.GetNestedTypeDeclarationIdentifiers() =
            let builder = ImArray.builder()
            let rec f (expr: OlySyntaxExpression) cont : FakeUnit =
                match expr with
                | OlySyntaxExpression.TypeDeclaration(_, _, _, syntaxTyDefName, _, _, _, _) ->
                    builder.Add(syntaxTyDefName.Identifier)
                    cont(FakeUnit)
                | OlySyntaxExpression.Sequential(expr1, expr2) ->
                    f expr1 (fun FakeUnit ->
                        f expr2 cont
                    )
                | _ ->
                    cont(FakeUnit)

            this.Children
            |> ImArray.iter (function
                | :? OlySyntaxExpression as expr ->
                    f expr id |> ignore
                | _ ->
                    ()
            )

            builder.ToImmutable()

        member this.GetMemberDeclarations() =
            let builder = ImArray.builder()
            let rec f (expr: OlySyntaxExpression) cont : FakeUnit =
                match expr with
                | OlySyntaxExpression.ValueDeclaration(syntaxAttrs, _, _, _, _, syntaxBinding) ->
                    let rec addBinding syntaxBinding =
                        match syntaxBinding with
                        | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _)
                        | OlySyntaxBinding.Signature(syntaxBindingDecl)
                        | OlySyntaxBinding.Property(syntaxBindingDecl, _)
                        | OlySyntaxBinding.PropertyWithDefault(syntaxBindingDecl, _, _, _)
                        | OlySyntaxBinding.PatternWithGuard(syntaxBindingDecl, _) ->
                            builder.Add(syntaxAttrs, syntaxBindingDecl)
                        | _ ->
                            ()
                    addBinding syntaxBinding
                    cont(FakeUnit)
                | OlySyntaxExpression.Sequential(expr1, expr2) ->
                    f expr1 (fun FakeUnit ->
                        f expr2 cont
                    )
                | _ ->
                    cont(FakeUnit)

            this.Children
            |> ImArray.iter (function
                | :? OlySyntaxExpression as expr ->
                    f expr id |> ignore
                | _ ->
                    ()
            )

            builder.ToImmutable()

    type OlySyntaxTypeDeclarationName with

        member this.Identifier =
            match this with
            | OlySyntaxTypeDeclarationName.Identifier(ident) -> ident
            | OlySyntaxTypeDeclarationName.Parenthesis(_, operator, _) -> operator
            | _ -> failwith "Invalid identifier."

    type OlySyntaxName with

        member this.GetAllTypeArguments(): OlySyntaxType imarray =
            match this with
            | OlySyntaxName.Generic(name, tyArgs) ->
                name.GetAllTypeArguments().AddRange(tyArgs.Values)
            | _ ->
                ImArray.empty

    type OlySyntaxExpression with

        member this.GetAllTypeArguments() =
            match this with
            | OlySyntaxExpression.Call(receiver, _) ->
                receiver.GetAllTypeArguments()
            | OlySyntaxExpression.Name(name) ->
                name.GetAllTypeArguments()
            | _ ->
                ImArray.empty       
                
        member this.FlattenSequentials(): OlySyntaxExpression imarray =
            let builder = ImArray.builder()

            let rec f (expr: OlySyntaxExpression) =
                match expr with
                | OlySyntaxExpression.Sequential(expr1, expr2) ->
                    f expr1
                    f expr2
                | _ ->
                    builder.Add(expr)

            f this

            builder.ToImmutable()

    type OlySyntaxNode with

        member this.TryGetBindingDeclaration() =
            match this with
            | :? OlySyntaxExpression as syntaxExpr ->
                match syntaxExpr with
                | OlySyntaxExpression.Sequential(syntaxExpr, _) ->
                    match syntaxExpr with
                    | OlySyntaxExpression.ValueDeclaration(_, _, _, _, _, syntaxBinding) ->
                        let rec tryGet syntaxBinding =
                            match syntaxBinding with
                            | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _)
                            | OlySyntaxBinding.Signature(syntaxBindingDecl)
                            | OlySyntaxBinding.Property(syntaxBindingDecl, _)
                            | OlySyntaxBinding.PropertyWithDefault(syntaxBindingDecl, _, _, _)
                            | OlySyntaxBinding.PatternWithGuard(syntaxBindingDecl, _) ->
                                ValueSome syntaxBindingDecl
                            | _ ->
                                ValueNone
                        tryGet syntaxBinding
                    | _ ->
                        ValueNone

                | OlySyntaxExpression.ValueDeclaration(_, _, _, _, _, syntaxBinding) ->
                    let rec tryGet syntaxBinding =
                        match syntaxBinding with
                        | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _)
                        | OlySyntaxBinding.Signature(syntaxBindingDecl)
                        | OlySyntaxBinding.Property(syntaxBindingDecl, _) 
                        | OlySyntaxBinding.PropertyWithDefault(syntaxBindingDecl, _, _, _)
                        | OlySyntaxBinding.PatternWithGuard(syntaxBindingDecl, _) ->
                            ValueSome syntaxBindingDecl
                        | _ ->
                            ValueNone
                    tryGet syntaxBinding
                | _ ->
                    ValueNone

            | :? OlySyntaxBinding as syntaxBinding ->
                let rec tryGet syntaxBinding =
                    match syntaxBinding with
                    | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _)
                    | OlySyntaxBinding.Signature(syntaxBindingDecl)
                    | OlySyntaxBinding.Property(syntaxBindingDecl, _) 
                    | OlySyntaxBinding.PropertyWithDefault(syntaxBindingDecl, _, _, _)
                    | OlySyntaxBinding.PatternWithGuard(syntaxBindingDecl, _) ->
                        ValueSome syntaxBindingDecl
                    | _ ->
                        ValueNone
                tryGet syntaxBinding
            | _ ->
                ValueNone

        member this.GetSuitableSyntaxForTypeError() : OlySyntaxNode =
            match this with
            | :? OlySyntaxExpression as expr ->
                match expr with
                | OlySyntaxExpression.If(_, _, _, _, expr, _) ->
                    expr.GetSuitableSyntaxForTypeError()
                | OlySyntaxExpression.Sequential(_, expr) ->
                    expr.GetSuitableSyntaxForTypeError()
                | OlySyntaxExpression.Match(_, _, _, _, matchClauseList) ->
                    let matchClauses = matchClauseList.ChildrenOfType
                    if matchClauses.IsEmpty then
                        this
                    else
                        match matchClauses.[0] with
                        | OlySyntaxMatchClause.MatchClause(_, _, _, _, targetExpr) ->
                            targetExpr.GetSuitableSyntaxForTypeError()
                        | _ ->
                            this
                | _ ->
                    this
            | _ ->
                this

    type OlySyntaxTree with

        member this.TryFindNode(position: int, ct) =
            let rec loop (node: OlySyntaxNode) =
                if node.FullTextSpan.Contains(position) then
                    let innerNode =
                        node.Children
                        |> ImArray.tryPick (loop)

                    if innerNode.IsSome then
                        innerNode
                    else
                        Some node
                else
                    None

            let root = this.GetRoot(CancellationToken.None)
            loop root

        member this.TryFindNode(textRange: OlyTextRange, ct) =
            match this.GetSourceText(ct).TryGetTextSpan(textRange) with
            | None -> None
            | Some textSpan ->
                
                let rec loop (textSpan: OlyTextSpan) (node: OlySyntaxNode) =
                    if node.FullTextSpan.Contains(textSpan) then
                        let innerNode =
                            node.Children
                            |> ImArray.tryPick (loop textSpan)

                        if innerNode.IsSome then
                            innerNode
                        else
                            Some node
                    else
                        None

                let root = this.GetRoot(CancellationToken.None)

                let fixedTextSpan =
                    // For the root node only, shave off the ending of the original span so that is does not include trailing trivia.
                    // We do this to potentially get a more specific node rather than the root node.
                    if root.Children.Length > 1 && root.Children[root.Children.Length - 1].IsEndOfSourceToken then
                        let nodeBeforeEndOfSource = root.Children[root.Children.Length - 2]
                        let possibleEnd = nodeBeforeEndOfSource.FullTextSpan.End

                        if textSpan.End >= possibleEnd then
                            OlyTextSpan.CreateWithEnd(textSpan.Start, nodeBeforeEndOfSource.FullTextSpan.End)
                        else
                            textSpan
                    else
                        textSpan

                loop fixedTextSpan root

        member this.GetOpenDeclarationNames(ct: CancellationToken) =
            let builder = ImArray.builder()

            let rec loop (expr: OlySyntaxExpression) =
                match expr with
                | OlySyntaxExpression.OpenDeclaration(_, name)
                | OlySyntaxExpression.OpenStaticDeclaration(_, _, name)
                | OlySyntaxExpression.OpenExtensionDeclaration(_, _, name) ->
                    builder.Add(name)
                    true
                | OlySyntaxExpression.Sequential(expr1, expr2) ->
                    if loop expr1 then
                        loop expr2
                    else
                        false
                | _ ->
                    false

            match this.GetRoot(ct) with
            | (:? OlySyntaxCompilationUnit as syntaxCompUnit) ->
                match syntaxCompUnit with
                | OlySyntaxCompilationUnit.Namespace(_, _, OlySyntaxTypeDeclarationBody.Body(_, _, _, expr), _) ->
                    loop expr |> ignore
                | OlySyntaxCompilationUnit.Module(_, _, _, _, _, OlySyntaxTypeDeclarationBody.Body(_, _, _, expr), _) ->
                    loop expr |> ignore
                | OlySyntaxCompilationUnit.AnonymousModule(OlySyntaxTypeDeclarationBody.Body(_, _, _, expr), _) ->
                    loop expr |> ignore
                | _ ->
                    ()
            | _ ->
                ()

            builder.ToImmutable()

[<AutoOpen>]
module OlySyntaxDiffs =

    type OlySyntaxTree with
        
        static member AreOpenDeclarationsEqual(syntaxTree1: OlySyntaxTree, syntaxTree2: OlySyntaxTree, ct: CancellationToken) =
            ct.ThrowIfCancellationRequested()

            if obj.ReferenceEquals(syntaxTree1, syntaxTree2) then
                true
            else

            let checkBlittableOptional (syntaxBlittableOptional1: OlySyntaxBlittableOptional) (syntaxBlittableOptional2: OlySyntaxBlittableOptional) =
                match syntaxBlittableOptional1, syntaxBlittableOptional2 with
                | OlySyntaxBlittableOptional.None _, OlySyntaxBlittableOptional.None _ -> true
                | OlySyntaxBlittableOptional.Some(syntaxBlittable1), OlySyntaxBlittableOptional.Some(syntaxBlittable2) ->
                    match syntaxBlittable1, syntaxBlittable2 with
                    | OlySyntaxBlittable.Blittable(syntaxToken1), OlySyntaxBlittable.Blittable(syntaxToken2) ->
                        syntaxToken1.GetText(ct).ContentEquals(syntaxToken2.GetText(ct))
                    | _ ->
                        false
                | _ ->
                    false

            let rec checkType (syntaxTy1: OlySyntaxType) (syntaxTy2: OlySyntaxType) =
                match syntaxTy1, syntaxTy2 with
                | OlySyntaxType.Error _, OlySyntaxType.Error _ -> true
                | OlySyntaxType.Name(syntaxName1), OlySyntaxType.Name(syntaxName2) ->
                    checkName syntaxName1 syntaxName2
                | OlySyntaxType.Array(syntaxElementTy1, syntaxBrackets1), OlySyntaxType.Array(syntaxElementTy2, syntaxBrackets2) ->
                    checkType syntaxElementTy1 syntaxElementTy2 &&
                    (
                        let syntaxTokens1 = syntaxBrackets1.Element.ChildrenOfType
                        let syntaxTokens2 = syntaxBrackets2.Element.ChildrenOfType
                        (syntaxTokens1, syntaxTokens2)
                        ||> ImArray.tryForall2 (fun syntaxToken1 syntaxToken2 ->
                            syntaxToken1.GetText(ct).ContentEquals(syntaxToken2.GetText(ct))
                        )
                    )
                | OlySyntaxType.MutableArray(_, syntaxElementTy1, syntaxBrackets1), OlySyntaxType.Array(syntaxElementTy2, syntaxBrackets2) ->
                    checkType syntaxElementTy1 syntaxElementTy2 &&
                    (
                        let syntaxTokens1 = syntaxBrackets1.Element.ChildrenOfType
                        let syntaxTokens2 = syntaxBrackets2.Element.ChildrenOfType
                        (syntaxTokens1, syntaxTokens2)
                        ||> ImArray.tryForall2 (fun syntaxToken1 syntaxToken2 ->
                            syntaxToken1.GetText(ct).ContentEquals(syntaxToken2.GetText(ct))
                        )
                    )
                | OlySyntaxType.Function(syntaxInputTy1, _, syntaxOutputTy1), OlySyntaxType.Function(syntaxInputTy2, _, syntaxOutputTy2) ->
                    checkType syntaxInputTy1 syntaxInputTy2 &&
                    checkType syntaxOutputTy1 syntaxOutputTy2
                | OlySyntaxType.FunctionPtr(syntaxStaticToken1, syntaxBlittableOptional1, syntaxInputTy1, _, syntaxOutputTy1), OlySyntaxType.FunctionPtr(syntaxStaticToken2, syntaxBlittableOptional2, syntaxInputTy2, _, syntaxOutputTy2) ->
                    syntaxStaticToken1.GetText(ct).ContentEquals(syntaxStaticToken2.GetText(ct)) &&
                    checkBlittableOptional syntaxBlittableOptional1 syntaxBlittableOptional2 &&
                    checkType syntaxInputTy1 syntaxInputTy2 &&
                    checkType syntaxOutputTy1 syntaxOutputTy2
                | OlySyntaxType.Tuple(_, syntaxTupleElementList1, _), OlySyntaxType.Tuple(_, syntaxTupleElementList2, _) ->
                    if syntaxTupleElementList1.Children.Length = syntaxTupleElementList2.Children.Length then
                        (syntaxTupleElementList1.ChildrenOfType, syntaxTupleElementList2.ChildrenOfType)
                        ||> ImArray.forall2 (fun syntaxTupleElement1 syntaxTupleElement2 ->
                            match syntaxTupleElement1, syntaxTupleElement2 with
                            | OlySyntaxTupleElement.Type(syntaxTy1), OlySyntaxTupleElement.Type(syntaxTy2) ->
                                checkType syntaxTy1 syntaxTy2
                            | OlySyntaxTupleElement.IdentifierWithTypeAnnotation(syntaxIdent1, _, syntaxTy1),
                              OlySyntaxTupleElement.IdentifierWithTypeAnnotation(syntaxIdent2, _, syntaxTy2) ->
                                syntaxIdent1.GetText(ct).ContentEquals(syntaxIdent2.GetText(ct)) &&
                                checkType syntaxTy1 syntaxTy2
                            | OlySyntaxTupleElement.Error(syntaxToken1), OlySyntaxTupleElement.Error(syntaxToken2) ->
                                syntaxToken1.GetText(ct).ContentEquals(syntaxToken2.GetText(ct))
                            | _ ->
                                false
                        )
                    else
                        false
                | OlySyntaxType.WildCard _, OlySyntaxType.WildCard _ ->
                    true
                | OlySyntaxType.Postfix(syntaxElementTy1, syntaxToken1), OlySyntaxType.Postfix(syntaxElementTy2, syntaxToken2) ->
                    checkType syntaxElementTy1 syntaxElementTy2 &&
                    syntaxToken1.GetText(ct).ContentEquals(syntaxToken2.GetText(ct))
                // REVIEW: We are just checking open declarations and currently it is not possible to use shapes as type arguments
                //         or opening a shape, so we can just return false here. However, in the future we may want to check for this
                //         in other cases that are not for checking to see if open declarations are the same.
                | OlySyntaxType.Shape _, OlySyntaxType.Shape _ ->
                    false
                | _ ->
                    false

            and checkTypeArguments (oldSyntaxTyArgs: OlySyntaxTypeArguments) (newSyntaxTyArgs: OlySyntaxTypeArguments) =
                match oldSyntaxTyArgs, newSyntaxTyArgs with
                | OlySyntaxTypeArguments.Empty _, OlySyntaxTypeArguments.Empty _ ->
                    true
                | OlySyntaxTypeArguments.TypeArguments(_, syntaxTyArgList1, _), OlySyntaxTypeArguments.TypeArguments(_, syntaxTyArgList2, _) ->
                    let syntaxTyArgs1 = syntaxTyArgList1.ChildrenOfType
                    let syntaxTyArgs2 = syntaxTyArgList2.ChildrenOfType
                    
                    (syntaxTyArgs1, syntaxTyArgs2)
                    ||> ImArray.tryForall2 (fun syntaxTyArg1 syntaxTyArg2 ->
                        checkType syntaxTyArg1 syntaxTyArg2
                    )
                | _ ->
                    false

            and checkName (oldSyntaxName: OlySyntaxName) (newSyntaxName: OlySyntaxName) =
                match oldSyntaxName, newSyntaxName with
                | OlySyntaxName.Parenthesis(_, syntaxIdentOrOperatorToken1, _), OlySyntaxName.Parenthesis(_, syntaxIdentOrOperatorToken2, _) ->
                    syntaxIdentOrOperatorToken1.GetText(ct).ContentEquals(syntaxIdentOrOperatorToken2.GetText(ct))
                | OlySyntaxName.Identifier(syntaxToken1), OlySyntaxName.Identifier(syntaxToken2) ->
                    syntaxToken1.GetText(ct).ContentEquals(syntaxToken2.GetText(ct))
                | OlySyntaxName.Generic(syntaxName1, syntaxTyArgs1), OlySyntaxName.Generic(syntaxName2, syntaxTyArgs2) ->
                    checkName syntaxName1 syntaxName2 &&
                    checkTypeArguments syntaxTyArgs1 syntaxTyArgs2
                | OlySyntaxName.Qualified(syntaxName11, _, syntaxName12), OlySyntaxName.Qualified(syntaxName21, _, syntaxName22) ->
                    checkName syntaxName11 syntaxName21 &&
                    checkName syntaxName12 syntaxName22
                | _ ->
                    false
                
            let rec checkExpr (oldSyntaxExpr: OlySyntaxExpression) (newSyntaxExpr: OlySyntaxExpression) =
                match oldSyntaxExpr, newSyntaxExpr with
                | OlySyntaxExpression.None _, OlySyntaxExpression.None _ ->
                    true
                | OlySyntaxExpression.OpenDeclaration(_, syntaxName1), OlySyntaxExpression.OpenDeclaration(_, syntaxName2) ->
                    checkName syntaxName1 syntaxName2
                | OlySyntaxExpression.OpenStaticDeclaration(_, _, syntaxName1), OlySyntaxExpression.OpenStaticDeclaration(_, _, syntaxName2) ->
                    checkName syntaxName1 syntaxName2
                | OlySyntaxExpression.OpenExtensionDeclaration(_, _, syntaxName1), OlySyntaxExpression.OpenExtensionDeclaration(_, _, syntaxName2) ->
                    checkName syntaxName1 syntaxName2
                | OlySyntaxExpression.Sequential(syntaxExpr11, syntaxExpr12), OlySyntaxExpression.Sequential(syntaxExpr21, syntaxExpr22) ->
                    checkExpr syntaxExpr11 syntaxExpr21 &&
                    checkExpr syntaxExpr12 syntaxExpr22

                | OlySyntaxExpression.OpenDeclaration _, _ 
                | OlySyntaxExpression.OpenStaticDeclaration _, _ 
                | OlySyntaxExpression.OpenExtensionDeclaration _, _ ->
                    false

                | _, OlySyntaxExpression.OpenDeclaration _ 
                | _, OlySyntaxExpression.OpenStaticDeclaration _ 
                | _, OlySyntaxExpression.OpenExtensionDeclaration _ ->
                    false

                | _ ->
                    true // We hit an expression that is not open-decl related, means that they are equal.

            let rec checkTyDeclBody (syntaxTyDeclBody1: OlySyntaxTypeDeclarationBody) (syntaxTyDeclBody2: OlySyntaxTypeDeclarationBody) =
                match syntaxTyDeclBody1, syntaxTyDeclBody2 with
                | OlySyntaxTypeDeclarationBody.Body(_, _, _, syntaxExpr1), OlySyntaxTypeDeclarationBody.Body(_, _, _, syntaxExpr2) ->
                    checkExpr syntaxExpr1 syntaxExpr2
                | _ ->
                    false

            match syntaxTree1.GetRoot(ct), syntaxTree2.GetRoot(ct) with
            | (:? OlySyntaxCompilationUnit as syntaxCompUnit1), (:? OlySyntaxCompilationUnit as syntaxCompUnit2) ->
                match syntaxCompUnit1, syntaxCompUnit2 with
                | OlySyntaxCompilationUnit.Namespace(_, syntaxName1, syntaxTyDeclBody1, _), OlySyntaxCompilationUnit.Namespace(_, syntaxName2, syntaxTyDeclBody2, _) ->
                    checkName syntaxName1 syntaxName2 &&
                    checkTyDeclBody syntaxTyDeclBody1 syntaxTyDeclBody2
                | OlySyntaxCompilationUnit.Module(syntaxAttrs1, syntaxAccessor1, _, syntaxName1, syntaxConstrClauseList1, syntaxTyDeclBody1, _), OlySyntaxCompilationUnit.Module(syntaxAttrs2, syntaxAccessor2, _, syntaxName2, syntaxConstrClauseList2, syntaxTyDeclBody2, _) ->
                    checkName syntaxName1 syntaxName2 &&
                    checkTyDeclBody syntaxTyDeclBody1 syntaxTyDeclBody2
                | OlySyntaxCompilationUnit.AnonymousModule(syntaxTyDeclBody1, _), OlySyntaxCompilationUnit.AnonymousModule(syntaxTyDeclBody2, _) ->
                    checkTyDeclBody syntaxTyDeclBody1 syntaxTyDeclBody2
                | _ ->
                    false
            | _ ->
                false