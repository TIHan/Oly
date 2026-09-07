[<AutoOpen>]
module internal rec Oly.Compiler.Syntax.Internal.Extensions

type SyntaxToken with

    member this.ValueText =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.ValueText

    member this.Text =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.Text

    member this.Width =
        match this with
        | SyntaxToken.Token(token=token)
        | SyntaxToken.TokenWithTrivia(token=token) -> token.Width

    member this.LeadingTriviaWidth =
        match this with
        | SyntaxToken.Token _ -> 0
        | SyntaxToken.TokenWithTrivia(_, t, fullWidth) ->
            fullWidth - t.Width

    member this.FullWidth =
        match this with
        | SyntaxToken.Token(token) -> token.Width
        | SyntaxToken.TokenWithTrivia(_, t, fullWidth) ->
            fullWidth

    member this.RawToken: Token =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token

    member this.IsDummy =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token = Token.Dummy

    member this.IsIdentifier =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.IsIdentifierToken

    member this.IsOperator =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.IsOperator

    member this.IsKeyword =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.IsKeyword

    member this.IsLiteral =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.IsLiteral

    member this.IsUtf16Literal =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.TryStringLiteralText.IsSome

    member this.IsIdentifierOrOperator =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.IsIdentifierToken || token.IsOperator

    member this.IsIdentifierOrKeyword =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.IsIdentifierToken || token.IsKeyword

    member this.IsComment =
        match this with
        | SyntaxToken.Token(token=token) 
        | SyntaxToken.TokenWithTrivia(token=token) -> token.IsComment

type SyntaxName with

    member this.Path =
        match this with
        | SyntaxName.Qualified(head, _, rest, _) ->
            head.PathText :: rest.Path
        | SyntaxName.Identifier _
        | SyntaxName.Generic _
        | SyntaxName.Parenthesis _ ->
            []

    member this.PathText =
        match this with
        | SyntaxName.Parenthesis(_, ident, _, _) ->  "(" + ident.ValueText + ")"
        | SyntaxName.Qualified(head, _, rest, _) -> head.PathText + "." + rest.PathText
        | SyntaxName.Generic(name, _, _) -> name.PathText
        | SyntaxName.Identifier(ident) -> ident.Text

    member this.NameText =
        match this with
        | SyntaxName.Identifier(ident) -> ident.Text
        | SyntaxName.Generic(name, _, _) -> name.NameText
        | SyntaxName.Qualified(_, _, rest, _) -> rest.NameText
        | SyntaxName.Parenthesis(_, ident, _, _) -> ident.ValueText

    member this.FirstIdentifier =
        match this with
        | SyntaxName.Parenthesis(_, ident, _, _) -> ident
        | SyntaxName.Qualified(head, _, _, _) -> head.FirstIdentifier
        | SyntaxName.Generic(name, _, _) -> name.FirstIdentifier
        | SyntaxName.Identifier(ident) -> ident

    member this.LastIdentifier =
        match this with
        | SyntaxName.Parenthesis(_, ident, _, _) -> ident
        | SyntaxName.Qualified(_, _, tail, _) -> tail.LastIdentifier
        | SyntaxName.Generic(name, _, _) -> name.LastIdentifier
        | SyntaxName.Identifier(ident) -> ident

    member this.LastName =
        match this with
        | SyntaxName.Identifier _
        | SyntaxName.Parenthesis _ -> this
        | SyntaxName.Generic(name, _, _) -> name.LastName
        | SyntaxName.Qualified(_, _, tail, _) -> tail.LastName

    member this.TypeArguments =
        match this with
        | SyntaxName.Identifier _
        | SyntaxName.Parenthesis _ -> Seq.empty
        | SyntaxName.Qualified(head, _, tail, _) -> 
            Seq.append head.TypeArguments tail.TypeArguments
        | SyntaxName.Generic(name, tyArgs, _) -> 
            Seq.append name.TypeArguments tyArgs.Values

    member this.LastGenericOrNonGenericName =
        match this with
        | SyntaxName.Identifier _
        | SyntaxName.Parenthesis _
        | SyntaxName.Generic _ -> this
        | SyntaxName.Qualified(_, _, tail, _) -> tail.LastGenericOrNonGenericName

type SyntaxType with

    member this.ExplicitTypeArgumentCount =
        match this with
        | SyntaxType.Name(SyntaxName.Generic(tyArgs=tyArgs)) -> tyArgs.Count
        | _ -> 0

type SyntaxParameter with

    member this.IsMutable =
        match this with
        | SyntaxParameter.Pattern(mutability=SyntaxMutability.Mutable _) -> true
        | _ -> false

type SyntaxTypeArguments with

    member this.Count =
        match this with
        | SyntaxTypeArguments.Empty _ -> 0
        | SyntaxTypeArguments.TypeArguments(_, tyArgList, _, _) -> tyArgList.Count

    member this.Values =
        match this with
        | SyntaxTypeArguments.Empty _ -> Seq.empty
        | SyntaxTypeArguments.TypeArguments(_, tyArgList, _, _) -> tyArgList.Values

type SyntaxBindingDeclaration with

    member this.Identifier =
        match this with
        | SyntaxBindingDeclaration.Function(funcName=funcName) ->
            match funcName with
            | SyntaxFunctionName.Identifier(ident) ->
                ident
            | SyntaxFunctionName.Parenthesis(_, ident, _, _) ->
                ident

        | SyntaxBindingDeclaration.Value(identifierToken=identifierToken) ->
            identifierToken
        | SyntaxBindingDeclaration.New(newToken, _, _) ->
            newToken
        | SyntaxBindingDeclaration.Getter(getToken, _, _)
        | SyntaxBindingDeclaration.Get(getToken) ->
            getToken
        | SyntaxBindingDeclaration.Setter(setToken, _, _)
        | SyntaxBindingDeclaration.Set(setToken) ->
            setToken
        | SyntaxBindingDeclaration.Error(errorToken) ->
            errorToken

    member this.HasReturnTypeAnnotation =
        match this with
        | SyntaxBindingDeclaration.Function(returnTyAnnot=tyAnnot)
        | SyntaxBindingDeclaration.Value(returnTyAnnot=tyAnnot) ->
            match tyAnnot with
            | SyntaxReturnTypeAnnotation.TypeAnnotation _ -> true
            | _ -> false
        | _ ->
            false

    member this.IsExplicitNew =
        match this with
        | SyntaxBindingDeclaration.New _ -> true
        | _ -> false

    member this.IsExplicitGet =
        match this with
        | SyntaxBindingDeclaration.Get _
        | SyntaxBindingDeclaration.Getter _ -> true
        | _ -> false

    member this.IsExplicitSet =
        match this with
        | SyntaxBindingDeclaration.Set _
        | SyntaxBindingDeclaration.Setter _ -> true
        | _ -> false

    member this.IsExplicitFunction =
        match this with
        | SyntaxBindingDeclaration.Function _ -> true
        | _ -> false

type ISyntaxNode with

    member this.GetLeadingTriviaWidth() =
        if this.IsToken then
            (this :?> SyntaxToken).LeadingTriviaWidth
        else
            if this.SlotCount > 0 then
                this.GetSlot(0).GetLeadingTriviaWidth()
            else
                0