namespace Oly.Compiler.Syntax.Internal

open Oly.Core

[<NoComparison>]
type internal Token =
    // Keywords
    | Let
    | LetExclamation
    | Get
    | Set
    | Type
    | Alias
    | Module
    | Of
    | Match
    | With
    | Mutable
    | When
    | Where
    | Inline
    | Trait
    | Static
    | Pure
    | Return
    | Component
    | New
    | Newtype
    | And
    | Or
    | Not
    | Is
    | Private
    | Internal
    | Public
    | Protected
    | Namespace
    | Refine
    | Open
    | If
    | Else
    | Import
    | Export
    | Inherits
    | Extends
    | Implements
    | Interface
    | Class
    | Shape
    | Constant
    | Abstract
    | Struct
    | Default
    | While
    | For
    | In
    | Out
    | Do
    | On
    | Attribute
    | Intrinsic
    | Extension
    | Implicit
    | Explicit
    | Overrides
    | Sealed
    | Data
    | Base
    | As
    | To
    | Pattern
    | Try
    | Catch
    | Finally
    | Throw
    | Constraint
    | Case
    | This
    | Null
    | Enum
    | Unchecked
    | Checked
    | Unmanaged
    | Managed
    | Blittable
    | Scoped
    | Require
    | Field

    // Literals
    | Int8Literal of text: string
    | UInt8Literal of text: string
    | Int16Literal of text: string
    | UInt16Literal of text: string
    | Int32Literal of text: string
    | UInt32Literal of text: string
    | Int64Literal of text: string
    | UInt64Literal of text: string
    | Float32Literal of text: string
    | Float64Literal of text: string
    | True
    | False
    | CharLiteral of singleQuoteToken1: Token * text: string * singleQuoteToken2: Token
    | StringLiteral of doubleQuoteToken1: Token * text: string * doubleQuoteToken2: Token * newLineCount: int * endColumn: int
    | IntegerLiteral of text: string
    | RealLiteral of text: string

    // Identifier
    | Identifier of text: string
    | ExplicitIdentifier of backQuoteToken1: Token * text: string * backQuoteToken2: Token

    // Other
    | Underscore
    | Dot
    | LeftBracket
    | RightBracket

    | LeftBracketInnerPipe
    | RightBracketInnerPipe

    | LeftParenthesis
    | RightParenthesis
    | LeftCurlyBracket
    | RightCurlyBracket
    | Comma
    | SingleQuotation
    | DoubleQuotation
    | BackQuote
    | LeftArrow
    | RightArrow
    | FatRightArrow
    | Colon
    | SemiColon
    | Hash
    | Invalid of text: string
    | Equal
    | EndOfSource

    // Operators
    | EqualEqual
    | EqualEqualEqual
    | Exclamation
    | ExclamationEqual
    | ExclamationEqualEqual
    | Dollar
    | ForwardSlash
    | Percent
    | Caret
    | Ampersand
    | AmpersandAmpersand
    | Star
    | StarStar
    | Minus
    | MinusMinus
    | Plus
    | PlusPlus
    | Pipe
    | PipePipe
    | DotDot
    | DotDotDot
    | Tilde
    | LessThan
    | LessThanEqual
    | LessThanEqualLessThan
    | GreaterThan
    | GreaterThanEqual
    | GreaterThanEqualGreaterThan

    | GreaterThanGreaterThan        // >>
    | LessThanLessThan              // <<

    | LessThanStarGreaterThan       // <*>
    | GreaterThanGreaterThanEqual  // >>=

    | PipeGreaterThan
    | PipeGreaterThanGreaterThan
    | LessThanColon
    | ColonGreaterThan

    // Trivia
    | Space of width: int
    | NewLine
    | CarriageReturn
    | CarriageReturnNewLine
    | SingleLineCommentStart
    | SingleLineComment of startToken: Token * text: string
    | MultiLineCommentStart
    | MultiLineCommentEnd
    | MultiLineComment of startToken: Token * text: string * endToken: Token
    | DirectiveFlag of hashToken: Token * token: Token
    | Directive of hashToken: Token * token: Token * whitespaceToken: Token * valueToken: Token
    | ConditionalDirective of prevToken: Token * bodyText: string * token: Token
    | HashIf of whitespaceToken: Token * identToken: Token
    | HashElse of whitespaceToken: Token * identToken: Token
    | HashEnd

    // Dummy token used to fill in tokens on syntax nodes that have errors
    | Dummy

    member this.Text =
        match this with
        | This -> "this"
        | Let -> "let"
        | LetExclamation -> "let!"
        | Get -> "get"
        | Set -> "set"
        | Type -> "type"
        | Alias -> "alias"
        | Module -> "module"
        | Of -> "of"
        | Match -> "match"
        | With -> "with"
        | Mutable -> "mutable"
        | When -> "when"
        | Where -> "where"
        | Inline -> "inline"
        | Trait -> "trait"
        | Static -> "static"
        | Pure -> "pure"
        | Return -> "return"
        | Component -> "component"
        | New -> "new"
        | Newtype -> "newtype"
        | And -> "and"
        | Or -> "or"
        | Not -> "not"
        | Is -> "is"
        | Private -> "private"
        | Internal -> "internal"
        | Public -> "public"
        | Protected -> "protected"
        | Namespace -> "namespace"
        | Refine -> "refine"
        | Open -> "open"
        | If -> "if"
        | Else -> "else"
        | Import -> "import"
        | Export -> "export"
        | Inherits -> "inherits"
        | Extends -> "extends"
        | Implements -> "implements"
        | Interface -> "interface"
        | Class -> "class"
        | Shape -> "shape"
        | Constant -> "constant"
        | Abstract -> "abstract"
        | Struct -> "struct"
        | Default -> "default"
        | While -> "while"
        | For -> "for"
        | In -> "in"
        | Out -> "out"
        | Do -> "do"
        | On -> "on"
        | Attribute -> "attribute"
        | Intrinsic -> "intrinsic"
        | Extension -> "extension"
        | Implicit -> "implicit"
        | Explicit -> "explicit"
        | Overrides -> "overrides"
        | Sealed -> "sealed"
        | Data -> "data"
        | Base -> "base"
        | As -> "as"
        | To -> "to"
        | Pattern -> "pattern"
        | Try -> "try"
        | Catch -> "catch"
        | Finally -> "finally"
        | Throw -> "throw"
        | Constraint -> "constraint"
        | Case -> "case"
        | Enum -> "enum"
        | Unchecked -> "unchecked"
        | Checked -> "checked"
        | Unmanaged -> "unmanaged"
        | Managed -> "managed"
        | Blittable -> "blittable"
        | Scoped -> "scoped"
        | Require -> "require"
        | Field -> "field"

        // Literals
        | Int8Literal text
        | UInt8Literal text
        | Int16Literal text
        | UInt16Literal text
        | Int32Literal text
        | UInt32Literal text
        | Int64Literal text
        | UInt64Literal text
        | Float32Literal text
        | Float64Literal text -> text
        | CharLiteral(singleQuoteToken1, text, singleQuoteToken2) -> singleQuoteToken1.Text + text + singleQuoteToken2.Text
        | StringLiteral(doubleQuoteToken1, text, doubleQuoteToken2, _, _) -> doubleQuoteToken1.Text + text + doubleQuoteToken2.Text
        | True -> "true"
        | False -> "false"
        | Null -> "null"
        | IntegerLiteral text
        | RealLiteral text -> text

        // Identifier
        | Identifier text -> text
        | ExplicitIdentifier(backQuoteToken1, text, backQuoteToken2) -> backQuoteToken1.Text + text + backQuoteToken2.Text

        // Other
        | Underscore -> "_"
        | Dot -> "."
        | LeftBracket -> "["
        | RightBracket -> "]"
        | LeftBracketInnerPipe -> "[|"
        | RightBracketInnerPipe -> "|]"
        | LeftParenthesis -> "("
        | RightParenthesis -> ")"
        | LeftCurlyBracket -> "{"
        | RightCurlyBracket -> "}"
        | Comma -> ","
        | SingleQuotation -> "'"
        | DoubleQuotation -> "\""
        | BackQuote -> "`"
        | LeftArrow -> "<-"
        | RightArrow -> "->"
        | FatRightArrow -> "=>"
        | Colon -> ":"
        | SemiColon -> ";"
        | Hash -> "#"
        | DirectiveFlag(hashToken, token) -> hashToken.Text + token.Text
        | Directive(hashToken, token, whitespaceToken, valueToken) -> hashToken.Text + token.Text + whitespaceToken.Text + valueToken.Text
        | ConditionalDirective(prevToken, bodyText, token) -> prevToken.Text + bodyText + token.Text
        | HashIf(whitespaceToken, identToken) -> "#if" + whitespaceToken.Text + identToken.Text
        | HashElse(whitespaceToken, identToken) -> "#else" + whitespaceToken.Text + identToken.Text
        | HashEnd -> "#end"
        | Invalid text -> text
        | Equal -> "="
        | EndOfSource -> ""

        // Operators
        | EqualEqual                   -> "=="
        | EqualEqualEqual              -> "==="
        | Exclamation                  -> "!"
        | ExclamationEqual             -> "!="
        | ExclamationEqualEqual        -> "!=="
        | Dollar                       -> "$"
        | ForwardSlash                 -> "/"
        | Percent                      -> "%"
        | Caret                        -> "^"
        | Ampersand                    -> "&"
        | AmpersandAmpersand           -> "&&"
        | Star                         -> "*"
        | StarStar                     -> "**"
        | Minus                        -> "-"
        | MinusMinus                   -> "--"
        | Plus                         -> "+"
        | PlusPlus                     -> "++"
        | Pipe                         -> "|"
        | PipePipe                     -> "||"
        | DotDot                       -> ".."
        | DotDotDot                    -> "..."
        | Tilde                        -> "~"
        | LessThan                     -> "<"
        | LessThanEqual                -> "<="
        | LessThanEqualLessThan        -> "<=<"
        | GreaterThan                  -> ">"
        | GreaterThanEqual             -> ">="
        | GreaterThanEqualGreaterThan  -> ">=>"
        | LessThanLessThan             -> "<<"
        | GreaterThanGreaterThan       -> ">>"
        | LessThanStarGreaterThan      -> "<*>"
        | GreaterThanGreaterThanEqual  -> ">>="
        | PipeGreaterThan              -> "|>"
        | PipeGreaterThanGreaterThan   -> "|>>"
        | LessThanColon                -> "<:"
        | ColonGreaterThan             -> ":>"

        // Trivia
        | Space width -> String.init width (fun _ -> " ")
        | NewLine -> "\n"
        | CarriageReturn -> "\r"
        | CarriageReturnNewLine -> "\r\n"
        | SingleLineCommentStart -> "//"
        | SingleLineComment(startToken, text) -> startToken.Text + text
        | MultiLineCommentStart -> "/*"
        | MultiLineCommentEnd -> "*/"
        | MultiLineComment(startToken, text, endToken) -> startToken.Text + text + endToken.Text

        | Dummy -> ""

    member this.ValueText =
        match this with
        | CharLiteral(_, text, _)
        | StringLiteral(_, text, _, _, _)
        | ExplicitIdentifier(_, text, _) 
        | SingleLineComment(_, text) 
        | MultiLineComment(_, text, _) -> text
        | _ -> this.Text

    override this.ToString() =
        match this with
        | EndOfSource -> ""

        // Trivia
        | Space _ -> " "
        | NewLine -> """\n"""
        | CarriageReturn -> """\r"""
        | CarriageReturnNewLine -> """\r\n"""
        
        | _ -> this.Text

    member this.TryStringLiteralText =
        match this with
        | StringLiteral(_, text, _, _, _) -> ValueSome text
        | _ -> ValueNone

    member this.IsPossibleNewLine =
        match this with
        | NewLine
        | CarriageReturn 
        | CarriageReturnNewLine -> true
        | _ -> false

    member this.Width =
        match this with
        | Space width -> width
        | CharLiteral(t1, text, t2) -> t1.Width + text.Length + t2.Width
        | StringLiteral(t1, text, t2, _, _) -> t1.Width + text.Length + t2.Width
        | ExplicitIdentifier(t1, text, t2) -> t1.Width + text.Length + t2.Width
        | DirectiveFlag(hashToken, token) -> hashToken.Width + token.Width
        | Directive(hashToken, token, whitespaceToken, valueToken) -> hashToken.Width + token.Width + whitespaceToken.Width + valueToken.Width
        | SingleLineComment(startToken, text) -> startToken.Width + text.Length
        | MultiLineComment(t1, text, t2) -> t1.Width + text.Length + t2.Width
        | _ -> this.Text.Length

    member this.IsKeyword =
        match this with
        | Let
        | LetExclamation
        | Get
        | Set
        | Type
        | Alias
        | Module
        | Of
        | Match
        | With
        | Mutable
        | When
        | Inline
        | Trait
        | Static
        | Pure
        | Return
        | Component
        | New
        | Newtype
        | And
        | Or
        | Not
        | Is
        | Private 
        | Internal 
        | Public 
        | Protected 
        | Namespace 
        | Refine
        | If
        | Else 
        | Open
        | Inherits
        | Extends
        | Implements
        | Class 
        | Import
        | Export 
        | Shape 
        | Constant
        | Abstract
        | Struct 
        | Default 
        | While
        | For
        | In 
        | Out
        | Do
        | On
        | Intrinsic 
        | Extension
        | Implicit 
        | Explicit
        | Overrides
        | Sealed 
        | Data
        | Base
        | As
        | To
        | Pattern
        | Try
        | Catch
        | Finally
        | Throw
        | Constraint 
        | This 
        | Null 
        | Enum 
        | Attribute 
        | Unchecked
        | Where
        | Checked 
        | Unmanaged
        | Managed
        | Blittable 
        | Scoped
        | Require 
        | Field -> true
        | _ -> false

    member this.IsLiteral =
        match this with
        | Int8Literal _
        | UInt8Literal _
        | Int16Literal _
        | UInt16Literal _
        | Int32Literal _
        | UInt32Literal _
        | Int64Literal _
        | UInt64Literal _
        | Float32Literal _
        | Float64Literal _
        | True
        | False
        | CharLiteral _
        | StringLiteral _ 
        | Null 
        | IntegerLiteral _
        | RealLiteral _ -> true
        | _ -> false

    member this.IsStringLiteral_t =
        match this with
        | StringLiteral _ -> true
        | _ -> false

    member this.IsIdentifierToken =
        match this with
        | Identifier _ 
        | ExplicitIdentifier _ -> true
        | _ -> false

    member this.IsOperator =
        match this with
        | EqualEqual
        | EqualEqualEqual
        | Exclamation
        | ExclamationEqual
        | ExclamationEqualEqual
        | Dollar
        | ForwardSlash
        | Percent
        | Caret
        | Ampersand
        | AmpersandAmpersand
        | Star
        | StarStar
        | Minus
        | MinusMinus
        | Plus
        | PlusPlus
        | Pipe
        | PipePipe
        | DotDot
        | DotDotDot
        | Tilde
        | LessThan
        | LessThanEqual
        | LessThanEqualLessThan
        | GreaterThan
        | GreaterThanEqual
        | GreaterThanEqualGreaterThan
        | LessThanStarGreaterThan
        | GreaterThanGreaterThanEqual
        | PipeGreaterThan
        | PipeGreaterThanGreaterThan
        | LessThanColon
        | ColonGreaterThan 
        | GreaterThanGreaterThan 
        | LessThanLessThan
        | Not
        | And
        | Or -> true

        // Special Operators
        | LetExclamation
        | Return -> true
        | ExplicitIdentifier(_, text, _) ->
            text = "[]" || text = "[,]"

        | _ -> false

    member this.IsSpecialOperator =
        match this with
        | LetExclamation
        | Return -> true
        | ExplicitIdentifier(_, text, _) ->
            text = "[]" || text = "[,]"
        | _ -> false

    member this.IsTriviaExceptEndOfSource =
        match this with
        | Space _
        | NewLine
        | CarriageReturn
        | CarriageReturnNewLine
        | SingleLineCommentStart
        | SingleLineComment _
        | MultiLineCommentStart
        | MultiLineCommentEnd
        | MultiLineComment _ 
        | DirectiveFlag _
        | Directive _ 
        | ConditionalDirective _
        | HashIf _
        | HashEnd -> true
        | _ -> false

    member this.IsTrivia =
        if this.IsTriviaExceptEndOfSource then
            true
        else
            match this with
            | EndOfSource -> true
            | _ -> false

    member this.IsComment =
        match this with
        | SingleLineComment _
        | MultiLineComment _ -> true
        | _ -> false

    member this.IsOther =
        match this with
        | Underscore
        | Dot
        | LeftBracket
        | RightBracket
        | LeftParenthesis
        | RightParenthesis
        | LeftCurlyBracket
        | RightCurlyBracket
        | Comma
        | SingleQuotation
        | DoubleQuotation
        | BackQuote
        | RightArrow
        | LeftArrow
        | Colon
        | SemiColon
        | Hash
        | Invalid _
        | Equal -> true
        | _ -> false

    member this.IsIdentifierOrOperatorOrKeyword =
        this.IsIdentifierToken || this.IsOperator || this.IsKeyword
