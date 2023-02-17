module internal Oly.Compiler.Syntax.Internal.Lexer

open System
open System.Threading
open System.Collections.Generic
open Oly.Compiler.Text
open Oly.Compiler.Syntax.Internal
open Oly.Core

#nowarn "9"

[<Literal>]
let private InvalidCharacter = Char.MaxValue

let private GlobalStringComparer =
    { new IEqualityComparer<ReadOnlyMemory<char>> with
        member _.GetHashCode(str: ReadOnlyMemory<char>) = str.Length
        member _.Equals(str1: ReadOnlyMemory<char>, str2: ReadOnlyMemory<char>) =
            str1.Span.SequenceEqual(str2.Span)
    }

let private GlobalStringIntern = System.Collections.Concurrent.ConcurrentDictionary<ReadOnlyMemory<char>, string>(GlobalStringComparer)
let private GlobalStringInternIdent = System.Collections.Concurrent.ConcurrentDictionary<ReadOnlyMemory<char>, Token>(GlobalStringComparer)

[<Literal>]
let private MaxGlobalStringInternSpaceCount = 128
let private GlobalStringInternSpace = Array.zeroCreate<Token> MaxGlobalStringInternSpaceCount

[<Sealed>]
type private SlidingTextWindow (text: IOlySourceText) =

    let length = text.Length
    let mutable startPos = 0
    let mutable endPos = 0

    member _.PeekChar() =
        if endPos >= length then
            InvalidCharacter
        else
            text.[endPos]

    member _.PeekCharN n =
        let peekPos = endPos + n
        if peekPos >= length then
            InvalidCharacter
        else
            text.[peekPos]

    member _.AdvanceChar() =
        endPos <- endPos + 1

    member _.ResetLexeme() =
        startPos <- endPos

    member _.LexemeStart = startPos

    member _.LexemeEnd = endPos

    member _.LexemeWidth = endPos - startPos

    member _.SetLexemeRange(s, e) =
        startPos <- s
        endPos <- e

    member this.Lexeme() =
        let subText = text.GetSubTextView(this.LexemeStart, this.LexemeWidth)
        this.ResetLexeme()
        match GlobalStringIntern.TryGetValue subText with
        | true, str -> str
        | _ ->
            let str = subText.ToString()
            GlobalStringIntern[str.AsMemory()] <- str
            str

    member this.LexemeIdent() =
        let subText = text.GetSubTextView(this.LexemeStart, this.LexemeWidth)
        this.ResetLexeme()
        match GlobalStringInternIdent.TryGetValue subText with
        | true, ident -> ident
        | _ ->
            let str = subText.ToString()
            let ident = Identifier(str)
            GlobalStringInternIdent[str.AsMemory()] <- ident
            ident

    member this.LexemeSpace() =
        let length = this.LexemeWidth
        this.ResetLexeme()
        if length >= MaxGlobalStringInternSpaceCount then
            Token.Space(length)
        else

        let mutable space = GlobalStringInternSpace[length]
        if obj.ReferenceEquals(space, null) then
            space <- Token.Space(length)
            GlobalStringInternSpace[length] <- space
        space

    member this.Text = text

type LexNumericLiteralKind =
    | DecimalInteger
    | HexadecimalInteger
    | BinaryInteger
    | Real

type Lexer =
    private {
        diagnostics: ResizeArray<int * int * string * bool * int>
        window: SlidingTextWindow
        conditionalDefines: string imarray
        conditionalDefinesLookup: HashSet<string>

        /// Non-condtional directives
        directives: ResizeArray<int * int * Token>

        mutable hasFirstNonTrivia: bool
        mutable currentColumn: int
        mutable currentConditionalCount: int

        // Newline info
        mutable wasPrevCarriageReturn: bool
    }

    member this.Text = this.window.Text

    /// Do not call this concurrently while the lexer is running.
    member this.CurrentColumn = this.currentColumn

    member this.CurrentPosition = this.window.LexemeStart

    member this.CurrentEndPosition = this.window.LexemeEnd

    member this.SetCurrentLexemeRange(startPos, endPos) =
        this.window.SetLexemeRange(startPos, endPos)

    member this.SetCurrentColumn(column) =
        this.currentColumn <- column

    /// Do not call this concurrently on the same lexer.
    /// Do not call this concurrently while the lexer is running.
    member this.GetCurrentDiagnostics() =
        this.diagnostics
        |> ImArray.ofSeq

module Lexer =

    let create (conditionalDefines: string imarray) text =
        {
            diagnostics = ResizeArray()
            window = SlidingTextWindow(text)
            conditionalDefines = conditionalDefines
            conditionalDefinesLookup = HashSet(conditionalDefines)
            directives = ResizeArray()
            hasFirstNonTrivia = false
            currentColumn = 0
            currentConditionalCount = 0
            wasPrevCarriageReturn = false
        }

    let resetLexeme lexer =
        lexer.window.ResetLexeme()

    let lexeme lexer =
        lexer.window.Lexeme()

    let lexemeIdent lexer =
        lexer.window.LexemeIdent()

    let lexemeSpace lexer =
        lexer.window.LexemeSpace()

    let lexemeWidth lexer =
        lexer.window.LexemeWidth

    let peek lexer =
        lexer.window.PeekChar()

    let peekN n lexer =
        lexer.window.PeekCharN n

    let advance lexer =
        let c = lexer.window.PeekChar()
        if c = '\n' then
            if lexer.wasPrevCarriageReturn then
                lexer.wasPrevCarriageReturn <- false
                lexer.currentColumn <- 0

        elif c = '\r' then
            lexer.wasPrevCarriageReturn <- true
            lexer.currentColumn <- lexer.currentColumn + 1
        else
            lexer.wasPrevCarriageReturn <- false
            lexer.currentColumn <- lexer.currentColumn + 1
        lexer.window.AdvanceChar()

    let isLetter c =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    let isGreekLetter c =
        (c >= 'α' && c <= 'ω') || (c >= 'Α' && c <= 'Ω')

    let isDigit c =
        (c >= '0' && c <= '9')

    let isUnderscore c =
        c = '_'

    let rec scanWhitespace lexer =
        match peek lexer with
        | ' ' ->
            advance lexer
            scanWhitespace lexer
        | _ ->
            lexemeSpace lexer

    let rec scanNumericLiteralAux lexer count kind =
        match peek lexer with
        | '.' when count > 0 && (kind = DecimalInteger) -> 
            advance lexer
            scanNumericLiteralAux lexer (count + 1) Real

        | '_' when count > 0 ->
            advance lexer
            scanNumericLiteralAux lexer 0 kind

        | '0'             
        | '1' ->
            advance lexer
            scanNumericLiteralAux lexer (count + 1) kind

        | '2'
        | '3'
        | '4'
        | '5'
        | '6'
        | '7'
        | '8'
        | '9' when not (kind = BinaryInteger) ->
            advance lexer
            scanNumericLiteralAux lexer (count + 1) kind

        | c when (kind = HexadecimalInteger) ->
            match c with
            | 'a'
            | 'b'
            | 'c'
            | 'd'
            | 'e'
            | 'f' 
            | 'A'
            | 'B'
            | 'C'
            | 'D'
            | 'E'
            | 'F' -> 
                advance lexer
                scanNumericLiteralAux lexer (count + 1) kind
            | _ ->
                let text = lexeme lexer
                IntegerLiteral(text)

        | 'u' when count > 0 && (kind = DecimalInteger) ->
            advance lexer
            match peek lexer, peekN 1 lexer with
            | '6', '4' ->
                advance lexer
                advance lexer
                let text = lexeme lexer
                UInt64Literal(text)
            | '3', '2' ->
                advance lexer
                advance lexer
                let text = lexeme lexer
                UInt32Literal(text)
            | '1', '6' ->
                advance lexer
                advance lexer
                let text = lexeme lexer
                UInt16Literal(text)
            | '8', _ ->
                advance lexer
                let text = lexeme lexer
                UInt8Literal(text)
            | _ ->
                let text = lexeme lexer
                UInt32Literal(text)

        | 'i' when count > 0 && (kind = DecimalInteger) ->
            advance lexer
            match peek lexer, peekN 1 lexer with
            | '6', '4' ->
                advance lexer
                advance lexer
                let text = lexeme lexer
                Int64Literal(text)
            | '3', '2' ->
                advance lexer
                advance lexer
                let text = lexeme lexer
                Int32Literal(text)
            | '1', '6' ->
                advance lexer
                advance lexer
                let text = lexeme lexer
                Int16Literal(text)
            | '8', _ ->
                advance lexer
                let text = lexeme lexer
                Int8Literal(text)
            | _ ->
                let text = lexeme lexer
                Int32Literal(text)

        | c when count > 0 && not(isLetter(c)) && (kind <> Real) ->
            let text = lexeme lexer
            IntegerLiteral(text)

        | 'f' when count > 0 && (kind = Real) ->
            advance lexer
            match peek lexer, peekN 1 lexer with
            | '6', '4' ->
                advance lexer
                advance lexer
                let text = lexeme lexer
                Float64Literal(text)
            | '3', '2' ->
                advance lexer
                advance lexer
                let text = lexeme lexer
                Float32Literal(text)
            | _ ->
                let text = lexeme lexer
                Float32Literal(text)

        | c when count > 0 && not(isLetter(c)) && (kind = Real) ->
            let text = lexeme lexer
            RealLiteral(text)

        | _ ->
            let startPos = lexer.window.LexemeStart
            let endPos = lexer.window.LexemeEnd

            lexer.diagnostics.Add(startPos, endPos, "Not a valid numeric literal.", true, 159)

            let text = lexeme lexer
            match kind with
            | Real -> RealLiteral(text)
            | _ -> IntegerLiteral(text)
      
    let rec scanNumericLiteral lexer =
        match peek lexer with
        | '0' ->
            match peekN 1 lexer with
            | 'b' ->
                advance lexer
                advance lexer
                scanNumericLiteralAux lexer 0 BinaryInteger
            | 'x' ->
                advance lexer
                advance lexer
                scanNumericLiteralAux lexer 0 HexadecimalInteger
            | _ ->
                scanNumericLiteralAux lexer 0 DecimalInteger
        | '1'
        | '2'
        | '3'
        | '4'
        | '5'
        | '6'
        | '7'
        | '8'
        | '9' ->
            scanNumericLiteralAux lexer 0 DecimalInteger
        | _ ->
            failwith "Invalid numeric literal."  

    let rec scanSingleLineComment lexer =
        match peek lexer with
        | '\r'
        | '\n' 
        | InvalidCharacter ->
            let text = lexeme lexer
            SingleLineComment(SingleLineCommentStart, text)
        | _ ->
            advance lexer
            scanSingleLineComment lexer

    let rec scanMultiLineComment lexer =
        match peek lexer with
        | '*' ->
            match peekN 1 lexer with
            | '/' ->
                let text = lexeme lexer
                advance lexer
                advance lexer
                resetLexeme lexer
                MultiLineComment(MultiLineCommentStart, text, MultiLineCommentEnd)
            | _ ->
                advance lexer
                scanMultiLineComment lexer

        | InvalidCharacter ->
            let text = lexeme lexer
            MultiLineComment(MultiLineCommentStart, text, EndOfSource)

        | _ ->
            advance lexer
            scanMultiLineComment lexer

    let rec scanText lexer =
        match peek lexer with
        | c when isLetter c || isDigit c || isUnderscore c ->
            advance lexer
            scanText lexer
        | _ ->
            lexemeIdent lexer

    let scanKeywordOrIdentifier lexer =
        let ident = scanText lexer
        match ident.ValueText with
        | "" -> Dummy
        | "let" -> 
            match peek lexer with
            | '!' ->
                advance lexer
                LetExclamation
            | _ ->
                Let
        | "get" -> Get
        | "set" -> Set
        | "type" -> Type
        | "alias" -> Alias
        | "module" -> Module
        | "of" -> Of
        | "match" -> Match
        | "with" -> With
        | "mutable" -> Mutable
        | "when" -> When
        | "where" -> Where
        | "inline" -> Inline
        | "trait" -> Trait
        | "static" -> Static
        | "pure" -> Pure
        | "return" -> Return
        | "component" -> Component
        | "new" -> New
        | "newtype" -> Newtype
        | "and" -> And
        | "or" -> Or
        | "not" -> Not
        | "is" -> Is
        | "public" -> Public
        | "private" -> Private
        | "internal" -> Internal
        | "protected" -> Protected
        | "namespace" -> Namespace
        | "refine" -> Refine
        | "true" -> True
        | "false" -> False
        | "open" -> Open
        | "if" -> If
        | "else" -> Else
        | "import" -> Import
        | "export" -> Export
        | "inherits" -> Inherits
        | "extends" -> Extends
        | "implements" -> Implements
        | "interface" -> Interface
        | "class" -> Class
        | "shape" -> Shape
        | "constant" -> Constant
        | "abstract" -> Abstract
        | "struct" -> Struct
        | "default" -> Default
        | "while" -> While
        | "for" -> For
        | "in" -> In
        | "out" -> Out
        | "do" -> Do
        | "on" -> On
        | "attribute" -> Attribute
        | "intrinsic" -> Intrinsic
        | "extension" -> Extension
        | "implicit" -> Implicit
        | "explicit" -> Explicit
        | "overrides" -> Overrides
        | "sealed" -> Sealed
        | "data" -> Data
        | "base" -> Base
        | "as" -> As
        | "to" -> To
        | "pattern" -> Pattern
        | "try" -> Try
        | "catch" -> Catch
        | "finally" -> Finally
        | "throw" -> Throw
        | "constraint" -> Constraint
        | "case" -> Case
        | "this" -> This
        | "null" -> Null
        | "enum" -> Enum
        | "unchecked" -> Unchecked
        | "checked" -> Checked
        | "unmanaged" -> Unmanaged
        | "managed" -> Managed
        | "blittable" -> Blittable
        | "scoped" -> Scoped
        | "require" -> Require
        | _ ->
            ident

    let endScanCharLiteral lexer c startPos hasNewLine endColumn =
        let text = lexeme lexer
        advance lexer
        resetLexeme lexer
        let terminalToken =
            match c with
            | ''' -> SingleQuotation
            | _ -> EndOfSource

        if hasNewLine then
            let endPos = 
                match terminalToken with
                | EndOfSource -> lexer.window.LexemeEnd - 1
                | _ -> lexer.window.LexemeEnd
            lexer.diagnostics.Add(startPos, endPos, "New-lines are not valid in character literals.", true, 170)
        else
            match terminalToken with
            | EndOfSource ->
                let endPos = lexer.window.LexemeEnd - 1
                lexer.diagnostics.Add(startPos, endPos, "Character literal reached end-of-source.", true, 171)
            | _ ->
                ()

        lexer.currentColumn <- endColumn
        CharLiteral(SingleQuotation, text, terminalToken)

    let rec scanCharLiteral lexer startPos hasNewLine endColumn began =
        match peek lexer with
        | ''' when began ->
            advance lexer
            resetLexeme lexer
            scanCharLiteral lexer startPos hasNewLine (endColumn + 1) false
        | c when c = ''' ->
            endScanCharLiteral lexer c startPos hasNewLine (endColumn + 1)
        | c when c = InvalidCharacter ->
            endScanCharLiteral lexer c startPos hasNewLine endColumn
        | c ->
            let hasNewLine, endColumn =
                if c = '\n' then
                    true, 0
                elif c = '\r' then
                    true, 0
                else
                    hasNewLine, endColumn + 1

            advance lexer
            scanCharLiteral lexer startPos hasNewLine endColumn began

    let rec scanStringLiteral lexer newLineCount endColumn began =
        match peek lexer with
        | '"' when began ->
            advance lexer
            resetLexeme lexer
            scanStringLiteral lexer newLineCount (endColumn + 1) false
        | c when c = '"' || c = InvalidCharacter ->
            let text = lexeme lexer
            advance lexer
            resetLexeme lexer
            let terminalToken =
                match c with
                | '"' -> DoubleQuotation
                | _ -> EndOfSource
            lexer.currentColumn <- endColumn + 1
            StringLiteral(DoubleQuotation, text, terminalToken, newLineCount, lexer.currentColumn)
        | c ->
            let newLineCount, endColumn =
                if c = '\n' then
                    if lexer.wasPrevCarriageReturn then
                        newLineCount, 0
                    else
                        newLineCount + 1, 0
                elif c = '\r' then
                    newLineCount + 1, 0
                else
                    newLineCount, endColumn + 1
            advance lexer
            scanStringLiteral lexer newLineCount endColumn began

    let rec scanExplicitIdentifier lexer began =
        match peek lexer with
        | '`' when began ->
            advance lexer
            resetLexeme lexer
            scanExplicitIdentifier lexer false
        | x when x = '`' || x = InvalidCharacter ->
            let text = lexeme lexer
            advance lexer
            resetLexeme lexer
            let terminalToken =
                match x with
                | '`' -> BackQuote
                | _ -> EndOfSource
            ExplicitIdentifier(BackQuote, text, terminalToken)
        | _ ->
            advance lexer
            scanExplicitIdentifier lexer began

    let isNextTokenHashIf (lexer: Lexer) =
        match peek lexer with
        | '#' ->
            match peekN 1 lexer with
            | 'i' ->
                match peekN 2 lexer with
                | 'f' ->
                    match peekN 3 lexer with
                    | ' ' -> true
                    | _ -> false
                | _ ->
                    false
            | _ ->
                false
        | _ ->
            false

    let isNextTokenHashEnd (lexer: Lexer) =
        match peek lexer with
        | '#' ->
            match peekN 1 lexer with
            | 'e' ->
                match peekN 2 lexer with
                | 'n' ->
                    match peekN 3 lexer with
                    | 'd' -> true
                    | _ -> false
                | _ ->
                    false
            | _ ->
                false
        | _ ->
            false

    let checkDirectiveIndentation (lexer: Lexer) startColumn startPos =
        let endPos = lexer.window.LexemeStart

        if startColumn > 0 then
            lexer.diagnostics.Add(startPos, endPos, "Directives may not be indented.", true, 150)

    let recordNonConditionalDirective (lexer: Lexer) startPos directive =
        let endPos = lexer.window.LexemeStart

        if lexer.hasFirstNonTrivia then
            lexer.diagnostics.Add(startPos, endPos, "Non-conditional directives must be declared at the top.", true, 151)
        else
            lexer.directives.Add(startPos, endPos, directive)

    let endConditionalDirective (lexer: Lexer) =
        let startColumn = lexer.currentColumn
        let startPos = lexer.window.LexemeStart

        advance lexer
        advance lexer
        advance lexer
        advance lexer
        resetLexeme lexer

        checkDirectiveIndentation lexer startColumn startPos

        if lexer.currentConditionalCount > 0 then
            lexer.currentConditionalCount <- lexer.currentConditionalCount - 1
        else
            let endPos = lexer.window.LexemeStart
            lexer.diagnostics.Add(startPos, endPos, "No corresponding conditional directive was found.", true, 154)

        HashEnd

    let tryEndConditionalDirective (lexer: Lexer) hashIfToken (outToken: outref<Token>) : bool =
        match peek lexer with
        | '#' ->
            match peekN 1 lexer with
            | 'e' ->
                match peekN 2 lexer with
                | 'n' ->
                    match peekN 3 lexer with
                    | 'd' ->
                        let bodyText = lexeme lexer
                        outToken <- ConditionalDirective(hashIfToken, bodyText, endConditionalDirective lexer)
                        true
                    | _ -> 
                        false
                | _ -> 
                    false
            | _ ->
                false
        | c ->
            if c = InvalidCharacter then
                let bodyText = lexeme lexer
                resetLexeme lexer
                outToken <- ConditionalDirective(hashIfToken, bodyText, EndOfSource)
                true
            else
                false

    let rec scanTextOfConditionalDefine (lexer: Lexer) hashIfToken =     
        let mutable token = Unchecked.defaultof<_>
        match tryEndConditionalDirective lexer hashIfToken &token with
        | true -> token
        | _ ->
            advance lexer
            scanTextOfConditionalDefine lexer hashIfToken
            
    let beginScanTextOfConditionalDefine (lexer: Lexer) =
        let startColumn = lexer.currentColumn
        let startPos = lexer.window.LexemeStart

        advance lexer // #
        advance lexer // i
        advance lexer // f
        resetLexeme lexer

        checkDirectiveIndentation lexer startColumn startPos

        lexer.currentConditionalCount <- lexer.currentConditionalCount + 1

        let whitespaceToken = scanWhitespace lexer
        resetLexeme lexer

        let startIdentPos = lexer.window.LexemeStart

        let identToken = scanKeywordOrIdentifier lexer
        resetLexeme lexer

        let hashIfToken = HashIf(whitespaceToken, identToken)

        match identToken with
        | Token.Identifier ident -> 
            if lexer.conditionalDefinesLookup.Contains(ident) then
                hashIfToken
            else
                scanTextOfConditionalDefine lexer hashIfToken
        | _ ->
            let endPos = lexer.window.LexemeStart
            lexer.diagnostics.Add(startIdentPos, endPos, "Invalid conditional define.", true, 153)
            ConditionalDirective(hashIfToken, String.Empty, dummyToken.RawToken)

    let handleNonTriviaPeek (lexer: Lexer) peekedChar =
        lexer.hasFirstNonTrivia <- true
        match peekedChar with
        | '#' ->
            if isNextTokenHashIf lexer then
                beginScanTextOfConditionalDefine lexer
            elif isNextTokenHashEnd lexer then
                endConditionalDirective lexer
            else
                advance lexer
                Hash

        | '/' ->
            advance lexer
            ForwardSlash

        | '_' -> 
            advance lexer
            match peek lexer with
            | c when isLetter c || isDigit c || isUnderscore c ->
                advance lexer
                scanText lexer
            | _ ->
                Underscore

        | '.' -> 
            advance lexer
            match peek lexer with
            | '.' ->
                advance lexer
                match peek lexer with
                | '.' ->
                    advance lexer
                    DotDotDot
                | _ ->
                    DotDot
            | _ ->
                Dot

        | '[' -> 
            advance lexer
            match peek lexer with
            | '|' ->
                advance lexer
                LeftBracketInnerPipe
            | _ ->
                LeftBracket

        | ']' -> 
            advance lexer
            RightBracket

        | '(' -> 
            advance lexer
            LeftParenthesis

        | ')' ->
            advance lexer
            RightParenthesis

        | '{' -> 
            advance lexer
            LeftCurlyBracket

        | '}' ->
            advance lexer
            RightCurlyBracket

        | ',' -> 
            advance lexer
            Comma

        | '|' -> 
            advance lexer
            match peek lexer with
            | '|' ->
                advance lexer
                PipePipe
            | '>' ->
                advance lexer
                match peek lexer with
                | '>' ->
                    PipeGreaterThanGreaterThan
                | _ ->
                    PipeGreaterThan
            | ']' ->
                advance lexer
                RightBracketInnerPipe
            | _ ->
                Pipe

        | '*' -> 
            advance lexer
            match peek lexer with
            | '*' ->
                advance lexer
                StarStar
            | _ ->
                Star

        | '-' ->
            advance lexer
            match peek lexer with
            | '>' -> 
                advance lexer
                RightArrow
            | '-' ->
                advance lexer
                MinusMinus
            | n when isDigit n ->
                scanNumericLiteral lexer
            | _ -> 
                Minus

        | '+' -> 
            advance lexer
            match peek lexer with
            | '+' -> 
                advance lexer
                PlusPlus
            | _ ->
                Plus

        | ':' ->
            advance lexer
            match peek lexer with
            | '>' ->
                advance lexer
                ColonGreaterThan
            | _ ->
                Colon

        | ';' ->
            advance lexer
            SemiColon

        | '>' ->
            advance lexer
            match peek lexer with
            | '=' ->
                advance lexer
                match peek lexer with
                | '>' ->
                    advance lexer
                    GreaterThanEqualGreaterThan
                | _ ->
                    GreaterThanEqual
            | '>' ->
                match peekN 1 lexer with
                | '=' ->
                    advance lexer
                    advance lexer
                    GreaterThanGreaterThanEqual
                | _ ->
                    advance lexer
                    GreaterThanGreaterThan
            | _ ->
                GreaterThan

        | '<' ->
            advance lexer
            match peek lexer with
            | '-' ->
                advance lexer
                LeftArrow
            | '*' ->
                match peek lexer with
                | '>' ->
                    advance lexer
                    advance lexer
                    LessThanStarGreaterThan
                | _ ->
                    LessThan
            | '=' ->
                advance lexer
                match peek lexer with
                | '<' ->
                    advance lexer
                    LessThanEqualLessThan
                | _ ->
                    LessThanEqual
            | ':' ->
                advance lexer
                LessThanColon
            | '<' ->
                advance lexer
                LessThanLessThan
            | _ ->
                LessThan

        | '=' ->
            advance lexer
            match peek lexer with
            | '=' ->
                advance lexer
                match peek lexer with
                | '=' ->
                    advance lexer
                    EqualEqualEqual
                | _ ->
                    EqualEqual
            | '>' ->
                advance lexer
                FatRightArrow
            | _ ->
                Equal

        | '!' ->
            advance lexer
            match peek lexer with
            | '=' ->
                advance lexer
                match peek lexer with
                | '=' ->
                    advance lexer
                    ExclamationEqualEqual
                | _ ->
                    ExclamationEqual
            | _ ->
                Exclamation

        | '%' ->
            advance lexer
            Percent

        | '$' ->
            advance lexer
            Dollar

        | '&' ->
            advance lexer
            match peek lexer with
            | '&' ->
                advance lexer
                AmpersandAmpersand
            | _ ->
                Ampersand

        | '~' ->
            advance lexer
            Tilde

        | '^' ->
            advance lexer
            Caret

        | ''' ->
            scanCharLiteral lexer lexer.window.LexemeStart false lexer.currentColumn true

        | '"' ->
            scanStringLiteral lexer 0 lexer.currentColumn true

        | '`' ->
            scanExplicitIdentifier lexer true

        | x when isDigit x ->
            scanNumericLiteral lexer

        | InvalidCharacter ->
            EndOfSource

        | c when isLetter c ->
            scanKeywordOrIdentifier lexer

        | c when isGreekLetter c ->
            advance lexer
            let text = lexeme lexer
            Identifier(text)

        | _ ->
            advance lexer
            let text = lexeme lexer
            Invalid(text)

    let handlePeek (lexer: Lexer) peekedChar =
        match peekedChar with
        | '#' ->
            if isNextTokenHashIf lexer then
                beginScanTextOfConditionalDefine lexer
            elif isNextTokenHashEnd lexer then
                endConditionalDirective lexer
            else

            match peekN 1 lexer with
            | c when isLetter c ->
                // Directives

                let startColumn = lexer.currentColumn
                let startPos = lexer.window.LexemeStart

                advance lexer
                resetLexeme lexer
                let token = scanKeywordOrIdentifier lexer

                let directive =
                    match token with
                    | Token.Identifier("library") -> 
                        resetLexeme lexer
                        DirectiveFlag(Hash, token)
                    | _ ->
                        resetLexeme lexer
                        let whitespaceToken = scanWhitespace lexer
                        resetLexeme lexer

                        let valueToken =
                            match peek lexer with
                            | '\"' ->
                                scanStringLiteral lexer 0 lexer.currentColumn true
                            | _ ->
                                scanKeywordOrIdentifier lexer

                        if not (valueToken.IsIdentifierToken || valueToken.IsStringLiteral_t) then
                            let endPos = lexer.window.LexemeStart
                            lexer.diagnostics.Add(startPos, endPos, "Invalid directive value.", true, 152)

                        Directive(Hash, token, whitespaceToken, valueToken)

                checkDirectiveIndentation lexer startColumn startPos
                recordNonConditionalDirective lexer startPos directive

                directive
            | _ ->
                handleNonTriviaPeek lexer '#'
        | ' ' ->
            scanWhitespace lexer
        | '\n' ->
            advance lexer
            NewLine

        | '\r' ->
            advance lexer
            match peek lexer with
            | '\n' ->
                advance lexer
                CarriageReturnNewLine
            | _ ->
                CarriageReturn
        | '/' ->
            match peekN 1 lexer with
            | '/' ->
                advance lexer
                advance lexer
                resetLexeme lexer
                scanSingleLineComment lexer
            | '*' ->
                advance lexer
                advance lexer
                resetLexeme lexer
                scanMultiLineComment lexer
            | _ ->
                handleNonTriviaPeek lexer '/'
        | c ->
            handleNonTriviaPeek lexer c
            
    let rec scanToken (lexer: Lexer) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        let token = handlePeek lexer (peek lexer)
        resetLexeme lexer
        token

    and getDirectives (lexer: Lexer) (ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()

        if lexer.hasFirstNonTrivia then
            lexer.directives |> ImArray.ofSeq
        else
            let lexer = create lexer.conditionalDefines lexer.Text
            let mutable token = EndOfSource
            while not lexer.hasFirstNonTrivia && 
                    (
                        token <- scanToken lexer ct
                        token <> EndOfSource
                    ) do ()
            lexer.directives |> ImArray.ofSeq
        