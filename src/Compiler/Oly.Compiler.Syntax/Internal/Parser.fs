module rec Oly.Compiler.Syntax.Internal.Parser

open System
open System.Threading
open System.Runtime.CompilerServices

open Oly.Core
open Oly.Compiler.Syntax.Internal
open Oly.Compiler.Syntax.Internal.Lexer
open Oly.Compiler.Syntax.Internal.SyntaxErrors

let dummyToken() = SyntaxHelpers.dummyToken
let SyntaxAttributeErrorF = SyntaxAttribute.Error

type [<RequireQualifiedAccess>] SyntaxTreeContext =
    | TopLevel
    | Local of skipSequential: bool

    member this.CanSkipSequential =
        match this with
        | TopLevel
        | Local(false) -> false
        | _ -> true

let SyntaxTreeContextLocal = SyntaxTreeContext.Local(false)
let SyntaxTreeContextLocalSkipSequential = SyntaxTreeContext.Local(true)

[<Flags>]
type OffsideFlags =
    | None =        0b00000uy
    | WillAlign =   0b00001uy
    | WillIgnore =  0b00010uy
    | IsFlexible =  0b00100uy

/// Amount represents the size of a
/// ring buffer that holds tokens
/// that are back-trackable.
/// 256 is conservative as there should never
/// be an instance where we need to back-track near
/// that many tokens - if there is such a case, we need to
/// solve the parser rules differently rather than increase the size
/// of the ring buffer.
/// Trivia is excluded from this buffer.
[<Literal>]
let MaxBackTrackableTokenAmount = 256 // Make sure this integer is a power of two.

[<Struct;NoEquality;NoComparison>]
type TokenInfo(token: SyntaxToken, start: int, column: int, newLine: bool) =
    member _.Token = token
    member _.Start = start
    member _.Column = column
    member _.NewLine = newLine

[<NoEquality;NoComparison>]
type ParserState =
    {
        lexer: Lexer
        ct: CancellationToken
        diagnostics: ConditionalWeakTable<ISyntaxNode, ResizeArray<DiagnosticSyntax>>

        mutable start: int
        mutable column: int
        mutable newLine: bool

        mutable prevOffside: int
        mutable noErrors: bool

        // Offside state
        mutable offside: int
        mutable offsideFlags: OffsideFlags

        // Back-track buffer
        mutable btBuffer: TokenInfo[]
        mutable btBufferCount: int
        mutable btBufferPosition: int

        // This is used for perf reasons. It only gave ~2-3% perf improvement, but still worth it.
        mutable peekedPosition: int
        mutable peekedToken: SyntaxToken
        
        buffers: System.Collections.Generic.Stack<TokenInfo[]>
    }

    member inline this.willAlign = this.offsideFlags.HasFlag(OffsideFlags.WillAlign)
    member inline this.willIgnore = this.offsideFlags.HasFlag(OffsideFlags.WillIgnore)
    member inline this.isFlexible = this.offsideFlags.HasFlag(OffsideFlags.IsFlexible)

    /// ">>" is always lexed as a single token, GreaterThanGreaterThan.
    /// In order to handle ">>" when parsing type arguments, i.e. "A<B<int32>>"
    ///     we need to split the token in two.
    member this.SplitGreaterThanGreaterThan(token: SyntaxToken) =
        OlyAssert.True(token.RawToken = GreaterThanGreaterThan)
        OlyAssert.True(obj.ReferenceEquals(this.btBuffer[(this.btBufferPosition - 1) % MaxBackTrackableTokenAmount].Token, token))

        this.column <- this.column - 1

        let newPrevToken = 
            match token with
            | SyntaxToken.Token(_) ->
                SyntaxToken.Token(GreaterThan)
            | SyntaxToken.TokenWithTrivia(leadingTrivia, _, fullWidth) ->
                SyntaxToken.TokenWithTrivia(leadingTrivia, GreaterThan, fullWidth - 1)

        let prevTokenInfo = this.btBuffer[(this.btBufferPosition - 1) % MaxBackTrackableTokenAmount]
        let newPrevTokenInfo = TokenInfo(newPrevToken, prevTokenInfo.Start, prevTokenInfo.Column - 1, false)
        this.btBuffer[(this.btBufferPosition - 1) % MaxBackTrackableTokenAmount] <- newPrevTokenInfo

        let nextToken = SyntaxToken.Token(GreaterThan)
        let nextTokenInfo = TokenInfo(nextToken, prevTokenInfo.Start + 1, prevTokenInfo.Column, prevTokenInfo.NewLine)

        let currentTokenInfo = this.btBuffer[this.btBufferPosition % MaxBackTrackableTokenAmount]
        this.btBuffer[this.btBufferPosition % MaxBackTrackableTokenAmount] <- nextTokenInfo

        if (this.btBufferCount <= this.btBufferPosition + 1) then
            this.btBuffer[(this.btBufferPosition + 1) % MaxBackTrackableTokenAmount] <- currentTokenInfo
            this.btBufferCount <- this.btBufferCount + 1
        else
            let diff = this.btBufferCount - (this.btBufferPosition + 1)
            OlyAssert.True(diff > 0)
            
            let startIndex = this.btBufferPosition + 1
            let endIndex = (startIndex + diff - 1)

            // Shift tokens in the back-track buffer
            let mutable j = endIndex + diff
            for i = startIndex to endIndex do
                this.btBuffer[j % MaxBackTrackableTokenAmount] <- this.btBuffer[i % MaxBackTrackableTokenAmount]
                j <- j - 1

            this.btBuffer[(this.btBufferPosition + 1) % MaxBackTrackableTokenAmount] <- currentTokenInfo
            this.btBufferCount <- this.btBufferCount + 1

        newPrevToken

type Parser<'T> = ParserState -> 'T

let inline liftOpt ([<InlineIfLambda>] p: _ -> _) state =
    Some(p state)

let inline isTokenNull (token: SyntaxToken) =
    obj.ReferenceEquals(token, null)

let scanToken (leadingTrivia: SyntaxToken) leadingTriviaWidth newLine state =
    let column = state.lexer.CurrentColumn
    let start = state.lexer.CurrentPosition
    let token = Lexer.scanToken state.lexer state.ct
    let newLine = newLine || token.IsPossibleNewLine
    if token.IsTriviaExceptEndOfSource then 
        let leadingTriviaWidth = leadingTriviaWidth + token.Width
        let leadingTrivia =
            if isTokenNull(leadingTrivia) then
                SyntaxToken.Token(token)
            else
                SyntaxToken.TokenWithTrivia(leadingTrivia, token, leadingTriviaWidth)
        scanToken leadingTrivia leadingTriviaWidth newLine state
    else
        if isTokenNull(leadingTrivia) then
            TokenInfo(SyntaxToken.Token(token), start, column, newLine)
        else
            TokenInfo(SyntaxToken.TokenWithTrivia(leadingTrivia, token, token.Width + leadingTrivia.FullWidth), start, column, newLine)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let peekTokenSkipTriviaImpl state =
    let info = 
        if state.btBufferPosition >= state.btBufferCount then
            for _ = 0 to state.btBufferPosition - state.btBufferCount + 1 do
                state.btBuffer[state.btBufferCount % MaxBackTrackableTokenAmount] <- scanToken Unchecked.defaultof<_> 0 false state
                state.btBufferCount <- state.btBufferCount + 1
            state.btBuffer.[state.btBufferPosition % MaxBackTrackableTokenAmount]
        else
            state.btBuffer.[state.btBufferPosition % MaxBackTrackableTokenAmount]
    state.start <- info.Start
    state.column <- info.Column
    state.newLine <- info.NewLine
#if DEBUG
    OlyAssert.True(state.column >= 0)
    OlyAssert.False(obj.ReferenceEquals(info.Token, null))
#endif
    state.peekedPosition <- state.btBufferPosition
    state.peekedToken <- info.Token
    info.Token
    
let inline peekTokenSkipTrivia state =
    if state.peekedPosition = state.btBufferPosition then
        state.peekedToken
    else
        peekTokenSkipTriviaImpl state

let inline isNextToken ([<InlineIfLambda>] predicate: Token -> bool) state =
    let token = peekTokenSkipTrivia state
    predicate token.RawToken

let isOffsides column (state: ParserState) =
    if state.willIgnore then
        false
    elif state.willAlign then
        if state.isFlexible then
            column < state.offside
        else
            column <> state.offside
    elif column <= state.offside then
        true
    else
        false

#if DEBUG
let alignAux (p: _ -> _) (state: ParserState) isFlexible (onOffsides: _ -> _ -> _) =
#else
let inline alignAux ([<InlineIfLambda>] p: _ -> _) (state: ParserState) isFlexible ([<InlineIfLambda>] onOffsides: _ -> _ -> _) =
#endif
    peekTokenSkipTrivia state |> ignore
    let prevOffside = state.offside
    let prevOffsideFlags = state.offsideFlags

    if isFlexible then
        state.offsideFlags <- state.offsideFlags ||| OffsideFlags.WillAlign ||| OffsideFlags.IsFlexible
    else
        state.offsideFlags <- (state.offsideFlags ||| OffsideFlags.WillAlign) &&& ~~~OffsideFlags.IsFlexible
    
    if isOffsides state.column state then
        onOffsides prevOffside prevOffsideFlags state
    else
        let res = p state
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        res

#if DEBUG
let alignWithRecoveryAux (recovery: _ -> _ -> _ -> _) (p: _ -> _) state isFlexible =
#else
let inline alignWithRecoveryAux ([<InlineIfLambda>] recovery: _ -> _ -> _ -> _) ([<InlineIfLambda>] p: _ -> _) state isFlexible =
#endif
    alignAux p state isFlexible (fun prevOffside prevOffsideFlags state -> 
        let column = state.column

        let amountOffsides = column - state.prevOffside
        let amountOffsides =
            if amountOffsides < 0 then column - state.offside
            else amountOffsides

        state.offside <- column
        let res = p state
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        recovery res amountOffsides state
    )

let inline tryFlex ([<InlineIfLambda>] p: _ -> _) state =
    peekTokenSkipTrivia state |> ignore
    let prevOffside = state.offside
    let prevOffsideFlags = state.offsideFlags

    state.offsideFlags <- state.offsideFlags ||| OffsideFlags.IsFlexible

    if isOffsides state.column state then
        None
    else
        let res = p state
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        res

/// Try to execute a parse function with offside alignment.
let inline tryAlign ([<InlineIfLambda>] p: _ -> _) state =
    alignAux p state false (fun prevOffside prevOffsideFlags state ->
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        None)

/// If the current offside is flexible, then return None.
/// Otherwise, try to execute a parse function with offside alignment.
let inline tryAlignIfNoFlex ([<InlineIfLambda>] p: _ -> _) (state: ParserState) =
    if state.isFlexible then
        None
    else
        tryAlign p state

let inline tryIfNoNewLine ([<InlineIfLambda>] p: _ -> _) state =
    if state.newLine then
        None
    else
        p state

let inline alignWithRecovery ([<InlineIfLambda>] recovery: _ -> _ -> _ -> _) ([<InlineIfLambda>] p: _ -> _) state =
    alignWithRecoveryAux recovery p state false

let inline tryFlexAlign ([<InlineIfLambda>] p: _ -> _) state =
    alignAux p state true (fun prevOffside prevOffsideFlags state ->
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        None)

#if DEBUG
let flexAlignWithRecovery (recovery: _ -> _ -> _ -> _) (p: _ -> _) state =
#else
let inline flexAlignWithRecovery ([<InlineIfLambda>] recovery: _ -> _ -> _ -> _) ([<InlineIfLambda>] p: _ -> _) state =
#endif
    alignWithRecoveryAux recovery p state true

#if DEBUG
let alignOrFlexAlignWithRecovery (recovery: _ -> _ -> _ -> _) (p: _ -> _) state =
#else
let inline alignOrFlexAlignWithRecovery ([<InlineIfLambda>] recovery: _ -> _ -> _ -> _) ([<InlineIfLambda>] p: _ -> _) state =
#endif
    alignWithRecoveryAux recovery p state state.isFlexible

let inline noAlign ([<InlineIfLambda>] p: _ -> _) state =
    peekTokenSkipTrivia state |> ignore
    let prevOffside = state.offside
    let prevOffsideFlags = state.offsideFlags

    state.offsideFlags <- state.offsideFlags &&& ~~~OffsideFlags.WillAlign

    let res = p state
    state.offside <- prevOffside
    state.offsideFlags <- prevOffsideFlags
    res

let inline tryOffside ([<InlineIfLambda>] p: _ -> _) state =
    match (peekTokenSkipTrivia state).RawToken with
    | Token.EndOfSource -> None
    | _ ->

    let prevOffside = state.offside
    let prevOffsideFlags = state.offsideFlags

    if isOffsides state.column state then
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        None
    else
        let column = state.column
        state.offside <- column

        // A new offside means nothing is flexible.
        state.offsideFlags <- (state.offsideFlags ||| OffsideFlags.WillAlign) &&& ~~~OffsideFlags.IsFlexible

        let res = p state
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        state.prevOffside <- column
        res
    
let inline ignoreOffside ([<InlineIfLambda>] p: _ -> _) state =
    let prevOffside = state.offside
    let prevOffsideFlags = state.offsideFlags

    state.offsideFlags <- state.offsideFlags ||| OffsideFlags.WillIgnore

    let res = p state
    state.offside <- prevOffside
    state.offsideFlags <- prevOffsideFlags
    res

[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let tryTokenProlog state =
    //System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
    let token = peekTokenSkipTrivia state
    state.btBufferPosition <- state.btBufferPosition + 1
    token

[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let tryTokenEpilog (token: SyntaxToken) state =
    let column = state.column
    if isOffsides column state then
        None
    else
        if state.willAlign then
            state.offsideFlags <- state.offsideFlags &&& ~~~OffsideFlags.WillAlign
        state.peekedPosition <- -1
        Some(token)

[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let tryTokenEpilog_nil (token: SyntaxToken) state =
    let column = state.column
    if isOffsides column state then
        Unchecked.defaultof<_>
    else
        if state.willAlign then
            state.offsideFlags <- state.offsideFlags &&& ~~~OffsideFlags.WillAlign
        state.peekedPosition <- -1
        token

let inline tryToken ([<InlineIfLambda>] predicate: _ -> _) state =
    let token = tryTokenProlog state
    if predicate token.RawToken then
        tryTokenEpilog token state
    else
        None

let inline tryToken_nil ([<InlineIfLambda>] predicate: _ -> _) state =
    let token = tryTokenProlog state
    if predicate token.RawToken then
        tryTokenEpilog_nil token state
    else
        Unchecked.defaultof<_>

let inline tryPeek ([<InlineIfLambda>] p: _ -> _) state =
    peekTokenSkipTrivia state |> ignore
    let prevPos = state.btBufferPosition
    let prevNoErrors = state.noErrors

    // -- Offside State
    let prevOffside = state.offside
    let prevOffsideFlags = state.offsideFlags
    // -- 

    state.noErrors <- true

    let res = p state
    state.btBufferPosition <- prevPos
    state.noErrors <- prevNoErrors

    // -- Offside State
    state.offside <- prevOffside
    state.offsideFlags <- prevOffsideFlags
    // --

    res

#if DEBUG
// We do not inline in Debug as the stack can become too large.
// Release seems to be fine as many optimizations are applied.
let backTrack p state =
#else
let inline backTrack ([<InlineIfLambda>] p: _ -> _) state =
#endif
    peekTokenSkipTrivia state |> ignore
    let prevPos = state.btBufferPosition

    // -- Offside State
    let prevOffside = state.offside
    let prevOffsideFlags = state.offsideFlags
    // -- 

    let res = p state
    match res with
    | None -> 
        state.btBufferPosition <- prevPos

        // -- Offside State
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        // --
    | _ -> ()
    res

#if DEBUG
// We do not inline in Debug as the stack can become too large.
// Release seems to be fine as many optimizations are applied.
let backTrack_nil p state =
#else
let inline backTrack_nil ([<InlineIfLambda>] p: _ -> _) state =
#endif
    peekTokenSkipTrivia state |> ignore
    let prevPos = state.btBufferPosition

    // -- Offside State
    let prevOffside = state.offside
    let prevOffsideFlags = state.offsideFlags
    // -- 

    let res = p state
    match box res with
    | null ->
        state.btBufferPosition <- prevPos

        // -- Offside State
        state.offside <- prevOffside
        state.offsideFlags <- prevOffsideFlags
        // --
    | _ -> ()
    res

let inline bt ([<InlineIfLambda>] p: _ -> 'T option) state : 'T option =
    backTrack p state

let inline bt_nil ([<InlineIfLambda>] p: _ -> 'T) state : 'T =
    backTrack_nil p state

let inline bt2 ([<InlineIfLambda>] p1: _ -> 'T1 option) ([<InlineIfLambda>] p2: _ -> 'T2 option) state =
    match backTrack p1 state with
    | Some _ as r1 ->
        match backTrack p2 state with
        | Some _ as r2 ->
            (r1, r2)
        | _ ->
            (r1, None)
    | _ ->
        (None, None)

let inline bt3 ([<InlineIfLambda>] p1: _ -> _) ([<InlineIfLambda>] p2: _ -> _) ([<InlineIfLambda>] p3: _ -> _) state =
    match backTrack p1 state with
    | Some _ as r1 ->
        match backTrack p2 state with
        | Some _ as r2 ->
            match backTrack p3 state with
            | Some _ as r3 ->
                (r1, r2, r3)
            | _ ->
                (r1, r2, None)
        | _ ->
            (r1, None, None)
    | _ ->
        (None, None, None)

let inline bt4 ([<InlineIfLambda>] p1: _ -> _) ([<InlineIfLambda>] p2: _ -> _) ([<InlineIfLambda>] p3: _ -> _) ([<InlineIfLambda>] p4: _ -> _) state =
    match backTrack p1 state with
    | Some _ as r1 ->
        match backTrack p2 state with
        | Some _ as r2 ->
            match backTrack p3 state with
            | Some _ as r3 ->
                match backTrack p4 state with
                | Some _ as r4 ->
                    (r1, r2, r3, r4)
                | _ ->
                    (r1, r2, r3, None)
            | _ ->
                (r1, r2, None, None)
        | _ ->
            (r1, None, None, None)
    | _ ->
        (None, None, None, None)

let inline bt5 ([<InlineIfLambda>] p1: _ -> _) ([<InlineIfLambda>] p2: _ -> _) ([<InlineIfLambda>] p3: _ -> _) ([<InlineIfLambda>] p4: _ -> _) ([<InlineIfLambda>] p5: _ -> _) state =
    match backTrack p1 state with
    | Some _ as r1 ->
        match backTrack p2 state with
        | Some _ as r2 ->
            match backTrack p3 state with
            | Some _ as r3 ->
                match backTrack p4 state with
                | Some _ as r4 ->
                    match backTrack p5 state with
                    | Some _ as r5 ->
                        (r1, r2, r3, r4, r5)
                    | _ ->
                        (r1, r2, r3, r4, None)
                | _ ->
                    (r1, r2, r3, None, None)
            | _ ->
                (r1, r2, None, None, None)
        | _ ->
            (r1, None, None, None, None)
    | _ ->
        (None, None, None, None, None)

let inline bt6 ([<InlineIfLambda>] p1: _ -> _) ([<InlineIfLambda>] p2: _ -> _) ([<InlineIfLambda>] p3: _ -> _) ([<InlineIfLambda>] p4: _ -> _) ([<InlineIfLambda>] p5: _ -> _) ([<InlineIfLambda>] p6: _ -> _) state =
    match backTrack p1 state with
    | Some _ as r1 ->
        match backTrack p2 state with
        | Some _ as r2 ->
            match backTrack p3 state with
            | Some _ as r3 ->
                match backTrack p4 state with
                | Some _ as r4 ->
                    match backTrack p5 state with
                    | Some _ as r5 ->
                        match backTrack p6 state with
                        | Some _ as r6 ->
                            (r1, r2, r3, r4, r5, r6)
                        | _ ->
                            (r1, r2, r3, r4, r5, None)
                    | _ ->
                        (r1, r2, r3, r4, None, None)
                | _ ->
                    (r1, r2, r3, None, None, None)
            | _ ->
                (r1, r2, None, None, None, None)
        | _ ->
            (r1, None, None, None, None, None)
    | _ ->
        (None, None, None, None, None, None)

/// Similar to 'bt', but more expensive, does not back-track offside rules and might back-track internal lexer state.
/// Should only be used in cases where the number of back-trackable tokens *could* exceed
/// the maximum amount.
/// These are the funcions that use this (keep this up-to-date):
///    tryParseParenthesisOrTupleOrLambdaExpression
let inline btLexer ([<InlineIfLambda>] p: _ -> _) state =
    peekTokenSkipTrivia state |> ignore

    let prevLexerStartPos = state.lexer.CurrentPosition
    let prevLexerEndPos = state.lexer.CurrentEndPosition
    let prevLexerColumn = state.lexer.CurrentColumn
    let prevBufferPos = state.btBufferPosition
    let prevBufferCount = state.btBufferCount
    let prevBuffer = state.btBuffer

    match state.buffers.TryPop() with
    | true, buffer ->
        state.btBuffer <- buffer
    | _ ->
        state.btBuffer <- Array.zeroCreate MaxBackTrackableTokenAmount
    prevBuffer.CopyTo(state.btBuffer.AsSpan())

    let res = p state

#if DEBUG
    // This will always happen in DEBUG for testing purposes.
    if true then
#else
    // Reset internal lexer state if we have too many tokens to back-track.
    if (state.btBufferCount - prevBufferCount) >= (MaxBackTrackableTokenAmount / 2) then
#endif
        state.lexer.SetCurrentLexemeRange(prevLexerStartPos, prevLexerEndPos)
        state.lexer.SetCurrentColumn(prevLexerColumn)
        state.btBufferPosition <- prevBufferPos
        state.btBufferCount <- prevBufferCount

        state.buffers.Push(state.btBuffer)
        state.btBuffer <- prevBuffer
    else
        state.buffers.Push(prevBuffer)

    res

let errorDo<'T when 'T :> ISyntaxNode> (error: SyntaxError, node: 'T) (state: ParserState) =
    if not state.noErrors then
        let diagnostics = state.diagnostics.GetOrCreateValue(node)
        diagnostics.Add(DiagnosticSyntax(error.Text, error.Code, DiagnosticSeverity.Error, node, error.OffsidesAmount, error.CanShrinkErrorRangeToEnd))

let error<'T when 'T :> ISyntaxNode> (error: SyntaxError, node: 'T) (state: ParserState) =
    if not state.noErrors then
        let diagnostics = state.diagnostics.GetOrCreateValue(node)
        diagnostics.Add(DiagnosticSyntax(error.Text, error.Code, DiagnosticSeverity.Error, node, error.OffsidesAmount, error.CanShrinkErrorRangeToEnd))
    Some node

let errorReturn<'T when 'T :> ISyntaxNode> (error: SyntaxError, node: 'T) (state: ParserState) =
    if not state.noErrors then
        let diagnostics = state.diagnostics.GetOrCreateValue(node)
        diagnostics.Add(DiagnosticSyntax(error.Text, error.Code, DiagnosticSeverity.Error, node, error.OffsidesAmount, error.CanShrinkErrorRangeToEnd))
    node

let inline alignRecover ([<InlineIfLambda>] p: _ -> _) state =
    alignWithRecovery (fun res offsideAmount state ->
        match res with
        | Some res ->
            error (Offsides(offsideAmount), res) state
        | _ ->
            None
    ) p state

#if DEBUG
let flexAlignRecover (p: _ -> _) state =
#else
let inline flexAlignRecover ([<InlineIfLambda>] p: _ -> _) state =
#endif
    flexAlignWithRecovery (fun res offsideAmount state -> 
        match res with
        | Some res ->
            error (Offsides(offsideAmount), res) state
        | _ ->
            None
    ) p state

let inline alignOrFlexAlignRecover ([<InlineIfLambda>] p: _ -> _) state =
    alignOrFlexAlignWithRecovery (fun res offsideAmount state ->
        match res with
        | Some res ->
            error (Offsides(offsideAmount), res) state
        | _ ->
            None
    ) p state

let inline LET state = tryToken (function Let -> true | _ -> false) state
let inline LET_EXCLAMATION state = tryToken (function LetExclamation -> true | _ -> false) state
let inline RETURN state = tryToken (function Return -> true | _ -> false) state
let inline GET state = tryToken (function Get -> true | _ -> false) state
let inline SET state = tryToken (function Set -> true | _ -> false) state
let inline MUTABLE state = tryToken (function Mutable -> true | _ -> false) state
let inline IDENTIFIER state = tryToken (function Identifier _ | ExplicitIdentifier _ -> true | _ -> false) state
let inline BASE state = tryToken (function Base -> true | _ -> false) state
let inline COLON state = tryToken (function Colon -> true | _ -> false) state
let inline DOT state = tryToken (function Dot -> true | _ -> false) state
let inline DOT_DOT_DOT state = tryToken (function DotDotDot -> true | _ -> false) state
let inline INT32 state = tryToken (function Int32Literal _ -> true | _ -> false) state
let inline EQUAL state = tryToken (function Equal -> true | _ -> false) state
let inline STAR state = tryToken (function Star -> true | _ -> false) state
let inline TRAIT state = tryToken (function Trait -> true | _ -> false) state
let inline WITH state = tryToken (function With -> true | _ -> false) state
let inline AND state = tryToken (function And -> true | _ -> false) state
let inline OR state = tryToken (function Or -> true | _ -> false) state
let inline SINGLE_QUOTATION state = tryToken (function SingleQuotation -> true | _ -> false) state
let inline DOUBLE_QUOTATION state = tryToken (function DoubleQuotation -> true | _ -> false) state
let inline COMMA state = tryToken (function Comma -> true | _ -> false) state
let inline LEFT_PARENTHESIS state = tryToken (function LeftParenthesis -> true | _ -> false) state
let inline RIGHT_PARENTHESIS state = tryToken (function RightParenthesis -> true | _ -> false) state
let inline LESS_THAN state = tryToken (function LessThan -> true | _ -> false) state
let inline GREATER_THAN state = tryToken (function GreaterThan -> true | _ -> false) state
let inline GREATER_THAN_GREATER_THAN state = tryToken (function GreaterThanGreaterThan -> true | _ -> false) state
let inline LEFT_ARROW state = tryToken (function LeftArrow -> true | _ -> false) state
let inline RIGHT_ARROW state = tryToken (function RightArrow -> true | _ -> false) state
let inline FAT_RIGHT_ARROW state = tryToken (function FatRightArrow -> true | _ -> false) state
let inline INT8_LITERAL state = tryToken (function Int8Literal _ -> true | _ -> false) state
let inline UINT8_LITERAL state = tryToken (function UInt8Literal _ -> true | _ -> false) state
let inline INT16_LITERAL state = tryToken (function Int16Literal _ -> true | _ -> false) state
let inline UINT16_LITERAL state = tryToken (function UInt16Literal _ -> true | _ -> false) state
let inline INT32_LITERAL state = tryToken (function Int32Literal _ -> true | _ -> false) state
let inline UINT32_LITERAL state = tryToken (function UInt32Literal _ -> true | _ -> false) state
let inline INT64_LITERAL state = tryToken (function Int64Literal _ -> true | _ -> false) state
let inline UINT64_LITERAL state = tryToken (function UInt64Literal _ -> true | _ -> false) state
let inline FLOAT32_LITERAL state = tryToken (function Float32Literal _ -> true | _ -> false) state
let inline FLOAT64_LITERAL state = tryToken (function Float64Literal _ -> true | _ -> false) state
let inline INTEGER_LITERAL state = tryToken (function IntegerLiteral _ -> true | _ -> false) state
let inline REAL_LITERAL state = tryToken (function RealLiteral _ -> true | _ -> false) state
let inline TRUE state = tryToken (function True -> true | _ -> false) state
let inline FALSE state = tryToken (function False -> true | _ -> false) state
let inline CHAR_LITERAL state = tryToken (function CharLiteral _ -> true | _ -> false) state
let inline STRING_LITERAL state = tryToken (function StringLiteral _ -> true | _ -> false) state
let inline END_OF_SOURCE state = tryToken (function EndOfSource -> true | _ -> false) state
let inline PLUS state = tryToken (function Plus -> true | _ -> false) state
let inline MINUS state = tryToken (function Plus -> true | _ -> false) state
let inline FORWARD_SLASH state = tryToken (function ForwardSlash -> true | _ -> false) state
let inline TYPE state = tryToken (function Type -> true | _ -> false) state
let inline ALIAS state = tryToken (function Alias -> true | _ -> false) state
let inline PUBLIC state = tryToken (function Public -> true | _ -> false) state
let inline PRIVATE state = tryToken (function Private -> true | _ -> false) state
let inline INTERNAL state = tryToken (function Internal -> true | _ -> false) state
let inline PROTECTED state = tryToken (function Protected -> true | _ -> false) state
let inline MODULE state = tryToken (function Module -> true | _ -> false) state
let inline NEW state = tryToken (function New -> true | _ -> false) state
let inline NEWTYPE state = tryToken (function Newtype -> true | _ -> false) state
let inline LEFT_CURLY_BRACKET state = tryToken (function LeftCurlyBracket -> true | _ -> false) state
let inline RIGHT_CURLY_BRACKET state = tryToken (function RightCurlyBracket -> true | _ -> false) state
let inline LEFT_BRACKET state = tryToken (function LeftBracket -> true | _ -> false) state
let inline RIGHT_BRACKET state = tryToken (function RightBracket -> true | _ -> false) state
let inline LEFT_BRACKET_INNER_PIPE state = tryToken (function LeftBracketInnerPipe -> true | _ -> false) state
let inline RIGHT_BRACKET_INNER_PIPE state = tryToken (function RightBracketInnerPipe -> true | _ -> false) state
let inline SEMI_COLON state = tryToken (function SemiColon -> true | _ -> false) state
let inline OPEN state = tryToken (function Open -> true | _ -> false) state
let inline HASH state = tryToken (function Hash -> true | _ -> false) state
let inline IF state = tryToken (function If -> true | _ -> false) state
let inline ELSE state = tryToken (function Else -> true | _ -> false) state
let inline STATIC state = tryToken (function Static -> true | _ -> false) state
let inline IMPORT state = tryToken (function Import -> true | _ -> false) state
let inline EXPORT state = tryToken (function Export -> true | _ -> false) state
let inline UNDERSCORE state = tryToken (function Underscore -> true | _ -> false) state
let inline WHEN state = tryToken (function When -> true | _ -> false) state
let inline WHERE state = tryToken (function Where -> true | _ -> false) state
let inline LESS_THAN_COLON state = tryToken (function LessThanColon -> true | _ -> false) state
let inline COLON_GREATER_THAN state = tryToken (function ColonGreaterThan -> true | _ -> false) state
let inline INHERITS state = tryToken (function Inherits -> true | _ -> false) state
let inline IMPLEMENTS state = tryToken (function Implements -> true | _ -> false) state
let inline INTERFACE state = tryToken (function Interface -> true | _ -> false) state
let inline CLASS state = tryToken (function Class -> true | _ -> false) state
let inline NAMESPACE state = tryToken (function Namespace -> true | _ -> false) state
let inline SHAPE state = tryToken (function Shape -> true | _ -> false) state
let inline STRUCT state = tryToken (function Struct -> true | _ -> false) state
let inline ENUM state = tryToken (function Enum -> true | _ -> false) state
let inline ATTRIBUTE state = tryToken (function Attribute -> true | _ -> false) state
let inline DEFAULT state = tryToken (function Default -> true | _ -> false) state
let inline WHILE state = tryToken (function While -> true | _ -> false) state
let inline FOR state = tryToken (function For -> true | _ -> false) state
let inline IN state = tryToken (function In -> true | _ -> false) state
let inline INTRINSIC state = tryToken (function Intrinsic -> true | _ -> false) state
let inline INLINE state = tryToken (function Inline -> true | _ -> false) state
let inline EXTENSION state = tryToken (function Extension -> true | _ -> false) state
let inline ABSTRACT state = tryToken (function Abstract -> true | _ -> false) state
let inline CONSTANT state = tryToken (function Constant -> true | _ -> false) state
let inline SEALED state = tryToken (function Sealed -> true | _ -> false) state
let inline OVERRIDES state = tryToken (function Overrides -> true | _ -> false) state
let inline MATCH state = tryToken (function Match -> true | _ -> false) state
let inline DO state = tryToken (function Do -> true | _ -> false) state
let inline ON state = tryToken (function On -> true | _ -> false) state
let inline CASE state = tryToken (function Case -> true | _ -> false) state
let inline PIPE state = tryToken (function Pipe -> true | _ -> false) state
let inline THIS state = tryToken (function This -> true | _ -> false) state
let inline NULL state = tryToken (function Null -> true | _ -> false) state
let inline NOT state = tryToken (function Not -> true | _ -> false) state
let inline UNCHECKED state = tryToken (function Unchecked -> true | _ -> false) state
let inline CHECKED state = tryToken (function Checked -> true | _ -> false) state
let inline UNMANAGED state = tryToken (function Unmanaged -> true | _ -> false) state
let inline MANAGED state = tryToken (function Managed -> true | _ -> false) state
let inline BLITTABLE state = tryToken (function Blittable -> true | _ -> false) state
let inline SCOPED state = tryToken (function Scoped -> true | _ -> false) state
let inline PURE state = tryToken (function Pure -> true | _ -> false) state
let inline PATTERN state = tryToken (function Pattern -> true | _ -> false) state
let inline IMPLICIT state = tryToken (function Implicit -> true | _ -> false) state
let inline REQUIRE state = tryToken (function Require -> true | _ -> false) state
let inline THROW state = tryToken (function Throw -> true | _ -> false) state
let inline TRY state = tryToken (function Try -> true | _ -> false) state
let inline CATCH state = tryToken (function Catch -> true | _ -> false) state
let inline FINALLY state = tryToken (function Finally -> true | _ -> false) state
let inline FIELD state = tryToken (function Field -> true | _ -> false) state

//
// These functions return 'nil' as to avoid Option allocations

let inline IDENTIFIER_nil state = tryToken_nil (function Identifier _ | ExplicitIdentifier _ -> true | _ -> false) state
let inline THIS_nil state = tryToken_nil (function This -> true | _ -> false) state
let inline BASE_nil state = tryToken_nil (function Base -> true | _ -> false) state

let inline tryTerminal state =
    tryToken (function
        | Comma
        | RightParenthesis
        | SemiColon
        | RightBracket
        | RightCurlyBracket
        | RightBracketInnerPipe
        | When
        | Else
        | LeftArrow
        | RightArrow 
        | FatRightArrow
        | Equal
        | Pipe -> true
        | _ -> false
    ) state
    
let tryPeekIdentifierOrOperationOrKeyword state = 
    tryPeek (tryToken (fun x -> x.IsIdentifierOrOperatorOrKeyword)) state

let tryOptionalSemiColon state =
    match bt SEMI_COLON state with
    | result when result.IsSome -> result
    | _ ->
        match bt tryPeekIdentifierOrOperationOrKeyword state with
        | result when result.IsSome -> Some(dummyToken())
        | _ ->
            None

let tryOptionalSemiColonWithTerminalRightBracket state =
    match bt SEMI_COLON state with
    | result when result.IsSome -> result
    | _ -> 
        if isNextToken (function RightBracket -> true | _ -> false) state then
            None
        else
            Some(dummyToken())

let tryPeekOffsideTerminal = ignoreOffside (tryPeek tryTerminal)

let parseAccessor state =
    match bt PUBLIC state with
    | Some(token) -> SyntaxAccessor.Public(token)
    | _ ->

    match bt PRIVATE state with
    | Some(token) -> SyntaxAccessor.Private(token)
    | _ ->

    match bt INTERNAL state with
    | Some(token) -> SyntaxAccessor.Internal(token)
    | _ ->

    match bt PROTECTED state with
    | Some(token) -> SyntaxAccessor.Protected(token)
    | _ ->

    SyntaxAccessor.None()

let tryRecoverableRightParenthesis state =
    match bt (flexAlignRecover RIGHT_PARENTHESIS) state with
    | Some(rightParenToken) -> rightParenToken |> Some
    | _ -> None

let tryLeftRightParenthesis state =
    match bt2 LEFT_PARENTHESIS tryRecoverableRightParenthesis state with
    | Some(leftParenToken), Some(rightParenToken) ->
        (leftParenToken, rightParenToken) |> Some
    | Some(leftParenToken), _ ->
        match bt tryPeekOffsideTerminal state with
        | Some _ ->
            errorDo(ExpectedTokenAfterToken(RightParenthesis, LeftParenthesis), leftParenToken) state
            (leftParenToken, dummyToken()) |> Some
        | _ ->
            None
    | _ ->
        None

let tryParseParenthesisOld tryParseNode state =
    match bt3 LEFT_PARENTHESIS tryParseNode tryRecoverableRightParenthesis state with
    | Some(leftParenToken), Some(node), Some(rightParenToken) ->
        (leftParenToken, node, rightParenToken) |> Some

    | Some(leftParenToken), Some(node), _ ->
        errorDo(ExpectedToken RightParenthesis, node) state
        (leftParenToken, node, dummyToken ()) |> Some

    | _ ->
        None

let tryRightCurlyBracket state =
    match bt (flexAlignRecover RIGHT_CURLY_BRACKET) state with
    | Some(rightCurlyBracketToken) -> rightCurlyBracketToken |> Some
    | _ -> None

let tryLeftRightCurlyBracket state =
    match bt2 LEFT_CURLY_BRACKET tryRightCurlyBracket state with
    | Some(leftCurlyBracketToken), Some(rightCurlyBracketToken) ->
        (leftCurlyBracketToken, rightCurlyBracketToken) |> Some
    | _ ->
        None

let tryRightBracket state =
    match bt (flexAlignRecover RIGHT_BRACKET) state with
    | Some(rightParenToken) -> rightParenToken |> Some
    | _ -> None

let tryRightBracketInnerPipe state =
    match bt (flexAlignRecover RIGHT_BRACKET_INNER_PIPE) state with
    | Some(pipeRightBracketToken) -> pipeRightBracketToken |> Some
    | _ -> None

let tryParseListWithSeparatorOld separatorToken nodeName errorNode tryParseNode state =
    let s = sp state

    match bt3 tryParseNode separatorToken (tryParseListWithSeparatorOld separatorToken nodeName errorNode tryParseNode) state with
    | Some(node), Some(separatorToken), Some(rest) ->
        SyntaxSeparatorList.List(node, separatorToken, rest, ep s state) |> Some

    | Some(node), Some(separatorToken), _ ->
        let result = SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.List(errorNode(dummyToken()), dummyToken(), SyntaxSeparatorList.Empty(), 0), ep s state)
        errorDo(ExpectedSyntaxAfterToken(nodeName, separatorToken.RawToken), separatorToken) state
        result |> Some

    | Some(node), _ , _ ->
        SyntaxSeparatorList.List(node, dummyToken(), SyntaxSeparatorList.Empty(), ep s state) |> Some

    | _ ->
        None

let parseSeparatorListOld separatorToken nodeName errorNode tryParseNode state =
    let s = sp state

    match bt3 tryParseNode separatorToken (tryParseListWithSeparatorOld separatorToken nodeName errorNode tryParseNode) state with
    | Some(node), Some(separatorToken), Some(rest) ->
        SyntaxSeparatorList.List(node, separatorToken, rest, ep s state)

    | Some(node), Some(separatorToken), _ ->
        let listNode = SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.Empty(), ep s state)
        errorDo (ExpectedSyntax "remaining nodes", listNode) state
        listNode

    | Some(node), _ , _ ->
        SyntaxSeparatorList.List(node, dummyToken(), SyntaxSeparatorList.Empty(), ep s state)

    | _ ->
        SyntaxSeparatorList.Empty()

let tryParseListWithSeparatorOptionalCommaAux willAlign tryParseNode state =
    let s = sp state

    let tryParse = 
        if willAlign then
            tryAlign tryParseNode
        else
            tryParseNode

    match bt2 tryParse COMMA state with
    | Some(node), Some(separatorToken) ->
        match tryParseListWithSeparatorOptionalCommaAux false tryParseNode state with
        | Some(rest) ->
            SyntaxSeparatorList.List(node, separatorToken, rest, ep s state) |> Some
        | _ ->
            errorDo (InvalidSyntax "',' is optional.", separatorToken) state
            SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.Empty(), ep s state) |> Some

    | Some(node), _ ->
        match tryParseListWithSeparatorOptionalCommaAux true tryParseNode state with
        | Some(rest) ->
            SyntaxSeparatorList.List(node, dummyToken(), rest, ep s state) |> Some
        | _ ->
            SyntaxSeparatorList.List(node, dummyToken(), SyntaxSeparatorList.Empty(), ep s state) |> Some

    | _ ->
        None

let tryParseListWithSeparatorOptionalComma tryParseNode state =
    tryParseListWithSeparatorOptionalCommaAux false tryParseNode state

let tryParseList tryParseNode state =
    let s = sp state

    match bt2 tryParseNode (tryParseList tryParseNode) state with
    | Some(node), Some(rest) ->
        SyntaxList.List(node, rest, ep s state) |> Some

    | Some(node), _ ->
        SyntaxList.List(node, SyntaxList.Empty(), ep s state) |> Some

    | _ ->
        None

// ** parenthesis, brackets, lists **

let tryParseListWithSeparator tryParseSeparatorToken nodeName tryParseNode tryErrorNode state =
    let s = sp state

    match bt3 tryParseNode tryParseSeparatorToken (tryParseListWithSeparator tryParseSeparatorToken nodeName tryParseNode tryErrorNode) state with
    | Some(node), Some(separatorToken), Some(rest) ->
        match rest with
        | SyntaxSeparatorList.Error ->
            match tryErrorNode (dummyToken()) with
            | Some errorNode ->
                SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.List(errorNode, dummyToken(), SyntaxSeparatorList.Empty(), 0), ep s state) |> Some
            | _ ->
                SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.Error, ep s state) |> Some
        | _ ->
            SyntaxSeparatorList.List(node, separatorToken, rest, ep s state) |> Some

    | Some(node), Some(separatorToken), _ ->
        let listNode = 
            match tryErrorNode (dummyToken()) with
            | Some errorNode ->
                SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.List(errorNode, dummyToken(), SyntaxSeparatorList.Empty(), 0), ep s state)
            | _ ->
                SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.Error, ep s state)
        errorDo(ExpectedSyntaxAfterToken(nodeName, separatorToken.RawToken), separatorToken) state
        listNode |> Some

    | Some(node), _ , _ ->
        SyntaxSeparatorList.List(node, dummyToken(), SyntaxSeparatorList.Empty(), ep s state) |> Some

    | _ ->
        None

let parseSeparatorList (previousTokenForErrorRecoveryOpt: SyntaxToken option) tryParseSeparatorToken nodeName tryParseNode tryErrorNode state =
    let s = sp state

    // Error recovery for the first item.
    let nodeOpt =
        if isNextToken (fun x -> x.IsIdentifierOrOperatorOrKeyword) state then 
            bt tryParseNode state
        else
            match previousTokenForErrorRecoveryOpt with
            | Some(previousToken) ->
                match bt (tryPeek tryParseSeparatorToken) state with
                | Some _ -> 
                    errorDo(ExpectedSyntaxAfterToken(nodeName, previousToken.RawToken), previousToken) state
                    tryErrorNode (dummyToken())
                | _ -> 
                    bt tryParseNode state
            | _ ->
                bt tryParseNode state

    match nodeOpt with
    | Some(node) ->
        match bt2 tryParseSeparatorToken (tryParseListWithSeparator tryParseSeparatorToken nodeName tryParseNode tryErrorNode) state with
        | Some(separatorToken), Some(rest) ->
            match rest with
            | SyntaxSeparatorList.Error ->
                match tryErrorNode (dummyToken()) with
                | Some errorNode ->
                    SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.List(errorNode, dummyToken(), SyntaxSeparatorList.Empty(), 0), ep s state)
                | _ ->
                    SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.Error, ep s state)
            | _ ->
                SyntaxSeparatorList.List(node, separatorToken, rest, ep s state)

        | Some(separatorToken), _ ->
            let listNode = 
                match tryErrorNode (dummyToken()) with
                | Some errorNode ->
                    SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.List(errorNode, dummyToken(), SyntaxSeparatorList.Empty(), 0), ep s state)
                | _ ->
                    SyntaxSeparatorList.List(node, separatorToken, SyntaxSeparatorList.Error, ep s state)
            errorDo(ExpectedSyntaxAfterToken(nodeName, separatorToken.RawToken), separatorToken) state
            listNode

        | _ ->
            SyntaxSeparatorList.List(node, dummyToken(), SyntaxSeparatorList.Empty(), ep s state)

    | _ ->
        SyntaxSeparatorList.Empty()

let parseList tryParseNode state =
    let s = sp state

    match bt2 tryParseNode (tryParseList tryParseNode) state with
    | Some(node), Some(rest) ->
        SyntaxList.List(node, rest, ep s state)

    | Some(node), _ ->
        SyntaxList.List(node, SyntaxList.Empty(), ep s state)

    | _ ->
        SyntaxList.Empty()

let tryParseParenthesis (parseNode: Parser<_>) state =
    // TODO: This is weird with the RIGHT_PARENTHESIS, why not tryRecoverableRightParenthesis?
    match bt2 LEFT_PARENTHESIS RIGHT_PARENTHESIS state with
    | Some _, Some _ -> None
    | Some(leftParenToken), _ ->
        let element = parseNode state
        match bt tryRecoverableRightParenthesis state with
        | Some(rightParenToken) ->
            Some(leftParenToken, element, rightParenToken)
        | _ ->
            errorDo (ExpectedToken RightParenthesis, element) state
            Some(leftParenToken, element, dummyToken())

    | _ ->
        None

let tryParseBrackets nodeName errorNode tryParseNode state =
    let s = sp state

    match bt3 LEFT_BRACKET tryParseNode tryRightBracket state with
    | Some(leftBracketToken), Some(node), Some(rightBracketToken) ->
        SyntaxBrackets.Brackets(leftBracketToken, node, rightBracketToken, ep s state) |> Some
    | Some(leftBracketToken), Some(node), _ ->
        error(ExpectedToken RightBracket, SyntaxBrackets.Brackets(leftBracketToken, node, dummyToken(), ep s state)) state
    | Some(leftBracketToken), _, _ ->
        errorDo(ExpectedSyntaxAfterToken($"'{nodeName}'", LeftBracket), leftBracketToken) state
        match bt tryRightBracket state with
        | Some(rightBracketToken) ->
            SyntaxBrackets.Brackets(leftBracketToken, errorNode(dummyToken()), rightBracketToken, ep s state) |> Some
        | _ ->
            SyntaxBrackets.Brackets(leftBracketToken, errorNode(dummyToken()), dummyToken(), ep s state) |> Some

    | _ ->
        None

let tryParseBracketsList tryParseNode state =
    let s = sp state

    match bt LEFT_BRACKET state with
    | Some(leftBracketToken) ->
        let elementList = parseList tryParseNode state
        match tryRightBracket state with
        | Some(rightBracketToken) ->
            SyntaxBrackets.Brackets(leftBracketToken, elementList, rightBracketToken, ep s state) |> Some
        | _ ->
            error(ExpectedToken RightBracket, SyntaxBrackets.Brackets(leftBracketToken, elementList, dummyToken(), ep s state)) state

    | _ ->
        None

let tryParseBracketInnerPipesList tryParseNode state =
    let s = sp state

    match bt LEFT_BRACKET_INNER_PIPE state with
    | Some(leftBracketInnerPipeToken) ->
        let elementList = parseList tryParseNode state
        match bt tryRightBracketInnerPipe state with
        | Some(rightBracketInnerPipeToken) ->
            SyntaxBracketInnerPipes.BracketInnerPipes(leftBracketInnerPipeToken, elementList, rightBracketInnerPipeToken, ep s state) |> Some
        | _ ->
            error(ExpectedToken RightBracket, SyntaxBracketInnerPipes.BracketInnerPipes(leftBracketInnerPipeToken, elementList, dummyToken(), ep s state)) state

    | _ ->
        None

let tryParseParenthesisSeparatorList tryParseSeparatorToken nodeName tryParseNode tryErrorNode state =
    match bt LEFT_PARENTHESIS state with
    | Some(leftParenToken) as previousTokenOpt ->
        let elementList = parseSeparatorList previousTokenOpt tryParseSeparatorToken nodeName (tryOffside tryParseNode) tryErrorNode state
        match bt tryRecoverableRightParenthesis state with
        | Some(rightParenToken) ->
            Some(leftParenToken, elementList, rightParenToken)
        | _ ->
            errorDo (ExpectedToken RightParenthesis, elementList) state
            Some(leftParenToken, elementList, dummyToken())

    | _ ->
        None

let tryParseBracketsSeparatorList tryParseSeparatorToken nodeName tryParseNode tryErrorNode state =
    let s = sp state

    match bt LEFT_BRACKET state with
    | Some(leftBracketToken) as previousTokenOpt ->
        let elementList = parseSeparatorList previousTokenOpt tryParseSeparatorToken nodeName (tryOffside tryParseNode) tryErrorNode state
        match bt tryRightBracket state with
        | Some(rightBracketToken) ->
            SyntaxBrackets.Brackets(leftBracketToken, elementList, rightBracketToken, ep s state) |> Some
        | _ ->
            error(ExpectedToken RightBracket, SyntaxBrackets.Brackets(leftBracketToken, elementList, dummyToken(), ep s state)) state

    | _ ->
        None

let tryParseBracketInnerPipesSeparatorList tryParseSeparatorToken nodeName tryParseNode tryErrorNode state =
    let s = sp state

    match bt LEFT_BRACKET_INNER_PIPE state with
    | Some(leftBracketInnerPipeToken) as previousTokenOpt ->
        let elementList = parseSeparatorList previousTokenOpt tryParseSeparatorToken nodeName (tryOffside tryParseNode) tryErrorNode state
        match bt tryRightBracketInnerPipe state with
        | Some(rightBracketInnerPipeToken) ->
            SyntaxBracketInnerPipes.BracketInnerPipes(leftBracketInnerPipeToken, elementList, rightBracketInnerPipeToken, ep s state) |> Some
        | _ ->
            error(ExpectedToken RightBracket, SyntaxBracketInnerPipes.BracketInnerPipes(leftBracketInnerPipeToken, elementList, dummyToken(), ep s state)) state

    | _ ->
        None

let tryParseCurlyBracketsSeparatorList tryParseSeparatorToken nodeName tryParseNode tryErrorNode state =
    let s = sp state

    match bt LEFT_CURLY_BRACKET state with
    | Some(leftCurlyBracketToken) as previousTokenOpt ->
        let elementList = parseSeparatorList previousTokenOpt tryParseSeparatorToken nodeName (tryOffside tryParseNode) tryErrorNode state
        match bt tryRightCurlyBracket state with
        | Some(rightCurlyBracketToken) ->
            SyntaxCurlyBrackets.CurlyBrackets(leftCurlyBracketToken, elementList, rightCurlyBracketToken, ep s state) |> Some
        | _ ->
            error(ExpectedToken RightBracket, SyntaxCurlyBrackets.CurlyBrackets(leftCurlyBracketToken, elementList, dummyToken(), ep s state)) state

    | _ ->
        None

// ************************************************

let tryParseOperatorAux isSpecial state =
    tryToken (function
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
        | GreaterThanGreaterThan
        | LessThanLessThan 
        | Not
        | Or
        | And -> true
        | LetExclamation 
        | Return 
        | Throw 
        | Pipe -> isSpecial
        | ExplicitIdentifier(_, ident, _) -> isSpecial && (ident = "[]" || ident = "[,]")
        | _ -> false) state

let tryParsePrefixOrInfixOperator state = tryParseOperatorAux false state
let tryParseOperator state = tryParseOperatorAux true state

let parseOperator state =
    match bt tryParseOperator state with
    | Some(operator) -> operator
    | _ ->

    match bt (tryToken (fun _ -> true)) state with
    | Some(invalidOperator) ->
        errorDo(UnexpectedToken(invalidOperator.RawToken), invalidOperator) state
        invalidOperator
    | _ ->
        dummyToken()

let tryParseIdentifierOrOperator state =
    let result = backTrack tryParseOperator state
    if result.IsSome then result
    else

    IDENTIFIER state

let parseIdentifierOrOperator state =
    match bt tryParseIdentifierOrOperator state with
    | Some(identOrOperator) -> identOrOperator
    | _ ->

    match bt (tryToken (fun _ -> true)) state with
    | Some(invalidIdentOrOperator) ->
        errorDo(UnexpectedToken(invalidIdentOrOperator.RawToken), invalidIdentOrOperator) state
        invalidIdentOrOperator
    | _ ->
        dummyToken()

let tryParseParenthesisOperator state =
    tryParseParenthesis parseOperator state

let tryParseParenthesisIdentifierOrOperator state =
    tryParseParenthesis parseIdentifierOrOperator state

let tryParseTypeOperator state =
    tryToken (function
        | Caret
        | Ampersand
        | Star -> true
        | _ -> false) state 

let parseTypeOperator state =
    match bt tryParseTypeOperator state with
    | Some(operator) -> operator
    | _ ->

    match bt (tryToken (fun _ -> true)) state with
    | Some(invalidOperator) ->
        errorDo(UnexpectedToken(invalidOperator.RawToken), invalidOperator) state
        invalidOperator
    | _ ->
        dummyToken()

let tryParseParenthesisTypeOperator state =
    tryParseParenthesis parseTypeOperator state

let tryParseIdentifierOrThisOrBase_nil state =
    let ident = bt_nil IDENTIFIER_nil state
    if not(isTokenNull(ident)) then
        ident
    else

    let thisToken = bt_nil THIS_nil state
    if not(isTokenNull(thisToken)) then
        thisToken
    else

    let baseToken = bt_nil BASE_nil state
    if not(isTokenNull(baseToken)) then
        baseToken
    else

    Unchecked.defaultof<_>

[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>]
let sp state =
    let token = peekTokenSkipTrivia state
    state.start - token.LeadingTriviaWidth

[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>]
let ep s state =
    let token = peekTokenSkipTrivia state
#if DEBUG
    OlyAssert.False(obj.ReferenceEquals(token, null))
    let value = (state.start - token.LeadingTriviaWidth) - s
    if value < 0 then
        failwith "Invalid full width from parsing."
    value
#else
    (state.start - token.LeadingTriviaWidth) - s
#endif

let tryParseName context state =
    let s = sp state

    let ident = bt_nil tryParseIdentifierOrThisOrBase_nil state
    if not(isTokenNull(ident)) then
        match bt (tryParseTypeArguments context) state with
        | Some(tyArgs) ->
            let genericName = SyntaxName.Generic(SyntaxName.Identifier(ident), tyArgs, ep s state)
            match bt2 DOT (tryParseName context) state with
            | Some(dotToken), Some(tail) ->
                SyntaxName.Qualified(genericName, dotToken, tail, ep s state) |> Some
            | Some(dotToken), _ ->
                error (ExpectedSyntaxAfterToken("an identifier", Dot), SyntaxName.Qualified(genericName, dotToken, SyntaxName.Identifier(dummyToken ()), ep s state)) state
            | _ ->

            SyntaxName.Generic(SyntaxName.Identifier(ident), tyArgs, ep s state) |> Some
        | _ ->

        match bt2 DOT (tryParseName context) state with
        | Some(dotToken), Some(rest) ->
            SyntaxName.Qualified(SyntaxName.Identifier(ident), dotToken, rest, ep s state) |> Some
        | Some(dotToken), _ ->
            error (ExpectedSyntaxAfterToken("an identifier", Dot), SyntaxName.Qualified(SyntaxName.Identifier(ident), dotToken, SyntaxName.Identifier(dummyToken ()), ep s state)) state
        | _ ->
            SyntaxName.Identifier(ident) |> Some

    else

    match bt tryParseParenthesisIdentifierOrOperator state with
    | Some(leftParenToken, identOrOperatorToken, rightParenToken) ->
        let operatorName = SyntaxName.Parenthesis(leftParenToken, identOrOperatorToken, rightParenToken, ep s state)
        match bt (tryParseTypeArguments context) state with
        | Some(tyArgs) ->
            SyntaxName.Generic(operatorName, tyArgs, ep s state) |> Some
        | _ ->

        operatorName |> Some
    | _ ->
     
    None

let tryParseLiteral state =
    let s = sp state

    match bt INT8_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.Int8(valueToken) |> Some
    | _ ->

    match bt UINT8_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.UInt8(valueToken) |> Some
    | _ ->

    match bt INT16_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.Int16(valueToken) |> Some
    | _ ->

    match bt UINT16_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.UInt16(valueToken) |> Some
    | _ ->

    match bt INT32_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.Int32(valueToken) |> Some
    | _ ->

    match bt UINT32_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.UInt32(valueToken) |> Some
    | _ ->

    match bt INT64_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.Int64(valueToken) |> Some
    | _ ->

    match bt UINT64_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.UInt64(valueToken) |> Some
    | _ ->

    match bt INTEGER_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.Integer(valueToken) |> Some
    | _ ->

    match bt FLOAT32_LITERAL state with
    | Some(valueToken) ->
        match valueToken.ValueText.Replace(" ", "").EndsWith(".f") with
        | true ->
            errorDo(ExpectedSyntaxAfterToken("digit", Dot), valueToken) state
            SyntaxLiteral.Float32(valueToken) |> Some
        | _ ->
            SyntaxLiteral.Float32(valueToken) |> Some
    | _ ->

    match bt FLOAT64_LITERAL state with
    | Some(valueToken) ->
        match valueToken.ValueText.EndsWith(".") with
        | true ->
            errorDo(ExpectedSyntaxAfterToken("digit", Dot), valueToken) state
            SyntaxLiteral.Float64(valueToken) |> Some
        | _ ->
            SyntaxLiteral.Float64(valueToken) |> Some
    | _ ->

    match bt REAL_LITERAL state with
    | Some(valueToken) ->
        match valueToken.ValueText.EndsWith(".") with
        | true ->
            errorDo(ExpectedSyntaxAfterToken("digit", Dot), valueToken) state
            SyntaxLiteral.Real(valueToken) |> Some
        | _ ->
            SyntaxLiteral.Real(valueToken) |> Some
    | _ ->

    match bt TRUE state with
    | Some(valueToken) ->
        SyntaxLiteral.Bool(valueToken) |> Some
    | _ ->

    match bt FALSE state with
    | Some(valueToken) ->
        SyntaxLiteral.Bool(valueToken) |> Some
    | _ ->

    match bt STRING_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.Utf16(valueToken) |> Some
    | _ ->

    match bt CHAR_LITERAL state with
    | Some(valueToken) ->
        SyntaxLiteral.Char16(valueToken) |> Some
    | _ ->

    match bt NULL state with
    | Some(nullToken) ->
        SyntaxLiteral.Null(nullToken) |> Some
    | _ ->

    match bt DEFAULT state with
    | Some(defaultToken) ->
        SyntaxLiteral.Default(defaultToken) |> Some
    | _ ->

    match bt2 UNCHECKED DEFAULT state with
    | Some(uncheckedToken), Some(defaultToken) ->
        SyntaxLiteral.UncheckedDefault(uncheckedToken, defaultToken, ep s state) |> Some
    | _ ->

    None

let tryParseBlittable state =
    match bt BLITTABLE state with
    | Some(blittableToken) ->
        SyntaxBlittable.Blittable(blittableToken) |> Some
    | _ ->
        None

let parseBlittableOptional state =
    if isNextToken (function Blittable -> true | _ -> false) state then
        match tryParseBlittable state with
        | Some(blittable) ->
            SyntaxBlittableOptional.Some(blittable)
        | _ ->
            SyntaxBlittableOptional.None()
    else
        SyntaxBlittableOptional.None()

let tryParseVariadicType state =
    let s = sp state

    match bt2 tryParseIdentifierOrOperator DOT_DOT_DOT state with
    | Some(ident), Some(dotDotDotToken) ->
        match bt3 LEFT_BRACKET (tryParseOffsideExpression SyntaxTreeContextLocal) tryRightBracket state with
        | Some(lessThanToken), Some(expr), Some(greaterThanToken) ->
            SyntaxType.VariadicIndexer(ident, dotDotDotToken, lessThanToken, expr, greaterThanToken, ep s state) |> Some
        | Some(lessThanToken), Some(expr), _ ->
            errorDo(ExpectedToken GreaterThan, expr) state
            SyntaxType.VariadicIndexer(ident, dotDotDotToken, lessThanToken, expr, dummyToken(), ep s state) |> Some
        | Some(lessThanToken), _, _ ->
            errorDo(ExpectedSyntaxAfterToken("constant expression", lessThanToken.RawToken), lessThanToken) state
            SyntaxType.VariadicIndexer(ident, dotDotDotToken, lessThanToken, SyntaxExpression.Error(dummyToken()), dummyToken(), ep s state) |> Some
        | _ ->
            SyntaxType.Variadic(ident, dotDotDotToken, ep s state) |> Some
    | _ ->
        None

let tryParseInputOrOutput state =
    let s = sp state

    match bt (tryParseParenthesisSeparatorList COMMA "type" tryParseTupleElement (fun x -> SyntaxTupleElement.Error x |> Some)) state with
    | Some(leftParenToken, tupleElementList, rightParenToken) ->
        SyntaxType.Tuple(leftParenToken, tupleElementList, rightParenToken, ep s state) |> Some
    | _ ->

    match bt UNDERSCORE state with
    | Some(underscoreToken) ->
        SyntaxType.WildCard(underscoreToken) |> Some
    | _ ->

    match bt tryParseVariadicType state with
    | Some(ty) -> 
        Some ty
    | _ ->

    match bt (tryParseName TypeParameterContext.Default) state with
    | Some(name) ->
        SyntaxType.Name(name) |> Some
    | _ ->

    match bt (tryParseCurlyBracketsSeparatorList tryOptionalSemiColon "expression" (tryParseOffsideExpression SyntaxTreeContext.TopLevel) (fun x -> SyntaxExpression.Error(x) |> Some)) state with
    | Some(curlyBrackets) ->
        SyntaxType.Shape(curlyBrackets) |> Some
    | _ ->

    match bt tryParseLiteral state with
    | Some(literal) ->
        SyntaxType.Literal(literal) |> Some
    | _ ->
                
    None

let errorMutableArrayType s mutableToken input state =
    let emptyBrackets = SyntaxBrackets.Brackets(dummyToken(), SyntaxList.Empty(), dummyToken(), 0)
    let result = SyntaxType.MutableArray(mutableToken, input, emptyBrackets, ep s state)
    errorDo(ExpectedSyntaxAfterToken("array type", mutableToken.RawToken), result) state
    result

let parseInputType s (mutableTokenOpt: SyntaxToken option) input state =
    match mutableTokenOpt with
    | Some(mutableToken) ->
        match bt (tryParseBracketsList COMMA) state with
        | Some(brackets) -> SyntaxType.MutableArray(mutableToken, input, brackets, ep s state)
        | _ -> errorMutableArrayType s mutableToken input state
    | _ ->

    match bt tryParseTypeOperator state with
    | Some(operator) ->
        SyntaxType.Postfix(input, operator, ep s state)
    | _ ->

    match bt (tryParseBracketsList COMMA) state with
    | Some(brackets) ->
        SyntaxType.Array(input, brackets, ep s state)
    | _ ->

    input

let tryParseType state =
    let s = sp state

    // TODO: Kind of a cheap way to error recover.
    let canRecover state =
        match bt4 RIGHT_ARROW LEFT_PARENTHESIS IDENTIFIER COLON state with
        | Some _, Some _, Some _, Some _ -> true
        | _ -> false

    match bt STATIC state with
    | Some(staticToken) ->
        let blittableOptional = parseBlittableOptional state
        match bt tryParseInputOrOutput state with
        | Some(input) ->
            let input = parseInputType s None input state
            match (tryPeek canRecover) state with
            | true -> 
                errorDo (ExpectedSyntax "function type", input) state
                Some(SyntaxType.FunctionPtr(staticToken, blittableOptional, input, dummyToken(), SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state))
            | _ ->
                match bt2 RIGHT_ARROW tryParseType state with
                | Some(rightArrowToken), Some(output) ->
                    SyntaxType.FunctionPtr(staticToken, blittableOptional, input, rightArrowToken, output, ep s state) |> Some
                | Some(rightArrowToken), _ ->
                    error (ExpectedSyntax "an output type annotation", SyntaxType.FunctionPtr(staticToken, blittableOptional, input, rightArrowToken, SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state)) state
                | _ ->

                errorDo (ExpectedSyntax "function type", input) state
                Some(SyntaxType.FunctionPtr(staticToken, blittableOptional, input, dummyToken(), SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state))
        | _ ->
            errorDo (ExpectedSyntaxAfterToken("function type", staticToken.RawToken), staticToken) state
            Some(SyntaxType.FunctionPtr(staticToken, blittableOptional, SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), dummyToken(), SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state))
    | _ ->

    match bt SCOPED state with
    | Some(scopedToken) ->
        match bt tryParseInputOrOutput state with
        | Some(input) ->
            let input = parseInputType s None input state
            match (tryPeek canRecover) state with
            | true -> 
                errorDo (ExpectedSyntax "function type", input) state
                Some(SyntaxType.ScopedFunction(scopedToken, input, dummyToken(), SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state))
            | _ ->
                match bt2 RIGHT_ARROW tryParseType state with
                | Some(rightArrowToken), Some(output) ->
                    SyntaxType.ScopedFunction(scopedToken, input, rightArrowToken, output, ep s state) |> Some
                | Some(rightArrowToken), _ ->
                    error (ExpectedSyntax "an output type annotation", SyntaxType.ScopedFunction(scopedToken, input, rightArrowToken, SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state)) state
                | _ ->

                errorDo (ExpectedSyntax "function type", input) state
                Some(SyntaxType.ScopedFunction(scopedToken, input, dummyToken(), SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state))
        | _ ->
            errorDo (ExpectedSyntaxAfterToken("function type", scopedToken.RawToken), scopedToken) state
            Some(SyntaxType.ScopedFunction(scopedToken, SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), dummyToken(), SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state))
    | _ ->

        let mutableTokenOpt = bt MUTABLE state
        match bt tryParseInputOrOutput state with
        | Some(input) ->
            let input = parseInputType s mutableTokenOpt input state
            match (tryPeek canRecover) state with
            | true -> 
                Some(input)
            | _ ->
                match bt2 RIGHT_ARROW tryParseType state with
                | Some(rightArrowToken), Some(output) ->
                    SyntaxType.Function(input, rightArrowToken, output, ep s state) |> Some
                | Some(rightArrowToken), _ ->
                    error (ExpectedSyntax "an output type annotation", SyntaxType.Function(input, rightArrowToken, SyntaxType.Name(SyntaxName.Identifier(dummyToken ())), ep s state)) state
                | _ ->

                Some(input)
        | _ ->
            match mutableTokenOpt with
            | Some(mutableToken) ->
                let input = SyntaxType.Error(dummyToken())
                errorMutableArrayType s mutableToken input state
                |> Some
            | _ ->

            None

let parseMutability state =
    match bt MUTABLE state with
    | Some(mutableToken) ->
        SyntaxMutability.Mutable(mutableToken)
    | _ ->

    SyntaxMutability.None()

let tryParseTypeAnnotation state =
    match bt2 COLON tryParseType state with
    | Some(colonToken), Some(ty) ->               
        (colonToken, ty) |> Some
    | Some(colonToken), _ ->
        // Better error recovery if we use a keyword
        match bt (tryToken (fun t -> t.IsKeyword)) state with
        | Some(token) ->
            errorDo(InvalidSyntax("Not a valid identifier."), token) state
            (colonToken, SyntaxType.Error(token)) |> Some
        | _ ->
            let result = (colonToken, SyntaxType.Error(dummyToken()))
            errorDo(ExpectedSyntaxAfterToken("type", Colon), colonToken) state
            result |> Some
    | _ ->

    None

let parseTypeAnnotation state =
    match bt tryParseTypeAnnotation state with
    | Some(_ as res) ->
        res
    | _ ->
        (dummyToken(), SyntaxType.Error(dummyToken()))

let tryParseReturnTypeAnnotation state =
    let s = sp state

    match bt tryParseTypeAnnotation state with
    | Some(colonToken, ty) ->
        SyntaxReturnTypeAnnotation.TypeAnnotation(colonToken, ty, ep s state) |> Some
    | _ ->
        None

let parseReturnTypeAnnotation state =
    match bt tryParseReturnTypeAnnotation state with
    | Some(tyAnnot) ->
        tyAnnot
    | _ ->

    SyntaxReturnTypeAnnotation.None()

let tryParseIdentifier state =
    let s = sp state

    // TODO: Simplify this let make it faster.
    // REVIEW: This is very similar to tryParseParameter, could we re-use anything from there?
    match bt IDENTIFIER state with
    | Some(identifierToken) ->
        match bt (tryPeek COLON) state with
        | Some _ ->
            let colonToken, ty = parseTypeAnnotation state
            SyntaxTupleElement.IdentifierWithTypeAnnotation(identifierToken, colonToken, ty, ep s state) |> Some
        | _ ->

        None
    | _ ->
        None

let tryParseTupleElement state =
    match bt tryParseIdentifier state with
    | Some res -> Some res
    | _ ->

    match bt tryParseType state with
    | Some(ty) ->
        SyntaxTupleElement.Type(ty) |> Some
    | _ ->
        None

let tryParseParameterIdentifier s attrs mutability state = 
    // TODO: Simplify this let make it faster.
    match bt IDENTIFIER state with
    | Some(identifierToken) ->
        match bt (tryPeek COLON) state with
        | Some _ ->
            let colonToken, ty = parseTypeAnnotation state
            SyntaxParameter.IdentifierWithTypeAnnotation(attrs, mutability, identifierToken, colonToken, ty, ep s state) |> Some
        | _ ->

        match bt (tryPeek RIGHT_ARROW) state with
        | Some _ ->
            SyntaxParameter.Identifier(attrs, mutability, identifierToken, ep s state) |> Some
        | _ ->

        match bt (tryPeek RIGHT_PARENTHESIS) state with
        | Some _ ->
            SyntaxParameter.Identifier(attrs, mutability, identifierToken, ep s state) |> Some
        | _ ->

        match bt (tryPeek COMMA) state with
        | Some _ ->
            SyntaxParameter.Identifier(attrs, mutability, identifierToken, ep s state) |> Some
        | _ ->

        None
    | _ ->
        None

let tryParseParameter state : SyntaxParameter option =
    let s = sp state

    let attrs = parseAttributes state
    let mutability = parseMutability state

    match bt (tryParseParameterIdentifier s attrs mutability) state with
    | Some(par) -> Some par
    | _ ->

    match bt tryParseType state with
    | Some(ty) ->
        match mutability with
        | SyntaxMutability.Mutable(mutableToken) ->
            // Note: This is a little special since 'mutable' can signify a mutable array type.
            //       To handle this ambiguity, we match on the type to see if it is an immutable array and then create a mutable array type.
            //       There are not many cases where we do this sort of thing and it can be dangerous because we throw away the old syntax.
            //       Throwing away the old syntax means we lose any diagnostics specifically on that node, however it should be fine for this case.
            match ty with
            | SyntaxType.Array(elementTy, brackets, _) ->
                SyntaxParameter.Type(attrs, SyntaxType.MutableArray(mutableToken, elementTy, brackets, ep s state - (attrs :> ISyntaxNode).FullWidth), ep s state)
                |> Some
            | _ ->
                let par = SyntaxParameter.IdentifierWithTypeAnnotation(attrs, mutability, dummyToken(), dummyToken(), ty, ep s state)
                errorReturn (InvalidSyntax "'mutable' not expected for a qualified name.", mutability) state
                |> ignore
                par |> Some
        | _ ->
            SyntaxParameter.Type(attrs, ty, ep s state) |> Some
    | _ ->

    match mutability with
    | SyntaxMutability.Mutable _ ->
        error (ExpectedSyntaxAfterToken("identifier for parameter", Mutable), SyntaxParameter.IdentifierWithTypeAnnotation(attrs, mutability, dummyToken(), dummyToken(), SyntaxType.Error(dummyToken()), ep s state)) state
    | _ ->

    match attrs with
    | SyntaxAttributes.Empty _ ->
        None
    | _ ->
        error (ExpectedSyntaxAfterSyntax("parameter", "attributes"), SyntaxParameter.IdentifierWithTypeAnnotation(attrs, mutability, dummyToken(), dummyToken(), SyntaxType.Error(dummyToken()), ep s state)) state

let tryParseParameters state =
    let s = sp state

    match bt tryLeftRightParenthesis state with
    | Some(leftParenthesisToken, rightParenthesisToken) ->
        SyntaxParameters.Parameters(leftParenthesisToken, SyntaxSeparatorList.Empty(), rightParenthesisToken, ep s state) |> Some
    | _ ->

    match bt3 LEFT_PARENTHESIS (tryParseListWithSeparatorOld COMMA "parameter" (SyntaxParameter.Error) tryParseParameter) tryRecoverableRightParenthesis state with
    | Some(leftParenthesisToken), Some(parameterList), Some(rightParenthesisToken) ->
        SyntaxParameters.Parameters(leftParenthesisToken, parameterList, rightParenthesisToken, ep s state) |> Some

    | Some(leftParenthesisToken), Some(parameterList), _ ->
        error (ExpectedToken RightParenthesis, SyntaxParameters.Parameters(leftParenthesisToken, parameterList, dummyToken (), ep s state)) state

    | _ ->
        None

let parseParameters state =
    match bt tryParseParameters state with
    | Some(pars) -> pars
    | _ -> SyntaxParameters.Empty()

let tryParseConstraint state =
    let s = sp state

    match bt NULL state with
    | Some(nullToken) ->
        SyntaxConstraint.Null(nullToken) |> Some
    | _ ->

    match bt STRUCT state with
    | Some(structToken) ->
        SyntaxConstraint.Struct(structToken) |> Some
    | _ ->

    match bt2 NOT STRUCT state with
    | Some(notToken), Some(structToken) ->
        SyntaxConstraint.NotStruct(notToken, structToken, ep s state) |> Some
    | Some(notToken), _ ->
        errorDo(ExpectedTokenAfterToken(Struct, Not), notToken) state
        SyntaxConstraint.NotStruct(notToken, dummyToken(), ep s state) |> Some
    | _ ->

    match bt UNMANAGED state with
    | Some(unmanagedToken) ->
        SyntaxConstraint.Unmanaged(unmanagedToken) |> Some
    | _ ->

    match bt SCOPED state with
    | Some(scopedToken) ->
        SyntaxConstraint.Scoped(scopedToken) |> Some
    | _ ->

    match bt2 CONSTANT tryParseType state with
    | Some(constantToken), Some(ty) ->
        SyntaxConstraint.ConstantType(constantToken, ty, ep s state) |> Some
    | Some(constantToken), _ ->
        errorDo(ExpectedSyntaxAfterToken("type", constantToken.RawToken), constantToken) state
        SyntaxConstraint.ConstantType(constantToken, SyntaxType.Error(dummyToken()), ep s state) |> Some
    | _ ->

    match bt tryParseType state with
    | Some(ty) ->
        SyntaxConstraint.Type(ty) |> Some
    | _ ->

    None

let tryParseConstraints state =
    let s = sp state

    match bt4 WHERE tryParseType COLON (tryParseListWithSeparatorOld COMMA "constraint" (SyntaxConstraint.Error) tryParseConstraint) state with
    | Some(whereToken), Some(ty), Some(colonToken), Some(constrs) ->
        SyntaxConstraintClause.ConstraintClause(whereToken, ty, colonToken, constrs, ep s state) |> Some
    | Some(whereToken), Some(ty), Some(colonToken), _ ->
        errorDo(ExpectedSyntaxAfterToken("constraints", Colon), colonToken) state
        SyntaxConstraintClause.ConstraintClause(whereToken, ty, colonToken, SyntaxSeparatorList.Empty(), ep s state) |> Some
    | Some(whereToken), Some(ty), _, _ ->
        errorDo(ExpectedTokenAfterSyntax(Colon, "type"), ty) state
        SyntaxConstraintClause.ConstraintClause(whereToken, ty, dummyToken(), SyntaxSeparatorList.Empty(), ep s state) |> Some
    | Some(whereToken), _, _, _ ->
        let node = errorReturn(ExpectedSyntaxAfterToken("type", Where), SyntaxConstraintClause.ConstraintClause(whereToken, SyntaxType.Error(dummyToken()), dummyToken(), SyntaxSeparatorList.Empty(), ep s state)) state
        Some node
    | _ ->
        None

let parseConstraintClauseList state =
    parseSeparatorListOld tryOptionalSemiColon "constraint clause" (SyntaxConstraintClause.Error) tryParseConstraints state

let parseTypeParameter state =
    match bt tryParseType state with
    | Some(ty) -> ty
    | _ -> errorReturn (ExpectedSyntax "type parameter", SyntaxType.Error(dummyToken())) state

[<RequireQualifiedAccess>]
type TypeParameterContext =
    | Default
    | Operator

let tryParseTypeParameters context state =
    let s = sp state

    match bt LESS_THAN state with
    | Some(lessThanToken) ->
        let requireTokenOpt = bt REQUIRE state

        match bt (tryParseListWithSeparatorOld COMMA "type parameter" SyntaxType.Error (liftOpt parseTypeParameter)) state with
        | Some(typeParameterList) ->
            match bt GREATER_THAN state with
            | Some(greaterThanToken) ->
                match requireTokenOpt with
                | Some(requireToken) ->
                    SyntaxTypeParameters.RequireTypeParameters(lessThanToken, requireToken, typeParameterList, greaterThanToken, ep s state) |> Some
                | _ ->
                    SyntaxTypeParameters.TypeParameters(lessThanToken, typeParameterList, greaterThanToken, ep s state) |> Some
            | _ ->

            if context = TypeParameterContext.Operator then
                // We return 'None' instead of showing an error because '<' is an operator.
                None
            else
                let tyPars = 
                    match requireTokenOpt with
                    | Some(requireToken) ->
                        SyntaxTypeParameters.RequireTypeParameters(lessThanToken, requireToken, typeParameterList, dummyToken(), ep s state)
                    | _ ->
                        SyntaxTypeParameters.TypeParameters(lessThanToken, typeParameterList, dummyToken(), ep s state)
                errorDo(ExpectedToken GreaterThan, tyPars) state
                tyPars |> Some
        | _ ->

        None
    | _ ->
            
    None

let parseTypeParameters context state =
    match bt (tryParseTypeParameters context) state with
    | Some(tyPars) -> tyPars
    | _ -> SyntaxTypeParameters.Empty()

let tryParseTypeArguments context state =
    let s = sp state

    match bt LESS_THAN state with
    | Some(lessThanToken) ->
        match bt (tryParseListWithSeparatorOld COMMA "type argument" (SyntaxType.Error) tryParseType) state with
        | Some(typeArgumentList) ->
            match bt GREATER_THAN state with
            | Some(greaterThanToken) ->
                SyntaxTypeArguments.TypeArguments(lessThanToken, typeArgumentList, greaterThanToken, ep s state) |> Some
            | _ ->

            match bt GREATER_THAN_GREATER_THAN state with
            | Some(greaterThanGreaterThanToken) ->
                let greaterThanToken = state.SplitGreaterThanGreaterThan(greaterThanGreaterThanToken)
                SyntaxTypeArguments.TypeArguments(lessThanToken, typeArgumentList, greaterThanToken, ep s state) |> Some
            | _ ->

            if context = TypeParameterContext.Operator then
                // We return 'None' instead of showing an error because '<' is an operator.
                None
            else
                errorDo(ExpectedToken GreaterThan, typeArgumentList) state
                SyntaxTypeArguments.TypeArguments(lessThanToken, typeArgumentList, dummyToken(), ep s state) |> Some
        | _ ->

        None
    | _ ->
            
    None

let parseTypeArguments context state =
    match bt (tryParseTypeArguments context) state with
    | Some(tyArgs) -> tyArgs
    | _ -> SyntaxTypeArguments.Empty()

let tryParseLetPatternBinding context state =
    if isNextToken (function Let -> true | _ -> false) state then
        let s = sp state

        match bt2 LET (tryParsePattern true) state with
        | Some(letToken), Some(pat) ->
            match pat with
            | SyntaxPattern.Name(SyntaxName.Identifier _) ->
                // This is handled in value declaration.
                None
            | _ ->
                match bt2 EQUAL (tryParseOffsideExpression context) state with
                | Some(equalToken), Some(expr) ->
                    SyntaxLetPatternBinding.Binding(letToken, pat, equalToken, expr, ep s state) |> Some
                | _ ->
                    None
        | _ ->
            None
    else
        None

let tryParseBindingDeclaration state =
    let s = sp state

    // Identifiers
    match bt IDENTIFIER state with
    | Some(identifierToken) ->
        let tyPars = parseTypeParameters TypeParameterContext.Default state
        let pars = parseParameters state
        let tyAnnot = parseReturnTypeAnnotation state
        let constrClauseList = parseConstraintClauseList state
        match tyPars, pars, constrClauseList with
        | SyntaxTypeParameters.Empty _, SyntaxParameters.Empty _, SyntaxSeparatorList.Empty _ ->
            SyntaxBindingDeclaration.Value(identifierToken, tyAnnot, ep s state) |> Some
        | _ ->
            SyntaxBindingDeclaration.Function(SyntaxFunctionName.Identifier(identifierToken), tyPars, pars, tyAnnot, constrClauseList, ep s state) |> Some
    | _ ->

    // Operators
    match bt tryParseParenthesisOperator state with
    | Some(leftParenToken, operatorToken, rightParenToken) ->
        let funcName = SyntaxFunctionName.Parenthesis(leftParenToken, operatorToken, rightParenToken, ep s state)
        let tyPars = parseTypeParameters TypeParameterContext.Default state
        let pars = parseParameters state
        let returnTyAnnot = parseReturnTypeAnnotation state
        let constrClauseList = parseConstraintClauseList state

        let result = SyntaxBindingDeclaration.Function(funcName, tyPars, pars, returnTyAnnot, constrClauseList, ep s state)
        match tyPars, pars, constrClauseList with
        | SyntaxTypeParameters.Empty _, SyntaxParameters.Empty _, SyntaxSeparatorList.Empty _ ->
            errorDo(ExpectedSyntax("parameters"), result) state
            result |> Some
        | _ ->
            result |> Some
    | _ ->

    match bt2 NEW tryParseParameters state with
    | Some(newToken), Some(pars) ->
        SyntaxBindingDeclaration.New(newToken, pars, ep s state) |> Some
    | Some(newToken), _ ->
        error(ExpectedSyntaxAfterToken("parameters", New), SyntaxBindingDeclaration.New(newToken, SyntaxParameters.Parameters(dummyToken(), SyntaxSeparatorList.Empty(), dummyToken(), 0), ep s state)) state
    | _ ->

    None

let parseLambdaKind state =
    match bt STATIC state with
    | Some(staticToken) -> 
        SyntaxLambdaKind.Static(staticToken)
    | _ ->
        SyntaxLambdaKind.None()

let tryParseValueDeclarationNewPremodifier state =
    match bt NEW state with
    | Some(newToken) ->
        // Lookahead to disambiguate.
        match bt (tryPeek (ignoreOffside LEFT_PARENTHESIS)) state with
        | Some _ -> None
        | _ ->
            SyntaxValueDeclarationPremodifier.New(newToken) |> Some
    | _ ->
        
    None

let tryParseValueDeclarationPremodifier state =
    match bt STATIC state with
    | Some(staticToken) ->
        SyntaxValueDeclarationPremodifier.Static(staticToken) |> Some
    | _ ->

    match bt OVERRIDES state with
    | Some(overridesToken) ->
        SyntaxValueDeclarationPremodifier.Overrides(overridesToken) |> Some
    | _ ->

    match bt ABSTRACT state with
    | Some(abstractToken) ->
        SyntaxValueDeclarationPremodifier.Abstract(abstractToken) |> Some
    | _ ->

    match bt DEFAULT state with
    | Some(defaultToken) ->
        SyntaxValueDeclarationPremodifier.Default(defaultToken) |> Some
    | _ ->

    match bt MUTABLE state with
    | Some(mutableToken) ->
        SyntaxValueDeclarationPremodifier.Mutable(mutableToken) |> Some
    | _ ->

    match bt tryParseValueDeclarationNewPremodifier state with
    | Some(result) -> Some(result)
    | _ ->

    None

let tryParseValueDeclarationPostmodifier state =
    match bt MUTABLE state with
    | Some(mutableToken) ->
        SyntaxValueDeclarationPostmodifier.Mutable(mutableToken) |> Some
    | _ ->

    None

let parseValueDeclarationPremodifierList =
    parseList tryParseValueDeclarationPremodifier

let parseValueDeclarationPostmodifierList =
    parseList tryParseValueDeclarationPostmodifier

let tryParseValueDeclarationKind state =
    let s = sp state

    match bt LET state with
    | Some(letToken) ->
        SyntaxValueDeclarationKind.Let(letToken) |> Some
    | _ ->

    match bt LET_EXCLAMATION state with
    | Some(letExclamationToken) ->
        SyntaxValueDeclarationKind.LetBind(letExclamationToken) |> Some
    | _ ->

    match bt CONSTANT state with
    | Some(constantToken) ->
        SyntaxValueDeclarationKind.Constant(constantToken) |> Some
    | _ ->

    match bt PATTERN state with
    | Some(patternToken) ->
        SyntaxValueDeclarationKind.Pattern(patternToken) |> Some
    | _ ->

    match bt FIELD state with
    | Some(fieldToken) ->
        SyntaxValueDeclarationKind.Field(fieldToken) |> Some
    | _ ->

    None

let parseValueDeclarationKind state =
    match bt tryParseValueDeclarationKind state with
    | Some(kind) -> kind
    | _ -> SyntaxValueDeclarationKind.None()

let tryParseEntityExtendsTypeIfNoNewLine state =
    match bt (tryIfNoNewLine tryParseType) state with
    | Some(ty) ->
        SyntaxExtends.Type(ty) |> Some
    | _ ->
        None

let tryParseEntityExtendsInherits state =
    let s = sp state

    match bt2 INHERITS (tryParseListWithSeparatorOld COMMA "type" (SyntaxType.Error) tryParseType) state with
    | Some(inheritsToken), Some(tys) ->
        SyntaxExtends.Inherits(inheritsToken, tys, ep s state) |> Some
    | Some(inheritsToken), _ ->
        error(ExpectedSyntaxAfterToken("types", Inherits), SyntaxExtends.Inherits(inheritsToken, SyntaxSeparatorList.Empty(), ep s state)) state
    | _ ->

    // Smarter error recovery
    match bt2 COLON (tryParseListWithSeparatorOld COMMA "type" (SyntaxType.Error) tryParseType) state with
    | Some(colonToken), Some(tys) ->
        let colonToken = errorReturn (ExpectedToken Inherits, colonToken) state
        SyntaxExtends.Inherits(colonToken, tys, ep s state) |> Some
    | Some(colonToken), _ ->
        let colonToken = errorReturn (ExpectedToken Inherits, colonToken) state
        SyntaxExtends.Inherits(colonToken, SyntaxSeparatorList.Empty(), ep s state) |> Some
    | _ ->      

    None

let tryParseEntityImplements state =
    let s = sp state

    match bt2 IMPLEMENTS (tryParseListWithSeparatorOld COMMA "type" (SyntaxType.Error) tryParseType) state with
    | Some(implementsToken), Some(ents) ->
        SyntaxImplements.Implements(implementsToken, ents, ep s state) |> Some
    | Some(implementsToken), _ ->
        error(ExpectedSyntaxAfterToken("types", Implements), SyntaxImplements.Implements(implementsToken, SyntaxSeparatorList.Empty(), ep s state)) state
    | _ ->

    // Smarter error recovery
    match bt2 COLON (tryParseListWithSeparatorOld COMMA "type" (SyntaxType.Error) tryParseType) state with
    | Some(colonToken), Some(ents) ->
        let colonToken = errorReturn (ExpectedToken Implements, colonToken) state
        SyntaxImplements.Implements(colonToken, ents, ep s state) |> Some
    | Some(colonToken), _ ->
        let colonToken = errorReturn (ExpectedToken Implements, colonToken) state
        SyntaxImplements.Implements(colonToken, SyntaxSeparatorList.Empty(), ep s state) |> Some
    | _ ->    

    None

let tryParseTypeDeclarationKind state =
    let s = sp state

    match bt ALIAS state with
    | Some(aliasToken) ->
        SyntaxTypeDeclarationKind.Alias(aliasToken) |> Some
    | _ ->

    match bt CLASS state with
    | Some(classToken) ->
        SyntaxTypeDeclarationKind.Class(classToken) |> Some
    | _ ->

    match bt INTERFACE state with
    | Some(interfaceToken) ->
        SyntaxTypeDeclarationKind.Interface(interfaceToken) |> Some
    | _ ->

    match bt ABSTRACT state with
    | Some(abstractToken) ->
        match bt CLASS state with
        | Some(classToken) ->
            SyntaxTypeDeclarationKind.AbstractClass(abstractToken, classToken, ep s state) |> Some
        | _ ->

        match bt2 DEFAULT CLASS state with
        | Some(defaultToken), Some(classToken) ->
            SyntaxTypeDeclarationKind.AbstractDefaultClass(abstractToken, defaultToken, classToken, ep s state) |> Some
        | _ ->
            // We immediately return None as 'abstract' and 'abstract default' will be picked up when parsing members.
            // This will back-track.
            None
    | _ ->

    match bt SEALED state with
    | Some(sealedToken) ->
        match bt INTERFACE state with
        | Some(interfaceToken) ->
            SyntaxTypeDeclarationKind.SealedInterface(sealedToken, interfaceToken, ep s state) |> Some
        | _ ->

        errorDo(ExpectedTokenAfterToken(Interface, sealedToken.RawToken), sealedToken) state
        SyntaxTypeDeclarationKind.SealedInterface(sealedToken, dummyToken(), ep s state) |> Some
    | _ ->

    match bt MODULE state with
    | Some(moduleToken) ->
        SyntaxTypeDeclarationKind.Module(moduleToken) |> Some
    | _ ->

    match bt SHAPE state with
    | Some(shapeToken) ->
        SyntaxTypeDeclarationKind.Shape(shapeToken) |> Some
    | _ ->

    match bt STRUCT state with
    | Some(structToken) ->
        SyntaxTypeDeclarationKind.Struct(structToken) |> Some
    | _ ->

    match bt ENUM state with
    | Some(enumToken) ->
        SyntaxTypeDeclarationKind.Enum(enumToken) |> Some
    | _ ->

    match bt EXTENSION state with
    | Some(extensionToken) ->
        SyntaxTypeDeclarationKind.Extension(extensionToken) |> Some
    | _ ->

    match bt NEWTYPE state with
    | Some(newtypeToken) ->
        SyntaxTypeDeclarationKind.Newtype(newtypeToken) |> Some
    | _ ->

    None

let tryParseTypedExpression s expr state =
    match bt2 COLON tryParseType state with
    | Some(colonToken), Some(ty) ->
        SyntaxExpression.Typed(expr, colonToken, ty, ep s state) |> Some
    | Some(colonToken), _ ->
        error(ExpectedSyntaxAfterToken("type annotation", Colon), SyntaxExpression.Typed(expr, colonToken, SyntaxType.Error(dummyToken()), ep s state)) state
    | _ ->
        
    None

let tryParseOpenAttribute state =
    match bt OPEN state with
    | Some(openToken) ->
        SyntaxAttribute.AutoOpen(openToken) |> Some
    | _ ->

    None

let tryParseBlittableAttribute state =
    match tryParseBlittable state with
    | Some(blittable) ->
        SyntaxAttribute.Blittable(blittable) |> Some
    | _ ->

    None

let tryParsePureAttribute state =
    match bt PURE state with
    | Some(pureToken) ->
        SyntaxAttribute.Pure(pureToken) |> Some
    | _ ->

    None

let tryParseNullAttribute state =
    match bt NULL state with
    | Some(nullToken) ->
        SyntaxAttribute.Nullable(nullToken) |> Some
    | _ ->

    None

let tryParseIntrinsicAttribute state =
    let s = sp state

    match bt4 INTRINSIC LEFT_PARENTHESIS STRING_LITERAL tryRecoverableRightParenthesis state with
    | Some(intrinsicToken), Some(leftParenthesisToken), Some(literal), Some(rightParenthesisToken) ->
        SyntaxAttribute.Intrinsic(intrinsicToken, leftParenthesisToken, literal, rightParenthesisToken, ep s state) |> Some
    | Some(intrinsicToken), Some(leftParenthesisToken), Some(literal), _ ->
        errorDo (ExpectedSyntaxAfterSyntax("')' token", "string literal"), literal) state
        SyntaxAttribute.Intrinsic(intrinsicToken, leftParenthesisToken, literal, dummyToken(), ep s state) |> Some
    | Some(intrinsicToken), Some(leftParenthesisToken), _, _ ->
        errorDo (ExpectedSyntaxAfterToken("string literal", LeftParenthesis), leftParenthesisToken) state
        match bt tryRecoverableRightParenthesis state with
        | Some(rightParenthesisToken) ->
            SyntaxAttribute.Intrinsic(intrinsicToken, leftParenthesisToken, dummyToken(), rightParenthesisToken, ep s state) |> Some
        | _ ->
            SyntaxAttribute.Intrinsic(intrinsicToken, leftParenthesisToken, dummyToken(), dummyToken(), ep s state) |> Some
    | Some(intrinsicToken), _, _, _ ->
        errorDo (ExpectedSyntaxAfterToken("'(' token", Intrinsic), intrinsicToken) state
        SyntaxAttribute.Intrinsic(intrinsicToken, dummyToken(), dummyToken(), dummyToken(), ep s state) |> Some
    | _ ->
        None

let tryParseInlineAttribute state =
    let s = sp state

    match bt2 INLINE (tryParseParenthesisOld IDENTIFIER) state with
    | Some(inlineToken), Some(leftParenToken, identToken, rightParenToken) ->
        match identToken.ValueText with
        | "never"
        | "always"
        | "stack" ->
            SyntaxAttribute.Inline(inlineToken, leftParenToken, identToken, rightParenToken, ep s state) |> Some
        | _ ->
            errorDo (InvalidSyntax("inline argument"), identToken) state
            SyntaxAttribute.Inline(inlineToken, leftParenToken, identToken, rightParenToken, ep s state) |> Some
    | Some(inlineToken), _ ->
        SyntaxAttribute.Inline(inlineToken, dummyToken(), dummyToken(), dummyToken(), ep s state) |> Some
    | _ ->
        None

let tryParseUnmanagedAttribute state =
    let s = sp state

    match bt2 UNMANAGED (tryParseParenthesisOld IDENTIFIER) state with
    | Some(unmanagedToken), Some(leftParenToken, identToken, rightParenToken) ->
        match identToken.ValueText with
        | "allocation_only" ->
            SyntaxAttribute.Unmanaged(unmanagedToken, leftParenToken, identToken, rightParenToken, ep s state) |> Some
        | _ ->
            errorDo (InvalidSyntax("unmanaged argument"), identToken) state
            SyntaxAttribute.Unmanaged(unmanagedToken, leftParenToken, identToken, rightParenToken, ep s state) |> Some
    | Some(unmanagedToken), _ ->
        errorDo (InvalidSyntax("missing unmanaged argument"), unmanagedToken) state
        SyntaxAttribute.Unmanaged(unmanagedToken, dummyToken(), dummyToken(), dummyToken(), ep s state) |> Some
    | _ ->
        None

let tryParseOpenDeclarationExpression state =
    if isNextToken (function Open -> true | _ -> false) state then
        let s = sp state

        match bt OPEN state with
        | Some(openToken) ->
            match bt (tryParseName TypeParameterContext.Default) state with
            | Some(name) ->
                SyntaxExpression.OpenDeclaration(openToken, name, ep s state) |> Some
            | _ ->

            match bt2 STATIC (tryParseName TypeParameterContext.Default) state with
            | Some(staticToken), Some(name) ->
                SyntaxExpression.OpenStaticDeclaration(openToken, staticToken, name, ep s state) |> Some
            | Some(staticToken), _ ->
                error (ExpectedSyntax "a qualified name or identifier", SyntaxExpression.OpenStaticDeclaration(openToken, staticToken, SyntaxName.Identifier(dummyToken ()), ep s state)) state
            | _ ->

            match bt2 EXTENSION (tryParseName TypeParameterContext.Default) state with
            | Some(extensionToken), Some(name) ->
                SyntaxExpression.OpenExtensionDeclaration(openToken, extensionToken, name, ep s state) |> Some
            | Some(extensionToken), _ ->
                error (ExpectedSyntax "a qualified name or identifier", SyntaxExpression.OpenStaticDeclaration(openToken, extensionToken, SyntaxName.Identifier(dummyToken ()), ep s state)) state
            | _ ->

            error (ExpectedSyntax "a qualified name or identifier", SyntaxExpression.OpenDeclaration(openToken, SyntaxName.Identifier(dummyToken ()), ep s state)) state
        | _ ->
    
            None
    else
        None

let tryParsePropertyBindingDeclaration state =
    let s = sp state

    match bt2 GET tryParseParameters state with
    | Some(getToken), Some(pars) ->
        SyntaxBindingDeclaration.Getter(getToken, pars, ep s state) |> Some
    | Some(getToken), _ ->
        SyntaxBindingDeclaration.Get(getToken) |> Some
    | _ ->

    match bt2 SET tryParseParameters state with
    | Some(setToken), Some(pars) ->
        SyntaxBindingDeclaration.Setter(setToken, pars, ep s state) |> Some
    | Some(setToken), _ ->
        SyntaxBindingDeclaration.Set(setToken) |> Some
    | _ ->

    None

let tryParsePropertyBinding state =
    let s = sp state

    let attrs = parseAttributes state
    let accessor = parseAccessor state
    let premodifiers = parseValueDeclarationPremodifierList state
    let kind = parseValueDeclarationKind state

    let sBinding = sp state

    match bt tryParsePropertyBindingDeclaration state with
    | Some(getOrSetBindingDecl) ->
        match getOrSetBindingDecl with
        | SyntaxBindingDeclaration.Get _
        | SyntaxBindingDeclaration.Set _ ->
            let binding = SyntaxBinding.Signature(getOrSetBindingDecl)
            SyntaxPropertyBinding.Binding(attrs, accessor, premodifiers, kind, binding, ep s state) |> Some
        | _ ->
            match bt2 EQUAL (tryParseOffsideExpression SyntaxTreeContextLocal) state with
            | Some(equalsToken), Some(rhsExpr) ->
                let binding = SyntaxBinding.Implementation(getOrSetBindingDecl, equalsToken, rhsExpr, ep sBinding state)
                SyntaxPropertyBinding.Binding(attrs, accessor, premodifiers, kind, binding, ep s state) |> Some
            | Some(equalsToken), _ ->
                errorDo(ExpectedSyntaxAfterToken("expression", Equal), equalsToken) state
                let binding = SyntaxBinding.Implementation(getOrSetBindingDecl, equalsToken, SyntaxExpression.Error(dummyToken()), ep sBinding state)
                SyntaxPropertyBinding.Binding(attrs, accessor, premodifiers, kind, binding, ep s state) |> Some
            | _ ->
                let binding = SyntaxBinding.Signature(getOrSetBindingDecl)
                SyntaxPropertyBinding.Binding(attrs, accessor, premodifiers, kind, binding, ep s state) |> Some
    | _ ->
    
    None

let tryParsePropertyBindings state =
    match bt (tryOffside (tryParseListWithSeparatorOptionalComma tryParsePropertyBinding)) state with
    | Some(getOrSetBindingList) ->
        getOrSetBindingList |> Some
    | _ ->
        None

let parseOptionalIdentifier state =
    match bt IDENTIFIER state with
    | Some(identToken) -> identToken
    | _ -> dummyToken()

let tryParseGuardBinding state =
    if isNextToken (function When -> true | _ -> false) state then
        let s = sp state

        match bt WHEN state with
        | Some(whenToken) ->
            let identTokenOptional = parseOptionalIdentifier state
            match bt5 LEFT_PARENTHESIS (tryParseOffsideExpression SyntaxTreeContextLocal) tryRecoverableRightParenthesis FAT_RIGHT_ARROW (tryParseOffsideExpression SyntaxTreeContextLocal) state with
            | Some(leftParenToken), Some(conditionalExpr), Some(rightParenToken), Some(fatRightArrowToken), Some(rhsExpr) ->
                SyntaxGuardBinding.Implementation(whenToken, identTokenOptional, leftParenToken, conditionalExpr, rightParenToken, fatRightArrowToken, rhsExpr, ep s state)
                |> Some

            | Some(leftParenToken), Some(conditionalExpr), Some(rightParenToken), Some(fatRightArrowToken), _ ->
                errorDo(ExpectedSyntaxAfterToken("expression", fatRightArrowToken.RawToken), fatRightArrowToken) state
                SyntaxGuardBinding.Implementation(whenToken, identTokenOptional, leftParenToken, conditionalExpr, rightParenToken, fatRightArrowToken, SyntaxExpression.Error(dummyToken()), ep s state)
                |> Some

            | Some(leftParenToken), Some(conditionalExpr), Some(rightParenToken), _, _ ->
                errorDo(ExpectedTokenAfterToken(FatRightArrow, rightParenToken.RawToken), rightParenToken) state
                SyntaxGuardBinding.Implementation(whenToken, identTokenOptional, leftParenToken, conditionalExpr, rightParenToken, dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state)
                |> Some

            | Some(leftParenToken), Some(conditionalExpr), _, _, _ ->
                errorDo(ExpectedTokenAfterSyntax(RightParenthesis, "conditional expression"), conditionalExpr) state
                SyntaxGuardBinding.Implementation(whenToken, identTokenOptional, leftParenToken, conditionalExpr, dummyToken(), dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state)
                |> Some

            | Some(leftParenToken), _, _, _, _ ->
                errorDo(ExpectedSyntaxAfterToken("conditional expression", leftParenToken.RawToken), leftParenToken) state
                SyntaxGuardBinding.Implementation(whenToken, identTokenOptional, leftParenToken, SyntaxExpression.Error(dummyToken()), dummyToken(), dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state)
                |> Some

            | _ ->
                SyntaxGuardBinding.Signature(whenToken, identTokenOptional, ep s state)
                |> Some
        | _ ->
            None
    else
        None

// ----------------------------------------------------------------------------------------------------
// TODO: This is a mess with fullWidth
let tryParseValueDeclarationExpressionWithModifiers s attrs accessor context premodifierList kind postmodifierList state =
    match context, kind with
    | SyntaxTreeContext.TopLevel, _
    | SyntaxTreeContext.Local _, SyntaxValueDeclarationKind.Error _ 
    | SyntaxTreeContext.Local _, SyntaxValueDeclarationKind.Let _
    | SyntaxTreeContext.Local _, SyntaxValueDeclarationKind.LetBind _ ->

        let sBinding = sp state

        let parseBody state =
            if context.CanSkipSequential then
                SyntaxExpression.None()
            else
                parseAlignedExpression context state

        match bt tryParseBindingDeclaration state with
        | Some(binding) ->
            match tryParseGuardBinding state with
            | Some(guardBinding) ->
                let binding = SyntaxBinding.PatternWithGuard(binding, guardBinding, ep sBinding state)
                let eValueDecl = ep s state
                let body = parseBody state
                SyntaxExpression.Sequential(
                    SyntaxExpression.ValueDeclaration(attrs, accessor, premodifierList, kind, postmodifierList, binding, eValueDecl),
                    body, ep s state
                )|> Some
            | _ ->
                match bt2 EQUAL (tryParseOffsideExpression SyntaxTreeContextLocal) state with
                | Some(equalsToken), Some(expr) ->
                    let binding = SyntaxBinding.Implementation(binding, equalsToken, expr, ep sBinding state)
                    let eValueDecl = ep s state
                    let body = parseBody state
                    SyntaxExpression.Sequential(
                        SyntaxExpression.ValueDeclaration(attrs, accessor, premodifierList, kind, postmodifierList, binding, eValueDecl),
                        body, ep s state
                    )|> Some
                | Some(equalsToken), _ ->
                    errorDo(ExpectedSyntaxAfterToken("expression", Equal), equalsToken) state
                    let binding = SyntaxBinding.Implementation(binding, equalsToken, SyntaxExpression.Error(dummyToken()), ep sBinding state)
                    let eValueDecl = ep s state
                    let body = parseBody state
                    SyntaxExpression.Sequential(
                        SyntaxExpression.ValueDeclaration(attrs, accessor, premodifierList, kind, postmodifierList, binding, eValueDecl),
                        body, ep s state
                    ) |> Some
                | _ ->
                    match bt tryParsePropertyBindings state with
                    | Some(propBindingList) ->
                        let binding = 
                            match bt2 EQUAL (tryParseOffsideExpression SyntaxTreeContextLocal) state with
                            | Some(equalToken), Some(expr) ->
                                SyntaxBinding.PropertyWithDefault(binding, propBindingList, equalToken, expr, ep sBinding state)
                            | Some(equalToken), _ ->
                                errorDo(ExpectedSyntaxAfterToken("expression", Equal), equalToken) state
                                SyntaxBinding.PropertyWithDefault(binding, propBindingList, equalToken, SyntaxExpression.Error(dummyToken()), ep sBinding state)
                            | _ ->
                                SyntaxBinding.Property(binding, propBindingList, ep sBinding state)

                        let eValueDecl = ep s state
                        SyntaxExpression.ValueDeclaration(attrs, accessor, premodifierList, kind, postmodifierList, binding, eValueDecl)
                        |> Some
                    | _ ->
                        let binding = SyntaxBinding.Signature(binding)
                        let eValueDecl = ep s state
                        let body = parseBody state
                        SyntaxExpression.Sequential(
                            SyntaxExpression.ValueDeclaration(attrs, accessor, premodifierList, kind, postmodifierList, binding, eValueDecl),
                            body, ep s state
                        )
                        |> Some                   
        | _ ->

        match attrs, accessor, premodifierList, kind, postmodifierList with
        | SyntaxAttributes.Empty _, SyntaxAccessor.None _, SyntaxList.Empty _, SyntaxValueDeclarationKind.None _, SyntaxList.Empty _ ->
            None
        | _ ->
            let binding = SyntaxBinding.Signature(SyntaxBindingDeclaration.Error(dummyToken()))
            error(ExpectedSyntax("binding"), SyntaxExpression.ValueDeclaration(attrs, accessor, premodifierList, kind, postmodifierList, binding, ep s state)) state
    | _ ->
        None

let tryParseValueDeclarationExpression s attrs accessor context state =
    let premodifierList = parseValueDeclarationPremodifierList state
    let kind = parseValueDeclarationKind state
    let postmodifierList = parseValueDeclarationPostmodifierList state
    tryParseValueDeclarationExpressionWithModifiers s attrs accessor context premodifierList kind postmodifierList state

let possibleLambdaExpression state =
    let _kind = parseLambdaKind state

    match bt2 tryParseParameters RIGHT_ARROW state with
    | Some _, Some _ -> 
        Some()
    | _ ->

    match bt2 tryParseParameter RIGHT_ARROW state with
    | Some _, Some _ -> 
        Some()
    | _ ->
        
    None

let isPossibleLambdaExpression state : bool =
    match bt (btLexer (tryPeek possibleLambdaExpression)) state with
    | Some _ -> true
    | _ -> false

let possibleParenthesisOperator state =
    match bt3 LEFT_PARENTHESIS tryParseOperator tryRecoverableRightParenthesis state with
    | Some _, Some _, Some _ -> Some()
    | _ -> None

let isPossibleParenthesisOperator state =
    match bt (tryPeek possibleParenthesisOperator) state with
    | Some _ -> true
    | _ -> false

let tryParseParenthesisOrTupleOrLambdaExpression state =
    if isNextToken (function LeftParenthesis | Identifier _ | Static _ -> true | _ -> false) state then
        if isPossibleLambdaExpression state then
            tryParseLambdaExpression state
        else

        // Disambiguate parenthesis expressions let parenthesis operators.
        if isPossibleParenthesisOperator state then
            None
        else

        let s = sp state

        match bt (tryParseParenthesisSeparatorList COMMA "expression" (tryParseExpression SyntaxTreeContextLocal) (fun x -> SyntaxExpression.Error(x) |> Some)) state with
        | Some(leftParenToken, exprList, rightParenToken) ->
            SyntaxExpression.Parenthesis(leftParenToken, exprList, rightParenToken, ep s state) |> Some
        | _ ->

            None
    else
        None

let tryParseArrayExpression context state =
    match context with
    | SyntaxTreeContext.TopLevel -> None
    | _ ->

    if isNextToken (function LeftBracket | Mutable -> true | _ -> false) state then
        let s = sp state

        let mutableTokenOpt = bt MUTABLE state

        match bt3 LEFT_BRACKET (tryParseListWithSeparatorOld tryOptionalSemiColonWithTerminalRightBracket "expression" SyntaxExpression.Error (tryParseOffsideExpression SyntaxTreeContextLocalSkipSequential)) (tryFlexAlign RIGHT_BRACKET) state with
        | Some(leftBracketToken), Some(items), Some(rightBracketToken) ->
            match mutableTokenOpt with
            | Some(mutableToken) ->
                SyntaxExpression.MutableArray(mutableToken, leftBracketToken, items, rightBracketToken, ep s state) |> Some
            | _ ->
                SyntaxExpression.Array(leftBracketToken, items, rightBracketToken, ep s state) |> Some

        | Some(leftBracketToken), Some(items), _ ->
            match mutableTokenOpt with
            | Some(mutableToken) ->
                error (ExpectedToken RightBracket, SyntaxExpression.MutableArray(mutableToken, leftBracketToken, items, dummyToken (), ep s state)) state
            | _ ->
                error (ExpectedToken RightBracket, SyntaxExpression.Array(leftBracketToken, items, dummyToken (), ep s state)) state

        | Some(leftBracketToken), _, _ ->
            match bt (tryFlexAlign RIGHT_BRACKET) state with
            | Some(rightBracketToken) ->
                match mutableTokenOpt with
                | Some(mutableToken) ->
                    SyntaxExpression.MutableArray(mutableToken, leftBracketToken, SyntaxSeparatorList.Empty(), rightBracketToken, ep s state) |> Some
                | _ ->
                    SyntaxExpression.Array(leftBracketToken, SyntaxSeparatorList.Empty(), rightBracketToken, ep s state) |> Some
            | _ ->

            match mutableTokenOpt with
            | Some(mutableToken) ->
                error (ExpectedSyntax "] or items", SyntaxExpression.MutableArray(mutableToken, leftBracketToken, SyntaxSeparatorList.Empty(), dummyToken (), ep s state)) state
            | _ ->
                error (ExpectedSyntax "] or items", SyntaxExpression.Array(leftBracketToken, SyntaxSeparatorList.Empty(), dummyToken (), ep s state)) state
        | _ ->
            match mutableTokenOpt with
            | Some(mutableToken) ->
                error (ExpectedSyntaxAfterToken("array expression", mutableToken.RawToken), SyntaxExpression.MutableArray(mutableToken, dummyToken(), SyntaxSeparatorList.Empty(), dummyToken (), ep s state)) state
            | _ ->
                None
    else
        None

let tryCreateRecordExpression context state =
    if isNextToken (function LeftCurlyBracket -> true | _ -> false) state then
        match bt (tryParseConstructType context) state with
        | Some(constructTy) ->
           SyntaxExpression.CreateRecord(constructTy) |> Some
        | _ ->
            None
    else
        None

let tryParseNameExpression state =
    if isNextToken (function Identifier _ | LeftParenthesis | This | Base -> true | _ -> false) state then
        match bt (tryParseName TypeParameterContext.Operator) state with
        | Some(name) ->
            SyntaxExpression.Name(name) |> Some
        | _ ->
            None
    else
        None

let literalOrPrefixCallOrNameExpr context state =
    let s = sp state

    match bt tryParseLiteral state with
    | Some(literal) ->
        SyntaxExpression.Literal(literal) |> Some
    | _ ->

    match bt2 tryParsePrefixOrInfixOperator (tryParseOffsideExpression context) state with
    | Some(operator), Some(arg) ->
        let rec rewritePrefix e arg =
            match arg with
            // Handles precedence for prefix calls with infix calls
            | SyntaxExpression.InfixCall(left, name, right, _) ->
                SyntaxExpression.InfixCall(rewritePrefix ((operator :> ISyntaxNode).FullWidth + (left :> ISyntaxNode).FullWidth) left, name, right, e)
            | SyntaxExpression.Mutate(left, leftArrowToken, right, _) ->
                // Note: Be careful not to error on 'SyntaxExpression.Mutate' as it can be re-written.
                SyntaxExpression.Mutate(rewritePrefix ((operator :> ISyntaxNode).FullWidth + (left :> ISyntaxNode).FullWidth) left, leftArrowToken, right, e)
            | _ ->
                SyntaxExpression.PrefixCall(SyntaxName.Identifier(operator), arg, e)
        let e = ep s state
        rewritePrefix e arg |> Some
    | Some(operator), _ ->
        error(ExpectedSyntaxAfterToken("expression", operator.RawToken), SyntaxExpression.PrefixCall(SyntaxName.Identifier(operator), SyntaxExpression.Error(dummyToken ()), ep s state)) state
    | _ ->

    match bt2 RETURN (tryParseOffsideExpression context) state with
    | Some(returnToken), Some(arg) ->
        SyntaxExpression.PrefixCall(SyntaxName.Identifier(returnToken), arg, ep s state) |> Some
    | Some(returnToken), _ ->
        error(ExpectedSyntaxAfterToken("expression", returnToken.RawToken), SyntaxExpression.PrefixCall(SyntaxName.Identifier(returnToken), SyntaxExpression.Error(dummyToken ()), ep s state)) state
    | _ ->

    tryParseNameExpression state

let tryCreateTerminalExpression context state =
    match bt (ignoreOffside (tryToken (fun _ -> true))) state with
    | Some token ->
        errorDo(UnexpectedToken token.RawToken, token) state
        SyntaxExpression.Error(token) |> Some
    | _ ->
        None

let checkContextForValueOrTypeDeclarationExpression (context: SyntaxTreeContext) syntaxNode state =
    if context.CanSkipSequential then
        errorDo(InvalidSyntax("Declaration not valid in this context."), syntaxNode) state

let tryParseValueOrTypeDeclarationExpression (context: SyntaxTreeContext) state =
    let s = sp state

    let tryParseAux s attrs context state =
        let accessor = parseAccessor state

        // Order matters: we need to try to do this first.
        match bt (tryParseTypeDeclarationExpression s attrs accessor) state with
        | Some result -> 
            checkContextForValueOrTypeDeclarationExpression context result state
            if context.CanSkipSequential then
                SyntaxExpression.Sequential(
                    result,
                    SyntaxExpression.Error(dummyToken()),
                    ep s state
                )
                |> Some
            else
                Some result
        | _ ->

        match bt (tryParseValueDeclarationExpression s attrs accessor context) state with
        | Some result -> 
            checkContextForValueOrTypeDeclarationExpression context result state
            if context.CanSkipSequential then
                SyntaxExpression.Sequential(
                    result,
                    SyntaxExpression.Error(dummyToken()),
                    ep s state
                )
                |> Some
            else
                Some result
        | _ ->

        None

    let tryParse s context state =
        match bt tryParseAttributes state with
        | Some attrs ->
            tryParseAux s attrs context state
        | _ ->
            tryParseAux s (SyntaxAttributes.Empty()) context state

    bt (alignRecover (tryParse s context)) state

let rec tryParseCallExpression s expr state =
    match bt tryParseArguments state with
    | Some(arguments) ->
        let resultExpr = SyntaxExpression.Call(expr, arguments, ep s state)
        match bt (tryParseCallExpression s resultExpr) state with
        | (Some _) as result -> result
        | _ ->
            resultExpr |> Some
    | _ ->

    None

let rec parseDefaultOrMemberAccessExpression s defaultExpr state =
    if isNextToken (function LeftParenthesis -> true | _ -> false) state then
        match bt (tryParseCallExpression s defaultExpr) state with
        | Some(receiver) -> 
            (parseMemberAccessExpression s receiver) state
        | _ ->
            (parseMemberAccessExpression s defaultExpr) state
    else
        (parseMemberAccessExpression s defaultExpr) state

and parseMemberAccessExpression s receiver state =
    if isNextToken (function Dot -> true | _ -> false) state then
        match bt DOT state with
        | Some(dotToken) ->
            let sForDefaultOrMemberAccess = sp state

            match bt (tryParseName TypeParameterContext.Operator) state with
            | Some(name) ->
                let expr = parseDefaultOrMemberAccessExpression sForDefaultOrMemberAccess (SyntaxExpression.Name(name)) state
                SyntaxExpression.MemberAccess(receiver, dotToken, expr, ep s state)
            | _ ->
                errorReturn (InvalidSyntax "Missing qualification after the '.'.", SyntaxExpression.MemberAccess(receiver, dotToken, SyntaxExpression.Name(SyntaxName.Identifier(dummyToken())), ep s state)) state
        | _ ->
            receiver
    else
        receiver

let tryParseIndexer s left state =
    match bt (tryParseBracketsSeparatorList COMMA "expression" (tryParseOffsideExpression SyntaxTreeContextLocal) (fun x -> SyntaxExpression.Error(x) |> Some)) state with
    | Some(brackets) ->
        let eLeftBrackets = ep s state

        match bt2 LEFT_ARROW (tryParseOffsideExpression SyntaxTreeContextLocal) state with
        | Some(leftArrowToken), Some(expr) ->
            SyntaxExpression.Mutate(SyntaxExpression.Indexer(left, brackets, eLeftBrackets), leftArrowToken, expr, ep s state) |> Some
        | Some(leftArrowToken), _ ->
            errorDo(ExpectedSyntaxAfterToken("expression", LeftArrow), leftArrowToken) state
            SyntaxExpression.Mutate(SyntaxExpression.Indexer(left, brackets, eLeftBrackets), leftArrowToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
        | _ ->
            SyntaxExpression.Indexer(left, brackets, ep s state) |> Some
    | _ ->

    None

let tryParseThrowExpression state =
    if isNextToken (function Throw -> true | _ -> false) state then
        let s = sp state

        match bt2 THROW (tryParseOffsideExpression SyntaxTreeContextLocal) state with
        | Some(throwToken), Some(expr) ->
            SyntaxExpression.Throw(throwToken, expr, ep s state) |> Some
        | Some(throwToken), _ ->
            errorDo(ExpectedSyntaxAfterToken("expression", throwToken.RawToken), throwToken) state
            SyntaxExpression.Throw(throwToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
        | _ ->
            None
    else
        None

let parseDefaultOrMutateExpression s lhs state =
    if isNextToken (function LeftArrow | LeftBracket -> true | _ -> false) state then
        match bt2 LEFT_ARROW (tryParseOffsideExpression SyntaxTreeContextLocal) state with
        | Some(leftArrowToken), Some(rhs) ->
            SyntaxExpression.Mutate(lhs, leftArrowToken, rhs, ep s state)
        | Some(leftArrowToken), _ ->
            errorDo(ExpectedSyntaxAfterToken("expression", leftArrowToken.RawToken), leftArrowToken) state
            SyntaxExpression.Mutate(lhs, leftArrowToken, SyntaxExpression.Error(dummyToken()), ep s state)
        | _ ->

        match tryParseIndexer s lhs state with
        | Some(res) -> 
            let res2 = parseDefaultOrMemberAccessExpression s res state
            parseDefaultOrMutateExpression s res2 state
        | _ ->
            lhs
    else
        lhs

let binaryPrecedence (leftTok: Token) (rightTok: Token) =
    match leftTok, rightTok with
    // * / %
    | Star, _
    | ForwardSlash, _
    | Percent, _ -> false
    | _, Star
    | _, ForwardSlash
    | _, Percent -> true

    // + -
    | Plus, _
    | Minus, _ -> false
    | _, Plus
    | _, Minus -> true

    // &
    | Ampersand, _ -> false
    | _, Ampersand -> true

    // ^
    | Caret, _ -> false
    | _, Caret -> true

    // |
    | Pipe, _ -> false
    | _, Pipe -> true

    // > >= < <=
    | GreaterThan, _
    | GreaterThanEqual, _
    | LessThan, _
    | LessThanEqual, _ -> false
    | _, GreaterThan
    | _, GreaterThanEqual
    | _, LessThan
    | _, LessThanEqual -> true

    // == != === !==
    | EqualEqual, _
    | EqualEqualEqual, _
    | ExclamationEqual, _ 
    | ExclamationEqualEqual, _ -> false
    | _, EqualEqual
    | _, EqualEqualEqual
    | _, ExclamationEqual 
    | _, ExclamationEqualEqual -> true

    // && || and or
    | AmpersandAmpersand, _
    | PipePipe, _ 
    | And, _
    | Or, _ -> false
    | _, AmpersandAmpersand
    | _, PipePipe
    | _, And
    | _, Or -> true

    | _ -> false

let getFullWidth (node: ISyntaxNode) = node.FullWidth

// This needs to be handled with care as we are taking old nodes
// and creating new ones. The old nodes should not have any errors associated with them
// as the identity of the node is tied to the reference itself.
let rec restructureForInfixTail left (opName: SyntaxName) right cont =
    match right with
    | SyntaxExpression.InfixCall(left2, nextOpName, right2, _) ->
        if binaryPrecedence opName.FirstIdentifier.RawToken nextOpName.FirstIdentifier.RawToken then
            cont (SyntaxExpression.InfixCall(left, opName, right, getFullWidth left + getFullWidth opName + getFullWidth right))
        else
            restructureForInfixTail left opName left2 (fun left3 ->
                cont (
                    SyntaxExpression.InfixCall(
                        left3,
                        nextOpName,
                        right2,
                        getFullWidth left3 + getFullWidth nextOpName + getFullWidth right2
                    )
                )
            )
    | _ ->
        cont (SyntaxExpression.InfixCall(left, opName, right, getFullWidth left + getFullWidth opName + getFullWidth right))

let restructureForInfix left (opName: SyntaxName) right =
    restructureForInfixTail left opName right id

let tryInfixOperator s left state =
    match bt2 tryParseOperator (tryParseFlexExpression SyntaxTreeContextLocal) state with
    | Some(operatorToken), Some(right) ->
        restructureForInfix left (SyntaxName.Identifier(operatorToken)) right |> Some

    | Some(operatorToken), _ ->
        let operatorOrIdentifier = SyntaxName.Identifier(operatorToken)
        let expr = SyntaxExpression.InfixCall(left, operatorOrIdentifier, SyntaxExpression.Error(dummyToken ()), ep s state)
        errorDo (ExpectedSyntaxAfterToken("right-side argument expression", operatorToken.RawToken), operatorOrIdentifier) state
        expr |> Some
    | _ ->
        None


let parseExpressionAux context state =
    let s = sp state

    let res =
        match bt (alignOrFlexAlignRecover tryParseOpenDeclarationExpression) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover tryParseIfExpression) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover tryParseTryExpression) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover tryParseMatchExpression) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover tryParseWhileExpression) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover tryParseParenthesisOrTupleOrLambdaExpression) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover (tryParseArrayExpression context)) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover (tryCreateRecordExpression context)) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover (tryParseLetPatternBinding context)) state with
        | Some result -> SyntaxExpression.LetPatternDeclaration(result)
        | _ ->

        match bt (alignOrFlexAlignRecover (tryParseValueOrTypeDeclarationExpression context)) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover tryParseThrowExpression) state with
        | Some result -> result
        | _ ->

        match bt (alignOrFlexAlignRecover (literalOrPrefixCallOrNameExpr context)) state with
        | Some result -> result
        | _ ->

        match bt (tryCreateTerminalExpression context) state with
        | Some result -> result
        | _ ->

        failwith "Internal error: Invalid syntax expression"

    let res = parseDefaultOrMemberAccessExpression s res state
    match res with
    | SyntaxExpression.Name _
    | SyntaxExpression.Literal _
    | SyntaxExpression.Parenthesis _
    | SyntaxExpression.PrefixCall _
    | SyntaxExpression.Call _
    | SyntaxExpression.MemberAccess _ 
    | SyntaxExpression.CreateRecord _ ->
        let left = parseDefaultOrMutateExpression s res state

        match bt (tryInfixOperator s left) state with
        | Some(res) -> parseNextAlignedExpression s context res state
        | _ ->

        match bt (tryParseTypedExpression s left) state with
        | Some(res) -> 
            parseNextAlignedExpression s context res state
        | _ ->

        match bt (tryParseUpdateRecordExpression s left context) state with
        | Some(res) ->
            parseNextAlignedExpression s context res state
        | _ ->

        parseNextAlignedExpression s context left state
    | _ ->
        match bt (tryParseTypedExpression s res) state with
        | Some(res) ->
            parseNextAlignedExpression s context res state
        | _ ->
            parseNextAlignedExpression s context res state

let parseExpression context state =
#if DEBUG
    DebugStackGuard.Do(fun () ->
        noAlign (parseExpressionAux context) state
    )
#else
    noAlign (parseExpressionAux context) state
#endif

let tryParseUpdateRecordExpression (s: int) (expr: SyntaxExpression) (context: SyntaxTreeContext) state =
    if isNextToken (function With -> true | _ -> false) state then
        match bt2 WITH (tryParseConstructType context) state with
        | Some(withToken), Some(constructTy) ->
            SyntaxExpression.UpdateRecord(expr, withToken, constructTy, ep s state) |> Some
        | Some(withToken), _ ->
            error(ExpectedSyntaxAfterToken("record syntax", With), SyntaxExpression.UpdateRecord(expr, withToken, SyntaxConstructType.Anonymous(dummyToken(), SyntaxSeparatorList.Empty(), dummyToken(), 0), ep s state)) state
        | _ ->
            None
    else
        None

let tryParseConstructType context state =
    if isNextToken (function LeftCurlyBracket -> true | _ -> false) state then
        let s = sp state

        match bt3 LEFT_CURLY_BRACKET (tryParseListWithSeparatorOld tryOptionalSemiColon "field pattern" SyntaxFieldPattern.Error (tryParseFieldPattern context)) (tryFlexAlign RIGHT_CURLY_BRACKET) state with
        | Some(leftCurlyBracketToken), Some(fieldPatList), Some(rightCurlyBracketToken) ->
            SyntaxConstructType.Anonymous(leftCurlyBracketToken, fieldPatList, rightCurlyBracketToken, ep s state) |> Some
        | Some(leftCurlyBracketToken), Some(fieldPatList), _ ->
            let result = SyntaxConstructType.Anonymous(leftCurlyBracketToken, fieldPatList, dummyToken(), ep s state)
            errorDo(ExpectedToken RightCurlyBracket, result) state
            result |> Some
        | Some(leftCurlyBracketToken), _, _ ->
            match bt (tryFlexAlign RIGHT_CURLY_BRACKET) state with
            | Some(rightCurlyBracketToken) ->
                SyntaxConstructType.Anonymous(leftCurlyBracketToken, SyntaxSeparatorList.Empty(), rightCurlyBracketToken, ep s state) |> Some
            | _ ->
                let result = SyntaxConstructType.Anonymous(leftCurlyBracketToken, SyntaxSeparatorList.Empty(), dummyToken(), ep s state)
                errorDo(ExpectedToken RightCurlyBracket, result) state
                result |> Some
        | _ ->
            None
    else
        None

let tryParseImportAttribute state =
    let s = sp state

    match bt2 IMPORT tryParseArguments state with
    | Some(importToken), Some(args) ->
        SyntaxAttribute.Import(SyntaxName.Identifier(importToken), args, ep s state) |> Some
    | Some(importToken), _ ->
        errorDo (ExpectedSyntaxAfterToken("arguments", importToken.RawToken), importToken) state
        SyntaxAttribute.Import(SyntaxName.Identifier(importToken), SyntaxArguments.Empty(), ep s state) |> Some
    | _ ->
        None

let tryParseExportAttribute state =
    match bt EXPORT state with
    | Some(exportToken) ->
        SyntaxAttribute.Export(exportToken) |> Some
    | _ ->
        None

let tryParseLambdaExpression state =
    let s = sp state

    let kind = parseLambdaKind state

    match bt3 tryParseParameters RIGHT_ARROW (tryParseOffsideExpression SyntaxTreeContextLocal) state with
    | Some(pars), Some(rightArrowToken), Some(body) ->
        SyntaxExpression.Lambda(kind, pars, rightArrowToken, body, ep s state) |> Some
    | Some(pars), Some(rightArrowToken), _ ->
        errorDo(ExpectedSyntaxAfterToken("expression body", RightArrow), rightArrowToken) state
        SyntaxExpression.Lambda(kind, pars, rightArrowToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
    | _ ->

    match bt tryParseParameter state with
    | Some(par) ->       
        let ePar = ep s state
        match bt2 RIGHT_ARROW (tryParseOffsideExpression SyntaxTreeContextLocal) state with
        | Some(rightArrowToken), Some(body) ->
            let parList = SyntaxSeparatorList.List(par, dummyToken(), SyntaxSeparatorList.Empty(), ePar)
            let pars = SyntaxParameters.Parameters(dummyToken(), parList, dummyToken(), ePar)
            SyntaxExpression.Lambda(kind, pars, rightArrowToken, body, ep s state) |> Some
        | Some(rightArrowToken), _ ->
            let parList = SyntaxSeparatorList.List(par, dummyToken(), SyntaxSeparatorList.Empty(), ePar)
            let pars = SyntaxParameters.Parameters(dummyToken(), parList, dummyToken(), ePar)
            errorDo(ExpectedSyntaxAfterToken("expression body", RightArrow), rightArrowToken) state
            SyntaxExpression.Lambda(kind, pars, rightArrowToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
        | _ ->
            None
    | _ ->

    None          

let tryParseIfExpression state =
    if isNextToken (function If -> true | _ -> false) state then
        let s = sp state

        match bt6 IF LEFT_PARENTHESIS (tryParseOffsideExpression SyntaxTreeContextLocal) tryRecoverableRightParenthesis (tryParseOffsideExpression SyntaxTreeContextLocal) (tryFlexAlign tryParseOffsideElseIfOrElseExpression) state with
        | Some(ifToken), Some(leftParenToken), Some(predicateExpr), Some(rightParenToken), Some(targetExpr), Some(whenOrElseExpr) ->
            SyntaxExpression.If(ifToken, leftParenToken, predicateExpr, rightParenToken, targetExpr, whenOrElseExpr, ep s state) |> Some

        | Some(ifToken), Some(leftParenToken), Some(predicateExpr), Some(rightParenToken), Some(targetExpr), _ ->
            SyntaxExpression.If(ifToken, leftParenToken, predicateExpr, rightParenToken, targetExpr, SyntaxElseIfOrElseExpression.None(dummyToken ()), ep s state) |> Some

        | Some(ifToken), Some(leftParenToken), Some(predicateExpr), Some(rightParenToken), _, _ ->
            let expr = SyntaxExpression.If(ifToken, leftParenToken, predicateExpr, rightParenToken, SyntaxExpression.Error(dummyToken ()), SyntaxElseIfOrElseExpression.None(dummyToken ()), ep s state)
            error (ExpectedSyntaxAfterSyntax("target expression", "\"if\" expression"), expr) state

        | Some(ifToken), Some(leftParenToken), Some(predicateExpr), _, _, _ ->
            // Better error recovery
            let targetExprOpt = tryParseOffsideExpression SyntaxTreeContextLocal state
            let whenOrElseExprOpt = tryFlexAlign tryParseOffsideElseIfOrElseExpression state
            let targetExpr = match targetExprOpt with Some targetExpr -> targetExpr | _ -> SyntaxExpression.Error(dummyToken ())
            let whenOrElseExpr = match whenOrElseExprOpt with Some whenOrElseExpr -> whenOrElseExpr | _ -> SyntaxElseIfOrElseExpression.None(dummyToken ()) 
            errorDo(ExpectedToken RightParenthesis, predicateExpr) state
            SyntaxExpression.If(ifToken, leftParenToken, predicateExpr, dummyToken (), targetExpr, whenOrElseExpr, ep s state) |> Some

        | Some(ifToken), Some(leftParenToken), _, _, _, _ ->
            let expr = SyntaxExpression.If(ifToken, leftParenToken, SyntaxExpression.Error(dummyToken()), dummyToken (), SyntaxExpression.Error(dummyToken ()), SyntaxElseIfOrElseExpression.None(dummyToken ()), ep s state)
            error (ExpectedSyntax("conditional expression"), expr) state

        | Some(ifToken), _, _, _, _, _ ->
            let expr = SyntaxExpression.If(ifToken, dummyToken(), SyntaxExpression.Error(dummyToken ()), dummyToken (), SyntaxExpression.Error(dummyToken ()), SyntaxElseIfOrElseExpression.None(dummyToken ()), ep s state)
            error (ExpectedToken LeftParenthesis, expr) state

        | _ ->
            None
    else
        None

let parseNextCatchOrFinallyExpression state =
    match tryFlexAlign tryParseCatchOrFinallyExpression state with
    | Some(catchOrFinallExpr) -> catchOrFinallExpr
    | _ -> SyntaxCatchOrFinallyExpression.None(dummyToken())

let tryParseCatchOrFinallyExpression state =
    let s = sp state

    match bt6 CATCH LEFT_PARENTHESIS tryParseParameter tryRecoverableRightParenthesis FAT_RIGHT_ARROW (tryParseOffsideExpression SyntaxTreeContextLocal) state with
    | Some(catchToken), Some(leftParenToken), Some(par), Some(rightParenToken), Some(fatRightArrowToken), Some(catchBodyExpr) ->
        let catchOrFinallyExpr = parseNextCatchOrFinallyExpression state
        SyntaxCatchOrFinallyExpression.Catch(catchToken, leftParenToken, par, rightParenToken, fatRightArrowToken, catchBodyExpr, catchOrFinallyExpr, ep s state) |> Some

    | Some(catchToken), Some(leftParenToken), Some(par), Some(rightParenToken), Some(fatRightArrowToken), _ ->
        errorDo (ExpectedSyntaxAfterToken("'catch' body expression", fatRightArrowToken.RawToken), fatRightArrowToken) state
        let catchOrFinallyExpr = parseNextCatchOrFinallyExpression state
        SyntaxCatchOrFinallyExpression.Catch(catchToken, leftParenToken, par, rightParenToken, fatRightArrowToken, SyntaxExpression.Error(dummyToken()), catchOrFinallyExpr, ep s state) |> Some

    | Some(catchToken), Some(leftParenToken), Some(par), Some(rightParenToken), _, _ ->
        errorDo (ExpectedTokenAfterToken(FatRightArrow, rightParenToken.RawToken), rightParenToken) state
        let catchOrFinallyExpr = parseNextCatchOrFinallyExpression state
        SyntaxCatchOrFinallyExpression.Catch(catchToken, leftParenToken, par, rightParenToken, dummyToken(), SyntaxExpression.Error(dummyToken()), catchOrFinallyExpr, ep s state) |> Some

    | Some(catchToken), Some(leftParenToken), Some(par), _, _, _ ->
        errorDo (ExpectedTokenAfterSyntax(RightParenthesis, "parameter"), par) state
        let catchOrFinallyExpr = parseNextCatchOrFinallyExpression state
        SyntaxCatchOrFinallyExpression.Catch(catchToken, leftParenToken, par, dummyToken(), dummyToken(), SyntaxExpression.Error(dummyToken()), catchOrFinallyExpr, ep s state) |> Some

    | Some(catchToken), Some(leftParenToken), _, _, _, _ ->
        errorDo (ExpectedSyntaxAfterToken("parameter", leftParenToken.RawToken), leftParenToken) state
        let catchOrFinallyExpr = parseNextCatchOrFinallyExpression state
        SyntaxCatchOrFinallyExpression.Catch(catchToken, leftParenToken, SyntaxParameter.Error(dummyToken()), dummyToken(), dummyToken(), SyntaxExpression.Error(dummyToken()), catchOrFinallyExpr, ep s state) |> Some

    | Some(catchToken), _, _, _, _, _ ->
        errorDo (ExpectedTokenAfterToken(LeftParenthesis, catchToken.RawToken), catchToken) state
        let catchOrFinallyExpr = parseNextCatchOrFinallyExpression state
        SyntaxCatchOrFinallyExpression.Catch(catchToken, dummyToken(), SyntaxParameter.Error(dummyToken()), dummyToken(), dummyToken(), SyntaxExpression.Error(dummyToken()), catchOrFinallyExpr, ep s state) |> Some
    | _ ->

    match bt2 FINALLY (tryParseOffsideExpression SyntaxTreeContextLocal) state with
    | Some(finallyToken), Some(finallyBodyExpr) ->
        SyntaxCatchOrFinallyExpression.Finally(finallyToken, finallyBodyExpr, ep s state) |> Some
    
    | Some(finallyToken), _ ->
        errorDo (ExpectedSyntaxAfterToken("'finally' body expression", finallyToken.RawToken), finallyToken) state
        SyntaxCatchOrFinallyExpression.Finally(finallyToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
    | _ ->

    None

let tryParseTryExpression state =
    if isNextToken (function Try -> true | _ -> false) state then
        let s = sp state

        match bt3 TRY (tryParseOffsideExpression SyntaxTreeContextLocal) (tryFlexAlign tryParseCatchOrFinallyExpression) state with
        | Some(tryToken), Some(bodyExpr), Some(catchOrFinallyExpr) ->
            SyntaxExpression.Try(tryToken, bodyExpr, catchOrFinallyExpr, ep s state) |> Some

        | Some(tryToken), Some(bodyExpr), _ ->
            errorDo (ExpectedSyntaxAfterSyntax("'catch' or 'finally'", "'try' body expression"), tryToken) state
            SyntaxExpression.Try(tryToken, bodyExpr, SyntaxCatchOrFinallyExpression.None(dummyToken()), ep s state) |> Some

        | Some(tryToken), _, _ ->
            errorDo (ExpectedSyntaxAfterToken("'try' body expression", tryToken.RawToken), tryToken) state
            SyntaxExpression.Try(tryToken, SyntaxExpression.Error(dummyToken()), SyntaxCatchOrFinallyExpression.None(dummyToken()), ep s state) |> Some

        | _ ->
            None
    else
        None

let tryParseMatchExpression state =
    if isNextToken (function Match -> true | _ -> false) state then
        let s = sp state

        match bt5 MATCH LEFT_PARENTHESIS (tryParseListWithSeparatorOld COMMA "expression" SyntaxExpression.Error (tryParseOffsideExpression SyntaxTreeContextLocal)) tryRecoverableRightParenthesis (tryParseList (tryAlign tryParseMatchClause)) state with
        | Some(matchToken), Some(leftParenToken), Some(matchExprList), Some(rightParenToken), Some(matchCaseList) ->
            SyntaxExpression.Match(matchToken, leftParenToken, matchExprList, rightParenToken, matchCaseList, ep s state) |> Some
        | Some(matchToken), Some(leftParenToken), Some(matchExprList), Some(rightParenToken), _ ->
            let result = SyntaxExpression.Match(matchToken, leftParenToken, matchExprList, rightParenToken, SyntaxList.Empty(), ep s state)
            errorDo(ExpectedSyntaxAfterSyntax("match clause", "match expression"), result) state
            result |> Some
        | Some(matchToken), Some(leftParenToken), Some(matchExprList), _, _ ->
            errorDo(ExpectedToken RightParenthesis, matchExprList) state
            SyntaxExpression.Match(matchToken, leftParenToken, matchExprList, dummyToken(), SyntaxList.Empty(), ep s state) |> Some
        | Some(matchToken), Some(leftParenToken), _, _, _ ->
            errorDo(ExpectedSyntaxAfterToken("expressions to match", leftParenToken.RawToken), leftParenToken) state
            SyntaxExpression.Match(matchToken, leftParenToken, SyntaxSeparatorList.Empty(), dummyToken(), SyntaxList.Empty(), ep s state) |> Some
        | Some(matchToken), _, _, _, _ ->
            errorDo(ExpectedTokenAfterToken(LeftParenthesis, matchToken.RawToken), matchToken) state
            SyntaxExpression.Match(matchToken, dummyToken(), SyntaxSeparatorList.Empty(), dummyToken(), SyntaxList.Empty(), ep s state) |> Some
        | _ ->            
            None
    else
        None

let tryParseWhileExpression state =
    if isNextToken (function While -> true | _ -> false) state then
        let s = sp state

        match bt5 WHILE LEFT_PARENTHESIS (tryParseOffsideExpression SyntaxTreeContextLocal) tryRecoverableRightParenthesis (tryParseOffsideExpression SyntaxTreeContextLocal) state with
        | Some(whileToken), Some(leftParenToken), Some(conditionExpr), Some(rightParenToken), Some(bodyExpr) ->
            SyntaxExpression.While(whileToken, leftParenToken, conditionExpr, rightParenToken, bodyExpr, ep s state) |> Some
        | Some(whileToken), Some(leftParenToken), Some(conditionExpr), Some(rightParenToken), _ ->
            let result = SyntaxExpression.While(whileToken, leftParenToken, conditionExpr, rightParenToken, SyntaxExpression.Error(dummyToken()), ep s state)
            errorDo(ExpectedSyntaxAfterSyntax("loop body expression", "while expression"), result) state
            result |> Some
        | Some(whileToken), Some(leftParenToken), Some(conditionExpr), _, _ ->
            errorDo(ExpectedToken RightParenthesis, conditionExpr) state
            SyntaxExpression.While(whileToken, leftParenToken, conditionExpr, dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state) |> Some
        | Some(whileToken), Some(leftParenToken), _, _, _ ->
            errorDo(ExpectedSyntaxAfterToken("condition expression", leftParenToken.RawToken), leftParenToken) state
            SyntaxExpression.While(whileToken, leftParenToken, SyntaxExpression.Error(dummyToken()), dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state) |> Some
        | Some(whileToken), _, _, _, _ ->
            errorDo(ExpectedTokenAfterToken(LeftParenthesis, whileToken.RawToken), whileToken) state
            SyntaxExpression.While(whileToken, dummyToken(), SyntaxExpression.Error(dummyToken()), dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state) |> Some
        | _ ->            
            None
    else
        None

let tryParseTypeDeclarationName state =
    match bt IDENTIFIER state with
    | Some(ident) ->
        SyntaxTypeDeclarationName.Identifier(ident) |> Some
    | _ ->

    let s = sp state

    match bt tryParseParenthesisTypeOperator state with
    | Some(leftParenToken, operatorToken, rightParenToken) ->
        SyntaxTypeDeclarationName.Parenthesis(leftParenToken, operatorToken, rightParenToken, ep s state) |> Some
    | _ ->

    None

let tryParseTypeDeclarationExpression s attrs (accessor: SyntaxAccessor) state =
    match bt2 tryParseTypeDeclarationKind tryParseTypeDeclarationName state with
    | Some(kind), Some(tyDefName) ->
        let tyPars = parseTypeParameters TypeParameterContext.Default state
        let constrClauseList = parseConstraintClauseList state
        match bt2 EQUAL (tryOffside (liftOpt (parseEntityDefinitionBody false))) state with
        | Some(equalsToken), Some(body) ->
            SyntaxExpression.TypeDeclaration(attrs, accessor, kind, tyDefName, tyPars, constrClauseList, equalsToken, body, ep s state) |> Some
        | Some(equalsToken), _ ->
            errorDo(ExpectedSyntaxAfterToken("declaration body", Equal), equalsToken) state
            SyntaxExpression.TypeDeclaration(attrs, accessor, kind, tyDefName, tyPars, constrClauseList, equalsToken, SyntaxTypeDeclarationBody.None(), ep s state) |> Some
        | _ ->
            SyntaxExpression.TypeDeclaration(attrs, accessor, kind, tyDefName, tyPars, constrClauseList, dummyToken(), SyntaxTypeDeclarationBody.None(), ep s state) |> Some
    | Some(kind), _ ->
        let tyPars = parseTypeParameters TypeParameterContext.Default state
        let constrClauseList = parseConstraintClauseList state
        errorDo(ExpectedSyntaxAfterSyntax("type declaration name", "type declaration kind"), kind) state
        match bt2 EQUAL (tryOffside (liftOpt (parseEntityDefinitionBody false))) state with
        | Some(equalsToken), Some(body) ->
            SyntaxExpression.TypeDeclaration(attrs, accessor, kind, SyntaxTypeDeclarationName.Identifier(dummyToken()), tyPars, constrClauseList, equalsToken, body, ep s state) |> Some
        | Some(equalsToken), _ ->
            errorDo(ExpectedSyntaxAfterToken("declaration body", Equal), equalsToken) state
            SyntaxExpression.TypeDeclaration(attrs, accessor, kind, SyntaxTypeDeclarationName.Identifier(dummyToken()), tyPars, constrClauseList, equalsToken, SyntaxTypeDeclarationBody.None(), ep s state) |> Some
        | _ ->
            SyntaxExpression.TypeDeclaration(attrs, accessor, kind, SyntaxTypeDeclarationName.Identifier(dummyToken()), tyPars, constrClauseList, dummyToken(), SyntaxTypeDeclarationBody.None(), ep s state) |> Some
    | _ ->
        None

let tryParseTypeDeclarationCase state =
    let s = sp state

    match bt2 PIPE IDENTIFIER state with
    | Some(pipeToken), Some(ident) ->
        match bt2 EQUAL (tryParseOffsideExpression SyntaxTreeContextLocal) state with
        | Some(equalToken), Some(expr) ->
            SyntaxTypeDeclarationCase.EnumCase(pipeToken, ident, equalToken, expr, ep s state) |> Some
        | Some(equalToken), _ ->
            errorDo(ExpectedSyntaxAfterToken("expression", Equal), equalToken) state
            SyntaxTypeDeclarationCase.EnumCase(pipeToken, ident, equalToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
        | _ ->
            SyntaxTypeDeclarationCase.Case(pipeToken, ident, ep s state) |> Some
    | Some(pipeToken), _ ->
        errorDo(ExpectedSyntaxAfterToken("identifier", Pipe), pipeToken) state
        SyntaxTypeDeclarationCase.Case(pipeToken, dummyToken(), ep s state) |> Some
    | _ ->

    None

let parseTypeDeclarationCaseList state =
    parseList (alignRecover tryParseTypeDeclarationCase) state

let __parseEntityDefinitionBody_Loop s expr state =
    match bt (tryPeek (ignoreOffside END_OF_SOURCE)) state with
    | Some _ -> expr
    | _ ->
        let expr2 = ignoreOffside (parseExpression SyntaxTreeContext.TopLevel) state
        let seqExpr = SyntaxExpression.Sequential(expr, expr2, ep s state)
        __parseEntityDefinitionBody_Loop s seqExpr state

let parseEntityDefinitionBody isRoot state =
    let s = sp state

    let extends = 
        // Example: alias byte = uint8
        let extendsTyOpt =
            if isRoot then None
            else tryParseEntityExtendsTypeIfNoNewLine state

        match extendsTyOpt with
        | Some extendsTy -> extendsTy
        | _ ->

        alignRecover tryParseEntityExtendsInherits state |> Option.defaultWith (fun _ -> SyntaxExtends.Empty())

    let implements = alignRecover tryParseEntityImplements state |> Option.defaultWith (fun _ -> SyntaxImplements.Empty())
    let caseList = parseTypeDeclarationCaseList state

    let sLoop = sp state
    match bt (tryAlign (tryParseExpression SyntaxTreeContext.TopLevel)) state with
    | Some(expr) ->
        if isRoot then
            let expr2 = __parseEntityDefinitionBody_Loop sLoop expr state
            SyntaxTypeDeclarationBody.Body(extends, implements, caseList, expr2, ep s state)
        else
            SyntaxTypeDeclarationBody.Body(extends, implements, caseList, expr, ep s state)
    | _ ->
                
    if isRoot then
        let expr = __parseEntityDefinitionBody_Loop sLoop (SyntaxExpression.None()) state
        SyntaxTypeDeclarationBody.Body(extends, implements, caseList, expr, ep s state)
    else
        SyntaxTypeDeclarationBody.Body(extends, implements, caseList, SyntaxExpression.None(), ep s state)

let parseNextAlignedExpression s context currentExpr state =

    // For top level contexts, we flex align to prevent infinite loops.
    match context with
    | SyntaxTreeContext.TopLevel ->
        match bt (tryFlexAlign (tryParseExpression context)) state with
        | Some(next) ->
            SyntaxExpression.Sequential(currentExpr, next, ep s state)
        | _ ->
            currentExpr

    | SyntaxTreeContext.Local(skipSequential) ->
        if skipSequential then currentExpr
        else

        match bt (tryAlignIfNoFlex (tryParseExpression context)) state with
        | Some(next) ->
            SyntaxExpression.Sequential(currentExpr, next, ep s state)
        | _ ->

            // Error recovery.
            match bt (tryIfNoNewLine (tryParseName TypeParameterContext.Default)) state with
            | Some(name) ->
                let error = SyntaxExpression.Name(name)
                errorDo(InvalidSyntax "Unexpected syntax.", error) state
                SyntaxExpression.Sequential(currentExpr, error, ep s state)
            | _ ->

            // Error recovery.
            match bt (tryIfNoNewLine tryParseLiteral) state with
            | Some(literal) ->
                let error = SyntaxExpression.Literal(literal)
                errorDo(InvalidSyntax "Unexpected literal.", error) state
                SyntaxExpression.Sequential(currentExpr, error, ep s state)
            | _ ->

            currentExpr
    
let tryParseFlexExpression (context: SyntaxTreeContext) state =
    tryFlex (tryParseExpression context) state

let tryParseExpression (context: SyntaxTreeContext) state =
    match bt (tryPeek (ignoreOffside END_OF_SOURCE)) state with
    | Some _ -> None
    | _ ->

    match bt tryPeekOffsideTerminal state with
    | Some _ -> None
    | _ ->

    parseExpression context state
    |> Some

let parseAlignedExpression context state =
    match bt (tryAlign (tryParseExpression context)) state with
    | Some(expr) ->
        expr
    | _ ->
        SyntaxExpression.None()

let tryParseOffsideExpression (context: SyntaxTreeContext) (state: ParserState) : SyntaxExpression option =
    match bt (tryOffside (tryParseExpression context)) state with
    | Some expr ->
        expr |> Some
    | _ ->
        None

let parseOffsideExpression (context: SyntaxTreeContext, errorNode: ISyntaxNode) (state: ParserState) : SyntaxExpression =
    match bt (tryParseOffsideExpression context) state with
    | Some expr ->
        expr
    | _ ->
        errorDo (InvalidSyntax("Missing expression body."), errorNode) state
        SyntaxExpression.Error(dummyToken())

let tryParsePattern (isLetBinding: bool) (state: ParserState) : SyntaxPattern option =
    let s = sp state

    match bt tryParseLiteral state with
    | Some(literal) ->
        SyntaxPattern.Literal(literal) |> Some
    | _ ->

    match bt UNDERSCORE state with
    | Some(underscoreToken) ->
        SyntaxPattern.Discard(underscoreToken) |> Some
    | _ ->

    if isNextToken (function LeftParenthesis -> true | _ -> false) state then
        match bt (tryParseParenthesisSeparatorList COMMA "pattern" (tryParsePattern false) (fun token -> SyntaxPattern.Error(token) |> Some)) state with
        | Some(leftParenToken, patList, rightParenToken) ->
            SyntaxPattern.Parenthesis(leftParenToken, patList, rightParenToken, ep s state) |> Some
        | _ ->
            None
    else
        if isLetBinding then
            None
        else
            match bt (tryParseName TypeParameterContext.Operator) state with
            | Some(name) ->
                match bt (tryParseParenthesisSeparatorList COMMA "pattern" (tryParsePattern false) (fun token -> SyntaxPattern.Error(token) |> Some)) state with
                | Some(leftParenToken, patList, rightParenToken) ->
                    SyntaxPattern.Function(name, leftParenToken, patList, rightParenToken, ep s state) |> Some
                | _ ->
                    SyntaxPattern.Name(name) |> Some
            | _ ->
                None

let parseMatchGuard (state: ParserState) =
    let s = sp state

    match bt4 WHEN LEFT_PARENTHESIS (tryParseOffsideExpression SyntaxTreeContextLocal) tryRecoverableRightParenthesis state with
    | Some(whenToken), Some(leftParenToken), Some(conditionalExpr), Some(rightParenToken) ->
        SyntaxMatchGuard.MatchGuard(whenToken, leftParenToken, conditionalExpr, rightParenToken, ep s state)

    | Some(whenToken), Some(leftParenToken), Some(conditionalExpr), _ ->
        errorDo(ExpectedToken RightParenthesis, conditionalExpr) state
        SyntaxMatchGuard.MatchGuard(whenToken, leftParenToken, conditionalExpr, dummyToken(), ep s state)

    | Some(whenToken), Some(leftParenToken), _, _ ->
        errorDo(ExpectedSyntax "expression", leftParenToken) state
        SyntaxMatchGuard.MatchGuard(whenToken, leftParenToken, SyntaxExpression.Error(dummyToken()), dummyToken(), ep s state)

    | Some(whenToken), _, _, _ ->
        errorDo(ExpectedSyntax "conditional expression", whenToken) state
        SyntaxMatchGuard.MatchGuard(whenToken, dummyToken(), SyntaxExpression.Error(dummyToken()), dummyToken(), ep s state)

    | _ ->
            
    SyntaxMatchGuard.None()

let tryParseMatchPatternSingleUnderscore state =
    match bt UNDERSCORE state with
    | Some(underscoreToken) ->
        match bt (tryPeek COMMA) state with
        | Some _ -> None
        | _ ->

        SyntaxMatchPattern.Discard(underscoreToken)
        |> Some
    | _ ->

    None

let tryParseMatchPatternLhs state =
    match bt tryParseMatchPatternSingleUnderscore state with
    | Some result -> result |> Some
    | _ ->

    match bt (tryParseListWithSeparatorOld COMMA "pattern" SyntaxPattern.Error (tryParsePattern false)) state with
    | Some(patList) ->
        SyntaxMatchPattern.Patterns(patList) |> Some
    | _ ->

    None

let tryParseMatchPattern (state: ParserState) =
    let s = sp state

    match bt tryParseMatchPatternLhs state with
    | Some(lhsMatchPat) ->
        match bt2 (tryFlexAlign PIPE) tryParseMatchPattern state with
        | Some(pipeToken), Some(rhsMatchPat) ->
            SyntaxMatchPattern.Or(lhsMatchPat, pipeToken, rhsMatchPat, ep s state) |> Some
        | Some(pipeToken), _ ->
            error(ExpectedSyntaxAfterToken("match pattern", pipeToken.RawToken), SyntaxMatchPattern.Or(lhsMatchPat, pipeToken, SyntaxMatchPattern.Error(dummyToken()), ep s state)) state
        | _ ->
                
        Some lhsMatchPat
    | _ ->
                
    None

let tryParseMatchClause (state: ParserState) =
    let s = sp state

    match bt2 PIPE tryParseMatchPattern state with
    | Some(pipeToken), Some(matchPattern) ->
        let matchGuard = parseMatchGuard state

        match bt2 FAT_RIGHT_ARROW (tryParseOffsideExpression (SyntaxTreeContextLocal)) state with
        | Some(fatRightArrowToken), Some(targetExpr) ->
            SyntaxMatchClause.MatchClause(pipeToken, matchPattern, matchGuard, fatRightArrowToken, targetExpr, ep s state) |> Some
        | Some(fatRightArrowToken), _ ->
            errorDo(ExpectedSyntaxAfterToken("expression", fatRightArrowToken.RawToken), fatRightArrowToken) state
            SyntaxMatchClause.MatchClause(pipeToken, matchPattern, matchGuard, fatRightArrowToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
        | _ ->

        error(ExpectedToken(FatRightArrow), SyntaxMatchClause.MatchClause(pipeToken, matchPattern, matchGuard, dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state)) state

    | Some(pipeToken), _ ->
        error(ExpectedSyntaxAfterToken("match pattern", pipeToken.RawToken), SyntaxMatchClause.MatchClause(pipeToken, SyntaxMatchPattern.Error(dummyToken()), SyntaxMatchGuard.None(), dummyToken(), SyntaxExpression.Error(dummyToken(())), ep s state)) state

    | _ ->

    None

let tryParseFieldPattern context state : SyntaxFieldPattern option =
    let s = sp state

    match bt3 IDENTIFIER EQUAL (tryParseOffsideExpression context) state with
    | Some(ident), Some(equalsToken), Some(expr) ->
        SyntaxFieldPattern.FieldPattern(SyntaxName.Identifier(ident), equalsToken, expr, ep s state) |> Some
        
    | Some(ident), Some(equalsToken), _ ->
        errorDo (ExpectedSyntaxAfterToken("expression", Equal), equalsToken) state
        SyntaxFieldPattern.FieldPattern(SyntaxName.Identifier(ident), equalsToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
    | Some(ident), _, _ ->
        error (ExpectedTokenAfterSyntax(Equal, "field label"), SyntaxFieldPattern.FieldPattern(SyntaxName.Identifier(ident), dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state)) state
        
    | _ ->

    None

let tryParseAttribute state =
    match bt (tryOffside tryParseOpenAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryOffside tryParseBlittableAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryOffside tryParsePureAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryOffside tryParseNullAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryOffside tryParseIntrinsicAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryOffside tryParseInlineAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryOffside tryParseUnmanagedAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryOffside tryParseImportAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryOffside tryParseExportAttribute) state with
    | Some attr -> attr |> Some
    | _ ->

    match bt (tryParseOffsideExpression SyntaxTreeContextLocal) state with
    | Some expr ->
        SyntaxAttribute.Expression(expr) |> Some
    | _ ->

    None

let tryParseHashAttribute state =
    let s = sp state

    match bt2 HASH (tryParseBrackets "attribute" SyntaxAttributeErrorF tryParseAttribute) state with
    | Some(hashToken), Some(brackets) ->
        SyntaxHashAttribute.HashAttribute(hashToken, brackets, ep s state) |> Some
    | Some(hashToken), _ ->
        error (SyntaxError.ExpectedSyntax "'[' or directive", SyntaxHashAttribute.HashAttribute(hashToken, SyntaxBrackets.Brackets(dummyToken(), SyntaxAttributeErrorF(dummyToken()), dummyToken(), 0), ep s state)) state
    | _ ->

    None

let tryParseAttributes state =
    match bt (tryParseList (tryFlexAlign tryParseHashAttribute)) state with
    | Some(hashAttrList) ->
        SyntaxAttributes.Attributes(hashAttrList) |> Some
    | _ ->
        None      

let parseAttributes state : SyntaxAttributes =
    if isNextToken (function Hash -> true | _ -> false) state then
        match bt tryParseAttributes state with
        | Some(attrs) -> 
            attrs
        | _ ->
            SyntaxAttributes.Empty()
    else
        SyntaxAttributes.Empty()

let tryParseElseIfOrElseExpression state =
    let s = sp state

    match bt ELSE state with
    | Some(elseToken) ->
        match bt (tryOffside IF) state with
        | Some(ifToken) ->
            match bt3 (tryParseParenthesis (parseOffsideExpression (SyntaxTreeContextLocal, ifToken))) (tryParseOffsideExpression SyntaxTreeContextLocal) (tryFlexAlign tryParseOffsideElseIfOrElseExpression) state with
            | Some(leftParenToken, conditionExpr, rightParenToken), Some(targetExpr), Some(next) ->
                SyntaxElseIfOrElseExpression.ElseIf(elseToken, ifToken, leftParenToken, conditionExpr, rightParenToken, targetExpr, next, ep s state) |> Some
            | Some(leftParenToken, conditionExpr, rightParenToken), Some(targetExpr), _ ->
                SyntaxElseIfOrElseExpression.ElseIf(elseToken, ifToken, leftParenToken, conditionExpr, rightParenToken, targetExpr, SyntaxElseIfOrElseExpression.None(dummyToken ()), ep s state) |> Some
            | Some(leftParenToken, conditionExpr, rightParenToken), _, _ ->
                errorDo(ExpectedSyntaxAfterSyntax("target expression", "conditional expression"), rightParenToken) state
                SyntaxElseIfOrElseExpression.ElseIf(elseToken, ifToken, leftParenToken, conditionExpr, rightParenToken, SyntaxExpression.Error(dummyToken()), SyntaxElseIfOrElseExpression.None(dummyToken()), ep s state) |> Some
            | _ ->
                errorDo(ExpectedSyntaxAfterToken("conditional expression", If), ifToken) state
                SyntaxElseIfOrElseExpression.ElseIf(elseToken, ifToken, dummyToken(), SyntaxExpression.Error(dummyToken()), dummyToken(), SyntaxExpression.Error(dummyToken()), SyntaxElseIfOrElseExpression.None(dummyToken()), ep s state) |> Some
        | _ ->

        match bt (tryParseOffsideExpression SyntaxTreeContextLocal) state with
        | Some(elseBody) ->
            SyntaxElseIfOrElseExpression.Else(elseToken, elseBody, ep s state) |> Some
        | _ ->
            error (SyntaxError.ExpectedSyntaxAfterSyntax("target expression", "\"else\" expression"), SyntaxElseIfOrElseExpression.Else(elseToken, SyntaxExpression.Error(dummyToken()), ep s state)) state
    | _ ->
                        
    None

let tryParseAlignElseIfOrElseExpression state =
    alignRecover tryParseElseIfOrElseExpression state

let parseElseIfOrElseExpression state =
    match bt tryParseAlignElseIfOrElseExpression state with
    | Some(result) -> result
    | _ -> SyntaxElseIfOrElseExpression.None(dummyToken ())

let tryParseOffsideElseIfOrElseExpression state =            
    tryOffside (liftOpt parseElseIfOrElseExpression) state

let possibleNamedArgument state = 
    match bt2 IDENTIFIER EQUAL state with 
    | Some _, Some _ -> true 
    | _ -> false

let tryParseArgument state =
    match (tryPeek possibleNamedArgument) state with
    | true -> None
    | _ -> tryParseOffsideExpression SyntaxTreeContextLocal state

let tryParseNamedArgument state =
    let s = sp state

    match bt3 IDENTIFIER EQUAL (tryParseOffsideExpression SyntaxTreeContextLocal) state with
    | Some(ident), Some(equalToken), Some(expr) ->
        SyntaxNamedArgument.NamedArgument(ident, equalToken, expr, ep s state) |> Some
    | Some(ident), Some(equalToken), _ ->
        errorDo(ExpectedSyntaxAfterToken("expression", Token.Equal), equalToken) state
        SyntaxNamedArgument.NamedArgument(ident, equalToken, SyntaxExpression.Error(dummyToken()), ep s state) |> Some
    | Some(ident), _, _ ->
        errorDo(InvalidSyntax("Expecting an explicitly named argument. i.e. 'argName = <expr>'"), ident) state
        SyntaxNamedArgument.NamedArgument(ident, dummyToken(), SyntaxExpression.Error(dummyToken()), ep s state) |> Some
    | _ ->
        None

let tryParseArguments state =
    let s = sp state

    match bt LEFT_PARENTHESIS state with
    | Some(leftParenthesisToken) as previousTokenOpt ->
        let argumentList = parseSeparatorList previousTokenOpt COMMA "expression" tryParseArgument (fun x -> SyntaxExpression.Error(x) |> Some) state
        let namedArgumentList = parseSeparatorList None COMMA "named argument" tryParseNamedArgument (fun _ -> None) state // TODO: Not passing a 'tryErrorNode', we should so we can get better recovery.
        match bt tryRecoverableRightParenthesis state with
        | Some(rightParenthesisToken) ->
            SyntaxArguments.Arguments(leftParenthesisToken, argumentList, namedArgumentList, rightParenthesisToken, ep s state) |> Some
        | _ ->
            error (ExpectedToken RightParenthesis, SyntaxArguments.Arguments(leftParenthesisToken, argumentList, namedArgumentList, dummyToken (), ep s state)) state
    | _ ->
        None

let tryParseRootNamespace state =
    let s = sp state

    match bt2 NAMESPACE (tryParseName TypeParameterContext.Default) state with
    | Some(namespaceToken), Some(name) ->
        let body = parseEntityDefinitionBody true state
        match bt (ignoreOffside END_OF_SOURCE) state with
        | Some(endOfSourceToken) ->
            SyntaxCompilationUnit.Namespace(namespaceToken, name, body, endOfSourceToken, ep s state) |> Some
        | _ ->
            failwith "Expected end-of-source token."
    | Some(namespaceToken), _ ->
        errorDo(ExpectedSyntaxAfterToken("qualified name or identifier", Namespace), namespaceToken) state
        let body = parseEntityDefinitionBody true state
        match bt (ignoreOffside END_OF_SOURCE) state with
        | Some(endOfSourceToken) ->
            SyntaxCompilationUnit.Namespace(namespaceToken, SyntaxName.Identifier(dummyToken()), body, endOfSourceToken, ep s state) |> Some
        | _ ->
            failwith "Expected end-of-source token."               
    | _ ->

    None

let tryParseRootModule state =
    let s = sp state

    let attrs = parseAttributes state
    let accessor = parseAccessor state

    match bt2 MODULE (tryParseName TypeParameterContext.Default) state with
    | Some(moduleToken), Some(name) ->
        match bt (tryPeek EQUAL) state with
        | Some _ -> None
        | _ ->

        let constrClauseList = parseConstraintClauseList state

        let body = parseEntityDefinitionBody true state
        match bt (ignoreOffside END_OF_SOURCE) state with
        | Some(endOfSourceToken) ->
            SyntaxCompilationUnit.Module(attrs, accessor, moduleToken, name, constrClauseList, body, endOfSourceToken, ep s state) |> Some
        | _ ->
            failwith "Expected end-of-source token."  
    | Some(moduleToken), _ ->
        errorDo(ExpectedSyntaxAfterToken("qualified name or identifier", Module), moduleToken) state

        let constrClauseList = parseConstraintClauseList state

        let body = parseEntityDefinitionBody true state
        match bt (ignoreOffside END_OF_SOURCE) state with
        | Some(endOfSourceToken) ->
            SyntaxCompilationUnit.Module(attrs, accessor, moduleToken, SyntaxName.Identifier(dummyToken()), constrClauseList, body, endOfSourceToken, ep s state) |> Some
        | _ ->
            failwith "Expected end-of-source token."  
    | _ ->

    None

let parseRoot state =
    let s = sp state

    match bt (alignRecover tryParseRootNamespace) state with
    | Some result -> result
    | _ ->

    match bt (alignRecover tryParseRootModule) state with
    | Some result -> result
    | _ ->

    let body = parseEntityDefinitionBody true state
    match bt (ignoreOffside END_OF_SOURCE) state with
    | Some(endOfSourceToken) ->
        SyntaxCompilationUnit.AnonymousModule(body, endOfSourceToken, ep s state)
    | _ ->
        failwith "Expected end-of-source token."  

let parseAux p lexer diagnostics ct =
    let state =
        {
            lexer = lexer
            ct = ct
            diagnostics = diagnostics

            start = 0
            btBufferPosition = 0
            prevOffside = 0
            column = 0
            noErrors = false
            newLine = false

            offside = 0
            offsideFlags = OffsideFlags.None

            btBuffer = Array.zeroCreate MaxBackTrackableTokenAmount
            btBufferCount = 0
            buffers = System.Collections.Generic.Stack()

            peekedPosition = -1
            peekedToken = Unchecked.defaultof<_>
        }
    p state

let Parse (path: OlyPath, lexer, ct) =
    let diagnostics = ConditionalWeakTable()
    let root = parseAux parseRoot lexer diagnostics ct
    SyntaxTree(path, root, diagnostics)