namespace rec Oly.Compiler.Syntax.Internal

#nowarn "3535"
#nowarn "3536"

type internal ISyntaxNode =

    /// Indicates the syntax node is the last node of its siblings.
    abstract IsTerminal : bool

    /// Indicates the syntax node represents a single token.
    abstract IsToken : bool

    /// Indicates the syntax node is an error.
    abstract IsError : bool

    /// Gets a slot (or child) syntax node.
    abstract GetSlot : index: int -> ISyntaxNode

    abstract SlotCount : int

    abstract FullWidth: int

    abstract Tag: int
    abstract InnerTag: int

    static abstract StaticTag: int
    static abstract StaticInnerTag: int

[<Sealed>]
type internal SyntaxDummy() =

    interface ISyntaxNode with
        member _.IsTerminal = true
        member _.IsToken = false
        member _.IsError = false
        member _.GetSlot _ = failwith "Internal error: Syntax node does not exist."
        member _.SlotCount = 0
        member _.FullWidth = 0
        member _.Tag = Tags.Terminal
        member _.InnerTag = Tags.Terminal
        static member StaticTag = Tags.Terminal
        static member StaticInnerTag = Tags.Terminal

[<AutoOpen>]
module internal SyntaxHelpers =
    let dummyToken = SyntaxToken.Token(Dummy)

    [<RequireQualifiedAccess>]
    module Tags =

        [<Literal>]
        let Terminal          = System.Int32.MaxValue

        [<Literal>]
        let Token             = 1024

        [<Literal>]
        let List              = 1025

        [<Literal>]
        let SeparatorList     = 1026

        [<Literal>]
        let Brackets          = 1027

        [<Literal>]
        let CurlyBrackets     = 1028

        [<Literal>]
        let BracketInnerPipes = 1029

    let syntaxTerminal: ISyntaxNode = SyntaxDummy()

type ISyntaxSeparatorList = 

    abstract TryFindIndexByRelativePosition : relativePosition: int32 -> int32

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type internal SyntaxSeparatorList<'T when 'T :> ISyntaxNode> =
    | Empty of unit
    | List of head: 'T * separatorToken: SyntaxToken * tail: SyntaxSeparatorList<'T> * fullWidth: int
    | Error

    member this.Count =
        match this with
        | Empty _ -> 0
        | List(_, _, tail, _) ->
            1 + tail.Count
        | Error -> 0

    member this.Values =
        seq {
            match this with
            | Empty _ -> ()
            | List(head, _, tail, _) ->
                yield head
                yield! tail.Values
            | Error -> ()
        }

    member this.TryHead() =
        match this with
        | List(head=head) -> Some head
        | _ -> None

    interface ISyntaxSeparatorList with

        member this.TryFindIndexByRelativePosition(relativePosition) =
            match this with
            | Empty _ 
            | Error -> 0
            | _ ->
                let rec loop totalWidth node i =
                    match node with
                    | Empty _ 
                    | Error -> i - 1 
                    | List(head, separatorToken, tail, _) ->
                        match separatorToken with
                        | SyntaxToken.Token _ ->
                            if relativePosition <= (totalWidth + head.FullWidth) then
                                i
                            else
                                loop (totalWidth + head.FullWidth + (separatorToken :> ISyntaxNode).FullWidth) tail (i + 1)
                        | SyntaxToken.TokenWithTrivia(leadingTrivia, _, fullWidth) ->
                            if relativePosition <= (totalWidth + head.FullWidth + (leadingTrivia :> ISyntaxNode).FullWidth) then
                                i
                            else
                                loop (totalWidth + head.FullWidth + fullWidth) tail (i + 1)

                loop 0 this 0           

    interface ISyntaxNode with

        member _.IsTerminal = false

        member _.IsToken = false

        member this.IsError = 
            match this with
            | Error -> true
            | _ -> false

        member this.GetSlot index =
            match this with
            | Empty _ ->
                match index with
                | _ -> failwith "invalid slot"
            | List(head, separatorToken, tail, _) ->
                match index with
                | 0 -> head :> ISyntaxNode
                | 1 -> separatorToken :> ISyntaxNode
                | _ -> (tail :> ISyntaxNode).GetSlot(index - 2)
            | Error ->
                match index with
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Empty _ -> 0
            | List(_, _, tail, _) -> (tail :> ISyntaxNode).SlotCount + 2
            | Error -> 0

        member this.FullWidth =
            match this with
            | List(fullWidth=fullWidth) -> fullWidth
            | _ -> 0

        member _.Tag = Tags.SeparatorList
        member _.InnerTag = 'T.StaticTag
        static member StaticTag = Tags.SeparatorList
        static member StaticInnerTag = 'T.StaticTag

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type internal SyntaxList<'T when 'T :> ISyntaxNode> =
    | Empty of unit
    | List of head: 'T * tail: SyntaxList<'T> * fullWidth: int

    member this.Count =
        match this with
        | Empty _ -> 0
        | List(_, tail, _) ->
            1 + tail.Count

    member this.Values =
        seq {
            match this with
            | Empty _ -> ()
            | List(head, tail, _) ->
                yield head
                yield! tail.Values
        }

    interface ISyntaxNode with

        member _.IsTerminal = false

        member _.IsToken = false

        member _.IsError = false

        member this.GetSlot index =
            match this with
            | Empty _ ->
                match index with
                | _ -> failwith "invalid slot"
            | List(head, tail, _) ->
                match index with
                | 0 -> head :> ISyntaxNode
                | _ -> (tail :> ISyntaxNode).GetSlot(index - 1)

        member this.SlotCount =
            match this with
            | Empty _ -> 0
            | List(_, tail, _) -> (tail :> ISyntaxNode).SlotCount + 1

        member this.FullWidth =
            match this with
            | List(fullWidth=fullWidth) -> fullWidth
            | _ -> 0

        member _.Tag = Tags.List
        member _.InnerTag = 'T.StaticTag
        static member StaticTag = Tags.List
        static member StaticInnerTag = 'T.StaticTag

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type internal SyntaxBrackets<'T when 'T :> ISyntaxNode> =
    | Brackets
        of
        leftBracketToken: SyntaxToken *
        element: 'T *
        rightBracketToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with
    
        member this.IsTerminal = false
    
        member this.IsToken = false
    
        member this.IsError = false
    
        member this.GetSlot(index) =
            match this with
            | Brackets(leftBracketToken, element, rightBracketToken, _) ->
                match index with
                | 0 -> leftBracketToken :> ISyntaxNode
                | 1 -> element :> ISyntaxNode
                | 2 -> rightBracketToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
    
        member this.SlotCount =
            match this with
            | Brackets _ -> 3

        member this.FullWidth =
            match this with
            | Brackets(fullWidth=fullWidth) -> fullWidth

        member _.Tag = Tags.Brackets
        member _.InnerTag = 'T.StaticTag
        static member StaticTag = Tags.Brackets
        static member StaticInnerTag = 'T.StaticTag

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type internal SyntaxBracketInnerPipes<'T when 'T :> ISyntaxNode> =
    | BracketInnerPipes
        of
        leftBracketInnerPipeToken: SyntaxToken *
        element: 'T *
        rightBracketInnerPipeToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with
    
        member this.IsTerminal = false
    
        member this.IsToken = false
    
        member this.IsError = false
    
        member this.GetSlot(index) =
            match this with
            | BracketInnerPipes(leftBracketInnerPipeToken, element, rightBracketInnerPipeToken, _) ->
                match index with
                | 0 -> leftBracketInnerPipeToken :> ISyntaxNode
                | 1 -> element :> ISyntaxNode
                | 2 -> rightBracketInnerPipeToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
    
        member this.SlotCount =
            match this with
            | BracketInnerPipes _ -> 3

        member this.FullWidth =
            match this with
            | BracketInnerPipes(fullWidth=fullWidth) -> fullWidth

        member _.Tag = Tags.BracketInnerPipes
        member _.InnerTag = 'T.StaticTag
        static member StaticTag = Tags.BracketInnerPipes
        static member StaticInnerTag = 'T.StaticTag

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type internal SyntaxCurlyBrackets<'T when 'T :> ISyntaxNode> =
    | CurlyBrackets
        of
        leftCurlyBracketToken: SyntaxToken *
        element: 'T *
        rightCurlyBracketToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with
    
        member this.IsTerminal = false
    
        member this.IsToken = false
    
        member this.IsError = false
    
        member this.GetSlot(index) =
            match this with
            | CurlyBrackets(leftCurlyBracketToken, element, rightCurlyBracketToken, _) ->
                match index with
                | 0 -> leftCurlyBracketToken :> ISyntaxNode
                | 1 -> element :> ISyntaxNode
                | 2 -> rightCurlyBracketToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
    
        member this.SlotCount =
            match this with
            | CurlyBrackets _ -> 3

        member this.FullWidth =
            match this with
            | CurlyBrackets(fullWidth=fullWidth) -> fullWidth

        member _.Tag = Tags.CurlyBrackets
        member _.InnerTag = 'T.StaticTag
        static member StaticTag = Tags.CurlyBrackets
        static member StaticInnerTag = 'T.StaticTag

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type internal SyntaxToken =
    | Token of token: Token
    | TokenWithTrivia
        of
        leadingTrivia: SyntaxToken *
        token: Token *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = true

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Token _ ->
                failwith "invalid slot"
            | _ when index > 1 || index < 0 ->
                failwith "invalid slot"
            | TokenWithTrivia(leadingTrivia, _, _) ->
                leadingTrivia

        member this.SlotCount =
            match this with
            | Token _ ->
                0
            | TokenWithTrivia _ ->
                1

        member this.FullWidth =
            match this with
            | TokenWithTrivia(fullWidth=fullWidth) -> fullWidth
            | Token(token) -> token.Width

        member _.Tag = Tags.Token
        member _.InnerTag = Tags.Terminal
        static member StaticTag = Tags.Token
        static member StaticInnerTag = Tags.Terminal
