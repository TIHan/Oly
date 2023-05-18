// Generated File Do Not Modify
[<AutoOpen>]
module internal rec Oly.Compiler.Syntax.Internal.Generated

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxAccessor =
    | Public
        of
        token: SyntaxToken
    | Private
        of
        token: SyntaxToken
    | Internal
        of
        token: SyntaxToken
    | Protected
        of
        token: SyntaxToken
    | None
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Public(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Private(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Internal(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Protected(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Public _ -> 1
            | Private _ -> 1
            | Internal _ -> 1
            | Protected _ -> 1
            | None _ -> 0

        member this.FullWidth =
            match this with
            | Public(x) ->
                (x :> ISyntaxNode).FullWidth
            | Private(x) ->
                (x :> ISyntaxNode).FullWidth
            | Internal(x) ->
                (x :> ISyntaxNode).FullWidth
            | Protected(x) ->
                (x :> ISyntaxNode).FullWidth
            | None _ ->
                0

        member _.Tag = 0

[<RequireQualifiedAccess>]
module SyntaxAccessor =

    [<Literal>]
    let Tag = 0

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxName =
    | Identifier
        of
        ident: SyntaxToken
    | Generic
        of
        name: SyntaxName *
        tyArgs: SyntaxTypeArguments *
        fullWidth: int
    | Qualified
        of
        head: SyntaxName *
        dotToken: SyntaxToken *
        tail: SyntaxName *
        fullWidth: int
    | Parenthesis
        of
        leftParenToken: SyntaxToken *
        identOrOperatorToken: SyntaxToken *
        rightParenToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Identifier(ident) ->
                match index with
                | 0 -> ident :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Generic(name, tyArgs, _) ->
                match index with
                | 0 -> name :> ISyntaxNode
                | 1 -> tyArgs :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Qualified(head, dotToken, tail, _) ->
                match index with
                | 0 -> head :> ISyntaxNode
                | 1 -> dotToken :> ISyntaxNode
                | 2 -> tail :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Parenthesis(leftParenToken, identOrOperatorToken, rightParenToken, _) ->
                match index with
                | 0 -> leftParenToken :> ISyntaxNode
                | 1 -> identOrOperatorToken :> ISyntaxNode
                | 2 -> rightParenToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Identifier _ -> 1
            | Generic _ -> 2
            | Qualified _ -> 3
            | Parenthesis _ -> 3

        member this.FullWidth =
            match this with
            | Identifier(x) ->
                (x :> ISyntaxNode).FullWidth
            | Generic(fullWidth=fullWidth) ->
                fullWidth
            | Qualified(fullWidth=fullWidth) ->
                fullWidth
            | Parenthesis(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 1

[<RequireQualifiedAccess>]
module SyntaxName =

    [<Literal>]
    let Tag = 1

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxBlittable =
    | Blittable
        of
        blittableToken: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Blittable(blittableToken) ->
                match index with
                | 0 -> blittableToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Blittable _ -> 1

        member this.FullWidth =
            match this with
            | Blittable(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 2

[<RequireQualifiedAccess>]
module SyntaxBlittable =

    [<Literal>]
    let Tag = 2

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxBlittableOptional =
    | Some
        of
        blittable: SyntaxBlittable
    | None
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Some(blittable) ->
                match index with
                | 0 -> blittable :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Some _ -> 1
            | None _ -> 0

        member this.FullWidth =
            match this with
            | Some(x) ->
                (x :> ISyntaxNode).FullWidth
            | None _ ->
                0

        member _.Tag = 3

[<RequireQualifiedAccess>]
module SyntaxBlittableOptional =

    [<Literal>]
    let Tag = 3

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxAttribute =
    | AutoOpen
        of
        openToken: SyntaxToken
    | Nullable
        of
        nullToken: SyntaxToken
    | Blittable
        of
        blittable: SyntaxBlittable
    | Intrinsic
        of
        intrinsicToken: SyntaxToken *
        leftParenthesisToken: SyntaxToken *
        ident: SyntaxToken *
        rightParenthesisToken: SyntaxToken *
        fullWidth: int
    | Import
        of
        importName: SyntaxName *
        args: SyntaxArguments *
        fullWidth: int
    | Export
        of
        exportToken: SyntaxToken
    | Inline
        of
        inlineToken: SyntaxToken *
        leftParenthesisToken: SyntaxToken *
        ident: SyntaxToken *
        rightParenthesisToken: SyntaxToken *
        fullWidth: int
    | Pure
        of
        pureToken: SyntaxToken
    | Expression
        of
        expr: SyntaxExpression
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | AutoOpen(openToken) ->
                match index with
                | 0 -> openToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Nullable(nullToken) ->
                match index with
                | 0 -> nullToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Blittable(blittable) ->
                match index with
                | 0 -> blittable :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Intrinsic(intrinsicToken, leftParenthesisToken, ident, rightParenthesisToken, _) ->
                match index with
                | 0 -> intrinsicToken :> ISyntaxNode
                | 1 -> leftParenthesisToken :> ISyntaxNode
                | 2 -> ident :> ISyntaxNode
                | 3 -> rightParenthesisToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Import(importName, args, _) ->
                match index with
                | 0 -> importName :> ISyntaxNode
                | 1 -> args :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Export(exportToken) ->
                match index with
                | 0 -> exportToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Inline(inlineToken, leftParenthesisToken, ident, rightParenthesisToken, _) ->
                match index with
                | 0 -> inlineToken :> ISyntaxNode
                | 1 -> leftParenthesisToken :> ISyntaxNode
                | 2 -> ident :> ISyntaxNode
                | 3 -> rightParenthesisToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Pure(pureToken) ->
                match index with
                | 0 -> pureToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Expression(expr) ->
                match index with
                | 0 -> expr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | AutoOpen _ -> 1
            | Nullable _ -> 1
            | Blittable _ -> 1
            | Intrinsic _ -> 4
            | Import _ -> 2
            | Export _ -> 1
            | Inline _ -> 4
            | Pure _ -> 1
            | Expression _ -> 1
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | AutoOpen(x) ->
                (x :> ISyntaxNode).FullWidth
            | Nullable(x) ->
                (x :> ISyntaxNode).FullWidth
            | Blittable(x) ->
                (x :> ISyntaxNode).FullWidth
            | Intrinsic(fullWidth=fullWidth) ->
                fullWidth
            | Import(fullWidth=fullWidth) ->
                fullWidth
            | Export(x) ->
                (x :> ISyntaxNode).FullWidth
            | Inline(fullWidth=fullWidth) ->
                fullWidth
            | Pure(x) ->
                (x :> ISyntaxNode).FullWidth
            | Expression(x) ->
                (x :> ISyntaxNode).FullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 4

[<RequireQualifiedAccess>]
module SyntaxAttribute =

    [<Literal>]
    let Tag = 4

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxHashAttribute =
    | HashAttribute
        of
        hashToken: SyntaxToken *
        brackets: SyntaxAttribute SyntaxBrackets *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | HashAttribute(hashToken, brackets, _) ->
                match index with
                | 0 -> hashToken :> ISyntaxNode
                | 1 -> brackets :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | HashAttribute _ -> 2

        member this.FullWidth =
            match this with
            | HashAttribute(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 5

[<RequireQualifiedAccess>]
module SyntaxHashAttribute =

    [<Literal>]
    let Tag = 5

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxAttributes =
    | Attributes
        of
        hashAttrList: SyntaxHashAttribute SyntaxList
    | Empty
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Attributes(hashAttrList) ->
                match index with
                | 0 -> hashAttrList :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Empty _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Attributes _ -> 1
            | Empty _ -> 0

        member this.FullWidth =
            match this with
            | Attributes(x) ->
                (x :> ISyntaxNode).FullWidth
            | Empty _ ->
                0

        member _.Tag = 6

[<RequireQualifiedAccess>]
module SyntaxAttributes =

    [<Literal>]
    let Tag = 6

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxConstraint =
    | Null
        of
        nullToken: SyntaxToken
    | Struct
        of
        structToken: SyntaxToken
    | NotStruct
        of
        notToken: SyntaxToken *
        structToken: SyntaxToken *
        fullWidth: int
    | Unmanaged
        of
        unmanagedToken: SyntaxToken
    | Type
        of
        ty: SyntaxType
    | Error
        of
        token: SyntaxToken
    | ConstantType
        of
        constantToken: SyntaxToken *
        ty: SyntaxType *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | Null(nullToken) ->
                match index with
                | 0 -> nullToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Struct(structToken) ->
                match index with
                | 0 -> structToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | NotStruct(notToken, structToken, _) ->
                match index with
                | 0 -> notToken :> ISyntaxNode
                | 1 -> structToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Unmanaged(unmanagedToken) ->
                match index with
                | 0 -> unmanagedToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Type(ty) ->
                match index with
                | 0 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | ConstantType(constantToken, ty, _) ->
                match index with
                | 0 -> constantToken :> ISyntaxNode
                | 1 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Null _ -> 1
            | Struct _ -> 1
            | NotStruct _ -> 2
            | Unmanaged _ -> 1
            | Type _ -> 1
            | Error _ -> 1
            | ConstantType _ -> 2

        member this.FullWidth =
            match this with
            | Null(x) ->
                (x :> ISyntaxNode).FullWidth
            | Struct(x) ->
                (x :> ISyntaxNode).FullWidth
            | NotStruct(fullWidth=fullWidth) ->
                fullWidth
            | Unmanaged(x) ->
                (x :> ISyntaxNode).FullWidth
            | Type(x) ->
                (x :> ISyntaxNode).FullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth
            | ConstantType(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 7

[<RequireQualifiedAccess>]
module SyntaxConstraint =

    [<Literal>]
    let Tag = 7

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxConstraintClause =
    | ConstraintClause
        of
        whereToken: SyntaxToken *
        ty: SyntaxType *
        colonToken: SyntaxToken *
        constrs: SyntaxConstraint SyntaxSeparatorList *
        fullWidth: int
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | ConstraintClause(whereToken, ty, colonToken, constrs, _) ->
                match index with
                | 0 -> whereToken :> ISyntaxNode
                | 1 -> ty :> ISyntaxNode
                | 2 -> colonToken :> ISyntaxNode
                | 3 -> constrs :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | ConstraintClause _ -> 4
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | ConstraintClause(fullWidth=fullWidth) ->
                fullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 8

[<RequireQualifiedAccess>]
module SyntaxConstraintClause =

    [<Literal>]
    let Tag = 8

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxTypeParameters =
    | TypeParameters
        of
        lessThanToken: SyntaxToken *
        tyParList: SyntaxType SyntaxSeparatorList *
        greaterThanToken: SyntaxToken *
        fullWidth: int
    | RequireTypeParameters
        of
        lessThanToken: SyntaxToken *
        requireToken: SyntaxToken *
        tyParList: SyntaxType SyntaxSeparatorList *
        greaterThanToken: SyntaxToken *
        fullWidth: int
    | Empty
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | TypeParameters(lessThanToken, tyParList, greaterThanToken, _) ->
                match index with
                | 0 -> lessThanToken :> ISyntaxNode
                | 1 -> tyParList :> ISyntaxNode
                | 2 -> greaterThanToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | RequireTypeParameters(lessThanToken, requireToken, tyParList, greaterThanToken, _) ->
                match index with
                | 0 -> lessThanToken :> ISyntaxNode
                | 1 -> requireToken :> ISyntaxNode
                | 2 -> tyParList :> ISyntaxNode
                | 3 -> greaterThanToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Empty _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | TypeParameters _ -> 3
            | RequireTypeParameters _ -> 4
            | Empty _ -> 0

        member this.FullWidth =
            match this with
            | TypeParameters(fullWidth=fullWidth) ->
                fullWidth
            | RequireTypeParameters(fullWidth=fullWidth) ->
                fullWidth
            | Empty _ ->
                0

        member _.Tag = 9

[<RequireQualifiedAccess>]
module SyntaxTypeParameters =

    [<Literal>]
    let Tag = 9

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxTypeConstructor =
    | Identifier
        of
        identifierToken: SyntaxToken *
        tyPars: SyntaxTypeParameters *
        fullWidth: int
    | QualifiedName
        of
        identifierToken: SyntaxToken *
        tyPars: SyntaxTypeParameters *
        dotToken: SyntaxToken *
        rest: SyntaxTypeConstructor *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Identifier(identifierToken, tyPars, _) ->
                match index with
                | 0 -> identifierToken :> ISyntaxNode
                | 1 -> tyPars :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | QualifiedName(identifierToken, tyPars, dotToken, rest, _) ->
                match index with
                | 0 -> identifierToken :> ISyntaxNode
                | 1 -> tyPars :> ISyntaxNode
                | 2 -> dotToken :> ISyntaxNode
                | 3 -> rest :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Identifier _ -> 2
            | QualifiedName _ -> 4

        member this.FullWidth =
            match this with
            | Identifier(fullWidth=fullWidth) ->
                fullWidth
            | QualifiedName(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 10

[<RequireQualifiedAccess>]
module SyntaxTypeConstructor =

    [<Literal>]
    let Tag = 10

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxTupleElement =
    | Type
        of
        ty: SyntaxType
    | IdentifierWithTypeAnnotation
        of
        identToken: SyntaxToken *
        colonToken: SyntaxToken *
        ty: SyntaxType *
        fullWidth: int
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | Type(ty) ->
                match index with
                | 0 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | IdentifierWithTypeAnnotation(identToken, colonToken, ty, _) ->
                match index with
                | 0 -> identToken :> ISyntaxNode
                | 1 -> colonToken :> ISyntaxNode
                | 2 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Type _ -> 1
            | IdentifierWithTypeAnnotation _ -> 3
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | Type(x) ->
                (x :> ISyntaxNode).FullWidth
            | IdentifierWithTypeAnnotation(fullWidth=fullWidth) ->
                fullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 11

[<RequireQualifiedAccess>]
module SyntaxTupleElement =

    [<Literal>]
    let Tag = 11

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxType =
    | Name
        of
        name: SyntaxName
    | Tuple
        of
        leftParenToken: SyntaxToken *
        tupleElementList: SyntaxTupleElement SyntaxSeparatorList *
        rightParenToken: SyntaxToken *
        fullWidth: int
    | Variadic
        of
        ident: SyntaxToken *
        dotDotDotToken: SyntaxToken *
        fullWidth: int
    | VariadicIndexer
        of
        ident: SyntaxToken *
        dotDotDotToken: SyntaxToken *
        leftBracketToken: SyntaxToken *
        expr: SyntaxExpression *
        rightBracketToken: SyntaxToken *
        fullWidth: int
    | Array
        of
        elementTy: SyntaxType *
        brackets: SyntaxToken SyntaxList SyntaxBrackets *
        fullWidth: int
    | MutableArray
        of
        elementTy: SyntaxType *
        bracketInnerPipes: SyntaxToken SyntaxList SyntaxBracketInnerPipes *
        fullWidth: int
    | Shape
        of
        curlyBrackets: SyntaxExpression SyntaxSeparatorList SyntaxCurlyBrackets
    | WildCard
        of
        underscore: SyntaxToken
    | Function
        of
        inputTy: SyntaxType *
        rightArrowToken: SyntaxToken *
        outputTy: SyntaxType *
        fullWidth: int
    | FunctionPtr
        of
        staticToken: SyntaxToken *
        blittableOptional: SyntaxBlittableOptional *
        inputTy: SyntaxType *
        rightArrowToken: SyntaxToken *
        outputTy: SyntaxType *
        fullWidth: int
    | Postfix
        of
        elementTy: SyntaxType *
        ident: SyntaxToken *
        fullWidth: int
    | Literal
        of
        literal: SyntaxLiteral
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | Name(name) ->
                match index with
                | 0 -> name :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Tuple(leftParenToken, tupleElementList, rightParenToken, _) ->
                match index with
                | 0 -> leftParenToken :> ISyntaxNode
                | 1 -> tupleElementList :> ISyntaxNode
                | 2 -> rightParenToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Variadic(ident, dotDotDotToken, _) ->
                match index with
                | 0 -> ident :> ISyntaxNode
                | 1 -> dotDotDotToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | VariadicIndexer(ident, dotDotDotToken, leftBracketToken, expr, rightBracketToken, _) ->
                match index with
                | 0 -> ident :> ISyntaxNode
                | 1 -> dotDotDotToken :> ISyntaxNode
                | 2 -> leftBracketToken :> ISyntaxNode
                | 3 -> expr :> ISyntaxNode
                | 4 -> rightBracketToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Array(elementTy, brackets, _) ->
                match index with
                | 0 -> elementTy :> ISyntaxNode
                | 1 -> brackets :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | MutableArray(elementTy, bracketInnerPipes, _) ->
                match index with
                | 0 -> elementTy :> ISyntaxNode
                | 1 -> bracketInnerPipes :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Shape(curlyBrackets) ->
                match index with
                | 0 -> curlyBrackets :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | WildCard(underscore) ->
                match index with
                | 0 -> underscore :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Function(inputTy, rightArrowToken, outputTy, _) ->
                match index with
                | 0 -> inputTy :> ISyntaxNode
                | 1 -> rightArrowToken :> ISyntaxNode
                | 2 -> outputTy :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | FunctionPtr(staticToken, blittableOptional, inputTy, rightArrowToken, outputTy, _) ->
                match index with
                | 0 -> staticToken :> ISyntaxNode
                | 1 -> blittableOptional :> ISyntaxNode
                | 2 -> inputTy :> ISyntaxNode
                | 3 -> rightArrowToken :> ISyntaxNode
                | 4 -> outputTy :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Postfix(elementTy, ident, _) ->
                match index with
                | 0 -> elementTy :> ISyntaxNode
                | 1 -> ident :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Literal(literal) ->
                match index with
                | 0 -> literal :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Name _ -> 1
            | Tuple _ -> 3
            | Variadic _ -> 2
            | VariadicIndexer _ -> 5
            | Array _ -> 2
            | MutableArray _ -> 2
            | Shape _ -> 1
            | WildCard _ -> 1
            | Function _ -> 3
            | FunctionPtr _ -> 5
            | Postfix _ -> 2
            | Literal _ -> 1
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | Name(x) ->
                (x :> ISyntaxNode).FullWidth
            | Tuple(fullWidth=fullWidth) ->
                fullWidth
            | Variadic(fullWidth=fullWidth) ->
                fullWidth
            | VariadicIndexer(fullWidth=fullWidth) ->
                fullWidth
            | Array(fullWidth=fullWidth) ->
                fullWidth
            | MutableArray(fullWidth=fullWidth) ->
                fullWidth
            | Shape(x) ->
                (x :> ISyntaxNode).FullWidth
            | WildCard(x) ->
                (x :> ISyntaxNode).FullWidth
            | Function(fullWidth=fullWidth) ->
                fullWidth
            | FunctionPtr(fullWidth=fullWidth) ->
                fullWidth
            | Postfix(fullWidth=fullWidth) ->
                fullWidth
            | Literal(x) ->
                (x :> ISyntaxNode).FullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 12

[<RequireQualifiedAccess>]
module SyntaxType =

    [<Literal>]
    let Tag = 12

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxMutability =
    | Mutable
        of
        mutableToken: SyntaxToken
    | None
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Mutable(mutableToken) ->
                match index with
                | 0 -> mutableToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Mutable _ -> 1
            | None _ -> 0

        member this.FullWidth =
            match this with
            | Mutable(x) ->
                (x :> ISyntaxNode).FullWidth
            | None _ ->
                0

        member _.Tag = 13

[<RequireQualifiedAccess>]
module SyntaxMutability =

    [<Literal>]
    let Tag = 13

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxParameter =
    | Identifier
        of
        attrs: SyntaxAttributes *
        mutability: SyntaxMutability *
        identToken: SyntaxToken *
        fullWidth: int
    | IdentifierWithTypeAnnotation
        of
        attrs: SyntaxAttributes *
        mutability: SyntaxMutability *
        identToken: SyntaxToken *
        colonToken: SyntaxToken *
        ty: SyntaxType *
        fullWidth: int
    | Type
        of
        attrs: SyntaxAttributes *
        ty: SyntaxType *
        fullWidth: int
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | Identifier(attrs, mutability, identToken, _) ->
                match index with
                | 0 -> attrs :> ISyntaxNode
                | 1 -> mutability :> ISyntaxNode
                | 2 -> identToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | IdentifierWithTypeAnnotation(attrs, mutability, identToken, colonToken, ty, _) ->
                match index with
                | 0 -> attrs :> ISyntaxNode
                | 1 -> mutability :> ISyntaxNode
                | 2 -> identToken :> ISyntaxNode
                | 3 -> colonToken :> ISyntaxNode
                | 4 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Type(attrs, ty, _) ->
                match index with
                | 0 -> attrs :> ISyntaxNode
                | 1 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Identifier _ -> 3
            | IdentifierWithTypeAnnotation _ -> 5
            | Type _ -> 2
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | Identifier(fullWidth=fullWidth) ->
                fullWidth
            | IdentifierWithTypeAnnotation(fullWidth=fullWidth) ->
                fullWidth
            | Type(fullWidth=fullWidth) ->
                fullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 14

[<RequireQualifiedAccess>]
module SyntaxParameter =

    [<Literal>]
    let Tag = 14

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxTypeArguments =
    | TypeArguments
        of
        lessThanToken: SyntaxToken *
        tyArgList: SyntaxType SyntaxSeparatorList *
        greaterThanToken: SyntaxToken *
        fullWidth: int
    | Empty
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | TypeArguments(lessThanToken, tyArgList, greaterThanToken, _) ->
                match index with
                | 0 -> lessThanToken :> ISyntaxNode
                | 1 -> tyArgList :> ISyntaxNode
                | 2 -> greaterThanToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Empty _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | TypeArguments _ -> 3
            | Empty _ -> 0

        member this.FullWidth =
            match this with
            | TypeArguments(fullWidth=fullWidth) ->
                fullWidth
            | Empty _ ->
                0

        member _.Tag = 15

[<RequireQualifiedAccess>]
module SyntaxTypeArguments =

    [<Literal>]
    let Tag = 15

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxParameters =
    | Parameters
        of
        leftParenthesisToken: SyntaxToken *
        parList: SyntaxParameter SyntaxSeparatorList *
        rightParenthesisToken: SyntaxToken *
        fullWidth: int
    | Empty
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Parameters(leftParenthesisToken, parList, rightParenthesisToken, _) ->
                match index with
                | 0 -> leftParenthesisToken :> ISyntaxNode
                | 1 -> parList :> ISyntaxNode
                | 2 -> rightParenthesisToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Empty _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Parameters _ -> 3
            | Empty _ -> 0

        member this.FullWidth =
            match this with
            | Parameters(fullWidth=fullWidth) ->
                fullWidth
            | Empty _ ->
                0

        member _.Tag = 16

[<RequireQualifiedAccess>]
module SyntaxParameters =

    [<Literal>]
    let Tag = 16

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxLambdaKind =
    | Static
        of
        staticToken: SyntaxToken
    | None
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Static(staticToken) ->
                match index with
                | 0 -> staticToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Static _ -> 1
            | None _ -> 0

        member this.FullWidth =
            match this with
            | Static(x) ->
                (x :> ISyntaxNode).FullWidth
            | None _ ->
                0

        member _.Tag = 17

[<RequireQualifiedAccess>]
module SyntaxLambdaKind =

    [<Literal>]
    let Tag = 17

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxReturnTypeAnnotation =
    | TypeAnnotation
        of
        colonToken: SyntaxToken *
        ty: SyntaxType *
        fullWidth: int
    | None
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | TypeAnnotation(colonToken, ty, _) ->
                match index with
                | 0 -> colonToken :> ISyntaxNode
                | 1 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | TypeAnnotation _ -> 2
            | None _ -> 0

        member this.FullWidth =
            match this with
            | TypeAnnotation(fullWidth=fullWidth) ->
                fullWidth
            | None _ ->
                0

        member _.Tag = 18

[<RequireQualifiedAccess>]
module SyntaxReturnTypeAnnotation =

    [<Literal>]
    let Tag = 18

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxFunctionName =
    | Identifier
        of
        ident: SyntaxToken
    | Parenthesis
        of
        leftParenToken: SyntaxToken *
        operatorToken: SyntaxToken *
        rightParenToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Identifier(ident) ->
                match index with
                | 0 -> ident :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Parenthesis(leftParenToken, operatorToken, rightParenToken, _) ->
                match index with
                | 0 -> leftParenToken :> ISyntaxNode
                | 1 -> operatorToken :> ISyntaxNode
                | 2 -> rightParenToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Identifier _ -> 1
            | Parenthesis _ -> 3

        member this.FullWidth =
            match this with
            | Identifier(x) ->
                (x :> ISyntaxNode).FullWidth
            | Parenthesis(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 19

[<RequireQualifiedAccess>]
module SyntaxFunctionName =

    [<Literal>]
    let Tag = 19

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxBindingDeclaration =
    | Function
        of
        funcName: SyntaxFunctionName *
        tyPars: SyntaxTypeParameters *
        pars: SyntaxParameters *
        returnTyAnnot: SyntaxReturnTypeAnnotation *
        constrClauseList: SyntaxConstraintClause SyntaxSeparatorList *
        fullWidth: int
    | Value
        of
        identifierToken: SyntaxToken *
        returnTyAnnot: SyntaxReturnTypeAnnotation *
        fullWidth: int
    | New
        of
        newToken: SyntaxToken *
        pars: SyntaxParameters *
        fullWidth: int
    | Get
        of
        getToken: SyntaxToken
    | Set
        of
        setToken: SyntaxToken
    | Getter
        of
        getToken: SyntaxToken *
        pars: SyntaxParameters *
        fullWidth: int
    | Setter
        of
        setToken: SyntaxToken *
        pars: SyntaxParameters *
        fullWidth: int
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | Function(funcName, tyPars, pars, returnTyAnnot, constrClauseList, _) ->
                match index with
                | 0 -> funcName :> ISyntaxNode
                | 1 -> tyPars :> ISyntaxNode
                | 2 -> pars :> ISyntaxNode
                | 3 -> returnTyAnnot :> ISyntaxNode
                | 4 -> constrClauseList :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Value(identifierToken, returnTyAnnot, _) ->
                match index with
                | 0 -> identifierToken :> ISyntaxNode
                | 1 -> returnTyAnnot :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | New(newToken, pars, _) ->
                match index with
                | 0 -> newToken :> ISyntaxNode
                | 1 -> pars :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Get(getToken) ->
                match index with
                | 0 -> getToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Set(setToken) ->
                match index with
                | 0 -> setToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Getter(getToken, pars, _) ->
                match index with
                | 0 -> getToken :> ISyntaxNode
                | 1 -> pars :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Setter(setToken, pars, _) ->
                match index with
                | 0 -> setToken :> ISyntaxNode
                | 1 -> pars :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Function _ -> 5
            | Value _ -> 2
            | New _ -> 2
            | Get _ -> 1
            | Set _ -> 1
            | Getter _ -> 2
            | Setter _ -> 2
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | Function(fullWidth=fullWidth) ->
                fullWidth
            | Value(fullWidth=fullWidth) ->
                fullWidth
            | New(fullWidth=fullWidth) ->
                fullWidth
            | Get(x) ->
                (x :> ISyntaxNode).FullWidth
            | Set(x) ->
                (x :> ISyntaxNode).FullWidth
            | Getter(fullWidth=fullWidth) ->
                fullWidth
            | Setter(fullWidth=fullWidth) ->
                fullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 20

[<RequireQualifiedAccess>]
module SyntaxBindingDeclaration =

    [<Literal>]
    let Tag = 20

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxPropertyBinding =
    | Binding
        of
        attrs: SyntaxAttributes *
        accessor: SyntaxAccessor *
        premodifierList: SyntaxValueDeclarationPremodifier SyntaxList *
        kind: SyntaxValueDeclarationKind *
        binding: SyntaxBinding *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Binding(attrs, accessor, premodifierList, kind, binding, _) ->
                match index with
                | 0 -> attrs :> ISyntaxNode
                | 1 -> accessor :> ISyntaxNode
                | 2 -> premodifierList :> ISyntaxNode
                | 3 -> kind :> ISyntaxNode
                | 4 -> binding :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Binding _ -> 5

        member this.FullWidth =
            match this with
            | Binding(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 21

[<RequireQualifiedAccess>]
module SyntaxPropertyBinding =

    [<Literal>]
    let Tag = 21

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxGuardBinding =
    | Implementation
        of
        whenToken: SyntaxToken *
        identTokenOptional: SyntaxToken *
        leftParenToken: SyntaxToken *
        condExpr: SyntaxExpression *
        rightParenToken: SyntaxToken *
        fatRightArrowToken: SyntaxToken *
        rhsExpr: SyntaxExpression *
        fullWidth: int
    | Signature
        of
        whenToken: SyntaxToken *
        identTokenOptional: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Implementation(whenToken, identTokenOptional, leftParenToken, condExpr, rightParenToken, fatRightArrowToken, rhsExpr, _) ->
                match index with
                | 0 -> whenToken :> ISyntaxNode
                | 1 -> identTokenOptional :> ISyntaxNode
                | 2 -> leftParenToken :> ISyntaxNode
                | 3 -> condExpr :> ISyntaxNode
                | 4 -> rightParenToken :> ISyntaxNode
                | 5 -> fatRightArrowToken :> ISyntaxNode
                | 6 -> rhsExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Signature(whenToken, identTokenOptional, _) ->
                match index with
                | 0 -> whenToken :> ISyntaxNode
                | 1 -> identTokenOptional :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Implementation _ -> 7
            | Signature _ -> 2

        member this.FullWidth =
            match this with
            | Implementation(fullWidth=fullWidth) ->
                fullWidth
            | Signature(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 22

[<RequireQualifiedAccess>]
module SyntaxGuardBinding =

    [<Literal>]
    let Tag = 22

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxBinding =
    | Implementation
        of
        bindingDecl: SyntaxBindingDeclaration *
        equalToken: SyntaxToken *
        rhsExpr: SyntaxExpression *
        fullWidth: int
    | Signature
        of
        bindingDecl: SyntaxBindingDeclaration
    | Property
        of
        bindingDecl: SyntaxBindingDeclaration *
        bindingList: SyntaxPropertyBinding SyntaxSeparatorList *
        fullWidth: int
    | PropertyWithDefault
        of
        bindingDecl: SyntaxBindingDeclaration *
        bindingList: SyntaxPropertyBinding SyntaxSeparatorList *
        equalToken: SyntaxToken *
        rhsExpr: SyntaxExpression *
        fullWidth: int
    | PatternWithGuard
        of
        bindingDecl: SyntaxBindingDeclaration *
        guardBinding: SyntaxGuardBinding *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Implementation(bindingDecl, equalToken, rhsExpr, _) ->
                match index with
                | 0 -> bindingDecl :> ISyntaxNode
                | 1 -> equalToken :> ISyntaxNode
                | 2 -> rhsExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Signature(bindingDecl) ->
                match index with
                | 0 -> bindingDecl :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Property(bindingDecl, bindingList, _) ->
                match index with
                | 0 -> bindingDecl :> ISyntaxNode
                | 1 -> bindingList :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | PropertyWithDefault(bindingDecl, bindingList, equalToken, rhsExpr, _) ->
                match index with
                | 0 -> bindingDecl :> ISyntaxNode
                | 1 -> bindingList :> ISyntaxNode
                | 2 -> equalToken :> ISyntaxNode
                | 3 -> rhsExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | PatternWithGuard(bindingDecl, guardBinding, _) ->
                match index with
                | 0 -> bindingDecl :> ISyntaxNode
                | 1 -> guardBinding :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Implementation _ -> 3
            | Signature _ -> 1
            | Property _ -> 2
            | PropertyWithDefault _ -> 4
            | PatternWithGuard _ -> 2

        member this.FullWidth =
            match this with
            | Implementation(fullWidth=fullWidth) ->
                fullWidth
            | Signature(x) ->
                (x :> ISyntaxNode).FullWidth
            | Property(fullWidth=fullWidth) ->
                fullWidth
            | PropertyWithDefault(fullWidth=fullWidth) ->
                fullWidth
            | PatternWithGuard(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 23

[<RequireQualifiedAccess>]
module SyntaxBinding =

    [<Literal>]
    let Tag = 23

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxTypeDeclarationKind =
    | Alias
        of
        aliasToken: SyntaxToken
    | Class
        of
        classToken: SyntaxToken
    | AbstractClass
        of
        abstractToken: SyntaxToken *
        classToken: SyntaxToken *
        fullWidth: int
    | SealedClass
        of
        sealedToken: SyntaxToken *
        classToken: SyntaxToken *
        fullWidth: int
    | Interface
        of
        interfaceToken: SyntaxToken
    | SealedInterface
        of
        sealedToken: SyntaxToken *
        interfaceToken: SyntaxToken *
        fullWidth: int
    | Module
        of
        moduleToken: SyntaxToken
    | Shape
        of
        shapeToken: SyntaxToken
    | Struct
        of
        structToken: SyntaxToken
    | Extension
        of
        extensionToken: SyntaxToken
    | Enum
        of
        enumToken: SyntaxToken
    | Newtype
        of
        newtypeToken: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Alias(aliasToken) ->
                match index with
                | 0 -> aliasToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Class(classToken) ->
                match index with
                | 0 -> classToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | AbstractClass(abstractToken, classToken, _) ->
                match index with
                | 0 -> abstractToken :> ISyntaxNode
                | 1 -> classToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | SealedClass(sealedToken, classToken, _) ->
                match index with
                | 0 -> sealedToken :> ISyntaxNode
                | 1 -> classToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Interface(interfaceToken) ->
                match index with
                | 0 -> interfaceToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | SealedInterface(sealedToken, interfaceToken, _) ->
                match index with
                | 0 -> sealedToken :> ISyntaxNode
                | 1 -> interfaceToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Module(moduleToken) ->
                match index with
                | 0 -> moduleToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Shape(shapeToken) ->
                match index with
                | 0 -> shapeToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Struct(structToken) ->
                match index with
                | 0 -> structToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Extension(extensionToken) ->
                match index with
                | 0 -> extensionToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Enum(enumToken) ->
                match index with
                | 0 -> enumToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Newtype(newtypeToken) ->
                match index with
                | 0 -> newtypeToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Alias _ -> 1
            | Class _ -> 1
            | AbstractClass _ -> 2
            | SealedClass _ -> 2
            | Interface _ -> 1
            | SealedInterface _ -> 2
            | Module _ -> 1
            | Shape _ -> 1
            | Struct _ -> 1
            | Extension _ -> 1
            | Enum _ -> 1
            | Newtype _ -> 1

        member this.FullWidth =
            match this with
            | Alias(x) ->
                (x :> ISyntaxNode).FullWidth
            | Class(x) ->
                (x :> ISyntaxNode).FullWidth
            | AbstractClass(fullWidth=fullWidth) ->
                fullWidth
            | SealedClass(fullWidth=fullWidth) ->
                fullWidth
            | Interface(x) ->
                (x :> ISyntaxNode).FullWidth
            | SealedInterface(fullWidth=fullWidth) ->
                fullWidth
            | Module(x) ->
                (x :> ISyntaxNode).FullWidth
            | Shape(x) ->
                (x :> ISyntaxNode).FullWidth
            | Struct(x) ->
                (x :> ISyntaxNode).FullWidth
            | Extension(x) ->
                (x :> ISyntaxNode).FullWidth
            | Enum(x) ->
                (x :> ISyntaxNode).FullWidth
            | Newtype(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 24

[<RequireQualifiedAccess>]
module SyntaxTypeDeclarationKind =

    [<Literal>]
    let Tag = 24

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxLiteral =
    | Int8
        of
        valueToken: SyntaxToken
    | UInt8
        of
        valueToken: SyntaxToken
    | Int16
        of
        valueToken: SyntaxToken
    | UInt16
        of
        valueToken: SyntaxToken
    | Int32
        of
        valueToken: SyntaxToken
    | UInt32
        of
        valueToken: SyntaxToken
    | Int64
        of
        valueToken: SyntaxToken
    | UInt64
        of
        valueToken: SyntaxToken
    | Float32
        of
        valueToken: SyntaxToken
    | Float64
        of
        valueToken: SyntaxToken
    | Bool
        of
        valueToken: SyntaxToken
    | Char16
        of
        valueToken: SyntaxToken
    | Utf16
        of
        valueToken: SyntaxToken
    | Null
        of
        nullToken: SyntaxToken
    | Default
        of
        defaultToken: SyntaxToken
    | UncheckedDefault
        of
        uncheckedToken: SyntaxToken *
        defaultToken: SyntaxToken *
        fullWidth: int
    | Integer
        of
        valueToken: SyntaxToken
    | Real
        of
        valueToken: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Int8(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | UInt8(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Int16(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | UInt16(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Int32(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | UInt32(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Int64(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | UInt64(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Float32(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Float64(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Bool(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Char16(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Utf16(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Null(nullToken) ->
                match index with
                | 0 -> nullToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Default(defaultToken) ->
                match index with
                | 0 -> defaultToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | UncheckedDefault(uncheckedToken, defaultToken, _) ->
                match index with
                | 0 -> uncheckedToken :> ISyntaxNode
                | 1 -> defaultToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Integer(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Real(valueToken) ->
                match index with
                | 0 -> valueToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Int8 _ -> 1
            | UInt8 _ -> 1
            | Int16 _ -> 1
            | UInt16 _ -> 1
            | Int32 _ -> 1
            | UInt32 _ -> 1
            | Int64 _ -> 1
            | UInt64 _ -> 1
            | Float32 _ -> 1
            | Float64 _ -> 1
            | Bool _ -> 1
            | Char16 _ -> 1
            | Utf16 _ -> 1
            | Null _ -> 1
            | Default _ -> 1
            | UncheckedDefault _ -> 2
            | Integer _ -> 1
            | Real _ -> 1

        member this.FullWidth =
            match this with
            | Int8(x) ->
                (x :> ISyntaxNode).FullWidth
            | UInt8(x) ->
                (x :> ISyntaxNode).FullWidth
            | Int16(x) ->
                (x :> ISyntaxNode).FullWidth
            | UInt16(x) ->
                (x :> ISyntaxNode).FullWidth
            | Int32(x) ->
                (x :> ISyntaxNode).FullWidth
            | UInt32(x) ->
                (x :> ISyntaxNode).FullWidth
            | Int64(x) ->
                (x :> ISyntaxNode).FullWidth
            | UInt64(x) ->
                (x :> ISyntaxNode).FullWidth
            | Float32(x) ->
                (x :> ISyntaxNode).FullWidth
            | Float64(x) ->
                (x :> ISyntaxNode).FullWidth
            | Bool(x) ->
                (x :> ISyntaxNode).FullWidth
            | Char16(x) ->
                (x :> ISyntaxNode).FullWidth
            | Utf16(x) ->
                (x :> ISyntaxNode).FullWidth
            | Null(x) ->
                (x :> ISyntaxNode).FullWidth
            | Default(x) ->
                (x :> ISyntaxNode).FullWidth
            | UncheckedDefault(fullWidth=fullWidth) ->
                fullWidth
            | Integer(x) ->
                (x :> ISyntaxNode).FullWidth
            | Real(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 25

[<RequireQualifiedAccess>]
module SyntaxLiteral =

    [<Literal>]
    let Tag = 25

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxFieldPattern =
    | FieldPattern
        of
        name: SyntaxName *
        equalsToken: SyntaxToken *
        expr: SyntaxExpression *
        fullWidth: int
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | FieldPattern(name, equalsToken, expr, _) ->
                match index with
                | 0 -> name :> ISyntaxNode
                | 1 -> equalsToken :> ISyntaxNode
                | 2 -> expr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | FieldPattern _ -> 3
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | FieldPattern(fullWidth=fullWidth) ->
                fullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 26

[<RequireQualifiedAccess>]
module SyntaxFieldPattern =

    [<Literal>]
    let Tag = 26

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxNamedArgument =
    | NamedArgument
        of
        ident: SyntaxToken *
        equalToken: SyntaxToken *
        rhsExpr: SyntaxExpression *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | NamedArgument(ident, equalToken, rhsExpr, _) ->
                match index with
                | 0 -> ident :> ISyntaxNode
                | 1 -> equalToken :> ISyntaxNode
                | 2 -> rhsExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | NamedArgument _ -> 3

        member this.FullWidth =
            match this with
            | NamedArgument(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 27

[<RequireQualifiedAccess>]
module SyntaxNamedArgument =

    [<Literal>]
    let Tag = 27

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxArguments =
    | Arguments
        of
        leftParenthesisToken: SyntaxToken *
        argumentList: SyntaxExpression SyntaxSeparatorList *
        namedArgumentList: SyntaxNamedArgument SyntaxSeparatorList *
        rightParenthesisToken: SyntaxToken *
        fullWidth: int
    | Empty
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Arguments(leftParenthesisToken, argumentList, namedArgumentList, rightParenthesisToken, _) ->
                match index with
                | 0 -> leftParenthesisToken :> ISyntaxNode
                | 1 -> argumentList :> ISyntaxNode
                | 2 -> namedArgumentList :> ISyntaxNode
                | 3 -> rightParenthesisToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Empty _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Arguments _ -> 4
            | Empty _ -> 0

        member this.FullWidth =
            match this with
            | Arguments(fullWidth=fullWidth) ->
                fullWidth
            | Empty _ ->
                0

        member _.Tag = 28

[<RequireQualifiedAccess>]
module SyntaxArguments =

    [<Literal>]
    let Tag = 28

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxElseIfOrElseExpression =
    | ElseIf
        of
        elseToken: SyntaxToken *
        ifToken: SyntaxToken *
        leftParenToken: SyntaxToken *
        conditionExpr: SyntaxExpression *
        rightParenToken: SyntaxToken *
        targetExpr: SyntaxExpression *
        nextExpr: SyntaxElseIfOrElseExpression *
        fullWidth: int
    | Else
        of
        elseToken: SyntaxToken *
        targetExpr: SyntaxExpression *
        fullWidth: int
    | None
        of
        terminalToken: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | ElseIf(elseToken, ifToken, leftParenToken, conditionExpr, rightParenToken, targetExpr, nextExpr, _) ->
                match index with
                | 0 -> elseToken :> ISyntaxNode
                | 1 -> ifToken :> ISyntaxNode
                | 2 -> leftParenToken :> ISyntaxNode
                | 3 -> conditionExpr :> ISyntaxNode
                | 4 -> rightParenToken :> ISyntaxNode
                | 5 -> targetExpr :> ISyntaxNode
                | 6 -> nextExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Else(elseToken, targetExpr, _) ->
                match index with
                | 0 -> elseToken :> ISyntaxNode
                | 1 -> targetExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None(terminalToken) ->
                match index with
                | 0 -> terminalToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | ElseIf _ -> 7
            | Else _ -> 2
            | None _ -> 1

        member this.FullWidth =
            match this with
            | ElseIf(fullWidth=fullWidth) ->
                fullWidth
            | Else(fullWidth=fullWidth) ->
                fullWidth
            | None(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 29

[<RequireQualifiedAccess>]
module SyntaxElseIfOrElseExpression =

    [<Literal>]
    let Tag = 29

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxCatchOrFinallyExpression =
    | Catch
        of
        catchToken: SyntaxToken *
        leftParenToken: SyntaxToken *
        par: SyntaxParameter *
        rightParenToken: SyntaxToken *
        fatRightToken: SyntaxToken *
        catchBodyExpr: SyntaxExpression *
        nextExpr: SyntaxCatchOrFinallyExpression *
        fullWidth: int
    | Finally
        of
        finallyToken: SyntaxToken *
        finallyBodyExpr: SyntaxExpression *
        fullWidth: int
    | None
        of
        terminalToken: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Catch(catchToken, leftParenToken, par, rightParenToken, fatRightToken, catchBodyExpr, nextExpr, _) ->
                match index with
                | 0 -> catchToken :> ISyntaxNode
                | 1 -> leftParenToken :> ISyntaxNode
                | 2 -> par :> ISyntaxNode
                | 3 -> rightParenToken :> ISyntaxNode
                | 4 -> fatRightToken :> ISyntaxNode
                | 5 -> catchBodyExpr :> ISyntaxNode
                | 6 -> nextExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Finally(finallyToken, finallyBodyExpr, _) ->
                match index with
                | 0 -> finallyToken :> ISyntaxNode
                | 1 -> finallyBodyExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None(terminalToken) ->
                match index with
                | 0 -> terminalToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Catch _ -> 7
            | Finally _ -> 2
            | None _ -> 1

        member this.FullWidth =
            match this with
            | Catch(fullWidth=fullWidth) ->
                fullWidth
            | Finally(fullWidth=fullWidth) ->
                fullWidth
            | None(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 30

[<RequireQualifiedAccess>]
module SyntaxCatchOrFinallyExpression =

    [<Literal>]
    let Tag = 30

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxValueDeclarationPremodifier =
    | Static
        of
        staticToken: SyntaxToken
    | Overrides
        of
        overridesToken: SyntaxToken
    | Abstract
        of
        abstractToken: SyntaxToken
    | Default
        of
        defaultToken: SyntaxToken
    | Mutable
        of
        mutableToken: SyntaxToken
    | New
        of
        newToken: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Static(staticToken) ->
                match index with
                | 0 -> staticToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Overrides(overridesToken) ->
                match index with
                | 0 -> overridesToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Abstract(abstractToken) ->
                match index with
                | 0 -> abstractToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Default(defaultToken) ->
                match index with
                | 0 -> defaultToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Mutable(mutableToken) ->
                match index with
                | 0 -> mutableToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | New(newToken) ->
                match index with
                | 0 -> newToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Static _ -> 1
            | Overrides _ -> 1
            | Abstract _ -> 1
            | Default _ -> 1
            | Mutable _ -> 1
            | New _ -> 1

        member this.FullWidth =
            match this with
            | Static(x) ->
                (x :> ISyntaxNode).FullWidth
            | Overrides(x) ->
                (x :> ISyntaxNode).FullWidth
            | Abstract(x) ->
                (x :> ISyntaxNode).FullWidth
            | Default(x) ->
                (x :> ISyntaxNode).FullWidth
            | Mutable(x) ->
                (x :> ISyntaxNode).FullWidth
            | New(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 31

[<RequireQualifiedAccess>]
module SyntaxValueDeclarationPremodifier =

    [<Literal>]
    let Tag = 31

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxValueDeclarationPostmodifier =
    | Mutable
        of
        mutableToken: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Mutable(mutableToken) ->
                match index with
                | 0 -> mutableToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Mutable _ -> 1

        member this.FullWidth =
            match this with
            | Mutable(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 32

[<RequireQualifiedAccess>]
module SyntaxValueDeclarationPostmodifier =

    [<Literal>]
    let Tag = 32

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxValueDeclarationKind =
    | Let
        of
        letToken: SyntaxToken
    | LetBind
        of
        letExclamationToken: SyntaxToken
    | Constant
        of
        constantToken: SyntaxToken
    | Pattern
        of
        patternToken: SyntaxToken
    | Field
        of
        fieldToken: SyntaxToken
    | None
        of unit
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | Let(letToken) ->
                match index with
                | 0 -> letToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | LetBind(letExclamationToken) ->
                match index with
                | 0 -> letExclamationToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Constant(constantToken) ->
                match index with
                | 0 -> constantToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Pattern(patternToken) ->
                match index with
                | 0 -> patternToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Field(fieldToken) ->
                match index with
                | 0 -> fieldToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Let _ -> 1
            | LetBind _ -> 1
            | Constant _ -> 1
            | Pattern _ -> 1
            | Field _ -> 1
            | None _ -> 0
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | Let(x) ->
                (x :> ISyntaxNode).FullWidth
            | LetBind(x) ->
                (x :> ISyntaxNode).FullWidth
            | Constant(x) ->
                (x :> ISyntaxNode).FullWidth
            | Pattern(x) ->
                (x :> ISyntaxNode).FullWidth
            | Field(x) ->
                (x :> ISyntaxNode).FullWidth
            | None _ ->
                0
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 33

[<RequireQualifiedAccess>]
module SyntaxValueDeclarationKind =

    [<Literal>]
    let Tag = 33

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxExtends =
    | Type
        of
        ty: SyntaxType
    | Inherits
        of
        inheritsToken: SyntaxToken *
        tys: SyntaxType SyntaxSeparatorList *
        fullWidth: int
    | Empty
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Type(ty) ->
                match index with
                | 0 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Inherits(inheritsToken, tys, _) ->
                match index with
                | 0 -> inheritsToken :> ISyntaxNode
                | 1 -> tys :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Empty _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Type _ -> 1
            | Inherits _ -> 2
            | Empty _ -> 0

        member this.FullWidth =
            match this with
            | Type(x) ->
                (x :> ISyntaxNode).FullWidth
            | Inherits(fullWidth=fullWidth) ->
                fullWidth
            | Empty _ ->
                0

        member _.Tag = 34

[<RequireQualifiedAccess>]
module SyntaxExtends =

    [<Literal>]
    let Tag = 34

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxImplements =
    | Implements
        of
        implementsToken: SyntaxToken *
        tys: SyntaxType SyntaxSeparatorList *
        fullWidth: int
    | Empty
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Implements(implementsToken, tys, _) ->
                match index with
                | 0 -> implementsToken :> ISyntaxNode
                | 1 -> tys :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Empty _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Implements _ -> 2
            | Empty _ -> 0

        member this.FullWidth =
            match this with
            | Implements(fullWidth=fullWidth) ->
                fullWidth
            | Empty _ ->
                0

        member _.Tag = 35

[<RequireQualifiedAccess>]
module SyntaxImplements =

    [<Literal>]
    let Tag = 35

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxTypeDeclarationCase =
    | Case
        of
        pipeToken: SyntaxToken *
        ident: SyntaxToken *
        fullWidth: int
    | EnumCase
        of
        pipeToken: SyntaxToken *
        ident: SyntaxToken *
        equalToken: SyntaxToken *
        rhsExpr: SyntaxExpression *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Case(pipeToken, ident, _) ->
                match index with
                | 0 -> pipeToken :> ISyntaxNode
                | 1 -> ident :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | EnumCase(pipeToken, ident, equalToken, rhsExpr, _) ->
                match index with
                | 0 -> pipeToken :> ISyntaxNode
                | 1 -> ident :> ISyntaxNode
                | 2 -> equalToken :> ISyntaxNode
                | 3 -> rhsExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Case _ -> 2
            | EnumCase _ -> 4

        member this.FullWidth =
            match this with
            | Case(fullWidth=fullWidth) ->
                fullWidth
            | EnumCase(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 36

[<RequireQualifiedAccess>]
module SyntaxTypeDeclarationCase =

    [<Literal>]
    let Tag = 36

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxTypeDeclarationBody =
    | Body
        of
        extends: SyntaxExtends *
        implements: SyntaxImplements *
        caseList: SyntaxTypeDeclarationCase SyntaxList *
        bodyExpr: SyntaxExpression *
        fullWidth: int
    | None
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Body(extends, implements, caseList, bodyExpr, _) ->
                match index with
                | 0 -> extends :> ISyntaxNode
                | 1 -> implements :> ISyntaxNode
                | 2 -> caseList :> ISyntaxNode
                | 3 -> bodyExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Body _ -> 4
            | None _ -> 0

        member this.FullWidth =
            match this with
            | Body(fullWidth=fullWidth) ->
                fullWidth
            | None _ ->
                0

        member _.Tag = 37

[<RequireQualifiedAccess>]
module SyntaxTypeDeclarationBody =

    [<Literal>]
    let Tag = 37

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxTypeDeclarationName =
    | Identifier
        of
        ident: SyntaxToken
    | Parenthesis
        of
        leftParenToken: SyntaxToken *
        operatorToken: SyntaxToken *
        rightParenToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Identifier(ident) ->
                match index with
                | 0 -> ident :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Parenthesis(leftParenToken, operatorToken, rightParenToken, _) ->
                match index with
                | 0 -> leftParenToken :> ISyntaxNode
                | 1 -> operatorToken :> ISyntaxNode
                | 2 -> rightParenToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Identifier _ -> 1
            | Parenthesis _ -> 3

        member this.FullWidth =
            match this with
            | Identifier(x) ->
                (x :> ISyntaxNode).FullWidth
            | Parenthesis(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 38

[<RequireQualifiedAccess>]
module SyntaxTypeDeclarationName =

    [<Literal>]
    let Tag = 38

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxPattern =
    | Literal
        of
        literal: SyntaxLiteral
    | Name
        of
        name: SyntaxName
    | Function
        of
        name: SyntaxName *
        leftParenToken: SyntaxToken *
        patArgs: SyntaxPattern SyntaxSeparatorList *
        rightParenToken: SyntaxToken *
        fullWidth: int
    | Parenthesis
        of
        leftParenToken: SyntaxToken *
        patArgs: SyntaxPattern SyntaxSeparatorList *
        rightParenToken: SyntaxToken *
        fullWidth: int
    | Discard
        of
        underscoreToken: SyntaxToken
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | Literal(literal) ->
                match index with
                | 0 -> literal :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Name(name) ->
                match index with
                | 0 -> name :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Function(name, leftParenToken, patArgs, rightParenToken, _) ->
                match index with
                | 0 -> name :> ISyntaxNode
                | 1 -> leftParenToken :> ISyntaxNode
                | 2 -> patArgs :> ISyntaxNode
                | 3 -> rightParenToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Parenthesis(leftParenToken, patArgs, rightParenToken, _) ->
                match index with
                | 0 -> leftParenToken :> ISyntaxNode
                | 1 -> patArgs :> ISyntaxNode
                | 2 -> rightParenToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Discard(underscoreToken) ->
                match index with
                | 0 -> underscoreToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Literal _ -> 1
            | Name _ -> 1
            | Function _ -> 4
            | Parenthesis _ -> 3
            | Discard _ -> 1
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | Literal(x) ->
                (x :> ISyntaxNode).FullWidth
            | Name(x) ->
                (x :> ISyntaxNode).FullWidth
            | Function(fullWidth=fullWidth) ->
                fullWidth
            | Parenthesis(fullWidth=fullWidth) ->
                fullWidth
            | Discard(x) ->
                (x :> ISyntaxNode).FullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 39

[<RequireQualifiedAccess>]
module SyntaxPattern =

    [<Literal>]
    let Tag = 39

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxMatchGuard =
    | MatchGuard
        of
        whenToken: SyntaxToken *
        leftParenToken: SyntaxToken *
        conditionExpr: SyntaxExpression *
        rightParenToken: SyntaxToken *
        fullWidth: int
    | None
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | MatchGuard(whenToken, leftParenToken, conditionExpr, rightParenToken, _) ->
                match index with
                | 0 -> whenToken :> ISyntaxNode
                | 1 -> leftParenToken :> ISyntaxNode
                | 2 -> conditionExpr :> ISyntaxNode
                | 3 -> rightParenToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | MatchGuard _ -> 4
            | None _ -> 0

        member this.FullWidth =
            match this with
            | MatchGuard(fullWidth=fullWidth) ->
                fullWidth
            | None _ ->
                0

        member _.Tag = 40

[<RequireQualifiedAccess>]
module SyntaxMatchGuard =

    [<Literal>]
    let Tag = 40

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxMatchPattern =
    | Patterns
        of
        patList: SyntaxPattern SyntaxSeparatorList
    | Or
        of
        lhs: SyntaxMatchPattern *
        pipeToken: SyntaxToken *
        rhs: SyntaxMatchPattern *
        fullWidth: int
    | Discard
        of
        underscoreToken: SyntaxToken
    | Error
        of
        token: SyntaxToken

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | Patterns(patList) ->
                match index with
                | 0 -> patList :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Or(lhs, pipeToken, rhs, _) ->
                match index with
                | 0 -> lhs :> ISyntaxNode
                | 1 -> pipeToken :> ISyntaxNode
                | 2 -> rhs :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Discard(underscoreToken) ->
                match index with
                | 0 -> underscoreToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Patterns _ -> 1
            | Or _ -> 3
            | Discard _ -> 1
            | Error _ -> 1

        member this.FullWidth =
            match this with
            | Patterns(x) ->
                (x :> ISyntaxNode).FullWidth
            | Or(fullWidth=fullWidth) ->
                fullWidth
            | Discard(x) ->
                (x :> ISyntaxNode).FullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth

        member _.Tag = 41

[<RequireQualifiedAccess>]
module SyntaxMatchPattern =

    [<Literal>]
    let Tag = 41

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxMatchClause =
    | MatchClause
        of
        pipeToken: SyntaxToken *
        matchPat: SyntaxMatchPattern *
        matchGuard: SyntaxMatchGuard *
        fatRightArrow: SyntaxToken *
        targetExpr: SyntaxExpression *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | MatchClause(pipeToken, matchPat, matchGuard, fatRightArrow, targetExpr, _) ->
                match index with
                | 0 -> pipeToken :> ISyntaxNode
                | 1 -> matchPat :> ISyntaxNode
                | 2 -> matchGuard :> ISyntaxNode
                | 3 -> fatRightArrow :> ISyntaxNode
                | 4 -> targetExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | MatchClause _ -> 5

        member this.FullWidth =
            match this with
            | MatchClause(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 42

[<RequireQualifiedAccess>]
module SyntaxMatchClause =

    [<Literal>]
    let Tag = 42

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxConstructType =
    | Anonymous
        of
        leftCurlyBracketToken: SyntaxToken *
        fieldPatList: SyntaxFieldPattern SyntaxSeparatorList *
        rightCurlyBracketToken: SyntaxToken *
        fullWidth: int
    | Named
        of
        name: SyntaxName *
        leftCurlyBracketToken: SyntaxToken *
        fieldPatList: SyntaxFieldPattern SyntaxSeparatorList *
        rightCurlyBracketToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Anonymous(leftCurlyBracketToken, fieldPatList, rightCurlyBracketToken, _) ->
                match index with
                | 0 -> leftCurlyBracketToken :> ISyntaxNode
                | 1 -> fieldPatList :> ISyntaxNode
                | 2 -> rightCurlyBracketToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Named(name, leftCurlyBracketToken, fieldPatList, rightCurlyBracketToken, _) ->
                match index with
                | 0 -> name :> ISyntaxNode
                | 1 -> leftCurlyBracketToken :> ISyntaxNode
                | 2 -> fieldPatList :> ISyntaxNode
                | 3 -> rightCurlyBracketToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Anonymous _ -> 3
            | Named _ -> 4

        member this.FullWidth =
            match this with
            | Anonymous(fullWidth=fullWidth) ->
                fullWidth
            | Named(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 43

[<RequireQualifiedAccess>]
module SyntaxConstructType =

    [<Literal>]
    let Tag = 43

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxCompilationUnit =
    | Namespace
        of
        namespaceToken: SyntaxToken *
        name: SyntaxName *
        tyDefBody: SyntaxTypeDeclarationBody *
        terminalToken: SyntaxToken *
        fullWidth: int
    | Module
        of
        attrs: SyntaxAttributes *
        accessor: SyntaxAccessor *
        moduleToken: SyntaxToken *
        name: SyntaxName *
        constrClauseList: SyntaxConstraintClause SyntaxSeparatorList *
        tyDefBody: SyntaxTypeDeclarationBody *
        terminalToken: SyntaxToken *
        fullWidth: int
    | AnonymousModule
        of
        tyDefBody: SyntaxTypeDeclarationBody *
        terminalToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Namespace(namespaceToken, name, tyDefBody, terminalToken, _) ->
                match index with
                | 0 -> namespaceToken :> ISyntaxNode
                | 1 -> name :> ISyntaxNode
                | 2 -> tyDefBody :> ISyntaxNode
                | 3 -> terminalToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Module(attrs, accessor, moduleToken, name, constrClauseList, tyDefBody, terminalToken, _) ->
                match index with
                | 0 -> attrs :> ISyntaxNode
                | 1 -> accessor :> ISyntaxNode
                | 2 -> moduleToken :> ISyntaxNode
                | 3 -> name :> ISyntaxNode
                | 4 -> constrClauseList :> ISyntaxNode
                | 5 -> tyDefBody :> ISyntaxNode
                | 6 -> terminalToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | AnonymousModule(tyDefBody, terminalToken, _) ->
                match index with
                | 0 -> tyDefBody :> ISyntaxNode
                | 1 -> terminalToken :> ISyntaxNode
                | _ -> failwith "invalid slot"

        member this.SlotCount =
            match this with
            | Namespace _ -> 4
            | Module _ -> 7
            | AnonymousModule _ -> 2

        member this.FullWidth =
            match this with
            | Namespace(fullWidth=fullWidth) ->
                fullWidth
            | Module(fullWidth=fullWidth) ->
                fullWidth
            | AnonymousModule(fullWidth=fullWidth) ->
                fullWidth

        member _.Tag = 44

[<RequireQualifiedAccess>]
module SyntaxCompilationUnit =

    [<Literal>]
    let Tag = 44

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxExpression =
    | OpenDeclaration
        of
        openToken: SyntaxToken *
        name: SyntaxName *
        fullWidth: int
    | OpenStaticDeclaration
        of
        openToken: SyntaxToken *
        staticToken: SyntaxToken *
        name: SyntaxName *
        fullWidth: int
    | OpenExtensionDeclaration
        of
        openToken: SyntaxToken *
        extensionToken: SyntaxToken *
        name: SyntaxName *
        fullWidth: int
    | Sequential
        of
        expr1: SyntaxExpression *
        expr2: SyntaxExpression *
        fullWidth: int
    | Parenthesis
        of
        leftParenToken: SyntaxToken *
        exprList: SyntaxExpression SyntaxSeparatorList *
        rightParenToken: SyntaxToken *
        fullWidth: int
    | Array
        of
        leftBracketToken: SyntaxToken *
        elements: SyntaxExpression SyntaxSeparatorList *
        rightBracketToken: SyntaxToken *
        fullWidth: int
    | MutableArray
        of
        leftBracketInnerPipeToken: SyntaxToken *
        elements: SyntaxExpression SyntaxSeparatorList *
        rightBracketInnerPipeToken: SyntaxToken *
        fullWidth: int
    | Call
        of
        expression: SyntaxExpression *
        arguments: SyntaxArguments *
        fullWidth: int
    | InfixCall
        of
        left: SyntaxExpression *
        name: SyntaxName *
        right: SyntaxExpression *
        fullWidth: int
    | PrefixCall
        of
        name: SyntaxName *
        arg: SyntaxExpression *
        fullWidth: int
    | Throw
        of
        throwToken: SyntaxToken *
        arg: SyntaxExpression *
        fullWidth: int
    | Indexer
        of
        lhsExpr: SyntaxExpression *
        brackets: SyntaxExpression SyntaxSeparatorList SyntaxBrackets *
        fullWidth: int
    | Name
        of
        name: SyntaxName
    | Literal
        of
        lit: SyntaxLiteral
    | CreateRecord
        of
        constructTy: SyntaxConstructType
    | UpdateRecord
        of
        expr: SyntaxExpression *
        withToken: SyntaxToken *
        constructTy: SyntaxConstructType *
        fullWidth: int
    | If
        of
        ifToken: SyntaxToken *
        leftParenToken: SyntaxToken *
        predicateExpr: SyntaxExpression *
        rightParenToken: SyntaxToken *
        target: SyntaxExpression *
        elseIfOrElseExpr: SyntaxElseIfOrElseExpression *
        fullWidth: int
    | Try
        of
        tryToken: SyntaxToken *
        bodyExpr: SyntaxExpression *
        catchOrFinallyExpr: SyntaxCatchOrFinallyExpression *
        fullWidth: int
    | Match
        of
        matchToken: SyntaxToken *
        leftParenToken: SyntaxToken *
        matchExprList: SyntaxExpression SyntaxSeparatorList *
        rightParenToken: SyntaxToken *
        matchClauseList: SyntaxMatchClause SyntaxList *
        fullWidth: int
    | While
        of
        whileToken: SyntaxToken *
        leftParenToken: SyntaxToken *
        conditionExpr: SyntaxExpression *
        rightParenToken: SyntaxToken *
        bodyExpr: SyntaxExpression *
        fullWidth: int
    | Typed
        of
        expr: SyntaxExpression *
        colonToken: SyntaxToken *
        ty: SyntaxType *
        fullWidth: int
    | ValueDeclaration
        of
        attrs: SyntaxAttributes *
        accessor: SyntaxAccessor *
        premodifierList: SyntaxValueDeclarationPremodifier SyntaxList *
        kind: SyntaxValueDeclarationKind *
        postmodifierList: SyntaxValueDeclarationPostmodifier SyntaxList *
        binding: SyntaxBinding *
        fullWidth: int
    | TypeDeclaration
        of
        attrs: SyntaxAttributes *
        accessor: SyntaxAccessor *
        kind: SyntaxTypeDeclarationKind *
        name: SyntaxTypeDeclarationName *
        typeParameters: SyntaxTypeParameters *
        constrClauseList: SyntaxConstraintClause SyntaxSeparatorList *
        equalsToken: SyntaxToken *
        body: SyntaxTypeDeclarationBody *
        fullWidth: int
    | MemberAccess
        of
        receiver: SyntaxExpression *
        dotToken: SyntaxToken *
        expr: SyntaxExpression *
        fullWidth: int
    | Mutate
        of
        lhsExpr: SyntaxExpression *
        leftArrowToken: SyntaxToken *
        rhsExpr: SyntaxExpression *
        fullWidth: int
    | Lambda
        of
        kind: SyntaxLambdaKind *
        pars: SyntaxParameters *
        rightArrowToken: SyntaxToken *
        body: SyntaxExpression *
        fullWidth: int
    | Error
        of
        token: SyntaxToken
    | None
        of unit

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = false

        member this.IsError = match this with | Error _ -> true | _ -> false

        member this.GetSlot(index) =
            match this with
            | OpenDeclaration(openToken, name, _) ->
                match index with
                | 0 -> openToken :> ISyntaxNode
                | 1 -> name :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | OpenStaticDeclaration(openToken, staticToken, name, _) ->
                match index with
                | 0 -> openToken :> ISyntaxNode
                | 1 -> staticToken :> ISyntaxNode
                | 2 -> name :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | OpenExtensionDeclaration(openToken, extensionToken, name, _) ->
                match index with
                | 0 -> openToken :> ISyntaxNode
                | 1 -> extensionToken :> ISyntaxNode
                | 2 -> name :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Sequential(expr1, expr2, _) ->
                match index with
                | 0 -> expr1 :> ISyntaxNode
                | 1 -> expr2 :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Parenthesis(leftParenToken, exprList, rightParenToken, _) ->
                match index with
                | 0 -> leftParenToken :> ISyntaxNode
                | 1 -> exprList :> ISyntaxNode
                | 2 -> rightParenToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Array(leftBracketToken, elements, rightBracketToken, _) ->
                match index with
                | 0 -> leftBracketToken :> ISyntaxNode
                | 1 -> elements :> ISyntaxNode
                | 2 -> rightBracketToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | MutableArray(leftBracketInnerPipeToken, elements, rightBracketInnerPipeToken, _) ->
                match index with
                | 0 -> leftBracketInnerPipeToken :> ISyntaxNode
                | 1 -> elements :> ISyntaxNode
                | 2 -> rightBracketInnerPipeToken :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Call(expression, arguments, _) ->
                match index with
                | 0 -> expression :> ISyntaxNode
                | 1 -> arguments :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | InfixCall(left, name, right, _) ->
                match index with
                | 0 -> left :> ISyntaxNode
                | 1 -> name :> ISyntaxNode
                | 2 -> right :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | PrefixCall(name, arg, _) ->
                match index with
                | 0 -> name :> ISyntaxNode
                | 1 -> arg :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Throw(throwToken, arg, _) ->
                match index with
                | 0 -> throwToken :> ISyntaxNode
                | 1 -> arg :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Indexer(lhsExpr, brackets, _) ->
                match index with
                | 0 -> lhsExpr :> ISyntaxNode
                | 1 -> brackets :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Name(name) ->
                match index with
                | 0 -> name :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Literal(lit) ->
                match index with
                | 0 -> lit :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | CreateRecord(constructTy) ->
                match index with
                | 0 -> constructTy :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | UpdateRecord(expr, withToken, constructTy, _) ->
                match index with
                | 0 -> expr :> ISyntaxNode
                | 1 -> withToken :> ISyntaxNode
                | 2 -> constructTy :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | If(ifToken, leftParenToken, predicateExpr, rightParenToken, target, elseIfOrElseExpr, _) ->
                match index with
                | 0 -> ifToken :> ISyntaxNode
                | 1 -> leftParenToken :> ISyntaxNode
                | 2 -> predicateExpr :> ISyntaxNode
                | 3 -> rightParenToken :> ISyntaxNode
                | 4 -> target :> ISyntaxNode
                | 5 -> elseIfOrElseExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Try(tryToken, bodyExpr, catchOrFinallyExpr, _) ->
                match index with
                | 0 -> tryToken :> ISyntaxNode
                | 1 -> bodyExpr :> ISyntaxNode
                | 2 -> catchOrFinallyExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Match(matchToken, leftParenToken, matchExprList, rightParenToken, matchClauseList, _) ->
                match index with
                | 0 -> matchToken :> ISyntaxNode
                | 1 -> leftParenToken :> ISyntaxNode
                | 2 -> matchExprList :> ISyntaxNode
                | 3 -> rightParenToken :> ISyntaxNode
                | 4 -> matchClauseList :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | While(whileToken, leftParenToken, conditionExpr, rightParenToken, bodyExpr, _) ->
                match index with
                | 0 -> whileToken :> ISyntaxNode
                | 1 -> leftParenToken :> ISyntaxNode
                | 2 -> conditionExpr :> ISyntaxNode
                | 3 -> rightParenToken :> ISyntaxNode
                | 4 -> bodyExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Typed(expr, colonToken, ty, _) ->
                match index with
                | 0 -> expr :> ISyntaxNode
                | 1 -> colonToken :> ISyntaxNode
                | 2 -> ty :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | ValueDeclaration(attrs, accessor, premodifierList, kind, postmodifierList, binding, _) ->
                match index with
                | 0 -> attrs :> ISyntaxNode
                | 1 -> accessor :> ISyntaxNode
                | 2 -> premodifierList :> ISyntaxNode
                | 3 -> kind :> ISyntaxNode
                | 4 -> postmodifierList :> ISyntaxNode
                | 5 -> binding :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | TypeDeclaration(attrs, accessor, kind, name, typeParameters, constrClauseList, equalsToken, body, _) ->
                match index with
                | 0 -> attrs :> ISyntaxNode
                | 1 -> accessor :> ISyntaxNode
                | 2 -> kind :> ISyntaxNode
                | 3 -> name :> ISyntaxNode
                | 4 -> typeParameters :> ISyntaxNode
                | 5 -> constrClauseList :> ISyntaxNode
                | 6 -> equalsToken :> ISyntaxNode
                | 7 -> body :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | MemberAccess(receiver, dotToken, expr, _) ->
                match index with
                | 0 -> receiver :> ISyntaxNode
                | 1 -> dotToken :> ISyntaxNode
                | 2 -> expr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Mutate(lhsExpr, leftArrowToken, rhsExpr, _) ->
                match index with
                | 0 -> lhsExpr :> ISyntaxNode
                | 1 -> leftArrowToken :> ISyntaxNode
                | 2 -> rhsExpr :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Lambda(kind, pars, rightArrowToken, body, _) ->
                match index with
                | 0 -> kind :> ISyntaxNode
                | 1 -> pars :> ISyntaxNode
                | 2 -> rightArrowToken :> ISyntaxNode
                | 3 -> body :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | Error(token) ->
                match index with
                | 0 -> token :> ISyntaxNode
                | _ -> failwith "invalid slot"
            | None _ ->
                failwith "invalid slot"

        member this.SlotCount =
            match this with
            | OpenDeclaration _ -> 2
            | OpenStaticDeclaration _ -> 3
            | OpenExtensionDeclaration _ -> 3
            | Sequential _ -> 2
            | Parenthesis _ -> 3
            | Array _ -> 3
            | MutableArray _ -> 3
            | Call _ -> 2
            | InfixCall _ -> 3
            | PrefixCall _ -> 2
            | Throw _ -> 2
            | Indexer _ -> 2
            | Name _ -> 1
            | Literal _ -> 1
            | CreateRecord _ -> 1
            | UpdateRecord _ -> 3
            | If _ -> 6
            | Try _ -> 3
            | Match _ -> 5
            | While _ -> 5
            | Typed _ -> 3
            | ValueDeclaration _ -> 6
            | TypeDeclaration _ -> 8
            | MemberAccess _ -> 3
            | Mutate _ -> 3
            | Lambda _ -> 4
            | Error _ -> 1
            | None _ -> 0

        member this.FullWidth =
            match this with
            | OpenDeclaration(fullWidth=fullWidth) ->
                fullWidth
            | OpenStaticDeclaration(fullWidth=fullWidth) ->
                fullWidth
            | OpenExtensionDeclaration(fullWidth=fullWidth) ->
                fullWidth
            | Sequential(fullWidth=fullWidth) ->
                fullWidth
            | Parenthesis(fullWidth=fullWidth) ->
                fullWidth
            | Array(fullWidth=fullWidth) ->
                fullWidth
            | MutableArray(fullWidth=fullWidth) ->
                fullWidth
            | Call(fullWidth=fullWidth) ->
                fullWidth
            | InfixCall(fullWidth=fullWidth) ->
                fullWidth
            | PrefixCall(fullWidth=fullWidth) ->
                fullWidth
            | Throw(fullWidth=fullWidth) ->
                fullWidth
            | Indexer(fullWidth=fullWidth) ->
                fullWidth
            | Name(x) ->
                (x :> ISyntaxNode).FullWidth
            | Literal(x) ->
                (x :> ISyntaxNode).FullWidth
            | CreateRecord(x) ->
                (x :> ISyntaxNode).FullWidth
            | UpdateRecord(fullWidth=fullWidth) ->
                fullWidth
            | If(fullWidth=fullWidth) ->
                fullWidth
            | Try(fullWidth=fullWidth) ->
                fullWidth
            | Match(fullWidth=fullWidth) ->
                fullWidth
            | While(fullWidth=fullWidth) ->
                fullWidth
            | Typed(fullWidth=fullWidth) ->
                fullWidth
            | ValueDeclaration(fullWidth=fullWidth) ->
                fullWidth
            | TypeDeclaration(fullWidth=fullWidth) ->
                fullWidth
            | MemberAccess(fullWidth=fullWidth) ->
                fullWidth
            | Mutate(fullWidth=fullWidth) ->
                fullWidth
            | Lambda(fullWidth=fullWidth) ->
                fullWidth
            | Error(x) ->
                (x :> ISyntaxNode).FullWidth
            | None _ ->
                0

        member _.Tag = 45

[<RequireQualifiedAccess>]
module SyntaxExpression =

    [<Literal>]
    let Tag = 45

