module internal Oly.Compiler.Syntax.Internal.SyntaxErrors

open Oly.Compiler.Syntax.Internal
open Oly.Core

type SyntaxError =
    | ExpectedToken of Token
    | ExpectedSyntax of syntaxDescription: string
    | ExpectedSyntaxAfterToken of syntaxDescription: string * Token
    | ExpectedSyntaxAfterSyntax of syntaxDescription1: string * syntaxDescription2: string
    | ExpectedTokenAfterSyntax of Token * syntaxDescription: string
    | ExpectedTokenAfterToken of Token * Token
    | Offsides of amount: int
    | UnexpectedToken of Token
    | InvalidSyntax of name: string

    member this.CanShrinkErrorRangeToEnd =
        match this with
        | ExpectedToken _ -> true
        | _ -> false

    member this.Text =
        match this with
        | ExpectedToken tok -> sprintf "Expected '%s'." tok.Text
        | ExpectedSyntax desc -> sprintf "Expected '%s'." desc
        | ExpectedSyntaxAfterToken(desc, tok) -> sprintf "Expected '%s' after '%s'." desc tok.Text
        | ExpectedSyntaxAfterSyntax(desc1, desc2) -> sprintf "Expected '%s' after '%s'." desc1 desc2
        | ExpectedTokenAfterSyntax(tok, desc) -> sprintf "Expected '%s' after '%s'." tok.Text desc
        | ExpectedTokenAfterToken(tok1, tok2) -> sprintf "Expected '%s' after '%s'." tok1.Text tok2.Text
        | Offsides amount -> 
            if amount >= 0 then
                sprintf "Offsides by %i space(s)." amount
            else
                sprintf "Offsides by %i space(s) from the right." (abs amount)
        | UnexpectedToken tok -> sprintf "Unexpected '%s'." tok.Text
        | InvalidSyntax(text) -> text

    member this.Code =
        match this with
        | ExpectedToken _ ->                100
        | ExpectedSyntax _ ->               101
        | ExpectedSyntaxAfterToken _ ->     102
        | ExpectedSyntaxAfterSyntax _ ->    103
        | ExpectedTokenAfterSyntax _ ->     104
        | ExpectedTokenAfterToken _ ->      105
        | Offsides _ ->                     106
        | UnexpectedToken _ ->              107
        | InvalidSyntax _ ->                108

    member this.OffsidesAmount =
        match this with
        | Offsides(width) -> width
        | _ -> 0
        