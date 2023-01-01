[<AutoOpen>]
module Oly.Compiler.Extensions.SyntaxTreeExtensions

open System.Threading
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Text

let private filterPossibleTokens (possibleTokens: OlyToken seq) =
    // Identifiers, operators, and keywords take precedence; with the exception of '<' and '>' when used in type parameters/arguments.
    possibleTokens
    |> Seq.sortByDescending (fun x ->
        x.IsIdentifierOrOperatorOrKeyword && not x.HasParentTypeParametersOrTypeArguments
    )
    |> Seq.sortByDescending (fun x ->
        x.IsLiteral
    )
    |> Seq.sortByDescending (fun x ->
        not x.IsTrivia
    )
    |> Seq.tryHead

type OlySyntaxNode with

    member this.TryFindToken(position: int, ?skipTrivia: bool, ?ct: CancellationToken) =
        let skipTrivia = defaultArg skipTrivia true
        let ct = defaultArg ct CancellationToken.None
        this.FindTokens(position, skipTrivia, ct)
        |> filterPossibleTokens

    member this.TryFindToken(textPosition: OlyTextPosition, ?skipTrivia: bool, ?ct: CancellationToken) =
        let skipTrivia = defaultArg skipTrivia true
        let ct = defaultArg ct CancellationToken.None
        this.FindTokens(textPosition, skipTrivia, ct)
        |> filterPossibleTokens
