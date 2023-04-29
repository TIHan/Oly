module internal Oly.Compiler.Internal.SemanticDiagnostics

open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.PrettyPrint

// TODO: Expand this to all semantic diagnostics.
[<NoEquality;NoComparison>]
type SemanticDiagnostic =
    | Error_MissingConstraint of benv: BoundEnvironment * syntaxNode: OlySyntaxNode * tyArg: TypeSymbol * constr: ConstraintSymbol

    // TODO: We should figure out what permanent code numbers to use.
    member this.Code =
        match this with
        | Error_MissingConstraint _ -> 500

type OlyDiagnosticLogger with

    member this.Report(diag: SemanticDiagnostic) =
        let code = diag.Code
        match diag with
        | Error_MissingConstraint(benv, syntaxNode, tyArg, constr) ->
            this.Error($"Type instantiation '{printType benv tyArg}' is missing the constraint '{printConstraint benv constr}'.", code, syntaxNode)