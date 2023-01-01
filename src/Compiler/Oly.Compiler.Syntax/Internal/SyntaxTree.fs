namespace Oly.Compiler.Syntax.Internal

open System.Runtime.CompilerServices
open Oly.Core
open Oly.Compiler.Text

[<RequireQualifiedAccess>]
type internal DiagnosticSeverity =
    | Warning
    | Error

[<Sealed;System.Diagnostics.DebuggerDisplay("{Message}")>]
type internal DiagnosticSyntax(message: string, code: int, severity: DiagnosticSeverity, node: ISyntaxNode, offsidesAmount: int, canShrinkErrorRangeToEnd: bool) =

    do
        if node.IsTerminal then
            invalidArg (nameof(node)) "Internal syntax node cannot be terminal."

    member _.Message = message

    member _.Code = code

    member _.Severity = severity
    
    member _.Node = node

    // TODO: We should remove offsides amount. 
    //       Instead of passing a code, pass in a type of error, and get the code at the very end. That way we can get the offsides amount from the error itself.
    member _.OffsidesAmount = offsidesAmount

    member _.CanShrinkErrorRangeToEnd = canShrinkErrorRangeToEnd

[<Sealed>] 
type internal SyntaxTree internal (
                                    path: OlyPath, 
                                    root: SyntaxCompilationUnit, 
                                    diagnostics: ConditionalWeakTable<ISyntaxNode, ResizeArray<DiagnosticSyntax>>) =

    member _.Root = root

    member this.GetDiagnostics() =
        diagnostics
        |> Seq.map (fun pair -> pair.Value)
        |> Seq.concat
        |> ImArray.ofSeq

    member this.GetDiagnostics(node: ISyntaxNode) =
        match diagnostics.TryGetValue node with
        | true, diagnostics -> diagnostics |> ImArray.ofSeq
        | _ -> ImArray.empty

    member this.HasErrors = 
        not (Seq.isEmpty (this.GetDiagnostics()))

    member _.Path = path

    member internal _.RawDiagnostics = diagnostics

    member this.WithPath(path: OlyPath) =
        SyntaxTree(path, root, diagnostics)