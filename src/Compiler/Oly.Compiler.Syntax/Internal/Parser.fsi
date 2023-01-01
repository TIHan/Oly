module internal Oly.Compiler.Syntax.Internal.Parser

open System.Threading
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax.Internal
open Oly.Compiler.Syntax.Internal.Lexer

val Parse : path: OlyPath * lexer: Lexer * ct: CancellationToken -> SyntaxTree
