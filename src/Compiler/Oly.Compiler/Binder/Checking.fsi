[<AutoOpen>]
module internal Oly.Compiler.Internal.Binder.Checking

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.WellKnownExpressions
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.FunctionOverloading
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.SymbolQuery
open Oly.Compiler.Internal.SymbolQuery.Extensions
open Oly.Compiler.Internal

val checkSyntaxBindingDeclaration: cenv -> ValueExplicitness -> syntaxBindingDecl: OlySyntaxBindingDeclaration -> unit

val checkSyntaxDeclarationBinding: cenv -> EnclosingSymbol -> MemberFlags -> ValueExplicitness -> syntaxBinding: OlySyntaxBinding -> unit

val checkBindingSignature: cenv -> attrs: AttributeSymbol imarray -> EnclosingSymbol -> BindingInfoSymbol -> MemberFlags -> ValueExplicitness -> mustHaveImpl: bool -> syntaxBindingDecl: OlySyntaxBindingDeclaration -> bool

val checkEnumForInvalidFieldOrFunction: cenv -> OlySyntaxNode -> BindingInfoSymbol -> unit

val checkEntityExport: cenv -> OlySyntaxNode -> EntitySymbol -> unit

val checkValueExport: cenv -> OlySyntaxNode -> IValueSymbol -> unit

val checkExpression: cenv -> BinderEnvironment -> TypeSymbol option -> E -> E