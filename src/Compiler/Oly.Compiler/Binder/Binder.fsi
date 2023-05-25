[<AutoOpen>]
module internal Oly.Compiler.Internal.Binder.Binder

open System.Threading

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.CompilerImports
open Oly.Compiler.Internal.Symbols
open Oly.Metadata

/// Pass4 - Bind, check, and solve implementations.
[<Sealed>]
type BinderPass4 =

    member Entity : EntitySymbol

    member PartialDeclarationTable : BoundDeclarationTable

    member Bind : CancellationToken -> BoundTree * OlyDiagnostic imarray

    member SyntaxTree : OlySyntaxTree

/// Pass3 - Resolve late bound attributes, set function overrides, and check implicit default instance constructors.
///         Open declarations scope in values - all open declarations are fully processed.
///         Check constraints.
///         Check type constructors.
[<Sealed>]
type BinderPass3 =

    member Bind : CancellationToken -> BinderPass4 * OlyDiagnostic imarray

/// Pass2 - Gather function, field and property definitions.
[<Sealed>]
type BinderPass2 =

    member Bind : CancellationToken -> BinderPass3

/// Pass1 - Gather inherits/extends and implements for type definitions. 
///         Open declarations scope in entities.
[<Sealed>]
type BinderPass1 =

    member Entity : EntitySymbol

    member Bind : CompilerImports * CancellationToken -> BinderPass2

/// Pass0 - Gather type definitions. 
///         Open declarations are partially processed but will scope in everything(entities/values) - results are stored in the pre-pass environment.
[<Sealed>]
type BinderPass0 =

    member PrePassEnvironment: CacheValue<BinderEnvironment * BoundDeclarationTable * OlyDiagnostic imarray>

    member Bind : CancellationToken -> BinderPass1

val CreateDefaultBinderEnvironment: OlyILAssemblyIdentity -> BinderEnvironment

val computePrologEnvironment: CompilerImports -> OlyDiagnosticLogger -> BinderEnvironment -> BoundDeclarationTable -> OpenContent -> CancellationToken -> BinderEnvironment

val bindSyntaxTree: AssemblySymbol -> BinderEnvironment -> syntaxTree: OlySyntaxTree -> BinderPass0

val bindSyntaxTreeFast: AssemblySymbol -> CacheValue<BinderEnvironment * BoundDeclarationTable * OlyDiagnostic imarray> -> syntaxTree: OlySyntaxTree -> BinderPass0