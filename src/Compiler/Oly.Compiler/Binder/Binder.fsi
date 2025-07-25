﻿[<AutoOpen>]
module internal Oly.Compiler.Internal.Binder.Binder

open System.Threading

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.CompilerImports
open Oly.Compiler.Internal.Symbols
open Oly.Metadata

// All the binder passes, or phases.
// Pass0, Pass1, Pass2, Pass3 will modify symbols; they are side-effectful.
// Pass4 will not modify symbols.
//
// Do not cache results from Pass0, Pass1 or Pass2 as they are not safe.
// Pass3 results are the signature and can be cached. Pass4 results can also be cached.


/// Pass4 - Bind, check, and solve implementations.
[<Sealed>]
type BinderPass4 =

    member Entity : EntityDefinitionSymbol

    member PartialDeclarationTable : BoundDeclarationTable

    member Bind : CancellationToken -> BoundTree

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

    member Entity : EntityDefinitionSymbol

    member Bind : CompilerImports * CancellationToken -> BinderPass2

/// Pass0 - Gather type definitions. 
[<Sealed>]
type BinderPass0 =

    member Bind : CancellationToken -> BinderPass1

/// PrePass - Imports references.
///           Open declarations are partially processed but will scope in everything(entities/values) from those imported references.
///           This is all cached. The eviction policy is when one of the references changes in the compilation.
[<Sealed>]
type BinderPrePass =

    member PrePassEnvironment: CacheValue<BinderEnvironment * BoundDeclarationTable * OlyDiagnostic imarray>

    member Bind : CancellationToken -> BinderPass0

val CreateDefaultBinderEnvironment: OlyILAssemblyIdentity -> BinderEnvironment

val computePrologEnvironment: CompilerImports -> OlyDiagnosticLogger -> BinderEnvironment -> BoundDeclarationTable -> OpenContent -> CancellationToken -> BinderEnvironment

val bindSyntaxTree: AssemblySymbol -> BinderEnvironment -> syntaxTree: OlySyntaxTree -> BinderPrePass

val bindSyntaxTreeFast: AssemblySymbol -> syntaxTree: OlySyntaxTree -> CacheValue<BinderEnvironment * BoundDeclarationTable * OlyDiagnostic imarray> -> BinderPrePass