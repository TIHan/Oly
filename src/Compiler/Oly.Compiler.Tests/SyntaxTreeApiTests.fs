module SyntaxTreeApiTests

open System.Threading
open Xunit
open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax

[<Fact>]
let ``Basic prepend works``() =
    let tree: OlySyntaxTree = OlySyntaxTree.Parse(OlyPath.Create("test.oly"), "type =")
    let rootNode = tree.GetRoot(CancellationToken.None)
    let tokens = rootNode.GetDescendantTokens() |> Array.ofSeq
    let equalsToken = tokens.[1]
    Assert.True(equalsToken.IsEqual)

    let newEqualsToken = equalsToken.Prepend("Test ", CancellationToken.None)
    Assert.True(newEqualsToken.IsEqual)

    let newSrc = newEqualsToken.Tree.GetRoot(CancellationToken.None).BuildSource(CancellationToken.None)
    Assert.Equal("type Test =", newSrc)

[<Fact>]
let ``Basic reference directive works``() =
    let tree: OlySyntaxTree = OlySyntaxTree.Parse(OlyPath.Create("test.oly"), """#load "test2.oly" """)
    let config = tree.GetCompilationUnitConfiguration(CancellationToken.None)
    Assert.Equal(1, config.Loads.Length)
    Assert.Equal(None, config.Target)

[<Fact>]
let ``Basic reference directive works 2``() =
    let sourceText =
        OlySourceText.Create("""
/*
test
*/

#reference "test2.oly" """)
    let tree: OlySyntaxTree = OlySyntaxTree.Parse(OlyPath.Create("test.oly"), sourceText)
    let config = tree.GetCompilationUnitConfiguration(CancellationToken.None)
    Assert.Equal(1, config.References.Length)
    Assert.Equal(None, config.Target)

