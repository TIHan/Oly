module CompilationTests

open Xunit
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Symbols
open Oly.Compiler.Compilation
open System.Threading

[<Fact>]
let ``Compilation should remain immutable - not affect previous compilations`` () =
    let src1 =
        """
public module Test1
        """
    let tree1 = OlySyntaxTree.Parse(OlyPath.Create("test"), src1) 
    let c1 = OlyCompilation.Create("testasm", [tree1])
    let symbols1 = 
        c1.GetBoundModel(OlyPath.Create("test")).GetSymbols(tree1.GetRoot(CancellationToken.None), CancellationToken.None)
        |> ImArray.map (fun x -> x.Symbol)
    Assert.Equal(1, symbols1.Length)
    let test1ModuleSymbol1 = symbols1[0]
    Assert.Equal("Test1", test1ModuleSymbol1.Name)
    Assert.Equal(0, (test1ModuleSymbol1 :?> OlyTypeSymbol).Functions.Length)

    let src2 =
        """
public module Test1

public test(): () = ()
        """
    let tree2 = tree1.ApplySourceText(OlySourceText.Create(src2))
    let c2 = c1.SetSyntaxTree(tree2)
    let symbols2 = 
        c2.GetBoundModel(OlyPath.Create("test")).GetSymbols(tree2.GetRoot(CancellationToken.None), CancellationToken.None)
        |> ImArray.map (fun x -> x.Symbol)
    Assert.Equal(2, symbols2.Length)
    let test1ModuleSymbol2 = symbols2[0]
    Assert.Equal("Test1", test1ModuleSymbol2.Name)
    Assert.Equal(1, (test1ModuleSymbol2 :?> OlyTypeSymbol).Functions.Length)

    // Old symbol remains to have zero functions.
    Assert.Equal(0, (test1ModuleSymbol1 :?> OlyTypeSymbol).Functions.Length)

    let src3 =
        """
public module Test1

public test(): () = ()
public test2(): () = ()
        """
    let tree3 = tree2.ApplySourceText(OlySourceText.Create(src3))
    let c3 = c2.SetSyntaxTree(tree3)
    let symbols3 = 
        c3.GetBoundModel(OlyPath.Create("test")).GetSymbols(tree3.GetRoot(CancellationToken.None), CancellationToken.None)
        |> ImArray.map (fun x -> x.Symbol)
    Assert.Equal(3, symbols3.Length)
    let test1ModuleSymbol3 = symbols3[0]
    Assert.Equal("Test1", test1ModuleSymbol3.Name)
    Assert.Equal(2, (test1ModuleSymbol3 :?> OlyTypeSymbol).Functions.Length)

    // Old symbols remains to have zero functions.
    Assert.Equal(0, (test1ModuleSymbol1 :?> OlyTypeSymbol).Functions.Length)
    Assert.Equal(1, (test1ModuleSymbol2 :?> OlyTypeSymbol).Functions.Length)

    let src4 =
        """
public module Test1
        """
    let tree4 = tree3.ApplySourceText(OlySourceText.Create(src4))
    let c4 = c3.SetSyntaxTree(tree4)
    let symbols4 = 
        c4.GetBoundModel(OlyPath.Create("test")).GetSymbols(tree4.GetRoot(CancellationToken.None), CancellationToken.None)
        |> ImArray.map (fun x -> x.Symbol)
    Assert.Equal(1, symbols4.Length)
    let test1ModuleSymbol4 = symbols4[0]
    Assert.Equal("Test1", test1ModuleSymbol4.Name)
    Assert.Equal(0, (test1ModuleSymbol4 :?> OlyTypeSymbol).Functions.Length)

    // Old symbols remains to have zero functions.
    Assert.Equal(0, (test1ModuleSymbol1 :?> OlyTypeSymbol).Functions.Length)
    Assert.Equal(1, (test1ModuleSymbol2 :?> OlyTypeSymbol).Functions.Length)
    Assert.Equal(2, (test1ModuleSymbol3 :?> OlyTypeSymbol).Functions.Length)

    Assert.True(test1ModuleSymbol1.IsSimilarTo(test1ModuleSymbol1))
    Assert.True(test1ModuleSymbol1.IsSimilarTo(test1ModuleSymbol2))
    Assert.True(test1ModuleSymbol2.IsSimilarTo(test1ModuleSymbol3))
    Assert.True(test1ModuleSymbol3.IsSimilarTo(test1ModuleSymbol1))
