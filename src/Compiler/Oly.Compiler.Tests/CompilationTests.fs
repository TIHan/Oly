module CompilationTests

open Xunit
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Analysis
open Oly.Compiler.Analysis.Patterns
open Oly.Compiler.Symbols
open Oly.Compiler.Compilation
open System.Threading

[<Fact>]
let ``Compilation should remain immutable - not affect previous compilations`` () =
    let src1 =
        """
module Test1
        """
    let tree1 = OlySyntaxTree.Parse(OlyPath.Create("test"), src1) 
    let c1 = OlyCompilation.Create("testasm", [tree1])
    let symbols1 = c1.GetBoundModel(OlyPath.Create("test")).GetSymbols(tree1.GetRoot(CancellationToken.None), CancellationToken.None)
    Assert.Equal(1, symbols1.Length)
    let test1ModuleSymbol1 = symbols1[0]
    Assert.Equal("Test1", test1ModuleSymbol1.Name)
    Assert.Equal(0, (test1ModuleSymbol1 :?> OlyTypeSymbol).Functions.Length)

    let src2 =
        """
module Test1

test(): () = ()
        """
    let tree2 = tree1.ApplySourceText(OlySourceText.Create(src2))
    let c2 = c1.SetSyntaxTree(tree2)
    let symbols2 = c2.GetBoundModel(OlyPath.Create("test")).GetSymbols(tree2.GetRoot(CancellationToken.None), CancellationToken.None)
    Assert.Equal(2, symbols2.Length)
    let test1ModuleSymbol2 = symbols2[0]
    Assert.Equal("Test1", test1ModuleSymbol2.Name)
    Assert.Equal(1, (test1ModuleSymbol2 :?> OlyTypeSymbol).Functions.Length)

    // Old symbol remains to have zero functions.
    Assert.Equal(0, (test1ModuleSymbol1 :?> OlyTypeSymbol).Functions.Length)

    let src3 =
        """
module Test1

test(): () = ()
test2(): () = ()
        """
    let tree3 = tree2.ApplySourceText(OlySourceText.Create(src3))
    let c3 = c2.SetSyntaxTree(tree3)
    let symbols3 = c3.GetBoundModel(OlyPath.Create("test")).GetSymbols(tree3.GetRoot(CancellationToken.None), CancellationToken.None)
    Assert.Equal(3, symbols3.Length)
    let test1ModuleSymbol3 = symbols3[0]
    Assert.Equal("Test1", test1ModuleSymbol3.Name)
    Assert.Equal(2, (test1ModuleSymbol3 :?> OlyTypeSymbol).Functions.Length)

    // Old symbols remains to have zero functions.
    Assert.Equal(0, (test1ModuleSymbol1 :?> OlyTypeSymbol).Functions.Length)
    Assert.Equal(1, (test1ModuleSymbol2 :?> OlyTypeSymbol).Functions.Length)

    let src4 =
        """
module Test1
        """
    let tree4 = tree3.ApplySourceText(OlySourceText.Create(src4))
    let c4 = c3.SetSyntaxTree(tree4)
    let symbols4 = c4.GetBoundModel(OlyPath.Create("test")).GetSymbols(tree4.GetRoot(CancellationToken.None), CancellationToken.None)
    Assert.Equal(1, symbols4.Length)
    let test1ModuleSymbol4 = symbols4[0]
    Assert.Equal("Test1", test1ModuleSymbol4.Name)
    Assert.Equal(0, (test1ModuleSymbol4 :?> OlyTypeSymbol).Functions.Length)

    // Old symbols remains to have zero functions.
    Assert.Equal(0, (test1ModuleSymbol1 :?> OlyTypeSymbol).Functions.Length)
    Assert.Equal(1, (test1ModuleSymbol2 :?> OlyTypeSymbol).Functions.Length)
    Assert.Equal(2, (test1ModuleSymbol3 :?> OlyTypeSymbol).Functions.Length)

    Assert.True(test1ModuleSymbol1.IsSimilarTo(test1ModuleSymbol1))
    Assert.False(test1ModuleSymbol1.IsSimilarTo(test1ModuleSymbol2))
    Assert.False(test1ModuleSymbol2.IsSimilarTo(test1ModuleSymbol3))
    Assert.False(test1ModuleSymbol3.IsSimilarTo(test1ModuleSymbol1))

[<Fact>]
let ``Basic analyzer`` () =
    let src1 =
        """
module Test1

test(): () = ()
test2(): () = test()
        """
    let tree = OlySyntaxTree.Parse(OlyPath.Create("test"), src1) 
    let c = OlyCompilation.Create("testasm", [tree])
    let boundModel = c.GetBoundModel(OlyPath.Create("test"))

    let diags = boundModel.GetDiagnostics(CancellationToken.None)
    Assert.Equal(0, diags.Length)

    let analyzer = 
        fun (diags: OlyDiagnosticLogger) (func: OlyValueSymbol) (expr: OlyAnalysisExpression) (_ct: CancellationToken) ->
            if func.Name = "test2" then
                match expr with
                | Lambda(tyPars, pars, bodyExpr) ->
                    Assert.Equal(0, tyPars.Length)
                    Assert.Equal(0, pars.Length)
                    match bodyExpr with
                    | Call(OlyCallKind.Concrete, callFunc, callArgExprs) ->
                        Assert.Equal(0, callArgExprs.Length)
                        if callFunc.Name = "test" && callFunc.Enclosing.TryType.Value.Name = "Test1" then
                            diags.Error("You used 'test', how dare you!", 999, callFunc.UseSyntax)
                    | _ ->
                        failwith "Expected Call"
                | _ ->
                    failwith "Expected Lambda"

    let diags = boundModel.AnalyzeFunctionImplementations(ImArray.createOne analyzer, CancellationToken.None)
    Assert.Equal(1, diags.Length)
    Assert.Equal("You used 'test', how dare you!", diags[0].Message)

[<Fact>]
let ``Basic analyzer 2`` () =
    let src1 =
        """
module Test1

test(): () = ()
test2(x: __oly_int32): () = test()
        """
    let tree = OlySyntaxTree.Parse(OlyPath.Create("test"), src1) 
    let c = OlyCompilation.Create("testasm", [tree])
    let boundModel = c.GetBoundModel(OlyPath.Create("test"))

    let diags = boundModel.GetDiagnostics(CancellationToken.None)
    Assert.Equal(0, diags.Length)

    let analyzer = 
        fun (diags: OlyDiagnosticLogger) (func: OlyValueSymbol) (expr: OlyAnalysisExpression) (_ct: CancellationToken) ->
            if func.Name = "test2" then
                match expr with
                | Lambda(tyPars, pars, _) ->
                    Assert.Equal(0, tyPars.Length)
                    Assert.Equal(1, pars.Length)
                    diags.Error("Test error.", 10, pars[0].UseSyntax)
                | _ ->
                    failwith "Expected Lambda"

    let diags = boundModel.AnalyzeFunctionImplementations(ImArray.createOne analyzer, CancellationToken.None)
    Assert.Equal(1, diags.Length)
    Assert.Equal("test2(x: __oly_int32): () = test()
      ^", diags[0].GetHelperText())

[<Fact>]
let ``Basic analyzer 3`` () =
    let src1 =
        """
module Test1

test(): () =
    let y = 1
        """
    let tree = OlySyntaxTree.Parse(OlyPath.Create("test"), src1) 
    let c = OlyCompilation.Create("testasm", [tree])
    let boundModel = c.GetBoundModel(OlyPath.Create("test"))

    let diags = boundModel.GetDiagnostics(CancellationToken.None)
    Assert.Equal(0, diags.Length)

    let analyzer = 
        fun (diags: OlyDiagnosticLogger) (func: OlyValueSymbol) (expr: OlyAnalysisExpression) (_ct: CancellationToken) ->
            if func.Name = "test" then
                match expr with
                | Lambda(tyPars, pars, bodyExpr) ->
                    Assert.Equal(0, tyPars.Length)
                    Assert.Equal(0, pars.Length)
                    match bodyExpr with
                    | Let(value, rhsExpr, bodyExpr) ->
                        Assert.Equal("y", value.Name)
                        diags.Error("Test error.", 10, value.UseSyntax)
                    | _ ->
                        failwith "Expected Let"
                | _ ->
                    failwith "Expected Lambda"

    let diags = boundModel.AnalyzeFunctionImplementations(ImArray.createOne analyzer, CancellationToken.None)
    Assert.Equal(1, diags.Length)
    Assert.Equal("    let y = 1
        ^", diags[0].GetHelperText())

[<Fact(Skip = "figure this out later")>]
let ``Basic analyzer using pattern matching`` () =
    let src1 =
        """
module Test1

test(): () =
    match (7)
    | 2 => ()
    | 5 => ()
    | _ => ()
        """
    let tree = OlySyntaxTree.Parse(OlyPath.Create("test"), src1) 
    let c = OlyCompilation.Create("testasm", [tree])
    let boundModel = c.GetBoundModel(OlyPath.Create("test"))

    let diags = boundModel.GetDiagnostics(CancellationToken.None)
    Assert.Equal(0, diags.Length)

    let analyzer = 
        fun (diags: OlyDiagnosticLogger) (func: OlyValueSymbol) (expr: OlyAnalysisExpression) (_ct: CancellationToken) ->
            match expr with
            | Lambda(_, _, bodyExpr) ->
                match bodyExpr with
                | IfElse(Equals(ConstantInt32(7), ConstantInt32(2)), Unit, falseTargetExpr) ->
                    match falseTargetExpr with
                    | IfElse(Equals(ConstantInt32(7) as cns1, ConstantInt32(5)), Unit, Unit) ->
                        diags.Error("You used 7 twice.", 999, cns1.SyntaxNode)
                    | _ ->
                        failwith "Expected specific IfElse"
                | _ ->
                    failwith "Expected specific IfElse"
            | _ ->
                failwith "Expected Lambda"

    let diags = boundModel.AnalyzeFunctionImplementations(ImArray.createOne analyzer, CancellationToken.None)
    Assert.Equal(1, diags.Length)
    Assert.Equal("You used 7 twice.", diags[0].Message)

[<Fact>]
let ``Basic rules analyzer`` () =
    let src1 =
        """
module Module

#[intrinsic("int32")]
alias int32

#[intrinsic("int16")]
alias int16

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>

class OverloadTest =

    M(x: byref<int32>): () = ()
    M(x: inref<int32>): () = ()

        """
    let tree = OlySyntaxTree.Parse(OlyPath.Create("test"), src1) 
    let c = OlyCompilation.Create("testasm", [tree])
    let boundModel = c.GetBoundModel(OlyPath.Create("test"))

    let diags = boundModel.GetDiagnostics(CancellationToken.None)
    Assert.Equal(0, diags.Length)

    let rules = 
        OverloadedFunctionDefinitionsCannotDisambiguateType(
            ByRef(
                WildcardByRefKind, 
                DefaultType
            )
        )
        |> ImArray.createOne

    let diags = boundModel.CheckRules(rules, CancellationToken.None)
    Assert.Equal(2, diags.Length)
    //Assert.Equal("    let y = 1
    //    ^", diags[0].GetHelperText())