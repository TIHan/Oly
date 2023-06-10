module TestUtilities

open System
open System.Threading
open Xunit
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Extensions

[<NoComparison;NoEquality;RequireQualifiedAccess>]
type TestCompilation =
    private {
        c: OlyCompilation
        debugc: OlyCompilation
    }

    member this.Compilation = this.c

    static member Create(c: OlyCompilation) =
        let debugc = c.Update(options = { c.Options with Debuggable = true })
        {
            c = c
            debugc = debugc
        }     

    static member Create(src: string) =
        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true }
        let c = OlyCompilation.Create("olytest", [OlySyntaxTree.Parse(OlyPath.Create("olytest"), src, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true; CompilationUnitConfigurationEnabled = true })], [], options = options)
        TestCompilation.Create(c)

    static member CreateWithConditionalDefines(src: string, conditionalDefines) =
        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true }
        let c = OlyCompilation.Create("olytest", [OlySyntaxTree.Parse(OlyPath.Create("olytest"), src, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true; CompilationUnitConfigurationEnabled = true; ConditionalDefines = conditionalDefines })], [], options = options)
        TestCompilation.Create(c)

    static member CreateWithReference(src: string, refSrc: string) =
        let options = { OlyCompilationOptions.Default with Parallel = false }
        let refc = OlyCompilation.Create("olytestref", [OlySyntaxTree.Parse(OlyPath.Create("olytestref"), refSrc, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = false })], [], options = options)
        let refcRef = OlyCompilationReference.Create(OlyPath.Create "olytestref", (fun () -> refc))

        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true }
        let c = OlyCompilation.Create("olytest", [OlySyntaxTree.Parse(OlyPath.Create("olytest"), src, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true; CompilationUnitConfigurationEnabled = true })], [refcRef], options = options)
        TestCompilation.Create(c)

    static member CreateTwo(src1: string, src2: string) =
        let syntaxTrees =
            [
                OlySyntaxTree.Parse(
                    OlyPath.Create("olytest1"), 
                    src1, 
                    parsingOptions = 
                        { 
                            OlyParsingOptions.Default with 
                                AnonymousModuleDefinitionAllowed = true
                                CompilationUnitConfigurationEnabled = true 
                        }
                )
                OlySyntaxTree.Parse(
                    OlyPath.Create("olytest2"), 
                    src2, 
                    parsingOptions = 
                        { 
                            OlyParsingOptions.Default with 
                                AnonymousModuleDefinitionAllowed = false
                                CompilationUnitConfigurationEnabled = false 
                        }
                )
            ]
        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true }
        let c = OlyCompilation.Create("olytest", syntaxTrees, [], options = options)
        TestCompilation.Create(c)

let getFirstSyntaxTree (c: TestCompilation) =
    c.c.SyntaxTrees.[0]

/// Stress tests updating the source text character by character to ensure the compiler does not crash
/// in various syntax tree states.
/// Does not cover any permutations as it would lead to excessively large test execution times.
let private stressTyping (c: TestCompilation) =
    let tree = c.Compilation.SyntaxTrees[0]
    let text = tree.GetSourceText(CancellationToken.None)
    let str = text.ToString()

    let mutable comp = c.Compilation
    let length = str.Length
    for i = 0 to length - 1 do
        comp <- comp.SetSyntaxTree(tree.ApplySourceText(OlySourceText.Create(str.Substring(0, i))))
        let boundModel = comp.GetBoundModel(tree.Path)
        boundModel.GetSymbols(comp.SyntaxTrees[0].GetRoot(CancellationToken.None), CancellationToken.None)
        |> ignore

let private stressTest origSrc (c: TestCompilation) =
    let syntaxTree = getFirstSyntaxTree c
    Assert.Equal(origSrc, syntaxTree.GetRoot(CancellationToken.None).BuildSource(CancellationToken.None))

#if STRESS
    stressTyping c

    // We do this to ensure that syntax nodes with diagnostics do not get collected.
    // Syntax nodes associated with diagnostics are kept in a ConditionalWeakTable internally.
    GC.Collect(2, GCCollectionMode.Forced, true)
    GC.WaitForPendingFinalizers()
#endif

    c

let withNoSyntaxDiagnostics (c: TestCompilation) =
    let diags = c.c.SyntaxTrees.[0].GetDiagnostics(CancellationToken.None)
    if diags.IsEmpty then
        c
    else
        failwithf "%A" diags

let withSyntaxErrorDiagnostics (expected: string list) (c: TestCompilation) =
    let errorMsgs = c.c.SyntaxTrees.[0].GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> x.Message)
    Assert.Equal(expected, errorMsgs)
    c

let withSyntaxErrorHelperTextDiagnostics (expected: (string * string) list) (c: TestCompilation) =
    let errorMsgs = c.c.SyntaxTrees[0].GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> (x.Message, "\r\n" + x.GetHelperText() + "\r\n")) |> Array.ofSeq
    (expected, errorMsgs)
    ||> Seq.iter2 (fun (expectedMsg, expectedText) (msg, text) ->
        Assert.Equal(expectedMsg, msg)
        Assert.Equal(expectedText.Replace("\r", ""), text.Replace("\r", ""))
    )
    Assert.Equal(expected.Length, errorMsgs.Length)
    c

let withNoDiagnostics (c: TestCompilation) =
    Assert.Empty(c.c.GetDiagnostics(CancellationToken.None))
    c

let withErrorDiagnostics (expected: string list) (c: TestCompilation) =
    let errorMsgs = c.c.GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> x.Message) |> Array.ofSeq
    (expected, errorMsgs)
    ||> Seq.iter2 (fun expectedMsg msg ->
        Assert.Equal(expectedMsg, msg)
    )
    Assert.Equal(expected.Length, errorMsgs.Length)
    c

let hasErrorDiagnostics (c: TestCompilation) =
    let errorMsgs = c.c.GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> x.Message) |> Array.ofSeq
    Assert.NotEmpty(errorMsgs)

let withErrorHelperTextDiagnostics (expected: (string * string) list) (c: TestCompilation) =
    let errorMsgs = c.c.GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> (x.Message, "\r\n" + x.GetHelperText() + "\r\n")) |> Array.ofSeq
    (expected, errorMsgs)
    ||> Seq.iter2 (fun (expectedMsg, expectedText) (msg, text) ->
        Assert.Equal(expectedMsg, msg)
        Assert.Equal(expectedText, text)
    )
    Assert.Equal(expected.Length, errorMsgs.Length)
    c

[<NoEquality;NoComparison>]
type TestCompilationOutput =
    {
        c: TestCompilation
        ilAsm: Oly.Metadata.OlyILAssembly
        ilAsmDebug: Oly.Metadata.OlyILAssembly
    }

let shouldCompile (c: TestCompilation) =
    let c = 
        c
        |> withNoSyntaxDiagnostics
    let ilAsm =
        match c.c.GetILAssembly(CancellationToken.None) with
        | Ok ilAsm -> ilAsm
        | Error diags -> raise(Exception(OlyDiagnostic.PrepareForOutput(diags, CancellationToken.None)))
    let ilAsmDebug =
        match c.debugc.GetILAssembly(CancellationToken.None) with
        | Ok ilAsm -> ilAsm
        | Error diags -> raise(Exception(OlyDiagnostic.PrepareForOutput(diags, CancellationToken.None)))
    {
        c = c
        ilAsm = ilAsm
        ilAsmDebug = ilAsmDebug
    }

/// Will also assert that the syntax tree produced will equal the source.
let Oly (src: string) =
    TestCompilation.Create(src)
    |> stressTest src

/// Will also assert that the syntax tree produced will equal the source.
let OlyWithConditionalDefines (conditionalDefines: string list) (src: string) =
    TestCompilation.CreateWithConditionalDefines(src, ImArray.ofSeq conditionalDefines)
    |> stressTest src

/// Will also assert that the syntax tree produced will equal the source.
let OlyWithRef refSrc src =
    TestCompilation.CreateWithReference(src, refSrc)
    |> stressTest src

/// Will also assert that the syntax tree produced will equal the source.
let OlyWithReference refSrc src =
    let c =
        TestCompilation.CreateWithReference(src, refSrc)
        |> stressTest src
    c.Compilation

/// Does not check for syntax tree - source equality.
let OlyTwo src1 src2 =
    let c = TestCompilation.CreateTwo(src1, src2)
    c

let getAllSymbols (src: string) =
    let c = Oly (src.Replace("~^~", ""))
    let st = c.c.SyntaxTrees.[0]
    let sm = c.c.GetBoundModel(st.Path)
    sm.GetSymbols(st.GetRoot(CancellationToken.None), CancellationToken.None)

let getSymbolByCursorIgnoreDiagnostics (src: string) =
    let cursor = src.IndexOf("~^~")

    let c = Oly (src.Replace("~^~", ""))

    let st = c.c.SyntaxTrees.[0]
    let sm = c.c.GetBoundModel(st.Path)
    
    let tokenOpt = st.GetRoot(CancellationToken.None).TryFindToken(cursor)
    if tokenOpt.IsNone then
        failwith "Unable to find syntax token by cursor."

    let token = tokenOpt.Value

    let symbolOpt = sm.TryFindSymbol(token, CancellationToken.None)
    if symbolOpt.IsNone then
        failwith "Unable to find symbol by cursor."

    symbolOpt.Value

let hasSymbolSignatureTextByCursorIgnoreDiagnostics expectedText (src: string) =
    let symbol = getSymbolByCursorIgnoreDiagnostics src
    Assert.Equal(expectedText, symbol.SignatureText)

let getSymbolByCursor (src: string) =
    let cursor = src.IndexOf("~^~")

    let c = Oly (src.Replace("~^~", "")) |> withNoDiagnostics

    let st = c.c.SyntaxTrees.[0]
    let sm = c.c.GetBoundModel(st.Path)
    
    let tokenOpt = st.GetRoot(CancellationToken.None).TryFindToken(cursor, skipTrivia = false)
    if tokenOpt.IsNone then
        failwith "Unable to find syntax token by cursor."

    let token = tokenOpt.Value

    let symbolOpt = sm.TryFindSymbol(token, CancellationToken.None)
    if symbolOpt.IsNone then
        failwith "Unable to find symbol by cursor."

    symbolOpt.Value

let hasSymbolSignatureTextByCursor expectedText (src: string) =
    let symbol = getSymbolByCursor src
    Assert.Equal(expectedText, symbol.SignatureText)
