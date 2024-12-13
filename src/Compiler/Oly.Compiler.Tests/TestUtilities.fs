module TestUtilities

open System
open System.Threading
open System.Diagnostics
open Xunit
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Compiler.Extensions

module Configuration =

    let mutable defaultReferences: OlyCompilationReference imarray = ImArray.empty
    let mutable implicitExtendsForStruct: string option = None
    let mutable implicitExtendsForEnum: string option = None

open Configuration

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
        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true; ImplicitExtendsForEnum = implicitExtendsForEnum; ImplicitExtendsForStruct = implicitExtendsForStruct }
        let c = OlyCompilation.Create("olytest", [OlySyntaxTree.Parse(OlyPath.Create("olytest1"), src, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true; CompilationUnitConfigurationEnabled = true })], defaultReferences, options = options)
        TestCompilation.Create(c)

    static member CreateWithConditionalDefines(src: string, conditionalDefines) =
        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true; ImplicitExtendsForEnum = implicitExtendsForEnum; ImplicitExtendsForStruct = implicitExtendsForStruct }
        let c = OlyCompilation.Create("olytest", [OlySyntaxTree.Parse(OlyPath.Create("olytest1"), src, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true; CompilationUnitConfigurationEnabled = true; ConditionalDefines = conditionalDefines })], defaultReferences, options = options)
        TestCompilation.Create(c)

    static member CreateWithReference(src: string, refSrc: string) =
        let options = { OlyCompilationOptions.Default with Parallel = false; ImplicitExtendsForEnum = implicitExtendsForEnum; ImplicitExtendsForStruct = implicitExtendsForStruct }
        let refc = OlyCompilation.Create("olytestref", [OlySyntaxTree.Parse(OlyPath.Create("olytestref1"), refSrc, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = false })], defaultReferences, options = options)
        let refcRef = OlyCompilationReference.Create(OlyPath.Create "olytestref", (fun () -> refc))
        TestCompilation.CreateWithCRef(src, refcRef)

    static member CreateWithTwoReferences(src: string, refSrc1: string, refSrc2: string) =
        let options = { OlyCompilationOptions.Default with Parallel = false; ImplicitExtendsForEnum = implicitExtendsForEnum; ImplicitExtendsForStruct = implicitExtendsForStruct }

        let refc2 = OlyCompilation.Create("olytestref2", [OlySyntaxTree.Parse(OlyPath.Create("olytestref2"), refSrc2, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = false })], defaultReferences, options = options)
        let refcRef2 = OlyCompilationReference.Create(OlyPath.Create "olytestref2", (fun () -> refc2))

        let refc1 = OlyCompilation.Create("olytestref1", [OlySyntaxTree.Parse(OlyPath.Create("olytestref1"), refSrc1, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = false })], defaultReferences.Add(refcRef2), options = options)
        let refcRef1 = OlyCompilationReference.Create(OlyPath.Create "olytestref1", (fun () -> refc1))

        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true; ImplicitExtendsForEnum = implicitExtendsForEnum; ImplicitExtendsForStruct = implicitExtendsForStruct }
        let c = OlyCompilation.Create("olytest", [OlySyntaxTree.Parse(OlyPath.Create("olytest1"), src, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true; CompilationUnitConfigurationEnabled = true })], defaultReferences.Add(refcRef1).Add(refcRef2), options = options)
        TestCompilation.Create(c)

    static member CreateWithCRef(src: string, refcRef: OlyCompilationReference) =
        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true; ImplicitExtendsForEnum = implicitExtendsForEnum; ImplicitExtendsForStruct = implicitExtendsForStruct }
        let c = OlyCompilation.Create("olytest", [OlySyntaxTree.Parse(OlyPath.Create("olytest1"), src, parsingOptions = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true; CompilationUnitConfigurationEnabled = true })], defaultReferences.Add(refcRef), options = options)
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
        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true; ImplicitExtendsForEnum = implicitExtendsForEnum; ImplicitExtendsForStruct = implicitExtendsForStruct }
        let c = OlyCompilation.Create("olytest", syntaxTrees, defaultReferences, options = options)
        TestCompilation.Create(c)

    static member CreateThree(src1: string, src2: string, src3: string) =
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
                OlySyntaxTree.Parse(
                    OlyPath.Create("olytest3"), 
                    src3, 
                    parsingOptions = 
                        { 
                            OlyParsingOptions.Default with 
                                AnonymousModuleDefinitionAllowed = false
                                CompilationUnitConfigurationEnabled = false 
                        }
                )
            ]
        let options = { OlyCompilationOptions.Default with Parallel = false; Executable = true; ImplicitExtendsForEnum = implicitExtendsForEnum; ImplicitExtendsForStruct = implicitExtendsForStruct }
        let c = OlyCompilation.Create("olytest", syntaxTrees, defaultReferences, options = options)
        TestCompilation.Create(c)

let getFirstSyntaxTree (c: TestCompilation) =
    c.c.SyntaxTrees.[0]

/// Stress tests updating the source text character by character to ensure the compiler does not crash
/// in various syntax tree states.
/// Does not cover any permutations as it would lead to excessively large test execution times.
let private stressTyping (c: TestCompilation) =
    let tree = c.Compilation.GetSyntaxTree(OlyPath.Create("olytest1"))
    let text = tree.GetSourceText(CancellationToken.None)
    let str = text.ToString()

    let mutable comp = c.Compilation
    let length = str.Length
    for i = 0 to length - 1 do
        let str = str.Substring(0, i)

        try
            let newTree = tree.ApplySourceText(OlySourceText.Create(str))

            let newRoot = newTree.GetRoot(CancellationToken.None)
            Assert.Equal(str, newRoot.BuildSource(CancellationToken.None))
            let rec loop (node: OlySyntaxNode) =
                node.GetFullTextRange(CancellationToken.None) |> ignore
                node.Children
                |> ImArray.iter loop
            loop newRoot

            comp <- comp.SetSyntaxTree(newTree)
            let boundModel = comp.GetBoundModel(tree.Path)
            boundModel.GetSymbols(comp.GetSyntaxTree(OlyPath.Create("olytest1")).GetRoot(CancellationToken.None), CancellationToken.None)
            |> ignore
        with
        | ex ->
            raise(System.InvalidOperationException(str, ex))

let private randomPartialSyntax (c: TestCompilation) =
    let tree = c.Compilation.GetSyntaxTree(OlyPath.Create("olytest1"))
    let text = tree.GetSourceText(CancellationToken.None)
    let str = text.ToString()

    let random = Random()
    for _ = 1 to 10 do
        let tokens = tree.GetRoot(CancellationToken.None).GetDescendantTokens()

        let randomToken =
            let i = random.Next(0, tokens.Length - 1)
            tokens[i]

        let range = randomToken.Node.GetTextRange(CancellationToken.None)
        let span = text.GetTextSpan(range)

        let str = str.Remove(span.Start, span.Width)

        try
            let newTree = tree.ApplySourceText(OlySourceText.Create(str))

            let newRoot = newTree.GetRoot(CancellationToken.None)
            Assert.Equal(str, newRoot.BuildSource(CancellationToken.None))
            let rec loop (node: OlySyntaxNode) =
                node.GetFullTextRange(CancellationToken.None) |> ignore
                node.Children
                |> ImArray.iter loop
            loop newRoot

            let comp = c.Compilation.SetSyntaxTree(newTree)
            let boundModel = comp.GetBoundModel(tree.Path)
            boundModel.GetSymbols(comp.GetSyntaxTree(OlyPath.Create("olytest1")).GetRoot(CancellationToken.None), CancellationToken.None)
            |> ignore
        with
        | ex ->
            raise(System.InvalidOperationException(str, ex))

let private stressTest origSrc (c: TestCompilation) =
    let syntaxTree = getFirstSyntaxTree c
    let root = syntaxTree.GetRoot(CancellationToken.None)
    Assert.Equal(origSrc, root.BuildSource(CancellationToken.None))
    let rec loop (node: OlySyntaxNode) =
        node.GetFullTextRange(CancellationToken.None) |> ignore
        node.Children
        |> ImArray.iter loop
    loop root

#if STRESS
    stressTyping c

    // We do this to ensure that syntax nodes with diagnostics do not get collected.
    // Syntax nodes associated with diagnostics are kept in a ConditionalWeakTable internally.
    GC.Collect(2, GCCollectionMode.Forced, true)
    GC.WaitForPendingFinalizers()

   // randomPartialSyntax c
#endif

    c

let withNoSyntaxDiagnostics (c: TestCompilation) =
    let diags = c.c.GetSyntaxTree(OlyPath.Create("olytest1")).GetDiagnostics(CancellationToken.None)
    if diags.IsEmpty then
        c
    else
        failwithf "%A" diags

let withSyntaxErrorDiagnostics (expected: string list) (c: TestCompilation) =
    let errorMsgs = c.c.GetSyntaxTree(OlyPath.Create("olytest1")).GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> x.Message)
    Assert.Equal(expected, errorMsgs)
    c

let withSyntaxErrorHelperTextDiagnostics (expected: (string * string) list) (c: TestCompilation) =
    let errorMsgs = c.c.GetSyntaxTree(OlyPath.Create("olytest1")).GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> (x.Message, "\r\n" + x.GetHelperText() + "\r\n")) |> Array.ofSeq
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
    Assert.Equal(expected, errorMsgs)
    c

let hasErrorDiagnostics (c: TestCompilation) =
    let errorMsgs = c.c.GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> x.Message) |> Array.ofSeq
    Assert.NotEmpty(errorMsgs)

let withErrorHelperTextDiagnosticsAux (expected: (string * string) list) (c: TestCompilation) =
    let errorMsgs = c.c.GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> (x.Message, "\r\n" + x.GetHelperText() + "\r\n")) |> Array.ofSeq

    let builder = Text.StringBuilder()
    (expected |> ImArray.ofSeq, errorMsgs |> ImArray.ofSeq)
    ||> ImArray.tryIter2 (fun expected actual ->
        try
            Assert.Equal(expected, actual)
        with
        | _ ->
            builder.Append($"=========================================\nExpected:\n{expected}\n\nActual:\n{actual}\n\n")
            |> ignore
    )

    if expected.Length > errorMsgs.Length then
        expected
        |> ImArray.ofSeq
        |> ImArray.skip errorMsgs.Length
        |> ImArray.iter (fun expected ->
            builder.Append($"=========================================\nExpected:\n{expected}\n\nActual:\n\n\n")
            |> ignore
        )
    elif expected.Length < errorMsgs.Length then
        errorMsgs
        |> ImArray.ofSeq
        |> ImArray.skip expected.Length
        |> ImArray.iter (fun actual ->
            builder.Append($"=========================================\nExpected:\n\n\nActual:\n{actual}\n\n")
            |> ignore
        )

    if builder.Length > 0 then
        Assert.Fail(builder.ToString())
    c

[<DebuggerHidden>]
let withErrorHelperTextDiagnostics expected c = withErrorHelperTextDiagnosticsAux expected c

/// Same as 'withErrorHelperTextDiagnostics' but ignores the result.
[<DebuggerHidden>]
let hasErrorHelperTextDiagnostics expected c = withErrorHelperTextDiagnosticsAux expected c |> ignore

let containsErrorHelperTextDiagnostics (expected: (string * string) list) (c: TestCompilation) =
    let errorMsgs = c.c.GetDiagnostics(CancellationToken.None) |> Seq.filter (fun x -> x.IsError) |> Seq.map (fun x -> (x.Message, "\r\n" + x.GetHelperText() + "\r\n")) |> Array.ofSeq
    let result =
        expected
        |> Seq.forall (fun (expectedMsg, expectedText) ->
            errorMsgs
            |> Array.exists (fun (msg, text) ->
                expectedMsg = msg && expectedText = text
            )
        )
    Assert.True(result, if (errorMsgs.Length = 0) then "Expected error messages but received none." else sprintf "%A" errorMsgs)
    c

[<NoEquality;NoComparison>]
type TestCompilationOutput =
    {
        c: TestCompilation
        ilAsm: Oly.Metadata.OlyILAssembly
        ilAsmDebug: Oly.Metadata.OlyILAssembly
    }

let private withCompileAux (c: TestCompilation) =
    let c = 
        c
        |> withNoSyntaxDiagnostics
    let ilAsmDebug =
        match c.debugc.GetILAssembly(CancellationToken.None) with
        | Ok ilAsm -> ilAsm
        | Error diags -> raise(Exception(OlyDiagnostic.PrepareForOutput(diags, CancellationToken.None)))
    let ilAsm =
        match c.c.GetILAssembly(CancellationToken.None) with
        | Ok ilAsm -> ilAsm
        | Error diags -> raise(Exception(OlyDiagnostic.PrepareForOutput(diags, CancellationToken.None)))

    {
        c = c
        ilAsm = ilAsm
        ilAsmDebug = ilAsmDebug
    }

let withCompile c = withCompileAux c

/// Same as 'withCompile' but ignores the result.
let shouldCompile c = withCompileAux c |> ignore

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
let OlyWithRefTwo refSrc2 refSrc1 src =
    TestCompilation.CreateWithTwoReferences(src, refSrc1, refSrc2)
    |> stressTest src

/// Will also assert that the syntax tree produced will equal the source.
let OlyWithReference refSrc src =
    let c =
        TestCompilation.CreateWithReference(src, refSrc)
        |> stressTest src
    c.Compilation

let OlyWithCRef cRef src =
    let c =
        TestCompilation.CreateWithCRef(src, cRef)
        |> stressTest src
    c

/// Does not check for syntax tree - source equality.
let OlyTwo src1 src2 =
    TestCompilation.CreateTwo(src1, src2)

/// Does not check for syntax tree - source equality.
let OlyThree src1 src2 src3 =
    TestCompilation.CreateThree(src1, src2, src3)

let getAllSymbols (src: string) =
    let c = Oly (src.Replace("~^~", ""))
    let st = c.c.GetSyntaxTree(OlyPath.Create("olytest1"))
    let sm = c.c.GetBoundModel(st.Path)
    sm.GetSymbols(st.GetRoot(CancellationToken.None), CancellationToken.None)

let getSymbolByCursorIgnoreDiagnostics (src: string) =
    let cursor = src.IndexOf("~^~")

    let c = Oly (src.Replace("~^~", ""))

    let st = c.c.GetSyntaxTree(OlyPath.Create("olytest1"))
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
    let symbolInfo = getSymbolByCursorIgnoreDiagnostics src
    Assert.Equal(expectedText, symbolInfo.SignatureText)

let getSymbolByCursor (src: string) =
    let cursor = src.IndexOf("~^~")

    let c = Oly (src.Replace("~^~", "")) |> withNoDiagnostics

    let st = c.c.GetSyntaxTree(OlyPath.Create("olytest1"))
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
    let symbolInfo = getSymbolByCursor src
    Assert.Equal(expectedText, symbolInfo.SignatureText)

let getSymbolByCursor2 (src2: string) (src1: string) =
    let cursor = src1.IndexOf("~^~")

    let c = OlyTwo (src1.Replace("~^~", "")) src2 |> withNoDiagnostics

    let st = c.c.GetSyntaxTree(OlyPath.Create("olytest1"))
    let sm = c.c.GetBoundModel(st.Path)
    
    let tokenOpt = st.GetRoot(CancellationToken.None).TryFindToken(cursor, skipTrivia = false)
    if tokenOpt.IsNone then
        failwith "Unable to find syntax token by cursor."

    let token = tokenOpt.Value

    let symbolOpt = sm.TryFindSymbol(token, CancellationToken.None)
    if symbolOpt.IsNone then
        failwith "Unable to find symbol by cursor."

    symbolOpt.Value

let getSymbolByCursor3 (src3: string) (src2: string) (src1: string) =
    let cursor = src1.IndexOf("~^~")

    let c = OlyThree (src1.Replace("~^~", "")) src2 src3 |> withNoDiagnostics

    let st = c.c.GetSyntaxTree(OlyPath.Create("olytest1"))
    let sm = c.c.GetBoundModel(st.Path)
    
    let tokenOpt = st.GetRoot(CancellationToken.None).TryFindToken(cursor, skipTrivia = false)
    if tokenOpt.IsNone then
        failwith "Unable to find syntax token by cursor."

    let token = tokenOpt.Value

    let symbolOpt = sm.TryFindSymbol(token, CancellationToken.None)
    if symbolOpt.IsNone then
        failwith "Unable to find symbol by cursor."

    symbolOpt.Value
