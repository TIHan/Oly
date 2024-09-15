open System
open System.Text
open System.Linq
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open OmniSharp.Extensions.LanguageServer
open OmniSharp.Extensions.LanguageServer.Server.Abstractions
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Server
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities
open Oly.Compiler.Text
open Oly.Compiler
open OmniSharp.Extensions.LanguageServer.Protocol.Workspace
open MediatR
open OmniSharp.Extensions.JsonRpc
open Oly.Runtime
open System.IO
open Oly.Core
open Oly.Runtime.Tools
open Oly.Runtime.Clr.Emitter
open Oly.Compiler.Workspace
open Oly.Compiler.Workspace.Service
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open Oly.Compiler.Extensions
open Oly.Compiler.Workspace.Extensions
open Oly.Compiler.Syntax
open Oly.Runtime.Target.DotNet
open Oly.Runtime.Target.Interpreter
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Collections.Immutable

[<AutoOpen>]
module OlyViewModels =

    [<NoEquality;NoComparison>]
    type OlySyntaxNodeViewModel =
        {
            id: string
            color: string
            range: OlyTextRange
            label: string
            description: string
            tooltip: string
            children: OlySyntaxNodeViewModel[]
            collapsibleState: int
            icon: string
            isToken: bool
        }

    [<NoEquality;NoComparison>]
    type OlySyntaxTreeViewModel =
        {
            nodes: OlySyntaxNodeViewModel[]
        }

type OlyTypeSymbol with

    member symbol.TextKind =
        if symbol.IsAlias then
            "alias"
        elif symbol.IsInterface then
            "interface"
        elif symbol.IsClass then
            "class"
        elif symbol.IsShape then
            "shape"
        elif symbol.IsEnum then
            "enum"
        elif symbol.IsStruct then
            "struct"
        elif symbol.IsModule then
            "module"
        elif symbol.IsTypeExtension then
            "type extension"
        else                                
            "type"

type OlyTextRange with

    member range.ToLspRange() =
        Range(Position(range.Start.Line, range.Start.Column), Position(range.End.Line, range.End.Column))

type Position with

    member this.ToOlyTextPosition() =
        OlyTextPosition(this.Line, this.Character)

type Range with

    member this.ToOlyTextRange() =
        OlyTextRange(this.Start.ToOlyTextPosition(), this.End.ToOlyTextPosition())

type OlySourceLocation with

    member this.ToLspLocation(ct) =
        let r = this.GetTextRange(ct).ToLspRange()
        let uri = Protocol.DocumentUri.From(this.SyntaxTree.Path.ToString())
        Location(Range = r, Uri = uri)

let createDiagnostic (olyDiagnostic: OlyDiagnostic) ct =
    let range = 
        match olyDiagnostic.SyntaxTree with
        | Some syntaxTree ->
            syntaxTree.GetSourceText(ct).GetTextRange(olyDiagnostic.TextSpan)
        | _ ->
            OlyTextRange()

    Diagnostic(
        Source = "oly",
        Message = olyDiagnostic.Message,
        Range = range.ToLspRange(),
        Severity = (if olyDiagnostic.IsError then DiagnosticSeverity.Error elif olyDiagnostic.IsWarning then DiagnosticSeverity.Warning else DiagnosticSeverity.Information),
        Code = DiagnosticCode(int64 olyDiagnostic.Code)
    )

let hoverText header olyContent =
    let content =
        MarkupContent(
            Kind = MarkupKind.Markdown,
            Value = sprintf """
%s
```oly
%s
```
                    """ header olyContent)

    Hover(Contents = MarkedStringsOrMarkupContent(content))

let normalizeFilePath (path: string) =
    OlyPath.Create(path)

type OlyDocument with

    member this.ToLspDiagnostics(ct) =
        this.GetDiagnostics(ct)
        |> ImArray.map (fun diag -> createDiagnostic diag ct)

type ILanguageServerFacade with

    member this.PublishDiagnostics(uri, version, diags) =
        let diagnosticParams = 
            PublishDiagnosticsParams(
                Uri = uri,
                Diagnostics = Container(diags |> Array.ofSeq),
                Version = version
            )
        this.TextDocument.PublishDiagnostics(diagnosticParams)

    member this.RefreshClientAsync(ct) =
        backgroundTask {
            let result = this.Client.SendRequest("workspace/semanticTokens/refresh")
            do! result.ReturningVoid(ct)
        }

type OlyCompilation with

    member this.HasErrors(ct) = not (this.GetDiagnostics(ct).IsEmpty)

type ParsedToken =
    {
        line: int
        startCharacter: int
        length: int
        tokenType: string
        tokenModifiers: string []
    }

type IOlyRequest<'T> =
    inherit IRequest<'T>

    abstract DocumentPath: string with get, set

[<NoEquality;NoComparison>]
type OlyLspCompilationResult =
    {
        mutable resultPath: string
        mutable error: string
    }

[<Method("oly/compile", Direction.ClientToServer)>]
type OlyCompileRequest() =

    member val DocumentPath: string = null with get, set

    interface IOlyRequest<OlyLspCompilationResult> with

        member this.DocumentPath
            with get() = this.DocumentPath
            and set value = this.DocumentPath <- value

[<Method("oly/getSyntaxTree", Direction.ClientToServer)>]
type OlyGetSyntaxTreeRequest() =

    member val DocumentPath: string = null with get, set

    interface IOlyRequest<OlySyntaxTreeViewModel> with

           member this.DocumentPath
               with get() = this.DocumentPath
               and set value = this.DocumentPath <- value

[<Method("oly/getIR", Direction.ClientToServer)>]
type OlyGetIRRequest() =

    member val DocumentPath: string = null with get, set
    member val Position: Position = Unchecked.defaultof<_> with get, set
    member val Opts: bool = false with get, set

    interface IOlyRequest<string> with

           member this.DocumentPath
               with get() = this.DocumentPath
               and set value = this.DocumentPath <- value

[<Method("oly/getSemanticClassification", Direction.ClientToServer)>]
type OlyGetSemanticClassificationRequest() =

    member val Range: OlyTextRange = OlyTextRange() with get, set
    member val DocumentPath: string = null with get, set

    interface IOlyRequest<ParsedToken[]> with

           member this.DocumentPath
               with get() = this.DocumentPath
               and set value = this.DocumentPath <- value

type OlyClassificationKind with

    member this.ToLspClassificationModifiers() =
        let modifiers = ImArray.builder()

        match this with
        | OlyClassificationKind.Field
        | OlyClassificationKind.StaticField -> 
            modifiers.Add("readonly")
        | _ ->
            ()

        match this with
        | OlyClassificationKind.StaticField
        | OlyClassificationKind.MutableStaticField
        | OlyClassificationKind.StaticFunction 
        | OlyClassificationKind.StaticProperty
        | OlyClassificationKind.StaticAbstractFunction
        | OlyClassificationKind.Pattern
        | OlyClassificationKind.AbstractPattern ->
            modifiers.Add("static")
        | _ ->
            ()

        match this with
        | OlyClassificationKind.AbstractProperty
        | OlyClassificationKind.AbstractFunction
        | OlyClassificationKind.StaticAbstractProperty
        | OlyClassificationKind.StaticAbstractFunction
        | OlyClassificationKind.AbstractPattern ->
            modifiers.Add("abstract")
        | _ ->
            ()

        modifiers.ToArray()

    member this.ToLspClassificationKind() =
        match this with
        | OlyClassificationKind.Constructor ->
            "type"
        | OlyClassificationKind.ConstructorStruct ->
            "struct"
        | OlyClassificationKind.Function
        | OlyClassificationKind.StaticFunction
        | OlyClassificationKind.AbstractFunction
        | OlyClassificationKind.StaticAbstractFunction ->
            "function"
        | OlyClassificationKind.Operator ->
            "operator"
        | OlyClassificationKind.Class 
        | OlyClassificationKind.Enum ->
            "class"
        | OlyClassificationKind.Interface ->
            "interface"
        | OlyClassificationKind.Field
        | OlyClassificationKind.MutableField
        | OlyClassificationKind.StaticField 
        | OlyClassificationKind.MutableStaticField ->
            "field"
        | OlyClassificationKind.FieldConstant ->
            "enumMember"
        | OlyClassificationKind.LocalValue
        | OlyClassificationKind.MutableLocalValue ->
            "variable"
        | OlyClassificationKind.Module ->
            "module"
        | OlyClassificationKind.Parameter
        | OlyClassificationKind.MutableParameter ->
            "parameter"
        | OlyClassificationKind.Struct 
        | OlyClassificationKind.EnumStruct ->
            "struct"
        | OlyClassificationKind.Shape ->
            "interface"
        | OlyClassificationKind.TypeParameter ->
            "typeParameter"
        | OlyClassificationKind.Property
        | OlyClassificationKind.StaticProperty
        | OlyClassificationKind.AbstractProperty
        | OlyClassificationKind.StaticAbstractProperty ->
            "property"
        | OlyClassificationKind.Namespace ->
            "namespace"
        | OlyClassificationKind.Type ->
            "type"
        | OlyClassificationKind.Keyword ->
            "macro"
        | OlyClassificationKind.KeywordControl ->
            "keyword"
        | OlyClassificationKind.Pattern
        | OlyClassificationKind.AbstractPattern ->
            "enumMember"
        | _ ->
            "label"

    member this.ToLspCompletionItemKind() =
        match this with
        | OlyClassificationKind.Function
        | OlyClassificationKind.StaticFunction
        | OlyClassificationKind.AbstractFunction 
        | OlyClassificationKind.StaticAbstractFunction ->
            CompletionItemKind.Function
        | OlyClassificationKind.Constructor
        | OlyClassificationKind.ConstructorStruct ->
            CompletionItemKind.Constructor
        | OlyClassificationKind.Operator ->
            CompletionItemKind.Operator
        | OlyClassificationKind.Class ->
            CompletionItemKind.Class
        | OlyClassificationKind.Interface ->
            CompletionItemKind.Interface
        | OlyClassificationKind.Field
        | OlyClassificationKind.MutableField
        | OlyClassificationKind.StaticField 
        | OlyClassificationKind.MutableStaticField ->
            CompletionItemKind.Field
        | OlyClassificationKind.FieldConstant ->
            CompletionItemKind.Constant
        | OlyClassificationKind.LocalValue
        | OlyClassificationKind.MutableLocalValue ->
            CompletionItemKind.Variable
        | OlyClassificationKind.Module ->
            CompletionItemKind.Module
        | OlyClassificationKind.Parameter
        | OlyClassificationKind.MutableParameter ->
            CompletionItemKind.Variable
        | OlyClassificationKind.Struct ->
            CompletionItemKind.Struct
        | OlyClassificationKind.Shape ->
            CompletionItemKind.Interface
        | OlyClassificationKind.TypeParameter ->
            CompletionItemKind.TypeParameter
        | OlyClassificationKind.Property
        | OlyClassificationKind.StaticProperty ->
            CompletionItemKind.Property
        | OlyClassificationKind.Namespace ->
            CompletionItemKind.Module
        | OlyClassificationKind.Type ->
            CompletionItemKind.Class
        | OlyClassificationKind.Keyword ->
            CompletionItemKind.Keyword
        | OlyClassificationKind.KeywordControl ->
            CompletionItemKind.Keyword
        | OlyClassificationKind.Enum ->
            CompletionItemKind.Enum
        | OlyClassificationKind.EnumStruct ->
            CompletionItemKind.Enum
        | OlyClassificationKind.Pattern
        | OlyClassificationKind.AbstractPattern ->
            CompletionItemKind.EnumMember
        | _ ->
            CompletionItemKind.Text

    member this.ToLspSymbolKind() =
        match this with
        | OlyClassificationKind.Constructor ->
            SymbolKind.Class
        | OlyClassificationKind.ConstructorStruct ->
            SymbolKind.Struct
        | OlyClassificationKind.Function
        | OlyClassificationKind.StaticFunction
        | OlyClassificationKind.AbstractFunction 
        | OlyClassificationKind.StaticAbstractFunction ->
            SymbolKind.Function
        | OlyClassificationKind.Operator ->
            SymbolKind.Operator
        | OlyClassificationKind.Class ->
            SymbolKind.Class
        | OlyClassificationKind.Interface ->
            SymbolKind.Interface
        | OlyClassificationKind.Field
        | OlyClassificationKind.MutableField
        | OlyClassificationKind.StaticField 
        | OlyClassificationKind.MutableStaticField ->
            SymbolKind.Field
        | OlyClassificationKind.FieldConstant ->
            SymbolKind.Constant
        | OlyClassificationKind.LocalValue
        | OlyClassificationKind.MutableLocalValue ->
            SymbolKind.Variable
        | OlyClassificationKind.Module ->
            SymbolKind.Module
        | OlyClassificationKind.Parameter
        | OlyClassificationKind.MutableParameter ->
            SymbolKind.Variable
        | OlyClassificationKind.Struct ->
            SymbolKind.Struct
        | OlyClassificationKind.Shape ->
            SymbolKind.Interface
        | OlyClassificationKind.TypeParameter ->
            SymbolKind.TypeParameter
        | OlyClassificationKind.Property
        | OlyClassificationKind.StaticProperty 
        | OlyClassificationKind.AbstractProperty
        | OlyClassificationKind.StaticAbstractProperty ->
            SymbolKind.Property
        | OlyClassificationKind.Namespace ->
            SymbolKind.Namespace
        | OlyClassificationKind.Type ->
            SymbolKind.Class
        | OlyClassificationKind.Keyword ->
            SymbolKind.Key
        | OlyClassificationKind.KeywordControl ->
            SymbolKind.Key
        | OlyClassificationKind.Pattern
        | OlyClassificationKind.AbstractPattern ->
            SymbolKind.EnumMember
        | _ ->
            SymbolKind.Object

type Async<'T> with

    member this.AsLspTask(ct: CancellationToken) =
        let tcs = TaskCompletionSource<'T>()
        let onCancel() =
            tcs.SetCanceled(ct)
        Async.StartWithContinuations(this,
            (fun res -> 
                tcs.SetResult(res)),
            (fun res ->
                match res with
                | :? OperationCanceledException ->
                    onCancel()
                | _ ->
                    tcs.SetException(res)),
            (fun _ -> 
                onCancel()),
            ct
        )
        tcs.Task

type OlySymbol with

    member this.ToLspSymbolInfo(ct: CancellationToken) =
        SymbolInformation(
            Name = this.Name,
            Location = this.UseSyntax.GetLocation().ToLspLocation(ct),
            Kind = this.ClassificationKind.ToLspSymbolKind(),
            ContainerName = if this.UseSyntax.IsDefinition then "Definition" else String.Empty
        )

    member this.ToLspWorkspaceSymbol(ct: CancellationToken) =
        WorkspaceSymbol(
            Name = this.Name,
            Location = this.UseSyntax.GetLocation().ToLspLocation(ct),
            Kind = this.ClassificationKind.ToLspSymbolKind(),
            ContainerName = if this.UseSyntax.IsDefinition then "Definition" else String.Empty
        )

    member this.ToLspParameterInfo() =
        ParameterInformation(
            Label = ParameterInformationLabel(this.SignatureText),
            Documentation = StringOrMarkupContent(this.Name)
        )

    member this.ToLspSignatureInfo(activeParameter) =
        SignatureInformation(
            Label = this.SignatureText,
            Documentation = StringOrMarkupContent(this.Name),
            Parameters =
                (
                match this with
                | :? OlyValueSymbol as value ->
                    value.LogicalParameters |> Seq.map (fun x -> x.ToLspParameterInfo()) |> Array.ofSeq |> Container
                | _ ->
                    Container([||])
                ),
            ActiveParameter = activeParameter
        )

    member this.ToLspSignaturePatternInfo(activeParameter) =
        match this.AsValue.ReturnType with
        | Some(returnTy) ->
            if returnTy.IsTuple then
                SignatureInformation(
                    Label = $"{this.Name}{returnTy.SignatureText}",
                    Documentation = StringOrMarkupContent(this.Name),
                    Parameters =
                        (
                        let items =
                            returnTy.GetTupleItemSignatureTexts()
                            |> Seq.map (fun text ->
                                ParameterInformation(
                                    Label = ParameterInformationLabel(text),
                                    Documentation = StringOrMarkupContent("")
                                )
                            )
                            |> Array.ofSeq
                        Container(items)
                        ),
                    ActiveParameter = activeParameter
                )
            elif returnTy.IsUnit then
                SignatureInformation(
                    Label = $"{this.Name}",
                    Documentation = StringOrMarkupContent(this.Name),
                    Parameters = Container([||]),
                    ActiveParameter = activeParameter
                )
            else
                this.ToLspSignatureInfo(activeParameter)
        | _ ->
            this.ToLspSignatureInfo(activeParameter)
            
type OlySymbol with

    member x.TryToLspDocumentSymbol(lspRange, ct) =
        DocumentSymbol(
            Name = x.Name,
            Detail = x.SignatureText,
            Range = lspRange,
            SelectionRange = x.UseSyntax.GetTextRange(ct).ToLspRange(),
            Kind = x.ClassificationKind.ToLspSymbolKind()
        )
        |> Some

    member this.GetLspDefinitionLocation(ct) =
        match this.TryGetDefinitionLocation(ct) with
        | Some location ->
            let r = location.ToLspLocation(ct)
            let link = LocationOrLocationLink(r)
            LocationOrLocationLinks.From([|link|])
        | _ ->
            LocationOrLocationLinks.From([||])

let getSymbolsBySymbol symbol (doc: OlyDocument) ct =
    doc.FindSimilarSymbols(symbol, ct)
    |> Seq.map (fun x -> 
        let syntax = x.UseSyntax
        OlySourceLocation.Create(syntax.TextSpan, syntax.Tree)
    )

let getLocationsBySymbol symbol doc ct =
    getSymbolsBySymbol symbol doc ct
    |> Seq.map (fun x ->
        x.ToLspLocation(ct)
    )

[<Literal>]
let LspWorkspaceStateDirectory = ".olyworkspace/"

[<Literal>]
let LspWorkspaceStateFileName = "state.json"
 
[<Sealed>]
type OlyWorkspaceLspResourceService(server: ILanguageServerFacade, dirWatch: DirectoryWatcher, workspace: OlyWorkspace) as this =
    
    do
        OlyTrace.Log <-
            fun msg ->
                let trace = OmniSharp.Extensions.LanguageServer.Protocol.Models.LogTraceParams(Message = msg)
                server.SendNotification(trace)

    let invalidate (rs: OlyWorkspaceResourceSnapshot) (filePath: string) =
        let solution = workspace.GetSolutionAsync(rs, CancellationToken.None).Result
        let dir = OlyPath.Create(filePath) |> OlyPath.GetDirectory
        solution.GetProjects()
        |> ImArray.iter (fun proj ->
            let mustInvalidate =
                proj.Documents
                |> ImArray.exists (fun x ->
                    OlyPath.Equals(OlyPath.GetDirectory(x.Path), dir)
                )
            if mustInvalidate then
                workspace.RemoveProject(rs, proj.Path, CancellationToken.None)
        )

    let lazyRs = 
        lazy
            let activeConfigPath =
                Path.Combine(Path.Combine(server.ClientSettings.RootPath, LspWorkspaceStateDirectory), LspWorkspaceStateFileName)
                |> OlyPath.Create
            let mutable rs = OlyWorkspaceResourceSnapshot.Create(activeConfigPath)
            
            dirWatch.WatchSubdirectories(server.ClientSettings.RootPath)
            dirWatch.WatchSubdirectories(Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location))

            Directory.EnumerateFiles(server.ClientSettings.RootPath, "*.oly*", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    let filePath = OlyPath.Create(filePath)
                    rs <- rs.SetResourceAsCopy(filePath)
                    if filePath.HasExtension(".olyx") then
                        let _ = workspace.GetDocumentsAsync(rs, filePath, CancellationToken.None)
                        ()
                with
                | _ ->
                    ()
            )

            Directory.EnumerateFiles(Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location), "*.oly*", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    let filePath = OlyPath.Create(filePath)
                    rs <- rs.SetResourceAsCopy(filePath)
                    if filePath.HasExtension(".olyx") then
                        let _ = workspace.GetDocumentsAsync(rs, filePath, CancellationToken.None)
                        ()
                with
                | _ ->
                    ()
            )

            Directory.EnumerateFiles(server.ClientSettings.RootPath, "*.json", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    rs <- rs.SetResourceAsCopy(OlyPath.Create(filePath))
                with
                | _ ->
                    ()
            )

            Directory.EnumerateFiles(Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location), "*.json", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    rs <- rs.SetResourceAsCopy(OlyPath.Create(filePath))
                with
                | _ ->
                    ()
            )

            rs

    let rsObj = obj()
    let mutable rsOpt = None

    let checkValidation filePath =
        match rsOpt with
        | Some rs when (OlyPath.Create(filePath).HasExtension(".oly") || OlyPath.Create(filePath).HasExtension(".olyx")) ->
            invalidate rs filePath
        | _ ->
            ()

    do
        dirWatch.FileCreated.Add(
            fun filePath ->
                let rs: OlyWorkspaceResourceSnapshot = this.State
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(OlyPath.Create(filePath)))
                with
                | _ ->
                    ()
                checkValidation filePath
        )

        dirWatch.FileChanged.Add(
            fun filePath ->
                let rs: OlyWorkspaceResourceSnapshot = this.State
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(OlyPath.Create(filePath)))
                with
                | _ ->
                    ()
                checkValidation filePath
        )

        dirWatch.FileDeleted.Add(
            fun filePath ->
                let rs: OlyWorkspaceResourceSnapshot = this.State
                rsOpt <- Some(rs.RemoveResource(OlyPath.Create(filePath)))
                checkValidation filePath
        )

        dirWatch.FileRenamed.Add(
            fun (oldFilePath, filePath) ->
                let rs: OlyWorkspaceResourceSnapshot = this.State
                let rs = rs.RemoveResource(OlyPath.Create(oldFilePath))
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(OlyPath.Create(filePath)))
                    checkValidation filePath
                with
                | _ ->
                    rsOpt <- Some(rs)
                    checkValidation filePath
        )

    member this.State = 
        match rsOpt with
        | None ->
            lock rsObj (fun () ->
                match rsOpt with
                | None ->
                    rsOpt <- Some lazyRs.Value
                | _ ->
                    ()
            )
            rsOpt.Value
        | Some rs ->
            rs

type ITextDocumentIdentifierParams with

    member this.HandleOlyDocument(rs: OlyWorkspaceResourceSnapshot, ct: CancellationToken, getCts: OlyPath -> CancellationTokenSource, workspace: OlyWorkspace, textManager: OlySourceTextManager, f: OlyDocument -> CancellationToken -> Task<'T>) =
        let documentPath = this.TextDocument.Uri.Path |> normalizeFilePath
        
        try
            let cts = getCts documentPath
            use handleCts = new CancellationTokenSource()

            use _dispose = ct.Register(fun _ -> handleCts.Cancel())
            use _dispose = cts.Token.Register(fun _ -> handleCts.Cancel())
            cts.Token.ThrowIfCancellationRequested()
            ct.ThrowIfCancellationRequested()
            let ct = handleCts.Token

            backgroundTask {
                match textManager.TryGet(documentPath) with
                | Some (sourceText, _) ->
                    let! docs = workspace.UpdateDocumentAsync(rs, documentPath, sourceText, ct)
                    if docs.Length >= 1 then
                        let doc = docs.[0]
                        return! f doc ct
                    else
                        return Unchecked.defaultof<_>
                | _ ->
                    return Unchecked.defaultof<_>
            }
        with
        | _ ->
            Unchecked.defaultof<_>

[<AutoOpen>]
module ExtensionHelpers =

    let rec getViewModel ct (nextId: int ref) (node: OlySyntaxNode) =
        if node.FullTextSpan.Width = 0 then 
            [||]
        elif node.IsSequentialExpression then
            match node :?> OlySyntaxExpression with
            | OlySyntaxExpression.Sequential(expr1, expr2) ->
                let nodes1 = expr1 |> getViewModel ct nextId
                let nodes2 = expr2 |> getViewModel ct nextId
                Array.append nodes1 nodes2
                |> Array.ofSeq
            | _ ->
                failwith "should not happen"
        else
            let children = 
                node.Children 
                |> Seq.map (getViewModel ct nextId)
                |> Array.ofSeq

            let classifyToken (token: OlyToken) =
                let icon =
                    if token.IsNumericLiteral then
                        "symbol-numeric"
                    elif token.IsBoolLiteral then
                        "symbol-boolean"
                    elif token.IsCharLiteral || token.IsStringLiteral then
                        "symbol-string"
                    elif token.IsKeyword then
                        "symbol-keyword"
                    elif token.IsOperator then
                        "symbol-operator"
                    elif token.IsTrivia then
                        "symbol-misc"
                    elif token.IsIdentifier then
                        "symbol-text"
                    else
                        "symbol-key"

                let label =
                    if token.IsEndOfSource then
                        "End of Source"
                    elif token.IsTrivia then
                        if token.IsWhitespaceTrivia then
                            "Whitespace"
                        elif token.IsSingleLineCommentTrivia || token.IsMultiLineCommentTrivia then
                            "Comment"
                        elif token.IsNewLineTrivia then
                            """\n"""
                        elif token.IsCarriageReturnTrivia then
                            """\r"""
                        elif token.IsCarriageReturnNewLineTrivia then
                            """\r\n"""
                        else
                            token.Text
                    else
                        token.Text

                label, icon

            let label, icon =
                match node.TryGetToken() with
                | Some token -> classifyToken token
                | _ -> 
                    let icon =
                        if node.IsError then
                            "error"
                        elif node.IsName then
                            "symbol-text"
                        elif node.IsValueBindingDeclaration then
                            "symbol-value"
                        elif node.IsFunctionBindingDeclaration then
                            "symbol-function"
                        elif node.IsParameter then
                            "symbol-parameter"
                        elif node.IsParameters then
                            "list-tree"
                        elif node.IsTypeParameter || node.IsTypeArgument then
                            "symbol-type-parameter"
                        elif node.IsTypeParameters then
                            "list-tree"
                        elif node.IsArguments then
                            "list-tree"
                        elif node.IsTypeArguments then
                            "list-tree"
                        elif node.IsConstraints then
                            "list-tree"
                        elif node.IsSeparatorList then
                            "list-flat"
                        elif node.IsAttributes then
                            "list-tree"
                        elif node.IsType then
                            "symbol-class"
                        elif node.IsLiteral then
                            "symbol-constant"
                        elif node.IsAttribute then
                            "symbol-color"
                        elif node.IsCompilationUnit then
                            "symbol-file"
                        elif node.IsTypeDeclarationKind || node.IsLambdaKind || node.IsValueKind then
                            "symbol-enum"
                        else
                            "symbol-object"

                    node.DebugName, icon

            let description = 
                match node.TryGetToken() with
                | Some(token) when token.IsTrivia -> "SyntaxTrivia"
                | _ -> node.KindName

            let color =
                if node.IsError then "errorForeground"
                else ""

            [|
                let id = nextId.contents
                nextId.contents <- nextId.contents + 1
                {
                    id = string id
                    color = color
                    range = node.GetTextRange(ct)
                    label = label
                    description = description
                    tooltip = ""
                    children = children |> Array.concat
                    collapsibleState = if children.Length > 0 then 1 else 0
                    icon = icon
                    isToken = node.IsToken
                }
            |]

    type IOlyRequest<'T> with

        member this.HandleOlyDocument(rs: OlyWorkspaceResourceSnapshot, ct: CancellationToken, getCts: OlyPath -> CancellationTokenSource, workspace: OlyWorkspace, textManager: OlySourceTextManager, f: OlyDocument -> CancellationToken -> Task<'T>) =
            backgroundTask {
                let documentPath = this.DocumentPath |> normalizeFilePath
        
                let cts = getCts documentPath
                use handleCts = new CancellationTokenSource()

                use _dispose = ct.Register(fun _ -> handleCts.Cancel())
                use _dispose = cts.Token.Register(fun _ -> handleCts.Cancel())
                cts.Token.ThrowIfCancellationRequested()
                let ct = handleCts.Token

                match textManager.TryGet(documentPath) with
                | Some (sourceText, _) ->
                    let! docs = workspace.UpdateDocumentAsync(rs, documentPath, sourceText, ct)
                    if docs.Length >= 1 then
                        let doc = docs.[0]
                        return! f doc ct
                    else
                        return raise(OperationCanceledException())
                | _ ->
                    return raise(OperationCanceledException())
            }
        

[<NoEquality;NoComparison>]
type WorkspaceSettings =
    {
        editedDocumentDiagnosticMaxDelay: float
        editedDocumentDependentDiagnosticDelay: float
    }

    static member Default =
        {
            editedDocumentDiagnosticMaxDelay = 500
            editedDocumentDependentDiagnosticDelay = 1000
        }

type TextDocumentSyncHandler(server: ILanguageServerFacade) =

    let dirWatch = new DirectoryWatcher()

    let textManager = OlySourceTextManager()
    let targets = 
        [
            InterpreterTarget() :> OlyBuild
            DotNetTarget()
        ] |> ImArray.ofSeq
    let workspace = OlyWorkspace.Create(targets)
    let rs = OlyWorkspaceLspResourceService(server, dirWatch, workspace)

    let documentSelector = TextDocumentSelector(TextDocumentFilter(Scheme = "file", Language = "oly"))

    let mutable settings = WorkspaceSettings.Default

    // Default CTS
    let ctsLock = obj()
    let ctsTable = ConcurrentDictionary<OlyPath, CancellationTokenSource>()
    let getCts filePath =
        match ctsTable.TryGetValue(filePath) with
        | true, cts -> cts
        | _ ->
            lock ctsLock <| fun () ->
                match ctsTable.TryGetValue(filePath) with
                | true, cts -> cts
                | _ ->
                    let cts = new CancellationTokenSource()
                    ctsTable.[filePath] <- cts
                    cts
    
    let cancelAndGetCts filePath =
        lock ctsLock <| fun () ->
            match ctsTable.TryGetValue(filePath) with
            | true, cts ->
                cts.Cancel()
                cts.Dispose()
            | _ ->
                ()
            let cts = new CancellationTokenSource()
            ctsTable.[filePath] <- cts
            cts

    let emptyCodeActionContainer = CommandOrCodeActionContainer([])

    member this.OnDidOpenDocumentAsync(documentPath: OlyPath, version) =
        backgroundTask {
            let cts = getCts documentPath
            let ct = cts.Token
            return MediatR.Unit.Value
        }

    member this.OnDidChangeDocumentAsync(documentPath: OlyPath, version, sourceText: IOlySourceText) =
        backgroundTask {
            let cts = cancelAndGetCts documentPath
            let ct = cts.Token
            workspace.UpdateDocument(rs.State, documentPath, sourceText, ct)
            let work =
                backgroundTask {
                    try
                        let! docs = workspace.GetDocumentsAsync(rs.State, documentPath, ct)

                        for doc in docs do
                            let stopwatch = Stopwatch.StartNew()
                            let diags = doc.ToLspDiagnostics(ct)
                            stopwatch.Stop()

                            let delay = settings.editedDocumentDiagnosticMaxDelay - stopwatch.Elapsed.TotalMilliseconds |> int
                            if delay > 0 then
                                do! Task.Delay(delay, ct).ConfigureAwait(false)

                            server.PublishDiagnostics(Protocol.DocumentUri.From(documentPath.ToString()), version, diags)

                        // Start diagnostic crawling
                        do! Task.Delay(int settings.editedDocumentDependentDiagnosticDelay, ct).ConfigureAwait(false)

                        // TODO Uncomment below: this has performance issues that need to be investigated.
                        //(*
                        for doc in docs do
                            doc.Project.GetDocumentsExcept(doc.Path)
                            |> ImArray.iter (fun doc ->
                                let diags = doc.ToLspDiagnostics(ct)
                                server.PublishDiagnostics(Protocol.DocumentUri.From(doc.Path.ToString()), Nullable(), diags)
                            )

                        do! Task.Delay(int settings.editedDocumentDependentDiagnosticDelay, ct).ConfigureAwait(false)

                        for doc in docs do
                            let depsOn = doc.Project.Solution.GetProjectsDependentOnReference(doc.Project.Path)
                            for dep in depsOn do
                                ct.ThrowIfCancellationRequested()
                                dep.Documents
                                |> ImArray.iter (fun doc ->
                                    ct.ThrowIfCancellationRequested()
                                    let diags = doc.ToLspDiagnostics(ct)
                                    server.PublishDiagnostics(Protocol.DocumentUri.From(doc.Path.ToString()), Nullable(), diags)
                                )
                        //*)
                    with
                    | :? OperationCanceledException ->
                        ()
                }

            try work.Start() with | _ -> ()
            return MediatR.Unit.Value
        }
    
    interface IDocumentRangeFormattingHandler with
        member this.GetRegistrationOptions(capability: DocumentRangeFormattingCapability, clientCapabilities: ClientCapabilities): DocumentRangeFormattingRegistrationOptions = 
            let options = DocumentRangeFormattingRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options

        member this.Handle(request: DocumentRangeFormattingParams, ct: CancellationToken): Task<TextEditContainer> = 
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                let textRange = request.Range.ToOlyTextRange()
                // TODO:
                return null
            })

    interface IDocumentOnTypeFormattingHandler with
        member this.GetRegistrationOptions(capability: DocumentOnTypeFormattingCapability, clientCapabilities: ClientCapabilities): DocumentOnTypeFormattingRegistrationOptions = 
            let options = DocumentOnTypeFormattingRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.WorkDoneProgress <- true
            options.FirstTriggerCharacter <- "="
            options.MoreTriggerCharacter <- Container([|";";",";":";"."|])
            options

        member this.Handle(request: DocumentOnTypeFormattingParams, ct: CancellationToken): Task<TextEditContainer> = 
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                match doc.SyntaxTree.GetRoot(ct).TryFindToken(request.Position.ToOlyTextPosition(), ct = ct) with
                | None -> return TextEditContainer()
                | Some token ->
                    if token.IsEqual then
                        let leadingTrivia = token.GetLeadingTrivia()
                        if leadingTrivia.IsEmpty || not leadingTrivia.[leadingTrivia.Length - 1].IsWhitespaceTrivia then                           
                            let textEdit = 
                                TextEdit(
                                    NewText = " " + token.Text,
                                    Range = token.GetTextRange(ct).ToLspRange()
                                )

                            return TextEditContainer([|textEdit|])
                        else
                            return TextEditContainer()

                    elif token.IsColon || token.IsSemiColon || token.IsDot || token.IsComma then
                        let leadingTrivia = token.GetLeadingTrivia()
                        if leadingTrivia.IsEmpty || not leadingTrivia.[leadingTrivia.Length - 1].IsWhitespaceTrivia then
                            return TextEditContainer()
                        else
                            let whitespaceTriviaRange = leadingTrivia.[leadingTrivia.Length - 1].GetTextRange(ct)

                            let textEdit = 
                                TextEdit(
                                    NewText = token.Text,
                                    Range = whitespaceTriviaRange.Combine(token.GetTextRange(ct)).ToLspRange()
                                )

                            return TextEditContainer([|textEdit|])

                    else
                        return TextEditContainer()
            })

    interface IDidChangeConfigurationHandler with

        member this.SetCapability(capability: DidChangeConfigurationCapability, clientCapabilities: ClientCapabilities) =
            ()
        
        member this.Handle(request: DidChangeConfigurationParams, ct: CancellationToken) =
            let work =
                async {
                    let newSettings =
                        {
                            editedDocumentDiagnosticMaxDelay = request.Settings["editedDocumentDiagnosticMaxDelay"].ToString() |> Double.Parse
                            editedDocumentDependentDiagnosticDelay = request.Settings["editedDocumentDependentDiagnosticDelay"].ToString() |> Double.Parse
                        }
                    settings <- newSettings
                    return MediatR.Unit()
                }
            work.AsLspTask(ct)

    interface ITextDocumentSyncHandler with
        member this.GetRegistrationOptions(_: TextSynchronizationCapability, _: ClientCapabilities): TextDocumentChangeRegistrationOptions = 
            let options = TextDocumentChangeRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.SyncKind <- TextDocumentSyncKind.Incremental
            options

        member this.GetRegistrationOptions(_: TextSynchronizationCapability, _: ClientCapabilities): TextDocumentOpenRegistrationOptions = 
            let options = TextDocumentOpenRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options

        member this.GetRegistrationOptions(_: TextSynchronizationCapability, _: ClientCapabilities): TextDocumentCloseRegistrationOptions = 
            let options = TextDocumentCloseRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options

        member this.GetRegistrationOptions(_: TextSynchronizationCapability, _: ClientCapabilities): TextDocumentSaveRegistrationOptions = 
            let options = TextDocumentSaveRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options

        member this.GetTextDocumentAttributes(uri: OmniSharp.Extensions.LanguageServer.Protocol.DocumentUri): TextDocumentAttributes = 
            new TextDocumentAttributes(uri, "oly")

        member this.Handle(request: DidChangeTextDocumentParams, _ct: CancellationToken): Task<MediatR.Unit> = 
            let documentPath = request.TextDocument.Uri.Path |> normalizeFilePath
            let textChanges =
                request.ContentChanges
                |> Seq.map (fun change -> 
                    OlyTextChangeWithRange(change.Range.ToOlyTextRange(), change.Text)
                )
              
            match textManager.OnChange(documentPath, request.TextDocument.Version, textChanges) with
            | Some(sourceText) ->
                try
                    this.OnDidChangeDocumentAsync(documentPath, request.TextDocument.Version, sourceText).Result
                    |> ignore
                with
                | _ -> ()
            | _ ->
                ()
            Task.FromResult(MediatR.Unit.Value)

        member this.Handle(request: DidOpenTextDocumentParams, _ct: CancellationToken): Task<MediatR.Unit> =
            task {
                let documentPath = request.TextDocument.Uri.Path |> normalizeFilePath
                textManager.OnOpen(documentPath, request.TextDocument.Version)
                this.OnDidOpenDocumentAsync(documentPath, request.TextDocument.Version).Start()
                return MediatR.Unit.Value
            }

        member this.Handle(request: DidCloseTextDocumentParams, _ct: CancellationToken): Task<MediatR.Unit> =
            task {
                let documentPath = request.TextDocument.Uri.Path |> normalizeFilePath
                textManager.OnClose(documentPath)
                return MediatR.Unit.Value
            }

        member this.Handle(_request: DidSaveTextDocumentParams, _ct: CancellationToken): Task<MediatR.Unit> = 
            task {
                return MediatR.Unit.Value
            }

    interface ICodeActionHandler with

        member this.GetRegistrationOptions(_: CodeActionCapability, _: ClientCapabilities): CodeActionRegistrationOptions = 
            let options = CodeActionRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.CodeActionKinds <- Container([CodeActionKind.QuickFix])
            options

        member this.Handle(request: CodeActionParams, ct: CancellationToken): Task<CommandOrCodeActionContainer> = 
            match request.Context.Diagnostics |> Seq.tryFind (fun x -> x.Code.Value.Long = 100L) with
            | Some lspDiag ->
                request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                    let diags = doc.GetDiagnostics(ct)
                    let diagOpt =
                        let r2 = lspDiag.Range
                        diags
                        |> ImArray.tryFind (fun diag ->
                            try
                                let r = doc.SyntaxTree.GetSourceText(ct).GetTextRange(diag.TextSpan)
                                        
                                r2.Start.Line = r.Start.Line && r2.Start.Character = r.Start.Column &&
                                r2.End.Line = r.End.Line && r2.End.Character = r.End.Column &&
                                lspDiag.Code.Value.Long = int64 diag.Code &&
                                lspDiag.Message = diag.Message
                            with
                            | _ ->
                                false
                        )

                    match diagOpt with
                    | Some diag ->
                        let textSpan = diag.TextSpan
                        try
                            let r = doc.SyntaxTree.GetSourceText(ct).GetTextRange(textSpan)
                            let tokenToAdd = diag.Message.Replace("Expected '", "").Replace("'.", "")

                            let edit = 
                                TextEdit(
                                    NewText = tokenToAdd,
                                    Range =
                                        Range(Position(r.End.Line, r.End.Column), 
                                              Position(r.End.Line, r.End.Column))
                                )

                            let textEdits = [edit]

                            let tdi = OptionalVersionedTextDocumentIdentifier(Uri = request.TextDocument.Uri)
                            let textdocedit = 
                                TextDocumentEdit(
                                    Edits = TextEditContainer(textEdits),
                                    TextDocument = tdi
                                )
                            let docChange = WorkspaceEditDocumentChange(textdocedit)

                            let action = 
                                CodeAction(
                                    Title = $"Add '{tokenToAdd}'",
                                    Kind = CodeActionKind.QuickFix,
                                    Edit = WorkspaceEdit(DocumentChanges = Container([docChange]))
                                )
                            return CommandOrCodeActionContainer([CommandOrCodeAction(action)])
                        with
                        | _ ->
                            return emptyCodeActionContainer
                    | _ ->

                    return emptyCodeActionContainer
                })
            | _ ->
                Task.FromResult(emptyCodeActionContainer)

    interface ISignatureHelpHandler with
        member this.GetRegistrationOptions(capability: SignatureHelpCapability, clientCapabilities: ClientCapabilities): SignatureHelpRegistrationOptions = 
            let options = SignatureHelpRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.TriggerCharacters <- Container([",";"("])
            options

        member this.Handle(request: SignatureHelpParams, ct: CancellationToken): Task<SignatureHelp> = 
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                if request.Context.TriggerKind = SignatureHelpTriggerKind.Invoked || request.Context.TriggerKind = SignatureHelpTriggerKind.TriggerCharacter then
                    match doc.TryFindFunctionCallInfo(request.Position.Line, request.Position.Character, ct) with
                    | Some info ->
                        let activeParameter = if info.ActiveParameterIndex = -1 then Unchecked.defaultof<Nullable<_>> else Nullable(info.ActiveParameterIndex)
                        let activeSignature = if info.ActiveFunctionIndex = -1 then Unchecked.defaultof<Nullable<_>> else Nullable(info.ActiveFunctionIndex)
                        match info.Function with
                        | :? OlyFunctionGroupSymbol as funcGroup ->
                            return SignatureHelp(
                                Signatures =
                                    (
                                    funcGroup.Functions
                                    |> Seq.map (fun x -> if info.IsPattern then x.ToLspSignaturePatternInfo(activeParameter) else x.ToLspSignatureInfo(activeParameter))
                                    |> Array.ofSeq
                                    |> Container
                                    ),
                                ActiveParameter = activeParameter,
                                ActiveSignature = activeSignature
                            )
                        | func ->
                            return SignatureHelp(
                                Signatures = Container([|if info.IsPattern then func.ToLspSignaturePatternInfo(activeParameter) else func.ToLspSignatureInfo(activeParameter)|]),
                                ActiveParameter = activeParameter,
                                ActiveSignature = activeSignature
                            )
                    | _ ->
                        return null
                else
                    return null  
            })

    interface IWorkspaceSymbolsHandler with
        member this.GetRegistrationOptions(capability: WorkspaceSymbolCapability, clientCapabilities: ClientCapabilities): WorkspaceSymbolRegistrationOptions = 
            WorkspaceSymbolRegistrationOptions()

        member this.Handle(request: WorkspaceSymbolParams, ct: CancellationToken): Task<Container<WorkspaceSymbol>> = 
            backgroundTask {
                if String.IsNullOrWhiteSpace request.Query then
                    return Container<WorkspaceSymbol>()
                else

                let! solution = workspace.GetSolutionAsync(rs.State, CancellationToken.None)

                let lspSymbols =
                    solution.GetProjects()
                    |> ImArray.map (fun proj ->
                        proj.Documents
                        |> ImArray.map (fun doc ->
                            doc.GetAllSymbolsByPossibleName(request.Query, ct)
                        )
                        |> ImArray.concat
                    )
                    |> ImArray.concat
                    |> ImArray.map (fun x ->
                        x.ToLspWorkspaceSymbol(ct)
                    )

                return Container<WorkspaceSymbol>(lspSymbols)
            }

    interface IDocumentSymbolHandler with
        member this.GetRegistrationOptions(capability: DocumentSymbolCapability, clientCapabilities: ClientCapabilities): DocumentSymbolRegistrationOptions = 
            let options = DocumentSymbolRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.Label <- "Oly Symbols"
            options

        member this.Handle(request: DocumentSymbolParams, ct: CancellationToken): Task<SymbolInformationOrDocumentSymbolContainer> = 
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                let symbols = doc.GetAllSymbols(ct)
                let result =
                    let sortedSymbols =
                        symbols
                        |> ImArray.choose(fun symbol ->
                            match symbol.UseSyntax.TryGetParentExpression(true, ct) with
                            | Some syntaxExprNode ->
                                Some(symbol, syntaxExprNode)
                            | _ ->
                                None
                        )
                        |> Seq.groupBy(fun (_, syntaxNode) ->
                            syntaxNode    
                        )
                        |> Seq.choose (fun (syntaxNode, symbols) ->
                            let principalSymbolOpt =
                                symbols
                                |> Seq.filter (fun (symbol, _) ->
                                    match symbol with
                                    | :? OlyValueSymbol as symbol when symbol.IsParameter -> false
                                    | :? OlyTypeSymbol as symbol when symbol.IsTypeParameter -> false
                                    | _ ->
                                        symbol.UseSyntax.IsDefinition
                                )
                                |> Seq.tryHead
                                |> Option.bind (fun (x, syntaxNode) -> 
                                    let lspRange = syntaxNode.GetTextRange(ct).ToLspRange()
                                    x.TryToLspDocumentSymbol(lspRange, ct))

                            match principalSymbolOpt with
                            | Some principalSymbol ->
                                Some(syntaxNode, principalSymbol)
                            | _ ->
                                None
                        )
                        |> Seq.sortBy (fun (syntaxNode, _) ->
                            let textSpan = syntaxNode.TextSpan
                            (textSpan.Start, textSpan.Width)
                        )
                        |> ImArray.ofSeq

                    let parentStack = Stack()

                    let pushParent x isChild =
                        let r = (x, ResizeArray(), isChild)
                        parentStack.Push(r)

                    let results = ResizeArray()

                    sortedSymbols
                    |> ImArray.iter (fun ((syntaxNode, principalSymbol) as x) ->
                        let rec loop () =
                            match parentStack.TryPeek() with
                            | false, _ ->
                                pushParent x false
                            | true, ((parentSyntaxNode, parentSymbol), parentChildren, isChild) ->
                                if parentSyntaxNode.TextSpan.Contains(syntaxNode.TextSpan) then
                                    parentChildren.Add(principalSymbol)
                                    pushParent x true
                                else
                                    let ((parentSyntaxNode, parentSymbol), parentChildren, isChild) = parentStack.Pop()
                                   // parentSymbol.Children <- Container(parentChildren) // TODO: Fix this.
                                    if not isChild then
                                        results.Add(parentSymbol)
                                    loop()
                        loop()
                    )

                    parentStack
                    |> Seq.iter (fun ((_, symbol), children, isChild) ->
                       // symbol.Children <- Container(children) // TODO: Fix this.
                        if not isChild then
                            results.Add(symbol)
                    )

                    results
                    |> Seq.map (fun symbol ->
                        SymbolInformationOrDocumentSymbol.Create(symbol)
                    )

                return SymbolInformationOrDocumentSymbolContainer.From(result)
            })

    interface IDocumentHighlightHandler with
        member this.GetRegistrationOptions(capability: DocumentHighlightCapability, clientCapabilities: ClientCapabilities): DocumentHighlightRegistrationOptions = 
            let options = DocumentHighlightRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options

        member this.Handle(request: DocumentHighlightParams, ct: CancellationToken): Task<DocumentHighlightContainer> = 
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                let symbolOpt = doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct)
                match symbolOpt with
                | Some symbol ->
                    let symbols = doc.FindSimilarSymbols(symbol, ct)
                    let hs =
                        symbols
                        |> Seq.map (fun x ->
                            DocumentHighlight(
                                Range = x.UseSyntax.GetTextRange(ct).ToLspRange(),
                                Kind = DocumentHighlightKind.Text
                            )
                        )
                        |> Array.ofSeq
                    return DocumentHighlightContainer.From(hs)                        
                | _ ->
                    return DocumentHighlightContainer.From([||])
            })

    member this.FindAllReferences(doc: OlyDocument, line: int, column: int, ct) =
        backgroundTask {
            let allSymbols = List()
            let symbolOpt = doc.TryFindSymbol(line, column, ct)
            match symbolOpt with
            | Some symbol ->
                if symbol.IsInLocalScope then
                    allSymbols.AddRange(getLocationsBySymbol symbol doc ct)
                else
                    let! solution = workspace.GetSolutionAsync(rs.State, ct)
                    match solution.GetDocuments(doc.Path) |> ImArray.tryFind (fun x -> OlyPath.Equals(x.Project.Path, doc.Project.Path)) with
                    | Some(doc) ->
                        // Re-find the symbol because the solution and projects could have changed.
                        let symbolOpt = doc.TryFindSymbol(line, column, ct)
                        match symbolOpt with
                        | Some symbol ->
                            let originatingProj = doc.Project
                            let projs = solution.GetProjectsDependentOnReference(originatingProj.Path).Add(originatingProj)
                            projs
                            |> ImArray.iter (fun proj ->
                                proj.Documents
                                |> ImArray.iter (fun doc ->
                                    allSymbols.AddRange(getLocationsBySymbol symbol doc ct)
                                )
                            )
                        | _ ->
                            ()
                    | _ ->
                        ()
            | _ ->
                ()
            return allSymbols
        }

    interface IReferencesHandler with
        member this.GetRegistrationOptions(capability: ReferenceCapability, clientCapabilities: ClientCapabilities): ReferenceRegistrationOptions = 
            let options = ReferenceRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options

        member this.Handle(request: ReferenceParams, ct: CancellationToken): Task<LocationContainer> = 
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                let! allSymbols = this.FindAllReferences(doc, request.Position.Line, request.Position.Character, ct)
                return LocationContainer.From(allSymbols)                   
            })

    interface IDefinitionHandler with
        member this.GetRegistrationOptions(capability: DefinitionCapability, clientCapabilities: ClientCapabilities): DefinitionRegistrationOptions = 
            let options = DefinitionRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.WorkDoneProgress <- true
            options

        member this.Handle(request: DefinitionParams, ct: CancellationToken): Task<LocationOrLocationLinks> =
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                let symbolOpt = doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct)
                match symbolOpt with
                | Some symbol ->
                    return symbol.GetLspDefinitionLocation(ct)
                | _ ->
                    return LocationOrLocationLinks.From([||])
            })

    interface ITypeDefinitionHandler with
        member this.GetRegistrationOptions(capability: TypeDefinitionCapability, clientCapabilities: ClientCapabilities): TypeDefinitionRegistrationOptions = 
            let options = TypeDefinitionRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.WorkDoneProgress <- true
            options

        member this.Handle(request: TypeDefinitionParams, ct: CancellationToken): Task<LocationOrLocationLinks> =
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                let symbolOpt = doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct)
                match symbolOpt with
                | Some symbol ->
                    match symbol with 
                    | :? OlyTypeSymbol ->
                        return symbol.GetLspDefinitionLocation(ct)
                    | :? OlyValueSymbol as symbol ->
                        match symbol.Enclosing.TryType with
                        | Some tySymbol ->
                            return tySymbol.GetLspDefinitionLocation(ct)
                        | _ ->
                            return LocationOrLocationLinks.From([||])
                    | _ ->
                        return LocationOrLocationLinks.From([||])
                | _ ->

                return LocationOrLocationLinks.From([||])
            })

    interface ICodeLensHandler with

        member this.GetRegistrationOptions(capability: CodeLensCapability, clientCapabilities: ClientCapabilities): CodeLensRegistrationOptions = 
            let options = CodeLensRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options

        member this.Handle(request: CodeLensParams, ct: CancellationToken): Task<CodeLensContainer> = 
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                let syntaxRoot = doc.SyntaxTree.GetRoot(ct)
                let entryPointOpt = 
                    syntaxRoot.ChooseDescendants(function 
                        | :? OlySyntaxExpression as syntaxExpr ->
                            match syntaxExpr with 
                            | OlySyntaxExpression.ValueDeclaration(_, _, _, _, _, syntaxBinding) ->
                                match syntaxBinding with
                                | OlySyntaxBinding.Implementation(syntaxBindingDecl, _, _) ->
                                    match syntaxBindingDecl with
                                    | OlySyntaxBindingDeclaration.Function(syntaxFuncName, _, _, _, _) ->
                                        match syntaxFuncName with
                                        | OlySyntaxFunctionName.Identifier(syntaxIdent) when syntaxIdent.GetText(ct).ContentEquals(OlySourceText.Create("main")) ->
                                            Some syntaxIdent
                                        | _ ->
                                            None
                                    | _ ->
                                        None
                                | _ ->
                                    None
                            | _ ->
                                None
                        | _ -> 
                            None
                    )
                    |> Seq.tryExactlyOne
                    |> Option.bind (fun x -> x.TryGetToken())
                    |> Option.bind (fun x -> doc.BoundModel.TryFindSymbol(x, ct))
                    |> Option.bind (fun x -> 
                        match x with
                        | :? OlyValueSymbol as value when value.IsEntryPoint -> Some value
                        | _ -> None
                    )

                match entryPointOpt with
                | None -> return CodeLensContainer.From([||])
                | Some entryPoint ->      
                    match entryPoint.TryGetDefinitionLocation(ct) with
                    | None -> return CodeLensContainer.From([||])
                    | Some location ->

                        let cmd = Command(Title = "Run", Name = "workbench.action.debug.run")
                        let clRun = CodeLens(Command = cmd, Range = location.GetTextRange(ct).ToLspRange())

                        let cmd = Command(Title = "Debug", Name = "workbench.action.debug.start")
                        let clDebug = CodeLens(Command = cmd, Range = location.GetTextRange(ct).ToLspRange())

                        return CodeLensContainer.From([|clRun;clDebug|])
            })

    interface IHoverHandler with
        member this.GetRegistrationOptions(_: HoverCapability, _: ClientCapabilities): HoverRegistrationOptions = 
            let options = HoverRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.WorkDoneProgress <- true
            options

        member this.Handle(request: HoverParams, ct: CancellationToken): Task<Hover> = 
            backgroundTask {
                return! request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                    let symbolOpt = doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct)
                    
                    match symbolOpt with
                    | Some symbol ->
                        match symbol with
                        | :? OlyFunctionGroupSymbol as symbol ->
                            let strBuilder = StringBuilder()
                            symbol.Functions
                            |> ImArray.iter (fun func -> strBuilder.Append("    " + func.SignatureText + "\n") |> ignore)
                            return hoverText "Possible overloads:\n" (strBuilder.ToString())
                        | :? OlyValueSymbol as symbol ->
                            let textResult = symbol.SignatureText

                            let textResult =
                                if symbol.IsAbstract then
                                    sprintf "abstract %s" textResult
                                else
                                    textResult

                            let textResult =
                                if symbol.IsField then
                                    if symbol.IsStatic then
                                        match symbol.TryFieldConstant with
                                        | ValueSome(constant) ->
                                            let valueText =
                                                match constant.Value with
                                                | OlyConstant.UInt8(value) -> string value
                                                | OlyConstant.Int8(value) -> string value
                                                | OlyConstant.UInt16(value) -> string value
                                                | OlyConstant.Int16(value) -> string value
                                                | OlyConstant.UInt32(value) -> string value
                                                | OlyConstant.Int32(value) -> string value
                                                | OlyConstant.UInt64(value) -> string value
                                                | OlyConstant.Int64(value) -> string value
                                                | OlyConstant.Float32(value) -> string value
                                                | OlyConstant.Float64(value) -> string value
                                                | OlyConstant.True -> "true"
                                                | OlyConstant.False -> "false"
                                                | OlyConstant.Char16(value) -> $"'{value}'"
                                                | OlyConstant.Utf16(value) -> $"\"{value}\""
                                                | OlyConstant.Null -> "null"
                                                | OlyConstant.Default -> "default"
                                                | OlyConstant.Array _ -> "OlyConstant.Array (implement this)"
                                                | OlyConstant.External(value) -> value.SignatureText
                                                | OlyConstant.Variable(ty) -> ty.SignatureText
                                                | OlyConstant.Error -> "?"
                                            sprintf "constant %s: %s = %s" symbol.Name symbol.Type.SignatureText valueText
                                        | _ ->
                                            sprintf "%s" textResult
                                    else
                                        sprintf "%s" textResult
                                elif symbol.IsProperty then
                                    textResult
                                elif symbol.IsParameter then
                                    sprintf "(parameter) %s" textResult
                                elif symbol.IsInLocalScope then
                                    sprintf "(local) %s" textResult
                                else
                                    textResult

                            let header = String.Empty   
                            let header, olyContent = header, textResult
                            return hoverText header olyContent

                        | :? OlyTypeSymbol as symbol ->
                            let textResult = 
                                let documentation = symbol.Documentation
                                if documentation.Length = 0 then
                                    sprintf "%s %s" symbol.TextKind symbol.SignatureText
                                else
                                    // TODO: Handle multiple lines.
                                    sprintf "// %s\n%s %s" documentation symbol.TextKind symbol.SignatureText
                            let textResult =
                                match symbol.TryGetAliasedType() with
                                | Some(aliasedSymbol) ->
                                    $"{textResult} = {aliasedSymbol.FullyQualifiedName}"
                                | _ ->
                                    textResult                             
                            return hoverText "" textResult

                        | :? OlyNamespaceSymbol as symbol ->
                            let textResult = sprintf "namespace %s" symbol.SignatureText
                            return hoverText "" textResult

                        | _ ->
                            return hoverText "" symbol.SignatureText
                    | _ ->
                        return null
                })
            }

    interface ICompletionHandler with
        member this.GetRegistrationOptions(c: CompletionCapability, client: ClientCapabilities) =
            let options = CompletionRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.TriggerCharacters <- Container([".";",";":";" "])
            options.WorkDoneProgress <- true
            options.AllCommitCharacters <- null
            options.ResolveProvider <- true
            options

        member this.Handle(request, ct) =
            backgroundTask {
                try
                    let documentPath = request.TextDocument.Uri.Path |> normalizeFilePath
                    match textManager.TryGet(documentPath) with
                    | Some (sourceText, _) ->
                        let! docs = workspace.UpdateDocumentAsync(rs.State, documentPath, sourceText, ct)
                        if docs.IsEmpty then
                            return null
                        else
                            let doc = docs[0]
                            let items =
                                doc.GetCompletions(request.Position.Line, request.Position.Character, ct)
                                |> Seq.distinctBy (fun x -> x.Label)
                                |> Seq.map (fun x ->
                                    ct.ThrowIfCancellationRequested()
                                    let kind = x.ClassificationKind.ToLspCompletionItemKind()
                                    CompletionItem(Label = x.Label, Detail = x.Detail, InsertText = x.InsertText, Kind = kind)
                                )
                                |> ImArray.ofSeq
                            return CompletionList(items)
                    | _ ->
                        return null
                with
                | _ ->
                    return null
            }

    interface ICompletionResolveHandler with
        member this.Handle(request: CompletionItem, cancellationToken: CancellationToken): Task<CompletionItem> = 
            Task.FromResult(request)
        member this.Id: Guid = 
            Guid.NewGuid()
        member this.SetCapability(capability: CompletionCapability, clientCapabilities: ClientCapabilities): unit = 
            ()

    interface IRenameHandler with

        member this.GetRegistrationOptions(capability: RenameCapability, clientCapabilities: ClientCapabilities): RenameRegistrationOptions =
            RenameRegistrationOptions()

        member this.Handle(request, ct): Task<WorkspaceEdit> =
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct -> backgroundTask {
                let! allSymbols = this.FindAllReferences(doc, request.Position.Line, request.Position.Character, ct)
                if allSymbols.Count > 0 then    
                    let edit = WorkspaceEdit(Changes = Dictionary())
                    allSymbols
                    |> Seq.iter (fun loc ->
                        let uri = loc.Uri

                        let textEdits =
                            match edit.Changes.TryGetValue(uri) with
                            | true, textEdits ->
                                textEdits :?> List<TextEdit>
                            | _ ->
                                let textEdits = List<TextEdit>()
                                edit.Changes[uri] <- textEdits
                                textEdits

                        let textEdit = 
                            TextEdit(
                                Range = loc.Range,
                                NewText = request.NewName
                            )

                        textEdits.Add(textEdit)
                    )
                    return edit                  
                else
                    return WorkspaceEdit()
            })



    interface IJsonRpcRequestHandler<OlyCompileRequest, OlyLspCompilationResult> with

        member _.Handle(request, ct) =
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct ->
                backgroundTask {
                    match! workspace.BuildProjectAsync(rs.State, doc.Project.Path, ct) with
                    | Ok prog -> 
                        return { resultPath = prog.Path.ToString(); error = null }
                    | Error error -> 
                        let diags =
                            error
                            |> Seq.groupBy (fun x -> match x.SyntaxTree with Some syntaxTree -> syntaxTree.Path | _ -> OlyPath.Create(request.DocumentPath))

                        diags
                        |> Seq.iter (fun (docPath, diags) ->
                            let diags = diags |> Seq.map (fun x -> createDiagnostic x ct)
                            server.PublishDiagnostics(Protocol.DocumentUri.From(docPath.ToString()), Nullable(), diags)
                        )
                        return { resultPath = null; error = OlyDiagnostic.PrepareForOutput(error, ct) }
                }
            )

    interface IJsonRpcRequestHandler<OlyGetIRRequest, string> with

        member _.Handle(request, ct) =
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct ->
                backgroundTask {
                    match doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct) with
                    | None ->
                        return "Symbol not found."
                    | Some symbol ->
                        match symbol with
                        | :? OlyValueSymbol as valueSymbol when valueSymbol.IsFunction ->
                            try
                                let c = doc.Project.Compilation
                                let allRefs =
                                    c.References
                                    |> ImArray.map (fun x -> x.GetILAssembly(ct))
                                match c.GetILAssembly(ct) with
                                | Error(_) ->
                                    return "Compilation has failures."
                                | Ok(ilAsm) ->
                                    let dummyEmitter = DummyEmitter()
                                    let runtime = OlyRuntime(dummyEmitter)

                                    runtime.ImportAssembly(ilAsm.ToReadOnly())

                                    allRefs
                                    |> ImArray.iter (fun x -> 
                                        match x with
                                        | Ok asm ->
                                            runtime.ImportAssembly(asm.ToReadOnly())
                                        | _ ->
                                            ()
                                    )

                                    runtime.InitializeEmitter()

                                    runtime.EmitEntryPoint()
                                    let fullQualifiedTyName =
                                        match valueSymbol.Enclosing.TryType with
                                        | Some ty ->
                                            match ty.Enclosing.TryNamespace with
                                            | Some(nmspc: OlyNamespaceSymbol) -> 
                                                nmspc.SignatureText + "." + ty.Name
                                            | _ ->
                                                ty.Name
                                        | _ -> 
                                            ""
                                    match runtime.TryGetIROfFunction(fullQualifiedTyName, valueSymbol.Name) with
                                    | Error(msg) ->
                                        return msg
                                    | Ok(irFuncBody) ->
                                        return irFuncBody.Expression.ToString()                               
                            with
                            | ex ->
                                return ex.Message
                        | _ ->
                            return "Invalid symbol."
                }
            )

    interface IJsonRpcRequestHandler<OlyGetSyntaxTreeRequest, OlySyntaxTreeViewModel> with

        member _.Handle(request, ct) =
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun doc ct ->
                backgroundTask {
                    let mutable nextId = ref 1               
                    let nodes = doc.SyntaxTree.GetRoot(ct) |> getViewModel ct nextId
                    return { nodes = nodes }
                }
            )

    interface IJsonRpcRequestHandler<OlyGetSemanticClassificationRequest, ParsedToken[]> with

        member _.Handle(request, ct) =
            request.HandleOlyDocument(rs.State, ct, getCts, workspace, textManager, fun (doc: OlyDocument) ct ->
                backgroundTask {
                    let classify line column width tokenType tokenModifiers =
                        {
                            line = line
                            startCharacter = column
                            length = width
                            tokenType = tokenType
                            tokenModifiers = tokenModifiers
                        }
                    let classifications =
                        doc.GetSemanticClassifications(request.Range, ct)
                        |> Seq.map (fun item ->
                            let tokenType = item.Kind.ToLspClassificationKind()
                            let tokenModifiers = item.Kind.ToLspClassificationModifiers()
                            classify item.Start.Line item.Start.Column item.Width tokenType tokenModifiers
                        )
                        |> Array.ofSeq
            
                    return classifications
                })

[<EntryPoint>]
let main argv =
#if DEBUG
    System.Diagnostics.Debugger.Launch() |> ignore
#endif
    let configureServices = 
        fun (services: IServiceCollection) -> ()
    let configureServices = Action<IServiceCollection>(configureServices)
    let optionsf =
        fun (options: LanguageServerOptions) ->
            options.Concurrency <- Environment.ProcessorCount |> Nullable
            options
             .WithInput(Console.OpenStandardInput())
             .WithOutput(Console.OpenStandardOutput())
             .WithLoggerFactory(new LoggerFactory())
             .AddDefaultLoggingProvider()
             .WithServices(configureServices)
             .WithHandler<TextDocumentSyncHandler>()
             |> ignore

    let server = LanguageServer.From(optionsf).Result
    server.WaitForExit.Wait()

    0