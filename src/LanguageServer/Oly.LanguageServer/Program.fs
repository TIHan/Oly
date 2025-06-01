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
        let diags =
            this.GetDiagnostics(ct)
            |> ImArray.map (fun diag -> createDiagnostic diag ct)
        let analyzerDiags =
            this.GetAnalyzerDiagnostics(ct)
            |> ImArray.map (fun diag -> createDiagnostic diag ct)
        diags.AddRange(analyzerDiags)

type ILanguageServerFacade with

    member this.PublishDiagnostics(uri, version, diags) =
        let diagnosticParams = 
            PublishDiagnosticsParams(
                Uri = uri,
                Diagnostics = Container(diags |> Array.ofSeq),
                Version = version
            )
        this.TextDocument.PublishDiagnostics(diagnosticParams)

    member this.ClearDiagnostics(uri) =
        let diagnosticParams = 
            PublishDiagnosticsParams(
                Uri = uri,
                Diagnostics = Container([||]),
                Version = Nullable()
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

type IOlyDocumentRequest<'T> =
    inherit IRequest<'T>

    abstract DocumentPath: string with get, set

[<NoEquality;NoComparison>]
type OlyCompilationResult =
    {
        mutable resultPath: string
        mutable error: string
    }

[<Method("oly/compileActiveProject", Direction.ClientToServer)>]
type OlyCompileActiveProjectRequest() =

    member val DocumentPath: string = null with get, set

    interface IOlyDocumentRequest<OlyCompilationResult> with

        member this.DocumentPath
            with get() = this.DocumentPath
            and set value = this.DocumentPath <- value

[<Method("oly/getSyntaxTree", Direction.ClientToServer)>]
type OlyGetSyntaxTreeRequest() =

    member val DocumentPath: string = null with get, set

    interface IOlyDocumentRequest<OlySyntaxTreeViewModel> with

        member this.DocumentPath
            with get() = this.DocumentPath
            and set value = this.DocumentPath <- value

[<Method("oly/getProjectList", Direction.ClientToServer)>]
type OlyGetProjectListRequest() =

    interface IRequest<string[]>


[<Method("oly/getActiveProjectConfigurationList", Direction.ClientToServer)>]
type OlyGetActiveProjectConfigurationListRequest() =

    interface IRequest<string[]>

[<Method("oly/tryGetActiveProjectConfiguration", Direction.ClientToServer)>]
type OlyTryGetActiveProjectConfigurationRequest() =

    interface IRequest<string>

[<Method("oly/doesProjectExist", Direction.ClientToServer)>]
type OlyDoesProjectExistRequest() =

    member val ProjectName: string = null with get, set
    
    interface IRequest<bool>

[<Method("oly/doesActiveProjectConfigurationExist", Direction.ClientToServer)>]
type OlyDoesActiveProjectConfigurationExistRequest() =

    member val ConfigurationName: string = null with get, set
    
    interface IRequest<bool>

[<Method("oly/cleanWorkspace", Direction.ClientToServer)>]
type OlyCleanWorkspaceRequest() =

    interface IRequest

[<Method("oly/getIR", Direction.ClientToServer)>]
type OlyGetIRRequest() =

    member val DocumentPath: string = null with get, set
    member val Position: Position = Unchecked.defaultof<_> with get, set
    member val Opts: bool = false with get, set

    interface IOlyDocumentRequest<string> with

           member this.DocumentPath
               with get() = this.DocumentPath
               and set value = this.DocumentPath <- value

[<Method("oly/getSemanticClassification", Direction.ClientToServer)>]
type OlyGetSemanticClassificationRequest() =

    member val Range: OlyTextRange = OlyTextRange() with get, set
    member val DocumentPath: string = null with get, set
    member val Version: Nullable<int> = Nullable() with get, set

    interface IOlyDocumentRequest<ParsedToken[]> with

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

    member this.ToLspParameterInfo(subModel: OlyBoundSubModel) =
        ParameterInformation(
            Label = ParameterInformationLabel(subModel.GetSignatureText(this)),
            Documentation = StringOrMarkupContent(this.Name)
        )

    member this.ToLspSignatureInfo(subModel: OlyBoundSubModel, activeParameter) =
        SignatureInformation(
            Label = subModel.GetSignatureText(this),
            Documentation = StringOrMarkupContent(this.Name),
            Parameters =
                (
                match this with
                | :? OlyValueSymbol as value ->
                    value.LogicalParameters |> Seq.map (fun x -> x.ToLspParameterInfo(subModel)) |> Array.ofSeq |> Container
                | _ ->
                    Container([||])
                ),
            ActiveParameter = activeParameter
        )

    member this.ToLspSignaturePatternInfo(subModel: OlyBoundSubModel, activeParameter) =
        match this.AsValue.ReturnType with
        | Some(returnTy) ->
            if returnTy.IsTuple then
                SignatureInformation(
                    Label = $"{this.Name}{subModel.GetSignatureText(returnTy)}",
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
                this.ToLspSignatureInfo(subModel, activeParameter)
        | _ ->
            this.ToLspSignatureInfo(subModel, activeParameter)

    member this.GetLspDefinitionLocation(boundModel, ct) =
        match this.TryGetDefinitionLocation(boundModel, ct) with
        | Some location ->
            let r = location.ToLspLocation(ct)
            let link = LocationOrLocationLink(r)
            LocationOrLocationLinks.From([|link|])
        | _ ->
            LocationOrLocationLinks.From([||])

type OlySymbolUseInfo with

    member this.ToLspSymbolInfo(ct: CancellationToken) =
        SymbolInformation(
            Name = this.Symbol.Name,
            Location = this.Syntax.GetLocation().ToLspLocation(ct),
            Kind = this.Symbol.ClassificationKind.ToLspSymbolKind(),
            ContainerName = if this.Syntax.IsDefinition then "Definition" else String.Empty
        )

    member this.ToLspWorkspaceSymbol(ct: CancellationToken) =
        WorkspaceSymbol(
            Name = this.Symbol.Name,
            Location = this.Syntax.GetLocation().ToLspLocation(ct),
            Kind = this.Symbol.ClassificationKind.ToLspSymbolKind(),
            ContainerName = if this.Syntax.IsDefinition then "Definition" else String.Empty
        )

    member this.ToLspParameterInfo() =
        ParameterInformation(
            Label = ParameterInformationLabel(this.SignatureText),
            Documentation = StringOrMarkupContent(this.Symbol.Name)
        )

    member this.ToLspSignatureInfo(subModel: OlyBoundSubModel, activeParameter) =
        SignatureInformation(
            Label = this.SignatureText,
            Documentation = StringOrMarkupContent(this.Symbol.Name),
            Parameters =
                (
                match this.Symbol with
                | :? OlyValueSymbol as value ->
                    value.LogicalParameters |> Seq.map (fun x -> x.ToLspParameterInfo(subModel)) |> Array.ofSeq |> Container
                | _ ->
                    Container([||])
                ),
            ActiveParameter = activeParameter
        )
            
type OlySymbolUseInfo with

    member x.TryToLspDocumentSymbol(lspRange, ct) =
        DocumentSymbol(
            Name = x.Symbol.Name,
            Detail = x.SignatureText,
            Range = lspRange,
            SelectionRange = x.Syntax.GetTextRange(ct).ToLspRange(),
            Kind = x.Symbol.ClassificationKind.ToLspSymbolKind()
        )
        |> Some

let getSymbolsBySymbol symbol (doc: OlyDocument) ct =
    doc.FindSimilarSymbols(symbol, ct)
    |> Seq.map (fun x -> 
        let syntax = x.Syntax
        OlySourceLocation.Create(syntax.TextSpan, syntax.Tree)
    )

let getLocationsBySymbol symbol doc ct =
    getSymbolsBySymbol symbol doc ct
    |> Seq.map (fun x ->
        x.ToLspLocation(ct)
    )

[<Sealed>]
type OlyLspWorkspaceProgress(server: ILanguageServerFacade) =

    let analysisLock = obj()
    let mutable observer: Protocol.Progress.IProgressObserver<obj> = Unchecked.defaultof<_>

    let mutable analysisCount = 0
    let mutable buildCount = 0

    member private this.GetObserver() = 
        if isNull observer then
            lock analysisLock <| fun _ ->
                if isNull observer then
                    observer <- server.ProgressManager.For(ProgressToken("oly/analysis"), CancellationToken.None)
        observer

    member private this.OnBeginAnalysisProgress() =
        if Interlocked.Increment(&analysisCount) = 1 then
            let observer = this.GetObserver()
            observer.OnNext({| kind = "begin"; title = "Analyzing" |}: obj)

    member private this.OnEndAnalysisProgress() =
        if Interlocked.Decrement(&analysisCount) = 0 then
            let observer = this.GetObserver()
            observer.OnNext({| kind = "end" |}: obj)

    member this.ForAnalyzingProgress(ct: CancellationToken, f) = backgroundTask {
        ct.ThrowIfCancellationRequested()
        this.OnBeginAnalysisProgress()
        try
            return! f(ct)
        finally
            this.OnEndAnalysisProgress()
    }

    member private this.OnBeginBuildProgress() =
        if Interlocked.Increment(&buildCount) = 1 then
            let observer = this.GetObserver()
            observer.OnNext({| kind = "begin"; title = "Building"; message = "building" |}: obj)

    member private this.OnEndBuildProgress() =
        if Interlocked.Decrement(&buildCount) = 0 then
            let observer = this.GetObserver()
            observer.OnNext({| kind = "end"; message = "building" |}: obj)

    member this.CreateBuildProgress() =
        this.OnBeginBuildProgress()
        { new IDisposable with member _.Dispose() = this.OnEndBuildProgress() }

    interface IOlyWorkspaceProgress with
        member this.OnBeginWork() = 
            this.OnBeginAnalysisProgress()

        member this.OnEndWork() = 
            this.OnEndAnalysisProgress()

type ITextDocumentIdentifierParams with

    member this.HandleOlyDocument(progress: OlyLspWorkspaceProgress, rs: OlyWorkspaceResourceSnapshot, ct: CancellationToken, getCts: OlyPath -> CancellationTokenSource, workspace: OlyWorkspace, f: OlyDocument -> CancellationToken -> Task<'T>) =
        let documentPath = this.TextDocument.Uri.Path |> normalizeFilePath
        
        try
            let cts = getCts documentPath
            use handleCts = new CancellationTokenSource()

            use _dispose = ct.Register(fun _ -> handleCts.Cancel())
            use _dispose = cts.Token.Register(fun _ -> handleCts.Cancel())
            cts.Token.ThrowIfCancellationRequested()
            ct.ThrowIfCancellationRequested()
            let ct = handleCts.Token

            progress.ForAnalyzingProgress(ct,
                fun ct ->
                    backgroundTask {
                        match rs.TextEditors.TryGet(documentPath) with
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
            )
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

    let tryGetActiveProject (snapshot: OlyWorkspaceResourceSnapshot) (workspace: OlyWorkspace) ct = backgroundTask {
        let activeConfigPath = snapshot.ActiveConfigurationPath
        let settingsPath = OlyPath.Combine(OlyPath.GetDirectory(activeConfigPath), "settings.json")
        try
            let jsonOptions = Json.JsonSerializerOptions()
            jsonOptions.PropertyNameCaseInsensitive <- true
            let vscode = Json.JsonSerializer.Deserialize<{| activeProject: string |}>(File.ReadAllText(settingsPath.ToString()), jsonOptions)
            let! solution = workspace.GetSolutionAsync(snapshot, ct)            
            return solution.TryGetProjectByName(vscode.activeProject)
        with
        | _ ->
            return None
    }

    type IOlyDocumentRequest<'T> with

        member this.HandleOlyDocument(progress: OlyLspWorkspaceProgress, rs: OlyWorkspaceResourceSnapshot, ct: CancellationToken, getCts: OlyPath -> CancellationTokenSource, workspace: OlyWorkspace, f: OlyDocument -> CancellationToken -> Task<'T>) =
            progress.ForAnalyzingProgress(ct,
                fun ct ->
                    backgroundTask {
                        let documentPath = this.DocumentPath |> normalizeFilePath
        
                        let cts = getCts documentPath
                        use handleCts = new CancellationTokenSource()

                        use _dispose = ct.Register(fun _ -> handleCts.Cancel())
                        use _dispose = cts.Token.Register(fun _ -> handleCts.Cancel())
                        cts.Token.ThrowIfCancellationRequested()
                        let ct = handleCts.Token

                        match rs.TextEditors.TryGet(documentPath) with
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
            )
        

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

    let progress = OlyLspWorkspaceProgress(server)

    do
        OlyTrace.Log <-
            fun msg ->
                let trace = OmniSharp.Extensions.LanguageServer.Protocol.Models.LogTraceParams(Message = msg)
                server.SendNotification(trace)

    let lazyGetRootPath = lazy OlyPath.Create(server.ClientSettings.RootPath)

    let mutable textManager = OlySourceTextManager.Empty
    // NOTE: Removing any of these targets may result in exceptions when modifying prelude.
    let targets = 
        [
            DotNetTarget() :> OlyBuild
            Oly.Runtime.Target.Spirv.SpirvTarget()
            InterpreterTarget()
        ] |> ImArray.ofSeq
    let workspace = OlyWorkspace.Create(targets, progress)
    let workspaceListener = OlyWorkspaceListener(workspace, lazyGetRootPath)

    let getSnapshot() =
        workspaceListener.ResourceSnapshot.WithTextEditors(textManager)

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

    let publishDiagnostics (documentPath: OlyPath, version, ct) = backgroundTask {
            let! docs = progress.ForAnalyzingProgress(ct,
                fun ct -> backgroundTask {
                    let! docs = workspace.GetDocumentsAsync(getSnapshot(), documentPath, ct)

                    for doc in docs do
                        let stopwatch = Stopwatch.StartNew()
                        let diags = doc.ToLspDiagnostics(ct)
                        stopwatch.Stop()

                        let delay = settings.editedDocumentDiagnosticMaxDelay - stopwatch.Elapsed.TotalMilliseconds |> int
                        if delay > 0 then
                            do! Task.Delay(delay, ct).ConfigureAwait(false)

                        server.PublishDiagnostics(Protocol.DocumentUri.From(documentPath.ToString()), version, diags)

                    return docs
                })

            // Start diagnostic crawling
            do! Task.Delay(int settings.editedDocumentDependentDiagnosticDelay, ct).ConfigureAwait(false)

            let! _ = progress.ForAnalyzingProgress(ct,
                fun ct -> backgroundTask {
                    for doc in docs do
                        doc.Project.GetDocumentsExcept(doc.Path)
                        |> ImArray.iter (fun doc ->
                            let diags = doc.ToLspDiagnostics(ct)
                            server.PublishDiagnostics(Protocol.DocumentUri.From(doc.Path.ToString()), Nullable(), diags)
                        )
                })

            do! Task.Delay(int settings.editedDocumentDependentDiagnosticDelay, ct).ConfigureAwait(false)

            let! _ = progress.ForAnalyzingProgress(ct,
                fun ct -> backgroundTask {
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
                })

            ()
        }

    member this.OnDidOpenDocumentAsync(documentPath: OlyPath, version) =
        backgroundTask {
            let cts = getCts documentPath
            let ct = cts.Token
            let work =
                backgroundTask {
                    try
                        do! publishDiagnostics(documentPath, version, ct)
                    with
                    | :? OperationCanceledException ->
                        ()
                }
            try work.Start() with | _ -> ()
            return MediatR.Unit.Value
        }

    member this.OnDidChangeDocumentAsync(documentPath: OlyPath, version, sourceText: IOlySourceText) =
        backgroundTask {
            let cts = cancelAndGetCts documentPath
            let ct = cts.Token
            workspace.UpdateDocument(getSnapshot(), documentPath, sourceText, ct)
            let work =
                backgroundTask {
                    try
                        do! publishDiagnostics(documentPath, version, ct)
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
            options.WorkDoneProgress <- true
            options

        member this.Handle(request: DocumentRangeFormattingParams, ct: CancellationToken): Task<TextEditContainer> = 
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
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
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
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
            | Some(sourceText, newTextManager) ->
                textManager <- newTextManager
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
                textManager <- textManager.OnOpen(documentPath, request.TextDocument.Version)
                this.OnDidOpenDocumentAsync(documentPath, request.TextDocument.Version).Start()
                return MediatR.Unit.Value
            }

        member this.Handle(request: DidCloseTextDocumentParams, _ct: CancellationToken): Task<MediatR.Unit> =
            task {
                let documentPath = request.TextDocument.Uri.Path |> normalizeFilePath
                textManager <- textManager.OnClose(documentPath)
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
            options.WorkDoneProgress <- true
            options.CodeActionKinds <- Container([CodeActionKind.QuickFix])
            options

        member this.Handle(request: CodeActionParams, ct: CancellationToken): Task<CommandOrCodeActionContainer> = 
            match request.Context.Diagnostics |> Seq.tryFind (fun x -> x.Code.Value.Long = 100L) with
            | Some lspDiag ->
                request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
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
            options.WorkDoneProgress <- true
            options.TriggerCharacters <- Container([",";"("])
            options

        member this.Handle(request: SignatureHelpParams, ct: CancellationToken): Task<SignatureHelp> = 
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
                if request.Context.TriggerKind = SignatureHelpTriggerKind.Invoked || request.Context.TriggerKind = SignatureHelpTriggerKind.TriggerCharacter then
                    match doc.TryFindFunctionCallInfo(request.Position.Line, request.Position.Character, ct) with
                    | Some info ->
                        let activeParameter = if info.ActiveParameterIndex = -1 then Unchecked.defaultof<Nullable<_>> else Nullable(info.ActiveParameterIndex)
                        let activeSignature = if info.ActiveFunctionIndex = -1 then Unchecked.defaultof<Nullable<_>> else Nullable(info.ActiveFunctionIndex)
                        let subModel = info.SubModel
                        match info.Function with
                        | :? OlyFunctionGroupSymbol as funcGroup ->
                            return SignatureHelp(
                                Signatures =
                                    (
                                    funcGroup.Functions
                                    |> Seq.map (fun x -> if info.IsPattern then x.ToLspSignaturePatternInfo(subModel, activeParameter) else x.ToLspSignatureInfo(subModel, activeParameter))
                                    |> Array.ofSeq
                                    |> Container
                                    ),
                                ActiveParameter = activeParameter,
                                ActiveSignature = activeSignature
                            )
                        | func ->
                            return SignatureHelp(
                                Signatures = Container([|if info.IsPattern then func.ToLspSignaturePatternInfo(subModel, activeParameter) else func.ToLspSignatureInfo(subModel, activeParameter)|]),
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

                let! solution = workspace.GetSolutionAsync(getSnapshot(), CancellationToken.None)

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
            options.WorkDoneProgress <- true
            options.Label <- "Oly Symbols"
            options

        member this.Handle(request: DocumentSymbolParams, ct: CancellationToken): Task<SymbolInformationOrDocumentSymbolContainer> = 
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
                let symbols = doc.GetAllSymbols(ct)
                let result =
                    let sortedSymbols =
                        symbols
                        |> ImArray.choose(fun symbolInfo ->
                            match symbolInfo.Syntax.TryGetParentExpression(true, ct) with
                            | Some syntaxExprNode ->
                                Some(symbolInfo, syntaxExprNode)
                            | _ ->
                                None
                        )
                        |> Seq.groupBy(fun (_, syntaxNode) ->
                            syntaxNode    
                        )
                        |> Seq.choose (fun (syntaxNode, symbols) ->
                            let principalSymbolOpt =
                                symbols
                                |> Seq.filter (fun (symbolInfo, _) ->
                                    match symbolInfo.Symbol with
                                    | :? OlyValueSymbol as symbol when symbol.IsParameter -> false
                                    | :? OlyTypeSymbol as symbol when symbol.IsTypeParameter -> false
                                    | _ ->
                                        symbolInfo.Syntax.IsDefinition
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
            options.WorkDoneProgress <- true
            options

        member this.Handle(request: DocumentHighlightParams, ct: CancellationToken): Task<DocumentHighlightContainer> = 
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
                let symbolOpt = doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct)
                match symbolOpt with
                | Some symbolInfo ->
                    let symbols = doc.FindSimilarSymbols(symbolInfo.Symbol, ct)
                    let hs =
                        symbols
                        |> Seq.map (fun x ->
                            DocumentHighlight(
                                Range = x.Syntax.GetTextRange(ct).ToLspRange(),
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
            let symbolInfoOpt = doc.TryFindSymbol(line, column, ct)
            match symbolInfoOpt with
            | Some symbolInfo ->
                if symbolInfo.Symbol.IsInLocalScope then
                    allSymbols.AddRange(getLocationsBySymbol symbolInfo.Symbol doc ct)
                else
                    match symbolInfo.Symbol.TryGetDefinitionLocation(doc.BoundModel, ct) with
                    | Some location ->
                        let definitionPath = location.SyntaxTree.Path
                        let solution = doc.Project.Solution
                        let docs = solution.GetDocuments(definitionPath)
                        for doc in docs do
                            let originatingProj = doc.Project
                            let projs = solution.GetProjectsDependentOnReference(originatingProj.Path).Add(originatingProj)
                            projs
                            |> ImArray.iter (fun proj ->
                                proj.Documents
                                |> ImArray.iter (fun doc ->
                                    allSymbols.AddRange(getLocationsBySymbol symbolInfo.Symbol doc ct)
                                )
                            )
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
            options.WorkDoneProgress <- true
            options

        member this.Handle(request: ReferenceParams, ct: CancellationToken): Task<LocationContainer> = 
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
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
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
                let symbolInfoOpt = doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct)
                match symbolInfoOpt with
                | Some symbolInfo ->
                    return symbolInfo.Symbol.GetLspDefinitionLocation(doc.BoundModel, ct)
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
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
                let symbolInfoOpt = doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct)
                match symbolInfoOpt with
                | Some symbolInfo ->
                    match symbolInfo.Symbol with 
                    | :? OlyTypeSymbol as symbol ->
                        return symbol.GetLspDefinitionLocation(doc.BoundModel, ct)
                    | :? OlyValueSymbol as symbol ->
                        match symbol.Enclosing.TryType with
                        | Some tySymbol ->
                            return tySymbol.GetLspDefinitionLocation(doc.BoundModel, ct)
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
            options.WorkDoneProgress <- true
            options

        member this.Handle(request: CodeLensParams, ct: CancellationToken): Task<CodeLensContainer> = 
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
                match! tryGetActiveProject (getSnapshot()) workspace ct with
                | None -> return CodeLensContainer.From([||])
                | Some(proj) when not(OlyPath.Equals(proj.Path, doc.Path)) -> return CodeLensContainer.From([||])
                | _ ->

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
                        match x.Symbol with
                        | :? OlyValueSymbol as value when value.IsEntryPoint -> Some value
                        | _ -> None
                    )

                match entryPointOpt with
                | None -> return CodeLensContainer.From([||])
                | Some entryPoint ->      
                    match entryPoint.TryGetDefinitionLocation(doc.BoundModel, ct) with
                    | None -> return CodeLensContainer.From([||])
                    | Some location ->

                        let cmd = Command(Title = "Run", Name = "workbench.action.debug.run")
                        let clRun = CodeLens(Command = cmd, Range = location.GetTextRange(ct).ToLspRange())

                        if doc.Project.Configuration.Debuggable then
                            let cmd = Command(Title = "Debug", Name = "workbench.action.debug.start")
                            let clDebug = CodeLens(Command = cmd, Range = location.GetTextRange(ct).ToLspRange())

                            return CodeLensContainer.From([|clRun;clDebug|])
                        else
                            return CodeLensContainer.From([|clRun|])
            })

    interface IHoverHandler with
        member this.GetRegistrationOptions(_: HoverCapability, _: ClientCapabilities): HoverRegistrationOptions = 
            let options = HoverRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.WorkDoneProgress <- true
            options

        member this.Handle(request: HoverParams, ct: CancellationToken): Task<Hover> = 
            backgroundTask {
                return! request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
                    let symbolInfoOpt = doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct)
                    
                    match symbolInfoOpt with
                    | Some symbolInfo ->
                        let subModel = symbolInfo.SubModel
                        match symbolInfo.Symbol with
                        | :? OlyFunctionGroupSymbol as symbol ->
                            let strBuilder = StringBuilder()
                            symbol.Functions
                            |> ImArray.iter (fun func -> strBuilder.Append("    " + subModel.GetSignatureText(func) + "\n") |> ignore)
                            return hoverText "Possible overloads:\n" (strBuilder.ToString())
                        | :? OlyValueSymbol as symbol ->
                            let textResult = symbolInfo.SignatureText

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
                                                | OlyConstant.External(value) -> subModel.GetSignatureText(value)
                                                | OlyConstant.Variable(ty) -> subModel.GetSignatureText(ty)
                                                | OlyConstant.Error -> "?"
                                            sprintf "constant %s: %s = %s" symbol.Name (subModel.GetSignatureText(symbol.Type)) valueText
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
                                    sprintf "%s %s" symbol.TextKind (subModel.GetSignatureText(symbol))
                                else
                                    // TODO: Handle multiple lines.
                                    sprintf "// %s\n%s %s" documentation symbol.TextKind (subModel.GetSignatureText(symbol))
                            let textResult =
                                
                                match symbolInfo.TryGetAliasedType() with
                                | Some(aliasedSymbol) ->
                                    $"{textResult} = {aliasedSymbol.FullyQualifiedName}"
                                | _ ->
                                    textResult                             
                            return hoverText "" textResult

                        | :? OlyNamespaceSymbol as symbol ->
                            let textResult = sprintf "namespace %s" (subModel.GetSignatureText(symbol))
                            return hoverText "" textResult

                        | _ ->
                            return hoverText "" symbolInfo.SignatureText
                    | _ ->
                        return null
                })
            }

    interface ICompletionHandler with
        member this.GetRegistrationOptions(c: CompletionCapability, client: ClientCapabilities) =
            let options = CompletionRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.WorkDoneProgress <- true
            options.TriggerCharacters <- Container([".";",";":";" "])
            options.AllCommitCharacters <- null
            options.ResolveProvider <- true
            options

        member this.Handle(request, ct) =
            backgroundTask {
                try
                    let documentPath = request.TextDocument.Uri.Path |> normalizeFilePath
                    match textManager.TryGet(documentPath) with
                    | Some (sourceText, _) ->
                        let! docs = workspace.UpdateDocumentAsync(getSnapshot(), documentPath, sourceText, ct)
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
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct -> backgroundTask {
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

    interface IJsonRpcRequestHandler<OlyCompileActiveProjectRequest, OlyCompilationResult> with

        member _.Handle(request, ct) =
            backgroundTask {
                use _ = progress.CreateBuildProgress()

                let snapshot = getSnapshot()
                match! tryGetActiveProject snapshot workspace ct with
                | Some proj ->
                    server.ClearDiagnostics(Protocol.DocumentUri.From(proj.Path.ToString()))
                    match! workspace.BuildProjectAsync(snapshot, proj.Path, ct) with
                    | Ok prog -> 
                        prog.Run()
                        return { resultPath = prog.Path.ToString(); error = null }
                    | Error error -> 
                        let diags =
                            error
                            |> Seq.groupBy (fun x -> match x.SyntaxTree with Some syntaxTree -> syntaxTree.Path | _ -> proj.Path)

                        diags
                        |> Seq.iter (fun (docPath, diags) ->
                            let diags = 
                                diags 
                                |> Seq.map (fun x -> 
                                    let lspDiag = createDiagnostic x ct
                                    lspDiag
                                 )
                            server.PublishDiagnostics(Protocol.DocumentUri.From(docPath.ToString()), Nullable(), diags)
                        )

                        return { resultPath = null; error = OlyDiagnostic.PrepareForOutput(error, ct) }
                | _ ->
                    return { resultPath = null; error = "Active project not set" }
            }

    interface IJsonRpcRequestHandler<OlyGetIRRequest, string> with

        member _.Handle(request, ct) =
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct ->
                backgroundTask {
                    match doc.TryFindSymbol(request.Position.Line, request.Position.Character, ct) with
                    | None ->
                        return "Symbol not found."
                    | Some symbolInfo ->
                        let subModel = symbolInfo.SubModel
                        match symbolInfo.Symbol with
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
                                                subModel.GetSignatureText(nmspc) + "." + ty.Name
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
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun doc ct ->
                backgroundTask {
                    let mutable nextId = ref 1               
                    let nodes = doc.SyntaxTree.GetRoot(ct) |> getViewModel ct nextId
                    return { nodes = nodes }
                }
            )

    interface IJsonRpcRequestHandler<OlyGetProjectListRequest, string[]> with

        member _.Handle(_request, ct) =
            backgroundTask {
                ct.ThrowIfCancellationRequested()
                let! solution = workspace.GetSolutionAsync(getSnapshot(), ct)
                return 
                    solution.GetProjects() 
                    |> Seq.filter (fun x -> x.TargetInfo.IsExecutable) 
                    |> Seq.map (fun x -> x.Name) 
                    |> Seq.distinct 
                    |> Seq.sort
                    |> Array.ofSeq
            }

    interface IJsonRpcRequestHandler<OlyGetActiveProjectConfigurationListRequest, string[]> with

        member _.Handle(_request, ct) =
            backgroundTask {
                ct.ThrowIfCancellationRequested()
                let snapshot = getSnapshot()
                match! tryGetActiveProject snapshot workspace ct with
                | Some (proj) ->
                    let projConfigs = snapshot.GetAllProjectConfigurations(proj.Path)
                    return
                        projConfigs
                        |> Seq.map (fun x -> x.Name)
                        |> Seq.sort
                        |> Array.ofSeq
                | _ ->
                    return [||]
            }

    interface IJsonRpcRequestHandler<OlyTryGetActiveProjectConfigurationRequest, string> with

        member _.Handle(_request, ct) =
            backgroundTask {
                ct.ThrowIfCancellationRequested()
                let snapshot = getSnapshot()
                match! tryGetActiveProject snapshot workspace ct with
                | Some (proj) ->
                    return proj.Configuration.Name
                | _ ->
                    return null
            }

    interface IJsonRpcRequestHandler<OlyDoesProjectExistRequest, bool> with

        member _.Handle(request, ct) =
            backgroundTask {
                ct.ThrowIfCancellationRequested()
                let! solution = workspace.GetSolutionAsync(getSnapshot(), ct)
                return solution.GetProjects() |> ImArray.exists (fun x -> x.Name = request.ProjectName)
            }

    interface IJsonRpcRequestHandler<OlyDoesActiveProjectConfigurationExistRequest, bool> with

        member _.Handle(request, ct) =
            backgroundTask {
                ct.ThrowIfCancellationRequested()
                let snapshot = getSnapshot()
                match! tryGetActiveProject snapshot workspace ct with
                | Some (proj) ->
                    let projConfigs = snapshot.GetAllProjectConfigurations(proj.Path)
                    return
                        projConfigs
                        |> ImArray.exists (fun x -> x.Name = request.ConfigurationName)
                | _ ->
                    return false
            }

    interface IJsonRpcRequestHandler<OlyCleanWorkspaceRequest> with

        member _.Handle(_request, ct) =
            backgroundTask {
                ct.ThrowIfCancellationRequested()
                do! workspaceListener.CleanWorkspace() 
                return Unit()
            }

    interface IJsonRpcRequestHandler<OlyGetSemanticClassificationRequest, ParsedToken[]> with

        member _.Handle(request, ct) =
            request.HandleOlyDocument(progress, getSnapshot(), ct, getCts, workspace, fun (doc: OlyDocument) ct -> backgroundTask {
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

                let extraClassifications =
                    // TODO: We should do this in GetSemanticClassifications
                    let syntaxTree = doc.SyntaxTree
                    let sourceText = doc.GetSourceText(ct)
                    let lines = sourceText.Lines
                    match syntaxTree.TryFindNode(request.Range, ct) with
                    | Some node ->
                        node.GetDescendantTokens(false, (fun x -> x.IsConditionalDirective), ct)
                        |> Seq.map (fun x ->
                            let startLine = lines.GetLineFromPosition(x.TextSpan.Start)
                            let endLine = lines.GetLineFromPosition(x.TextSpan.End)
                            seq {
                                for lineIndex = startLine.LineIndex + 1 to endLine.LineIndex - 1 do
                                    let line = lines[lineIndex]
                                    let textRange = sourceText.GetTextRange(line.TextSpan)
                                    classify textRange.Start.Line textRange.Start.Column line.TextSpan.Width "conditionalDirectiveBody" Array.empty
                            }
                        )
                        |> Seq.concat
                        |> Array.ofSeq
                    | _ ->
                        Array.empty
            
                return Array.append classifications extraClassifications
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