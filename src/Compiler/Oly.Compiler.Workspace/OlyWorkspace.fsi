namespace rec Oly.Compiler.Workspace

open System
open System.Threading
open System.Threading.Tasks
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Core

[<Sealed>]
type OlyReferenceInfo =

    new : path: OlyPath * textSpan: OlyTextSpan -> OlyReferenceInfo

    member Path: OlyPath
    member TextSpan: OlyTextSpan

[<Sealed>]
type OlyPackageInfo =

    new : text: string * textSpan: OlyTextSpan -> OlyPackageInfo

    member Text: string
    member TextSpan: OlyTextSpan

[<Sealed>]
type OlyReferenceResolutionInfo =

    new : paths: OlyPath imarray * diags: OlyDiagnostic imarray -> OlyReferenceResolutionInfo 

    member Paths: OlyPath imarray
    member Diagnostics: OlyDiagnostic imarray

[<RequireQualifiedAccess>]
type OlyOutputKind =
    | Library
    | Executable

[<Sealed>]
type OlyTargetInfo =

    new : name: string * outputKind: OlyOutputKind -> OlyTargetInfo

    member Name: string
    member OutputKind: OlyOutputKind
    member IsExecutable: bool

[<AbstractClass>]
type OlyBuild =

    new : platformName: string -> OlyBuild

    member PlatformName : string

    /// The given path can be a directory or a file.
    /// Directory example: 'C:\work\' -> 'C:\work\.olycache\{platformName}\'
    /// File example: 'C:\work\CoolProject.project.oly' -> 'C:\work\.olycache\{platformName}\CoolProject.project.oly\'
    member GetAbsoluteCacheDirectory : absolutePath: OlyPath -> OlyPath

    /// The given path can be a directory or a file.
    /// Directory example: 'C:\work\' -> 'C:\work\bin\{platformName}\'
    /// File example: 'C:\work\CoolProject.project.oly' -> 'C:\work\bin\{platformName}\CoolProject.project.oly\'
    member GetAbsoluteBinDirectory : absolutePath: OlyPath -> OlyPath

    abstract IsValidTargetName : targetInfo: OlyTargetInfo -> bool

    abstract ResolveReferencesAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * referenceInfos: OlyReferenceInfo imarray * packageInfos: OlyPackageInfo imarray * ct: CancellationToken -> Task<OlyReferenceResolutionInfo>

    abstract CanImportReference : path: OlyPath -> bool

    abstract ImportReferenceAsync : targetInfo: OlyTargetInfo * path: OlyPath * ct: CancellationToken -> Task<Result<OlyCompilationReference option, string>>

    abstract OnBeforeReferencesImported : unit -> unit
    
    abstract OnAfterReferencesImported : unit -> unit

    abstract BuildProjectAsync : proj: OlyProject * ct: CancellationToken -> Task<Result<string, string>>

[<Sealed>]
type OlyProjectReference =

    static member Create : OlyCompilationReference -> OlyProjectReference

[<Sealed>]
type OlyDocument =

    member Path : OlyPath
    member Project : OlyProject
    member SyntaxTree : OlySyntaxTree
    member BoundModel : OlyBoundModel
    member IsProjectDocument : bool
    member ExtraDiagnostics : OlyDiagnostic imarray
    member GetSourceText : CancellationToken -> IOlySourceText
    member GetDiagnostics : CancellationToken -> OlyDiagnostic imarray

[<Sealed>]
type OlyProjectConfiguration =

    new : name: string * defines: string imarray * debuggable: bool -> OlyProjectConfiguration

    member Name : string

    member Defines : string imarray

    member Debuggable : bool

[<Sealed>]
type OlyProject =

    member Path : OlyPath
    member Name : string
    member Configuration : OlyProjectConfiguration
    member Solution : OlySolution
    member Compilation : OlyCompilation
    member Documents : OlyDocument imarray
    member References : OlyProjectReference imarray
    member TargetInfo : OlyTargetInfo
    member SharedBuild : OlyBuild

    member TryGetDocument : documentPath: OlyPath -> OlyDocument option
    member GetDocumentsExcept : documentPath: OlyPath -> OlyDocument imarray

[<Sealed>]
type OlySolution =

    member Version : uint64

    member HasDocument : documentPath: OlyPath -> bool

    member GetDocuments : documentPath: OlyPath -> OlyDocument imarray

    member HasProject : projectPath: OlyPath -> bool

    member GetProject : projectPath: OlyPath -> OlyProject

    member TryGetProject : projectPath: OlyPath -> OlyProject option

    member GetProjects : unit -> OlyProject imarray

    member GetProjectsDependentOnReference : referencePath: OlyPath -> OlyProject imarray

    member CreateProject : projectPath: OlyPath * projectConfig: OlyProjectConfiguration * platformName: string * targetInfo: OlyTargetInfo * ct: CancellationToken -> OlySolution * OlyProject
    member CreateProject : projectPath: OlyPath * projectConfig: OlyProjectConfiguration * platformName: string * targetInfo: OlyTargetInfo * packages: OlyPackageInfo imarray * ct: CancellationToken -> OlySolution * OlyProject

    member UpdateDocument : projectPath: OlyPath * documentPath: OlyPath * syntaxTree: OlySyntaxTree * extraDiagnostics: OlyDiagnostic imarray -> OlySolution * OlyProject * OlyDocument

    member RemoveDocument : projectPath: OlyPath * documentPath: OlyPath -> OlySolution

    member UpdateReferences : projectPath: OlyPath * projectReferences: OlyProjectReference imarray * ct: CancellationToken -> OlySolution * OlyProject

type IOlyWorkspaceResourceService =

    abstract LoadSourceText: filePath: OlyPath -> IOlySourceText

    /// Returns UTC time-stamp.
    abstract GetTimeStamp: filePath: OlyPath -> DateTime

    abstract FindSubPaths: dirPath: OlyPath -> OlyPath imarray

    abstract LoadProjectConfigurationAsync: projectConfigPath: OlyPath * ct: CancellationToken -> Task<OlyProjectConfiguration>

[<Sealed>]
type OlyDefaultWorkspaceResourceService =

    new: unit -> OlyDefaultWorkspaceResourceService

    interface IOlyWorkspaceResourceService

[<Sealed>]
type OlyWorkspace =

    /// TODO: Remove this API.
    member Solution : OlySolution

    /// Updates documents by path with the given source text.
    member UpdateDocumentAsync : documentPath: OlyPath * sourceText: IOlySourceText * ct: CancellationToken -> Task<OlyDocument imarray>

    /// Updates documents by path with the given source text.
    /// Non-blocking.
    member UpdateDocument : documentPath: OlyPath * sourceText: IOlySourceText * ct: CancellationToken -> unit

    member InvalidateProject : projectPath: OlyPath * ct: CancellationToken -> unit

    /// Get documents by path.
    member GetDocumentsAsync : documentPath: OlyPath * ct: CancellationToken -> Task<OlyDocument imarray>

    /// We should make this better.
    member BuildProjectAsync : projectPath: OlyPath * ct: CancellationToken -> Task<Result<string, string>>

    /// Clears the entire solution.
    member ClearSolutionAsync : ct: CancellationToken -> Task<unit>

    static member Create : targets: OlyBuild seq * ?rs: IOlyWorkspaceResourceService -> OlyWorkspace
