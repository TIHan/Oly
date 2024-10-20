namespace rec Oly.Compiler.Workspace

open System
open System.Threading
open System.Threading.Tasks
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Core

/// TODO: This shouldn't be public, but it is to make Text.Json work.
type ActiveConfigurationState =
    {
        mutable activeConfiguration: string
    }

/// TODO: This shouldn't be public, but it is to make Text.Json work.
[<Sealed>]
type ProjectConfiguration =
    new : name: string * defines: string [] * debuggable: bool -> ProjectConfiguration
    member Name: string
    member Defines: string []
    member Debuggable: bool

/// TODO: This shouldn't be public, but it is to make Text.Json work.
[<Sealed>]
type ProjectConfigurations =
    new : configurations: ProjectConfiguration [] -> ProjectConfigurations

    member Configurations: ProjectConfiguration []

    static member Default: ProjectConfigurations

[<Sealed>]
type OlyProgram =

    new : path: OlyPath * run: (unit -> unit) -> OlyProgram

    member Path: OlyPath
    member Run: unit -> unit

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
type OlyCopyFileInfo =

    new : path: OlyPath * textSpan: OlyTextSpan -> OlyCopyFileInfo

    member Path: OlyPath
    member TextSpan: OlyTextSpan

[<Sealed>]
type OlyReferenceResolutionInfo =

    new : paths: OlyPath imarray * filesToCopy: OlyPath imarray * diags: OlyDiagnostic imarray -> OlyReferenceResolutionInfo 

    member Paths: OlyPath imarray
    member FilesToCopy: OlyPath imarray
    member Diagnostics: OlyDiagnostic imarray

[<RequireQualifiedAccess>]
type OlyOutputKind =
    | Library
    | Executable

[<Sealed>]
type OlyTargetInfo =

    new : name: string * outputKind: OlyOutputKind * implicitExtendsForStructOpt: string option * implicitExtendsForEnumOpt: string option -> OlyTargetInfo

    member Name: string
    member OutputKind: OlyOutputKind
    member IsExecutable: bool
    member ImplicitExtendsForStruct: string option
    member ImplicitExtendsForEnum: string option

[<Sealed>]
type OlyImportedReference =

    new : compRef: OlyCompilationReference * isTransitive: bool -> OlyImportedReference

    member CompilationReference: OlyCompilationReference
    member IsTransitive: bool

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

    abstract ImportReferenceAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * path: OlyPath * ct: CancellationToken -> Task<Result<OlyImportedReference option, string>>

    abstract OnBeforeReferencesImportedAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * ct: CancellationToken -> Task<unit>
    
    abstract OnAfterReferencesImported : unit -> unit

    abstract BuildProjectAsync : proj: OlyProject * ct: CancellationToken -> Task<Result<OlyProgram, OlyDiagnostic imarray>>

    abstract GetImplicitExtendsForStruct: unit -> string option
    default GetImplicitExtendsForStruct: unit -> string option

    abstract GetImplicitExtendsForEnum: unit -> string option
    default GetImplicitExtendsForEnum: unit -> string option

[<Sealed>]
type OlyProjectReference =

    member IsTransitive: bool

    static member Create : OlyCompilationReference -> OlyProjectReference

    static member CreateNonTransitive : OlyCompilationReference -> OlyProjectReference

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

    static member Deserialize : configName: string * System.IO.Stream -> OlyProjectConfiguration

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
    member CopyFileInfos: OlyCopyFileInfo imarray

    member TryGetDocument : documentPath: OlyPath -> OlyDocument option
    member GetDocumentsExcept : documentPath: OlyPath -> OlyDocument imarray
    member GetDiagnostics : ct: CancellationToken -> OlyDiagnostic imarray

[<Sealed>]
type OlySolution =

    member Version : uint64

    member HasDocument : documentPath: OlyPath -> bool

    member GetDocuments : documentPath: OlyPath -> OlyDocument imarray

    member GetAllDocuments : unit -> OlyDocument imarray

    member HasProject : projectPath: OlyPath -> bool

    member GetProject : projectPath: OlyPath -> OlyProject

    member TryGetProject : projectPath: OlyPath -> OlyProject option

    member GetProjects : unit -> OlyProject imarray

    member GetProjectsDependentOnReference : referencePath: OlyPath -> OlyProject imarray

    member CreateProject : projectPath: OlyPath * projectConfig: OlyProjectConfiguration * platformName: string * targetInfo: OlyTargetInfo * ct: CancellationToken -> OlySolution * OlyProject
    member CreateProject : projectPath: OlyPath * projectConfig: OlyProjectConfiguration * platformName: string * targetInfo: OlyTargetInfo * packages: OlyPackageInfo imarray * copyFileInfos: OlyCopyFileInfo imarray * ct: CancellationToken -> OlySolution * OlyProject

    member UpdateDocument : projectPath: OlyPath * documentPath: OlyPath * syntaxTree: OlySyntaxTree * extraDiagnostics: OlyDiagnostic imarray -> OlySolution * OlyProject * OlyDocument

    member RemoveDocument : projectPath: OlyPath * documentPath: OlyPath -> OlySolution

    member UpdateReferences : projectPath: OlyPath * projectReferences: OlyProjectReference imarray * ct: CancellationToken -> OlySolution * OlyProject

    member GetTransitiveProjectReferencesFromProject: projectPath: OlyPath * ct: CancellationToken -> OlyProject imarray

[<Sealed>]
type OlyWorkspaceResourceSnapshot =

    member SetResourceAsCopy : OlyPath -> OlyWorkspaceResourceSnapshot

    member SetResourceAsCopy : OlyPath * System.IO.Stream -> OlyWorkspaceResourceSnapshot

    member RemoveResource : OlyPath -> OlyWorkspaceResourceSnapshot

    member GetSourceText: filePath: OlyPath -> IOlySourceText

    /// Returns UTC time-stamp.
    member GetTimeStamp: filePath: OlyPath -> DateTime

    member FindSubPaths: dirPath: OlyPath -> OlyPath imarray

    member GetProjectConfiguration: projectFilePath: OlyPath -> OlyProjectConfiguration

    member GetActiveConfigurationName: unit -> string

    static member Create : activeConfigPath: OlyPath -> OlyWorkspaceResourceSnapshot

[<Sealed>]
type OlyWorkspace =

    member StaleSolution : OlySolution

    member CancelEverything : unit -> unit

    member GetSolutionAsync : OlyWorkspaceResourceSnapshot * ct: CancellationToken -> Task<OlySolution>

    /// Updates documents by path with the given source text.
    member UpdateDocumentAsync : OlyWorkspaceResourceSnapshot * documentPath: OlyPath * sourceText: IOlySourceText * ct: CancellationToken -> Task<OlyDocument imarray>

    /// Updates documents by path.
    /// Non-blocking.
    member UpdateDocuments : OlyWorkspaceResourceSnapshot * documentPaths: OlyPath imarray * ct: CancellationToken -> unit

    /// Updates documents by path with the given source text.
    /// Non-blocking.
    member UpdateDocument : OlyWorkspaceResourceSnapshot * documentPath: OlyPath * sourceText: IOlySourceText * ct: CancellationToken -> unit

    member RemoveProject : OlyWorkspaceResourceSnapshot * projectPath: OlyPath * ct: CancellationToken -> unit

    /// Get documents by path.
    member GetDocumentsAsync : OlyWorkspaceResourceSnapshot * documentPath: OlyPath * ct: CancellationToken -> Task<OlyDocument imarray>

    /// Get all the documents in the workspace's solution.
    member GetAllDocumentsAsync : OlyWorkspaceResourceSnapshot * ct: CancellationToken -> Task<OlyDocument imarray>

    /// TODO: We should make this API better.
    member BuildProjectAsync : OlyWorkspaceResourceSnapshot * projectPath: OlyPath * ct: CancellationToken -> Task<Result<OlyProgram, OlyDiagnostic imarray>>

    /// Clears the entire solution.
    /// Non-blocking.
    member ClearSolution : ct: CancellationToken -> unit

    static member Create : targets: OlyBuild seq -> OlyWorkspace
