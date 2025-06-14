﻿namespace rec Oly.Compiler.Workspace

open Oly.Core
open Oly.Core.TaskExtensions
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open System
open System.IO
open System.IO.MemoryMappedFiles
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

[<Sealed>]
type OlyProgram(path: OlyPath, run: unit -> unit) =

    member _.Path = path
    member _.Run() = run()

[<AutoOpen>]
module Helpers =

    [<Literal>]
    let ProjectExtension = ".olyx"

    [<Literal>]
    let CacheDirectoryName = ".olycache"

    [<Literal>]
    let BinDirectoryName = "bin"

    [<Literal>]
    let ProjectConfigurationExtension = ".json"

    let inline internal getInlineCache (valueCache: byref<'T voption>) (f: unit -> 'T) =
        match valueCache with
        | ValueSome value -> value
        | _ ->
            let value = f ()
            valueCache <- ValueSome value
            value

[<Sealed>]
[<System.Diagnostics.DebuggerDisplay("{Path}")>]
type OlyReferenceInfo(path: OlyPath, textSpan: OlyTextSpan) =

    member _.Path = path
    member _.TextSpan = textSpan

[<Sealed>]
[<System.Diagnostics.DebuggerDisplay("{Text}")>]
type OlyPackageInfo(text: string, textSpan: OlyTextSpan) =

    member _.Text = text
    member _.TextSpan = textSpan

[<Sealed>]
[<System.Diagnostics.DebuggerDisplay("{Path}")>]
type OlyCopyFileInfo(path: OlyPath, textSpan: OlyTextSpan) =

    member _.Path = path
    member _.TextSpan = textSpan

[<Sealed>]
type OlyReferenceResolutionInfo(paths: OlyPath imarray, filesToCopy: OlyPath imarray, diags: OlyDiagnostic imarray) = 

    member _.Paths = paths
    member _.FilesToCopy = filesToCopy
    member _.Diagnostics = diags

[<RequireQualifiedAccess>]
type OlyOutputKind =
    | Library
    | Executable

[<Sealed>]
type OlyTargetInfo(name: string, projConfig: OlyProjectConfiguration, outputKind: OlyOutputKind, implicitExtendsForStructOpt: string option, implicitExtendsForEnumOpt: string option) =

    member _.Name = name
    member _.ProjectConfiguration = projConfig
    member _.OutputKind = outputKind
    member _.IsExecutable = outputKind = OlyOutputKind.Executable
    member _.ImplicitExtendsForStruct = implicitExtendsForStructOpt
    member _.ImplicitExtendsForEnum = implicitExtendsForEnumOpt

[<Sealed>]
type OlyImportedReference(compRef: OlyCompilationReference, isTransitive: bool) =
    
    member _.CompilationReference = compRef
    member _.IsTransitive = isTransitive

[<AbstractClass>]
type OlyBuild(platformName: string) =

    member _.PlatformName = platformName

    member _.GetProjectCacheDirectory(configName: string, projectPath: OlyPath) =
        if projectPath.IsFile then
            let fileName = Path.GetFileNameWithoutExtension(OlyPath.GetFileName(projectPath))
            let dir = OlyPath.GetDirectory(projectPath)
            OlyPath.Combine(dir, OlyPath.Create($"{CacheDirectoryName}/{fileName}/{platformName}/{configName}/"))
        else
            invalidOp "Expected file"

    member _.GetProjectBinDirectory(configName: string, projectPath: OlyPath) =
        if projectPath.IsFile then
            let fileName = Path.GetFileNameWithoutExtension(OlyPath.GetFileName(projectPath))
            let dir = OlyPath.GetDirectory(projectPath)
            OlyPath.Combine(dir, OlyPath.Create($"{BinDirectoryName}/{fileName}/{platformName}/{configName}/"))
        else
            invalidOp "Expected file"

    member _.GetProjectConfigurationPath(projectPath: OlyPath) =
        if projectPath.IsFile then
            OlyPath.ChangeExtension(projectPath, ".json")
        else
            invalidOp "Expected file"

    abstract IsValidTargetName : targetInfo: OlyTargetInfo -> bool

    abstract ResolveReferencesAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * referenceInfos: OlyReferenceInfo imarray * packageInfos: OlyPackageInfo imarray * ct: CancellationToken -> Task<OlyReferenceResolutionInfo>

    abstract CanImportReference : path: OlyPath -> bool

    abstract ImportReferenceAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * path: OlyPath * ct: CancellationToken -> Task<Result<OlyImportedReference option, string>>

    abstract OnBeforeReferencesImportedAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * ct: CancellationToken -> Task<unit>
    
    abstract OnAfterReferencesImported : unit -> unit

    abstract BuildProjectAsync : proj: OlyProject * ct: CancellationToken -> Task<Result<OlyProgram, OlyDiagnostic imarray>>

    abstract GetImplicitExtendsForStruct: unit -> string option
    default _.GetImplicitExtendsForStruct() = None

    abstract GetImplicitExtendsForEnum: unit -> string option
    default _.GetImplicitExtendsForEnum() = None

    abstract GetAnalyzerDiagnostics : targetInfo: OlyTargetInfo * boundModel: OlyBoundModel * ct: CancellationToken -> OlyDiagnostic imarray
    default _.GetAnalyzerDiagnostics(_, _, _) = ImArray.empty

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type OlyProjectReference =
    | Compilation of OlyCompilationReference * disableTransitive: bool
    | Project of projectPath: OlyPath

    member this.IsTransitive =
        match this with
        | Compilation(disableTransitive=true) -> false
        | _ -> true

    member this.Path =
        match this with
        | Compilation(r, _) -> r.Path
        | Project(path) -> path

    static member Create(compilationReference) = Compilation(compilationReference, false)
    static member CreateNonTransitive(compilationReference) = Compilation(compilationReference, true)

[<Sealed>]
type OlyDocument(newProjectLazy: OlyProject Lazy, documentPath: OlyPath, syntaxTree: OlySyntaxTree, extraDiagnostics: OlyDiagnostic imarray) =

    let mutable boundModel = ValueNone
    
    member _.Path = documentPath

    member _.Project : OlyProject = newProjectLazy.Value

    member _.SyntaxTree = syntaxTree

    member _.GetSourceText(ct) = syntaxTree.GetSourceText(ct)

    member this.BoundModel =
        getInlineCache &boundModel (fun () ->
            this.Project.Compilation.GetBoundModel(documentPath)
        )

    member this.ExtraDiagnostics = extraDiagnostics

    member this.GetDiagnostics(ct) =
        let diags1 = syntaxTree.GetDiagnostics(ct)
        let diags2 = this.BoundModel.GetDiagnostics(ct)
        diags1.AddRange(diags2).AddRange(this.ExtraDiagnostics)

    member this.IsProjectDocument =
        documentPath.HasExtension(ProjectExtension)

    member this.GetAnalyzerDiagnostics(ct) =
        let project = this.Project
        let boundModel = this.BoundModel
        project.SharedBuild.GetAnalyzerDiagnostics(this.Project.TargetInfo, boundModel, ct)

type ActiveConfigurationState =
    {
        mutable activeConfiguration: string
    }

[<Sealed>]
type ProjectConfiguration [<JsonConstructor>] (name: string, defines: string [], debuggable: bool) =
    member _.Name = name
    member _.Defines = defines
    member _.Debuggable = debuggable

[<Sealed>]
type ProjectConfigurations [<JsonConstructor>] (configurations: ProjectConfiguration []) =
    static let defaultConfig =
        ProjectConfigurations(
            [|
                ProjectConfiguration("Debug", [|"DEBUG"|], true)
                ProjectConfiguration("Release", [|"RELEASE"|], false)
            |]
        )

    let configurations =
        if configurations.Length = 0 then
            defaultConfig.Configurations // If configurations are empty, use default configurations.
        else
            configurations

    static member Default = defaultConfig

    member _.Configurations = configurations

    static member Deserialize(stream: System.IO.Stream): ProjectConfigurations =
        let jsonOptions = JsonSerializerOptions()
        jsonOptions.PropertyNameCaseInsensitive <- true
        JsonSerializer.Deserialize<ProjectConfigurations>(stream, jsonOptions)

    member _.GetConfiguration(configName: string): OlyProjectConfiguration =
        let configOpt =
            configurations
            |> Array.tryFind (fun x -> (not(String.IsNullOrEmpty(x.Name))) && x.Name.Equals(configName, StringComparison.OrdinalIgnoreCase))

        let config =
            match configOpt with
            | None ->
                match configurations |> Array.tryHead with
                | Some config -> config
                | _ -> defaultConfig.Configurations[1] // Default to Release config
            | Some config ->
                config

        let name = config.Name
        let conditionalDefines = config.Defines |> ImArray.ofSeq
        let isDebuggable = config.Debuggable
        OlyProjectConfiguration(name, conditionalDefines, isDebuggable)

[<Sealed>]
type OlyProjectConfiguration(name: string, defines: string imarray, debuggable: bool) =

    member _.Name = name

    member _.Defines = defines

    member _.Debuggable = debuggable

[<Sealed>]
[<System.Diagnostics.DebuggerDisplay("{Path}")>]
type OlyProject (
    solution: OlySolution Lazy, 
    projPath: OlyPath,
    projName: string,
    projConfig: OlyProjectConfiguration,
    compilationOptions: OlyCompilationOptions,
    compilation: OlyCompilation CacheValue, 
    documents: ImmutableDictionary<OlyPath, OlyDocument>, 
    references: OlyProjectReference imarray,
    packages: OlyPackageInfo imarray,
    copyFileInfos: OlyCopyFileInfo imarray,
    platformName: string, 
    targetInfo: OlyTargetInfo) =

    let mutable documentList = ValueNone

    member _.DocumentLookup = documents
    member _.PlatformName = platformName
    member _.TargetInfo = targetInfo
    member _.SharedBuild : OlyBuild =
        solution.Value.State.workspace.GetBuild(projPath)
    
    member _.Solution = solution.Value
    member _.CompilationOptions = compilationOptions
    member _.CompilationLazy = compilation
    member _.Compilation: OlyCompilation = compilation.GetValue(CancellationToken.None)
    member _.Documents: OlyDocument imarray =
        getInlineCache &documentList (fun () ->
            documents.Values.ToImmutableArray()
        )
    member _.References = references
    member _.Name = projName
    member _.Path = projPath
    member _.Configuration = projConfig
    member _.Packages = packages
    member _.CopyFileInfos = copyFileInfos

    member this.InvalidateReferences(newSolutionLazy) =
#if DEBUG || CHECKED
        OlyTrace.Log($"OlyWorkspace - Invalidating Project References - {projPath.ToString()}")
#endif
        this.UpdateReferences(newSolutionLazy, this.References, CancellationToken.None)

    member val AsCompilationReference = OlyCompilationReference.Create(projPath, CacheValue(fun ct -> compilation.GetValue(ct)))

    member this.TryGetDocument(documentPath: OlyPath) =
        match documents.TryGetValue documentPath with
        | true, document -> Some document
        | _ -> None

    member this.GetDocument(documentPath: OlyPath) =
        match this.TryGetDocument(documentPath) with
        | None -> failwithf "Unable to find document '%A'." documentPath
        | Some document -> document

    member this.GetDocumentsExcept(documentPath: OlyPath) =
        this.Documents
        |> ImArray.filter (fun doc -> not(OlyPath.Equals(doc.Path, documentPath)))

    member this.UpdateDocument(newSolutionLazy, documentPath: OlyPath, syntaxTree: OlySyntaxTree, extraDiagnostics) =
        let mutable newProject = this
        let newProjectLazy = lazy newProject
        let newDocument = OlyDocument(newProjectLazy, documentPath, syntaxTree, extraDiagnostics)
        let newCompilation = 
            if compilation.HasValue then
                CacheValue.FromValue(compilation.GetValue(CancellationToken.None).SetSyntaxTree(newDocument.SyntaxTree))
            else
                CacheValue(fun ct ->
                    compilation.GetValue(ct).SetSyntaxTree(newDocument.SyntaxTree)
                )

        let newDocuments = 
            documents.SetItem(documentPath, newDocument).Values
            |> Seq.map (fun document ->
                KeyValuePair(document.Path, OlyDocument(newProjectLazy, document.Path, document.SyntaxTree, document.ExtraDiagnostics))
            )
            |> ImmutableDictionary.CreateRange

        newProject <- OlyProject(newSolutionLazy, projPath, projName, projConfig, compilationOptions, newCompilation, newDocuments, references, packages, copyFileInfos, platformName, targetInfo)
        newProjectLazy.Force() |> ignore
        newProject, newDocument

    member this.RemoveDocument(newSolutionLazy, documentPath: OlyPath) =
        if not (documents.ContainsKey(documentPath)) then
            failwithf "Unable to find document '%A'." documentPath

        let document = documents.[documentPath]

        let mutable newProject = this
        let newProjectLazy = lazy newProject
        let newCompilation = 
            CacheValue(fun ct ->
                compilation.GetValue(ct).RemoveSyntaxTree(document.SyntaxTree.Path)
            )
        let newDocuments = 
            documents.Remove(document.Path).Values
            |> Seq.map (fun document ->
                KeyValuePair(document.Path, OlyDocument(newProjectLazy, document.Path, document.SyntaxTree, document.ExtraDiagnostics))
            )
            |> ImmutableDictionary.CreateRange

        newProject <- OlyProject(newSolutionLazy, projPath, projName, projConfig, compilationOptions, newCompilation, newDocuments, references, packages, copyFileInfos, platformName, targetInfo)
        newProjectLazy.Force() |> ignore
        newProject

    member this.UpdateReferences(newSolutionLazy: Lazy<OlySolution>, projectReferences: OlyProjectReference imarray, ct) =
        let mutable newProject = this
        let newProjectLazy = lazy newProject
        let newCompilation = 
            CacheValue(fun ct ->
                OlyAssert.True(newSolutionLazy.IsValueCreated)
                let solution = newSolutionLazy.Value
                let transitiveReferences = getTransitiveCompilationReferences solution projectReferences ct
                compilation.GetValue(ct).Update(references = transitiveReferences)
            )

        let newDocuments = 
            documents.Values
            |> Seq.map (fun document ->
                KeyValuePair(document.Path, OlyDocument(newProjectLazy, document.Path, document.SyntaxTree, document.ExtraDiagnostics))
            )
            |> ImmutableDictionary.CreateRange

        newProject <- OlyProject(newSolutionLazy, projPath, projName, projConfig, compilationOptions, newCompilation, newDocuments, projectReferences, packages, copyFileInfos, platformName, targetInfo)
        newProjectLazy.Force() |> ignore
        newProject

    member this.GetDiagnostics(ct: CancellationToken) : OlyDiagnostic imarray =
        let builder = ImArray.builder()
        builder.AddRange(this.Compilation.GetDiagnostics(ct))
        this.Documents
        |> ImArray.iter (fun doc -> builder.AddRange(doc.ExtraDiagnostics))
        OlyAssert.True(solution.IsValueCreated)
        let projs = getTransitiveProjectReferences solution.Value this.References ct
        projs
        |> ImArray.iter (fun (proj: OlyProject) ->
            proj.Documents
            |> ImArray.iter (fun doc ->
                builder.AddRange(doc.ExtraDiagnostics)
            )
        )
        builder.ToImmutable()

    member this.GetAnalyzerDiagnostics(ct: CancellationToken) : OlyDiagnostic imarray =
        let builder = ImArray.builder()
        this.Documents
        |> ImArray.iter (fun doc -> builder.AddRange(doc.GetAnalyzerDiagnostics(ct)))
        OlyAssert.True(solution.IsValueCreated)
        let projs = getTransitiveProjectReferences solution.Value this.References ct
        projs
        |> ImArray.iter (fun (proj: OlyProject) ->
            proj.Documents
            |> ImArray.iter (fun doc ->
                builder.AddRange(doc.GetAnalyzerDiagnostics(ct))
            )
        )
        builder.ToImmutable()


[<NoEquality;NoComparison>]
type ProjectChanged =
    {
        References: OlyCompilationReference imarray
        Diagnostics: OlyDiagnostic imarray
        FixupProjects: OlyProject imarray
    }

[<AutoOpen>]
module WorkspaceHelpers =

    let getTransitiveCompilationReferences (solution: OlySolution) references (ct: CancellationToken) =
        let transitiveReferences =
            let h = HashSet<string>(StringComparer.OrdinalIgnoreCase)
            let builder = imarray.CreateBuilder()
            let rec loop checkTransitive (references: OlyProjectReference imarray) =
                references
                |> ImArray.iter (fun r ->
                    ct.ThrowIfCancellationRequested()
                    if not(checkTransitive) || r.IsTransitive then
                        if h.Add(r.Path.ToString()) then
                            match r with
                            | OlyProjectReference.Project(projectId) ->
                                match solution.TryGetProject projectId with
                                | Some refProj -> 
                                    let compRef = refProj.AsCompilationReference
                                    builder.Add(compRef)
                                    compRef.TryGetCompilation(ct) |> ignore // force evaluate to get rid of project+solution inside the CacheValue
                                    loop true refProj.References
                                | _ -> 
                                    invalidOp "Unable to find project."
                            | OlyProjectReference.Compilation(r, _) ->
                                builder.Add(r)
                )
            loop false references
            builder.ToImmutable()
        transitiveReferences

    let getTransitiveProjectReferences (solution: OlySolution) (references: OlyProjectReference imarray) (ct: CancellationToken) =
        let transitiveReferences =
            let h = HashSet<string>(StringComparer.OrdinalIgnoreCase)
            let builder = imarray.CreateBuilder()
            let rec loop checkTransitive (references: OlyProjectReference imarray) =
                references
                |> ImArray.iter (fun r ->
                    ct.ThrowIfCancellationRequested()
                    if not(checkTransitive) || r.IsTransitive then
                        match r with
                        | OlyProjectReference.Project(projectId) ->
                            if h.Add(projectId.ToString()) then
                                match solution.TryGetProject projectId with
                                | Some refProj -> 
                                    builder.Add(refProj)
                                    loop true refProj.References
                                | _ -> 
                                    OlyAssert.Fail("Unable to find project.")
                        | OlyProjectReference.Compilation _ ->
                            ()
                )
            loop false references
            builder.ToImmutable()
        transitiveReferences

    let getTransitivePackages (solution: OlySolution) references (ct: CancellationToken) =
        let transitiveReferences =
            let h = HashSet<string>(StringComparer.OrdinalIgnoreCase)
            let builder = imarray.CreateBuilder()
            let rec loop (references: OlyProjectReference imarray) =
                references
                |> ImArray.iter (fun r ->
                    ct.ThrowIfCancellationRequested()
                    match r with
                    | OlyProjectReference.Project(projectId) ->
                        if h.Add(projectId.ToString()) then
                            match solution.TryGetProject projectId with
                            | Some refProj ->
                                builder.AddRange(refProj.Packages)
                                loop refProj.References
                            | _ -> 
                                OlyAssert.Fail("Unable to find project.")
                    | OlyProjectReference.Compilation _ ->
                        ()
                )
            loop references
            builder.ToImmutable()
        transitiveReferences
        |> Seq.distinctBy (fun x -> x.Text)
        |> ImArray.ofSeq

    let getSyntaxTrees (documents: OlyDocument imarray) =
        documents
        |> ImArray.map (fun x -> x.SyntaxTree)

    let getReferenceDirectives (syntaxTree: OlySyntaxTree) ct =
        syntaxTree.GetCompilationUnitConfiguration(ct).References
        |> ImArray.map (fun (textSpan, referencePath) ->
            let dir = OlyPath.GetDirectory(syntaxTree.Path)
            let newReferencePath =
                OlyPath.Combine(dir, referencePath.ToString())
            (textSpan, newReferencePath)
        )
    
    let createProject 
            (newSolution: OlySolution Lazy)
            projectId 
            projectName 
            (projectConfig: OlyProjectConfiguration) 
            (documents: OlyDocument imarray) 
            (projectReferences: OlyProjectReference imarray) 
            (packages: OlyPackageInfo imarray)
            (copyFiles: OlyCopyFileInfo imarray)
            platformName 
            (targetInfo: OlyTargetInfo) =

        let isDebuggable = projectConfig.Debuggable

        let syntaxTrees = getSyntaxTrees documents
        let options = { Debuggable = isDebuggable; Parallel = true; Executable = targetInfo.IsExecutable; ImplicitExtendsForStruct = targetInfo.ImplicitExtendsForStruct; ImplicitExtendsForEnum = targetInfo.ImplicitExtendsForEnum }
        let compilation = 
            CacheValue(fun ct ->
                OlyAssert.True(newSolution.IsValueCreated)
                let solution = newSolution.Value
                let transitiveReferences = getTransitiveCompilationReferences solution projectReferences ct
                OlyCompilation.Create(projectName, syntaxTrees, references = transitiveReferences, options = options)
            )
        let documents =
            documents
            |> ImArray.map (fun x -> KeyValuePair(x.Path, x))
            |> ImmutableDictionary.CreateRange

        OlyProject(newSolution, projectId, projectName, projectConfig, options, compilation, documents, projectReferences, packages, copyFiles, platformName, targetInfo)   

    let updateProject (newSolutionLazy: OlySolution Lazy) (project: OlyProject) =
        OlyAssert.False(newSolutionLazy.IsValueCreated)
        let mutable project = project
        let newProjectLazy = lazy project
        let newDocuments = 
            project.DocumentLookup.Values
            |> Seq.map (fun document ->
                KeyValuePair(document.Path, OlyDocument(newProjectLazy, document.Path, document.SyntaxTree, document.ExtraDiagnostics))
            )
            |> ImmutableDictionary.CreateRange

        let options = project.CompilationOptions
        let compilationLazy =
            if project.CompilationLazy.HasValue then
                project.CompilationLazy
            else
                let syntaxTrees = getSyntaxTrees project.Documents
                CacheValue(fun ct ->
                    OlyAssert.True(newSolutionLazy.IsValueCreated)
                    OlyAssert.True(newProjectLazy.IsValueCreated)
                    let solution = newSolutionLazy.Value
                    let project = newProjectLazy.Value
                    let transitiveReferences = getTransitiveCompilationReferences solution project.References ct
                    OlyCompilation.Create(project.Name, syntaxTrees, references = transitiveReferences, options = options)
                )

        project <- OlyProject(newSolutionLazy, project.Path, project.Name, project.Configuration, options, compilationLazy, newDocuments, project.References, project.Packages, project.CopyFileInfos, project.PlatformName, project.TargetInfo)
        newProjectLazy.Force() |> ignore
        project

    let updateSolution (newSolution: OlySolution) (newSolutionLazy: Lazy<OlySolution>) =
        OlyAssert.False(newSolutionLazy.IsValueCreated)
        let newProjects =
            newSolution.State.projects.Values
            |> Seq.map (fun x ->
                KeyValuePair(x.Path, updateProject newSolutionLazy x)
            )
            |> ImmutableDictionary.CreateRange

        { newSolution.State with
            projects = newProjects
        }
        |> OlySolution

[<NoComparison;NoEquality>]
type SolutionState =
    {
        workspace: OlyWorkspace
        projects: ImmutableDictionary<OlyPath, OlyProject>
        version: uint64
    }

[<Sealed>]
type OlySolution (state: SolutionState) =

    let state = { state with version = state.version + 1UL }

    static member InvalidateDependentProjectsOnCore(newSolution: byref<OlySolution>, newSolutionLazy: Lazy<OlySolution>, projectPath: OlyPath): unit =
        let projectsToInvalidate = 
            newSolution.GetProjectsDependentOnReference(projectPath)
            |> ImArray.map (fun x -> x.Path)

        let newProjects =
            (newSolution.State.projects, projectsToInvalidate)
            ||> ImArray.fold (fun projects x -> 
                projects.SetItem(x, projects[x].InvalidateReferences(newSolutionLazy))
            )
        newSolution <- { newSolution.State with projects = newProjects } |> OlySolution

    member this.State = state

    member this.Version = state.version

    member this.HasProject(projectPath: OlyPath): bool =
        state.projects.ContainsKey(projectPath)

    member this.TryGetProject(projectPath: OlyPath): OlyProject option =
        match state.projects.TryGetValue projectPath with
        | true, project -> Some project
        | _ -> None

    member this.GetProject(projectPath: OlyPath) =
        match this.TryGetProject(projectPath) with
        | None -> failwith $"Unable to find project '{projectPath}'. Check if the project is updated in the workspace."
        | Some project -> project

    member this.TryGetProjectByName(projectName: string) =
        state.projects.Values
        |> Seq.tryFind (fun x -> x.Name = projectName)

    member this.GetProjects() =
        state.projects.Values |> ImArray.ofSeq

    member this.GetProjectsDependentOnReference(referencePath: OlyPath) : OlyProject imarray =
        state.projects.Values
        |> Seq.filter (fun x ->
            x.Compilation.References
            |> ImArray.exists (fun x -> OlyPath.Equals(x.Path, referencePath))
        )
        |> ImArray.ofSeq

    member this.HasDocument(documentPath: OlyPath) =
        state.projects.Values
        |> Seq.exists (fun proj ->
            proj.DocumentLookup.ContainsKey(documentPath)
        )

    member this.GetDocuments(documentPath: OlyPath) =
        state.projects.Values
        |> Seq.choose (fun proj ->
            proj.TryGetDocument(documentPath)
        )
        |> ImArray.ofSeq

    member this.GetAllDocuments() =
        state.projects.Values
        |> Seq.map (fun proj -> proj.Documents)
        |> Seq.concat
        |> ImArray.ofSeq

    member this.CreateProject(projectPath, projectConfig, platformName, targetInfo, packages, copyFileInfos, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        let projectName = OlyPath.GetFileNameWithoutExtension(projectPath)
        let mutable newSolution = this
        let newSolutionLazy = lazy newSolution
        let newProject = createProject newSolutionLazy projectPath projectName projectConfig ImArray.empty ImArray.empty packages copyFileInfos platformName targetInfo
        let newProjects = newSolution.State.projects.SetItem(newProject.Path, newProject)
        newSolution <- { newSolution.State with projects = newProjects } |> OlySolution
        newSolution <- updateSolution newSolution newSolutionLazy
        newSolutionLazy.Force() |> ignore
        newSolution, newProject

    member this.CreateProject(projectPath, projectConfig, platformName, targetInfo, ct: CancellationToken) =
        this.CreateProject(projectPath, projectConfig, platformName, targetInfo, ImArray.empty, ImArray.empty, ct)

    member this.UpdateDocument(projectPath: OlyPath, documentPath, syntaxTree: OlySyntaxTree, extraDiagnostics: OlyDiagnostic imarray) =
        let project = this.GetProject(projectPath)
        let mutable newSolution = this
        let newSolutionLazy = lazy newSolution
        OlySolution.InvalidateDependentProjectsOnCore(&newSolution, newSolutionLazy, projectPath)
        let newProject, newDocument = project.UpdateDocument(newSolutionLazy, documentPath, syntaxTree, extraDiagnostics)
        let newProjects = newSolution.State.projects.SetItem(newProject.Path, newProject)
        newSolution <- { newSolution.State with projects = newProjects } |> OlySolution
        newSolution <- updateSolution newSolution newSolutionLazy
        newSolutionLazy.Force() |> ignore

#if DEBUG || CHECKED
        OlyAssert.Equal(newProject, newDocument.Project)
        OlyAssert.Equal(newSolution, newDocument.Project.Solution)

        let checkProj (newProject: OlyProject) =
            OlyAssert.Equal(newSolution, newProject.Solution)
            newProject.Documents
            |> ImArray.iter (fun doc ->
                OlyAssert.Equal(newProject, doc.Project)
                OlyAssert.Equal(newSolution, doc.Project.Solution)
            )
        checkProj newProject
        newSolution.GetProjects()
        |> ImArray.iter checkProj
#endif

        newSolution, newProject, newDocument

    member this.RemoveDocument(projectPath, documentPath) =
        if OlyPath.Equals(projectPath, documentPath) then
            this.RemoveProject(projectPath)
        else
            let project = this.GetProject(projectPath)
            let mutable newSolution = this
            let newSolutionLazy = lazy newSolution
            OlySolution.InvalidateDependentProjectsOnCore(&newSolution, newSolutionLazy, projectPath)
            let newProject = project.RemoveDocument(newSolutionLazy, documentPath)
            let newProjects = newSolution.State.projects.SetItem(newProject.Path, newProject)
            newSolution <- { newSolution.State with projects = newProjects } |> OlySolution
            newSolution <- updateSolution newSolution newSolutionLazy
            newSolutionLazy.Force() |> ignore
            newSolution

    member this.RemoveProject(projectPath) =
        match this.TryGetProject(projectPath) with
        | Some project ->
            let mutable newSolution = this
            let newSolutionLazy = lazy newSolution
            let projectsToRemove = 
                newSolution.GetProjectsDependentOnReference(projectPath).Add(project)
                |> ImArray.map (fun x -> x.Path)
            let newProjects = newSolution.State.projects.RemoveRange(projectsToRemove)
            newSolution <- { newSolution.State with projects = newProjects } |> OlySolution
            newSolution <- updateSolution newSolution newSolutionLazy
            newSolutionLazy.Force() |> ignore
            newSolution
        | _ ->
            this

    member this.UpdateReferences(projectPath, projectReferences: OlyProjectReference imarray, ct) =
#if DEBUG || CHECKED
        OlyTrace.Log($"OlyWorkspace - Updating References For Project - {projectPath.ToString()}")
#endif
        let project = this.GetProject(projectPath)
        let mutable newSolution = this
        let newSolutionLazy = lazy newSolution
        let newProject = project.UpdateReferences(newSolutionLazy, projectReferences, ct)
        let newProjects = state.projects.SetItem(newProject.Path, newProject)
        newSolution <- { state with projects = newProjects } |> OlySolution
        newSolution <- updateSolution newSolution newSolutionLazy
        newSolutionLazy.Force() |> ignore
        newSolution, newProject

    member this.GetTransitiveProjectReferencesFromProject(projectPath, ct) =
        let project = this.GetProject(projectPath)
        getTransitiveProjectReferences this project.References ct


[<NoEquality;NoComparison>]
type internal ResourceState =
    {
        files: ImmutableDictionary<OlyPath, int64 * MemoryMappedFile * DateTime>
        textEditors: OlySourceTextManager
        version: DateTime
    }

[<Sealed>]
type OlyWorkspaceResourceSnapshot(isForced: bool, state: ResourceState, activeConfigPath: OlyPath) =

    static let sourceTexts = ConditionalWeakTable<MemoryMappedFile, WeakReference<IOlySourceText>>()

    static let deltaComparer =
        {
            new IEqualityComparer<KeyValuePair<OlyPath, (int64 * MemoryMappedFile * DateTime)>> with
                member _.GetHashCode(pair) = pair.Key.GetHashCode()
                member _.Equals(pair1, pair2) =
                    if OlyPath.Equals(pair1.Key, pair2.Key) then
                        match pair1.Value, pair2.Value with
                        | (length1, mmap1, dt1), (length2, mmap2, dt2) ->
                            length1 = length2 && obj.ReferenceEquals(mmap1, mmap2) && dt1 = dt2
                    else
                        false
        }

    member private this.State = state

    member private this.HasResourceChanged(filePath: OlyPath, dt: DateTime): bool =
        if isForced then
            invalidOp "Snapshot is forced"
        match state.files.TryGetValue filePath with
        | true, (_, _, storedDt) -> storedDt <> dt
        | _ -> true

    member _.TextEditors = state.textEditors

    member this.GetDeltaEvents(prevRs: OlyWorkspaceResourceSnapshot) =
        if isForced then
            invalidOp "Snapshot is forced"
        let events = ImArray.builder()
        let deferChangedEvents = List()
        let result = System.Linq.Enumerable.Except(prevRs.State.files, state.files, deltaComparer)
        for x in result do
            if (prevRs.State.files.ContainsKey(x.Key) && state.files.ContainsKey(x.Key)) then
                deferChangedEvents.Add(OlyWorkspaceResourceEvent.Changed x.Key)
            else
                events.Add(OlyWorkspaceResourceEvent.Deleted x.Key)
        let result = System.Linq.Enumerable.Except(state.files.Keys, prevRs.State.files.Keys, OlyPathEqualityComparer.Instance)
        for x in result do
            events.Add(OlyWorkspaceResourceEvent.Added x)
        events.AddRange(deferChangedEvents)
        events.ToImmutable()

    member _.Version = state.version

    member this.SetResourceAsCopy(filePath: OlyPath) =
        if isForced then
            invalidOp "Snapshot is forced"
        let fileInfo = FileInfo(filePath.ToString())
        let dt = fileInfo.LastWriteTimeUtc
        if this.HasResourceChanged(filePath, dt) then
            let streamToCopy = File.Open(fileInfo.FullName, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite ||| IO.FileShare.Delete)
            try
                this.SetResourceAsCopy(filePath, streamToCopy, dt)
            finally
                streamToCopy.Dispose()
        else
            this

    member this.SetResourceAsCopy(filePath: OlyPath, stream: Stream) =
        if isForced then
            invalidOp "Snapshot is forced"
        this.SetResourceAsCopy(filePath, stream, DateTime.UtcNow)

    member private this.SetResourceAsCopy(filePath: OlyPath, streamToCopy: Stream, dt: DateTime): OlyWorkspaceResourceSnapshot =
        if isForced then
            invalidOp "Snapshot is forced"
        let length = streamToCopy.Length - streamToCopy.Position
        if length > 0 then
            let mmap = MemoryMappedFile.CreateNew(null, length, MemoryMappedFileAccess.ReadWrite)
            let view = mmap.CreateViewStream(0, length, MemoryMappedFileAccess.Write)
            try
                streamToCopy.CopyTo(view)
                this.SetResource(filePath, length, mmap, dt)
            finally
                view.Dispose()
        else
            let mmap = MemoryMappedFile.CreateNew(null, 1, MemoryMappedFileAccess.ReadWrite)
            this.SetResource(filePath, length, mmap, dt)

    member private _.SetResource(filePath: OlyPath, length: int64, mmap: MemoryMappedFile, dt: DateTime): OlyWorkspaceResourceSnapshot =
        if isForced then
            invalidOp "Snapshot is forced"
        OlyWorkspaceResourceSnapshot(
            false,
            { state with files = state.files.SetItem(filePath, (length, mmap, dt)); version = DateTime.UtcNow },
            activeConfigPath
        )

    member _.RemoveResource(filePath: OlyPath) =
        if isForced then
            invalidOp "Snapshot is forced"
        OlyWorkspaceResourceSnapshot(false, { state with files = state.files.Remove(filePath); version = DateTime.UtcNow }, activeConfigPath)

    member _.GetSourceText(filePath) =
        match state.textEditors.TryGet filePath with
        | Some(sourceText, _) -> sourceText
        | _ ->

        if isForced then
            OlySourceText.FromFile(filePath)
        else

        let (length, mmap, _) = state.files[filePath]
        if length = 0 then
            OlySourceText.Create("")
        else

        match sourceTexts.TryGetValue mmap with
        | true, weakTarget ->
            match weakTarget.TryGetTarget() with
            | true, sourceText -> sourceText
            | _ ->
                lock mmap (fun () ->
                    match weakTarget.TryGetTarget() with
                    | true, sourceText -> sourceText
                    | _ ->
                        let view = mmap.CreateViewStream(0, length, MemoryMappedFileAccess.Read)
                        try
                            let sourceText = OlySourceText.FromStream(view)
                            weakTarget.SetTarget(sourceText)
                            sourceText
                        finally
                            view.Dispose()
                )
        | _ ->
            lock mmap (fun () ->
                match sourceTexts.TryGetValue mmap with
                | true, weakTarget ->
                    match weakTarget.TryGetTarget() with
                    | true, sourceText -> sourceText
                    | _ ->
                        match weakTarget.TryGetTarget() with
                        | true, sourceText -> sourceText
                        | _ ->
                            let view = mmap.CreateViewStream(0, length, MemoryMappedFileAccess.Read)
                            try
                                let sourceText = OlySourceText.FromStream(view)
                                weakTarget.SetTarget(sourceText)
                                sourceText
                            finally
                                view.Dispose()
                | _ ->
                    let view = mmap.CreateViewStream(0, length, MemoryMappedFileAccess.Read)
                    try
                        let sourceText = OlySourceText.FromStream(view)
                        sourceTexts.AddOrUpdate(mmap, WeakReference<_>(sourceText))
                        sourceText
                    finally
                        view.Dispose()
            )

    member _.GetTimeStamp(filePath) =
        if isForced then
            File.GetLastAccessTimeUtc(filePath.ToString())
        else
            let (_, _, dt) = state.files[filePath]
            dt

    member _.FindSubPaths(dirPath: OlyPath) =
        if isForced then
            Directory.EnumerateFiles(dirPath.ToString(), "*.*", SearchOption.TopDirectoryOnly)
            |> Seq.map (fun x -> OlyPath.Create(x))
            |> ImArray.ofSeq
        else
            let builder = ImArray.builder()

            // TODO: This isn't optimal. We iterate through every resource to find the ones with the dfirectory.
            //       We should introduce a side-table that has this information instead of having to do this iteration.
            //       For now, it works.
            for pair in state.files do
                if (OlyPath.Equals(OlyPath.GetDirectory(pair.Key), dirPath)) then
                    builder.Add(pair.Key)

            builder.ToImmutable()

    member this.GetAllProjectConfigurations(projectFilePath: OlyPath) =
        match state.files.TryGetValue(OlyPath.ChangeExtension(projectFilePath, ".json")) with
        | true, (length, mmap, _) ->
            let view = mmap.CreateViewStream(0, length, MemoryMappedFileAccess.Read)
            try    
                let configs = ProjectConfigurations.Deserialize(view)
                configs.Configurations
                |> Array.map (fun x -> configs.GetConfiguration(x.Name))
                |> ImArray.ofSeq
            finally
                view.Dispose()
        | _ -> 
            let configs = ProjectConfigurations.Default
            configs.Configurations
            |> Array.map (fun x -> configs.GetConfiguration(x.Name))
            |> ImArray.ofSeq

    member this.GetProjectConfiguration(projectFilePath: OlyPath) =
        let configName = this.GetActiveConfigurationName()
        match state.files.TryGetValue(OlyPath.ChangeExtension(projectFilePath, ".json")) with
        | true, (length, mmap, _) ->
            let view = mmap.CreateViewStream(0, length, MemoryMappedFileAccess.Read)
            try    
                let configs = ProjectConfigurations.Deserialize(view)
                configs.GetConfiguration(configName)
            finally
                view.Dispose()
        | _ -> 
            ProjectConfigurations.Default.GetConfiguration(configName)

    member _.GetActiveConfigurationName() =
        match state.files.TryGetValue(OlyPath.ChangeExtension(activeConfigPath, ".json")) with
        | true, (length, mmap, _) ->
            let view = mmap.CreateViewStream(0, length, MemoryMappedFileAccess.Read)
            try
                let jsonOptions = JsonSerializerOptions()
                jsonOptions.PropertyNameCaseInsensitive <- true
                let contents = JsonSerializer.Deserialize<ActiveConfigurationState>(view, jsonOptions)
                contents.activeConfiguration
            finally
                view.Dispose()
        | _ ->
            "Release" // Default to Release

    member this.WithTextEditors(textEditors: OlySourceTextManager) =
        if isForced then
            invalidOp "Snapshot is forced"
        if obj.ReferenceEquals(state.textEditors, textEditors) then
            this
        else
            OlyWorkspaceResourceSnapshot(false, { state with textEditors = textEditors }, activeConfigPath)

    member _.ActiveConfigurationPath = activeConfigPath

    static member Create(activeConfigPath: OlyPath) =
        OlyWorkspaceResourceSnapshot(false, { files = ImmutableDictionary.Create(OlyPathEqualityComparer.Instance); version = DateTime(); textEditors = OlySourceTextManager.Empty }, activeConfigPath)

    static member CreateForced(activeConfigPath: OlyPath, activeConfigStream: Stream) =
        let rs = OlyWorkspaceResourceSnapshot.Create(activeConfigPath).SetResourceAsCopy(activeConfigPath, activeConfigStream);
        OlyWorkspaceResourceSnapshot(true, rs.State, activeConfigPath)

[<NoComparison;NoEquality>]
type private WorkspaceState =
    {
        defaultTargetPlatform: OlyBuild
        targetPlatforms: ImmutableDictionary<string, OlyBuild>
        preludeDirectory: OlyPath
        progress: IOlyWorkspaceProgress
    }

[<NoComparison;NoEquality>]
type WorkspaceMessage =
    | UpdateDocumentNoReply of OlyWorkspaceResourceSnapshot * documentPath: OlyPath * sourceText: IOlySourceText * ct: CancellationToken
    | UpdateDocumentNoReplyNoText of OlyWorkspaceResourceSnapshot * documentPath: OlyPath * ct: CancellationToken
    | UpdateDocumentsNoReplyNoText of OlyWorkspaceResourceSnapshot * documentPaths: OlyPath imarray * ct: CancellationToken
    | UpdateDocument of OlyWorkspaceResourceSnapshot * documentPath: OlyPath * sourceText: IOlySourceText * ct: CancellationToken * AsyncReplyChannel<OlyDocument imarray>
    | GetDocuments of OlyWorkspaceResourceSnapshot * documentPath: OlyPath * ct: CancellationToken * AsyncReplyChannel<OlyDocument imarray>
    | GetAllDocuments of OlyWorkspaceResourceSnapshot * ct: CancellationToken * AsyncReplyChannel<OlyDocument imarray>
    | RemoveProject of OlyWorkspaceResourceSnapshot * projectPath: OlyPath * ct: CancellationToken
    | GetSolution of OlyWorkspaceResourceSnapshot * ct: CancellationToken * AsyncReplyChannel<OlySolution>
    | ClearSolution of ct: CancellationToken
    | Clean of AsyncReplyChannel<unit>

type IOlyWorkspaceProgress =

    abstract OnBeginWork: unit -> unit

    abstract OnEndWork: unit -> unit

[<Sealed>]
type OlyWorkspace private (state: WorkspaceState) as this =

    static let getStortedPaths (rs: OlyWorkspaceResourceSnapshot) absoluteDir (paths: (OlyTextSpan * OlyPath) seq) =
        paths
        |> Seq.map (fun (textSpan, path) ->
            match path.TryGetGlob() with
            | Some(dir, ext) ->
                let dir = OlyPath.Combine(absoluteDir, dir.ToString())
                rs.FindSubPaths(dir)
                |> ImArray.choose (fun path ->
                    if path.ToString().EndsWith(ext, StringComparison.OrdinalIgnoreCase) then
                        Some(textSpan, path)
                    else
                        None
                )
            | _ ->
                ImArray.createOne (textSpan, path)
        )
        |> Seq.concat
        |> Seq.sortBy (fun (_, path) -> path.ToString())
        |> ImArray.ofSeq

    static let getSortedLoadsFromConfig rs absoluteDir (config: OlyCompilationUnitConfiguration) =
        getStortedPaths rs absoluteDir config.Loads

    static let getSortedReferencesFromConfig rs absoluteDir (config: OlyCompilationUnitConfiguration) =
        getStortedPaths rs absoluteDir config.References

    let mutable solutionRef = 
        {
            workspace = this
            projects = ImmutableDictionary.Empty
            version = 0UL
        }
        |> OlySolution
        |> ref

    let checkProject rs (proj: OlyProject) ct =
        async {
            let syntaxTree = proj.DocumentLookup[proj.Path].SyntaxTree
            let! newSolutionOpt = OlyWorkspace.UpdateProjectAsync(solutionRef, rs, state, solutionRef.contents, syntaxTree, proj.Path, proj.Configuration, ct) |> Async.AwaitTask
            match newSolutionOpt with
            | Some(newSolution) ->
                solutionRef.contents <- newSolution
            | _ ->
                ()
        }

    let checkProjectsByDocuments rs (docs: OlyDocument imarray) ct =
        async {
            let projs =
                docs 
                |> ImArray.map (fun x -> x.Project)
                |> ImArray.distinctBy (fun x -> x.Name)
                // Sort projects by number of references that are projects to provide results faster for the workspace solution.
                |> ImArray.sortBy (fun x -> (x.References |> ImArray.filter (fun y -> y.IsProject)).Length)
            for proj in projs do
                do! checkProject rs proj ct
        }

    let checkProjectsThatContainDocument rs (documentPath: OlyPath) ct =
        async {
            let docs = solutionRef.contents.GetDocuments(documentPath)
            do! checkProjectsByDocuments rs docs ct
        }

    let checkAllProjects rs ct =
        async {
            let docs = solutionRef.contents.GetAllDocuments()
            do! checkProjectsByDocuments rs docs ct
        }

    let mutable cts = new CancellationTokenSource()

    let mapCtToCtr (ct: CancellationToken) =
        async {
            cts.Dispose()
            cts <- new CancellationTokenSource()
            return 
                (
                    let ctr =
                        ct.Register(
                            fun () ->
                                cts.Cancel()
                        )
                    try
                        ct.ThrowIfCancellationRequested()
                    with
                    | _ ->
                        ctr.Dispose()
                        reraise()
                    ctr,
                    cts.Token
                )
        }

    let clearSolution() =
        solutionRef.contents <- 
            {
                workspace = this
                projects = ImmutableDictionary.Empty
                version = solutionRef.contents.Version
            }
            |> OlySolution

    let mutable currentRs = Unchecked.defaultof<OlyWorkspaceResourceSnapshot>
    let getRs (rs: OlyWorkspaceResourceSnapshot) (ct: CancellationToken) =
        async {
            let prevSolution = solutionRef.contents
            let prevRs = currentRs
            try
                if isNull(currentRs: obj) then
                    clearSolution()
                    currentRs <- rs
                elif rs.Version > currentRs.Version then
                    let deltaEvents = rs.GetDeltaEvents(currentRs)

                    for deltaEvent in deltaEvents do
                        match deltaEvent with
                        | OlyWorkspaceResourceEvent.Added filePath ->
                            solutionRef.contents.GetProjects()
                            |> ImArray.iter (fun project ->
                                let projConfig = project.Configuration
                                let platformName = project.PlatformName

                                let projectDir = OlyPath.GetDirectory(project.Path)
                                let syntaxTree = project.Compilation.GetSyntaxTree(project.Path)
                                let config = syntaxTree.GetCompilationUnitConfiguration(ct)

                                if filePath.HasExtension(ProjectExtension) |> not then
                                    config.Loads
                                    |> ImArray.iter (fun (_, loadPath) ->
                                        let loadPath = OlyPath.Combine(projectDir, loadPath)
                                        if loadPath.ContainsDirectoryOrFile(filePath) then
                                            let sourceText = rs.GetSourceText(filePath)
                                            let parsingOptions = { OlyParsingOptions.Default with ConditionalDefines = projConfig.Defines.Add(platformName.ToUpper()) }
                                            let syntaxTree = OlySyntaxTree.Parse(filePath, (fun ct -> ct.ThrowIfCancellationRequested(); sourceText), parsingOptions)
                                            let (newSolution, _, _) = solutionRef.contents.UpdateDocument(project.Path, filePath, syntaxTree, ImArray.empty)
                                            solutionRef.contents <- newSolution
                                    )
                            )
                        | OlyWorkspaceResourceEvent.Deleted filePath ->
                            if filePath.HasExtension(ProjectExtension) then
                                solutionRef.contents <- solutionRef.contents.RemoveProject(filePath)
                            else
                                let docs = solutionRef.contents.GetDocuments(filePath)
                                docs
                                |> ImArray.iter (fun doc ->
                                    solutionRef.contents <- solutionRef.contents.RemoveDocument(doc.Project.Path, doc.Path)
                                )
                        | OlyWorkspaceResourceEvent.Changed filePath ->
                            do! checkProjectsThatContainDocument rs filePath ct

                    currentRs <- rs
                return currentRs
            with
            | :? OperationCanceledException ->
                solutionRef.contents <- prevSolution
                if isNull(currentRs: obj) then
                    clearSolution()
                    currentRs <- rs
                    return currentRs
                else
                    return prevRs
        }

    let mutable rsCheckForUpToDate = Unchecked.defaultof<OlyWorkspaceResourceSnapshot>

    let onBeginWork _ct = 
        async {
            state.progress.OnBeginWork()
        }
    let onEndWork _ct = 
        async {
            state.progress.OnEndWork()
            rsCheckForUpToDate <- currentRs
        }

    let mbp = new MailboxProcessor<WorkspaceMessage>(fun mbp ->
        let rec loop() =
            async {
                match! mbp.Receive() with
                | GetSolution(rs, ct, reply) ->
                    do! onBeginWork ct

                    let! _ = getRs rs ct
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - GetSolution()")
#endif
                    let prevSolution = solutionRef.contents
                    
                    try
                        let! ctr, ct = mapCtToCtr ct
                        use _ = ctr
                        ct.ThrowIfCancellationRequested()
                        reply.Reply(solutionRef.contents)
                    with
                    | ex ->
                        match ex with
                        | :? OperationCanceledException -> ()
                        | _ -> OlyTrace.Log($"OlyWorkspace - GetSolution:\n" + ex.ToString())
                        solutionRef.contents <- prevSolution

                    do! onEndWork ct

                | RemoveProject(rs, projectPath, ct) ->
                    do! onBeginWork ct

                    let! _ = getRs rs ct
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - RemoveProject({projectPath.ToString()})")
#endif
                    let prevSolution = solutionRef.contents
                    try
                        let! ctr, ct = mapCtToCtr ct
                        use _ = ctr
                        ct.ThrowIfCancellationRequested()
                        solutionRef.contents <- solutionRef.contents.RemoveProject(projectPath)
                    with
                    | ex ->
                        match ex with
                        | :? OperationCanceledException -> ()
                        | _ -> OlyTrace.Log($"OlyWorkspace - RemoveProject({projectPath.ToString()}):\n" + ex.ToString())
                        solutionRef.contents <- prevSolution

                    do! onEndWork ct

                | ClearSolution(ct) ->
                    do! onBeginWork ct

                    currentRs <- Unchecked.defaultof<_>
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - ClearSolution")
#endif
                    let prevSolution = solutionRef.contents
                    try
                        let! ctr, ct = mapCtToCtr ct
                        use _ = ctr
                        ct.ThrowIfCancellationRequested()
                        clearSolution()
                    with
                    | ex ->
                        match ex with
                        | :? OperationCanceledException -> ()
                        | _ -> OlyTrace.Log($"OlyWorkspace - ClearSolution:\n" + ex.ToString())
                        solutionRef.contents <- prevSolution

                    do! onEndWork ct

                | UpdateDocumentNoReply(rs, documentPath, sourceText, ct) ->
                    do! onBeginWork ct

                    let! rs = getRs rs ct
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - UpdateDocumentNoReply({documentPath.ToString()})")
#endif
                    let prevSolution = solutionRef.contents
                    try
                        let! ctr, ct = mapCtToCtr ct
                        use _ = ctr
                        ct.ThrowIfCancellationRequested()
                        do! checkProjectsThatContainDocument rs documentPath ct
                        do! this.UpdateDocumentAsyncCore(rs, documentPath, sourceText, ct) |> Async.AwaitTask
                    with
                    | ex ->
                        match ex with
                        | :? OperationCanceledException -> ()
                        | _ -> OlyTrace.Log($"OlyWorkspace - UpdateDocumentNoReply:\n" + ex.ToString())
                        solutionRef.contents <- prevSolution

                    do! onEndWork ct

                | UpdateDocumentNoReplyNoText(rs, documentPath, ct) ->
                    do! onBeginWork ct

                    let! rs = getRs rs ct
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - UpdateDocumentNoReplyNoText")
#endif                    
                    let prevSolution = solutionRef.contents
                    try
                        let! ctr, ct = mapCtToCtr ct
                        use _ = ctr
                        ct.ThrowIfCancellationRequested()
                        do! checkProjectsThatContainDocument rs documentPath ct
                        do! this.UpdateDocumentAsyncCore(rs, documentPath, rs.GetSourceText(documentPath), ct) |> Async.AwaitTask
                    with
                    | ex ->
                        match ex with
                        | :? OperationCanceledException -> ()
                        | _ -> OlyTrace.Log($"OlyWorkspace - UpdateDocumentNoReplyNoText:\n" + ex.ToString())
                        solutionRef.contents <- prevSolution

                    do! onEndWork ct

                | UpdateDocumentsNoReplyNoText(rs, documentPaths, ct) ->
                    do! onBeginWork ct

                    let! rs = getRs rs ct
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - UpdateDocumentsNoReplyNoText")
#endif                    
                    for documentPath in documentPaths do
                        let prevSolution = solutionRef.contents
                        try
                            let! ctr, ct = mapCtToCtr ct
                            use _ = ctr
                            ct.ThrowIfCancellationRequested()
                            do! checkProjectsThatContainDocument rs documentPath ct
                            do! this.UpdateDocumentAsyncCore(rs, documentPath, rs.GetSourceText(documentPath), ct) |> Async.AwaitTask
                        with
                        | ex ->
                            match ex with
                            | :? OperationCanceledException -> ()
                            | _ -> OlyTrace.Log($"OlyWorkspace - UpdateDocumentsNoReplyNoText:\n" + ex.ToString())
                            solutionRef.contents <- prevSolution

                    do! onEndWork ct

                | UpdateDocument(rs, documentPath, sourceText, ct, reply) ->
                    do! onBeginWork ct

                    let! rs = getRs rs ct
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - UpdateDocument({documentPath.ToString()})")
#endif
                    let prevSolution = solutionRef.contents
                    try
                        let! ctr, ct = mapCtToCtr ct
                        use _ = ctr
                        ct.ThrowIfCancellationRequested()
                        do! checkProjectsThatContainDocument rs documentPath ct
                        do! this.UpdateDocumentAsyncCore(rs, documentPath, sourceText, ct) |> Async.AwaitTask
                        let docs = solutionRef.contents.GetDocuments(documentPath)
                        reply.Reply(docs)
                    with
                    | ex ->
                        match ex with
                        | :? OperationCanceledException -> ()
                        | _ -> OlyTrace.Log($"OlyWorkspace - UpdateDocument({documentPath.ToString()})\n" + ex.ToString())
                        solutionRef.contents <- prevSolution
                        reply.Reply(ImArray.empty)

                    do! onEndWork ct

                | GetDocuments(rs, documentPath, ct, reply) ->
                    do! onBeginWork ct

                    let! rs = getRs rs ct
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - GetDocuments({documentPath.ToString()})")
#endif
                    let prevSolution = solutionRef.contents
                    try
                        let! ctr, ct = mapCtToCtr ct
                        use _ = ctr
                        ct.ThrowIfCancellationRequested()
                        do! checkProjectsThatContainDocument rs documentPath ct
                        let docs = solutionRef.contents.GetDocuments(documentPath)
                        reply.Reply(docs)
                    with
                    | ex ->
                        match ex with
                        | :? OperationCanceledException -> ()
                        | _ -> OlyTrace.Log($"OlyWorkspace - GetDocuments({documentPath.ToString()})\n" + ex.ToString())
                        solutionRef.contents <- prevSolution
                        reply.Reply(ImArray.empty)

                    do! onEndWork ct

                | GetAllDocuments(rs, ct, reply) ->
                    do! onBeginWork ct

                    let! rs = getRs rs ct
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - GetAllDocuments")
#endif
                    let prevSolution = solutionRef.contents
                    try
                        let! ctr, ct = mapCtToCtr ct
                        use _ = ctr
                        ct.ThrowIfCancellationRequested()
                        do! checkAllProjects rs ct
                        let docs = solutionRef.contents.GetAllDocuments()
                        reply.Reply(docs)
                    with
                    | ex ->
                        match ex with
                        | :? OperationCanceledException -> ()
                        | _ -> OlyTrace.Log($"OlyWorkspace - GetAllDocuments:\n" + ex.ToString())
                        solutionRef.contents <- prevSolution
                        reply.Reply(ImArray.empty)

                    do! onEndWork ct

                | Clean(reply) ->
                    // Clean cannot be canceled
                    do! onBeginWork CancellationToken.None

#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - Clean")
#endif

                    try
                        solutionRef.contents.GetProjects()
                        |> ImArray.iter (fun proj ->
                            let projDir = OlyPath.GetDirectory(proj.Path)
                            try Directory.Delete(OlyPath.Combine(projDir, CacheDirectoryName).ToString(), true) with | _ -> ()
                            try Directory.Delete(OlyPath.Combine(projDir, BinDirectoryName).ToString(), true) with | _ -> ()
                        )

                        clearSolution()
                        reply.Reply(())
                    with
                    | ex ->
                         OlyTrace.Log($"OlyWorkspace - Clean:\n" + ex.ToString())
                         reply.Reply(())

                    do! onEndWork CancellationToken.None

                return! loop()
            }
        loop()
    )

    do
        mbp.Start()

    member this.CancelCurrentWork() =
        try
            cts.Cancel()
        with
        | _ ->
            ()

    member this.GetBuild(projPath: OlyPath) =   
        let solution = solutionRef.contents
        let project = solution.GetProject(projPath)
        state.targetPlatforms.[project.PlatformName]

    static member private GetProjectConfiguration(rs: OlyWorkspaceResourceSnapshot, projPath: OlyPath) =
        let projConfigPath = OlyPath.ChangeExtension(projPath, ProjectConfigurationExtension)
        rs.GetProjectConfiguration(projConfigPath)

    static member private ReloadProjectAsync(workspaceSolutionRef: OlySolution ref, rs: OlyWorkspaceResourceSnapshot, state: WorkspaceState, solution: OlySolution, syntaxTree: OlySyntaxTree, projPath: OlyPath, projConfig: OlyProjectConfiguration, ct: CancellationToken) =
        backgroundTask {
            if syntaxTree.ParsingOptions.CompilationUnitConfigurationEnabled |> not then
                failwith "Unable to load project: Compilation unit configuration must be enabled."

            let filePath = syntaxTree.Path
            if filePath.HasExtension(ProjectExtension) |> not then
                failwithf "Invalid project file path '%A'" filePath

            let config = syntaxTree.GetCompilationUnitConfiguration(ct)
            return! OlyWorkspace.ReloadProjectAsync(workspaceSolutionRef, rs, state, solution, syntaxTree, projPath, config, filePath, projConfig, ct)
        }

    static member private ReloadProjectAsync(workspaceSolutionRef: OlySolution ref, rs: OlyWorkspaceResourceSnapshot, state: WorkspaceState, solution: OlySolution, syntaxTree: OlySyntaxTree, projPath: OlyPath, config: OlyCompilationUnitConfiguration, filePath, projConfig, ct: CancellationToken) =
        backgroundTask {
            let diags = ImArray.builder ()

            let absoluteDir = OlyPath.GetDirectory(filePath)

            let platformName, targetName = 
                match config.Target with
                | Some(_, targetName) -> 
                    let targetName = targetName.Replace(" ", "")
                    let index = targetName.IndexOf(':')
                    if (index = -1) || (index + 1 = targetName.Length) || targetName.Contains("\n") then
                        diags.Add(OlyDiagnostic.CreateError("Invalid target."))
                        String.Empty, String.Empty
                    else
                        targetName.Substring(0, index), targetName.Substring(index + 1)
                | _ -> 
                    diags.Add(OlyDiagnostic.CreateError("'#target' directive needs to be specified."))
                    String.Empty, String.Empty

            let targetPlatform =
                if String.IsNullOrWhiteSpace(platformName) then
                    state.defaultTargetPlatform
                else
                    match state.targetPlatforms.TryGetValue platformName with
                    | true, target -> target
                    | _ -> 
                        match config.Target with
                        | Some(textSpan, _) ->
                            diags.Add(OlyDiagnostic.CreateError($"Target platform '{platformName}' does not exist in the workspace.", OlySourceLocation.Create(textSpan, syntaxTree)))
                        | _ ->
                            diags.Add(OlyDiagnostic.CreateError($"Target platform '{platformName}' does not exist in the workspace."))
                        state.defaultTargetPlatform

            let outputKind = 
                if config.IsLibrary then
                    OlyOutputKind.Library
                else
                    OlyOutputKind.Executable

            let targetInfo =
                OlyTargetInfo(targetName, projConfig, outputKind, targetPlatform.GetImplicitExtendsForStruct(), targetPlatform.GetImplicitExtendsForEnum())

            if String.IsNullOrWhiteSpace(platformName) |> not && targetPlatform.IsValidTargetName(targetInfo) |> not then
                diags.Add(OlyDiagnostic.CreateError($"'{targetName}' is an invalid target for '{platformName}'."))

            try
                do! targetPlatform.OnBeforeReferencesImportedAsync(projPath, targetInfo, ct)
            with
            | ex ->
                diags.Add(OlyDiagnostic.CreateError(ex.Message))

            let projPath = OlyPath.Create(filePath.ToString())

            let chooseReference textSpan (path: OlyPath) =
                backgroundTask {
                    let path = OlyPath.Combine(absoluteDir, path.ToString())
                    if targetPlatform.CanImportReference path then
                        match! targetPlatform.ImportReferenceAsync(projPath, targetInfo, path, ct) with
                        | Ok r -> 
                            match r with
                            | Some r ->
                                if r.IsTransitive then
                                    return OlyProjectReference.Create(r.CompilationReference) |> Some
                                else
                                    return OlyProjectReference.CreateNonTransitive(r.CompilationReference) |> Some
                            | _ ->
                                return None
                        | Error msg ->
                            diags.Add(OlyDiagnostic.CreateError(msg, OlySourceLocation.Create(textSpan, syntaxTree)))
                            return None
                    else
                        diags.Add(OlyDiagnostic.CreateError($"Reference '{path}' is not valid.", OlySourceLocation.Create(textSpan, syntaxTree)))
                        return None
                }

            // Some basic validation
            config.References
            |> ImArray.iter (fun (textSpan, path) ->
                if path.HasExtension(".oly") then
                    diags.Add(OlyDiagnostic.CreateError($"Cannot reference Oly file(s) '{path}'. Use '#load' instead.", OlySourceLocation.Create(textSpan, syntaxTree)))
            )

            let referenceInfos = 
                getSortedReferencesFromConfig rs absoluteDir config
                |> ImArray.map (fun (textSpan, path) ->
                    OlyReferenceInfo(OlyPath.Combine(absoluteDir, path.ToString()), textSpan)
                )

            let packageInfos =
                config.Packages
                |> ImArray.map (fun (textSpan, text) ->
                    OlyPackageInfo(text, textSpan)
                )

            let copyFileInfos =
                config.CopyFiles
                |> ImArray.map (fun (textSpan, path) ->
                    OlyCopyFileInfo(OlyPath.Combine(absoluteDir, path.ToString()), textSpan)
                )

            let olyxReferenceInfos =
                referenceInfos
                |> ImArray.filter (fun x -> x.Path.HasExtension(".olyx"))

            let olyxReferenceInfos =
                let preludeProjectPath = OlyPath.Combine(state.preludeDirectory, $"prelude_{platformName}.olyx")
                if OlyPath.Equals(projPath, preludeProjectPath) then
                    olyxReferenceInfos
                else
                    olyxReferenceInfos.Add(OlyReferenceInfo(preludeProjectPath, OlyTextSpan.Create(0, 0)))

            let olyxReferenceInfosToUpdate =
                olyxReferenceInfos
                |> ImArray.map (fun x ->
                    OlyWorkspace.ParseProject(rs, x.Path, rs.GetSourceText(x.Path)), x.TextSpan
                )

            let olyxReferenceInfosToUpdate =

                // TODO: Theoretically, this could stack overflow. Make this tail recursive.
                // REVIEW: This *may* have performance implications due to the following:
                //     - We could be redundantly parsing for the unit configurations if caching is not happening
                let visited = Dictionary<OlyPath, int>(OlyPathEqualityComparer.Instance)
                let rec getProjectReferenceCount (absolutePath: OlyPath) =
                    match visited.TryGetValue absolutePath with
                    | true, count -> count
                    | _ ->
                        if absolutePath.HasExtension(ProjectExtension) then
                            let (syntaxTree: OlySyntaxTree), _ = OlyWorkspace.ParseProject(rs, absolutePath, rs.GetSourceText(absolutePath))
                            let config = syntaxTree.GetCompilationUnitConfiguration(ct)
                            let absoluteDir = OlyPath.GetDirectory(absolutePath)
                            let count = config.References.Length + (config.References |> ImArray.sumBy (fun (_, x) -> getProjectReferenceCount (OlyPath.Combine(absoluteDir, x))))
                            visited.Add(absolutePath, count)
                            count
                        else
                            let count = 0
                            visited.Add(absolutePath, count)
                            count
                        
                    
                // This is a heuristic as we want to update projects that have the least amount of references to projects as to provide
                // results faster for the workspace solution.
                try
                    olyxReferenceInfosToUpdate
                    |> ImArray.sortBy (fun ((syntaxTree, _), _) ->
                        getProjectReferenceCount syntaxTree.Path
                    )
                with
                | ex ->
                    OlyTrace.Log(ex.Message)
                    ImArray.empty

            let projectReferencesInWorkspace = ImArray.builder()
            let mutable solution = solution
            for (syntaxTree: OlySyntaxTree, projConfig), projTextSpan in olyxReferenceInfosToUpdate do
                let projPath = syntaxTree.Path
                try
                    let! resultOpt = OlyWorkspace.UpdateProjectAsync(workspaceSolutionRef, rs, state, solution, syntaxTree, projPath, projConfig, ct)
                    match resultOpt with
                    | Some result ->
                        solution <- result
                        workspaceSolutionRef.contents <- result // Update the main workspace solution as to provide faster results as multiple projects are being built.
                    | _ ->
                        ()
                    match solution.TryGetProject(projPath) with
                    | Some proj ->
                        projectReferencesInWorkspace.Add(OlyProjectReference.Project(proj.Path))
                    | _ ->
                        diags.Add(OlyDiagnostic.CreateError($"Cannot reference Oly project '{projPath}' as it does not exist in the current workspace.", OlySourceLocation.Create(projTextSpan, syntaxTree)))
                with
                | ex ->
                    System.Diagnostics.Debug.WriteLine(ex.Message)
                    diags.Add(OlyDiagnostic.CreateError($"Cannot reference Oly project '{projPath}'. Internal Error: {ex.Message}", OlySourceLocation.Create(projTextSpan, syntaxTree)))
            let projectReferencesInWorkspace = projectReferencesInWorkspace.ToImmutable()

            let transitivePackageInfos =
                getTransitivePackages solution projectReferencesInWorkspace ct

            let combinedPackageInfos =
                // TODO: What happens if we have multiple of the same packages with different versions? Just kick it down to the target platform resolution?
                // TODO: Wrong text-span for the transitive package infos, we will need to fix that. Perhaps OlyPackageInfo should use a source location instead of a text-span.
                ImArray.append transitivePackageInfos packageInfos
                |> Seq.distinctBy (fun x -> x.Text)
                |> ImArray.ofSeq

            let transitiveProjectReferences =
                getTransitiveProjectReferences solution projectReferencesInWorkspace ct

            let transitiveReferenceInfos =            
                transitiveProjectReferences
                |> ImArray.map (fun x -> x.References)
                |> ImArray.concat
                |> ImArray.map (fun x -> OlyReferenceInfo(x.Path, OlyTextSpan.Create(0, 0))) // TODO: TextSpan is not right.
                |> ImArray.append referenceInfos
                |> ImArray.distinctBy (fun x ->
                    x.Path
                )

            let referenceInfos =
                transitiveReferenceInfos
                |> ImArray.filter (fun x -> 
                    not(x.Path.HasExtension(".olyx")) &&
                    not(x.Path.HasExtension(".oly"))
                )
            
            let! resInfo = targetPlatform.ResolveReferencesAsync(projPath, targetInfo, referenceInfos, combinedPackageInfos, ct)

            resInfo.Diagnostics
            |> ImArray.iter diags.Add

            let resolvedReferences =
                resInfo.Paths
                |> ImArray.map (fun x -> (OlyTextSpan.Create(0, 0), x)) // TODO: TextSpan not right.
                |> Seq.distinctBy (fun (_, x) -> x.ToString())
                |> Seq.sortBy (fun (_, x) -> x.ToString())
                |> ImArray.ofSeq

            let! projectReferences =
                resolvedReferences
                |> Seq.distinctBy (fun x -> x.ToString().ToLower()) // TODO: This allocates extra with '.ToLower()', figure out a better way for this.
                |> Seq.map (fun (textSpan, path) ->
                    chooseReference textSpan path
                )
                |> Task.WhenAll

            let projectReferences =
                projectReferences
                |> Seq.choose id
                |> ImArray.ofSeq

            let copyFileInfos =
                resInfo.FilesToCopy
                |> ImArray.map (fun x ->
                    OlyCopyFileInfo(x, OlyTextSpan.Create(0, 0)) // TODO: TextSpan not right.
                )
                |> ImArray.append copyFileInfos
                |> ImArray.distinct

            let projectReferences = ImArray.append projectReferences projectReferencesInWorkspace

            try
                targetPlatform.OnAfterReferencesImported()
            with
            | ex ->
                diags.Add(OlyDiagnostic.CreateError(ex.Message))

            let solution, _ = solution.CreateProject(projPath, projConfig, platformName, targetInfo, packageInfos, copyFileInfos, ct)

            let loads = getSortedLoadsFromConfig rs absoluteDir config

            let solution, _, _ = solution.UpdateDocument(projPath, filePath, syntaxTree, diags.ToImmutable())

            let solution =
                (solution, loads)
                ||> ImArray.fold (fun solution (textSpan, path) ->
                    if path.HasExtension(ProjectExtension) then
                        diags.Add(OlyDiagnostic.CreateError("Project files cannot be loaded, only referenced. Use '#reference'.", OlySourceLocation.Create(textSpan, syntaxTree)))
                        solution
                    else
                        let path = OlyPath.Combine(absoluteDir, path.ToString())
                        if OlyPath.Equals(filePath, path) then
                            solution
                        else
                            try
                                let sourceText = rs.GetSourceText(path)
                                let parsingOptions = { OlyParsingOptions.Default with ConditionalDefines = projConfig.Defines.Add(platformName.ToUpper()) }
                                let syntaxTree = 
                                    OlySyntaxTree.Parse(path, (fun ct -> ct.ThrowIfCancellationRequested(); sourceText), parsingOptions)
                                let solution, _, _ = solution.UpdateDocument(projPath, path, syntaxTree, ImArray.empty)
                                solution
                            with
                            | ex ->
                                diags.Add(OlyDiagnostic.CreateError(ex.Message, OlySourceLocation.Create(textSpan, syntaxTree)))
                                solution
                )

            let solution, _ = solution.UpdateReferences(projPath, projectReferences, ct)
            return solution
        }

    static member private UpdateProjectAsync(workspaceSolutionRef: OlySolution ref, rs: OlyWorkspaceResourceSnapshot, state: WorkspaceState, solution: OlySolution, syntaxTree: OlySyntaxTree, projPath: OlyPath, projConfig: OlyProjectConfiguration, ct) : Task<OlySolution option> =
        backgroundTask {
            let filePath = syntaxTree.Path
            match solution.TryGetProject(filePath) with
            | Some project ->
                let currentDoc = project.GetDocument(syntaxTree.Path)
                let currentSyntaxTree = currentDoc.SyntaxTree
                let currentConfig = currentSyntaxTree.GetCompilationUnitConfiguration(ct)
                let currentLoads =  currentConfig.Loads
                let currentRefs = currentConfig.References
                let currentPackages = currentConfig.Packages
                let currentCopyFiles = currentConfig.CopyFiles
                let currentTarget = currentConfig.Target
                let currentIsLibrary = currentConfig.IsLibrary

                let config = syntaxTree.GetCompilationUnitConfiguration(ct)
                let loads = config.Loads
                let refs = config.References
                let packages = config.Packages
                let copyFiles = config.CopyFiles
                let target = config.Target
                let isLibrary = config.IsLibrary

                let currentTargetName = currentTarget |> Option.map snd |> Option.defaultValue ""
                let targetName = target |> Option.map snd |> Option.defaultValue ""

                if loads.Length <> currentLoads.Length || refs.Length <> currentRefs.Length || packages.Length <> currentPackages.Length ||
                   copyFiles.Length <> currentCopyFiles.Length || targetName <> currentTargetName || currentIsLibrary <> isLibrary then
#if DEBUG || CHECKED
                    OlyTrace.Log($"OlyWorkspace - Reloading Existing Project - {projPath.ToString()}")
#endif
                    let! result = OlyWorkspace.ReloadProjectAsync(workspaceSolutionRef, rs, state, solution, syntaxTree, projPath, projConfig, ct)
                    return Some result
                else
                    let loadsAreSame =
                        (loads, currentLoads)
                        ||> ImArray.forall2 (fun (_, path1) (_, path2) -> OlyPath.Equals(path1, path2))

                    let refsAreSame =
                        (refs, currentRefs)
                        ||> ImArray.forall2 (fun (_, path1) (_, path2) -> OlyPath.Equals(path1, path2))

                    let packagesAreSame =
                        (packages, currentPackages)
                        ||> ImArray.forall2 (fun (_, text1) (_, text2) -> text1 = text2)

                    let copyFilesAreSame =
                        (copyFiles, currentCopyFiles)
                        ||> ImArray.forall2 (fun (_, path1) (_, path2) -> OlyPath.Equals(path1, path2))

                    if loadsAreSame && refsAreSame && packagesAreSame && copyFilesAreSame then
                        return None
                    else
#if DEBUG || CHECKED
                        OlyTrace.Log($"OlyWorkspace - Reloading Existing Project - {projPath.ToString()}")
#endif                            
                        let! result = OlyWorkspace.ReloadProjectAsync(workspaceSolutionRef, rs, state, solution, syntaxTree, projPath, projConfig, ct)
                        return Some result
            | _ -> 
#if DEBUG || CHECKED
                OlyTrace.Log($"OlyWorkspace - Creating Project - {projPath.ToString()}")
#endif
                let! result = OlyWorkspace.ReloadProjectAsync(workspaceSolutionRef, rs, state, solution, syntaxTree, projPath, projConfig, ct)
                return Some result
        }

    static member private ParseProject(rs: OlyWorkspaceResourceSnapshot, projPath: OlyPath, sourceText: IOlySourceText) =
        OlyAssert.True(projPath.HasExtension(ProjectExtension))

        // Handle project config for syntax tree
        let projConfig = OlyWorkspace.GetProjectConfiguration(rs, projPath)
        let parsingOptions = 
            { OlyParsingOptions.CompilationUnitConfigurationEnabled = true
              OlyParsingOptions.AnonymousModuleDefinitionAllowed = true 
              OlyParsingOptions.ConditionalDefines = projConfig.Defines }

        OlySyntaxTree.Parse(projPath, sourceText, parsingOptions), projConfig

    static member private UpdateDocumentAsyncCore(workspaceSolutionRef: OlySolution ref, rs: OlyWorkspaceResourceSnapshot, state, solution: OlySolution, documentPath: OlyPath, sourceText: IOlySourceText, ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            let docs = solution.GetDocuments(documentPath)

            if docs.IsEmpty then
                if documentPath.HasExtension(ProjectExtension) then
                    let syntaxTree, projConfig = OlyWorkspace.ParseProject(rs, documentPath, sourceText)
                    match! OlyWorkspace.UpdateProjectAsync(workspaceSolutionRef, rs, state, solution, syntaxTree, documentPath, projConfig, ct) with
                    | Some solution -> return solution
                    | _ -> return solution
                else
                    return solution
            else
                let mutable solutionResult = solution
                for i = 0 to docs.Length - 1 do
                    ct.ThrowIfCancellationRequested()
                    let doc = docs[i]
                    let prevSourceText = doc.GetSourceText(ct)
                    if (not(obj.ReferenceEquals(prevSourceText, sourceText)) && prevSourceText.GetHashCode() <> sourceText.GetHashCode()) then
                        let syntaxTree = doc.SyntaxTree.ApplySourceText(sourceText)
                        if doc.IsProjectDocument then

                            // Handle project config for syntax tree
                            let projPath = doc.Path
                            let projConfig = OlyWorkspace.GetProjectConfiguration(rs, projPath)
                            let parsingOptions = 
                                { OlyParsingOptions.CompilationUnitConfigurationEnabled = true
                                  OlyParsingOptions.AnonymousModuleDefinitionAllowed = true 
                                  OlyParsingOptions.ConditionalDefines = projConfig.Defines }

                            let syntaxTree =
                                if OlyParsingOptions.AreEqual(syntaxTree.ParsingOptions, parsingOptions) then
                                    syntaxTree
                                else
                                    OlySyntaxTree.Parse(projPath, sourceText, parsingOptions)

                            match! OlyWorkspace.UpdateProjectAsync(workspaceSolutionRef, rs, state, solutionResult, syntaxTree, projPath, projConfig, ct) with
                            | Some solution -> solutionResult <- solution
                            | _ ->
                                let solution, _, _ = solutionResult.UpdateDocument(doc.Project.Path, doc.Path, syntaxTree, doc.ExtraDiagnostics)
                                solutionResult <- solution
                        else
                            let solution, _, _ = solutionResult.UpdateDocument(doc.Project.Path, doc.Path, syntaxTree, doc.ExtraDiagnostics)
                            solutionResult <- solution
                    else
                        ()
                return solutionResult
        }

    member _.StaleSolution = solutionRef.contents

    member this.GetSolutionAsync(rs, ct) =
        backgroundTask {
            return! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.GetSolution(rs, ct, reply))
        }

    member this.UpdateDocumentAsyncCore(rs: OlyWorkspaceResourceSnapshot, documentPath: OlyPath, sourceText: IOlySourceText, ct: CancellationToken): Task<unit> =
        backgroundTask {
            let prevSolution = solutionRef.contents
            try
                ct.ThrowIfCancellationRequested()
                let! newSolution = OlyWorkspace.UpdateDocumentAsyncCore(solutionRef, rs, state, prevSolution, documentPath, sourceText, ct)
                solutionRef.contents <- newSolution
            with
            | ex ->
                solutionRef.contents <- prevSolution
                raise ex
        }

    member this.UpdateDocument(rs, documentPath: OlyPath, sourceText: IOlySourceText, ct: CancellationToken): unit =
        mbp.Post(WorkspaceMessage.UpdateDocumentNoReply(rs, documentPath, sourceText, ct))

    member this.UpdateDocument(rs, documentPath: OlyPath, ct: CancellationToken): unit =
        mbp.Post(WorkspaceMessage.UpdateDocumentNoReplyNoText(rs, documentPath, ct))

    member this.UpdateDocuments(rs, documentPaths: OlyPath imarray, ct: CancellationToken): unit =
        mbp.Post(WorkspaceMessage.UpdateDocumentsNoReplyNoText(rs, documentPaths, ct))

    member this.RemoveProject(rs, projectPath: OlyPath, ct: CancellationToken): unit =
        mbp.Post(WorkspaceMessage.RemoveProject(rs, projectPath, ct))

    member this.UpdateDocumentAsync(rs: OlyWorkspaceResourceSnapshot, documentPath: OlyPath, sourceText: IOlySourceText, ct: CancellationToken): Task<OlyDocument imarray> =
        backgroundTask {
            if not(isNull(rsCheckForUpToDate: obj)) && rsCheckForUpToDate.Version = rs.Version then
                let staleSolution = this.StaleSolution
                let docs = staleSolution.GetDocuments(documentPath)
                let isDocUpToDate = 
                    if docs.IsEmpty then false
                    else
                        docs
                        |> ImArray.forall (fun doc -> doc.GetSourceText(ct).GetHashCode() = sourceText.GetHashCode())
                if isDocUpToDate then
                    OlyTrace.Log($"{nameof(this.UpdateDocumentAsync)} - Using stale results for: {documentPath.ToString()}")
                    return docs
                else
                    return! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.UpdateDocument(rs, documentPath, sourceText, ct, reply))
            else
                return! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.UpdateDocument(rs, documentPath, sourceText, ct, reply))
        }

    member this.GetDocumentsAsync(rs, documentPath: OlyPath, ct: CancellationToken): Task<OlyDocument imarray> =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            return! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.GetDocuments(rs, documentPath, ct, reply))
        }

    member this.GetAllDocumentsAsync(rs, ct: CancellationToken): Task<OlyDocument imarray> =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            return! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.GetAllDocuments(rs, ct, reply))
        }

    member this.BuildProjectAsync(rs, projectPath: OlyPath, ct: CancellationToken) =
        backgroundTask {          
            this.UpdateDocument(rs, projectPath, ct)
            let! solution = this.GetSolutionAsync(rs, ct)
            let proj = solution.GetProject(projectPath)
            let target = proj.SharedBuild

            try
                return! target.BuildProjectAsync(proj, ct)
            with
            | ex ->
                return Error(ImArray.createOne (OlyDiagnostic.CreateError(ex.Message + "\n" + ex.StackTrace.ToString())))
        }

    static member CreateCore(targetPlatforms: OlyBuild seq, progress) =
        let targetPlatforms =
            targetPlatforms
            |> ImArray.ofSeq

        if targetPlatforms.IsEmpty then
            invalidArg (nameof(targetPlatforms)) "No targets were specified. A workspace requires one or more targets."
            
        let defaultTargetPlatform = targetPlatforms.[0]

        let targets =
            targetPlatforms
            |> ImArray.map (fun x -> KeyValuePair(x.PlatformName, x))
            |> ImmutableDictionary.CreateRange

        let preludeDirName = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

        let preludeDir = OlyPath.CreateAbsolute(preludeDirName)
        let preludeDir =
            if preludeDir.IsDirectory then
                preludeDir
            else
                OlyPath.Create(preludeDirName + "/")

        let workspace =
            OlyWorkspace({
                defaultTargetPlatform = defaultTargetPlatform
                targetPlatforms = targets
                preludeDirectory = preludeDir
                progress = progress
            })
        workspace

    member this.ClearSolution(ct) =
        mbp.Post(WorkspaceMessage.ClearSolution(ct))

    member _.CleanAsync() = backgroundTask {
        do! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.Clean(reply))
    }      

    static member Create(targets) =
        let progress =
            { new IOlyWorkspaceProgress with
                member _.OnBeginWork () = 
                    ()
                member _.OnEndWork () = 
                    ()
            }
        OlyWorkspace.Create(targets, progress)

    static member Create(targets, progress: IOlyWorkspaceProgress) =
        let progress =
            { new IOlyWorkspaceProgress with
                member _.OnBeginWork () = 
                    try progress.OnBeginWork() with | _ -> ()

                member _.OnEndWork () =  
                    try progress.OnEndWork() with | _ -> ()
            }
        OlyWorkspace.CreateCore(targets, progress)
