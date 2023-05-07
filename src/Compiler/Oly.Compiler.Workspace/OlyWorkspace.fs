namespace rec Oly.Compiler.Workspace

open Oly.Core
open Oly.Core.TaskExtensions
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Collections.Immutable

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
type OlyReferenceInfo(path: OlyPath, textSpan: OlyTextSpan) =

    member _.Path = path
    member _.TextSpan = textSpan

[<Sealed>]
type OlyPackageInfo(text: string, textSpan: OlyTextSpan) =

    member _.Text = text
    member _.TextSpan = textSpan

[<Sealed>]
type OlyCopyFileInfo(path: OlyPath, textSpan: OlyTextSpan) =

    member _.Path = path
    member _.TextSpan = textSpan

[<Sealed>]
type OlyReferenceResolutionInfo(paths: OlyPath imarray, diags: OlyDiagnostic imarray) = 

    member _.Paths = paths
    member _.Diagnostics = diags

[<RequireQualifiedAccess>]
type OlyOutputKind =
    | Library
    | Executable

[<Sealed>]
type OlyTargetInfo(name: string, outputKind: OlyOutputKind, implicitExtendsForStructOpt: string option, implicitExtendsForEnumOpt: string option) =

    member _.Name = name
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

    let relativeCacheDir = OlyPath.Create($"{CacheDirectoryName}/{platformName}/")
    let relativeBinDir = OlyPath.Create($"{BinDirectoryName}/{platformName}/")

    member _.PlatformName = platformName

    member _.GetAbsoluteCacheDirectory(absolutePath: OlyPath) =
        if absolutePath.IsFile then
            let fileName = OlyPath.GetFileName(absolutePath)
            let dir = OlyPath.GetDirectory(absolutePath)
            OlyPath.Combine(dir, OlyPath.Combine(relativeCacheDir, fileName + "/"))
        else
            OlyPath.Combine(absolutePath, relativeCacheDir)

    member _.GetAbsoluteBinDirectory(absolutePath: OlyPath) =
        if absolutePath.IsFile then
            let fileName = OlyPath.GetFileName(absolutePath)
            let dir = OlyPath.GetDirectory(absolutePath)
            OlyPath.Combine(dir, OlyPath.Combine(relativeBinDir, fileName + "/"))
        else
            OlyPath.Combine(absolutePath, relativeBinDir)

    abstract IsValidTargetName : targetInfo: OlyTargetInfo -> bool

    abstract ResolveReferencesAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * referenceInfos: OlyReferenceInfo imarray * packageInfos: OlyPackageInfo imarray * ct: CancellationToken -> Task<OlyReferenceResolutionInfo>

    abstract CanImportReference : path: OlyPath -> bool

    abstract ImportReferenceAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * path: OlyPath * ct: CancellationToken -> Task<Result<OlyImportedReference option, string>>

    abstract OnBeforeReferencesImportedAsync : projPath: OlyPath * targetInfo: OlyTargetInfo * ct: CancellationToken -> Task<unit>
    
    abstract OnAfterReferencesImported : unit -> unit

    abstract BuildProjectAsync : proj: OlyProject * ct: CancellationToken -> Task<Result<string, string>>

    abstract GetImplicitExtendsForStruct: unit -> string option
    default _.GetImplicitExtendsForStruct() = None

    abstract GetImplicitExtendsForEnum: unit -> string option
    default _.GetImplicitExtendsForEnum() = None

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
type OlyDocument(newProjectLazy: OlyProject Lazy, documentPath: OlyPath, syntaxTree: OlySyntaxTree) =

    let mutable boundModel = ValueNone
    let mutable extraDiags = ValueNone
    
    member _.Path = documentPath

    member _.Project = newProjectLazy.Value

    member _.SyntaxTree = syntaxTree

    member _.GetSourceText(ct) = syntaxTree.GetSourceText(ct)

    member this.BoundModel =
        getInlineCache &boundModel (fun () ->
            this.Project.Compilation.GetBoundModel(documentPath)
        )

    member this.ExtraDiagnostics =
        getInlineCache &extraDiags (fun () ->
            this.Project.Compilation.GetExtraDiagnostics(documentPath)
        )

    member this.GetDiagnostics(ct) =
        let diags1 = syntaxTree.GetDiagnostics(ct)
        let diags2 = this.BoundModel.GetDiagnostics(ct)
        diags1.AddRange(diags2).AddRange(this.ExtraDiagnostics)

    member this.IsProjectDocument =
        documentPath.HasExtension(ProjectExtension)

[<Sealed>]
type OlyProjectConfiguration(name: string, defines: string imarray, debuggable: bool) =

    member _.Name = name

    member _.Defines = defines

    member _.Debuggable = debuggable

[<Sealed>]
type OlyProject (
    solution: OlySolution Lazy, 
    projPath: OlyPath,
    projName: string,
    projConfig: OlyProjectConfiguration,
    compilation: OlyCompilation, 
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
    member _.SharedBuild =
        solution.Value.State.workspace.GetBuild(projPath)
    
    member _.Solution = solution.Value
    member _.Compilation: OlyCompilation = compilation
    member _.Documents =
        getInlineCache &documentList (fun () ->
            documents.Values.ToImmutableArray()
        )
    member _.References = references
    member _.Name = projName
    member _.Path = projPath
    member _.Configuration = projConfig
    member _.Packages = packages
    member _.CopyFileInfos = copyFileInfos

    member val AsCompilationReference = OlyCompilationReference.Create(projPath, (fun () -> compilation))

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
        let newDocument = OlyDocument(newProjectLazy, documentPath, syntaxTree)
        let newCompilation = compilation.SetSyntaxTree(newDocument.SyntaxTree).SetExtraDiagnostics(newDocument.Path, extraDiagnostics)

        let newDocuments = 
            documents.SetItem(documentPath, newDocument).Values
            |> Seq.map (fun document ->
                KeyValuePair(document.Path, OlyDocument(newProjectLazy, document.Path, document.SyntaxTree))
            )
            |> ImmutableDictionary.CreateRange

        newProject <- OlyProject(newSolutionLazy, projPath, projName, projConfig, newCompilation, newDocuments, references, packages, copyFileInfos, platformName, targetInfo)
        newProjectLazy.Force() |> ignore
        newProject, newDocument

    member this.RemoveDocument(newSolutionLazy, documentPath: OlyPath) =
        if not (documents.ContainsKey(documentPath)) then
            failwithf "Unable to find document '%A'." documentPath

        let document = documents.[documentPath]

        let mutable newProject = this
        let newProjectLazy = lazy newProject
        let newCompilation = compilation.RemoveSyntaxTree(document.SyntaxTree.Path)
        let newDocuments = 
            documents.Remove(document.Path).Values
            |> Seq.map (fun document ->
                KeyValuePair(document.Path, OlyDocument(newProjectLazy, document.Path, document.SyntaxTree))
            )
            |> ImmutableDictionary.CreateRange

        newProject <- OlyProject(newSolutionLazy, projPath, projName, projConfig, newCompilation, newDocuments, references, packages, copyFileInfos, platformName, targetInfo)
        newProjectLazy.Force() |> ignore
        newProject

    member this.UpdateReferences(newSolutionLazy, projectReferences: OlyProjectReference imarray, ct) =
        let transitiveReferences = getTransitiveCompilationReferences this.Solution projectReferences ct
        let mutable newProject = this
        let newProjectLazy = lazy newProject
        let newCompilation = compilation.Update(references = transitiveReferences)

        let newDocuments = 
            documents.Values
            |> Seq.map (fun document ->
                KeyValuePair(document.Path, OlyDocument(newProjectLazy, document.Path, document.SyntaxTree))
            )
            |> ImmutableDictionary.CreateRange

        newProject <- OlyProject(newSolutionLazy, projPath, projName, projConfig, newCompilation, newDocuments, projectReferences, packages, copyFileInfos, platformName, targetInfo)
        newProjectLazy.Force() |> ignore
        newProject

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
                        match r with
                        | OlyProjectReference.Project(projectId) ->
                            if h.Add(projectId.ToString()) then
                                match solution.TryGetProject projectId with
                                | Some refProj -> 
                                    let compRef = refProj.AsCompilationReference
                                    builder.Add(compRef)
                                    loop true refProj.References
                                | _ -> 
                                    OlyAssert.Fail("Unable to find project.")
                        | OlyProjectReference.Compilation(r, _) ->
                            if h.Add(r.Path.ToString()) then
                                builder.Add(r)
                )
            loop false references
            builder.ToImmutable()
        transitiveReferences

    let getTransitiveProjectReferences (solution: OlySolution) references (ct: CancellationToken) =
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
                                let compRef = refProj.AsCompilationReference
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
            (solution: OlySolution) 
            projectId 
            projectName 
            (projectConfig: OlyProjectConfiguration) 
            (documents: OlyDocument imarray) 
            (projectReferences: OlyProjectReference imarray) 
            (packages: OlyPackageInfo imarray)
            (copyFiles: OlyCopyFileInfo imarray)
            platformName 
            (targetInfo: OlyTargetInfo)
            ct =
        let isDebuggable = projectConfig.Debuggable

        let syntaxTrees = getSyntaxTrees documents
        let transitiveReferences = getTransitiveCompilationReferences solution projectReferences ct
        let options = { Debuggable = isDebuggable; Parallel = true; Executable = targetInfo.IsExecutable; ImplicitExtendsForStruct = targetInfo.ImplicitExtendsForStruct; ImplicitExtendsForEnum = targetInfo.ImplicitExtendsForEnum }
        let compilation = OlyCompilation.Create(projectName, syntaxTrees, references = transitiveReferences, options = options)
        let documents =
            documents
            |> ImArray.map (fun x -> KeyValuePair(x.Path, x))
            |> ImmutableDictionary.CreateRange
        OlyProject(newSolution, projectId, projectName, projectConfig, compilation, documents, projectReferences, packages, copyFiles, platformName, targetInfo)   

    let updateProject (newSolutionLazy: OlySolution Lazy) (project: OlyProject) =
        let mutable project = project
        let newProjectLazy = lazy project
        let newDocuments = 
            project.DocumentLookup.Values
            |> Seq.map (fun document ->
                KeyValuePair(document.Path, OlyDocument(newProjectLazy, document.Path, document.SyntaxTree))
            )
            |> ImmutableDictionary.CreateRange
        project <- OlyProject(newSolutionLazy, project.Path, project.Name, project.Configuration, project.Compilation, newDocuments, project.References, project.Packages, project.CopyFileInfos, project.PlatformName, project.TargetInfo)
        newProjectLazy.Force() |> ignore
        project

    let updateSolution (newSolution: OlySolution) newSolutionLazy =
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

    member this.State = state

    member this.Version = state.version

    member this.InternalSetProject(project: OlyProject) =
        { state with
            projects = state.projects.SetItem(project.Path, project)
        }
        |> OlySolution

    member this.HasProject(projectPath: OlyPath): bool =
        state.projects.ContainsKey(projectPath)

    member this.TryGetProject(projectPath: OlyPath): OlyProject option =
        match state.projects.TryGetValue projectPath with
        | true, project -> Some project
        | _ -> None

    member this.GetProject(projectPath: OlyPath) =
        match this.TryGetProject(projectPath) with
        | None -> failwithf "Unable to find project '%A'." projectPath
        | Some project -> project

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

    member this.CreateProject(projectPath, projectConfig, platformName, targetInfo, packages, copyFileInfos, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        let projectName = OlyPath.GetFileNameWithoutExtension(projectPath)
        let mutable newSolution = this
        let newSolutionLazy = lazy newSolution
        let newProject = createProject newSolutionLazy this projectPath projectName projectConfig ImArray.empty ImArray.empty packages copyFileInfos platformName targetInfo ct
        newSolution <- this.InternalSetProject(newProject)
        newSolution <- updateSolution newSolution newSolutionLazy
        newSolutionLazy.Force() |> ignore
        newSolution, newProject

    member this.CreateProject(projectPath, projectConfig, platformName, targetInfo, ct: CancellationToken) =
        this.CreateProject(projectPath, projectConfig, platformName, targetInfo, ImArray.empty, ImArray.empty, ct)

    member this.UpdateDocument(projectPath: OlyPath, documentPath, syntaxTree: OlySyntaxTree, extraDiagnostics: OlyDiagnostic imarray) =
        let project = this.GetProject(projectPath)
        let mutable newSolution = this
        let newSolutionLazy = lazy newSolution
        let newProject, newDocument = project.UpdateDocument(newSolutionLazy, documentPath, syntaxTree, extraDiagnostics)
        let newProjects = state.projects.SetItem(newProject.Path, newProject)

        newSolution <- { state with projects = newProjects } |> OlySolution
        newSolution <- updateSolution newSolution newSolutionLazy
        newSolutionLazy.Force() |> ignore
        newSolution, newProject, newDocument

    member this.RemoveDocument(projectPath, documentPath) =
        if OlyPath.Equals(projectPath, documentPath) then
            this.RemoveProject(projectPath)
        else
            let project = this.GetProject(projectPath)
            let mutable newSolution = this
            let newSolutionLazy = lazy newSolution
            let newProject = project.RemoveDocument(newSolutionLazy, documentPath)
            let newProjects = state.projects.SetItem(newProject.Path, newProject)
            newSolution <- { state with projects = newProjects } |> OlySolution
            newSolution <- updateSolution newSolution newSolutionLazy
            newSolutionLazy.Force() |> ignore
            newSolution

    member this.RemoveProject(projectPath) =
        match this.TryGetProject(projectPath) with
        | Some project ->
            let projectsToRemove = 
                this.GetProjectsDependentOnReference(projectPath).Add(project)
                |> ImArray.map (fun x -> x.Path)

            let mutable newSolution = this
            let newSolutionLazy = lazy newSolution
            let newProjects = state.projects.RemoveRange(projectsToRemove)
            newSolution <- { state with projects = newProjects } |> OlySolution
            newSolution <- updateSolution newSolution newSolutionLazy
            newSolutionLazy.Force() |> ignore
            newSolution
        | _ ->
            this

    member this.UpdateReferences(projectPath, projectReferences: OlyProjectReference imarray, ct) =
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

type IOlyWorkspaceResourceService =

    abstract LoadSourceText: filePath: OlyPath -> IOlySourceText

    abstract GetTimeStamp: filePath: OlyPath -> DateTime

    abstract FindSubPaths: dirPath: OlyPath -> OlyPath imarray

    abstract LoadProjectConfigurationAsync: projectConfigPath: OlyPath * ct: CancellationToken -> Task<OlyProjectConfiguration>

[<Sealed>]
type OlyDefaultWorkspaceResourceService() =

    interface IOlyWorkspaceResourceService with

        member _.LoadSourceText(filePath) =
            OlySourceText.FromFile(filePath.ToString())

        member _.GetTimeStamp(filePath) =
            System.IO.File.GetLastWriteTimeUtc(filePath.ToString())

        member _.FindSubPaths(dirPath) =
            try
                System.IO.Directory.EnumerateFiles(dirPath.ToString())
                |> Seq.map OlyPath.Create
                |> ImArray.ofSeq
            with
            | _ ->
                ImArray.empty

        member _.LoadProjectConfigurationAsync(_projectFilePath: OlyPath, ct: CancellationToken) =
            backgroundTask {
                ct.ThrowIfCancellationRequested()
                return OlyProjectConfiguration(String.Empty, ImArray.empty, false)
            }

[<NoComparison;NoEquality>]
type private WorkspaceState =
    {
        defaultTargetPlatform: OlyBuild
        targetPlatforms: ImmutableDictionary<string, OlyBuild>
        rs: IOlyWorkspaceResourceService
    }

[<NoComparison;NoEquality>]
type WorkspaceMessage =
    | UpdateDocumentNoReply of documentPath: OlyPath * sourceText: IOlySourceText * ct: CancellationToken
    | UpdateDocument of documentPath: OlyPath * sourceText: IOlySourceText * ct: CancellationToken * AsyncReplyChannel<OlyDocument imarray>
    | GetDocuments of documentPath: OlyPath * ct: CancellationToken * AsyncReplyChannel<OlyDocument imarray>
    | InvalidateProject of projectPath: OlyPath * ct: CancellationToken
    | ClearSolution of ct: CancellationToken * AsyncReplyChannel<unit>

[<Sealed>]
type OlyWorkspace private (state: WorkspaceState) as this =

    static let getStortedPaths (rs: IOlyWorkspaceResourceService) absoluteDir (paths: (OlyTextSpan * OlyPath) seq) =
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

    let mutable solution = 
        {
            workspace = this
            projects = ImmutableDictionary.Empty
            version = 0UL
        }
        |> OlySolution

    let mbp = new MailboxProcessor<WorkspaceMessage>(fun mbp ->
        let rec loop() =
            async {
                match! mbp.Receive() with
                | InvalidateProject(projectPath, ct) ->
                    try
                        ct.ThrowIfCancellationRequested()
                        solution <- solution.RemoveProject(projectPath)
                    with
                    | _ ->
                        ()
                | ClearSolution(ct, reply) ->
                    try
                        try
                            ct.ThrowIfCancellationRequested()
                            solution <- 
                                {
                                    workspace = this
                                    projects = ImmutableDictionary.Empty
                                    version = 0UL
                                }
                                |> OlySolution
                        with
                        | _ ->
                            ()
                    finally
                        reply.Reply(())
                | UpdateDocumentNoReply(documentPath, sourceText, ct) ->
                    try
                        ct.ThrowIfCancellationRequested()
                        do! this.UpdateDocumentAsyncCore(documentPath, sourceText, ct) |> Async.AwaitTask
                        solution.GetDocuments(documentPath)
                        |> ImArray.iter (fun doc ->
                            solution.GetProjectsDependentOnReference(doc.Project.Path)
                            |> ImArray.iter (fun proj ->
                                solution <- solution.RemoveProject(proj.Path)
                            )
                        )
                    with
                    | _ ->
                        ()
                | UpdateDocument(documentPath, sourceText, ct, reply) ->
                    try
                        ct.ThrowIfCancellationRequested()
                        do! this.UpdateDocumentAsyncCore(documentPath, sourceText, ct) |> Async.AwaitTask
                        let docs = this.Solution.GetDocuments(documentPath)
                        reply.Reply(docs)
                        docs
                        |> ImArray.iter (fun doc ->
                            solution.GetProjectsDependentOnReference(doc.Project.Path)
                            |> ImArray.iter (fun proj ->
                                solution <- solution.RemoveProject(proj.Path)
                            )
                        )
                    with
                    | _ ->
                        reply.Reply(ImArray.empty)
                | GetDocuments(documentPath, ct, reply) ->
                    try
                        ct.ThrowIfCancellationRequested()
                        let docs = this.Solution.GetDocuments(documentPath)
                        reply.Reply(docs)
                    with
                    | _ ->
                        reply.Reply(ImArray.empty)

                return! loop()
            }
        loop()
    )

    do
        mbp.Start()

    member _.Solution: OlySolution = solution

    member this.GetBuild(projPath: OlyPath) =   
        let solution = solution
        let project = solution.GetProject(projPath)
        state.targetPlatforms.[project.PlatformName]

    static member private LoadProjectConfigurationAsync(rs: IOlyWorkspaceResourceService, projPath: OlyPath, ct) =
        let projConfigPath = OlyPath.ChangeExtension(projPath, ProjectConfigurationExtension)
        rs.LoadProjectConfigurationAsync(projConfigPath, ct)

    static member private ReloadProjectAsync(state: WorkspaceState, solution: OlySolution, syntaxTree: OlySyntaxTree, projPath: OlyPath, projConfig: OlyProjectConfiguration, ct: CancellationToken) =
        backgroundTask {
            if syntaxTree.ParsingOptions.CompilationUnitConfigurationEnabled |> not then
                failwith "Unable to load project: Compilation unit configuration must be enabled."

            let filePath = syntaxTree.Path
            if filePath.HasExtension(ProjectExtension) |> not then
                failwithf "Invalid project file path '%A'" filePath

            let diags = ImArray.builder ()

            let absoluteDir = OlyPath.GetDirectory(filePath)

            let config = syntaxTree.GetCompilationUnitConfiguration(ct)
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
                OlyTargetInfo(targetName, outputKind, targetPlatform.GetImplicitExtendsForStruct(), targetPlatform.GetImplicitExtendsForEnum())

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
                getSortedReferencesFromConfig state.rs absoluteDir config
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

            let projectReferencesInWorkspace = ImArray.builder()
            let mutable solution = solution
            for info in olyxReferenceInfos do
                try
                    let! result = OlyWorkspace.UpdateDocumentAsyncCore(state, solution, info.Path, state.rs.LoadSourceText(info.Path), ct)
                    solution <- result
                    match solution.TryGetProject(info.Path) with
                    | Some proj ->
                        projectReferencesInWorkspace.Add(OlyProjectReference.Project(proj.Path))
                    | _ ->
                        diags.Add(OlyDiagnostic.CreateError($"Cannot reference Oly project '{info.Path}' as it does not exist in the current workspace.", OlySourceLocation.Create(info.TextSpan, syntaxTree)))
                with
                | ex ->
                    diags.Add(OlyDiagnostic.CreateError($"Cannot reference Oly project '{info.Path}'. Internal Error: {ex.Message}", OlySourceLocation.Create(info.TextSpan, syntaxTree)))
            let projectReferencesInWorkspace = projectReferencesInWorkspace.ToImmutable()

            let transitivePackageInfos =
                getTransitivePackages solution projectReferencesInWorkspace ct

            let combinedPackageInfos =
                // TODO: What happens if we have multiple of the same packages with different versions? Just kick it down to the target platform resolution?
                // TODO: Wrong text-span for the transitive package infos, we will need to fix that. Perhaps OlyPackageInfo should use a source location instead of a text-span.
                ImArray.append transitivePackageInfos packageInfos
                |> Seq.distinctBy (fun x -> x.Text)
                |> ImArray.ofSeq

            let referenceInfos =
                referenceInfos
                |> ImArray.filter (fun x -> 
                    not(x.Path.HasExtension(".olyx")) &&
                    not(x.Path.HasExtension(".oly"))
                )
            
            // TODO: We should include the transitive references to make it consistent with the packages.
            let! resInfo = targetPlatform.ResolveReferencesAsync(projPath, targetInfo, referenceInfos, combinedPackageInfos, ct)

            resInfo.Diagnostics
            |> ImArray.iter diags.Add

            let resolvedReferences =
                resInfo.Paths
                |> ImArray.map (fun x -> (OlyTextSpan.Create(0, 0), x))
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

            let projectReferences = ImArray.append projectReferences projectReferencesInWorkspace

            try
                targetPlatform.OnAfterReferencesImported()
            with
            | ex ->
                diags.Add(OlyDiagnostic.CreateError(ex.Message))

            let solution, _ = solution.CreateProject(projPath, projConfig, platformName, targetInfo, packageInfos, copyFileInfos, ct)

            let loads = getSortedLoadsFromConfig state.rs absoluteDir config

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
                                let sourceText = state.rs.LoadSourceText(path)
                                let parsingOptions = { OlyParsingOptions.Default with ConditionalDefines = projConfig.Defines }
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

    static member private UpdateProjectAsync(state: WorkspaceState, solution: OlySolution, syntaxTree: OlySyntaxTree, projPath: OlyPath, projConfig: OlyProjectConfiguration, ct) =
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
                    let! result = OlyWorkspace.ReloadProjectAsync(state, solution, syntaxTree, projPath, projConfig, ct)
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
                        let! result = OlyWorkspace.ReloadProjectAsync(state, solution, syntaxTree, projPath, projConfig, ct)
                        return Some result
            | _ -> 
                let! result = OlyWorkspace.ReloadProjectAsync(state, solution, syntaxTree, projPath, projConfig, ct)
                return Some result
        }

    static member private UpdateDocumentAsyncCore(state, solution: OlySolution, documentPath: OlyPath, sourceText: IOlySourceText, ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            let docs = solution.GetDocuments(documentPath)

            if docs.IsEmpty then
                if documentPath.HasExtension(ProjectExtension) then

                    // Handle project config for syntax tree
                    let projPath = documentPath
                    let! projConfig = OlyWorkspace.LoadProjectConfigurationAsync(state.rs, projPath, ct)
                    let parsingOptions = 
                        { OlyParsingOptions.CompilationUnitConfigurationEnabled = true
                          OlyParsingOptions.AnonymousModuleDefinitionAllowed = true 
                          OlyParsingOptions.ConditionalDefines = projConfig.Defines }

                    let syntaxTree = OlySyntaxTree.Parse(projPath, sourceText, parsingOptions)
                    match! OlyWorkspace.UpdateProjectAsync(state, solution, syntaxTree, documentPath, projConfig, ct) with
                    | Some solution -> return solution
                    | _ -> return solution
                else
                    return solution
            else
                let mutable solutionResult = solution
                for i = 0 to docs.Length - 1 do
                    ct.ThrowIfCancellationRequested()
                    let doc = docs[i]
                    if doc.GetSourceText(ct).GetHashCode() <> sourceText.GetHashCode() then
                        let syntaxTree = doc.SyntaxTree.ApplySourceText(sourceText)
                        if doc.IsProjectDocument then

                            // Handle project config for syntax tree
                            let projPath = doc.Path
                            let! projConfig = OlyWorkspace.LoadProjectConfigurationAsync(state.rs, projPath, ct)
                            let parsingOptions = 
                                { OlyParsingOptions.CompilationUnitConfigurationEnabled = true
                                  OlyParsingOptions.AnonymousModuleDefinitionAllowed = true 
                                  OlyParsingOptions.ConditionalDefines = projConfig.Defines }

                            let syntaxTree =
                                if OlyParsingOptions.AreEqual(syntaxTree.ParsingOptions, parsingOptions) then
                                    syntaxTree
                                else
                                    OlySyntaxTree.Parse(projPath, sourceText, parsingOptions)

                            match! OlyWorkspace.UpdateProjectAsync(state, solutionResult, syntaxTree, projPath, projConfig, ct) with
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

    member this.UpdateDocumentAsyncCore(documentPath: OlyPath, sourceText: IOlySourceText, ct: CancellationToken): Task<unit> =
        backgroundTask {
            let prevSolution = solution
            try
                ct.ThrowIfCancellationRequested()
                let! newSolution = OlyWorkspace.UpdateDocumentAsyncCore(state, prevSolution, documentPath, sourceText, ct)
                solution <- newSolution
            with
            | ex ->
                solution <- prevSolution
                raise ex
        }

    member this.UpdateDocument(documentPath: OlyPath, sourceText: IOlySourceText, ct: CancellationToken): unit =
        mbp.Post(WorkspaceMessage.UpdateDocumentNoReply(documentPath, sourceText, ct))

    member this.InvalidateProject(projectPath: OlyPath, ct: CancellationToken): unit =
        mbp.Post(WorkspaceMessage.InvalidateProject(projectPath, ct))

    member this.UpdateDocumentAsync(documentPath: OlyPath, sourceText: IOlySourceText, ct: CancellationToken): Task<OlyDocument imarray> =
        backgroundTask {
            return! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.UpdateDocument(documentPath, sourceText, ct, reply))
        }

    member this.GetDocumentsAsync(documentPath: OlyPath, ct: CancellationToken): Task<OlyDocument imarray> =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            return! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.GetDocuments(documentPath, ct, reply))
        }

    member this.BuildProjectAsync(projectPath: OlyPath, ct: CancellationToken) =
        let solution = this.Solution
        backgroundTask {          
            let proj = solution.GetProject(projectPath)
            let target = proj.SharedBuild
            try
                return! target.BuildProjectAsync(proj, ct)
            with
            | ex ->
                return Error(ex.Message)
        }

    static member CreateCore(targetPlatforms: OlyBuild seq, rs: IOlyWorkspaceResourceService) =
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

        let workspace =
            OlyWorkspace({
                defaultTargetPlatform = defaultTargetPlatform
                targetPlatforms = targets
                rs = rs
            })
        workspace

    member this.ClearSolutionAsync(ct) =
        backgroundTask {
            return! mbp.PostAndAsyncReply(fun reply -> WorkspaceMessage.ClearSolution(ct, reply))
        }

    static member Create(targets, ?rs: IOlyWorkspaceResourceService) =
        let rs = defaultArg rs (OlyDefaultWorkspaceResourceService())
        OlyWorkspace.CreateCore(targets, rs)

