namespace Oly.Compiler.Workspace.Service

open System
open System.IO
open System.Collections.Generic
open System.Threading
open System.Reflection
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Workspace

[<Sealed>]
type OlySourceTextManager() =

    let lockObj = obj()
    let openedTexts = Dictionary<OlyPath, IOlySourceText * Nullable<int>>()

    member _.OnOpen(path: OlyPath, version) =
        lock lockObj <| fun _ ->
        let sourceText = OlySourceText.FromFile(path.ToString())
        openedTexts[path] <- sourceText, version

    member _.OnClose(path: OlyPath) =
        lock lockObj <| fun _ ->
        match openedTexts.Remove(path) with
        | _ -> ()

    member _.OnChange(path: OlyPath, version, textChanges: OlyTextChangeWithRange seq) =
        lock lockObj <| fun _ ->
        match openedTexts.TryGetValue(path) with
        | true, (sourceText, _) ->
            let newSourceText = sourceText.ApplyTextChanges(textChanges)
            openedTexts[path] <- newSourceText, version
            Some(newSourceText)
        | _ ->
            None

    member _.TryGet(path: OlyPath) =
        lock lockObj <| fun _ ->
        match openedTexts.TryGetValue(path) with
        | true, sourceText -> Some sourceText
        | _ -> None

    member _.TrySet(path, sourceText) =
        lock lockObj <| fun _ ->
        match openedTexts.TryGetValue(path) with
        | true, _ -> 
            openedTexts[path] <- sourceText
            true
        | _ ->
            false

[<Sealed>]
type OlyWorkspaceListener(workspace: OlyWorkspace, getRootPath: Lazy<OlyPath>) as this =

    [<Literal>]
    let WorkspaceStateDirectory = ".olyworkspace/"

    [<Literal>]
    let WorkspaceStateFileName = "state.json"

    let dirWatch = new DirectoryWatcher()

    let invalidate (rs: OlyWorkspaceResourceSnapshot) (filePath: OlyPath) =
        let solution = workspace.GetSolutionAsync(rs, CancellationToken.None).Result
        let dir = filePath |> OlyPath.GetDirectory
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

    let getActiveConfigPath =
        lazy
            let rootPath = getRootPath.Value
            Path.Combine(Path.Combine(rootPath.ToString(), WorkspaceStateDirectory), WorkspaceStateFileName)
            |> OlyPath.Create

    let mutable lazyRs = 
        lazy
            let rootPath = getRootPath.Value
            let activeConfigPath = getActiveConfigPath.Value

            let mutable rs = OlyWorkspaceResourceSnapshot.Create(activeConfigPath)

            let projectsToUpdate = ImArray.builder()

            Directory.EnumerateFiles(rootPath.ToString(), "*.oly*", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    let filePath = OlyPath.Create(filePath)
                    rs <- rs.SetResourceAsCopy(filePath)
                    if filePath.HasExtension(".olyx") then
                        projectsToUpdate.Add(filePath)
                with
                | _ ->
                    ()
            )

            Directory.EnumerateFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.oly*", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    let filePath = OlyPath.Create(filePath)
                    rs <- rs.SetResourceAsCopy(filePath)
                    if filePath.HasExtension(".olyx") then
                        projectsToUpdate.Add(filePath)
                with
                | _ ->
                    ()
            )

            Directory.EnumerateFiles(rootPath.ToString(), "*.json", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                let filePath = OlyPath.Create(filePath)
                try
                    rs <- rs.SetResourceAsCopy(filePath)
                    // TODO: handle activeconfig
                with
                | _ ->
                    ()
            )

            Directory.EnumerateFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.json", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    rs <- rs.SetResourceAsCopy(OlyPath.Create(filePath))
                with
                | _ ->
                    ()
            )

            workspace.UpdateDocuments(rs, projectsToUpdate.ToImmutable(), CancellationToken.None)

            rs

    let rsObj = obj()
    let mutable rsOpt = None

    let checkValidation (filePath: OlyPath) =
        match rsOpt with
        | Some rs ->
            if (filePath.HasExtension(".oly") || filePath.HasExtension(".olyx")) then
                invalidate rs filePath
            elif OlyPath.Equals(filePath, getActiveConfigPath.Value) then
                workspace.ClearSolution(CancellationToken.None)
        | _ ->
            ()

    do
        dirWatch.FileCreated.Add(
            fun filePath ->
                let filePath = OlyPath.Create(filePath)
                let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(filePath))
                with
                | _ ->
                    ()
                checkValidation filePath
                if filePath.HasExtension(".olyx") then
                    workspace.UpdateDocuments(rs, ImArray.createOne filePath, CancellationToken.None)
                    
        )

        dirWatch.FileChanged.Add(
            fun filePath ->
                let filePath = OlyPath.Create(filePath)
                let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(filePath))
                with
                | _ ->
                    ()
                checkValidation filePath
        )

        dirWatch.FileDeleted.Add(
            fun filePath ->
                let filePath = OlyPath.Create(filePath)
                let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                rsOpt <- Some(rs.RemoveResource(filePath))
                checkValidation filePath
        )

        dirWatch.FileRenamed.Add(
            fun (oldFilePath, filePath) ->
                let oldFilePath = OlyPath.Create(oldFilePath)
                let filePath = OlyPath.Create(filePath)
                let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                let rs = rs.RemoveResource(oldFilePath)
                checkValidation oldFilePath
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(filePath))
                    checkValidation filePath
                    if filePath.HasExtension(".olyx") then
                        workspace.UpdateDocuments(rs, ImArray.createOne filePath, CancellationToken.None)
                with
                | _ ->
                    rsOpt <- Some(rs)
        )

    member this.ResourceSnapshot = 
        match rsOpt with
        | None ->
            lock rsObj (fun () ->
                match rsOpt with
                | None ->
                    rsOpt <- Some lazyRs.Value
                    lazyRs <- Unchecked.defaultof<_>

                    let rootPath = getRootPath.Value
                    let activeConfigPath = getActiveConfigPath.Value

                    dirWatch.WatchSubdirectories(rootPath.ToString())
                    dirWatch.WatchSubdirectories(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location))
                    dirWatch.WatchFiles(OlyPath.GetDirectory(activeConfigPath).ToString(), "*.json")

                | _ ->
                    ()
            )
            rsOpt.Value
        | Some rs ->
            rs