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

    let refresh() =
        workspace.CancelCurrentWork()

        let rootPath = getRootPath.Value
        let projectsToUpdate = ImArray.builder()

        Directory.EnumerateFiles(rootPath.ToString(), "*.oly*", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            try
                let filePath = OlyPath.Create(filePath)
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
                if filePath.HasExtension(".olyx") then
                    projectsToUpdate.Add(filePath)
            with
            | _ ->
                ()
        )

        match rsOpt with
        | Some rs ->
            workspace.UpdateDocuments(rs, projectsToUpdate.ToImmutable(), CancellationToken.None)
        | _ ->
            ()

    do
        dirWatch.FileCreated.Add(
            fun filePath ->
                let filePath = OlyPath.Create(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                    rsOpt <- Some(rs.SetResourceAsCopy(filePath))
                    refresh()
        )

        dirWatch.FileChanged.Add(
            fun filePath ->
                let filePath = OlyPath.Create(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                    rsOpt <- Some(rs.SetResourceAsCopy(filePath))
        )

        dirWatch.FileDeleted.Add(
            fun filePath ->
                let filePath = OlyPath.Create(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                    rsOpt <- Some(rs.RemoveResource(filePath))
                    refresh()
        )

        dirWatch.FileRenamed.Add(
            fun (oldFilePath, filePath) ->
                if not (filePath.ToString().Contains(".olycache")) then
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

                    dirWatch.WatchFiles(rootPath.ToString(), "*.oly*")
                    dirWatch.WatchFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.oly*")
                    dirWatch.WatchFiles(OlyPath.GetDirectory(activeConfigPath).ToString(), "*.json")

                | _ ->
                    ()
            )
            rsOpt.Value
        | Some rs ->
            rs