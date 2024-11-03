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

    let getActiveConfigPath =
        lazy
            let rootPath = getRootPath.Value
            Path.Combine(Path.Combine(rootPath.ToString(), WorkspaceStateDirectory), WorkspaceStateFileName)
            |> OlyPath.CreateAbsolute

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
        // TODO: Handle state.json

        dirWatch.FileCreated.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                    try
                        rsOpt <- Some(rs.SetResourceAsCopy(filePath))
                        refresh()
                    with
                    | _ ->
                        rsOpt <- Some(rs)
        )

        dirWatch.FileChanged.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                    try
                        rsOpt <- Some(rs.SetResourceAsCopy(filePath))
                        refresh()
                    with
                    | _ ->
                        rsOpt <- Some(rs)
        )

        dirWatch.FileDeleted.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                    try
                        rsOpt <- Some(rs.RemoveResource(filePath))
                        refresh()
                    with
                    | _ ->
                        rsOpt <- Some(rs)
        )

        dirWatch.FileRenamed.Add(
            fun (oldFilePath, filePath) ->
                if not (filePath.ToString().Contains(".olycache")) then
                    let oldFilePath = OlyPath.CreateAbsolute(oldFilePath)
                    let filePath = OlyPath.CreateAbsolute(filePath)
                    let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                    try
                        let rs = rs.RemoveResource(oldFilePath)
                        rsOpt <- Some(rs.SetResourceAsCopy(filePath))
                        refresh()
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