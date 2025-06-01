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

    let refresh () =
        let rootPath = getRootPath.Value
        let activeConfigPath = getActiveConfigPath.Value

        if not(File.Exists(activeConfigPath.ToString())) then
            let dir = OlyPath.GetDirectory(activeConfigPath)
            if not(Directory.Exists(dir.ToString())) then
                Directory.CreateDirectory(dir.ToString()) |> ignore
            File.WriteAllText(activeConfigPath.ToString(),"""{
    "activeConfiguration": "Debug"
}"""
            )

        let mutable rs = OlyWorkspaceResourceSnapshot.Create(activeConfigPath)

        let projectsToUpdate = ImArray.builder()

        Directory.EnumerateFiles(rootPath.ToString(), "*.oly*", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            rs <- rs.SetResourceAsCopy(filePath)
            if filePath.HasExtension(".olyx") then
                projectsToUpdate.Add(filePath)
        )

        Directory.EnumerateFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.oly*", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            rs <- rs.SetResourceAsCopy(filePath)
            if filePath.HasExtension(".olyx") then
                projectsToUpdate.Add(filePath)
        )

        Directory.EnumerateFiles(rootPath.ToString(), "*.json", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            rs <- rs.SetResourceAsCopy(filePath)
        )

        Directory.EnumerateFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.json", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            rs <- rs.SetResourceAsCopy(OlyPath.CreateAbsolute(filePath))
        )

        workspace.CancelCurrentWork()
        workspace.ClearSolution(CancellationToken.None)
        workspace.UpdateDocuments(rs, projectsToUpdate.ToImmutable(), CancellationToken.None)

        rs

    let mutable lazyRs = lazy refresh()

    let rsObj = obj()
    let mutable rsOpt = None

    let setResourceAsCopy filePath =
        let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
        rsOpt <- Some(rs.SetResourceAsCopy(filePath))

    do

        dirWatch.FileCreated.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    lock rsObj <| fun _ ->
                        setResourceAsCopy filePath
                        if filePath.HasExtension(".olyx") then
                            workspace.UpdateDocuments(rsOpt.Value, ImArray.createOne filePath, CancellationToken.None)
        )

        dirWatch.FileChanged.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    lock rsObj <| fun _ ->
                        setResourceAsCopy filePath
                        if OlyPath.Equals(filePath, getActiveConfigPath.Value) then
                            rsOpt <- Some(refresh())
        )

        dirWatch.FileDeleted.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if not (filePath.ToString().Contains(".olycache")) then
                    lock rsObj <| fun _ ->
                        let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                        rsOpt <-Some(rs.RemoveResource(filePath))
        )

        dirWatch.FileRenamed.Add(
            fun (oldFilePath, filePath) ->
                if not (filePath.ToString().Contains(".olycache")) then
                    lock rsObj <| fun _ ->
                        let oldFilePath = OlyPath.CreateAbsolute(oldFilePath)
                        let filePath = OlyPath.CreateAbsolute(filePath)
                        let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                        rsOpt <- Some(rs.RemoveResource(oldFilePath))
                        setResourceAsCopy filePath
                        if filePath.HasExtension(".olyx") then
                            workspace.RemoveProject(rsOpt.Value, oldFilePath, CancellationToken.None)
                            workspace.UpdateDocuments(rsOpt.Value, ImArray.createOne filePath, CancellationToken.None)
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

                    dirWatch.WatchFiles(rootPath.ToString(), "*.oly")
                    dirWatch.WatchFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.oly")
                    dirWatch.WatchFiles(rootPath.ToString(), "*.olyx")
                    dirWatch.WatchFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.olyx")
                    dirWatch.WatchFiles(OlyPath.GetDirectory(activeConfigPath).ToString(), "*.json")

                | _ ->
                    ()
            )
            rsOpt.Value
        | Some rs ->
            rs

    member _.CleanWorkspace() = backgroundTask {
        Monitor.Enter(rsObj)
        try
            do! workspace.CleanAsync()
            rsOpt <- Some(refresh())
        finally
            Monitor.Exit(rsObj)
    }