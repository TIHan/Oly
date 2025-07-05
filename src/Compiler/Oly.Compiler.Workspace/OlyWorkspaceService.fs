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

    static let isValidFileToListenFor (filePath: OlyPath) =
        not (filePath.ToString().Contains(".olycache")) && not (filePath.ToString().Contains("/bin/"))

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

        let rs, projectsToUpdate = OlyWorkspaceListener.GetProjectsFromDirectory(activeConfigPath, rootPath)
        workspace.CancelCurrentWork()
        workspace.ClearSolution(CancellationToken.None)
        workspace.UpdateDocuments(rs, projectsToUpdate, CancellationToken.None)

        rs

    let mutable lazyRs = lazy refresh()

    let rsObj = obj()
    let mutable rsOpt = None

    let mutable isCleaning = 0

    let setResourceAsCopy filePath =
        let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
        rsOpt <- Some(rs.SetResourceAsCopy(filePath))

    do

        dirWatch.FileCreated.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if isValidFileToListenFor filePath then
                    lock rsObj <| fun _ ->
                        setResourceAsCopy filePath
                        if filePath.HasExtension(".olyx") then
                            workspace.UpdateDocument(rsOpt.Value, filePath, CancellationToken.None)
        )

        dirWatch.FileChanged.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if isValidFileToListenFor filePath then
                    lock rsObj <| fun _ ->
                        setResourceAsCopy filePath
                        if OlyPath.Equals(filePath, getActiveConfigPath.Value) then
                            rsOpt <- Some(refresh())
        )

        dirWatch.FileDeleted.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if isValidFileToListenFor filePath then
                    lock rsObj <| fun _ ->
                        let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                        rsOpt <-Some(rs.RemoveResource(filePath))
        )

        dirWatch.FileRenamed.Add(
            fun (oldFilePath, filePath) ->
                let oldFilePath = OlyPath.CreateAbsolute(oldFilePath)
                let filePath = OlyPath.CreateAbsolute(filePath)
                if isValidFileToListenFor filePath || isValidFileToListenFor oldFilePath then
                    lock rsObj <| fun _ ->
                        if isValidFileToListenFor oldFilePath then
                            let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                            rsOpt <- Some(rs.RemoveResource(oldFilePath))
                        if isValidFileToListenFor filePath then
                            setResourceAsCopy filePath
                if oldFilePath.HasExtension(".olyx") then
                     workspace.RemoveProject(rsOpt.Value, oldFilePath, CancellationToken.None)
                if filePath.HasExtension(".olyx") then
                    workspace.UpdateDocument(rsOpt.Value, filePath, CancellationToken.None)
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

    member _.CleanWorkspace(rs) = backgroundTask {
        if Interlocked.CompareExchange(&isCleaning, 1, 0) = 0 then
            workspace.CancelCurrentWork()
            do! workspace.CleanAsync(rs)
            rsOpt <- lock rsObj (fun () -> Some(refresh()))
            isCleaning <- 0
    }

    static member GetProjectsFromDirectory(activeConfigPath, rootPath: OlyPath) =
        let mutable rs = OlyWorkspaceResourceSnapshot.Create(activeConfigPath)
        let projectsToUpdate = ImArray.builder()

        Directory.EnumerateFiles(rootPath.ToString(), "*.oly*", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            if isValidFileToListenFor filePath then
                rs <- rs.SetResourceAsCopy(filePath)
                if filePath.HasExtension(".olyx") then
                    projectsToUpdate.Add(filePath)
        )

        Directory.EnumerateFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.oly*", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            if isValidFileToListenFor filePath then
                rs <- rs.SetResourceAsCopy(filePath)
                if filePath.HasExtension(".olyx") then
                    projectsToUpdate.Add(filePath)
        )

        Directory.EnumerateFiles(rootPath.ToString(), "*.json", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            if isValidFileToListenFor filePath then
                rs <- rs.SetResourceAsCopy(filePath)
        )

        Directory.EnumerateFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.json", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            if isValidFileToListenFor filePath then
                rs <- rs.SetResourceAsCopy(filePath)
        )

        (rs, projectsToUpdate.ToImmutable())