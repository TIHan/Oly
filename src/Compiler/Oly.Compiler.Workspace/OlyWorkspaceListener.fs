namespace Oly.Compiler.Workspace

open System
open System.IO
open System.Reflection

open Oly.Core
open Oly.Compiler.Workspace

[<Sealed>]
type OlyWorkspaceListener(workspace: OlyWorkspace) =

    static let isValidFileToListenFor (filePath: OlyPath) =
        not (filePath.ToString().Contains(".oly/cache")) && not (filePath.ToString().Contains(".oly/bin"))      

    let dirWatch = new DirectoryWatcher()

    do
        dirWatch.WatchFiles(workspace.WorkspaceDirectory.ToString(), "*.oly")
        dirWatch.WatchFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.oly")
        dirWatch.WatchFiles(workspace.WorkspaceDirectory.ToString(), "*.olyx")
        dirWatch.WatchFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.olyx")
        dirWatch.WatchFiles(workspace.WorkspaceStateDirectory.ToString(), "*.json")

    do

        dirWatch.FileCreated.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if isValidFileToListenFor filePath then
                    workspace.FileCreated(filePath)
        )

        dirWatch.FileChanged.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if isValidFileToListenFor filePath then
                    workspace.FileChanged(filePath)
        )

        dirWatch.FileDeleted.Add(
            fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                if isValidFileToListenFor filePath then
                    workspace.FileDeleted(filePath)
        )

        dirWatch.FileRenamed.Add(
            fun (oldFilePath, filePath) ->
                let oldFilePath = OlyPath.CreateAbsolute(oldFilePath)
                let newFilePath = OlyPath.CreateAbsolute(filePath)
                if isValidFileToListenFor oldFilePath then
                    workspace.FileDeleted(oldFilePath)
                if isValidFileToListenFor newFilePath then
                    workspace.FileCreated(newFilePath)
        )

    override this.Finalize (): unit = 
        (this: IDisposable).Dispose()

    interface IDisposable with
        member _.Dispose() =
            (dirWatch: IDisposable).Dispose()

    static member GetProjectsFromDirectory(rootPath: OlyPath) =
        let projects = ImArray.builder()

        Directory.EnumerateFiles(rootPath.ToString(), "*.olyx", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            if isValidFileToListenFor filePath then
                projects.Add(filePath)
        )

        Directory.EnumerateFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.olyx", SearchOption.AllDirectories)
        |> Seq.iter (fun filePath ->
            let filePath = OlyPath.CreateAbsolute(filePath)
            if isValidFileToListenFor filePath then
                projects.Add(filePath)
        )

        projects.ToImmutable()
