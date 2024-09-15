namespace Oly.Compiler.Workspace.Service

open System
open System.IO
open System.Collections.Generic
open System.Threading
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

    let invalidate (rs: OlyWorkspaceResourceSnapshot) (filePath: string) =
        let solution = workspace.GetSolutionAsync(rs, CancellationToken.None).Result
        let dir = OlyPath.Create(filePath) |> OlyPath.GetDirectory
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

    let mutable lazyRs = 
        lazy
            let rootPath = getRootPath.Value
            let activeConfigPath =
                Path.Combine(Path.Combine(rootPath.ToString(), WorkspaceStateDirectory), WorkspaceStateFileName)
                |> OlyPath.Create
            let mutable rs = OlyWorkspaceResourceSnapshot.Create(activeConfigPath)
            
            dirWatch.WatchSubdirectories(rootPath.ToString())
            dirWatch.WatchSubdirectories(Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location))
            dirWatch.WatchFiles(OlyPath.GetDirectory(activeConfigPath).ToString(), "*.json")

            Directory.EnumerateFiles(rootPath.ToString(), "*.oly*", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    let filePath = OlyPath.Create(filePath)
                    rs <- rs.SetResourceAsCopy(filePath)
                    if filePath.HasExtension(".olyx") then
                        let _ = workspace.GetDocumentsAsync(rs, filePath, CancellationToken.None)
                        ()
                with
                | _ ->
                    ()
            )

            Directory.EnumerateFiles(Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location), "*.oly*", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    let filePath = OlyPath.Create(filePath)
                    rs <- rs.SetResourceAsCopy(filePath)
                    if filePath.HasExtension(".olyx") then
                        let _ = workspace.GetDocumentsAsync(rs, filePath, CancellationToken.None)
                        ()
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

            Directory.EnumerateFiles(Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location), "*.json", SearchOption.AllDirectories)
            |> Seq.iter (fun filePath ->
                try
                    rs <- rs.SetResourceAsCopy(OlyPath.Create(filePath))
                with
                | _ ->
                    ()
            )

            rs

    let rsObj = obj()
    let mutable rsOpt = None

    let checkValidation filePath =
        match rsOpt with
        | Some rs when (OlyPath.Create(filePath).HasExtension(".oly") || OlyPath.Create(filePath).HasExtension(".olyx")) ->
            invalidate rs filePath
        | _ ->
            ()

    do
        dirWatch.FileCreated.Add(
            fun filePath ->
                let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(OlyPath.Create(filePath)))
                with
                | _ ->
                    ()
                checkValidation filePath
        )

        dirWatch.FileChanged.Add(
            fun filePath ->
                let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(OlyPath.Create(filePath)))
                with
                | _ ->
                    ()
                checkValidation filePath
        )

        dirWatch.FileDeleted.Add(
            fun filePath ->
                let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                rsOpt <- Some(rs.RemoveResource(OlyPath.Create(filePath)))
                checkValidation filePath
        )

        dirWatch.FileRenamed.Add(
            fun (oldFilePath, filePath) ->
                let rs: OlyWorkspaceResourceSnapshot = this.ResourceSnapshot
                let rs = rs.RemoveResource(OlyPath.Create(oldFilePath))
                try
                    rsOpt <- Some(rs.SetResourceAsCopy(OlyPath.Create(filePath)))
                    checkValidation filePath
                with
                | _ ->
                    rsOpt <- Some(rs)
                    checkValidation filePath
        )

    member this.ResourceSnapshot = 
        match rsOpt with
        | None ->
            lock rsObj (fun () ->
                match rsOpt with
                | None ->
                    rsOpt <- Some lazyRs.Value
                    lazyRs <- Unchecked.defaultof<_>
                | _ ->
                    ()
            )
            rsOpt.Value
        | Some rs ->
            rs