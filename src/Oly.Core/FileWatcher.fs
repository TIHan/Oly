namespace Oly.Core

open System
open System.IO
open System.Collections.Concurrent

[<CustomEquality;NoComparison>]
type private DirectoryFileKey =
    {
        Path: OlyPath
        Filter: string
    }

    override this.GetHashCode() = this.Path.ToString().GetHashCode()

    override this.Equals(o: obj) =
        match o with
        | :? DirectoryFileKey as o ->
            this.Filter.Equals(o.Filter, StringComparison.OrdinalIgnoreCase) &&
            OlyPath.Equals(this.Path, o.Path)
        | _ ->
            false


[<Sealed>]
type DirectoryWatcher() =

    let directoryDeleted = Event<string>()

    let fileChanged = Event<string>()
    let fileCreated = Event<string>()
    let fileDeleted = Event<string>()
    let fileRenamed = Event<string * string>()

    let dirGate = obj()
    let dirWatchers = ConcurrentDictionary<OlyPath, FileSystemWatcher>()

    let dirFilesGate = obj()
    let dirFilesWatchers = ConcurrentDictionary<DirectoryFileKey, FileSystemWatcher * ConcurrentDictionary<OlyPath, DateTime>>()

    let dirWatcherGate = obj()
    let eventGate = obj()

    let updateLastWriteTime filePath (files: ConcurrentDictionary<OlyPath, DateTime>) =
        try
            let lastWriteTime = File.GetLastWriteTimeUtc(filePath.ToString())
            match files.TryGetValue filePath with
            | true, dt when dt = lastWriteTime -> false
            | _ ->
                files.[filePath] <- lastWriteTime
                true
        with
        | _ ->
            false

    let createDirectoryWatcher dir =
        let watcher = new FileSystemWatcher(dir)
        watcher.NotifyFilter <- NotifyFilters.DirectoryName
        watcher.Deleted.Add(fun args ->
            lock dirWatcherGate (fun () ->
                let path = OlyPath.Create(args.FullPath)
                directoryDeleted.Trigger(path.ToString())

                dirFilesWatchers
                |> ImArray.ofSeq
                |> ImArray.iter (fun pair ->
                    if pair.Key.Path.StartsWith(path) then
                        match pair.Value with
                        | (fsw, files) ->
                            let filePaths = files.Keys |> ImArray.ofSeq

                            filePaths
                            |> ImArray.iter (fun filePath ->
                                files.TryRemove(filePath) |> ignore
                                fileDeleted.Trigger(filePath.ToString())
                            )
                            fsw.Dispose()
                        
                            dirFilesWatchers.TryRemove(pair.Key) |> ignore                         
                )
            )
        )
        watcher.IncludeSubdirectories <- true
        watcher.EnableRaisingEvents <- true
        watcher

    let createWatcher dir filter (files: ConcurrentDictionary<OlyPath, DateTime>) =
        let watcher = new FileSystemWatcher(dir, filter)
        watcher.Changed.Add(fun args -> 
            lock eventGate <| fun () ->
            if (args.ChangeType = WatcherChangeTypes.Changed) then
                let path = OlyPath.Create(args.FullPath)
                if updateLastWriteTime path files then
                    fileChanged.Trigger(path.ToString())
        )
        watcher.Created.Add(fun args ->
            lock eventGate <| fun () ->
            let path = OlyPath.Create(args.FullPath)
            if updateLastWriteTime path files then
                fileCreated.Trigger(path.ToString())
        )
        watcher.Deleted.Add(fun args ->
            lock eventGate <| fun () ->
            let path = OlyPath.Create(args.FullPath)
            let mutable result = Unchecked.defaultof<_>
            if files.TryRemove(path, &result) then
                fileDeleted.Trigger(args.FullPath)
        )
        watcher.Renamed.Add(fun args ->
            lock eventGate <| fun () ->
            let oldPath = OlyPath.Create(args.OldFullPath)
            let path = OlyPath.Create(args.FullPath)
            if updateLastWriteTime (OlyPath.Create(args.OldFullPath)) files || updateLastWriteTime (OlyPath.Create(args.FullPath)) files then
                fileRenamed.Trigger(oldPath.ToString(), path.ToString())
        )
        watcher.IncludeSubdirectories <- true
        watcher.EnableRaisingEvents <- true

        Directory.EnumerateFiles(dir, filter)
        |> Seq.iter (fun filePath ->
            updateLastWriteTime (OlyPath.Create(filePath)) files |> ignore
        )      

        watcher

    member _.WatchSubdirectories(dir: string) =
        if Path.IsPathRooted dir |> not then
            invalidArg (nameof(dir)) "Directory path must be rooted."

        let dir = OlyPath.Create(dir)

        // fast route
        match dirWatchers.TryGetValue(dir) with
        | true, _ -> ()
        | _ ->
            lock dirGate (fun () ->
                match dirWatchers.TryGetValue(dir) with
                | true, _ -> ()
                | _  ->
                    let watcher = createDirectoryWatcher (dir.ToString())
                    dirWatchers.[dir] <- watcher
            )        

    member _.WatchFiles(dir: string, filter) =
        if Path.IsPathRooted dir |> not then
            invalidArg (nameof(dir)) "Directory path must be rooted."

        if String.IsNullOrWhiteSpace(filter) then
            invalidArg (nameof(filter)) "Cannot be null or white-space."

        let dir = OlyPath.Create(dir)

        let key = { Path = dir; Filter = filter }

        // fast route
        match dirFilesWatchers.TryGetValue(key) with
        | true, _ -> ()
        | _ ->
            lock dirFilesGate (fun () ->
                match dirFilesWatchers.TryGetValue(key) with
                | true, _ -> ()
                | _  ->
                    let files = ConcurrentDictionary(OlyPathEqualityComparer.Instance)
                    let watcher = createWatcher (dir.ToString()) filter files
                    dirFilesWatchers.[key] <- (watcher, files)
            )

    member _.FileChanged = fileChanged.Publish
    member _.FileCreated = fileCreated.Publish
    member _.FileDeleted = fileDeleted.Publish
    member _.FileRenamed = fileRenamed.Publish

    member _.DirectoryDeleted = directoryDeleted.Publish

    interface IDisposable with
        member _.Dispose() =
            dirWatchers.Values
            |> Seq.iter (fun fsw -> fsw.Dispose())
            dirFilesWatchers.Values
            |> Seq.iter (fun (fsw, _) -> fsw.Dispose())
