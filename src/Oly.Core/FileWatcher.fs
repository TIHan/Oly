namespace Oly.Core

open System
open System.IO
open System.Collections.Concurrent

[<Sealed>]
type DirectoryWatcher() =

    let fileChanged = Event<string>()
    let fileCreated = Event<string>()
    let fileDeleted = Event<string>()
    let fileRenamed = Event<string>()

    let dirGate = obj()
    let directoryWatchers = ConcurrentDictionary<string * string, FileSystemWatcher * ConcurrentDictionary<string, DateTime>>()
    let addDirectoryWatcher (dir: string) filter =
        // fast route
        match directoryWatchers.TryGetValue((dir, filter)) with
        | true, _ -> ()
        | _ ->
            lock dirGate (fun () ->
                match directoryWatchers.TryGetValue((dir, filter)) with
                | true, _ -> ()
                | _  ->
                    let watcher = new FileSystemWatcher(dir, filter)
                    let files = ConcurrentDictionary()
                    watcher.Changed.Add(fun args -> 
                        let lastWriteTime = File.GetLastWriteTimeUtc(args.FullPath)
                        match files.TryGetValue args.FullPath with
                        | true, dt when dt = lastWriteTime -> ()
                        | _ ->
                            files.[args.FullPath] <- lastWriteTime
                            fileChanged.Trigger(args.FullPath)
                    )
                    watcher.Created.Add(fun args ->
                        fileCreated.Trigger(args.FullPath)
                    )
                    watcher.Deleted.Add(fun args ->
                        fileDeleted.Trigger(args.FullPath)
                    )
                    watcher.Renamed.Add(fun args ->
                        fileRenamed.Trigger(args.FullPath)
                    )
                    directoryWatchers.[(dir, filter)] <- (watcher, files)
                    watcher.EnableRaisingEvents <- true
            )

    member _.Add(directory: string, filter) =
        if Path.IsPathRooted directory |> not then
            failwith "Directory path must be rooted."

        addDirectoryWatcher directory filter

    member _.FileChanged = fileChanged.Publish
    member _.FileCreated = fileCreated.Publish
    member _.FileDeleted = fileDeleted.Publish
    member _.FileRenamed = fileRenamed.Publish

