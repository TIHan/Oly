namespace Oly.Core

open System
open System.IO
open System.Text.Json
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks

/// Serializes a type to JSON and stores it in a file.
[<Sealed>]
type JsonFileStore<'T>(path: OlyPath, defaultContents: 'T, dirWatcher: DirectoryWatcher) =

    let mutable isInit = false
    let mutable isDirty = true
    let mutable cachedContents = defaultContents

    let setContents contents ct =
        backgroundTask {
            use fs = File.Open(path.ToString(), FileMode.OpenOrCreate, FileAccess.ReadWrite)
            let jsonOptions = JsonSerializerOptions()
            jsonOptions.PropertyNameCaseInsensitive <- true
            jsonOptions.WriteIndented <- true
            do! JsonSerializer.SerializeAsync(fs, contents, jsonOptions, cancellationToken = ct)
            cachedContents <- contents
            isDirty <- false
        }

    let getContents ct =
        backgroundTask {
            use fs = File.Open(path.ToString(), FileMode.Open, FileAccess.Read)
            let jsonOptions = JsonSerializerOptions()
            jsonOptions.PropertyNameCaseInsensitive <- true
            let! contents = JsonSerializer.DeserializeAsync<'T>(fs, jsonOptions, cancellationToken = ct)
            cachedContents <- contents
            isDirty <- false
            return contents
        }

    let sem = new SemaphoreSlim(1)

    let fileChangedSub = 
        dirWatcher.FileChanged.Subscribe(fun fullPath ->
            let pathThatChanged = OlyPath.Create(fullPath)
            if OlyPath.Equals(pathThatChanged, path) then
                isDirty <- true
        )

    let fileDeletedSub =
        dirWatcher.FileDeleted.Subscribe(fun fullPath ->
            let pathThatChanged = OlyPath.Create(fullPath)
            if OlyPath.Equals(pathThatChanged, path) then
                sem.Wait()
                isInit <- false
                isDirty <- true
                sem.Release() |> ignore
        )

    let fileRenamedSub =
        dirWatcher.FileRenamed.Subscribe(fun (oldFullPath, _) ->
            let pathThatChanged = OlyPath.Create(oldFullPath)
            if OlyPath.Equals(pathThatChanged, path) then
                sem.Wait()
                isInit <- false
                isDirty <- true
                sem.Release() |> ignore
        )

    member private this.Init() =
        if not isInit then
            isInit <- true
            try
                Directory.CreateDirectory(OlyPath.GetDirectory(path).ToString()) |> ignore
            with
            | _ ->
                ()
            dirWatcher.WatchFiles(OlyPath.GetDirectory(path).ToString(), OlyPath.GetFileName(path))

    member this.UpdateContentsAsync(contents: 'T, ct: CancellationToken): Task<'T> =
        backgroundTask {
            ct.ThrowIfCancellationRequested()

            try
                do! sem.WaitAsync()
                try
                    this.Init()
                    do! setContents contents ct
                finally
                    sem.Release() |> ignore

                return contents
            with
            | :? FileNotFoundException ->
                return! this.UpdateContentsAsync(contents, ct)
        }

    member this.GetContentsAsync(ct: CancellationToken): Task<'T> =
        if isDirty then
            backgroundTask {
                ct.ThrowIfCancellationRequested()

                if isDirty then
                    try
                        do! sem.WaitAsync()
                        try
                            this.Init()
                            if isDirty then
                                try
                                    return! getContents ct
                                with
                                | :? FileNotFoundException ->
                                    do! setContents cachedContents ct
                                    return! getContents ct
                            else
                                return cachedContents
                        finally
                            sem.Release() |> ignore
                    with
                    | :? FileNotFoundException ->
                        return! this.GetContentsAsync(ct)
                else
                    return cachedContents
            }
        else
            Task.FromResult(cachedContents)

    interface IDisposable with

        member this.Dispose() =
            fileChangedSub.Dispose()
            fileRenamedSub.Dispose()
            fileDeletedSub.Dispose()
