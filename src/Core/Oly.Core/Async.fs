module Oly.Core.TaskExtensions

open System
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module Seq =

    let concatAsync (collections: Async<'T> seq) : Async<'T seq> =
        async {
            let output = ResizeArray()
            for item in collections do
                let! result = item
                output.Add(result)
            return output :> _ seq
        }

    let filterAsync (predicate: 'T -> Async<bool>) (collection: 'T seq) : Async<'T seq> =
        async {
            let output = ResizeArray()
            for item in collection do
                let! result = predicate item
                if result then
                    output.Add(item)
            return output :> _ seq
        }

[<RequireQualifiedAccess>]
module ImArray =

    let filterAsync (predicate: 'T -> Async<bool>) (collection: 'T imarray) : Async<'T imarray> =
        async {
            let output = ImArray.builder()
            for item in collection do
                let! result = predicate item
                if result then
                    output.Add(item)
            return output.ToImmutable()
        }

    let chooseAsync (chooser: 'T -> Async<'U option>) (collection: 'T imarray) : Async<'U imarray> =
        async {
            let output = ImArray.builder()
            for item in collection do
                match! chooser item with
                | Some result ->
                    output.Add(result)
                | _ ->
                    ()
            return output.ToImmutable()
        }

    let mapAsync (mapper: 'T -> Async<'U>) (collection: 'T imarray) : Async<'U imarray> =
        async {
            let output = ImArray.builderWithSize collection.Length
            for item in collection do
                let! result = mapper item
                output.Add(result)
            return output.MoveToImmutable()
        }

[<RequireQualifiedAccess>]
module AsyncSeq =

    let init count (initializer: int -> Async<'T>) : Async<'T seq> =
        async {
            let output = ResizeArray()
            for i = 0 to count - 1 do
                let! result = initializer i
                output.Add(result)
            return output :> _ seq
        }
    

    let map (mapper: 'T -> Async<'U>) (task: Async<'T seq>) : Async<'U seq> =
        async {
            let! ts = task
            let us = ResizeArray()
            for t in ts do
                let! u = mapper t
                us.Add(u)
            return us :> _ seq
        }

    let filter (predicate: 'T -> Async<bool>) (task: Async<'T seq>) : Async<'T seq> =
        async {
            let! ts = task
            let output = ResizeArray()
            for t in ts do
                let! result = predicate t
                if result then
                    output.Add(t)
            return output :> _ seq
        }

type Async with

    static member RunImmediate (computation: Async<'T>, ?cancellationToken ) =
        let cancellationToken = defaultArg cancellationToken Async.DefaultCancellationToken
        let ts = TaskCompletionSource<'T>()
        let task = ts.Task
        Async.StartWithContinuations(
            computation,
            (fun k -> ts.SetResult k),
            (fun exn -> ts.SetException exn),
            (fun _ -> ts.SetCanceled()),
            cancellationToken)
        task.Result

/// Thread-safe.
[<Sealed>]
type AsyncCancellationTokenSource() =
    let projectBuildSemaphore = new SemaphoreSlim(1, 1)
    let mutable projectBuildCts = new CancellationTokenSource()

    /// Communicates a request for cancellation.
    member _.Cancel(): unit =
        projectBuildSemaphore.Wait()
        try
            projectBuildCts.Cancel()
        finally
            projectBuildSemaphore.Release()
            |> ignore

    /// Communicates a request for cancellation asynchronously.
    member _.CancelAsync(): Task =
        backgroundTask {
            do! projectBuildSemaphore.WaitAsync()
            try
                do! projectBuildCts.CancelAsync()
            finally
                projectBuildSemaphore.Release()
                |> ignore
        }

    /// Communicates a request for cancellation asynchronously
    /// then resets the source and gets a new token.
    /// Can be passed a token that registers its cancellation
    /// to cancel the source. If the source gets reset, the
    /// registrar will not cause a cancellation on the source that was reset.
    member _.CancelAndGetNewTokenAsync(registrar: CancellationToken): Task<CancellationToken * CancellationTokenRegistration> =
        backgroundTask {
            do! projectBuildSemaphore.WaitAsync()
            try
                do! projectBuildCts.CancelAsync()
                projectBuildCts.Dispose()
                projectBuildCts <- new CancellationTokenSource()
                let registration = registrar.Register(
                    fun () ->
                        projectBuildCts.Cancel()
                )
                return projectBuildCts.Token, registration
            finally
                projectBuildSemaphore.Release()
                |> ignore
        }

    /// Gets the current token.
    member _.Token: CancellationToken = 
        projectBuildSemaphore.Wait()
        try
            projectBuildCts.Token
        finally
            projectBuildSemaphore.Release()
            |> ignore

    interface IDisposable with
        member _.Dispose (): unit = 
            projectBuildSemaphore.Wait()
            try
                projectBuildCts.Dispose()
            finally
                projectBuildSemaphore.Dispose()