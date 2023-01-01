module Oly.Core.TaskExtensions

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

// This is like a band-aid.
[<Sealed>]
type StackGuard(maxDepth: int) =

    member val MaxDepth = maxDepth

    member val Depth = 1 with get, set

    member inline this.Guard([<InlineIfLambda>] f) =
        this.Depth <- this.Depth + 1
        if this.Depth % this.MaxDepth = 0 then
            try
                async { 
                    do! Async.SwitchToNewThread()
                    return f()
                } |> Async.RunImmediate
            finally
                this.Depth <- this.Depth - 1
        else
            let result = f()
            this.Depth <- this.Depth - 1
            result
