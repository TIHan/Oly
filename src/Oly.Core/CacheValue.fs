namespace Oly.Core

open System
open System.Threading
open System.Collections.Generic

/// Least-Recently-Used Cache
/// Least-recently-used is the eviction policy of the cache.
[<Sealed>]
type LruCache<'TKey, 'TValue when 'TKey: equality> (maxCount: int) =
    do
        if maxCount <= 0 then
            invalidArg (nameof(maxCount)) "Must greater than zero."

    let linkedItems = LinkedList<'TKey * 'TValue>()
    let itemsLookup = Dictionary<'TKey, LinkedListNode<'TKey * 'TValue>>()

    let lockObj = obj()

    member this.SetItem(key, item) =
        lock lockObj (fun () ->
            match itemsLookup.TryGetValue(key) with
            | true, linkedItem ->
                linkedItems.Remove(linkedItem)
            | _ ->
                ()

            if itemsLookup.Count >= maxCount then
                let key, _ = linkedItems.Last.Value
                linkedItems.RemoveLast()
                itemsLookup.Remove(key) |> ignore

            itemsLookup[key] <- linkedItems.AddFirst((key, item))
        )

    member this.TryGetValue(key) =
        lock lockObj (fun () ->
            match itemsLookup.TryGetValue(key) with
            | true, linkedItem -> 
                linkedItems.Remove(linkedItem)
                linkedItems.AddFirst(linkedItem)
                ValueSome(linkedItem.Value |> snd)
            | _ -> 
                ValueNone
        )

[<Sealed;NoComparison;NoEquality>]
type CacheValue<'T> =

    val gate : obj
    val mutable f : CancellationToken -> 'T
    val mutable value : 'T voption

    new (f) = { gate = obj (); f = f; value = Unchecked.defaultof<_> }
    private new (value) = { gate = obj (); f = Unchecked.defaultof<_>; value = ValueSome value }

    member this.HasValue = 
        this.value.IsSome

    member this.GetValue(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        match this.value with
        | ValueNone ->
            lock this.gate (fun () ->
                ct.ThrowIfCancellationRequested()
                match this.value with
                | ValueNone ->
                    let value = this.f ct
                    this.value <- ValueSome value
                    this.f <- Unchecked.defaultof<_>
                    value
                | ValueSome value ->
                    value
            )
        | ValueSome value ->
            value

    static member FromValue(value: 'T) = CacheValue(value)

[<Sealed;NoComparison;NoEquality>]
type WeakCacheValue<'T when 'T: not struct> =

    val gate : obj
    val mutable f : CancellationToken -> 'T
    val mutable value : WeakReference<'T>

    new (f) = { gate = obj (); f = f; value = WeakReference<_>(Unchecked.defaultof<_>) }

    member this.GetValue(ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        match this.value.TryGetTarget() with
        | true, value -> value
        | _ ->
            lock this.gate (fun () ->
                ct.ThrowIfCancellationRequested()
                match this.value.TryGetTarget() with
                | true, value -> value
                | _ ->
                    let value = this.f ct
                    this.value.SetTarget(value)
                    value
            )

[<Sealed;NoComparison;NoEquality>]
type CacheValueWithArg<'Arg, 'T> =

    val gate : obj
    val mutable f : 'Arg -> CancellationToken -> 'T
    val mutable value : 'T voption

    new (f) = { gate = obj (); f = f; value = Unchecked.defaultof<_> }
    private new (value) = { gate = obj (); f = Unchecked.defaultof<_>; value = ValueSome value }

    member this.HasValue = 
        this.value.IsSome

    member this.GetValue(arg: 'Arg, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        match this.value with
        | ValueNone ->
            lock this.gate (fun () ->
                ct.ThrowIfCancellationRequested()
                match this.value with
                | ValueNone ->
                    let value = this.f arg ct
                    this.value <- ValueSome value
                    this.f <- Unchecked.defaultof<_>
                    value
                | ValueSome value ->
                    value
            )
        | ValueSome value ->
            value

    static member FromValue<'Arg, 'T>(value: 'T) = CacheValueWithArg<'Arg, 'T>(value)

[<Sealed;NoComparison;NoEquality>]
type WeakCacheValueWithArg<'Arg, 'T when 'T: not struct> =

    val gate : obj
    val mutable f : 'Arg -> CancellationToken -> 'T
    val mutable value : WeakReference<'T>

    new (f) = { gate = obj (); f = f; value = WeakReference<_>(Unchecked.defaultof<_>) }

    member this.HasValue = 
        this.value.TryGetTarget() |> fst

    member this.GetValue(arg: 'Arg, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        match this.value.TryGetTarget() with
        | true, value -> value
        | _ ->
            lock this.gate (fun () ->
                ct.ThrowIfCancellationRequested()
                match this.value.TryGetTarget() with
                | true, value -> value
                | _ ->
                    let value = this.f arg ct
                    this.value.SetTarget(value)
                    value
            )