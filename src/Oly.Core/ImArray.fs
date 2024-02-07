namespace Oly.Core

open System.Collections.Immutable

type imarray = ImmutableArray
type 'T imarray = ImmutableArray<'T>
type imarrayb<'T> = ImmutableArray<'T>.Builder

[<RequireQualifiedAccess>]
module ImArray =

    let inline builder () : imarrayb<'T> =
        imarray.CreateBuilder()

    let inline builderWithSize size : imarrayb<'T> =
        imarray.CreateBuilder(size)

    let inline sumBy ([<InlineIfLambda>] f: ^T -> ^U) (arr: imarray< ^T>) : ^U =
        match arr.Length with
        | 0 -> Unchecked.defaultof<_>
        | 1 -> f arr.[0]
        | n ->
            let mutable result = f arr.[0]
            for i = 1 to n - 1 do
                result <- result + f arr.[i]
            result   

    let inline prependOne (item: 'T) (arr: imarray<'T>) : imarray<_> =
        imarray.Create(item).AddRange(arr)

    let inline append (arr1: imarray<'T1>) (arr2: imarray<'T1>) : imarray<_> =
        arr1.AddRange(arr2)

    let inline appendOne (item: 'T) (arr: imarray<'T>) : imarray<_> =
        arr.Add(item)

    let inline createOne (item: 'T) : imarray<_> =
        imarray.Create(item)

    let inline createTwo (item1: 'T) (item2: 'T) : imarray<_> =
        imarray.Create(item1, item2)

    [<GeneralizableValue>]
    let empty<'T> = ImmutableArray<'T>.Empty

    let inline init n ([<InlineIfLambda>] f: int -> 'T) : imarray<_> =
        match n with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(f 0)
        | n ->
            if n < 0 then
                invalidArg "n" "Below zero."

            let builder = ImmutableArray.CreateBuilder(n)
            for i = 0 to n - 1 do
                builder.Add(f i)
            builder.MoveToImmutable()

    let inline iter ([<InlineIfLambda>] f) (arr: imarray<'T>) =
        for i = 0 to arr.Length - 1 do
            f arr.[i]

    let inline iteri f (arr: imarray<'T>) =
        for i = 0 to arr.Length - 1 do
            f i arr.[i]

    let inline iter2 f (arr1: imarray<'T1>) (arr2: imarray<'T2>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."

        for i = 0 to arr1.Length - 1 do
            f arr1.[i] arr2.[i]

    let inline iter3 f (arr1: imarray<'T1>) (arr2: imarray<'T2>) (arr3: imarray<'T3>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."
        if arr1.Length <> arr3.Length then
            invalidOp "Block lengths do not match."

        for i = 0 to arr1.Length - 1 do
            f arr1.[i] arr2.[i] arr3.[i]

    let inline iteri2 f (arr1: imarray<'T1>) (arr2: imarray<'T2>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."

        for i = 0 to arr1.Length - 1 do
            f i arr1.[i] arr2.[i]

    let inline tryIter2 f (arr1: imarray<'T1>) (arr2: imarray<'T2>) =
        for i = 0 to (min arr1.Length arr2.Length) - 1 do
            f arr1.[i] arr2.[i]

    let inline map (mapper: 'T -> 'U) (arr: imarray<'T>) : imarray<_> =
        match arr.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(mapper arr.[0])
        | _ ->
            let builder = ImmutableArray.CreateBuilder(arr.Length)
            for i = 0 to arr.Length - 1 do
                builder.Add(mapper arr.[i])
            builder.MoveToImmutable()

    let inline mapi (mapper: int -> 'T -> 'U) (arr: imarray<'T>) : imarray<_> =
        match arr.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(mapper 0 arr.[0])
        | _ ->
            let builder = ImmutableArray.CreateBuilder(arr.Length)
            for i = 0 to arr.Length - 1 do
                builder.Add(mapper i arr.[i])
            builder.MoveToImmutable()

    let inline map2 (mapper: 'T1 -> 'T2 -> 'T) (arr1: imarray<'T1>) (arr2: imarray<'T2>) : imarray<_> =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."
      
        match arr1.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(mapper arr1.[0] arr2.[0])
        | n ->
            let builder = ImmutableArray.CreateBuilder(n)
            for i = 0 to n - 1 do
                builder.Add(mapper arr1.[i] arr2.[i])
            builder.MoveToImmutable()

    let inline mapi2 (mapper: int -> 'T1 -> 'T2 -> 'T) (arr1: imarray<'T1>) (arr2: imarray<'T2>) : imarray<_> =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."
      
        match arr1.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(mapper 0 arr1.[0] arr2.[0])
        | n ->
            let builder = ImmutableArray.CreateBuilder(n)
            for i = 0 to n - 1 do
                builder.Add(mapper i arr1.[i] arr2.[i])
            builder.MoveToImmutable()

    let inline collect (mapping: 'T -> 'U imarray) (arr: imarray<'T>) : imarray<'U> =
        // TODO: Make this performant.
        Seq.collect mapping arr
        |> ImmutableArray.CreateRange

    let inline rev (arr: imarray<'T>) : imarray<'T> =
        // TODO: Make this performant.
        arr
        |> Seq.rev
        |> ImmutableArray.CreateRange

    let inline concat (arrs: imarray<imarray<'T>>) : imarray<'T> =
        match arrs.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> arrs.[0]
        | 2 -> arrs.[0].AddRange(arrs.[1])
        | _ ->
            let mutable acc = 0    
            for h in arrs do
                acc <- acc + h.Length

            let builder = ImmutableArray.CreateBuilder(acc)
            for i = 0 to arrs.Length - 1 do
                builder.AddRange(arrs.[i])
            builder.MoveToImmutable()

    let inline reduce (reduction: 'T -> 'T -> 'T) (arr: imarray<'T>) : 'T =
        Seq.reduce reduction arr

    let inline reduceBack (reduction: 'T -> 'T -> 'T) (arr: imarray<'T>) : 'T =
        Seq.reduceBack reduction arr

    let inline forall predicate (arr: imarray<'T>) =
        let len = arr.Length
        if len = 0 then true
        else
            let rec loop i = i >= len || (predicate arr.[i] && loop (i+1))
            loop 0

    let inline forall2 predicate (arr1: imarray<'T1>) (arr2: imarray<'T2>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Array lengths do not match."

        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
        let len1 = arr1.Length
        if len1 = 0 then true
        else
            let rec loop i = i >= len1 || (f.Invoke(arr1.[i], arr2.[i]) && loop (i+1))
            loop 0

    let inline tryForall2 predicate (arr1: imarray<'T1>) (arr2: imarray<'T2>) =
        if arr1.Length <> arr2.Length then
            false
        else
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
            let len1 = arr1.Length
            if len1 = 0 then true
            else
                let rec loop i = i >= len1 || (f.Invoke(arr1.[i], arr2.[i]) && loop (i+1))
                loop 0

    let inline foralli predicate (arr: imarray<'T>) =
        let len = arr.Length
        if len = 0 then true
        else
            let rec loop i = i >= len || (predicate i arr.[i] && loop (i+1))
            loop 0

    let inline foralli2 predicate (arr1: imarray<'T1>) (arr2: imarray<'T2>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."

        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(predicate)
        let len1 = arr1.Length
        if len1 = 0 then true
        else
            let rec loop i = i >= len1 || (f.Invoke(i, arr1.[i], arr2.[i]) && loop (i+1))
            loop 0

    let inline tryFind predicate (arr: imarray<'T>) =
        let rec loop i = 
            if i >= arr.Length then None else 
            if predicate arr.[i] then Some arr.[i]  else loop (i+1)
        loop 0 

    let inline find predicate (arr: imarray<'T>) =
        match tryFind predicate arr with
        | Some x -> x
        | _ -> failwith "Unable to find item in the array."

    let inline tryFindIndex predicate (arr: imarray<'T>) =
        let len = arr.Length 
        let rec go n = if n >= len then None elif predicate arr.[n] then Some n else go (n+1)
        go 0 

    let inline findIndex predicate (arr: imarray<'T>) =
        match tryFindIndex predicate arr with
        | Some x -> x
        | _ -> failwith "Unable to find item in the array."

    let inline tryPick ([<InlineIfLambda>] chooser) (arr: imarray<'T>) =
        let rec loop i = 
            if i >= arr.Length then None else 
            match chooser arr.[i] with 
            | None -> loop(i+1)
            | res -> res
        loop 0 

    let inline tryPicki ([<InlineIfLambda>] chooser) (arr: imarray<'T>) =
        let rec loop i = 
            if i >= arr.Length then None else 
            match chooser i arr.[i] with 
            | None -> loop(i+1)
            | res -> res
        loop 0 

    let inline tryExactlyOne (arr: imarray<'T>) =
        if arr.Length = 1 then
            Some arr[0]
        else
            None

    let inline pick chooser (arr: imarray<'T>) =
        let valueOpt = tryPick chooser arr
        if valueOpt.IsNone then
            failwith "Unable to pick item in the array."
        valueOpt.Value

    let inline ofSeq (xs: 'T seq) =
        match xs with
        | :? ImmutableArray<'T> as xs -> xs
        | _ -> ImmutableArray.CreateRange(xs)

    let inline skip amount (arr: imarray<'T>) =
        // TODO: Make this efficient
        arr
        |> Seq.skip amount
        |> ofSeq

    let inline distinct (arr: imarray<'T>) =
        // TOOD: Make this efficient
        arr
        |> Seq.distinct
        |> ofSeq

    let inline distinctBy projection (arr: imarray<'T>) =
        // TOOD: Make this efficient
        arr
        |> Seq.distinctBy projection
        |> ofSeq

    let inline take count (arr: imarray<'T>) =
        // TOOD: Make this efficient
        arr
        |> Seq.take count
        |> ofSeq

    let inline filter predicate (arr: imarray<'T>) : imarray<'T> =
        let builder = ImmutableArray.CreateBuilder(arr.Length)
        for i = 0 to arr.Length - 1 do
            if predicate arr.[i] then
                builder.Add(arr.[i])
        builder.Capacity <- builder.Count
        builder.MoveToImmutable()

    let inline exists predicate (arr: imarray<'T>) =
        let len = arr.Length
        let rec loop i = i < len && (predicate arr.[i] || loop (i+1))
        len > 0 && loop 0

    let inline existsi predicate (arr: imarray<'T>) =
        let len = arr.Length
        let rec loop i = i < len && (predicate i arr.[i] || loop (i+1))
        len > 0 && loop 0

    let inline choose (chooser: 'T -> 'U option) (arr: imarray<'T>) : imarray<'U> =
        let builder = ImmutableArray.CreateBuilder(arr.Length)
        for i = 0 to arr.Length - 1 do
            let result = chooser arr.[i]
            if result.IsSome then
                builder.Add(result.Value)
        builder.Capacity <- builder.Count
        builder.MoveToImmutable()

    let inline choosei (chooser: int -> 'T -> 'U option) (arr: imarray<'T>) : imarray<'U> =
        let builder = ImmutableArray.CreateBuilder(arr.Length)
        for i = 0 to arr.Length - 1 do
            let result = chooser i arr.[i]
            if result.IsSome then
                builder.Add(result.Value)
        builder.Capacity <- builder.Count
        builder.MoveToImmutable()

    let inline choose2 (chooser: 'T1 -> 'T2 -> 'U option) (arr1: imarray<'T1>) (arr2: imarray<'T2>) : imarray<'U> =
        if arr1.Length <> arr2.Length then
            invalidOp "Array lengths do not match."

        let builder = ImmutableArray.CreateBuilder(arr1.Length)
        for i = 0 to arr1.Length - 1 do
            let result = chooser arr1[i] arr2[i]
            if result.IsSome then
                builder.Add(result.Value)
        builder.Capacity <- builder.Count
        builder.MoveToImmutable()

    let inline isEmpty (arr: imarray<_>) = arr.IsEmpty

    let fold folder state (arr: imarray<_>) =
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable state = state
        for i = 0 to arr.Length - 1 do 
            state <- f.Invoke(state, arr.[i])
        state   

    let fold2 folder state (arr1: imarray<_>) (arr2: imarray<_>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."

        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable state = state
        for i = 0 to arr1.Length - 1 do 
            state <- f.Invoke(state, arr1.[i], arr2.[i])
        state   


    let foldBack folder state (arr: imarray<_>) =
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable state = state
        for i = arr.Length - 1 downto 0 do 
            state <- f.Invoke(state, arr.[i])
        state 
        
    let foldBacki folder state (arr: imarray<_>) =
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable state = state
        for i = arr.Length - 1 downto 0 do 
            state <- f.Invoke(state, i, arr.[i])
        state 

    let foldBack2 folder state (arr1: imarray<_>) (arr2: imarray<_>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."

        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable state = state
        for i = arr1.Length - 1 downto 0 do 
            state <- f.Invoke(state, arr1.[i], arr2.[i])
        state   

    let foldMap f state (arr: imarray<_>) =
        match arr.Length with
        | 0 -> state, ImmutableArray.Empty
        | 1 -> 
            let newState, (newItem: 'b) = f state arr.[0]
            newState, ImmutableArray.Create(newItem)
        | _ ->
            let builder = ImmutableArray.CreateBuilder(arr.Length)
            let mutable state = state
            for i = 0 to arr.Length - 1 do
                let newState, newItem = f state arr.[i]
                state <- newState
                builder.Add(newItem)
            state, builder.MoveToImmutable()

    let foldMap2 f state (arr1: imarray<_>) (arr2: imarray<_>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."

        match arr1.Length with
        | 0 -> state, ImmutableArray.Empty
        | 1 -> 
            let newState, (newItem: 'b) = f state arr1.[0] arr2.[0]
            newState, ImmutableArray.Create(newItem)
        | _ ->
            let builder = ImmutableArray.CreateBuilder(arr1.Length)
            let mutable state = state
            for i = 0 to arr1.Length - 1 do
                let newState, newItem = f state arr1.[i] arr2.[i]
                state <- newState
                builder.Add(newItem)
            state, builder.MoveToImmutable()

    let foldMapi f state (arr: imarray<_>) =
        match arr.Length with
        | 0 -> state, ImmutableArray.Empty
        | 1 -> 
            let newState, (newItem: 'b) = f state 0 arr.[0]
            newState, ImmutableArray.Create(newItem)
        | _ ->
            let builder = ImmutableArray.CreateBuilder(arr.Length)
            let mutable state = state
            for i = 0 to arr.Length - 1 do
                let newState, newItem = f state i arr.[i]
                state <- newState
                builder.Add(newItem)
            state, builder.MoveToImmutable()

    let foldBackMap2 foldMapper state (arr1: imarray<_>) (arr2: imarray<_>) =
        if arr1.Length <> arr2.Length then
            invalidOp "Block lengths do not match."

        match arr1.Length with
        | 0 -> state, ImmutableArray.Empty
        | 1 -> 
            let newState, (newItem: 'b) = foldMapper state arr1[0] arr2[1]
            newState, ImmutableArray.Create(newItem)
        | _ ->
            let builder = ImmutableArray.CreateBuilder(arr1.Length)
            builder.Count <- arr1.Length
            let mutable state = state
            for i = arr1.Length - 1 downto 0 do 
                let newState, newItem = foldMapper state arr1[0] arr2[1]
                state <- newState
                builder.Insert(i, newItem)
            state, builder.MoveToImmutable()

    module Parallel =

        open System
        open System.Threading.Tasks
    
        let iteri f (arr: 'T imarray) =
            let parallelOptions = ParallelOptions(MaxDegreeOfParallelism = max (min Environment.ProcessorCount arr.Length) 1)
            try
                Parallel.For(0, arr.Length, parallelOptions, fun i ->
                    f i arr.[i]
                ) |> ignore
            with
            | :? AggregateException as ex when ex.InnerExceptions.Count = 1 ->
                raise(ex.InnerExceptions.[0])
    
        let iter f (arr: 'T imarray) =
            arr |> iteri (fun _ item -> f item)
    
        let mapi f (arr: 'T imarray) =
            let builder = ImmutableArray.CreateBuilder(arr.Length)
            for _ = 0 to arr.Length - 1 do
                builder.Add(Unchecked.defaultof<_>)
            arr |> iteri (fun i item -> builder.[i] <- f i item)
            builder.MoveToImmutable()
    
        let map f (arr: 'T imarray) =
            arr |> mapi (fun _ item -> f item)

    let inline min (arr: imarray<_>) =
        // TODO: Make this performant
        arr
        |> Seq.min

    let inline minBy projection (arr: imarray<_>) =
        // TODO: Make this performant
        arr
        |> Seq.minBy projection

type 'T romem = System.ReadOnlyMemory<'T>

[<AutoOpen>]
module ReadOnlyMemoryExtensions =
    type System.ReadOnlyMemory<'T> with

        member inline this.Item n = this.Span[n]

[<RequireQualifiedAccess>]
module ROMem =
    open System

    let inline init n (f: int -> 'T) : romem<_> =
        match n with
        | 0 -> ReadOnlyMemory.Empty
        | 1 -> ImmutableArray.Create(f 0).AsMemory()
        | n ->
            if n < 0 then
                invalidArg "n" "Below zero."

            let builder = ImmutableArray.CreateBuilder(n)
            for i = 0 to n - 1 do
                builder.Add(f i)
            builder.MoveToImmutable().AsMemory()

    let inline map (mapper: 'T -> 'U) (arr: romem<'T>) : romem<_> =
        match arr.Length with
        | 0 -> ReadOnlyMemory.Empty
        | 1 -> ImmutableArray.Create(mapper arr.Span[0]).AsMemory()
        | _ ->
            let arr = arr.Span
            let builder = ImmutableArray.CreateBuilder(arr.Length)
            for i = 0 to arr.Length - 1 do
                builder.Add(mapper arr.[i])
            builder.MoveToImmutable().AsMemory()

    let inline map2AsImArray (mapper: 'T1 -> 'T2 -> 'T) (arr1: romem<'T1>) (arr2: romem<'T2>) : imarray<_> =
        if arr1.Length <> arr2.Length then
            invalidOp "Lengths do not match."
      
        match arr1.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(mapper arr1.[0] arr2.[0])
        | n ->
            let builder = ImmutableArray.CreateBuilder(n)
            for i = 0 to n - 1 do
                builder.Add(mapper arr1.[i] arr2.[i])
            builder.MoveToImmutable()

    let inline mapAsImArray (mapper: 'T -> 'U) (arr: romem<'T>) : imarray<_> =
        match arr.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(mapper arr.Span[0])
        | _ ->
            let arr = arr.Span
            let builder = ImmutableArray.CreateBuilder(arr.Length)
            for i = 0 to arr.Length - 1 do
                builder.Add(mapper arr.[i])
            builder.MoveToImmutable()

    let inline toImArray (arr: romem<'T>) : imarray<'T> =
        match arr.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(arr.Span[0])
        | _ ->
            let arr = arr.Span
            let builder = ImmutableArray.CreateBuilder(arr.Length)
            for i = 0 to arr.Length - 1 do
                builder.Add(arr.[i])
            builder.MoveToImmutable()

    let inline iter2 ([<InlineIfLambda>] f) (items1: romem<'T1>) (items2: romem<'T2>) =
        if items1.Length <> items2.Length then
            invalidOp "Lengths do not match."

        let mutable i = 0
        let items1 = items1.Span
        let items2 = items2.Span
        while(i < items1.Length) do
            f items1[i] items2[i]
            i <- i + 1

    let inline tryIter2 ([<InlineIfLambda>] f) (items1: romem<'T1>) (items2: romem<'T2>) =
        for i = 0 to (min items1.Length items2.Length) - 1 do
            f items1.[i] items2.[i]

    let inline forall2 ([<InlineIfLambda>] f) (items1: romem<'T1>) (items2: romem<'T2>) =
        if items1.Length <> items2.Length then
            invalidOp "Lengths do not match."

        let mutable allAreEqual = true
        let mutable i = 0
        let items1 = items1.Span
        let items2 = items2.Span
        while(allAreEqual && i < items1.Length) do
            allAreEqual <- f items1[i] items2[i]
            i <- i + 1
        allAreEqual