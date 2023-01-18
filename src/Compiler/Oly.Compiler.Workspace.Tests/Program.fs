module Program

open Xunit

[<assembly: CollectionBehavior(CollectionBehavior.CollectionPerClass, MaxParallelThreads = 0)>]
do()

let [<EntryPoint>] main _ = 0