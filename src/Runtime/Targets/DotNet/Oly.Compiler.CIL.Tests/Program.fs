module Program

open Xunit

[<assembly: CollectionBehavior(CollectionBehavior.CollectionPerAssembly, MaxParallelThreads = 16)>]
do()

let [<EntryPoint>] main _ = 0
