open System
open Oly.Core
open olylib

OlyTrace.Log <- fun text -> Console.WriteLine(text)
OlyTrace.LogWarning <- 
    fun text -> 
        let originalColor = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Yellow
        Console.WriteLine(text)
        Console.ForegroundColor <- originalColor
OlyTrace.LogError <- fun text -> Console.Error.WriteLine(text)

let results = Oly.Build("Release", OlyPath.Create("../../../oly/oly.olyx"), System.Threading.CancellationToken.None).Result
if results.IsError then
    failwith "Failed: Oly Cli Bootstrap"
else
    printfn "Oly Cli Bootstrap Succeeded"