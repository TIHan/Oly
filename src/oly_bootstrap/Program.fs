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

let results = Oly.Build("Release", OlyPath.Create(Environment.GetCommandLineArgs()[1]), System.Threading.CancellationToken.None).Result
match results with
| Ok(_) -> printfn "Oly Cli Bootstrap Succeeded"
| Error(results) ->
    results
    |> ImArray.iter (fun x -> OlyTrace.LogError(x.ToString()))
    failwith "Failed: Oly Cli Bootstrap"