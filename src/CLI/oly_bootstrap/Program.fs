open System
open System.IO
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

Oly.Clean(Path.GetDirectoryName(Environment.GetCommandLineArgs()[1]))
let config =
#if DEBUG
    "Debug"
#else
    "Release"
#endif
let results = Oly.Build(config, OlyPath.Create(Environment.GetCommandLineArgs()[1]), System.Threading.CancellationToken.None).Result.Value
match results with
| Ok(_) -> printfn "Oly Bootstrap Succeeded"
| Error(results) ->
    results
    |> ImArray.iter (fun x -> OlyTrace.LogError(x.ToString()))
    failwith "Oly Bootstrap Failed"