open System
open System.Diagnostics
open System.Threading
open Oly.Core
open olylib

let stopwatch = Stopwatch.StartNew()

let args = Environment.GetCommandLineArgs()

// Parse args
if args.Length <= 1 || args.Length > 2 then
    printfn "Expected Oly project file."
    Environment.ExitCode <- 1
else
    let ct = CancellationToken.None

    let run configName projectPath =
        let projectPath = OlyPath.Create(projectPath)

        printfn "Compiling '%s'..." (projectPath.ToString())
        let result = Oly.Build(configName, projectPath, ct).Result
        match result with
        | Ok _ ->
            stopwatch.Stop()
            printfn "Compiled '%s' successfully in %f seconds." (projectPath.ToString()) stopwatch.Elapsed.TotalSeconds
            Environment.ExitCode <- 0
        | Error diags ->
            diags
            |> ImArray.iter (fun diag ->
                printfn "%s\n" (diag.GetHelperText(ct))
            )
            Environment.ExitCode <- 1

    match args with
    | [|_; "-c"; configName; projectPath|] ->
        run configName projectPath
    | [|_; projectPath|] ->
        run "Release" projectPath
    | _ ->
        printfn "Invalid command-line arguments."
        Environment.ExitCode <- 1
