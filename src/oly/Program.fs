open System
open System.Diagnostics
open System.Threading
open Oly.Core
open olylib

let stopwatch = Stopwatch.StartNew()

let args = Environment.GetCommandLineArgs()

// Parse args
if args.Length <= 1 || args.Length > 2 then
    printf "Expected Oly project file."
    Environment.ExitCode <- 1
else
    let ct = CancellationToken.None
    let projectPath = OlyPath.Create(args[1])

    printfn "Compiling '%s'..." (projectPath.ToString())
    let result = Oly.Build(projectPath, ct).Result
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
