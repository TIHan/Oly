open System
open System.Diagnostics
open System.Threading
open Oly.Core
open olylib

let stopwatch = Stopwatch.StartNew()

let args = Environment.GetCommandLineArgs()

// Parse args
if args.Length <= 1 then
    printfn "Expected Oly project file."
    Environment.ExitCode <- 1
else
    let ct = CancellationToken.None

    let build configName projectPath willRunAfterBuild =
        let projectPath = OlyPath.Create(projectPath)

        let defaultColor = Console.ForegroundColor

        if not willRunAfterBuild then
            Console.ForegroundColor <- ConsoleColor.Yellow
            printfn "Building '%s'..." (projectPath.ToString())

        let result = Oly.Build(configName, projectPath, ct).Result
        match result with
        | Ok prog ->
            stopwatch.Stop()

            if not willRunAfterBuild then
                Console.ForegroundColor <- ConsoleColor.Green
                printfn "Built '%s' successfully in %.2f seconds.\n" (projectPath.ToString()) stopwatch.Elapsed.TotalSeconds
                Console.ForegroundColor <- defaultColor
            else
                prog.Run()

            Environment.ExitCode <- 0
        | Error diags ->
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "Building '%s' failed.\n" (projectPath.ToString())
            diags
            |> ImArray.iter (fun diag ->
                printfn "%s\n" (diag.ToString())
            )
            Environment.ExitCode <- 1

    match args with
    | [|_; "run"; "-c"; configName; projectPath|] ->
        build configName projectPath true
    | [|_; "run"; projectPath|] ->
        build "Release" projectPath true
    | [|_; "-c"; configName; projectPath|] ->
        build configName projectPath false
    | [|_; projectPath|] ->
        build "Release" projectPath false
    | _ ->
        printfn "Invalid command-line arguments."
        Environment.ExitCode <- 1
