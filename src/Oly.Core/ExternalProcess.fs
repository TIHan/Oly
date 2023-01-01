namespace Oly.Core

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Diagnostics

[<Sealed>]
type ExternalProcess(filePath: string, args: string, ?workingDirectory: string) =

    let workingDirectory =
        defaultArg workingDirectory Environment.CurrentDirectory

    let startInfo = ProcessStartInfo()
    do
        startInfo.FileName <- filePath
        startInfo.Arguments <- args
        startInfo.RedirectStandardError <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.CreateNoWindow <- true

    let p = new Process()

    do
        p.StartInfo <- startInfo

    member this.RunAsync(ct: CancellationToken) =
        backgroundTask {
            let output = System.Text.StringBuilder()
            let error = System.Text.StringBuilder()
            use outputWaitHandle = new AutoResetEvent(false)
            use errorWaitHandle = new AutoResetEvent(false)

            p.OutputDataReceived.Add(fun e ->
                if e.Data = null then
                    outputWaitHandle.Set() |> ignore
                else
                    output.AppendLine(e.Data) |> ignore
            )

            p.ErrorDataReceived.Add(fun e ->
                if e.Data = null then
                    errorWaitHandle.Set() |> ignore
                else
                    error.AppendLine(e.Data) |> ignore
            )

            p.Start() |> ignore

            p.BeginOutputReadLine()
            p.BeginErrorReadLine()

            do! p.WaitForExitAsync(ct).ConfigureAwait(false)
            outputWaitHandle.WaitOne() |> ignore
            errorWaitHandle.WaitOne() |> ignore
            let output = output.ToString()
            let errors = error.ToString()
            if p.ExitCode <> 0 then
                failwith $"Process '{filePath} {args}' failed with:\n{output}\n\n{errors}"
            return {| Output = output; Errors = errors |}
        }

    interface IDisposable with

        member this.Dispose() =
            p.Dispose()