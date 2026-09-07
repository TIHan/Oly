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
        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardError <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.CreateNoWindow <- true

    let mutable p = new Process()

    let lockObj = obj()

    do
        p.StartInfo <- startInfo

    member this.SendLine(text: string, ct: CancellationToken) =
        ct.ThrowIfCancellationRequested()
        lock lockObj (fun () ->
            ct.ThrowIfCancellationRequested()
            p.StandardInput.WriteLine(text)
            let mutable outputStr = p.StandardOutput.ReadLine()
            if String.IsNullOrWhiteSpace(outputStr) then
                // TODO: This is an automatic restart by default. Expose this as a property.
                p.Dispose()
                p <- new Process()
                p.StartInfo <- startInfo
                p.Start() |> ignore
                {| Output = ""; Errors = "process failed - restarting" |}
            elif outputStr.StartsWith("Error:") then
                {| Output = ""; Errors = outputStr |}
            else
                {| Output = outputStr; Errors = "" |}
        )

    member this.Start(): unit =
        p.Start() |> ignore

    member this.RunAsync(ct: CancellationToken) =
        backgroundTask {
            OlyTrace.Log $"[ExternalProcess] {filePath} {args}"
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
            this.Start()
            p.BeginOutputReadLine()
            p.BeginErrorReadLine()
            do! p.WaitForExitAsync(ct).ConfigureAwait(false)
            outputWaitHandle.WaitOne() |> ignore
            errorWaitHandle.WaitOne() |> ignore
            let outputStr = output.ToString()
            let errorsStr = error.ToString()
            output.Clear() |> ignore
            error.Clear() |> ignore
            if p.ExitCode <> 0 then
                failwith $"Process '{filePath} {args}' failed with:\n{outputStr}\n\n{errorsStr}"
            return {| Output = outputStr; Errors = errorsStr |}
        }

    interface IDisposable with

        member this.Dispose() =
            p.Dispose()