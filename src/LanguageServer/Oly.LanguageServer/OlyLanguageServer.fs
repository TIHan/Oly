module Oly.LanguageServer.OlyLanguageServer

open System
open OmniSharp.Extensions.LanguageServer.Server
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

let run (argv: string[]) =
#if DEBUG
    System.Diagnostics.Debugger.Launch() |> ignore
#endif
    let configureServices = 
        fun (services: IServiceCollection) -> ()
    let configureServices = Action<IServiceCollection>(configureServices)
    let optionsf =
        fun (options: LanguageServerOptions) ->
            options.Concurrency <- Environment.ProcessorCount |> Nullable
            options
                .WithInput(Console.OpenStandardInput())
                .WithOutput(Console.OpenStandardOutput())
                .WithLoggerFactory(new LoggerFactory())
                .AddDefaultLoggingProvider()
                .WithServices(configureServices)
                .WithHandler<LanguageServer.TextDocumentSyncHandler>()
                |> ignore

    let server = LanguageServer.From(optionsf).Result
    server.WaitForExit.Wait()

    0