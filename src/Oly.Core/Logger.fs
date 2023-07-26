[<AutoOpen>]
module Oly.Core.Logger

open Microsoft.Extensions.Logging

[<AutoOpen>]
module Logger =

    let private loggerFactory = 
        LoggerFactory.Create(
            fun builder ->
                builder.AddDebug() |> ignore
                builder.AddEventSourceLogger() |> ignore
        )

    let private logger = loggerFactory.CreateLogger("OlyRuntime")

    let Log msg =
        logger.Log(LogLevel.Information, msg)