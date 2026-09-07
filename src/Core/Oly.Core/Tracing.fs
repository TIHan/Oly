[<AutoOpen>]
module Oly.Core.Tracing

module OlyTrace =

    let mutable Log : string -> unit = fun _ -> ()
    let mutable LogWarning : string -> unit = fun _ -> ()
    let mutable LogError : string -> unit = fun _ -> ()