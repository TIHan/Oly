namespace Oly.Core

open System
open System.Threading.Tasks

#if DEBUG
/// Only used in DEBUG builds to prevent stack-overflows when tail calls are disabled.
[<AbstractClass;Sealed>]
type DebugStackGuard =

    static member MaxDepth with get() = 3

    [<DefaultValue(false)>]
    [<ThreadStatic>]
    static val mutable private CurrentDepth: int32

    static member Do<'T>(f: unit -> 'T) =
        DebugStackGuard.CurrentDepth <- DebugStackGuard.CurrentDepth + 1
        try
            if DebugStackGuard.CurrentDepth % DebugStackGuard.MaxDepth = 0 then
                Task.Run(fun () -> f()).Result
            else
                f()
        finally
            DebugStackGuard.CurrentDepth <- DebugStackGuard.CurrentDepth - 1
#endif

