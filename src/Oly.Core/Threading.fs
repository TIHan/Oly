namespace Oly.Core

open System
open System.Threading.Tasks

[<AbstractClass;Sealed>]
type DebugStackGuard =

    static member MaxDepth with get() = 
#if DEBUG
        3
#else
        15
#endif


    [<DefaultValue(false)>]
    [<ThreadStatic>]
    static val mutable private currentDepth: int32
    static member CurrentDepth with get() = DebugStackGuard.currentDepth and set(value) = DebugStackGuard.currentDepth <- value

    static member inline Do<'T>([<InlineIfLambda>] f: unit -> 'T) =
        DebugStackGuard.CurrentDepth <- DebugStackGuard.CurrentDepth + 1
        try
            if DebugStackGuard.CurrentDepth % DebugStackGuard.MaxDepth = 0 then
                Task.Run(fun () -> f()).Result
            else
                f()
        finally
            DebugStackGuard.CurrentDepth <- DebugStackGuard.CurrentDepth - 1
