namespace Oly.Core

open System
open System.Threading.Tasks

[<AbstractClass;Sealed>]
type StackGuard =

    static member MaxDepth with get() = 
#if DEBUG
        3
#else
        15
#endif


    [<DefaultValue(false)>]
    [<ThreadStatic>]
    static val mutable private currentDepth: int32
    static member CurrentDepth with get() = StackGuard.currentDepth and set(value) = StackGuard.currentDepth <- value

    static member inline Do<'T>([<InlineIfLambda>] f: unit -> 'T) =
        StackGuard.CurrentDepth <- StackGuard.CurrentDepth + 1
        try
            if StackGuard.CurrentDepth % StackGuard.MaxDepth = 0 then
                Task.Run(fun () -> f()).Result
            else
                f()
        finally
            StackGuard.CurrentDepth <- StackGuard.CurrentDepth - 1
