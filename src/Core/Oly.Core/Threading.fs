namespace Oly.Core

open System
open System.Threading.Tasks

[<AbstractClass;Sealed>]
type StackGuard =

    static member MaxDepth with get() = 
#if DEBUG
        1
#else
        5
#endif


    [<DefaultValue(false)>]
    [<ThreadStatic>]
    static val mutable private currentDepth: int32
    static member CurrentDepth with get() = StackGuard.currentDepth and set(value) = StackGuard.currentDepth <- value

    static member inline Do<'T>([<InlineIfLambda>] f: unit -> 'T) =
        StackGuard.CurrentDepth <- StackGuard.CurrentDepth + 1
        try
            let sufficientStack =
                if StackGuard.CurrentDepth > StackGuard.MaxDepth then
                    System.Runtime.CompilerServices.RuntimeHelpers.TryEnsureSufficientExecutionStack()
                else
                    true
            if (not sufficientStack) then
                Task.Run(fun () -> f()).Result
            else
                f()
        finally
            StackGuard.CurrentDepth <- StackGuard.CurrentDepth - 1
