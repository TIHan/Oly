[<AutoOpen>]
module Oly.Core.MiscHelpers

open System
open System.Diagnostics
open System.Runtime.CompilerServices

[<DebuggerHidden>]
[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
let inline unreached() =
    raise(UnreachableException())

[<AbstractClass; Sealed; RequireQualifiedAccess>]
type LazyOption<'T> =

    static member val None: Lazy<Option<'T>> = Lazy<_>.CreateFromValue(None)

[<AbstractClass; Sealed; RequireQualifiedAccess>]
type LazyValueOption<'T> =

    static member val None: Lazy<ValueOption<'T>> = Lazy<_>.CreateFromValue(ValueNone)

