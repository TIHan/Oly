[<AutoOpen>]
module Oly.Core.MiscHelpers

open System
open System.Diagnostics
open System.Runtime.CompilerServices

[<DebuggerHidden>]
[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
let inline unreached() =
    raise(UnreachableException())

