namespace Oly.Compiler

open System.Diagnostics
open System.Runtime.CompilerServices

[<Sealed>]
type internal InternalCompilerException() =
    inherit System.Exception("An internal compiler error has occurred.")

[<Sealed>]
type internal InternalCompilerUnreachedException() =
    inherit System.Exception("An internal compiler error has occurred due to unreached logic.")
