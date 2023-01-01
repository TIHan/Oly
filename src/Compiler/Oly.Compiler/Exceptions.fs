namespace Oly.Compiler

[<Sealed>]
type internal InternalCompilerException() =
    inherit System.Exception("An internal compiler error has occurred.")

[<Sealed>]
type internal InternalCompilerUnreachedException() =
    inherit System.Exception("An internal compiler error has occurred due to unreached logic.")
