module internal Oly.Compiler.Internal.SemanticErrors

open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.PrettyPrint

// TODO: This is FAR from complete as we inline the errors throughout the compiler.

[<RequireQualifiedAccess>]
module SemanticError =

    let AbstractValueInferredMismatch benv (expectedValue: IValueSymbol) =
        sprintf
            "The value '%s' was unable to be inferred from its corresponding abstract one on '%s'. Consider using explicit type annotations on the signature."
            expectedValue.Name
            (printEnclosingDefinition benv expectedValue.Formal.Enclosing)

    let AbstractValueMismatch benv (expectedValue: IValueSymbol) =
        sprintf
            "The value '%s' has a signature that does not match its corresponding abstract one on '%s'. Expected signature: '%s'."
            expectedValue.Name
            (printEnclosingDefinition benv expectedValue.Formal.Enclosing)
            (printValue benv expectedValue)

    let AbstractValueTypeParameterConstraintMismatch benv (expectedValue: IValueSymbol) =
        sprintf 
            "The value '%s' has a signature that does not match its corresponding abstract one on '%s'. The type parameter constraints differ. Expected signature: '%s.'" 
            expectedValue.Name
            (printEnclosingDefinition benv expectedValue.Formal.Enclosing)
            (printValue benv expectedValue)