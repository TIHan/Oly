module TestFramework

open System
open System.Threading

type private XAssert = Xunit.Assert

[<AbstractClass;Sealed>]
type Assert private () =

    static member Equal(expected: obj, actual: obj) =
        XAssert.Equal(expected, actual)