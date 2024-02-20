namespace Oly.Core

open System
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Assert =

    let ThrowIf (value: bool) =
        if value then
            failwith "Assertion failed."

    let ThrowIfNot (value: bool) =
        if not value then
            failwith "Assertion failed."

[<Sealed>]
type OlyAssertionException(msg: string) =
    inherit Exception(msg)

[<RequireQualifiedAccess;AbstractClass;Sealed>]
type OlyAssert =

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member Equal<'T when 'T: equality> (expected: 'T, actual: 'T) =
        if expected <> actual then
            OlyAssertionException $"Assertion failed. Expected '{expected}', but was '{actual}'."
            |> raise

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member NotEqual<'T when 'T: equality> (expected: 'T, actual: 'T) =
        if expected = actual then
            OlyAssertionException $"Assertion failed. Not expected '{expected}', but was '{actual}'."
            |> raise

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member EqualArray<'T when 'T: equality> (expected: 'T imarray, actual: 'T imarray) =
        if expected.Length = actual.Length then
            (expected, actual)
            ||> ImArray.iter2 (fun expected actual ->
                if expected <> actual then
                    OlyAssertionException $"Assertion failed. Expected '{expected}', but was '{actual}'."
                    |> raise
            )

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member Contains<'T> (src: HashSet<'T>, expected: 'T) =
        if not(src.Contains(expected)) then
            OlyAssertionException "Contains assertion failed."
            |> raise

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member NotContains<'T> (src: HashSet<'T>, expected: 'T) =
        if src.Contains(expected) then
            OlyAssertionException "NotContains assertion failed."
            |> raise

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member ContainsKey<'TKey, 'TValue> (src: IDictionary<'TKey, 'TValue>, expectedKey: 'TKey) =
        if not(src.ContainsKey(expectedKey)) then
            OlyAssertionException "ContainsKey assertion failed."
            |> raise

    [<DebuggerHidden>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member Fail(msg: string) =
        OlyAssertionException msg
        |> raise

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member True(actual: bool) =
        OlyAssert.Equal(true, actual)

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member False(actual: bool) =
        OlyAssert.Equal(false, actual)

    [<DebuggerHidden>]
    [<Conditional("DEBUG")>]
    [<Conditional("CHECKED")>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member NotNull(o: obj) =
        match o with
        | null -> OlyAssertionException "Assertion failed. Object is null." |> raise
        | _ -> ()