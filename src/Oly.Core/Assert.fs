namespace Oly.Core

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Assert =

    let ThrowIf (value: bool) =
        if value then
            failwith "Assertion failed."

    let ThrowIfNot (value: bool) =
        if not value then
            failwith "Assertion failed."

[<RequireQualifiedAccess>]
module OlyAssert =

    let inline Equal<'T when 'T: equality> (expected: 'T, actual: 'T) =
#if DEBUG
        if expected <> actual then
            failwith $"Assertion failed. Expected '{expected}', but was '{actual}'."
#else
        ()
#endif

    let inline Contains<'T> (src: HashSet<'T>, expected: 'T) =
#if DEBUG
        if not(src.Contains(expected)) then
            failwith "Assertion failed."
#else
        ()
#endif

    let inline NotContains<'T> (src: HashSet<'T>, expected: 'T) =
#if DEBUG
        if src.Contains(expected) then
            failwith "Assertion failed."
#else
        ()
#endif

    let inline ContainsKey<'TKey, 'TValue> (src: IDictionary<'TKey, 'TValue>, expectedKey: 'TKey) =
#if DEBUG
        if not(src.ContainsKey(expectedKey)) then
            failwith "Assertion failed."
#else
        ()
#endif

    let inline Fail(msg: string) =
        failwith msg

    let inline True(actual: bool) =
        Equal(true, actual)

    let inline False(actual: bool) =
        Equal(false, actual)

    let inline NotNull(o: obj) =
#if DEBUG
        match o with
        | null -> failwith "Assertion failed. Object is null."
        | _ -> ()
#else
        ()
#endif