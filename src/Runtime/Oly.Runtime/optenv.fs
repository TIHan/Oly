namespace Oly.Runtime.CodeGen.Internal

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable
open System.Diagnostics
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Runtime.CodeGen.Patterns
open Oly.Metadata
open Oly.Core
open Oly.Core.TaskExtensions

[<Sealed>]
type internal ArgumentLocalManager (argFlags: OlyIRLocalFlags [], localFlags: ResizeArray<OlyIRLocalFlags>) =

    member _.IsLocalMutable(localIndex) =
        localFlags[localIndex].HasFlag(OlyIRLocalFlags.Mutable)

    member _.IsLocalByRefType(localIndex) =
        localFlags[localIndex].HasFlag(OlyIRLocalFlags.ByRefType)

    member _.IsLocalAddressExposed(localIndex) =
        localFlags[localIndex].HasFlag(OlyIRLocalFlags.AddressExposed)

    member _.IsArgumentMutable(argIndex) =
        argFlags[argIndex].HasFlag(OlyIRLocalFlags.Mutable)

    member _.IsArgumentByRefType(argIndex) =
        argFlags[argIndex].HasFlag(OlyIRLocalFlags.ByRefType)

    member _.IsArgumentAddressExposed(argIndex) =
        argFlags[argIndex].HasFlag(OlyIRLocalFlags.AddressExposed)

    member _.GetLocalFlags(localIndex) =
        localFlags[localIndex]

    member _.GetLocalFlags() = localFlags |> Array.ofSeq
    member _.LocalCount = localFlags.Count

    member _.GetArgumentFlags() = argFlags
    member _.ArgumentCount = argFlags.Length

    member _.CreateLocal(flags) =
        let localIndex = localFlags.Count
        localFlags.Add(flags)
        localIndex

[<NoEquality;NoComparison>]
type internal optenv<'Type, 'Function, 'Field> =
    {
        func: RuntimeFunction
        tryGetFunctionBody: RuntimeFunction -> OlyIRFunctionBody<'Type, 'Function, 'Field> option
        emitFunction: RuntimeFunction * RuntimeFunction -> 'Function
        emitType: RuntimeType -> 'Type
        inlineSet: Dictionary<RuntimeFunction, int>
        irTier: OlyIRFunctionTier
        genericContext: GenericContext
        argLocalManager: ArgumentLocalManager
    }

    member this.IsDebuggable =
        match this.irTier with
        | OlyIRFunctionTier.Tier0 true -> true
        | _ -> false

    member this.IsLocalMutable(localIndex) =
        this.argLocalManager.IsLocalMutable(localIndex)

    member this.IsLocalByRefType(localIndex) =
        this.argLocalManager.IsLocalByRefType(localIndex)

    member this.IsLocalAddressExposed(localIndex) =
        this.argLocalManager.IsLocalAddressExposed(localIndex)

    member this.IsArgumentMutable(argIndex) =
        this.argLocalManager.IsArgumentMutable(argIndex)

    member this.IsArgumentByRefType(argIndex) =
        this.argLocalManager.IsArgumentByRefType(argIndex)

    member this.IsArgumentAddressExposed(argIndex) =
        this.argLocalManager.IsArgumentAddressExposed(argIndex)

    member this.GetLocalFlags(localIndex) =
        this.argLocalManager.GetLocalFlags(localIndex)

    member this.GetLocalFlags() = this.argLocalManager.GetLocalFlags()
    member this.LocalCount = this.argLocalManager.LocalCount

    member this.GetArgumentFlags() = this.argLocalManager.GetArgumentFlags()
    member this.ArgumentCount = this.argLocalManager.ArgumentCount

    member this.CreateLocal(flags) =
        this.argLocalManager.CreateLocal(flags)

    member this.CanPropagateLocal(localIndex) =
        not(this.IsLocalMutable(localIndex)) &&
        not(this.IsLocalAddressExposed(localIndex))

    member this.CanPropagateArgument(argIndex) =
        not(this.IsArgumentMutable(argIndex)) &&
        not(this.IsArgumentAddressExposed(argIndex))

[<AutoOpen>]
module internal Helpers =

    [<Literal>]
    let SideEffectDepthLimit = 10

    let rec hasSideEffectAux (optenv: optenv<_, _, _>) limit checkAddressExposed depth (irExpr: E<_, _, _>) =
        if depth >= limit then true
        else

        match irExpr with
        | E.None(textRange, _) -> 
            if optenv.IsDebuggable then
                // We consider this a side effect as we do not want to remove the expression.
                // This is for debugging purposes.
                not(String.IsNullOrWhiteSpace(textRange.Path.ToString()))
            else
                false
        | E.Value(value=value) when checkAddressExposed ->
            match value with
            | V.Local(localIndex, _) -> 
                optenv.IsLocalMutable(localIndex)
            | V.Argument(argIndex, _) ->
                optenv.IsArgumentMutable(argIndex)
            | V.LocalAddress(index=localIndex;kind=kind) ->
                match kind with
                | OlyIRByRefKind.Read ->
                    optenv.IsLocalMutable(localIndex)
                | _ ->
                    true
            | V.ArgumentAddress(index=argIndex;kind=kind) ->
                match kind with
                | OlyIRByRefKind.Read ->
                    optenv.IsArgumentMutable(argIndex)
                | _ ->
                    true
            | _ ->
                false
        | E.Value _ -> false
        | E.Operation(_, irOp) ->
            match irOp with
            | O.New(irFunc, _, _) -> 
                if irFunc.IsClosureInstanceConstructor then
                    let mutable anyArgsHaveSideEffects = false
                    irOp.ForEachArgument(fun _ irArgExpr -> 
                        if not anyArgsHaveSideEffects then
                            anyArgsHaveSideEffects <- hasSideEffectAux optenv limit checkAddressExposed (depth + 1) irArgExpr
                    )
                    anyArgsHaveSideEffects
                else
                    true
            | O.Store _
            | O.StoreToAddress _
            | O.StoreArgument _
            | O.StoreArrayElement _
            | O.StoreField _
            | O.StoreRefCellContents _
            | O.StoreStaticField _
            | O.Call _
            | O.CallIndirect _
            | O.CallVirtual _
            | O.CallStaticConstructor _
            | O.Print _ 
            | O.Throw _ -> true
            | _ ->
                let mutable anyArgsHaveSideEffects = false
                irOp.ForEachArgument(fun _ irArgExpr -> 
                    if not anyArgsHaveSideEffects then
                        anyArgsHaveSideEffects <- hasSideEffectAux optenv limit checkAddressExposed (depth + 1) irArgExpr
                )
                anyArgsHaveSideEffects

        | E.Let(_, _, irRhsExpr, irBodyExpr) ->
            hasSideEffectAux optenv limit checkAddressExposed (depth + 1) irRhsExpr ||
            hasSideEffectAux optenv limit checkAddressExposed (depth + 1) irBodyExpr

        | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, _) ->
            hasSideEffectAux optenv limit checkAddressExposed (depth + 1) irConditionExpr ||
            hasSideEffectAux optenv limit checkAddressExposed (depth + 1) irTrueTargetExpr ||
            hasSideEffectAux optenv limit checkAddressExposed (depth + 1) irFalseTargetExpr

        | _ -> 
            true

    let hasSideEffect optenv (irExpr: E<_, _, _>) =
        hasSideEffectAux optenv SideEffectDepthLimit false 0 irExpr

    let areConstantsDefinitelyEqual (c1: C<'Type, 'Function>) (c2: C<'Type, 'Function>) =
        match c1, c2 with
        | C.UInt8(value1), C.UInt8(value2) -> value1 = value2
        | C.Int8(value1), C.Int8(value2) -> value1 = value2
        | C.UInt16(value1), C.UInt16(value2) -> value1 = value2
        | C.Int16(value1), C.Int16(value2) -> value1 = value2
        | C.UInt32(value1), C.UInt32(value2) -> value1 = value2
        | C.Int32(value1), C.Int32(value2) -> value1 = value2
        | C.UInt64(value1), C.UInt64(value2) -> value1 = value2
        | C.Int64(value1), C.Int64(value2) -> value1 = value2
        | C.Float32(value1), C.Float32(value2) -> value1 = value2
        | C.Float64(value1), C.Float64(value2) -> value1 = value2
        | C.Char16(value1), C.Char16(value2) -> value1 = value2
        | C.Utf16(value1), C.Utf16(value2) -> value1 = value2
        | C.True, C.True
        | C.False, C.False -> true
        | _ -> false

    let areConstantsDefinitelyNotEqual (c1: C<'Type, 'Function>) (c2: C<'Type, 'Function>) =
        match c1, c2 with
        | C.UInt8(value1), C.UInt8(value2) -> value1 <> value2
        | C.Int8(value1), C.Int8(value2) -> value1 <> value2
        | C.UInt16(value1), C.UInt16(value2) -> value1 <> value2
        | C.Int16(value1), C.Int16(value2) -> value1 <> value2
        | C.UInt32(value1), C.UInt32(value2) -> value1 <> value2
        | C.Int32(value1), C.Int32(value2) -> value1 <> value2
        | C.UInt64(value1), C.UInt64(value2) -> value1 <> value2
        | C.Int64(value1), C.Int64(value2) -> value1 <> value2
        | C.Float32(value1), C.Float32(value2) -> value1 <> value2
        | C.Float64(value1), C.Float64(value2) -> value1 <> value2
        | C.Char16(value1), C.Char16(value2) -> value1 <> value2
        | C.Utf16(value1), C.Utf16(value2) -> value1 <> value2
        | C.True, C.False -> true
        | C.False, C.True -> true
        | _ -> false

    let NoRange = OlyIRDebugSourceTextRange.Empty

    let OptimizeImmediateConstantFolding irExpr =
        match irExpr with
        | E.Operation(irTextRange, op) ->
            match op with
            | O.Add(E.Value(_, V.Constant(C.Int32 value1, _)), E.Value(_, V.Constant(C.Int32 value2, _)), resultTy) ->
                E.Value(irTextRange, V.Constant(C.Int32(value1 + value2), resultTy)) 

            | O.Equal(irArgExpr1, irArgExpr2, resultTy) ->
                match irArgExpr1, irArgExpr2 with
                | E.Value(_, V.Constant(c1, _)), E.Value(_, V.Constant(c2, _)) ->
                    if areConstantsDefinitelyEqual c1 c2 then
                        E.Value(NoRange, V.Constant(C.True, resultTy))
                    elif areConstantsDefinitelyNotEqual c1 c2 then
                        E.Value(NoRange, V.Constant(C.False, resultTy))
                    else
                        irExpr

                | E.Operation(op=O.Cast(E.Value(_, V.Constant(c1, _)), _)),
                  E.Operation(op=O.Cast(E.Value(_, V.Constant(c2, _)), _)) ->
                    if areConstantsDefinitelyEqual c1 c2 then
                        E.Value(NoRange, V.Constant(C.True, resultTy))
                    elif areConstantsDefinitelyNotEqual c1 c2 then
                        E.Value(NoRange, V.Constant(C.False, resultTy))
                    else
                        irExpr

                | _ ->
                    irExpr

            | O.NotEqual(irArgExpr1, irArgExpr2, resultTy) ->
                match irArgExpr1, irArgExpr2 with
                | E.Value(_, V.Constant(c1, _)), E.Value(_, V.Constant(c2, _)) ->
                    if areConstantsDefinitelyNotEqual c1 c2 then
                        E.Value(NoRange, V.Constant(C.True, resultTy))
                    elif areConstantsDefinitelyEqual c1 c2 then
                        E.Value(NoRange, V.Constant(C.False, resultTy))
                    else
                        irExpr

                | E.Operation(op=O.Cast(E.Value(_, V.Constant(c1, _)), _)),
                  E.Operation(op=O.Cast(E.Value(_, V.Constant(c2, _)), _)) ->
                    if areConstantsDefinitelyNotEqual c1 c2 then
                        E.Value(NoRange, V.Constant(C.True, resultTy))
                    elif areConstantsDefinitelyEqual c1 c2 then
                        E.Value(NoRange, V.Constant(C.False, resultTy))
                    else
                        irExpr

                | _ ->
                    irExpr

            | O.GreaterThan(irArgExpr1, irArgExpr2, resultTy) ->
                match irArgExpr1, irArgExpr2 with
                | E.Value(_, V.Constant(c1, _)), E.Value(_, V.Constant(c2, _)) ->
                    match c1, c2 with
                    | C.UInt8(value1), C.UInt8(value2) ->
                        if value1 > value2 then
                            E.Value(NoRange, V.Constant(C.True, resultTy))
                        else
                            E.Value(NoRange, V.Constant(C.False, resultTy))
                    | _ ->
                        irExpr
                | _ ->
                    irExpr

            | O.GreaterThanOrEqual(irArgExpr1, irArgExpr2, resultTy) ->
                match irArgExpr1, irArgExpr2 with
                | E.Value(_, V.Constant(c1, _)), E.Value(_, V.Constant(c2, _)) ->
                    match c1, c2 with
                    | C.UInt8(value1), C.UInt8(value2) ->
                        if value1 >= value2 then
                            E.Value(NoRange, V.Constant(C.True, resultTy))
                        else
                            E.Value(NoRange, V.Constant(C.False, resultTy))
                    | _ ->
                        irExpr
                | _ ->
                    irExpr

            | O.LessThan(irArgExpr1, irArgExpr2, resultTy) ->
                match irArgExpr1, irArgExpr2 with
                | E.Value(_, V.Constant(c1, _)), E.Value(_, V.Constant(c2, _)) ->
                    match c1, c2 with
                    | C.UInt8(value1), C.UInt8(value2) ->
                        if value1 < value2 then
                            E.Value(NoRange, V.Constant(C.True, resultTy))
                        else
                            E.Value(NoRange, V.Constant(C.False, resultTy))
                    | _ ->
                        irExpr
                | _ ->
                    irExpr

            | O.LessThanOrEqual(irArgExpr1, irArgExpr2, resultTy) ->
                match irArgExpr1, irArgExpr2 with
                | E.Value(_, V.Constant(c1, _)), E.Value(_, V.Constant(c2, _)) ->
                    match c1, c2 with
                    | C.UInt8(value1), C.UInt8(value2) ->
                        if value1 <= value2 then
                            E.Value(NoRange, V.Constant(C.True, resultTy))
                        else
                            E.Value(NoRange, V.Constant(C.False, resultTy))
                    | _ ->
                        irExpr
                | _ ->
                    irExpr

            | O.BitwiseOr(irArgExpr1, irArgExpr2, resultTy) ->
                match irArgExpr1, irArgExpr2 with
                | E.Value(_, V.Constant(c1, _)), E.Value(_, V.Constant(c2, _)) ->
                    match c1, c2 with
                    | C.UInt8(value1), C.UInt8(value2) ->
                        E.Value(NoRange, V.Constant(C.UInt8(value1 ||| value2), resultTy))
                    | C.Int32(value1), C.Int32(value2) ->
                        E.Value(NoRange, V.Constant(C.Int32(value1 ||| value2), resultTy))
                    | _ ->
                        irExpr
                | _ ->
                    irExpr

            | O.Not(E.Value(value=V.Constant(C.True, _)), resultTy) ->
                E.Value(NoRange, V.Constant(C.False, resultTy))

            | O.Not(E.Value(value=V.Constant(C.False, _)), resultTy) ->
                E.Value(NoRange, V.Constant(C.True, resultTy))

            | _ ->
                irExpr
        | _ ->
            irExpr