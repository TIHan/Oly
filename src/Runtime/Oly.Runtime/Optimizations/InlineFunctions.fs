module rec Oly.Runtime.CodeGen.Internal.InlineFunctions

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

[<Literal>]
let RecursiveInlineLimit = 3

let canInline optenv (func: RuntimeFunction) =
    if func.Flags.IsInlineable then
        let key = func
        match optenv.inlineSet.TryGetValue key with
        | true, count ->
            if count > RecursiveInlineLimit then
                false
            else
                true
        | _ ->
            true
    else
        false

let pushInline optenv (func: RuntimeFunction) =
    let key = func
    match optenv.inlineSet.TryGetValue key with
    | true, count ->
        if count >= RecursiveInlineLimit then
            false
        else
            optenv.inlineSet[key] <- count + 1
            true
    | _ ->
        optenv.inlineSet[key] <- 1
        true

let popInline optenv (func: RuntimeFunction) =
    let key = func
    match optenv.inlineSet.TryGetValue key with
    | true, count ->
        if count = 1 then
            optenv.inlineSet.Remove(key) |> ignore
        else
            optenv.inlineSet[key] <- count - 1
    | _ ->
        ()

let isPassthroughExpression argCount irExpr =
    match irExpr with
    | E.Operation(_, irOp) when irOp.ArgumentCount = argCount && not(isBaseCall irOp) ->
        let mutable isPassthrough = true
        irOp.ForEachArgument (fun i irArgExpr ->
            match irArgExpr with
            | E.Value(value=V.Argument(argIndex, _)) ->
                if isPassthrough then
                    isPassthrough <- argIndex = i
            | _ ->
                isPassthrough <- false
        )
        isPassthrough
    | _ ->
        false

let tryGetFunctionBody optenv func =
    optenv.tryGetFunctionBody func

let transformClosureConstructorCallToUseMoreSpecificTypeArgument (forwardSubLocals: Dictionary<int, ForwardSubValue<_, _, _>>) optenv (irCtor: OlyIRFunction<_, _, _>) (argExprs: E<_, _, _> imarray) resultTy =
    OlyAssert.True(irCtor.IsClosureInstanceConstructor)

    let ctor = irCtor.RuntimeFunction
    let enclosingTy = ctor.EnclosingType
    let enclosingTyArgs = enclosingTy.TypeArguments

    // No type arguments to transform.
    if enclosingTyArgs.IsEmpty then
        (irCtor, argExprs, resultTy, false)
    else
        let mutable didChangeTyArg = false

        let enclosingTyArgs = enclosingTyArgs.ToBuilder()
        let newArgExprs =
            argExprs
            |> ImArray.mapi (fun i argExpr ->
                match argExpr with
                | E.Value(_, V.Local(localIndex, _)) ->
                    match forwardSubLocals.TryGetValue(localIndex) with
                    | true, ForwardSubValue.LoadInlineableFunction(_, irFunc, funcReceiverExpr, _) when irFunc.HasEnclosingClosureType && canInline optenv irFunc.RuntimeFunction ->
                        enclosingTyArgs[i] <- irFunc.RuntimeFunction.EnclosingType
                        didChangeTyArg <- true
                        funcReceiverExpr
                    | _ ->
                        argExpr
                | _ ->
                    argExpr
            )
            
        if didChangeTyArg then
            let ctor = ctor.Formal.MakeInstance(enclosingTy.Formal.Apply(enclosingTyArgs.MoveToImmutable()).SetWitnesses(enclosingTy.Witnesses), ctor.TypeArguments).SetWitnesses(ctor.Witnesses)

            let emittedCtor = optenv.emitFunction(optenv.func, ctor)
            let resultTy = optenv.emitType(ctor.EnclosingType)

            let irCtor = OlyIRFunction(emittedCtor, ctor)

            (irCtor, newArgExprs, resultTy, true)
        else
            (irCtor, argExprs, resultTy, false)

let transformClosureInvokeToUseMoreSpecificTypeArgument (forwardSubLocals: Dictionary<int, ForwardSubValue<_, _, _>>) (optenv: optenv<_, _, _>) (irFunc: OlyIRFunction<_, _, _>) (argExprs: E<_, _, _> imarray) =
    OlyAssert.True(irFunc.IsClosureInstanceInvoke)

    // TODO: This has a minor issue in that we could substitute multiple locals with a new closure.
    //       Instead, we should find a way to simply create a tmp local for the new closure and use that.
    //       Then we let dead code elimination take care of the rest.

    if optenv.IsDebuggable then
        (irFunc, argExprs)
    else

    let newArgExprs =
        argExprs
        |> ImArray.mapi (fun i argExpr ->
            if i = 0 then
                match argExpr with
                | E.Value(textRange, V.Local(localIndex, _)) ->
                    match forwardSubLocals.TryGetValue(localIndex) with
                    | true, ForwardSubValue.NewClosure(_, cloCtor, cloArgExprs, cloResultTy) ->
                        let cloCtor, cloArgExprs, cloResultTy, didChange = 
                            transformClosureConstructorCallToUseMoreSpecificTypeArgument
                                forwardSubLocals
                                optenv
                                cloCtor
                                cloArgExprs
                                cloResultTy
                        if didChange then
                            E.Operation(textRange, O.New(cloCtor, cloArgExprs, cloResultTy))
                        else
                            argExpr
                    | _ ->
                        argExpr
                | _ ->
                    argExpr
            else
                argExpr
        )
    let func, didChange =
        if newArgExprs.Length > 0 then
            match newArgExprs[0] with
            | E.Operation(_, O.New(cloCtor, _, _)) when cloCtor.HasEnclosingClosureType ->
                if irFunc.RuntimeFunction.EnclosingType <> cloCtor.RuntimeFunction.EnclosingType then
                    let rfunc = irFunc.RuntimeFunction.Formal.MakeInstance(cloCtor.RuntimeFunction.EnclosingType, irFunc.RuntimeFunction.TypeArguments).SetWitnesses(irFunc.RuntimeFunction.Witnesses)
                    let emittedFunc = optenv.emitFunction(optenv.func, rfunc)
                    OlyIRFunction(emittedFunc, rfunc), true
                else
                    irFunc, false

            | _ ->
                irFunc, false
        else
            irFunc, false

    if didChange then
        (func, newArgExprs)
    else
        (func, argExprs)

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type ForwardSubValue<'Type, 'Function, 'Field> =
    | Local of localIndex: int * isNew: bool
    | LocalAddress of localIndex: int * kind: OlyIRByRefKind
    | Argument of argIndex: int
    | ArgumentAddress of argIndex: int * kind: OlyIRByRefKind
    | Constant of C<'Type, 'Function>
    | Function of func: OlyIRFunction<'Type, 'Function, 'Field>

    | LoadInlineableFunction of newLocalIndex: int * OlyIRFunction<'Type, 'Function, 'Field> * receiver: E<'Type, 'Function, 'Field> * resultTy: 'Type
    | NewClosure of newLocalIndex: int * OlyIRFunction<'Type, 'Function, 'Field> * argExprs: E<'Type, 'Function, 'Field> imarray * resultTy: 'Type

let recordForwardSub (forwardSubLocals: Dictionary<int, ForwardSubValue<_, _, _>>) (optenv: optenv<_, _, _>) localIndex rhsExpr =
    if optenv.IsLocalMutable(localIndex) |> not then
        match rhsExpr with
        | E.Operation(op=O.LoadFunction(func, argExpr, resultTy)) when canInline optenv func.RuntimeFunction && canSafelyPropagate optenv argExpr ->
            forwardSubLocals[localIndex] <- ForwardSubValue.LoadInlineableFunction(localIndex, func, argExpr, resultTy)
        | E.Operation(op=O.New(irCtor, argExprs, resultTy)) 
                when irCtor.IsClosureInstanceConstructor && 
                        argExprs |> ImArray.forall (canSafelyPropagateForNewClosure optenv) ->
            forwardSubLocals[localIndex] <- ForwardSubValue.NewClosure(localIndex, irCtor, argExprs, resultTy)

        | _ ->
            ()

let handleLiberalForwardSub (forwardSubLocals: Dictionary<int, ForwardSubValue<_, _, _>>) (optenv: optenv<_, _, _>) origExpr =
    // TODO: Decide whether to keep this or not.
    if true || optenv.IsDebuggable then
        origExpr
    else
        match origExpr with
        | E.Value(origTextRange, value) ->
            match value with
            | V.Local(localIndex, resultTy) ->
                match forwardSubLocals.TryGetValue(localIndex) with
                | true, subValue ->
                    match subValue with
                    | ForwardSubValue.Local(localIndex, _) ->
                        E.Value(origTextRange, V.Local(localIndex, resultTy))
                        |> handleLiberalForwardSub forwardSubLocals optenv
                    | ForwardSubValue.Argument(argIndex) ->
                        E.Value(origTextRange, V.Argument(argIndex, resultTy))
                    | ForwardSubValue.NewClosure(_, ctor, ctorArgExprs, _) ->
                        E.Operation(origTextRange, O.New(ctor, ctorArgExprs, resultTy))
                    | ForwardSubValue.LoadInlineableFunction(_, func, receiverExpr, _) ->
                        E.Operation(origTextRange, O.LoadFunction(func, receiverExpr, resultTy))
                    | _ ->
                        origExpr
                | _ ->
                    origExpr
            | _ ->
                origExpr
        | _ ->
            origExpr

let tryInlineFunction (forwardSubLocals: Dictionary<int, ForwardSubValue<_, _, _>>) optenv irExpr =
    match irExpr with
    | E.Operation(irTextRange, O.CallIndirect(_, E.Operation(op=O.LoadFunction(irFunc, irArgExpr, _)), irArgExprs, resultTy)) ->
        let irCallExprToInline =
            E.Operation(irTextRange,
                O.Call(irFunc,
                    irArgExprs
                    |> ImArray.prependOne irArgExpr,
                    resultTy
                )
            )
        tryInlineFunction forwardSubLocals optenv irCallExprToInline

    | E.Operation(irTextRange, O.CallIndirect(_, E.Value(value=V.Function(irFunc, _)), irArgExprs, resultTy)) ->
        let irCallExprToInline =
            E.Operation(irTextRange,
                O.Call(irFunc,
                    irArgExprs,
                    resultTy
                )
            )
        tryInlineFunction forwardSubLocals optenv irCallExprToInline

    | E.Operation(_, O.CallIndirect(argTys, receiverExpr, argExprs, resultTy)) ->
        tryInlineCallIndirect forwardSubLocals optenv argTys receiverExpr argExprs resultTy

    | E.Operation(irTextRange, O.Call(irFunc, irArgExprs, resultTy)) when canInline optenv irFunc.RuntimeFunction ->
        let func = irFunc.RuntimeFunction

        if not <| pushInline optenv func then
            let emittedFunc = optenv.emitFunction(optenv.func, func)
            E.Operation(irTextRange, O.Call(OlyIRFunction(emittedFunc, func), irArgExprs, resultTy))
            |> Some
        else

        match tryGetFunctionBody optenv func with
        | Some(irFuncBody) ->
#if DEBUG || CHECKED
            Log(
                let witnesses = func.Witnesses
                let witnessText = 
                    if witnesses.IsEmpty then
                        ""
                    else
                        let text = witnesses |> ImArray.map (fun x -> x.TypeExtension.Name.ToString()) |> (String.concat "\n")
                        $" - Witnesses: {text}"
                $"Inlining Function: {func.EnclosingType.Name}.{func.Name}{witnessText}"
            )
#endif

            let parCount =
                if func.Flags.IsStatic then
                    func.Parameters.Length
                else
                    func.Parameters.Length + 1

            let irFuncBodyExpr = irFuncBody.Expression

            if isPassthroughExpression irArgExprs.Length irFuncBodyExpr then
                match irFuncBodyExpr with
                | E.Operation(irTextRange, irOp) ->
                    let irInlinedExpr = E.Operation(irTextRange, irOp.ReplaceArguments(irArgExprs))
                    let irInlinedExpr =
                        match tryInlineFunction forwardSubLocals optenv irInlinedExpr with
                        | Some expr -> expr
                        | _ -> irInlinedExpr
                    popInline optenv func                 
                    Some irInlinedExpr
                | _ ->
                    OlyAssert.Fail("Invalid passthrough expression.")
            else

            let argMap =
                ImArray.init parCount (fun i ->
                    let isMutable = irFuncBody.ArgumentFlags[i].HasFlag(OlyIRLocalFlags.Mutable)
                    let isArgAddressExposed = irFuncBody.ArgumentFlags[i].HasFlag(OlyIRLocalFlags.AddressExposed)

                    let isForwardSub =
                        if isMutable then
                            false
                        elif isArgAddressExposed then
                            match irArgExprs[i] with
                            | E.Operation(op=O.LoadFunction(func, argExpr, _)) when canInline optenv func.RuntimeFunction ->
                                match argExpr with
                                | E.Let(localIndex=localIndex;rhsExpr=rhsExpr;bodyExpr=E.Value(value=V.LocalAddress(index=localIndex2)))
                                        when (localIndex = localIndex2) && canSafelyPropagate optenv rhsExpr ->
                                    OlyAssert.False(canSafelyPropagate optenv argExpr)
                                    true
                                | _ ->
                                    false
                            | _ ->
                                false
                        else
                            match irArgExprs[i] with
                            | E.Value(value=irValue) ->
                                match irValue with
                                | V.Local(localIndex, _) ->
                                    // This is conservative.
                                    not(optenv.IsLocalMutable(localIndex))
                                //| V.LocalAddress _ ->
                                //    true
                                | V.Argument(argIndex, _) ->
                                    // This is conservative.
                                    not(optenv.IsArgumentMutable(argIndex))
                                //| V.ArgumentAddress _ ->
                                //    true
                                | V.Constant _ ->
                                    true
                                | _ -> 
                                    false
                            | E.Operation(op=O.LoadFunction(func, argExpr, _)) when canInline optenv func.RuntimeFunction ->
                                if canSafelyPropagate optenv argExpr then
                                    true
                                else
                                    match argExpr with
                                    | E.Let(localIndex=localIndex;rhsExpr=rhsExpr;bodyExpr=E.Value(value=V.LocalAddress(index=localIndex2)))
                                            when (localIndex = localIndex2) && canSafelyPropagate optenv rhsExpr ->
                                        true
                                    | _ ->
                                        false
                            | E.Operation(op=O.New(ctor, argExprs, _)) 
                                    when ctor.IsClosureInstanceConstructor && 
                                         argExprs |> ImArray.forall (canSafelyPropagateForNewClosure optenv) ->
                                true

                            | E.Let(_, localIndex, E.Operation(op=O.New(ctor, argExprs, _)), E.Value(value=V.LocalAddress(localIndex2, _, _)))
                                when localIndex = localIndex2 && ctor.IsClosureInstanceConstructor && 
                                         argExprs |> ImArray.forall (canSafelyPropagateForNewClosure optenv) ->
                                true

                            | _ ->
                                false

                    if isForwardSub then
                        match irArgExprs[i] with
                        | E.Value(value=irValue) ->
                            match irValue with
                            | V.Local(localIndex, _) -> 
                                ForwardSubValue.Local(localIndex, false)
                            | V.LocalAddress(index=localIndex) -> 
                                ForwardSubValue.Local(localIndex, false)
                            | V.Argument(index=argIndex) -> 
                                ForwardSubValue.Argument(argIndex)
                            | V.ArgumentAddress(index=argIndex) -> 
                                ForwardSubValue.Argument(argIndex)
                            | V.Constant(constant, _) -> 
                                ForwardSubValue.Constant(constant)
                            | _ -> 
                                OlyAssert.Fail($"bad forwardsub {irValue}")

                        | E.Operation(op=O.LoadFunction(func, argExpr, resultTy)) ->
                            let newLocalIndex = optenv.CreateLocal(irFuncBody.ArgumentFlags[i])
                            let subValue = ForwardSubValue.LoadInlineableFunction(newLocalIndex, func, argExpr, resultTy)
                            forwardSubLocals[newLocalIndex] <- subValue
                            subValue

                        | E.Operation(op=O.New(ctor, argExprs, resultTy)) ->
                            let newLocalIndex = optenv.CreateLocal(irFuncBody.ArgumentFlags[i])
                            let subValue = ForwardSubValue.NewClosure(newLocalIndex, ctor, argExprs, resultTy)
                            forwardSubLocals[newLocalIndex] <- subValue
                            subValue

                        | E.Let(_, _, E.Operation(op=O.New(ctor, argExprs, _)), E.Value(value=V.LocalAddress(_, _, _))) ->
                            let newLocalIndex = optenv.CreateLocal(irFuncBody.ArgumentFlags[i])
                            let subValue = ForwardSubValue.NewClosure(newLocalIndex, ctor, argExprs, resultTy)
                            forwardSubLocals[newLocalIndex] <- subValue
                            subValue

                        | _ ->
                            OlyAssert.Fail($"bad forwardsub {irExpr}")
                    else                
                        ForwardSubValue.Local(optenv.CreateLocal(irFuncBody.ArgumentFlags[i]), true)
                )

            let localOffset = optenv.LocalCount

            for i = 0 to irFuncBody.LocalCount - 1 do
                optenv.CreateLocal(irFuncBody.LocalFlags[i])
                |> ignore

            let mutable hasInlineFailed = false

            let irFinalExpr =
                inlineFunction forwardSubLocals optenv func localOffset argMap irFuncBody &hasInlineFailed

            // Inlining can fail if we find a base call in the function's body.
            //
            // This is somewhat arbitrary. It isn't necessarily "wrong", but since Oly's main target is .NET,
            // the ECMA spec says that you cannot make a base call if the 'this' parameter is not 'this'.
            // Inlining can cause this rule to be broken for .NET. So, the Oly runtime adopted the same rule.
            //
            // The newly created locals will get cleaned up in normalization.
            // Ideally, we should learn about this information earlier, but it is tricky since we determine inlineability before
            // resolving the function's body. By 'resolving', it means the IL -> IR transformation, or importing phase.
            // 
            // This seems a little wasteful because we could be constantly trying to inline a function that cannot be inlined.
            // However, we need this for correctness. If the runtime doesn't want to be wasteful, the front-end compilers should mark
            // these functions as *never* to inline if they detect a base call in their body.
            //
            // We could be a little more optimistic in that we *could* inline the function that makes a base call IF
            // we are inlining in the same enclosing type. But at the moment, this is conservative.
            if hasInlineFailed then
                popInline optenv func
                None
            else

            let irInlinedExpr =
                let normalizeLet expr =
                    match expr with
                    | E.Let(localName, localIndex, rhsExpr, E.Value(textRange, V.LocalAddress(localIndexAddress, kind, resultTy))) when localIndex = localIndexAddress ->
                        let newLocalIndex = optenv.argLocalManager.CreateLocal(optenv.argLocalManager.GetLocalFlags(localIndex))
                        E.Let(localName, newLocalIndex, rhsExpr, E.Value(textRange, V.LocalAddress(newLocalIndex, kind, resultTy)))
                    | _ ->
                        expr
                (irFinalExpr, argMap)
                ||> ImArray.foldBacki (fun irAccExpr i subValue ->
                    match subValue with
                    | ForwardSubValue.Local(localIndex, true) ->
                        E.Let("tmp", localIndex, normalizeLet irArgExprs[i], irAccExpr)
                    | ForwardSubValue.LoadInlineableFunction(localIndex, _, _, _) ->
                        E.Let("tmpFunc", localIndex, normalizeLet irArgExprs[i], irAccExpr)
                    | ForwardSubValue.NewClosure(localIndex, _, _, _) ->
                        E.Let("tmpClo", localIndex, normalizeLet irArgExprs[i], irAccExpr)
                    | _ ->
                        irAccExpr
                )

            popInline optenv func

            Some irInlinedExpr
        | _ ->
            None
    | _ ->
        None

let createCallThenTryInlineFunction (forwardSubLocals: Dictionary<int, ForwardSubValue<_, _, _>>) optenv func receiverArgExpr argExprs resultTy =
    let expr =
        E.Operation(NoRange,
            O.Call(func, argExprs |> ImArray.prependOne receiverArgExpr, resultTy)
        )
    tryInlineFunction forwardSubLocals optenv expr

let tryInlineCallIndirect (forwardSubLocals: Dictionary<int, ForwardSubValue<_, _, _>>) (optenv: optenv<_, _, _>) argTys receiverExpr argExprs resultTy =
    match receiverExpr with

    | E.Let(_, localIndex, rhsExpr, E.Value(value=V.LocalAddress(localIndex2, _, _))) when localIndex = localIndex2 ->
        match rhsExpr with
        | E.Operation(_, O.LoadFunction(func, receiverArgExpr, _)) ->
            if canSafelyPropagate optenv receiverArgExpr then
                createCallThenTryInlineFunction
                    forwardSubLocals
                    optenv
                    func
                    receiverArgExpr
                    argExprs
                    resultTy
            else
                match receiverArgExpr with
                | E.Let(localName, localIndex, rhsExpr, E.Value(ilTextRange, V.LocalAddress(localIndex2, localByRefKind, localResultTy))) 
                        when (localIndex = localIndex2) && canSafelyPropagate optenv rhsExpr ->

                    let newReceiverArgExpr =
                        // 'receiverArgExpr' can actually be safe if we create new local indices.
                        // We do this to avoid duplication of local indices.
                        let newLocalIndex = optenv.CreateLocal(optenv.GetLocalFlags(localIndex))
                        E.Let(localName, newLocalIndex, rhsExpr, E.Value(ilTextRange, V.LocalAddress(newLocalIndex, localByRefKind, localResultTy)))

                    createCallThenTryInlineFunction
                        forwardSubLocals
                        optenv
                        func
                        newReceiverArgExpr
                        argExprs
                        resultTy
                | _ ->
                    None
        | _ ->
            None

    | E.Value(value=V.Local(localIndex, _)) ->
        match forwardSubLocals.TryGetValue(localIndex) with
        | true, ForwardSubValue.LoadInlineableFunction(_, func, receiverExpr, innerResultTy) ->
            let receiverExpr = handleLiberalForwardSub forwardSubLocals optenv receiverExpr
            let expr =
                E.Operation(NoRange,
                    O.CallIndirect(argTys, E.Operation(NoRange, O.LoadFunction(func, receiverExpr, innerResultTy)), argExprs, resultTy)
                )
            tryInlineFunction forwardSubLocals optenv expr
        | _ ->
            None

    | _ ->
        None

let inlineFunction (forwardSubLocals: Dictionary<int, ForwardSubValue<_, _, _>>) (optenv: optenv<_, _, _>) (func: RuntimeFunction) localOffset (argMap: ForwardSubValue<_, _, _> imarray) (irFuncBody: OlyIRFunctionBody<_, _, _>) (hasInlineFailed: byref<bool>) =

    let mutable didFailInline = false

    let optimizeOperation irExpr =
        match irExpr with
        | E.Operation(irTextRange, irOp) ->
            match irOp with
            | O.Store(localIndex, irRhsExpr, resultTy) ->
                let fixedLocalIndex = localOffset + localIndex
                OlyAssert.True(optenv.IsLocalMutable(fixedLocalIndex))
                E.Operation(irTextRange, O.Store(fixedLocalIndex, irRhsExpr, resultTy))

            | O.StoreArgument(argIndex, irRhsExpr, resultTy) ->
                match argMap[argIndex] with
                | ForwardSubValue.Local(localIndex, _) ->
                    OlyAssert.True(optenv.IsLocalMutable(localIndex))
                    E.Operation(irTextRange, O.Store(localIndex, irRhsExpr, resultTy))

                | sub ->
                    OlyAssert.Fail($"StoreArgument: bad forwardsub {sub}")

            | O.Call _
            | O.CallIndirect _ ->
                match tryInlineFunction forwardSubLocals optenv irExpr with
                | Some(inlinedExpr) -> inlinedExpr
                | _ -> irExpr

            | _ ->
                irExpr
        | _ ->
            OlyAssert.Fail("Expected opertion")

    let rec handleOperation irTextRange irExpr (irOp: O<_, _, _>) =
        //let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> handleExpression argExpr)
        //if newOp = irOp then
        //    optimizeOperation irExpr
        //else
        //    E.Operation(irTextRange, newOp)
        //    |> optimizeOperation
        let irNewArgExprs = irOp.MapArguments(fun _ irArgExpr -> handleExpression irArgExpr)
        let mutable areSame = true
        irOp.ForEachArgument(fun i irArgExpr ->
            if irNewArgExprs[i] <> irArgExpr then
                areSame <- false
        )
        if areSame then
            optimizeOperation irExpr
        else
            let irNewOp = irOp.ReplaceArguments(irNewArgExprs) 
            E.Operation(irTextRange, irNewOp)
            |> optimizeOperation

    and handleCallIndirect origExpr =
        match origExpr with
        | E.Operation(origTextRange, origOp) ->
            match origOp with

            // Arguments

            | O.CallIndirect(argTys, (E.Value(value=V.Argument(argIndex, _)) as callReceiverExpr), argExprs, resultTy)
            | O.CallIndirect(argTys, (E.Value(value=V.ArgumentAddress(argIndex, _, _)) as callReceiverExpr), argExprs, resultTy) ->
                match argMap[argIndex] with
                | ForwardSubValue.LoadInlineableFunction(_, funcToLoad, receiverExpr, innerResultTy) ->
                    let newCallReceiverExpr = E.Operation(NoRange, O.LoadFunction(funcToLoad, receiverExpr, innerResultTy))
                    let newCallReceiverExpr =
                        match callReceiverExpr with
                        | E.Value(value=V.ArgumentAddress(_, addrKind, addrResultTy)) ->
                            let newLocalIndex = optenv.CreateLocal(irFuncBody.ArgumentFlags[argIndex])
                            E.Let(String.Empty, newLocalIndex, newCallReceiverExpr, E.Value(NoRange, V.LocalAddress(newLocalIndex, addrKind, addrResultTy)))
                        | _ ->
                            newCallReceiverExpr
                    let newArgExprs = argExprs |> ImArray.map (handleExpression)
                    let expr =
                        E.Operation(origTextRange,
                            O.CallIndirect(argTys, newCallReceiverExpr, newArgExprs, resultTy)
                        )
                    match tryInlineFunction forwardSubLocals optenv expr with
                    | Some(expr) -> expr
                    | _ ->
                        Log($"Unable to inline call indirect expression:\n{expr}\n")
                        handleOperation origTextRange origExpr origOp
                | _ ->
                    handleOperation origTextRange origExpr origOp

            | O.CallIndirect(argTys, E.Operation(op=O.LoadField(field, E.Value(value=V.Argument(argIndex, _)), _)), argExprs, resultTy)
                    when field.RuntimeEnclosingType.IsClosure ->
                match argMap[argIndex] with
                | ForwardSubValue.NewClosure(_, _, ctorArgExprs, _) ->
                    let newArgExprs = argExprs |> ImArray.map (handleExpression)

                    let receiverExpr =
                        let receiverExpr = ctorArgExprs[field.RuntimeField.Value.Index]
                        match receiverExpr with
                        | E.Value(textRange, V.Local(localIndex, _)) ->
                            match forwardSubLocals.TryGetValue(localIndex) with
                            | true, ForwardSubValue.LoadInlineableFunction(_, func, funcReceiverExpr, resultTy) ->
                                E.Operation(textRange, O.LoadFunction(func, funcReceiverExpr, resultTy))
                            | _ ->
                                receiverExpr
                        | _ ->
                            receiverExpr

                    let expr =
                        E.Operation(origTextRange,
                            O.CallIndirect(argTys, receiverExpr, newArgExprs, resultTy)
                        )
                    match tryInlineFunction forwardSubLocals optenv expr with
                    | Some(expr) -> expr
                    | _ ->
                        Log($"Unable to inline call indirect expression: \n{expr}\n")
                        handleOperation origTextRange origExpr origOp
                | _ ->
                    handleOperation origTextRange origExpr origOp  
                    
            | O.CallIndirect(argTys, E.Operation(op=O.LoadFieldAddress(field, E.Value(value=V.Argument(argIndex, _)), _, _)), argExprs, resultTy)
                    when field.RuntimeEnclosingType.IsClosure ->
                match argMap[argIndex] with
                | ForwardSubValue.NewClosure(_, _, ctorArgExprs, _) ->
                    let newArgExprs = argExprs |> ImArray.map (handleExpression)
                    let expr =
                        E.Operation(origTextRange,
                            O.CallIndirect(argTys, handleLiberalForwardSub forwardSubLocals optenv ctorArgExprs[field.RuntimeField.Value.Index], newArgExprs, resultTy)
                        )
                    match tryInlineFunction forwardSubLocals optenv expr with
                    | Some(expr) -> expr
                    | _ ->
                        Log($"Unable to inline call indirect expression:\n{expr}\n")
                        handleOperation origTextRange origExpr origOp
                | _ ->
                    handleOperation origTextRange origExpr origOp

            // Locals

            | O.CallIndirect(argTys, E.Value(value=V.Local(localIndex, _)), argExprs, resultTy) ->
                match forwardSubLocals.TryGetValue(localIndex) with
                | true, ForwardSubValue.LoadInlineableFunction(_, func, receiverExpr, innerResultTy) ->
                    let newArgExprs = argExprs |> ImArray.map (handleExpression)
                    let expr =
                        E.Operation(origTextRange,
                            O.CallIndirect(argTys, E.Operation(NoRange, O.LoadFunction(func, receiverExpr, innerResultTy)), newArgExprs, resultTy)
                        )
                    match tryInlineFunction forwardSubLocals optenv expr with
                    | Some(expr) -> expr
                    | _ ->
                        Log($"Unable to inline call indirect expression:\n{expr}\n")
                        handleOperation origTextRange origExpr origOp
                | _ ->
                    handleOperation origTextRange origExpr origOp

            | O.CallIndirect(argTys, E.Operation(op=O.LoadField(field, E.Value(value=V.Local(localIndex, _)), _)), argExprs, resultTy)
                    when field.RuntimeEnclosingType.IsClosure ->
                match forwardSubLocals.TryGetValue(localIndex) with
                | true, ForwardSubValue.NewClosure(_, _, ctorArgExprs, _) ->
                    let newArgExprs = argExprs |> ImArray.map (handleExpression)
                    let expr =
                        E.Operation(origTextRange,
                            O.CallIndirect(argTys, handleLiberalForwardSub forwardSubLocals optenv ctorArgExprs[field.RuntimeField.Value.Index], newArgExprs, resultTy)
                        )
                    match tryInlineFunction forwardSubLocals optenv expr with
                    | Some(expr) -> expr
                    | _ ->
                        Log($"Unable to inline call indirect expression:\n{expr}\n")
                        handleOperation origTextRange origExpr origOp
                | _ ->
                    handleOperation origTextRange origExpr origOp  
                    
            | O.CallIndirect(argTys, E.Operation(op=O.LoadFieldAddress(field, E.Value(value=V.Local(localIndex, _)), _, _)), argExprs, resultTy)
                    when field.RuntimeEnclosingType.IsClosure ->
                match forwardSubLocals.TryGetValue(localIndex) with
                | true, ForwardSubValue.NewClosure(_, _, ctorArgExprs, _) ->
                    let newArgExprs = argExprs |> ImArray.map (handleExpression)
                    let expr =
                        E.Operation(origTextRange,
                            O.CallIndirect(argTys, handleLiberalForwardSub forwardSubLocals optenv ctorArgExprs[field.RuntimeField.Value.Index], newArgExprs, resultTy)
                        )
                    match tryInlineFunction forwardSubLocals optenv expr with
                    | Some(expr) -> expr
                    | _ ->
                        Log($"Unable to inline call indirect expression:\n{expr}\n")
                        handleOperation origTextRange origExpr origOp
                | _ ->
                    handleOperation origTextRange origExpr origOp

            // the rest

            | O.CallIndirect _ ->
                handleOperation origTextRange origExpr origOp

            | _ ->
                failwith "Expected 'CallIndirect'"
        | _ ->
            failwith "Expected 'CallIndirect'"

    and handleExpressionAux (origExpr: E<_, _, _>) : E<_, _, _> =
        match origExpr with
        | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
            let irNewRhsExpr = handleExpression irRhsExpr
            let newLocalIndex = localOffset + localIndex

            let rec normalize irNewRhsExpr =
                match irNewRhsExpr with
                | E.Let(name2, localIndex2, rhsExpr2, bodyExpr2) ->
                    E.Let(
                        name2,
                        localIndex2,
                        rhsExpr2,
                        normalize bodyExpr2
                    )
                | _ ->
                    recordForwardSub forwardSubLocals optenv newLocalIndex irNewRhsExpr

                    let irNewBodyExpr = handleExpression irBodyExpr

                    if newLocalIndex = localIndex && irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                        origExpr
                    else
                        E.Let(name, newLocalIndex, irNewRhsExpr, irNewBodyExpr)

            normalize irNewRhsExpr

        | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
            let irNewConditionExpr = handleExpression irConditionExpr

            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.True, _)) ->
                handleExpression irTrueTargetExpr

            | E.Value(value=V.Constant(C.False, _)) ->
                handleExpression irFalseTargetExpr

            | _ ->
                let irNewTrueTargetExpr = handleExpression irTrueTargetExpr
                let irNewFalseTargetExpr = handleExpression irFalseTargetExpr

                if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                    origExpr
                else
                    E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

        | E.While(irConditionExpr, irBodyExpr, resultTy) ->
            let irNewConditionExpr = handleExpression irConditionExpr

            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.False, _)) ->
                E.None(NoRange, resultTy)
            | _ ->
                let irNewBodyExpr = handleExpression irBodyExpr

                if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                    origExpr
                else
                    E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

        | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
            let irNewBodyExpr = handleExpression irBodyExpr

            let mutable didChange = false
            let irNewCatchCases =
                irCatchCases
                |> ImArray.map (fun irCatchCase ->
                    match irCatchCase with
                    | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                        let newLocalIndex = localOffset + localIndex
                        let irNewCaseBodyExpr = handleExpression irCaseBodyExpr

                        if irNewCaseBodyExpr = irCaseBodyExpr && localIndex = newLocalIndex then
                            irCatchCase
                        else
                            didChange <- true
                            OlyIRCatchCase.CatchCase(localName, newLocalIndex, irNewCaseBodyExpr, catchTy)
                )

            let irNewFinallyBodyExprOpt =
                irFinallyBodyExprOpt
                |> Option.map (fun irExpr ->
                    let irNewExpr = handleExpression irExpr
                    if irNewExpr = irExpr then
                        irExpr
                    else
                        didChange <- true
                        irNewExpr
                )

            if irNewBodyExpr = irBodyExpr && not didChange then
                origExpr
            else
                E.Try(irNewBodyExpr, irNewCatchCases, irNewFinallyBodyExprOpt, resultTy)

        | E.Sequential(irExpr1, irExpr2) ->
            let irNewExpr1 = handleExpression irExpr1
            let irNewExpr2 = handleExpression irExpr2

            if hasSideEffect optenv irNewExpr1 then
                if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
                    origExpr
                else
                    E.Sequential(irNewExpr1, irNewExpr2)
            else
                irNewExpr2

        | E.Operation(origTextRange, origOp) ->
            if isBaseCall origOp then
                didFailInline <- true

            match origOp with
            | O.CallIndirect _ ->
                handleCallIndirect origExpr

            // Locals

            | O.Call(func, argExprs, resultTy) when func.HasEnclosingClosureType && canInline optenv func.RuntimeFunction ->
                let newArgExprs =
                    argExprs
                    |> ImArray.map (fun argExpr ->
                        let argExpr = handleExpression argExpr
                        match argExpr with
                        | E.Value(textRange, V.Local(localIndex, _)) ->
                            match forwardSubLocals.TryGetValue(localIndex) with
                            | true, ForwardSubValue.NewClosure(_, cloCtor, cloArgExprs, cloResultTy) ->
                                E.Operation(textRange, O.New(cloCtor, cloArgExprs, cloResultTy))
                            | _ ->
                                argExpr
                        | _ ->
                            argExpr
                    )
                let expr = E.Operation(origTextRange, O.Call(func, newArgExprs, resultTy))
                match tryInlineFunction forwardSubLocals optenv expr with
                | Some(expr) -> expr
                | _ -> 
                    handleOperation origTextRange origExpr origOp

            | O.Call(irFunc, argExprs, resultTy) when irFunc.HasEnclosingClosureType && irFunc.RuntimeFunction.Flags.IsInstance ->
                let newArgExprs = argExprs |> ImArray.map (handleExpression)
                let (func, newArgExprs) = transformClosureInvokeToUseMoreSpecificTypeArgument forwardSubLocals optenv irFunc newArgExprs
                E.Operation(origTextRange, O.Call(func, newArgExprs, resultTy))

            // Arguments

            | O.LoadField(field, E.Value(value=V.Argument(argIndex, _)), _) when field.RuntimeEnclosingType.IsClosure ->
                match argMap[argIndex] with
                | ForwardSubValue.NewClosure(_, _, ctorArgExprs, _) ->
                    ctorArgExprs[field.RuntimeField.Value.Index]
                | _ ->
                    handleOperation origTextRange origExpr origOp

            // Locals

            | O.LoadField(field, E.Value(value=V.Local(localIndex, _)), _) when field.RuntimeEnclosingType.IsClosure ->
                match forwardSubLocals.TryGetValue(localIndex) with
                | true, ForwardSubValue.NewClosure(_, _, ctorArgExprs, _) ->
                    ctorArgExprs[field.RuntimeField.Value.Index]
                | _ ->
                    handleOperation origTextRange origExpr origOp

            | _ ->
                handleOperation origTextRange origExpr origOp

        | E.Value(textRange, irValue) ->
            match irValue with
            | V.Local(localIndex, resultTy) ->
                E.Value(textRange, V.Local(localOffset + localIndex, resultTy))

            | V.LocalAddress(localIndex, kind, resultTy) ->
                E.Value(textRange, V.LocalAddress(localOffset + localIndex, kind, resultTy))

            | V.Argument(argIndex, resultTy) ->
                match argMap[argIndex] with
                | ForwardSubValue.Local(localIndex, _) ->
                    OlyAssert.True(func.IsArgumentByRefType(argIndex) = optenv.IsLocalByRefType(localIndex))
                    E.Value(textRange, V.Local(localIndex, resultTy))
                | ForwardSubValue.Argument(argIndex2) ->
                    OlyAssert.True(func.IsArgumentByRefType(argIndex) = optenv.IsArgumentByRefType(argIndex2))
                    E.Value(textRange, V.Argument(argIndex2, resultTy))
                | ForwardSubValue.Constant(irConstant) ->
                    OlyAssert.False(func.IsArgumentByRefType(argIndex))
                    E.Value(textRange, V.Constant(irConstant, resultTy))
                | ForwardSubValue.LoadInlineableFunction(newLocalIndex, _, _, _) ->
                    OlyAssert.True(func.IsArgumentByRefType(argIndex) = optenv.IsLocalByRefType(newLocalIndex))
                    E.Value(textRange, V.Local(newLocalIndex, resultTy))
                | ForwardSubValue.NewClosure(newLocalIndex, _, _, _) ->
                    OlyAssert.True(func.IsArgumentByRefType(argIndex) = optenv.IsLocalByRefType(newLocalIndex))
                    E.Value(textRange, V.Local(newLocalIndex, resultTy))

                | ForwardSubValue.LocalAddress(localIndex, byRefKind) ->
                    OlyAssert.True(func.IsArgumentByRefType(argIndex))
                    OlyAssert.False(optenv.IsLocalByRefType(localIndex))
#if DEBUG || CHECKED
                    match byRefKind with
                    | OlyIRByRefKind.Read -> OlyAssert.True(func.IsArgumentReadOnlyByRefType(argIndex))
                    | OlyIRByRefKind.ReadWrite -> OlyAssert.True(func.IsArgumentReadWriteByRefType(argIndex))
#endif
                    E.Value(textRange, V.LocalAddress(localIndex, byRefKind, resultTy))

                | sub ->
                    OlyAssert.Fail($"Argument: bad forwardsub {sub}")

            | V.ArgumentAddress(argIndex, kind, resultTy) ->
                match argMap[argIndex] with
                | ForwardSubValue.Local(localIndex, _) ->
                    OlyAssert.True(func.IsArgumentByRefType(argIndex) = optenv.IsLocalByRefType(localIndex))
                    E.Value(textRange, V.LocalAddress(localIndex, kind, resultTy))

                | ForwardSubValue.LoadInlineableFunction(localIndex, _, _, _) ->
                    OlyAssert.False(func.IsArgumentByRefType(argIndex))
                    OlyAssert.False(optenv.IsLocalByRefType(localIndex))                 
                    E.Value(textRange, V.LocalAddress(localIndex, kind, resultTy))

                | sub ->
                    OlyAssert.Fail($"ArgumentAddress: bad forwardsub {sub}")

            | _ ->
                origExpr

        | _ ->
            origExpr

    and handleExpression irExpr =
        StackGuard.Do(fun () ->
            let irExpr = handleExpressionAux irExpr
            OptimizeImmediateConstantFolding irExpr
        )

    let result = handleExpression irFuncBody.Expression
    hasInlineFailed <- didFailInline
    result

let InlineFunctions optenv (irExpr: E<_, _, _>) =

    let forwardSubLocals = Dictionary<int, ForwardSubValue<_, _, _>>()
    
    let optimizeOperation irExpr =
        match irExpr with
        | E.Operation(origTextRange, irOp) ->

            // TODO: There is very similar logic in 'inlineFunction', we should combine them.
            match irOp with
            | O.Call(func, argExprs, resultTy) when func.HasEnclosingClosureType && canInline optenv func.RuntimeFunction ->
                let newArgExprs =
                    argExprs
                    |> ImArray.map (fun argExpr ->
                        match argExpr with
                        | E.Value(textRange, V.Local(localIndex, _)) ->
                            match forwardSubLocals.TryGetValue(localIndex) with
                            | true, ForwardSubValue.NewClosure(_, cloCtor, cloArgExprs, cloResultTy) ->
                                E.Operation(textRange, O.New(cloCtor, cloArgExprs, cloResultTy))
                            | _ ->
                                argExpr
                        | _ ->
                            argExpr
                    )
                let expr = E.Operation(origTextRange, O.Call(func, newArgExprs, resultTy))
                match tryInlineFunction forwardSubLocals optenv expr with
                | Some(expr) -> expr
                | _ -> 
                    irExpr

            | O.Call(irFunc, argExprs, resultTy) when irFunc.HasEnclosingClosureType && irFunc.RuntimeFunction.Flags.IsInstance ->
                let (func, newArgExprs) = transformClosureInvokeToUseMoreSpecificTypeArgument forwardSubLocals optenv irFunc argExprs
                E.Operation(origTextRange, O.Call(func, newArgExprs, resultTy))

            | _ ->

            match tryInlineFunction forwardSubLocals optenv irExpr with
            | Some irInlinedExpr -> irInlinedExpr
            | _ -> irExpr
        | _ ->
            OlyAssert.Fail("Expected operation")

    let rec handleExpression irExpr =
        StackGuard.Do(fun () ->
            handleExpressionAux irExpr
        )

    and handleExpressionAux irExpr : E<_, _, _> =
        match irExpr with
        | E.Let(name, localIndex, irRhsExpr, irBodyExpr) ->
            let irNewRhsExpr = handleExpression irRhsExpr

            let rec normalize irNewRhsExpr =
                match irNewRhsExpr with
                | E.Let(name2, localIndex2, rhsExpr2, bodyExpr2) ->
                    E.Let(
                        name2,
                        localIndex2,
                        rhsExpr2,
                        normalize bodyExpr2
                    )
                | _ ->
                    recordForwardSub forwardSubLocals optenv localIndex irNewRhsExpr

                    let irNewBodyExpr = handleExpression irBodyExpr

                    if irNewRhsExpr = irRhsExpr && irNewBodyExpr = irBodyExpr then
                        irExpr
                    else
                        E.Let(name, localIndex, irNewRhsExpr, irNewBodyExpr)

            normalize irNewRhsExpr

        | E.IfElse(irConditionExpr, irTrueTargetExpr, irFalseTargetExpr, resultTy) ->
            let irNewConditionExpr = handleExpression irConditionExpr

            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.True, _)) ->
                handleExpression irTrueTargetExpr

            | E.Value(value=V.Constant(C.False, _)) ->
                handleExpression irFalseTargetExpr

            | _ ->
                let irNewTrueTargetExpr = handleExpression irTrueTargetExpr
                let irNewFalseTargetExpr = handleExpression irFalseTargetExpr

                if irNewConditionExpr = irConditionExpr && irNewTrueTargetExpr = irTrueTargetExpr && irNewFalseTargetExpr = irFalseTargetExpr then
                    irExpr
                else
                    E.IfElse(irNewConditionExpr, irNewTrueTargetExpr, irNewFalseTargetExpr, resultTy)

        | E.While(irConditionExpr, irBodyExpr, resultTy) ->
            let irNewConditionExpr = handleExpression irConditionExpr

            match irNewConditionExpr with
            | E.Value(value=V.Constant(C.False, _)) ->
                E.None(NoRange, resultTy)
            | _ ->
                let irNewBodyExpr = handleExpression irBodyExpr

                if irNewConditionExpr = irConditionExpr && irNewBodyExpr = irBodyExpr then
                    irExpr
                else
                    E.While(irNewConditionExpr, irNewBodyExpr, resultTy)

        | E.Try(irBodyExpr, irCatchCases, irFinallyBodyExprOpt, resultTy) ->
            let irNewBodyExpr = handleExpression irBodyExpr

            let mutable didChange = false
            let irNewCatchCases =
                irCatchCases
                |> ImArray.map (fun irCatchCase ->
                    match irCatchCase with
                    | OlyIRCatchCase.CatchCase(localName, localIndex, irCaseBodyExpr, catchTy) ->
                        let irNewCaseBodyExpr = handleExpression irCaseBodyExpr

                        if irNewCaseBodyExpr = irCaseBodyExpr then
                            irCatchCase
                        else
                            didChange <- true
                            OlyIRCatchCase.CatchCase(localName, localIndex, irNewCaseBodyExpr, catchTy)
                )

            let irNewFinallyBodyExprOpt =
                irFinallyBodyExprOpt
                |> Option.map (fun irExpr ->
                    let irNewExpr = handleExpression irExpr
                    if irNewExpr = irExpr then
                        irExpr
                    else
                        didChange <- true
                        irNewExpr
                )

            if irNewBodyExpr = irBodyExpr && not didChange then
                irExpr
            else
                E.Try(irNewBodyExpr, irNewCatchCases, irNewFinallyBodyExprOpt, resultTy)

        | E.Sequential(irExpr1, irExpr2) ->
            let irNewExpr1 = handleExpression irExpr1
            let irNewExpr2 = handleExpression irExpr2

            if hasSideEffect optenv irNewExpr1 then
                if irNewExpr1 = irExpr1 && irNewExpr2 = irExpr2 then
                    irExpr
                else
                    E.Sequential(irNewExpr1, irNewExpr2)
            else
                irNewExpr2

        | E.Operation(irTextRange, irOp) ->
            //let newOp = irOp.MapAndReplaceArguments(fun _ argExpr -> handleExpression argExpr)
            //if newOp = irOp then
            //    optimizeOperation irExpr
            //else
            //    E.Operation(irTextRange, newOp)
            //    |> optimizeOperation
            let irNewArgExprs = irOp.MapArguments(fun _ irArgExpr -> handleExpression irArgExpr)
            let mutable areSame = true
            irOp.ForEachArgument(fun i irArgExpr ->
                if irNewArgExprs[i] <> irArgExpr then
                    areSame <- false
            )
            if areSame then
                optimizeOperation irExpr
            else
                let irNewOp = irOp.ReplaceArguments(irNewArgExprs) 
                E.Operation(irTextRange, irNewOp)
                |> optimizeOperation

        | _ ->
            irExpr

    handleExpression irExpr

