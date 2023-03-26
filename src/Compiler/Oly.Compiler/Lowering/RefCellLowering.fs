[<RequireQualifiedAccess>]
module internal rec Oly.Compiler.Internal.Lowering.RefCellLowering

open System.Collections.Generic

open Oly.Core
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeRewriter
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.WellKnownExpressions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations

let canRewrite (expr: E) =
    match expr with
    | E.EntityDefinition _
    | E.MemberDefinition _ -> false
    | _ -> true

type cenv =
    {
        locals: HashSet<int64>
        localSubs: Dictionary<int64, IValueSymbol>
        tree: BoundTree
    }

let analysisLocalLambdaExpression (cenv: cenv) origExpr =
    match origExpr with
    | E.Lambda _ ->
       
        let freeMutableValues = origExpr.GetImmediateFreeLocals()
        if freeMutableValues.Count > 0 then
            freeMutableValues.Values
            |> Seq.iter (fun (syntaxNameOpt, x) ->
                if x.IsMutable then
                    cenv.locals.Add(x.Id) |> ignore
            )
            origExpr
        else
            origExpr

    | _ ->
        failwith "Expected lambda."

let analysisLocalExpression (cenv: cenv) (origExpr: E) =
    match origExpr with
    | E.Lambda(flags=flags) when not(flags.HasFlag(LambdaFlags.Static)) && not(flags.HasFlag(LambdaFlags.StackEmplace)) ->
        analysisLocalLambdaExpression cenv origExpr

    | _ ->
        origExpr

let rewriteLocalLambdaExpression (cenv: cenv) origExpr =
    match origExpr with
    | E.Lambda _ ->
       
        let freeMutableValues = origExpr.GetFreeMutableLocals()
        if freeMutableValues.Count > 0 then
            freeMutableValues.Values
            |> Seq.iter (fun (syntaxNameOpt, x) ->
                cenv.locals.Add(x.Id) |> ignore
            )
            raise(System.NotImplementedException())
        else
            origExpr

    | _ ->
        failwith "Expected lambda."

let rewritePreorderLocalExpression (cenv: cenv) (origExpr: E) =
    match origExpr with
    | E.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
        match bindingInfo with
        | BindingLocal(value) ->
            if cenv.locals.Contains(value.Id) then
                OlyAssert.True(value.IsMutable)
                let newValue = createLocalValue value.Name (TypeSymbol.RefCell(value.Type))
                cenv.localSubs.Add(value.Id, newValue)
                E.Let(
                    syntaxInfo,
                    BindingLocal(newValue),
                    NewRefCell rhsExpr,
                    bodyExpr
                )
            else
                origExpr
        | _ ->
            origExpr

    | _ ->
        origExpr

let rewriteLocalExpression (cenv: cenv) (origExpr: E) =
    match origExpr with
    | E.Value(syntaxInfo, value) ->
        match cenv.localSubs.TryGetValue value.Id with
        | true, newValue ->
            LoadRefCellContents (E.Value(syntaxInfo, newValue))
        | _ ->
            origExpr

    | E.SetValue(syntaxInfo, syntaxNameOpt, value, rhsExpr) ->
        match cenv.localSubs.TryGetValue value.Id with
        | true, newValue ->
            let syntaxInfoValue =
                match syntaxInfo.TryEnvironment with
                | Some benv ->
                    match syntaxNameOpt with
                    | Some syntaxName -> BoundSyntaxInfo.User(syntaxName, benv)
                    | _ -> syntaxInfo
                | _ ->
                    BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree)
            StoreRefCellContents (E.Value(syntaxInfoValue, newValue)) rhsExpr
        | _ ->
            origExpr

    | E.SetContentsOfAddress(syntaxInfo, lhsExpr, rhsExpr) ->
        if lhsExpr.Type.IsRefCell_t then
            raise(System.NotImplementedException())
        origExpr

    | E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, syntaxNameOpt, value, isVirtualCall) ->
        match cenv.localSubs.TryGetValue value.Id with
        | true, newValue ->
            let bridgeValue = createLocalBridgeValue value.Type
            let syntaxInfoBridge = BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree)
            E.Let(
                syntaxInfoBridge,
                BindingLocal(bridgeValue),
                LoadRefCellContents (E.Value(syntaxInfoBridge, newValue)),
                E.Call(syntaxInfo, receiverExprOpt, witnessArgs, argExprs, syntaxNameOpt, bridgeValue, isVirtualCall)
            )
        | _ ->
            origExpr

    | _ ->
        origExpr

let rewriteTopLevelLambda (cenv: cenv) (origExpr: E) =
    match origExpr with
    | E.Lambda(syntaxInfo, lambdaFlags, tyPars, pars, lazyBodyExpr, _, _, _) ->
        let newBodyExpr = lazyBodyExpr.Expression.Rewrite(canRewrite, analysisLocalExpression cenv)
        let newBodyExpr = 
            if cenv.locals.Count > 0 then
                let parSubs =
                    pars
                    |> ImArray.choose (fun x ->
                        if cenv.locals.Contains(x.Id) then
                            OlyAssert.True(x.IsMutable)
                            let newValue = createLocalValue x.Name (TypeSymbol.RefCell(x.Type))
                            cenv.localSubs.Add(x.Id, newValue)
                            Some(x, newValue)
                        else
                            None
                    )

                (newBodyExpr.Rewrite(rewritePreorderLocalExpression cenv, rewriteLocalExpression cenv, canRewrite), parSubs)
                ||> ImArray.foldBack (fun expr (par, newValue) ->
                    E.Let(
                        syntaxInfo,
                        BindingLocal(newValue),
                        NewRefCell(E.Value(syntaxInfo, par)),
                        expr
                    )
                )
            else
                newBodyExpr

        cenv.locals.Clear()
        cenv.localSubs.Clear()

        if newBodyExpr = lazyBodyExpr.Expression then
            origExpr
        else
            E.CreateLambda(syntaxInfo, lambdaFlags, tyPars, pars, LazyExpression.CreateNonLazy(None, fun _ -> newBodyExpr))

    | _ ->
        failwith "Expected lambda."

type RefCellRewriterCore(cenv: cenv) =
    inherit BoundTreeRewriterCore()

    override this.Rewrite(origExpr) =
        match origExpr with
        | E.MemberDefinition(syntaxInfo1, BoundBinding.Implementation(syntaxInfo2, bindingInfo, rhsExpr)) when not bindingInfo.Value.IsLocal && bindingInfo.Value.IsFunction ->
            let newRhsExpr =
                match rhsExpr with
                | E.Lambda _ ->
                    rewriteTopLevelLambda cenv rhsExpr
                | _ ->
                    origExpr

            if newRhsExpr = rhsExpr then
                origExpr
            else
                E.MemberDefinition(syntaxInfo1, BoundBinding.Implementation(syntaxInfo2, bindingInfo, newRhsExpr))
        | _ ->
            origExpr

let Lower (tree: BoundTree) =
    let cenv =
        {
            locals = HashSet()
            localSubs = Dictionary()
            tree = tree
        }

    let rewriter = BoundTreeRewriter(RefCellRewriterCore(cenv))
    tree.UpdateRoot(rewriter.RewriteRoot(tree.Root))