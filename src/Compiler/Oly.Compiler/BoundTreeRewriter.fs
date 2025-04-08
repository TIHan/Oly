module internal Oly.Compiler.Internal.BoundTreeRewriter

open System.Collections.Generic
open System.Collections.Immutable
open System.Collections.ObjectModel
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.BoundTree
open Oly.Core

/// TODO: Use/replace with the new BoundTreeRewriteVisitor instead.
type BoundTreeRewriterCore() =

    abstract CanPostorderRewrite : BoundExpression -> bool
    default _.CanPostorderRewrite _ = true

    abstract CanRewrite : BoundExpression -> bool
    default _.CanRewrite _ = true

    abstract PreorderRewrite : BoundExpression -> BoundExpression
    default _.PreorderRewrite expr = expr

    abstract Rewrite : BoundExpression -> BoundExpression
    default _.Rewrite expr = expr

/// TODO: Use/replace with the new BoundTreeRewriteVisitor instead.
type BoundTreeRewriter(core: BoundTreeRewriterCore) =
    inherit BoundTreeRewriterCore()

    override this.CanPostorderRewrite(expr) =
        core.CanPostorderRewrite(expr)

    override this.CanRewrite(expr) =
        core.CanRewrite(expr)

    override this.PreorderRewrite(expr) =
        core.PreorderRewrite(expr)

    override this.Rewrite(expr) =
#if DEBUG || CHECKED
        StackGuard.Do <| fun () ->
#endif
        if this.CanRewrite expr then
            let expr = this.PreorderRewrite(expr)
            match expr with
            | BoundExpression.Try(syntaxInfo, bodyExpr, catchCases, finallyBodyExprOpt) ->
                let newBodyExpr = this.Rewrite(bodyExpr)
                let newCatchCases =
                    catchCases
                    |> ImArray.map (fun catchCase ->
                        match catchCase with
                        | BoundCatchCase.CatchCase(syntaxInfo, local, catchBodyExpr) ->
                            let newCatchBodyExpr = this.Rewrite(catchBodyExpr)

                            if newCatchBodyExpr = catchBodyExpr then
                                catchCase
                            else
                                BoundCatchCase.CatchCase(syntaxInfo, local, newCatchBodyExpr)
                    )
                let newFinallyBodyExprOpt =
                    finallyBodyExprOpt
                    |> Option.map (fun finallyBodyExpr ->
                        this.Rewrite(finallyBodyExpr)
                    )

                if newBodyExpr = bodyExpr && ((catchCases, newCatchCases) ||> ImArray.forall2 (=)) && finallyBodyExprOpt = newFinallyBodyExprOpt then
                    expr
                else
                    BoundExpression.Try(syntaxInfo, newBodyExpr, newCatchCases, newFinallyBodyExprOpt)

            | BoundExpression.While(syntaxInfo, conditionExpr, bodyExpr) ->
                let newConditionExpr = this.Rewrite(conditionExpr)
                let newBodyExpr = this.Rewrite(bodyExpr)

                if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
                    expr
                else
                    BoundExpression.While(syntaxInfo, newConditionExpr, newBodyExpr)

            | BoundExpression.IfElse(syntaxInfo, conditionExpr, trueTargetExpr, falseTargetExpr, cachedExprTy) ->
                let newConditionExpr = this.Rewrite(conditionExpr)
                let newTrueTargetExpr = this.Rewrite(trueTargetExpr)
                let newFalseTargetExpr = this.Rewrite(falseTargetExpr)

                if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
                    expr
                else
                    // REVIEW: For 'cachedExprTy', do we need to re-evaluate the expression type?
                    BoundExpression.IfElse(syntaxInfo, newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, cachedExprTy)

            | BoundExpression.Match(syntax, benv, matchExprs, matchClauses, cachedExprTy) ->
                let newMatchExprs = matchExprs |> ImArray.map (fun x -> this.Rewrite(x))
                let newMatchClauses =
                    matchClauses
                    |> ImArray.map (fun matchClause ->
                        match matchClause with
                        | BoundMatchClause.MatchClause(syntaxMatchClause, matchPattern, guardExprOpt, targetExpr) ->
                            let newGuardExprOpt = guardExprOpt |> Option.map (this.Rewrite)
                            let newTargetExpr = this.Rewrite(targetExpr)
                            
                            if newGuardExprOpt = guardExprOpt && newTargetExpr = targetExpr then
                                matchClause
                            else
                                BoundMatchClause.MatchClause(syntaxMatchClause, matchPattern, newGuardExprOpt, newTargetExpr)
                    )

                if ((matchExprs, newMatchExprs) ||> ImArray.forall2 (=)) && ((matchClauses, newMatchClauses) ||> ImArray.forall2 (=)) then
                    expr
                else
                    // REVIEW: For 'cachedExprTy', do we need to re-evaluate the expression type?
                    BoundExpression.Match(syntax, benv, newMatchExprs, newMatchClauses, cachedExprTy)

            | BoundExpression.Lambda(syntaxInfo, lambdaFlags, tyPars, parValues, body, _, _, _) ->
                let newBody = this.Rewrite(body.Expression)

                if newBody = body.Expression then
                    expr
                else
                    let lazyBody = LazyExpression.CreateNonLazy(None, fun _ -> newBody)

                    BoundExpression.CreateLambda(syntaxInfo, lambdaFlags, tyPars, parValues, lazyBody)

            | BoundExpression.Call(syntaxInfo, receiverOpt, witnessArgs, args, value, isVirtualCall) ->
                let newReceiverOpt =  receiverOpt |> Option.map this.Rewrite
                let newArgs = args |> ImArray.map (fun arg -> this.Rewrite(arg))

                if newReceiverOpt = receiverOpt && ((args, newArgs) ||> ImArray.forall2 (=)) then
                    expr
                else
                    BoundExpression.Call(syntaxInfo, newReceiverOpt, witnessArgs, newArgs, value, isVirtualCall)

            | BoundExpression.MemberDefinition(syntaxInfo, BoundBinding.Implementation(syntaxInfo2, bindingInfo, rhsExpr)) ->
                let newRhsExpr = this.Rewrite(rhsExpr)

                if newRhsExpr = rhsExpr then
                    expr
                else
                    BoundExpression.MemberDefinition(syntaxInfo, BoundBinding.Implementation(syntaxInfo2, bindingInfo, newRhsExpr))

            | BoundExpression.MemberDefinition(_, BoundBinding.Signature _) ->
                expr

            | BoundExpression.Value _ ->
                expr

            | BoundExpression.GetField(syntaxInfo, receiver, field) ->
                let newReceiver = this.Rewrite(receiver)

                if newReceiver = receiver then
                    expr
                else
                    BoundExpression.GetField(syntaxInfo, newReceiver, field)

            | BoundExpression.SetField(syntaxInfo, receiver, field, rhs, isCtorInit) ->
                let newRhs = this.Rewrite(rhs)
                let newReceiver = this.Rewrite(receiver)

                if newReceiver = receiver && newRhs = rhs then
                    expr
                else
                    BoundExpression.SetField(syntaxInfo, newReceiver, field, newRhs, isCtorInit)

            | BoundExpression.GetProperty(syntaxInfo, receiverOpt, prop, isVirtual) ->
                let newReceiverOpt = receiverOpt |> Option.map this.Rewrite

                if newReceiverOpt = receiverOpt then
                    expr
                else
                    BoundExpression.GetProperty(syntaxInfo, newReceiverOpt, prop, isVirtual)

            | BoundExpression.SetProperty(syntaxInfo, receiverOpt, prop, rhs, isVirtual) ->
                let newRhs = this.Rewrite(rhs)
                let newReceiverOpt = receiverOpt |> Option.map this.Rewrite

                if newReceiverOpt = receiverOpt && newRhs = rhs then
                    expr
                else
                    BoundExpression.SetProperty(syntaxInfo, newReceiverOpt, prop, newRhs, isVirtual)

            | BoundExpression.Sequential(syntaxInfo, expr1, expr2, semantic) ->
                let newExpr1 = this.Rewrite(expr1)
                let newExpr2 = this.Rewrite(expr2)

                if newExpr1 = expr1 && newExpr2 = expr2 then
                    expr
                else
                    BoundExpression.Sequential(syntaxInfo, newExpr1, newExpr2, semantic)

            | BoundExpression.NewTuple(syntaxInfo, args, ty) ->
                let newArgs = args |> ImArray.map (fun arg -> this.Rewrite(arg))

                if (args, newArgs) ||> ImArray.forall2 (=) then
                    expr
                else
                    BoundExpression.NewTuple(syntaxInfo, newArgs, ty)

            | BoundExpression.NewArray(syntax, benv, elements, resultTy) ->
                let newElements = elements |> ImArray.map (fun arg -> this.Rewrite(arg))

                if (elements, newElements) ||> ImArray.forall2 (=) then
                    expr
                else
                    BoundExpression.NewArray(syntax, benv, newElements, resultTy)

            | BoundExpression.SetValue(syntaxInfo, value, rhs) ->
                let newRhs = this.Rewrite(rhs)

                if newRhs = rhs then
                    expr
                else
                    BoundExpression.SetValue(syntaxInfo, value, newRhs)

            | BoundExpression.SetContentsOfAddress(syntaxInfo, lhsExpr, rhsExpr) ->
                let newLhsExpr = this.Rewrite(lhsExpr)
                let newRhsExpr = this.Rewrite(rhsExpr)

                if newLhsExpr = lhsExpr && newRhsExpr = rhsExpr then
                    expr
                else
                    BoundExpression.SetContentsOfAddress(syntaxInfo, newLhsExpr, newRhsExpr)

            | BoundExpression.Witness(syntaxInfo, benv, castFunc, bodyExpr, witnessArgOptRef, exprTy) ->
                let newBodyExpr = this.Rewrite(bodyExpr)

                if newBodyExpr = bodyExpr then
                    expr
                else
                    BoundExpression.Witness(syntaxInfo, benv, castFunc, newBodyExpr, witnessArgOptRef, exprTy)

            | BoundExpression.EntityDefinition(syntaxInfo, body, ent) ->
                let newBody = this.Rewrite(body)

                if newBody = body then
                    expr
                else
                    BoundExpression.EntityDefinition(syntaxInfo, newBody, ent)

            | BoundExpression.Typed(syntaxInfo, bodyExpr, ty) ->
                let newBodyExpr = this.Rewrite(bodyExpr)

                if newBodyExpr = bodyExpr then
                    expr
                else
                    BoundExpression.Typed(syntaxInfo, newBodyExpr, ty)

            | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
                let newRhsExpr = this.Rewrite(rhsExpr)
                let newBodyExpr = this.Rewrite(bodyExpr)

                if newRhsExpr = rhsExpr && newBodyExpr = bodyExpr then
                    expr
                else
                    BoundExpression.Let(syntaxInfo, bindingInfo, newRhsExpr, newBodyExpr)

            | _ ->
                // TODO: Rewrite missing ones.
                expr

            |> core.Rewrite
        else
            expr

    member this.RewriteRoot(root) =
        match root with
        | BoundRoot.Namespace(syntax, benv, namespac, body) ->
            let newBody = this.Rewrite(body)

            if newBody = body then
                root
            else
                BoundRoot.Namespace(syntax, benv, namespac, newBody)

        | BoundRoot.Global(syntax, benv, body) ->
            let newBody = this.Rewrite(body)

            if newBody = body then
                root
            else
                BoundRoot.Global(syntax, benv, newBody)

type private BoundExpressionVisitResultKind =
    | Visited = 0
    | Continue = 1
    | Abort = 2

[<Struct;NoComparison;NoEquality>]
type BoundExpressionVisitResult =
    private {
        Expression: BoundExpression
        Kind: BoundExpressionVisitResultKind
    }

    static member Visited(expr) =
        {
            Expression = expr
            Kind = BoundExpressionVisitResultKind.Visited
        }

    static member Continue(expr) =
        {
            Expression = expr
            Kind = BoundExpressionVisitResultKind.Continue
        }

    static member Abort() =
        {
            Expression = Unchecked.defaultof<_>
            Kind = BoundExpressionVisitResultKind.Abort
        }

type BoundTreeRewriteVisitor() =

    abstract Preorder : BoundExpression * visit: (BoundExpression ->  BoundExpression) -> BoundExpressionVisitResult
    default _.Preorder(expr, _) = BoundExpressionVisitResult.Continue(expr)

    abstract Postorder : BoundExpression -> BoundExpression
    default _.Postorder expr = expr

[<Sealed>]
type private BoundTreeRewriteVisitorRunner(core: BoundTreeRewriteVisitor) as this =

    let rewrite = this.Rewrite

    member this.Rewrite(expr) =
#if DEBUG || CHECKED
        StackGuard.Do <| fun () ->
#endif
        let exprResult = core.Preorder(expr, rewrite)
        match exprResult.Kind with
        | BoundExpressionVisitResultKind.Continue ->
        let expr = exprResult.Expression
        match expr with
        | BoundExpression.Try(syntaxInfo, bodyExpr, catchCases, finallyBodyExprOpt) ->
            let newBodyExpr = this.Rewrite(bodyExpr)
            let newCatchCases =
                catchCases
                |> ImArray.map (fun catchCase ->
                    match catchCase with
                    | BoundCatchCase.CatchCase(syntaxInfo, local, catchBodyExpr) ->
                        let newCatchBodyExpr = this.Rewrite(catchBodyExpr)

                        if newCatchBodyExpr = catchBodyExpr then
                            catchCase
                        else
                            BoundCatchCase.CatchCase(syntaxInfo, local, newCatchBodyExpr)
                )
            let newFinallyBodyExprOpt =
                finallyBodyExprOpt
                |> Option.map (fun finallyBodyExpr ->
                    this.Rewrite(finallyBodyExpr)
                )

            if newBodyExpr = bodyExpr && ((catchCases, newCatchCases) ||> ImArray.forall2 (=)) && finallyBodyExprOpt = newFinallyBodyExprOpt then
                expr
            else
                BoundExpression.Try(syntaxInfo, newBodyExpr, newCatchCases, newFinallyBodyExprOpt)

        | BoundExpression.While(syntaxInfo, conditionExpr, bodyExpr) ->
            let newConditionExpr = this.Rewrite(conditionExpr)
            let newBodyExpr = this.Rewrite(bodyExpr)

            if newConditionExpr = conditionExpr && newBodyExpr = bodyExpr then
                expr
            else
                BoundExpression.While(syntaxInfo, newConditionExpr, newBodyExpr)

        | BoundExpression.IfElse(syntaxInfo, conditionExpr, trueTargetExpr, falseTargetExpr, cachedExprTy) ->
            let newConditionExpr = this.Rewrite(conditionExpr)
            let newTrueTargetExpr = this.Rewrite(trueTargetExpr)
            let newFalseTargetExpr = this.Rewrite(falseTargetExpr)

            if newConditionExpr = conditionExpr && newTrueTargetExpr = trueTargetExpr && newFalseTargetExpr = falseTargetExpr then
                expr
            else
                // REVIEW: For 'cachedExprTy', do we need to re-evaluate the expression type?
                BoundExpression.IfElse(syntaxInfo, newConditionExpr, newTrueTargetExpr, newFalseTargetExpr, cachedExprTy)

        | BoundExpression.Match(syntax, benv, matchExprs, matchClauses, cachedExprTy) ->
            let newMatchExprs = matchExprs |> ImArray.map (fun x -> this.Rewrite(x))
            let newMatchClauses =
                matchClauses
                |> ImArray.map (fun matchClause ->
                    match matchClause with
                    | BoundMatchClause.MatchClause(syntaxMatchClause, matchPattern, guardExprOpt, targetExpr) ->
                        let newGuardExprOpt = guardExprOpt |> Option.map (this.Rewrite)
                        let newTargetExpr = this.Rewrite(targetExpr)
                            
                        if newGuardExprOpt = guardExprOpt && newTargetExpr = targetExpr then
                            matchClause
                        else
                            BoundMatchClause.MatchClause(syntaxMatchClause, matchPattern, newGuardExprOpt, newTargetExpr)
                )

            if ((matchExprs, newMatchExprs) ||> ImArray.forall2 (=)) && ((matchClauses, newMatchClauses) ||> ImArray.forall2 (=)) then
                expr
            else
                // REVIEW: For 'cachedExprTy', do we need to re-evaluate the expression type?
                BoundExpression.Match(syntax, benv, newMatchExprs, newMatchClauses, cachedExprTy)

        | BoundExpression.Lambda(syntaxInfo, lambdaFlags, tyPars, parValues, body, _, _, _) ->
            let newBody = this.Rewrite(body.Expression)

            if newBody = body.Expression then
                expr
            else
                let lazyBody = LazyExpression.CreateNonLazy(None, fun _ -> newBody)

                BoundExpression.CreateLambda(syntaxInfo, lambdaFlags, tyPars, parValues, lazyBody)

        | BoundExpression.Call(syntaxInfo, receiverOpt, witnessArgs, args, value, isVirtualCall) ->
            let newReceiverOpt =  receiverOpt |> Option.map this.Rewrite
            let newArgs = args |> ImArray.map (fun arg -> this.Rewrite(arg))

            if newReceiverOpt = receiverOpt && ((args, newArgs) ||> ImArray.forall2 (=)) then
                expr
            else
                BoundExpression.Call(syntaxInfo, newReceiverOpt, witnessArgs, newArgs, value, isVirtualCall)

        | BoundExpression.MemberDefinition(syntaxInfo, BoundBinding.Implementation(syntaxInfo2, bindingInfo, rhsExpr)) ->
            let newRhsExpr = this.Rewrite(rhsExpr)

            if newRhsExpr = rhsExpr then
                expr
            else
                BoundExpression.MemberDefinition(syntaxInfo, BoundBinding.Implementation(syntaxInfo2, bindingInfo, newRhsExpr))

        | BoundExpression.MemberDefinition(_, BoundBinding.Signature _) ->
            expr

        | BoundExpression.Value _ ->
            expr

        | BoundExpression.GetField(syntaxInfo, receiver, field) ->
            let newReceiver = this.Rewrite(receiver)

            if newReceiver = receiver then
                expr
            else
                BoundExpression.GetField(syntaxInfo, newReceiver, field)

        | BoundExpression.SetField(syntaxInfo, receiver, field, rhs, isCtorInit) ->
            let newRhs = this.Rewrite(rhs)
            let newReceiver = this.Rewrite(receiver)

            if newReceiver = receiver && newRhs = rhs then
                expr
            else
                BoundExpression.SetField(syntaxInfo, newReceiver, field, newRhs, isCtorInit)

        | BoundExpression.GetProperty(syntaxInfo, receiverOpt, prop, isVirtual) ->
            let newReceiverOpt = receiverOpt |> Option.map this.Rewrite

            if newReceiverOpt = receiverOpt then
                expr
            else
                BoundExpression.GetProperty(syntaxInfo, newReceiverOpt, prop, isVirtual)

        | BoundExpression.SetProperty(syntaxInfo, receiverOpt, prop, rhs, isVirtual) ->
            let newRhs = this.Rewrite(rhs)
            let newReceiverOpt = receiverOpt |> Option.map this.Rewrite

            if newReceiverOpt = receiverOpt && newRhs = rhs then
                expr
            else
                BoundExpression.SetProperty(syntaxInfo, newReceiverOpt, prop, newRhs, isVirtual)

        | BoundExpression.Sequential(syntaxInfo, expr1, expr2, semantic) ->
            let newExpr1 = this.Rewrite(expr1)
            let newExpr2 = this.Rewrite(expr2)

            if newExpr1 = expr1 && newExpr2 = expr2 then
                expr
            else
                BoundExpression.Sequential(syntaxInfo, newExpr1, newExpr2, semantic)

        | BoundExpression.NewTuple(syntaxInfo, args, ty) ->
            let newArgs = args |> ImArray.map (fun arg -> this.Rewrite(arg))

            if (args, newArgs) ||> ImArray.forall2 (=) then
                expr
            else
                BoundExpression.NewTuple(syntaxInfo, newArgs, ty)

        | BoundExpression.NewArray(syntax, benv, elements, resultTy) ->
            let newElements = elements |> ImArray.map (fun arg -> this.Rewrite(arg))

            if (elements, newElements) ||> ImArray.forall2 (=) then
                expr
            else
                BoundExpression.NewArray(syntax, benv, newElements, resultTy)

        | BoundExpression.SetValue(syntaxInfo, value, rhs) ->
            let newRhs = this.Rewrite(rhs)

            if newRhs = rhs then
                expr
            else
                BoundExpression.SetValue(syntaxInfo, value, newRhs)

        | BoundExpression.SetContentsOfAddress(syntaxInfo, lhsExpr, rhsExpr) ->
            let newLhsExpr = this.Rewrite(lhsExpr)
            let newRhsExpr = this.Rewrite(rhsExpr)

            if newLhsExpr = lhsExpr && newRhsExpr = rhsExpr then
                expr
            else
                BoundExpression.SetContentsOfAddress(syntaxInfo, newLhsExpr, newRhsExpr)

        | BoundExpression.Witness(syntaxInfo, benv, castFunc, bodyExpr, witnessArgOptRef, exprTy) ->
            let newBodyExpr = this.Rewrite(bodyExpr)

            if newBodyExpr = bodyExpr then
                expr
            else
                BoundExpression.Witness(syntaxInfo, benv, castFunc, newBodyExpr, witnessArgOptRef, exprTy)

        | BoundExpression.EntityDefinition(syntaxInfo, body, ent) ->
            let newBody = this.Rewrite(body)

            if newBody = body then
                expr
            else
                BoundExpression.EntityDefinition(syntaxInfo, newBody, ent)

        | BoundExpression.Typed(syntaxInfo, bodyExpr, ty) ->
            let newBodyExpr = this.Rewrite(bodyExpr)

            if newBodyExpr = bodyExpr then
                expr
            else
                BoundExpression.Typed(syntaxInfo, newBodyExpr, ty)

        | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
            let newRhsExpr = this.Rewrite(rhsExpr)
            let newBodyExpr = this.Rewrite(bodyExpr)

            if newRhsExpr = rhsExpr && newBodyExpr = bodyExpr then
                expr
            else
                BoundExpression.Let(syntaxInfo, bindingInfo, newRhsExpr, newBodyExpr)

        | _ ->
            // TODO: Rewrite missing ones.
            expr

        |> core.Postorder
        | BoundExpressionVisitResultKind.Abort -> expr
        | BoundExpressionVisitResultKind.Visited -> exprResult.Expression
        | _ -> failwith "Invalid result kind"

    member this.Visit(root) =
        match root with
        | BoundRoot.Namespace(syntax, benv, namespac, body) ->
            let newBody = this.Rewrite(body)

            if newBody = body then
                root
            else
                BoundRoot.Namespace(syntax, benv, namespac, newBody)

        | BoundRoot.Global(syntax, benv, body) ->
            let newBody = this.Rewrite(body)

            if newBody = body then
                root
            else
                BoundRoot.Global(syntax, benv, newBody)

type BoundRoot with

    member this.Visit(core: BoundTreeRewriteVisitor) =
        let runner = BoundTreeRewriteVisitorRunner(core)
        runner.Visit(this)