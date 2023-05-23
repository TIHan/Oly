module Oly.Compiler.Internal.BoundTreeVisitor

open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.BoundTree

type internal BoundTreeVisitorCore() =

    abstract CanVisit : BoundExpression -> bool
    default _.CanVisit _ = true

    abstract VisitBinding : BoundBinding -> bool
    default _.VisitBinding _ = true 

    abstract VisitBindingInfo : BindingInfoSymbol -> bool
    default _.VisitBindingInfo(_) = true

    abstract VisitLocalBindingInfo : LocalBindingInfoSymbol -> bool
    default _.VisitLocalBindingInfo(_) = true

    abstract VisitExpression : BoundExpression -> bool
    default _.VisitExpression _ = true

    abstract VisitRoot : BoundRoot -> bool
    default _.VisitRoot _ = true

    abstract VisitPattern : BoundCasePattern -> bool
    default _.VisitPattern _ = true

type internal BoundTreeVisitor(core: BoundTreeVisitorCore) =
    inherit BoundTreeVisitorCore()

    override this.CanVisit(expr) =
        core.CanVisit(expr)

    override this.VisitPattern(pattern) =
        let result = core.VisitPattern(pattern)

        if result then
            match pattern with
            | BoundCasePattern.Tuple(_, patArgs)
            | BoundCasePattern.Function(_, _, _, patArgs) ->
                patArgs
                |> ImArray.iter (fun x -> this.VisitPattern x |> ignore)
            | _ ->
                ()

        result

    override this.VisitBinding(boundBinding) =
        let result = core.VisitBinding(boundBinding)

        if result then
            match boundBinding with
            | BoundBinding.Implementation(syntaxInfo=syntaxInfo;bindingInfo=bindingInfo;rhs=rhsExpr) ->
                this.VisitExpression(rhsExpr) |> ignore
                this.VisitBindingInfo(bindingInfo) |> ignore
            | BoundBinding.Signature(syntaxInfo=syntaxInfo;bindingInfo=bindingInfo) ->
                this.VisitBindingInfo(bindingInfo) |> ignore

        result

    override this.VisitBindingInfo(bindingInfo) =
        let result = core.VisitBindingInfo(bindingInfo)

        if result then
            match bindingInfo with
            | BindingProperty(getterAndSetterBindings=bindingInfos) ->
                bindingInfos
                |> ImArray.iter (fun bindingInfo ->
                    this.VisitBindingInfo(bindingInfo) |> ignore
                )
            | _ ->
                ()

        result

    override this.VisitLocalBindingInfo(bindingInfo) =
        core.VisitLocalBindingInfo(bindingInfo)

    override this.VisitExpression(boundExpression) =
        let result = 
            if core.CanVisit(boundExpression) then
                core.VisitExpression(boundExpression)
            else
                false

        if result then
            match boundExpression with
            | BoundExpression.Try(_, bodyExpr, catchCases, finallyBodyExprOpt) ->
                this.VisitExpression(bodyExpr) |> ignore
                catchCases
                |> ImArray.iter (function
                    | BoundCatchCase.CatchCase(_, catchBodyExpr) ->
                        this.VisitExpression(catchBodyExpr) |> ignore
                )
                finallyBodyExprOpt
                |> Option.iter (fun finallyBodyExpr ->
                    this.VisitExpression(finallyBodyExpr) |> ignore
                )

            | BoundExpression.While(_, conditionExpr, bodyExpr) ->
                this.VisitExpression(conditionExpr) |> ignore
                this.VisitExpression(bodyExpr) |> ignore

            | BoundExpression.IfElse(_, conditionExpr, trueTargetExpr, falseTargetExpr, _) ->
                this.VisitExpression(conditionExpr) |> ignore
                this.VisitExpression(trueTargetExpr) |> ignore
                this.VisitExpression(falseTargetExpr) |> ignore

            | BoundExpression.Match(_, _, matchExprs, matchClauses, _) ->
                matchExprs
                |> ImArray.iter (fun x -> this.VisitExpression(x) |> ignore)

                matchClauses
                |> ImArray.iter (fun x ->
                    match x with
                    | BoundMatchClause.MatchClause(_, matchPattern, guardExprOpt, targetExpr) ->
                        let rec loop matchPattern =
                            match matchPattern with
                            | BoundMatchPattern.Cases(_, patterns) ->
                                patterns
                                |> ImArray.iter (fun x -> this.VisitPattern x |> ignore)
                            | BoundMatchPattern.Or(_, lhs, rhs) ->
                                loop lhs
                                loop rhs
                        loop matchPattern

                        guardExprOpt
                        |> Option.iter (fun guardExpr -> this.VisitExpression(guardExpr) |> ignore)
                        this.VisitExpression(targetExpr) |> ignore
                )

            | BoundExpression.Witness(expr, _, _) ->
                this.VisitExpression(expr) |> ignore

            | BoundExpression.NewTuple(_, args, _) ->
                for i = 0 to args.Length - 1 do
                    this.VisitExpression(args.[i]) |> ignore

            | BoundExpression.NewArray(_, _, elements, _) ->
                for i = 0 to elements.Length - 1 do
                    this.VisitExpression(elements.[i]) |> ignore

            | BoundExpression.Typed(_, bodyExpr, _) ->
                this.VisitExpression(bodyExpr) |> ignore

            | BoundExpression.Call(_, receiverOpt, _, args, _, _) ->
                for i = 0 to args.Length - 1 do
                    this.VisitExpression(args.[i]) |> ignore
                match receiverOpt with
                | Some receiver -> this.VisitExpression(receiver) |> ignore
                | _ -> ()

            | BoundExpression.None _ -> ()
            | BoundExpression.Lambda(_, _, _, _, body, _, _, _) ->
                OlyAssert.True(body.HasExpression)
                this.VisitExpression(body.Expression) |> ignore

            | BoundExpression.MemberDefinition(_, binding) ->
                this.VisitBinding(binding) |> ignore          
                
            | BoundExpression.Sequential(_, boundExpression1, boundExpression2) ->
                this.VisitExpression(boundExpression1) |> ignore
                this.VisitExpression(boundExpression2) |> ignore

            | BoundExpression.Let(_, binding, rhsExpr, bodyExpr) ->
                this.VisitExpression(rhsExpr) |> ignore
                this.VisitLocalBindingInfo(binding) |> ignore
                this.VisitExpression(bodyExpr) |> ignore

            | BoundExpression.EntityDefinition(_, boundBody, _) ->
                this.VisitExpression(boundBody) |> ignore

            | BoundExpression.GetField(receiver=receiver) ->
                this.VisitExpression(receiver) |> ignore
            | BoundExpression.SetField(receiver=receiver;rhs=rhs) ->
                this.VisitExpression(rhs) |> ignore
                this.VisitExpression(receiver) |> ignore

            | BoundExpression.GetProperty(receiverOpt=receiverOpt) ->
                receiverOpt
                |> Option.iter (fun x -> this.VisitExpression(x) |> ignore)

            | BoundExpression.SetProperty(receiverOpt=receiverOpt;rhs=rhs) ->
                this.VisitExpression(rhs) |> ignore
                receiverOpt
                |> Option.iter (fun x -> this.VisitExpression(x) |> ignore)

            | BoundExpression.SetValue(rhs=rhs) ->
                this.VisitExpression(rhs) |> ignore
            | BoundExpression.SetContentsOfAddress(lhs=lhs;rhs=rhs) ->
                this.VisitExpression(rhs) |> ignore
                this.VisitExpression(lhs) |> ignore

            | BoundExpression.Value _
            | BoundExpression.Unit _
            | BoundExpression.Literal _
            | BoundExpression.None _
            | BoundExpression.Error _
            | BoundExpression.ErrorWithNamespace _
            | BoundExpression.ErrorWithType _ ->
                ()

        result

    override this.VisitRoot(boundRoot) =
        let result = core.VisitRoot(boundRoot)

        if result then
            match boundRoot with
            | BoundRoot.Namespace(body=body) 
            | BoundRoot.Global(body=body) ->
                this.VisitExpression(body) |> ignore
        result
