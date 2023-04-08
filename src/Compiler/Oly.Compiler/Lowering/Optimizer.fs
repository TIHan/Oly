[<RequireQualifiedAccess>]
module rec Oly.Compiler.Internal.Lowering.Optimizer

open System.Threading
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.WellKnownExpressions
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations

type OptimizerSettings =
    {
        LocalValueElimination: bool
        BranchElimination: bool
    }

[<NoEquality;NoComparison>]
type cenv =
    {
        settings: OptimizerSettings
        syntaxTree: OlySyntaxTree
        ct: CancellationToken
    }

let canConvertLambdaToStaticLambda (expr: E) =
    match expr with
    | E.Lambda(tyPars=tyPars) ->
        if tyPars.IsEmpty then
            let freeTyVars = expr.GetFreeTypeVariables()
            let freeLocals = expr.GetFreeLocals()
            freeTyVars.Count = 0 && freeLocals.Count = 0
        else
            false
    | _ ->
        OlyAssert.Fail("Expected lambda expression")

let canEliminateBinding (settings: OptimizerSettings) (bindingInfo: LocalBindingInfoSymbol) (rhsExprTy: TypeSymbol) =
    // Generated let-bindings will always get eliminated regardless if local value elimination is enabled or not.
    (not bindingInfo.Value.IsBase) &&
        ((not settings.LocalValueElimination && not bindingInfo.Value.IsGenerated) || 
            bindingInfo.Value.IsMutable || 
            not (areTypesEqual bindingInfo.Type rhsExprTy))
    |> not

let analyzeImmediateExpression cenv origExpr =
    cenv.ct.ThrowIfCancellationRequested()

let optimizeImmediateExpressionPreorder cenv origExpr =
    origExpr

let optimizeImmediateExpression cenv origExpr =
    cenv.ct.ThrowIfCancellationRequested()
    
    let settings = cenv.settings

    match origExpr with
    | LogicalAnd(argExpr1, argExpr2) when settings.BranchElimination || origExpr.IsGenerated ->
        match argExpr1, argExpr2 with
        | True, _ -> argExpr2
        | _, True -> argExpr1
        | E.Sequential(_, expr1, True), _ ->
            E.CreateSequential(expr1, argExpr2)
        | _ ->
            origExpr

    | LogicalOr(argExpr1, argExpr2) when settings.BranchElimination || origExpr.IsGenerated ->
        match argExpr1, argExpr2 with
        | True, _
        | False, True -> E.CreateLiteral(cenv.syntaxTree, BoundLiteralTrue)
        | _, False -> argExpr1
        | _, True ->
            let flattenedArgExpr1 = argExpr1.FlattenSequentialExpressions()
            let lastIndex = flattenedArgExpr1.Length - 1
            match flattenedArgExpr1.[lastIndex] with
            | NoSideEffect ->
                E.CreateSequential(
                    flattenedArgExpr1.RemoveAt(lastIndex),
                    argExpr2
                )
            | lastExpr ->
                E.CreateSequential(
                    E.CreateSequential(
                        flattenedArgExpr1.RemoveAt(lastIndex),
                        Ignore lastExpr
                    ),
                    argExpr2
                )
        | _ -> 
            origExpr

    | E.IfElse(syntaxInfo, conditionExpr, truePathExpr, falsePathExpr, resultTy) when settings.BranchElimination || conditionExpr.IsGenerated ->
        match conditionExpr with
        | True -> truePathExpr
        | False -> falsePathExpr
        | _ ->
            match falsePathExpr with
            | E.IfElse(_, conditionExpr2, truePathExpr2, falsePathExpr2, _) 
                    when (settings.BranchElimination || conditionExpr2.IsGenerated) && 
                         areTargetExpressionsEqual truePathExpr truePathExpr2 ->
                E.IfElse(syntaxInfo, LogicalOr conditionExpr conditionExpr2, truePathExpr, falsePathExpr2, resultTy)
            | _ ->

            let flattenedConditionExpr = conditionExpr.FlattenSequentialExpressions()
            match flattenedConditionExpr.[flattenedConditionExpr.Length - 1] with
            | True _ ->
                E.CreateSequential(
                    flattenedConditionExpr.RemoveAt(flattenedConditionExpr.Length - 1),
                    truePathExpr
                )
            | LogicalAnd(argExpr1, argExpr2) ->
                let flattenedArgExpr2 = argExpr2.FlattenSequentialExpressions()
                match flattenedArgExpr2.[flattenedArgExpr2.Length - 1] with
                | True _ ->
                    let liftedArgExpr2 = flattenedArgExpr2.RemoveAt(flattenedArgExpr2.Length - 1)
                    E.IfElse(
                        syntaxInfo,
                        E.CreateSequential(
                            flattenedConditionExpr.RemoveAt(flattenedConditionExpr.Length - 1),
                            argExpr1
                        ),
                        E.CreateSequential(
                            liftedArgExpr2,
                            truePathExpr
                        ),
                        falsePathExpr,
                        resultTy
                    )
                | _ ->
                    origExpr
            | _ ->
                origExpr

    // Local value elimination. aka let-binding elimination
    (* Example 1:
            test(x: __oly_int32): () =
                let y = x
    
        Turns Into:
            test(x: __oly_int32): () = 
                ()

       Example 2:
            test(): () =
                let x = 1
                let y = x
                let z = x
                let w = y

        Turns Into:
            test(): () =
                let x = 1
    *)
    // This is a conservative approach to eliminating locals by not allowing the optimization if either the left-hand or right-hand sides are mutable.
    // However, this optimization is very sufficient for removing locals created by pattern matching.

    // TODO: This is a little quadratic as we are looping through 'expr2'
    //       for each local that we want to eliminate. We should stop this and instead
    //       do full single passes.

#if DEBUG
    | E.MemberDefinition(binding=binding) ->
        Assert.ThrowIf(binding.Info.Value.IsLocal)
        origExpr
#endif

    | E.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) when bindingInfo.Value.IsLocal || bindingInfo.Value.IsBase ->
        match rhsExpr with
        | E.Value(_, rhsValue) when not(rhsValue.IsMutable) && rhsValue.IsLocal ->
            if canEliminateBinding settings bindingInfo rhsValue.Type then
                let newBodyExpr =
                    bodyExpr.Rewrite(function
                        | E.Value(syntaxInfo, value) when value.IsLocal && value.Id = bindingInfo.Value.Id ->
                            E.Value(syntaxInfo, rhsValue)
                        | E.Call(syntaxInfo, receiverOpt, witnessArgs, args, value, isVirtualCall) when value.Formal.Id = bindingInfo.Value.Id ->
                            E.Call(syntaxInfo, receiverOpt, witnessArgs, args, actualValue rhsValue.Enclosing value.AllTypeArguments rhsValue, isVirtualCall)
                        | expr ->
                            expr
                    )
                if bodyExpr = newBodyExpr then
                    origExpr
                else
                    newBodyExpr
            else
                origExpr

        | E.Literal(_, literal) ->
            if canEliminateBinding settings bindingInfo literal.Type then
                let mutable canEliminate = true
                let newBodyExpr =
                    bodyExpr.Rewrite(
                        (fun expr -> 
                            match expr with
                            | AddressOf(E.Value(value=value)) ->
                                if value.Id = bindingInfo.Value.Id then
                                    canEliminate <- false
                                    false
                                else
                                    true
                            | E.Call(value=value) ->
                                // We do this to ensure we do not replace an argument for a stack-emplaced function
                                // with a literal as it is illegal.
                                if value.IsStackEmplace then
                                    canEliminate <- false
                                    false
                                else
                                    true
                            | _ ->
                                true
                        ), fun expr ->
                        match expr with
                        | E.Value(_, value) when value.IsLocal && value.Id = bindingInfo.Value.Id ->
                            rhsExpr
                        | _ ->
                            expr
                    )
                if newBodyExpr = bodyExpr then
                    origExpr
                else
                    if canEliminate then
                        newBodyExpr
                    else
                        E.Let(syntaxInfo, bindingInfo, rhsExpr, newBodyExpr)
            else
                origExpr
        | _ ->
            origExpr
    | _ ->
        origExpr

let Lower ct settings (boundTree: BoundTree) =
    let cenv = {
            settings = settings
            syntaxTree = boundTree.SyntaxTree
            ct = ct
        }
    boundTree.ForEachExpression(analyzeImmediateExpression cenv)
    boundTree.RewriteExpression(optimizeImmediateExpressionPreorder cenv, optimizeImmediateExpression cenv)