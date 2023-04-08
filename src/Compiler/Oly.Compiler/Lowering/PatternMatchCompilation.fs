[<RequireQualifiedAccess>]
module rec Oly.Compiler.Internal.Lowering.PatternMatchCompilation

open System
open System.Threading
open System.Collections.Generic
open System.Collections.ObjectModel

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.WellKnownExpressions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments  

let getFreeLocalsFromCasePattern (localLookup: Dictionary<int64, ILocalSymbol>) (casePat: BoundCasePattern) =
    match casePat with
    | BoundCasePattern.Discard _
    | BoundCasePattern.Literal _
    | BoundCasePattern.FieldConstant _ -> ()
    | BoundCasePattern.Function(_, _, _, _, casePatArgs)
    | BoundCasePattern.Tuple(_, casePatArgs) ->
        casePatArgs
        |> ImArray.iter (getFreeLocalsFromCasePattern localLookup)
    | BoundCasePattern.Local(_, _, local) ->
        localLookup[local.Id] <- local

let getFreeLocalsFromMatchPattern (matchPattern: BoundMatchPattern) =
    let localLookup = Dictionary()
    let rec loop (matchPattern: BoundMatchPattern) =
        match matchPattern with
        | BoundMatchPattern.Cases(_, casePats) ->
            casePats
            |> ImArray.iter (fun x -> getFreeLocalsFromCasePattern localLookup x)
        | BoundMatchPattern.Or(_, lhs, rhs) ->
            loop lhs
            loop rhs
    loop matchPattern
    localLookup.Values |> ImArray.ofSeq

let substituteLocals
        (
            expr: E, 
            localLookup: Dictionary<int64, ILocalSymbol>
        ) =
    let newExpr =
        expr.Rewrite(
            (fun origExpr ->
                origExpr
            ),
            (fun origExpr ->
                match origExpr with
                | BoundExpression.Value(syntaxInfo, value) when value.IsLocal && not value.IsFunction ->
                    match localLookup.TryGetValue value.Formal.Id with
                    | true, newValue ->          
                        BoundExpression.Value(syntaxInfo, newValue)
                    | _ ->
                        origExpr

                | BoundExpression.Call(syntaxInfo, None, witnessArgs, argExprs, value, isVirtualCall) 
                        when value.IsLocal && not value.IsFunction ->

                    match localLookup.TryGetValue value.Formal.Id with
                    | true, newValue ->
                        BoundExpression.Call(
                            syntaxInfo,
                            None,
                            witnessArgs,
                            argExprs,
                            newValue,
                            isVirtualCall
                        )
                    | _ ->
                        origExpr
                | _ ->
                    origExpr
            ),
            fun _ -> true
        )

    newExpr

let toTargetJump(expr: E) =
    let syntaxInfo = BoundSyntaxInfo.Generated(expr.Syntax.Tree)

    let local =
        createFunctionValue
            EnclosingSymbol.Local
            ImArray.empty
            "jump"
            ImArray.empty
            ImArray.empty
            expr.Type
            MemberFlags.Private
            FunctionFlags.StackEmplace
            WellKnownFunction.None
            None
            false

    let lambdaBodyExpr = expr

    let lambdaExpr =
        BoundExpression.CreateLambda(
            syntaxInfo,
            LambdaFlags.StackEmplace,
            local.TypeParameters,
            local.Parameters,
            LazyExpression.CreateNonLazy(
                None, 
                fun _ -> lambdaBodyExpr
            )
        )

    let callExpr =
        BoundExpression.Call(
            syntaxInfo,
            None,
            CacheValueWithArg.FromValue(ImArray.empty),
            ImArray.empty,
            local,
            false
        )

    let bindingInfo = BindingLocalFunction(local)

    syntaxInfo, bindingInfo, lambdaExpr, callExpr

let toTargetJumpWithFreeLocals (freeLocals: ILocalSymbol imarray) (expr: E) =
    if freeLocals.IsEmpty then toTargetJump(expr)
    else

    let pars = 
        freeLocals 
        |> ImArray.map (fun x -> 
            OlyAssert.False(x.IsMutable)
            createLocalParameterValue(ImArray.empty, x.Name, x.Type, false)
        )

    let localLookup = Dictionary()
    (freeLocals, pars)
    ||> ImArray.iter2 (fun local par -> localLookup[local.Id] <- par :> ILocalSymbol)

    let syntaxInfo = BoundSyntaxInfo.Generated(expr.Syntax.Tree)

    let local =
        createFunctionValue
            EnclosingSymbol.Local
            ImArray.empty
            "jump"
            ImArray.empty
            pars
            expr.Type
            MemberFlags.Private
            FunctionFlags.StackEmplace
            WellKnownFunction.None
            None
            false

    let lambdaBodyExpr = substituteLocals(expr, localLookup)

    let lambdaExpr =
        BoundExpression.CreateLambda(
            syntaxInfo,
            LambdaFlags.StackEmplace,
            local.TypeParameters,
            local.Parameters,
            LazyExpression.CreateNonLazy(
                None, 
                fun _ -> lambdaBodyExpr
            )
        )

    let callExpr =
        BoundExpression.Call(
            syntaxInfo,
            None,
            CacheValueWithArg.FromValue(ImArray.empty),
            freeLocals |> ImArray.map (fun x -> E.Value(syntaxInfo, x)),
            local,
            false
        )

    let bindingInfo = BindingLocalFunction(local)

    syntaxInfo, bindingInfo, lambdaExpr, callExpr

let OptimizedAnd expr1 expr2 =
    match expr1, expr2 with
    | False, _ -> expr1
    | True, _ -> expr2
    | _, True -> expr1
    | _ -> And expr1 expr2

// -----------------------------------------------------------------------------------------------------------

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type Decision =
    | Condition of matchIndex: int * conditionExpr: E // DO NOT PUT A PRE EXPRESSION HERE, if it requires it, we need a continuation expr.
    | ConditionContinuation of matchIndex: int * letExprPreOpt: E option * conditionExpr: E * postExpr: E * contExpr: E

    member this.MatchIndex = 
        match this with
        | Condition(matchIndex=matchIndex)
        | ConditionContinuation(matchIndex=matchIndex) -> matchIndex

type private MatchPatternLookup = ResizeArray<ResizeArray<ILocalSymbol * IValueSymbol>>

[<NoEquality;NoComparison>]
type cenv =
    {
        BoundEnvironment: BoundEnvironment
        TrueLiteralExpression: E
        NoneExpression: E
        Syntax: OlySyntaxNode
        GeneratedSyntaxInfo: BoundSyntaxInfo
        BeforeMatchExpressions: ResizeArray<E>
        MatchValueInfos: {| syntaxInfo: BoundSyntaxInfo; value: ILocalSymbol; isTmp: bool; index: int32 |} imarray
        CachedExpressionType: TypeSymbol
    }

let isSimpleCasePattern casePat =
    match casePat with
    | BoundCasePattern.Literal _
    | BoundCasePattern.Discard _ 
    | BoundCasePattern.FieldConstant _ -> true
    | BoundCasePattern.Tuple(_, casePats) ->
        casePats |> ImArray.forall isSimpleCasePattern
    | _ -> 
        false

let isSimpleMatchClause (matchClause: BoundMatchClause) =
    match matchClause with
    | BoundMatchClause.MatchClause(_, BoundMatchPattern.Cases(_, casePats), None, _) ->
        casePats 
        |> ImArray.forall isSimpleCasePattern
    | _ ->
        false

let isReallySimpleExpression (expr: E) =
    match expr with
    | E.Literal _ -> true
    | E.Value(value=value) -> value.IsLocal || value.IsFieldConstant
    | _ -> false

let isSimpleExpression (expr: E) =
    match expr with
    | E.Call(receiverOpt=Some receiver;args=argExprs) when isReallySimpleExpression receiver ->
        argExprs
        |> ImArray.forall isReallySimpleExpression
    | E.Call(receiverOpt=None;args=argExprs;value=value) ->
        if value.IsStackEmplace then
#if DEBUG
            argExprs
            |> ImArray.forall (function 
                | E.Value(value=value) -> 
                    value.IsLocal && not value.IsFunction 
                | _ -> 
                    false
            )
            |> OlyAssert.True
#endif
            true
        else
            argExprs
            |> ImArray.forall isReallySimpleExpression
    | _ ->
        isReallySimpleExpression expr

let normalizeTargetExpression cenv (targetExpr: E) matchPattern =
    if isSimpleExpression targetExpr then targetExpr
    else
        let freeLocals = getFreeLocalsFromMatchPattern matchPattern
        let syntaxInfo, bindingInfo, lambdaExpr, callExpr = toTargetJumpWithFreeLocals freeLocals targetExpr
                
        let targetJumpDefExpr =
            E.Let(
                syntaxInfo,
                bindingInfo,
                lambdaExpr,
                cenv.NoneExpression
            )
                
        cenv.BeforeMatchExpressions.Add(targetJumpDefExpr)
        callExpr

let normalizeContinuationExpression cenv (contExpr: E) =
    if isSimpleExpression contExpr then contExpr
    else
        let syntaxInfo, bindingInfo, lambdaExpr, callExpr = toTargetJump contExpr
                
        let targetJumpDefExpr =
            E.Let(
                syntaxInfo,
                bindingInfo,
                lambdaExpr,
                cenv.NoneExpression
            )
                
        cenv.BeforeMatchExpressions.Add(targetJumpDefExpr)
        callExpr

[<NoComparison;NoEquality>]
type PatternExpressionsInfo =
    {
        Pre: E option
        Condition: E
        Post: E
    }

let createInfo preExpr conditionExpr postExpr =
    {
        Pre = preExpr
        Condition = conditionExpr
        Post = postExpr
    }

let createCallExpression syntaxInfo (func: IFunctionSymbol) witnessArgs argExprs =
    BoundExpression.Call(
        syntaxInfo,
        None,
        CacheValueWithArg.FromValue(witnessArgs),
        argExprs,
        func,
        func.IsVirtual
    )

let createLiteralInfo cenv syntax benv matchValueExpr literal =
    let literalExpr = E.Literal(cenv.GeneratedSyntaxInfo, literal)
    createInfo None (EqualWithSyntax (BoundSyntaxInfo.User(syntax, benv)) matchValueExpr literalExpr) cenv.NoneExpression

let transformPattern cenv (valueLookup: MatchPatternLookup) matchPatternIndex matchValueExpr (pattern: BoundCasePattern) (contExprOpt: E option) =
    let trueLiteralExpr = cenv.TrueLiteralExpression

    match pattern with
    | BoundCasePattern.Discard _ ->
        createInfo None trueLiteralExpr (Ignore matchValueExpr)

    | BoundCasePattern.Literal(syntax, benv, literal) ->        
        createLiteralInfo cenv syntax benv matchValueExpr literal

    | BoundCasePattern.FieldConstant(syntax, benv, field) ->
        let literal = field.Constant.Value.ToLiteral()
        createLiteralInfo cenv syntax benv matchValueExpr literal

    | BoundCasePattern.Local(syntax, benv, value) ->
        let newValue = createLocalValue value.Name value.Type
        valueLookup.[matchPatternIndex].Add(newValue, value)

        let postExpr =
            E.Let(
                BoundSyntaxInfo.User(syntax, benv),
                BindingLocal(newValue),
                matchValueExpr,
                cenv.NoneExpression
            )

        createInfo None trueLiteralExpr postExpr

    | BoundCasePattern.Tuple(_, casePatArgs) ->
        let tmpValue = createLocalValue "tmp" matchValueExpr.Type
        let matchValueLetExpr =
            E.Let(
                cenv.GeneratedSyntaxInfo,
                BindingLocal(tmpValue),
                matchValueExpr,
                cenv.NoneExpression
            )

        let info =
            transformTuplePattern
                cenv
                valueLookup
                matchPatternIndex
                tmpValue
                casePatArgs
                contExprOpt

        createInfo (Some matchValueLetExpr) info.Condition info.Post

    | BoundCasePattern.Function(syntax, benv, pat, witnessArgs, casePatArgs) ->
        let patFunc = pat.PatternFunction

        match pat.PatternGuardFunction with
        | Some(patGuardFunc) ->
            let matchValueExpr, matchValueLetExpr =
                let tmpValueName =
                    match matchValueExpr with
                    | E.Value(_, value) -> value.Name
                    | _ -> "tmp"
                let tmpValue = createLocalValue tmpValueName matchValueExpr.Type
                E.Value(cenv.GeneratedSyntaxInfo, tmpValue),
                E.Let(
                    cenv.GeneratedSyntaxInfo,
                    BindingLocal(tmpValue),
                    matchValueExpr,
                    cenv.NoneExpression
                )

            let callGuardExpr =
                createCallExpression
                    (BoundSyntaxInfo.User(syntax, benv))
                    patGuardFunc
                    witnessArgs
                    (ImArray.createOne matchValueExpr)

            let callExpr =
                createCallExpression
                    cenv.GeneratedSyntaxInfo
                    patFunc
                    witnessArgs
                    (ImArray.createOne matchValueExpr)

            match casePatArgs.Length with
            | 0 ->
                OlyAssert.True(callExpr.Type.IsUnit_t)
                createInfo (Some matchValueLetExpr) callGuardExpr (Ignore callExpr)

            | 1 ->
                let info = transformPattern cenv valueLookup matchPatternIndex callExpr casePatArgs[0] contExprOpt
                let postExpr =
                    match info.Pre with
                    | Some(preExpr) ->
                        let branchExpr =
                            E.IfElse(
                                cenv.GeneratedSyntaxInfo,
                                info.Condition,
                                info.Post,
                                contExprOpt.Value,
                                info.Post.Type
                            )
                        insertExpressionIntoExpression preExpr branchExpr
                    | _ ->
                        let branchExpr =
                            E.IfElse(
                                cenv.GeneratedSyntaxInfo,
                                info.Condition,
                                info.Post,
                                contExprOpt.Value,
                                info.Post.Type
                            )
                        branchExpr

                createInfo (Some matchValueLetExpr) callGuardExpr postExpr

            | _ ->
                let tmpTupleValue = createLocalValue "tmp" callExpr.Type
                let tmpTupleValueLetExpr =
                    E.Let(
                        cenv.GeneratedSyntaxInfo,
                        BindingLocal(tmpTupleValue),
                        callExpr,
                        cenv.NoneExpression
                    )

                let info =
                    transformTuplePattern
                        cenv
                        valueLookup
                        matchPatternIndex
                        tmpTupleValue
                        casePatArgs
                        contExprOpt

                let postExpr =
                    match info.Pre with
                    | Some(preExpr) ->
                        let branchExpr =
                            E.IfElse(
                                cenv.GeneratedSyntaxInfo,
                                info.Condition,
                                info.Post,
                                contExprOpt.Value,
                                info.Post.Type
                            )
                        insertExpressionIntoExpression preExpr branchExpr
                        |> insertExpressionIntoExpression tmpTupleValueLetExpr
                    | _ ->
                        let branchExpr =
                            E.IfElse(
                                cenv.GeneratedSyntaxInfo,
                                info.Condition,
                                info.Post,
                                contExprOpt.Value,
                                info.Post.Type
                            )
                        insertExpressionIntoExpression tmpTupleValueLetExpr branchExpr

                createInfo (Some matchValueLetExpr) callGuardExpr postExpr
                
        | _ ->
            let callExpr =
                createCallExpression
                    (BoundSyntaxInfo.User(syntax, benv))
                    patFunc
                    witnessArgs
                    (ImArray.createOne matchValueExpr)

            match casePatArgs.Length with
            | 0 ->
                OlyAssert.True(callExpr.Type.IsUnit_t)
                createInfo None trueLiteralExpr (Ignore callExpr)
            | 1 ->
                transformPattern cenv valueLookup matchPatternIndex callExpr casePatArgs[0] contExprOpt
            | _ ->
                let tmpTupleValue = createLocalValue "tmp" callExpr.Type
                let tmpTupleValueLetExpr =
                    E.Let(
                        cenv.GeneratedSyntaxInfo,
                        BindingLocal(tmpTupleValue),
                        callExpr,
                        cenv.NoneExpression
                    )

                let info =
                    transformTuplePattern
                        cenv
                        valueLookup
                        matchPatternIndex
                        tmpTupleValue
                        casePatArgs
                        contExprOpt

                let postExpr =
                    match info.Pre with
                    | Some(preExpr) ->
                        let branchExpr =
                            E.IfElse(
                                cenv.GeneratedSyntaxInfo,
                                info.Condition,
                                info.Post,
                                contExprOpt.Value,
                                info.Post.Type
                            )
                        insertExpressionIntoExpression preExpr branchExpr
                        |> insertExpressionIntoExpression tmpTupleValueLetExpr
                    | _ ->
                        let branchExpr =
                            E.IfElse(
                                cenv.GeneratedSyntaxInfo,
                                info.Condition,
                                info.Post,
                                contExprOpt.Value,
                                info.Post.Type
                            )
                        insertExpressionIntoExpression tmpTupleValueLetExpr branchExpr

                createInfo None trueLiteralExpr postExpr

let transformTuplePattern cenv (valueLookup: MatchPatternLookup) matchPatternIndex (tupleValue: IValueSymbol) (patArgs: BoundCasePattern imarray) contExprOpt : PatternExpressionsInfo =
    patArgs
    |> ImArray.mapi (fun i patArg ->
        let loadTupleElementExpr =
            let valueExpr = E.CreateValue(cenv.GeneratedSyntaxInfo, tupleValue)
            LoadTupleElement i tupleValue.Type valueExpr

        transformPattern
            cenv
            valueLookup
            matchPatternIndex
            loadTupleElementExpr
            patArg
            contExprOpt
    )
    |> ImArray.reduceBack (fun info1 info2 ->
        let preExprOpt1 = info1.Pre
        let preExprOpt2 = info2.Pre
        let conditionExpr1 = info1.Condition
        let conditionExpr2 = info2.Condition
        let postExpr1 = info1.Post
        let postExpr2 = info2.Post

        let preExprOpt =
            match preExprOpt1 with
            | Some(preExpr1) ->
                match preExprOpt2 with
                | Some(preExpr2) ->
                    insertExpressionIntoExpression preExpr1 preExpr2 |> Some
                | _ ->
                    preExpr1 |> Some
            | _ ->
                match preExprOpt2 with
                | Some(preExpr2) ->
                    Some preExpr2
                | _ ->
                    None

        let conditionExpr = OptimizedAnd conditionExpr1 conditionExpr2
        match postExpr1 with
        | E.Let _
        | E.IfElse _ ->
            let postExpr = insertExpressionIntoExpression postExpr1 postExpr2
            createInfo preExprOpt conditionExpr postExpr

        | E.None _ ->
            createInfo preExprOpt conditionExpr postExpr2

        | Ignore _ ->
            let postExpr =
                E.Sequential(
                    cenv.GeneratedSyntaxInfo,
                    postExpr1,
                    postExpr2
                )

            createInfo preExprOpt conditionExpr postExpr

        | _ ->
            failwith "Invalid expression"
    )

let transformTopLevelPattern (cenv: cenv) (valueInfos: {| syntaxInfo: BoundSyntaxInfo; value: ILocalSymbol; isTmp: bool; index: int32 |} imarray) (valueLookup: MatchPatternLookup) matchPatternIndex column (casePat: BoundCasePattern) contExprOpt =
    let trueLiteralExpr = cenv.TrueLiteralExpression

    let valueExpr = E.Value(cenv.GeneratedSyntaxInfo, valueInfos.[column].value)
    match casePat with
    | BoundCasePattern.Discard _ -> 
        createInfo None trueLiteralExpr cenv.NoneExpression

    | BoundCasePattern.Literal _
    | BoundCasePattern.FieldConstant _ ->
        let info = transformPattern cenv valueLookup matchPatternIndex valueExpr casePat contExprOpt
        OlyAssert.True(info.Pre.IsNone)
        createInfo None info.Condition info.Post

    | BoundCasePattern.Local _ ->
        let valueExpr = E.Value(cenv.GeneratedSyntaxInfo, valueInfos.[column].value)
        transformPattern cenv valueLookup matchPatternIndex valueExpr casePat contExprOpt

    | BoundCasePattern.Tuple(_, casePatArgs) ->
        transformTuplePattern
            cenv
            valueLookup
            matchPatternIndex
            valueInfos[column].value
            casePatArgs
            contExprOpt

    | BoundCasePattern.Function _ ->
        let valueExpr = E.Value(cenv.GeneratedSyntaxInfo, valueInfos.[column].value)
        transformPattern cenv valueLookup matchPatternIndex valueExpr casePat contExprOpt

let tryCombineDecision (cenv: cenv) (decision1: Decision) (decision2: Decision) =
    match decision1, decision2 with
    | Decision.Condition(matchIndex1, conditionExpr1),
      Decision.Condition(matchIndex2, conditionExpr2) when matchIndex1 = matchIndex2 ->
        Decision.Condition(matchIndex1, OptimizedAnd conditionExpr1 conditionExpr2)
        |> Some
    | Decision.ConditionContinuation(matchIndex1, None, conditionExpr1, E.None _, contExpr1),
      Decision.ConditionContinuation(matchIndex2, None, conditionExpr2, postExpr, contExpr2)
            when matchIndex1 = matchIndex2 && areTargetExpressionsEqual contExpr1 contExpr2 ->
      Decision.ConditionContinuation(matchIndex1, None, OptimizedAnd conditionExpr1 conditionExpr2, postExpr, contExpr1)
      |> Some

    | _ ->
        None

let combineDecisions (cenv: cenv) (decisions: Decision imarray) =
    if decisions.Length <= 1 then decisions
    else
        let newDecisions = ImArray.builderWithSize decisions.Length

        decisions
        |> ImArray.iter (fun decision ->
            if newDecisions.Count > 0 then
                match tryCombineDecision cenv newDecisions[newDecisions.Count - 1] decision with
                | Some combinedDecision ->
                    newDecisions[newDecisions.Count - 1] <- combinedDecision
                | _ ->
                    newDecisions.Add(decision)
            else
                newDecisions.Add(decision)
        )

        newDecisions.ToImmutable()

let combineDecisionGroup (cenv: cenv) (decisionGroup: Decision imarray imarray) =
    decisionGroup
    |> ImArray.map (fun decisions -> combineDecisions cenv decisions)

let transformPatterns (cenv: cenv) (matchPatternLookup: MatchPatternLookup) matchPatternIndex (contExprOpt: E option) (casePats: BoundCasePattern imarray) =
    let matchValueInfos = cenv.MatchValueInfos

    if casePats.IsEmpty then
        failwith "Expected patterns."
    else
        let decisions = ImArray.builder()

        let rec handlePatterns i (casePats: ReadOnlyMemory<BoundCasePattern>) (decisions: imarrayb<Decision>) =
            if casePats.IsEmpty then ()
            else
                let casePat = casePats[0]
                let info = transformTopLevelPattern cenv matchValueInfos matchPatternLookup matchPatternIndex i casePat contExprOpt
                let preExprOpt = info.Pre
                let conditionExpr = info.Condition
                let postExpr = info.Post

                match preExprOpt, postExpr with
                | None, E.None _ when contExprOpt.IsNone ->
                    Decision.Condition(matchPatternIndex, conditionExpr)
                    |> decisions.Add
                    handlePatterns (i + 1) (casePats.Slice(1)) decisions
                | _ ->
                    OlyAssert.True(contExprOpt.IsSome)
                    let contExpr = contExprOpt.Value

                    (matchPatternIndex, preExprOpt, conditionExpr, postExpr, contExpr)
                    |> Decision.ConditionContinuation
                    |> decisions.Add
                    handlePatterns (i + 1) (casePats.Slice(1)) decisions

        handlePatterns 0 (casePats.AsMemory()) decisions
        decisions.ToImmutable()

/// Using this example:
///     ...
///     match (x, x, x, x, x, x, x, x, x, x)
///     | _, y, _, _, _, _, _, _, _, 1
///     | _, _, y, _, _, _, _, _, _, 2
///     | y, _, _, _, _, _, _, _, _, 3
///     | _, _, _, _, y, _, _, _, _, 4 
///     | _, _, _, _, _, _, _, y, _, 5 => $(targetExpr)
///     ...
///
///     'PatternMatchCompilation' will generate five instance of 'y' with different 'IValueSymbol.Id's.
///     The original 'y' (before PatternMatchCompilation) is still used in $(targetExpr).
///     This function's responsibility is to replace the original 'y' with one of the generated 'y' instances.
let replaceTargetExpressionByValue (valueLookup: MatchPatternLookup) i (expr: E) =
    let tmpValues, patternValues = 
        valueLookup.[i]
        |> Array.ofSeq
        |> Array.unzip
    if tmpValues.Length > 0 then
        // This may appear quadratic since we are in the middle of lowering,
        // but it should be ok since the main lowering part only looks
        // at match expressions.
        expr.Rewrite(fun expr ->
            match expr with
            | E.Value(syntaxInfo, value) ->
                // Try to get the largest index from 'patternValues' that has our value; this is why we use 'Array.rev' and 'flipIndex'.
                // This is the way to get the latest generated value.
                // Think of it like 'Array.tryBackFindIndex' would be.
                match patternValues |> Array.rev |> Array.tryFindIndex (fun value2 -> value2.Id = value.Id) with
                | Some index ->
                    let flipIndex index =
                        patternValues.Length - 1 - index
                    E.Value(syntaxInfo, tmpValues.[flipIndex index])
                | _ ->
                    expr
            | _ ->
                expr
        )
    else
        expr

let tryTransformTargetExpression (cenv: cenv) (valueLookup: MatchPatternLookup) i (expr: E) =
    let matchValueInfos = cenv.MatchValueInfos

    let tmpValues, patternValues = 
        valueLookup.[i]
        |> Array.ofSeq
        |> Array.unzip
    if tmpValues.Length > 0 then
        let valueDeclExprs =
            tmpValues
            |> Seq.map (fun tmpValue ->
                let syntaxInfo = BoundSyntaxInfo.Generated(cenv.Syntax.Tree)
                E.Let(
                    syntaxInfo,
                    BindingLocal(tmpValue),
                    (E.CreateValue(cenv.Syntax.Tree, matchValueInfos.[i].value)),
                    cenv.NoneExpression
                )
            )
        Some(valueDeclExprs, replaceTargetExpressionByValue valueLookup i expr)
    else
        None

let transformTargetExpression (cenv: cenv) matchPatternLookup i targetExpr =
    match tryTransformTargetExpression cenv matchPatternLookup i targetExpr with
    | Some(_, newTargetExpr) ->
        newTargetExpr
    | _ ->
        targetExpr

let insertExpressionIntoExpression (expr: E) (exprToInsert: E) : E =
    (expr.FlattenSequentialExpressions(), exprToInsert)
    ||> Seq.foldBack (fun letExpr expr ->
        let rec transform foldingExpr =
            match foldingExpr with
            | E.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
                Assert.ThrowIfNot(
                    match bodyExpr with
                    | E.None _ 
                    | E.Let _ -> true
                    | E.IfElse _ -> true
                    | Ignore _ -> true
                    | _ -> false
                )
                match bodyExpr with
                | E.None _ ->
                    E.Let(syntaxInfo, bindingInfo, rhsExpr, expr)
                | E.Let _ 
                | E.IfElse _
                | Ignore _ ->
                    E.Let(syntaxInfo, bindingInfo, rhsExpr, transform bodyExpr)
                | _ ->
                    OlyAssert.Fail("bad expression")
            | E.None _ ->
                expr
            | Ignore _ ->
                E.Sequential(
                    BoundSyntaxInfo.Generated(expr.Syntax.Tree),
                    foldingExpr,
                    expr
                )
            | E.IfElse(syntaxInfo, conditionExpr, trueTargetExpr, falseTargetExpr, cachedExprTy) ->
                E.IfElse(syntaxInfo, conditionExpr, transform trueTargetExpr, falseTargetExpr, cachedExprTy)
            | E.Sequential(syntaxInfo, expr1, expr2) ->
                E.Sequential(syntaxInfo, expr1, transform expr2)
            | _ ->
                OlyAssert.Fail("bad expression")
        transform letExpr
    )

let transformTargetAndGuardExpression (cenv: cenv) matchPatternLookup i (decisions: Decision imarray) targetExpr guardExprOpt : (E * E) =
    OlyAssert.False(decisions.IsEmpty)

    let trueLiteralExpr = cenv.TrueLiteralExpression
    let syntax = cenv.Syntax

    let i = decisions[0].MatchIndex

    let decisions =
        match guardExprOpt with
        | Some(guardExpr) ->
            Decision.Condition(i, replaceTargetExpressionByValue matchPatternLookup i guardExpr)
            |> decisions.Add
        | _ ->
            decisions

    let targetExpr = transformTargetExpression cenv matchPatternLookup i targetExpr

    let rec f (decisions: ReadOnlyMemory<Decision>) targetExpr conditionExpr : (E * E) =
        if decisions.IsEmpty then
            conditionExpr, targetExpr
        else
            match decisions[0] with
            | Decision.Condition(_, conditionExpr2) ->
                match conditionExpr with
                | True ->
                    f (decisions.Slice(1)) targetExpr conditionExpr2
                | _ ->
                    f (decisions.Slice(1)) targetExpr (OptimizedAnd conditionExpr conditionExpr2)

            // TP Optimization: No need to create nested IfElse expressions.
            | Decision.ConditionContinuation(_, None, conditionExpr2, E.None _, contExpr) ->
                let falseTargetExpr = contExpr

                let conditionExpr3, finalTrueTargetExpr = f (decisions.Slice(1)) targetExpr conditionExpr

                match OptimizedAnd conditionExpr2 conditionExpr3 with
                | True ->
                    conditionExpr, finalTrueTargetExpr
                | False ->
                    conditionExpr, falseTargetExpr
                | innerConditionExpr ->
                    let newTargetExpr =              
                        E.IfElse(
                            BoundSyntaxInfo.Generated(syntax.Tree),
                            innerConditionExpr,
                            finalTrueTargetExpr,
                            falseTargetExpr,
                            falseTargetExpr.Type
                        )
                
                    conditionExpr, newTargetExpr

            | Decision.ConditionContinuation(_, preExprOpt, conditionExpr2, postExpr, contExpr) ->
                let falseTargetExpr = contExpr

                let conditionExpr3, finalTrueTargetExpr = f (decisions.Slice(1)) targetExpr conditionExpr

                let trueTargetExpr =
                    let expr =
                        match conditionExpr3 with
                        | True ->
                            finalTrueTargetExpr
                        | False ->
                            falseTargetExpr
                        | _ ->
                            E.IfElse(
                                BoundSyntaxInfo.Generated(syntax.Tree),
                                conditionExpr3,
                                finalTrueTargetExpr,
                                falseTargetExpr,
                                falseTargetExpr.Type
                            )
                    insertExpressionIntoExpression postExpr expr

                let newTargetExpr =
                    match conditionExpr2 with
                    | True ->
                        trueTargetExpr
                    | False ->
                        falseTargetExpr
                    | _ ->
                        E.IfElse(
                            BoundSyntaxInfo.Generated(syntax.Tree),
                            conditionExpr2,
                            trueTargetExpr,
                            falseTargetExpr,
                            falseTargetExpr.Type
                        )

                let newTargetExpr =
                    match preExprOpt with
                    | Some(letExpr) ->
                        insertExpressionIntoExpression letExpr newTargetExpr
                    | _ ->
                        newTargetExpr
                
                conditionExpr, newTargetExpr

    f (decisions.AsMemory()) targetExpr trueLiteralExpr

let transformMatchPattern (cenv: cenv) (matchPatternLookup: MatchPatternLookup) matchPatternIndex (contExprOpt: E option) (targetExpr: E) (guardExprOpt: E option) (matchPattern): (Decision imarray imarray) =
    match matchPattern with
    | BoundMatchPattern.Cases(_, patterns) ->
        matchPatternLookup.Add(ResizeArray())
        transformPatterns cenv matchPatternLookup matchPatternIndex contExprOpt patterns
        |> combineDecisions cenv
        |> ImArray.createOne

    | BoundMatchPattern.Or(_, matchPattern1, matchPattern2) ->
        let decisionGroup2 = 
            transformMatchPattern cenv matchPatternLookup (matchPatternIndex) contExprOpt targetExpr guardExprOpt matchPattern2
            |> combineDecisionGroup cenv

        // TP Optimization: This prevents creating a new jump when it is not needed.
        if decisionGroup2.Length = 1 && 
           decisionGroup2[0].Length = 1 && 
           (match decisionGroup2[0][0] with Decision.ConditionContinuation(_, None, _, E.None _, _) -> true | _ -> false) then
                let contExpr = 
                    decisionGroup2
                    |> decisionsToClauseExpressions cenv matchPatternLookup targetExpr guardExprOpt
                    |> finalizeClauseExpressions cenv

                transformMatchPattern cenv matchPatternLookup (matchPatternIndex + decisionGroup2.Length) (Some contExpr) targetExpr guardExprOpt matchPattern1
                |> combineDecisionGroup cenv
        else

        let contExpr = 
            decisionGroup2
            |> decisionsToClauseExpressions cenv matchPatternLookup targetExpr guardExprOpt
            |> finalizeClauseExpressions cenv
            
        let contExprOpt = Some(normalizeContinuationExpression cenv contExpr)

        transformMatchPattern cenv matchPatternLookup (matchPatternIndex + decisionGroup2.Length) contExprOpt targetExpr guardExprOpt matchPattern1
        |> combineDecisionGroup cenv

let decisionsToClauseExpressions (cenv: cenv) matchPatternLookup targetExpr guardExprOpt (decisionGroup: Decision imarray imarray) : (E * E) imarray =
    decisionGroup
    |> ImArray.mapi (fun i decisions ->
        transformTargetAndGuardExpression cenv matchPatternLookup i decisions targetExpr guardExprOpt
    )

let transformMatchClause (cenv: cenv) contExprOpt matchClause : (E * E) imarray =
    let matchPatternLookup = ResizeArray<ResizeArray<ILocalSymbol * _>>()
    match matchClause with
    | BoundMatchClause.MatchClause(_, matchPattern, guardExprOpt, targetExpr) ->
        let targetExpr = 
            // TP optimization - we don't need to create a lambda continuation if the next match clause is very simple
            //                   by not having divergent paths.
            if isSimpleMatchClause matchClause then
                targetExpr
            else
                normalizeTargetExpression cenv targetExpr matchPattern
        transformMatchPattern cenv matchPatternLookup 0 contExprOpt targetExpr guardExprOpt matchPattern
        |> decisionsToClauseExpressions cenv matchPatternLookup targetExpr guardExprOpt

// Examples:
//      match (value)
//      | Pattern(123) => () // <-- match clause
//      | Pattern(456) => () // <-- match clause
//      | _ => ()            // <-- match clause
//      match (value)
//      | Pattern(123)       // |
//      | Pattern(456) => () // <-- match clause
//      | _ => ()            // <-- match clause
let transformMatchClauses (cenv: cenv) (matchClauses: ReadOnlyMemory<BoundMatchClause>) (contExprOpt: E option) =
    OlyAssert.False(matchClauses.IsEmpty)
    let matchClause = matchClauses[0]
    let clauseExprs = transformMatchClause cenv contExprOpt matchClause
    if matchClauses.Length = 1 then
        clauseExprs
        |> finalizeClauseExpressions cenv
    else
        let contExpr =
            clauseExprs
            |> finalizeClauseExpressions cenv

        let matchClauses = matchClauses.Slice(1)
        let nextMatchClause = matchClauses[0]

        let contExprOpt = 
            // TP optimization - we don't need to create a lambda continuation if the next match clause is very simple
            //                   by not having divergent paths.
            if isSimpleMatchClause nextMatchClause then
                Some contExpr
            else
                Some(normalizeContinuationExpression cenv contExpr)
        transformMatchClauses cenv matchClauses contExprOpt

let finalizeClauseExpressions (cenv: cenv) (clauseExprs: (E * E) imarray) =
    if clauseExprs.IsEmpty then
        cenv.NoneExpression
    elif clauseExprs.Length = 1 then               
        let _conditionExpr, targetExpr = clauseExprs.[0]
        // At this point, conditionExpr is irrelevant since it's the last clause.
        targetExpr
    else
            
        let rec reduce i =
            let j = i + 1
            if j >= clauseExprs.Length then
                let _conditionExpr, targetExpr = clauseExprs.[i]
                // At this point, conditionExpr is irrelevant since it's the last clause.
                targetExpr
            else
                let conditionExpr1, targetExpr1 = clauseExprs.[i]
                let conditionExpr2, targetExpr2 = clauseExprs.[j]
                let next = j + 1
                if next >= clauseExprs.Length then
                    match conditionExpr1 with
                    | True ->
                        targetExpr1
                    | _ ->
                        // At this point, conditionExpr2 is irrelevant since it's the last clause.
                        E.IfElse(
                            cenv.GeneratedSyntaxInfo,
                            conditionExpr1,
                            targetExpr1,
                            targetExpr2,
                            targetExpr1.Type
                        )
                else
                    match conditionExpr1 with
                    | True ->
                        targetExpr1
                    | _ ->
                        E.IfElse(
                            cenv.GeneratedSyntaxInfo,
                            conditionExpr1,
                            targetExpr1,
                            (
                            match conditionExpr2 with
                            | True ->
                                targetExpr2
                            | _ ->
                                E.IfElse(
                                    cenv.GeneratedSyntaxInfo,
                                    conditionExpr2,
                                    targetExpr2,
                                    reduce next,
                                    targetExpr2.Type
                                )
                            ),
                            targetExpr1.Type
                        )
        reduce 0

let tryCombineCasePattern (casePat1: BoundCasePattern) (casePat2: BoundCasePattern) =
    match casePat1, casePat2 with
    | BoundCasePattern.Discard _, _ ->
        Some(casePat1)
    | BoundCasePattern.Literal(_, _, literal1),
      BoundCasePattern.Literal(_, _, literal2) ->
        if areLiteralsEqual literal1 literal2 then
            Some(casePat1)
        else
            None

    | BoundCasePattern.Tuple(_, casePats1),
      BoundCasePattern.Tuple(_, casePats2) when casePats1.Length = casePats2.Length ->
        let results =
            (casePats1, casePats2)
            ||> ImArray.map2 tryCombineCasePattern

        // Conservative because all the cases must have identity.
        // TODO: We should be able to use partial results and create a new match expression.
        if results |> ImArray.forall (fun x -> x.IsSome) then
            Some(casePat1)
        else
            None

    | BoundCasePattern.Function(_, _, pat1, witnessArgs1, casePats1),
      BoundCasePattern.Function(_, _, pat2, witnessArgs2, casePats2) 
            when pat1.PatternFunction.IsPure && 
                 areLogicalFunctionSignaturesEqual pat1.PatternFunction pat2.PatternFunction && 
                 witnessArgs1.Length = witnessArgs2.Length &&
                 ((witnessArgs1, witnessArgs2) ||> ImArray.forall2 (fun x y -> areWitnessesEqual x.Solution.Value y.Solution.Value)) ->
        let results =
            (casePats1, casePats2)
            ||> ImArray.map2 tryCombineCasePattern

        // Conservative because all the cases must have identity.
        // TODO: We should be able to use partial results and create a new match expression.
        if results |> ImArray.forall (fun x -> x.IsSome) then
            Some(casePat1)
        else
            None

    | _ ->
        None

let tryCombineMatchPattern (matchPat1: BoundMatchPattern) (matchPat2: BoundMatchPattern) =
    match matchPat1, matchPat2 with
    | BoundMatchPattern.Cases(syntaxMatchPat1, casePats1), BoundMatchPattern.Cases(_, casePats2) when casePats1.Length = casePats1.Length ->
        let results =
            (casePats1, casePats2)
            ||> ImArray.map2 (fun casePat1 casePat2 ->
                tryCombineCasePattern casePat1 casePat2 
            )

        // Conservative because all the cases must have identity.
        // TODO: We should be able to use partial results and create a new match expression.
        if results |> ImArray.forall (fun x -> x.IsSome) then
            let casePats = results |> ImArray.map (fun x -> x.Value)
            Some(BoundMatchPattern.Cases(syntaxMatchPat1, casePats) |> Choice1Of2)
        else
            None

    | BoundMatchPattern.Cases(syntaxMatchPat1, casePats1), BoundMatchPattern.Or(_, lhsMatchPat, rhsMatchPat) ->
        match lhsMatchPat with
        | BoundMatchPattern.Cases(_, casePats2) when casePats1.Length = casePats2.Length ->
            let results =
                (casePats1, casePats2)
                ||> ImArray.map2 (fun casePat1 casePat2 ->
                    tryCombineCasePattern casePat1 casePat2
                )

            // Conservative because all the cases must have identity.
            // TODO: We should be able to use partial results and create a new match expression.
            if results |> ImArray.forall (fun x -> x.IsSome) then
                let casePats = results |> ImArray.map (fun x -> x.Value)
                let lhsMatchPat = BoundMatchPattern.Cases(syntaxMatchPat1, casePats)
                match tryCombineMatchPattern lhsMatchPat rhsMatchPat with
                | Some(Choice2Of2 _) -> raise(System.NotImplementedException())
                | Some(Choice1Of2 combinedMatchPat) ->
                    Some(combinedMatchPat |> Choice1Of2)
                | _ ->
                    Some(BoundMatchPattern.Or(syntaxMatchPat1, lhsMatchPat, rhsMatchPat) |> Choice1Of2)
            else
                None
        | _ ->
            None

    | BoundMatchPattern.Or(_, lhsMatchPat, rhsMatchPat), BoundMatchPattern.Cases _ ->
        match tryCombineMatchPattern lhsMatchPat rhsMatchPat with
        | Some(Choice2Of2 _) -> raise(System.NotImplementedException())
        | Some(Choice1Of2 combinedMatchPat) ->
            match tryCombineMatchPattern combinedMatchPat matchPat2 with
            | Some(Choice2Of2 _) -> raise(System.NotImplementedException())
            | Some(Choice1Of2 combinedMatchPat) ->
                Some(combinedMatchPat |> Choice1Of2)
            | _ ->
                Some(Choice2Of2(combinedMatchPat, matchPat2))
        | _ ->
            None

    | BoundMatchPattern.Or(_, lhsMatchPat1, rhsMatchPat1), BoundMatchPattern.Or(_, lhsMatchPat2, rhsMatchPat2) ->
        match tryCombineMatchPattern lhsMatchPat1 rhsMatchPat1 with
        | Some(Choice2Of2 _) -> raise(System.NotImplementedException())
        | Some(Choice1Of2 combinedMatchPat1) ->
            match tryCombineMatchPattern combinedMatchPat1 lhsMatchPat2 with
            | Some(Choice2Of2 _) -> raise(System.NotImplementedException())
            | Some(Choice1Of2 combinedMatchPat2) ->
                match tryCombineMatchPattern combinedMatchPat2 rhsMatchPat2 with
                | Some(Choice2Of2 _) -> raise(System.NotImplementedException())
                | Some(Choice1Of2 combinedMatchPat3) ->
                    Some(combinedMatchPat3 |> Choice1Of2)
                | _ ->
                    Some(Choice2Of2(combinedMatchPat2, rhsMatchPat2))
            | _ ->
                Some(Choice2Of2(combinedMatchPat1, matchPat2))
        | _ ->
            None
    | _ ->
        None

let tryCombineMatchClause (matchClause1: BoundMatchClause) (matchCaluse2: BoundMatchClause) =
    match matchClause1, matchCaluse2 with
    | BoundMatchClause.MatchClause(syntaxMatchClause1, matchPat1, guardExprOpt1, targetExpr1),
      BoundMatchClause.MatchClause(syntaxMatchClause2, matchPat2, guardExprOpt2, targetExpr2) ->
        if guardExprOpt1.IsNone && guardExprOpt2.IsNone then
            match tryCombineMatchPattern matchPat1 matchPat2 with
            | Some(Choice1Of2 combinedMatchPat) ->
                BoundMatchClause.MatchClause(syntaxMatchClause1, combinedMatchPat, None, targetExpr1)
                |> Choice1Of2
                |> Some
            | Some(Choice2Of2(matchPat1, matchPat2)) ->
                (
                    BoundMatchClause.MatchClause(syntaxMatchClause1, matchPat1, None, targetExpr1),
                    BoundMatchClause.MatchClause(syntaxMatchClause2, matchPat2, None, targetExpr2)
                )
                |> Choice2Of2
                |> Some
            | _ ->
                None
        else
            None

let combineMatchClauses (matchClauses: BoundMatchClause imarray) =
    if matchClauses.IsEmpty || matchClauses.Length = 1 then
        matchClauses
    else
        let builder = ImArray.builder()
        let rec loop (matchClause: BoundMatchClause) nextIndex =
            if nextIndex >= matchClauses.Length then
                builder.Add(matchClause)
                builder.ToImmutable()
            else
                let nextMatchClause = matchClauses[nextIndex]
                match tryCombineMatchClause matchClause nextMatchClause with
                | Some(Choice1Of2 combinedMatchClause) ->
                    loop combinedMatchClause (nextIndex + 1)
                | Some(Choice2Of2(combinedMatchClause, nextMatchClause)) ->
                    builder.Add(combinedMatchClause)
                    loop nextMatchClause (nextIndex + 1)
                | _ ->
                    builder.Add(matchClause)
                    loop nextMatchClause (nextIndex + 1)
        loop matchClauses[0] 1 

let generateNewFrontiers (matchExpr: E) =
    match matchExpr with
    | E.Match(syntax, benv, matchValueExprs, matchClauses, cachedExprTy) ->
        let matchClauses = combineMatchClauses matchClauses
        E.Match(syntax, benv, matchValueExprs, matchClauses, cachedExprTy)
    | _ ->
        OlyAssert.Fail("Expected match expression")

let lowerMatchExpression (matchExpr: E) =
    match matchExpr with
    | E.Match(syntax, benv, matchValueExprs, matchClauses, cachedExprTy) ->
        let matchValueInfos =
            matchValueExprs
            |> ImArray.mapi (fun i expr ->
                match expr with
                | E.Value(syntaxInfo, value) when value.IsLocal ->
                    {| syntaxInfo = syntaxInfo; value = value :?> ILocalSymbol; isTmp = false; index = i |}
                | _ ->
                    let tmpValue = createLocalGeneratedValue "tmp" expr.Type
                    let syntaxInfo = BoundSyntaxInfo.Generated(syntax.Tree)
                    {| syntaxInfo = syntaxInfo; value = tmpValue; isTmp = true; index = i |}
            )

        let beforeMatchExprs = ResizeArray()

        let cenv =
            {
                BoundEnvironment = benv
                Syntax = syntax
                GeneratedSyntaxInfo = BoundSyntaxInfo.Generated(syntax.Tree)
                BeforeMatchExpressions = beforeMatchExprs
                TrueLiteralExpression = E.Literal(BoundSyntaxInfo.Generated(syntax.Tree), BoundLiteralTrue)
                NoneExpression = E.None(BoundSyntaxInfo.Generated(syntax.Tree))
                CachedExpressionType = cachedExprTy
                MatchValueInfos = matchValueInfos
            }

        let finalExpr = transformMatchClauses cenv ((matchClauses |> ImArray.rev).AsMemory()) None

        let firstExpr =
            (cenv.NoneExpression, cenv.MatchValueInfos)
            ||> ImArray.foldBack (fun expr info ->
                if info.isTmp then
                    E.Let(info.syntaxInfo, BindingLocal(info.value), matchValueExprs[info.index], expr)
                else
                    expr
            )

        beforeMatchExprs.Insert(0, firstExpr)        

        (beforeMatchExprs, finalExpr)
        ||> Seq.foldBack (fun beforeMatchExpr expr ->
            let rec transform foldingExpr =
                match foldingExpr with
                | E.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
                    Assert.ThrowIfNot(
                        match bodyExpr with
                        | E.None _ 
                        | E.Let _ -> true
                        | _ -> false
                    )
                    match bodyExpr with
                    | E.None _ ->
                        E.Let(syntaxInfo, bindingInfo, rhsExpr, expr)
                    | E.Let _ ->
                        E.Let(syntaxInfo, bindingInfo, rhsExpr, transform bodyExpr)
                    | _ ->
                        Assert.ThrowIf(true)
                        expr
                | E.None _ ->
                    expr
                | _ ->
                    Assert.ThrowIf(true)
                    expr
            transform beforeMatchExpr
        )
    | _ ->
        OlyAssert.Fail("Expected match expression")
    
let lowerMatchExpressions (expr: E) =
    expr.Rewrite(fun origExpr ->
        match origExpr with
        | E.Match _ ->
            lowerMatchExpression origExpr
        | _ ->
            origExpr
    )

let Lower (ct: CancellationToken) (boundTree: BoundTree) =
    boundTree.RewriteExpression(fun origExpr ->
        ct.ThrowIfCancellationRequested()
        match origExpr with
#if DEBUG
        | E.MemberDefinition(binding=binding) ->
            Assert.ThrowIf(binding.Info.Value.IsLocal)
            origExpr
#endif

        | E.Match _ ->
            let frontieredExpr = generateNewFrontiers origExpr
            lowerMatchExpressions frontieredExpr
        | _ ->
            origExpr
    )