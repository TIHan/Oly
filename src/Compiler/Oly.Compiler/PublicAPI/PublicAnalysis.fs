module rec Oly.Compiler.Analysis

open System
open System.Text
open System.Threading
open System.Diagnostics
open System.Collections.Immutable
open Oly.Core
open Oly.Metadata
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Syntax.Internal
open Oly.Compiler.Internal
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.Binder

[<NoEquality;NoComparison;DebuggerDisplay("{DebugText}")>]
type OlyAnalysisExpression =
    private
    | Expression of BoundExpression * OlyBoundModel * ct: CancellationToken

    override this.ToString() =
        match this with
        | Patterns.Let _ -> "Let"
        | Patterns.Value _ -> "Value"
        | _ -> "todo - implement ToString"

    member private this.DebugText = this.ToString()

    member this.IsGenerated = 
         match this with
         | Expression(expr, _, _) -> expr.IsGenerated

    member this.SyntaxNode =
        match this with
        | Expression(expr, _, _) -> expr.Syntax

    member this.Type =
        match this with
        | Expression(expr, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            OlyTypeSymbol(boundModel, boundModel.GetBoundTree(ct).RootEnvironment, expr.Syntax, expr.Type)

[<NoEquality;NoComparison;DebuggerDisplay("{DebugText}")>]
type OlyAnalysisPattern =
    private
    | Pattern of BoundCasePattern * OlyBoundModel * ct: CancellationToken

    override this.ToString() = "Pattern"

    member private this.DebugText = this.ToString()

    member this.SyntaxNode =
        match this with
        | Pattern(casePat, _, _) -> casePat.Syntax

[<NoEquality;NoComparison;DebuggerDisplay("{DebugText}")>]
type OlyAnalysisMatchPattern =
    private
    | MatchPattern of BoundMatchPattern * OlyBoundModel * ct: CancellationToken

    override this.ToString() = "MatchPattern"

    member private this.DebugText = this.ToString()

    member this.SyntaxNode =
        match this with
        | MatchPattern(matchPattern, _, _) -> matchPattern.Syntax

[<NoEquality;NoComparison;DebuggerDisplay("{DebugText}")>]
type OlyAnalysisMatchClause =
    private
    | MatchClause of BoundMatchClause * OlyBoundModel * ct: CancellationToken

    override this.ToString() = "MatchClause"

    member private this.DebugText = this.ToString()

    member this.SyntaxNode =
        match this with
        | MatchClause(matchClause, _, _) -> matchClause.Syntax

let private cleanExpr (expr: BoundExpression) =
    match expr with
    | BoundExpression.MemberDefinition(syntaxInfo, binding) when binding.Info.Value.IsLocal ->
        BoundExpression.CreateSequential(expr.Syntax.Tree, [expr;BoundExpression.None(syntaxInfo)])
    | _ ->
        expr

let rec private stripExpr (expr: BoundExpression) =
    let newExpr = expr.Strip()
    match newExpr with
    | BoundExpression.Sequential(syntaxInfo, expr1, expr2, semantic) as expr ->
        let newExpr2 = cleanExpr expr2
        match expr1, newExpr2 with
        | BoundExpression.None _, _ -> newExpr2
        | _, BoundExpression.None _ -> cleanExpr expr1
        | _ ->
            if newExpr2 = expr2 then
                expr
            else
                BoundExpression.Sequential(
                    syntaxInfo,
                    expr1,
                    newExpr2,
                    semantic
                )
    | _ ->
        newExpr

let rec private convert (boundModel: OlyBoundModel) (expr: BoundExpression) ct : OlyAnalysisExpression =
    Expression(stripExpr expr, boundModel, ct)

type OlyAnalysisByRefKindDescription =
    | DefaultByRefKind
    | WildcardByRefKind
    | Read
    | ReadWrite

[<NoEquality;NoComparison>]
type OlyAnalysisTypeDescription =
    | DefaultType
    | WildcardType
    | Int32
    | ByRef of kind: OlyAnalysisByRefKindDescription * elementTy: OlyAnalysisTypeDescription

type OlyAnalysisRule =
    | OverloadedFunctionDefinitionsCannotDisambiguateType of OlyAnalysisTypeDescription

let rec private areTypesEqualWithDescription (tTy: OlyAnalysisTypeDescription) (ty1: TypeSymbol) (ty2: TypeSymbol) =
    match tTy with
    | WildcardType -> true
    | DefaultType -> areTypesEqual ty1 ty2
    | Int32 -> areTypesEqual ty1 TypeSymbol.Int32 && areTypesEqual ty1 ty2

    | ByRef(tByRefKind, tElementTy) ->
        match stripTypeEquationsAndBuiltIn ty1, stripTypeEquationsAndBuiltIn ty2 with
        | TypeSymbol.ByRef(elementTy1, byRefKind1), TypeSymbol.ByRef(elementTy2, byRefKind2) ->
            match tByRefKind, byRefKind1, byRefKind2 with
            | WildcardByRefKind, _, _
            | Read, ByRefKind.Read, ByRefKind.Read
            | ReadWrite, ByRefKind.ReadWrite, ByRefKind.ReadWrite ->
                areTypesEqualWithDescription tElementTy elementTy1 elementTy2

            | DefaultByRefKind, _, _ -> byRefKind1 = byRefKind2 && areTypesEqualWithDescription tElementTy elementTy1 elementTy2
            | _ -> false
        | _ ->
            false

type OlyBoundModel with

    member this.CheckRules(rules: OlyAnalysisRule imarray, ct: CancellationToken) =
        let diagLogger = OlyDiagnosticLogger.Create()
        let boundTree = this.GetBoundTree(ct)
        let diags = this.SyntaxTree.GetDiagnostics(ct).AddRange(this.GetDiagnostics(ct))
        if diags |> ImArray.exists (fun x -> x.IsError) then
            ImArray.empty
        else

            let rules_OverloadedFunctionDefinitionsCannotDisambiguateType =
                rules
                |> ImArray.choose (function
                    | OverloadedFunctionDefinitionsCannotDisambiguateType(tTy) -> Some tTy
                )

            boundTree.ForEachForTooling(function
                | :? BoundExpression as expr ->
                    match expr with
                    | BoundExpression.EntityDefinition(syntaxInfo, bodyExpr, ent) ->
                        match syntaxInfo.TryEnvironment with
                        | None -> ()
                        | Some benv ->
                            if rules_OverloadedFunctionDefinitionsCannotDisambiguateType.IsEmpty then ()
                            else
                                let overloadedFunctions = 
                                    ent.Functions 
                                    |> Seq.groupBy (fun x -> (x.Name, x.TypeArguments.Length, x.Parameters.Length, x.IsInstance))
                                    |> Seq.choose (fun (_, funcs) ->
                                        let funcs = funcs |> ImArray.ofSeq
                                        if funcs.Length > 1 then
                                            Some(funcs)
                                        else
                                            None
                                    )
                                    |> ImArray.ofSeq

                                if overloadedFunctions.IsEmpty then ()
                                else
                                    overloadedFunctions
                                    |> ImArray.iter (fun funcs ->
                                        funcs
                                        |> ImArray.iter (fun func ->
                                            funcs
                                            |> ImArray.iter (fun func2 ->
                                                if obj.ReferenceEquals(func, func2) then ()
                                                else
                                                    rules_OverloadedFunctionDefinitionsCannotDisambiguateType
                                                    |> ImArray.iter (fun tTy ->
                                                        let mutable allParsAreSame = true
                                                        (func.Parameters, func2.Parameters)
                                                        ||> ImArray.iter2 (fun par1 par2 ->
                                                            if not(areTypesEqual par1.Type par2.Type) then
                                                                if not(areTypesEqualWithDescription tTy par1.Type par2.Type) then
                                                                    allParsAreSame <- false                                                          
                                                        )

                                                        if not(areTypesEqual func.ReturnType func2.ReturnType) then
                                                            if not(areTypesEqualWithDescription tTy func.ReturnType func2.ReturnType) then
                                                                allParsAreSame <- false 

                                                        if allParsAreSame then
                                                            diagLogger.Error($"Unable to disambiguate types on function '{printValue benv func}'.", 10, syntaxInfo.Syntax)
                                                    )
                                            )
                                        )
                                    )
                    | _ ->
                        ()
                | _ ->
                    ()
            )
            diagLogger.GetDiagnostics()

    /// Runs the given analyzers against function implementations.
    /// If the bound model has errors, then this immediately returns an empty result.
    /// Analyzers will be run in parallel.
    member this.AnalyzeFunctionImplementations(analyzers: _ imarray, ct: CancellationToken) =
        let diagLogger = OlyDiagnosticLogger.Create()
        let boundTree = this.GetBoundTree(ct)
        let diags = this.SyntaxTree.GetDiagnostics(ct).AddRange(this.GetDiagnostics(ct))
        if diags |> ImArray.exists (fun x -> x.IsError) then
            ImArray.empty
        else
            analyzers
            |> ImArray.Parallel.iter (fun analyzer ->
                boundTree.ForEachForTooling(function
                    | :? BoundExpression as expr ->
                        match expr with
                        | BoundExpression.MemberDefinition(syntaxInfo, binding) when binding.Info.Value.IsFunction ->
                            match binding with
                            | BoundBinding.Implementation(_, _, rhsExpr) ->
                                match syntaxInfo.TrySyntaxAndEnvironment with
                                | Some(syntaxNode, benv) ->
                                    let valueSymbol = OlyValueSymbol(this, benv, syntaxNode, binding.Info.Value)
                                    analyzer diagLogger valueSymbol (convert this rhsExpr ct) ct
                                | _ ->
                                    ()
                            | _ ->
                                ()
                        | _ ->
                            ()
                    | _ ->
                        ()
                )
            )
            diagLogger.GetDiagnostics()
            
[<RequireQualifiedAccess>]
type OlyCallKind =
    | Concrete
    | Virtual

module Patterns =

    let (|Sequential|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Sequential(_, expr1, expr2, _), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            Some(convert boundModel expr1 ct, convert boundModel expr2 ct)
        | _ ->
            None

    let private (|LocalValueDefinition|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.MemberDefinition(_, binding), boundModel, ct) when binding.Info.Value.IsLocal ->
            ct.ThrowIfCancellationRequested()
            let syntaxInfo = binding.SyntaxInfo
            match binding with
            | BoundBinding.Implementation(_, bindingInfo, rhsExpr) ->
                match bindingInfo with
                | BindingFunction(func=func)
                | BindingPattern(_, func) when not func.IsFunctionGroup ->
                    match syntaxInfo.TrySyntaxAndEnvironment with
                    | Some(syntax, benv) ->
                        let syntax =
                            match syntax.TryGetBindingDeclaration() with
                            | ValueSome syntax -> syntax :> OlySyntaxNode
                            | _ -> syntax
                        let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, func)
                        Some(valueSymbol, convert boundModel rhsExpr ct)
                    | _ ->
                        failwith "Should have user syntax."
                | BindingField(field=field) ->
                    match syntaxInfo.TrySyntaxAndEnvironment with
                    | Some(syntax, benv) ->
                        let syntax =
                            match syntax.TryGetBindingDeclaration() with
                            | ValueSome syntax -> syntax :> OlySyntaxNode
                            | _ -> syntax
                        let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, field)
                        Some(valueSymbol, convert boundModel rhsExpr ct)
                    | _ ->
                        failwith "Should have user syntax."
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None

    let (|Let|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Sequential(
            LocalValueDefinition(value, rhsExpr),
            bodyExpr
          ) ->
            Some(value, rhsExpr, bodyExpr)
        | Expression(BoundExpression.Let(syntaxInfo, binding, rhsExpr, bodyExpr), boundModel, ct) ->
            match binding with
            | BindingLocalFunction(func=func) when not func.IsFunctionGroup ->
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntax, benv) ->
                    let syntax =
                        match syntax.TryGetBindingDeclaration() with
                        | ValueSome syntax -> syntax :> OlySyntaxNode
                        | _ -> syntax
                    let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, func)
                    Some(valueSymbol, convert boundModel rhsExpr ct, convert boundModel bodyExpr ct)
                | _ ->
                    failwith "Should have user syntax."
            | BindingLocal(value=value) ->
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntax, benv) ->
                    let syntax =
                        match syntax.TryGetBindingDeclaration() with
                        | ValueSome syntax -> syntax :> OlySyntaxNode
                        | _ -> syntax
                    let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, value)
                    Some(valueSymbol, convert boundModel rhsExpr ct, convert boundModel bodyExpr ct)
                | _ ->
                    failwith "Should have user syntax."
            | _ ->
                None
        | _ ->
            None

    let (|Call|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Call(syntaxInfo=syntaxInfo;receiverOpt=receiverExprOpt;args=argExprs;value=value;flags=flags), boundModel, ct)
                when value.TryWellKnownFunction = ValueNone ->
            ct.ThrowIfCancellationRequested()
            let benv =
                match syntaxInfo.TryEnvironment with
                | Some(benv) ->
                    benv
                | _ ->
                    failwith "Should have user syntax."

            let callKind =
                if flags.HasFlag(CallFlags.Virtual) then
                    OlyCallKind.Virtual
                else
                    OlyCallKind.Concrete
            let argExprs =
                match receiverExprOpt with
                | Some receiverExpr ->
                    argExprs
                    |> ImArray.prependOne receiverExpr
                | _ ->
                    argExprs
            let argTExprs =
                argExprs
                |> ImArray.map (fun x -> convert boundModel x ct)
            let syntax = syntaxInfo.SyntaxNameOrDefault
            let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, value)
            // TODO: Add witnesses
            Some(callKind, valueSymbol, argTExprs)

        | _ ->
            None

    let (|ConstantInt32|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Literal(syntaxInfo, literal), _, ct) ->
            ct.ThrowIfCancellationRequested()
            if syntaxInfo.IsGenerated then
                failwith "Should have user syntax."
            match literal with
            | BoundLiteral.Constant(ConstantSymbol.Int32 value) ->
                Some(value)
            | BoundLiteral.NumberInference(lazyLiteral, _) ->
                match lazyLiteral.Value with
                | Ok(BoundLiteral.Constant(ConstantSymbol.Int32 value)) ->
                    Some(value)
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None

    let (|Value|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Value(syntaxInfo, value), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            let syntax, benv =
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntaxNode, benv) ->
                    syntaxNode, benv
                | _ ->
                    failwith "Should have user syntax."

            let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, value)
            Some(valueSymbol)
        | _ ->
            None

    let (|SetValue|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.SetValue(syntaxInfo, value, rhsExpr), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            let benv =
                match syntaxInfo.TryEnvironment with
                | Some(benv) ->
                    benv
                | _ ->
                    failwith "Should have user syntax."

            let valueSymbol = OlyValueSymbol(boundModel, benv, syntaxInfo.SyntaxNameOrDefault, value)
            Some(valueSymbol, convert boundModel rhsExpr ct)
        | _ ->
            None

    let (|SetContentsOfAddress|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.SetContentsOfAddress(_, lhsExpr, rhsExpr), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            Some(convert boundModel lhsExpr ct, convert boundModel rhsExpr ct)
        | _ ->
            None

    let (|InstanceField|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.GetField(syntaxInfo, receiver, field), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            let benv =
                match syntaxInfo.TryEnvironment with
                | Some(benv) ->
                    benv
                | _ ->
                    failwith "Should have user syntax."

            let valueSymbol = OlyValueSymbol(boundModel, benv, syntaxInfo.SyntaxNameOrDefault, field)
            Some(convert boundModel receiver ct, valueSymbol)
        | _ ->
            None

    let (|SetInstanceField|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.SetField(syntaxInfo, receiver, field, rhsExpr), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            let benv =
                match syntaxInfo.TryEnvironment with
                | Some(benv) ->
                    benv
                | _ ->
                    failwith "Should have user syntax."

            let valueSymbol = OlyValueSymbol(boundModel, benv, syntaxInfo.SyntaxNameOrDefault, field)
            Some(convert boundModel receiver ct, valueSymbol, convert boundModel rhsExpr ct)
        | _ ->
            None

    let (|If|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.IfElse(_, conditionExpr, targetExpr, BoundExpression.None _, _), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            Some(convert boundModel conditionExpr ct, convert boundModel targetExpr ct)
        | _ ->
            None

    let (|IfElse|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.IfElse(_, conditionExpr, trueTargetExpr, falseTargetExpr, _), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            Some(convert boundModel conditionExpr ct, convert boundModel trueTargetExpr ct, convert boundModel falseTargetExpr ct)
        | _ ->
            None

    let (|Match|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Match(_, _, matchInputExprs, matchClauses, _), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            Some(matchInputExprs |> ImArray.map (fun x -> convert boundModel x ct), 
                 matchClauses |> ImArray.map (fun x -> MatchClause(x, boundModel, ct))  
            )
        | _ ->
            None

    let (|MatchCases|_|) (tmatchPat: OlyAnalysisMatchPattern) =
        match tmatchPat with
        | MatchPattern(matchPat, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            match matchPat with
            | BoundMatchPattern.Cases(_, casePats) ->
                Some(casePats |> ImArray.map (fun x -> Pattern(x, boundModel, ct)))
            | _ ->
                None

    let (|MatchOr|_|) (tmatchPat: OlyAnalysisMatchPattern) =
        match tmatchPat with
        | MatchPattern(matchPat, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            match matchPat with
            | BoundMatchPattern.Or(_, lhsPat, rhsPat) ->
                Some(MatchPattern(lhsPat, boundModel, ct), MatchPattern(rhsPat, boundModel, ct))
            | _ ->
                None

    let (|MatchClause|) (tmatchClause: OlyAnalysisMatchClause) =
        match tmatchClause with
        | OlyAnalysisMatchClause.MatchClause(matchClause, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            match matchClause with
            | BoundMatchClause.MatchClause(_, matchPat, guardExprOpt, targetExpr) ->
                (MatchPattern(matchPat, boundModel, ct), guardExprOpt |> Option.map (fun x -> convert boundModel x ct), convert boundModel targetExpr ct)

    let (|PatternConstant|_|) (tPat: OlyAnalysisPattern) =
        match tPat with
        | OlyAnalysisPattern.Pattern(casePat, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            match casePat with
            | BoundCasePattern.Literal(syntaxInfo, literal) ->
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntaxNode, benv) ->
                    Some(OlyConstantSymbol(boundModel, benv, syntaxNode, literal))
                | _ ->
                    OlyAssert.Fail("Expected user syntax.")
            | _ ->
                None

    let (|PatternLocal|_|) (tPat: OlyAnalysisPattern) =
        match tPat with
        | OlyAnalysisPattern.Pattern(casePat, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            match casePat with
            | BoundCasePattern.Local(syntaxInfo, local) ->
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntaxNode, benv) ->
                    Some(OlyValueSymbol(boundModel, benv, syntaxNode, local))
                | _ ->
                    OlyAssert.Fail("Expected user syntax.")
            | _ ->
                None

    let (|PatternFieldConstant|_|) (tPat: OlyAnalysisPattern) =
        match tPat with
        | OlyAnalysisPattern.Pattern(casePat, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            match casePat with
            | BoundCasePattern.FieldConstant(syntaxInfo, field) ->
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntaxNode, benv) ->
                    Some(OlyValueSymbol(boundModel, benv, syntaxNode, field))
                | _ ->
                    OlyAssert.Fail("Expected user syntax.")
            | _ ->
                None

    let (|PatternFunction|_|) (tPat: OlyAnalysisPattern) =
        match tPat with
        | OlyAnalysisPattern.Pattern(casePat, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            match casePat with
            | BoundCasePattern.Function(syntaxInfo, pat, _, casePatArgs) ->
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntaxNode, benv) ->
                    Some(OlyValueSymbol(boundModel, benv, syntaxNode, pat), casePatArgs |> ImArray.map (fun x -> OlyAnalysisPattern.Pattern(x, boundModel, ct)))
                | _ ->
                    OlyAssert.Fail("Expected user syntax.")
            | _ ->
                None

    let (|PatternTuple|_|) (tPat: OlyAnalysisPattern) =
        match tPat with
        | OlyAnalysisPattern.Pattern(casePat, boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            match casePat with
            | BoundCasePattern.Tuple(_, casePatArgs) ->
                Some(casePatArgs |> ImArray.map (fun x -> OlyAnalysisPattern.Pattern(x, boundModel, ct)))
            | _ ->
                None

    let (|PatternDiscard|_|) (tPat: OlyAnalysisPattern) =
        match tPat with
        | OlyAnalysisPattern.Pattern(casePat, _, ct) ->
            ct.ThrowIfCancellationRequested()
            match casePat with
            | BoundCasePattern.Discard _ -> 
                Some()
            | _ ->
                None

    let (|Unit|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Unit _, _, ct) ->
            ct.ThrowIfCancellationRequested()
            Some()
        | _ ->
            None

    let (|Lambda|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Lambda(syntaxInfo, _, tyPars, pars, lazyBodyExpr, _, _, _), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            let bodyExpr = lazyBodyExpr.Expression

            let syntaxNode, benv =
                match syntaxInfo.TrySyntaxAndEnvironment with
                | Some(syntaxNode, benv) ->
                    syntaxNode, benv
                | _ ->
                    match bodyExpr.TryEnvironment, bodyExpr.Syntax.Parent with
                    | Some benv, syntaxNode when not(isNull(syntaxNode)) ->
                        syntaxNode, benv
                    | _ ->
                        bodyExpr.Syntax, boundModel.GetBoundTree(ct).RootEnvironment

            let syntaxTyPars, syntaxPars =
                match syntaxNode with
                | :? OlySyntaxExpression as syntaxExpr ->
                    match syntaxExpr with
                    | OlySyntaxExpression.Lambda(_, syntaxPars, _, _) ->
                        ImArray.init tyPars.Length (fun _ -> syntaxNode),
                        syntaxPars.Values 
                        |> ImArray.map (fun x -> 
                            match x with 
                            | OlySyntaxParameter.Identifier(_, _, syntaxIdent)
                            | OlySyntaxParameter.IdentifierWithTypeAnnotation(_, _, syntaxIdent, _, _) ->
                                syntaxIdent :> OlySyntaxNode
                            | _ ->
                                x :> OlySyntaxNode
                        )
                    | _ ->
                        ImArray.init tyPars.Length (fun _ -> syntaxNode),
                        ImArray.init pars.Length (fun _ -> syntaxNode)
                | :? OlySyntaxBinding as syntaxBinding ->
                    match syntaxBinding with
                    | OlySyntaxBinding.Implementation(OlySyntaxBindingDeclaration.Function(_, syntaxTyPars, syntaxPars, _, _), _, _) ->
                        syntaxTyPars.Values 
                        |> ImArray.map (fun x -> x :> OlySyntaxNode), 
                        syntaxPars.Values 
                        |> ImArray.map (fun x -> 
                            match x with 
                            | OlySyntaxParameter.Identifier(_, _, syntaxIdent)
                            | OlySyntaxParameter.IdentifierWithTypeAnnotation(_, _, syntaxIdent, _, _) ->
                                syntaxIdent :> OlySyntaxNode
                            | _ ->
                                x :> OlySyntaxNode
                        )
                    | _ ->
                        ImArray.init tyPars.Length (fun _ -> syntaxNode),
                        ImArray.init pars.Length (fun _ -> syntaxNode)
                | _ ->
                    ImArray.init tyPars.Length (fun _ -> syntaxNode),
                    ImArray.init pars.Length (fun _ -> syntaxNode)

            let bodyTExpr = convert boundModel bodyExpr ct
            let tyParSymbols =
                (tyPars, syntaxTyPars)
                ||> ImArray.map2 (fun tyPar syntaxTyPar ->
                    OlyTypeSymbol(boundModel, benv, syntaxTyPar, tyPar.AsType)
                )
            let parSymbols =
                (pars, syntaxPars)
                ||> ImArray.map2 (fun par syntaxPar ->
                    OlyValueSymbol(boundModel, benv, syntaxPar, par)
                ) 
            Some(tyParSymbols, parSymbols, bodyTExpr)
        | _ ->
            None

    // Intrinsics
    let (|AddressOf|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Call(receiverOpt=None;args=argExprs;value=value), boundModel, ct)
                when argExprs.Length = 1 ->
            ct.ThrowIfCancellationRequested()

            if value.IsAddressOf then
                Some(convert boundModel argExprs[0] ct)
            else
                None
        | _ ->
            None

    let (|FromAddress|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Call(receiverOpt=None;args=argExprs;value=value), boundModel, ct)
                when value.TryWellKnownFunction = ValueSome WellKnownFunction.FromAddress && argExprs.Length = 1 ->
            ct.ThrowIfCancellationRequested()
            Some(convert boundModel argExprs[0] ct)
        | _ ->
            None

    let (|Equals|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Call(receiverOpt=None;args=argExprs;value=value), boundModel, ct)
                when value.TryWellKnownFunction = ValueSome WellKnownFunction.Equal && argExprs.Length = 2 ->
            ct.ThrowIfCancellationRequested()
            Some(convert boundModel argExprs[0] ct, convert boundModel argExprs[1] ct)
        | _ ->
            None

