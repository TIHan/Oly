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

let private cleanExpr (expr: BoundExpression) =
    match expr with
    | BoundExpression.MemberDefinition(syntaxInfo, binding) when binding.Info.Value.IsLocal ->
        BoundExpression.CreateSequential(expr.Syntax.Tree, [expr;BoundExpression.None(syntaxInfo)])
    | _ ->
        expr

let rec private stripExpr (expr: BoundExpression) =
    let newExpr = expr.Strip()
    match newExpr with
    | BoundExpression.Sequential(syntaxInfo, expr1, expr2) as expr ->
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
                    newExpr2
                )
    | _ ->
        newExpr

let rec private convert (boundModel: OlyBoundModel) (expr: BoundExpression) ct : OlyAnalysisExpression =
    Expression(stripExpr expr, boundModel, ct)

type OlyBoundModel with

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

            // We run through these lowering phases to clean-up and normalize the tree
            // to make it easier to write analyzers.
            //
            // As an example, trying to do flow analysis on a Match expression would be painful -
            // which is why we lower it to IfElse expressions, then later clean the generated expressions
            // in the optimizer.
            //
            // It is redudant that we have to do these lowering phases twice per-file, one for ILGen
            // and one for analyzers. But since they would happen in parallel, it is fine for now. We could
            // do lowering only once for the phases that do not produce different results in
            // Debug or Release(non-debuggable) compilations, such as PatternMatchCompilation and CommonLowering.
            //
            // It looks like the only reason to run the optimizer is to clean up the 'tmps' and True/False literals
            // produced by PatternMatchCompilation. Is there a way to simply just make them cleaner within it instead
            // of the optimizer? If we could, then we would not have to run the optimizer here.
            let boundTree =
                boundTree
                |> Lowering.PatternMatchCompilation.Lower ct
                |> Lowering.CommonLowering.Lower ct
                |> Lowering.Optimizer.Lower ct { LocalValueElimination = false; BranchElimination = false }

            analyzers
            |> ImArray.Parallel.iter (fun analyzer ->
                boundTree.ForEachForTooling(function
                    | :? BoundExpression as expr ->
                        match expr with
                        | BoundExpression.MemberDefinition(BoundSyntaxInfo.User(syntaxNode, benv), binding) when binding.Info.Value.IsFunction ->
                            match binding with
                            | BoundBinding.Implementation(_, _, rhsExpr) ->
                                let valueSymbol = OlyValueSymbol(this, benv, syntaxNode, binding.Info.Value)
                                analyzer diagLogger valueSymbol (convert this rhsExpr ct) ct
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
        | Expression(BoundExpression.Sequential(_, expr1, expr2), boundModel, ct) ->
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
                    match syntaxInfo with
                    | BoundSyntaxInfo.User(syntax, benv) ->
                        let syntax =
                            match syntax.TryGetBindingDeclaration() with
                            | ValueSome syntax -> syntax :> OlySyntaxNode
                            | _ -> syntax
                        let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, func)
                        Some(valueSymbol, convert boundModel rhsExpr ct)
                    | BoundSyntaxInfo.Generated _ ->
                        failwith "Should have user syntax."
                | BindingField(field=field) ->
                    match syntaxInfo with
                    | BoundSyntaxInfo.User(syntax, benv) ->
                        let syntax =
                            match syntax.TryGetBindingDeclaration() with
                            | ValueSome syntax -> syntax :> OlySyntaxNode
                            | _ -> syntax
                        let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, field)
                        Some(valueSymbol, convert boundModel rhsExpr ct)
                    | BoundSyntaxInfo.Generated _ ->
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
                match syntaxInfo with
                | BoundSyntaxInfo.User(syntax, benv) ->
                    let syntax =
                        match syntax.TryGetBindingDeclaration() with
                        | ValueSome syntax -> syntax :> OlySyntaxNode
                        | _ -> syntax
                    let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, func)
                    Some(valueSymbol, convert boundModel rhsExpr ct, convert boundModel bodyExpr ct)
                | BoundSyntaxInfo.Generated _ ->
                    failwith "Should have user syntax."
            | BindingLocal(value=value) ->
                match syntaxInfo with
                | BoundSyntaxInfo.User(syntax, benv) ->
                    let syntax =
                        match syntax.TryGetBindingDeclaration() with
                        | ValueSome syntax -> syntax :> OlySyntaxNode
                        | _ -> syntax
                    let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, value)
                    Some(valueSymbol, convert boundModel rhsExpr ct, convert boundModel bodyExpr ct)
                | BoundSyntaxInfo.Generated _ ->
                    failwith "Should have user syntax."
            | _ ->
                None
        | _ ->
            None

    let (|Call|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Call(syntaxInfo=syntaxInfo;receiverOpt=receiverExprOpt;args=argExprs;syntaxValueNameOpt=syntaxValueNameOpt;value=value;isVirtualCall=isVirtualCall), boundModel, ct)
                when value.TryWellKnownFunction = ValueNone ->
            ct.ThrowIfCancellationRequested()
            let syntaxNode, benv =
                match syntaxInfo with
                | BoundSyntaxInfo.User(syntaxNode, benv) ->
                    syntaxNode, benv
                | BoundSyntaxInfo.Generated _ ->
                    failwith "Should have user syntax."

            let callKind =
                if isVirtualCall then
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
            let syntax =
                match syntaxValueNameOpt with
                | Some syntaxName -> syntaxName :> OlySyntaxNode
                | _ -> syntaxNode
            let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, value)
            // TODO: Add witnesses
            Some(callKind, valueSymbol, argTExprs)

        | _ ->
            None

    let (|ConstantInt32|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.Literal(syntaxInfo, literal), _, ct) ->
            ct.ThrowIfCancellationRequested()
            if syntaxInfo.IsGeneratedKind then
                failwith "Should have user syntax."
            match literal with
            | BoundLiteral.Constant(ConstantSymbol.Int32 value) ->
                Some(value)
            | BoundLiteral.NumberInference(lazyLiteral, _) ->
                match lazyLiteral.Value with
                | BoundLiteral.Constant(ConstantSymbol.Int32 value) ->
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
                match syntaxInfo with
                | BoundSyntaxInfo.User(syntaxNode, benv) ->
                    syntaxNode, benv
                | BoundSyntaxInfo.Generated _ ->
                    failwith "Should have user syntax."

            let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, value)
            Some(valueSymbol)
        | _ ->
            None

    let (|SetValue|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.SetValue(syntaxInfo, syntaxNameOpt, value, rhsExpr), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            let syntaxNode, benv =
                match syntaxInfo with
                | BoundSyntaxInfo.User(syntaxNode, benv) ->
                    syntaxNode, benv
                | BoundSyntaxInfo.Generated _ ->
                    failwith "Should have user syntax."

            let syntax =
                match syntaxNameOpt with
                | Some syntaxName -> syntaxName :> OlySyntaxNode
                | _ -> syntaxNode
            let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, value)
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
        | Expression(BoundExpression.GetField(syntaxInfo, receiver, syntaxNameOpt, field), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            let syntaxNode, benv =
                match syntaxInfo with
                | BoundSyntaxInfo.User(syntaxNode, benv) ->
                    syntaxNode, benv
                | BoundSyntaxInfo.Generated _ ->
                    failwith "Should have user syntax."

            let syntax =
                match syntaxNameOpt with
                | Some syntaxName -> syntaxName :> OlySyntaxNode
                | _ -> syntaxNode
            let valueSymbol = OlyValueSymbol(boundModel, benv, syntax, field)
            Some(convert boundModel receiver ct, valueSymbol)
        | _ ->
            None

    let (|SetInstanceField|_|) (texpr: OlyAnalysisExpression) =
        match texpr with
        | Expression(BoundExpression.SetField(syntaxInfo, receiver, syntaxNameOpt, field, rhsExpr), boundModel, ct) ->
            ct.ThrowIfCancellationRequested()
            let syntaxNode, benv =
                match syntaxInfo with
                | BoundSyntaxInfo.User(syntaxNode, benv) ->
                    syntaxNode, benv
                | BoundSyntaxInfo.Generated _ ->
                    failwith "Should have user syntax."

            let syntaxNode =
                match syntaxNameOpt with
                | Some syntaxName -> syntaxName :> OlySyntaxNode
                | _ -> syntaxNode
            let valueSymbol = OlyValueSymbol(boundModel, benv, syntaxNode, field)
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
                match syntaxInfo with
                | BoundSyntaxInfo.User(syntaxNode, benv) ->
                    syntaxNode, benv
                | BoundSyntaxInfo.Generated _ ->
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

