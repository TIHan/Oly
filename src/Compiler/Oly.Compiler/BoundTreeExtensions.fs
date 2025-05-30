﻿module internal rec Oly.Compiler.Internal.BoundTreeExtensions

open System.Collections.Generic
open System.Collections.ObjectModel

open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreeVisitor
open Oly.Compiler.Internal.BoundTreeRewriter
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.SymbolQuery
open Oly.Compiler.Internal.SymbolQuery.Extensions
open System.Threading

// TODO: Rename 'intrinsic' uses to something else as we use 'intrinsic' to mean compiler 'intrinsic'.
// TODO: Rename 'extrinsic' to something else.

[<AutoOpen>]
module private Helpers =

    type Locals = System.Collections.Generic.HashSet<int64>

    type Iterator(predicate, canCache, checkInnerLambdas, freeLocals: FreeLocals, locals: Locals) =
        inherit BoundTreeVisitor(BoundTreeVisitorCore())

        let checkValue (value: IValueSymbol) =
            value.IsLocal && not value.IsStaticLocalFunction && predicate value && not (locals.Contains(value.Id))

        static member HandlePossibleLambda(predicate: IValueSymbol -> bool, canCache, checkInnerLambdas, expr, freeLocals: FreeLocals, locals: Locals) =
            match expr with
            | BoundExpression.Lambda(pars=pars;freeLocals=freeLocalsRef;body=bodyExpr) ->
                for i = 0 to pars.Length - 1 do
                    let par = pars.[i]
                    if predicate par then
                        if not (locals.Add(par.Id)) then 
                            failwith "Local already added"
                    
                let iterator = Iterator(predicate, canCache, checkInnerLambdas, freeLocals, locals)
                if not bodyExpr.HasExpression then
                    bodyExpr.Run()
                iterator.VisitExpression(bodyExpr.Expression) |> ignore

            | _ ->
                let iterator = Iterator(predicate, canCache, checkInnerLambdas, freeLocals, locals)
                iterator.VisitExpression(expr) |> ignore

        override _.VisitLocalBindingInfo(bindingInfo) =
            let value = bindingInfo.Value
            //OlyAssert.Equal(value.Formal, value)
            if not value.IsStaticLocalFunction && predicate value && not (locals.Add(value.Id)) then 
                failwithf "Local already added - name: %s id: %i" value.Name value.Id
            true            

        override this.VisitExpression(expr) =
            // TODO/REVIEW: Should we handle match expressions? They get lowered to If/Else expressions so it doesn't impact later phases.
            match expr with
            | BoundExpression.Lambda(flags=flags) ->
                if checkInnerLambdas || flags.HasFlag(LambdaFlags.Scoped) then
                    Iterator.HandlePossibleLambda((fun x -> not(locals.Contains(x.Formal.Id)) && predicate x), canCache, true, expr, freeLocals, locals)
                    

                false

            | BoundExpression.Value(syntaxInfo=syntaxInfo;value=value) ->
                if checkValue value.Formal then
                    freeLocals.[value.Formal.Id] <- (syntaxInfo.Syntax.TryName, value.Formal)
                base.VisitExpression(expr)

            // TODO: We shouldn't use the Call's syntax, we need the value syntax
            | BoundExpression.Call(syntaxInfo=syntaxInfo;value=value) ->
                let syntax = syntaxInfo.Syntax
                if checkValue value.Formal then
                    let syntaxOpt =
                        match syntaxInfo.TrySyntaxName with
                        | Some syntaxName -> Some syntaxName
                        | _ ->
                            match syntax with
                            | :? OlySyntaxExpression as syntax ->
                                match syntax with
                                | OlySyntaxExpression.Name(syntaxName) -> Some syntaxName
                                | OlySyntaxExpression.Call(OlySyntaxExpression.Name(syntaxName), _) -> Some syntaxName
                                | _ -> None
                            | _ -> None
                    freeLocals.[value.Formal.Id] <- (syntaxOpt, value.Formal)
                base.VisitExpression(expr)

            | BoundExpression.SetValue(value=value) ->
                if checkValue value.Formal then
                    freeLocals.[value.Formal.Id] <- (None, value.Formal)
                base.VisitExpression(expr)

            | E.Try(catchCases=catchCases) ->
                catchCases
                |> ImArray.iter (function
                    | BoundCatchCase.CatchCase(_, par, _) ->
                        if not (locals.Add(par.Id)) then
                            failwithf "Local already added - name: %s id: %i" par.Name par.Id
                )
                base.VisitExpression(expr)
                    
            | _ ->
                base.VisitExpression(expr)

    let computeFreeImmutableLocals (expr: BoundExpression) =
        let freeLocals = FreeLocals()
        let locals = Locals()

        Iterator.HandlePossibleLambda((fun x -> not x.IsMutable && (not x.IsFunction)), false, true, expr, freeLocals, locals)

        ReadOnlyFreeLocals(freeLocals)

    let computeFreeMutableLocals (expr: BoundExpression) =
        let freeLocals = FreeLocals()
        let locals = Locals()

        Iterator.HandlePossibleLambda((fun x -> x.IsMutable && (not x.IsFunction)), false, true, expr, freeLocals, locals)

        ReadOnlyFreeLocals(freeLocals)

    let computeFreeLocals (expr: BoundExpression) =
        let freeLocals = FreeLocals()
        let locals = Locals()

        Iterator.HandlePossibleLambda((fun x -> not x.IsFunction), false, true, expr, freeLocals, locals)

        ReadOnlyFreeLocals(freeLocals)

    let computeImmediateFreeLocals (expr: BoundExpression) =
        let freeLocals = FreeLocals()
        let locals = Locals()

        Iterator.HandlePossibleLambda((fun _ -> true), false, false, expr, freeLocals, locals)

        ReadOnlyFreeLocals(freeLocals)
    
    let getFreeInferenceVariablesFromType add (ty: TypeSymbol) =
        let rec implType ty =
            match stripTypeEquations ty with
            | TypeSymbol.InferenceVariable(_, solution) ->
                add solution.Id ty
            | TypeSymbol.HigherInferenceVariable(_, tyArgs, _, solution) ->
                for i = 0 to tyArgs.Length - 1 do
                    implType tyArgs.[i]
                add solution.Id ty
            | TypeSymbol.HigherVariable(_, tyArgs) ->
                for i = 0 to tyArgs.Length - 1 do
                    implType tyArgs.[i]
            | TypeSymbol.NativeFunctionPtr(_, inputTy, returnTy)
            | TypeSymbol.Function(inputTy, returnTy, _) ->
                implType inputTy
                implType returnTy
            | TypeSymbol.ForAll(tyPars, innerTy) ->
                // TODO: What to do with the type parameters?
                implType innerTy
            | TypeSymbol.Tuple(tyArgs, _) ->
                tyArgs |> Seq.iter implType
            | TypeSymbol.Entity(ent) ->
                for i = 0 to ent.TypeArguments.Length - 1 do
                    implType ent.TypeArguments.[i]
            | _ ->
                ()
    
        implType ty
    
    let getFreeInferenceVariables (expr: BoundExpression) =
        let inputs = ResizeArray<struct(int64 * TypeSymbol)>()
    
        let addInput id item =
            let exists =
                inputs |> Seq.exists (fun struct(id2, _) -> id = id2)
            if not exists then
                inputs.Add(struct(id, item))
    
        let implType ty = getFreeInferenceVariablesFromType addInput ty

        let handleLiteral (literal: BoundLiteral) =
            // TODO:
            ()

        let rec handlePattern (pat: BoundCasePattern) =
            match pat with
            | BoundCasePattern.Discard _ -> ()
            | BoundCasePattern.Literal(_, literal) -> handleLiteral literal
            | BoundCasePattern.Tuple(_, pats) ->
                pats |> ImArray.iter handlePattern
            | BoundCasePattern.Local(_, value) ->
                implType value.Type
            | BoundCasePattern.Function(_, func, _, pats) ->
                implType func.Type
                pats |> ImArray.iter handlePattern
            | BoundCasePattern.FieldConstant(_, field) ->
                implType field.Type

        let rec handleMatchPattern (matchPat: BoundMatchPattern) =
            match matchPat with
            | BoundMatchPattern.Cases(_, pats) ->
                pats |> ImArray.iter handlePattern
            | BoundMatchPattern.Or(_, lhsPat, rhsPat) ->
                handleMatchPattern lhsPat
                handleMatchPattern rhsPat
    
        let rec handleExpression expr =
            match expr with
            | BoundExpression.Let(rhsExpr=rhsExpr;bodyExpr=bodyExpr) ->
                handleExpression rhsExpr
                handleExpression bodyExpr

            | BoundExpression.Sequential(_, left, right, _) ->
                handleExpression left
                handleExpression right

            | BoundExpression.IfElse(_, conditionExpr, trueTargetExpr, falseTargetExpr, _) ->
                handleExpression conditionExpr
                handleExpression trueTargetExpr
                handleExpression falseTargetExpr

            | BoundExpression.Match(_, _, matchExprs, matchClauses, _) ->
                matchExprs
                |> ImArray.iter handleExpression

                matchClauses
                |> ImArray.iter (function
                    | BoundMatchClause.MatchClause(_, matchPat, conditionExprOpt, targetExpr) ->
                        handleMatchPattern matchPat
                        conditionExprOpt |> Option.iter handleExpression
                        handleExpression targetExpr
                )

            | BoundExpression.Typed(_, bodyExpr, ty) ->
                handleExpression bodyExpr
                implType ty

            | BoundExpression.Value(value=value) ->
                if value.IsLocal then
                    implType value.Type

            | BoundExpression.Literal(_, literal) ->
                handleLiteral literal

            | BoundExpression.Call(receiverOpt=receiverOpt;args=args;value=value) ->
                args |> Seq.iter (fun arg -> handleExpression arg)
                receiverOpt
                |> Option.iter (fun receiver -> handleExpression receiver)
                implType value.Type

            | BoundExpression.Lambda(body=bodyExpr;cachedLambdaTy=cachedLambdaTy) ->
                OlyAssert.True(bodyExpr.HasExpression)
                handleExpression bodyExpr.Expression
                implType cachedLambdaTy.Type

            | _ ->
                ()
    
        handleExpression expr
        inputs

[<AutoOpen>]
module private FreeVariablesHelper =

    type Variables = System.Collections.Generic.HashSet<int64>

    type Iterator(freeVars: FreeVariables, vars: Variables) =
        inherit BoundTreeVisitor(BoundTreeVisitorCore())

        let ignoredValues = Variables()

        let addTyPars (tyPars: TypeParameterSymbol imarray) =
            tyPars
            |> ImArray.iter (fun tyPar ->
                vars.Add(tyPar.Id) |> ignore
            )

        let rec visitType (ty: TypeSymbol) =
            match ty.TryTypeParameter with
            | ValueSome tyPar ->
                if not (vars.Contains tyPar.Id) then
                    freeVars.[tyPar.Id] <- tyPar
            | _ -> ()

            match stripTypeEquations ty with
            | TypeSymbol.NativeFunctionPtr(_, inputTy, returnTy)
            | TypeSymbol.Function(inputTy, returnTy, _) ->
                visitType inputTy
                visitType returnTy
            | TypeSymbol.ForAll(tyPars, innerTy) ->
                addTyPars tyPars
                visitType innerTy
            | _ ->
                if not ty.IsTypeConstructor then
                    ty.TypeArguments
                    |> ImArray.iter (fun ty -> visitType ty)

        override this.CanVisit(expr) =
            match expr with
            | BoundExpression.EntityDefinition _ 
            | BoundExpression.MemberDefinition _ -> false
            | _ -> true

        override this.VisitExpression(expr) =
            match expr with
            | BoundExpression.EntityDefinition _ 
            | BoundExpression.MemberDefinition _ -> false
            | BoundExpression.Lambda(tyPars=tyPars;pars=pars;freeVars=freeVarsRef) ->
                addTyPars tyPars

                pars
                |> ImArray.iter (fun par ->
                    visitType par.Type
                )
                base.VisitExpression(expr)

            | BoundExpression.Value(value=value) ->
                if not (ignoredValues.Contains(value.Id)) then
                    visitType value.Type
                    match value.Enclosing with
                    | EnclosingSymbol.Witness(concreteTy, _) ->
                        visitType concreteTy
                    | _ ->
                        ()
                base.VisitExpression(expr)

            | BoundExpression.Let(bindingInfo=bindingInfo) ->
                bindingInfo.Value.TypeParameters
                |> ImArray.iter (fun tyPar -> vars.Add(tyPar.Id) |> ignore)
                base.VisitExpression(expr)

            | BoundExpression.GetProperty(prop=prop)
            | BoundExpression.SetProperty(prop=prop) ->
                visitType prop.Type
                match prop.Enclosing with
                | EnclosingSymbol.Witness(concreteTy, _) ->
                    visitType concreteTy
                | EnclosingSymbol.Entity(ent) ->
                    visitType ent.AsType
                | _ ->
                    ()
                base.VisitExpression(expr)

            | BoundExpression.SetValue(value=value) ->
                if not (ignoredValues.Contains(value.Id)) then
                    visitType value.Type
                    match value.Enclosing with
                    | EnclosingSymbol.Witness(concreteTy, _) ->
                        visitType concreteTy
                    | EnclosingSymbol.Entity(ent) ->
                        visitType ent.AsType
                    | _ ->
                        ()
                base.VisitExpression(expr)

            | BoundExpression.GetField(field=field)
            | BoundExpression.SetField(field=field) ->
                visitType field.Type
                match field.Enclosing with
                | EnclosingSymbol.Entity(ent) ->
                    visitType ent.AsType
                | _ ->
                    ()
                base.VisitExpression(expr)

            | BoundExpression.Literal(_, lit) ->
                match lit with
                | BoundLiteral.Constant(constantSymbol) ->
                    match constantSymbol with
                    | ConstantSymbol.External(func) ->
                        visitType func.Type
                        match func.Enclosing with
                        | EnclosingSymbol.Witness(concreteTy, _) ->
                            visitType concreteTy
                        | EnclosingSymbol.Entity(ent) ->
                            visitType ent.AsType
                        | _ ->
                            ()
                    | _ ->
                        visitType constantSymbol.Type
                | BoundLiteral.ConstantEnum(constantSymbol, enumTy) ->
                    visitType enumTy
                    match constantSymbol with
                    | ConstantSymbol.External(func) ->
                        visitType func.Type
                        match func.Enclosing with
                        | EnclosingSymbol.Witness(concreteTy, _) ->
                            visitType concreteTy
                        | EnclosingSymbol.Entity(ent) ->
                            visitType ent.AsType
                        | _ ->
                            ()
                    | _ ->
                        visitType constantSymbol.Type
                | _ ->
                    visitType lit.Type

                base.VisitExpression(expr)

            | BoundExpression.Call(value=value) ->
                match value.Enclosing with
                | EnclosingSymbol.Witness(concreteTy, _) ->
                    visitType concreteTy
                | EnclosingSymbol.Entity(ent) ->
                    visitType ent.AsType
                | _ ->
                    ()
                value.TypeArguments
                |> ImArray.iter (fun tyArg ->
                    visitType tyArg
                )

                // We do not want to check the formal value if it has type parameters.
                if value.TypeParameters.IsEmpty then
                    visitType value.Type
                else
                    ignoredValues.Add(value.Formal.Id) |> ignore
                    if not value.IsFormal then
                        visitType value.Type

                base.VisitExpression(expr)
                    
            | _ ->
                base.VisitExpression(expr)

    let computeFreeTypeVariables (expr: BoundExpression) =
        let find expr =
            let freeVars = FreeVariables()
            let vars = Variables()

            let iterator = Iterator(freeVars, vars)
            iterator.VisitExpression(expr) |> ignore

            ReadOnlyFreeTypeVariables(freeVars)

        match expr with
        | BoundExpression.Lambda(freeVars=freeVarsRef) ->
            match freeVarsRef.contents with
            | ValueSome(freeVars) -> freeVars
            | _ ->
                let freeVars = find expr
                freeVarsRef.contents <- ValueSome(freeVars)
                freeVars
        | _ ->
            find expr

type BoundExpression with

    /// Gets all the free locals within the body of the expression.
    /// Does not include mutable or function values.
    member this.GetFreeImmutableLocals() =
        computeFreeImmutableLocals this

    /// Gets all the free mutable locals within the body of the expression.
    member this.GetFreeMutableLocals() =
        computeFreeMutableLocals this

    member this.GetFreeLocals() =
        computeFreeLocals this

    /// Gets all the free locals within the body of the expression; will not traverse inner lambda expressions hence "Immediate".
    /// Used for checking individual lambda expressions to see if they contain any locals.
    member this.GetImmediateFreeLocals() =
        computeImmediateFreeLocals this

    member this.GetFreeInferenceVariables() =
        getFreeInferenceVariables this

    member this.GetFreeTypeVariables() =
        computeFreeTypeVariables this

    /// Does not include type variables from types themselves; only function type parameters.
    /// Sorted by index.
    member this.GetLogicalFreeTypeVariables() =
        this.GetFreeTypeVariables().Values
        |> Seq.filter (fun x ->
            x.Kind <> TypeParameterKind.Type
        )
        |> Seq.sortBy (fun x -> x.Index) 
        |> ImArray.ofSeq

    member this.GetLogicalFreeAnyLocals() =
        this.GetFreeLocals()
        |> Seq.map (fun x -> x.Value |> snd)
        |> ImArray.ofSeq

type TypeSymbol with

    member this.ReplaceInferenceVariablesWithError() =
        match stripTypeEquations this with
        | TypeSymbol.InferenceVariable(tyParOpt, _)
        | TypeSymbol.HigherInferenceVariable(tyParOpt=tyParOpt) -> TypeSymbol.Error(tyParOpt, None)
        | _ ->
            let tyArgs =
                this.TypeArguments
                |> ImArray.map (fun tyArg ->
                    tyArg.ReplaceInferenceVariablesWithError()
                )
            if tyArgs.IsEmpty then
                this
            else
                applyType this.Formal tyArgs

type BoundTree with

    member this.ForEachForTooling(filter: IBoundNode -> bool, f: IBoundNode -> unit) =
        let mutable iterator = Unchecked.defaultof<BoundTreeVisitor>
        let visitor =
            { new BoundTreeVisitorCore() with

                override _.VisitPattern(pattern) =
                    if filter pattern then
                        f pattern
                        true
                    else
                        false

                override _.VisitBinding(binding) =
                    if filter binding then
                        f binding
                        true
                    elif binding.IsGenerated then
                        match binding with
                        | BoundBinding.Implementation(_, bindingInfo, rhsExpr) ->
                            iterator.VisitExpression(rhsExpr) |> ignore
                            iterator.VisitBindingInfo(bindingInfo) |> ignore
                        | BoundBinding.Signature(_, bindingInfo) ->
                            iterator.VisitBindingInfo(bindingInfo) |> ignore
                        false
                    else
                        false

                override _.VisitExpression(expr) =
                    if expr.IsGenerated then
                        match expr with
                        | BoundExpression.Call(receiverOpt=receiverOpt;args=args) ->
                            for i = 0 to args.Length - 1 do
                                iterator.VisitExpression(args.[i]) |> ignore
                            receiverOpt
                            |> Option.iter (fun receiver -> iterator.VisitExpression(receiver) |> ignore)
                        
                        | BoundExpression.Lambda(body=body) ->
                            Assert.ThrowIfNot(body.HasExpression)
                            iterator.VisitExpression(body.Expression) |> ignore
                        
                        | BoundExpression.SetField(_, receiver, _, rhs, _) ->
                            iterator.VisitExpression(receiver) |> ignore
                            iterator.VisitExpression(rhs) |> ignore

                        | BoundExpression.EntityDefinition(body=body) ->
                            iterator.VisitExpression(body) |> ignore

                        | BoundExpression.Sequential(_, expr1, expr2, _) ->
                            iterator.VisitExpression(expr1) |> ignore
                            iterator.VisitExpression(expr2) |> ignore

                        | BoundExpression.MemberDefinition(_, binding) ->
                            iterator.VisitBinding(binding) |> ignore

                        | BoundExpression.Let(bindingInfo=bindingInfo;rhsExpr=rhsExpr;bodyExpr=bodyExpr) ->
                            iterator.VisitExpression(rhsExpr) |> ignore
                            iterator.VisitLocalBindingInfo(bindingInfo) |> ignore
                            iterator.VisitExpression(bodyExpr) |> ignore
                        
                        | _ ->
                            ()
                        false
                    elif filter expr then
                        f expr
                        true
                    else
                        false

                override _.VisitRoot(root) =
                    if filter root then
                        f root
                        true
                    else
                        false
            }

        iterator <- BoundTreeVisitor(visitor)
        
        iterator.VisitRoot(this.Root) |> ignore

    member this.ForEachExpression(f: BoundExpression -> unit) =
        let mutable iterator = Unchecked.defaultof<BoundTreeVisitor>
        let visitor =
            { new BoundTreeVisitorCore() with

                override _.VisitPattern(pattern) = false

                override _.VisitBinding(binding) =
                    match binding with
                    | BoundBinding.Implementation(_, bindingInfo, rhsExpr) ->
                        iterator.VisitExpression(rhsExpr) |> ignore
                    | _ ->
                        ()
                    false

                override _.VisitExpression(expr) =
                    f expr
                    true

                override _.VisitRoot _ = true
            }

        iterator <- BoundTreeVisitor(visitor)
        
        iterator.VisitRoot(this.Root) |> ignore

    member this.ForEachForTooling(f) =
        this.ForEachForTooling((fun _ -> true), f)

    member this.RewriteExpression(rewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.Rewrite(expr) =
                    rewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        this.UpdateRoot(rewriter.RewriteRoot(this.Root))

    member this.RewriteExpressionPreorder(rewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.PreorderRewrite(expr) =
                    rewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        this.UpdateRoot(rewriter.RewriteRoot(this.Root))

    member this.RewriteExpression(preorderRewrite, postorderRewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.PreorderRewrite(expr) =
                    preorderRewrite expr

                override _.Rewrite(expr) =
                    postorderRewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        this.UpdateRoot(rewriter.RewriteRoot(this.Root))

type BoundExpression with

    member this.Rewrite(rewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.Rewrite(expr) =
                    rewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    member this.Rewrite(rewritePreorder, rewritePostorder, canRewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.Rewrite(expr) =
                    rewritePostorder expr

                override _.PreorderRewrite(expr) =
                    rewritePreorder expr

                override _.CanRewrite(expr) =
                    canRewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    member this.PreorderRewrite(preorderRewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.PreorderRewrite(expr) =
                    preorderRewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    member this.PreorderRewrite(canRewrite, preorderRewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.CanRewrite(expr) =
                    canRewrite expr

                override _.PreorderRewrite(expr) =
                    preorderRewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    member this.Rewrite(canRewrite, rewrite) =
        let mutable rewriter = Unchecked.defaultof<BoundTreeRewriter>
        let rewriterCore =
            { new BoundTreeRewriterCore() with

                override _.CanRewrite(boundNode) =
                    canRewrite boundNode

                override _.Rewrite(expr) =
                    rewrite expr
            }

        rewriter <- BoundTreeRewriter(rewriterCore)
        
        rewriter.Rewrite(this)

    static member TryImplicitCall_LoadFunction(argExpr: BoundExpression, argTy: TypeSymbol) =
        if argTy.IsAnyFunction then
            let argExprTy = argExpr.Type
            if argExprTy.IsClosure then
                let cloInvoke = argExprTy.GetClosureInvoke()
                let funcExpr =
                    BoundExpression.CreateValue(
                        argExpr.Syntax.Tree,
                        cloInvoke
                    )
                let argExpr = WellKnownExpressions.ReadOnlyAddressOfReceiverIfPossible argExprTy argExpr
                let resultExpr =
                    WellKnownExpressions.LoadFunction
                        argExpr
                        funcExpr
                        argTy
                resultExpr
            else
                match argExpr with
                | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, (BoundExpression.Value(_, value) as bodyExpr)) when bindingInfo.Value.Id = value.Id && value.IsStaticLocalFunction ->
                    let resultExpr =
                        WellKnownExpressions.LoadStaticFunction
                            bodyExpr
                            argTy
                    BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, resultExpr)
                | _ ->
                    argExpr
        else
            argExpr

    static member TryImplicitCalls_LoadFunction(argExprs: BoundExpression imarray, value: IValueSymbol) =
        let logicalTy = value.LogicalType
        let argTys = logicalTy.FunctionArgumentTypes
        (argExprs, argTys)
        ||> ImArray.map2 (fun argExpr argTy ->                                   
            BoundExpression.TryImplicitCall_LoadFunction(argExpr, argTy)
        )

type IFunctionSymbol with

    /// Fucntion is inlineable or always inlineable.
    member this.IsInline =
        (this.FunctionFlags &&& FunctionFlags.InlineMask = FunctionFlags.Inline) ||
        (this.FunctionFlags &&& FunctionFlags.InlineMask = FunctionFlags.InlineAlways)

    member this.IsInlineNever =
        this.FunctionFlags &&& FunctionFlags.InlineMask = FunctionFlags.InlineNever

    member this.IsInlineAlways =
        this.FunctionFlags &&& FunctionFlags.InlineMask = FunctionFlags.InlineAlways

// ************************************************************************

/// Associates constraints with their corresponding syntax, because
/// it is by-design that internal symbols do not reference syntax nodes directly.
/// This relies on the order in which the constraints are added to the type parameter,
/// which is not unsual for associations of other syntax and symbols.
/// There have been other approaches to this problem such as storing the associations
/// as part of bound definitions themselves, but that introduces complexity and makes
/// the tree be more error-prone when the tree is transformed in lowering.
let forEachConstraintBySyntaxConstraintClause (syntaxConstrClauses: OlySyntaxConstraintClause imarray) (tyPars: TypeParameterSymbol imarray) f =
    syntaxConstrClauses
    |> ImArray.iter (fun syntaxConstrClause ->
        match syntaxConstrClause with
        | OlySyntaxConstraintClause.ConstraintClause(_, syntaxTy, _, _) ->
            match syntaxTy with
            | OlySyntaxType.Name(syntaxName) ->
                match syntaxName with
                | OlySyntaxName.Identifier(syntaxIdent) ->
                    let ident = syntaxIdent.ValueText
                    match tyPars |> ImArray.tryFind (fun x -> x.Name = ident) with
                    | Some tyPar ->
                        let constrs = 
                            // non-second-order generic constraints
                            tyPar.Constraints 
                            |> ImArray.filter (function ConstraintSymbol.SubtypeOf(lazyTy) | ConstraintSymbol.TraitType(lazyTy) when lazyTy.Value.IsTypeConstructor -> false | _ -> true)
                        f syntaxConstrClause tyPar constrs
                    | _ ->
                        ()
                | OlySyntaxName.Generic(OlySyntaxName.Identifier(syntaxIdent), _) ->
                    let ident = syntaxIdent.ValueText
                    match tyPars |> ImArray.tryFind (fun x -> x.Name = ident) with
                    | Some tyPar ->
                        let constrs = 
                            // second-order generic constraints
                            tyPar.Constraints 
                            |> ImArray.filter (function ConstraintSymbol.SubtypeOf(lazyTy) | ConstraintSymbol.TraitType(lazyTy) when lazyTy.Value.IsTypeConstructor -> true | _ -> false)
                        f syntaxConstrClause tyPar constrs
                    | _ ->
                        ()
                | _ ->
                    ()
            | _ ->
                ()
        | _ ->
            ()
    )

// ************************************************************************

let subsumesShapeMembersWith benv rigidity queryFunc (superShapeTy: TypeSymbol) (ty: TypeSymbol) =
    OlyAssert.True(superShapeTy.IsShape)

    if ty.IsTypeConstructor then
        Seq.empty
    else

    let superFuncs = 
        superShapeTy.FindIntrinsicFunctions(benv, QueryMemberFlags.StaticOrInstance, FunctionFlags.None)
        |> Seq.map (fun superFunc ->
            if superFunc.IsInstanceConstructor then
                superFunc.MorphShapeConstructor(ty, superShapeTy).AsFunction
            else
                superFunc
        )
        |> Seq.cache

    let lookup = Dictionary<string, IFunctionSymbol imarray>()
    superFuncs
    |> Seq.iter (fun superFunc ->
        if not(lookup.ContainsKey(superFunc.Name)) then
            let nameToFind =
                if superFunc.IsConstructor then
                    ty.Name
                else
                    superFunc.Name
            lookup[superFunc.Name] <- ty.FindFunctions(benv, QueryMemberFlags.StaticOrInstance, FunctionFlags.None, queryFunc, nameToFind)
    )
   
    superFuncs
    |> Seq.map (fun superFunc ->
        let results =
            lookup[superFunc.Name]
            |> ImArray.filter (fun func ->
                if func.IsInstance = superFunc.IsInstance && (func.Name = superFunc.Name || (func.IsInstanceConstructor && superFunc.IsInstanceConstructor)) && func.TypeArguments.Length = superFunc.TypeArguments.Length && func.Parameters.Length = superFunc.Parameters.Length then
                    // TODO: This really isn't right.
                    let isInstance = func.IsInstance
                    let result =
                        (superFunc.Parameters, func.Parameters)
                        ||> ImArray.foralli2 (fun i par1 par2 ->
                            if i = 0 && isInstance then
                                // We return true here because the first parameter of an instance member function is the 'shape' entity,
                                // which cannot be unified.
                                true
                            else
                                UnifyTypes rigidity par1.Type par2.Type
                        ) &&
                        UnifyTypes rigidity superFunc.ReturnType func.ReturnType

                    if (rigidity = Rigid) && not(areFunctionTypeParameterConstraintsEqualWith Indexable superFunc.Formal.AsFunction func.Formal.AsFunction) then
                        false
                    elif not result then
                        areLogicalFunctionSignaturesEqual superFunc func
                    else
                        true
                else
                    false
            )
        (superFunc, results |> filterMostSpecificFunctions |> ImArray.ofSeq)
    )

let subsumesTypeOrShapeWith benv rigidity (superTy: TypeSymbol) (ty: TypeSymbol) =
    if superTy.IsShape then
        subsumesShapeWith benv rigidity superTy ty
    else
        subsumesTypeWith rigidity superTy ty

let subsumesTypeOrShape benv superTy ty =
    subsumesTypeOrShapeWith benv Rigid superTy ty

let subsumesShapeWith benv rigidity (superShapeTy: TypeSymbol) (ty: TypeSymbol) =
    OlyAssert.True(superShapeTy.IsShape)
    
    if ty.IsTypeConstructor then
        false
    else
        let areFuncsValid =
            subsumesShapeMembersWith benv rigidity QueryFunction.Intrinsic superShapeTy ty
            |> Seq.forall (fun (_, xs) -> 
                xs.Length = 1
            )

        areFuncsValid

let subsumesShape benv (superShapeTy: TypeSymbol) (ty: TypeSymbol) =
    subsumesShapeWith benv TypeVariableRigidity.Rigid superShapeTy ty

// Similar to subsumesTypeAndUnifyTypes - but includes the bound environment which will take into account type extensions.
let subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity (superTy: TypeSymbol) (ty: TypeSymbol) =
    if subsumesTypeOrShapeWith benv rigidity superTy ty then true
    else
        if superTy.IsTypeConstructor then
            subsumesTypeConstructorWith rigidity superTy ty
        else
            match stripTypeEquations ty, stripTypeEquations superTy with
            | TypeSymbol.ForAll(_, innerTy), superTy when not superTy.IsTypeConstructor ->
                subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity superTy innerTy
            | TypeSymbol.Variable(tyPar), superTy ->
                tyPar.Constraints
                |> Seq.exists (function
                    | ConstraintSymbol.Null
                    | ConstraintSymbol.Struct
                    | ConstraintSymbol.NotStruct 
                    | ConstraintSymbol.Unmanaged
                    | ConstraintSymbol.Blittable
                    | ConstraintSymbol.Scoped -> true
                    | ConstraintSymbol.SubtypeOf(ty) ->
                        subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity superTy ty.Value
                    | ConstraintSymbol.ConstantType(ty) ->
                        subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity superTy ty.Value
                    | ConstraintSymbol.TraitType(ty) ->
                        subsumesTypeOrShapeOrTypeConstructorAndUnifyTypesWith benv rigidity superTy ty.Value
                )
            | _ -> 
                false

let stripLiteral (literal: BoundLiteral) =
    match literal with
    | BoundLiteral.NumberInference(lazyLiteral, _) ->
        match lazyLiteral.Value with
        | Ok(literal) -> stripLiteral literal
        | _ -> literal
    | _ ->
        literal

let areLiteralsEqual (literal1: BoundLiteral) (literal2: BoundLiteral) =
    match stripLiteral literal1, stripLiteral literal2 with
    | BoundLiteral.Constant(cns1),
      BoundLiteral.Constant(cns2) -> areConstantsEqual cns1 cns2
    | BoundLiteral.ConstantEnum(cns1, ty1),
      BoundLiteral.ConstantEnum(cns2, ty2) -> areTypesEqual ty1 ty2 && areConstantsEqual cns1 cns2
    | BoundLiteral.NullInference(ty1),
      BoundLiteral.NullInference(ty2) -> areTypesEqual ty1 ty2
    | BoundLiteral.DefaultInference(ty1, isUnchecked1),
      BoundLiteral.DefaultInference(ty2, isUnchecked2) -> areTypesEqual ty1 ty2 && isUnchecked1 = isUnchecked2
    | _ -> false

let areTargetExpressionsEqual (expr1: E) (expr2: E) =
    if obj.ReferenceEquals(expr1, expr2) then true
    else
        match expr1, expr2 with
        | E.Call(_, None, witnessArgs1, argExprs1, value1, flags1),
            E.Call(_, None, witnessArgs2, argExprs2, value2, flags2) ->
            flags1 = flags2 &&
            value1.IsFunction &&
            areValueSignaturesEqual value1 value2 && 
            argExprs1.Length = argExprs2.Length &&
            witnessArgs1.IsEmpty &&
            witnessArgs2.IsEmpty &&
            (argExprs1, argExprs2)
            ||> ImArray.forall2 (fun expr1 expr2 ->
                match expr1, expr2 with
                | E.Value(value=value1), E.Value(value=value2) when value1.IsLocal && not value1.IsMutable ->
                    areValueSignaturesEqual value1 value2
                | E.Literal(_, literal1), E.Literal(_, literal2) ->
                    areLiteralsEqual literal1 literal2
                | _ ->
                    false
            )
        | E.Value(value=value1), E.Value(value=value2) when value1.IsLocal && not value1.IsMutable ->
            areValueSignaturesEqual value1 value2
        | E.Literal(_, literal1), E.Literal(_, literal2) ->
            areLiteralsEqual literal1 literal2
        | _ ->
            false