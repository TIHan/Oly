module internal rec Oly.Compiler.Internal.BoundTree

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.SymbolQuery

[<Flags>]
type LambdaFlags =
    | None                  = 0x0000000
    | Inline                = 0x0000001
    | InlineNever           = 0x0000010
    | InlineAlways          = 0x0000101
    | Continuation          = 0x0011101
    | Static                = 0x0010000
    | Scoped                = 0x0100000

    /// Only used in lambda-lifting
    | Bound                 = 0x1000000 // TODO: Get rid of this.

[<Flags>]
type CallFlags =
    | None    = 0b0000
    | Virtual = 0b0001
    | Partial = 0b0010

type InlineArgumentSymbol with

    member this.ToLambdaFlags() =
        match this with
        | InlineArgumentSymbol.None ->
            LambdaFlags.Inline
        | InlineArgumentSymbol.Never ->
            LambdaFlags.InlineNever
        | InlineArgumentSymbol.Always ->
            LambdaFlags.InlineAlways

type IBoundNode =

    abstract Syntax : OlySyntaxNode

let boundNone (syntaxTree: OlySyntaxTree) = 
    { new IBoundNode with 
        member _.Syntax = syntaxTree.DummyNode
    }

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type BoundBinding =
    | Implementation of syntaxInfo: BoundSyntaxInfo * bindingInfo: BindingInfoSymbol * rhs: BoundExpression
    | Signature of syntaxInfo: BoundSyntaxInfo * bindingInfo: BindingInfoSymbol

    member this.SyntaxInfo =
        match this with
        | Implementation(syntaxInfo=syntaxInfo)
        | Signature(syntaxInfo=syntaxInfo) -> syntaxInfo

    member this.IsGenerated =
        this.SyntaxInfo.IsGenerated

    member this.Info =
        match this with
        | Implementation(_, bindingInfo, _)
        | Signature(_, bindingInfo) -> bindingInfo

    member this.HasExpression =
        match this with
        | Implementation _ -> true
        | _ -> false

    member this.TryExpression =
        match this with
        | Implementation(rhs=rhsExpr) -> ValueSome rhsExpr
        | _ -> ValueNone

    member this.GetValidUserSyntax() =
        match this with
        | Implementation(syntaxInfo=syntaxInfo;rhs=rhsExpr) ->
            if syntaxInfo.IsGenerated then
                rhsExpr.GetValidUserSyntax()
            else
                syntaxInfo.Syntax
        | Signature(syntaxInfo=syntaxInfo) -> syntaxInfo.Syntax

    interface IBoundNode with

        member this.Syntax =
            match this with
            | Implementation(syntaxInfo=syntaxInfo)
            | Signature(syntaxInfo=syntaxInfo) -> syntaxInfo.Syntax

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type BoundLiteral =
    | Constant of ConstantSymbol
    | NullInference of ty: TypeSymbol
    | DefaultInference of ty: TypeSymbol * isUnchecked: bool
    | NumberInference of Lazy<Result<BoundLiteral, OlyDiagnostic>> * ty: TypeSymbol
    | ConstantEnum of ConstantSymbol * enumTy: TypeSymbol
    | Error

    member this.Type =
        match this with
        | Constant(cns) -> cns.Type
        | NullInference ty -> ty
        | DefaultInference(ty, _) -> ty
        | NumberInference(ty=ty) -> ty
        | ConstantEnum(enumTy=ty) -> ty
        | Error -> TypeSymbolError

and [<Sealed>] LazyExpression (syntaxExprOpt: OlySyntaxExpression option, f: OlySyntaxExpression option -> BoundExpression) =

    let mutable f = f
    let mutable cache = ValueNone
    let mutable syntaxExprOpt = syntaxExprOpt
    
    let exprTy = mkInferenceVariableType None

    member _.Run() =
        match cache with
        | ValueSome _ -> failwith "Lazy expression already ran."
        | _ ->
            let expr : BoundExpression = f syntaxExprOpt
            f <- Unchecked.defaultof<_>
            syntaxExprOpt <- Unchecked.defaultof<_>
            cache <- ValueSome expr
            UnifyTypes Flexible exprTy expr.Type |> ignore

    member _.Expression =
        match cache with
        | ValueSome expr -> expr
        | _ -> failwith "Expression must be evaluated."

    member _.ExpressionType = exprTy

    member _.TrySyntax = syntaxExprOpt

    member _.HasExpression = cache.IsSome

    static member CreateNonLazy(syntaxExpr, f) =
        let lazyExpr = LazyExpression(syntaxExpr, f)
        lazyExpr.Run()
        lazyExpr
    
and [<Sealed>] LazyExpressionType (syntaxTree) =

    let mutable ty = Unchecked.defaultof<_>

    member val Expression = BoundExpression.None(BoundSyntaxInfo.Generated(syntaxTree)) with get, set

    member this.Type =
        match box ty with
        | null ->
            let ty0 =
                let expr = this.Expression
                match expr with
                | BoundExpression.Lambda(flags=flags;tyPars=tyPars;pars=pars;body=body) ->
                    let argTys = pars |> ImArray.map (fun x -> x.Type)
                    let kind =
                        if flags.HasFlag(LambdaFlags.Scoped) then
                            FunctionKind.Scoped
                        else
                            FunctionKind.Normal
                    TypeSymbol.CreateFunction(tyPars, argTys, body.ExpressionType, kind)
                | _ ->
                    expr.Type
            ty <- ty0
            ty
        | _ ->
            ty

and FreeLocals = System.Collections.Generic.Dictionary<int64, OlySyntaxName option * IValueSymbol>

and ReadOnlyFreeLocals = System.Collections.ObjectModel.ReadOnlyDictionary<int64, OlySyntaxName option * IValueSymbol>

and FreeVariables = System.Collections.Generic.Dictionary<int64, TypeParameterSymbol>

and ReadOnlyFreeTypeVariables = System.Collections.ObjectModel.ReadOnlyDictionary<int64, TypeParameterSymbol>

and [<RequireQualifiedAccess;NoComparison;ReferenceEquality;DebuggerDisplay("{ToDebugString()}")>] BoundSyntaxInfo =
    private
    | InternalUser of 
        syntax: OlySyntaxNode * 
        benv: BoundEnvironment

    | InternalUserWithName of 
        syntax: OlySyntaxNode *  
        benv: BoundEnvironment *
        syntaxName: OlySyntaxName *
        tyOpt: TypeSymbol option

    | InternalGenerated of syntaxTree: OlySyntaxTree // TODO: We should be allowed to pass a syntaxNode instead of the tree

    member this.TryEnvironment =
        match this with
        | InternalUser(_, benv)
        | InternalUserWithName(_, benv, _, _) -> Some benv
        | _ -> None

    member this.Syntax =
        match this with
        | InternalUser(syntax, _) 
        | InternalUserWithName(syntax, _, _, _) -> syntax
        | InternalGenerated(syntaxTree) -> syntaxTree.DummyNode

    member this.SyntaxNameOrDefault =
        match this.TrySyntaxName with
        | Some(syntaxName) -> syntaxName :> OlySyntaxNode
        | _ -> this.Syntax

    member this.TrySyntaxAndEnvironment =
        match this with
        | InternalUser(syntax, benv)
        | InternalUserWithName(syntax, benv, _, _) -> Some(syntax, benv)
        | _ -> None

    member this.TrySyntaxNameAndEnvironment =
        match this with
        | InternalUserWithName(_, benv, syntaxName, _) -> Some(syntaxName, benv)
        | _ -> None

    member this.TryType =
        match this with
        | InternalUserWithName(tyOpt=tyOpt) -> tyOpt
        | _ -> None

    member this.TrySyntaxName: OlySyntaxName option =
        match this with
        | InternalUserWithName(_, _, syntaxName, _) -> Some syntaxName
        | _ -> None

    member this.IsGenerated =
        match this with
        | InternalGenerated _ -> true
        | _ -> false

    member private this.ToDebugString() =
        if this.IsGenerated then
            "(GENERATED)"
        else
            this.Syntax.GetText(CancellationToken.None).ToString()

    override this.ToString() =
        this.ToDebugString()

    member this.ReplaceIfPossible(syntaxNode: OlySyntaxNode) =
        match this with
        | InternalUser(_, benv) ->
            InternalUser(syntaxNode, benv)
        | InternalUserWithName(_, benv, syntaxName, tyOpt) ->
            InternalUserWithName(syntaxNode, benv, syntaxName, tyOpt)
        | _ ->
            this          

    static member Generated(syntaxTree: OlySyntaxTree) =
        InternalGenerated(syntaxTree)

    static member User(syntaxNode: OlySyntaxNode, benv) =
        match syntaxNode.TryName with
        | Some(syntaxName) ->
            InternalUserWithName(syntaxNode, benv, syntaxName, None)
        | _ ->
            InternalUser(syntaxNode, benv)

    static member User(syntaxNode, benv, syntaxNameOpt, tyOpt) =
        match syntaxNameOpt with
        | Some(syntaxName) ->
            InternalUserWithName(syntaxNode, benv, syntaxName, tyOpt)
        | _ ->
            match syntaxNode.TryName with
            | Some(syntaxName) ->
                InternalUserWithName(syntaxNode, benv, syntaxName, tyOpt)
            | _ ->
                InternalUser(syntaxNode, benv)

and [<RequireQualifiedAccess;Struct;NoComparison;NoEquality;DebuggerDisplay("{ToDebugString()}")>] BoundSyntaxInfo<'T when 'T :> OlySyntaxNode and 'T : not struct> =
    private {
        inner: BoundSyntaxInfo
    }

    member this.Inner = this.inner

    member this.Syntax: 'T voption =
        match this.inner with
        | BoundSyntaxInfo.InternalGenerated _ -> ValueNone
        | BoundSyntaxInfo.InternalUser(syntax, _)
        | BoundSyntaxInfo.InternalUserWithName(syntax, _, _, _) -> ValueSome(System.Runtime.CompilerServices.Unsafe.As syntax)

    static member Generated(syntaxTree: OlySyntaxTree) =
        { inner = BoundSyntaxInfo.Generated(syntaxTree) }

    static member User(syntaxNode: 'T, benv) =
        { inner = BoundSyntaxInfo.User(syntaxNode, benv) }

and [<RequireQualifiedAccess;NoComparison;ReferenceEquality>] BoundCatchCase =
    | CatchCase of syntaxInfo: BoundSyntaxInfo * ILocalParameterSymbol * catchBodyExpr: BoundExpression

and BoundSequentialSemantic =
    | NormalSequential
    | ConstructorInitSequential

and [<RequireQualifiedAccess;NoComparison;ReferenceEquality;DebuggerDisplay("{ToDebugString()}")>] BoundExpression =
    | None of syntaxInfo: BoundSyntaxInfo
    | Error of syntaxInfo: BoundSyntaxInfo
    | ErrorWithNamespace of syntax: OlySyntaxName * benv: BoundEnvironment * namespaceEnt: INamespaceSymbol
    | ErrorWithType of syntax: OlySyntaxName * benv: BoundEnvironment * ty: TypeSymbol
    | Sequential of syntaxInfo: BoundSyntaxInfo * expr1: BoundExpression * expr2: BoundExpression * BoundSequentialSemantic
    | MemberDefinition of syntaxInfo: BoundSyntaxInfo * binding: BoundBinding

    | Call of 
        syntaxInfo: BoundSyntaxInfo * 
        receiverOpt: BoundExpression option * 
        witnessArgs: WitnessSolution imarray * 
        args: ImmutableArray<BoundExpression> *
        value: IValueSymbol * 
        flags: CallFlags

    | Value of syntaxInfo: BoundSyntaxInfo * value: IValueSymbol
    | SetValue of syntaxInfo: BoundSyntaxInfo * value: IValueSymbol * rhs: BoundExpression
    | SetContentsOfAddress of syntaxInfo: BoundSyntaxInfo * lhs: BoundExpression * rhs: BoundExpression
    | Literal of syntaxInfo: BoundSyntaxInfo * BoundLiteral
    | EntityDefinition of syntaxInfo: BoundSyntaxInfo * body: BoundExpression * ent: EntityDefinitionSymbol
    | GetField of syntaxInfo: BoundSyntaxInfo * receiver: BoundExpression * field: IFieldSymbol
    | SetField of syntaxInfo: BoundSyntaxInfo * receiver: BoundExpression * field: IFieldSymbol * rhs: BoundExpression * isCtorInit: bool

    // REVIEW: Do GetProperty and SetProperty need witnessArgs? Not really since we do not handle witnesses for type-level type parameters/arguments.
    | GetProperty of syntaxInfo: BoundSyntaxInfo * receiverOpt: BoundExpression option * prop: IPropertySymbol * isVirtual: bool
    | SetProperty of syntaxInfo: BoundSyntaxInfo * receiverOpt: BoundExpression option * prop: IPropertySymbol * rhs: BoundExpression * isVirtual: bool

    | Lambda of syntaxInfo: BoundSyntaxInfo * flags: LambdaFlags * tyPars: TypeParameterSymbol imarray * pars: ImmutableArray<ILocalParameterSymbol> * body: LazyExpression * cachedLambdaTy: LazyExpressionType * freeLocals: ReadOnlyFreeLocals voption ref * freeVars: ReadOnlyFreeTypeVariables voption ref
    | Typed of syntaxInfo: BoundSyntaxInfo * body: BoundExpression * ty: TypeSymbol
    | Unit of syntaxInfo: BoundSyntaxInfo

    | NewTuple of syntaxInfo: BoundSyntaxInfo * ImmutableArray<BoundExpression> * ty: TypeSymbol
    | NewArray of syntax: OlySyntaxExpression * benv: BoundEnvironment * ImmutableArray<BoundExpression> * ty: TypeSymbol

    | Witness of syntaxInfo: BoundSyntaxInfo * benv: BoundEnvironment * castFunc: IFunctionSymbol * bodyExpr: BoundExpression * witnessArg: TypeSymbol option ref * exprTy: TypeSymbol

    | Let of syntaxInfo: BoundSyntaxInfo * bindingInfo: LocalBindingInfoSymbol * rhsExpr: BoundExpression * bodyExpr: BoundExpression
    | IfElse of syntaxInfo: BoundSyntaxInfo * conditionExpr: BoundExpression * trueTargetExpr: BoundExpression * falseTargetExpr: BoundExpression * cachedExprTy: TypeSymbol
    | Match of syntax: OlySyntaxExpression * benv: BoundEnvironment * BoundExpression imarray * BoundMatchClause imarray * cachedExprTy: TypeSymbol
    | While of syntaxInfo: BoundSyntaxInfo * conditionExpr: BoundExpression * bodyExpr: BoundExpression

    | Try of syntaxInfo: BoundSyntaxInfo * bodyExpr: BoundExpression * catchCases: BoundCatchCase imarray * finallyBodyExprOpt: BoundExpression option

    member this.GetValidUserSyntax(): OlySyntaxNode =
        match this with
        | Witness(bodyExpr=bodyExpr) -> bodyExpr.GetValidUserSyntax()
        | Lambda _ -> this.Syntax
        | Sequential(_, e1, e2, _) ->
            let r1 = e1.GetValidUserSyntax()
            if r1.IsDummy then
                e2.GetValidUserSyntax()
            else
                r1
        | Call(syntaxInfo=syntaxInfo) -> syntaxInfo.Syntax
        | MemberDefinition(binding=binding) ->
            binding.GetValidUserSyntax()
        | GetField(syntaxInfo, receiver, _) when syntaxInfo.IsGenerated ->
            receiver.GetValidUserSyntax()
        | SetField(syntaxInfo, receiver, _, rhs, _) when syntaxInfo.IsGenerated ->
            let r1 = receiver.GetValidUserSyntax()
            if r1.IsDummy then
                rhs.GetValidUserSyntax()
            else
                r1
        // TODO: Add entity definition generated
        | _ ->
            this.Syntax

    member this.IsGenerated =
        match this with
        | None(syntaxInfo=syntaxInfo)
        | Error(syntaxInfo=syntaxInfo)
        | Literal(syntaxInfo=syntaxInfo)
        | EntityDefinition(syntaxInfo=syntaxInfo)
        | GetField(syntaxInfo=syntaxInfo)
        | SetField(syntaxInfo=syntaxInfo)
        | Call(syntaxInfo=syntaxInfo)
        | Lambda(syntaxInfo=syntaxInfo)
        | SetValue(syntaxInfo=syntaxInfo)
        | SetContentsOfAddress(syntaxInfo=syntaxInfo)
        | Let(syntaxInfo=syntaxInfo)
        | Value(syntaxInfo=syntaxInfo)
        | Sequential(syntaxInfo=syntaxInfo)
        | IfElse(syntaxInfo=syntaxInfo)
        | NewTuple(syntaxInfo=syntaxInfo)
        | MemberDefinition(syntaxInfo=syntaxInfo) -> syntaxInfo.IsGenerated
        | _ -> false

    member this.IsLambdaExpression =
        match this with
        | Lambda _ -> true
        | _ -> false

    member this.TryEnvironment: BoundEnvironment option =
        match this with
        | Match(benv=benv) -> Some benv
        | NewArray(benv=benv) -> Some benv
        | While(syntaxInfo=syntaxInfo)
        | None(syntaxInfo=syntaxInfo)
        | Unit(syntaxInfo=syntaxInfo)
        | Error(syntaxInfo=syntaxInfo)
        | Call(syntaxInfo=syntaxInfo)
        | SetValue(syntaxInfo=syntaxInfo)
        | SetContentsOfAddress(syntaxInfo=syntaxInfo)
        | Literal(syntaxInfo=syntaxInfo)
        | GetField(syntaxInfo=syntaxInfo)
        | SetField(syntaxInfo=syntaxInfo)
        | EntityDefinition(syntaxInfo=syntaxInfo)
        | Value(syntaxInfo=syntaxInfo)
        | Sequential(syntaxInfo=syntaxInfo)
        | MemberDefinition(syntaxInfo=syntaxInfo)
        | Let(syntaxInfo=syntaxInfo)
        | IfElse(syntaxInfo=syntaxInfo) 
        | GetProperty(syntaxInfo=syntaxInfo)
        | SetProperty(syntaxInfo=syntaxInfo)
        | Lambda(syntaxInfo=syntaxInfo)
        | NewTuple(syntaxInfo=syntaxInfo)
        | Typed(syntaxInfo=syntaxInfo)
        | Try(syntaxInfo=syntaxInfo) -> syntaxInfo.TryEnvironment
        | ErrorWithNamespace(benv=benv)
        | ErrorWithType(benv=benv) -> Some benv
        | Witness(benv=benv) -> Some benv

    member this.Syntax : OlySyntaxNode =
        match this with
        | While(syntaxInfo=syntaxInfo)
        | None(syntaxInfo=syntaxInfo)
        | Unit(syntaxInfo=syntaxInfo)
        | Error(syntaxInfo=syntaxInfo)
        | Call(syntaxInfo=syntaxInfo)
        | SetValue(syntaxInfo=syntaxInfo)
        | SetContentsOfAddress(syntaxInfo=syntaxInfo)
        | Value(syntaxInfo=syntaxInfo)
        | Sequential(syntaxInfo=syntaxInfo)
        | MemberDefinition(syntaxInfo=syntaxInfo)
        | Let(syntaxInfo=syntaxInfo)
        | IfElse(syntaxInfo=syntaxInfo) 
        | GetProperty(syntaxInfo=syntaxInfo)
        | SetProperty(syntaxInfo=syntaxInfo) 
        | Literal(syntaxInfo=syntaxInfo)
        | GetField(syntaxInfo=syntaxInfo)
        | SetField(syntaxInfo=syntaxInfo)
        | EntityDefinition(syntaxInfo=syntaxInfo)
        | Lambda(syntaxInfo=syntaxInfo)
        | NewTuple(syntaxInfo=syntaxInfo)
        | Typed(syntaxInfo=syntaxInfo) 
        | Try(syntaxInfo=syntaxInfo) -> syntaxInfo.Syntax
        | Match(syntax=syntax) -> syntax :> OlySyntaxNode
        | NewArray(syntax=syntax) -> syntax :> OlySyntaxNode
        | ErrorWithNamespace(syntax=syntax) -> syntax :> OlySyntaxNode
        | ErrorWithType(syntax=syntax) -> syntax :> OlySyntaxNode
        | Witness(syntaxInfo=syntaxInfo) -> syntaxInfo.Syntax

    member this.SyntaxNameOrDefault =
        match this with
        | While(syntaxInfo=syntaxInfo)
        | None(syntaxInfo=syntaxInfo)
        | Unit(syntaxInfo=syntaxInfo)
        | Error(syntaxInfo=syntaxInfo)
        | Call(syntaxInfo=syntaxInfo)
        | SetValue(syntaxInfo=syntaxInfo)
        | SetContentsOfAddress(syntaxInfo=syntaxInfo)
        | Value(syntaxInfo=syntaxInfo)
        | Sequential(syntaxInfo=syntaxInfo)
        | MemberDefinition(syntaxInfo=syntaxInfo)
        | Let(syntaxInfo=syntaxInfo)
        | IfElse(syntaxInfo=syntaxInfo) 
        | GetProperty(syntaxInfo=syntaxInfo)
        | SetProperty(syntaxInfo=syntaxInfo) 
        | Literal(syntaxInfo=syntaxInfo)
        | GetField(syntaxInfo=syntaxInfo)
        | SetField(syntaxInfo=syntaxInfo)
        | EntityDefinition(syntaxInfo=syntaxInfo)
        | Lambda(syntaxInfo=syntaxInfo)
        | NewTuple(syntaxInfo=syntaxInfo)
        | Typed(syntaxInfo=syntaxInfo) 
        | Try(syntaxInfo=syntaxInfo) -> syntaxInfo.SyntaxNameOrDefault
        | Match(syntax=syntax) -> syntax :> OlySyntaxNode
        | NewArray(syntax=syntax) -> syntax :> OlySyntaxNode
        | ErrorWithNamespace(syntax=syntax) -> syntax :> OlySyntaxNode
        | ErrorWithType(syntax=syntax) -> syntax :> OlySyntaxNode
        | Witness(syntaxInfo=syntaxInfo) -> syntaxInfo.SyntaxNameOrDefault

    member this.FirstReturnExpression =
        match this with
        | While _ -> this
        | IfElse _ -> this
        | Try _ -> this
        | Match _ -> this
        | Witness _ -> this
        | NewTuple _ 
        | NewArray _ -> this
        | Lambda _ -> this
        | Sequential(_, e, None _, _) -> e.FirstReturnExpression
        | Sequential(_, _, e, _) -> e.FirstReturnExpression
        | Let(_, _, _, bodyExpr) -> bodyExpr.FirstReturnExpression
        | Call _ -> this
        | Value _
        | SetValue _
        | SetContentsOfAddress _ -> this
        | Literal(_, _) -> this
        | MemberDefinition _ -> this
        | Typed _ -> this
        | Unit _ -> this
        | EntityDefinition _
        | GetField _
        | SetField _
        | GetProperty _
        | SetProperty _
        | None _
        | Error _
        | ErrorWithNamespace _
        | ErrorWithType _ -> this

    member this.Type: TypeSymbol =
        match this.FirstReturnExpression with
        | While _ -> TypeSymbol.Unit
        | IfElse(cachedExprTy=exprTy) -> exprTy
        | Match(cachedExprTy=exprTy) -> exprTy
        | Try(bodyExpr=bodyExpr) -> bodyExpr.Type
        | Witness(exprTy=exprTy) -> exprTy
        | NewTuple(ty=ty) -> ty
        | NewArray(ty=ty) -> ty
        | Call(value=value) ->
            let ty = value.Type
            match ty.TryFunction with
            | ValueSome(_, outputTy) -> outputTy
            | _ ->
                ty.TryEntity
                |> ValueOption.bind (fun ent -> ent.TryClosureInvoke : IFunctionSymbol voption)
                |> ValueOption.map (fun x -> x.ReturnType)
                |> ValueOption.defaultValue ty
        | Value(value=value) -> value.Type
        | Literal(_, boundLiteral) -> boundLiteral.Type
        | MemberDefinition _ -> TypeSymbol.Unit
        | GetField(field=field) -> field.Type
        | GetProperty(prop=prop) -> prop.Type
        | Typed(_, _, ty) -> ty
        | Sequential(expr2=expr2) -> expr2.Type
        | Let(bodyExpr=bodyExpr) -> bodyExpr.Type
        | SetValue _
        | SetContentsOfAddress _
        | SetField _
        | SetProperty _
        | EntityDefinition _ -> TypeSymbol.Unit
        | Unit _ -> TypeSymbolRealUnit
        | Lambda(cachedLambdaTy=cachedLambdaTy) -> cachedLambdaTy.Type
        | None _ -> TypeSymbol.Unit
        | Error _
        | ErrorWithNamespace _
        | ErrorWithType _ -> TypeSymbolError

    member private this.FlattenSequentialExpressionsImpl() =
        match this with
        | Sequential(_, e, rest, _) ->
            e.FlattenSequentialExpressionsImpl() @ rest.FlattenSequentialExpressionsImpl()
        | e ->
            [e]

    member this.FlattenSequentialExpressions() : _ imarray =
        // TODO: Optimize this as we really do not want to create a list then turn it into an array
        this.FlattenSequentialExpressionsImpl() |> ImArray.ofSeq

    member this.ForEachReturningTargetExpression(action) =
        let rec f (expr: BoundExpression) =
            match expr with
            | BoundExpression.Let(bodyExpr=bodyExpr) ->
                f bodyExpr
            | BoundExpression.Sequential(_, _, nextExpr, _) ->
                f nextExpr
            | BoundExpression.Match(_, _, _, matchClauses, _) ->
                matchClauses
                |> ImArray.iter (fun x ->
                    match x with
                    | BoundMatchClause.MatchClause(_, _, _, targetExpr) ->
                        f targetExpr
                )
            | BoundExpression.IfElse(_, _, targetExpr1, targetExpr2, _) ->
                f targetExpr1
                f targetExpr2
            | _ ->
                action expr
        f this

    member inline this.RewriteReturningTargetExpression([<InlineIfLambda>] rewrite) =
        let rec f (expr: BoundExpression) =
            match expr with
            | BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, bodyExpr) ->
                let newBodyExpr = f bodyExpr
                if newBodyExpr = bodyExpr then
                    expr
                else
                    BoundExpression.Let(syntaxInfo, bindingInfo, rhsExpr, newBodyExpr)

            | BoundExpression.Sequential(syntaxInfo, expr1, expr2, semantic) ->
                let newExpr2 = f expr2
                if newExpr2 = expr2 then
                    expr
                else
                    BoundExpression.Sequential(syntaxInfo, expr1, expr2, semantic)

            | BoundExpression.Match(syntax, benv, matchArgExprs, matchClauses, exprTy) ->
                let mutable didChange = false
                let newMatchClauses =
                    matchClauses
                    |> ImArray.map (fun x ->
                        match x with
                        | BoundMatchClause.MatchClause(syntax, matchPat, guardExprOpt, targetExpr) ->
                            let newTargetExpr = f targetExpr
                            if newTargetExpr = targetExpr then
                                x
                            else
                                didChange <- true
                                BoundMatchClause.MatchClause(syntax, matchPat, guardExprOpt, newTargetExpr)
                    )
                if didChange then
                    BoundExpression.Match(syntax, benv, matchArgExprs, newMatchClauses, exprTy)
                else
                    expr

            | BoundExpression.IfElse(syntaxInfo, conditionExpr, targetExpr1, targetExpr2, exprTy) ->
                let newTargetExpr1 = f targetExpr1
                let newTargetExpr2 = f targetExpr2
                if newTargetExpr1 = targetExpr1 && newTargetExpr2 = targetExpr2 then
                    expr
                else
                    BoundExpression.IfElse(syntaxInfo, conditionExpr, newTargetExpr1, newTargetExpr2, exprTy)

            | _ ->
                rewrite expr
        f this

    member this.GetReturningTargetExpressions() =
        let exprs = ImArray.builder()
        this.ForEachReturningTargetExpression(exprs.Add)
        exprs.ToImmutable()

    /// Get a list of instance fields from the expression.
    /// Does not traverse through Let, Match, If expression.
    /// This is used to ensure fields are set in the constructor.
    member this.GetThisSetInstanceFields() =
        let fields = ImArray.builder()
        let rec f (expr: BoundExpression) =
            match expr with
            | BoundExpression.Sequential(_, expr1, expr2, _) ->
                f expr1
                f expr2
            | BoundExpression.SetField(receiver=BoundExpression.Value(value=value);field=field) when value.IsThis && field.IsInstance ->
                fields.Add(field)
            | _ ->
                ()
        f this
        fields.ToImmutable()

    static member CreateSequential(syntaxNode, benv, expr1: BoundExpression, expr2: BoundExpression, semantic) =
        BoundExpression.Sequential(
            BoundSyntaxInfo.User(syntaxNode, benv),
            expr1,
            expr2,
            semantic
        )

    static member CreateSequential(syntaxNode, benv, exprs: _ seq, semantic) =
        BoundExpression.CreateSequential(BoundSyntaxInfo.User(syntaxNode, benv), exprs, semantic)

    static member CreateSequential(expr1: BoundExpression, expr2: BoundExpression, semantic) =
        let syntaxTree = expr1.Syntax.Tree
        if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
            failwith "Syntax trees do not match."

        BoundExpression.Sequential(
            BoundSyntaxInfo.Generated(syntaxTree),
            expr1,
            expr2,
            semantic
        )

    static member CreateSequential(expr1: BoundExpression, expr2: BoundExpression) =
        BoundExpression.CreateSequential(expr1, expr2, NormalSequential)

    static member CreateEntityDefinition(syntaxInfo, bodyExpr, ent: EntityDefinitionSymbol) =
        OlyAssert.True(ent.IsFormal)
        BoundExpression.EntityDefinition(syntaxInfo, bodyExpr, ent)

    static member CreateSequential(exprs: BoundExpression seq, expr: BoundExpression) =
        let exprs = exprs |> ImArray.ofSeq
        if exprs.IsEmpty then
            expr
        else
            let syntaxTree = expr.Syntax.Tree
            BoundExpression.CreateSequential(syntaxTree, exprs.Add(expr))

    static member CreateSequential(syntaxTree: OlySyntaxTree, exprs: _ seq) =
        BoundExpression.CreateSequential(BoundSyntaxInfo.Generated(syntaxTree), exprs)

    static member CreateSequential(syntaxTree: OlySyntaxTree, exprs: _ seq, semantic) =
        BoundExpression.CreateSequential(BoundSyntaxInfo.Generated(syntaxTree), exprs, semantic)

    static member CreateSequential(syntaxInfo: BoundSyntaxInfo, exprs: _ seq) =
        let exprs = exprs |> ImArray.ofSeq
        if exprs.IsEmpty then
            BoundExpression.None(syntaxInfo)
        elif exprs.Length = 1 then
            exprs.[0]
        else
            let rec loop i =
                let j = i + 1
                let expr1 = exprs.[i]
                if j >= exprs.Length then
                    expr1
                else
                    let expr2 = exprs.[j]
                    if j + 1 >= exprs.Length then
                        BoundExpression.Sequential(
                            syntaxInfo,
                            expr1,
                            expr2,
                            NormalSequential
                        )
                    else
                        BoundExpression.Sequential(
                            syntaxInfo,
                            expr1,
                            BoundExpression.Sequential(
                                syntaxInfo,
                                expr2,
                                loop (j + 1),
                                NormalSequential
                            ),
                            NormalSequential
                        )
            loop 0

    static member CreateSequential(syntaxInfo: BoundSyntaxInfo, exprs: _ seq, semantic) =
        let expr = BoundExpression.CreateSequential(syntaxInfo, exprs)
        match expr with
        | BoundExpression.Sequential(syntaxInfo, expr1, expr2, semantic2) ->
            if semantic2 = semantic then
                expr
            else
                BoundExpression.Sequential(syntaxInfo, expr1, expr2, semantic)
        | _ ->
            expr

    static member CreateLiteral(syntaxTree, literal) =
        BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), literal)

    static member CreateFunctionDefinition(func: FunctionSymbol, rhsExpr: BoundExpression) =
        let syntaxInfo = BoundSyntaxInfo.Generated(rhsExpr.Syntax.Tree)
        BoundExpression.MemberDefinition(
            syntaxInfo,
            BoundBinding.Implementation(
                syntaxInfo,
                BindingFunction(func),
                rhsExpr
            )
        )

    static member CreateValue(syntaxTree, value: IValueSymbol) =
        BoundExpression.Value(BoundSyntaxInfo.Generated(syntaxTree), value)

    static member CreateValue(syntaxInfo, value: IValueSymbol) =
        BoundExpression.Value(syntaxInfo, value)

    static member CreateSetValue(value: IValueSymbol, rhsExpr: BoundExpression) =
        if not value.IsMutable then
            failwith "Value must be mutable."
        BoundExpression.SetValue(BoundSyntaxInfo.Generated(rhsExpr.Syntax.Tree), value, rhsExpr)

    member this.ToDebugString() =
        let text = this.Syntax.GetText(CancellationToken.None).ToString()
        if String.IsNullOrWhiteSpace text then
            this.ToString()
        else
            text

    static member CreateLambda(syntax: OlySyntaxExpression, benv, lambdaFlags, tyPars, parValues, body) =
        let lazyExprTy = LazyExpressionType(syntax.Tree)
        let expr = BoundExpression.Lambda(BoundSyntaxInfo.User(syntax, benv), lambdaFlags, tyPars, parValues, body, lazyExprTy, ref ValueNone, ref ValueNone)
        lazyExprTy.Expression <- expr
        expr

    static member CreateLambda(syntaxTree, lambdaFlags, tyPars, parValues, body) =
        let lazyExprTy = LazyExpressionType(syntaxTree)
        let expr = BoundExpression.Lambda(BoundSyntaxInfo.Generated(syntaxTree), lambdaFlags, tyPars, parValues, body, lazyExprTy, ref ValueNone, ref ValueNone)
        lazyExprTy.Expression <- expr
        expr

    static member CreateLambda(syntaxInfo: BoundSyntaxInfo, lambdaFlags, tyPars, parValues, body) =
        let lazyExprTy = LazyExpressionType(syntaxInfo.Syntax.Tree)
        let expr = BoundExpression.Lambda(syntaxInfo, lambdaFlags, tyPars, parValues, body, lazyExprTy, ref ValueNone, ref ValueNone)
        lazyExprTy.Expression <- expr
        expr

    interface IBoundNode with

        member this.Syntax = this.Syntax

[<ReferenceEquality;NoComparison;RequireQualifiedAccess>]
type BoundCasePattern =
    | Literal of BoundSyntaxInfo * BoundLiteral
    | Local of BoundSyntaxInfo * ILocalSymbol
    | FieldConstant of BoundSyntaxInfo * IFieldSymbol
    | Function of BoundSyntaxInfo * IPatternSymbol * witnessArgs: WitnessSolution imarray * castPatArgs: BoundCasePattern imarray
    | Tuple of BoundSyntaxInfo * casePatArgs: BoundCasePattern imarray
    | Discard of BoundSyntaxInfo

    member this.Syntax: OlySyntaxNode =
        match this with
        | Literal(syntaxInfo, _)
        | Local(syntaxInfo, _)
        | FieldConstant(syntaxInfo, _)
        | Function(syntaxInfo, _, _, _)
        | Tuple(syntaxInfo, _)
        | Discard(syntaxInfo) -> syntaxInfo.Syntax

    member this.ExhaustivenessColumns =
        match this with
        | Discard _
        | Literal _
        | Local _
        | FieldConstant _ -> ImArray.createOne this
        | Function(_, pat, _, _) when pat.PatternGuardFunction.IsSome -> ImArray.createOne this
        | Function(_, _, _, casePatArgs)
        | Tuple(_, casePatArgs) ->
            casePatArgs
            |> Seq.collect (fun x ->
                x.ExhaustivenessColumns
            )
            |> ImArray.ofSeq

    member this.ExhaustivenessColumnCount =
        match this with
        | Discard _
        | Literal _
        | Local _
        | FieldConstant _ -> 1
        | Function(_, pat, _, _) when pat.PatternGuardFunction.IsSome -> 1
        | Function(_, _, _, casePatArgs)
        | Tuple(_, casePatArgs) ->
            casePatArgs
            |> Seq.sumBy (fun x -> x.ExhaustivenessColumnCount)


    interface IBoundNode with

        member this.Syntax = this.Syntax

[<ReferenceEquality;NoComparison;RequireQualifiedAccess>]
type BoundMatchPattern =
    | Cases of OlySyntaxNode * casePats: BoundCasePattern imarray
    | Or of OlySyntaxNode * lhsPattern: BoundMatchPattern * rhsPattern: BoundMatchPattern

    member this.CaseCount =
        match this with
        | Cases(_, casePats) ->
            casePats.Length
        | Or(_, lhsPattern, rhsPattern) ->
            lhsPattern.CaseCount + rhsPattern.CaseCount

    member this.Syntax: OlySyntaxNode =
        match this with
        | Cases(syntax, _) -> syntax
        | Or(syntax, _, _) -> syntax

[<ReferenceEquality;NoComparison;RequireQualifiedAccess>]
type BoundMatchClause =
    | MatchClause of OlySyntaxNode * BoundMatchPattern * guardExprOpt: BoundExpression option * targetExpr: BoundExpression

    member this.Syntax =
        match this with
        | MatchClause(syntax, _, _, _) -> syntax

    member this.MatchPattern =
        match this with
        | MatchClause(_, matchPattern, _, _) -> matchPattern

    member this.TargetType =
        match this with
        | MatchClause(targetExpr=targetExpr) -> targetExpr.Type

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type BoundRoot =
    | Namespace of syntax: OlySyntaxCompilationUnit * benv: BoundEnvironment * INamespaceSymbol * body: BoundExpression
    | Global of syntax: OlySyntaxCompilationUnit * benv: BoundEnvironment * body: BoundExpression

    member this.Syntax =
        match this with
        | Namespace(syntax=syntax)
        | Global(syntax=syntax) -> syntax :> OlySyntaxNode

    interface IBoundNode with

        member this.Syntax = this.Syntax

[<Sealed>]
type BoundDeclarationTable private (
    valueDecls: ImmutableDictionary<IValueSymbol, OlySourceLocation>, 
    entDecls: ImmutableDictionary<EntitySymbol, OlySourceLocation>, 
    tyParDecls: ImmutableDictionary<TypeParameterSymbol, OlySourceLocation>) =

    member _.ValueDeclarations = valueDecls
    member _.EntityDeclarations = entDecls
    member _.TypeParameterDeclarations = tyParDecls

    member this.SetValueDeclaration(key, value) =
        BoundDeclarationTable(
            valueDecls.SetItem(key, value),
            entDecls,
            tyParDecls
        )

    member this.SetEntityDeclaration(key, value) =
        BoundDeclarationTable(
            valueDecls,
            entDecls.SetItem(key, value),
            tyParDecls
        )

    member this.SetTypeParameterDeclaration(key, value) =
        BoundDeclarationTable(
            valueDecls,
            entDecls,
            tyParDecls.SetItem(key, value)
        )

    new() =
        BoundDeclarationTable(
            ImmutableDictionary.Create(SymbolComparers.SimilarValueSymbolComparer()), 
            ImmutableDictionary.Create(SymbolComparers.SimilarEntitySymbolComparer()),
            ImmutableDictionary.Create(SymbolComparers.TypeParameterSymbolComparer())
        )

[<Sealed>]
type BoundTree(asm: AssemblySymbol, declTable: BoundDeclarationTable, syntaxTree: OlySyntaxTree, root: BoundRoot, rootSymbol: EntitySymbol, diags: OlyDiagnostic imarray) =

    member _.Assembly = asm

    member _.SyntaxTree = syntaxTree

    member _.Root = root
    member _.TryNamespace : INamespaceSymbol voption =
        match root with
        | BoundRoot.Namespace(_, _, namespac, _) -> ValueSome namespac
        | _ -> ValueNone

    member _.RootEnvironment =
        match root with
        | BoundRoot.Namespace(benv=benv)
        | BoundRoot.Global(benv=benv) -> benv

    member _.RootSymbol = rootSymbol

    member _.DeclarationTable = declTable

    member _.UpdateRoot(root) =
        BoundTree(asm, declTable, syntaxTree, root, rootSymbol, diags)

    member _.Diagnostics = diags

    member _.AppendDiagnostics(newDiags: OlyDiagnostic imarray) =
        BoundTree(asm, declTable, syntaxTree, root, rootSymbol, diags.AddRange(newDiags))

    member _.PrependDiagnostics(newDiags: OlyDiagnostic imarray) =
        BoundTree(asm, declTable, syntaxTree, root, rootSymbol, newDiags.AddRange(diags))

[<Struct;NoEquality;NoComparison>]
type ArgumentInfo(ty: TypeSymbol, syntax: OlySyntaxNode) =

    member _.Type = ty
    member _.Syntax = syntax

let freshenType (benv: BoundEnvironment) (tyPars: TypeParameterSymbol imarray) (explicitTyInst: TypeSymbol imarray) ty =
    let cache = Dictionary<TypeParameterSymbol, TypeSymbol>(TypeParameterSymbolComparer())
    freshenTypeAux benv.TypeParameterExists benv.senv.enclosingTyInst false tyPars explicitTyInst ty cache

let rec tryExpressionValue (expression: BoundExpression) =
    match expression with
    | BoundExpression.Value(value=value) -> Some (expression.Syntax, value)
    | BoundExpression.Call(value=value) -> Some(expression.Syntax, value)
    | BoundExpression.GetField(field=field) -> Some(expression.Syntax, field :> IValueSymbol)
    | _ -> None

let invalidBoundLiteral () =
    BoundLiteral.Error

let invalidExpression syntax benv =
    BoundExpression.Error(BoundSyntaxInfo.User(syntax, benv))

let invalidArgumentList () : BoundExpression list = []

let invalidArguments () : BoundExpression list = []

let invalidBinding name =
    BindingField(invalidField name None)

let invalidLocalBinding name =
    let id = newId()
    let ty = TypeSymbolError
    let value =
        { new ILocalSymbol with
              member this.Enclosing = EnclosingSymbol.RootNamespace
              member this.Formal = this :> IValueSymbol
              member this.FunctionFlags = FunctionFlags.None
              member this.FunctionOverrides = None
              member this.Id = id
              member this.IsBase = false
              member this.IsField = false
              member this.IsFunction = false
              member this.IsFunctionGroup = false
              member this.IsPattern = false
              member this.IsProperty = false
              member this.IsThis = false
              member this.MemberFlags = MemberFlags.None
              member this.Name = name
              member this.Type = ty
              member this.TypeArguments = ImArray.empty
              member this.TypeParameters = ImArray.empty
              member this.ValueFlags = ValueFlags.Invalid
        }
    BindingLocal(value)

// *************************************************************************************************

// ** Queries

[<AutoOpen>]
module EntitySymbolExtensions =

    type EntitySymbol with

        member this.ExtendsAndImplementsForMemberOverriding =
            if this.IsTypeExtension then
                this.Implements
            elif this.IsInterface then
                this.Extends
            else
                this.Extends.AddRange(this.Implements)

let freshenValue (benv: BoundEnvironment) (value: IValueSymbol) =
    freshenValueAux benv.TypeParameterExists benv.senv.enclosingTyInst value

let createFunctionWithTypeParametersOfFunction (tyPars: TypeParameterSymbol imarray) (func: FunctionSymbol) =
    let funcTy = TypeSymbol.CreateFunction(tyPars, func.Parameters |> ImArray.map (fun x -> x.Type), func.ReturnType, FunctionKind.Normal)
    let tyArgs = tyPars |> ImArray.map (fun tyPar -> tyPar.AsType)

    OlyAssert.False(func.FunctionOverrides.IsSome)

    FunctionSymbol(
        func.Enclosing,
        func.Attributes,
        func.Name,
        funcTy,
        func.Parameters,
        tyPars,
        tyArgs,
        func.MemberFlags,
        func.FunctionFlags,
        func.Semantic,
        func.WellKnownFunction,
        None,
        func.IsMutable
    )

let createLocalDeclarationExpression (rhsExpr: BoundExpression) (bodyExprf: BoundSyntaxInfo -> IValueSymbol -> BoundExpression) =
    let local = createLocalGeneratedValue "local" rhsExpr.Type
    let syntaxInfo = BoundSyntaxInfo.Generated(rhsExpr.Syntax.Tree)
    BoundExpression.Let(
        syntaxInfo,
        BindingLocal(local),
        rhsExpr,
        bodyExprf syntaxInfo local
    ), local

let createBridgeLocalDeclarationReturnExpression (rhsExpr: BoundExpression) =
    let bridge = createLocalBridgeValue rhsExpr.Type
    let syntaxInfo = BoundSyntaxInfo.Generated(rhsExpr.Syntax.Tree)
    BoundExpression.Let(
        syntaxInfo,
        BindingLocal(bridge),
        rhsExpr,
        BoundExpression.Value(syntaxInfo, bridge)
    ), bridge

let createLocalDeclarationReturnExpression (rhsExpr: BoundExpression) =
    let local = createLocalGeneratedValue "local" rhsExpr.Type
    let syntaxInfo = BoundSyntaxInfo.Generated(rhsExpr.Syntax.Tree)
    BoundExpression.Let(
        syntaxInfo,
        BindingLocal(local),
        rhsExpr,
        BoundExpression.Value(syntaxInfo, local)
    ), local

let createMutableLocalDeclarationReturnExpression (rhsExpr: BoundExpression) =
    let local = createMutableLocalGeneratedValue "mlocal" rhsExpr.Type
    let syntaxInfo = BoundSyntaxInfo.Generated(rhsExpr.Syntax.Tree)
    BoundExpression.Let(
        syntaxInfo,
        BindingLocal(local),
        rhsExpr,
        BoundExpression.Value(syntaxInfo, local)
    ), local

type BoundExpression with

    /// Strips the expression from a typed expression.
    member this.Strip() =
        match this with
        | BoundExpression.Typed(body=bodyExpr) -> bodyExpr
        | _ -> this

[<AutoOpen>]
module BoundExpressionPatterns =

    let (|Lambda|_|) (expr: BoundExpression) =
        match expr.Strip() with
        | BoundExpression.Lambda(_, _, tyPars, parValues, body, lazyTy, _, _) ->
            Some(tyPars, parValues, body.Expression, lazyTy)
        | _ ->
            None

type ConstantSymbol with

    member this.ToLiteral() =
        BoundLiteral.Constant(this)

type BoundLiteral with

    member this.TryGetConstant() =
        match this with
        | BoundLiteral.Constant(cns) -> ValueSome cns
        | BoundLiteral.NumberInference(lazyLit, _) when lazyLit.IsValueCreated -> 
            match lazyLit.Value with
            | Ok(literal) ->
                literal.TryGetConstant()
            | _ ->
                ValueNone
        | BoundLiteral.ConstantEnum(cns, _) ->
            ValueSome cns
        | _ ->
            ValueNone

/// Holds data that tells us what the user was trying to be explicit about from syntax.
[<NoEquality;NoComparison>]
type ValueExplicitness =
    {
        IsExplicitConstant:  bool
        IsExplicitStatic: bool
        IsExplicitAbstract: bool
        IsExplicitOverrides: bool
        IsExplicitDefault: bool
        IsExplicitLet: bool
        IsExplicitMutable: bool
        IsExplicitGet: bool
        IsExplicitSet: bool
        IsExplicitPattern: bool
        IsExplicitNew: bool
        IsExplicitField: bool
    }

// --------------------------------------------------------------------

let freshWitnesses (tyPar: TypeParameterSymbol) =
    tyPar.Constraints
    |> ImArray.choose (fun x -> 
        match x.TryGetAnySubtypeOf() with
        | ValueSome constrTy ->
            match constrTy.TryEntity with
            | ValueSome ent when ent.IsInterface ->
                WitnessSolution(tyPar, ent, None)
                |> Some
            | _ ->
                None
        | _ ->
            None
    ) 

let freshWitnessesWithTypeArguments asm (tyArgs: TypeArgumentSymbol imarray) (tyPar: TypeParameterSymbol) =
    tyPar.Constraints
    |> ImArray.choose (fun constr -> 
        match constr.TryGetAnySubtypeOf() with
        | ValueSome constrTy ->
            match constrTy.TryEntity with
            | ValueSome ent when ent.IsInterface || ent.IsShape ->
                let ent = ent.Substitute(tyArgs)
                if ent.IsShape then
                    ent.Functions
                    |> ImArray.map (fun func ->
                        WitnessSolution(tyPar, ent, Some func)
                    )
                    |> Some
                else
                    WitnessSolution(tyPar, ent, None)
                    |> ImArray.createOne
                    |> Some
            | _ ->
                None 
        | _ ->
            None
    )
    |> ImArray.concat