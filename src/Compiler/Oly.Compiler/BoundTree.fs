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

[<Flags>]
type LambdaFlags =
    | None                  = 0x0000
    | Inline                = 0x0001
    | Static                = 0x0010
    | TargetJump            = 0x0100

    /// Only used in lambda-lifting
    | Bound                 = 0x1000

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
        this.SyntaxInfo.IsGeneratedKind

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
            if syntaxInfo.IsGeneratedKind then
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
            let expr = f syntaxExprOpt
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
                | BoundExpression.Lambda(tyPars=tyPars;pars=pars;body=body) ->
                    let argTys = pars |> ImArray.map (fun x -> x.Type)
                    TypeSymbol.CreateFunction(tyPars, argTys, body.ExpressionType)
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
    | User of syntax: OlySyntaxNode * benv: BoundEnvironment
    | Generated of syntaxTree: OlySyntaxTree // TODO: We should be allowed to pass a syntaxNode instead of the tree

    member this.TryEnvironment =
        match this with
        | User(_, benv) -> Some benv
        | _ -> None

    member this.Syntax =
        match this with
        | User(syntax, _) -> syntax
        | Generated(syntaxTree) -> syntaxTree.DummyNode

    member this.IsGeneratedKind =
        match this with
        | Generated _ -> true
        | _ -> false

    member this.ToDebugString() =
        let text = this.Syntax.GetText(CancellationToken.None).ToString()
        if String.IsNullOrWhiteSpace text then
            this.ToString()
        else
            text

and [<RequireQualifiedAccess;NoComparison;ReferenceEquality>] BoundCatchCase =
    | CatchCase of ILocalParameterSymbol * catchBodyExpr: BoundExpression

and [<RequireQualifiedAccess;NoComparison;ReferenceEquality;DebuggerDisplay("{ToDebugString()}")>] BoundExpression =
    | None of syntaxInfo: BoundSyntaxInfo
    | Error of syntaxInfo: BoundSyntaxInfo
    | ErrorWithNamespace of syntax: OlySyntaxName * benv: BoundEnvironment * namespaceEnt: INamespaceSymbol
    | ErrorWithType of syntax: OlySyntaxName * benv: BoundEnvironment * ty: TypeSymbol
    | Sequential of syntaxInfo: BoundSyntaxInfo * expr1: BoundExpression * expr2: BoundExpression
    | MemberDefinition of syntaxInfo: BoundSyntaxInfo * binding: BoundBinding
    | Call of syntaxInfo: BoundSyntaxInfo * receiverOpt: BoundExpression option * witnessArgs: CacheValueWithArg<OlySyntaxType imarray option, WitnessSolution imarray> * args: ImmutableArray<BoundExpression> * syntaxValueNameOpt: OlySyntaxName option * value: IValueSymbol * isVirtualCall: bool
    | Value of syntaxInfo: BoundSyntaxInfo * value: IValueSymbol
    | SetValue of syntaxInfo: BoundSyntaxInfo * syntaxValueNameOpt: OlySyntaxName option * value: IValueSymbol * rhs: BoundExpression
    | SetContentsOfAddress of syntaxInfo: BoundSyntaxInfo * lhs: BoundExpression * rhs: BoundExpression
    | Literal of syntaxInfo: BoundSyntaxInfo * BoundLiteral
    | EntityDefinition of syntaxInfo: BoundSyntaxInfo * body: BoundExpression * ent: EntitySymbol
    | GetField of syntaxInfo: BoundSyntaxInfo * receiver: BoundExpression * syntaxNameOpt: OlySyntaxName option * field: IFieldSymbol
    | SetField of syntaxInfo: BoundSyntaxInfo * receiver: BoundExpression * syntaxNameOpt: OlySyntaxName option * field: IFieldSymbol * rhs: BoundExpression
    | GetProperty of syntaxInfo: BoundSyntaxInfo * receiverOpt: BoundExpression option * syntaxName: OlySyntaxName option * prop: IPropertySymbol
    | SetProperty of syntaxInfo: BoundSyntaxInfo * receiverOpt: BoundExpression option * syntaxName: OlySyntaxName option * prop: IPropertySymbol * rhs: BoundExpression
    | Lambda of syntaxInfo: BoundSyntaxInfo * flags: LambdaFlags * tyPars: TypeParameterSymbol imarray * pars: ImmutableArray<ILocalParameterSymbol> * body: LazyExpression * cachedLambdaTy: LazyExpressionType * freeLocals: ReadOnlyFreeLocals voption ref * freeVars: ReadOnlyFreeTypeVariables voption ref
    | Typed of syntaxInfo: BoundSyntaxInfo * body: BoundExpression * ty: TypeSymbol
    | Unit of syntax: OlySyntaxExpression * benv: BoundEnvironment

    | NewTuple of syntaxInfo: BoundSyntaxInfo * ImmutableArray<BoundExpression> * ty: TypeSymbol
    | NewArray of syntax: OlySyntaxExpression * benv: BoundEnvironment * ImmutableArray<BoundExpression> * ty: TypeSymbol

    | Witness of BoundExpression * witnessArg: TypeSymbol * ty: TypeSymbol

    | Let of syntaxInfo: BoundSyntaxInfo * bindingInfo: LocalBindingInfoSymbol * rhsExpr: BoundExpression * bodyExpr: BoundExpression
    | IfElse of syntaxInfo: BoundSyntaxInfo * conditionExpr: BoundExpression * trueTargetExpr: BoundExpression * falseTargetExpr: BoundExpression * cachedExprTy: TypeSymbol
    | Match of syntax: OlySyntaxExpression * benv: BoundEnvironment * BoundExpression imarray * BoundMatchClause imarray * cachedExprTy: TypeSymbol
    | While of syntaxInfo: BoundSyntaxInfo * conditionExpr: BoundExpression * bodyExpr: BoundExpression

    | Try of syntaxInfo: BoundSyntaxInfo * bodyExpr: BoundExpression * catchCases: BoundCatchCase imarray * finallyBodyExprOpt: BoundExpression option

    member this.GetValidUserSyntax(): OlySyntaxNode =
        match this with
        | Witness(expr, _, _) -> expr.GetValidUserSyntax()
        | Lambda(body=body) -> 
            if not body.HasExpression then
                body.Run()
            body.Expression.GetValidUserSyntax()
        | Sequential(_, e1, e2) ->
            let r1 = e1.GetValidUserSyntax()
            if r1.IsDummy then
                e2.GetValidUserSyntax()
            else
                r1
        | Call(syntaxInfo=syntaxInfo) -> syntaxInfo.Syntax
        | MemberDefinition(binding=binding) ->
            binding.GetValidUserSyntax()
        | GetField(BoundSyntaxInfo.Generated _, receiver, _, _) ->
            receiver.GetValidUserSyntax()
        | SetField(BoundSyntaxInfo.Generated _, receiver, _, _, rhs) ->
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
        | MemberDefinition(syntaxInfo=syntaxInfo) -> syntaxInfo.IsGeneratedKind
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
        | Unit(benv=benv) -> Some benv
        | Witness(expr, _, _) -> expr.TryEnvironment

    member this.Syntax =
        match this with
        | While(syntaxInfo=syntaxInfo)
        | None(syntaxInfo=syntaxInfo)
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
        | Unit(syntax=syntax) -> syntax :> OlySyntaxNode
        | Witness(expr, _, _) -> expr.Syntax

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
        | Sequential(_, e, None _) -> e.FirstReturnExpression
        | Sequential(_, _, e) -> e.FirstReturnExpression
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
        | Witness(_, _, ty) -> ty
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
        | EntityDefinition _
        | Unit _ -> TypeSymbol.Unit
        | Lambda(cachedLambdaTy=cachedLambdaTy) -> cachedLambdaTy.Type
        | None _ -> TypeSymbol.Unit
        | Error _
        | ErrorWithNamespace _
        | ErrorWithType _ -> TypeSymbolError

    member private this.FlattenSequentialExpressionsImpl() =
        match this with
        | Sequential(_, e, rest) ->
            e.FlattenSequentialExpressionsImpl() @ rest.FlattenSequentialExpressionsImpl()
        | e ->
            [e]

    member this.FlattenSequentialExpressions() : _ imarray =
        // TODO: Optimize this as we really do not want to create a list then turn it into an array
        this.FlattenSequentialExpressionsImpl() |> ImArray.ofSeq

    member this.GetReturningTargetExpressions() =
        let exprs = ImArray.builder()
        let rec f (expr: BoundExpression) =
            match expr with
            | BoundExpression.Sequential(_, _, nextExpr) ->
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
                exprs.Add(expr)
        f this
        exprs.ToImmutable()

    /// Get a list of instance fields from the expression.
    /// Does not traverse through Let, Match, If expression.
    /// This is used to ensure fields are set in the constructor.
    member this.GetThisSetInstanceFields() =
        let fields = ImArray.builder()
        let rec f (expr: BoundExpression) =
            match expr with
            | BoundExpression.Sequential(_, expr1, expr2) ->
                f expr1
                f expr2
            | BoundExpression.SetField(receiver=BoundExpression.Value(value=value);field=field) when value.IsThis && field.IsInstance ->
                fields.Add(field)
            | _ ->
                ()
        f this
        fields.ToImmutable()

    static member CreateSequential(expr1: BoundExpression, expr2: BoundExpression) =
        let syntaxTree = expr1.Syntax.Tree
        if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
            failwith "Syntax trees do not match."

        BoundExpression.Sequential(
            BoundSyntaxInfo.Generated(syntaxTree),
            expr1,
            expr2
        )

    static member CreateEntityDefinition(syntaxInfo, bodyExpr, ent: EntitySymbol) =
        OlyAssert.True(ent.IsFormal)
        BoundExpression.EntityDefinition(syntaxInfo, bodyExpr, ent)

    static member CreateSequential(exprs: BoundExpression seq, expr: BoundExpression) =
        let exprs = exprs |> ImArray.ofSeq
        if exprs.IsEmpty then
            expr
        else
            let syntaxTree = expr.Syntax.Tree
            BoundExpression.CreateSequential(syntaxTree, exprs.Add(expr))

    static member CreateSequential(syntaxTree, exprs: _ seq) =
        let exprs = exprs |> ImArray.ofSeq
        if exprs.IsEmpty then
            BoundExpression.None(BoundSyntaxInfo.Generated(syntaxTree))
        elif exprs.Length = 1 then
            exprs.[0]
        else
            let syntaxInfo = BoundSyntaxInfo.Generated(syntaxTree)
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
                            expr2
                        )
                    else
                        BoundExpression.Sequential(
                            syntaxInfo,
                            expr1,
                            BoundExpression.Sequential(
                                syntaxInfo,
                                expr2,
                                loop (j + 1)
                            )
                        )
            loop 0

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
        BoundExpression.SetValue(BoundSyntaxInfo.Generated(rhsExpr.Syntax.Tree), option.None, value, rhsExpr)

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
    | Literal of OlySyntaxPattern * BoundEnvironment * BoundLiteral
    | Local of OlySyntaxPattern * BoundEnvironment * ILocalSymbol
    | FieldConstant of OlySyntaxPattern * BoundEnvironment * IFieldSymbol
    | Function of OlySyntaxPattern * BoundEnvironment * IPatternSymbol * witnessArgs: WitnessSolution imarray * castPatArgs: BoundCasePattern imarray
    | Tuple of OlySyntaxPattern * casePatArgs: BoundCasePattern imarray
    | Discard of OlySyntaxNode

    member this.Syntax: OlySyntaxNode =
        match this with
        | Literal(syntax, _, _)
        | Local(syntax, _, _)
        | FieldConstant(syntax, _, _)
        | Function(syntax, _, _, _, _)
        | Tuple(syntax, _) -> syntax :> OlySyntaxNode
        | Discard(syntax) -> syntax

    interface IBoundNode with

        member this.Syntax = this.Syntax

[<ReferenceEquality;NoComparison;RequireQualifiedAccess>]
type BoundMatchPattern =
    | Cases of OlySyntaxMatchPattern * casePats: BoundCasePattern imarray
    | Or of OlySyntaxMatchPattern * lhsPattern: BoundMatchPattern * rhsPattern: BoundMatchPattern

    member this.CaseCount =
        match this with
        | Cases(_, casePats) ->
            casePats.Length
        | Or(_, lhsPattern, rhsPattern) ->
            lhsPattern.CaseCount + rhsPattern.CaseCount

    member this.Syntax =
        match this with
        | Cases(syntax, _)
        | Or(syntax, _, _) -> syntax

[<ReferenceEquality;NoComparison;RequireQualifiedAccess>]
type BoundMatchClause =
    | MatchClause of OlySyntaxMatchClause * BoundMatchPattern * guardExprOpt: BoundExpression option * targetExpr: BoundExpression

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
    entDecls: ImmutableDictionary<IEntitySymbol, OlySourceLocation>, 
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
type BoundTree(asm: AssemblySymbol, declTable: BoundDeclarationTable, syntaxTree: OlySyntaxTree, root: BoundRoot) =

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

    member _.DeclarationTable = declTable

    member _.UpdateRoot(root) =
        BoundTree(asm, declTable, syntaxTree, root)

[<Struct;NoEquality;NoComparison>]
type ArgumentInfo(ty: TypeSymbol, syntax: OlySyntaxNode) =

    member _.Type = ty
    member _.Syntax = syntax

let freshenTypeAux (benv: BoundEnvironment) (tyPars: ImmutableArray<TypeParameterSymbol>) (explicitTyArgs: TypeArgumentSymbol imarray) ty (cache: System.Collections.Generic.Dictionary<TypeParameterSymbol, TypeSymbol>) : TypeSymbol =
    let tyArgOffset = tyPars.Length - explicitTyArgs.Length
    if tyArgOffset < 0 then
        failwith "Internal error: Invalid tyArgOffset, must be greater than or equal to zero."

    let rec freshen (tys: System.Collections.Generic.Dictionary<TypeParameterSymbol, TypeSymbol>) (explicitTyArgs: TypeArgumentSymbol imarray) ty =

        match benv.TypeParameterExists ty with
        | true -> ty
        | _ ->

        match stripTypeEquations ty with
        | TypeSymbol.Function(argTys, returnTy) ->
            TypeSymbol.Function(
                argTys |> ImArray.map (fun x -> freshen tys explicitTyArgs x),
                freshen tys explicitTyArgs returnTy
            )

        | TypeSymbol.ForAll(tyPars, innerTy) ->
            tyPars
            |> ImArray.iter (fun tyPar ->
                tys[tyPar] <- tyPar.AsType
            )
            TypeSymbol.ForAll(
                tyPars,
                freshen tys explicitTyArgs innerTy
            )

        | TypeSymbol.Variable(tyPar) ->   
            match tys.TryGetValue tyPar with
            | true, inferenceTy -> inferenceTy
            | _ ->
                let ty = 
                    match explicitTyArgs |> Seq.tryItem (tyPar.Index - tyArgOffset) with
                    | Some ty -> ty
                    | _ -> mkInferenceVariableType (Some tyPar)
                tys.Add(tyPar, ty)
                ty

        | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
            let inferenceTy =
                match tys.TryGetValue tyPar with
                | true, inferenceTy -> inferenceTy
                | _ ->
                    let ty = 
                        match explicitTyArgs |> Seq.tryItem (tyPar.Index - tyArgOffset) with
                        | Some ty -> ty
                        | _ -> mkInferenceVariableType (Some tyPar)
                    tys.Add(tyPar, ty)
                    ty
            applyType inferenceTy (tyArgs |> ImArray.map (freshen tys explicitTyArgs))

        | TypeSymbol.Tuple(tyArgs, names) ->
            TypeSymbol.Tuple(tyArgs |> ImArray.map (fun x -> freshen tys explicitTyArgs x), names)

        | TypeSymbol.Array(elementTy, rank, kind) ->
            TypeSymbol.Array(freshen tys explicitTyArgs elementTy, rank, kind)

        | TypeSymbol.Entity(ent) when not (ent.IsTypeConstructor) ->
            let enclosingTyInst =
                match benv.senv.enclosingTyInst.TryGetValue ent.Formal.Id with
                | true, enclosingTyInst -> enclosingTyInst
                | _ -> ImArray.empty
            let tyArgs =
                ent.TypeArguments
                |> Seq.skip enclosingTyInst.Length
                |> Seq.map (freshen tys explicitTyArgs)
            TypeSymbol.Entity(applyEntity (enclosingTyInst.AddRange(tyArgs)) ent.Formal)

        | TypeSymbol.ByRef(innerTy, kind) ->
            TypeSymbol.CreateByRef(freshen tys explicitTyArgs innerTy, kind)

        | _ ->
            ty
        
    // We do this specifically for inference variables as we want to maintain the type parameter.
    match ty with
    | TypeSymbol.InferenceVariable(Some tyPar, varSolution) when varSolution.HasSolution ->
        mkSolvedInferenceVariableType tyPar (freshen cache explicitTyArgs varSolution.Solution.Value)
    | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, _, varSolution) when varSolution.HasSolution ->
        let newTyArgs = tyArgs |> ImArray.map (fun tyArg -> freshen cache explicitTyArgs tyArg)
        mkSolvedHigherInferenceVariableType tyPar newTyArgs (freshen cache explicitTyArgs varSolution.Solution.Value)
    | _ ->
        freshen cache explicitTyArgs ty

let freshenType benv (tyPars: TypeParameterSymbol imarray) (explicitTyInst: TypeSymbol imarray) ty =
    let cache = Dictionary<TypeParameterSymbol, TypeSymbol>(TypeParameterSymbolComparer())
    freshenTypeAux benv tyPars explicitTyInst ty cache

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

type QueryMemberFlags =
    | StaticOrInstance =          0x0000
    | Static =                    0x0001
    | Instance =                  0x0010
    | Overridable =               0x0100

    /// This will by-pass any accessor logic.
    | InstanceFunctionOverrides = 0x1010
    // TODO: Add StaticInstanceFunctionOverrides

[<AutoOpen>]
module EntitySymbolExtensions =

    type IEntitySymbol with

        /// If extends is empty, it will include built-in base types.
        /// This is mainly used for member lookups.
        member this.ExtendsForMemberLookup =
            let extends = this.Extends

            // TODO: There are more cases to handle. What if the entity is a base struct?
            //       Then we would need to include a base object as its extends.
            if extends.IsEmpty && not(this.IsIntrinsic) then
                if this.IsEnum then
                    ImArray.createOne TypeSymbol.BaseStructEnum
                elif this.IsAnyStruct then
                    ImArray.createOne TypeSymbol.BaseStruct
                elif this.IsAttribute then
                    ImArray.createOne TypeSymbol.BaseAttribute
                elif this.IsClass || this.IsInterface then
                    ImArray.createOne TypeSymbol.BaseObject
                else
                    extends
            else
                extends

        member this.ExtendsAndImplementsForMemberOverriding =
            if this.IsTypeExtension then
                this.Implements
            elif this.IsInterface then
                this.Extends
            else
                this.ExtendsForMemberLookup.AddRange(this.Implements)

let findIntrinsicTypeIfPossible (benv: BoundEnvironment) (ty: TypeSymbol) =
    match benv.TryFindIntrinsicTypeByAliasType(ty) with
    | ValueSome intrinsicTy ->
        match benv.TryFindEntityByIntrinsicType(intrinsicTy) with
        | ValueSome ent -> ent.AsType
        | _ -> ty
    | _ ->
        match benv.TryFindEntityByIntrinsicType(ty) with
        | ValueSome ent -> ent.AsType
        | _ -> ty

let findIntrinsicAndExtrinsicInheritsAndImplementsOfType (benv: BoundEnvironment) (ty: TypeSymbol) =
    match stripTypeEquations ty with
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.HigherVariable(tyPar, _)
    | TypeSymbol.InferenceVariable(Some tyPar, _)
    | TypeSymbol.HigherInferenceVariable(Some tyPar, _, _, _) ->
        tyPar.Constraints
        |> ImArray.choose (fun x -> 
            match x.TryGetSubtypeOf() with
            | ValueSome constrTy -> Some constrTy
            | _ -> None
        )
    | ty ->
        let intrinsic = ty.AllLogicalInheritsAndImplements
        match benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty) with
        | ValueSome (tyExts) ->
            let extrinsic =
                tyExts.Values
                |> Seq.collect (fun x ->
                    if x.IsTypeExtension then
                        x.Implements
                    else
                        ImArray.empty
                )

            Seq.append intrinsic extrinsic
            |> ImArray.ofSeq
        | _ ->
            intrinsic

let filterFields (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) (fields: IFieldSymbol seq) =
    let isInstance = queryMemberFlags &&& QueryMemberFlags.Instance = QueryMemberFlags.Instance
    let isStatic = queryMemberFlags &&& QueryMemberFlags.Static = QueryMemberFlags.Static
    let isOverridable = queryMemberFlags &&& QueryMemberFlags.Overridable = QueryMemberFlags.Overridable
    fields
    |> Seq.filter (fun field -> 
        (if isStatic = isInstance then true else field.IsInstance = isInstance) &&
        (if isOverridable then field.IsOverridable = true else true) &&
        (field.ValueFlags &&& valueFlags = valueFlags) &&
        (
            match nameOpt with
            | None -> true
            | Some(name) -> name = field.Name
        ))

let filterProperties (queryMemberFlags: QueryMemberFlags) (valueFlags: ValueFlags) (nameOpt: string option) (props: IPropertySymbol seq) =
    let isInstance = queryMemberFlags &&& QueryMemberFlags.Instance = QueryMemberFlags.Instance
    let isStatic = queryMemberFlags &&& QueryMemberFlags.Static = QueryMemberFlags.Static
    let isOverridable = queryMemberFlags &&& QueryMemberFlags.Overridable = QueryMemberFlags.Overridable
    props
    |> Seq.filter (fun prop -> 
        (if isStatic = isInstance then true else prop.IsInstance = isInstance) &&
        (if isOverridable then prop.IsOverridable = true else true) &&
        (prop.ValueFlags &&& valueFlags = valueFlags) &&
        (
            match nameOpt with
            | None -> true
            | Some(name) -> name = prop.Name
        ))

let filterFunctions (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (funcs: IFunctionSymbol seq) =
    let isInstance = queryMemberFlags &&& QueryMemberFlags.Instance = QueryMemberFlags.Instance
    let isStatic = queryMemberFlags &&& QueryMemberFlags.Static = QueryMemberFlags.Static
    let isOverridable = queryMemberFlags &&& QueryMemberFlags.Overridable = QueryMemberFlags.Overridable
    let canCheckOverrides = queryMemberFlags &&& QueryMemberFlags.InstanceFunctionOverrides = QueryMemberFlags.InstanceFunctionOverrides
    funcs
    |> Seq.filter (fun func ->
        let func =
            if canCheckOverrides then
                match func.FunctionOverrides with
                | Some func -> func
                | _ -> func
            else
                func
            
        (if isStatic = isInstance then true else func.IsInstance = isInstance) &&
        (if isOverridable then func.IsOverridable = true else true) &&
        (func.FunctionFlags &&& funcFlags = funcFlags) &&
        (
            match nameOpt with
            | None -> true
            | Some(name) -> 
                if func.IsConstructor then
                    name = func.Enclosing.AsEntity.Name
                else
                    name = func.Name
        ))

[<RequireQualifiedAccess>]
type QueryField =
    | Intrinsic
    | IntrinsicAndExtrinsic

let canAccessValue (benv: BoundEnvironment) (value: IValueSymbol) =
    if value.IsPublic then true
    elif value.IsInternal then
        // TODO: There a way to make this a faster check?
        match benv.ac.Entity, value.Enclosing.TryEntity with
        | Some ent1, Some ent2 ->
            match ent1.ContainingAssembly, ent2.ContainingAssembly with
            | Some asm1, Some asm2 ->
                let ident1 = asm1.Identity
                let ident2 = asm2.Identity
                ident1.Name.Equals(ident2.Name) &&
                ident1.Key.Equals(ident2.Key)
            | _ ->
                false
        | _ -> 
            false
    elif value.IsProtected then
        match benv.ac.Entity, value.Enclosing.TryEntity with
        | Some ent1, Some ent2 -> subsumesEntity ent1 ent2 || subsumesEntity ent2 ent1
        | _ -> false
    else
        match benv.ac.Entity, value.Enclosing.TryEntity with
        | Some ent1, Some ent2 -> areEntitiesEqual ent1 ent2
        | _ -> false

let filterValuesByAccessibility<'T when 'T :> IValueSymbol> (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (values: 'T seq) =
    // We are querying for functions that override, we must include private functions in this case.
    if queryMemberFlags &&& QueryMemberFlags.InstanceFunctionOverrides = QueryMemberFlags.InstanceFunctionOverrides then 
        values
    else
        values
        |> Seq.filter (canAccessValue benv)

let findImmediateFunctionsOfEntity (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (ent: IEntitySymbol) =
    filterFunctions queryMemberFlags funcFlags nameOpt ent.Functions
    |> filterValuesByAccessibility benv queryMemberFlags

// Finds the most specific functions of an entity
let rec findMostSpecificIntrinsicFunctionsOfEntity (benv: BoundEnvironment) (queryMemberFlags: QueryMemberFlags) (funcFlags: FunctionFlags) (nameOpt: string option) (ent: IEntitySymbol) : IFunctionSymbol imarray =
    let funcs = findImmediateFunctionsOfEntity benv queryMemberFlags funcFlags nameOpt ent |> ImArray.ofSeq

    let overridenFuncs =
        funcs
        |> ImArray.filter (fun x -> x.FunctionOverrides.IsSome)

    let inheritedFuncs =
        let extends = ent.ExtendsForMemberLookup
        extends
        |> Seq.map (fun x ->
            findMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags nameOpt x         
        )
        |> Seq.concat
        |> Seq.filter (fun (x: IFunctionSymbol) -> 
            not x.IsConstructor &&
            let isOverriden =
                overridenFuncs
                |> ImArray.exists (fun y ->
                    x.IsVirtual && areLogicalFunctionSignaturesEqual x y.FunctionOverrides.Value
                )
            not isOverriden
        )
        |> filterValuesByAccessibility benv queryMemberFlags

    let nestedCtors =
        if (queryMemberFlags &&& QueryMemberFlags.Instance <> QueryMemberFlags.Instance) then
            ent.Entities
            |> Seq.map (fun ent -> 
                findMostSpecificIntrinsicFunctionsOfEntity benv (queryMemberFlags ||| QueryMemberFlags.Instance) (funcFlags ||| FunctionFlags.Constructor) nameOpt ent)
            |> Seq.concat
            |> filterValuesByAccessibility benv queryMemberFlags
        else
            Seq.empty

    let funcs = Seq.append inheritedFuncs funcs
    let funcs = Seq.append funcs nestedCtors |> ImArray.ofSeq

    // Most specific functions
    funcs
    |> filterMostSpecificFunctions

and findMostSpecificIntrinsicFunctionsOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (ty: TypeSymbol) =
    let ty = findIntrinsicTypeIfPossible benv ty
    match stripTypeEquations ty with
    | TypeSymbol.Entity(ent) ->
        findMostSpecificIntrinsicFunctionsOfEntity benv queryMemberFlags funcFlags nameOpt ent
    | TypeSymbol.Variable(tyPar)
    | TypeSymbol.InferenceVariable(Some tyPar, _) ->
        findMostSpecificIntrinsicFunctionsOfTypeParameter false tyPar
        |> filterFunctions queryMemberFlags funcFlags nameOpt
        |> filterValuesByAccessibility benv queryMemberFlags
        |> ImArray.ofSeq
    | TypeSymbol.HigherVariable(tyPar, tyArgs)
    | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, _, _) ->
        findMostSpecificIntrinsicFunctionsOfTypeParameter true tyPar
        |> filterFunctions queryMemberFlags funcFlags nameOpt
        |> filterValuesByAccessibility benv queryMemberFlags
        |> Seq.map (fun func ->
            let enclosing = 
                func.Enclosing
                |> actualEnclosing tyArgs 
            actualFunction enclosing (enclosing.TypeArguments.AddRange(func.TypeArguments)) func
        )
        |> ImArray.ofSeq
    | _ ->
        ImArray.empty

let findExtensionMembersOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (ty: TypeSymbol) =
    let find ty =
        match benv.senv.typeExtensionMembers.TryFind(stripTypeEquationsAndBuiltIn ty) with
        | ValueSome(exts) ->
            exts.Values
            |> Seq.choose (fun extMember ->
                match extMember with
                | ExtensionMemberSymbol.Function(func) -> 
                    // TODO: We should have a helper function for this.
                    //       We already have Substitute and Apply.
                    //       Substitute goes by type parameter id, so it has to be exact and doesn't work here.

                    OlyAssert.False(func.Enclosing.AsType.Inherits[0].IsAliasAndNotCompilerIntrinsic)
     
                    let tyArgs = ty.TypeArguments
                    let enclosing = applyEnclosing tyArgs func.Enclosing
                    let func = actualFunction enclosing (tyArgs.AddRange(func.TypeArguments)) func
                    Some(func)
                | ExtensionMemberSymbol.Property _ ->
                    // TODO: Handle properties.
                    None
            )
            |> filterFunctions queryMemberFlags funcFlags nameOpt
            |> filterValuesByAccessibility benv queryMemberFlags
        | _ ->
            Seq.empty

    let results = find ty
    if Seq.isEmpty results then
        // Filter most specific extended types
        ty.AllLogicalInheritsAndImplements
        |> Seq.map find
        |> Seq.concat
    else
        results

    |> ImArray.ofSeq
    |> filterMostSpecificFunctions

let findInterfaceExtensionMembersOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) ty =
    match benv.senv.typeExtensionsWithImplements.TryFind(stripTypeEquationsAndBuiltIn ty) with
    | ValueSome(exts) ->
        exts.Values
        |> Seq.collect (fun ext ->
            ext.Functions
            |> filterFunctions queryMemberFlags funcFlags nameOpt
            |> filterValuesByAccessibility benv queryMemberFlags
        )
    | _ ->
        Seq.empty

    |> ImArray.ofSeq
    |> filterMostSpecificFunctions

let findAllExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty =
    let extMembers = findExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
    let extInterfaceMembers = findInterfaceExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
    ImArray.append extMembers extInterfaceMembers
    |> filterMostSpecificFunctions

let combineConcreteAndExtensionMembers (concreteMembers: #IValueSymbol seq) (extMembers: #IValueSymbol seq) : #IValueSymbol imarray =
    let filteredExtMembers =
        // Concrete members take precedent.
        extMembers
        |> Seq.filter (fun extMember ->
#if DEBUG
            OlyAssert.True(extMember.Enclosing.IsTypeExtension)
#endif
            let concreteMemberExists =
                concreteMembers
                |> Seq.exists (fun concreteMember ->
#if DEBUG
                    OlyAssert.False(concreteMember.Enclosing.IsTypeExtension)
#endif
                    areValueSignaturesEqual concreteMember extMember
                )
            not concreteMemberExists
        )

    Seq.append concreteMembers filteredExtMembers
    |> ImArray.ofSeq

[<RequireQualifiedAccess>]
type QueryFunction =
    /// Query for members that are only directly on the type.
    | Intrinsic
    /// Query for members that are directly on the type and its extension members that are in scope.
    | IntrinsicAndExtrinsic

let findMostSpecificFunctionsOfType (benv: BoundEnvironment) queryMemberFlags funcFlags (nameOpt: string option) (queryFunc: QueryFunction) (ty: TypeSymbol) =
    let intrinsicFuncs = findMostSpecificIntrinsicFunctionsOfType benv queryMemberFlags funcFlags nameOpt ty

    let extrinsicFuncs =
        match queryFunc with
        | QueryFunction.IntrinsicAndExtrinsic ->
            findAllExtensionMembersOfType benv queryMemberFlags funcFlags nameOpt ty
        | _ ->
            ImArray.empty

    combineConcreteAndExtensionMembers intrinsicFuncs extrinsicFuncs

let freshenValue (benv: BoundEnvironment) (value: IValueSymbol) =
    match value with
    | :? LocalSymbol -> value
    | :? IFieldSymbol -> value // REVIEW/TODO: Do we need to do this?
    | :? IFunctionSymbol as func ->
        if value.Enclosing.TypeParameters.IsEmpty && value.TypeParameters.IsEmpty then value
        else
            let cache = Dictionary<TypeParameterSymbol, TypeSymbol>(TypeParameterSymbolComparer())
            let tyArgs =
                let tyPars = benv.GetScopedTypeParameters(func)
                let tyArgs = benv.GetScopedTypeArguments(func)
                tyArgs
                |> ImArray.map (fun ty ->
                    freshenTypeAux benv tyPars ImArray.empty ty cache
                )

            let enclosing = 
                let tyArgsForEnclosing =
                    tyArgs 
                    |> Seq.take value.Enclosing.TypeParameters.Length 
                    |> ImmutableArray.CreateRange
                applyEnclosing tyArgsForEnclosing value.Enclosing

            actualFunction enclosing tyArgs func :> IValueSymbol
    | _ ->
        if not value.IsInvalid then
            failwith "Invalid value symbol"
        value

let createFunctionWithTypeParametersOfFunction (tyPars: TypeParameterSymbol imarray) (func: FunctionSymbol) =
    let funcTy = TypeSymbol.CreateFunction(tyPars, func.Parameters |> ImArray.map (fun x -> x.Type), func.ReturnType)
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

let createMutableLocalDeclarationReturnExpression (rhsExpr: BoundExpression) =
    let local = createMutableLocalValue "mlocal" rhsExpr.Type
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
    }