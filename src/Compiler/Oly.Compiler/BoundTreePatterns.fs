module internal Oly.Compiler.Internal.BoundTreePatterns

open Oly.Core
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols

let private andName = Some "and"
let private orName = Some "or"
let private equalName = Some "equal"

type E = BoundExpression

let (|True|_|) (expr: E) =
    match expr.Strip() with
    | E.Literal(_, BoundLiteral.Constant(ConstantSymbol.True)) -> Some()
    | _ -> None

let (|False|_|) (expr: E) =
    match expr.Strip() with
    | E.Literal(_, BoundLiteral.Constant(ConstantSymbol.False)) -> Some()
    | _ -> None

let (|Int32|_|) (expr: E) =
    match expr.Strip() with
    | E.Literal(_, BoundLiteral.Constant(ConstantSymbol.Int32(value))) -> Some(value)
    | _ -> None

let (|LogicalAnd|_|) (expr: E) =
    match expr.Strip() with
    | E.IfElse(_, expr1, expr2, False, _) ->
        Some(expr1, expr2)
    | _ ->
        None

let (|LogicalOr|_|) (expr: E) =
    match expr.Strip() with
    | E.IfElse(_, expr1, True, expr2, _) ->
        Some(expr1, expr2)
    | _ ->
        None

let (|AndCall|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(syntaxInfo=syntaxInfo;receiverOpt=None;value=value;args=args) when args.Length = 2 && value.TryWellKnownFunction = ValueSome(WellKnownFunction.And) ->
        Some(syntaxInfo, args.[0], args.[1])
    | _ ->
        None

let (|OrCall|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(syntaxInfo=syntaxInfo;receiverOpt=None;value=value;args=args) when args.Length = 2 && value.TryWellKnownFunction = ValueSome(WellKnownFunction.Or) ->
        Some(syntaxInfo, args.[0], args.[1])
    | _ ->
        None

let (|StackEmplaceCall|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when value.IsStackEmplace ->
        Some(value :?> IFunctionSymbol, args)
    | _ ->
        None

let (|AddressOf|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when args.Length = 1 && value.IsAddressOf ->
        Some(args[0])
    | _ ->
        None

let (|FromAddress|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when args.Length = 1 && value.IsFromAddress ->
        Some(args[0])
    | _ ->
        None

let (|LambdaWrappedFunctionCall|_|) (expr: E) =
    match expr.Strip() with
    | E.Lambda(syntaxInfo=syntaxInfo;pars=pars;body=lazyBodyExpr) when syntaxInfo.IsGenerated ->
        match lazyBodyExpr.Expression with
        | E.Call(syntaxInfo=syntaxInfo;receiverOpt=None;args=argExprs;value=value) when value.IsFunction && pars.Length = argExprs.Length ->
            let areSame =
                (pars, argExprs)
                ||> ImArray.forall2 (fun par argExpr ->
                    match argExpr with
                    | E.Value(_, arg) -> par.Id = arg.Id
                    | _ -> false
                )
            if areSame then
                Some(syntaxInfo, value :?> IFunctionSymbol)
            else
                None
        | _ ->
            None
    | _ ->
        None

let (|LoadFunctionPtr|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(syntaxInfo=syntaxInfo;receiverOpt=None;value=value;args=args) when args.Length = 1 && value.IsLoadFunctionPtr ->
        Some(syntaxInfo, value :?> IFunctionSymbol, args[0])
    | _ ->
        None

let (|GetArrayElement|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when args.Length = 2 && value.TryWellKnownFunction = ValueSome WellKnownFunction.GetArrayElement ->
        Some(args[0], args[1])
    | _ ->
        None

let (|Equal|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when args.Length = 2 && value.TryWellKnownFunction = ValueSome(WellKnownFunction.Equal) ->
        Some(args.[0], args.[1])
    | _ ->
        None

let (|Ignore|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when args.Length = 1 && value.TryWellKnownFunction = ValueSome(WellKnownFunction.Ignore) ->
        Some(args.[0])
    | _ ->
        None

let (|AutoDereferenced|_|) (expr: E) =
    let expr = expr.Strip()
    match expr with
    | FromAddress(argExpr) when expr.IsGenerated ->
        Some(argExpr)

    | E.Let(syntaxInfo, bindingInfo, rhsExpr, FromAddress(E.Value(value=value2))) when syntaxInfo.IsGenerated && bindingInfo.Value.Id = value2.Id ->
        Some(rhsExpr)

    | _ ->
        None

/// The expression is known to not have any side effects.
/// Impure functions and mutations are side effectful.
let rec (|NoSideEffect|_|) (expr: E) =
    match expr.Strip() with
    | E.None _
    | E.Unit _
    | E.Value _
    | E.Literal _
    | E.GetField(receiver=NoSideEffect)
    | E.Typed(body=NoSideEffect)
    // REVIEW: It's fine to go through a sequential expression, but it might be expensive if we do it alot. 
    //         Might be wise to consider adding a simple cached flag indicating that it doesn't cause side effects.
    | E.Sequential(_, NoSideEffect, NoSideEffect)
    | LogicalAnd(NoSideEffect, NoSideEffect)
    | LogicalOr(NoSideEffect, NoSideEffect)
    | Equal(NoSideEffect, NoSideEffect) -> 
        // TODO: Add more cases, i.e. NotEqual, Add, GreaterThan, etc.
        Some()

    | _ -> 
        None

let (|NewRefCell|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when args.Length = 1 && value.TryWellKnownFunction = ValueSome(WellKnownFunction.NewRefCell) ->
        Some(args.[0])
    | _ ->
        None

let (|GetRefCellContents|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when args.Length = 1 && value.TryWellKnownFunction = ValueSome(WellKnownFunction.LoadRefCellContents) ->
        Some(args.[0])
    | _ ->
        None

let (|SetRefCellContents|_|) (expr: E) =
    match expr.Strip() with
    | E.Call(receiverOpt=None;value=value;args=args) when args.Length = 2 && value.TryWellKnownFunction = ValueSome(WellKnownFunction.StoreRefCellContents) ->
        Some(args.[0], args.[1])
    | _ ->
        None

let BoundLiteralTrue = BoundLiteral.Constant(ConstantSymbol.True)
let BoundLiteralFalse = BoundLiteral.Constant(ConstantSymbol.False)
let BoundLiteralInt32 value = BoundLiteral.Constant(ConstantSymbol.Int32(value))