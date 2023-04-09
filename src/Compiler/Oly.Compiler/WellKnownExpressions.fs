module internal rec Oly.Compiler.Internal.WellKnownExpressions

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.BoundTreePatterns

let UnsafeCast benv (expr: BoundExpression) (castToType: TypeSymbol) =
    let syntaxTree = expr.Syntax.Tree
    let argExprs = [|expr|] |> ImArray.ofSeq
    let func = (freshenValue benv WellKnownFunctions.UnsafeCast).AsFunction
    UnifyTypes TypeVariableRigidity.Flexible func.ReturnType castToType |> ignore
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, CacheValueWithArg.FromValue(ImArray.empty), argExprs, func, false)

let ImplicitCast benv (expr: BoundExpression) castToType =
    let exprTy = expr.Type
    if exprTy.IsEnum then
        match exprTy.AsEntity.RuntimeType with
        | Some(runtimeTy) when areTypesEqual runtimeTy castToType ->
            Oly.Compiler.Internal.WellKnownExpressions.UnsafeCast benv expr castToType
        | _ ->
            expr
    else
        expr

let ExplicitCast benv (expr: BoundExpression) (castToType: TypeSymbol) =
    if castToType.IsEnum then
        let exprTy = expr.Type
        match castToType.AsEntity.RuntimeType with
        | Some(runtimeTy) when areTypesEqual runtimeTy exprTy ->
            Oly.Compiler.Internal.WellKnownExpressions.UnsafeCast benv expr castToType
        | _ ->
            expr
    else
        expr

let Ignore (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let argExprs = ImArray.createOne expr
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, CacheValueWithArg.FromValue(ImArray.empty), argExprs, WellKnownFunctions.IgnoreFunction, false)

let EqualWithSyntax (syntaxInfo: BoundSyntaxInfo) (expr1: BoundExpression) (expr2: BoundExpression) =
    if not (obj.ReferenceEquals(syntaxInfo.Syntax.Tree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    let argExprs = [|expr1;expr2|] |> ImArray.ofSeq
    BoundExpression.Call(syntaxInfo, None, CacheValueWithArg.FromValue(ImArray.empty), argExprs, WellKnownFunctions.equalFunc, false)

let Equal (expr1: BoundExpression) (expr2: BoundExpression) =
    EqualWithSyntax (BoundSyntaxInfo.Generated(expr1.Syntax.Tree)) expr1 expr2 

let NotEqual (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    let argExprs = [|expr1;expr2|] |> ImArray.ofSeq
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, CacheValueWithArg.FromValue(ImArray.empty), argExprs, WellKnownFunctions.notEqualFunc, false)

let And (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    let argExprs = [|expr1;expr2|] |> ImArray.ofSeq
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, CacheValueWithArg.FromValue(ImArray.empty), argExprs, WellKnownFunctions.andFunc, false)

let Or (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    let argExprs = [|expr1;expr2|] |> ImArray.ofSeq
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, CacheValueWithArg.FromValue(ImArray.empty), argExprs, WellKnownFunctions.orFunc, false)

let LogicalAnd (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    BoundExpression.IfElse(BoundSyntaxInfo.Generated(syntaxTree), expr1, expr2, BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), BoundLiteralTrue), TypeSymbol.Bool)

let LogicalOr (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    BoundExpression.IfElse(BoundSyntaxInfo.Generated(syntaxTree), expr1, BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), BoundLiteralTrue), expr2, TypeSymbol.Bool)

let LoadTupleElement (elementIndex: int) (elementTy: TypeSymbol) (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let tyArgs =
        (TypeSymbol.ConstantInt32(elementIndex), elementTy)
        ||> ImArray.createTwo
    let argExprs = [|expr|] |> ImArray.ofSeq
    BoundExpression.Call(
        BoundSyntaxInfo.Generated(syntaxTree), 
        None, 
        CacheValueWithArg.FromValue(ImArray.empty), 
        argExprs,
        WellKnownFunctions.LoadTupleElement.Apply(tyArgs), 
        false
    )

let InlineAnd (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."

    let trueTargetExpr =
        BoundExpression.IfElse(
            BoundSyntaxInfo.Generated(syntaxTree),
            expr2,
            BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), BoundLiteralTrue),
            BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), BoundLiteralFalse),
            TypeSymbol.Bool
        )
        
    let falseTargetExpr = BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), BoundLiteralFalse)

    BoundExpression.IfElse(
        BoundSyntaxInfo.Generated(syntaxTree),
        expr1,
        trueTargetExpr,
        falseTargetExpr,
        TypeSymbol.Bool
    )

let InlineOr (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."

    let trueTargetExpr = BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), BoundLiteralTrue)

    let falseTargetExpr =
        BoundExpression.IfElse(
            BoundSyntaxInfo.Generated(syntaxTree),
            expr2,
            BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), BoundLiteralTrue),
            BoundExpression.Literal(BoundSyntaxInfo.Generated(syntaxTree), BoundLiteralFalse),
            TypeSymbol.Bool
        )

    BoundExpression.IfElse(
        BoundSyntaxInfo.Generated(syntaxTree),
        expr1,
        trueTargetExpr,
        falseTargetExpr,
        TypeSymbol.Bool
    )

/// TODO: This is kinda of bad. What do we actually want to pass here? What if the 'value' is not formal? What does that mean?
let private createGeneratedCallExpression syntaxTree (value: IValueSymbol) (tyArgs: TypeArgumentSymbol imarray) witnessArgs args isVirtualCall =
    BoundExpression.Call(
        BoundSyntaxInfo.Generated(syntaxTree),
        None,
        CacheValueWithArg.FromValue(witnessArgs),
        args,
        actualValue value.Enclosing tyArgs value.Formal,
        isVirtualCall
    )

let LoadFunction (receiverExpr: BoundExpression) (expr: BoundExpression) funcTy =
    let syntaxTree = expr.Syntax.Tree
    createGeneratedCallExpression
        syntaxTree
        WellKnownFunctions.LoadFunction
        (ImArray.createOne funcTy)
        ImArray.empty
        (ImArray.createTwo receiverExpr expr)
        false

let LoadStaticFunction (expr: BoundExpression) funcTy =
    let syntaxTree = expr.Syntax.Tree
    createGeneratedCallExpression
        syntaxTree
        WellKnownFunctions.LoadStaticFunction
        (ImArray.createOne funcTy)
        ImArray.empty
        (ImArray.createOne expr)
        false

let NewRefCell (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let exprTy = expr.Type
    createGeneratedCallExpression
        syntaxTree
        WellKnownFunctions.NewRefCell
        (ImArray.createOne exprTy)
        ImArray.empty
        (ImArray.createOne expr)
        false

let LoadRefCellContents (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let exprTy = expr.Type
    match exprTy.TryGetReferenceCellElement with
    | ValueSome elementTy ->
        createGeneratedCallExpression
            syntaxTree
            WellKnownFunctions.LoadRefCellContents
            (ImArray.createOne elementTy)
            ImArray.empty
            (ImArray.createOne expr)
            false
    | _ ->
        failwith "Invalid expression."

let StoreRefCellContents (receiver: BoundExpression) (rhs: BoundExpression) =
    let syntaxTree = rhs.Syntax.Tree
    let refCellArgTy = receiver.Type
    match refCellArgTy.TryGetReferenceCellElement with
    | ValueSome elementTy ->
        createGeneratedCallExpression
            syntaxTree
            WellKnownFunctions.StoreRefCellContents
            (ImArray.createOne elementTy)
            ImArray.empty
            (ImArray.createTwo receiver rhs)
            false
    | _ ->
        failwith "Invalid expression."

let FromAddress (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let exprTy = expr.Type
    match exprTy.TryByReferenceElementType with
    | ValueSome elementTy ->
        createGeneratedCallExpression
            syntaxTree
            WellKnownFunctions.FromAddress
            (ImArray.createOne elementTy)
            ImArray.empty
            (ImArray.createOne expr)
            false
    | _ ->
        failwith "Invalid expression."

let rec AutoDereferenceIfPossible (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let exprTy = expr.Type
    if exprTy.IsByRef_t then
        // We want to dereference non-generated expressions.
        match expr with
        | BoundExpression.Value _ ->
            FromAddress expr

        | BoundExpression.Call(value=value) when not value.IsAddressOf ->
            let value = createLocalBridgeValue exprTy
            let syntaxInfo = BoundSyntaxInfo.Generated(syntaxTree)
            BoundExpression.Let(
                syntaxInfo,
                BindingLocal(value),
                expr,
                FromAddress (BoundExpression.Value(syntaxInfo, value))
            )
        | _ ->
            expr
    else
        expr

and AutoDereferenceReceiverIfPossible (expr: BoundExpression) =
    let exprTy = expr.Type
    match exprTy.TryByReferenceElementType with
    // If the receiver's type is a struct or a type parameter, we do not want to dereference.
    | ValueSome elementTy when elementTy.IsAnyStruct || elementTy.IsTypeVariable ->
        expr
    | _ ->
        AutoDereferenceIfPossible expr

/// Creates a 'Call' expression to get the address
/// based on the given expression. Expression type is a read/write ByRef type.
let AddressOf (expr: BoundExpression) =
    createGeneratedCallExpression
        expr.Syntax.Tree
        WellKnownFunctions.AddressOf
        (ImArray.createOne expr.Type)
        ImArray.empty
        (ImArray.createOne expr)
        false

/// Creates a 'Call' expression to get the address
/// based on the given expression. Expression type is a read/write ByRef type.
let AddressOfMutable (expr: BoundExpression) =
    createGeneratedCallExpression
        expr.Syntax.Tree
        WellKnownFunctions.AddressOfMutable
        (ImArray.createOne expr.Type)
        ImArray.empty
        (ImArray.createOne expr)
        false

let AddressOfReceiverIfPossible (enclosingTy: TypeSymbol) (expr: BoundExpression) =
    let exprTy = expr.Type
    if ((exprTy.IsAnyStruct && (enclosingTy.IsAnyStruct || enclosingTy.IsTypeExtension)) || exprTy.IsTypeVariable) then
        if exprTy.IsByRef_t then
            failwith "Invalid expression."
        else
            match expr with
            | BoundExpression.Value(value=value) when not value.IsFieldConstant -> 
                if value.IsReadOnly then
                    AddressOf expr
                else
                    AddressOfMutable expr
            | BoundExpression.GetField(receiver=receiver;field=field) -> 
                if field.IsReadOnly then
                    AddressOf expr
                else
                    match stripTypeEquations receiver.Type with
                    | TypeSymbol.ByRef(elementTy, kind) when elementTy.IsAnyStruct ->
                        match kind with
                        | ByRefKind.ReadWrite -> AddressOfMutable expr
                        | ByRefKind.Read -> AddressOf expr
                    | _ ->
                        AddressOf expr
            | GetArrayElement(expr1, _) ->
                let expr1Ty = expr1.Type
                match stripTypeEquations expr1Ty with
                | TypeSymbol.Array(_, _, kind) ->
                    match kind with
                    | ArrayKind.Immutable ->
                        AddressOf expr
                    | ArrayKind.Mutable ->
                        AddressOfMutable expr
                | _ ->
                    failwith "should not happen"
            | AutoDereferenced(expr2) ->
                expr2
            | _ ->
                let expr2, _ = createMutableLocalDeclarationReturnExpression expr
                match expr2 with
                | BoundExpression.Let(syntaxInfo, value, rhsExpr, bodyExpr) ->
                    BoundExpression.Let(syntaxInfo, value, rhsExpr, AddressOfMutable bodyExpr)
                | _ ->
                    failwith "should not happen"
    else
        expr