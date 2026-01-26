module internal rec Oly.Compiler.Internal.WellKnownExpressions

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.BoundTreePatterns

let UnsafeCast benv (expr: BoundExpression) (castToType: TypeSymbol) =
    let syntaxTree = expr.Syntax.Tree
    let argExprs = [|expr|] |> ImArray.ofSeq
    let func = (freshenValue benv WellKnownFunctions.UnsafeCast).AsFunction
    UnifyTypes TypeVariableRigidity.Flexible func.ReturnType castToType |> ignore
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, ImArray.empty, argExprs, func, CallFlags.None)

let Ignore (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let argExprs = ImArray.createOne expr
    let ignoreFunc = WellKnownFunctions.IgnoreFunction
    let ignoreFunc = actualValue ignoreFunc.Enclosing (ImArray.createOne expr.Type) ignoreFunc
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, ImArray.empty, argExprs, ignoreFunc, CallFlags.None)

let EqualWithSyntax (syntaxInfo: BoundSyntaxInfo) (expr1: BoundExpression) (expr2: BoundExpression) =
    if not (obj.ReferenceEquals(syntaxInfo.Syntax.Tree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    let argExprs = [|expr1;expr2|] |> ImArray.ofSeq
    BoundExpression.Call(syntaxInfo, None, ImArray.empty, argExprs, WellKnownFunctions.equalFunc, CallFlags.None)

let Equal (expr1: BoundExpression) (expr2: BoundExpression) =
    EqualWithSyntax (BoundSyntaxInfo.Generated(expr1.Syntax.Tree)) expr1 expr2 

let NotEqual (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    let argExprs = [|expr1;expr2|] |> ImArray.ofSeq
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, ImArray.empty, argExprs, WellKnownFunctions.notEqualFunc, CallFlags.None)

let And (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    let argExprs = [|expr1;expr2|] |> ImArray.ofSeq
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, ImArray.empty, argExprs, WellKnownFunctions.andFunc, CallFlags.None)

let Or (expr1: BoundExpression) (expr2: BoundExpression) =
    let syntaxTree = expr1.Syntax.Tree
    if not (obj.ReferenceEquals(syntaxTree, expr2.Syntax.Tree)) then
        failwith "Expected same syntax tree."
    let argExprs = [|expr1;expr2|] |> ImArray.ofSeq
    BoundExpression.Call(BoundSyntaxInfo.Generated(syntaxTree), None, ImArray.empty, argExprs, WellKnownFunctions.orFunc, CallFlags.None)

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
        ImArray.empty, 
        argExprs,
        WellKnownFunctions.LoadTupleElement.Apply(tyArgs), 
        CallFlags.None
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

let private createCallExpression syntaxInfo (formalValue: IValueSymbol) (tyArgs: TypeArgumentSymbol imarray) witnessArgs args isVirtualCall =
    OlyAssert.True(formalValue.IsFormal)
    let value =
        let tyArgs =
            (formalValue.TypeParameters, tyArgs)
            ||> ImArray.map2 (fun tyPar tyArg -> 
#if DEBUG || CHECKED
                match tyArg.TryImmediateTypeParameter with
                | ValueSome(tyPar2) ->
                    OlyAssert.NotEqual(tyPar.Id, tyPar2.Id)
                | _ ->
                    ()
#endif
                mkSolvedInferenceVariableType tyPar tyArg
            )
        formalValue.Apply(tyArgs)
    BoundExpression.Call(
        syntaxInfo,
        None,
        witnessArgs,
        args,
        value,
        isVirtualCall
    )

let private createGeneratedCallExpression syntaxTree (formalValue: IValueSymbol) (tyArgs: TypeArgumentSymbol imarray) witnessArgs args isVirtualCall =
    OlyAssert.True(formalValue.IsFormal)
    let value =
        let tyArgs =
            (formalValue.TypeParameters, tyArgs)
            ||> ImArray.map2 (fun tyPar tyArg -> 
#if DEBUG || CHECKED
                match tyArg.TryImmediateTypeParameter with
                | ValueSome(tyPar2) ->
                    OlyAssert.NotEqual(tyPar.Id, tyPar2.Id)
                | _ ->
                    ()
#endif
                mkSolvedInferenceVariableType tyPar tyArg
            )
        formalValue.Apply(tyArgs)
    BoundExpression.Call(
        BoundSyntaxInfo.Generated(syntaxTree),
        None,
        witnessArgs,
        args,
        value,
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
        CallFlags.None

let LoadStaticFunction (expr: BoundExpression) funcTy =
    let syntaxTree = expr.Syntax.Tree
    createGeneratedCallExpression
        syntaxTree
        WellKnownFunctions.LoadStaticFunction
        (ImArray.createOne funcTy)
        ImArray.empty
        (ImArray.createOne expr)
        CallFlags.None

let NewRefCell (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let exprTy = expr.Type
    createGeneratedCallExpression
        syntaxTree
        WellKnownFunctions.NewRefCell
        (ImArray.createOne exprTy)
        ImArray.empty
        (ImArray.createOne expr)
        CallFlags.None

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
            CallFlags.None
    | _ ->
        failwith "Invalid expression."

let StoreRefCellContents syntaxInfo (receiver: BoundExpression) (rhs: BoundExpression) =
    let syntaxTree = rhs.Syntax.Tree
    let refCellArgTy = receiver.Type
    match refCellArgTy.TryGetReferenceCellElement with
    | ValueSome elementTy ->
        createCallExpression
            syntaxInfo
            WellKnownFunctions.StoreRefCellContents
            (ImArray.createOne elementTy)
            ImArray.empty
            (ImArray.createTwo receiver rhs)
            CallFlags.None
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
            CallFlags.None
    | _ ->
        failwith "Invalid expression."

let rec AutoDereferenceIfPossible (expr: BoundExpression) =
    let syntaxTree = expr.Syntax.Tree
    let exprTy = expr.Type
    if exprTy.IsAnyByRef_ste then
        // We want to dereference non-generated expressions.
        match expr with
        | BoundExpression.Value _ 
        | BoundExpression.GetProperty _ ->
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

/// Creates a 'Call' expression to get the address
/// based on the given expression. Expression type is a read/write ByRef type.
let AddressOf (expr: BoundExpression) =
    createGeneratedCallExpression
        expr.Syntax.Tree
        WellKnownFunctions.AddressOf
        (ImArray.createOne expr.Type)
        ImArray.empty
        (ImArray.createOne expr)
        CallFlags.None

/// Creates a 'Call' expression to get the address
/// based on the given expression. Expression type is a read/write ByRef type.
let AddressOfMutable (expr: BoundExpression) =
    createGeneratedCallExpression
        expr.Syntax.Tree
        WellKnownFunctions.AddressOfMutable
        (ImArray.createOne expr.Type)
        ImArray.empty
        (ImArray.createOne expr)
        CallFlags.None

let private AddressOfReceiverIfPossibleAux isMutable (enclosingTy: TypeSymbol) (expr: BoundExpression) =
    let exprTy = expr.Type
    if (exprTy.IsStruct_ste && (enclosingTy.IsTypeExtensionExtendingStruct_ste || enclosingTy.IsStruct_ste)) || exprTy.IsAnyVariable_ste then
        match expr with
        // Cannot take the address of a constant.
        | BoundExpression.Value(value=value) when not value.IsFieldConstant -> 
            if value.IsImmutable && not exprTy.IsAnyVariable_ste then
                AddressOf expr
            else
                if isMutable then
                    AddressOfMutable expr
                else
                    expr
        | BoundExpression.GetField(syntaxInfo, receiver, field) ->
            let expr = 
                if field.Enclosing.IsType then
                    let newReceiver = AddressOfReceiverIfPossibleAux isMutable receiver.Type receiver
                    if newReceiver = receiver then
                        expr
                    else
                        BoundExpression.GetField(syntaxInfo, newReceiver, field)
                else
                    expr

            if exprTy.IsAnyVariable_ste then
                if isMutable then
                    AddressOfMutable expr
                else
                    expr
            elif field.IsImmutable then
                AddressOf expr
            else
                match stripTypeEquations receiver.Type with
                | TypeSymbol.ByRef(elementTy, kind) when elementTy.IsStruct_ste ->
                    match kind with
                    | ByRefKind.ReadWrite
                    | ByRefKind.WriteOnly -> 
                        if isMutable then
                            AddressOfMutable expr
                        else
                            expr
                    | ByRefKind.ReadOnly -> 
                        AddressOf expr
                | _ ->
                    if field.IsMutable then
                        if isMutable then
                            AddressOfMutable expr
                        else
                            expr
                    else
                        AddressOf expr
        | GetArrayElement(expr1, _) ->
            let expr1Ty = expr1.Type
            match stripTypeEquations expr1Ty with
            | TypeSymbol.Array(_, _, kind)
            | TypeSymbol.FixedArray( _, _, kind) ->
                match kind with
                | ArrayKind.Immutable ->
                    AddressOf expr
                | ArrayKind.Mutable ->
                    if isMutable then
                        AddressOfMutable expr
                    else
                        expr
            | _ ->
                unreached()
        | AutoDereferenced(expr2) ->
            expr2
        | _ ->
            if isMutable then
                let expr2, _ = createMutableLocalDeclarationReturnExpression expr
                match expr2 with
                | BoundExpression.Let(syntaxInfo, value, rhsExpr, bodyExpr) ->
                    BoundExpression.Let(syntaxInfo, value, rhsExpr, AddressOfMutable bodyExpr)
                | _ ->
                    unreached()
            else
                let expr2, _ = createLocalDeclarationReturnExpression expr
                match expr2 with
                | BoundExpression.Let(syntaxInfo, value, rhsExpr, bodyExpr) ->
                    BoundExpression.Let(syntaxInfo, value, rhsExpr, AddressOf bodyExpr)
                | _ ->
                    unreached()
    else
        // This prevents an inref of T to not mutate the reference.
        // inref T -> deref T -> byref T
        match exprTy.TryByReferenceElementType with
        | ValueSome(exprElementTy) when exprTy.IsReadOnlyByRef_ste && (enclosingTy.IsAnyVariable_ste && areTypesEqual enclosingTy exprElementTy) ->
            let newExpr, _ = createMutableLocalDeclarationReturnExpression (FromAddress expr)
            match newExpr with
            | BoundExpression.Let(syntaxInfo, value, rhsExpr, bodyExpr) ->
                BoundExpression.Let(syntaxInfo, value, rhsExpr, AddressOfMutable bodyExpr)
            | _ ->
                unreached()
        | _ ->
            expr

let AddressOfReceiverIfPossible enclosingTy expr =
    AddressOfReceiverIfPossibleAux true enclosingTy expr

let ReadOnlyAddressOfReceiverIfPossible enclosingTy expr =
    AddressOfReceiverIfPossibleAux false enclosingTy expr