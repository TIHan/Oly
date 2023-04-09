[<AutoOpen>]
module internal rec Oly.Compiler.Internal.Binder.NameResolution

open System
open Oly.Compiler
open Oly.Compiler.Syntax
open Oly.Core
open System.Collections.Generic
open System.Collections.Immutable
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.Solver
open Oly.Compiler.Internal.Checker
open Oly.Compiler.Internal.PrettyPrint
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.FunctionOverloading
open Oly.Compiler.Internal.WellKnownExpressions
open Oly.Compiler.Internal.Binder
open Oly.Compiler.Internal.ImplicitRules

[<RequireQualifiedAccess>]
type ResolutionContext =
    | All
    | TypeOnly
    | ValueOnly
    | ValueOnlyAttribute
    | PatternOnly

[<NoComparison;NoEquality>]
type ResolutionInfo =
    {
        resArgs: ResolutionArguments
        argExprs: BoundExpression imarray
        argInfos: ArgumentInfo imarray

        syntaxTyArgs: (OlySyntaxTypeArguments * OlySyntaxType imarray) option
        resTyArity: ResolutionTypeArity
        resContext: ResolutionContext
    }

    member this.InTypeOnlyContext =
        match this.resContext with
        | ResolutionContext.TypeOnly -> true
        | _ -> false

    member this.InPatternOnlyContext =
        match this.resContext with
        | ResolutionContext.PatternOnly -> true
        | _ -> false

    member this.InValueOnlyAttributeContext =
        match this.resContext with
        | ResolutionContext.ValueOnlyAttribute -> true
        | _ -> false

    member this.UpdateContext(resContext: ResolutionContext) =
        { this with resContext = resContext }

    /// Default of Resolution Info implies that we are not dealing with a function call.
    static member Default =
        {
            resArgs = ResolutionArguments.NotAFunctionCall
            argExprs = ImArray.empty
            argInfos = ImArray.empty
            syntaxTyArgs = None
            resTyArity = ResolutionTypeArityZero
            resContext = ResolutionContext.ValueOnly
        }

    static member Create(argExprs: BoundExpression imarray voption, resTyArity: ResolutionTypeArity, resContext: ResolutionContext) =
        {
            resArgs =
                match argExprs with
                | ValueSome argExprs ->
                    argExprs
                    |> ImArray.map (fun x ->
                        x.Type
                    )
                    |> ResolutionArguments.ByType
                | _ ->
                    ResolutionArguments.Any
            argExprs = argExprs |> ValueOption.defaultValue ImArray.empty
            argInfos =
                argExprs
                |> ValueOption.map (fun argExprs ->
                    argExprs
                    |> ImArray.map (fun argExpr ->
                        ArgumentInfo(argExpr.Type, argExpr.Syntax)
                    )
                )
                |> ValueOption.defaultValue ImArray.empty
            syntaxTyArgs = None
            resTyArity = resTyArity
            resContext = resContext
        }

    static member Create(argTys: TypeSymbol imarray, resTyArity: ResolutionTypeArity, resContext: ResolutionContext) =
        {
            resArgs =
                ResolutionArguments.ByType argTys
            argExprs = ImArray.empty
            argInfos = ImArray.empty
            syntaxTyArgs = None
            resTyArity = resTyArity
            resContext = resContext
        }

[<RequireQualifiedAccess;NoComparison;NoEquality>]
type ReceiverItem =
    | Type of TypeSymbol
    | Namespace of INamespaceSymbol

[<NoComparison;NoEquality>]
type ReceiverInfo =
    {
        item: ReceiverItem
        isStatic: bool
        expr: BoundExpression option
    }

[<RequireQualifiedAccess>]
type ResolutionItem = 
    | Type of syntaxName: OlySyntaxName * TypeSymbol
    | Namespace of syntaxName: OlySyntaxName * IEntitySymbol
    | MemberCall of syntaxToCapture: OlySyntaxExpression * receiverInfoOpt: ReceiverInfo option * syntaxBodyExpr: OlySyntaxExpression * syntaxArgs: OlySyntaxExpression imarray * syntaxMemberExprOpt: OlySyntaxExpression option
    | MemberIndexerCall of syntaxToCapture: OlySyntaxExpression * syntaxReceiver: OlySyntaxExpression * syntaxBrackets: OlySyntaxBrackets<OlySyntaxSeparatorList<OlySyntaxExpression>> * syntaxMemberExprOpt: OlySyntaxExpression option * expectedTyOpt: TypeSymbol option
    | Parenthesis of syntaxToCapture: OlySyntaxExpression * syntaxExprList: OlySyntaxSeparatorList<OlySyntaxExpression> * syntaxMemberExprOpt: OlySyntaxExpression option
    | Expression of BoundExpression
    | Pattern of syntax: OlySyntaxNode * IPatternSymbol * witnessArgs: WitnessSolution imarray
    | Invalid of syntax: OlySyntaxNode
    | Error of syntax: OlySyntaxNode

    member this.Syntax =
        match this with
        | Type(syntaxName=syntaxName) -> syntaxName :> OlySyntaxNode
        | Namespace(syntaxName=syntaxName) -> syntaxName :> OlySyntaxNode    
        | MemberCall(syntaxToCapture=syntaxExpr) -> syntaxExpr :> OlySyntaxNode
        | MemberIndexerCall(syntaxToCapture=syntaxExpr) -> syntaxExpr :> OlySyntaxNode
        | Parenthesis(syntaxToCapture=syntaxExpr) -> syntaxExpr :> OlySyntaxNode
        | Expression(expr) -> expr.Syntax
        | Pattern(syntax, _, _) -> syntax
        | Invalid(syntax) -> syntax
        | Error(syntax) -> syntax

let getSyntaxArgumentsAsSyntaxExpressions (cenv: cenv) (syntaxArgs: OlySyntaxArguments) =
    match syntaxArgs with
    | OlySyntaxArguments.Arguments(_, syntaxArgList, syntaxNamedArgList, _) ->
        if not syntaxNamedArgList.Children.IsEmpty then
            cenv.diagnostics.Error("Named arguments not supported yet.", 10, syntaxNamedArgList)
        syntaxArgList.ChildrenOfType
    | _ ->
        ImArray.empty

let private createWitnessArguments (cenv: cenv) (env: BinderEnvironment) syntaxNode (syntaxNameOpt: OlySyntaxName option) (value: IValueSymbol) =
    CacheValueWithArg(fun (syntaxTyArgsOpt: OlySyntaxType imarray option) ct ->
        ct.ThrowIfCancellationRequested()

        let witnessArgs =
            let allTyPars = value.AllTypeParameters
            let allTyArgs = value.AllTypeArguments 
            allTyPars
            |> ImArray.map (freshWitnessesWithTypeArguments cenv.asm allTyArgs)
            |> ImArray.concat

        let syntaxNode: OlySyntaxNode =
            match syntaxNameOpt with
            | Some(syntaxName) ->
                match syntaxName.Parent with
                | null -> syntaxName
                | syntaxParent -> 
                    match syntaxParent with
                    | :? OlySyntaxName as syntaxParentName ->
                        syntaxParentName
                    | _ ->
                        syntaxName
            | _ ->
                syntaxNode

        let enclosingTyArgs = value.Enclosing.TypeArguments
        let funcTyArgs = value.TypeArguments

        let syntaxEnclosingTyArgsOpt =
            syntaxTyArgsOpt 
            |> Option.bind (fun xs -> 
                if xs.Length > enclosingTyArgs.Length then
                    None
                else
                    Some(xs |> Seq.take enclosingTyArgs.Length |> ImArray.ofSeq)
            )

        let syntaxFuncTyArgsOpt =
            syntaxTyArgsOpt
            |> Option.bind (fun xs ->
                if xs.Length > (enclosingTyArgs.Length + funcTyArgs.Length) then
                    None
                else
                    Some(xs |> Seq.skip enclosingTyArgs.Length |> ImArray.ofSeq)
            )

        checkConstraintsForSolving 
            (SolverEnvironment.Create(cenv.diagnostics, env.benv)) 
            syntaxNode 
            syntaxEnclosingTyArgsOpt
            enclosingTyArgs
            syntaxFuncTyArgsOpt
            funcTyArgs
            witnessArgs
        witnessArgs
    )

let bindConstantExpression (cenv: cenv) (env: BinderEnvironment) expectedTyOpt (syntaxExpr: OlySyntaxExpression) =
    match syntaxExpr with
    | OlySyntaxExpression.Literal(syntaxLiteral) ->
        let expr = BoundExpression.Literal(BoundSyntaxInfo.User(syntaxLiteral, env.benv), bindLiteral cenv env expectedTyOpt syntaxLiteral)
        match expectedTyOpt with
        | Some expectedTy -> checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv)) expectedTy expr
        | _ -> ()
        expr
    | OlySyntaxExpression.Name(syntaxName) ->
        let resInfo = ResolutionInfo.Default
        let item = bindNameAsItem cenv env (Some syntaxExpr) None resInfo syntaxName
        match item with
        | ResolutionItem.Type(_, ty) when ty.IsTypeVariable ->
            let tyPar = ty.TryTypeParameter.Value
            let expr = BoundExpression.Literal(BoundSyntaxInfo.User(syntaxName, env.benv), BoundLiteral.Constant(ConstantSymbol.TypeVariable(tyPar)))
            match expectedTyOpt with
            | Some expectedTy -> checkExpressionType (SolverEnvironment.Create(cenv.diagnostics, env.benv)) expectedTy expr
            | _ -> ()
            expr
        | _ ->
            cenv.diagnostics.Error("Not a valid constant expression.", 10, syntaxExpr)
            BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))
    | OlySyntaxExpression.Typed(syntaxInnerExpr, _, syntaxTy) ->
        let ty = bindType cenv env (Some syntaxExpr) ResolutionTypeArity.Any syntaxTy
        bindConstantExpression cenv env (Some ty) syntaxInnerExpr
    | _ ->
        cenv.diagnostics.Error("Not a valid constant expression.", 10, syntaxExpr)
        BoundExpression.Error(BoundSyntaxInfo.User(syntaxExpr, env.benv))

let tryEvaluateFixedIntegralConstantExpression (cenv: cenv) (env: BinderEnvironment) (expr: BoundExpression) : (int64 * ConstantSymbol) voption =
    let handleConstantSymbol constantSymbol =
        match constantSymbol with
        | ConstantSymbol.UInt8(value) -> (int64 value, constantSymbol) |> ValueSome
        | ConstantSymbol.Int8(value) -> (int64 value, constantSymbol) |> ValueSome
        | ConstantSymbol.UInt16(value) -> (int64 value, constantSymbol) |> ValueSome
        | ConstantSymbol.Int16(value) -> (int64 value, constantSymbol) |> ValueSome
        | ConstantSymbol.UInt32(value) -> (int64 value, constantSymbol) |> ValueSome
        | ConstantSymbol.Int32(value) -> (int64 value, constantSymbol) |> ValueSome
        | ConstantSymbol.UInt64(value) -> (int64 value, constantSymbol) |> ValueSome
        | ConstantSymbol.Int64(value) -> (value, constantSymbol) |> ValueSome
        | _ -> ValueNone

    match expr with
    | BoundExpression.Literal(_, literal) ->
        match literal with
        | BoundLiteral.Constant(constantSymbol) when constantSymbol.Type.IsFixedInteger ->
            handleConstantSymbol constantSymbol
        | _ ->
            ValueNone
    | _ ->
        ValueNone

[<RequireQualifiedAccess>]
type ResolutionFormalItem =
    | None
    | Error
    | Type of TypeSymbol
    | Value of receiverTyOpt: TypeSymbol option * IValueSymbol
    | Namespace of INamespaceSymbol

/// Order of name resolution precedence:
///     - Type parameter (all|type-only)
///     - Namespace (all|type-only)
///     - Value (all)
///     - Type (all|type-only)
let private bindIdentifierWithNoReceiverAsFormalItem (cenv: cenv) (env: BinderEnvironment) syntaxNode (resInfo: ResolutionInfo) (ident: string) =
    match tryBindIdentifierAsTypeParameter cenv env syntaxNode resInfo.resTyArity ident with
    | Some tyPar ->
        ResolutionFormalItem.Type(tyPar.AsType)
    | _ ->
        if resInfo.InTypeOnlyContext then
            match env.benv.TryGetNamespace(ident) with
            | Some namespaceEnt ->
                ResolutionFormalItem.Namespace(namespaceEnt)
            | _ ->
                match tryBindIdentifierAsType cenv env syntaxNode resInfo.resTyArity ident with
                | Some ty -> ResolutionFormalItem.Type ty
                | _ ->
                    cenv.diagnostics.Error(sprintf "Type identifier '%s' not found in scope." ident, 10, syntaxNode)
                    ResolutionFormalItem.Type(invalidType())
        else
            match env.benv.TryGetNamespace(ident) with
            | Some namespaceEnt ->
                ResolutionFormalItem.Namespace(namespaceEnt)
            | _ ->
                let possibleAttrIdent =
                    if resInfo.InValueOnlyAttributeContext then
                        ident + "Attribute"
                    else
                        ident
                match tryBindIdentifierAsValueForExpression cenv env syntaxNode resInfo.resTyArity resInfo.resArgs resInfo.InPatternOnlyContext possibleAttrIdent with
                | Some(item) -> item
                | _ ->
                    match tryBindIdentifierAsType cenv env syntaxNode resInfo.resTyArity possibleAttrIdent with
                    | Some ty -> ResolutionFormalItem.Type ty
                    | _ ->
                        if resInfo.InValueOnlyAttributeContext then
                            // We did not find an attribute with the added "Attribute" postfix, try to find it normally.
                            bindIdentifierWithNoReceiverAsFormalItem cenv env syntaxNode (resInfo.UpdateContext(ResolutionContext.ValueOnly)) ident
                        else
                            cenv.diagnostics.Error(sprintf "Identifier '%s' not found in scope." ident, 10, syntaxNode)
                            ResolutionFormalItem.Error

let private determineConstructorOrTypeAsFormalItem (cenv: cenv) (env: BinderEnvironment) (ent: IEntitySymbol) (resArgs: ResolutionArguments) =
    let ctors = 
        match resArgs with
        | ResolutionArguments.NotAFunctionCall -> ImArray.empty
        | _ ->
            ent.FindMostSpecificIntrinsicFunctions(env.benv, QueryMemberFlags.Instance, FunctionFlags.Constructor, ent.Name)
            |> filterFunctionsForOverloadingPart1 env.benv ResolutionTypeArity.Any (resArgs.TryGetCount())
            |> ImArray.ofSeq

    if ctors.IsEmpty then
        // No constructors found, return the type.
        ResolutionFormalItem.Type(ent.AsType)
    else
        ResolutionFormalItem.Value(
            None,
            FunctionGroupSymbol.CreateIfPossible(ctors)
        )

let private bindIdentifierWithReceiverTypeAsFormalItemConstructorOrType (cenv: cenv) (env: BinderEnvironment) syntaxNode (receiverTy: TypeSymbol) resTyArity (resArgs: ResolutionArguments) isPatternContext (ident: string) =
    if isPatternContext then
        let value = bindIdentifierAsMemberValue cenv env syntaxNode true receiverTy resTyArity resArgs ((* isPatternContext *) true) ident
        ResolutionFormalItem.Value(Some receiverTy, value)
    else
        // Nested entities of receiverTy take precedence over its member values.
        receiverTy.TryFindNestedEntity(env.benv, ident, resTyArity)
        |> Option.map (fun nestedEnt ->
            determineConstructorOrTypeAsFormalItem cenv env nestedEnt resArgs
        )
        |> Option.defaultWith (fun () ->
            let value = bindIdentifierAsMemberValue cenv env syntaxNode true receiverTy resTyArity resArgs ((* isPatternContext *) false) ident
            ResolutionFormalItem.Value(Some receiverTy, value)
        )

let private bindIdentifierWithReceiverNamespaceAsFormalItem (cenv: cenv) (env: BinderEnvironment) (syntaxNode: OlySyntaxNode) (receiverNamespaceEnt: INamespaceSymbol) resTyArity (args: ResolutionArguments) (ident: string) =
    if System.String.IsNullOrWhiteSpace(ident) then
        if not cenv.syntaxTree.HasErrors then
            cenv.diagnostics.Error("Empty identifiers are not allowed.", 10, syntaxNode)
        ResolutionFormalItem.None
    else
        match receiverNamespaceEnt.TryFindNestedEntity(env.benv, ident, resTyArity) with
        | Some nestedEnt ->
            if nestedEnt.IsNamespace then
                ResolutionFormalItem.Namespace(nestedEnt)
            else
                determineConstructorOrTypeAsFormalItem cenv env nestedEnt args
        | _ ->
            ResolutionFormalItem.None

let private bindTypeOnlyIdentifierWithReceiverNamespaceAsFormalItem (cenv: cenv) (env: BinderEnvironment) syntaxNode (receiverNamespaceEnt: INamespaceSymbol) resTyArity (ident: string) =
    if System.String.IsNullOrWhiteSpace(ident) then
        if not cenv.syntaxTree.HasErrors then
            cenv.diagnostics.Error("Empty identifiers are not allowed.", 10, syntaxNode)
        ResolutionFormalItem.Error
    else
        match receiverNamespaceEnt.TryFindNestedEntity(env.benv, ident, resTyArity) with
        | Some nestedEnt ->
            if nestedEnt.IsNamespace then
                ResolutionFormalItem.Namespace(nestedEnt)
            else
                ResolutionFormalItem.Type(nestedEnt.AsType)
        | _ ->
            cenv.diagnostics.Error(sprintf "Type identifier '%s' not found on '%s'." ident (printEntity env.benv receiverNamespaceEnt), 10, syntaxNode)
            ResolutionFormalItem.Error

let private bindIdentifierWithReceiverTypeAsFormalItem (cenv: cenv) (env: BinderEnvironment) syntaxNode isStatic (receiverTy: TypeSymbol) (resTyArity: ResolutionTypeArity) (resArgs: ResolutionArguments) isPatternContext (ident: string) =
    if isStatic then
        bindIdentifierWithReceiverTypeAsFormalItemConstructorOrType cenv env syntaxNode receiverTy resTyArity resArgs isPatternContext ident
    else
        let value = bindIdentifierAsMemberValue cenv env syntaxNode isStatic receiverTy resTyArity resArgs isPatternContext ident
        ResolutionFormalItem.Value(Some receiverTy, value)

let private bindTypeOnlyIdentifierWithReceiverTypeAsFormalItem (cenv: cenv) (env: BinderEnvironment) syntaxNode (receiverTy: TypeSymbol) (resTyArity: ResolutionTypeArity) (ident: string) =
    if System.String.IsNullOrWhiteSpace(ident) then
        if not cenv.syntaxTree.HasErrors then
            cenv.diagnostics.Error("Empty identifiers are not allowed.", 10, syntaxNode)
        ResolutionFormalItem.Error
    else
        match receiverTy.TryFindNestedEntity(env.benv, ident, resTyArity) with
        | Some nestedEnt ->
            ResolutionFormalItem.Type(nestedEnt.AsType)
        | _ ->
            cenv.diagnostics.Error(sprintf "Type identifier '%s' not found on '%s'." ident (printType env.benv receiverTy), 10, syntaxNode)
            ResolutionFormalItem.Error

let private bindIdentifierWithReceiverAsFormalItem (cenv: cenv) (env: BinderEnvironment) syntaxNode (receiverInfo: ReceiverInfo) (resInfo: ResolutionInfo) (ident: string) =
    let isStatic = receiverInfo.isStatic
    let receiverItem = receiverInfo.item

    match receiverItem with
    | ReceiverItem.Type(receiverTy) ->
        if resInfo.InTypeOnlyContext && isStatic then
            bindTypeOnlyIdentifierWithReceiverTypeAsFormalItem cenv env syntaxNode receiverTy resInfo.resTyArity ident
        else
            bindIdentifierWithReceiverTypeAsFormalItem cenv env syntaxNode isStatic receiverTy resInfo.resTyArity resInfo.resArgs resInfo.InPatternOnlyContext ident
    
    | ReceiverItem.Namespace(receiverNamespaceEnt) ->
        if resInfo.InTypeOnlyContext && isStatic then
            bindTypeOnlyIdentifierWithReceiverNamespaceAsFormalItem cenv env syntaxNode receiverNamespaceEnt resInfo.resTyArity ident
        else
            bindIdentifierWithReceiverNamespaceAsFormalItem cenv env syntaxNode receiverNamespaceEnt resInfo.resTyArity resInfo.resArgs ident

let bindIdentifierAsFormalItem (cenv: cenv) (env: BinderEnvironment) syntaxNode (receiverInfoOpt: ReceiverInfo option) (resInfo: ResolutionInfo) (ident: string) =
    match receiverInfoOpt with
    | Some receiverInfo ->
        bindIdentifierWithReceiverAsFormalItem cenv env syntaxNode receiverInfo resInfo ident
    | _ ->
        bindIdentifierWithNoReceiverAsFormalItem cenv env syntaxNode resInfo ident

let bindValueAsFieldOrNotFunctionExpression (cenv: cenv) env (syntaxToCapture: OlySyntaxNode) (receiverInfoOpt: ReceiverInfo option) (syntaxNameOpt: OlySyntaxName option) (value: IValueSymbol) =
    let createValueExpr (value: IValueSymbol) =
        // If the value is invalid, we probably have a syntax name that isn't valid.
        // Therefore, try to get the syntax name from the main syntax node.
        if value.IsInvalid then
            match syntaxToCapture.TryName with
            | Some syntaxName ->
                BoundExpression.Value(BoundSyntaxInfo.User(syntaxName, env.benv), value)
            | _ ->
                BoundExpression.Value(BoundSyntaxInfo.Generated(cenv.syntaxTree), value)
        else
            OlyAssert.False(value.IsFunction)
            let syntaxInfo = 
                BoundSyntaxInfo.User(
                    syntaxToCapture, 
                    env.benv, 
                    syntaxNameOpt, 
                    receiverInfoOpt 
                    |> Option.bind (fun x ->
                        match x.item with
                        | ReceiverItem.Type(ty) -> Some ty
                        | _ -> None
                    )
                )
            BoundExpression.Value(syntaxInfo, value)

    match value with
    | :? IFieldSymbol as field ->
        match receiverInfoOpt with
        | Some({ expr = Some receiver }) ->
            let receiver2 =
                // We are only interested in taking the address of the receiver if it's a value.
                match receiver with
                | BoundExpression.Value _ ->
                    if field.Enclosing.IsType then
                        AddressOfReceiverIfPossible field.Enclosing.AsType receiver
                    else
                        receiver
                | _ ->
                    receiver
            BoundExpression.GetField(BoundSyntaxInfo.User(syntaxToCapture, env.benv), receiver2, syntaxNameOpt, field)
        | _ ->
            createValueExpr field

    | :? IPropertySymbol as prop ->
        match receiverInfoOpt with
        | Some({ expr = Some receiver }) ->
            let receiverExpr =
                if prop.Enclosing.IsType then
                    AddressOfReceiverIfPossible prop.Enclosing.AsType receiver
                else
                    receiver
            let expr = BoundExpression.GetProperty(BoundSyntaxInfo.User(syntaxToCapture, env.benv), Some(receiverExpr), syntaxNameOpt, freshenValue env.benv prop :?> IPropertySymbol)
            checkReceiverOfExpression (SolverEnvironment.Create(cenv.diagnostics, env.benv)) expr
            expr
        | _ ->
            BoundExpression.GetProperty(BoundSyntaxInfo.User(syntaxToCapture, env.benv), None, syntaxNameOpt, freshenValue env.benv prop :?> IPropertySymbol)
    | _ ->
        createValueExpr value

let bindNameAsFormalItem (cenv: cenv) env syntaxToCaptureOpt (receiverInfoOpt: ReceiverInfo option) resInfo (syntaxName: OlySyntaxName) =
    match syntaxName with
    | OlySyntaxName.Identifier(syntaxIdent) ->
        bindIdentifierAsFormalItem cenv env syntaxIdent receiverInfoOpt resInfo syntaxIdent.ValueText

    | OlySyntaxName.Parenthesis(_, syntaxIdent, _) ->
        bindIdentifierAsFormalItem cenv env syntaxIdent receiverInfoOpt resInfo syntaxIdent.ValueText

    | OlySyntaxName.Generic(syntaxName, syntaxTyArgs) ->
        let resInfo = { resInfo with syntaxTyArgs = Some(syntaxTyArgs, syntaxTyArgs.Values); resTyArity = ResolutionTypeArity.Create syntaxTyArgs.Count }
        bindNameAsFormalItem cenv env syntaxToCaptureOpt receiverInfoOpt resInfo syntaxName

    | OlySyntaxName.Qualified(syntaxHeadName, _, _) ->
        bindNameAsFormalItem cenv env syntaxToCaptureOpt receiverInfoOpt { ResolutionInfo.Default with resContext = resInfo.resContext } syntaxHeadName

    | _ ->
        raise(InternalCompilerException())

let bindValueAsCallExpressionWithSyntaxTypeArguments (cenv: cenv) (env: BinderEnvironment) (syntaxInfo: BoundSyntaxInfo) (receiverExprOpt: BoundExpression option) (argExprs: BoundExpression imarray) (syntaxTyArgsRoot: OlySyntaxNode, syntaxTyArgs: OlySyntaxType imarray) (originalValue: IValueSymbol) =
    let tyArgOffset =
        if not originalValue.IsInstance then
            0
        else
            match originalValue.Enclosing with
            | EnclosingSymbol.Entity(ent) ->
                let enclosingTyInst = env.benv.GetEnclosingTypeArguments(ent.Formal.Id)
                enclosingTyInst.Length
            | _ ->
                0
    let tyPars = originalValue.TypeParametersOrConstructorEnclosingTypeParameters
    let tyArgs = bindTypeArguments cenv env tyArgOffset tyPars (syntaxTyArgsRoot, syntaxTyArgs)

    let finalExpr, value =
        bindValueAsCallExpression cenv env syntaxInfo receiverExprOpt argExprs tyArgs originalValue

    match value.TryWellKnownFunction with
    | ValueSome(WellKnownFunction.Upcast) when argExprs.Length = 1 ->
        let syntax = syntaxInfo.SyntaxNameOrDefault
        if syntaxTyArgs.IsEmpty then
            cenv.diagnostics.Error("Intrinsic 'upcast' call must have explicit type arguments.", 10, syntax)

        let argTy = argExprs[0].Type
        let func = value :?> IFunctionSymbol
        let tyArgs = func.TypeArguments
        if tyArgs.Length = 1 then    
            if subsumesTypeWith Generalizable tyArgs[0] argTy then
                checkSubsumesType (SolverEnvironment.Create(cenv.diagnostics, env.benv)) syntax tyArgs[0] argTy
                finalExpr
            else
                match tryFindTypeHasTypeExtensionImplementedType env.benv tyArgs[0] argTy with
                | ValueSome ent ->
                    BoundExpression.Witness(finalExpr, ent.AsType, tyArgs[0])
                | _ ->
                    cenv.diagnostics.Error($"Unable to upcast type '{printType env.benv argTy}' to '{printType env.benv tyArgs[0]}.", 10, syntax)
                    finalExpr
        else
            finalExpr
    | _ ->
        finalExpr

let bindValueAsCallExpression (cenv: cenv) (env: BinderEnvironment) syntaxInfo (receiverExprOpt: BoundExpression option) (argExprs: BoundExpression imarray) (tyArgs: TypeArgumentSymbol imarray) (originalValue: IValueSymbol) : _ * IValueSymbol =
    let value = originalValue.Substitute(tyArgs)

    let isLastTyParVariadic =
        if originalValue.IsConstructor then
            match originalValue.Enclosing with
            | EnclosingSymbol.Entity(ent) ->
                let tyPars = ent.TypeParameters
                if tyPars.IsEmpty then
                    false
                else
                    tyPars[tyPars.Length - 1].IsVariadic
            | _ ->
                false
        else
            if originalValue.TypeParameters.IsEmpty then
                false
            else
                originalValue.TypeParameters[originalValue.TypeParameters.Length - 1].IsVariadic
            
    let argExprs =
        if isLastTyParVariadic then
            match value.LogicalType.TryFunction with
            | ValueSome(argTys, _) ->
                if argExprs.Length > argTys.Length then
                    let lastArgIndex = argTys.Length - 1
                    let headArgExprs = argExprs.RemoveRange(lastArgIndex, argExprs.Length - lastArgIndex)
                    let tailArgExprs = argExprs.RemoveRange(0, lastArgIndex)
                    headArgExprs.Add(
                        BoundExpression.NewTuple(BoundSyntaxInfo.Generated(cenv.syntaxTree),
                            tailArgExprs,
                            TypeSymbol.CreateTuple(tailArgExprs |> ImArray.map (fun x -> x.Type))
                        )
                    )
                else
                    argExprs
            | _ ->
                argExprs
        else
            argExprs

    let value, argExprs = 
        if value.IsFunction then
            let func, argExprs = ImplicitPassingArguments env.benv value.AsFunction argExprs
            (func :> IValueSymbol, argExprs)
        else
            (value, argExprs)

    let value = freshenAndCheckValue (SolverEnvironment.Create(cenv.diagnostics, env.benv)) argExprs syntaxInfo.Syntax value   
    let witnessArgs = createWitnessArguments cenv env syntaxInfo.Syntax syntaxInfo.TrySyntaxName value

    let receiverExprOpt =
        receiverExprOpt
        |> Option.map (fun expr -> 
            let expr = AutoDereferenceReceiverIfPossible expr // TODO/REVIEW: What is this for?
            if value.IsInstance then
                if value.Enclosing.IsType then
                    AddressOfReceiverIfPossible value.Enclosing.AsType expr
                else
                    expr
            else
                expr
        )
        
    let isVirtualCall =
        if value.IsVirtual then
            match receiverExprOpt with
            | Some(BoundExpression.Value(value=value)) ->
                not value.IsBase
            | _ ->
                true
        else
            false

    let receiverOpt =
        // Implicitly include 'this' as the receiver for the base constructor call.
        if value.IsBase && value.IsInstanceConstructor && receiverExprOpt.IsNone then
            match env.benv.senv.unqualifiedSymbols.TryGetValue "this" with
            | true, (UnqualifiedSymbol.Local thisValue) ->
                Some(BoundExpression.Value(BoundSyntaxInfo.Generated(cenv.syntaxTree), thisValue))
            | _ ->
                receiverExprOpt
        else
            receiverExprOpt

    match value with
    | :? IPropertySymbol as prop when prop.Getter.IsNone ->
        let syntaxNode = syntaxInfo.SyntaxNameOrDefault
        cenv.diagnostics.Error($"Property '{prop.Name}' cannot be used for an indirect call because it does not have a 'get' function.", 10, syntaxNode)
    | _ ->
        ()

    if value.IsProperty then
        let getPropertyExpr =
            E.GetProperty(
                syntaxInfo,
                receiverOpt,
                syntaxInfo.TrySyntaxName,
                value.AsProperty
            )

        let bridge = createLocalBridgeValue value.Type

        let callExpr = 
            BoundExpression.Call(
                syntaxInfo,
                None,
                CacheValueWithArg.FromValue(ImArray.empty),
                argExprs,
                bridge,
                false
            )

        let expr =
            E.Let(
                BoundSyntaxInfo.Generated(cenv.syntaxTree),
                BindingLocal(bridge),
                getPropertyExpr,
                callExpr
            )
        expr, value
    else
        let expr = 
            BoundExpression.Call(
                syntaxInfo,
                receiverOpt,
                witnessArgs,
                argExprs,
                value,
                isVirtualCall
            )
        expr, value

let bindMemberAccessExpressionAsItem (cenv: cenv) (env: BinderEnvironment) syntaxToCapture prevReceiverInfoOpt (syntaxReceiver: OlySyntaxExpression) (syntaxMemberExpr: OlySyntaxExpression) =
    match syntaxReceiver with
    | OlySyntaxExpression.Call(syntaxCallBodyExpr, syntaxArgs) ->
        let syntaxArgs = getSyntaxArgumentsAsSyntaxExpressions cenv syntaxArgs
        ResolutionItem.MemberCall(syntaxToCapture, prevReceiverInfoOpt, syntaxCallBodyExpr, syntaxArgs, Some syntaxMemberExpr)

    | OlySyntaxExpression.Indexer(syntaxReceiver, syntaxBrackets) ->
        OlyAssert.True(prevReceiverInfoOpt.IsNone)
        ResolutionItem.MemberIndexerCall(syntaxToCapture, syntaxReceiver, syntaxBrackets, Some syntaxMemberExpr, None)

    | OlySyntaxExpression.Name(syntaxName) ->
        let resTyArity = typeResolutionArityOfName syntaxName
        let resInfo = ResolutionInfo.Create(ValueNone, resTyArity, ResolutionContext.ValueOnly)
        bindMemberExpressionAsItem cenv env syntaxToCapture (bindNameAsFormalItem cenv env (Some syntaxReceiver) prevReceiverInfoOpt resInfo syntaxName |> Choice2Of2) syntaxMemberExpr

    | OlySyntaxExpression.Parenthesis(_, syntaxExprList, _) when prevReceiverInfoOpt.IsNone ->
        ResolutionItem.Parenthesis(syntaxToCapture, syntaxExprList, Some syntaxMemberExpr)

    | OlySyntaxExpression.Literal(syntaxLiteral) when prevReceiverInfoOpt.IsNone ->
        let syntaxInfo = BoundSyntaxInfo.User(syntaxLiteral, env.benv)
        let literal = bindLiteral cenv env None syntaxLiteral
        bindMemberExpressionAsItem cenv env syntaxToCapture (Choice1Of2(E.Literal(syntaxInfo, literal))) syntaxMemberExpr

    | _ ->
        ResolutionItem.Invalid(syntaxReceiver)

let bindMemberExpressionWithTypeAsItem (cenv: cenv) (env: BinderEnvironment) syntaxToCapture (ty: TypeSymbol) (syntaxMemberExpr: OlySyntaxExpression) : ResolutionItem =
    match syntaxMemberExpr with
    | OlySyntaxExpression.Call(syntaxCallBodyExpr, syntaxArgs) ->
        let syntaxArgs = getSyntaxArgumentsAsSyntaxExpressions cenv syntaxArgs
        let receiverInfo = { isStatic = true; item = ReceiverItem.Type(ty); expr = None }
        ResolutionItem.MemberCall(syntaxToCapture, Some receiverInfo, syntaxCallBodyExpr, syntaxArgs, None)
    | OlySyntaxExpression.Name(syntaxName) ->
        failwith "not implemented"
    | _ ->
        failwith "not implemented"

let bindMemberExpressionAsItem (cenv: cenv) (env: BinderEnvironment) (syntaxToCapture: OlySyntaxExpression) (receiverChoice: Choice<BoundExpression, ResolutionFormalItem>) (syntaxMemberExpr: OlySyntaxExpression) =
    let bind cenv env (receiver: BoundExpression) syntaxMemberExpr =
        let receiverInfo = { item = ReceiverItem.Type(receiver.Type); isStatic = false; expr = Some receiver }
        match syntaxMemberExpr with
        | OlySyntaxExpression.Name(syntaxName) ->
            let resTyArity = typeResolutionArityOfName syntaxName
            let resInfo = ResolutionInfo.Create(ValueNone, resTyArity, ResolutionContext.ValueOnly)
            let receiverInfo = { item = ReceiverItem.Type(receiver.Type); isStatic = false; expr = Some receiver }
            bindNameAsItem cenv env (Some syntaxToCapture) (Some receiverInfo) resInfo syntaxName

        | OlySyntaxExpression.Call(syntaxCallBodyExpr, syntaxArgs) ->
            let syntaxArgs = getSyntaxArgumentsAsSyntaxExpressions cenv syntaxArgs
            ResolutionItem.MemberCall(syntaxToCapture, Some receiverInfo, syntaxCallBodyExpr, syntaxArgs, None)

        | OlySyntaxExpression.MemberAccess(syntaxReceiver, _, syntaxMemberExpr) ->
            let receiverInfo = { item = ReceiverItem.Type(receiver.Type); isStatic = false; expr = Some receiver }
            bindMemberAccessExpressionAsItem cenv env syntaxToCapture (Some receiverInfo) syntaxReceiver syntaxMemberExpr

        | _ ->
            ResolutionItem.Invalid(syntaxMemberExpr)

    match receiverChoice with
    | Choice1Of2(receiver) ->
        match receiver with
        // For member accesses, we only want to undo auto-dereferencing if it is a struct type only.
        | AutoDereferenced(receiverAsAddr) when receiver.Type.IsAnyStruct ->
            bind cenv env receiverAsAddr syntaxMemberExpr
        | _ ->
            bind cenv env receiver syntaxMemberExpr
    | Choice2Of2(receiverItem) ->
        match receiverItem with
        | ResolutionFormalItem.None
        | ResolutionFormalItem.Namespace _ ->
            ResolutionItem.Invalid(syntaxMemberExpr)
        | ResolutionFormalItem.Error ->
            ResolutionItem.Error(syntaxMemberExpr)
        | ResolutionFormalItem.Value(_, value) ->
            bind cenv env (BoundExpression.CreateValue(cenv.syntaxTree, value)) syntaxMemberExpr
        | ResolutionFormalItem.Type ty ->
            bindMemberExpressionWithTypeAsItem cenv env syntaxToCapture ty syntaxMemberExpr

let tryBindNameAsNamespace (cenv: cenv) env (receiverInfoOpt: ReceiverInfo option) (syntaxName: OlySyntaxName) =
    if receiverInfoOpt.IsNone then
        env.benv.TryGetNamespace(syntaxName.LastIdentifier.ValueText)
    else
        None

let resolveFormalValue (cenv: cenv) env syntaxToCapture (syntaxNode: OlySyntaxNode) (receiverInfoOpt: ReceiverInfo option) (resInfo: ResolutionInfo) (syntaxTyArgsRoot: OlySyntaxNode, syntaxTyArgs: OlySyntaxType imarray) (value: IValueSymbol) =
    let syntaxNameOpt =
        match syntaxNode with
        | :? OlySyntaxName as syntaxName -> Some syntaxName
        | _ -> None

    let receiverExprOpt =
        receiverInfoOpt
        |> Option.bind (fun x -> x.expr)

    let syntaxInfo =
        BoundSyntaxInfo.User(
            syntaxToCapture, 
            env.benv, 
            syntaxNameOpt,
            receiverInfoOpt
            |> Option.bind (fun x ->
                match x.item with
                | ReceiverItem.Type(ty) -> Some ty
                | _ -> None
            )
        )

    match value with
    | :? IFunctionSymbol as func ->
        if func.IsFunctionGroup && resInfo.resArgs.IsExplicit then
            BoundExpression.Call(
                syntaxInfo,
                receiverExprOpt,
                CacheValueWithArg(fun _ _ -> ImArray.empty),
                resInfo.argExprs,
                func,
                false
            )
            |> ResolutionItem.Expression
        else

        if (resInfo.resArgs.IsExplicit || func.Semantic = GetterFunction || (resInfo.resArgs.IsAny_t && value.IsParameterLessFunction)) then
            let argExprs =
                if func.IsPatternFunction then
                    // Dummy argExprs.
                    match resInfo.resArgs with
                    | ResolutionArguments.ByType(argTys) ->
                        argTys |> ImArray.map (fun x -> E.Typed(BoundSyntaxInfo.Generated(cenv.syntaxTree), E.Error(BoundSyntaxInfo.Generated(cenv.syntaxTree)), x))
                    | _ ->
                        func.Parameters |> ImArray.map (fun _ -> BoundExpression.CreateValue(syntaxNode.Tree, invalidValue None))
                else
                    resInfo.argExprs

            bindValueAsCallExpressionWithSyntaxTypeArguments cenv env syntaxInfo receiverExprOpt argExprs (syntaxTyArgsRoot, syntaxTyArgs) value
            |> ResolutionItem.Expression
        else
            if resInfo.InPatternOnlyContext then
                cenv.diagnostics.Error("Invalid expression.", 10, syntaxNode)
                ResolutionItem.Error(syntaxNode)
            else
                BoundExpression.Value(BoundSyntaxInfo.User(syntaxNode, env.benv), func)
                |> ResolutionItem.Expression
    | _ ->
        // We do not have a function at this point, but we might have a value that is a function type.
        // However, if we are in a pattern-only context, we ignore the resolution args being explicit as
        // we might have a constant.
        if not syntaxTyArgs.IsEmpty || (resInfo.resArgs.IsExplicit && not resInfo.InPatternOnlyContext) then
            bindValueAsCallExpressionWithSyntaxTypeArguments cenv env syntaxInfo receiverExprOpt resInfo.argExprs (syntaxTyArgsRoot, syntaxTyArgs) value
            |> ResolutionItem.Expression
        else
            bindValueAsFieldOrNotFunctionExpression cenv env syntaxToCapture receiverInfoOpt syntaxNameOpt value
            |> ResolutionItem.Expression

let private bindNameAsItemAux (cenv: cenv) env (syntaxExprOpt: OlySyntaxExpression option) (receiverInfoOpt: ReceiverInfo option) resInfo (syntaxParentNameOpt: OlySyntaxName option) (syntaxRootName: OlySyntaxName) (syntaxTyArgsRoot: OlySyntaxNode, syntaxTyArgs: OlySyntaxType imarray) =
    let rec bind cenv env (receiverInfoOpt: ReceiverInfo option) resInfo syntaxRootName syntaxName =
        match syntaxName with
        | OlySyntaxName.Identifier _
        | OlySyntaxName.Parenthesis _ ->
            match bindNameAsFormalItem cenv env syntaxExprOpt receiverInfoOpt resInfo syntaxName with
            | ResolutionFormalItem.None ->
                // TODO: This path is a little weird for error recovery, if None means an error then why not just return Error?
                match receiverInfoOpt with
                | Some({ item = receiverItem }) ->
                    if not cenv.syntaxTree.HasErrors then
                        cenv.diagnostics.Error(sprintf "Identifier '%s' not found in scope." syntaxName.LastIdentifier.ValueText, 10, syntaxName)
                    match receiverItem with
                    | ReceiverItem.Type(ty) ->
                        match ty.TryEntity with
                        | ValueSome(ent) ->
                            ResolutionItem.Namespace(syntaxRootName, invalidNamespaceWithEnclosing ent.AsEnclosing)
                        | _ ->
                            ResolutionItem.Invalid(syntaxName)
                    | ReceiverItem.Namespace(namespaceEnt) ->
                        ResolutionItem.Namespace(syntaxRootName, invalidNamespaceWithEnclosing namespaceEnt.AsEnclosing)
                | _ ->
                    ResolutionItem.Invalid(syntaxName)
            | ResolutionFormalItem.Error ->
                if resInfo.InTypeOnlyContext then
                    match receiverInfoOpt with
                    | Some receiverInfo ->
                        match receiverInfo.item with
                        | ReceiverItem.Namespace(namespaceEnt) ->
                            ResolutionItem.Namespace(syntaxRootName, invalidNamespaceWithEnclosing namespaceEnt.AsEnclosing)
                        | ReceiverItem.Type(ty) ->
                            match ty.TryEntity with
                            | ValueSome ent ->
                                ResolutionItem.Type(syntaxName, (invalidEntityWithEnclosing ent.AsEnclosing).AsType)
                            | _ ->
                                ResolutionItem.Type(syntaxName, ty)
                    | _ ->
                        ResolutionItem.Type(syntaxName, invalidType())
                else
                    invalidExpression syntaxName env.benv
                    |> ResolutionItem.Expression

            | ResolutionFormalItem.Namespace namespaceEnt ->
                ResolutionItem.Namespace(syntaxName, namespaceEnt)

            | ResolutionFormalItem.Type ty ->
                let ty =
                    if resInfo.resTyArity.IsSecondOrder_t && syntaxTyArgs.IsEmpty && not ty.IsError_t then
                        let expectedTyArity = resInfo.resTyArity.TryArity.Value
                        let tyArity = ty.Arity
                        if expectedTyArity = tyArity then
                            ty
                        else
                            // This is effectively doing partial instantiation of a type.
                            let partialTyPars = 
                                ty.TypeArguments
                                |> ImArray.skip (tyArity - expectedTyArity)
                                |> ImArray.map (fun x ->
                                    x.TryTypeParameter.Value
                                )

                            let tyPars =
                                partialTyPars
                                |> ImArray.mapi (fun i tyPar -> 
                                    let newTyPar =
                                        TypeParameterSymbol(
                                            tyPar.Name,
                                            i,
                                            tyPar.Arity,
                                            TypeParameterKind.Type,
                                            ref ImArray.empty
                                        )
                                    newTyPar
                                )

                            let tyArgs =
                                (partialTyPars, tyPars)
                                ||> ImArray.map2 (fun partialTyPar tyPar ->
                                    mkSolvedInferenceVariableType partialTyPar tyPar.AsType
                                )

                            (partialTyPars, tyPars)
                            ||> ImArray.iter2 (fun partialTyPar tyPar ->
                                if partialTyPar.Constraints.IsEmpty |> not then
                                    tyPar.SetConstraints(
                                        partialTyPar.Constraints
                                        |> ImArray.map (fun constr ->
                                            actualConstraint tyArgs constr
                                        )
                                    )
                            )

                            OlyAssert.False(tyPars.IsEmpty)
                            TypeSymbol.ForAll(tyPars, ty.Substitute(tyArgs))
                    else
                        ty
                let ty = bindTypeConstructor cenv env syntaxName resInfo.resTyArity ty (syntaxTyArgsRoot, syntaxTyArgs)
                ResolutionItem.Type(syntaxName, ty)

            | ResolutionFormalItem.Value(_, value) ->
                let syntaxNode =
                    match syntaxExprOpt with
                    | Some syntaxExpr -> syntaxExpr :> OlySyntaxNode
                    | _ -> syntaxName :> OlySyntaxNode

                let syntaxRootName =
                    match syntaxParentNameOpt with
                    | Some(OlySyntaxName.Generic _ as syntaxRootName) -> syntaxRootName
                    | _ -> syntaxName

                resolveFormalValue cenv env syntaxNode syntaxRootName receiverInfoOpt resInfo (syntaxTyArgsRoot, syntaxTyArgs) value

        | OlySyntaxName.Generic(syntaxInnerName, syntaxTyArgs) ->
            let resInfo = 
                { resInfo with 
                    resTyArity = 
                        // We must preserve if we are in a higher order resolution context.
                        if resInfo.resTyArity.IsSecondOrder_t then
                            ResolutionTypeArity.SecondOrder(syntaxTyArgs.Count)
                        else
                            ResolutionTypeArity.Create syntaxTyArgs.Count 
                }
            bindNameAsItemAux cenv env syntaxExprOpt receiverInfoOpt resInfo (Some syntaxName) syntaxInnerName (syntaxTyArgs, syntaxTyArgs.Values)

        | OlySyntaxName.Qualified(syntaxHeadName, _, syntaxTailName) ->
            // We want types to take precedent for the 'syntaxHeadName'.
            // If a type is not resolved, then it will try to choose a value - local,field, or function.
            let newContext =
                match resInfo.resContext with
                | ResolutionContext.ValueOnly
                | ResolutionContext.ValueOnlyAttribute -> ResolutionContext.All
                | _ -> resInfo.resContext
            match bindNameAsFormalItem cenv env (Some syntaxExprOpt) receiverInfoOpt { ResolutionInfo.Default with resContext = newContext } syntaxHeadName with
            | ResolutionFormalItem.None ->
                ResolutionItem.Invalid(syntaxName)
            | ResolutionFormalItem.Error ->
                ResolutionItem.Error(syntaxName)

            | ResolutionFormalItem.Namespace(namespaceEnt) ->
                let receiverInfo = { item = ReceiverItem.Namespace(namespaceEnt); isStatic = true; expr = None }
                bind cenv env (Some receiverInfo) resInfo syntaxRootName syntaxTailName

            | ResolutionFormalItem.Type ty ->
                let ty = 
                    match syntaxHeadName with
                    | OlySyntaxName.Generic(_, syntaxTyArgsRoot) ->
                        let syntaxTyArgs = syntaxTyArgsRoot.Values
                        bindTypeConstructor cenv env syntaxHeadName ResolutionTypeArity.Any ty (syntaxTyArgsRoot, syntaxTyArgs)
                    | _ ->
                        ty

                let receiverInfo = { item = ReceiverItem.Type(ty); isStatic = true; expr = None }
                bind cenv env (Some receiverInfo) resInfo syntaxRootName syntaxTailName

            | ResolutionFormalItem.Value(_, value) ->
                match syntaxExprOpt with
                | None -> ResolutionItem.Invalid(syntaxName)
                | Some syntaxExpr ->
                    let receiverExprOpt =
                        if value.IsFunction then None
                        else
                            bindValueAsFieldOrNotFunctionExpression cenv env syntaxExpr receiverInfoOpt (Some syntaxHeadName) value 
                            |> Some
                    let receiverInfo = 
                        { 
                            item = ReceiverItem.Type(value.Type)
                            isStatic = false 
                            expr = receiverExprOpt
                        }
                    bind cenv env (Some receiverInfo) resInfo syntaxRootName syntaxTailName

        | _ ->
            raise(InternalCompilerException())

    bind cenv env receiverInfoOpt resInfo syntaxRootName syntaxRootName

let bindNameAsItem (cenv: cenv) (env: BinderEnvironment) (syntaxExprOpt: OlySyntaxExpression option) (receiverInfoOpt: ReceiverInfo option) (resInfo: ResolutionInfo) (syntaxRootName: OlySyntaxName) =
    bindNameAsItemAux cenv env syntaxExprOpt receiverInfoOpt resInfo None syntaxRootName (cenv.syntaxTree.DummyNode, ImArray.empty)

//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************
//*****************************************************************************************************************

let tryBindIdentifierAsTypeParameter (cenv: cenv) env (syntaxNode: OlySyntaxNode) (resTyArity: ResolutionTypeArity) (ident: string) : TypeParameterSymbol option =
    let result = env.benv.TryFindTypeParameter ident

    match result with
    | Some tyPar ->
        match resTyArity.TryArity with
        | ValueSome resTyArity ->
            // We allow type arities of zero as we may want to partially apply.
            // TODO/REVIEW: T<_>   -   T : Add     T<_>.add() or T.add()
            if tyPar.Arity <> resTyArity && resTyArity <> 0 then
                cenv.diagnostics.Error(sprintf "Second-order type parameter '%s' expected to have an arity of %i but got %i." (printType env.benv tyPar.AsType) tyPar.Arity resTyArity, 10, syntaxNode)
        | _ ->
            ()
    | _ ->
        ()

    result

let tryBindIdentifierAsType (cenv: cenv) (env: BinderEnvironment) (syntaxNode: OlySyntaxNode) (resTyArity: ResolutionTypeArity) (ident: string) =
    // Type parameters have precendence.
    match tryBindIdentifierAsTypeParameter cenv env syntaxNode resTyArity ident with
    | Some tyPar ->
        Some(TypeSymbol.Variable(tyPar))
    | _ ->
        let tys = 
            env.benv.GetUnqualifiedType(ident, resTyArity, 
                fun ty ->
                    if env.isInOpenDeclaration then
                        ty.Enclosing.IsRootNamespaceEnclosing ||
                        ty.Enclosing.IsAnonymousModule
                    else
                        true            
            )
        if tys.IsEmpty then
            None
        elif tys.Length = 1 then
            Some(tys[0])
        else
            let ty = tys[0]
            let refs = 
                tys
                |> ImArray.map (fun x -> $"'{printEnclosing env.benv x.Enclosing}'")
                |> String.concat ", "
            cenv.diagnostics.Error($"'{printType env.benv ty}' is ambiguous due to references: {refs}.", 10, syntaxNode)
            Some(TypeSymbolError)

let bindIdentifierAsType (cenv: cenv) (env: BinderEnvironment) (syntaxNode: OlySyntaxNode) resTyArity (ident: string) =
    match tryBindIdentifierAsType cenv env syntaxNode resTyArity ident with
    | Some ty -> ty
    | _ -> 
        if canReportMissingIdentifier cenv ident then
            cenv.diagnostics.Error(sprintf "Type '%s' does not exist in the current scope." ident, 0, syntaxNode)
        invalidType()

[<RequireQualifiedAccess>]
type PossibleValues =
    | Function of IFunctionSymbol
    | FunctionGroup of FunctionGroupSymbol
    | Value of IValueSymbol
    | None

let tryFindIdentifierAsPossibleValues (cenv: cenv) (env: BinderEnvironment) syntaxNode (ident: string) =
    match env.benv.TryGetUnqualifiedValue(ident) with
    | Some unqualified ->
        match unqualified with
        | UnqualifiedSymbol.Local value -> 
            PossibleValues.Value(value)
        | UnqualifiedSymbol.Field value ->
            PossibleValues.Value(value)
        | UnqualifiedSymbol.Property value ->
            PossibleValues.Value(value)
        | UnqualifiedSymbol.Function func ->
            PossibleValues.Function(func)
        | UnqualifiedSymbol.FunctionGroup(funcGroup) ->
            PossibleValues.FunctionGroup(funcGroup)

        | UnqualifiedSymbol.AmbiguousValues(values) ->
            if values.IsEmpty then
                PossibleValues.None
            elif values.Length = 1 then
                PossibleValues.Value(values[0])
            else
                let refs =
                    values
                    |> ImArray.map (fun x -> $"'{printEnclosing env.benv x.Enclosing}'")
                    |> String.concat ", "
                cenv.diagnostics.Error($"'{ident}' is ambiguous as they reference {refs}", 10, syntaxNode)
                PossibleValues.None

    | _ ->
        PossibleValues.None

let tryBindIdentifierAsValue (cenv: cenv) (env: BinderEnvironment) syntaxNode (ident: string) : IValueSymbol option =
    let possibleValues = tryFindIdentifierAsPossibleValues cenv env syntaxNode ident
    match possibleValues with
    | PossibleValues.None -> None
    | PossibleValues.Value value -> Some (value)
    | PossibleValues.Function func -> Some (func :> IValueSymbol)
    | PossibleValues.FunctionGroup funcGroup -> Some (funcGroup :> IValueSymbol)

let bindIdentifierAsValue (cenv: cenv) (env: BinderEnvironment) syntaxNode (args: ImmutableArray<ArgumentInfo>) (ident: string) =
    match tryBindIdentifierAsValue cenv env syntaxNode ident with
    | Some value -> value
    | _ -> 
        if canReportMissingIdentifier cenv ident then
            cenv.diagnostics.Error(sprintf "'%s' does not exist in scope." ident, 0, syntaxNode)

        if args.IsEmpty then
            invalidValue None
        else
            invalidFunction () :> IValueSymbol

let bindIdentifierAsMemberValue (cenv: cenv) (env: BinderEnvironment) (syntaxNode: OlySyntaxNode) isStatic (ty: TypeSymbol) resTyArity resArgs isPatternContext (ident: string) =
    let ty = stripByReference ty

    let value =
        let queryMemberFlags =
            if isStatic then
                QueryMemberFlags.Static
            else
                QueryMemberFlags.Instance

        let funcs =
            findMostSpecificFunctionsOfType env.benv queryMemberFlags FunctionFlags.None (Some ident) QueryFunction.IntrinsicAndExtrinsic ty
            |> filterFunctionsForOverloadingPart1 env.benv resTyArity (resArgs.TryGetCount())
            |> ImArray.filter (fun x -> x.IsPatternFunction = isPatternContext)
        if not funcs.IsEmpty then
            FunctionGroupSymbol.CreateIfPossible(funcs) :> IValueSymbol
        else
            let queryField = QueryField.IntrinsicAndExtrinsic
            let fields = ty.FindFields(env.benv, queryMemberFlags, queryField, ident) |> List.ofSeq

            match fields with
            | [] ->
                let propOpt = ty.FindProperties(env.benv, queryMemberFlags, queryField, ident) |> Seq.tryHead
                match propOpt with
                | Some prop -> prop :> IValueSymbol
                | _ ->

                if canReportMissingIdentifier cenv ident && not ty.IsError_t then
                    if System.String.IsNullOrWhiteSpace(ident) then
                        cenv.diagnostics.Error(sprintf "Type '%s' does not contain any members." (printType env.benv ty), 0, syntaxNode)
                    else
                        cenv.diagnostics.Error(sprintf "Member '%s' does not exist on type '%s'." ident (printType env.benv ty), 0, syntaxNode)

                if funcs.IsEmpty then
                    invalidField ident (Some ty) :> IValueSymbol
                else
                    let principalFunc = funcs.[0]
                    let fakeParCount = 
                        match resArgs.TryGetCount() with
                        | ValueSome(count) -> count
                        | _ -> 0
                    FunctionGroupSymbol(principalFunc.Name, funcs |> ImArray.ofSeq, fakeParCount) :> IValueSymbol
            | [field] ->
                field :> IValueSymbol
            | _ ->
                cenv.diagnostics.Error(sprintf "'%s' has ambiguous fields on type '%s'." ident (printType env.benv ty), 0, syntaxNode)
                invalidField ident (Some ty) :> IValueSymbol

    match value.Enclosing, stripTypeEquations ty with
    | EnclosingSymbol.Entity(ent), TypeSymbol.Variable(_) ->
        value.WithEnclosing(EnclosingSymbol.Witness(ty, ent))
    | EnclosingSymbol.Entity(ent), TypeSymbol.HigherVariable(_, tyArgs) ->
        if ent.IsTypeConstructor then
            let appliedEnt =
                if ent.IsFormal then
                    ent.Apply(tyArgs)
                else
                    OlyAssert.Equal(ent.TypeParameters.Length, tyArgs.Length)
                    ent
            value.WithEnclosing(EnclosingSymbol.Witness(ty, appliedEnt))
        else
            value.WithEnclosing(EnclosingSymbol.Witness(ty, ent))
    | _ ->
        // TODO: This is weird, we should make sure there is an error when a value is invalid.
        if value.IsInvalid then
            match ty with
            | TypeSymbol.Entity(ent) ->
                value.WithEnclosing(EnclosingSymbol.Entity(ent))
            | TypeSymbol.Variable(tyPar) ->
                value.WithEnclosing(EnclosingSymbol.Witness(ty, invalidEntity))
            | _ ->
                value
        else
            value

let bindIdentifierAsTypeVariable (cenv: cenv) env (syntaxIdent: OlySyntaxToken) =
    let ident = syntaxIdent.ValueText

    match env.benv.TryFindTypeParameter ident with
    | Some tyPar ->
        tyPar.AsType
    | _ ->
        if canReportMissingIdentifier cenv ident then
            cenv.diagnostics.Error(sprintf "Type parameter '%s' not found." ident, 0, syntaxIdent)
        invalidType ()

let bindIdentifierAsHigherTypeVariable (cenv: cenv) env (tys: ImmutableArray<TypeSymbol>) (syntaxIdent: OlySyntaxToken) =
    let ident = syntaxIdent.ValueText

    match env.benv.TryFindTypeParameter ident with
    | Some tyPar ->
        
        if tyPar.Arity <> tys.Length then
            cenv.diagnostics.Error(sprintf "Type '%s' has a type argument arity of %i and not %i." tyPar.Name tys.Length tyPar.Arity, 10, syntaxIdent)

        TypeSymbol.HigherVariable(tyPar, tys)
    | _ ->
        if canReportMissingIdentifier cenv ident then
            cenv.diagnostics.Error(sprintf "Type parameter '%s' not found." ident, 0, syntaxIdent)
        invalidType ()

let tryBindIdentifierAsValueForExpression cenv env (syntaxNode: OlySyntaxNode) (resTyArity: ResolutionTypeArity) (resArgs: ResolutionArguments) isPatternContext (ident: string) =
    match tryBindIdentifierAsValue cenv env syntaxNode ident with
    | Some value ->
        match value with
        | :? FunctionGroupSymbol as funcGroup ->
            // TODO: Umm, what is this? We have similar crap elsewhere..
            match resArgs with
            | ResolutionArguments.NotAFunctionCall -> None
            | _ ->

            Assert.ThrowIfNot(funcGroup.Functions.Length <> 1)

            let funcs =
                funcGroup.Functions
                |> ImArray.filter (fun x -> x.IsPatternFunction = isPatternContext)
                |> filterFunctionsForOverloadingPart1 env.benv resTyArity (resArgs.TryGetCount())

            if funcs.IsEmpty then
                None
            elif funcs.Length = 1 then
                ResolutionFormalItem.Value(None, funcs[0]) |> Some
            else
                ResolutionFormalItem.Value(None, FunctionGroupSymbol.Create(funcs)) |> Some

        | _ ->
            if value.IsFunction then
                match resArgs with
                | ResolutionArguments.NotAFunctionCall -> None
                | _ -> 
                    if value.AsFunction.IsPatternFunction = isPatternContext then
                        ResolutionFormalItem.Value(None, value) |> Some
                    else
                        None
            else
                ResolutionFormalItem.Value(None, value) |> Some
    | _ ->
        None

let bindNameAsNamespace (cenv: cenv) env (syntaxName: OlySyntaxName) =
    let resInfo = ResolutionInfo.Create(ValueNone, ResolutionTypeArityZero, ResolutionContext.TypeOnly)
    match bindNameAsItem cenv env None None resInfo syntaxName with
    | ResolutionItem.Namespace(_syntaxName, namespaceEnt) ->
        namespaceEnt : INamespaceSymbol
    | resItem ->
        cenv.diagnostics.Error("Not a valid namespace.", 10, resItem.Syntax)
        invalidNamespace

let bindNameAsType (cenv: cenv) env syntaxExprOpt (resTyArity: ResolutionTypeArity) (syntaxName: OlySyntaxName) =
    let resInfo = ResolutionInfo.Create(ValueNone, resTyArity, ResolutionContext.TypeOnly)
    match bindNameAsItem cenv env syntaxExprOpt None resInfo syntaxName with
    | ResolutionItem.Type(_syntaxName, ty) ->
        ty
    | ResolutionItem.Namespace(syntaxName, namespaceEnt) ->
        cenv.diagnostics.Error("Not a valid type.", 10, syntaxName)
        namespaceEnt.AsNamespaceType
    | resItem ->
        cenv.diagnostics.Error("Not a valid type.", 10, resItem.Syntax)
        invalidType()

let bindReturnTypeAnnotation (cenv: cenv) env syntaxTyAnnot =
    match syntaxTyAnnot with
    | OlySyntaxReturnTypeAnnotation.TypeAnnotation(_, syntaxTy) ->
        bindType cenv env None ResolutionTypeArityZero syntaxTy
    | _ ->
        match currentEnclosing env with
        | EnclosingSymbol.Local ->
            mkInferenceVariableType None
        | _ ->
            // Non-local declarations with no type annotations will error.
            // REVIEW: This may change in the future when we start to allow 'let' declarations
            //         on top-level constructs.
            TypeSymbolError

let bindType (cenv: cenv) env syntaxExprOpt (resTyArity: ResolutionTypeArity) (syntaxTy: OlySyntaxType) =
    let rec bind cenv env resTyArity isFuncInput syntaxTy =
        match syntaxTy with
        | OlySyntaxType.Name(syntaxName) ->
            bindNameAsType cenv env syntaxExprOpt resTyArity syntaxName

        | OlySyntaxType.Tuple(_, syntaxTupleElementList, _) ->
            match syntaxTupleElementList.ChildrenOfType |> List.ofSeq with
            | [] -> TypeSymbol.Unit
            | [syntaxElement] -> 
                let ty =
                    match syntaxElement with
                    | OlySyntaxTupleElement.Type(syntaxTy) ->
                        bind cenv env resTyArity false syntaxTy
                    | OlySyntaxTupleElement.IdentifierWithTypeAnnotation(syntaxIdent, _, syntaxTy) ->
                        cenv.diagnostics.Error("A single element tuple cannot have a name.", 10, syntaxIdent)
                        bind cenv env resTyArity false syntaxTy
                    | OlySyntaxTupleElement.Error _ ->
                        TypeSymbolError
                    | _ ->
                        raise(InternalCompilerUnreachedException())

                if isFuncInput then
                    // TODO: Kind of a hack using TypeSymbol.Tuple.
                    TypeSymbol.Tuple(ImArray.createOne ty, ImArray.empty)
                else
                    ty

            | syntaxElements ->
                let names = ImArray.builder()
                let tys = 
                    syntaxElements
                    |> Seq.map (fun syntaxElement ->
                        match syntaxElement with
                        | OlySyntaxTupleElement.Type(syntaxTy) ->
                            names.Add("")
                            bind cenv env resTyArity false syntaxTy
                        | OlySyntaxTupleElement.IdentifierWithTypeAnnotation(syntaxIdent, _, syntaxTy) ->
                            names.Add(syntaxIdent.ValueText)
                            bind cenv env resTyArity false syntaxTy
                        | OlySyntaxTupleElement.Error _ ->
                            names.Add("")
                            TypeSymbolError
                        | _ ->
                            raise(InternalCompilerUnreachedException())
                    )
                    |> ImmutableArray.CreateRange

                let names = names.ToImmutable()
                let hasNoNames =
                    names
                    |> ImArray.forall String.IsNullOrWhiteSpace

                if hasNoNames then
                    TypeSymbol.Tuple(tys, ImArray.empty)
                else
                    TypeSymbol.Tuple(tys, names)

        | OlySyntaxType.Variadic(syntaxIdent, _)
        | OlySyntaxType.VariadicIndexer(syntaxIdent, _, _, _, _) ->
            let ty = bindIdentifierAsType cenv env syntaxIdent resTyArity syntaxIdent.ValueText
            let isVariadicTyPar =
                match ty.TryTypeParameter with
                | ValueSome tyPar -> tyPar.IsVariadic
                | _ -> false

            if not isVariadicTyPar then
                cenv.diagnostics.Error("Expected variadic type variable.", 10, syntaxIdent)
                TypeSymbolError
            else
                match syntaxTy with
                | OlySyntaxType.VariadicIndexer(_, _, _, syntaxConstExpr, _) ->
                    let constExpr = bindConstantExpression cenv env None syntaxConstExpr
                    let literalOpt =
                        match constExpr with
                        | BoundExpression.Literal(_, BoundLiteral.NumberInference(lazyLiteral, _)) ->
                            match tryEvaluateLazyLiteral cenv.diagnostics lazyLiteral with
                            | ValueSome literal -> Some literal
                            | _ -> None
                        | BoundExpression.Literal(_, literal) -> literal |> Some
                        | _ -> None
                    match literalOpt with
                    | Some(BoundLiteral.Constant(ConstantSymbol.Int32(value))) ->
                        TypeSymbol.DependentIndexer(TypeSymbol.ConstantInt32(value), ty)
                    | Some(BoundLiteral.Constant(ConstantSymbol.TypeVariable(tyPar))) ->
                        TypeSymbol.DependentIndexer(tyPar.AsType, ty)
                    | _ ->
                        cenv.diagnostics.Error("Expected a 32-bit constant integer.", 10, syntaxConstExpr)
                        TypeSymbolError
                | _ ->
                    ty

        | OlySyntaxType.Array(syntaxElementTy, syntaxBrackets) ->
            let rank = syntaxBrackets.Element.Children.Length + 1
            let elementTy = bind cenv env resTyArity false syntaxElementTy
            if rank > 1 then
                TypeSymbol.CreateArray(elementTy, rank)
            else
                TypeSymbol.CreateArray(elementTy)

        | OlySyntaxType.MutableArray(syntaxElementTy, syntaxBracketPipes) ->
            let rank = syntaxBracketPipes.Element.Children.Length + 1
            let elementTy = bind cenv env resTyArity false syntaxElementTy
            if rank > 1 then
                TypeSymbol.CreateMutableArray(elementTy, rank)
            else
                TypeSymbol.CreateMutableArray(elementTy)

        | OlySyntaxType.Shape(syntaxCurlyBrackets) ->
            cenv.bindAnonymousShapeTypeHole cenv env ImArray.empty syntaxCurlyBrackets.Element

        | OlySyntaxType.Function(syntaxInputTy, _, syntaxOutputTy) ->
            let inputTy = bind cenv env resTyArity true syntaxInputTy
            
            let argTys =
                match inputTy with
                | TypeSymbol.Tuple(itemTys, _) -> itemTys
                | TypeSymbol.Unit -> ImArray.empty
                | _ -> ImArray.createOne inputTy
            let returnTy = bind cenv env resTyArity false syntaxOutputTy

            TypeSymbol.CreateFunction(ImmutableArray.Empty, argTys, returnTy)

        | OlySyntaxType.FunctionPtr(_, syntaxBlittableOptional, syntaxInputTy, _, syntaxOutputTy) ->
            let inputTy = bind cenv env resTyArity true syntaxInputTy

            let argTys =
                match inputTy with
                | TypeSymbol.Tuple(itemTys, _) -> itemTys
                | TypeSymbol.Unit -> ImArray.empty
                | _ -> ImArray.createOne inputTy
            let returnTy = bind cenv env resTyArity false syntaxOutputTy
            let ilCallConv =
                match syntaxBlittableOptional with
                | OlySyntaxBlittableOptional.Some(OlySyntaxBlittable.Blittable _) ->
                    Oly.Metadata.OlyILCallingConvention.Blittable
                | _ ->
                    Oly.Metadata.OlyILCallingConvention.Default
            TypeSymbol.CreateFunctionPtr(ilCallConv, argTys, returnTy)

        | OlySyntaxType.WildCard _ ->
            if env.resolutionMustSolveTypes then
                cenv.diagnostics.Error("Inferring types are not allowed in this context, be explicit.", 10, syntaxTy)
                TypeSymbolError
            else
                mkInferenceVariableType None

        | OlySyntaxType.Postfix(syntaxElementTy, syntaxIdent) ->
            match resTyArity with
            | ResolutionTypeArityZero
            | ResolutionTypeArity.Any -> ()
            | _ -> cenv.diagnostics.Error("Explicit type arguments using '<' and '>' are not allowed on prefix types.", 10, syntaxTy)

            let ty = bindIdentifierAsType cenv env syntaxIdent (ResolutionTypeArity.FirstOrder 1) syntaxIdent.ValueText
            let elementTy = bind cenv env ResolutionTypeArity.Any false syntaxElementTy
            if ty.Arity = 1 then
                applyType ty (ImArray.createOne elementTy)
            else
                TypeSymbolError

        | OlySyntaxType.Literal(syntaxLiteral) ->
            match bindLiteral cenv env None syntaxLiteral with
            | BoundLiteral.Constant(ConstantSymbol.Int32(value)) ->
                TypeSymbol.ConstantInt32(value)
            | BoundLiteral.NumberInference(lazyLiteral, literalTy) ->
                checkTypes (SolverEnvironment.Create(cenv.diagnostics, env.benv)) syntaxLiteral TypeSymbol.Int32 literalTy
                match tryEvaluateLazyLiteral cenv.diagnostics lazyLiteral with
                | ValueSome literal ->
                    match literal with
                    | BoundLiteral.Constant(ConstantSymbol.Int32(value)) ->
                        TypeSymbol.ConstantInt32(value)
                    | _ ->
                        cenv.diagnostics.Error("Invalid type literal.", 10, syntaxLiteral)
                        TypeSymbolError
                | _ ->
                    TypeSymbolError
            | _ ->
                cenv.diagnostics.Error("Invalid type literal.", 10, syntaxLiteral)
                TypeSymbolError

        | OlySyntaxType.Error _ ->
            TypeSymbolError

        | _ ->
            raise(InternalCompilerUnreachedException())

    bind cenv env resTyArity false syntaxTy

let bindTypeConstructor cenv env (syntaxNode: OlySyntaxNode) (resTyArity: ResolutionTypeArity) (ty: TypeSymbol) (syntaxTyArgsRoot, syntaxTyArgs: OlySyntaxType imarray) =
    if resTyArity.IsSecondOrder_t && syntaxTyArgs.IsEmpty then
        if not ty.IsTypeConstructor then
            cenv.diagnostics.Error($"'{printType env.benv ty}' is not a type constructor.", 10, syntaxNode)
        ty
    else
        let tyArities =
            if ty.IsError_t (* error recovery *) then
                ImArray.init syntaxTyArgs.Length (fun _ -> ResolutionTypeArity.Any)
            else
                match ty.TryTypeParameter with
                | ValueSome tyPar ->
                    ImArray.init tyPar.Arity (fun _ -> ResolutionTypeArityZero)
                | _ ->
                    ty.LogicalTypeParameters
                    |> ImArray.choose (fun x ->                       
                        if x.IsHidden then 
                            // Handles generic local type definitions.
                            None
                        else
                            if x.Arity = 0 then
                                ResolutionTypeArityZero
                                |> Some
                            else
                                ResolutionTypeArity.SecondOrder x.Arity
                                |> Some
                    )

        // Wild card open declaration rules
        let hasOpenDeclWildCard =
            if env.isInOpenDeclaration && not env.resolutionMustSolveTypes then
                syntaxTyArgs
                |> ImArray.exists (fun x ->
                    match x with
                    | OlySyntaxType.WildCard _ -> true
                    | _ -> false
                )
            else
                false

        if hasOpenDeclWildCard then
            let isValid =
                syntaxTyArgs
                |> ImArray.forall (fun x ->
                    match x with
                    | OlySyntaxType.WildCard _ -> true
                    | _ -> false
                )
            if not isValid then
                cenv.diagnostics.Error("Open declarations using one or more wild cards, '_', requires using wild cards for all type arguments.", 10, syntaxTyArgsRoot)

        let partialTyInst: TypeSymbol imarray = 
            if hasOpenDeclWildCard then
                ImArray.empty
            else
                let env = 
                    if env.isInOpenDeclaration then
                        env.SetResolutionMustSolveTypes()
                    else
                        env
                bindTypeArgumentsAsTypes cenv env tyArities (syntaxTyArgsRoot, syntaxTyArgs)

        let tyArgs =
            if hasOpenDeclWildCard then
                ImArray.empty
            else
                if ty.TypeParameters.Length > partialTyInst.Length then
                    let enclosingTyInst =
                        ty.TypeArguments
                        |> Seq.take (ty.TypeParameters.Length - partialTyInst.Length)
                        |> Seq.map (fun x ->
                            match x with
                            // Handles generic local type definitions.
                            | TypeSymbol.Variable(tyPar) when tyPar.HiddenLink.IsSome ->
                                tyPar.HiddenLink.Value.AsType
                            | _ ->
                                x
                        )
                        |> ImArray.ofSeq
                    enclosingTyInst.AddRange(partialTyInst)
                else
                    partialTyInst

        let ty =
            if hasOpenDeclWildCard then
                ty

            // TODO: This check is a bit weird. A type parameter who is a generic type constructor, T<_>, will not have any type parameters, but it will have arity. Fix this.
            elif ty.Arity = tyArgs.Length then
                ty.Apply(tyArgs)
            else
                if not ty.IsError_t (* error recovery *) then
                    if ty.TypeParameters.Length <> tyArgs.Length || (ty.HasTypeVariableArity && resTyArity.IsZero) then
                        let syntaxNode =
                            if syntaxTyArgsRoot.IsDummy then
                                syntaxNode
                            else
                                syntaxTyArgsRoot
                        cenv.diagnostics.Error("Type argument count do not match the type parameter count.", 10, syntaxNode)
                ty

        if not env.skipCheckTypeConstructor then
            checkTypeConstructor (SolverEnvironment.Create(cenv.diagnostics, env.benv)) syntaxTyArgsRoot syntaxTyArgs ty
        if resTyArity.IsSecondOrder_t && not ty.IsTypeConstructor then
            cenv.diagnostics.Error($"'{printType env.benv ty}' is not a type constructor.", 10, syntaxNode)

        ty

let bindTypeAndInferConstraints (cenv: cenv) (env: BinderEnvironment) syntaxTy =
    match syntaxTy with
    | OlySyntaxType.Name(OlySyntaxName.Generic(syntaxName, syntaxTyArgsRoot) as syntaxTyName) ->
        let syntaxTyArgs = syntaxTyArgsRoot.Values

        let lookup = ResizeArray()

        let envWithTyPars =
            let mutable i = env.EnclosingTypeParameters.Length
            (env, syntaxTyArgs)
            ||> Seq.fold (fun env syntaxTyArg ->
                let env =
                    match syntaxTyArg with
                    | OlySyntaxType.Name(OlySyntaxName.Identifier(syntaxIdent)) ->
                        let constrsHole = ref ImArray.empty
                        let tyPar = TypeParameterSymbol(syntaxIdent.ValueText, i, 0, TypeParameterKind.Type, constrsHole)
                        lookup.Add constrsHole
                        addTypeParameter cenv env syntaxTyArg tyPar |> fst
                    | OlySyntaxType.Name(OlySyntaxName.Generic(OlySyntaxName.Identifier(syntaxIdent), syntaxTyArgsRoot)) ->
                        let syntaxTyArgs = syntaxTyArgsRoot.Values
                        checkSyntaxHigherTypeArguments cenv syntaxTyArgs
                        let constrsHole = ref ImArray.empty
                        let tyPar = TypeParameterSymbol(syntaxIdent.ValueText, i, syntaxTyArgs.Length, TypeParameterKind.Type, constrsHole)
                        lookup.Add constrsHole
                        addTypeParameter cenv env syntaxTyArg tyPar |> fst
                    | _ ->
                        env
                i <- i + 1
                env
            )

        let resTyArity =
            if syntaxTyArgs.Length = 0 then
                ResolutionTypeArityZero
            else
                ResolutionTypeArity.FirstOrder syntaxTyArgs.Length
            
        let ty = bindNameAsType cenv envWithTyPars None resTyArity syntaxTyName

        ty.TypeParameters
        |> Seq.iteri (fun i tyPar ->
            let constrsHole = lookup.[i]
            constrsHole.contents <- constrsHole.contents.AddRange(tyPar.Constraints)
        )

        envWithTyPars, ty
    | _ ->
        env, bindType cenv env None ResolutionTypeArityZero syntaxTy

let bindTypeArgument (cenv: cenv) env (tyPars: ImmutableArray<TypeParameterSymbol>) isLastTyParVariadic (offset: int) (n: int) (syntaxTyArg: OlySyntaxType) =
    let index = offset + n
    let tyPar = 
        if isLastTyParVariadic && index >= tyPars.Length then
            tyPars.[tyPars.Length - 1]
        else
            tyPars.[index]
    let ty =
        match syntaxTyArg with
        | OlySyntaxType.WildCard _ ->
            mkInferenceVariableType (Some tyPar)
        | _ ->
            let resTyArity =
                if tyPar.Arity > 0 then
                    ResolutionTypeArity.SecondOrder(tyPar.Arity)
                else
                    ResolutionTypeArityZero
            bindType cenv env None resTyArity syntaxTyArg
    mkSolvedInferenceVariableType tyPar ty

let bindTypeArguments (cenv: cenv) (env: BinderEnvironment) offset (tyPars: ImmutableArray<TypeParameterSymbol>) (syntaxTyArgsRoot: OlySyntaxNode, syntaxTyArgs: OlySyntaxType imarray) : TypeArgumentSymbol imarray =
    let expectedTypeParameterCount = tyPars.Length - offset

    if syntaxTyArgs.IsEmpty then
        tyPars
        |> ImArray.map (fun tyPar -> tyPar.AsType)
    else
        let isLastTyParVariadic = 
            if tyPars.IsEmpty then
                false
            else
                tyPars.[tyPars.Length - 1].IsVariadic

        let isValid =
            if isLastTyParVariadic then
                syntaxTyArgs.Length < expectedTypeParameterCount
            else
                syntaxTyArgs.Length <> expectedTypeParameterCount

        if isValid then
            cenv.diagnostics.Error(sprintf "Expected '%i' type argument(s) but got '%i'." expectedTypeParameterCount syntaxTyArgs.Length, 0, syntaxTyArgsRoot)
            tyPars |> Seq.map (fun tyPar -> mkInferenceVariableType (Some tyPar)) |> ImmutableArray.CreateRange
        else

            let tyArgsTail = 
                syntaxTyArgs 
                |> Seq.mapi (bindTypeArgument cenv env tyPars isLastTyParVariadic offset)

            let tyArgs =
                if offset > 0 then
                    let tyArgsHead =
                        tyPars
                        |> Seq.take offset
                        |> Seq.map (fun tyPar -> tyPar.AsType)

                    Seq.append tyArgsHead tyArgsTail
                    |> ImArray.ofSeq
                else
                    tyArgsTail |> ImmutableArray.CreateRange

            if isLastTyParVariadic && syntaxTyArgs.Length <> expectedTypeParameterCount then
                let lastIndex = tyPars.Length - 1
                let headTyArgs = tyArgs.RemoveRange(lastIndex, tyArgs.Length - lastIndex)
                let tailTyArgs = tyArgs.RemoveRange(0, lastIndex)
                headTyArgs.Add(mkSolvedInferenceVariableType tyPars[lastIndex] (TypeSymbol.CreateTuple(tailTyArgs)))
            else
                tyArgs
                

let bindTypeArgumentAsType (cenv: cenv) (env: BinderEnvironment) resTyArity (syntaxTyArg: OlySyntaxType) =
    bindType cenv env None resTyArity syntaxTyArg

let bindTypeArgumentsAsTypes (cenv: cenv) (env: BinderEnvironment) (tyArities: ImmutableArray<ResolutionTypeArity>) (syntaxTyArgsRoot, syntaxTyArgs: OlySyntaxType imarray) =
    if syntaxTyArgs.IsEmpty then
        // TODO/REVIEW: Should we get rid of this?
        ImArray.empty
    else
        if tyArities.Length <> syntaxTyArgs.Length then
            cenv.diagnostics.Error(sprintf "Expected '%i' type argument(s) but got '%i'." tyArities.Length syntaxTyArgs.Length, 0, syntaxTyArgsRoot)
            ImArray.empty
        else
            (tyArities, syntaxTyArgs)
            ||> ImArray.map2 (fun resTyArity syntaxTyArg -> bindTypeArgumentAsType cenv env resTyArity syntaxTyArg)

let bindTypeParameter (cenv: cenv) (env: BinderEnvironment) tyParIndex tyParKind (syntaxTyPar: OlySyntaxType) =
    let name, higherArity, isVariadic =
        match syntaxTyPar with
        | OlySyntaxType.Variadic(syntaxIdent, _) -> syntaxIdent.ValueText, 0, true
        | OlySyntaxType.Name(OlySyntaxName.Identifier(syntaxIdent)) -> syntaxIdent.ValueText, 0, false
        | OlySyntaxType.Name(OlySyntaxName.Generic(OlySyntaxName.Identifier(syntaxIdent), syntaxTyArgsRoot)) ->
            let syntaxTyArgs = syntaxTyArgsRoot.Values
            checkSyntaxHigherTypeArguments cenv syntaxTyArgs
            syntaxIdent.ValueText, syntaxTyArgs.Length, false
        | OlySyntaxType.WildCard _ ->
            cenv.diagnostics.Error("Invalid use of wild card in type parameter definition.", 10, syntaxTyPar)
            "", 0, false
        | _ -> 
            "", 0, false

    let tyPar = TypeParameterSymbol(name, tyParIndex, higherArity, isVariadic, tyParKind, ref ImArray.empty)
    recordTypeParameterDeclaration cenv tyPar syntaxTyPar
    addTypeParameter cenv env syntaxTyPar tyPar
    
let bindTypeParameters (cenv: cenv) (env: BinderEnvironment) isFunc (syntaxTyPars: OlySyntaxType imarray) =
    let env1, tyPars =
        let env1, tyPars =
            let tyParIndexOffset = env.EnclosingTypeParameters.Length
            let mutable tyParIndex = tyParIndexOffset
            let mutable i = 0
            ((env, []), syntaxTyPars)
            ||> ImArray.fold (fun (env, tyPars) syntaxTyPar ->
                let tyParKind =
                    if isFunc then
                        TypeParameterKind.Function i
                    else
                        TypeParameterKind.Type
                let env, tyPar = bindTypeParameter cenv env tyParIndex tyParKind syntaxTyPar
                tyParIndex <- tyParIndex + 1
                i <- i + 1
                env, tyPars @ [tyPar]
            )
        
        env1, (tyPars |> ImArray.ofSeq)

    if isFunc then
        env1, tyPars
    else
        // REVIEW: This is a little weird, but ok.
        match currentEnclosing env1 with
        | EnclosingSymbol.Entity(ent) ->
            env1, ent.TypeParameters.AddRange(tyPars)
        | _ ->
            env1, env1.EnclosingTypeParameters.AddRange(tyPars)

let bindConstraint (cenv: cenv) (env: BinderEnvironment) (tyPar: TypeParameterSymbol) (resTyArity: ResolutionTypeArity) syntaxConstr =
    match syntaxConstr with
    | OlySyntaxConstraint.Type(syntaxConstrTy) ->

        let resTyArity2 =
            if resTyArity.IsAny_t then
                ResolutionTypeArity.Create syntaxConstrTy.ExplicitTypeArgumentCount
            else
                if syntaxConstrTy.ExplicitTypeArgumentCount > 0 then
                    cenv.diagnostics.Error(sprintf "Constraint type is expected to be a type constructor with an arity of '%i'." resTyArity.TryArity.Value, 10, syntaxConstrTy)
                resTyArity

        let constrTy = bindType cenv env None resTyArity2 syntaxConstrTy
        let constrTy =
            if tyPar.HasArity && resTyArity.IsSecondOrder_t then
                // interface IInterface<T>
                // ...
                // where U<_>: IInterface
                if constrTy.TypeParameters |> ImArray.exists (fun tyPar -> tyPar.HasArity) then
                    // interface IInterface<T<_>>
                    // ...
                    // where U<_>: IInterface
                    //
                    // Note: We do not support "higher-rank" types.
                    cenv.diagnostics.Error($"'{printType env.benv constrTy}' cannot be a partially applied constraint as it contains second-order generic type parameters.", 10, syntaxConstrTy)
                constrTy
            else
                constrTy

        let constrTy =
            match constrTy.TryEntity with
            | ValueSome(ent) when ent.IsShape && ent.IsAnonymous ->
                if ent.TypeParameters.IsEmpty |> not then
                    failwith "Did not expect type parameters for anonymous shape"

                let freeTyPars = constrTy.GetFreeTypeParameters()

                if freeTyPars.IsEmpty then
                    constrTy
                else
                    if freeTyPars.IsEmpty then
                        constrTy
                    else
                        let tyPars =
                            freeTyPars
                            |> ImArray.mapi (fun i tyPar ->
                                TypeParameterSymbol(tyPar.Name, i, tyPar.Arity, TypeParameterKind.Type, ref ImArray.empty)
                            )

                        let tyArgs =
                            freeTyPars
                            |> ImArray.map (fun x -> x.AsType)

                        match syntaxConstrTy with
                        | OlySyntaxType.Shape(syntaxCurlyBrakcets) ->
                            let constrTy = cenv.bindAnonymousShapeTypeHole cenv env tyPars syntaxCurlyBrakcets.Element
                            constrTy.Apply(tyArgs)
                        | _ ->
                            raise(InternalCompilerUnreachedException())
            | _ ->
                constrTy

        let constr = ConstraintSymbol.SubtypeOf(Lazy<_>.CreateFromValue(constrTy))

        if env.EnclosingTypeParameters |> Seq.exists (fun tyPar2 -> tyPar2.Index = tyPar.Index) then
            cenv.diagnostics.Error(sprintf "Cannot add additional constraints to the captured type parameter '%s'." tyPar.DisplayName, 10, syntaxConstr)
        else
            tyPar.AddConstraint(constr)

    | OlySyntaxConstraint.ConstantType(_, syntaxTy) ->
        let resTyArity2 =
            if resTyArity.IsAny_t then
                ResolutionTypeArity.Create syntaxTy.ExplicitTypeArgumentCount
            else
                if syntaxTy.ExplicitTypeArgumentCount > 0 then
                    cenv.diagnostics.Error(sprintf "Constraint type is expected to be a type constructor with an arity of '%i'." resTyArity.TryArity.Value, 10, syntaxTy)
                resTyArity

        let constTy = bindType cenv env None resTyArity2 syntaxTy
        let constr = ConstraintSymbol.ConstantType(Lazy<_>.CreateFromValue(constTy))
        match stripTypeEquations constTy with
        | TypeSymbol.Int32 ->
            tyPar.AddConstraint(constr)
        | _ ->
            cenv.diagnostics.Error($"'{printType env.benv constTy}' is not a supported constant type.", 10, syntaxTy)         

    | OlySyntaxConstraint.NotStruct _ ->
        tyPar.AddConstraint(ConstraintSymbol.NotStruct)

    | OlySyntaxConstraint.Struct _ ->
        tyPar.AddConstraint(ConstraintSymbol.Struct)

    | OlySyntaxConstraint.Null _ ->
        tyPar.AddConstraint(ConstraintSymbol.Null)

    | OlySyntaxConstraint.Unmanaged _ ->
        tyPar.AddConstraint(ConstraintSymbol.Unmanaged)

    | OlySyntaxConstraint.Error _ ->
        ()

    | _ ->
        raise(InternalCompilerUnreachedException())

let bindConstraintClause (cenv: cenv) (env: BinderEnvironment) (hash: HashSet<_>) (syntaxConstrClause: OlySyntaxConstraintClause) =
    match syntaxConstrClause with
    | OlySyntaxConstraintClause.ConstraintClause(_, syntaxTy, _, syntaxConstrList) ->
        let ty, isTyCtor = 
            match syntaxTy with
            | OlySyntaxType.Name(syntaxName) ->
                let isTyCtor =
                    match syntaxName with
                    | OlySyntaxName.Generic _ -> true
                    | _ -> false
                bindType cenv env None ResolutionTypeArity.Any syntaxTy, isTyCtor
            | _ ->
                TypeSymbolError, false

        let tyPar =
            match ty with
            | TypeSymbol.Variable(tyPar) -> 
                tyPar
            | TypeSymbol.HigherVariable(tyPar, tyArgs) ->
                if tyArgs |> Seq.exists (fun x -> x.IsSolved) then
                    cenv.diagnostics.Error("A type parameter with a generic instantiation is not allowed.", 10, syntaxTy)
                tyPar
            | _ ->
                cenv.diagnostics.Error("Expected a type parameter for the constraint.", 10, syntaxTy)
                invalidTypeParameter TypeParameterKind.Type

        if not (hash.Add(struct(tyPar.Id, isTyCtor))) then
            cenv.diagnostics.Error(sprintf "'%s' already has specified constraints." (printType env.benv tyPar.AsType), 10, syntaxTy)
        else
            let resTyArity =
                if tyPar.HasArity then
                    match syntaxTy with
                    | OlySyntaxType.Name(OlySyntaxName.Generic _) ->
                        ResolutionTypeArity.SecondOrder tyPar.Arity
                    | _ ->
                        ResolutionTypeArity.Any
                else
                    ResolutionTypeArity.Any

            syntaxConstrList.ChildrenOfType
            |> ImArray.iter (fun syntaxConstr -> bindConstraint cenv { env with isInConstraint = true } tyPar resTyArity syntaxConstr)

    | OlySyntaxConstraintClause.Error _ ->
        ()

    | _ ->
        raise(InternalCompilerException())

let bindConstraintClauseList (cenv: cenv) (env: BinderEnvironment) (syntaxConstrClauseList: OlySyntaxSeparatorList<OlySyntaxConstraintClause>) =
    let hash = HashSet()

    let syntaxConstrClauses =
        syntaxConstrClauseList.ChildrenOfType

    syntaxConstrClauses
    |> ImArray.iter (fun syntaxConstrClause ->
        // Binding a constraint clause will not check the type constructors of the constraints.
        bindConstraintClause cenv env hash syntaxConstrClause
    )

/// Determine member flags and value explicitness based on the modifiers and kind.
/// Performs validation on the modifiers and kind.
let bindValueModifiersAndKindAsMemberFlags 
        (cenv: cenv) 
        (env: BinderEnvironment) 
        isStaticProp
        (syntaxValueDeclPremodifiers: OlySyntaxValueDeclarationPremodifier imarray) 
        (syntaxValueDeclKind: OlySyntaxValueDeclarationKind) 
        (syntaxValueDeclPostmodifiers: OlySyntaxValueDeclarationPostmodifier imarray) : _ * ValueExplicitness =

    let enclosing = currentEnclosing env

    let mutable isExplicitConstant = false
    let mutable isExplicitStatic = isStaticProp
    let mutable isExplicitAbstract = false
    let mutable isExplicitOverrides = false
    let mutable isExplicitDefault = false
    let mutable isExplicitLet = false
    let mutable isExplicitMutable = false
    let mutable isExplicitGet = false
    let mutable isExplicitSet = false
    let mutable isExplicitPattern = false
    let mutable isExplicitNew = false

    match syntaxValueDeclKind with
    | OlySyntaxValueDeclarationKind.Constant _ ->
        isExplicitConstant <- true
    | OlySyntaxValueDeclarationKind.Let _
    | OlySyntaxValueDeclarationKind.LetBind _ ->
        isExplicitLet <- true
    | OlySyntaxValueDeclarationKind.Pattern _ ->
        isExplicitPattern <- true
    | _ ->
        ()

    // REVIEW: We could find a better way to check validation by looking at a particular shape.
    //         But, right now is also fine as there are not that many modifiers.
    // Check valid use of modifiers.
    // Expected premodifier order:
    //     static abstract overrides default mutable

    syntaxValueDeclPremodifiers
    |> ImArray.iter (function
        | OlySyntaxValueDeclarationPremodifier.Static(syntaxToken) ->
            if isExplicitStatic || isExplicitAbstract || isExplicitOverrides || isExplicitDefault || isExplicitMutable || isExplicitNew then
                cenv.diagnostics.Error("Invalid use of 'static' premodifier.", 10, syntaxToken)
            isExplicitStatic <- true
        | OlySyntaxValueDeclarationPremodifier.Abstract(syntaxToken) ->
            if isExplicitAbstract || isExplicitOverrides || isExplicitDefault || isExplicitMutable || isExplicitNew then
                cenv.diagnostics.Error("Invalid use of 'abstract' premodifier.", 10, syntaxToken)
            isExplicitAbstract <- true
        | OlySyntaxValueDeclarationPremodifier.Overrides(syntaxToken) ->
            if isExplicitOverrides || isExplicitDefault || isExplicitMutable || isExplicitNew then
                cenv.diagnostics.Error("Invalid use of 'overrides' premodifier.", 10, syntaxToken)
            isExplicitOverrides <- true
        | OlySyntaxValueDeclarationPremodifier.Default(syntaxToken) ->
            if isExplicitDefault || isExplicitMutable || isExplicitNew then
                cenv.diagnostics.Error("Invalid use of 'default' premodifier.", 10, syntaxToken)
            isExplicitDefault <- true
        | OlySyntaxValueDeclarationPremodifier.Mutable(syntaxToken) ->
            if isExplicitMutable || isExplicitNew then
                cenv.diagnostics.Error("Invalid use of 'mutable' premodifier.", 10, syntaxToken)
            if isExplicitLet then
                cenv.diagnostics.Error("'mutable' premodifers cannot be used on 'let' declarations. Use 'mutable' as a postmodifier instead.", 10, syntaxToken)
            isExplicitMutable <- true
        | OlySyntaxValueDeclarationPremodifier.New(syntaxToken) ->
            if isExplicitNew then
                cenv.diagnostics.Error("Invalid use of 'new' premodifier.", 10, syntaxToken)
            if isExplicitLet then
                cenv.diagnostics.Error("'new' premodifers cannot be used on 'let' declarations.", 10, syntaxToken)
            isExplicitNew <- true
        | _ ->
            ()
    )

    if isExplicitAbstract && isExplicitOverrides then
        let syntaxAbstractToken =
            syntaxValueDeclPremodifiers
            |> ImArray.pick (function OlySyntaxValueDeclarationPremodifier.Abstract(syntaxToken) -> Some syntaxToken | _ -> None)
        let syntaxOverridesToken =
            syntaxValueDeclPremodifiers
            |> ImArray.pick (function OlySyntaxValueDeclarationPremodifier.Overrides(syntaxToken) -> Some syntaxToken | _ -> None)
        cenv.diagnostics.Error("'abstract' and 'overrides' premodifiers cannot be used together.", 10, syntaxAbstractToken)
        cenv.diagnostics.Error("'abstract' and 'overrides' premodifiers cannot be used together.", 10, syntaxOverridesToken)
        isExplicitAbstract <- false // Make this false for better error recovery.

    syntaxValueDeclPostmodifiers
    |> ImArray.iter (function
        | OlySyntaxValueDeclarationPostmodifier.Mutable(syntaxMutableToken) ->
            if not isExplicitLet then
                cenv.diagnostics.Error("'mutable' postmodifers can only be used on 'let' declarations.", 10, syntaxMutableToken)
            isExplicitMutable <- true
        | _ ->
            ()
    )

    let valueExplicitness =
        {
            IsExplicitConstant = isExplicitConstant
            IsExplicitStatic = isExplicitStatic
            IsExplicitAbstract = isExplicitAbstract
            IsExplicitOverrides = isExplicitOverrides
            IsExplicitDefault = isExplicitDefault
            IsExplicitLet = isExplicitLet
            IsExplicitMutable = isExplicitMutable
            IsExplicitGet = isExplicitGet
            IsExplicitSet = isExplicitSet
            IsExplicitPattern = isExplicitPattern
            IsExplicitNew = isExplicitNew
        }

    if isExplicitConstant then
        cenv.diagnostics.Error("Constants are not supported (yet).", 10, syntaxValueDeclKind)

    if isExplicitPattern then
        if isExplicitConstant ||
           isExplicitStatic ||
           isExplicitAbstract ||
           isExplicitOverrides ||
           isExplicitDefault ||
           isExplicitLet ||
           isExplicitMutable ||
           isExplicitGet ||
           isExplicitSet ||
           isExplicitNew then
            cenv.diagnostics.Error("Invalid modifiers for 'pattern' declaration.", 10, syntaxValueDeclKind)

        MemberFlags.None, valueExplicitness

    elif enclosing.IsModule then
        if isExplicitStatic then
            cenv.diagnostics.Error("Module members are always implicitly static. Remove 'static'.", 10, syntaxValueDeclKind)

        if isExplicitDefault then
            cenv.diagnostics.Error("Module members must always have an implementation. Remove 'default'.", 10, syntaxValueDeclKind)

        if isExplicitAbstract then
            cenv.diagnostics.Error("Module members can never be abstract. Remove 'abstract'.", 10, syntaxValueDeclKind)

        if isExplicitOverrides then
            cenv.diagnostics.Error("Module members can never override. Remove 'overrides'.", 10, syntaxValueDeclKind)

        if isExplicitNew then
            cenv.diagnostics.Error("Module members can never have hide over existing members.", 10, syntaxValueDeclKind)

        if isExplicitLet then
            cenv.diagnostics.Error("Modules can never have let-bound members (yet).", 10, syntaxValueDeclKind)

        MemberFlags.None, valueExplicitness

    elif enclosing.IsInterface then
        if isExplicitStatic && isExplicitNew then
            cenv.diagnostics.Error("Interface static members can never have hide over existing members.", 10, syntaxValueDeclKind)

        if isExplicitOverrides then
            cenv.diagnostics.Error("Interface members can never override. Remove 'overrides'.", 10, syntaxValueDeclKind)

        if isExplicitLet then
            cenv.diagnostics.Error("Interfaces can never have let-bound members (yet).", 10, syntaxValueDeclKind)

        let memberFlags =
            if isExplicitStatic then
                if isExplicitAbstract then
                    if isExplicitDefault then
                        MemberFlags.Virtual
                    else
                        MemberFlags.Abstract
                else
                    if isExplicitDefault then
                        cenv.diagnostics.Error("Static interface members must always have an implementation unless marked 'abstract'. Remove 'default'.", 10, syntaxValueDeclKind)

                    MemberFlags.None
            else
                if isExplicitAbstract then
                    cenv.diagnostics.Error("Non-static interface members are always implicitly abstract. Remove 'abstract'.", 10, syntaxValueDeclKind)

                let memberFlags = MemberFlags.NewSlot ||| MemberFlags.Instance
                if isExplicitDefault then
                    memberFlags ||| MemberFlags.Virtual
                else
                    memberFlags ||| MemberFlags.Abstract

        memberFlags, valueExplicitness
            
    elif enclosing.IsShape then
        if isExplicitAbstract || isExplicitDefault ||
           isExplicitLet || isExplicitOverrides || isExplicitNew then
            cenv.diagnostics.Error("Shape members are always public and either static or not; they do not require any other modifiers.", 10, syntaxValueDeclKind)

        if isExplicitStatic then
            MemberFlags.Abstract, valueExplicitness
        else
            MemberFlags.Instance ||| MemberFlags.Abstract, valueExplicitness

    elif enclosing.IsTypeExtension then
        if isExplicitNew then
            cenv.diagnostics.Error("Type extension members always hide over existing members.", 10, syntaxValueDeclKind)

        if isExplicitDefault then
            cenv.diagnostics.Error("Type extension members will always have an implementation. Remove 'default'.", 10, syntaxValueDeclKind)

        if isExplicitAbstract then
            cenv.diagnostics.Error("Type extension members are never abstract. Remove 'abstract'.", 10, syntaxValueDeclKind)

        if isExplicitOverrides && not isExplicitStatic then
            cenv.diagnostics.Error("Type extension members can never override instance members. Remove 'overrides'.", 10, syntaxValueDeclKind)

        if isExplicitLet then
            cenv.diagnostics.Error("Type extensions can never have let-bound members (yet).", 10, syntaxValueDeclKind)

        let memberFlags =
            if isExplicitStatic then
                if isExplicitOverrides then
                    MemberFlags.Sealed ||| MemberFlags.Virtual ||| MemberFlags.ExplicitOverrides
                else
                    MemberFlags.None
            else
                MemberFlags.Sealed ||| MemberFlags.NewSlot ||| MemberFlags.Instance

        memberFlags, valueExplicitness

    else
        match enclosing with
        | EnclosingSymbol.Local ->
            if not isExplicitLet then
                cenv.diagnostics.Error("Invalid local value.", 10, syntaxValueDeclKind)

            MemberFlags.Private, valueExplicitness
        | _ ->
            if isExplicitLet then
                cenv.diagnostics.Error("Types can never have let-bound members (yet).", 10, syntaxValueDeclKind)

            if isExplicitStatic && isExplicitNew then
                cenv.diagnostics.Error("Static members can never have hide over existing members.", 10, syntaxValueDeclKind)

            if isExplicitNew && (isExplicitAbstract || isExplicitDefault || isExplicitOverrides) then
                cenv.diagnostics.Error("Invalid modifiers with the 'new' modifier.", 10, syntaxValueDeclKind)

            let memberFlags =
                if isExplicitAbstract && isExplicitDefault then
                    if isExplicitStatic then
                        MemberFlags.Virtual
                    else
                        MemberFlags.Virtual ||| MemberFlags.NewSlot ||| MemberFlags.Instance
                else
                    if isExplicitAbstract then
                        cenv.diagnostics.Error("Members cannot be abstract without a default implemenation. Remove 'abstract' or add 'default'.", 10, syntaxValueDeclKind)

                    if isExplicitDefault then
                        cenv.diagnostics.Error("Members must always have an implementation. Use 'default' on members who are abstract to have overridable members.", 10, syntaxValueDeclKind)

                    if isExplicitStatic then
                        if isExplicitOverrides then
                            MemberFlags.Sealed ||| MemberFlags.Virtual ||| MemberFlags.ExplicitOverrides
                        else
                            MemberFlags.None
                    else
                        if isExplicitOverrides then
                            MemberFlags.Virtual ||| MemberFlags.Instance ||| MemberFlags.ExplicitOverrides
                        else
                            MemberFlags.Instance

            let memberFlags =
                if isExplicitNew && not isExplicitStatic then
                    memberFlags ||| MemberFlags.NewSlot
                else
                    memberFlags

            memberFlags, valueExplicitness

//\'	Single quote	0x0027
//\"	Double quote	0x0022
//\\	Backslash	0x005C
//\0	Null	0x0000
//\a	Alert	0x0007
//\b	Backspace	0x0008
//\f	Form feed	0x000C
//\n	New line	0x000A
//\r	Carriage return	0x000D
//\t	Horizontal tab	0x0009
//\v	Vertical tab	0x000B
//\u	Unicode escape sequence (UTF-16)	\uHHHH (range: 0000 - FFFF; example: \u00E7 = "ç")
//\U	Unicode escape sequence (UTF-32)	\U00HHHHHH (range: 000000 - 10FFFF; example: \U0001F47D = "👽")
//\x	Unicode escape sequence similar to "\u" except with variable length	\xH[H][H][H] (range: 0 - FFFF; example: \x00E7 or \x0E7 or \xE7 = "ç")
let rec private unescapeTextAux cenv syntaxNode syntaxOffset (s: ReadOnlySpan<char>) (builder: System.Text.StringBuilder) =
    let checkHex (c: char) =
        (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')
            
    if s.IsEmpty then
        builder.ToString()
    else
        match s[0] with
        | '\\' ->
            if s.Length < 2 then
                cenv.diagnostics.Error("Invalid escape sequence.", 10, syntaxOffset, 1, syntaxNode)
                builder.ToString()
            else
                let v, skip =
                    match s[1] with
                    | ''' ->    0x0027u, 1
                    | '"' ->    0x0022u, 1
                    | '\\' ->   0x005Cu, 1
                    | '0' ->    0x0000u, 1
                    | 'a' ->    0x0007u, 1
                    | 'b' ->    0x0008u, 1
                    | 'f' ->    0x000Cu, 1
                    | 'n' ->    0x000Au, 1
                    | 'r' ->    0x000Du, 1
                    | 't' ->    0x0009u, 1
                    | 'v' ->    0x000Bu, 1
                    | 'u' ->
                        if s.Length < 6 then
                            cenv.diagnostics.Error("Invalid unicode escape sequence (UTF-16).", 10, syntaxOffset, 2, syntaxNode)
                            0x0000u, 1
                        else
                            let hex = s.Slice(2, 4)
                            let mutable isError = false
                            for c in hex do
                                if not(checkHex c) then
                                    isError <- true

                            if isError then
                                cenv.diagnostics.Error("Invalid hexadecimal value.", 10, syntaxOffset + 2, 4, syntaxNode)
                                0x0000u, 5
                            else
                                uint32(System.UInt16.Parse(hex, Globalization.NumberStyles.HexNumber)), 5
                    | 'U' ->
                        if s.Length < 10 then
                            cenv.diagnostics.Error("Invalid unicode escape sequence (UTF-32).", 10, syntaxOffset, 2, syntaxNode)
                            0x0000u, 1
                        else
                            let hex = s.Slice(2, 8)
                            let mutable isError = false
                            for c in hex do
                                if not(checkHex c) then
                                    isError <- true

                            if isError then
                                cenv.diagnostics.Error("Invalid hexadecimal value.", 10, syntaxOffset + 2, 8, syntaxNode)
                                0x0000u, 9
                            else
                                System.UInt32.Parse(hex, Globalization.NumberStyles.HexNumber), 9
                    | 'x' ->
                        if s.Length < 3 then
                            cenv.diagnostics.Error("Invalid unicode escape sequence (UTF-16).", 10, syntaxOffset, 2, syntaxNode)
                            0x0000u, 1
                        else
                            // This is kinda of hacky.
                            let possibleHex = s.Slice(2)
                            let mutable hexCount = 0
                            let mutable stop = false
                            for i = 0 to (min 3 (possibleHex.Length - 1)) do
                                if not stop && checkHex possibleHex[i] then
                                    hexCount <- hexCount + 1
                                else
                                    stop <- true
                            if hexCount = 0 then
                                cenv.diagnostics.Error("Invalid unicode escape sequence (UTF-16).", 10, syntaxOffset, 2, syntaxNode)
                                0x0000u, 1
                            else
                                uint32(System.UInt16.Parse(possibleHex.Slice(0, hexCount), Globalization.NumberStyles.HexNumber)), 1 + hexCount
                    | _ ->
                        cenv.diagnostics.Error("Invalid escape sequence.", 10, syntaxOffset, 2, syntaxNode)
                        0x0000u, 1

                let appendingText = System.Char.ConvertFromUtf32(int v)
                unescapeTextAux cenv syntaxNode (syntaxOffset + 1 + skip) (s.Slice(1 + skip)) (builder.Append(appendingText))
        | c ->
            unescapeTextAux cenv syntaxNode (syntaxOffset + 1) (s.Slice(1)) (builder.Append(c))

let rec bindLiteralAux (cenv: cenv) (syntaxLiteral: OlySyntaxLiteral) =
    let cleanNumericText (text: string) =
        if text.StartsWith("0x") then
            text.Replace("_", String.Empty)
        else
            let index = text.IndexOfAny([|'u';'i';'f'|])
            if index = -1 then
                text.Replace("_", String.Empty)
            else
                text.Substring(0, index).Replace("_", String.Empty)

    let unescapeText (text: string) =
        let builder = System.Text.StringBuilder(text.Length)
        unescapeTextAux cenv syntaxLiteral 1 (text.AsSpan()) builder

    match syntaxLiteral with
    | OlySyntaxLiteral.Int8(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.Int8(System.SByte.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.UInt8(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.UInt8(System.Byte.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.Int16(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.Int16(System.Int16.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.UInt16(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.UInt16(System.UInt16.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.Int32(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.Int32(System.Int32.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.UInt32(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.UInt32(System.UInt32.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.Int64(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.Int64(System.Int64.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.UInt64(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.UInt64(System.UInt64.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.Float32(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.Float32(System.Single.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.Float64(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.Float64(System.Double.Parse(cleanNumericText syntaxToken.ValueText)))
    | OlySyntaxLiteral.Bool(syntaxToken) ->
        if syntaxToken.IsTrueToken then
            BoundLiteral.Constant(ConstantSymbol.True)
        else
            BoundLiteral.Constant(ConstantSymbol.False)
    | OlySyntaxLiteral.Char16(syntaxToken) ->
        let text = unescapeText syntaxToken.ValueText
        match Char.TryParse(text) with
        | true, c ->
            BoundLiteral.Constant(ConstantSymbol.Char16(c))
        | _ ->
            cenv.diagnostics.Error("Invalid character literal.", 100, syntaxToken)
            BoundLiteral.Error
    | OlySyntaxLiteral.Utf16(syntaxToken) ->
        BoundLiteral.Constant(ConstantSymbol.Utf16(unescapeText syntaxToken.ValueText))
    | OlySyntaxLiteral.Null _ ->
        BoundLiteral.NullInference(TypeSymbol.ObjectInferenceVariable(mkVariableSolution()))

    | OlySyntaxLiteral.Default _ ->
        BoundLiteral.DefaultInference(mkInferenceVariableType None, false)
    | OlySyntaxLiteral.UncheckedDefault _ ->
        BoundLiteral.DefaultInference(mkInferenceVariableType None, true)

    // These are the defaults for integer and rational values.
    | OlySyntaxLiteral.Integer(syntaxToken) ->
        let valueText = cleanNumericText syntaxToken.ValueText
        let ty = TypeSymbol.NumberInferenceVariable(mkVariableSolution(), TypeSymbol.Int32, valueText)
        let lazyValue =
            lazy
                try
                    let cleanedText = cleanNumericText syntaxToken.ValueText

                    let fromBase, cleanedText =
                        if cleanedText.StartsWith("0b") then
                            2, cleanedText.Substring(2)
                        elif cleanedText.StartsWith("0x") then
                            16, cleanedText.Substring(2)
                        else
                            10, cleanedText

                    let literal =
                        match stripTypeEquations ty with
                        | TypeSymbol.Int8 ->
                            BoundLiteral.Constant(ConstantSymbol.Int8(Convert.ToSByte(cleanedText, fromBase)))
                        | TypeSymbol.UInt8 ->
                            BoundLiteral.Constant(ConstantSymbol.UInt8(Convert.ToByte(cleanedText, fromBase)))
                        | TypeSymbol.Int16 ->
                            BoundLiteral.Constant(ConstantSymbol.Int16(Convert.ToInt16(cleanedText, fromBase)))
                        | TypeSymbol.UInt16 ->
                            BoundLiteral.Constant(ConstantSymbol.UInt16(Convert.ToUInt16(cleanedText, fromBase)))
                        | TypeSymbol.UInt32 ->
                            BoundLiteral.Constant(ConstantSymbol.UInt32(Convert.ToUInt32(cleanedText, fromBase)))
                        | TypeSymbol.Int64 ->
                            BoundLiteral.Constant(ConstantSymbol.Int64(Convert.ToInt64(cleanedText, fromBase)))
                        | TypeSymbol.UInt64 ->
                            BoundLiteral.Constant(ConstantSymbol.UInt64(Convert.ToUInt64(cleanedText, fromBase)))
                        | TypeSymbol.Float32 ->
                            BoundLiteral.Constant(ConstantSymbol.Float32(System.Single.Parse(cleanedText)))
                        | TypeSymbol.Float64 ->
                            BoundLiteral.Constant(ConstantSymbol.Float64(System.Double.Parse(cleanedText)))
                        | _ ->
                            BoundLiteral.Constant(ConstantSymbol.Int32(Convert.ToInt32(cleanedText, fromBase)))

                    Ok(literal)
                with
                | ex ->
                    let diag = 
                        OlyDiagnostic.CreateError(
                            $"Invalid numeric literal: {ex.Message}",
                            10,
                            syntaxToken
                        )
                    Error(diag)
        BoundLiteral.NumberInference(lazyValue, ty)
    | OlySyntaxLiteral.Real(syntaxToken) ->
        let valueText = cleanNumericText syntaxToken.ValueText
        let ty = TypeSymbol.NumberInferenceVariable(mkVariableSolution(), TypeSymbol.Float64, cleanNumericText valueText)
        let lazyValue =
            lazy
                try
                    let literal =
                        match stripTypeEquations ty with
                        | TypeSymbol.Float32 ->
                            BoundLiteral.Constant(ConstantSymbol.Float32(System.Single.Parse(cleanNumericText syntaxToken.ValueText)))
                        | _ ->
                            BoundLiteral.Constant(ConstantSymbol.Float64(System.Double.Parse(cleanNumericText syntaxToken.ValueText)))
                    Ok(literal)
                with
                | ex ->
                    let diag = 
                        OlyDiagnostic.CreateError(
                            $"Invalid numeric literal: {ex.Message}",
                            10,
                            syntaxToken
                        )
                    Error(diag)
        BoundLiteral.NumberInference(lazyValue, ty)

    | _ ->
        raise(InternalCompilerException())

let bindLiteral cenv env expectedTyOpt syntaxLiteral =
    let literal = bindLiteralAux cenv syntaxLiteral
    match expectedTyOpt with
    | Some(expectedTy) ->
        checkTypes (SolverEnvironment.Create(cenv.diagnostics, env.benv)) syntaxLiteral expectedTy literal.Type
        stripLiteral literal
    | _ ->
        literal

let bindExtends (cenv: cenv) (env: BinderEnvironment) (syntaxExtends: OlySyntaxExtends) =
    match syntaxExtends with
    | OlySyntaxExtends.Empty _ ->
        ImArray.empty
    | OlySyntaxExtends.Inherits(_, syntaxTys) ->
        syntaxTys.ChildrenOfType 
        |> ImArray.map (fun x -> bindType cenv env None ResolutionTypeArity.Any x)
    | OlySyntaxExtends.Type(syntaxTy) ->
        ImArray.createOne (bindType cenv env None ResolutionTypeArity.Any syntaxTy)

    | _ ->
        raise(InternalCompilerException())

let bindImplements (cenv: cenv) (env: BinderEnvironment) (syntaxImplements: OlySyntaxImplements) =
    match syntaxImplements with
    | OlySyntaxImplements.Empty _ ->
        ImArray.empty
    | OlySyntaxImplements.Implements(_, syntaxTys) ->
        syntaxTys.ChildrenOfType
        |> ImArray.map (fun x -> bindType cenv env None ResolutionTypeArity.Any x)

    | _ ->
        raise(InternalCompilerException())

let bindValueAsCallExpressionWithOptionalSyntaxName (cenv: cenv) (env: BinderEnvironment) (syntaxInfo: BoundSyntaxInfo) receiverExprOpt argExprs (value: IValueSymbol, syntaxNameOpt: OlySyntaxName option) =
    match syntaxNameOpt with
    | Some(OlySyntaxName.Generic(_, syntaxTyArgs)) ->
        bindValueAsCallExpressionWithSyntaxTypeArguments cenv env syntaxInfo receiverExprOpt argExprs (syntaxTyArgs, syntaxTyArgs.Values) value
    | _ -> 
        bindValueAsCallExpression cenv env syntaxInfo receiverExprOpt argExprs ImArray.empty value
        |> fst