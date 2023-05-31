[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.Lowering.CommonLowering

open System.Threading

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations

let lowerAutoProperty (syntaxInfo: BoundSyntaxInfo) (bindingInfo: BindingInfoSymbol) (rhsExprOpt: E option) mainExpr =
    match bindingInfo with
    | BindingProperty(_, prop) ->
        let backingField = prop.BackingField.Value
        let expr =
            E.CreateSequential(syntaxInfo.Syntax.Tree,
                [
                    yield mainExpr

                    match rhsExprOpt with
                    | Some(rhsExpr) ->
                        yield E.MemberDefinition(
                            syntaxInfo,
                            BoundBinding.Implementation(
                                syntaxInfo,
                                BindingField(
                                    backingField :?> FieldSymbol
                                ),
                                rhsExpr
                            )
                        )
                    | _ ->
                        ()

                    match prop.Getter with
                    | Some(getter) ->
                        let pars = getter.Parameters
                        yield E.MemberDefinition(
                            syntaxInfo,
                            BoundBinding.Implementation(
                                syntaxInfo,
                                BindingFunction(getter),
                                E.Lambda(
                                    syntaxInfo,
                                    LambdaFlags.None,
                                    ImArray.empty,
                                    pars,
                                    LazyExpression.CreateNonLazy(
                                        None,
                                        fun _ ->
                                            if getter.IsInstance then
                                                E.GetField(
                                                    syntaxInfo,
                                                    E.CreateValue(
                                                        syntaxInfo.Syntax.Tree,
                                                        pars[0]
                                                    ),
                                                    backingField
                                                )
                                            else
                                                E.Value(
                                                    syntaxInfo,
                                                    backingField
                                                )
                                    ),
                                    LazyExpressionType(syntaxInfo.Syntax.Tree),
                                    ref ValueNone,
                                    ref ValueNone
                                )
                            )
                        )
                    | _ ->
                        ()

                    match prop.Setter with
                    | Some(setter) ->
                        let pars = setter.Parameters
                        yield E.MemberDefinition(
                            syntaxInfo,
                            BoundBinding.Implementation(
                                syntaxInfo,
                                BindingFunction(setter),
                                E.Lambda(
                                    syntaxInfo,
                                    LambdaFlags.None,
                                    ImArray.empty,
                                    pars,
                                    LazyExpression.CreateNonLazy(
                                        None,
                                        fun _ ->
                                            if setter.IsInstance then
                                                E.SetField(
                                                    syntaxInfo,
                                                    E.CreateValue(
                                                        syntaxInfo.Syntax.Tree,
                                                        pars[0]
                                                    ),
                                                    backingField,
                                                    E.CreateValue(
                                                        syntaxInfo.Syntax.Tree,
                                                        pars[1]
                                                    )
                                                )
                                            else
                                                E.SetValue(
                                                    syntaxInfo,
                                                    backingField,
                                                    E.CreateValue(
                                                        syntaxInfo.Syntax.Tree,
                                                        pars[0]
                                                    )
                                                )
                                    ),
                                    LazyExpressionType(syntaxInfo.Syntax.Tree),
                                    ref ValueNone,
                                    ref ValueNone
                                )
                            )
                        )
                    | _ ->
                        ()
                ]
            )
        expr
    | _ ->
        mainExpr

let rec lower (ct: CancellationToken) syntaxTree (origExpr: E) =
    ct.ThrowIfCancellationRequested()
    match origExpr with
    // Logical And/Or calls transform to IfElse expressions
    | AndCall(syntaxInfo, expr1, expr2) ->
        E.IfElse(syntaxInfo, expr1, expr2, E.CreateLiteral(syntaxTree, BoundLiteralFalse), TypeSymbol.Bool)
        |> lower ct syntaxTree
    | OrCall(syntaxInfo, expr1, expr2) ->
        E.IfElse(syntaxInfo, expr1, E.CreateLiteral(syntaxTree, BoundLiteralTrue), expr2, TypeSymbol.Bool)
        |> lower ct syntaxTree

    // Sequential normalization
    | E.Sequential(_, E.Sequential(_, expr1, expr2, semantic1), expr3, semantic2) when semantic1 = semantic2 ->
        E.CreateSequential(
            [
                expr1
                expr2
            ],
            expr3
        )

    | E.Typed(body=bodyExpr) ->
        bodyExpr

    // LoadFunctionPtr lambda removal
    // Removes the wrapping lambda if this is a LoadFunctionPtr.
    // LoadFunctionPtr will now have a direct argument of the function value.
    | LoadFunctionPtr(syntaxInfo, funcLoadFunctionPtr, LambdaWrappedFunctionCall(syntaxInfoFunc, func)) ->
        E.Call(
            syntaxInfo,
            None,
            ImArray.empty,
            ImArray.createOne(E.Value(syntaxInfoFunc, func)),
            funcLoadFunctionPtr,
            false
        )

    // Auto-properties
    | E.MemberDefinition(binding=BoundBinding.Signature(syntaxInfo, bindingInfo)) when bindingInfo.Value.Enclosing.IsClassOrStructOrModule && bindingInfo.Value.IsAutoProperty ->
        lowerAutoProperty syntaxInfo bindingInfo None origExpr
    | E.MemberDefinition(binding=BoundBinding.Implementation(syntaxInfo, bindingInfo, rhsExpr)) when bindingInfo.Value.Enclosing.IsClassOrStructOrModule && bindingInfo.Value.IsAutoProperty ->
        lowerAutoProperty syntaxInfo bindingInfo (Some rhsExpr) origExpr

#if DEBUG
    | E.MemberDefinition(binding=binding) ->
        Assert.ThrowIf(binding.Info.Value.IsLocal)
        origExpr
#endif

    // Get/Set property calls
    | E.GetProperty(syntaxInfo, receiverOpt, prop) ->
        match prop.Getter with
        | Some getter ->
            let isVirtualCall =
                if not getter.IsConcrete then
                    match receiverOpt with
                    | Some(E.Value(value=value)) ->
                        not value.IsBase
                    | _ ->
                        true
                else
                    false
            E.Call(syntaxInfo, receiverOpt, ImArray.empty, ImArray.empty, getter, isVirtualCall)
        | _ ->
            origExpr
    | E.SetProperty(syntaxInfo, receiverOpt, prop, rhs) ->
        match prop.Setter with
        | Some setter ->
            let isVirtualCall =
                if not setter.IsConcrete then
                    match receiverOpt with
                    | Some(E.Value(value=value)) ->
                        not value.IsBase
                    | _ ->
                        true
                else
                    false
            E.Call(syntaxInfo, receiverOpt, ImArray.empty, ImArray.createOne rhs, setter, isVirtualCall)
        | _ ->
            origExpr

    // Static field constant uses
    | E.Value(syntaxInfo, value) when not value.IsInstance && value.IsField ->
        let field = value :?> IFieldSymbol

        if field.Constant.IsNone then origExpr
        else

        match field.Constant with
        | ValueSome constant ->
            if field.Type.IsEnum then
                E.Literal(syntaxInfo, BoundLiteral.ConstantEnum(constant, field.Type))
            else
                let literal = constant.ToLiteral()
                E.CreateLiteral(syntaxTree, literal)
        | _ ->
            origExpr

    // REVIEW: What the hell is this?
    | E.EntityDefinition(syntaxInfo, bodyExpr, ent) when ent.IsTypeExtension && not ent.Implements.IsEmpty ->
        let funcs = ent.Functions

        let implExists (baseFunc: IFunctionSymbol) =
            funcs
            |> ImArray.exists (fun x ->
                match x.FunctionOverrides with
                | Some overrides ->
                    areLogicalFunctionSignaturesEqual (overrides.Formal :?> IFunctionSymbol) (baseFunc.Formal :?> IFunctionSymbol)
                | _ ->
                    false
            )

        let funcsToImplement =
            ent.AllLogicallyInheritedAndImplementedFunctions
            |> Seq.filter (fun baseFunc ->
                if baseFunc.IsInstance then
                    not (implExists baseFunc)
                else
                    false
            )
            |> ImArray.ofSeq


        if funcsToImplement.IsEmpty then
            origExpr
        else

        let getImpl (baseFunc: IFunctionSymbol) =
            ent.Extends[0].Functions
            |> ImArray.tryPick (fun x ->
                if areLogicalFunctionSignaturesEqual (x.Formal :?> IFunctionSymbol) (baseFunc.Formal :?> IFunctionSymbol) then
                    Some x
                else
                    None
            )

        let enclosing = ent.AsEnclosing

        let valueDeclExprs =
            funcsToImplement
            |> ImArray.choose (fun func ->
                match getImpl func with
                | None -> None // this could happen if the implemented interface has a default impl
                | Some funcToCall ->

                let tyPars =
                    func.TypeParameters
                    |> ImArray.mapi (fun i tyPar ->
                        // TODO: This isn't right yet. WWe need to replace original type parameter info in the constraints.
                        TypeParameterSymbol(
                            tyPar.Name, 
                            ent.TypeParameters.Length + i, 
                            tyPar.Arity, 
                            TypeParameterKind.Function i, 
                            ref ImArray.empty)
                    )
                let pars =
                    // TODO: This isn't right yet. We need to replace original type parameter info.
                    func.Parameters
                    |> ImArray.mapi (fun i par ->
                        if i = 0 && func.IsInstance then
                            createThisValue 
                                par.Name 
                                false 
                                (false (* TODO: we are assuming this is not read-only, that true? *))
                                ent
                        else
                            par
                    )

                let bindingInfo =
                    let overridingFunc =
                        createFunctionValue 
                            enclosing 
                            ImArray.empty 
                            func.Name 
                            tyPars 
                            pars 
                            func.ReturnType
                            ((func.MemberFlags &&& ~~~MemberFlags.Abstract) ||| MemberFlags.Sealed)
                            FunctionFlags.Extra
                            WellKnownFunction.None
                            (Some func)
                            func.IsMutable

                    BindingFunction(overridingFunc)

                let rhsExpr =
                    let parValues =
                        pars
                        |> ImArray.map (fun x -> x :> IValueSymbol)

                    let receiverOpt =
                        if func.IsInstance then
                            E.CreateValue(syntaxTree, parValues[0])
                            |> Some
                        else
                            None

                    let argExprs =
                        if func.IsInstance then
                            parValues.RemoveAt(0)
                            |> ImArray.map (fun x ->
                                E.CreateValue(syntaxTree, x)
                            )
                        else
                            parValues
                            |> ImArray.map (fun x ->
                                E.CreateValue(syntaxTree, x)
                            )

                    let isVirtualCall =
                        if funcToCall.IsFinal || not funcToCall.IsVirtual then
                            false
                        else
                            true                            

                    let callExpr =
                        E.Call(
                            BoundSyntaxInfo.Generated(syntaxTree),
                            receiverOpt,
                            ImArray.empty,
                            argExprs,
                            funcToCall,
                            isVirtualCall
                        )

                    E.Lambda(
                        BoundSyntaxInfo.Generated(syntaxTree),
                        LambdaFlags.None,
                        tyPars,
                        pars,
                        (LazyExpression.CreateNonLazy(None, fun _ -> callExpr)),
                        LazyExpressionType(syntaxTree),
                        ref ValueNone,
                        ref ValueNone
                    )

                E.MemberDefinition(BoundSyntaxInfo.Generated(syntaxTree),
                    BoundBinding.Implementation(BoundSyntaxInfo.Generated(syntaxTree),
                        bindingInfo,
                        rhsExpr
                    )
                )
                |> Some
            )

        if valueDeclExprs.IsEmpty then
            origExpr
        else
            let newBodyExpr =
                E.CreateSequential(
                    bodyExpr,
                    E.CreateSequential(syntaxTree, valueDeclExprs)
                )
            E.CreateEntityDefinition(syntaxInfo, newBodyExpr, ent)

    // Implicit default constructors
    | E.EntityDefinition(syntaxInfo, bodyExpr, ent) 
        when not ent.IsCompilerIntrinsic && 
             not ent.IsImported && (ent.IsClass || ent.IsStruct || ent.IsNewtype || ent.IsModule) 
             ->

        OlyAssert.True(ent.IsFormal)

        let implicitDefaultCtors = 
            ent.FunctionDefinitions
            |> ImArray.filter (fun x -> x.FunctionFlags.HasFlag(FunctionFlags.ImplicitDefaultConstructor))

        OlyAssert.True(implicitDefaultCtors.Length <= 2)

        let newBodyExpr =
            (bodyExpr, implicitDefaultCtors)
            ||> ImArray.fold (fun bodyExpr ctor ->
                OlyAssert.True(ctor.IsFormal)

                let exprs = bodyExpr.FlattenSequentialExpressions()
                let fieldBindings =
                    exprs
                    |> ImArray.choose (function
                        | E.MemberDefinition(binding=binding) when binding.Info.Value.IsField && not binding.Info.Value.IsLocal ->
                            Some binding
                        | _ ->
                            None
                    )

                if ctor.IsInstance then
                    let thisPar = ctor.Parameters.[0]
                    let thisExpr = E.CreateValue(syntaxInfo.Syntax.Tree, thisPar)
                    let newBodyExpr =
                        let firstExprOpt =
                            if ent.Extends.IsEmpty || ent.IsNewtype then None
                            else
                                match ent.Extends.[0].TryEntity with
                                | ValueSome(baseEnt) ->
                                    let baseDefaultInstanceCtorOpt =
                                        createBaseInstanceConstructors "" baseEnt
                                        |> ImArray.tryFind (fun x -> x.IsInstanceConstructor && x.LogicalParameterCount = 0)

                                    match baseDefaultInstanceCtorOpt with
                                    | Some(baseDefaultInstanceCtor) ->
                                        Some(E.Call(
                                            BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree),
                                            (Some(thisExpr)),
                                            ImArray.empty,
                                            ImArray.empty,
                                            baseDefaultInstanceCtor,
                                            false
                                        ))
                                    | _ ->
                                        None
                                | _ ->
                                    None

                        let ctorBodyExpr =
                            if ent.IsNewtype then
                                let syntaxInfoGenerated = BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree)
                                E.SetField(
                                    syntaxInfoGenerated,
                                    thisExpr,
                                    (ent.GetInstanceFields()[0]),
                                    E.CreateValue(syntaxInfoGenerated, ctor.Parameters[1])
                                )
                            else
                                (E.None(BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree)), fieldBindings)
                                ||> ImArray.foldBack (fun expr fieldBinding ->
                                    match fieldBinding with
                                    | BoundBinding.Implementation(bindingInfo=BindingField(field=field); rhs=rhsExpr) when field.IsInstance ->
                                        E.CreateSequential(
                                            E.SetField(BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree), thisExpr, field, rhsExpr),
                                            expr
                                        )
                                    | _ ->
                                        expr
                                )

                        let ctorBodyExpr =
                            match firstExprOpt with
                            | Some(firstExpr) ->
                                E.CreateSequential(firstExpr, ctorBodyExpr)
                            | _ ->
                                ctorBodyExpr

                        let lazyCtorBodyExpr = LazyExpression.CreateNonLazy(None, fun _ -> ctorBodyExpr)

                        let ctorRhsExpr =
                            E.CreateLambda(
                                syntaxInfo.Syntax.Tree,
                                LambdaFlags.None,
                                ctor.TypeParameters,
                                ctor.Parameters,
                                lazyCtorBodyExpr
                            )
                        E.CreateSequential(
                            E.CreateFunctionDefinition(ctor, ctorRhsExpr),
                            bodyExpr
                        )

                    newBodyExpr
                else
                    let newBodyExpr =
                        let ctorBodyExpr =
                            (E.None(BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree)), fieldBindings)
                            ||> ImArray.foldBack (fun expr fieldBinding ->
                                match fieldBinding with
                                | BoundBinding.Implementation(bindingInfo=BindingField(field=field); rhs=rhsExpr) when not field.IsInstance ->
                                    E.CreateSequential(
                                        E.SetValue(BoundSyntaxInfo.Generated(syntaxInfo.Syntax.Tree), field, rhsExpr),
                                        expr
                                    )
                                | _ ->
                                    expr
                            )

                        OlyAssert.True(ctor.Parameters.IsEmpty)

                        let lazyCtorBodyExpr = LazyExpression.CreateNonLazy(None, fun _ -> ctorBodyExpr)

                        let ctorRhsExpr =
                            E.CreateLambda(
                                syntaxInfo.Syntax.Tree,
                                LambdaFlags.None,
                                ctor.TypeParameters,
                                ImArray.empty,
                                lazyCtorBodyExpr
                            )
                        E.CreateSequential(
                            E.CreateFunctionDefinition(ctor, ctorRhsExpr),
                            bodyExpr
                        )
                    newBodyExpr
            )

        if obj.ReferenceEquals(bodyExpr, newBodyExpr) then
            origExpr
        else
            E.CreateEntityDefinition(
                syntaxInfo,
                newBodyExpr,
                ent
            )

    | _ ->
        origExpr

let Lower (ct: CancellationToken) (boundTree: BoundTree) =
    boundTree.RewriteExpression(fun origExpr ->
        lower ct boundTree.SyntaxTree origExpr
    )