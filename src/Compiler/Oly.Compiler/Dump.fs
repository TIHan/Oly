module internal rec Oly.Compiler.Internal.Dump

// Dump is meant for dumping a tree for debugging purposes.

open System
open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.BoundTree
open Oly.Compiler.Internal.BoundTreePatterns
open Oly.Compiler.Internal.BoundTreeExtensions
open Oly.Compiler.Internal.SymbolEnvironments

let private benvEmpty = BoundEnvironment.Empty

let private leafLine (str: string) =
    let lineStr = "|---"
    let indentStr =
        String.init lineStr.Length (fun _ -> " ")

    let lines = str.Split('\n') //|> Array.filter (fun x -> x <> "")
    if lines.Length > 0 then
        lines[0] <- lineStr + lines[0]
        for i = 1 to lines.Length - 1 do
            lines[i] <- indentStr + lines[i]
        lines
        |> String.concat "\n"
    else
        ""

let indentLines amount (str: string) =
    if amount <= 0 then str
    else
        let indentStr =
            String.init amount (fun _ -> " ")
        str.Split('\n')
        |> Array.choose (fun x -> if x = "" then None else Some <| indentStr + x)
        |> String.concat "\n"

let dumpTypeSymbol (ty: TypeSymbol) =
    PrettyPrint.printType benvEmpty ty

let dumpEntitySymbol (ent: EntitySymbol) =
    if ent.IsLocal then
        ent.Name + "__" + ent.Formal.Id.ToString()
    else
        ent.Name

let dumpTypeParameter (tyPar: TypeParameterSymbol) =
    if tyPar.HasArity then
        let arity = ImArray.init tyPar.Arity (fun _ -> "_") |> String.concat ", "
        tyPar.DisplayName + $"<{arity}>"
    else
        tyPar.DisplayName

let dumpyTypeParameters (tyPars: TypeParameterSymbol imarray) =
    if tyPars.IsEmpty then
        ""
    else
        let tyPars = tyPars |> ImArray.map (fun x -> dumpTypeSymbol x.AsType) |> String.concat ", "
        $"<{tyPars}>"

let dumpFunctionSymbol (func: IFunctionSymbol) =
    if func.TypeParameters.IsEmpty then
        if func.IsLocal then
            func.Name + "__" + func.Formal.Id.ToString()
        else
            func.Name
    else
        let tyArgs = func.TypeArguments |> ImArray.map dumpTypeSymbol |> String.concat ", "
        if func.IsLocal then
            func.Name + $"<{tyArgs}>" + "__" + func.Formal.Id.ToString()
        else
            func.Name + $"<{tyArgs}>"

let dumpConstantSymbol (constant: ConstantSymbol) =
    match constant with
    | ConstantSymbol.UInt8 value ->
        $"{value}"
    | ConstantSymbol.Int8 value ->
        $"{value}"
    | ConstantSymbol.UInt16 value ->
        $"{value}"
    | ConstantSymbol.Int16 value ->
        $"{value}"
    | ConstantSymbol.UInt32 value ->
        $"{value}"
    | ConstantSymbol.Int32 value ->
        $"{value}"
    | ConstantSymbol.UInt64 value ->
        $"{value}"
    | ConstantSymbol.Int64 value ->
        $"{value}"
    | ConstantSymbol.Float32 value ->
        $"{value}"
    | ConstantSymbol.Float64 value ->
        $"{value}"
    | ConstantSymbol.Char16 value ->
        $"{value}"
    | ConstantSymbol.Array(_, elements) ->
        let elements = elements |> ImArray.map dumpConstantSymbol |> String.concat ", "
        $"array({elements})"
    | ConstantSymbol.External(func) ->
        dumpFunctionSymbol func
    | ConstantSymbol.True ->
        "true"
    | ConstantSymbol.False ->
        "false"
    | ConstantSymbol.Utf16 value ->
        "\"" + value + "\""
    | ConstantSymbol.TypeVariable(tyPar) ->
        dumpTypeParameter tyPar
    | ConstantSymbol.Error ->
        "<error>"

let dumpLiteral (literal: BoundLiteral) =
    match literal with
    | BoundLiteral.Error ->
        "<error>"
    | BoundLiteral.Constant(ConstantSymbol.True) ->
        "true"
    | BoundLiteral.Constant(ConstantSymbol.False) ->
        "false"
    | BoundLiteral.Constant(ConstantSymbol.UInt8 value) ->
        $"{value}: uint8"
    | BoundLiteral.Constant(ConstantSymbol.Int8 value) ->
        $"{value}: int8"
    | BoundLiteral.Constant(ConstantSymbol.UInt16 value) ->
        $"{value}: uint16"
    | BoundLiteral.Constant(ConstantSymbol.Int16 value) ->
        $"{value}: int16"
    | BoundLiteral.Constant(ConstantSymbol.UInt32 value) ->
        $"{value}: uint32"
    | BoundLiteral.Constant(ConstantSymbol.Int32 value) ->
        $"{value}: int32"
    | BoundLiteral.Constant(ConstantSymbol.UInt64 value) ->
        $"{value}: uint64"
    | BoundLiteral.Constant(ConstantSymbol.Int64 value) ->
        $"{value}: int64"
    | BoundLiteral.Constant(ConstantSymbol.Float32 value) ->
        $"{value}: float32"
    | BoundLiteral.Constant(ConstantSymbol.Float64 value) ->
        $"{value}: float64"
    | BoundLiteral.Constant(ConstantSymbol.Char16 value) ->
        $"{value}: char16"
    | BoundLiteral.ConstantEnum(constant, enumTy) ->
        dumpConstantSymbol constant + ": " + dumpTypeSymbol enumTy
    | BoundLiteral.NumberInference(lazyLiteral, _) ->
        match lazyLiteral.Value with
        | Ok literal ->
            dumpLiteral literal
        | _ ->
            "?invalid literal?"
    | BoundLiteral.DefaultInference(ty, _) ->
        dumpTypeSymbol ty
    | _ ->
        // TODO:
        literal.ToString()

let dumpExpression indentAmount (expr: E) =
    match expr with
    | E.Let(_, bindingInfo, rhsExpr, bodyExpr) ->
        let name = bindingInfo.Value.Name + "__" + bindingInfo.Value.Id.ToString()
        let letBinding = $"LET `{name}`: {dumpTypeSymbol bindingInfo.Value.Type} =\n{dumpExpression 0 rhsExpr |> indentLines 4}"
        $"{letBinding}\n\n{dumpExpression 4 bodyExpr}"

    | E.Sequential(_, expr1, expr2, _) ->
        $"{dumpExpression indentAmount expr1}\n{dumpExpression indentAmount expr2}"

    | E.EntityDefinition(body=bodyExpr;ent=ent) ->
        let output =
            match bodyExpr with
            | E.None _ ->
                $"ENTITY `{dumpEntitySymbol ent}`{dumpyTypeParameters ent.TypeParameters}"
                |> indentLines indentAmount
            | _ ->
                $"ENTITY `{dumpEntitySymbol ent}`{dumpyTypeParameters ent.TypeParameters} =\n{dumpExpression 4 bodyExpr}\n"
                |> indentLines indentAmount
        output + "\n"

    | E.MemberDefinition(_, binding) ->
        let output =
            match binding with
            | BoundBinding.Implementation(_, bindingInfo, rhsExpr) ->
                $"MEMBER `{bindingInfo.Value.Name}`: {dumpTypeSymbol bindingInfo.Value.Type} =\n{dumpExpression 4 rhsExpr}\n"
                |> indentLines indentAmount
            | BoundBinding.Signature(_, bindingInfo) ->
                $"MEMBER SIGNATURE `{bindingInfo.Value.Name}`: {dumpTypeSymbol bindingInfo.Value.Type}\n"
                |> indentLines indentAmount
        output + "\n"

    | E.Lambda(pars=pars;body=lazyBodyExpr) ->
        let pars = 
            pars 
            |> ImArray.map (fun x -> let name = if x.Name = "" then $"__v{x.Id}" else x.Name in $"`{name}__{x.Id}`: {dumpTypeSymbol x.Type}") 
            |> String.concat ", "
        $"LAMBDA ({pars}) ->\n{dumpExpression 0 lazyBodyExpr.Expression |> indentLines 4}"
        |> indentLines indentAmount

    | E.Literal(_, literal) ->
        $"LITERAL {dumpLiteral literal}"

    | E.None _ -> "NONE"

    | E.GetField(receiver=receiver; field=field) ->
        $"LOAD_FIELD `{field.Name}`\n{dumpExpression 4 receiver |> indentLines 4}"

    | E.NewTuple(_, argExprs, _) ->
        let args =
            argExprs
            |> ImArray.map (fun argExpr -> dumpExpression 4 argExpr)
            |> String.concat "\n"
        "NEW_TUPLE\n" + (args |> indentLines 4)

    | E.Call(receiverOpt=receiverOpt;args=argExprs;value=value) ->
        let callName =
            match value.TryWellKnownFunction with
            | ValueSome(wellKnownFunc) ->
                match wellKnownFunc with
                | WellKnownFunction.GetTupleElement ->
                    "LOAD_TUPLE_ITEM"
                | _ ->
                    let name =
                        if value.IsLocal then
                            if value.Name = "" then
                                $"__v{value.Formal.Id}"
                            else
                                value.Name + "__" + value.Formal.Id.ToString()
                        else
                            value.Name
                    $"CALL `{name}`"
            | _ ->
                let name =
                    if value.IsLocal then
                        if value.Name = "" then
                            $"__v{value.Formal.Id}"
                        else
                            value.Name + "__" + value.Formal.Id.ToString()
                    else
                        value.Name
                $"CALL `{name}`"
        let argExprs =
            match receiverOpt with
            | Some receiver ->
                argExprs |> ImArray.prependOne receiver
            | _ ->
                argExprs
        let args =
            argExprs
            |> ImArray.map (fun argExpr -> leafLine (dumpExpression 4 argExpr))
            |> String.concat "\n"
        callName + "\n" + args

    | E.Value(_, value) ->
        let name =
            if value.IsLocal then
                if value.Name = "" then
                    $"__v{value.Formal.Id}"
                else
                    value.Name + "__" + value.Formal.Id.ToString()
            else
                value.Name
        $"VALUE `{name}`"

    | E.IfElse(_, condExpr, trueTargetExpr, falseTargetExpr, _) ->
        let args =
            [condExpr;trueTargetExpr;falseTargetExpr]
            |> ImArray.ofSeq
            |> ImArray.map (fun argExpr -> leafLine (dumpExpression 4 argExpr))
            |> String.concat "\n"
        $"IF_ELSE\n" + args

    | E.While(_, condExpr, bodyExpr) ->
        let args =
            [condExpr;bodyExpr]
            |> ImArray.ofSeq
            |> ImArray.map (fun argExpr -> leafLine (dumpExpression 4 argExpr))
            |> String.concat "\n"
        $"WHILE\n" + args

    | _ ->
        expr.GetType().Name

let dumpTree (tree: BoundTree) =
    match tree.Root with
    | BoundRoot.Global(body=bodyExpr) ->
        dumpExpression 0 bodyExpr
    | BoundRoot.Namespace(body=bodyExpr) ->
        dumpExpression 0 bodyExpr
