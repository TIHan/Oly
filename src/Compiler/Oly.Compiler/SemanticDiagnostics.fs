[<AutoOpen>]
module internal Oly.Compiler.Internal.SemanticDiagnostics

open System.Collections.Immutable

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolEnvironments
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.PrettyPrint

// TODO: Expand this to all semantic diagnostics.
[<NoEquality;NoComparison>]
type SemanticDiagnostic =
    | Error_AmbiguousFunctions of benv: BoundEnvironment * syntaxNode: OlySyntaxNode * funcGroup: FunctionGroupSymbol
    | Error_ShapeFunctionAmbiguousWitnesses of benv: BoundEnvironment * syntaxNode: OlySyntaxNode * abstractFunc: IFunctionSymbol * candidates: IFunctionSymbol imarray
    | Error_MissingConstraint of benv: BoundEnvironment * syntaxNode: OlySyntaxNode * tyArg: TypeSymbol * constr: ConstraintSymbol
    | Error_AnonymousTypeExtensionAlreadyDeclared 
        of 
            benv: BoundEnvironment *
            srcLoc: OlySourceLocation *
            existingExtendsTy: TypeSymbol * 
            extendsTy: TypeSymbol * 
            intersectedImplTys: ImmutableHashSet<TypeSymbol>

    // TODO: We should figure out what permanent code numbers to use.
    member this.Code =
        match this with
        | Error_AmbiguousFunctions _ 
        | Error_ShapeFunctionAmbiguousWitnesses _ 
        | Error_AnonymousTypeExtensionAlreadyDeclared _ -> 10
        | Error_MissingConstraint _ -> 500

type OlyDiagnosticLogger with

    member this.Report(diag: SemanticDiagnostic) =
        let code = diag.Code
        match diag with
        | Error_AmbiguousFunctions(benv, syntaxNode, funcGroup) ->
            let candidatesText =
                funcGroup.Functions
                |> ImArray.map (fun func -> "\n    " + printValue benv func.Formal)
                |> String.concat ""
            this.Error($"'{printValueName benv funcGroup}' has ambiguous functions. Candidates:{candidatesText}", code, syntaxNode)

        | Error_ShapeFunctionAmbiguousWitnesses(benv, syntaxNode, abstractFunc, candidates) ->
            let typesImplementingAbstractFuncText =
                let comparer = SymbolOperations.SymbolComparers.TypeSymbolComparer()
                let map = System.Collections.Generic.Dictionary(comparer)
                candidates
                |> ImArray.iter (fun func -> 
                    let ty = func.Enclosing.AsType
                    match map.TryGetValue ty with
                    | false, _ ->
                        let funcs = ResizeArray()
                        funcs.Add(func)
                        map[ty] <- funcs
                    | true, funcs ->
                        funcs.Add(func)
                )
                map
                |> Seq.sortBy (fun x -> printType benv x.Key)
                |> Seq.map (fun pair ->
                    let ty = pair.Key
                    let funcs = pair.Value

                    let text =
                        if ty.IsTypeExtension_ste then
                            "\n    extension " + printType benv ty
                        else
                            "\n    " + printType benv ty

                    if funcs.Count > 1 then
                        let funcText =
                            funcs
                            |> Seq.map (fun x -> "\n        " + printValue benv x)
                            |> String.concat ""
                        text + " = " + funcText
                    else
                        text
                )
                |> String.concat ""
            this.Error($"Solving shape function '{printValue benv abstractFunc.Formal}' has ambiguity from witnesses:{typesImplementingAbstractFuncText}", code, syntaxNode)

        | Error_MissingConstraint(benv, syntaxNode, tyArg, constr) ->
            this.Error($"Type instantiation '{printType benv tyArg}' is missing the constraint '{printConstraint benv constr}'.", code, syntaxNode)

        | Error_AnonymousTypeExtensionAlreadyDeclared(benv, srcLoc, existingExtendsTy, extendsTy, intersectedImplTys) ->
            OlyAssert.True(intersectedImplTys.Count > 0)
            intersectedImplTys
            |> Seq.iter (fun implTy ->
                let msg =
                    if areTypesEqualWith Indexable existingExtendsTy extendsTy then
                        $"Type '{printType benv existingExtendsTy}' already has an existing anonymous extension implementation of interface '{printType benv implTy}'."
                    else
                        $"Types '{printType benv existingExtendsTy}' and '{printType benv extendsTy}' conflict with an anonymous extension implementation of interface '{printType benv implTy}'."
                this.ErrorWithSourceLocation(msg, 10, srcLoc)
            )