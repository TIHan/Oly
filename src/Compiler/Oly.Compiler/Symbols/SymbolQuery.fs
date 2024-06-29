module internal rec Oly.Compiler.Internal.SymbolQuery

open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations
open Oly.Compiler.Internal.SymbolEnvironments

type QueryMemberFlags =
    | StaticOrInstance =          0x00000
    | Static =                    0x00001
    | Instance =                  0x00010
    | Overridable =               0x00100

    /// This will by-pass any accessor logic.
    | InstanceFunctionOverrides = 0x01010
    // TODO: Add StaticInstanceFunctionOverrides

    | PatternFunction =           0x10001

let queryHierarchicalTypesOfTypeParameter (tyPar: TypeParameterSymbol) =
    seq {
        for x in tyPar.Constraints do
            match x with
            | ConstraintSymbol.SubtypeOf(ty) 
            | ConstraintSymbol.TraitType(ty) ->
                yield! ty.Value.FlattenHierarchyIncluding()
            | _ ->
                ()
    }
    |> TypeSymbol.Distinct

let queryMostSpecificIntrinsicFunctionsOfTypeParameter (tyPar: TypeParameterSymbol): _ imarray =
    queryHierarchicalTypesOfTypeParameter tyPar
    |> Seq.collect (fun ty -> 
        ty.Functions
    )
    |> Seq.filter (fun func -> 
        (not func.IsFormal) ||
        (not tyPar.HasArity) || 
        (func.Formal.Enclosing.TypeParameterCount = 0) || 
        (func.Formal.Enclosing.TypeParameterCount = tyPar.Arity)
    )
    |> ImArray.ofSeq
    |> filterMostSpecificFunctions

type EntitySymbol with

    member this.TryFindDefaultInstanceConstructor() =
        this.Functions
        |> ImArray.tryFind (fun x -> x.IsInstanceConstructor && x.LogicalParameterCount = 0)

    member this.HasDefaultInstanceConstructor =
        this.Functions
        |> ImArray.exists (fun x -> x.IsInstanceConstructor && x.LogicalParameterCount = 0)

    member this.AllLogicallyInheritedAndImplementedFunctions =
        let builder = ImArray.builder()
        this.HierarchyForEach(fun x ->
            builder.AddRange(x.Functions)
        )
        builder.ToImmutable()

    member this.AllLogicalFunctions: _ seq =
        this.AllLogicallyInheritedAndImplementedFunctions
        |> Seq.append (this.Functions)

type TypeSymbol with

    /// All logical functions from the type and logical inherited/implemented types
    member this.AllLogicalFunctions =
        match stripTypeEquations this with
        | TypeSymbol.Entity(ent) -> ent.AllLogicalFunctions
        | TypeSymbol.Variable(tyPar)
        | TypeSymbol.InferenceVariable(Some tyPar, _) ->
            queryMostSpecificIntrinsicFunctionsOfTypeParameter tyPar
        | TypeSymbol.HigherVariable(tyPar, tyArgs)
        | TypeSymbol.HigherInferenceVariable(Some tyPar, tyArgs, _, _) -> 
            queryMostSpecificIntrinsicFunctionsOfTypeParameter tyPar
            |> Seq.map (fun (func: IFunctionSymbol) ->
                let enclosing = 
                    func.Formal.Enclosing
                    |> applyEnclosing tyArgs 
                actualFunction enclosing (enclosing.TypeArguments.AddRange(func.TypeArguments)) func
            )
        | _ ->
            Seq.empty

