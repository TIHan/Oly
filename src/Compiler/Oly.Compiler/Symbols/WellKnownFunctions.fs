[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.WellKnownFunctions

open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations

/// Validates the well-known function.
/// Returns true if it passes validation.
let Validate(wkf: WellKnownFunction, func: IFunctionSymbol) =
    match wkf with
    | WellKnownFunction.None -> true // Nothing to validate.
    | wkf ->
        if func.IsInstance then false
        elif func.IsConstructor then false
        elif func.IsVirtual || func.IsAbstract then false
        elif func.Enclosing.TypeParameterCount <> 0 then false // Function cannot be under an enclosing with type parameters.
        else
            match wkf with
            | WellKnownFunction.None -> true // Nothing to validate.
            | WellKnownFunction.Add
            | WellKnownFunction.Subtract
            | WellKnownFunction.Multiply
            | WellKnownFunction.Divide
            | WellKnownFunction.Remainder 
            | WellKnownFunction.BitwiseAnd
            | WellKnownFunction.BitwiseOr
            | WellKnownFunction.BitwiseExclusiveOr ->
                if func.Parameters.Length <> 2 then false
                else
                    let parTy = func.Parameters[0].Type
                    func.Parameters
                    |> ImArray.forall (fun par -> areTypesEqual par.Type parTy) &&
                    (areTypesEqual func.ReturnType parTy)

            | WellKnownFunction.BitwiseShiftLeft
            | WellKnownFunction.BitwiseShiftRight ->
                if func.Parameters.Length <> 2 then false
                else
                    let parTy = func.Parameters[0].Type
                    func.Parameters[1].Type.IsInteger &&
                    (areTypesEqual func.ReturnType parTy)

            | WellKnownFunction.BitwiseNot ->
                if func.Parameters.Length <> 1 then false
                else
                    (areTypesEqual func.ReturnType func.Parameters[0].Type)

            | WellKnownFunction.And
            | WellKnownFunction.Or ->
                if func.Parameters.Length <> 2 then false
                else
                    func.Parameters
                    |> ImArray.forall (fun par -> areTypesEqual par.Type TypeSymbol.Bool)

            | WellKnownFunction.Not ->
                if func.Parameters.Length <> 1 then false
                else
                    areTypesEqual func.Parameters[0].Type TypeSymbol.Bool &&
                    areTypesEqual func.ReturnType TypeSymbol.Bool

            | WellKnownFunction.Negate ->
                if func.Parameters.Length <> 1 then false
                else
                    areTypesEqual func.ReturnType func.Parameters[0].Type

            | WellKnownFunction.Equal
            | WellKnownFunction.NotEqual
            | WellKnownFunction.GreaterThan
            | WellKnownFunction.GreaterThanOrEqual
            | WellKnownFunction.LessThan
            | WellKnownFunction.LessThanOrEqual ->
                if func.Parameters.Length <> 2 then false
                else
                    let parTy = func.Parameters[0].Type
                    func.Parameters
                    |> ImArray.forall (fun par -> UnifyTypes Rigid par.Type parTy) &&
                    (areTypesEqual func.ReturnType TypeSymbol.Bool)

            | WellKnownFunction.Print ->
                if func.Parameters.Length <> 1 then false
                else
                    areTypesEqual func.ReturnType TypeSymbol.Unit

            | WellKnownFunction.Throw ->
                if func.Parameters.Length <> 1 || func.TypeParameters.Length <> 1 then false
                else
                    areTypesEqual func.ReturnType func.TypeParameters[0].AsType
                        
            | WellKnownFunction.Cast ->
                if func.Parameters.Length <> 1 || func.TypeParameters.Length <> 1 then false
                else 
                    true

            | WellKnownFunction.UnsafeCast ->
                if func.Parameters.Length <> 1 then false
                else 
                    true

            | WellKnownFunction.Ignore ->
                if func.Parameters.Length <> 1 then false
                else
                    areTypesEqual func.ReturnType TypeSymbol.Unit
                        
            | WellKnownFunction.AddressOf ->
                if func.Parameters.Length <> 1 then false
                else
                    not func.Parameters[0].Type.IsByRef_t &&
                    func.ReturnType.IsByRef_t

            | WellKnownFunction.UnsafeAddressOf ->
                if func.Parameters.Length <> 1 then false
                else
                    not func.Parameters[0].Type.IsByRef_t &&
                    func.ReturnType.IsNativePtr_t

            | WellKnownFunction.FromAddress ->
                if func.Parameters.Length <> 1 then false
                else
                    (func.Parameters[0].Type.IsByRef_t || func.Parameters[0].Type.IsNativePtr_t) &&
                    not func.ReturnType.IsByRef_t

            | WellKnownFunction.LoadNullPtr ->
                // REVIEW: Does LoadNullPtr need a type-parameter?
                if not func.Parameters.IsEmpty || func.TypeParameters.Length <> 1 then false
                else
                    func.ReturnType.IsAnyPtr

            | WellKnownFunction.GetTupleElement ->
                if func.Parameters.Length <> 1 || func.TypeParameters.Length <> 2 then false
                else
                    // TODO: Add better validation.
                    true

            | WellKnownFunction.GetArrayLength ->
                if func.Parameters.Length <> 1 then false
                else
                    func.Parameters[0].Type.IsAnyNonFixedArray &&
                    func.ReturnType.IsInteger

            | WellKnownFunction.GetArrayElement ->
                if func.Parameters.Length > 0 then
                    let par1Ty = func.Parameters[0].Type
                    if par1Ty.IsByRef_t then
                        par1Ty.FirstTypeArgument.IsAnyFixedArray &&
                        func.Parameters[1].Type.IsInteger &&
                        (
                            areTypesEqual func.ReturnType par1Ty.FirstTypeArgument.FirstTypeArgument ||
                            (func.ReturnType.IsByRef_t && areTypesEqual func.ReturnType.FirstTypeArgument par1Ty.FirstTypeArgument.FirstTypeArgument)
                        )
                    else                  
                        // TODO: Handle multi-dimensional arrays.
                        if func.Parameters.Length < 2 then false
                        else
                            par1Ty.IsAnyNonFixedArray &&
                            func.Parameters[1].Type.IsInteger &&
                            (
                                areTypesEqual func.ReturnType par1Ty.FirstTypeArgument ||
                                (func.ReturnType.IsByRef_t && areTypesEqual func.ReturnType.FirstTypeArgument par1Ty.FirstTypeArgument)
                            )
                else
                    false

            | WellKnownFunction.SetArrayElement ->
                // TODO: Handle multi-dimensional arrays.
                if func.Parameters.Length < 3 then false
                else
                    let isPar1Valid =
                        let par1Ty = func.Parameters[0].Type
                        if par1Ty.IsByRef_t then
                            par1Ty.FirstTypeArgument.IsAnyFixedArray
                        else
                            par1Ty.IsAnyNonFixedArray
                    isPar1Valid &&
                    func.Parameters[1].Type.IsInteger &&
                    (areTypesEqual func.ReturnType TypeSymbol.Unit)

            | WellKnownFunction.NewMutableArray ->
                // TODO: Handle multi-dimensional arrays.
                if func.Parameters.Length <> 1 then false
                else
                    func.Parameters[0].Type.IsInteger &&
                    func.ReturnType.IsMutableArray_t

            | WellKnownFunction.LoadFunctionPtr ->
                if func.TypeParameters.Length <> 3 then false
                else
                    // TODO: Add better validation.
                    true

            | WellKnownFunction.NewRefCell
            | WellKnownFunction.LoadRefCellContents
            | WellKnownFunction.StoreRefCellContents

            | WellKnownFunction.LoadFunction
            | WellKnownFunction.LoadStaticFunction ->
                // TODO: Make this validation pass for certain cases.
                false

            | WellKnownFunction.Import // TODO: this is a little weird as its not really a function or op...
            | WellKnownFunction.Constant ->
                true

let UnsafeCast =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("unsafe_cast"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_unsafe_cast" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Cast None false

let addFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("add"))
    let tyPars =
        seq {
            TypeParameterSymbol("T1", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
            TypeParameterSymbol("T2", 1, 0, TypeParameterKind.Function 1, ref ImArray.empty)
            TypeParameterSymbol("T3", 2, 0, TypeParameterKind.Function 2, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[1].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[2].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_add" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Add None false

let subtractFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("subtract"))
    let tyPars =
        seq {
            TypeParameterSymbol("T1", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
            TypeParameterSymbol("T2", 1, 0, TypeParameterKind.Function 1, ref ImArray.empty)
            TypeParameterSymbol("T3", 2, 0, TypeParameterKind.Function 2, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[1].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[2].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_subtract" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Subtract None false

let divideFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("divide"))
    let tyPars =
        seq {
            TypeParameterSymbol("T1", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
            TypeParameterSymbol("T2", 1, 0, TypeParameterKind.Function 1, ref ImArray.empty)
            TypeParameterSymbol("T3", 2, 0, TypeParameterKind.Function 2, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[1].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[2].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_divide" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Subtract None false

let multiplyFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("multiply"))
    let tyPars =
        seq {
            TypeParameterSymbol("T1", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
            TypeParameterSymbol("T2", 1, 0, TypeParameterKind.Function 1, ref ImArray.empty)
            TypeParameterSymbol("T3", 2, 0, TypeParameterKind.Function 2, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[1].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[2].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_multiply" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Multiply None false

let remainderFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("remainder"))
    let tyPars =
        seq {
            TypeParameterSymbol("T1", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
            TypeParameterSymbol("T2", 1, 0, TypeParameterKind.Function 1, ref ImArray.empty)
            TypeParameterSymbol("T3", 2, 0, TypeParameterKind.Function 2, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[1].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[2].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_remainder" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Remainder None false

let andFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("and"))
    let tyPars = ImArray.empty
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.Bool, false)
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.Bool, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_and" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.And None false

let orFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("or"))
    let tyPars = ImArray.empty
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.Bool, false)
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.Bool, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_or" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Or None false

let notFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("not"))
    let tyPars = ImArray.empty
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.Bool, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_not" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Not None false

let negateFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("negate"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_negate" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Negate None false

let equalFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("equal"))
    let tyPars = ImArray.empty
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_equal" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Equal None false

let notEqualFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("not_equal"))
    let tyPars = ImArray.empty
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_not_equal" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.NotEqual None false

let greaterThanFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("greater_than"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_greater_than" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.GreaterThan None false

let greaterThanOrEqualFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("greater_than_or_equal"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_greater_than_or_equal" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.GreaterThanOrEqual None false

let lessThanFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("less_than"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_less_than" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.LessThan None false

let lessThanOrEqualFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("less_than_or_equal"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Bool
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_less_than_or_equal" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.LessThanOrEqual None false

let importAttrFunc =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("import"))
    let tyPars = ImArray.empty
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "platform", TypeSymbol.Utf16, false)
            createLocalParameterValue(ImArray.empty, "path", TypeSymbol.Utf16, false)
            createLocalParameterValue(ImArray.empty, "name", TypeSymbol.Utf16, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Unit
    createFunctionValue EnclosingSymbol.RootNamespace attrs "import" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Import None false

let AddressOfMutable =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("address_of"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.CreateByRef(tyPars.[0].AsType, ByRefKind.ReadWrite)
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_address_of_mutable" tyPars pars returnTy MemberFlags.None FunctionFlags.UnmanagedAllocationOnly WellKnownFunction.AddressOf None false

let AddressOf =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("address_of"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.CreateByRef(tyPars.[0].AsType, ByRefKind.ReadOnly)
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_address_of" tyPars pars returnTy MemberFlags.None FunctionFlags.UnmanagedAllocationOnly WellKnownFunction.AddressOf None false

let UnsafeAddressOf =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("unsafe_address_of"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.NativePtr(tyPars.[0].AsType)
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_unsafe_address_of" tyPars pars returnTy MemberFlags.None FunctionFlags.UnmanagedAllocationOnly WellKnownFunction.UnsafeAddressOf None false

let FromAddress =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("from_address"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.CreateByRef(tyPars.[0].AsType, ByRefKind.ReadOnly), false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_from_address" tyPars pars returnTy MemberFlags.None FunctionFlags.UnmanagedAllocationOnly WellKnownFunction.FromAddress None false

let LoadFunction =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("load_function"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
            createLocalParameterValue(ImArray.empty, "", tyPars[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_load_function" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.LoadFunction None false

let LoadStaticFunction =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("load_static_function"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_load_static_function" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.LoadStaticFunction None false

let LoadFunctionPtr =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("load_function_ptr"))
    let tyPars =
        seq {
            TypeParameterSymbol("TFunctionPtr", 0, 0, false, TypeParameterKind.Function 0, ref ImArray.empty)
            TypeParameterSymbol("TReturn", 1, 0, false, TypeParameterKind.Function 1, ref ImArray.empty)
            TypeParameterSymbol("TParameters", 2, 0, true, TypeParameterKind.Function 2, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.Function(tyPars[2].AsType, tyPars[1].AsType, FunctionKind.Normal), false)
        } |> ImArray.ofSeq
    let returnTy = tyPars[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_load_function_ptr" tyPars pars returnTy MemberFlags.None FunctionFlags.UnmanagedAllocationOnly WellKnownFunction.LoadFunctionPtr None false

let NewRefCell =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("new_ref_cell"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.RefCell(tyPars.[0].AsType)
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_new_ref_cell" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.NewRefCell None false

let LoadRefCellContents =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("load_ref_cell_contents"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.RefCell(tyPars.[0].AsType), false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_load_ref_cell_contents" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.LoadRefCellContents None false

let StoreRefCellContents =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("store_ref_cell_contents"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.RefCell(tyPars.[0].AsType), false)
            createLocalParameterValue(ImArray.empty, "", tyPars.[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Unit
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_store_ref_cell_contents" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.StoreRefCellContents None false

/// TODO: Rename to 'LoadTupleItem'.
let LoadTupleElement =
    // __oly_load_tuple_element<N, T...>(__oly_tuple<T...>): T...[N] where N: constant __oly_int32 /// TODO: Rename to 'get_tuple_item'.
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("get_tuple_element")) /// TODO: Rename to 'get_tuple_item'.
    let tyParNConstrs =
        ConstraintSymbol.ConstantType(Lazy.CreateFromValue TypeSymbol.Int32)
        |> ImArray.createOne
    let tyPars =
        seq {
            TypeParameterSymbol("N", 0, 0, TypeParameterKind.Function 0, ref tyParNConstrs)
            TypeParameterSymbol("T", 1, 0, true, TypeParameterKind.Function 1, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.Tuple(ImArray.createOne tyPars[1].AsType, ImArray.empty), false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.DependentIndexer(tyPars[0].AsType, tyPars[1].AsType)
    /// TODO: Rename to '__oly_load_tuple_item'.
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_load_tuple_element" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.GetTupleElement None false

let PrintFunction =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("print"))
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Unit
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_print" ImArray.empty pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Print None false

let IgnoreFunction =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("ignore"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, true, TypeParameterKind.Function 0, ref(ImArray.createOne ConstraintSymbol.Scoped))
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", tyPars[0].AsType, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Unit
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_ignore" tyPars pars returnTy MemberFlags.None FunctionFlags.UnmanagedAllocationOnly WellKnownFunction.Ignore None false