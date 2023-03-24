[<RequireQualifiedAccess>]
module internal Oly.Compiler.Internal.WellKnownFunctions

open Oly.Core
open Oly.Compiler.Internal.Symbols
open Oly.Compiler.Internal.SymbolOperations

let Upcast =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("upcast"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_upcast" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Upcast None false

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
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_address_of_mutable" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.AddressOf None false

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
    let returnTy = TypeSymbol.CreateByRef(tyPars.[0].AsType, ByRefKind.Read)
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_address_of" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.AddressOf None false

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
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_unsafe_address_of" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.UnsafeAddressOf None false

let FromAddress =
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("from_address"))
    let tyPars =
        seq {
            TypeParameterSymbol("T", 0, 0, TypeParameterKind.Function 0, ref ImArray.empty)
        } |> ImArray.ofSeq
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.CreateByRef(tyPars.[0].AsType, ByRefKind.Read), false)
        } |> ImArray.ofSeq
    let returnTy = tyPars.[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_from_address" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.FromAddress None false

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
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.Function(ImArray.createOne tyPars[2].AsType, tyPars[1].AsType), false)
        } |> ImArray.ofSeq
    let returnTy = tyPars[0].AsType
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_load_function_ptr" tyPars pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.LoadFunctionPtr None false

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

let LoadTupleElement =
    // __oly_load_tuple_element<N, T...>(__oly_tuple<T...>): T...[N] where N: constant __oly_int32
    let attrs = ImArray.createOne(AttributeSymbol.Intrinsic("get_tuple_element"))
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
    let pars =
        seq {
            createLocalParameterValue(ImArray.empty, "", TypeSymbol.BaseObject, false)
        } |> ImArray.ofSeq
    let returnTy = TypeSymbol.Unit
    createFunctionValue EnclosingSymbol.RootNamespace attrs "__oly_ignore" ImArray.empty pars returnTy MemberFlags.None FunctionFlags.None WellKnownFunction.Ignore None false