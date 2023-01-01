namespace rec Oly.Runtime.CodeGen

open System
open System.Diagnostics
open System.Collections.Generic
open Oly.Core
open Oly.Core.TaskExtensions
        
[<NoComparison;NoEquality>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRParameter<'RuntimeType> =
    | OlyIRParameter of name: string * ty: 'RuntimeType * isMutable: bool

    member this.Name =
        match this with
        | OlyIRParameter(name=name) -> name

    member this.Type =
        match this with
        | OlyIRParameter(ty=ty) -> ty

    member this.IsMutable =
        match this with
        | OlyIRParameter(isMutable=isMutable) -> isMutable

    override this.ToString() =
        $"{this.Name}: {this.Type} - (mutable: {this.IsMutable})"

// TODO: Add constraints
[<NoComparison;NoEquality>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRTypeParameter<'RuntimeType> =
    | OlyIRTypeParameter of name: string

    member this.Name =
        match this with
        | OlyIRTypeParameter(name) -> name

    override this.ToString() =
        this.Name

[<NoComparison;NoEquality>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRType<'Type> =
    | OlyIRType of origTyPath: string imarray * origTyName: string * 'Type * OlyIRTypeParameter<'Type> imarray

    member this.EmittedType =
        match this with
        | OlyIRType(_, _, runtimeTy, _) -> runtimeTy

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type OlyIRConstant<'Type, 'Function> =
    | Int8 of value: int8
    | UInt8 of value: uint8
    | Int16 of value: int16
    | UInt16 of value: uint16
    | Int32 of value: int32
    | UInt32 of value: uint32
    | Int64 of value: int64
    | UInt64 of value: uint64
    | Float32 of value: float32
    | Float64 of value: float
    | True
    | False
    | Array of elementTy: 'Type * OlyIRConstant<'Type, 'Function> imarray
    | Char16 of value: char
    | Utf16 of value: string
    | Variable of index: int32 * kind: OlyIRTypeVariableKind
    | External of func: 'Function

    override this.ToString() =
        Dump.DumpConstant this

[<RequireQualifiedAccess>]
type OlyIRAttributeNamedArgumentKind =
    | Property
    | Field

[<RequireQualifiedAccess;NoEquality;NoComparison>]
type OlyIRAttributeNamedArgument<'Type, 'Function> =
    {
        Kind: OlyIRAttributeNamedArgumentKind
        Name: string
        Constant: OlyIRConstant<'Type, 'Function>
    }

[<NoEquality;NoComparison>]
type OlyIRAttribute<'Type, 'Function> =
    | OlyIRAttribute of ctor: 'Function * args: OlyIRConstant<'Type, 'Function> imarray * namedArgs: OlyIRAttributeNamedArgument<'Type, 'Function> imarray

[<NoEquality;NoComparison>]
[<RequireQualifiedAccess>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRValue<'Type, 'Function, 'Field> =
    | Unit of resultTy: 'Type
    | Null of resultTy: 'Type
    | Default of resultTy: 'Type
    | FunctionPtr of func: 'Function * resultTy: 'Type
    | Function of func: 'Function * resultTy: 'Type
    | Local of index: int32 * resultTy: 'Type
    | LocalAddress of index: int32 * kind: OlyIRByRefKind * resultTy: 'Type
    | Argument of index: int32 * resultTy: 'Type
    | ArgumentAddress of index: int32 * kind: OlyIRByRefKind * resultTy: 'Type
    | StaticField of OlyIRField<'Type, 'Function, 'Field> * resultTy: 'Type
    | StaticFieldAddress of OlyIRField<'Type, 'Function, 'Field> * kind: OlyIRByRefKind * resultTy: 'Type
    | Constant of OlyIRConstant<'Type, 'Function> * resultTy: 'Type

    member this.ResultType =
        match this with
        | Unit(resultTy)
        | Null(resultTy)
        | Default(resultTy)
        | FunctionPtr(resultTy=resultTy)
        | Function(resultTy=resultTy)
        | Local(resultTy=resultTy)
        | LocalAddress(resultTy=resultTy)
        | Argument(resultTy=resultTy)
        | ArgumentAddress(resultTy=resultTy) -> resultTy
        | StaticField(resultTy=resultTy) -> resultTy
        | StaticFieldAddress(resultTy=resultTy) -> resultTy
        | Constant(resultTy=resultTy) -> resultTy

    override this.ToString() =
        Dump.DumpValue this

[<NoEquality;NoComparison>]
type OlyIRFunctionBody<'Type, 'Function, 'Field>
    (bodyExpr: OlyIRExpression<'Type, 'Function, 'Field>, 
     argFlags: OlyIRLocalFlags imarray, 
     localFlags: OlyIRLocalFlags imarray) =

    member _.Expression = bodyExpr
    member _.ArgumentFlags = argFlags
    member _.LocalFlags = localFlags
    member _.LocalCount = localFlags.Length

[<Sealed;NoEquality;NoComparison;DebuggerDisplay("{DebugText}")>]
type OlyIRFunction<'Type, 'Function, 'Field> 
    private (func: 'Function, runtimeFunc: RuntimeFunction option) =

    member private this.DebugText = func.ToString()

    member this.EmittedFunction = func

    member internal this.RuntimeFunctionOption = runtimeFunc

    member internal this.RuntimeFunction = runtimeFunc.Value

    /// Function is able to be inlined in all situations.
    member internal this.IsInlineable =
        OlyAssert.Equal(true, runtimeFunc.IsSome)
        let runtimeFunc = runtimeFunc.Value
        runtimeFunc.Flags.IsInlineable

    member internal this.HasEnclosingClosureType =
        OlyAssert.Equal(true, runtimeFunc.IsSome)
        runtimeFunc.Value.EnclosingType.IsClosure

    member internal this.IsClosureInstanceConstructor =
        OlyAssert.Equal(true, runtimeFunc.IsSome)
        if this.HasEnclosingClosureType then
            runtimeFunc.Value.Flags.IsInstance &&
            runtimeFunc.Value.Flags.IsConstructor
        else
            false

    member internal this.IsClosureInstanceInvoke =
        OlyAssert.Equal(true, runtimeFunc.IsSome)
        if this.HasEnclosingClosureType then
            runtimeFunc.Value.Flags.IsInstance &&
            not(runtimeFunc.Value.Flags.IsConstructor)
        else
            false

    member internal this.GetEnclosingClosureTypeFields() =
        OlyAssert.Equal(true, this.HasEnclosingClosureType)
        runtimeFunc.Value.EnclosingType.Fields

    internal new(func: 'Function, runtimeFunc) =
        OlyIRFunction(func, Some runtimeFunc)

    new(func: 'Function) =
        OlyIRFunction(func, None)

[<Sealed;NoEquality;NoComparison>]
type OlyIRField<'Type, 'Function, 'Field> 
    private (emittedField: 'Field, runtimeFieldOpt: RuntimeField option) =

    member this.EmittedField = emittedField

    member internal this.RuntimeField = runtimeFieldOpt

    member internal this.IsMutable =
        match runtimeFieldOpt with
        | Some runtimeField -> runtimeField.IsMutable
        | _ -> true

    member internal this.RuntimeType =
        match runtimeFieldOpt with
        | Some runtimeField -> runtimeField.Type
        | _ -> failwith "assert"

    member internal this.RuntimeEnclosingType =
        match runtimeFieldOpt with
        | Some runtimeField -> runtimeField.EnclosingType
        | _ -> failwith "assert"

    static member internal AreEqual(field1: OlyIRField<'Type, 'Function, 'Field>, field2: OlyIRField<'Type, 'Function, 'Field>) =
        obj.ReferenceEquals(field1.EmittedField, field2.EmittedField)
        //match field1.RuntimeField, field2.RuntimeField with
        //| Some(runtimeField1), Some(runtimeField2) -> 
        //    if runtimeField1 = runtimeField2 then
        //        true
        //    elif runtimeField1.Formal = runtimeField2.Formal then
        //        runtimeField1.Name = runtimeField2.Name &&
        //        runtimeField1.EnclosingType.Equals(runtimeField2.EnclosingType) &&
        //        runtimeField1.Type.Equals(runtimeField2.Type)
        //    else
        //        false
        //| _ -> 
        //    false

    new(emittedField: 'Field) =
        OlyIRField(emittedField, None)

    internal new(emittedField: 'Field, runtimeField: RuntimeField) =
        OlyIRField(emittedField, Some runtimeField)

    override this.ToString() =
        match runtimeFieldOpt with
        | Some runtimeField -> runtimeField.Name
        | _ -> "(user generated field)"

[<ReferenceEquality;NoComparison>]
[<RequireQualifiedAccess>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIROperation<'Type, 'Function, 'Field> =
    | Add of                        arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Subtract of                   arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Multiply of                   arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Divide of                     arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Remainder of                  arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | BitwiseNot of                 arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | BitwiseAnd of                 arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | BitwiseOr of                  arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | BitwiseExclusiveOr of         arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | BitwiseShiftLeft of           arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | BitwiseShiftRight of          arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Not of                        arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Negate of                     arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | Equal of                      arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | NotEqual of                   arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | Utf16Equal of                arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | GreaterThan of                arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | GreaterThanOrEqual of         arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | LessThan of                   arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | LessThanOrEqual of            arg1: OlyIRExpression<'Type, 'Function, 'Field> * arg2: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | LoadRefCellContents of        receiver: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | StoreRefCellContents of       receiver: OlyIRExpression<'Type, 'Function, 'Field> * arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | Print of                      arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Throw of                      arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | Store of                      n: int32 * rhs: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | StoreArgument of              n: int32 * rhs: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | LoadFromAddress of            body: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | StoreToAddress of             arg: OlyIRExpression<'Type, 'Function, 'Field> * rhs: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | LoadField of                  field: OlyIRField<'Type, 'Function, 'Field> * receiver: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | LoadFieldAddress of           field: OlyIRField<'Type, 'Function, 'Field> * receiver: OlyIRExpression<'Type, 'Function, 'Field> * kind: OlyIRByRefKind * resultTy: 'Type
    | StoreField of                 field: OlyIRField<'Type, 'Function, 'Field> * receiver: OlyIRExpression<'Type, 'Function, 'Field> * rhs: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | StoreStaticField of           field: OlyIRField<'Type, 'Function, 'Field> * rhs: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | Call of                       func: OlyIRFunction<'Type, 'Function, 'Field> * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | CallVirtual of                func: OlyIRFunction<'Type, 'Function, 'Field> * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | CallIndirect of               argTys: 'Type imarray * receiver: OlyIRExpression<'Type, 'Function, 'Field> * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | New of                        func: OlyIRFunction<'Type, 'Function, 'Field> * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | NewTuple of                   elementTys: 'Type imarray * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | NewRefCell of                 elementTy: 'Type * arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | NewMutableArray of            elementTy: 'Type * sizeArg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | NewArray of                   elementTy: 'Type * kind: OlyIRArrayKind * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type

    | Witness of                    body: OlyIRExpression<'Type, 'Function, 'Field> * witnessTy: 'Type * resultTy: 'Type
    | Ignore of                     arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | LoadArrayElement of           receiver: OlyIRExpression<'Type, 'Function, 'Field> * indexArgs: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | LoadArrayElementAddress of    receiver: OlyIRExpression<'Type, 'Function, 'Field> * indexArgs: OlyIRExpression<'Type, 'Function, 'Field> imarray * kind: OlyIRByRefKind * resultTy: 'Type
    | StoreArrayElement of          receiver: OlyIRExpression<'Type, 'Function, 'Field> * indexArgs: OlyIRExpression<'Type, 'Function, 'Field> imarray * arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | LoadArrayLength of            receiver: OlyIRExpression<'Type, 'Function, 'Field> * rank: int * resultTy: 'Type
    | Box of                        arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Unbox of                      arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Upcast of                     arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Cast of                       arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | LoadTupleElement of           receiver: OlyIRExpression<'Type, 'Function, 'Field> * index: int32 * resultTy: 'Type

    | LoadFunction of               func: OlyIRFunction<'Type, 'Function, 'Field> * arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | CallStaticConstructor of      func: OlyIRFunction<'Type, 'Function, 'Field> * resultTy: 'Type

    member this.ForEachArgument f =
        match this with
        | Add(arg1, arg2, _)
        | Subtract(arg1, arg2, _)
        | Multiply(arg1, arg2, _)
        | Divide(arg1, arg2, _)
        | Remainder(arg1, arg2, _)
        | BitwiseAnd(arg1, arg2, _)
        | BitwiseOr(arg1, arg2, _)
        | BitwiseExclusiveOr(arg1, arg2, _)
        | BitwiseShiftLeft(arg1, arg2, _)
        | BitwiseShiftRight(arg1, arg2, _)
        | Equal(arg1, arg2, _)
        | NotEqual(arg1, arg2, _)
        | Utf16Equal(arg1, arg2, _)
        | GreaterThan(arg1, arg2, _)
        | GreaterThanOrEqual(arg1, arg2, _)
        | LessThan(arg1, arg2, _)
        | LessThanOrEqual(arg1, arg2, _)
        | StoreRefCellContents(arg1, arg2, _)
        | StoreToAddress(arg1, arg2, _)
        | StoreField(_, arg1, arg2, _) -> 
            f 0 arg1
            f 1 arg2
        | BitwiseNot(arg, _)
        | Not(arg, _)
        | Negate(arg, _)
        | Print(arg, _)
        | Throw(arg, _)
        | Box(arg, _) 
        | Unbox(arg, _) 
        | Upcast(arg, _)
        | Cast(arg, _)
        | LoadRefCellContents(arg, _)
        | LoadFromAddress(arg, _)
        | Store(_, arg, _) 
        | StoreArgument(_, arg, _)
        | LoadField(_, arg, _) 
        | LoadFieldAddress(_, arg, _, _) 
        | StoreStaticField(_, arg, _)
        | NewRefCell(_, arg, _) 
        | NewMutableArray(_, arg, _) 
        | Witness(arg, _, _)
        | LoadTupleElement(arg, _, _)
        | LoadArrayLength(arg, _, _) 
        | LoadFunction(_, arg, _) 
        | Ignore(arg, _) ->
            f 0 arg

        | Call(args=args)
        | CallVirtual(args=args)
        | New(args=args)
        | NewTuple(args=args)
        | NewArray(args=args) ->
            for i = 0 to args.Length - 1 do
                f i args[i]

        | CallIndirect(receiver=receiver;args=args) ->
            f 0 receiver
            for i = 0 to args.Length - 1 do
                f (i + 1) args[i]

        | LoadArrayElement(receiver=receiver;indexArgs=indexArgs)
        | LoadArrayElementAddress(receiver=receiver;indexArgs=indexArgs) ->
            f 0 receiver
            for i = 0 to indexArgs.Length - 1 do
                f (i + 1) indexArgs[i]
        
        | StoreArrayElement(receiver=receiver;indexArgs=indexArgs;arg=arg) ->
            f 0 receiver
            for i = 0 to indexArgs.Length - 1 do
                f (i + 1) indexArgs[i]
            f (indexArgs.Length + 1) arg

        | CallStaticConstructor _ -> ()

    member this.MapArguments mapper : OlyIRExpression<_, _, _> imarray =
        let args = ImArray.builderWithSize this.ArgumentCount
        this.ForEachArgument (fun i arg ->
            args.Add(mapper i arg)
        )
        args.MoveToImmutable()

    member this.ArgumentCount =
        match this with
        | Add(_, _, _)
        | Subtract(_, _, _)
        | Multiply(_, _, _)
        | Divide(_, _, _)
        | Remainder(_, _, _)
        | BitwiseAnd(_, _, _)
        | BitwiseOr(_, _, _)
        | BitwiseExclusiveOr(_, _, _)
        | BitwiseShiftLeft(_, _, _)
        | BitwiseShiftRight(_, _, _)
        | Equal(_, _, _)
        | NotEqual(_, _, _)
        | Utf16Equal(_, _, _)
        | GreaterThan(_, _, _)
        | GreaterThanOrEqual(_, _, _)
        | LessThan(_, _, _)
        | LessThanOrEqual(_, _, _)
        | StoreRefCellContents(_, _, _)
        | StoreToAddress(_, _, _)
        | StoreField(_, _, _, _) -> 
            2
        | BitwiseNot(_, _)
        | Not(_, _)
        | Negate(_, _)
        | Print(_, _)
        | Throw(_, _)
        | Box(_, _) 
        | Unbox(_, _) 
        | Upcast(_, _)
        | Cast(_, _)
        | LoadRefCellContents(_, _)
        | LoadFromAddress(_, _)
        | Store(_, _, _) 
        | StoreArgument(_, _, _)
        | LoadField(_, _, _) 
        | LoadFieldAddress(_, _, _, _) 
        | StoreStaticField(_, _, _)
        | NewRefCell(_, _, _) 
        | NewMutableArray(_, _, _) 
        | Witness(_, _, _)
        | LoadTupleElement(_, _, _)
        | LoadArrayLength(_, _, _) 
        | LoadFunction _ 
        | Ignore _ ->
            1

        | Call(args=args)
        | CallVirtual(args=args)
        | New(args=args)
        | NewTuple(args=args)
        | NewArray(args=args) ->
            args.Length

        | CallIndirect(receiver=_;args=args) ->
            args.Length + 1

        | LoadArrayElement(receiver=_;indexArgs=indexArgs)
        | LoadArrayElementAddress(receiver=_;indexArgs=indexArgs) ->
            indexArgs.Length + 1
        
        | StoreArrayElement(receiver=_;indexArgs=indexArgs;arg=_) ->
            indexArgs.Length + 2

        | CallStaticConstructor _ -> 0

    member this.GetArguments() =
        let builder = ImArray.builderWithSize this.ArgumentCount
        this.ForEachArgument(fun _ argExpr -> builder.Add(argExpr))
        builder.MoveToImmutable()

    member this.ReplaceArguments(newArgs: OlyIRExpression<'Type, 'Function, 'Field> imarray) =
        if this.ArgumentCount <> newArgs.Length then
            failwith "Invalid number of arguments."

        match this with
        | Add _ ->
            Add(newArgs[0], newArgs[1], this.ResultType)
        | Subtract _ ->
            Subtract(newArgs[0], newArgs[1], this.ResultType)
        | Multiply _ ->
            Multiply(newArgs[0], newArgs[1], this.ResultType)
        | Divide _ ->
            Divide(newArgs[0], newArgs[1], this.ResultType)
        | Remainder _ ->
            Remainder(newArgs[0], newArgs[1], this.ResultType)
        | BitwiseAnd _ ->
            BitwiseAnd(newArgs[0], newArgs[1], this.ResultType)
        | BitwiseOr _ ->
            BitwiseOr(newArgs[0], newArgs[1], this.ResultType)
        | BitwiseExclusiveOr _ ->
            BitwiseExclusiveOr(newArgs[0], newArgs[1], this.ResultType)
        | BitwiseShiftLeft _ ->
            BitwiseShiftLeft(newArgs[0], newArgs[1], this.ResultType)
        | BitwiseShiftRight _ ->
            BitwiseShiftRight(newArgs[0], newArgs[1], this.ResultType)
        | Equal _ ->
            Equal(newArgs[0], newArgs[1], this.ResultType)
        | NotEqual _ ->
            NotEqual(newArgs[0], newArgs[1], this.ResultType)
        | Utf16Equal _ ->
            Utf16Equal(newArgs[0], newArgs[1], this.ResultType)
        | GreaterThan _ ->
            GreaterThan(newArgs[0], newArgs[1], this.ResultType)
        | GreaterThanOrEqual _ ->
            GreaterThanOrEqual(newArgs[0], newArgs[1], this.ResultType)
        | LessThan _ ->
            LessThan(newArgs[0], newArgs[1], this.ResultType)
        | LessThanOrEqual _ ->
            LessThanOrEqual(newArgs[0], newArgs[1], this.ResultType)
        | StoreRefCellContents _ ->
            StoreRefCellContents(newArgs[0], newArgs[1], this.ResultType)
        | StoreToAddress _ ->
            StoreToAddress(newArgs[0], newArgs[1], this.ResultType)
        | StoreField(field, _, _, _) -> 
            StoreField(field, newArgs[0], newArgs[1], this.ResultType)

        | BitwiseNot _ ->
            BitwiseNot(newArgs[0], this.ResultType)
        | Not _ ->
            Not(newArgs[0], this.ResultType)
        | Negate _ ->
            Negate(newArgs[0], this.ResultType)
        | Print _ ->
            Print(newArgs[0], this.ResultType)
        | Throw _ ->
            Throw(newArgs[0], this.ResultType)
        | Box _ ->
            Box(newArgs[0], this.ResultType)
        | Unbox _ ->
            Unbox(newArgs[0], this.ResultType)
        | Upcast _ ->
            Upcast(newArgs[0], this.ResultType)
        | Cast _ ->
            Cast(newArgs[0], this.ResultType)
        | LoadRefCellContents _ ->
            LoadRefCellContents(newArgs[0], this.ResultType)
        | LoadFromAddress _ ->
            LoadFromAddress(newArgs[0], this.ResultType)
        | Store(n, _, _) ->
            Store(n, newArgs[0], this.ResultType)
        | StoreArgument(n, _, _) ->
            StoreArgument(n, newArgs[0], this.ResultType)
        | LoadField(field, _, _) ->
            LoadField(field, newArgs[0], this.ResultType)
        | LoadFieldAddress(field, _, byRefKind, _) ->
            LoadFieldAddress(field, newArgs[0], byRefKind, this.ResultType)
        | StoreStaticField(field, _, _) ->
            StoreStaticField(field, newArgs[0], this.ResultType)
        | NewRefCell(elementTy, _, _) ->
            NewRefCell(elementTy, newArgs[0], this.ResultType)
        | NewMutableArray(elementTy, _, _) ->
            NewMutableArray(elementTy, newArgs[0], this.ResultType)
        | Witness(_, witnessTy, _) ->
            Witness(newArgs[0], witnessTy, this.ResultType)
        | LoadTupleElement(_, index, _) ->
            LoadTupleElement(newArgs[0], index, this.ResultType)
        | LoadFunction(irFunc, _, _) ->
            LoadFunction(irFunc, newArgs[0], this.ResultType)
        | LoadArrayLength(_, rank, _) ->
            LoadArrayLength(newArgs[0], rank, this.ResultType)
        | Ignore(_, _) ->
            Ignore(newArgs[0], this.ResultType)

        | Call(func=func) ->
            Call(func, newArgs, this.ResultType)
        | CallVirtual(func=func) ->
            CallVirtual(func, newArgs, this.ResultType)
        | New(func=func) ->
            New(func, newArgs, this.ResultType)
        | NewTuple(elementTys, _, _) ->
            NewTuple(elementTys, newArgs, this.ResultType)
        | NewArray(elementTy, kind, _, _) ->
            NewArray(elementTy, kind, newArgs, this.ResultType)

        | CallIndirect(argTys,  _, _, _) ->
            CallIndirect(argTys, newArgs[0], newArgs.RemoveAt(0), this.ResultType)

        | LoadArrayElement _ ->
            LoadArrayElement(newArgs[0], newArgs.RemoveAt(0), this.ResultType)
        | LoadArrayElementAddress(_, _, byRefKind, _) ->
            LoadArrayElementAddress(newArgs[0], newArgs.RemoveAt(0), byRefKind, this.ResultType)
        
        | StoreArrayElement(receiver=_;indexArgs=indexArgs;arg=_) ->
            StoreArrayElement(newArgs[0], newArgs.RemoveAt(newArgs.Length - 1).RemoveAt(0), newArgs[newArgs.Length - 1], this.ResultType)

        | CallStaticConstructor _ ->
            this

    member this.ResultType =
        match this with
        | CallStaticConstructor(resultTy=resultTy)
        | LoadTupleElement(resultTy=resultTy)
        | LoadFunction(resultTy=resultTy)
        | LoadArrayElement(resultTy=resultTy)
        | LoadArrayElementAddress(resultTy=resultTy)
        | StoreArrayElement(resultTy=resultTy)
        | LoadArrayLength(resultTy=resultTy)
        | Add(resultTy=resultTy)
        | Subtract(resultTy=resultTy)
        | Multiply(resultTy=resultTy)
        | Divide(resultTy=resultTy)
        | Remainder(resultTy=resultTy)
        | BitwiseNot(resultTy=resultTy)
        | BitwiseAnd(resultTy=resultTy)
        | BitwiseOr(resultTy=resultTy)
        | BitwiseExclusiveOr(resultTy=resultTy)
        | BitwiseShiftLeft(resultTy=resultTy)
        | BitwiseShiftRight(resultTy=resultTy)
        | Not(resultTy=resultTy)
        | Negate(resultTy=resultTy)
        | Equal(resultTy=resultTy)
        | NotEqual(resultTy=resultTy)
        | Utf16Equal(resultTy=resultTy)
        | LessThan(resultTy=resultTy)
        | LessThanOrEqual(resultTy=resultTy)
        | GreaterThan(resultTy=resultTy)
        | GreaterThanOrEqual(resultTy=resultTy)
        | LoadRefCellContents(resultTy=resultTy)
        | StoreRefCellContents(resultTy=resultTy)
        | Print(resultTy=resultTy)
        | Throw(resultTy=resultTy)
        | Store(resultTy=resultTy)
        | StoreArgument(resultTy=resultTy)
        | LoadFromAddress(resultTy=resultTy)
        | StoreToAddress(resultTy=resultTy)
        | LoadField(resultTy=resultTy)
        | LoadFieldAddress(resultTy=resultTy)
        | StoreField(resultTy=resultTy)
        | StoreStaticField(resultTy=resultTy)
        | Call(resultTy=resultTy)
        | CallVirtual(resultTy=resultTy)
        | CallIndirect(resultTy=resultTy)
        | New(resultTy=resultTy)
        | NewTuple(resultTy=resultTy)
        | NewRefCell(resultTy=resultTy)
        | NewMutableArray(resultTy=resultTy)
        | Witness(resultTy=resultTy)
        | Box(resultTy=resultTy)
        | Unbox(resultTy=resultTy)
        | Upcast(resultTy=resultTy)
        | Cast(resultTy=resultTy)
        | NewArray(resultTy=resultTy) 
        | Ignore(resultTy=resultTy) -> resultTy

    override this.ToString() =
        Dump.DumpOperation this

[<Struct>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRDebugSourceTextRange(path: OlyPath, startLine: int, startColumn: int, endLine: int, endColumn: int) =

    member _.Path = path
    member _.StartLine = startLine
    member _.StartColumn = startColumn
    member _.EndLine = endLine
    member _.EndColumn = endColumn

    override this.ToString() =
        $"({startLine}, {startColumn}, {endLine}, {endColumn})"

    static member Empty =
        OlyIRDebugSourceTextRange(OlyPath.Empty, 0, 0, 0, 0)

[<ReferenceEquality;NoComparison>]
[<RequireQualifiedAccess>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRExpression<'Type, 'Function, 'Field> =
    | None of           resultTy: 'Type
    | Let of            name: string * localIndex: int * rhsExpr: OlyIRExpression<'Type, 'Function, 'Field> * bodyExpr: OlyIRExpression<'Type, 'Function, 'Field>
    | Value of          textRange: OlyIRDebugSourceTextRange * value: OlyIRValue<'Type, 'Function, 'Field>
    | Operation of      textRange: OlyIRDebugSourceTextRange * op: OlyIROperation<'Type, 'Function, 'Field>
    | IfElse of         predicateExpr: OlyIRExpression<'Type, 'Function, 'Field> * trueExpr: OlyIRExpression<'Type, 'Function, 'Field> * falseExpr: OlyIRExpression<'Type, 'Function, 'Field> * returnTy: 'Type
    | Sequential of     expr1: OlyIRExpression<'Type, 'Function, 'Field> * expr2: OlyIRExpression<'Type, 'Function, 'Field>

    | While of          predicateExpr: OlyIRExpression<'Type, 'Function, 'Field> * bodyExpr: OlyIRExpression<'Type, 'Function, 'Field> * returnTy: 'Type        

    member this.GetExpressions() : _ imarray =
        match this with
        | OlyIRExpression.Sequential(expr1, expr2) ->
            let exprs1 = expr1.GetExpressions()
            let exprs2 = expr2.GetExpressions()
            exprs1.AddRange(exprs2)
        | OlyIRExpression.None _ ->
            ImArray.empty
        | _ ->
            ImArray.createOne this

    static member CreateSequential(exprs: OlyIRExpression<'Type, 'Function, 'Field> imarray) : OlyIRExpression<'Type, 'Function, 'Field> =
        if exprs.IsEmpty then
            failwith "Expressions cannot be zero."

        if exprs.Length = 1 then
            exprs.[0]
        else
            exprs
            |> ImArray.reduceBack (fun expr1 expr2 ->
                OlyIRExpression.Sequential(expr1, expr2)
            )

    member this.ResultType =
        match this with
        | None(returnTy) -> returnTy
        | Let(_, _, _, bodyExpr) -> bodyExpr.ResultType
        | Value(_, value) -> value.ResultType
        | Operation(_, op) -> op.ResultType
        | While(returnTy=returnTy)
        | IfElse(returnTy=returnTy) -> returnTy
        | Sequential(_, expr) -> expr.ResultType

    override this.ToString() =
        Dump.DumpExpression this

module Patterns =

    type E<'Type, 'Function, 'Field> = OlyIRExpression<'Type, 'Function, 'Field>
    type V<'Type, 'Function, 'Field> = OlyIRValue<'Type, 'Function, 'Field>
    type O<'Type, 'Function, 'Field> = OlyIROperation<'Type, 'Function, 'Field>
    type C<'Type, 'Function> = OlyIRConstant<'Type, 'Function>

    let NoRange = OlyIRDebugSourceTextRange.Empty

    let (|And|_|) (expr: E<_, _, _>) =
        match expr with
        | OlyIRExpression.IfElse(expr1, expr2, E.Value(value=V.Constant(C.False, _)), resultTy) ->
            Some(expr1, expr2, resultTy)
        | _ ->
            None

    let (|Or|_|) (expr: E<_, _, _>) =
        match expr with
        | OlyIRExpression.IfElse(expr1, E.Value(value=V.Constant(C.True, _)), expr2, resultTy) ->
            Some(expr1, expr2, resultTy)
        | _ ->
            None

module Dump =
    open Patterns
    
    let private leafLine (str: string) =
        let lineStr = "|---"
        let indentStr =
            String.init lineStr.Length (fun _ -> " ")
    
        let lines = str.Split('\n')
        if lines.Length > 0 then
            lines[0] <- lineStr + lines[0]
            for i = 1 to lines.Length - 1 do
                lines[i] <- indentStr + lines[i]
            lines
            |> String.concat "\n"
        else
            ""

    let private dumpByRefKind (byRefKind: OlyIRByRefKind) =
        match byRefKind with
        | OlyIRByRefKind.ReadWrite -> "rw"
        | OlyIRByRefKind.Read -> "r"

    let private dumpTypeVariableKind (tyVarKind: OlyIRTypeVariableKind) =
        match tyVarKind with
        | OlyIRTypeVariableKind.Function -> "f"
        | OlyIRTypeVariableKind.Type -> "t"

    let DumpConstant (c: C<_, _>) : string =
        match c with
        | C.UInt8 value -> $"uint8 {value}"
        | C.Int8 value -> $"int8 {value}"
        | C.UInt16 value -> $"uint16 {value}"
        | C.Int16 value -> $"int16 {value}"
        | C.UInt32 value -> $"uint32 {value}"
        | C.Int32 value -> $"int32 {value}"
        | C.UInt64 value -> $"uint64 {value}"
        | C.Int64 value -> $"int64 {value}"
        | C.Float32 value -> $"float32 {value}"
        | C.Float64 value -> $"float64 {value}"
        | C.Char16 value -> $"char16 '{value}'"
        | C.Utf16 value -> $"utf16 \"{value}\""
        | C.True -> "true"
        | C.False -> "false"
        | C.Array(_, cs) -> 
            let cs = cs |> ImArray.map (fun c -> DumpConstant c) |> String.concat ";"
            $"array [{cs}]"
        | C.Variable(index, tyVarKind) ->
            $"variable {index} |{dumpTypeVariableKind tyVarKind}|"
        | C.External _ ->
            "external constant" // TODO: Include OlyIRFunction on C.External..

    let DumpValue (v: V<_, _, _>) : string =
        match v with
        | V.Default _ -> "default"
        | V.Null _ -> "null"
        | V.Unit _ -> "unit"
        | V.FunctionPtr _ -> "functionptr" // TODO: what function? V.FunctionPtr needs OlyIRFunction...
        | V.Function _ -> "function" // TODO: what function? V.FunctionPtr needs OlyIRFunction...
        | V.StaticField(field, _) -> $"static field {field}"
        | V.StaticFieldAddress(field, byRefKind, _) -> $"static field address {field} |{dumpByRefKind byRefKind}|"
        | V.Constant(c, _) -> DumpConstant c
        | V.Local(localIndex, _) -> $"local {localIndex}"
        | V.LocalAddress(localIndex, byRefKind, _) -> $"local address {localIndex} |{dumpByRefKind byRefKind}|"
        | V.Argument(argIndex, _) -> $"argument {argIndex}"
        | V.ArgumentAddress(argIndex, byRefKind, _) -> $"argument address {argIndex} |{dumpByRefKind byRefKind}|"

    let DumpOperation (o: O<'Type, 'Function, 'Field>) : string =
        let name = o.GetType().Name.ToLowerInvariant()
        let args = 
            o.GetArguments()
            |> ImArray.map (fun (argE: E<'Type, 'Function, 'Field>) -> leafLine (DumpExpression argE))
            |> String.concat "\n"

        match o with
        | O.Call(func=irFunc)
        | O.CallVirtual(func=irFunc) ->
            let funcName =
                match irFunc.RuntimeFunctionOption with
                | Some func -> func.Name
                | _ ->
                    match box irFunc.EmittedFunction with
                    | null -> ""
                    | _ -> irFunc.EmittedFunction.ToString()
            $"{name} `{funcName}`\n{args}"
        | _ ->
            $"{name}\n{args}"
        
    let DumpExpression<'Type, 'Function, 'Field> (e: E<'Type, 'Function, 'Field>) : string =
        match e with
        | E.Let(_, localIndex, rhsE, bodyE) ->
            let args =
                [rhsE;bodyE]
                |> ImArray.ofSeq
                |> ImArray.map (fun argE -> leafLine (DumpExpression argE))
                |> String.concat "\n"
            $"LET local {localIndex}\n{args}"
    
        | E.Sequential(e1, e2) ->
            let args =
                [e1;e2]
                |> ImArray.ofSeq
                |> ImArray.map (fun argE -> leafLine (DumpExpression argE))
                |> String.concat "\n"
            $"SEQUENTIAL\n" + args
    
        | E.None _ -> "NONE"
    
        | E.Value(_, v) ->
            $"VALUE {DumpValue v}"
    
        | E.IfElse(conditionE, trueTargetE, falseTargetE, _) ->
            let args =
                [conditionE;trueTargetE;falseTargetE]
                |> ImArray.ofSeq
                |> ImArray.map (fun argE -> leafLine (DumpExpression argE))
                |> String.concat "\n"
            $"IF_ELSE\n{args}"

        | E.Operation(_, o) ->
            $"OPERATION {DumpOperation o}"

        | E.While(conditionE, bodyE, _) ->
            let args =
                [conditionE;bodyE]
                |> ImArray.ofSeq
                |> ImArray.map (fun argE -> leafLine (DumpExpression argE))
                |> String.concat "\n"
            $"WHILE\n{args}"