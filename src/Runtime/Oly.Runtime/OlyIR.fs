﻿namespace rec Oly.Runtime.CodeGen

open System
open System.Diagnostics
open System.Collections.Generic
open Oly.Core
open Oly.Core.TaskExtensions
        
[<NoComparison;NoEquality>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRParameter<'RuntimeType, 'RuntimeFunction> =
    | OlyIRParameter of attrs: Lazy<OlyIRAttribute<'RuntimeType, 'RuntimeFunction> imarray> * name: string * ty: 'RuntimeType * isMutable: bool

    member this.Attributes =
        match this with
        | OlyIRParameter(attrs=attrs) when attrs.IsValueCreated -> attrs.Value
        | _ -> failwith "Attributes cannot be accessed at this point."

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

[<RequireQualifiedAccess>]
[<NoEquality;NoComparison>]
type OlyIRConstraint<'Type> =
    | Null
    | Struct
    | NotStruct
    | Unmanaged
    | Blittable
    | Scoped
    | SubtypeOf of 'Type
    | ConstantType of 'Type
    | TraitType of 'Type

[<NoComparison;NoEquality>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRTypeParameter<'RuntimeType> =
    | OlyIRTypeParameter of name: string * constrs: OlyIRConstraint<'RuntimeType> imarray

    member this.Name =
        match this with
        | OlyIRTypeParameter(name, _) -> name

    member this.Constraints =
        match this with
        | OlyIRTypeParameter(_, constrs) -> constrs

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
    | Function of func: OlyIRFunction<'Type, 'Function, 'Field> * resultTy: 'Type
    | Local of index: int32 * resultTy: 'Type
    | LocalAddress of index: int32 * kind: OlyIRByRefKind * resultTy: 'Type
    | Argument of index: int32 * resultTy: 'Type
    | ArgumentAddress of index: int32 * kind: OlyIRByRefKind * resultTy: 'Type
    | StaticField of OlyIRField<'Type, 'Function, 'Field> * resultTy: 'Type
    | StaticFieldAddress of OlyIRField<'Type, 'Function, 'Field> * kind: OlyIRByRefKind * resultTy: 'Type
    | Constant of OlyIRConstant<'Type, 'Function> * resultTy: 'Type

    member this.WithResultType(resultTy: 'Type) =
        match this with
        | Unit _ -> Unit(resultTy)
        | Null _ -> Null(resultTy)
        | Default _ -> Default(resultTy)
        | FunctionPtr(func, _) -> FunctionPtr(func, resultTy)
        | Function(func, _) -> Function(func, resultTy)
        | Local(index, _) -> Local(index, resultTy)
        | LocalAddress(index, kind, _) -> LocalAddress(index, kind, resultTy)
        | Argument(index, _) -> Argument(index, resultTy)
        | ArgumentAddress(index, kind, _) -> ArgumentAddress(index, kind, resultTy)
        | StaticField(field, _) -> StaticField(field, resultTy)
        | StaticFieldAddress(field, kind, _) -> StaticFieldAddress(field, kind, resultTy)
        | Constant(cns, _) -> Constant(cns, resultTy)

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
     argFlags: OlyIRLocalFlags [], 
     localFlags: OlyIRLocalFlags []) =

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
        match field1.RuntimeField, field2.RuntimeField with
        | Some(runtimeField1), Some(runtimeField2) -> 
            runtimeField1 = runtimeField2
        | _ -> 
            false

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
    | LoadRefCellContentsAddress of receiver: OlyIRExpression<'Type, 'Function, 'Field> * kind: OlyIRByRefKind * resultTy: 'Type
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
    | CallConstrained of            constrainedTy: 'Type * func: OlyIRFunction<'Type, 'Function, 'Field> * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | New of                        func: OlyIRFunction<'Type, 'Function, 'Field> * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | NewTuple of                   elementTys: 'Type imarray * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | NewRefCell of                 elementTy: 'Type * arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | NewMutableArray of            elementTy: 'Type * sizeArg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | NewArray of                   elementTy: 'Type * kind: OlyIRArrayKind * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type
    | NewFixedArray of              elementTy: 'Type * length: int * kind: OlyIRArrayKind * args: OlyIRExpression<'Type, 'Function, 'Field> imarray * resultTy: 'Type

    /// This is specialized in that this operation will only exist if the result type is a type variable.
    /// Rules:
    ///     1. The target platform must support generics.
    ///     2. Result type of the operation is a type variable.
    ///     3. During execution, if the type variable is substituted with a type struct that contains a parameterless instance constructor, call it.
    ///     4. During execution, if the type variable is substituted with a struct that does not contain a parameterless instance constructor, then initialize memory for the struct with all bytes to zero.
    ///     5. During execution, if the type variable is not substituted with a type struct or type variable, call the type's parameterless instance constructor.
    ///     6. Violation of any of the above rules will result in undefined behavior.
    | NewOrDefaultOfTypeVariable of resultTy: 'Type

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

    /// TODO: Rename to 'LoadTupleItem'.
    | LoadTupleElement of           receiver: OlyIRExpression<'Type, 'Function, 'Field> * index: int32 * resultTy: 'Type

    | LoadFunction of               func: OlyIRFunction<'Type, 'Function, 'Field> * arg: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type

    | CallStaticConstructor of      func: OlyIRFunction<'Type, 'Function, 'Field> * resultTy: 'Type

    member inline this.GetArgument(index: int) =
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
            match index with
            | 0 -> arg1
            | 1 -> arg2
            | _ -> raise(IndexOutOfRangeException())
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
        | LoadRefCellContentsAddress(arg, _, _)
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
            match index with
            | 0 -> arg
            | _ -> raise(IndexOutOfRangeException())

        | Call(args=args)
        | CallVirtual(args=args)
        | CallConstrained(args=args)
        | New(args=args)
        | NewTuple(args=args)
        | NewArray(args=args)
        | NewFixedArray(args=args) ->
            args[index]

        | CallIndirect(receiver=receiver;args=args) ->
            match index with
            | 0 -> receiver
            | _ -> args[index - 1]

        | LoadArrayElement(receiver=receiver;indexArgs=indexArgs)
        | LoadArrayElementAddress(receiver=receiver;indexArgs=indexArgs) ->
            match index with
            | 0 -> receiver
            | _ -> indexArgs[index - 1]
        
        | StoreArrayElement(receiver=receiver;indexArgs=indexArgs;arg=arg) ->
            match index with
            | 0 -> receiver
            | _ -> 
                if (indexArgs.Length + 1) = index then
                    arg
                else
                    indexArgs[index - 1]

        | CallStaticConstructor _
        | NewOrDefaultOfTypeVariable _ ->
            raise(IndexOutOfRangeException())

    member inline this.ForEachArgument ([<InlineIfLambda>] f) =
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
        | LoadRefCellContentsAddress(arg, _, _)
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
        | CallConstrained(args=args)
        | New(args=args)
        | NewTuple(args=args)
        | NewArray(args=args) 
        | NewFixedArray(args=args) ->
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

        | CallStaticConstructor _ 
        | NewOrDefaultOfTypeVariable _ -> ()

    member inline this.MapArguments ([<InlineIfLambda>] mapper: int -> OlyIRExpression<_, _, _> -> OlyIRExpression<_, _, _>) : OlyIRExpression<_, _, _> imarray =
        let args = ImArray.builderWithSize this.ArgumentCount
        this.ForEachArgument (fun i arg ->
            args.Add(mapper i arg)
        )
        args.MoveToImmutable()

    member inline this.MapAndReplaceArguments ([<InlineIfLambda>] mapper: int -> OlyIRExpression<_, _, _> -> OlyIRExpression<_, _, _>) : OlyIROperation<_, _, _> =
        let mutable newArgs = Unchecked.defaultof<_ imarrayb>
        this.ForEachArgument (fun i arg ->
            let newArg = mapper i arg
            if arg <> newArg then
                if newArgs = null then
                    newArgs <- ImArray.builderWithSize this.ArgumentCount
                    for j = 0 to i - 1 do
                        newArgs.Add(this.GetArgument(j))
            if newArgs <> null then
                newArgs.Add(newArg)
        )
        if newArgs = null then
            this
        else
            this.ReplaceArguments(newArgs.MoveToImmutable())

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
        | LoadRefCellContentsAddress _
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
        | CallConstrained(args=args)
        | New(args=args)
        | NewTuple(args=args)
        | NewArray(args=args)
        | NewFixedArray(args=args) ->
            args.Length

        | CallIndirect(receiver=_;args=args) ->
            args.Length + 1

        | LoadArrayElement(receiver=_;indexArgs=indexArgs)
        | LoadArrayElementAddress(receiver=_;indexArgs=indexArgs) ->
            indexArgs.Length + 1
        
        | StoreArrayElement(receiver=_;indexArgs=indexArgs;arg=_) ->
            indexArgs.Length + 2

        | CallStaticConstructor _
        | NewOrDefaultOfTypeVariable _ -> 0

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
        | LoadRefCellContentsAddress(_, byRefKind, _) ->
            LoadRefCellContentsAddress(newArgs[0], byRefKind, this.ResultType)
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
        | CallConstrained(constrainedTy=constrainedTy;func=func) ->
            CallConstrained(constrainedTy, func, newArgs, this.ResultType)
        | New(func=func) ->
            New(func, newArgs, this.ResultType)
        | NewTuple(elementTys, _, _) ->
            NewTuple(elementTys, newArgs, this.ResultType)
        | NewArray(elementTy, kind, _, _) ->
            NewArray(elementTy, kind, newArgs, this.ResultType)
        | NewFixedArray(elementTy, length, kind, _, _) ->
            NewFixedArray(elementTy, length, kind, newArgs, this.ResultType)

        | CallIndirect(argTys,  _, _, _) ->
            CallIndirect(argTys, newArgs[0], newArgs.RemoveAt(0), this.ResultType)

        | LoadArrayElement _ ->
            LoadArrayElement(newArgs[0], newArgs.RemoveAt(0), this.ResultType)
        | LoadArrayElementAddress(_, _, byRefKind, _) ->
            LoadArrayElementAddress(newArgs[0], newArgs.RemoveAt(0), byRefKind, this.ResultType)
        
        | StoreArrayElement(receiver=_;indexArgs=indexArgs;arg=_) ->
            StoreArrayElement(newArgs[0], newArgs.RemoveAt(newArgs.Length - 1).RemoveAt(0), newArgs[newArgs.Length - 1], this.ResultType)

        | CallStaticConstructor _
        | NewOrDefaultOfTypeVariable _ ->
            this

    member this.WithResultType(resultTy) =
        match this with
        | Add(arg1, arg2, _) -> Add(arg1, arg2, resultTy)
        | Subtract(arg1, arg2, _) -> Subtract(arg1, arg2, resultTy)
        | Multiply(arg1, arg2, _) -> Multiply(arg1, arg2, resultTy)
        | Divide(arg1, arg2, _) -> Divide(arg1, arg2, resultTy)
        | Remainder(arg1, arg2, _) -> Remainder(arg1, arg2, resultTy)
        | BitwiseAnd(arg1, arg2, _) -> BitwiseAnd(arg1, arg2, resultTy)
        | BitwiseOr(arg1, arg2, _) -> BitwiseOr(arg1, arg2, resultTy)
        | BitwiseExclusiveOr(arg1, arg2, _) -> BitwiseExclusiveOr(arg1, arg2, resultTy)
        | BitwiseShiftLeft(arg1, arg2, _) -> BitwiseShiftLeft(arg1, arg2, resultTy)
        | BitwiseShiftRight(arg1, arg2, _) -> BitwiseShiftRight(arg1, arg2, resultTy)
        | Equal(arg1, arg2, _) -> Equal(arg1, arg2, resultTy)
        | NotEqual(arg1, arg2, _) -> NotEqual(arg1, arg2, resultTy)
        | Utf16Equal(arg1, arg2, _) -> Utf16Equal(arg1, arg2, resultTy)
        | GreaterThan(arg1, arg2, _) -> GreaterThan(arg1, arg2, resultTy)
        | GreaterThanOrEqual(arg1, arg2, _) -> GreaterThanOrEqual(arg1, arg2, resultTy)
        | LessThan(arg1, arg2, _) -> LessThan(arg1, arg2, resultTy)
        | LessThanOrEqual(arg1, arg2, _) -> LessThanOrEqual(arg1, arg2, resultTy)
        | StoreRefCellContents(arg1, arg2, _) -> StoreRefCellContents(arg1, arg2, resultTy)
        | StoreToAddress(arg1, arg2, _) -> StoreToAddress(arg1, arg2, resultTy)
        | StoreField(field, arg1, arg2, _) -> StoreField(field, arg1, arg2, resultTy)

        | BitwiseNot(arg, _) -> BitwiseNot(arg, resultTy)
        | Not(arg, _) -> BitwiseNot(arg, resultTy)
        | Negate(arg, _) -> Negate(arg, resultTy)
        | Print(arg, _) -> Print(arg, resultTy)
        | Throw(arg, _) -> Print(arg, resultTy)
        | Box(arg, _) -> Print(arg, resultTy) 
        | Unbox(arg, _) -> Unbox(arg, resultTy) 
        | Upcast(arg, _) -> Upcast(arg, resultTy)
        | Cast(arg, _) -> Cast(arg, resultTy)
        | LoadRefCellContents(arg, _) -> LoadRefCellContents(arg, resultTy)
        | LoadRefCellContentsAddress(arg, kind, _) -> LoadRefCellContentsAddress(arg, kind, resultTy)
        | LoadFromAddress(arg, _) -> LoadFromAddress(arg, resultTy)
        | Store(n, arg, _) -> Store(n, arg, resultTy) 
        | StoreArgument(n, arg, _) -> StoreArgument(n, arg, resultTy)
        | LoadField(field, arg, _) -> LoadField(field, arg, resultTy)
        | LoadFieldAddress(field, arg, kind, _) -> LoadFieldAddress(field, arg, kind, resultTy)
        | StoreStaticField(field, arg, _) -> StoreStaticField(field, arg, resultTy)
        | NewRefCell(elementTy, arg, _) -> NewRefCell(elementTy, arg, resultTy) 
        | NewMutableArray(elementTy, arg, _) -> NewMutableArray(elementTy, arg, resultTy) 
        | Witness(arg, witnessTy, _) -> Witness(arg, witnessTy, resultTy)
        | LoadTupleElement(arg, index, _) -> LoadTupleElement(arg, index, resultTy)
        | LoadArrayLength(arg, rank, _) -> LoadArrayLength(arg, rank, resultTy) 
        | LoadFunction(func, arg, _) -> LoadFunction(func, arg, resultTy)
        | Ignore(arg, _) -> Ignore(arg, resultTy)

        | Call(func, args, _) -> Call(func, args, resultTy)
        | CallVirtual(func, args, _) -> CallVirtual(func, args, resultTy)
        | CallConstrained(constrainedTy, func, args, _) -> CallConstrained(constrainedTy, func, args, resultTy)
        | New(func, args, _) -> New(func, args, resultTy)
        | NewTuple(itemTys, args, _) -> NewTuple(itemTys, args, resultTy)
        | NewArray(elementTy, kind, args, _) -> NewArray(elementTy, kind, args, resultTy)

        | CallIndirect(argTys, receiverArg, args, _) ->
            CallIndirect(argTys, receiverArg, args, resultTy)

        | LoadArrayElement(receiverArg, args, _) -> LoadArrayElement(receiverArg, args, resultTy)
        | LoadArrayElementAddress(receiverArg, args, kind, _) -> LoadArrayElementAddress(receiverArg, args, kind, resultTy)
        
        | StoreArrayElement(receiverArg, args, arg, _) -> StoreArrayElement(receiverArg, args, arg, resultTy)

        | CallStaticConstructor(func, _) -> CallStaticConstructor(func, resultTy)

        | NewFixedArray(elementTy, length, kind, args, resultTy) ->
            NewFixedArray(elementTy, length, kind, args, resultTy)

        | NewOrDefaultOfTypeVariable(_) -> NewOrDefaultOfTypeVariable(resultTy)

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
        | LoadRefCellContentsAddress(resultTy=resultTy)
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
        | CallConstrained(resultTy=resultTy)
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
        | NewFixedArray(resultTy=resultTy)
        | NewOrDefaultOfTypeVariable(resultTy=resultTy)
        | Ignore(resultTy=resultTy) -> resultTy

    override this.ToString() =
        Dump.DumpOperation this

[<Struct>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRDebugSourceTextRange private (path: OlyPath, startLine: int, startColumn: int, endLine: int, endColumn: int) =

    member _.Path = path
    member _.StartLine = startLine
    member _.StartColumn = startColumn
    member _.EndLine = endLine
    member _.EndColumn = endColumn

    override this.ToString() =
        $"({startLine}, {startColumn}, {endLine}, {endColumn})"

    static member Empty =
        OlyIRDebugSourceTextRange(OlyPath.Empty, 0, 0, 0, 0)

    member this.IsEmpty =
        this.Path.IsEmpty

    static member Create(path: OlyPath, startLine, startColumn, endLine, endColumn) =
#if DEBUG || CHECKED
        if startLine = 0 && startColumn = 0 && endLine = 0 && endColumn = 0 && not path.IsEmpty then
            OlyAssert.Fail("Expected path to be empty.")
#endif
        OlyIRDebugSourceTextRange(path, startLine, startColumn, endLine, endColumn)

[<NoEquality;NoComparison>]
[<RequireQualifiedAccess>]
type OlyIRCatchCase<'Type, 'Function, 'Field> =
    | CatchCase of localName: string * localIndex: int32 * bodyExpr: OlyIRExpression<'Type, 'Function, 'Field> * catchTy: 'Type

[<ReferenceEquality;NoComparison>]
[<RequireQualifiedAccess>]
[<DebuggerDisplay("{ToString()}")>]
type OlyIRExpression<'Type, 'Function, 'Field> =
    | None of           textRange: OlyIRDebugSourceTextRange * resultTy: 'Type
    | Let of            name: string * localIndex: int * rhsExpr: OlyIRExpression<'Type, 'Function, 'Field> * bodyExpr: OlyIRExpression<'Type, 'Function, 'Field>
    | Value of          textRange: OlyIRDebugSourceTextRange * value: OlyIRValue<'Type, 'Function, 'Field>
    | Operation of      textRange: OlyIRDebugSourceTextRange * op: OlyIROperation<'Type, 'Function, 'Field>
    | Sequential of     expr1: OlyIRExpression<'Type, 'Function, 'Field> * expr2: OlyIRExpression<'Type, 'Function, 'Field>
    | IfElse of         conditionExpr: OlyIRExpression<'Type, 'Function, 'Field> * trueTargetExpr: OlyIRExpression<'Type, 'Function, 'Field> * falseTargetExpr: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | While of          conditionExpr: OlyIRExpression<'Type, 'Function, 'Field> * bodyExpr: OlyIRExpression<'Type, 'Function, 'Field> * resultTy: 'Type
    | Try of            bodyExpr: OlyIRExpression<'Type, 'Function, 'Field> * catchCases: OlyIRCatchCase<'Type, 'Function, 'Field> imarray * finallyBodyExprOpt: OlyIRExpression<'Type, 'Function, 'Field> option * resultTy: 'Type
    | Phi of            int imarray * resultTy: 'Type

    member this.WithResultType(resultTy: 'Type) =
        match this with
        | None(textRange, _) ->
            None(textRange, resultTy)
        | Let(name, localIndex, rhsExpr, bodyExpr) ->
            let newBodyExpr = bodyExpr.WithResultType(resultTy)
            Let(name, localIndex, rhsExpr, newBodyExpr)
        | Value(textRange, value) ->
            Value(textRange, value.WithResultType(resultTy))
        | Operation(textRange, op) ->
            Operation(textRange, op.WithResultType(resultTy))
        | Sequential(expr1, expr2) ->
            Sequential(expr1, expr2.WithResultType(resultTy))
        | IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, _) ->
            IfElse(conditionExpr, trueTargetExpr, falseTargetExpr, resultTy)
        | While(conditionExpr, bodyExpr, _) ->
            While(conditionExpr, bodyExpr, resultTy)
        | Try(bodyExpr, catchCases, finallyBodyExprOpt, _) ->
            Try(bodyExpr, catchCases, finallyBodyExprOpt, resultTy)
        | Phi(values, _) ->
            Phi(values, resultTy)

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

    member this.Strip() =
        match this with
        | Sequential(OlyIRExpression.None _, irExpr2) -> irExpr2.Strip()
        | _ -> this

    member this.ResultType =
        match this with
        | Let(_, _, _, bodyExpr) -> bodyExpr.ResultType
        | Value(_, value) -> value.ResultType
        | Operation(_, op) -> op.ResultType
        | Sequential(_, expr) -> expr.ResultType
        | None(resultTy=resultTy)
        | While(resultTy=resultTy)
        | IfElse(resultTy=resultTy) 
        | Try(resultTy=resultTy)
        | Phi(resultTy=resultTy) -> resultTy

    member this.TextRange =
        match this with
        | OlyIRExpression.None(textRange, _) -> textRange
        | Let(_, _, rhsExpr, _) -> rhsExpr.TextRange
        | Value(textRange, _) -> textRange
        | Operation(textRange, _) -> textRange
        | Sequential(expr, _) -> expr.TextRange
        | IfElse(conditionExpr, _, _, _)
        | While(conditionExpr, _, _) -> conditionExpr.TextRange
        | Try(bodyExpr=bodyExpr) -> bodyExpr.TextRange
        | Phi _ -> OlyIRDebugSourceTextRange.Empty

    override this.ToString() =
        Dump.DumpExpression this

module Patterns =

    type E<'Type, 'Function, 'Field> = OlyIRExpression<'Type, 'Function, 'Field>
    type V<'Type, 'Function, 'Field> = OlyIRValue<'Type, 'Function, 'Field>
    type O<'Type, 'Function, 'Field> = OlyIROperation<'Type, 'Function, 'Field>
    type C<'Type, 'Function> = OlyIRConstant<'Type, 'Function>

    let NoRange = OlyIRDebugSourceTextRange.Empty

    let And arg1 arg2 resultTy =
        E.IfElse(arg1, arg2, E.Value(NoRange, V.Constant(C.False, resultTy)), resultTy)

    let Or arg1 arg2 resultTy =
        E.IfElse(arg1, E.Value(NoRange, V.Constant(C.True, resultTy)), arg2, resultTy)

    let (|And|_|) (expr: E<_, _, _>) =
        match expr with
        | E.IfElse(expr1, expr2, E.Value(value=V.Constant(C.False, _)), resultTy) ->
            Some(expr1, expr2, resultTy)
        | _ ->
            None

    let (|Or|_|) (expr: E<_, _, _>) =
        match expr with
        | E.IfElse(expr1, E.Value(value=V.Constant(C.True, _)), expr2, resultTy) ->
            Some(expr1, expr2, resultTy)
        | _ ->
            None

    let (|True|_|) (expr: E<_, _, _>) =
        match expr with
        | E.Value(value=V.Constant(C.True, _)) -> Some()
        | _ -> None

    let (|False|_|) (expr: E<_, _, _>) =
        match expr with
        | E.Value(value=V.Constant(C.False, _)) -> Some()
        | _ -> None

    let (|Integral|_|) (expr: E<_, _, _>) =
        match expr with
        | E.Value(value=v) ->
            match v with
            | V.Constant(c, _) ->
                match c with
                | C.UInt8(value) ->
                    Some(int64 value)
                | C.UInt16(value) ->
                    Some(int64 value)
                | C.UInt32(value) ->
                    Some(int64 value)
                | C.UInt64(value) ->
                    Some(int64 value)
                | C.Int8(value) ->
                    Some(int64 value)
                | C.Int16(value) ->
                    Some(int64 value)
                | C.Int32(value) ->
                    Some(int64 value)
                | C.Int64(value) ->
                    Some(value)
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None        

    let (|IntegralZero|_|) (expr: E<_, _, _>) =
        match expr with
        | E.Value(value=v) ->
            match v with
            | V.Constant(c, _) ->
                match c with
                | C.UInt8(0uy)
                | C.UInt16(0us)
                | C.UInt32(0u)
                | C.UInt64(0UL)
                | C.Int8(0y)
                | C.Int16(0s)
                | C.Int32(0)
                | C.Int64(0L) ->
                    Some()
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None

    let (|IntegralOne|_|) (expr: E<_, _, _>) =
        match expr with
        | E.Value(value=v) ->
            match v with
            | V.Constant(c, _) ->
                match c with
                | C.UInt8(1uy)
                | C.UInt16(1us)
                | C.UInt32(1u)
                | C.UInt64(1UL)
                | C.Int8(1y)
                | C.Int16(1s)
                | C.Int32(1)
                | C.Int64(1L) ->
                    Some()
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None

    let (|Null|_|) (expr: E<_, _, _>) =
        match expr with
        | E.Value(value=v) ->
            match v with
            | V.Null _ -> Some()
            | _ -> None
        | _ ->
            None

    /// C#: for (var i = {RHS}; i < {CMP_OP2}; i++) { {BODY}; }
    let (|SimpleForLoop|_|) (expr: E<_, _, _>) =
        match expr with
        | E.Let(_, iv, irRhsExpr, 
                E.While(
                    E.Operation(_, O.LessThan(E.Value(value=V.Local(iv1, _)), cmpOp2, _)),
                    bodyExpr, 
                    resultTy)
            ) when iv = iv1 ->

            match bodyExpr with
            | E.Operation(_, 
                    O.Store(ivStore, 
                        E.Operation(_, O.Add(E.Value(value=V.Local(iv2, _)), IntegralOne, _)),
                        _
                    )
                ) when iv = ivStore && iv = iv2 ->
                Some(iv, irRhsExpr, cmpOp2, E.None(NoRange, resultTy), resultTy)

            | E.Sequential(bodyExpr,
                    E.Operation(_, 
                        O.Store(ivStore, 
                            E.Operation(_, O.Add(E.Value(value=V.Local(iv2, _)), IntegralOne, _)),
                            _
                        )
                    )
                ) when iv = ivStore && iv = iv2 ->
                Some(iv, irRhsExpr, cmpOp2, bodyExpr, resultTy)

            | _ ->
                None
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
        | OlyIRByRefKind.ReadOnly -> "r"
        | OlyIRByRefKind.WriteOnly -> "w"

    let private dumpTypeVariableKind (tyVarKind: OlyIRTypeVariableKind) =
        match tyVarKind with
        | OlyIRTypeVariableKind.Function -> "f"
        | OlyIRTypeVariableKind.Type -> "t"

    let DumpConstant (cns: C<_, _>) : string =
        match cns with
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
        | V.Default _ -> "defaultstruct"
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
                    | null -> "(unable to determine function name)"
                    | _ -> irFunc.EmittedFunction.ToString()
            $"{name} `{funcName}`\n{args}"
        | O.New(func=irFunc)->
            let funcName =
                match irFunc.RuntimeFunctionOption with
                | Some func -> func.EnclosingType.Name
                | _ ->
                    match box irFunc.EmittedFunction with
                    | null -> "(unable to determine constructor name)"
                    | _ -> irFunc.EmittedFunction.ToString()
            $"{name} `{funcName}`\n{args}"
        | O.LoadFunction(irFunc, _, _) when irFunc.IsInlineable ->
            let funcName =
                match irFunc.RuntimeFunctionOption with
                | Some func -> func.Name
                | _ ->
                    match box irFunc.EmittedFunction with
                    | null -> ""
                    | _ -> irFunc.EmittedFunction.ToString()
            $"{name} `{funcName}` |inlineable|\n {args}"
        | O.Store(n=n) ->
            $"{name} {n}\n{args}"
        | _ ->
            $"{name}\n{args}"
        
    let DumpExpression<'Type, 'Function, 'Field> (e: E<'Type, 'Function, 'Field>) : string =
        match e with
        | E.Let(name, localIndex, rhsE, bodyE) ->
            let args =
                [rhsE;bodyE]
                |> ImArray.ofSeq
                |> ImArray.map (fun argE -> leafLine (DumpExpression argE))
                |> String.concat "\n"
            $"LET local {localIndex} `{name}`\n{args}"
    
        | E.Sequential(e1, e2) ->
            let args =
                [e1;e2]
                |> ImArray.ofSeq
                |> ImArray.map (fun argE -> leafLine (DumpExpression argE))
                |> String.concat "\n"
            $"SEQUENTIAL\n" + args
    
        | E.None _ -> "NONE"
    
        | E.Value(_, v) ->
            match v with
            | V.Constant(cns, _) ->
                $"CONSTANT {DumpConstant cns}"
            | _ ->
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

        | E.Try _ ->
            // TODO: Implement this.
            "TRY"

        | E.Phi _ ->
            "PHI"

exception OlyGenericRecursionLimitReached of message: string * textRange: OlyIRDebugSourceTextRange