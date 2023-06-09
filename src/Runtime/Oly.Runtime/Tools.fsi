module Oly.Runtime.Tools

open Oly.Core
open Oly.Metadata
open Oly.Runtime.CodeGen

[<Sealed;NoComparison;NoEquality>]
type DummyType

[<Sealed;NoComparison;NoEquality>]
type DummyFunction

[<Sealed;NoComparison;NoEquality>]
type DummyField

[<Sealed;NoComparison;NoEquality>]
type DummyLocalManager =

    member CreateLocal : ilTy: OlyILType * ilFlags: OlyILLocalFlags -> int

    member GetLocals : unit -> OlyILLocal imarray

[<Sealed;NoComparison;NoEquality>]
type DummyEmitter =

    new : unit -> DummyEmitter

    interface IOlyRuntimeEmitter<DummyType, DummyFunction, DummyField>

[<Sealed;NoComparison;NoEquality>]
type DummyAssemblyBuilder =

    new : isDebuggable: bool -> DummyAssemblyBuilder

    member MainFunctionDefinitionHandle: OlyILFunctionDefinitionHandle

    member CreateLocalManager: unit -> DummyLocalManager

    member CreateEntityDefinitionHandle: unit -> OlyILEntityDefinitionHandle

    member CreateType:
        ilEntDefHandle: OlyILEntityDefinitionHandle *
        ilKind: OlyILEntityKind *
        name: string *
        ilFuncDefHandles: OlyILFunctionDefinitionHandle imarray * 
        ilFieldDefHandles: OlyILFieldDefinitionHandle imarray
            -> OlyILType

    member CreateFunctionDefinition:
        ilEnclosingEntDefHandle: OlyILEntityDefinitionHandle *
        name: string *
        ilTyPars: OlyILTypeParameter imarray *
        ilPars: OlyILParameter imarray *
        ilFlags: OlyILFunctionFlags *
        ilMemberFlags: OlyILMemberFlags *
        ilLocals: OlyILLocal imarray *
        ilExpr: OlyILExpression *
        ilExprTy: OlyILType
            -> OlyILFunctionDefinitionHandle * OlyILFunctionSpecificationHandle

    member CreateFieldDefinition:
        name: string *
        ilTy: OlyILType *
        ilFlags: OlyILFieldFlags *
        ilMemberFlags: OlyILMemberFlags
            -> OlyILFieldDefinitionHandle

    member CreateFieldReference:
        ilEnclosingTy: OlyILType *
        ilFieldDefHandle: OlyILFieldDefinitionHandle
            -> OlyILFieldReference

    member SetMainFunctionBody: ilEnclosingEntDefHandle: OlyILEntityDefinitionHandle * ilLocals: OlyILLocal imarray * ilExpr: OlyILExpression -> unit

    /// JIT means that the runtime will be spun up, import the internal assembly,
    ///  and emit the entry point.
    member TryGetIRFunctionBodyByJIT:
        ilFuncDefHandle: OlyILFunctionDefinitionHandle 
            -> OlyIRFunctionBody<DummyType, DummyFunction, DummyField> option