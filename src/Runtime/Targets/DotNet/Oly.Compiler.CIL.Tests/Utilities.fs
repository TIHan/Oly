module Utilities

open TestUtilities
open System.Threading
open Oly.Metadata
open Oly.Runtime
open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax

let private emitAssembly (refAsms: OlyILAssembly imarray) (asm: OlyILAssembly) =
    let emitter = TestPlatform.createEmitter(asm)
    let runtime = OlyRuntime(emitter)

    refAsms
    |> ImArray.iter (fun x -> runtime.ImportAssembly(x.ToReadOnly()))

    runtime.ImportAssembly(asm.ToReadOnly())
    runtime.InitializeEmitter()
    runtime.EmitEntryPoint()
    
    TestPlatform.emitterWrite emitter

let private runWithExpectedOutputAux expectedOutput (output: TestCompilationOutput) =
    let refAsms =
        output.c.Compilation.References
        |> ImArray.map (fun x -> 
            match x.GetILAssembly(CancellationToken.None) with
            | Ok ilAsm -> ilAsm
            | Error diags -> raise(System.Exception(OlyDiagnostic.PrepareForOutput(diags, CancellationToken.None)))
        )
    TestPlatform.run(emitAssembly refAsms output.ilAsm, expectedOutput)
    TestPlatform.run(emitAssembly refAsms output.ilAsmDebug, expectedOutput)

let shouldRunWithExpectedOutput expectedOutput result =
    runWithExpectedOutputAux expectedOutput result

let runWithExpectedOutput expectedOutput (c: OlyCompilation) =
    TestCompilation.Create(c)
    |> shouldCompile
    |> shouldRunWithExpectedOutput expectedOutput

let runWithExpectedExceptionMessage expectedExceptionMsg (c: OlyCompilation) =
    let output =
        TestCompilation.Create(c)
        |> shouldCompile
    let refAsms =
        output.c.Compilation.References
        |> ImArray.map (fun x -> 
            match x.GetILAssembly(CancellationToken.None) with
            | Ok ilAsm -> ilAsm
            | Error diags -> raise(System.Exception(OlyDiagnostic.PrepareForOutput(diags, CancellationToken.None)))
        )
    try
        TestPlatform.run(emitAssembly refAsms output.ilAsm, "(EXPECTED AN EXCEPTION MESSAGE)")
    with
    | :? System.Reflection.TargetInvocationException as ex ->
        match ex.InnerException with
        | null -> reraise()
        | ex ->
            OlyAssert.Equal(expectedExceptionMsg, ex.Message)
    | ex ->
        OlyAssert.Equal(expectedExceptionMsg, ex.Message)

    try
        TestPlatform.run(emitAssembly refAsms output.ilAsmDebug, "(EXPECTED AN EXCEPTION MESSAGE)")
    with
    | :? System.Reflection.TargetInvocationException as ex ->
        match ex.InnerException with
        | null -> reraise()
        | ex ->
            OlyAssert.Equal(expectedExceptionMsg, ex.Message)
    | ex ->
        OlyAssert.Equal(expectedExceptionMsg, ex.Message)

let private printApi =
    "
#[intrinsic(\"print\")]
print(object): ()
    "

let OlySharp src =
    let prelude =
        """
#[intrinsic("int16")]
alias int16

#[intrinsic("int32")]
alias int32

#[intrinsic("int64")]
alias int64

#[intrinsic("uint32")]
alias uint32

#[intrinsic("float32")]
alias float32

#[intrinsic("float64")]
alias float64

#[intrinsic("bool")]
alias bool

#[intrinsic("utf16")]
alias utf16

#[intrinsic("base_object")]
alias object

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>
        """
    Oly (prelude + printApi + src)
