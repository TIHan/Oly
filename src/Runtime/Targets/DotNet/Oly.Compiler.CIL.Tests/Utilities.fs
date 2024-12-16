module Utilities

open TestUtilities
open System.Threading
open Oly.Metadata
open Oly.Runtime
open Oly.Core
open Oly.Compiler
open Oly.Compiler.Syntax

let mutable private globalSetupComplete = false
let mutable private globalSetupCompleteLockObj = obj()
let private globalSetupCheck() =
    if not globalSetupComplete then
        lock globalSetupCompleteLockObj 
            (fun () ->
                if not globalSetupComplete then
                    TestPlatform.globalSetup()
                    globalSetupComplete <- true
            )

let getDefaultReferences() =
    globalSetupCheck()
    TestUtilities.Configuration.defaultReferences

/// Will also assert that the syntax tree produced will equal the source.
let Oly (src: string) =
    globalSetupCheck()
    Oly src

/// Will also assert that the syntax tree produced will equal the source.
let OlyWithConditionalDefines (conditionalDefines: string list) (src: string) =
    globalSetupCheck()
    OlyWithConditionalDefines conditionalDefines src

/// Will also assert that the syntax tree produced will equal the source.
let OlyWithRef refSrc src =
    globalSetupCheck()
    OlyWithRef refSrc src

let OlyWithRefTwo refSrc2 refSrc1 src =
    globalSetupCheck()
    OlyWithRefTwo refSrc2 refSrc1 src

let OlyWithCRef cRef src =
    globalSetupCheck()
    OlyWithCRef cRef src

/// Does not check for syntax tree - source equality.
let OlyTwo src1 src2 =
    globalSetupCheck()
    OlyTwo src1 src2

let private emitAssembly (refAsms: OlyILAssembly imarray) (asm: OlyILAssembly) =
    let emitter = TestPlatform.createEmitter(asm)
    let runtime = OlyRuntime(emitter)

    refAsms
    |> ImArray.iter (fun x -> runtime.ImportAssembly(x.ToReadOnly()))

    runtime.ImportAssembly(asm.ToReadOnly())

    TestPlatform.configureRuntime(runtime)

    runtime.InitializeEmitter()
    runtime.EmitEntryPoint()
    
    TestPlatform.emitterWrite(emitter, asm.IsDebuggable)

let private runWithExpectedOutputAux expectedOutput (output: TestCompilationOutput) =
    let refAsms =
        output.c.Compilation.References
        |> ImArray.map (fun x -> 
            match x.GetILAssembly(CancellationToken.None) with
            | Ok ilAsm -> ilAsm
            | Error diags -> raise(System.Exception(OlyDiagnostic.PrepareForOutput(diags, CancellationToken.None)))
        )
#if DEBUG || CHECKED
    Log($"Testing - Debug Build")
#endif
    TestPlatform.run(emitAssembly refAsms output.ilAsmDebug, expectedOutput)
#if DEBUG || CHECKED
    Log($"Testing - Optimized Build")
#endif
    TestPlatform.run(emitAssembly refAsms output.ilAsm, expectedOutput)

let shouldRunWithExpectedOutput expectedOutput result =
    runWithExpectedOutputAux expectedOutput result

let runWithExpectedOutput expectedOutput (c: OlyCompilation) =
    TestCompilation.Create(c)
    |> withCompile
    |> shouldRunWithExpectedOutput expectedOutput

let runWithExpectedExceptionMessage expectedExceptionMsg (c: OlyCompilation) =
    let output =
        TestCompilation.Create(c)
        |> withCompile
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

/// TODO: Get rid of this.
let private printApi =
    "
#[intrinsic(\"print\")]
print(object): ()
    "

/// TODO: Get rid of this.
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

#[intrinsic("by_ref")]
alias byref<T>

#[intrinsic("by_ref_read_only")]
alias inref<T>
        """
    Oly (prelude + printApi + src)
