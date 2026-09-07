module DotNet.Conformance.DebugTests

open Xunit
open TestUtilities
open Utilities

open System
open System.Threading

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Runtime
open Oly.Runtime.CodeGen
open DotNet.Metadata.ClrPatterns

let getEmitter (output: TestCompilationOutput) =
    let refAsms =
        output.c.Compilation.References
        |> ImArray.map (fun x -> 
            match x.GetILAssembly(CancellationToken.None) with
            | Ok ilAsm -> ilAsm
            | Error diags -> raise(System.Exception(OlyDiagnostic.PrepareForOutput(diags, CancellationToken.None)))
        )
    let emitter = TestPlatform.createEmitter(output.ilAsmDebug)
    let runtime = OlyRuntime(emitter)

    refAsms
    |> ImArray.iter (fun x -> runtime.ImportAssembly(x.ToReadOnly()))

    runtime.ImportAssembly(output.ilAsmDebug.ToReadOnly())

    TestPlatform.configureRuntime(runtime)

    runtime.InitializeEmitter()
    runtime.EmitEntryPoint()
    ((runtime: IOlyVirtualMachine<_, _, _>), emitter)

let getDebugMsilInstructions enclosingTypeName functionName parameterCount kind (output: TestCompilationOutput) =
    let (vm, emitter) = getEmitter output
    let meth = emitter.GetMethodDefinitionBuilder(vm, enclosingTypeName, functionName, parameterCount, kind)
    meth.BodyInstructions

[<Fact>]
let ``Should have two sequence points in M``() =
    let src =
        """
module Test.Module

M(): () =
    ()

main(): () =
    M()
        """

    let instrs =
        Oly src
        |> withCompile
        |> getDebugMsilInstructions "Test.Module" "M" 0 OlyFunctionKind.Static
        |> List.ofSeq

    match instrs with 
    | [
        I.SequencePoint(_, 4, 4, 9, 10)
        I.Nop
        I.SequencePoint(_, 5, 5, 5, 7)
        I.Nop
        I.HiddenSequencePoint
        I.Ret
      ] -> ()
    | _ ->
        OlyAssert.Fail("Invalid instructions.")

[<Fact>]
let ``Should have correct sequence points for constructor``() =
    let src =
        """
module Test.Module

struct S =
    new() =
        this { }

main(): () =
    let _ = S()
        """

    let instrs =
        Oly src
        |> withCompile
        |> getDebugMsilInstructions "Test.Module.S" "__oly_ctor" 0 OlyFunctionKind.Instance
        |> List.ofSeq

    match instrs with 
    | [
        I.SequencePoint(_, 5, 5, 11, 12)
        I.Nop
        I.SequencePoint(_, 6, 6, 9, 13)
        I.Nop
        I.Ldarg 0
        I.Pop
        I.SequencePoint(_, 6, 6, 16, 17)
        I.Nop
        I.HiddenSequencePoint
        I.Ret
      ] -> ()
    | _ ->
        OlyAssert.Fail("Invalid instructions.")

[<Fact>]
let ``Should have correct sequence points for constructor 2``() =
    let src =
        """
module Test.Module

struct S =
    field x: __oly_int32
    new(x: __oly_int32) =
        this { 
            x = x
        }

main(): () =
    let _ = S(123)
        """

    let instrs =
        Oly src
        |> withCompile
        |> getDebugMsilInstructions "Test.Module.S" "__oly_ctor" 1 OlyFunctionKind.Instance
        |> List.ofSeq

    match instrs with 
    | [
        I.SequencePoint(_, 6, 6, 25, 26)
        I.Nop
        I.SequencePoint(_, 7, 7, 9, 13)
        I.Nop
        I.Ldarg 0
        I.Pop
        I.SequencePoint(_, 8, 8, 13, 18)
        I.Nop
        I.Ldarg 0
        I.Ldarg 1
        I.Stfld _
        I.SequencePoint(_, 9, 9, 9, 10)
        I.Nop
        I.HiddenSequencePoint
        I.Ret
      ] -> ()
    | _ ->
        OlyAssert.Fail("Invalid instructions.")

[<Fact>]
let ``Should have correct sequence points for function calls``() =
    let src =
        """
module Test.Module

M2(): __oly_int32 = 456

M(x: __oly_int32): () =
    ()

main(): () =
    M(M2())
        """

    let instrs =
        Oly src
        |> withCompile
        |> getDebugMsilInstructions "Test.Module" "main" 0 OlyFunctionKind.Static
        |> List.ofSeq

    match instrs with 
    | [
        I.SequencePoint(_, 9, 9, 12, 13)
        I.Nop
        I.SequencePoint(_, 10, 10, 5, 12)
        I.Nop
        I.SequencePoint(_, 10, 10, 7, 11)
        I.Nop
        I.Call _
        I.Call _
        I.HiddenSequencePoint
        I.Ret
      ] -> ()
    | _ ->
        OlyAssert.Fail("Invalid instructions.")

[<Fact>]
let ``Should have correct sequence points for closure``() =
    let src =
        """
module Test.Module

main(): () =
    let mutable x = 1
    let f() =
        x <- 5
    f()
        """

    let instrs =
        Oly src
        |> withCompile
        // TODO: The name '__oly_gen_0_' is not really correct. Are we generating a closure with this name? Maybe it should just be 'f__oly_closure_0'.
        |> getDebugMsilInstructions "Test.Module.__oly_gen_0_f__oly_closure_0" "f" 0 OlyFunctionKind.Instance
        |> List.ofSeq

    match instrs with 
    | [
        I.SequencePoint(_, 6, 6, 13, 14)
        I.Nop
        I.SequencePoint(_, 7, 7, 9, 15)
        I.Nop
        I.Ldarg 0 // RefCell is an array with one item
        I.Ldfld _
        I.Ldc_i4 0
        I.Ldc_i4 5
        I.Stelem _
        I.HiddenSequencePoint
        I.Ret
      ] -> ()
    | _ ->
        OlyAssert.Fail("Invalid instructions.")

[<Fact>]
let ``Should have correct sequence points for closure 2``() =
    let src =
        """
module Test.Module

main(): () =
    let mutable x = 1
    let f =
        () ->
            x <- 5
    f()
        """

    let instrs =
        Oly src
        |> withCompile
        // TODO: The name '__oly_gen_0_' is not really correct. Are we generating a closure with this name? Maybe it should just be 'f__oly_closure_0'.
        |> getDebugMsilInstructions "Test.Module.__oly_gen_0_f__oly_closure_0" "f" 0 OlyFunctionKind.Instance
        |> List.ofSeq

    match instrs with 
    | [
        I.SequencePoint(_, 7, 7, 12, 14)
        I.Nop
        I.SequencePoint(_, 8, 8, 13, 19)
        I.Nop
        I.Ldarg 0 // RefCell is an array with one item
        I.Ldfld _
        I.Ldc_i4 0
        I.Ldc_i4 5
        I.Stelem _
        I.HiddenSequencePoint
        I.Ret
      ] -> ()
    | _ ->
        OlyAssert.Fail("Invalid instructions.")