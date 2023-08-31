﻿module OptimizationTests

open Xunit

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler

open Oly.Runtime
open Oly.Runtime.Tools
open Oly.Runtime.CodeGen

let getMainOptimizedIR (src: string) =
    let options = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true }
    let tree = OlySyntaxTree.Parse(OlyPath.Create("olytest1.oly"), src, options)
    let options = OlyCompilationOptions.Default
    let options = { options with Debuggable = false; Executable = true }
    let c = OlyCompilation.Create("olytest", [tree], options = options)

    match c.GetILAssembly(System.Threading.CancellationToken.None) with
    | Error diags -> failwithf "%A" diags
    | Ok ilAsm ->

    let mutable result = None
    let onEmitBody = 
        fun (emittedFunc: DummyFunction) (irLazyFuncBody: Lazy<OlyIRFunctionBody<DummyType, DummyFunction, DummyField>>) ->
            let irFuncBody = irLazyFuncBody.Value
            if "main" = emittedFunc.Name then
                match result with
                | Some _ -> ()
                | _ -> result <- Some irFuncBody

    let vm = OlyRuntime(DummyEmitter(onEmitBody))
    vm.ImportAssembly(ilAsm.ToReadOnly())
    vm.EmitEntryPoint()

    match result with
    | None -> failwith "Unable to find function."
    | Some body -> body.Expression

[<Fact>]
let ``Lambda will get inlined``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () =
    f()

main(): () =
    M(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 2``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () =
    f()
    f()

main(): () =
    M(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Sequential(
        OlyIRExpression.Operation(op=OlyIROperation.Call(func1, _, _)),
        OlyIRExpression.Operation(op=OlyIROperation.Call(func2, _, _))) ->
        OlyAssert.Equal("Work", func1.EmittedFunction.Name)
        OlyAssert.Equal("Work", func2.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 3``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] f: () -> ()): () =
    f()

#[inline]
M2(#[inline] f: () -> ()): () =
    M1(f)

main(): () =
    M2(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 4``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(f: () -> ()): () =
    f()

#[inline]
M2(#[inline] f: () -> ()): () =
    M1(f)

main(): () =
    M2(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 5``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(f: () -> ()): () =
    f()

#[inline]
M2(f: () -> ()): () =
    M1(f)

main(): () =
    // Lambda is inlined since it is small, this test verifies that.
    M2(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 6``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

M1(f: () -> ()): () =
    f()

M2(f: () -> ()): () =
    M1(f)

main(): () =
    // Lambda is inlined since it is small, this test verifies that.
    // Both M1 and M2 will be inlined since they are small.
    M2(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 7``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

M0(f: () -> ()): () =
    f()

M1(f: () -> ()): () =
    M0(f)

M2(f: () -> ()): () =
    M1(f)

main(): () =
    // Lambda is inlined since it is small, this test verifies that.
    // M0, M1 and M2 will be inlined since they are small.
    M2(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 8``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] f: () -> ()): () =
    f()

#[inline]
M2(#[inline] f: () -> ()): () =
    M1(() -> f())

main(): () =
    M2(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 9``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] f: () -> ()): () =
    f()

#[inline]
M2(#[inline] f: () -> ()): () =
    M1(() -> f())

main(): () =
    M2(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 10``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] f: () -> ()): () =
    f()

#[inline]
M2(#[inline] f: () -> ()): () =
    M1(() -> f())

main(): () =
    M2(Work)
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will not get inlined``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline(never)] f: () -> ()): () =
    f()

main(): () =
    M(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")
    | _ ->
        () // Pass
