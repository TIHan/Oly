﻿module OptimizationTests

open Xunit

open Oly.Core
open Oly.Compiler.Syntax
open Oly.Compiler

open Oly.Runtime
open Oly.Runtime.Tools
open Oly.Runtime.CodeGen

let getMainIR (src: string) =
    let options = { OlyParsingOptions.Default with AnonymousModuleDefinitionAllowed = true }
    let tree = OlySyntaxTree.Parse(OlyPath.Create("olytest1.oly"), src, options)
    let options = OlyCompilationOptions.Default
    let options = { options with Debuggable = true; Executable = true }
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

let getOptimizedIR funcName (src: string) =
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
            if funcName = emittedFunc.Name then
                result <- Some irFuncBody

    let vm = OlyRuntime(DummyEmitter(onEmitBody))
    vm.ImportAssembly(ilAsm.ToReadOnly())
    vm.EmitEntryPoint()

    match result with
    | None -> failwith "Unable to find function."
    | Some body -> body.Expression

let getMainOptimizedIR (src: string) =
    getOptimizedIR "main" src

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
let ``Lambda will get inlined 11``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

class C =

    #[inline]
    M(#[inline] f: () -> ()): () =
        f()

main(): () =
    let c = C()
    c.M(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Let(rhsExpr=OlyIRExpression.Operation(op=OlyIROperation.New _);bodyExpr=bodyExpr) ->
        match bodyExpr with
        | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
            OlyAssert.Equal("Work", func.EmittedFunction.Name)
        | _ ->
            OlyAssert.Fail($"Unexpected pattern:\n{ir}")
    | _ ->
          OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 12``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

class C =

    #[inline]
    M1<T1>(x: T1, #[inline] f: T1 -> ()): () =
        f(x)

    M1<T1, T2, T3>(z: T1, f: __oly_int32 -> ()): () =
        f(5)

main(): () =
    let c = C()
    c.M1(5, _y -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Let(rhsExpr=OlyIRExpression.Operation(op=OlyIROperation.New _);bodyExpr=bodyExpr) ->
        match bodyExpr with
        | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
            OlyAssert.Equal("Work", func.EmittedFunction.Name)
        | _ ->
            OlyAssert.Fail($"Unexpected pattern:\n{ir}")
    | _ ->
          OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 13``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () =
    #[inline]
    let cmd() =
        f()

    cmd()

main(): () =
    M(Work)
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 14``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () =
    #[inline(never)]
    let cmd() =
        f()

    cmd()

main(): () =
    M(Work)
        """
        |> getOptimizedIR "cmd"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 14 - part 2``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () =
    #[inline(never)]
    let cmd() =
        f()

    cmd()
    cmd()

main(): () =
    M(Work)
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Let(_, localIndex, rhsExpr, bodyExpr) ->
        match rhsExpr with
        | OlyIRExpression.Operation(_, OlyIROperation.New _) ->
            match bodyExpr with
            | OlyIRExpression.Sequential(
                OlyIRExpression.Operation(_, OlyIROperation.Call(_, argExprs1, _)),
                OlyIRExpression.Operation(_, OlyIROperation.Call(_, argExprs2, _))
                ) when argExprs1.Length = 1 && argExprs2.Length = 1 ->
                match argExprs1[0] with
                | OlyIRExpression.Value(_, OlyIRValue.Local(targetLocalIndex1, _)) ->
                    match argExprs2[0] with
                    | OlyIRExpression.Value(_, OlyIRValue.Local(targetLocalIndex2, _)) ->
                        OlyAssert.Equal(localIndex, targetLocalIndex1)
                        OlyAssert.Equal(localIndex, targetLocalIndex2)
                    | _ ->
                        OlyAssert.Fail($"Unexpected pattern:\n{ir}")
                | _ ->
                    OlyAssert.Fail($"Unexpected pattern:\n{ir}")
            | _ ->
                OlyAssert.Fail($"Unexpected pattern:\n{ir}")
        | _ ->
            OlyAssert.Fail($"Unexpected pattern:\n{ir}")
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 15``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () -> () =
    #[inline(never)]
    let cmd() =
        f()

    cmd

main(): () =
    let result = M(Work)
    result()
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("cmd", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 16``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () -> () =
    #[inline]
    let cmd() =
        f()

    cmd

main(): () =
    let result = M(Work)
    result()
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 17``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () -> () =
    #[inline(never)]
    let cmd() =
        f()

    cmd

main(): () =
    let result = M(Work)
    result()
        """
        |> getOptimizedIR "cmd"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 17 - part 2``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () -> () =
    #[inline(never)]
    let cmd() =
        f()

    cmd

main(): () =
    let result = M(Work)
    result()
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("cmd", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 18``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () -> () =
    #[inline(never)]
    let cmd<T>() =
        f()

    cmd<()>

main(): () =
    let result = M(Work)
    result()
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("cmd", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 19``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () -> () =
    #[inline]
    let cmd<T>() =
        f()

    cmd<()>

main(): () =
    let result = M(Work)
    result()
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 20``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: () -> ()): () -> () =
    #[inline(never)]
    let cmd<T>() =
        f()

    cmd<()>

main(): () =
    let result = M(Work)
    result()
        """
        |> getOptimizedIR "cmd"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 21``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

main(): () =
    #[inline]
    let f() = Work()

    #[inline(never)]
    let cmd() =
        f()

    cmd()
        """
        |> getOptimizedIR "cmd"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 22``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

main(): () =
    #[inline]
    let f<T>() = Work()

    #[inline(never)]
    let cmd<T>() =
        f<T>()

    cmd<()>()
        """
        |> getOptimizedIR "cmd"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 23``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

main(): () =
    #[inline]
    let f() = Work()

    #[inline(never)]
    let cmd() =
        f()

    cmd()
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("cmd", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 24``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

main(): () =
    #[inline]
    let f<T>() = Work()

    #[inline(never)]
    let cmd<T>() =
        f<T>()

    cmd<()>()
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("cmd", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 25``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

main(): () =
    #[inline]
    let f() = Work()

    #[inline]
    let cmd() =
        f()

    cmd()
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("Work", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Lambda will get inlined 26``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

main(): () =
    #[inline]
    let f<T>() = Work()

    #[inline]
    let cmd<T>() =
        f<T>()

    cmd<()>()
        """
        |> getOptimizedIR "main"
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

[<Fact>]
let ``Lambda will not get inlined 2``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] mutable f: () -> ()): () =
    f <- f
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
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")
    | _ ->
        () // Pass

[<Fact>]
let ``Scoped lambda will get inlined``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 2``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline] f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 3``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] f: scoped () -> ()): () =
    f()

#[inline]
M2(#[inline] f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 4``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(f: scoped () -> ()): () =
    f()

#[inline]
M2(#[inline] f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 5``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(f: scoped () -> ()): () =
    f()

#[inline]
M2(f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 6``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

M1(f: scoped () -> ()): () =
    f()

M2(f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 7``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

M0(f: scoped () -> ()): () =
    f()

M1(f: scoped () -> ()): () =
    M0(f)

M2(f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 8``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] f: scoped () -> ()): () =
    f()

#[inline]
M2(#[inline] f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 9``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] f: scoped () -> ()): () =
    f()

#[inline]
M2(#[inline] f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 10``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M1(#[inline] f: scoped () -> ()): () =
    f()

#[inline]
M2(#[inline] f: scoped () -> ()): () =
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
let ``Scoped lambda will get inlined 11``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

class C =

    #[inline]
    M(#[inline] f: scoped () -> ()): () =
        f()

main(): () =
    let c = C()
    c.M(() -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Let(rhsExpr=OlyIRExpression.Operation(op=OlyIROperation.New _);bodyExpr=bodyExpr) ->
        match bodyExpr with
        | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
            OlyAssert.Equal("Work", func.EmittedFunction.Name)
        | _ ->
            OlyAssert.Fail($"Unexpected pattern:\n{ir}")
    | _ ->
          OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Scoped lambda will get inlined 12``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

class C =

    #[inline]
    M1<T1>(x: T1, #[inline] f: scoped T1 -> ()): () =
        f(x)

    M1<T1, T2, T3>(z: T1, f: scoped __oly_int32 -> ()): () =
        f(5)

main(): () =
    let c = C()
    c.M1(5, _y -> Work())
        """
        |> getMainOptimizedIR 
    match ir with
    | OlyIRExpression.Let(rhsExpr=OlyIRExpression.Operation(op=OlyIROperation.New _);bodyExpr=bodyExpr) ->
        match bodyExpr with
        | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
            OlyAssert.Equal("Work", func.EmittedFunction.Name)
        | _ ->
            OlyAssert.Fail($"Unexpected pattern:\n{ir}")
    | _ ->
          OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Scoped lambda will get inlined 13``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M2(#[inline(never)] cmd: scoped () -> ()): () = cmd()

#[inline]
M(#[inline] f: scoped () -> ()): () =
    M2(() -> f())

main(): () =
    M(Work)
        """
        |> getOptimizedIR "main"
    match ir with
    | OlyIRExpression.Operation(op=OlyIROperation.Call(func, _, _)) ->
        OlyAssert.Equal("cmd", func.EmittedFunction.Name)
    | _ ->
        OlyAssert.Fail($"Unexpected pattern:\n{ir}")

[<Fact>]
let ``Scoped lambda will not get inlined``() =
    let ir =
        """
module Program

#[inline(never)]
Work(): () = ()

#[inline]
M(#[inline(never)] f: scoped () -> ()): () =
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
