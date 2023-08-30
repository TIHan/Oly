module OptimizationTests

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
        OlyAssert.Fail("Unexpected pattern")

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
        OlyAssert.Fail("Unexpected pattern")

