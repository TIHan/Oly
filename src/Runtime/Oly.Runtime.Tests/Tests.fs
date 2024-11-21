module Tests

open System
open Xunit

open Oly.Core
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.CodeGen
open Oly.Runtime.Tools

let private failwithexpr irExpr =
    failwithf "\n%A" (Dump.DumpExpression irExpr)

[<Fact>]
let ``Test JsonFileStore``() =
    let path = OlyPath.Create(System.IO.Path.Combine(Environment.CurrentDirectory, ".olyworkspace\\test.json"))
    try
        use watcher = new DirectoryWatcher()
        watcher.WatchSubdirectories(Environment.CurrentDirectory)
        use fileStore = new JsonFileStore<{| doot: int32 |}>(path, {| doot = 5 |}, watcher)
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(5, result.doot)
        fileStore.UpdateContentsAsync({| doot = 6 |}, System.Threading.CancellationToken.None).Result |> ignore
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(6, result.doot)
        try System.IO.File.Delete(path.ToString()) with | _ -> ()
        System.Threading.Thread.Sleep(1000)
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(6, result.doot)
        Assert.True(System.IO.File.Exists(path.ToString()))

    finally
        try System.IO.File.Delete(path.ToString()) with | _ -> ()
        try System.IO.Directory.Delete(OlyPath.GetDirectory(path).ToString(), true) with | _ -> ()

[<Fact>]
let ``Test JsonFileStore 2``() =
    let path = OlyPath.Create(System.IO.Path.Combine(Environment.CurrentDirectory, ".olyworkspace2\\test2.json"))
    try
        use watcher = new DirectoryWatcher()
        watcher.WatchSubdirectories(Environment.CurrentDirectory)
        use fileStore = new JsonFileStore<{| doot: int32 |}>(path, {| doot = 5 |}, watcher)
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(5, result.doot)
        fileStore.UpdateContentsAsync({| doot = 6 |}, System.Threading.CancellationToken.None).Result |> ignore
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(6, result.doot)
        try System.IO.File.Delete(path.ToString()) with | _ -> ()
        System.Threading.Thread.Sleep(1000)
        Assert.False(System.IO.File.Exists(path.ToString()))
    finally
        try System.IO.File.Delete(path.ToString()) with | _ -> ()
        try System.IO.Directory.Delete(OlyPath.GetDirectory(path).ToString(), true) with | _ -> ()

[<Fact>]
let ``Test JsonFileStore 3``() =
    let path = OlyPath.Create(System.IO.Path.Combine(Environment.CurrentDirectory, ".olyworkspace3\\test3.json"))
    try
        use watcher = new DirectoryWatcher()
        watcher.WatchSubdirectories(Environment.CurrentDirectory)
        use fileStore = new JsonFileStore<{| doot: int32 |}>(path, {| doot = 5 |}, watcher)
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(5, result.doot)
        fileStore.UpdateContentsAsync({| doot = 6 |}, System.Threading.CancellationToken.None).Result |> ignore
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(6, result.doot)

        try System.IO.Directory.Delete(OlyPath.GetDirectory(path).ToString(), true) with | _ -> ()
        System.Threading.Thread.Sleep(1000)
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(6, result.doot)
        Assert.True(System.IO.File.Exists(path.ToString()))

        try System.IO.Directory.Delete(OlyPath.GetDirectory(path).ToString(), true) with | _ -> ()
        System.Threading.Thread.Sleep(1000)
        System.IO.Directory.CreateDirectory(OlyPath.GetDirectory(path).ToString()) |> ignore
        System.IO.File.WriteAllText(path.ToString(), "{ \"doot\": 9 }")
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(9, result.doot)
        Assert.True(System.IO.File.Exists(path.ToString()))
    finally
        try System.IO.File.Delete(path.ToString()) with | _ -> ()
        try System.IO.Directory.Delete(OlyPath.GetDirectory(path).ToString(), true) with | _ -> ()

[<Fact>]
let ``Test JsonFileStore 4``() =
    let path = OlyPath.Create(System.IO.Path.Combine(Environment.CurrentDirectory, ".olyworkspace4\\test4.json"))
    try
        use watcher = new DirectoryWatcher()
        watcher.WatchSubdirectories(Environment.CurrentDirectory)
        use fileStore = new JsonFileStore<{| doot: int32 |}>(path, {| doot = 5 |}, watcher)
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(5, result.doot)
        fileStore.UpdateContentsAsync({| doot = 6 |}, System.Threading.CancellationToken.None).Result |> ignore
        let result = fileStore.GetContentsAsync(System.Threading.CancellationToken.None).Result
        Assert.Equal(6, result.doot)
        try System.IO.Directory.Delete(OlyPath.GetDirectory(path).ToString(), true) with | _ -> ()
        System.Threading.Thread.Sleep(1000)
        Assert.False(System.IO.File.Exists(path.ToString()))
    finally
        try System.IO.File.Delete(path.ToString()) with | _ -> ()
        try System.IO.Directory.Delete(OlyPath.GetDirectory(path).ToString(), true) with | _ -> ()

[<Fact>]
let ``DummyAssemblyBuilder instantiation`` () =
    let builder = DummyAssemblyBuilder(true)
    Assert.NotNull(builder)

[<Fact>]
let ``Get default main IR`` () =
    let builder = DummyAssemblyBuilder(true)
    let irFuncBodyOpt = builder.TryGetIRFunctionBodyByJIT(builder.MainFunctionDefinitionHandle)
    Assert.True(irFuncBodyOpt.IsSome)
    let irFuncBody = irFuncBodyOpt.Value
    Assert.Equal(0, irFuncBody.LocalCount)
    match irFuncBody.Expression with
    | OlyIRExpression.None _ -> ()
    | _ -> failwith "Expected OlyIRExpression.None"

[<Fact>]
let ``Should not get function body because it is not invoked in main`` () =
    let builder = DummyAssemblyBuilder(true)

    let ilEntDefHandle = builder.CreateEntityDefinitionHandle()
    let ilFuncDefHandle = 
        builder.CreateFunctionDefinition(
            ilEntDefHandle,
            "test", 
            ImArray.empty,
            ImArray.empty,
            OlyILFunctionFlags.None, 
            OlyILMemberFlags.Static, 
            ImArray.empty,
            OlyILExpression.None(OlyILDebugSourceTextRange.Empty),
            OlyILTypeVoid
        )
        |> fst
    let _ilTy = 
        builder.CreateType(
            ilEntDefHandle,
            OlyILEntityKind.Module,
            "Test",
            ImArray.createOne ilFuncDefHandle,
            ImArray.empty
        )

    let irFuncBodyOpt = builder.TryGetIRFunctionBodyByJIT(ilFuncDefHandle)
    Assert.True(irFuncBodyOpt.IsNone)

[<Fact>]
let ``Should get function body because it is invoked in main`` () =
    let builder = DummyAssemblyBuilder(true)

    let ilLocals = ImArray.empty
    let ilExpr = OlyILExpression.None(OlyILDebugSourceTextRange.Empty)
    let ilExprTy = OlyILTypeVoid

    let ilEntDefHandle = builder.CreateEntityDefinitionHandle()
    let ilFuncDefHandle, ilFuncSpecHandle = 
        builder.CreateFunctionDefinition(
            ilEntDefHandle,
            "test", 
            ImArray.empty,
            ImArray.empty,
            OlyILFunctionFlags.InlineNever, 
            OlyILMemberFlags.Static, 
            ilLocals,
            ilExpr,
            ilExprTy
        )

    let ilTy = 
        builder.CreateType(
            ilEntDefHandle,
            OlyILEntityKind.Module,
            "Test",
            ImArray.createOne ilFuncDefHandle,
            ImArray.empty
        )

    let ilEnclosing =
        match ilTy with
        | OlyILTypeEntity(ilEntInst) ->
            OlyILEnclosing.Entity(ilEntInst)
        | _ ->
            failwith "Expected an entity."

    builder.SetMainFunctionBody(ilEntDefHandle, ImArray.empty,
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.Call(
                OlyILFunctionInstance(
                    ilEnclosing,
                    ilFuncSpecHandle,
                    ImArray.empty,
                    ImArray.empty
                ),
                ImArray.empty
            )
        )
    )

    let irFuncBodyOpt = builder.TryGetIRFunctionBodyByJIT(ilFuncDefHandle)
    Assert.True(irFuncBodyOpt.IsSome)

/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////

let getIR 
        (builder: DummyAssemblyBuilder) 
        (ilFuncTyPars: OlyILTypeParameter imarray)
        (ilFuncPars: OlyILParameter imarray) 
        (ilLocals: OlyILLocal imarray) 
        (ilExpr: OlyILExpression) 
        (ilExprTy: OlyILType)
        ilMainExprF =

    let ilEntDefHandle = builder.CreateEntityDefinitionHandle()
    let ilFuncDefHandle, ilFuncSpecHandle = 
        builder.CreateFunctionDefinition(
            ilEntDefHandle,
            "test", 
            ilFuncTyPars,
            ilFuncPars,
            OlyILFunctionFlags.None, 
            OlyILMemberFlags.Static, 
            ilLocals,
            ilExpr,
            ilExprTy
        )

    let ilTy = 
        builder.CreateType(
            ilEntDefHandle,
            OlyILEntityKind.Module,
            "Test",
            ImArray.createOne ilFuncDefHandle,
            ImArray.empty
        )

    let ilEnclosing =
        match ilTy with
        | OlyILTypeEntity(ilEntInst) ->
            OlyILEnclosing.Entity(ilEntInst)
        | _ ->
            failwith "Expected an entity."

    let locals = builder.CreateLocalManager()
    let ilMainExpr = ilMainExprF locals ilEnclosing ilFuncSpecHandle

    let ilMainExpr =
        match ilExprTy with
        | OlyILTypeVoid -> ilMainExpr
        | _ ->
            OlyILExpression.Operation(OlyILDebugSourceTextRange.Empty, OlyILOperation.Ignore(ilMainExpr))

    builder.SetMainFunctionBody(ilEntDefHandle, locals.GetLocals(), ilMainExpr)

    let result = builder.TryGetIRFunctionBodyByJIT(ilFuncDefHandle).Value
    result.LocalCount, result.Expression

[<AutoOpen>]
module OlyExpressionHelpers =

    let ConstantInt32 value =
        OlyILExpression.Value(
            OlyILDebugSourceTextRange.Empty,
            OlyILValue.Constant(OlyILConstant.Int32(value))
        )

    let (|ConstantInt32|_|) expr =
        match expr with
        | OlyIRExpression.Value(_, OlyIRValue.Constant(OlyIRConstant.Int32(value), _)) ->
            Some value
        | _ ->
            None

    let Default ilTy =
        OlyILExpression.Value(
            OlyILDebugSourceTextRange.Empty,
            OlyILValue.Default ilTy
        )

    let (|DefaultStruct|_|) expr =
        match expr with
        | OlyIRExpression.Value(_, OlyIRValue.Default(irTy)) ->
            Some irTy
        | _ ->
            None

    let Local localIndex =
        OlyILExpression.Value(
            OlyILDebugSourceTextRange.Empty,
            OlyILValue.Local(localIndex)
        )

    let (|Local|_|) expr =
        match expr with
        | OlyIRExpression.Value(_, OlyIRValue.Local(localIndex, _)) ->
            Some localIndex
        | _ ->
            None

    let LocalAddress localIndex ilByRefKind =
        OlyILExpression.Value(
            OlyILDebugSourceTextRange.Empty,
            OlyILValue.LocalAddress(localIndex, ilByRefKind)
        )

    let (|LocalAddress|_|) expr =
        match expr with
        | OlyIRExpression.Value(_, OlyIRValue.LocalAddress(localIndex, byRefKind, _)) ->
            Some(localIndex, byRefKind)
        | _ ->
            None        

    let Argument argIndex =
        OlyILExpression.Value(
            OlyILDebugSourceTextRange.Empty,
            OlyILValue.Argument(argIndex)
        )

    let ArgumentAddress argIndex ilByRefKind =
        OlyILExpression.Value(
            OlyILDebugSourceTextRange.Empty,
            OlyILValue.ArgumentAddress(argIndex, ilByRefKind)
        )

    let LoadField ilFieldRef ilReceiverExpr =
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.LoadField(ilFieldRef, ilReceiverExpr)
        )

    let (|LoadField|_|) irExpr =
        match irExpr with
        | OlyIRExpression.Operation(_, OlyIROperation.LoadField(field, irReceiverExpr, _)) ->
            Some(field, irReceiverExpr)
        | _ ->
            None

    let LoadFieldAddress ilFieldRef ilReceiverExpr ilByRefKind =
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.LoadFieldAddress(ilFieldRef, ilReceiverExpr, ilByRefKind)
        )

    let (|LoadFieldAddress|_|) irExpr =
        match irExpr with
        | OlyIRExpression.Operation(_, OlyIROperation.LoadFieldAddress(field, irByRefKind, irReceiverExpr, _)) ->
            Some(field, irReceiverExpr, irByRefKind)
        | _ ->
            None

    let LoadFromAddress ilArgExpr =
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.LoadFromAddress(ilArgExpr)
        )

    let (|LoadFromAddress|_|) expr =
        match expr with
        | OlyIRExpression.Operation(_, OlyIROperation.LoadFromAddress(argExpr, _)) ->
            Some(argExpr)
        | _ ->
            None

    let Sequential expr1 expr2 =
        OlyILExpression.Sequential(expr1, expr2)

    let (|Sequential|_|) expr =
        match expr with
        | OlyIRExpression.Sequential(expr1, expr2) ->
            Some(expr1, expr2)
        | _ ->
            None

    let Let localIndex rhsExpr bodyExpr =
        OlyILExpression.Let(localIndex, rhsExpr, bodyExpr)

    let (|Let|_|) expr =
        match expr with
        | OlyIRExpression.Let(_, localIndex, rhsExpr, bodyExpr) ->
            Some(localIndex, rhsExpr, bodyExpr)
        | _ ->
            None

    let Call ilEnclosing ilFuncSpecHandle ilTyArgs ilWitnesses ilArgExprs =
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.Call(
                OlyILFunctionInstance(
                    ilEnclosing,
                    ilFuncSpecHandle,
                    ilTyArgs,
                    ilWitnesses
                ),
                ilArgExprs
            )
        )

    let Store localIndex rhsExpr =
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.Store(localIndex, rhsExpr)
        )

    let (|Store|_|) expr =
        match expr with
        | OlyIRExpression.Operation(_, OlyIROperation.Store(n, rhsExpr, _)) ->
            Some(n, rhsExpr)
        | _ ->
            None

    let StoreToAddress lhsExpr rhsExpr =
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.StoreToAddress(lhsExpr, rhsExpr)
        )

    let NewTuple tyInst argExprs =
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.NewTuple(tyInst, argExprs, ImArray.empty)
        )

    let LoadTupleElement receiverExpr index =
        OlyILExpression.Operation(
            OlyILDebugSourceTextRange.Empty,
            OlyILOperation.LoadTupleElement(receiverExpr, index)
        )

[<Fact>]
let ``Should not eliminate local because isDebuggable is true`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = true)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (ConstantInt32 456)
            (Local local0)

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    Assert.Equal(1, finalLocalCount)

    match irExpr with
    | Let(localIndex, _, Local n) ->
        Assert.Equal(localIndex, n)
    | _ ->
        failwithexpr irExpr

[<Fact>]
let ``Should eliminate local because isDebuggable is false`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (ConstantInt32 456)
            (Local local0)

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    Assert.Equal(0, finalLocalCount)

    match irExpr with
    | ConstantInt32(value) ->
        Assert.Equal(456, value)
    | _ ->
        failwithexpr irExpr

[<Fact>]
let ``Should eliminate local because isDebuggable is false 2`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (ConstantInt32 456)
            (Local local0)

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    Assert.Equal(0, finalLocalCount)

    match irExpr with
    | ConstantInt32(value) ->
        Assert.Equal(456, value)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "Ssa not completed")>]
let ``Should eliminate local because SSA works even when the local is mutated`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (ConstantInt32 456)
            (Sequential
                (Store local0 (ConstantInt32 789))
                (Local local0)
            )

    let finalLocalCount, finalExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    Assert.Equal(0, finalLocalCount)

// Elimination

[<Fact(Skip = "not ready")>]
let ``Test 1`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeByRef(OlyILTypeInt32, OlyILByRefKind.ReadOnly), OlyILLocalFlags.None)
        Let local0 (ConstantInt32 456)
            (Let local1 (LocalAddress local0 OlyILByRefKind.ReadOnly)
                (LoadFromAddress (Local local1))
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | ConstantInt32(value) ->
        Assert.Equal(0, finalLocalCount)
        Assert.Equal(456, value)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "not ready")>]
let ``Test 2`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = 456
                let local1 = &local1
                local1
        *)
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeByRef(OlyILTypeInt32, OlyILByRefKind.ReadWrite), OlyILLocalFlags.None)
        Let local0 (ConstantInt32 456)
            (Let local1 (LocalAddress local0 OlyILByRefKind.ReadWrite)
                (LoadFromAddress (Local local1))
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | ConstantInt32(456) ->
        Assert.Equal(0, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "not ready")>]
let ``Test 3`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = 456
                let local1 = &local0
                local1 <- 789
                local0
            Turns into:
                789
        *)
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeByRef(OlyILTypeInt32, OlyILByRefKind.ReadWrite), OlyILLocalFlags.None)
        Let local0 (ConstantInt32 456)
            (Let local1 (LocalAddress local0 OlyILByRefKind.ReadWrite)
                (Sequential
                    (StoreToAddress (Local local1) (ConstantInt32 789))
                    (Local local0)
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | ConstantInt32(789) ->
        Assert.Equal(0, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "not ready")>]
let ``Test 4`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = 456
                let local1 = &local0
                local1 <- 789
                fromAddr(local1)
            Turns into:
                789
        *)
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeByRef(OlyILTypeInt32, OlyILByRefKind.ReadWrite), OlyILLocalFlags.None)
        Let local0 (ConstantInt32 456)
            (Let local1 (LocalAddress local0 OlyILByRefKind.ReadWrite)
                (Sequential
                    (StoreToAddress (Local local1) (ConstantInt32 789))
                    (LoadFromAddress (Local local1))
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | ConstantInt32(789) ->
        Assert.Equal(1, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "not ready")>]
let ``Test 5`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let fieldDefX = builder.CreateFieldDefinition("X", OlyILTypeInt32, OlyILFieldFlags.Mutable, OlyILMemberFlags.Public)
    let tyStructA = builder.CreateType(builder.CreateEntityDefinitionHandle(), OlyILEntityKind.Struct, "StructA", ImArray.empty, ImArray.createOne fieldDefX)
    let fieldRefX = builder.CreateFieldReference(tyStructA, fieldDefX)

    let ilExpr =
        (*
            Pseudo:
                let local0 = default: StructA
                let local1 = local0.X
                let local2 = local0.X
                local2
            Turns into:
                let local0 = default: StructA
                local0.X
        *)
        let local0 = locals.CreateLocal(tyStructA, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local2 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (Default tyStructA)
            (Let local1 (LoadField fieldRefX (LocalAddress local0 OlyILByRefKind.ReadOnly))
                (Let local2 (LoadField fieldRefX (LocalAddress local0 OlyILByRefKind.ReadOnly))
                    (Local local2)
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | Let(0, DefaultStruct _, LoadField(_, LocalAddress(0, OlyIRByRefKind.ReadOnly))) ->
        Assert.Equal(1, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "not ready")>]
let ``Test 6`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let fieldDefX = builder.CreateFieldDefinition("X", OlyILTypeInt32, OlyILFieldFlags.Mutable, OlyILMemberFlags.Public)
    let tyStructA = builder.CreateType(builder.CreateEntityDefinitionHandle(), OlyILEntityKind.Struct, "StructA", ImArray.empty, ImArray.createOne fieldDefX)
    let fieldRefX = builder.CreateFieldReference(tyStructA, fieldDefX)

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = default: StructA
                let local1 = local0.X
                let local2 = local0.X
                local2
            Turns into:
                let local0 = default: StructA
                local0.X
        *)
        let local0 = locals.CreateLocal(tyStructA, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local2 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (Default tyStructA)
            (Let local1 (LoadField fieldRefX (LocalAddress local0 OlyILByRefKind.ReadOnly))
                (Let local2 (LoadField fieldRefX (LocalAddress local0 OlyILByRefKind.ReadOnly))
                    (Local local2)
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | Let(0, DefaultStruct _, LoadField(_, LocalAddress(0, OlyIRByRefKind.ReadOnly))) ->
        Assert.Equal(1, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "not ready")>]
let ``Test 7`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let fieldDefX = builder.CreateFieldDefinition("X", OlyILTypeInt32, OlyILFieldFlags.Mutable, OlyILMemberFlags.Public)
    let tyStructA = builder.CreateType(builder.CreateEntityDefinitionHandle(), OlyILEntityKind.Struct, "StructA", ImArray.empty, ImArray.createOne fieldDefX)
    let fieldRefX = builder.CreateFieldReference(tyStructA, fieldDefX)

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = default: StructA
                let local1 = local0.X
                let local2 = local0.X
                local0 <- default
                local2
            Turns into:
                let local0 = default: StructA
                local0.X
        *)
        let local0 = locals.CreateLocal(tyStructA, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local2 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (Default tyStructA)
            (Let local1 (LoadField fieldRefX (LocalAddress local0 OlyILByRefKind.ReadWrite))
                (Let local2 (LoadField fieldRefX (LocalAddress local0 OlyILByRefKind.ReadWrite))
                    (Sequential
                        (Store 0 (Default tyStructA))
                        (Local local2)
                    )   
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | Let(0, DefaultStruct _,
        LoadField(_, LocalAddress(0, OlyIRByRefKind.ReadOnly))) ->
        Assert.Equal(1, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact>]
let ``Test 8`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        (*
            Pseudo:
                let local0 = 1
                let local1 = local0
                let local2 = local1
                local2
            Turns into:
                1
        *)
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local2 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (ConstantInt32 1)
            (Let local1 (Local local0)
                (Let local2 (Local local1)
                    (Local local2)
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | ConstantInt32(1) ->
        Assert.Equal(0, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact>]
let ``Test 9`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = 1
                let local1 = local0
                let local2 = local1
                local2
            Turns into:
                1
        *)
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local2 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (ConstantInt32 1)
            (Let local1 (Local local0)
                (Let local2 (Local local1)
                    (Local local2)
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | ConstantInt32(1) ->
        Assert.Equal(0, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "Ssa not completed")>]
let ``Test 10`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = 1
                local0 <- 2
                let local1 = local0
                let local2 = local1
                local2
            Turns into:
                2

            The reason this works is SSA and because we do quick local elimination:
                Let(n, rhsExpr, Value(Local(n2)) when n = n2 -> rhsExpr
        *)
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local2 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (ConstantInt32 1)
            (Sequential
                (Store 0 (ConstantInt32 2))
                (Let local1 (Local local0)
                    (Let local2 (Local local1)
                        (Local local2)
                    )
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | ConstantInt32(2) ->
        Assert.Equal(0, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "not ready")>]
let ``Test 11`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = 1
                let local1 = local0
                let local2 = local1
                local0 <- 2
                local2
            Turns into:
                let mutable local0 = 1
                let local1 = local0
                local0 <- 2
                local2
        *)
        let local0 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local2 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (ConstantInt32 1)
            (Let local1 (Local local0)
                (Let local2 (Local local1)
                    (Sequential
                        (Store 0 (ConstantInt32 2))
                        (Local local2)
                    )
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | ConstantInt32(1) ->
        Assert.Equal(0, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact(Skip = "not ready")>]
let ``Test 12`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilExprTy = OlyILTypeInt32

    let fieldDefX = builder.CreateFieldDefinition("X", OlyILTypeInt32, OlyILFieldFlags.Mutable, OlyILMemberFlags.Public)
    let tyStructA = builder.CreateType(builder.CreateEntityDefinitionHandle(), OlyILEntityKind.Struct, "StructA", ImArray.empty, ImArray.createOne fieldDefX)
    let fieldRefX = builder.CreateFieldReference(tyStructA, fieldDefX)

    let ilExpr =
        (*
            Pseudo:
                let mutable local0 = default: StructA
                let local0r = &local0
                let local1 = local0r.X
                let local2 = local0r.X
                local2
            Turns into:
                let local0 = default: StructA
                local0.X
        *)
        let local0 = locals.CreateLocal(tyStructA, OlyILLocalFlags.None)
        let local0r = locals.CreateLocal(OlyILTypeByRef(tyStructA, OlyILByRefKind.ReadWrite), OlyILLocalFlags.None)
        let local1 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        let local2 = locals.CreateLocal(OlyILTypeInt32, OlyILLocalFlags.None)
        Let local0 (Default tyStructA)
            (Let local0r (LocalAddress local0 OlyILByRefKind.ReadWrite)
                (Let local1 (LoadField fieldRefX (Local local0r))
                    (Let local2 (LoadField fieldRefX (Local local0r))
                        (Local local2)
                    )
                )
            )

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    match irExpr with
    | Let(0, DefaultStruct _, LoadField(_, LocalAddress(0, OlyIRByRefKind.ReadOnly))) ->
        Assert.Equal(1, finalLocalCount)
    | _ ->
        failwithexpr irExpr

[<Fact>]
let ``Should eliminate tuple`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilTupleTy = OlyILTypeTuple(ImArray.createTwo OlyILTypeInt32 OlyILTypeInt32, ImArray.empty)
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        let local0 = locals.CreateLocal(ilTupleTy, OlyILLocalFlags.None)
        Let local0 (NewTuple (ImArray.createTwo OlyILTypeInt32 OlyILTypeInt32) (ImArray.createTwo (ConstantInt32 456) (ConstantInt32 789)))
            (LoadTupleElement (Local local0) 0)

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    Assert.Equal(0, finalLocalCount)

    match irExpr with
    | ConstantInt32(value) ->
        Assert.Equal(456, value)
    | _ ->
        failwithexpr irExpr

[<Fact>]
let ``Should eliminate tuple 2`` () =
    let builder = DummyAssemblyBuilder(isDebuggable = false)

    let locals = builder.CreateLocalManager()
    let ilTupleTy = OlyILTypeTuple(ImArray.createTwo OlyILTypeInt32 OlyILTypeInt32, ImArray.empty)
    let ilExprTy = OlyILTypeInt32

    let ilExpr =
        let local0 = locals.CreateLocal(ilTupleTy, OlyILLocalFlags.None)
        Let local0 (NewTuple (ImArray.createTwo OlyILTypeInt32 OlyILTypeInt32) (ImArray.createTwo (ConstantInt32 456) (ConstantInt32 789)))
            (LoadTupleElement (Local local0) 1)

    let finalLocalCount, irExpr = 
        getIR
            builder
            ImArray.empty
            ImArray.empty
            (locals.GetLocals())
            ilExpr
            ilExprTy
            (fun _ ilEnclosing ilFuncSpecHandle ->
                Call
                    ilEnclosing
                    ilFuncSpecHandle
                    ImArray.empty
                    ImArray.empty
                    ImArray.empty
            )

    Assert.Equal(0, finalLocalCount)

    match irExpr with
    | ConstantInt32(value) ->
        Assert.Equal(789, value)
    | _ ->
        failwithexpr irExpr