module TestPlatform

open System
open System.IO
open System.Text
open System.Runtime.Loader
open Oly.Core
open Oly.Metadata
open Oly.Runtime
open Oly.Emitters.DotNet
open Oly.Runtime.Target.DotNet
open Oly.Compiler
open Xunit
    
let globalSetup() =
    TestUtilities.Configuration.defaultReferences <- 
        let dotnetLocation = Path.GetDirectoryName(typeof<obj>.Assembly.Location)
        let asms = AppDomain.CurrentDomain.GetAssemblies()
        let consoleAsm = typeof<System.Console>.Assembly
        asms  
        |> Array.append [|consoleAsm|]
        |> Array.distinctBy (fun x -> x.FullName)
        |> Array.choose (fun x ->
            if x.Location.StartsWith(dotnetLocation) then
                use fs = File.OpenRead(x.Location)
                let asm = Importer.Import(x.GetName().Name, fs)
                let compRef = OlyCompilationReference.Create(OlyPath.Create(x.Location), 42UL, asm)
                compRef |> Some
            else
                None
        )
        |> ImArray.ofSeq
    TestUtilities.Configuration.implicitExtendsForEnum <- Some "System.Enum"
    TestUtilities.Configuration.implicitExtendsForStruct <- Some "System.ValueType"

let createEmitter(asm: OlyILAssembly) =
    OlyRuntimeClrEmitter(asm.Name, true, typeof<obj>.Assembly.GetName(), typeof<System.Console>.Assembly.GetName())

let configureRuntime(vm: OlyRuntime<ClrTypeInfo, ClrMethodInfo, ClrFieldInfo>) =
    ()

let emitterWrite(emitter: OlyRuntimeClrEmitter, isDebuggable) =
    let ms = new MemoryStream()
    use msPdb = new MemoryStream()
    emitter.Write(ms, msPdb, isDebuggable)
    ms.Position <- 0L
    ms

let private gate = obj()

let ilverify (ms: MemoryStream) =
    let tmpFile = Path.GetTempFileName()
    try File.Delete tmpFile with | _ -> ()
    let tmpFile = Path.ChangeExtension(tmpFile, ".dll")
    use fs = new FileStream(tmpFile, FileMode.Create)
    try
        let bytes = ms.GetBuffer()
        fs.Write(bytes, 0, bytes.Length)
        fs.Dispose()

        let ilverifyReferenceArgs =
            TestUtilities.Configuration.defaultReferences
            |> ImArray.map (fun x -> $"-r \"{x.Path.ToString().Replace('/', '\\')}\"")
            |> String.concat " "
        let ilverify = new ExternalProcess("ilverify", $"{tmpFile} -g ReturnPtrToStack -g ExpectedNumericType {ilverifyReferenceArgs}")

        let task = ilverify.RunAsync(Threading.CancellationToken.None)
        let result = task.Result
        if String.IsNullOrWhiteSpace(result.Errors) |> not then
            failwith result.Errors
    finally
        try File.Delete(tmpFile) with | _ -> ()

let private runILVerify = false

let runAndReturnOutput(ms: MemoryStream, input: string[]): string =
    if runILVerify then
        ilverify ms

    let context = AssemblyLoadContext(Guid.NewGuid().ToString(), isCollectible=true)
    try
        let rasm = context.LoadFromStream(ms)
        ms.Dispose()
        let mutable actualOutput = null
        try
            lock gate (fun () ->
                let builder = StringBuilder()
                let writer = new StringWriter(builder)
                Console.SetOut(writer)
                if rasm.EntryPoint = null then
                    failwith "Entry point not found."
                let args: obj array =
                    if input.Length = 0 then
                        [||]
                    else
                        [|input|]
                rasm.EntryPoint.Invoke(null, args) |> ignore
                actualOutput <- builder.ToString()
                writer.Dispose()
            )
        with
        | ex ->
            failwith $"Execution failed:\n{ex.Message}\n\n{ex.StackTrace}"
        actualOutput
    finally
        context.Unload()

let run (ms: MemoryStream, expectedOutput: string) =
    let actualOutput = runAndReturnOutput(ms, [||])
    Assert.Equal(expectedOutput, actualOutput)