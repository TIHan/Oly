module TestPlatform

open System
open System.IO
open System.Text
open System.Runtime.Loader
open Oly.Core
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.Clr.Emitter
open Oly.Runtime.Target.DotNet
open Oly.Compiler
open Xunit
    
let globalSetup() =
    TestUtilities.Configuration.defaultReferences <- 
        let dotnetLocation = Path.GetDirectoryName(typeof<obj>.Assembly.Location)
        let asms = AppDomain.CurrentDomain.GetAssemblies()
        asms 
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

let emitterWrite(emitter: OlyRuntimeClrEmitter) =
    let ms = new MemoryStream()
    use msPdb = new MemoryStream()
#if DEBUG
    emitter.Write(ms, msPdb, (* isDebuggable *) true)
#else
    emitter.Write(ms, msPdb, (* isDebuggable *) false)
#endif
    ms.Position <- 0L
    ms

let private gate = obj()

//let private runtimeconfigJson =
//    """{
//  "runtimeOptions": {
//    "tfm": "net7.0",
//    "framework": {
//      "name": "Microsoft.NETCore.App",
//      "version": "7.0.0"
//    },
//    "configProperties": {
//      "System.Reflection.Metadata.MetadataUpdater.IsSupported": false
//    }
//  }
//}"""

let run (ms: MemoryStream, expectedOutput: string) =
    //let tmpFile = Path.GetTempFileName()
    //try File.Delete tmpFile with | _ -> ()
    //let tmpFile = Path.ChangeExtension(tmpFile, ".dll")
    //let tmpJsonConfigFile = Path.ChangeExtension(tmpFile, ".runtimeconfig.json")

    //try
    //    let fs = new FileStream(tmpFile, FileMode.Create)
    //    let bytes = ms.GetBuffer()
    //    fs.Write(bytes, 0, bytes.Length)
    //    fs.Dispose()

    //    File.WriteAllText(tmpJsonConfigFile, runtimeconfigJson)

    //    use dotnet = new Oly.Core.ExternalProcess("dotnet", tmpFile)
    //    let result = dotnet.RunAsync(Threading.CancellationToken.None).Result
    //    if result.Output.EndsWith("\r\n") then             
    //        Assert.Equal(expectedOutput + "\r\n", result.Output)
    //    else
    //        Assert.Equal(expectedOutput, result.Output)
    //finally
    //    try File.Delete tmpFile with | _ -> ()
    //    try File.Delete tmpJsonConfigFile with | _ -> ()

    let context = AssemblyLoadContext(Guid.NewGuid().ToString(), isCollectible=true)
    try
        let rasm = context.LoadFromStream(ms)
        ms.Dispose()
        let mutable actual = null
        lock gate (fun () ->
            let builder = StringBuilder()
            let writer = new StringWriter(builder)
            Console.SetOut(writer)
            if rasm.EntryPoint = null then
                failwith "Entry point not found."
            rasm.EntryPoint.Invoke(null, [||]) |> ignore
            actual <- builder.ToString()
            writer.Dispose()
        )
        Assert.Equal(expectedOutput, actual)
    finally
        context.Unload()