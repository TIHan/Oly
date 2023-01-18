module TestPlatform

open System
open System.IO
open System.Text
open System.Reflection
open System.Runtime.Loader
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.Clr.Emitter
open Xunit

let createEmitter(asm: OlyILAssembly) =
    OlyRuntimeClrEmitter(asm.Name, true, typeof<obj>.Assembly.GetName(), typeof<System.Console>.Assembly.GetName())

let emitterWrite(emitter: OlyRuntimeClrEmitter) =
    let ms = new MemoryStream()
    use msPdb = new MemoryStream()
    emitter.Write(ms, msPdb)
    ms.Position <- 0L
    ms

let private gate = obj()

let private runtimeconfigJson =
    """{
  "runtimeOptions": {
    "tfm": "net7.0",
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": "7.0.0"
    },
    "configProperties": {
      "System.Reflection.Metadata.MetadataUpdater.IsSupported": false
    }
  }
}"""

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