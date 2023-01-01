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

let run (ms: MemoryStream, expectedOutput: string) =
    //let tmpFile = Path.GetTempFileName()
    //try File.Delete tmpFile with | _ -> ()
    //let tmpFile = Path.ChangeExtension(tmpFile, ".dll")
    //use fs = new FileStream(tmpFile, FileMode.Create)
    //let bytes = ms.GetBuffer()
    //fs.Write(bytes, 0, bytes.Length)
    //use dotnet = new Oly.Core.ExternalProcess("dotnet", tmpFile)
    //let result = dotnet.RunAsync(Threading.CancellationToken.None).Result
    //Assert.Equal(expectedOutput, result.Output)
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