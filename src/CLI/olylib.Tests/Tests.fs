module olylib.Tests

open System
open System.IO
open System.Threading
open Xunit
open Oly.Core
open olylib

let ExamplesDirectory = "../../../../../examples"

let project relativePath =
    OlyPath.Create(Path.Combine(ExamplesDirectory, relativePath)).ToAbsolute()

let build configName relativePath =
    let projPath = project relativePath
    Oly.Clean(projPath.GetDirectory().ToString())
    Oly.Build(configName, projPath, CancellationToken.None).Result

let run configName relativePath =
    match build configName relativePath with
    | Ok(program) ->
        program.Run([||])
    | Error(diags) ->
        let builder = System.Text.StringBuilder()
        diags
        |> ImArray.iter (fun x ->
            builder.Append(x.ToString()) |> ignore
        )
        failwith (builder.ToString())

[<Fact>]
let ``Debug HelloWorld`` () =
    let output = run "Debug" "HelloWorld/HelloWorld.olyx"
    Assert.Equal("Hello World!" + Environment.NewLine, output)

[<Fact>]
let ``Release HelloWorld`` () =
    let output = run "Release" "HelloWorld/HelloWorld.olyx"
    Assert.Equal("Hello World!" + Environment.NewLine, output)

[<Fact>]
let ``Debug HelloWorld_r2r`` () =
    let output = run "Debug" "HelloWorld/HelloWorld_r2r.olyx"
    Assert.Equal("Hello World!" + Environment.NewLine, output)

[<Fact>]
let ``Release HelloWorld_r2r`` () =
    let output = run "Release" "HelloWorld/HelloWorld_r2r.olyx"
    Assert.Equal("Hello World!" + Environment.NewLine, output)

[<Fact>]
let ``Debug HelloWorld_aot`` () =
    let output = run "Debug" "HelloWorld/HelloWorld_aot.olyx"
    Assert.Equal("Hello World!" + Environment.NewLine, output)

[<Fact>]
let ``Release HelloWorld_aot`` () =
    let output = run "Release" "HelloWorld/HelloWorld_aot.olyx"
    Assert.Equal("Hello World!" + Environment.NewLine, output)
