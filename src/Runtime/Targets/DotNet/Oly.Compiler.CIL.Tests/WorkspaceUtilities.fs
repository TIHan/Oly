﻿module WorkspaceUtilities

open System
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open TestFramework

let private buildWithAux target isDebug src =
    let preludeDirName = System.IO.Path.GetDirectoryName(typeof<OlyWorkspace>.Assembly.Location)
    let preludeDir = OlyPath.CreateAbsolute(preludeDirName)
    let preludeDir =
        if preludeDir.IsDirectory then
            preludeDir
        else
            OlyPath.Create(preludeDirName + "/")

    let workspace = OlyWorkspace.Create([target])

    let documentPath = OlyPath.Combine(preludeDir, "test.olyx")
    let srcText = OlySourceText.Create(src)

    let projConfigPath = OlyPath.Combine(preludeDir, "test.json")
    use projConfigMs = new IO.MemoryStream(Text.Encoding.Default.GetBytes("""{ "configurations": [{ "name": "Debug", "defines": ["DEBUG"], "debuggable": true }, { "name": "Release", "defines": ["RELEASE"], "debuggable": false }] }"""))

    let configPath = OlyPath.Create("state.json")
    use configMs = 
        if isDebug then
            new IO.MemoryStream(Text.Encoding.Default.GetBytes("""{ "activeConfiguration": "Debug" }"""))
        else
            new IO.MemoryStream(Text.Encoding.Default.GetBytes("""{ "activeConfiguration": "Release" }"""))

    let textManager = OlySourceTextManager.Empty.Set(documentPath, srcText, 1)
    let resourceSnapshot = OlyWorkspaceResourceSnapshot.Create(configPath).WithTextEditors(textManager)

    let resourceSnapshot = resourceSnapshot.SetResourceAsCopy(projConfigPath, projConfigMs)
    let resourceSnapshot = resourceSnapshot.SetResourceAsCopy(configPath, configMs)

    // Set up prelude
    let resourceSnapshot = resourceSnapshot.SetResourceAsCopy(OlyPath.Combine(preludeDir, $"{target.PlatformName}_prelude.olyx"))
    let resourceSnapshot = resourceSnapshot.SetResourceAsCopy(OlyPath.Combine(preludeDir, "prelude.oly"))

    let _doc = workspace.UpdateDocumentAsync(resourceSnapshot, documentPath, srcText, Threading.CancellationToken.None).Result[0]

    workspace.BuildProjectAsync(resourceSnapshot, documentPath, Threading.CancellationToken.None).Result

let buildWith target isDebug src =
    match buildWithAux target isDebug src with
    | Error(diags) ->
        raise(Exception(OlyDiagnostic.PrepareForOutput(diags, Threading.CancellationToken.None)))
    | Ok(program) ->
        program

let buildHasErrorsWith target (expected: (string * string) list) src =
    match buildWithAux target false src with
    | Error(diags) when diags |> ImArray.exists (fun x -> x.IsError) ->
        let errorMsgs = diags |> ImArray.filter (fun x -> x.IsError) |> Seq.map (fun x -> (x.Message, "\r\n" + x.GetHelperText() + "\r\n")) |> Array.ofSeq
        Assert.Equal(expected, errorMsgs)
    | _ ->
        failwith "Expected errors."