module WorkspaceUtilities

open System
open System.Numerics
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open TestFramework

let private createWorkspace (target: OlyBuild) isDebug =
    let preludeDirName = System.IO.Path.GetDirectoryName(typeof<OlyWorkspace>.Assembly.Location)
    let preludeDir = OlyPath.CreateAbsolute(preludeDirName)
    let preludeDir =
        if preludeDir.IsDirectory then
            preludeDir
        else
            OlyPath.Create(preludeDirName + "/")

    let configPath = OlyPath.Create("state.json")
    let rs = OlyWorkspaceResourceSnapshot.Create(configPath)
    use configMs = 
        if isDebug then
            new IO.MemoryStream(Text.Encoding.Default.GetBytes("""{ "activeConfiguration": "Debug" }"""))
        else
            new IO.MemoryStream(Text.Encoding.Default.GetBytes("""{ "activeConfiguration": "Release" }"""))
    let rs = rs.SetResourceAsCopy(configPath, configMs)

    // Set up prelude
    let rs = rs.SetResourceAsCopy(preludeDir.Join("prelude.oly"))
    let rs = rs.SetResourceAsCopy(preludeDir.Join($"numerics_{target.PlatformName}.oly"))
    let rs = rs.SetResourceAsCopy(preludeDir.Join($"prelude_{target.PlatformName}.olyx"))

    let workspace = OlyWorkspace.Create(([target]: OlyBuild seq), OlyPath.Empty, rs)
    let projectName = Guid.NewGuid().ToString()
    let documentPath = preludeDir.Join($"{projectName}.olyx")

    (workspace, documentPath)

let lockObj = obj()
let buildLookup = System.Collections.Concurrent.ConcurrentDictionary()
let cachedBuildWith (target: OlyBuild) (isDebug: bool) src =

    let (workspace, documentPath) =
        let key = (target.PlatformName, isDebug)
        match buildLookup.TryGetValue(key) with
        | true, result -> result
        | _ ->
            lock lockObj (fun () ->
                match buildLookup.TryGetValue(key) with
                | true, result -> result
                | _ ->
                    let result = createWorkspace target isDebug
                    buildLookup[key] <- result
                    result
            )
        
    let srcText = OlySourceText.Create(src)
    workspace.UpdateDocument(documentPath, srcText, Threading.CancellationToken.None)
    let doc = workspace.GetDocumentsAsync(documentPath, Threading.CancellationToken.None).Result[0]

    match target.BuildProjectAsync(doc.Project, Threading.CancellationToken.None).Result with
    | Error(diags) ->
        raise(Exception(OlyDiagnostic.PrepareForOutput(diags, Threading.CancellationToken.None)))
    | Ok(program) ->
        program

let private buildWithAux target isDebug src =
    let (workspace, documentPath) = createWorkspace target isDebug
    workspace.UpdateDocument(documentPath, OlySourceText.Create(src), Threading.CancellationToken.None)
    workspace.BuildProjectAsync(documentPath, Threading.CancellationToken.None).Result

let buildWith target isDebug src =
    match buildWithAux target isDebug src with
    | Error(diags) ->
        raise(Exception(OlyDiagnostic.PrepareForOutput(diags, Threading.CancellationToken.None)))
    | Ok(program) ->
        program

let buildHasErrorsWithPrefix target (expected: (string * string) list) src =
    match buildWithAux target false src with
    | Error(diags) when diags |> ImArray.exists (fun x -> x.IsError) ->
        let errorMsgs = 
            diags 
            |> ImArray.filter (fun x -> x.IsError) 
            |> Seq.map (fun x -> ($"{x.CodePrefix}: {x.Message}", "\n" + x.GetHelperText() + "\n"))

        let expected =
            expected
            |> Seq.map (fun (x, y) -> (x.ReplaceLineEndings("\n"), y.ReplaceLineEndings("\n")))
            |> ImArray.ofSeq

        let errorMsgs =
            errorMsgs
            |> Seq.map (fun (x, y) -> (x.ReplaceLineEndings("\n"), y.ReplaceLineEndings("\n")))
            |> ImArray.ofSeq

        Assert.Equal(expected, errorMsgs)
    | _ ->
        failwith "Expected errors."

let getNumericsDataKind<'T> =
    match typeof<'T> with
    | x when x = typeof<float32> -> "float"
    | x when x = typeof<Vector2> -> "vec2"
    | x when x = typeof<Vector3> -> "vec3"
    | x when x = typeof<Vector4> -> "vec4"
    | x when x = typeof<int32> -> "int"
    | x ->
        invalidOp $"Type '{x.FullName}' not supported or implemented."
