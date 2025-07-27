namespace olylib

open System
open System.IO
open System.Threading
open System.Collections.Generic
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Targets.DotNet
open Oly.Targets.Interpreter
open Oly.Targets.Spirv

module Oly =

    let private createWorkspace(rootPath: OlyPath, rs) =
        let targets = 
            [
                InterpreterTarget() :> OlyBuild
                DotNetTarget()
                SpirvTarget()
            ] |> ImArray.ofSeq
        OlyWorkspace.Create(targets, rootPath, rs)

    let Build (configName: string, projectPath: OlyPath, ct: CancellationToken) =
        let projectPath = OlyPath.Create(System.Environment.CurrentDirectory).Join(projectPath)
        let rootPath = projectPath.GetDirectory()
        let activeConfigPath = rootPath.Join(".oly/workspace/state.json")
        use ms = new MemoryStream(System.Text.Encoding.Default.GetBytes($"""{{ "activeConfiguration": "{configName}" }}"""))
        let rs = OlyWorkspaceResourceSnapshot.Create(activeConfigPath).SetResourceAsCopy(activeConfigPath, ms)
        let workspace = createWorkspace(rootPath, rs)
        workspace.FileChanged(projectPath)
        workspace.BuildProjectAsync(projectPath, ct)

    let rec private CleanProject (set: HashSet<OlyPath>) (projPath: OlyPath) =
        if set.Add(projPath) |> not then ()
        else

        match projPath.TryGetGlob() with
        | Some(dir, ext) when ext.Equals(".olyx", StringComparison.OrdinalIgnoreCase) ->
            Directory.EnumerateFiles(dir.ToString(), "*.olyx", SearchOption.TopDirectoryOnly)
            |> Seq.iter (fun filePath ->
                let filePath = OlyPath.CreateAbsolute(filePath)
                CleanProject set filePath
            )
        | _ ->

        let cacheDirectoryName = ".oly/cache"
        let binDirectoryName = ".oly/bin"

        let syntaxTree = OlySyntaxTree.Parse(projPath, OlySourceText.FromFile(projPath), OlyParsingOptions.Default)
        let config = syntaxTree.GetCompilationUnitConfiguration(CancellationToken.None)

        let projDir = projPath.GetDirectory()
        let projName = projPath.GetFileNameWithoutExtension()

        config.References
        |> ImArray.iter (fun (_, refPath) ->
            if refPath.EndsWith(".olyx") then
                if refPath.IsRooted then
                    CleanProject set refPath
                else
                    CleanProject set (projDir.Join(refPath))
        )

        try Directory.Delete(projDir.Join(cacheDirectoryName).ToString(), true) with | _ -> ()
        try Directory.Delete(projDir.Join(binDirectoryName).ToString(), true) with | _ -> ()

        OlyTrace.Log($"[Compilation] - Cleaned '{projName}'")

    let Clean (dir: string) =
        let projects = OlyWorkspaceListener.GetProjectsFromDirectory(OlyPath.Create(dir))
        let set = HashSet(OlyPathEqualityComparer.Instance)
        projects
        |> ImArray.iter (fun projectPath ->
            CleanProject set projectPath
        )