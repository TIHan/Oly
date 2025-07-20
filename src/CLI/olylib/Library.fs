namespace olylib

open System
open System.IO
open System.Threading
open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Compiler.Workspace.Service
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
        let projectPath = OlyPath.Combine(OlyPath.Create(System.Environment.CurrentDirectory), projectPath)
        let rootPath = OlyPath.GetDirectory(projectPath)
        let activeConfigPath = OlyPath.Combine(rootPath, ".olyworkspace/state.json")
        use ms = new MemoryStream(System.Text.Encoding.Default.GetBytes($"""{{ "activeConfiguration": "{configName}" }}"""))
        let rs = OlyWorkspaceResourceSnapshot.Create(activeConfigPath).SetResourceAsCopy(activeConfigPath, ms)
        let workspace = createWorkspace(rootPath, rs)
        workspace.FileChanged(projectPath)
        workspace.BuildProjectAsync(projectPath, ct)

    let rec Clean (dir: string) =

        // TODO: Handle recursive projects if they occur.

        let projects = OlyWorkspaceListener.GetProjectsFromDirectory(OlyPath.Create(dir))
        let cacheDirectoryName = ".olycache"
        let binDirectoryName = "bin"

        projects
        |> ImArray.iter (fun projPath ->
            let syntaxTree = OlySyntaxTree.Parse(projPath, OlySourceText.FromFile(projPath), OlyParsingOptions.Default)
            let config = syntaxTree.GetCompilationUnitConfiguration(CancellationToken.None)

            let projDir = OlyPath.GetDirectory(projPath)
            let projName = OlyPath.GetFileNameWithoutExtension(projPath)

            config.References
            |> ImArray.iter (fun (_, refPath) ->
                if refPath.EndsWith(".olyx") then
                    if refPath.IsRooted then
                        Clean(OlyPath.GetDirectory(refPath).ToString())
                    else
                        Clean(OlyPath.GetDirectory(OlyPath.Combine(projDir, refPath)).ToString())
            )

            try Directory.Delete(OlyPath.Combine(projDir, cacheDirectoryName).ToString(), true) with | _ -> ()
            try Directory.Delete(OlyPath.Combine(projDir, binDirectoryName).ToString(), true) with | _ -> ()

            OlyTrace.Log($"[Compilation] - Cleaned '{projName}'")
        )