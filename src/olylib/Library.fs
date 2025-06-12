namespace olylib

open System.IO
open System.Threading
open Oly.Core
open Oly.Compiler.Workspace
open Oly.Runtime.Target.DotNet
open Oly.Runtime.Target.Interpreter
open Oly.Runtime.Target.Spirv
open Oly.Compiler.Workspace.Service

module Oly =

    let Build (configName: string, projectPath: OlyPath, ct: CancellationToken) =
        let projectPath = OlyPath.Combine(OlyPath.Create(System.Environment.CurrentDirectory), projectPath)
        let rootPath = OlyPath.GetDirectory(projectPath)
        let activeConfigPath = OlyPath.Combine(rootPath, ".olyworkspace/state.json")

        let targets = 
            [
                InterpreterTarget() :> OlyBuild
                DotNetTarget()
                SpirvTarget()
            ] |> ImArray.ofSeq
        let workspace = OlyWorkspace.Create(targets)
        use ms = new MemoryStream(System.Text.Encoding.Default.GetBytes($"""{{ "activeConfiguration": "{configName}" }}"""))
        let rs = OlyWorkspaceResourceSnapshot.CreateForced(activeConfigPath, ms)
        workspace.BuildProjectAsync(rs, projectPath, ct)