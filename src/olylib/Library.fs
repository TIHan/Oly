namespace olylib

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Runtime.CompilerServices
open System.Text.Json.Serialization
open Oly.Core
open Oly.Compiler.Workspace
open Oly.Runtime.Target.DotNet
open Oly.Runtime.Target.Interpreter

module Oly =

    let Build (configName: string, projectPath: OlyPath, ct: CancellationToken) =
        // TODO: Fix this.
        let rs = OlyWorkspaceResourceSnapshot.Create(OlyPath.Empty)

        let targets = 
            [
                InterpreterTarget() :> OlyBuild
                DotNetTarget()
            ] |> ImArray.ofSeq
        let workspace = OlyWorkspace.Create(targets)
        workspace.UpdateDocument(rs, projectPath, rs.GetSourceText(projectPath), ct)
        workspace.BuildProjectAsync(rs, projectPath, ct)