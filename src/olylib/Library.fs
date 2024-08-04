namespace olylib

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Oly.Core
open Oly.Compiler.Workspace
open Oly.Runtime.Target.DotNet
open Oly.Runtime.Target.Interpreter

module Implementation =

    type LspProjectConfiguration =
        {
            mutable name: string
            mutable defines: string []
            mutable debuggable: bool
        }

    type LspProjectConfigurations =
        {
            mutable configurations: LspProjectConfiguration []
        }

        static member Default =
            {
                configurations =
                    [|
                        {
                            name = "Debug"
                            defines = [|"DEBUG"|]
                            debuggable = true
                        }
                        {
                            name = "Release"
                            defines = [||]
                            debuggable = false
                        }
                    |]
            }

    type LspWorkspaceState =
        {
            mutable activeConfiguration: string
        }

    [<Literal>]
    let LspWorkspaceStateDirectory = ".olyworkspace/"

    [<Literal>]
    let LspWorkspaceStateFileName = "state.json"

    [<Sealed>]
    type OlyWorkspaceResourceService() =

        let rs = OlyDefaultWorkspaceResourceService() :> IOlyWorkspaceResourceService

        interface IOlyWorkspaceResourceService with

            member _.LoadSourceText(filePath) = rs.LoadSourceText(filePath)

            member _.GetTimeStamp(filePath) = rs.GetTimeStamp(filePath)

            member _.FindSubPaths(dirPath) = rs.FindSubPaths(dirPath)

            member this.LoadProjectConfigurationAsync(projectConfigPath, ct) : Task<OlyProjectConfiguration> = 
                backgroundTask {         
                    let! state = 
                        JsonFileStore<LspWorkspaceState>.GetContents(
                            OlyPath.Combine(projectConfigPath, Path.Combine("../" + Path.Combine(LspWorkspaceStateDirectory, LspWorkspaceStateFileName), projectConfigPath.ToString())), 
                            { activeConfiguration = "Debug" }, 
                            ct
                        )
                    let mutable configs = LspProjectConfigurations.Default

                    try
                        let fs = File.OpenText(projectConfigPath.ToString())
                        try
                            let jsonOptions = System.Text.Json.JsonSerializerOptions()
                            jsonOptions.PropertyNameCaseInsensitive <- true
                            let! result = System.Text.Json.JsonSerializer.DeserializeAsync<LspProjectConfigurations>(fs.BaseStream, jsonOptions, cancellationToken = ct)
                            configs <- result
                        finally
                            fs.Dispose()
                    with
                    | :? OperationCanceledException as ex ->
                        raise ex
                    | _ ->
                        let fs = File.OpenWrite(projectConfigPath.ToString())
                        try
                            let jsonOptions = System.Text.Json.JsonSerializerOptions()
                            jsonOptions.PropertyNameCaseInsensitive <- true
                            jsonOptions.WriteIndented <- true
                            do! System.Text.Json.JsonSerializer.SerializeAsync(fs, configs, jsonOptions, cancellationToken = ct)
                        finally
                            fs.Dispose()

                    let configOpt =
                        configs.configurations
                        |> Array.tryFind (fun x -> (not(String.IsNullOrEmpty(x.name))) && x.name.Equals(state.activeConfiguration, StringComparison.OrdinalIgnoreCase))

                    let config =
                        match configOpt with
                        | None ->
                            match configs.configurations |> Array.tryHead with
                            | Some config -> config
                            | _ -> LspProjectConfigurations.Default.configurations[0]
                        | Some config ->
                            config

                    let name = config.name
                    let conditionalDefines = config.defines |> ImArray.ofSeq
                    let isDebuggable = config.debuggable

                    return OlyProjectConfiguration(name, conditionalDefines, isDebuggable)
                }

module Oly =

    let Build (projectPath: OlyPath, ct: CancellationToken) =
        let rs = Implementation.OlyWorkspaceResourceService() :> IOlyWorkspaceResourceService

        let targets = 
            [
                InterpreterTarget() :> OlyBuild
                DotNetTarget()
            ] |> ImArray.ofSeq
        let workspace = OlyWorkspace.Create(targets, rs = rs)
        workspace.UpdateDocument(projectPath, rs.LoadSourceText(projectPath), ct)
        workspace.BuildProjectAsync(projectPath, ct)