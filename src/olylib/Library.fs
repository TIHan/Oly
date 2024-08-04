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

module Implementation =

    [<Sealed>]
    type ProjectConfiguration [<JsonConstructor>] (name: string, defines: string [], debuggable: bool) =
        member _.Name = name
        member _.Defines = defines
        member _.Debuggable = debuggable

    [<Sealed>]
    type ProjectConfigurations [<JsonConstructor>] (configurations: ProjectConfiguration []) =
        member _.Configurations = configurations

        static member Default =
            ProjectConfigurations(
                [|
                    ProjectConfiguration("Debug", [|"DEBUG"|], true)
                    ProjectConfiguration("Release", [||], false)
                |]
            )

    [<Sealed>]
    type WorkspaceState [<JsonConstructor>] (activeConfiguration: string) =
        member _.ActiveConfiguration = activeConfiguration

    [<Literal>]
    let WorkspaceStateDirectory = ".olyworkspace/"

    [<Literal>]
    let WorkspaceStateFileName = "state.json"

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
                        JsonFileStore<WorkspaceState>.GetContents(
                            OlyPath.Combine(projectConfigPath, Path.Combine("../" + Path.Combine(WorkspaceStateDirectory, WorkspaceStateFileName), projectConfigPath.ToString())), 
                            WorkspaceState("Debug"), 
                            ct
                        )
                    let mutable configs = ProjectConfigurations.Default

                    try
                        let fs = File.OpenText(projectConfigPath.ToString())
                        try
                            let jsonOptions = System.Text.Json.JsonSerializerOptions()
                            jsonOptions.PropertyNameCaseInsensitive <- true
                            let! result = System.Text.Json.JsonSerializer.DeserializeAsync<ProjectConfigurations>(fs.BaseStream, jsonOptions, cancellationToken = ct)
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
                        configs.Configurations
                        |> Array.tryFind (fun x -> (not(String.IsNullOrEmpty(x.Name))) && x.Name.Equals(state.ActiveConfiguration, StringComparison.OrdinalIgnoreCase))

                    let config =
                        match configOpt with
                        | None ->
                            match configs.Configurations |> Array.tryHead with
                            | Some config -> config
                            | _ -> ProjectConfigurations.Default.Configurations[0]
                        | Some config ->
                            config

                    let name = config.Name
                    let conditionalDefines = config.Defines |> ImArray.ofSeq
                    let isDebuggable = config.Debuggable

                    return OlyProjectConfiguration(name, conditionalDefines, isDebuggable)
                }

module Oly =

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private DoNotTrim(value: bool) =
        if value then
            Console.WriteLine(Implementation.ProjectConfiguration("", [||], false))
            Console.WriteLine(Implementation.ProjectConfigurations([||]))
            Console.WriteLine(Implementation.WorkspaceState(""))

    let Build (projectPath: OlyPath, ct: CancellationToken) =
        DoNotTrim(false)

        let rs = Implementation.OlyWorkspaceResourceService() :> IOlyWorkspaceResourceService

        let targets = 
            [
                InterpreterTarget() :> OlyBuild
                DotNetTarget()
            ] |> ImArray.ofSeq
        let workspace = OlyWorkspace.Create(targets, rs = rs)
        workspace.UpdateDocument(projectPath, rs.LoadSourceText(projectPath), ct)
        workspace.BuildProjectAsync(projectPath, ct)