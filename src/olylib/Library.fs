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
                    ProjectConfiguration("Release", [||], false)
                    ProjectConfiguration("Debug", [|"DEBUG"|], true)
                |]
            )

    //[<Sealed>]
    //type OlyWorkspaceResourceService(configName: string) =

    //    let rs = OlyDefaultWorkspaceResourceService() :> OlyWorkspaceResourceState

    //    interface OlyWorkspaceResourceState with

    //        member _.LoadSourceText(filePath) = rs.LoadSourceText(filePath)

    //        member _.GetTimeStamp(filePath) = rs.GetTimeStamp(filePath)

    //        member _.FindSubPaths(dirPath) = rs.FindSubPaths(dirPath)

    //        member this.LoadProjectConfigurationAsync(projectConfigPath, ct) : Task<OlyProjectConfiguration> = 
    //            backgroundTask {         
    //                let mutable configs = ProjectConfigurations.Default

    //                try
    //                    let fs = File.OpenText(projectConfigPath.ToString())
    //                    try
    //                        let jsonOptions = System.Text.Json.JsonSerializerOptions()
    //                        jsonOptions.PropertyNameCaseInsensitive <- true
    //                        let! result = System.Text.Json.JsonSerializer.DeserializeAsync<ProjectConfigurations>(fs.BaseStream, jsonOptions, cancellationToken = ct)
    //                        configs <- result
    //                    finally
    //                        fs.Dispose()
    //                with
    //                | :? OperationCanceledException as ex ->
    //                    raise ex
    //                | _ ->
    //                    let fs = File.OpenWrite(projectConfigPath.ToString())
    //                    try
    //                        let jsonOptions = System.Text.Json.JsonSerializerOptions()
    //                        jsonOptions.PropertyNameCaseInsensitive <- true
    //                        jsonOptions.WriteIndented <- true
    //                        do! System.Text.Json.JsonSerializer.SerializeAsync(fs, configs, jsonOptions, cancellationToken = ct)
    //                    finally
    //                        fs.Dispose()

    //                let configOpt =
    //                    configs.Configurations
    //                    |> Array.tryFind (fun x -> (not(String.IsNullOrEmpty(x.Name))) && x.Name.Equals(configName, StringComparison.OrdinalIgnoreCase))

    //                let config =
    //                    match configOpt with
    //                    | None ->
    //                        match configs.Configurations |> Array.tryHead with
    //                        | Some config -> config
    //                        | _ -> ProjectConfigurations.Default.Configurations[0]
    //                    | Some config ->
    //                        config

    //                let name = config.Name
    //                let conditionalDefines = config.Defines |> ImArray.ofSeq
    //                let isDebuggable = config.Debuggable

    //                return OlyProjectConfiguration(name, conditionalDefines, isDebuggable)
    //            }

module Oly =

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private DoNotTrim(value: bool) =
        if value then
            Console.WriteLine(Implementation.ProjectConfiguration("", [||], false))
            Console.WriteLine(Implementation.ProjectConfigurations([||]))
            Console.WriteLine("")

    let Build (configName: string, projectPath: OlyPath, ct: CancellationToken) =
        DoNotTrim(false)

        let rs = OlyWorkspaceResourceState.Create()

        let targets = 
            [
                InterpreterTarget() :> OlyBuild
                DotNetTarget()
            ] |> ImArray.ofSeq
        let workspace = OlyWorkspace.Create(targets)
        workspace.UpdateDocument(rs, projectPath, rs.GetSourceText(projectPath), ct)
        workspace.BuildProjectAsync(rs, projectPath, ct)