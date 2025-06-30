module internal Oly.Runtime.Target.DotNet.MSBuild

open System
open System.IO
open System.Collections.Immutable
open System.Threading
open Oly.Core
open Oly.Core.IO

[<NoEquality;NoComparison>]
type ProjectBuildInfo =
    {
        TargetName: string
        ProjectPath: OlyPath
        ConfigurationPath: OlyPath
        ConfigurationTimestamp: DateTime
        OutputPath: string

        References: OlyPath imarray
        ReferenceNames: ImmutableHashSet<string>
        FilesToCopy: OlyPath imarray
    }

[<RequireQualifiedAccess>]
type MSBuildPublishKind =
    | None
    | ReadyToRun
    | NativeAOT

[<NoEquality;NoComparison>]
type MSBuildTargetInfo =
    {
        FullTargetName: string
        TargetName: string
        PublishKind: MSBuildPublishKind
    }

    member this.IsPublish = match this.PublishKind with MSBuildPublishKind.None -> false | _ -> true

    member this.IsNativeAOT = match this.PublishKind with MSBuildPublishKind.NativeAOT -> true | _ -> false

    static member Parse(targetName: string) =
        let parts = targetName.Split("_")
        if parts.Length > 1 then
            let publishKind =
                match parts[1] with
                | "r2r" -> MSBuildPublishKind.ReadyToRun
                | "aot" -> MSBuildPublishKind.NativeAOT
                | _ -> failwith "Invalid target."
            { FullTargetName = targetName; TargetName = parts[0]; PublishKind = publishKind }
        else
            { FullTargetName = targetName; TargetName = parts[0]; PublishKind = MSBuildPublishKind.None }

    static member ParseOnlyTargetName(targetName: string) =
        let info = MSBuildTargetInfo.Parse(targetName)
        { FullTargetName = info.TargetName; TargetName = info.TargetName; PublishKind = MSBuildPublishKind.None }

// TODO: This needs alot more work.
[<Sealed>]
type MSBuild() =
    
    let createProjStub isExe targetName (fileReferences: string seq) (dotnetProjectReferences: string seq) (dotnetPackages: string seq) publishKind =
        let outputType =
            if isExe then
                "<OutputType>Exe</OutputType>"
            else
                ""
        let publishKind =
            match publishKind with
            | MSBuildPublishKind.None -> ""
            | MSBuildPublishKind.ReadyToRun -> "<PublishReadyToRun>true</PublishReadyToRun>"
            | MSBuildPublishKind.NativeAOT -> "<PublishAot>true</PublishAot>"

        let references =
            fileReferences
            |> Seq.map (fun x ->
                let includeName = Path.GetFileNameWithoutExtension(x)
                $"<Reference Include=\"{includeName}\"><HintPath>{x}</HintPath></Reference>"
            )
            |> String.concat Environment.NewLine
        let projReferences =
            dotnetProjectReferences
            |> Seq.map (fun x ->
                $"<ProjectReference Include=\"{x}\" />"
            )
            |> String.concat Environment.NewLine
        let packages =
            dotnetPackages
            |> Seq.map (fun x ->
                let index = x.IndexOf(',')
                if index = -1 || (index + 1 = x.Length) then
                    $"<PackageReference Include=\"{x}\" />"
                else
                    $"<PackageReference Include=\"{x.Substring(0, index)}\" Version=\"{x.Substring(index + 1)}\" />"
            )
            |> String.concat Environment.NewLine
        $"""
<Project Sdk="Microsoft.NET.Sdk">
<PropertyGroup>
    <EnableDefaultItems>false</EnableDefaultItems>
    {outputType}
    {publishKind}
    <TargetFramework>{targetName}</TargetFramework>
</PropertyGroup>
<ItemGroup>
    <Compile Include="Program.cs" />
{references}
{projReferences}
{packages}
</ItemGroup>
<Target Name="WriteFrameworkReferences" AfterTargets="AfterBuild">
    <WriteLinesToFile File="FrameworkReferences.txt" Lines="@(ReferencePath)" Overwrite="true" WriteOnlyWhenDifferent="true" />
</Target>
</Project>
        """

    let getInfoCore programCs (outputPath: OlyPath) (configPath: string) (configName: string) (isExe: bool) (msbuildTargetInfo: MSBuildTargetInfo) fileReferences dotnetProjectReferences dotnetPackages (projectName: string) (ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            try Directory.Delete(outputPath.ToString(), true) with | _ -> ()

            let isPublish = msbuildTargetInfo.IsPublish

            let tmpFile = Path.GetTempFileName()
            try
                let stubDir =
                    let dir = tmpFile + "_stub"
                    try Directory.Delete(dir, true) with | _ -> ()
                    let dir = Directory.CreateDirectory(dir)
                    dir

                let stub = createProjStub isExe msbuildTargetInfo.TargetName fileReferences dotnetProjectReferences dotnetPackages msbuildTargetInfo.PublishKind
                ct.ThrowIfCancellationRequested()

                File.WriteAllText(Path.Combine(stubDir.FullName, "Program.cs"), programCs)
                File.WriteAllText(Path.Combine(stubDir.FullName, $"{projectName}.csproj"), stub)
                ct.ThrowIfCancellationRequested()

                let cleanup() =
                    if not isExe then
                        try File.Delete(Path.Combine(outputPath.ToString(), $"{projectName}.deps.json")) with | _ -> ()
                    try File.Delete(Path.Combine(outputPath.ToString(), $"{projectName}.dll")) with | _ -> ()
                    try File.Delete(Path.Combine(outputPath.ToString(), $"{projectName}.pdb")) with | _ -> ()

                let projectPath = Path.Combine(stubDir.FullName, $"{projectName}.csproj")

                try
                    let msbuildTask = backgroundTask {
                        let build =
                            if isPublish then "publish"
                            else "build"
                        use p = new ExternalProcess("dotnet", $"{build} -c {configName} -o {outputPath.ToString()} {projectName}.csproj", workingDirectory = stubDir.FullName)

                        try
                            let! _result = p.RunAsync(ct)
                            ()
                        with
                        | ex ->
                            OlyTrace.LogError($"[MSBuild] ${ex.ToString()}")
                            raise ex
                        ()
                    }

                    do! msbuildTask

                    let refs =
                        File.ReadAllText(Path.Combine(stubDir.FullName, "FrameworkReferences.txt")).Split("\n")
                        |> ImArray.ofSeq
                        |> ImArray.map (fun x -> OlyPath.Create(x.Replace("\r", "")))
                        |> ImArray.filter (fun x -> String.IsNullOrWhiteSpace(x.ToString()) |> not)
                        

                    let equality = 
                        { new System.Collections.Generic.IEqualityComparer<OlyPath> with
                            member _.GetHashCode o = OlyPath.GetFileName(o).ToString().GetHashCode()
                                
                            member _.Equals(x, y) =
                                x.EndsWith(OlyPath.GetFileName(y).ToString())
                        }
                    let hashRefs = System.Collections.Generic.HashSet<OlyPath>(equality)

                    let refNames =
                        refs
                        |> Seq.map (fun x -> 
                            hashRefs.Add(x) |> ignore
                            OlyPath.GetFileName(x)
                        )
                        |> ImmutableHashSet.CreateRange

                    cleanup()

                    let filesToCopy =
                        OlyIO.GetFilesFromDirectory(outputPath.ToString())
                        |> ImArray.choose (fun x ->
                            let x = OlyPath.Create(x)
                            if hashRefs.Contains(x) then
                                None
                            else
                                Some(x)
                        )

                    return 
                        { 
                            TargetName = msbuildTargetInfo.FullTargetName
                            ProjectPath = OlyPath.Create(projectPath)
                            ConfigurationPath = OlyPath.Create(configPath)
                            ConfigurationTimestamp = try File.GetLastWriteTimeUtc(configPath) with | _ -> DateTime()
                            OutputPath = outputPath.ToString()
                            References = refs
                            ReferenceNames = refNames
                            FilesToCopy = filesToCopy
                        }
                finally
                    try Directory.Delete(stubDir.FullName, true) with | _ -> ()
            finally
                try File.Delete(tmpFile) with | _ -> ()
        }
    
    let getInfo programCs (outputPath: OlyPath) (configPath: string) (configName: string) (isExe: bool) (msbuildTargetInfo: MSBuildTargetInfo) fileReferences dotnetProjectReferences dotnetPackages (projectName: string) (ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            OlyTrace.Log $"[MSBuild] Started resolving DotNet references for project: {projectName}"
            let s = System.Diagnostics.Stopwatch.StartNew()
            use _ = 
                { new IDisposable with 
                    member _.Dispose() = 
                        s.Stop()
                        OlyTrace.Log $"[MSBuild] Finished resolving DotNet references for project: {projectName} - {s.Elapsed.TotalMilliseconds}ms"
                }
            try
                let! result = getInfoCore programCs outputPath configPath configName isExe msbuildTargetInfo fileReferences dotnetProjectReferences dotnetPackages projectName ct
                return result
            with
            | ex ->
                match ex with
                | :? OperationCanceledException -> ()
                | _ -> OlyTrace.LogError $"[MSBuild] Failed resolving DotNet references for project: {projectName} - Exception:\n{ex.ToString()}"
                return raise ex
        }

    member this.CreateAndBuildProjectAsync(programCs, projectName: string, outputPath: OlyPath, configPath: string, configName: string, isExe: bool, targetName, fileReferences, dotnetProjectReferences, dotnetPackages, ct) =
        getInfo programCs outputPath configPath configName isExe targetName fileReferences dotnetProjectReferences dotnetPackages projectName ct

    member this.DeleteProjectObjDirectory(info: ProjectBuildInfo) =
        try Directory.Delete(Path.Combine(info.ProjectPath.ToString(), "obj"), true) with | _ -> ()

    member this.CopyOutput(info: ProjectBuildInfo, dstDir: OlyPath) =
        OlyIO.CopyDirectory(info.OutputPath, (dstDir.ToString()))



