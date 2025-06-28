module internal Oly.Runtime.Target.DotNet.MSBuild

open System
open System.IO
open System.Collections.Immutable
open System.Threading
open Oly.Core

[<AutoOpen>]
module private Helpers2 =

    let rec copyDir srcDir dstDir =
        let dir = DirectoryInfo(srcDir)
    
        // Cache directories before we start copying
        let dirs = dir.GetDirectories()

        // Create the destination directory
        Directory.CreateDirectory(dstDir) |> ignore

        // Get the files in the source directory and copy to the destination directory
        for file in dir.GetFiles() do
            let targetFilePath = Path.Combine(dstDir, file.Name)
            let targetFile = FileInfo(targetFilePath)
            if targetFile.Exists then
                if file.LastWriteTimeUtc > targetFile.LastWriteTimeUtc then                  
                    file.CopyTo(targetFilePath, true) |> ignore
            else
                file.CopyTo(targetFilePath) |> ignore

        for subDir in dirs do
            let newDestinationDir = Path.Combine(dstDir, subDir.Name)
            copyDir subDir.FullName newDestinationDir

    let getFiles dir =
        let files = ImArray.builder()
        let dir = DirectoryInfo(dir)

        // Get the files in the source directory and copy to the destination directory
        for file in dir.GetFiles() do
            files.Add(file.FullName)

        files.ToImmutable()

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

[<NoEquality;NoComparison>]
type MSBuildTargetInfo =
    {
        FullTargetName: string
        TargetName: string
        IsReadyToRun: bool
    }

    member this.IsPublish = this.IsReadyToRun

    static member Parse(targetName: string) =
        let parts = targetName.Split("_")
        if parts.Length > 1 then
            { FullTargetName = targetName; TargetName = parts[0]; IsReadyToRun = parts[1] = "r2r" }
        else
            { FullTargetName = targetName; TargetName = parts[0]; IsReadyToRun = false }

    static member ParseOnlyTargetName(targetName: string) =
        let info = MSBuildTargetInfo.Parse(targetName)
        { FullTargetName = info.TargetName; TargetName = info.TargetName; IsReadyToRun = false }

// TODO: This needs alot more work.
[<Sealed>]
type MSBuild() =

    let programCs = """static class Program
{
    static void Main()
    {
    }
}"""
    
    let createProjStub isExe targetName (referenceInfos: string seq) (projReferenceInfos: string seq) (packageInfos: string seq) isReadyToRun =
        let outputType =
            if isExe then
                "<OutputType>Exe</OutputType>"
            else
                ""
        let readyToRun =
            if isReadyToRun then
                "<PublishReadyToRun>true</PublishReadyToRun>"
            else
                ""
        let references =
            referenceInfos
            |> Seq.map (fun x ->
                let includeName = Path.GetFileNameWithoutExtension(x)
                $"<Reference Include=\"{includeName}\"><HintPath>{x}</HintPath></Reference>"
            )
            |> String.concat Environment.NewLine
        let projReferences =
            projReferenceInfos
            |> Seq.map (fun x ->
                $"<ProjectReference Include=\"{x}\" />"
            )
            |> String.concat Environment.NewLine
        let packages =
            packageInfos
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
    {readyToRun}
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

    let getInfoCore (outputPath: OlyPath) (configPath: string) (configName: string) (isExe: bool) (msbuildTargetInfo: MSBuildTargetInfo) referenceInfos projReferenceInfos packageInfos (projectName: string) (ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            try Directory.Delete(outputPath.ToString(), true) with | _ -> ()

            let isPublish = msbuildTargetInfo.IsReadyToRun

            let tmpFile = Path.GetTempFileName()
            try
                let stubDir =
                    let dir = tmpFile + "_stub"
                    try Directory.Delete(dir, true) with | _ -> ()
                    let dir = Directory.CreateDirectory(dir)
                    dir

                let stub = createProjStub isExe msbuildTargetInfo.TargetName referenceInfos projReferenceInfos packageInfos msbuildTargetInfo.IsReadyToRun
                ct.ThrowIfCancellationRequested()

                File.WriteAllText(Path.Combine(stubDir.FullName, "Program.cs"), programCs)
                File.WriteAllText(Path.Combine(stubDir.FullName, $"{projectName}.csproj"), stub)
                ct.ThrowIfCancellationRequested()

                let stubOutputDir = 
                    if isPublish then
                        Path.Combine(Path.Combine(Path.Combine(Path.Combine(stubDir.FullName, "bin"), configName), msbuildTargetInfo.TargetName), "publish")
                    else
                        Path.Combine(Path.Combine(Path.Combine(stubDir.FullName, "bin"), configName), msbuildTargetInfo.TargetName)

                let cleanup() =
                    try File.Delete(Path.Combine(stubOutputDir, $"{projectName}")) with | _ -> ()
                    try File.Delete(Path.Combine(stubOutputDir, $"{projectName}.dll")) with | _ -> ()
                    try File.Delete(Path.Combine(stubOutputDir, $"{projectName}.exe")) with | _ -> ()
                    try File.Delete(Path.Combine(stubOutputDir, $"{projectName}.pdb")) with | _ -> ()
                    try File.Delete(Path.Combine(stubDir.FullName, "FrameworkReferences.txt")) with | _ -> ()
                    try File.Delete(Path.Combine(stubDir.FullName, $"{projectName}.csproj")) with | _ -> ()
                    try File.Delete(Path.Combine(stubDir.FullName, "Program.cs")) with | _ -> ()
                    try Directory.Delete(Path.Combine(stubDir.FullName, "obj"), true) with | _ -> ()

                let projectPath = Path.Combine(stubDir.FullName, $"{projectName}.csproj")

                try
                    try Directory.Delete(stubOutputDir, true) with | _ -> ()

                    let msbuildTask = backgroundTask {
                        let build =
                            if isPublish then "publish"
                            else "build"
                        use p = new ExternalProcess("dotnet", $"{build} -c {configName} {projectName}.csproj", workingDirectory = stubDir.FullName)

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

                    let hashRefs = System.Collections.Generic.HashSet<string>()

                    let refNames =
                        refs
                        |> Seq.map (fun x -> 
                            hashRefs.Add(x.ToString()) |> ignore
                            OlyPath.GetFileName(x)
                        )
                        |> ImmutableHashSet.CreateRange

                    cleanup()
                    copyDir stubOutputDir (outputPath.ToString())

                    let filesToCopy =
                        getFiles (outputPath.ToString())
                        |> ImArray.choose (fun x ->
                            if hashRefs.Contains(x) then
                                None
                            else
                                Some(OlyPath.Create(x))
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
    
    let getInfo (outputPath: OlyPath) (configPath: string) (configName: string) (isExe: bool) (msbuildTargetInfo: MSBuildTargetInfo) referenceInfos projReferenceInfos packageInfos (projectName: string) (ct: CancellationToken) =
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
                let! result = getInfoCore outputPath configPath configName isExe msbuildTargetInfo referenceInfos projReferenceInfos packageInfos projectName ct
                return result
            with
            | ex ->
                match ex with
                | :? OperationCanceledException -> ()
                | _ -> OlyTrace.LogError $"[MSBuild] Failed resolving DotNet references for project: {projectName} - Exception:\n{ex.ToString()}"
                return raise ex
        }

    member this.CreateAndBuildProjectAsync(projectName: string, outputPath: OlyPath, configPath: string, configName: string, isExe: bool, targetName, references, projectReferences, packages, ct) =
        getInfo outputPath configPath configName isExe targetName references projectReferences packages projectName ct

    member this.DeleteProjectObjDirectory(info: ProjectBuildInfo) =
        try Directory.Delete(Path.Combine(info.ProjectPath.ToString(), "obj"), true) with | _ -> ()

    member this.CopyOutput(info: ProjectBuildInfo, dstDir: OlyPath) =
        copyDir info.OutputPath (dstDir.ToString())



