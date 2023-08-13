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

type ProjectBuildInfo =
    {
        ProjectPath: OlyPath
        OutputPath: string

        References: OlyPath imarray
        ReferenceNames: ImmutableHashSet<string>
        DepsJson: string
        RuntimeconfigJson: string option
    }

// TODO: This needs alot more work.
[<Sealed>]
type MSBuild() =

    let programCs = """static class Program
{
    static void Main()
    {
    }
}"""
    
    let createProjStub isExe targetName (referenceInfos: string seq) (projReferenceInfos: string seq) (packageInfos: string seq) =
        let outputType =
            if isExe then
                "<OutputType>Exe</OutputType>"
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
    {outputType}
    <TargetFramework>{targetName}</TargetFramework>
</PropertyGroup>
<ItemGroup>
{references}
{projReferences}
{packages}
</ItemGroup>
<Target Name="WriteFrameworkReferences" AfterTargets="AfterBuild">
    <WriteLinesToFile File="FrameworkReferences.txt" Lines="@(ReferencePath)" Overwrite="true" WriteOnlyWhenDifferent="true" />
</Target>
</Project>
        """
    
    let getInfo (outputPath: OlyPath) (isExe: bool) (targetName: string) referenceInfos projReferenceInfos packageInfos (projectName: string) (ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            let stub = createProjStub isExe targetName referenceInfos projReferenceInfos packageInfos
            try Directory.Delete(outputPath.ToString(), true) with | _ -> ()
            let dir = Directory.CreateDirectory(outputPath.ToString())
            ct.ThrowIfCancellationRequested()

            try
                File.WriteAllText(Path.Combine(dir.FullName, "Program.cs"), programCs)
                File.WriteAllText(Path.Combine(dir.FullName, $"{projectName}.csproj"), stub)
                ct.ThrowIfCancellationRequested()

                let publishDir = 
                    Path.Combine(Path.Combine(Path.Combine(dir.FullName, "bin"), "Release"), targetName)
                    //Path.Combine(Path.Combine(Path.Combine(Path.Combine(dir.FullName, "bin"), "Release"), targetName), "publish")

                let cleanup() =
                    try File.Delete(Path.Combine(publishDir, $"{projectName}.deps.json")) with | _ -> ()
                    try File.Delete(Path.Combine(publishDir, $"{projectName}.runtimeconfig.json")) with | _ -> ()
                    try File.Delete(Path.Combine(publishDir, $"{projectName}.dll")) with | _ -> ()
                    try File.Delete(Path.Combine(publishDir, $"{projectName}.exe")) with | _ -> ()
                    try File.Delete(Path.Combine(publishDir, $"{projectName}.pdb")) with | _ -> ()
                    try File.Delete(Path.Combine(dir.FullName, "FrameworkReferences.txt")) with | _ -> ()
                    try File.Delete(Path.Combine(dir.FullName, $"{projectName}.csproj")) with | _ -> ()
                    try File.Delete(Path.Combine(dir.FullName, "Program.cs")) with | _ -> ()
                    try Directory.Delete(Path.Combine(dir.FullName, "obj"), true) with | _ -> ()

                let projectPath = Path.Combine(dir.FullName, $"{projectName}.csproj")

                try
                    try Directory.Delete(publishDir) with | _ -> ()

                    use p = new ExternalProcess("dotnet", $"build -c Release {projectName}.csproj", workingDirectory = dir.FullName)
                    //use p = new ExternalProcess("dotnet", "publish -c Release __oly_placeholder.csproj", workingDirectory = dir.FullName)
                    let! _result = p.RunAsync(ct)
                    let refs =
                        File.ReadAllText(Path.Combine(dir.FullName, "FrameworkReferences.txt")).Split("\n")
                        |> ImArray.ofSeq
                        |> ImArray.map (fun x -> OlyPath.Create(x.Replace("\r", "")))
                        |> ImArray.filter (fun x -> String.IsNullOrWhiteSpace(x.ToString()) |> not)

                    let depsJson = 
                        try
                            File.ReadAllText(Path.Combine(publishDir, $"{projectName}.deps.json"))
                        with
                        | _ -> ""
                    let runtimeconfigJson = 
                        if isExe then
                            try
                                File.ReadAllText(Path.Combine(publishDir, $"{projectName}.runtimeconfig.json"))
                                |> Some
                            with
                            | _ -> None
                        else
                            None

                    let refNames =
                        refs
                        |> Seq.map (fun x -> OlyPath.GetFileName(x))
                        |> ImmutableHashSet.CreateRange

                    return { ProjectPath = OlyPath.Create(projectPath); OutputPath = publishDir; References = refs; ReferenceNames = refNames; DepsJson = depsJson; RuntimeconfigJson = runtimeconfigJson }
                finally
                    cleanup()
            finally
                ()
                //try Directory.Delete(dir.FullName, true) with | _ -> ()
        }

    member this.CreateAndBuildProjectAsync(projectName: string, outputPath: OlyPath, isExe: bool, targetName, references, projectReferences, packages, ct) =
        getInfo outputPath isExe targetName references projectReferences packages projectName ct

    member this.DeleteProjectObjDirectory(info: ProjectBuildInfo) =
        try Directory.Delete(Path.Combine(info.ProjectPath.ToString(), "obj"), true) with | _ -> ()

    member this.CopyOutput(info: ProjectBuildInfo, dstDir: OlyPath) =
        copyDir info.OutputPath (dstDir.ToString())



