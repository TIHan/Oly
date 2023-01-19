namespace Oly.Runtime.Target.DotNet

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Threading
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable

open Oly.Core
open Oly.Metadata
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Runtime.Clr.Emitter

open Microsoft.Build.Framework
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.CSharp

type private DotNetInfo =
    {
        StubPath: string
        References: OlyPath imarray
        ReferenceNames: ImmutableHashSet<string>
        NonFrameworkReferenceNames: ImmutableHashSet<string>
        DepsJson: string
        RuntimeconfigJson: string option
    }

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

module private DotNetReferences =

    let private programCs = """
static class Program
{
	static void Main()
	{
	}
}
"""

    let private createProjStub isExe targetName (referenceInfos: string seq) (projReferenceInfos: string seq) (packageInfos: string seq) =
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

    let getDotNetInfo (cacheDir: OlyPath) (isExe: bool) (targetName: string) referenceInfos projReferenceInfos packageInfos (ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            let stub = createProjStub isExe targetName referenceInfos projReferenceInfos packageInfos
            let dir = Directory.CreateDirectory(cacheDir.ToString())
            ct.ThrowIfCancellationRequested()

            try
                File.WriteAllText(Path.Combine(dir.FullName, "Program.cs"), programCs)
                File.WriteAllText(Path.Combine(dir.FullName, "__oly_placeholder.csproj"), stub)
                ct.ThrowIfCancellationRequested()
                use p = new ExternalProcess("dotnet", "build -c Release __oly_placeholder.csproj", workingDirectory = dir.FullName)
                //use p = new ExternalProcess("dotnet", "publish -c Release __oly_placeholder.csproj", workingDirectory = dir.FullName)
                let! _result = p.RunAsync(ct)
                let refs =
                    File.ReadAllText(Path.Combine(dir.FullName, "FrameworkReferences.txt")).Split("\n")
                    |> ImArray.ofSeq
                    |> ImArray.map (fun x -> OlyPath.Create(x.Replace("\r", "")))
                    |> ImArray.filter (fun x -> String.IsNullOrWhiteSpace(x.ToString()) |> not)

                let publishDir = 
                    Path.Combine(Path.Combine(Path.Combine(dir.FullName, "bin"), "Release"), targetName)
                    //Path.Combine(Path.Combine(Path.Combine(Path.Combine(dir.FullName, "bin"), "Release"), targetName), "publish")

                let depsJson = 
                    try
                        File.ReadAllText(Path.Combine(publishDir, "__oly_placeholder.deps.json"))
                    with
                    | _ -> ""
                let runtimeconfigJson = 
                    if isExe then
                        try
                            File.ReadAllText(Path.Combine(publishDir, "__oly_placeholder.runtimeconfig.json"))
                            |> Some
                        with
                        | _ -> None
                    else
                        None

                let minimalRefs =
                    Directory.EnumerateFiles(publishDir, "*.dll")
                    |> Seq.map (fun x -> Path.GetFileName(x))
                    |> ImmutableHashSet.CreateRange

                let refNames =
                    refs
                    |> Seq.map (fun x -> OlyPath.GetFileName(x))
                    |> ImmutableHashSet.CreateRange

                try File.Delete(Path.Combine(publishDir, "__oly_placeholder.deps.json")) with | _ -> ()
                try File.Delete(Path.Combine(publishDir, "__oly_placeholder.runtimeconfig.json")) with | _ -> ()
                try File.Delete(Path.Combine(publishDir, "__oly_placeholder.dll")) with | _ -> ()
                try File.Delete(Path.Combine(publishDir, "__oly_placeholder.exe")) with | _ -> ()
                try File.Delete(Path.Combine(publishDir, "__oly_placeholder.pdb")) with | _ -> ()
                try File.Delete(Path.Combine(dir.FullName, "FrameworkReferences.txt")) with | _ -> ()
                try File.Delete(Path.Combine(dir.FullName, "__oly_placeholder.csproj")) with | _ -> ()
                try File.Delete(Path.Combine(dir.FullName, "Program.cs")) with | _ -> ()
                try Directory.Delete(Path.Combine(dir.FullName, "obj"), true) with | _ -> ()

                return { StubPath = publishDir; References = refs; ReferenceNames = refNames; NonFrameworkReferenceNames = minimalRefs; DepsJson = depsJson; RuntimeconfigJson = runtimeconfigJson }
            finally
                ()
                //try Directory.Delete(dir.FullName, true) with | _ -> ()
        }

type DotNetTarget internal (platformName: string, copyReferences: bool, emitPdb: bool) =
    inherit OlyBuild(platformName)

    let gate = obj ()

    let allPossibleFixupsGate = obj ()
    let allPossibleAttrFixups = ResizeArray<OlyILAssembly * ResizeArray<OlyILEntityDefinitionHandle>>()
    let addPossibleFixups possibleFixups =
        lock allPossibleFixupsGate <| fun _ ->
            allPossibleAttrFixups.Add(possibleFixups)

    let allUnresolvedPossibleAttrFixups = ResizeArray<OlyILAssembly * ResizeArray<OlyILEntityDefinitionHandle>>()

    let assemblyVersions = ConcurrentDictionary<OlyPath, uint64>()
    let assemblyCache = ConcurrentDictionary<OlyPath, OlyPath * uint64 * OlyILAssembly * DateTime>()
    let olyAssemblys = ConcurrentDictionary<OlyILAssemblyIdentity, OlyILAssembly>()

    let primaryAssemblies = ConcurrentDictionary<OlyPath, AssemblyName>()
    let consoleAssemblies = ConcurrentDictionary<OlyPath, AssemblyName>()

    let addAssemblyReference path (ilAsm: OlyILAssembly) =
        lock gate (fun () ->
            match assemblyCache.TryGetValue(path) with
            | true, (path, version, ilAsm, _) -> 
                OlyCompilationReference.Create(path, version, ilAsm)
            | _ ->

            let version =
                match assemblyVersions.TryGetValue path with
                | true, version -> version + 1UL
                | _ -> 1UL
            assemblyVersions.[path] <- version
            let compRef = OlyCompilationReference.Create(path, version, ilAsm)
            assemblyCache.[path] <- (path, version, ilAsm, File.GetLastWriteTimeUtc(path.ToString()))
            olyAssemblys.[ilAsm.Identity] <- ilAsm
            compRef
        )

    let referenceChanged = Event<OlyPath>()

    let dirGate = obj()
    let directoryWatchers = ConcurrentDictionary<OlyPath * string, FileSystemWatcher>()
    let addDirectoryWatcher (dir: OlyPath) (filter: string) =
        // fast route
        match directoryWatchers.TryGetValue((dir, filter)) with
        | true, _ -> ()
        | _ ->
            lock dirGate (fun () ->
                match directoryWatchers.TryGetValue((dir, filter)) with
                | true, _ -> ()
                | _  ->
                    let watcher = new FileSystemWatcher(dir.ToString(), filter)
                    watcher.Changed.Add(fun args -> 
                        lock gate (fun () ->
                            let path = OlyPath.Create(args.FullPath)
                            try
                                let lastWriteTime = File.GetLastWriteTimeUtc(args.FullPath)
                                match assemblyCache.Remove(path) with
                                | true, (path, version, ilAsm, lastWriteTime2) ->
                                    if lastWriteTime = lastWriteTime2 then
                                        assemblyCache.[path] <- (path, version, ilAsm, lastWriteTime2)
                                    else
                                        referenceChanged.Trigger(path)
                                | _ ->
                                    referenceChanged.Trigger(path)
                            with
                            | _ ->
                                ()
                        )
                    )
                    directoryWatchers.[(dir, filter)] <- watcher
                    watcher.EnableRaisingEvents <- true
            )

    let csOutputsGate = obj()
    let csOutputs = ConcurrentDictionary<OlyPath, MemoryStream>()

    let netInfos = ConcurrentDictionary<OlyPath, DotNetInfo>()

    member _.GetCSharpOutput(path) =
        match csOutputs.TryGetValue path with
        | true, ms -> ms
        | _ -> failwithf "CSharp output does not exist: %A." path

    abstract GetReferenceAssemblyName : OlyPath -> string
    default _.GetReferenceAssemblyName(path) =
        let pathStr = path.ToString()
        Path.GetFileNameWithoutExtension(pathStr)   

    override this.OnBeforeReferencesImported() =
        ()

    override this.OnAfterReferencesImported() =
        // TODO: We need to add a test scenario for this.
        // Sometimes we do not know if an imported class is really an attribute class or not.
        // We cannot determine this when the reference is imported as importing references
        // are independent of each other.
        // Therefore, we check to see if to we need to fixup a class to become an attribute once all references
        // are imported.
        let handle (allPossibleAttrFixups: ResizeArray<OlyILAssembly * ResizeArray<OlyILEntityDefinitionHandle>>) =
            let mutable passed = true
            for i = 0 to allPossibleAttrFixups.Count - 1 do
                let olyAsm, possibleAttrFixups = allPossibleAttrFixups[i]
                for j = 0 to possibleAttrFixups.Count - 1 do
                    let olyEntDefHandle = possibleAttrFixups[j]

                    let rec fixup (olyAsm: OlyILAssembly) olyEntDefHandle : OlyILEntityDefinition =
                        let olyEntDef = olyAsm.GetEntityDefinition(olyEntDefHandle)
                        if olyEntDef.Kind = OlyILEntityKind.Class then
                            match olyEntDef.Extends |> Seq.tryExactlyOne with
                            | Some(OlyILTypeEntity(OlyILEntityInstance(olyInheritsEntDefOrRefHandle, _))) ->
                                let handleInherits (olyInheritsEntDef: OlyILEntityDefinition) =
                                    if olyInheritsEntDef.Kind = OlyILEntityKind.Attribute then
                                        let olyEntDefFixedUp = olyEntDef.UpdateKind(OlyILEntityKind.Attribute)
                                        olyAsm.SetEntityDefinition(olyEntDefHandle, olyEntDefFixedUp)
                                        olyEntDefFixedUp
                                    else
                                        olyEntDef

                                if olyInheritsEntDefOrRefHandle.Kind = OlyILTableKind.EntityDefinition then
                                    let olyInheritsEntDef = fixup olyAsm olyInheritsEntDefOrRefHandle
                                    handleInherits olyInheritsEntDef
                                else                                   
                                    let olyInheritsEntRef = olyAsm.GetEntityReference(olyInheritsEntDefOrRefHandle)
                                    let olyInheritsEntRefEnclosing = olyInheritsEntRef.Enclosing // TODO: Check enclosings to make sure they match.
                                    let olyInheritsEntName = olyInheritsEntRef.NameHandle |> olyAsm.GetStringOrEmpty
                                    match olyAssemblys.TryGetValue(olyAsm.GetAssemblyIdentity(olyInheritsEntRef)) with
                                    | true, olyInheritsAsm when String.IsNullOrWhiteSpace(olyInheritsEntName) |> not ->
                                        let olyInheritsEntDefHandleOpt =
                                            let olyInheritsEntDefHandles = olyInheritsAsm.FindEntityDefinitions(olyInheritsEntName)
                                            olyInheritsEntDefHandles
                                            |> ImArray.tryFind (fun olyInheritsEntDefHandle ->
                                                let olyInheritsEntDef = olyInheritsAsm.GetEntityDefinition(olyInheritsEntDefHandle)
                                                if olyInheritsEntDef.FullTypeParameterCount = olyInheritsEntRef.FullTypeParameterCount then
                                                    let olyInheritsEntDefEnclosing = olyInheritsEntDef.Enclosing
                                                    // TODO: Check enclosings to make sure they match.
                                                    true
                                                else
                                                    false
                                            )

                                        match olyInheritsEntDefHandleOpt with
                                        | None ->
                                            passed <- false
                                            // TODO: We need to un-comment this. and figure out when to handle the resolved attr fixups.
                                            //       Perhaps we should introduce a notion to re-read assemblies instead of getting them from the cache.
                                            //if obj.ReferenceEquals(allPossibleAttrFixups, allUnresolvedPossibleAttrFixups) |> not then
                                            //    allUnresolvedPossibleAttrFixups.Add(olyAsm, possibleAttrFixups)
                                            olyEntDef
                                        | Some olyInheritsEntDefHandle ->
                                            let olyInheritsEntDef = fixup olyInheritsAsm olyInheritsEntDefHandle
                                            handleInherits olyInheritsEntDef
                                    | _ ->
                                        passed <- false
                                        //if obj.ReferenceEquals(allPossibleAttrFixups, allUnresolvedPossibleAttrFixups) |> not then
                                        //    allUnresolvedPossibleAttrFixups.Add(olyAsm, possibleAttrFixups)
                                        olyEntDef
                            | _ ->
                                olyEntDef
                        else
                            olyEntDef

                    fixup olyAsm olyEntDefHandle
                    |> ignore
            passed

        let passed = handle allUnresolvedPossibleAttrFixups
        if passed then
            allUnresolvedPossibleAttrFixups.Clear()

        handle allPossibleAttrFixups
        |> ignore

        allPossibleAttrFixups.Clear()

    override _.IsValidTargetName _ = true 

    override _.CanImportReference path = 
        let isValid =
            let ext = OlyPath.GetExtension(path)
            ext.Equals(".dll", StringComparison.OrdinalIgnoreCase) ||
            ext.Equals(".cs", StringComparison.OrdinalIgnoreCase) ||
            ext.EndsWith("proj", StringComparison.OrdinalIgnoreCase)
        if isValid then
            if File.Exists(path.ToString()) || Directory.Exists(path.ToString()) then
                true
            else
                false
        else
            false

    override this.ImportReferenceAsync(targetInfo, path, ct) =
        backgroundTask {
            try
                let pathStr = path.ToString()
                let dir = OlyPath.GetDirectory(path)
                let name = this.GetReferenceAssemblyName(path)
                let ext = Path.GetExtension(pathStr).ToLower()
                match assemblyCache.TryGetValue(path) with
                | true, (path, version, ilAsm, _) -> return Result.Ok(OlyCompilationReference.Create(path, version, ilAsm) |> Some)
                | _ ->
                    if ext.Equals(".cs") then
                        // TODO: Remove ".cs" as an acceptable reference to import. We should only rely on ".csproj" or ".*proj" files.
                        let cacheDir = this.GetAbsoluteCacheDirectory(targetInfo, path)
                        let! netInfo = DotNetReferences.getDotNetInfo cacheDir false targetInfo.Name [] [] [] ct
                        let references = 
                            netInfo.References
                            |> ImArray.map (fun x -> PortableExecutableReference.CreateFromFile(x.ToString()) :> MetadataReference)
                        let parseOptions = CSharpParseOptions(LanguageVersion.Preview)
                        let sourceText =
                            use fs = File.OpenRead(pathStr)
                            SourceText.From(fs)
                        let syntaxTree = CSharpSyntaxTree.ParseText(sourceText, options = parseOptions, cancellationToken = ct)
                        let compOptions = CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                        let comp = CSharpCompilation.Create(name, syntaxTrees = [syntaxTree], options = compOptions, references = references)
               
                        let ms = new MemoryStream()

                        let emitOptions = Emit.EmitOptions(metadataOnly = true)
                        let result = comp.Emit(ms, options = emitOptions, cancellationToken = ct)
                        if result.Success then
                            ms.Position <- 0L
                            let ilAsm, possibleFixups = Importer.Import(comp.AssemblyName, ms)
                            if possibleFixups.Count > 0 then
                                addPossibleFixups(ilAsm, possibleFixups)
                            let compRef = addAssemblyReference path ilAsm
                            addDirectoryWatcher dir ("*" + ext)

                            ms.Position <- 0L
                            lock csOutputsGate (fun () ->
                                match csOutputs.TryGetValue path with
                                | true, ms -> ms.Dispose()
                                | _ -> ()
                                csOutputs.[path] <- ms
                            )

                            return Result.Ok(compRef |> Some)
                        else
                            ms.Dispose()
                            return Result.Error(sprintf "%A" result.Diagnostics)

                    elif (ext.EndsWith("proj", StringComparison.OrdinalIgnoreCase)) then
                        return Result.Ok(None)
                    else
                        use fs = File.OpenRead(pathStr)
                        let ilAsm, possibleFixups = Importer.Import(name, fs)
                        if possibleFixups.Count > 0 then
                            addPossibleFixups(ilAsm, possibleFixups)
                        let compRef = addAssemblyReference path ilAsm
                        addDirectoryWatcher dir ("*" + ext)
                        return Result.Ok(compRef |> Some)
            with
            | ex ->
                return Result.Error(ex.ToString())
        }

    override this.ResolveReferencesAsync(projPath, targetInfo, referenceInfos, packageInfos: OlyPackageInfo imarray, ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            try
                let projReferenceInfos =
                    referenceInfos
                    |> ImArray.filter (fun x -> x.Path.EndsWith("proj"))
                    |> ImArray.map (fun x -> x.Path.ToString())
                let referenceInfos =
                    referenceInfos
                    |> ImArray.filter (fun x -> x.Path.EndsWith("proj") |> not)
                    |> ImArray.map (fun x -> x.Path.ToString())
                let packageInfos =
                    packageInfos 
                    |> ImArray.map (fun x -> x.Text)
                let cacheDir = this.GetAbsoluteCacheDirectory(targetInfo, projPath)
                let! netInfo = DotNetReferences.getDotNetInfo cacheDir targetInfo.IsExecutable targetInfo.Name referenceInfos projReferenceInfos packageInfos ct
                netInfos[projPath] <- netInfo
                return OlyReferenceResolutionInfo(netInfo.References, ImArray.empty)
            with
            | ex ->
                let diag = OlyDiagnostic.CreateError($"Unable to resolve references: {ex.Message}")
                return OlyReferenceResolutionInfo(ImArray.empty, ImArray.createOne diag)
        }

    override this.BuildProjectAsync(proj, ct) = backgroundTask {
        ct.ThrowIfCancellationRequested()
        let comp = proj.Compilation
        let asm = comp.GetILAssembly(ct)
        match asm with
        | Error msg -> return Error msg.Message
        | Ok asm ->

        let netInfo = netInfos[proj.Path]

        let refs =
            comp.References
            |> ImArray.choose (fun x ->
                if not x.IsCompilation && x.Path.ToString().EndsWith(".dll", StringComparison.OrdinalIgnoreCase) then
                    x.Path |> Some
                else
                    None
            )

        let primaryAssemblyOpt =
            let primaryAssemblyOpt =
                refs
                |> ImArray.tryPick (fun path ->
                    match primaryAssemblies.TryGetValue path with
                    | true, asmName -> Some asmName
                    | _ -> None
                )
            match primaryAssemblyOpt with
            | Some _ -> primaryAssemblyOpt
            | _ ->

            refs
            |> ImArray.tryPick (fun path ->
                use fs = File.OpenRead(path.ToString())
                use peReader = new System.Reflection.PortableExecutable.PEReader(fs)
                let block = peReader.GetMetadata()
                let reader = System.Reflection.Metadata.MetadataReader(block.Pointer, block.Length)

                let hasObjectType =
                    reader.TypeDefinitions.ToImmutableArray()
                    |> ImArray.exists (fun handle ->
                        let tyDef = reader.GetTypeDefinition(handle)
                        let name = reader.GetString(tyDef.Name)
                        let namespac = reader.GetString(tyDef.Namespace)
                        name = "Object" && namespac = "System"                                           
                    )
                if hasObjectType then
                    let asmName = System.Reflection.AssemblyName.GetAssemblyName(path.ToString())
                    primaryAssemblies[path] <- asmName
                    Some asmName
                else
                    None
            )

        let consoleAssemblyOpt =
            let consoleAssemblyOpt =
                refs
                |> ImArray.tryPick (fun path ->
                    match consoleAssemblies.TryGetValue path with
                    | true, asmName -> Some asmName
                    | _ -> None
                )
            match consoleAssemblyOpt with
            | Some _ -> consoleAssemblyOpt
            | _ ->

            refs
            |> ImArray.tryPick (fun path ->
                use fs = File.OpenRead(path.ToString())
                use peReader = new System.Reflection.PortableExecutable.PEReader(fs)
                let block = peReader.GetMetadata()
                let reader = System.Reflection.Metadata.MetadataReader(block.Pointer, block.Length)

                let hasObjectType =
                    reader.TypeDefinitions.ToImmutableArray()
                    |> ImArray.exists (fun handle ->
                        let tyDef = reader.GetTypeDefinition(handle)
                        let name = reader.GetString(tyDef.Name)
                        let namespac = reader.GetString(tyDef.Namespace)
                        name = "Console" && namespac = "System"                                           
                    )
                if hasObjectType then
                    let asmName = System.Reflection.AssemblyName.GetAssemblyName(path.ToString())
                    consoleAssemblies[path] <- asmName
                    Some asmName
                else
                    None
            )

        match primaryAssemblyOpt with
        | None -> return Error "Unable to find primary assembly."
        | Some primaryAssembly ->

        match consoleAssemblyOpt with
        | None -> return Error "Unable to find console assembly."
        | Some consoleAssembly ->

        let emitter = OlyRuntimeClrEmitter(asm.Name, asm.EntryPoint.IsSome, primaryAssembly, consoleAssembly)
        let runtime = OlyRuntime(emitter)

        comp.References
        |> ImArray.iter (fun x -> x.GetILAssembly(ct).ToReadOnly() |> runtime.ImportAssembly)
        runtime.ImportAssembly(asm.ToReadOnly())

        if asm.EntryPoint.IsSome then
            runtime.EmitEntryPoint()
        else
            runtime.EmitAheadOfTime()
        
        let outputPath = this.GetAbsoluteBinDirectory(proj.TargetInfo, proj.Path)
        let dirInfo = outputPath.ToDirectoryInfo()
        dirInfo.Create()
        let outputPath = outputPath.ToString()

        if copyReferences then
            let deps = netInfo.DepsJson.Replace("__oly_placeholder/1.0.0", comp.AssemblyName + "/0.0.0").Replace("__oly_placeholder", comp.AssemblyName)
            let depsPath = Path.Combine(outputPath, comp.AssemblyName + ".deps.json")
            File.WriteAllText(depsPath, deps)

            if asm.EntryPoint.IsSome then
                match netInfo.RuntimeconfigJson with
                | Some runtimeConfig ->
                    let runtimeconfigPath = Path.Combine(outputPath, comp.AssemblyName + ".runtimeconfig.json")               
                    File.WriteAllText(runtimeconfigPath, runtimeConfig)
                | _ ->
                    ()

            let exePath = Path.Combine(outputPath, comp.AssemblyName + ".dll")
            let pdbPath = Path.Combine(outputPath, comp.AssemblyName + ".pdb")

            copyDir netInfo.StubPath outputPath

            let exeFile = new System.IO.FileStream(exePath, IO.FileMode.Create)
            let pdbFile = new System.IO.FileStream(pdbPath, IO.FileMode.Create)
            emitter.Write(exeFile, pdbFile)
            exeFile.Close()
            pdbFile.Close()
            return Ok exePath
        else
            let dllPath = Path.Combine(outputPath, comp.AssemblyName + ".dll")
            let pdbPath = Path.Combine(outputPath, comp.AssemblyName + ".pdb")
            let dllFile = new System.IO.FileStream(dllPath, IO.FileMode.Create)
            let pdbFile = new System.IO.FileStream(pdbPath, IO.FileMode.Create)
            emitter.Write(dllFile, pdbFile)
            dllFile.Close()
            pdbFile.Close()
            return Ok dllPath
        }

    new (?copyReferences: bool) =
        let copyReferences =
            defaultArg copyReferences true
        DotNetTarget("dotnet", copyReferences, true)
