﻿namespace Oly.Targets.DotNet

open System
open System.IO
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.Reflection
open System.Threading
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable

open Oly.Core
open Oly.Core.IO
open Oly.Metadata
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Emitters.DotNet

open Microsoft.Build.Framework
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.CSharp

open Oly.Targets.DotNet.MSBuild
open Oly.Targets.Core

[<Sealed;Serializable>]
type ProjectBuildInfoJsonFriendly [<System.Text.Json.Serialization.JsonConstructor>]
        (targetName: string, projectPath: string, outputPath: string, references: string array, filesToCopy: string array, dependencyTimeStamp: DateTime, isExe: bool) =

    member _.TargetName = targetName
    member _.ProjectPath = projectPath
    member _.OutputPath = outputPath
    member _.References = references
    member _.FilesToCopy = filesToCopy
    member _.DependencyTimeStamp = dependencyTimeStamp
    member _.IsExe = isExe

module private DotNet =

    let private defaultCs = """static class Program
{
    static void Main()
    {
    }
}"""

    let createMainCs call = $"""static class Program
{{
    static void Main(string[] args)
    {{
        {call}
    }}
}}"""

    let getBuildInfo 
            projectName 
            (cacheDir: OlyPath) 
            (configName: string) 
            (isExe: bool) 
            (targetName: string) 
            fileReferences 
            dotnetProjectReferences 
            dotnetPackages 
            (ct: CancellationToken) =
        backgroundTask {
            let cachedBuildInfoJson = cacheDir.Join($"__oly_cached_build_info.json")

            let build() = backgroundTask {
                let msbuild = MSBuild()
                let! result = msbuild.CreateAndBuildProjectAsync(defaultCs, projectName, cacheDir, configName, isExe, MSBuildTargetInfo.ParseOnlyTargetName(targetName), fileReferences, dotnetProjectReferences, dotnetPackages, ct)
                let resultJsonFriendly =
                    ProjectBuildInfoJsonFriendly(
                        targetName,
                        result.ProjectPath.ToString(),
                        result.OutputPath,
                        result.References |> Seq.map (fun x -> x.ToString()) |> Seq.toArray,
                        result.FilesToCopy |> Seq.map (fun x -> x.ToString()) |> Seq.toArray,
                        result.DependencyTimeStamp,
                        result.IsExe
                    )

                do! Json.SerializeAsFileAsync(cachedBuildInfoJson, resultJsonFriendly, ct)
                return result
            }

            if File.Exists(cachedBuildInfoJson.ToString()) then
                let! resultJsonFriendly = Json.DeserializeFromFileAsync<ProjectBuildInfoJsonFriendly>(cachedBuildInfoJson, ct)

                let isValid =
                    resultJsonFriendly.TargetName = targetName &&
                    resultJsonFriendly.References
                    |> Array.forall File.Exists

                if isValid then
                    let dependencyTimeStamp = MSBuild.GetObjPathTimeStamp dotnetProjectReferences
                    let hasDependencyChanged = resultJsonFriendly.DependencyTimeStamp <> dependencyTimeStamp
                    let hasIsExeChanged = resultJsonFriendly.IsExe <> isExe

                    // TODO: We need to check if other files are different. LastWriteTime, deleted or added files.
                    if hasDependencyChanged || hasIsExeChanged then
                        return! build()
                    else
                        OlyTrace.Log($"[MSBuild] Using cached DotNet assembly resolution: {cachedBuildInfoJson}")
                        return 
                            {
                                TargetName = resultJsonFriendly.TargetName
                                ProjectPath = OlyPath.Create(resultJsonFriendly.ProjectPath)
                                OutputPath = resultJsonFriendly.OutputPath
                                References = resultJsonFriendly.References |> Seq.map (OlyPath.Create) |> ImArray.ofSeq
                                FilesToCopy = resultJsonFriendly.FilesToCopy |> Seq.map (OlyPath.Create) |> ImArray.ofSeq
                                ReferenceNames =
                                    (ImmutableHashSet.Empty, resultJsonFriendly.References)
                                    ||> Array.fold (fun s r ->
                                        s.Add(Path.GetFileName(r))
                                    )
                                DependencyTimeStamp = resultJsonFriendly.DependencyTimeStamp
                                IsExe = resultJsonFriendly.IsExe
                            }
                else
                    return! build()
            else
                return! build()
        }

    let publish
            call
            projectName 
            (outputPath: OlyPath) 
            (configName: string) 
            (isExe: bool) 
            msbuildTargetInfo
            referenceInfos 
            projReferenceInfos 
            packageInfos 
            (ct: CancellationToken) =
        backgroundTask {
            let msbuild = MSBuild()
            let! result = msbuild.CreateAndBuildProjectAsync(createMainCs call, projectName, outputPath, configName, isExe, msbuildTargetInfo, referenceInfos, projReferenceInfos, packageInfos, ct)
            return result
        }

type DotNetTarget internal (platformName: string, copyReferences: bool) =
    inherit OlyBuild(platformName)

    let gate = obj ()

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

    let directoryWatcherEquality =
        { new IEqualityComparer<OlyPath * string> with
            member _.GetHashCode (obj: OlyPath * string): int = 
               (fst obj).GetHashCode()
            member _.Equals ((path1, filter1): OlyPath * string, (path2, filter2): OlyPath * string): bool = 
                path1 = path2 && filter1 = filter2
        }

    let dirGate = obj()
    let directoryWatchers = ConcurrentDictionary<OlyPath * string, FileSystemWatcher>(directoryWatcherEquality)
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

    let netInfos = ConcurrentDictionary<OlyPath, ProjectBuildInfo>()

    static member CompileCSharp(name: string, src: string, references: OlyPath imarray, ct: CancellationToken) =
        let references = 
            references
            |> ImArray.map (fun x -> PortableExecutableReference.CreateFromFile(x.ToString()) :> MetadataReference)
        let parseOptions = CSharpParseOptions(LanguageVersion.Preview)
        let sourceText = SourceText.From(src)
        let syntaxTree = CSharpSyntaxTree.ParseText(sourceText, options = parseOptions, cancellationToken = ct)
        let compOptions = CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
        let comp = CSharpCompilation.Create(name, syntaxTrees = [syntaxTree], options = compOptions, references = references)

        let ms = new MemoryStream()

        let emitOptions = Emit.EmitOptions()
        let result = comp.Emit(ms, options = emitOptions, cancellationToken = ct)
        if result.Success then
            ms.Position <- 0L
            ms
        else
            ms.Dispose()
            failwithf "%A" result.Diagnostics

    member _.GetCSharpOutput(path) =
        match csOutputs.TryGetValue path with
        | true, ms -> ms
        | _ -> failwithf "CSharp output does not exist: %A." path

    abstract GetReferenceAssemblyName : OlyPath -> string
    default _.GetReferenceAssemblyName(path) =
        let pathStr = path.ToString()
        Path.GetFileNameWithoutExtension(pathStr)

    override _.IsValidTargetName _ = true 

    override _.CanImportReference path = 
        let isValid =
            let ext = path.GetExtension()
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

    override this.ImportReferenceAsync(projPath, targetInfo, path, ct) =
        backgroundTask {
            let netInfo = netInfos[projPath]
            try
                let pathStr = path.ToString()
                let dir = path.GetDirectory()
                let name = this.GetReferenceAssemblyName(path)
                let ext = Path.GetExtension(pathStr).ToLower()

                let isTransitive =
                    // This is ok to check because, at this point, 'netInfo' only has framework references.
                    // We do not want to make the framework references transitive for dotnet.
                    not(netInfo.ReferenceNames.Contains(path.GetFileName()))

                match assemblyCache.TryGetValue(path) with
                | true, (path, version, ilAsm, _) -> 
                    let compRef = OlyCompilationReference.Create(path, version, ilAsm)
                    return Result.Ok(OlyImportedReference(compRef, isTransitive) |> Some)
                | _ ->
                    if ext.Equals(".cs") then
                        let cacheDir = this.GetProjectCacheDirectory(targetInfo, path)
                        let! netInfo = DotNet.getBuildInfo (Path.GetFileNameWithoutExtension(ext)) cacheDir targetInfo.ProjectConfiguration.Name false targetInfo.Name [] [] [] ct
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
                            let ilAsm = Importer.Import(comp.AssemblyName, ms)
                            let compRef = addAssemblyReference path ilAsm
                            addDirectoryWatcher dir ("*" + ext)

                            ms.Position <- 0L
                            lock csOutputsGate (fun () ->
                                match csOutputs.TryGetValue path with
                                | true, ms -> ms.Dispose()
                                | _ -> ()
                                csOutputs.[path] <- ms
                            )

                            return Result.Ok(OlyImportedReference(compRef, isTransitive) |> Some)
                        else
                            ms.Dispose()
                            return Result.Error(sprintf "%A" result.Diagnostics)

                    elif (ext.EndsWith("proj", StringComparison.OrdinalIgnoreCase)) then
                        return Result.Ok(None)
                    else
                        use fs = File.OpenRead(pathStr)
                        let ilAsm = Importer.Import(name, fs)
                        let compRef = addAssemblyReference path ilAsm
                        addDirectoryWatcher dir ("*" + ext)
                        return Result.Ok(OlyImportedReference(compRef, isTransitive) |> Some)
            with
            | ex ->
                return Result.Error(ex.ToString())
        }

    override this.ResolveReferencesAsync(projPath, targetInfo, referenceInfos, packageInfos: OlyPackageInfo imarray, ct: CancellationToken) =
        backgroundTask {
            ct.ThrowIfCancellationRequested()
            try
                let dotnetProjectReferences =
                    referenceInfos
                    |> ImArray.filter (fun x -> x.Path.EndsWith("proj"))
                    |> ImArray.map (fun x -> x.Path.ToString())
                let fileReferences =
                    referenceInfos
                    |> ImArray.filter (fun x -> x.Path.EndsWith("proj") |> not)
                    |> ImArray.map (fun x -> x.Path.ToString())
                let dotnetPackages =
                    packageInfos 
                    |> ImArray.map (fun x -> x.Text)
                let cacheDir = this.GetProjectCacheDirectory(targetInfo, projPath)
                let! netInfo = DotNet.getBuildInfo (projPath.GetFileNameWithoutExtension()) cacheDir targetInfo.ProjectConfiguration.Name targetInfo.IsExecutable targetInfo.Name fileReferences dotnetProjectReferences dotnetPackages ct
                netInfos[projPath] <- netInfo
                return OlyReferenceResolutionInfo(netInfo.References, netInfo.FilesToCopy, ImArray.empty)
            with
            | ex ->
                let diag = OlyDiagnostic.CreateError($"Unable to resolve references: {ex.Message}")
                return OlyReferenceResolutionInfo(ImArray.empty, ImArray.empty, ImArray.createOne diag)
        }

    [<DynamicDependency(DynamicallyAccessedMemberTypes.All, typeof<ProjectBuildInfoJsonFriendly>)>]
    override this.BuildProjectAsync(proj, ct) = backgroundTask {
        ct.ThrowIfCancellationRequested()

        let diags = proj.GetDiagnostics(ct)
        if diags |> ImArray.exists (fun x -> x.IsError) then
            return Error(diags)
        else

        let analyzerTask =
            // This is to make the happy path of successful compilations
            // faster as we can run the analyzers in the background
            // while we are compiling to IL.
            // TODO: Add a way to cancel this sooner.
            System.Threading.Tasks.Task.Factory.StartNew(fun () ->
                let diags = proj.GetAnalyzerDiagnostics(ct)
                if diags |> ImArray.exists (fun x -> x.IsError) then
                    Error(diags)
                else
                    Ok()
            )

        let comp = proj.Compilation
        let asm = comp.GetILAssembly(ct)
        match asm with
        | Error _ -> return Error(proj.GetDiagnostics(ct))
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
        | None -> return Error(OlyDiagnostic.CreateError("Unable to find primary assembly.") |> ImArray.createOne)
        | Some primaryAssembly ->

        match consoleAssemblyOpt with
        | None -> return Error(OlyDiagnostic.CreateError("Unable to find console assembly.") |> ImArray.createOne)
        | Some consoleAssembly ->

        let msbuildTargetInfo = MSBuildTargetInfo.Parse(proj.TargetInfo.Name)
        let asmName =
            if msbuildTargetInfo.IsPublish then
                asm.Name + "__oly_internal"
            else
                asm.Name

        let emitter = OlyRuntimeClrEmitter(asmName, asm.EntryPoint.IsSome, primaryAssembly, consoleAssembly)
        let runtime = OlyRuntime(emitter)

        let refDiags = ImArray.builder()
        comp.References
        |> ImArray.iter (fun x ->
            match x.GetILAssembly(ct) with
            | Ok x -> x.ToReadOnly() |> runtime.ImportAssembly
            | Error diags -> refDiags.AddRange(diags |> ImArray.filter (fun x -> x.IsError))
        )

        if refDiags.Count > 0 then
            return Error(refDiags.ToImmutable())
        else

        match! analyzerTask with
        | Error diags -> return Error(diags)
        | _ ->

        runtime.ImportAssembly(asm.ToReadOnly())

        runtime.InitializeEmitter()

        match OlyTarget.CheckedEmit(asm.EntryPoint.IsSome, proj, runtime, ct) with
        | Some(diag) -> return Error(ImArray.createOne diag)
        | _ ->
        
        let outputPath = this.GetProjectBinDirectory(proj.TargetInfo, proj.Path)
        let dirInfo = outputPath.ToDirectoryInfo()
        dirInfo.Create()
        let outputPath = outputPath.ToString()

        let copyFilesFromProject (proj: OlyProject) =
            proj.CopyFileInfos
            |> ImArray.iter (fun info ->
                let file = FileInfo(info.Path.ToString())
                let destFile = FileInfo(Path.Combine(outputPath, info.Path.GetFileName()))

                if destFile.Exists then
                    if file.LastWriteTimeUtc <> destFile.LastWriteTimeUtc then
                        // now you can safely overwrite it
                        file.CopyTo(destFile.FullName, true)
                        |> ignore
                else
                    file.CopyTo(destFile.FullName, true)
                    |> ignore
            )

        let copyFiles() =
            let transitiveRefProjs = proj.Solution.GetTransitiveProjectReferencesFromProject(proj.Path, ct)
            transitiveRefProjs
            |> ImArray.iter copyFilesFromProject
            copyFilesFromProject proj

        if copyReferences then
            let msbuildTargetInfo = MSBuildTargetInfo.Parse(proj.TargetInfo.Name)
            if msbuildTargetInfo.IsPublish then
                // TODO: This really needs some massive cleanup.

                let dotnetProjectReferences = ImArray.empty
                let fileReferences =
                    proj.Compilation.References
                    |> ImArray.choose (fun r ->
                        if r.Path.EndsWith(".dll") then
                            Some(r.Path.ToString())
                        else
                            None
                    )
                let dotnetPackages = ImArray.empty
                let cacheDir = this.GetProjectCacheDirectory(proj.TargetInfo, proj.Path)

                let dllPath = Path.Combine(cacheDir.ToString(), comp.AssemblyName + "__oly_internal.dll")
                let pdbPath = Path.Combine(cacheDir.ToString(), comp.AssemblyName + "__oly_internal.pdb")
                let dllFile = new System.IO.FileStream(dllPath, IO.FileMode.Create)
                let pdbFile = new System.IO.FileStream(pdbPath, IO.FileMode.Create)
                emitter.Write(dllFile, pdbFile, asm.IsDebuggable)
                dllFile.Close()
                pdbFile.Close()
                copyFiles()

                let projectName = proj.Path.GetFileNameWithoutExtension()

                let entryPoint = (runtime : Oly.Runtime.CodeGen.IOlyVirtualMachine<_, _, _>).TryGetEntryPoint().Value

                let call = 
                    let m = entryPoint.AsDefinition
                    let text = m.enclosingTyHandle.FullyQualifiedName + "." + m.name
                    if m.Parameters.IsEmpty then
                        text + "();"
                    else
                        text + "(args);"

                let! _ =
                    if msbuildTargetInfo.IsNativeAOT then
                        OlyTrace.Log($"[Compilation] Compiling NativeAOT...")
                    else
                        OlyTrace.Log($"[Compilation] Compiling ReadyToRun...")
                    DotNet.publish 
                        call
                        projectName
                        (OlyPath.Create(outputPath))
                        proj.TargetInfo.ProjectConfiguration.Name 
                        proj.TargetInfo.IsExecutable 
                        msbuildTargetInfo 
                        (fileReferences.Add(dllPath))
                        dotnetProjectReferences 
                        dotnetPackages 
                        ct

                let exePath = 
                    if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then
                        Path.Combine(outputPath, projectName + ".exe")
                    else
                        Path.Combine(outputPath, projectName)

                return Ok(OlyProgram(OlyPath.Create(exePath), 
                    fun args -> 
                        use p = new ExternalProcess(exePath, String.Join(' ', args))
                        let result = p.RunAsync(CancellationToken.None).Result
                        if not(System.String.IsNullOrWhiteSpace(result.Errors)) then
                            failwith result.Errors
                        else
                            result.Output
                ))

            else
                let dllPath = Path.Combine(outputPath, comp.AssemblyName + ".dll")
                let pdbPath = Path.Combine(outputPath, comp.AssemblyName + ".pdb")

                OlyIO.CopyDirectory(netInfo.OutputPath, outputPath)

                let dllFile = new System.IO.FileStream(dllPath, IO.FileMode.Create)
                let pdbFile = new System.IO.FileStream(pdbPath, IO.FileMode.Create)
                emitter.Write(dllFile, pdbFile, asm.IsDebuggable)
                dllFile.Close()
                pdbFile.Close()

                copyFiles()
                return Ok(OlyProgram(OlyPath.Create(dllPath), 
                    fun args ->
                        use p = new ExternalProcess("dotnet", dllPath + " " + String.Join(' ', args))
                        let result = p.RunAsync(CancellationToken.None).Result
                        if not(System.String.IsNullOrWhiteSpace(result.Errors)) then
                            failwith result.Errors
                        else
                            result.Output
                ))
        else
            let dllPath = Path.Combine(outputPath, comp.AssemblyName + ".dll")
            let pdbPath = Path.Combine(outputPath, comp.AssemblyName + ".pdb")
            let dllFile = new System.IO.FileStream(dllPath, IO.FileMode.Create)
            let pdbFile = new System.IO.FileStream(pdbPath, IO.FileMode.Create)
            emitter.Write(dllFile, pdbFile, asm.IsDebuggable)
            dllFile.Close()
            pdbFile.Close()

            copyFiles()
            return Ok(OlyProgram(OlyPath.Create(dllPath), 
                fun args -> 
                    use p = new ExternalProcess("dotnet", dllPath + " " + String.Join(' ', args))
                    let result = p.RunAsync(CancellationToken.None).Result
                    if not(System.String.IsNullOrWhiteSpace(result.Errors)) then
                        failwith result.Errors
                    else
                        result.Output
            ))
        }

    override _.GetImplicitExtendsForStruct() = Some "System.ValueType"

    override _.GetImplicitExtendsForEnum() = Some "System.Enum"

    override _.GetAnalyzerDiagnostics(_targetInfo, boundModel: OlyBoundModel, ct: CancellationToken): OlyDiagnostic imarray = 
        let diagnostics = OlyDiagnosticLogger.CreateWithPrefix("DotNet")

        let analyzeSymbol (symbolInfo: OlySymbolUseInfo) =
            if (symbolInfo.Syntax.IsDefinition || symbolInfo.Syntax.IsCompilationUnit) && symbolInfo.Symbol.IsExported && symbolInfo.Symbol.IsType then
                let ty = symbolInfo.Symbol.AsType
                let subModel = symbolInfo.SubModel

                let funcGroups =
                    ty.Functions
                    |> Seq.filter (fun x ->
                        if x.IsConstructor then
                            x.Enclosing.TryType.Value.IsSimilarTo(ty)
                        else
                            true
                    )
                    |> Seq.groupBy (fun x -> (x.Name, x.TypeParameterCount, x.Parameters.Length, x.IsStatic))
                    |> Seq.choose (fun (_, funcs) ->
                        let funcs = funcs |> ImArray.ofSeq
                        if funcs.Length <= 1 then
                            None
                        else
                            Some(funcs)
                    )
                    |> ImArray.ofSeq

                funcGroups
                |> ImArray.iter (fun funcs ->
                    funcs
                    |> ImArray.iter (fun func ->
                        funcs
                        |> ImArray.iter (fun func2 ->
                            if obj.ReferenceEquals(func, func2) then ()
                            else
                                let mutable allParsAreSame = true
                                (func.Parameters, func2.Parameters)
                                ||> ImArray.iter2 (fun par1 par2 ->
                                    if not(par1.Type.IsEqualTo(par2.Type)) then
                                        if not par1.Type.IsTypeAnyByRef || not par2.Type.IsTypeAnyByRef then
                                            allParsAreSame <- false     
                                )

                                match func.ReturnType, func2.ReturnType with
                                | Some(returnTy1), Some(returnTy2) ->
                                    if not(returnTy1.IsEqualTo(returnTy2)) then
                                        if not returnTy1.IsTypeAnyByRef || not returnTy2.IsTypeAnyByRef then
                                            allParsAreSame <- false
                                    elif func.Parameters.IsEmpty then
                                        allParsAreSame <- false
                                | _ ->
                                    allParsAreSame <- false

                                if allParsAreSame then
                                    let location = func.TryGetDefinitionLocation(boundModel, ct)
                                    match location with
                                    | Some(loc) ->
                                        let textRange = loc.GetTextRange(ct)
                                        let syntaxNode =
                                            match loc.SyntaxTree.TryFindNode(textRange, ct) with
                                            | Some syntaxNode -> syntaxNode
                                            | _ -> symbolInfo.Syntax
                                        diagnostics.Error($"Unable to disambiguate types on function '{subModel.GetSignatureText(func)}'.", 10, syntaxNode)
                                    | _ ->
                                        diagnostics.Error($"Unable to disambiguate types on function '{subModel.GetSignatureText(func)}'.", 10, symbolInfo.Syntax)
                        )
                    )
                )

        match boundModel.TryGetAnonymousModuleSymbol(ct) with
        | Some symbol ->
            analyzeSymbol symbol.UntypedInfo
        | _ ->
            ()

        boundModel.ForEachSymbol(boundModel.SyntaxTree.GetRoot(ct), analyzeSymbol, ct)

        diagnostics.GetDiagnostics()

    new (?copyReferences: bool) =
        let copyReferences =
            defaultArg copyReferences true
        DotNetTarget("dotnet", copyReferences)
