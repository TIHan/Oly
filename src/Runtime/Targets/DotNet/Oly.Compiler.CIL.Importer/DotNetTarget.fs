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

open Oly.Runtime.Target.DotNet.MSBuild

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

    let getDotNetInfo (cacheDir: OlyPath) (isExe: bool) (targetName: string) referenceInfos projReferenceInfos packageInfos (ct: CancellationToken) =
        let msbuild = MSBuild()
        msbuild.CreateAndBuildProjectAsync("__oly_placeholder", cacheDir, isExe, targetName, referenceInfos, projReferenceInfos, packageInfos, ct)

type DotNetTarget internal (platformName: string, copyReferences: bool, emitPdb: bool) =
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

    let netInfos = ConcurrentDictionary<OlyPath, ProjectBuildInfo>()

    let frameworkRefs = ConcurrentDictionary<string, ProjectBuildInfo>()

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

    override this.OnBeforeReferencesImportedAsync(projPath: OlyPath, targetInfo: OlyTargetInfo, ct: System.Threading.CancellationToken): System.Threading.Tasks.Task<unit> =
        backgroundTask {
            match frameworkRefs.TryGetValue targetInfo.Name with
            | true, netInfo ->
                netInfos[projPath] <- netInfo
                ()
            | _ ->
                let cacheDir = this.GetAbsoluteCacheDirectory(projPath)
                let! netInfo = DotNetReferences.getDotNetInfo cacheDir targetInfo.IsExecutable targetInfo.Name ImArray.empty ImArray.empty ImArray.empty ct
                netInfos[projPath] <- netInfo
                frameworkRefs[targetInfo.Name] <- netInfo
                return ()
        }

    override this.OnAfterReferencesImported() =
        ()

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

    override this.ImportReferenceAsync(projPath, targetInfo, path, ct) =
        backgroundTask {
            let netInfo = netInfos[projPath]
            try
                let pathStr = path.ToString()
                let dir = OlyPath.GetDirectory(path)
                let name = this.GetReferenceAssemblyName(path)
                let ext = Path.GetExtension(pathStr).ToLower()

                let isTransitive =
                    // This is ok to check because, at this point, 'netInfo' only has framework references.
                    // We do not want to make the framework references transitive for dotnet.
                    not(netInfo.ReferenceNames.Contains(OlyPath.GetFileName(path)))

                match assemblyCache.TryGetValue(path) with
                | true, (path, version, ilAsm, _) -> 
                    let compRef = OlyCompilationReference.Create(path, version, ilAsm)
                    return Result.Ok(OlyImportedReference(compRef, isTransitive) |> Some)
                | _ ->
                    if ext.Equals(".cs") then
                        // TODO: Remove ".cs" as an acceptable reference to import. We should only rely on ".csproj" or ".*proj" files.
                        let cacheDir = this.GetAbsoluteCacheDirectory(path)
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
                let cacheDir = this.GetAbsoluteCacheDirectory(projPath)
                let! netInfo = DotNetReferences.getDotNetInfo cacheDir targetInfo.IsExecutable targetInfo.Name referenceInfos projReferenceInfos packageInfos ct
                netInfos[projPath] <- netInfo
                return OlyReferenceResolutionInfo(netInfo.References, netInfo.FilesToCopy, ImArray.empty)
            with
            | ex ->
                let diag = OlyDiagnostic.CreateError($"Unable to resolve references: {ex.Message}")
                return OlyReferenceResolutionInfo(ImArray.empty, ImArray.empty, ImArray.createOne diag)
        }

    override this.BuildProjectAsync(proj, ct) = backgroundTask {
        ct.ThrowIfCancellationRequested()
        let comp = proj.Compilation
        let asm = comp.GetILAssembly(ct)
        match asm with
        | Error diags -> return Error(diags)
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

        let emitter = OlyRuntimeClrEmitter(asm.Name, asm.EntryPoint.IsSome, primaryAssembly, consoleAssembly)
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

        runtime.ImportAssembly(asm.ToReadOnly())

        runtime.InitializeEmitter()

        if asm.EntryPoint.IsSome then
            runtime.EmitEntryPoint()
        else
            runtime.EmitAheadOfTime()
        
        let outputPath = this.GetAbsoluteBinDirectory(proj.Path)
        let dirInfo = outputPath.ToDirectoryInfo()
        dirInfo.Create()
        let outputPath = outputPath.ToString()

        let copyFilesFromProject (proj: OlyProject) =
            proj.CopyFileInfos
            |> ImArray.iter (fun info ->
                let file = FileInfo(info.Path.ToString())
                let destFile = FileInfo(Path.Combine(outputPath, OlyPath.GetFileName(info.Path)))

                if destFile.Exists then
                    if file.LastWriteTimeUtc <> destFile.LastWriteTimeUtc then
                        // now you can safely overwrite it
                        file.CopyTo(destFile.FullName, true)
                        |> ignore
                else
                    file.CopyTo(destFile.FullName, true)
                    |> ignore
            )

        let transitiveRefProjs = proj.Solution.GetTransitiveProjectReferencesFromProject(proj.Path, ct)
        transitiveRefProjs
        |> ImArray.iter copyFilesFromProject
        copyFilesFromProject proj

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

            copyDir netInfo.OutputPath outputPath

            let exeFile = new System.IO.FileStream(exePath, IO.FileMode.Create)
            let pdbFile = new System.IO.FileStream(pdbPath, IO.FileMode.Create)
            emitter.Write(exeFile, pdbFile, asm.IsDebuggable)
            exeFile.Close()
            pdbFile.Close()
            return Ok exePath
        else
            let dllPath = Path.Combine(outputPath, comp.AssemblyName + ".dll")
            let pdbPath = Path.Combine(outputPath, comp.AssemblyName + ".pdb")
            let dllFile = new System.IO.FileStream(dllPath, IO.FileMode.Create)
            let pdbFile = new System.IO.FileStream(pdbPath, IO.FileMode.Create)
            emitter.Write(dllFile, pdbFile, asm.IsDebuggable)
            dllFile.Close()
            pdbFile.Close()
            return Ok dllPath
        }

    override _.GetImplicitExtendsForStruct() = Some "System.ValueType"

    override _.GetImplicitExtendsForEnum() = Some "System.Enum"

    new (?copyReferences: bool) =
        let copyReferences =
            defaultArg copyReferences true
        DotNetTarget("dotnet", copyReferences, true)
