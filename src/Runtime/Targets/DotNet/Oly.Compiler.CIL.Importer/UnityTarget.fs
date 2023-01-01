namespace Oly.Runtime.Target.DotNet

open System
open System.IO

open Oly.Core
open Oly.Compiler.Workspace

//[<AutoOpen>]
//module private UnityHelpers =

//    let CreateILPostProcessorProcess path targetAsmPath outputDir processorPaths asmDirs runtimeAsmPaths =
//        let o = $"--o=\"{outputDir}\""
//        let a = $"--a=\"{targetAsmPath}\""
//        let ps =
//            processorPaths
//            |> ImArray.map (fun x ->
//                $"--p=\"{x}\""
//            )
//        let fs =
//            asmDirs
//            |> ImArray.map (fun x ->
//                $"--f=\"{x}\""
//            )
//        let rs =
//            runtimeAsmPaths
//            |> ImArray.map (fun x ->
//                $"--r=\"{x}\""
//            )

//        let args =
//            seq {
//                yield o
//                yield a
//                yield! ps
//                yield! fs
//                yield! rs
//            }
//            |> String.concat " "

//        ExternalProcess(path, args)

[<Sealed>]
type UnityTarget() =
    inherit DotNetTarget("unity", false, false)

    // TODO: Implement auto execution of ILPostProcessorRunner.
    //"C:\Program Files\Unity\Hub\Editor\2021.2.8f1\Editor\Data\Tools\netcorerun\netcorerun.exe" 
    //    "C:\Program Files\Unity\Hub\Editor\2021.2.8f1\Editor\Data\Tools\ILPostProcessorRunner\ILPostProcessorRunner.exe" 
    //        --o="C:\work\ProjectBetaLambda\Assets\Scripts" 
    //        --a="C:\work\ProjectBetaLambda\Assets\Scripts\main.dll" 
    //        --p="C:\work\ProjectBetaLambda\Library\ScriptAssemblies\Unity.Netcode.Editor.CodeGen.dll" 
    //        --f="C:\work\ProjectBetaLambda\Library\PackageCache\com.unity.nuget.mono-cecil@1.10.1" 
    //        --f="C:\Program Files\Unity\Hub\Editor\2021.2.8f1\Editor\Data\MonoBleedingEdge\lib\mono\4.8-api\Facades" 
    //        --f="C:\Program Files\Unity\Hub\Editor\2021.2.8f1\Editor\Data\Managed\UnityEngine" 
    //        --f="C:\work\ProjectBetaLambda\Library\ScriptAssemblies" 
    //        --r="C:\work\ProjectBetaLambda\Library\ScriptAssemblies\Unity.Netcode.Runtime.dll"

    let defaultDir =
        """C:\Program Files\Unity\Hub\Editor"""

    let defaultRelativeUnityAssemblyDir =
        """Editor\Data\Managed\UnityEngine"""

    let defaultRelativeDotNetRefAssemblyDir =
        """Editor\Data\NetStandard\ref\2.1.0"""

    let defaultRelativeDotNetAssemblyDir =
        """Editor\Data\NetStandard\compat\2.1.0\shims\netfx"""

    let defaultRelativeDotNetStandardAssemblyDir =
        """Editor\Data\NetStandard\compat\2.1.0\shims\netstandard"""

    let defaultRelativeDotNetFacadesDir =
        """Editor\Data\MonoBleedingEdge\lib\mono\4.8-api\Facades"""

    let defaultRelativeILPostProcessorDir =
        """Editor\Data\Tools\ILPostProcessorRunner\ILPostProcessorRunner.exe"""

    let defaultRelativeProjectScriptAssembliesDir =
        """Library\ScriptAssemblies"""

    let getAssemblies dir targetName relativeDir =
        let dotnetAsmPath = Path.Combine(dir + """\""" + targetName, relativeDir)
        let dotnetAsms =
            Directory.EnumerateFiles(dotnetAsmPath)
            |> Seq.choose (fun x ->
                if x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase) then
                    Some(OlyPath.Create(x))
                else
                    None
            )
            |> ImArray.ofSeq
        dotnetAsms

    let getUnityRefPaths targetName =
        getAssemblies defaultDir targetName defaultRelativeUnityAssemblyDir

    member private this.GetNetStandardReferences(projPath, referenceInfos, packageInfos, ct) =
        base.ResolveReferencesAsync(projPath, OlyTargetInfo("netstandard2.1", OlyOutputKind.Library), referenceInfos, packageInfos, ct)

    override _.IsValidTargetName _ =
        true

    override _.GetReferenceAssemblyName(path) = 
        if path.ToString().EndsWith(".cs", StringComparison.OrdinalIgnoreCase) then
            "Assembly-CSharp"
        else
            base.GetReferenceAssemblyName(path)

    override this.ResolveReferencesAsync(projPath, targetInfo, referenceInfos, packageInfos, ct) =
        backgroundTask {
            let! resInfo = this.GetNetStandardReferences(projPath, referenceInfos, packageInfos, ct)
            try
                let unityAsms = getUnityRefPaths targetInfo.Name
                return OlyReferenceResolutionInfo(resInfo.Paths.AddRange(unityAsms), resInfo.Diagnostics)
            with
            | ex ->
                return resInfo
        }


