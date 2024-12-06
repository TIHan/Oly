namespace Oly.Runtime.Target.Spirv

open System
open System.IO
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Runtime.Target.Core
open Oly.Runtime.Target.Spirv.Emitter
open Spirv
open Spirv.SpirvModule

[<Sealed>]
type SpirvTarget() =
    inherit OlyTargetOutputOnly<SpirvEmitter, SpirvType, SpirvFunction, SpirvField>("spirv")

    override this.CreateEmitter(targetInfo) = 
        let splits = targetInfo.Name.Split(',')
        if splits.Length = 2 && targetInfo.IsExecutable then
            let executionModel =
                match splits |> Array.head with
                | "vertex" -> ExecutionModel.Vertex
                | "fragment" -> ExecutionModel.Fragment
                | "compute" -> ExecutionModel.GLCompute
                | _ -> raise(InvalidOperationException())

            match splits[1] with
            | "1.0" -> SpirvEmitter(1u, 0u, executionModel)
            | "1.1" -> SpirvEmitter(1u, 1u, executionModel)
            | "1.2" -> SpirvEmitter(1u, 2u, executionModel)
            | "1.3" -> SpirvEmitter(1u, 3u, executionModel)
            | "1.4" -> SpirvEmitter(1u, 4u, executionModel)
            | "1.5" -> SpirvEmitter(1u, 5u, executionModel)
            | "1.6" -> SpirvEmitter(1u, 6u, executionModel)
            | _ -> raise(InvalidOperationException(""))
        else
            raise(InvalidOperationException())

    override this.EmitOutput(project, _binDirectory, emitter, isDebuggable) = 
        let output = emitter.EmitOutput(isDebuggable)
        let outputFilePath = OlyPath.ChangeExtension(project.Path, ".spv")
        use fs = File.Open(outputFilePath.ToString(), FileMode.Create)
        SpirvModule.Serialize(fs, output.Module)

    override this.IsValidTargetName targetInfo = 
        // Target support
        let splits = targetInfo.Name.Split(',')
        if splits.Length = 2 && targetInfo.IsExecutable then
            match splits |> Array.head with
            | "vertex"
            | "fragment"
            | "compute" ->
                match splits[1] with
                | "1.0"
                | "1.1"
                | "1.2"
                | "1.3" 
                | "1.4" 
                | "1.5"
                | "1.6" -> true
                | _ -> false
            | _ -> 
                false
        elif splits.Length = 1 && not targetInfo.IsExecutable then
            match splits |> Array.head with
            | "lib" -> true
            | _ -> false
        else
            false

    override _.GetAnalyzerDiagnostics(boundModel: OlyBoundModel, ct): OlyDiagnostic imarray = 
        let diagLogger = OlyDiagnosticLogger.Create()

        let isSpirvAttributeName attrName =
            match attrName with
            | "uniformAttribute"
            | "locationAttribute"
            | "bindingAttribute"
            | "descriptor_setAttribute"
            | "positionAttribute"
            | "global_invocation_idAttribute" -> true
            | _ -> false

        let isSpirvInputName name =
            match name with
            | "uniform"
            | "location"
            | "binding"
            | "descriptor_set"
            | "global_invocation_id" -> true
            | _ -> false

        let isSpirvOutputName name =
            match name with
            | "location"
            | "position" -> true
            | _ -> false

        let checkSpirvInputAttributeUsage name (useSyntax: OlySyntaxNode) =
            let isValid =
                match useSyntax.TryFindParent<OlySyntaxPropertyBinding>(ct) with
                | Some syntaxPropBinding ->
                    match syntaxPropBinding with
                    | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, syntaxBinding) ->
                        match syntaxBinding with
                        | OlySyntaxBinding.Signature(syntaxBindingDecl) ->
                            match syntaxBindingDecl with
                            | OlySyntaxBindingDeclaration.Get _ when isSpirvInputName name
                            | OlySyntaxBindingDeclaration.Set _ when isSpirvOutputName name ->
                                true
                            | _ ->
                                false
                        | _ ->
                            false
                    | _ ->
                        unreached()
                | _ ->
                    false

            if not isValid then
                diagLogger.Error($"SpirV: Invalid use of '{name}' attribute.", 10, useSyntax)

        let analyzeSymbol (symbol: OlySymbol) =
            if not symbol.UseSyntax.IsDefinition && symbol.IsFunction then
                if (symbol.IsConstructor && isSpirvAttributeName symbol.Name) then
                    checkSpirvInputAttributeUsage (symbol.Name.Replace("Attribute", "")) symbol.UseSyntax

        match boundModel.TryGetAnonymousModuleSymbol(ct) with
        | Some symbol ->
            analyzeSymbol symbol
        | _ ->
            ()

        boundModel.ForEachSymbol(boundModel.SyntaxTree.GetRoot(ct), analyzeSymbol, ct)

        diagLogger.GetDiagnostics()
