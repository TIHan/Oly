namespace Oly.Targets.Spirv

open System
open System.IO
open System.Collections.Immutable

open Oly.Core
open Oly.Compiler
open Oly.Compiler.Text
open Oly.Compiler.Syntax
open Oly.Compiler.Workspace
open Oly.Runtime
open Oly.Targets.Core
open Oly.Emitters.Spirv
open Spirv
open Spirv.SpirvModule

[<RequireQualifiedAccess>]
 module private Constants =

    [<Literal>]
    let vertex = "vertex"

    [<Literal>]
    let fragment = "fragment"

    [<Literal>]
    let compute = "compute"

    [<Literal>]
    let lib = "lib"

[<Flags>]
type private AttributeFlags =
    | None               = 0b000000

    | vertex             = 0b000001
    | fragment           = 0b000010
    | compute            = 0b000100
    | ExecutionModelMask = 0b000111

[<Sealed>]
type SpirvTarget() =
    inherit OlyTargetOutputOnly<SpirvEmitter, SpirvType, SpirvFunction, SpirvField>("spirv")

    static let IsValidVersion(version: string) =
        match version with
        | "1.0"
        | "1.1"
        | "1.2"
        | "1.3"
        | "1.4"
        | "1.5"
        | "1.6" -> true
        | _ -> false

    static let IsValidExecutionModel(name: string) =
        match name with
        | Constants.vertex
        | Constants.fragment
        | Constants.compute -> true
        | _ -> false

    static let GetVersion(version: string) =
        match version with
        | "1.0" -> (1u, 0u)
        | "1.1" -> (1u, 1u)
        | "1.2" -> (1u, 2u)
        | "1.3" -> (1u, 3u)
        | "1.4" -> (1u, 4u)
        | "1.5" -> (1u, 5u)
        | "1.6" -> (1u, 6u)
        | _ -> invalidOp $"Invalid SpirV version: {version}"

    static let GetExecutionModel(name: string) =
        match name with
        | Constants.vertex -> ExecutionModel.Vertex
        | Constants.fragment -> ExecutionModel.Fragment
        | Constants.compute -> ExecutionModel.GLCompute
        | _ -> invalidOp $"Invalid SpirV execution model: {name}"

    static let GetVersionAndExecutionModel(name: string, version: string) =
        let majorVersion, minorVersion = GetVersion(version)
        let executionModel = GetExecutionModel(name)
        majorVersion, minorVersion, executionModel

    static let AttributeValidationLookup =
        let lookup = System.Collections.Generic.Dictionary()
        lookup["positionAttribute"] <- AttributeFlags.vertex
        lookup["global_invocation_idAttribute"] <- AttributeFlags.compute
        lookup["uniformAttribute"] <- AttributeFlags.vertex ||| AttributeFlags.fragment ||| AttributeFlags.compute
        lookup["locationAttribute"] <- AttributeFlags.vertex ||| AttributeFlags.fragment ||| AttributeFlags.compute
        lookup["descriptor_setAttribute"] <- AttributeFlags.vertex ||| AttributeFlags.fragment ||| AttributeFlags.compute
        lookup["bindingAttribute"] <- AttributeFlags.vertex ||| AttributeFlags.fragment ||| AttributeFlags.compute
        lookup["storage_bufferAttribute"] <- AttributeFlags.vertex ||| AttributeFlags.fragment ||| AttributeFlags.compute
        lookup

    override this.CreateEmitter(targetInfo) = 
        let splits = targetInfo.Name.Split(',')
        if splits.Length = 2 && targetInfo.IsExecutable then
            SpirvEmitter(GetVersionAndExecutionModel(splits[0], splits[1]))
        else
            raise(InvalidOperationException())

    override this.EmitOutput(project, _binDirectory, emitter, isDebuggable) = 
        let output = emitter.EmitOutput(isDebuggable)
        let outputFilePath = project.Path.ChangeExtension(".spv")
        use fs = File.Open(outputFilePath.ToString(), FileMode.Create)
        SpirvModule.Serialize(fs, output.Module)

    override this.IsValidTargetName targetInfo = 
        // Target support
        let splits = targetInfo.Name.Split(',')
        if splits.Length = 2 && targetInfo.IsExecutable then
            IsValidExecutionModel(splits[0]) && IsValidVersion(splits[1])
        elif splits.Length = 1 && not targetInfo.IsExecutable then
            match splits |> Array.head with
            | Constants.lib -> true
            | _ -> false
        else
            false

    override _.GetAnalyzerDiagnostics(targetInfo, boundModel: OlyBoundModel, ct): OlyDiagnostic imarray = 
        let splits = targetInfo.Name.Split(',')
        if splits.Length < 1 then ImArray.empty
        else

        let kind = splits[0]
        let versions = 
            if splits.Length = 1 then
                [|"1";"0"|]
            else
                splits[1].Split('.')

        let majorVersion = Int32.Parse(versions[0])
        let minorVersion = Int32.Parse(versions[1])

        let isGreaterOrEqualToVersion_1_3 = majorVersion >= 1 && minorVersion >= 3
        let isGreaterOrEqualToVersion_1_2 = majorVersion >= 1 && minorVersion >= 2

        let targetFlag =
            match kind with
            | Constants.vertex -> AttributeFlags.vertex
            | Constants.fragment -> AttributeFlags.fragment
            | Constants.compute -> AttributeFlags.compute
            | Constants.lib -> AttributeFlags.None
            | _ -> unreached()

        let diagnostics = OlyDiagnosticLogger.CreateWithPrefix("SPIRV")

        let isSpirvAttributeName attrName =
            match attrName with
            | "uniformAttribute"
            | "locationAttribute"
            | "bindingAttribute"
            | "descriptor_setAttribute"
            | "positionAttribute"
            | "global_invocation_idAttribute"
            | "storage_bufferAttribute" -> true
            | _ -> false

        let isSpirvInputName name =
            match name with
            | "uniform"
            | "location"
            | "binding"
            | "descriptor_set"
            | "global_invocation_id"
            | "storage_buffer" -> true
            | _ -> false

        let isSpirvOutputName name =
            match name with
            | "location"
            | "position" -> true
            | _ -> false

        let checkAlignment (ty: OlyTypeSymbol) (useSyntax: OlySyntaxNode) =
            let is16byteAligned =
                if ty.IsTypeExtension then
                    true
                else
                    match ty.TryGetPackedSizeInBytes() with
                    | ValueSome sizeInBytes ->
                        let rm = sizeInBytes % 16
                        if rm <> 0 && 
                            sizeInBytes <> 1 && 
                            sizeInBytes <> 2 && 
                            sizeInBytes <> 4 && 
                            sizeInBytes <> 8 then
                            false
                        else
                            true
                    | _ ->
                        false

            if not is16byteAligned then
                diagnostics.Error($"'{ty.Name}' must be 16-byte aligned.", 10, useSyntax)

        let checkAttributeUsage name (useSyntax: OlySyntaxNode) =
            let isValid =
                match useSyntax.TryFindParent<OlySyntaxPropertyBinding>(ct) with
                | Some syntaxPropBinding ->
                    match syntaxPropBinding with
                    | OlySyntaxPropertyBinding.Binding(_, _, _, _, _, syntaxBinding) ->
                        match syntaxBinding with
                        | OlySyntaxBinding.Signature(syntaxBindingDecl) ->
                            match syntaxBindingDecl with
                            | OlySyntaxBindingDeclaration.Get _ when isSpirvInputName name ->
                                true
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
                diagnostics.Error($"Invalid use of '{name}' attribute.", 10, useSyntax)

        let checkPropertyFunctionDefinition (valueSymbol: OlyValueSymbol) (useSyntax: OlySyntaxNode) =
            OlyAssert.True(valueSymbol.IsGetterFunction || valueSymbol.IsSetterFunction)
            // TODO: Add checks to verify if the combination of attributes and property type are used correctly.
            //       Or, we could do this verification when we check the attribute usage, but will require querying with a syntax node.
            match valueSymbol.ReturnType with
            | Some(retTy) when valueSymbol.IsGetterFunction ->
                let mutable isUniform = false
                let mutable isStorageBuffer = false
                valueSymbol.ForEachAttribute(
                    fun attr ->
                        match attr.Name with
                        | "uniformAttribute" ->
                            isUniform <- true
                        | "storage_bufferAttribute" ->
                            isStorageBuffer <- true
                        | _ ->
                            ()
                )
                if not isGreaterOrEqualToVersion_1_3 && isStorageBuffer then
                    diagnostics.Error("Storage buffers are only available in version 1.3 or greater.", 10, useSyntax)
                if isUniform then
                    if retTy.IsMutableArray && not retTy.IsFixedArray then
                        diagnostics.Error("Uniforms cannot be a mutable array type.", 10, useSyntax)

                if retTy.IsArray then
                    checkAlignment retTy.TypeArguments[0] useSyntax
            | _ ->
                ()

        let isIntrinsicImport (valueSymbol: OlyValueSymbol) =
            match valueSymbol.Enclosing.TryType with
            | Some enclosingTy ->
                match enclosingTy.Enclosing.TryType with
                | Some enclosingTy -> enclosingTy.Name.Contains("OlyPrelude")
                | _ -> false
            | _ ->
                false

        let checkPropertyDefinition (valueSymbol: OlyValueSymbol) (useSyntax: OlySyntaxNode) =
            OlyAssert.True(valueSymbol.IsProperty)
            match valueSymbol.TryPropertyGetterSetter with
            | Some(Some getter, None) when getter.IsImported ->
                checkPropertyFunctionDefinition getter useSyntax
            | Some(Some setter, None) when setter.IsImported ->
                checkPropertyFunctionDefinition setter useSyntax
            | Some(Some getter, Some setter) 
                    when (not (isIntrinsicImport valueSymbol)) && 
                    (getter.IsImported || setter.IsImported) ->
                diagnostics.Error($"This kind of property definition cannot have a getter and setter.", 10, useSyntax)
            | _ ->
                ()

        let checkPropertyFunctionUsage (valueSymbol: OlyValueSymbol) (useSyntax: OlySyntaxNode) =
            OlyAssert.True(valueSymbol.IsGetterFunction || valueSymbol.IsSetterFunction)
            valueSymbol.ForEachAttribute(
                fun attr ->
                    if attr.IsImported then
                        match AttributeValidationLookup.TryGetValue(attr.Name) with
                        | true, flags ->
                            if (flags &&& targetFlag <> targetFlag) || (targetFlag = AttributeFlags.None) then
                                diagnostics.Error($"Only available in execution model(s) '{flags &&& AttributeFlags.ExecutionModelMask}'.", 10, useSyntax)
                        | _ ->
                            ()
            )

            if valueSymbol.ReturnType.Value.IsArray then
                let report() =
                    diagnostics.Error($"Invalid use of runtime array.", 10, useSyntax)
                match useSyntax.TryFindParent<OlySyntaxExpression>(ct) with
                | Some syntaxExpr ->
                    match syntaxExpr.Parent with
                    | :? OlySyntaxExpression as syntaxExpr ->
                        match syntaxExpr with
                        | OlySyntaxExpression.Indexer _ -> ()
                        | _ -> report()
                    | _ -> report()
                | _ -> report()

        let analyzeSymbol (symbolInfo: OlySymbolUseInfo) =
            let symbol = symbolInfo.Symbol
            // Definition
            if symbolInfo.Syntax.IsDefinition then
                if symbolInfo.Symbol.IsProperty then
                    checkPropertyDefinition symbolInfo.Symbol.AsValue symbolInfo.Syntax

                let isStructTy (ty: OlyTypeSymbol) =
                    not ty.IsBuiltIn && not ty.IsAlias && ty.IsValue && not ty.IsImported

                if not isGreaterOrEqualToVersion_1_3 then
                    if symbolInfo.Symbol.IsFieldOrAutoProperty then
                        let ty = symbolInfo.Symbol.AsValue.Type
                        if isStructTy ty then
                            diagnostics.Error($"Nested structs are only available in version 1.3 or greater.", 10, symbolInfo.Syntax)

                if symbolInfo.Symbol.IsType then
                    let ty = symbolInfo.Symbol.AsType
                    if isStructTy ty then
                        let fields = ty.Fields |> Seq.filter (fun x -> not x.IsStatic) |> ImArray.ofSeq
                        if fields.IsEmpty then
                            diagnostics.Error($"'{symbolInfo.Symbol.Name}' must declare at least one or more fields.", 10, symbolInfo.Syntax)
                        else
                            checkAlignment ty symbolInfo.Syntax                           


            // Usage
            else
                if symbol.IsFunction then
                    if (symbol.IsImported && symbol.IsConstructor && isSpirvAttributeName symbol.Name) then
                        checkAttributeUsage (symbol.Name.Replace("Attribute", "")) symbolInfo.Syntax
                elif symbol.IsProperty then
                    let valueSymbol = symbol.AsValue
                    match valueSymbol.TryPropertyGetterSetter with
                    | Some(Some valueSymbol, None)
                    | Some(None, Some valueSymbol) when valueSymbol.IsImported ->
                        checkPropertyFunctionUsage valueSymbol symbolInfo.Syntax
                    | _ ->
                        ()                      

        match boundModel.TryGetAnonymousModuleSymbol(ct) with
        | Some symbol ->
            analyzeSymbol symbol.UntypedInfo
        | _ ->
            ()

        boundModel.ForEachSymbol(boundModel.SyntaxTree.GetRoot(ct), analyzeSymbol, ct)

        diagnostics.GetDiagnostics()
