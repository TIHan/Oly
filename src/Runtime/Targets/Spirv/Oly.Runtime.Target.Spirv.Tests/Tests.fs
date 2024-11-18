[<Xunit.Collection("Sequential")>]
module Oly.Runtime.Target.Spirv.Tests

open System
open Utilities
open TestUtilities
open Xunit
open Spirv.TestHelpers
open Oly.Compiler.Syntax
open Oly.Compiler.Text
open Oly.Runtime.Target.Spirv
open Oly.Compiler.Workspace
open Oly.Core

let OlyVertex (src: string) =
    let src = $"""
#target "spirv: 1.3,vertex"

{src}
"""

    // TODO: This does not handle a debug build.
    //       To do this, we must supply an 'activeConfig' to the workspace that specifies what kind of build.
    //       We also need to specify a '.json' file with the correct configuration as well.

    let preludeDirName = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    let preludeDir = OlyPath.CreateAbsolute(preludeDirName)
    let preludeDir =
        if preludeDir.IsDirectory then
            preludeDir
        else
            OlyPath.Create(preludeDirName + "/")

    let workspace = OlyWorkspace.Create([SpirvTarget()])

    let documentPath = OlyPath.Combine(preludeDir, "test.olyx")
    let srcText = OlySourceText.Create(src)

    let textManager = OlySourceTextManager.Empty.Set(documentPath, srcText, 1)
    let resourceSnapshot = OlyWorkspaceResourceSnapshot.Create(OlyPath.Empty).WithTextEditors(textManager)

    // Set up prelude
    let preludePath = OlyPath.Combine(preludeDir, "spirv_prelude.olyx")
    let resourceSnapshot = resourceSnapshot.SetResourceAsCopy(preludePath)

    let _doc = workspace.UpdateDocumentAsync(resourceSnapshot, documentPath, srcText, Threading.CancellationToken.None).Result[0]

    match workspace.BuildProjectAsync(resourceSnapshot, documentPath, Threading.CancellationToken.None).Result with
    | Error(diags) ->
        let resourceSnapshot = resourceSnapshot.RemoveResource(documentPath)
        workspace.RemoveProject(resourceSnapshot, documentPath, Threading.CancellationToken.None)
        raise(Exception(OlyDiagnostic.PrepareForOutput(diags, Threading.CancellationToken.None)))
    | Ok(program) ->
        let resourceSnapshot = resourceSnapshot.RemoveResource(documentPath)
        workspace.RemoveProject(resourceSnapshot, documentPath, Threading.CancellationToken.None)
        program

let shouldRun (program: OlyProgram) =
    program.Run()
    // TODO: This only works for vertex as of right now.
    let fs = System.IO.File.OpenRead(OlyPath.ChangeExtension(program.Path, ".spv").ToString())
    let sm = Spirv.SpirvModule.SpirvModule.Deserialize(fs)
    fs.Dispose()
    System.IO.File.Delete(program.Path.ToString())
    let _defaultVertexCode = @"
#version 450

layout(location = 0) in vec2 Position;
layout(location = 1) in vec4 Color;

layout(location = 0) out vec4 fsin_Color;

void main()
{
    gl_Position = vec4(Position, 0, 1);
    fsin_Color = Color;
}"

    let defaultFragmentCode = @"
#version 450

layout(location = 0) in vec4 fsin_Color;
layout(location = 0) out vec4 fsout_Color;

void main()
{
    fsout_Color = fsin_Color;
}"

    draw_quad(sm, glsl_to_fragment(defaultFragmentCode))

[<Fact>]
let ``Blank vertex shader`` () =
//#version 450

//void main()
//{
//}
    let src =
        """
main(): () =
    ()
        """
    OlyVertex src
    |> shouldRun

[<Fact>]
let ``Blank vertex shader but has output`` () =
//#version 450

//void main()
//{
//    gl_Position = vec4(1);
//}

    let glslSrc =
        """
#version 450

void main()
{
    gl_Position = vec4(1);
}
        """

    let target = glsl_to_vertex(glslSrc)
    let src =
        """
main(): vec4 =
    vec4(1)
        """
    OlyVertex src
    |> shouldRun

[<Fact>]
let ``Basic vertex shader`` () =
//#version 450

//layout(location = 0) in vec2 Position;
//layout(location = 1) in vec4 Color;

//layout(location = 0) out vec4 fsin_Color;

//void main()
//{
//    gl_Position = vec4(Position, 0, 1);
//    fsin_Color = Color;
//}
    let src =
        """
main(position: vec2, color: vec4): (position: vec4, color: vec4) =
    (vec4(position, 0, 1), color)
        """
    OlyVertex src
    |> shouldRun

