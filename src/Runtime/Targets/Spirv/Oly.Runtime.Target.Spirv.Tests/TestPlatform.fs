module TestPlatform

open System.IO
open System.Text
open Oly.Metadata
open Oly.Runtime
open Oly.Runtime.Target.Spirv.Emitter
open Oly.Core
open Xunit
open Spirv
open Spirv.SpirvModule
open Spirv.TestHelpers

let globalSetup() =
    ()

let createEmitter(asm: OlyILAssembly) =
    SpirvEmitter(ExecutionModel.Vertex) // TODO: What to do about fragment, compute, etc.?

let configureRuntime(vm: OlyRuntime<SpirvType, SpirvFunction, SpirvField>) =
    ()

let emitterWrite(emitter: SpirvEmitter, isDebuggable) =
    emitter.EmitOutput(isDebuggable)

let run (spvOutput: SpirvOutput, expectedOutput: string) : unit =
    let defaultVertexCode = @"
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

    draw_quad(spvOutput.Module, glsl_to_fragment(defaultFragmentCode))